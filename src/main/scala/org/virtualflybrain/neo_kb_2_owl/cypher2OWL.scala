package org.virtualflybrain.neo_kb_2_owl
import dosumis.brainscowl.BrainScowl
import org.phenoscape.scowl
import org.phenoscape.scowl._
import org.neo4j.driver.v1._
import scala.collection.JavaConversions._


//* Notes: 
// AIMS: generic translations of edges to owl using standard mapping
// A clear way to define which ind belongs to which dataset (are overlapping datasets allowed? May be an issue for EM data.)
// Minimal
// (DataSet)-[:has_source]-(a:Individual)-[:INSTANCEOF|RELATED]-(c:Class)
// With SCOWL - Don't need to check existence first. Just add everything with full IRIs
// For clusters we also want to extend out to facts:
// Or could just query for facts generically and then exclude any where type of target ind :INSTANCEOF channel.

// But then - shouldn't Individuals be referenced in some other dataset before adding?
// Better to do with config - set flag to add facts.
// (DataSet)-[:has_source]-(a:Individual)

// Extended (to include images:
// (DataSet)-[:has_source]-(a:Individual)-[depicts]-(channel:Individual)-[:in_register_with]-(fu)-[depicts]-(template)
//
// Xref writer
// Needs cycle over

class cypher2OWL(bs: BrainScowl, session: Session, dataset: String) {

  // constructor should include new Brainscowl object

  // Restricting to a single dataset is hard without encoding more of the schema into the translator.
  // Only anatomical individuals have a direct connection to a dataset.  So clearly all of those should be added.
  // Every class or individual one step away should be added too.
  // But this would miss edge links to templates
  // And class links to clusters (?)
  // And then what about when we start adding connectomics data?  
  // At that point any synaptic connection between two inds in any allowed datasets should be allowed.
  // 

  // DataSet matching strategies:
  // 1. Match in Cypher:
  ////  Match direct individuals; 
  // 2. Load all and filter. The latter has the advantage that it will be easy to support intra-datset links
  // coming from connectomics data.

  def add_typed_inds(test: Boolean = false) {

    val limit = if (test) {
      " limit 10"
    } else {
      ""
    } // Should really make test status into a separate function!
    val cypher = s"""MATCH (c:Class)<-[r:INSTANCEOF|Related]-(i:Individual)-[:has_source]->(ds:DataSet)
                  WHERE ds.short_form = '$dataset' 
                  RETURN c.iri, r.iri, i.iri, type(r) as rel_typ""" + limit

    val results = this.session.run(cypher)
    while (results.hasNext()) {
      val record = results.next();
      val i = NamedIndividual(record.get("i.iri").asString)
      // TODO: call to function to check if i already in owl, add label if not
      val c = Class(record.get("c.iri").asString)
      if (record.get("rel_typ").asString == "INSTANCEOF") {
        //print(s"""Adding $i Type $c \n""")
        this.bs.add_axiom(i Type c)
      } else if (record.get("rel_typ").asString == "Related") {
        val r = ObjectProperty(record.get("r.iri").asString)
        //print(s"""Adding $i Type $r some $c \n""")
        this.bs.add_axiom(i Type (r some c))
      } else {
        /// Add in a warning or fail.
      }
      // if (record.get("hs.accession"))  - How to check truth - Return none type or perhaps none as text
    }
  }
  
  def add_annotations(test: Boolean = false) {
    val limit = if (test) {
      " limit 10"
    } else {
      ""
    } 
    val label = AnnotationProperty("http://www.w3.org/2000/01/rdf-schema#label")
    val comment = AnnotationProperty("http://www.w3.org/2000/01/rdf-schema#comment")
    val synonym = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#hasExactSynonym")
    val cypher = s"""MATCH (i:Individual)-[:has_source]->(ds:DataSet)
                  WHERE ds.short_form = '$dataset' 
                  RETURN i.iri, i.label, i.comment, i.synonyms""" + limit
    val results = this.session.run(cypher)
    while (results.hasNext()) {
      val record = results.next();
      val i = NamedIndividual(record.get("i.iri").asString)             
      this.bs.add_axiom(i Annotation (label, record.get("i.label").asString()))
      this.bs.add_axiom(i Annotation (comment, record.get("i.comment").asString()))
      val syns = record.get("i.synonyms").asList.toArray
      for (s <- syns) {
          this.bs.add_axiom(i Annotation (synonym, s.toString))
          }            
     }
   }
  
  def add_xrefs(test: Boolean = false) {
    val limit = if (test) {
      " limit 10"
    } else {
      ""
    }
    val xref = AnnotationProperty("http://www.w3.org/2000/01/rdf-schema#label")
    val cypher = s"""MATCH (s:Site)<-[dbx:hasDbXref]-(i:Individual)-[:has_source]->(ds:DataSet)
                  WHERE ds.short_form = '$dataset' 
                  RETURN i.iri, i.label, i.comment""" + limit
    val results = this.session.run(cypher)
    while (results.hasNext()) {
      val record = results.next();
      val i = NamedIndividual(record.get("i.iri").asString)    
      this.bs.add_axiom(i Annotation (xref,
          record.get("s.db").asString + ":" + record.get("hs.accession").asString))
    }
  }

  
  def add_facts(blacklist: Array[String], test: Boolean = false) {
    // Add facts. Optionally specify relations blacklist
    val limit = if (test) {
      " limit 10"
    } else {
      ""
    }
    val cypher = s"""MATCH (c:Class)-[cr:INSTANCEOF:Related]-(j:Individual)<-[r:RELATED]-(i:Individual)-[:has_source]->(ds:DataSet) " 
                  WHERE ds.label = '$dataset'"
                  RETURN c.iri, i.iri, r.iri, type(r) as rel_typ""" + limit
    val results = this.session.run(cypher)
    while (results.hasNext()) {
      val record = results.next();
      if (!(blacklist contains record.get("r.iri").asString)) {
        val i = NamedIndividual(record.get("i.iri").asString)
        val j = NamedIndividual(record.get("j.iri").asString)
        val r = ObjectProperty(record.get("r.iri").asString)
        this.bs.add_axiom(i Fact (r, j))
        // Adding typing to J: This needs some refactoring to pull out generic typing into new method.
        val c = Class(record.get("c.iri").asString) 
        if (record.get("rel_typ").asString == "INSTANCEOF") {
          //print(s"""Adding $i Type $c \n""")
          this.bs.add_axiom(i Type c)
        } else if (record.get("rel_typ").asString == "Related") {
          val r = ObjectProperty(record.get("r.iri").asString)
          //print(s"""Adding $i Type $r some $c \n""")
          this.bs.add_axiom(i Type (r some c))
        } else {
          /// Add in a warning or fail.
        }
        // if (record.get("hs.accession"))  - How to check truth - Return none type or perhaps none as text?
        //       
        this.bs.add_axiom(j Type c) // Extend to add anonymous class typing to object ind
      }    
    }
  }
  // Matching APs would be cool if poss...
}