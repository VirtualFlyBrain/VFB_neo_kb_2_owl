package org.virtualflybrain.neo_kb_2_owl
import dosumis.brainscowl.BrainScowl	
import org.phenoscape.scowl
import org.phenoscape.scowl._
import org.neo4j.driver.v1._

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



class cypher2OWL(bs: BrainScowl, session: Session , dataset: String) {
  
  
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
  
  def add_typed_ind_by_dataset(dataset :String) {
    val xref = AnnotationProperty("") // Better to pull than hard wire?
    val cypher = """MATCH (c:Class)<-[r:INSTANCEOF|RELATED]-(i:Individual)-[:has_source]->(ds:DataSet)
                  WHERE ds.label = '%s' 
                  WITH c,r,i
                  OPTIONAL MATCH (i)-[hs:has_site]-(s:Site)
                  RETURN c, i, r type(r) as rel_typ, hs, s"""  // add interpolation with this.dataset
    val results = this.session.run(cypher)
    while (results.hasNext()) {
      val record = results.next();
      val i = NamedIndividual(record.get("i.iri").asString)
      val c = Class(record.get("c.iri").asString)
      if (record.get("rel_type").asString == "INSTANCEOF") {
        this.bs.add_axiom(i Type c)
      } else if (record.get("rel_type").asString == "RELATED") {
        val r = ObjectProperty(record.get("r.iri").asString)
        this.bs.add_axiom(i Type (r some c))
      } else {
        /// Add in a warning or fail.
      }
      // if (record.get("hs.accession"))  - How to check truth - Return none type or perhaps none as text?
      // 
      this.bs.add_axiom(i Annotation (xref, 
          record.get("s.db").asString + ":" +  record.get("hs.accession").asString))
  }
 }
    
  def add_facts_by_dataset(dataset :String, blacklist: Array[String]) {
    val cypher = """MATCH (c:Class)-[:INSTANCEOF]-(j:Individual)<-[r:RELATED]-(i:Individual)-[:has_source]->(ds:DataSet) " 
                  WHERE ds.label = '%s' "
                  RETURN c, i, r"""  // add interpolation with this.dataset
    val results = this.session.run(cypher)
    while (results.hasNext()) {
      // Add a step to check if c.class is on blacklist
      val record = results.next();
      val i = NamedIndividual(record.get("i.iri").asString)
      val j = NamedIndividual(record.get("j.iri").asString)
      val r = ObjectProperty(record.get("r.iri").asString)
      this.bs.add_axiom(i Fact (r, j))
      //val c = Class(record.get("c.iri").asString)  // Should we deal with this here at all?
      //this.bs.add_axiom(j Type (r some c))

    }
  }  

  
  
  // Matching APs would be cool if poss...
  
  
  
  
  
}