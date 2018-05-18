package org.virtualflybrain.neo_kb_2_owl
import scala.collection._
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

class cypher2OWL(bs: BrainScowl, support_ont: BrainScowl, session: Session, dataset: String) {
  
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
    //* Adds typed, annotated inds to bs.
    //* returns a map of feature short_form:label for use in rolling defs
    // Design is rather too contingent for comfort.  Better to have exp labels this in reference ontology.
    val limit = if (test) {
      " limit 10"
    } else {
      ""
    } // Should really make test status into a separate function!
    var feat_tracker = mutable.Set[String]()
    
    val cypher = s"""MATCH (c:Class)<-[r:INSTANCEOF|Related]-(i:Individual)-[:has_source]->(ds:DataSet)
                  WHERE ds.short_form = '$dataset' 
                  RETURN c.iri, r.iri, i.iri, c.short_form, type(r) as rel_typ, 
                  i.label, i.comment, i.synonyms, c.label""" + limit
    val ind_tracker = mutable.Set[String]()
    val results = this.session.run(cypher)
    val label = AnnotationProperty("http://www.w3.org/2000/01/rdf-schema#label")

    while (results.hasNext()) {      
      val record = results.next();
      val ind_iri = record.get("i.iri").asString
      val i = NamedIndividual(ind_iri)
      // TODO: call to function to check if i already in owl, add label if not
      val c = Class(record.get("c.iri").asString)
      val feature = Class("http://purl.obolibrary.org/obo/SO_0000400")
      if (record.get("rel_typ").asString == "INSTANCEOF") {
        //print(s"""Adding $i Type $c \n""")
        this.bs.add_axiom(i Type c)
      } else if (record.get("rel_typ").asString == "Related") {
        val riri = record.get("r.iri").asString
        val r = ObjectProperty(riri)
        //print(s"""Adding $i Type $r some $c \n""")
        this.bs.add_axiom(i Type (r some c))
        if (riri == "http://purl.obolibrary.org/obo/RO_0002292") {
          val ciri = record.get("c.iri").asString()
          if (!feat_tracker.contains(ciri)) {
            // Seems to be needed in otder 
            val clab = record.get("c.label")
            if (!clab.isNull()) {
               this.support_ont.add_axiom(c SubClassOf feature) // Using for declarations
               this.support_ont.add_axiom(c Annotation (label, clab.asString()))
               feat_tracker.add(ciri)
            }
          }
        }
      }
      if (!ind_tracker.contains(ind_iri)) {
          this.add_annotations(record)
          ind_tracker.add(ind_iri)
      }
    }
  }

  def add_annotations(record: Record) {  
    val label = AnnotationProperty("http://www.w3.org/2000/01/rdf-schema#label")
    val comment = AnnotationProperty("http://www.w3.org/2000/01/rdf-schema#comment")
    val synonym = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#hasExactSynonym")
      val i = NamedIndividual(record.get("i.iri").asString)
      val lab = record.get("i.label")
      if (!lab.isNull()) {
        this.bs.add_axiom(i Annotation (label, lab.asString()))
      }
      val comm = record.get("i.comment")
      if (!lab.isNull()) {
        this.bs.add_axiom(i Annotation (comment, lab.asString()))
      }
      val synrec = record.get("i.synonyms")
      if (!synrec.isNull()) {
        val syns = synrec.asList.toArray
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
    val xref = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#hasDbXref")
    val cypher = s"""MATCH (s:Site)<-[dbx:hasDbXref]-(i:Individual)-[:has_source]->(ds:DataSet)
                  WHERE ds.short_form = '$dataset' 
                  RETURN i.iri, s.label, dbx.accession""" + limit
    val results = this.session.run(cypher)
    while (results.hasNext()) {
      val record = results.next();
      val i = NamedIndividual(record.get("i.iri").asString)
      // Using Site label for now.  Probably not ideal.
      this.bs.add_axiom(i Annotation (xref,
          record.get("s.label").asString + ":" + record.get("dbx.accession").asString))
    }
  }
  
  case class Overlap(
      neuropil_class_iri: String,
      neuropil_class_label: String,
      neuropil_channel_name: String,
      left: String,
      right: String,
      center: String
      )  
  
  def infer_overlap_from_channels(cutoff: Int, test: Boolean = false) {
    val cypher = s"""MATCH (ds:DataSet { short_form: '$dataset' })<-[:has_source]-(neuron:Individual)
    -[:Related { short_form: 'depicts' }]->(s:Individual) 
		-[re:Related]->(o:Individual)-[:Related { short_form: 'depicts' }]->(x) 
		-[:INSTANCEOF]->(neuropil_class:Class) 
		WHERE re.label = 'overlaps' 
		AND ((re.voxel_overlap_left > $cutoff)  
		OR (re.voxel_overlap_right > $cutoff) 
		OR (re.voxel_overlap_center > $cutoff)) 
		RETURN properties(re) as voxel_overlap, neuron.short_form, neuron.iri, neuron.label,
		neuropil_class.short_form, neuropil_class.iri, neuropil_class.label, 
		o.label as neuropil_channel_name"""
    val results = this.session.run(cypher)
    ////	# Intermediate data-structures to add neuropils and iterate over neurons
    //	overlap_by_neuron = {}
    //	all_neuropils = set()
    //	for o in overlap_results:
    //		n = o['neuron.short_form']
    //		d = {'neuropil' : o['neuropil_class.short_form'],
    //			'voxel_overlap' : o['voxel_overlap']}
    //		all_neuropils.add(o['neuropil_class.iri'])
    //		if not (n in overlap_by_neuron.keys()):
    //			overlap_by_neuron[n] = []
    //		overlap_by_neuron[n].append(d)
    // A literal translation of python is quite painfull!
    // better to make case class ?
		var overlap_by_neuron = mutable.Map[String, mutable.ArrayBuffer[Overlap]]()
		var all_neuropils = mutable.Set[String]()
		while (results.hasNext()) {
      val record = results.next();
      val n = record.get("neuron.iri").asString()
      val vo = record.get("voxel_overlap").asMap
      val d = Overlap(neuropil_class_iri = record.get("neuropil_class.iri").asString,
                     neuropil_class_label =  record.get("neuropil_class.label").asString,
                     neuropil_channel_name = record.get("neuropil_channel_name").asString,
                     left = if (vo.keys.contains("voxel_overlap_center")) {
                       vo("voxel_overlap_left").toString
                       } else { "0" },
                     right = if (vo.keys.contains("voxel_overlap_right")) {
                       vo("voxel_overlap_right").toString
                       } else { "0" },
                     center = if (vo.keys.contains("voxel_overlap_center")) {
                       vo("voxel_overlap_center").toString
                       } else { "0" }
                      )
                  
                     
                 // voxel overlap should be float!
      all_neuropils.add(record.get("neuropil_class.iri").asString())
      if (!overlap_by_neuron.contains(n)) {
//          overlap_by_neuron(n) = mutable.ArrayBuffer[String]()
//        } else {
          overlap_by_neuron(n).append(d)
        }  
      }
//	vokeys = {'voxel_overlap_left': 'left', 
//		'voxel_overlap_right' : 'right', 
//		'voxel_overlap_center': ''}    
//    val vokeys = Map("voxel_overlap_left" -> "left",
//                    "voxel_overlap_right" -> "right", 
//                    "voxel_overlap_center" -> "")
//	for neuron, overlaps in overlap_by_neuron.items():
//		neuron_overlap_txt  = []
//		# Iterate over neuropils.
//		for o in overlaps:
//			voxel_overlap = o['voxel_overlap'] 
//			typ = "RO_0002131 some %s" % o['neuropil']
//			vfb_ind.type(typ,neuron)
//			txt = "Overlap of %s inferred from " % fbbt.getLabel(o['neuropil'])
//			vo_data = []
//			for k,v in vokeys.items():
//				if k in voxel_overlap.keys() and voxel_overlap[k] > cutoff:
//					vo_data.append(
//						"%d voxel overlap of the %s %s"  % (voxel_overlap[k], 
//														v, fbbt.getLabel(o['neuropil']))) # Better to ref painted domain?
//			neuron_overlap_txt.append(txt + ', '.join(vo_data))
//		vfb_ind.comment(neuron, '. '.join(neuron_overlap_txt) + '.')
    val ro_overlap = ObjectProperty("http://purl.obo.obrary.org/obo/RO_0002131")
    for ((neuron, overlaps) <- overlap_by_neuron.toIterable) {
       var neuron_overlap_txt = mutable.ArrayBuffer[String]()
       val n = NamedIndividual(neuron)
       for (o <- overlaps) {
         val c = Class(o.neuropil_class_iri)
         val channel_name = o.neuropil_channel_name
         val class_label = o.neuropil_class_label
         val txt = s"""Overlap of $class_label inferred from voxel overlap of
                       $channel_name: 
                       left: $o.left, right: $o.right, center: $o.center."""  // Ideally this will be edge annotation but for now we'll make it cat comment
         bs.add_axiom(n Type (ro_overlap some c))
       }
       }
    }

  
  def add_facts(blacklist: Array[String], test: Boolean = false) {
    // Add facts. Optionally specify relations blacklist
    val limit = if (test) {
      " limit 10"
    } else {
      ""
    }
    val cypher = s"""MATCH (c:Class)-[cr:INSTANCEOF|Related]-(j:Individual)
                  <-[r:RELATED]-(i:Individual)-[:has_source]->(ds:DataSet) 
                  WHERE ds.label = '$dataset' 
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