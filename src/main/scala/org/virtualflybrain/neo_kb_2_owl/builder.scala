package org.virtualflybrain.neo_kb_2_owl
import dosumis.brainscowl.BrainScowl
import org.neo4j.driver.v1._

// I'm sure there's a better way... This seems awfully repetitive
case class build_args(driver: Driver, ontology: BrainScowl, dataset: String,
                      facts: Boolean, test: Boolean, infer_overlaps: Boolean, 
                      include_images: Boolean, syntax: String)

class Builder(driver: Driver, 
              ontology: BrainScowl, 
              test: Boolean, 
              include_images: Boolean,
              syntax: String,
              out_path: String) {
  
  def build_all() {
    val cypher = """MATCH (ds:DataSet) 
                  RETURN ds.short_form as dataset, ds.production, 
                  ds.facts, ds.infer_overlap"""
    val session = driver.session()
    val results = session.run(cypher)
    // Building a list to iterate over once session closed as nesting caused problems.
    val build_list = collection.mutable.ArrayBuffer[build_args]()
       while (results.hasNext()) {  
          val record = results.next()
          val production = record.get("ds.production") 
          val p =  if (production.isNull) { false } else { production.asBoolean() }
          if (p) {
            val dataset = record.get("dataset")
            val facts = record.get("ds.facts")
            val f = if (facts.isNull) { false } else { facts.asBoolean() }
            val infer_overlap = record.get("ds.infer_overlap")
            val io = if (infer_overlap.isNull) { false } else { infer_overlap.asBoolean() }
            if (!dataset.isNull) {
              build_list.append(build_args(driver = this.driver, 
                      ontology = this.ontology, 
                      dataset = dataset.asString(),
                      facts = f,
                      test = this.test, 
                      infer_overlaps = io, 
                      include_images = this.include_images,
                      syntax = this.syntax))
            }
          }
       }
       session.close()
       for (b <- build_list) {
         this.build_private(b)
       }
  }

  
  def build_one(dataset: String, facts: Boolean, infer_overlaps:Boolean) {
    this.build_private(
         build_args(  driver = this.driver, 
                      ontology = this.ontology, 
                      dataset = dataset,
                      facts = facts,
                      test = this.test, 
                      infer_overlaps = infer_overlaps, 
                      include_images = this.include_images,
                      syntax = this.syntax)
                      )
  }
  
  def build_private(args:build_args) {
      val base = "http://virtualflybrain.org/owl/" // check
      var vfb_owl = new BrainScowl(iri_string = base + args.dataset, base_iri = base)
      val session = args.driver.session()
      val blacklist =
        if (!(args.include_images)) {
          Array[String]()
        } else {
          Array[String]("http://xmlns.com/foaf/0.1/depicts")
        }
      // Make this an arg?
      val ds = args.dataset

      var c2o = new cypher2OWL(vfb_owl, args.ontology, session, ds)
      println(s"*** Processing $ds")
      println("*** Adding, annotating typed inds")
      c2o.add_typed_inds(args.test)
      println("*** Adding xrefs")
      c2o.add_xrefs(args.test)
      if (args.facts) {
        println("*** Adding facts")
        c2o.add_facts(blacklist, args.test)
      }
      var dw = new definition_writer(vfb_owl, ontology)
      println("*** Adding defs")
      dw.add_defs()
      if (args.infer_overlaps) { 
        println("*** Adding overlaps infered from voxel overlap")
        c2o.infer_overlap_from_channels(cutoff = 1000) // Should be configurable from KB, not preset for all builds!
      }
      vfb_owl.save(file_path = this.out_path + "/" + ds + ".owl", syntax = args.syntax)
      vfb_owl.sleep()
      session.close()
  }
}