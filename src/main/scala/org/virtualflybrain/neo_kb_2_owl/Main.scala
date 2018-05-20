package org.virtualflybrain.neo_kb_2_owl
import dosumis.brainscowl.BrainScowl
import org.neo4j.driver.v1._

import org.backuity.clist._

class Cat extends Command(description = """
   Convert individuals directly associated with a specified dataset
   from neo4j KB to OWL. By default only individuals and their Types are
   translated. Facts refering to the translated individual may be optionally.  
   If facts are specified, facts pecifying image (channel) individuals are 
   ignored unless explicitly specified for inclusion.""") {
  var usr = arg[String](description = "username")
  var pwd = arg[String](description = "password")
  var endpoint = arg[String](description = "neo4J bolt endpoint URI")
  var dataset = arg[String](description = "dataset to load") // This would allow current pattern of control by shell script
  var ontology = arg[String](description = "Path to reference ontology used in build.") // Should be URI but work needed on BrainScowl loader
  var facts = opt[Boolean](abbrev = "f", description = "Set to add facts")
  var include_images = opt[Boolean](abbrev = "i", description = "Set to include individuals.")
  var infer_overlaps = opt[Boolean](abbrev = "io", description = """Infer overlaps between individual neurons and 
                                                                    neuropil classes based on voxel overlaps 
                                                                    between individuals""")  // Modify to allow cutoff spec?
  var syntax = opt[String](description = "Syntax of output file.", default = "ofn")
  var test = opt[Boolean](abbrev = "t", description = "Run in test mode (sets return limits on queries)")
}

// I'm sure there's a better way... This seems awfully repetitive
case class build_args(driver: Driver, ontology: BrainScowl, dataset: String,
                      facts: Boolean, test: Boolean, infer_overlaps: Boolean, 
                      include_images: Boolean, syntax: String)

class Builder(driver: Driver, 
              ontology: BrainScowl, 
              test: Boolean, 
              include_images: Boolean,
              syntax: String) {
  
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
        c2o.infer_overlap_from_channels(cutoff = 1000)
      }
      vfb_owl.save(file_path = ds + ".owl", syntax = args.syntax)
      vfb_owl.sleep()
      session.close()
  }
}
    
    
    

object Main extends (App) {
  Cli.parse(args).withCommand(new Cat) {
    case cat =>
      val g = GraphDatabase.driver(cat.endpoint, AuthTokens.basic(cat.usr, cat.pwd))
      var fbbt = new BrainScowl(file_path = cat.ontology)
      val builder = new Builder(g, fbbt, cat.test, cat.include_images, cat.syntax)
      if (cat.dataset == "ALL") {
        builder.build_all()
      } else {
        builder.build_one(dataset=cat.dataset, 
                          facts = cat.facts,
                          infer_overlaps = cat.infer_overlaps)
      }
      fbbt.sleep()
      g.close()
  }
}
