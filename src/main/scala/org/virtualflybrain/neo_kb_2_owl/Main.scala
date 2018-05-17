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

object Main extends (App) {
  Cli.parse(args).withCommand(new Cat) {
    case cat =>
      val g = GraphDatabase.driver(cat.endpoint, AuthTokens.basic(cat.usr, cat.pwd))
      val session = g.session()
      val base = "http://virtualflybrain.org/owl/" // check
      val blacklist =
        if (!(cat.include_images)) {
          Array[String]()
        } else {
          Array[String]("http://xmlns.com/foaf/0.1/depicts")
        }
      // Make this an arg?
      val ds = cat.dataset
      var vfb_owl = new BrainScowl(iri_string = base + ds, base_iri = base)
      var c2o = new cypher2OWL(vfb_owl, session, cat.dataset)
      var fbbt = new BrainScowl(file_path = cat.ontology)
      println(s"*** Processing $ds")
      println("*** Adding typed inds")
      c2o.add_typed_inds(cat.test)
      println("*** Adding Annotations")
      c2o.add_annotations(cat.test)
      println("*** Adding xrefs")
      c2o.add_xrefs(cat.test)
      if (cat.facts) {
        println("*** Adding facts")
        c2o.add_facts(blacklist, cat.test)
      }
      var dw = new definition_writer(vfb_owl, fbbt)
      println("*** Adding defs")
      dw.add_defs()
      if (cat.infer_overlaps) { 
        c2o.infer_overlap_from_channels(cutoff = 1000, dataset = ds)
      }
      vfb_owl.save(file_path = cat.dataset + ".owl", syntax = "ofn")
      session.close()
      g.close()
  }
}
