package org.virtualflybrain.neo_kb_2_owl
import dosumis.brainscowl.BrainScowl
import org.neo4j.driver.v1._

import org.backuity.clist._

class Cat extends Command(description = "Command line options and args for .") {
  var usr = arg[String](description = "username")
  var pwd = arg[String](description = "password")
  var endpoint = arg[String](description = "neo4J bolt endpoint URI")
  var dataset = arg[String](description = "dataset to load") // This would allow current pattern of control by shell script
  var facts = opt[Boolean](abbrev = "f", description = "Set to add facts")
  var include_images = opt[Boolean](abbrev = "i", description = "Set to include individuals.")
  var syntax  = opt[String](description = "Syntax of output file.")
  var test = opt[Boolean](abbrev = "t", description = "Run in test mode (sets return limits on queries)")
}

object Main extends App {
  Cli.parse(args).withCommand(new Cat) { case cat => 
  val g = GraphDatabase.driver(cat.endpoint, AuthTokens.basic(cat.usr,cat.pwd))
  val session = g.session()
  val base = "http://virtualflybrain.org/owl/" // check
  val blacklist = 
      if (!(cat.include_images)) {
        Array[String]()
      } else {
        Array[String]("http://xmlns.com/foaf/0.1/depicts")
      }
     // Make this an arg?
  var owl = new BrainScowl(base + cat.dataset, base)
  var c2o = new cypher2OWL(owl, session, cat.dataset)
  c2o.add_typed_inds(cat.test)
  if (cat.facts) {
    c2o.add_facts(blacklist, cat.test)
  }
  val fbbt = new BrainScowl("http://purl.obolibrary.org/obo/fbbt-simple.owl")
  var dw = new definition_writer(owl, fbbt)
  dw.add_defs()
    
//  var c20 = cypher2OWL(s, owl, d)  // datasets could have flags corresponding to particular scheme needs.  e.g. voxel overlap
//    c20.add_inds
//    c20.add_direct_named_types
//    c20.add_direct_anonymous_types
//    owl.save(file_path = cat.dataset + ".owl", syntax = "ofn")
  g.close()
  } 
}
