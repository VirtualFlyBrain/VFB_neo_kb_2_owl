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
}

object main extends App {
  Cli.parse(args).withCommand(new Cat) { case cat => 
  val g = GraphDatabase.driver(cat.endpoint, AuthTokens.basic(cat.usr,cat.pwd))
  val session = g.session()
  val datasets = Vector[String]("Chiang2010", "Ito2012") // This should be pushed to config.
  val base = "http://virtualflybrain.org/owl/" // check
  for (d <- datasets) {
    var owl = new BrainScowl(base + d, base)
//  var c20 = cypher2OWL(s, owl, d)  // datasets could have flags corresponding to particular scheme needs.  e.g. voxel overlap
//    c20.add_inds
//    c20.add_direct_named_types
//    c20.add_direct_anonymous_types
  }
  
  g.close()
  } 
}
