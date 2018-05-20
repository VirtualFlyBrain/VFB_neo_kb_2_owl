package org.virtualflybrain.neo_kb_2_owl
import dosumis.brainscowl.BrainScowl
import org.virtualflybrain.neo_kb_2_owl.Builder
import org.neo4j.driver.v1._


import org.backuity.clist._

class Cat extends Command(description = """
   Convert individuals directly associated with a specified dataset
   from neo4j KB to OWL. By default only individuals and their Types are
   translated. Facts refering to the translated individual may be optionally.  
   If facts are specified, facts specifying image (channel) individuals are 
   ignored unless explicitly specified for inclusion.
   If include_overlaps is specified then individual -> Class assertions of 
   overlap are added based on channel:channel voxel overlap.
   If dataset = 'ALL' then all dataset marked production:True are loaded""") {
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
  var out_path = opt[String](description = "Path to output files", default = ".")
  var test = opt[Boolean](abbrev = "t", description = "Run in test mode (sets return limits on queries)")
}


    
    
    

object Main extends (App) {
  Cli.parse(args).withCommand(new Cat) {
    case cat =>
      val g = GraphDatabase.driver(cat.endpoint, AuthTokens.basic(cat.usr, cat.pwd))
      var fbbt = new BrainScowl(file_path = cat.ontology)
      val builder = new Builder(g, fbbt, cat.test, cat.include_images, cat.syntax, cat.out_path)
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
