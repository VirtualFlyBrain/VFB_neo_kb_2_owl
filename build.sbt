organization  := "org.virtualflybrain"

name          := "neo_kb_2_owl"

version       := "0.0.1"

scalaVersion  := "2.11.8" // scowl allegedly works with scala 2.12 - but tests failed

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

licenses := Seq("MIT license" -> url("https://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/dosumis/brainscowl"))

javaOptions += "-Xmx6G"

// initialCommands in (Test, console) := """ammonite.Main().run()"""

libraryDependencies ++= {
    //  TO CHECK: Is OWL API really needed or does in come with scowl?
    //   "net.sourceforge.owlapi"     %  "owlapi-distribution" % "4.2.1",

    Seq(
      "org.backuity.clist" %% "clist-core"   % "3.2.2",
      "org.backuity.clist" %% "clist-macros" % "3.2.2" % "provided",
      "org.neo4j.driver" % "neo4j-java-driver" % "1.0.4"  withJavadoc(),
      "org.phenoscape"             %% "scowl"            % "1.3" withJavadoc(),
      "org.semanticweb.elk"    %   "elk-owlapi"          % "0.4.3" withJavadoc(),
      "org.scalactic" %% "scalactic" % "3.0.1",      
      "org.scalatest" %% "scalatest" % "3.0.1"  withJavadoc()
      )  
}

// Note - brainscowl import from GitHub is specified in project/build.scala

initialCommands := "import org.virtualflybrain.neo_kb_2_owl"

