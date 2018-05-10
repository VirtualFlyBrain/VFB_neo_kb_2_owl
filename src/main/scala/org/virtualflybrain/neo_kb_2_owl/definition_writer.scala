package org.virtualflybrain.neo_kb_2_owl
import dosumis.brainscowl.BrainScowl
import scala.collection.JavaConversions._
import dosumis.brainscowl.obo_style._
import org.semanticweb.owlapi.model._
import scala.collection.mutable.ListBuffer
import org.phenoscape.scowl._ 


class definition_writer(ont: BrainScowl, fbbt: BrainScowl) {

  // Something to find all individuals in signature

  def add_defs() {
    val defintion = AnnotationProperty("http://purl.obolibrary.org/obo/IAO_")
    val inds = this.ont.ontology.getIndividualsInSignature()
    for (i <- inds) {
      val sf = ont.bi_sfp.getShortForm(i)
      println(s"**** Defining $sf.")
      val ax = ont.add_axiom(i Annotation (defintion, roll_def(sf)))
    } 
  }
  
  

  def roll_def(ind_short_form: String): String = {
    val typs = ont.getTypes(sfid = ind_short_form)   
    var genus = "" // Generic typing
    var	spec_genus = "" // Specific typing for use in def.
    var	po = ""
    var exp = ""
    var gender = ""
    for (typ <- typs) {
       if (!typ.isAnonymous) {        
         val claz = ont.bi_sfp.getShortForm(typ.asOWLClass)
          if (claz == "FBbt_00005106" || ont.getSuperClasses(claz).keys.contains("FBbt_00005106")) {
            var genus = "neuron"
   				  var spec_genus = claz              
          }
          if (claz == "CARO_0030002" || ont.getSuperClasses(claz).keys.contains("CARO_0030002")) {
            var genus = "expression pattern"
   				  var spec_genus = claz           
          }
          if (claz == "FBbt_00007683" || ont.getSuperClasses(claz).keys.contains("FBbt_00007683")) {
            var genus = "neuroblast lineage clone"
   				  var spec_genus = claz           
          }
       } else {         
         val rels = typ.getObjectPropertiesInSignature()
         val classes = typ.getClassesInSignature()
         val rel = if (rels.toArray.length == 1) {
           ont.bi_sfp.getShortForm(rels.last).toString()
         } else {
           ""
         }
         val claz = if (classes.toArray.length == 1) {
           ont.bi_sfp.getShortForm(classes.last).toString()
         } else {
           ""
         }
         val po = if ((rel == "BFO_0000050") && (claz == "FBbt_00003624")) {
           "adult brain"
         }
         val gender = if ((rel ==  "BFO_0000050") && (claz == "FBbt_00007004")) {
           "M"
         } else if ((rel ==  "BFO_0000050") && (claz == "FBbt_00007011"))  {
           "F"
         }
         val exp = if (rel ==  "RO_0002292") {
          "fu" // Need to look up label expressed thingy
         }         
       }
    }     
     var def_comps = ListBuffer[String]()
     if (genus == "neuron") {
       if (!spec_genus.isEmpty()) {
         def_comps(0) = s"A $spec_genus"
       } else {
  			 def_comps(0) = s"A $genus"
       }
       if (!exp.isEmpty()) {
  			  def_comps(1) = s"expressing $exp"
       }
       if (!po.isEmpty()) {
  			  def_comps(2) = s"that is part of an $po"
       }
     }
     if (genus == "expression pattern") {
       if (!po.isEmpty() && !exp.isEmpty()) {
         def_comps(0) = s"An $po"
       }
     }
     if (genus == "neuroblast lineage clone") {
       if (!spec_genus.isEmpty()) {
    			def_comps(0) = s"An example of a(n) $spec_genus"
       }
       if (!po.isEmpty()) {
          def_comps(0) = "that is part of a(n) $po"
       }
     }
    val def_pre = def_comps.mkString("")
   return def_pre.stripSuffix(" ") + "."
  }
  //		if spec_genus:
  //			def_comps[0] = "A %s" % spec_genus
  //		else:
  //			def_comps[0] = "A %s" % genus
  //		if exp:
  //			def_comps[1] = 'expressing %s' % exp
  //		if po:
  //			def_comps[2] = 'that is part of an %s' % po
  //	elif genus == 'expression pattern':
  //		if po and exp:
  //			def_comps[0] = "An %s" % po
  //			def_comps[1] = "expressing %s" % exp
  //	elif genus == 'neuroblast lineage clone':
  //		if spec_genus:
  //			def_comps[0] = "An example of a(n) %s" % spec_genus
  //		if po:
  //			def_comps[1] = "that is part of a(n) %s" % spec_genus			
  //	def_pre = ' '.join(def_comps)
  //	defn = def_pre.strip()  + '.'
  //	return defn           
  //       }
  //     }
  //  }


  //  // Initialise with an FBbt brainscowl object to use for looking up classifications
  //  // But note - this will require extensions to brainscowl to bring in queries.
  //  
  //  // Might be better as a function than as a class + main method...

  //  extensions to BrainScowl needed:

  // ont.getTypes - From 
  // ont.isSuperClass  - From
  // 

  /// Existing Python code  
  //  feat_ont = ont_dict['fb_feature']
  //	fbbt = ont_dict['fbbt']
  //	genus = '' # Generic typing
  //	spec_genus = '' # Specific typing for use in def.
  //	po = ''
  //	exp = ''
  //	gender = ''
  //	for typ in types:
  //		#print "Generating %s at %s" % (typ, time.gmtime())
  //		sce = simpleClassExpression(typ)
  //		if not typ.isAnonymous():
  //			parent_class = sce.get_class_sfid()
  //			if fbbt.isSuperClass('FBbt_00005106', parent_class, 0):
  //				genus = 'neuron'
  //				spec_genus = fbbt.getLabel(parent_class)
  //			if (parent_class == 'FBbt_00005106'): # neuron
  //				genus = 'neuron'
  //				#			if parent_class == 'FBbt_00003624': # adult brain - hack for EP! change back once fixed on site!!!!!!!
  //			if (parent_class == 'CARO_0030002'):
  //				genus = 'expression pattern' 
  //					# po = 'adult brain' # hack for EP! change back once fixed on site!!!!!!!
  //			if fbbt.isSuperClass('FBbt_00007683', parent_class, 0) or (parent_class == 'FBbt_00007683') : # neuroblast lineage clone
  //				genus = 'neuroblast lineage clone'
  //				spec_genus = fbbt.getLabel(parent_class)
  //		else:
  //			rel = sce.get_rel_sfid()
  //			object_class = sce.get_class_sfid()
  //			if (rel == 'BFO_0000050') & ( object_class== 'FBbt_00003624'): # part of adult brain // Use pattern match?
  //				po = 'adult brain'
  //				if (rel == 'BFO_0000050') & (object_class == 'FBbt_00007004'): # part male organism
  //					gender = 'M'
  //			if (rel == 'BFO_0000050') & (object_class == 'FBbt_00007011'): # part female organism
  //					gender = 'F'
  //			if (rel == 'RO_0002292'): # expresses  X
  //				if feat_ont.knowsClass(object_class):				
  //					exp = feat_ont.getLabel(object_class)
  //				else: 
  //					warnings.warn("%s is not a valid class in fb_features.owl. Not rolling def." % object_class) # Requires declaration of expression pattern class
  //				continue
  //	if gender == 'M':
  //		po = 'adult male brain'
  //	if gender == 'F':
  //		po = 'adult female brain'
  //	def_comps = ['', '', '']
  //	if genus == 'neuron':
  //		if spec_genus:
  //			def_comps[0] = "A %s" % spec_genus
  //		else:
  //			def_comps[0] = "A %s" % genus
  //		if exp:
  //			def_comps[1] = 'expressing %s' % exp
  //		if po:
  //			def_comps[2] = 'that is part of an %s' % po
  //	elif genus == 'expression pattern':
  //		if po and exp:
  //			def_comps[0] = "An %s" % po
  //			def_comps[1] = "expressing %s" % exp
  //	elif genus == 'neuroblast lineage clone':
  //		if spec_genus:
  //			def_comps[0] = "An example of a(n) %s" % spec_genus
  //		if po:
  //			def_comps[1] = "that is part of a(n) %s" % spec_genus			
  //	def_pre = ' '.join(def_comps)
  //	defn = def_pre.strip()  + '.'
  //	return defn
  //  

}

