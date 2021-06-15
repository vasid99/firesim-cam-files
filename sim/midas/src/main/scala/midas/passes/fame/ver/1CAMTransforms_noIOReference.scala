package midas.passes.fame

import firrtl._
import Mappers._
import ir._
import annotations._
import collection.mutable.ArrayBuffer
import midas.targetutils.{FirrtlCamModelAnnotation, FirrtlFAMEModelAnnotation}


class LabelCAMModels extends Transform{
  def inputForm = HighForm
  def outputForm = HighForm
  
  override def execute(state: CircuitState): CircuitState = {
    val circ = state.circuit
    val moduleNS = Namespace(circ)
    val memModelAnnos = new ArrayBuffer[Annotation]
    val memModules = new ArrayBuffer[Module]
    val annotatedMems = state.annotations.collect({
      case FirrtlCamModelAnnotation(rt) => rt
    }).toSet

    println(s"[MIDAS 2.0] CAM Models To Extract: ${annotatedMems.size}")
    state.annotations.foreach(println)
    val transformedModules = circ.modules.map({
      case m:Module =>
        val mt = ModuleTarget(circ.main, m.name)
        println("Module target created")
        def onStmt(stmt:Statement):Statement = stmt.map(onStmt) match{
          case cam:DefInstance if annotatedMems.contains(mt.instOf(cam.name, cam.module)) =>
            println("Found CAM")
            val connects = Seq("io").map(p => PartialConnect(NoInfo, WSubField(WRef(cam.name), p), WRef(p))) //i think they are inside the module, not outside
            println("connects created")
            val modPorts = circ.modules.find({case m=>m.name==cam.module}).get.ports
            println("modPorts created")
            val wrapper = Module(cam.info, cam.name, modPorts, Block(cam +: connects)).copy(name=moduleNS.newName(cam.name))
            println("wrapper created")
            val wrapperTarget = ModuleTarget(circ.main, wrapper.name)
            println("wrapperTarget created")
            memModules += wrapper
            println("wrapper added")
            memModelAnnos += FirrtlFAMEModelAnnotation(mt.instOf(cam.name, wrapper.name))
            println("memModelAnnos appended [1]")
            //memModelAnnos ++= // halp plis
            WDefInstance(cam.info, cam.name, wrapper.name, UnknownType)
           case s=>s
        }
        println("now running onStmt")
        m.copy(body=m.body.map(onStmt))
      case m=>m
    })
    println("transformedModules created")
    val transformedCircuit = circ.copy(modules = memModules ++ transformedModules)
    println("transformedCircuit created")
    state.copy(circuit = transformedCircuit, annotations = state.annotations ++ memModelAnnos)
  }
}

/* Do the following:
 * 1. Wrap the instance in a module (ports are still an issue though; deal using individual connect clauses)
 * 2. Add wrapper to module list and port annotations to annolist...idk though, ports are still an issue. Manual Port() declarations?
 * 3. Return WDefInstance as output to mapper
 */
 
/*
[MIDAS 2.0] CAM Models To Extract: 1
Module target created
now running onStmt
Module target created
now running onStmt
Module target created
now running onStmt
Found CAM
connects created
modPorts created
wrapper created
wrapperTarget created
wrapper added
memModelAnnos appended [1]
Module target created
now running onStmt
transformedModules created
transformedCircuit created
(run-main-0) firrtl.CustomTransformException:
Caused by: firrtl.passes.CheckHighFormLike$UndeclaredReferenceException:  @[CAM.scala 208:61]: [module cam] Reference io is not declared.
*/
