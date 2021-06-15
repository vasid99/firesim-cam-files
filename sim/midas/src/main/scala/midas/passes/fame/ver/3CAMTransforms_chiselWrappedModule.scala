package midas.passes.fame

import firrtl._
import Mappers._
import traversals.Foreachers._
import ir._
import annotations._
import collection.mutable.ArrayBuffer
import midas.targetutils.{FirrtlCamModelAnnotation, FirrtlFAMEModelAnnotation}


class LabelCAMModels extends Transform{
  def inputForm = HighForm
  def outputForm = HighForm
  
  override def execute(state: CircuitState): CircuitState = {
    val memModelAnnos = new ArrayBuffer[Annotation]
    val annotatedMems = state.annotations.collect({
      case FirrtlCamModelAnnotation(it) => it
    }).toSet

    println(s"[MIDAS 2.0] CAM Models To Extract: ${annotatedMems.size}")
    state.circuit.modules.foreach({
      case m: Module =>
        def onStmt(stmt: Statement): Statement = stmt.map(onStmt) match {
          case s: DefInstance =>
            val it = ModuleTarget(state.circuit.main, m.name).instOf(s.name, s.module)
            if(annotatedMems.contains(it)) {
              val wrapperMod = state.circuit.modules.find(m => m.name==it.ofModule).get
              val itInner = wrapperMod match {
                case m: Module => m.body match {
                  case blk: Block => blk.stmts.collectFirst({
                    case s: DefInstance => ModuleTarget(state.circuit.main, wrapperMod.name).instOf(s.name, s.module)
                  }).get
                }
              }
              val wrappedMod = state.circuit.modules.find(m => m.name==itInner.ofModule).get
              memModelAnnos ++= wrappedMod.ports.map(p=>)
              memModelAnnos ++= Seq( FirrtlFAMEModelAnnotation(it) , FirrtlFAMEModelAnnotation(itInner) )
            }
            s
          case s=>s
        }
        m.body foreach onStmt
      case m =>
    })
    state.copy(annotations = state.annotations ++ memModelAnnos)
  }
}

class EmitAndWrapCAMModels extends Transform{
  def inputForm = LowForm
  def outputForm = HighForm
  
  override def execute(state: CircuitState): CircuitState = {
    //println("Emit started")
    val circ = state.circuit
    val ns = Namespace(circ)
    val addedModules = new ArrayBuffer[Module]
    val annotatedMems = state.annotations.collect({
      case FirrtlCamModelAnnotation(it) => ModuleTarget(circ.main, it.instance)
    }).toSet
    
    val transformedModules = circ.modules.map({
      case m: Module if annotatedMems.contains(ModuleTarget(circ.main, m.name))=>println("Found one:\n"+m+"\n");m
      case m=>m
    })
    
    state.copy(circuit = circ.copy(modules = transformedModules ++ addedModules))
  }
}
