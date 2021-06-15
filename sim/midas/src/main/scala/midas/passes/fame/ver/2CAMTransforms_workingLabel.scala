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
    val addedModules = new ArrayBuffer[Module]
    val annotatedMems = state.annotations.collect({
      case FirrtlCamModelAnnotation(it) => it
    }).toSet

    println(s"[MIDAS 2.0] CAM Models To Extract: ${annotatedMems.size}")
    val transformedModules = circ.modules.map({
      case m: Module =>
        val mt = ModuleTarget(circ.main, m.name)
        def onStmt(stmt: Statement): Statement = stmt.map(onStmt) match{
          case camInst: DefInstance if annotatedMems.contains(mt.instOf(camInst.name, camInst.module)) =>
            val camMod = circ.modules.find({case m: Module => m.name==camInst.module;case m=>false}).get
            val interconnects = camMod.ports.map(p => if(p.direction==Input) PartialConnect(NoInfo, WSubField(WRef(camInst.name), p.name), WRef(p.name))
                                                                        else PartialConnect(NoInfo, WRef(p.name), WSubField(WRef(camInst.name), p.name)))
            val wrapperMod = Module(camInst.info, camInst.name, camMod.ports, Block(camInst.copy(name = moduleNS.newName(camInst.name)) +: interconnects))
            addedModules += wrapperMod
            memModelAnnos += FirrtlFAMEModelAnnotation(mt.instOf(camInst.name, wrapperMod.name))
            memModelAnnos ++= wrapperMod.ports.map(p => CamPortAnnotation(mt.ref(p.name)))
            WDefInstance(camInst.info, camInst.name, wrapperMod.name, UnknownType)
          case s=>s
        }
        m.copy(body=m.body.map(onStmt))
      case m=>m
    })
    val transformedCircuit = circ.copy(modules = transformedModules ++ addedModules)
    state.copy(circuit = transformedCircuit, annotations = state.annotations ++ memModelAnnos)
  }
}

class EmitAndWrapCAMModels extends Transform{
  def inputForm = LowForm
  def outputForm = HighForm
  
  override def execute(state: CircuitState): CircuitState = {
    println("Emit started")
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

/* Do the following:
 * 1. Wrap the instance in a module (ports are still an issue though; deal using individual connect clauses)
 * 2. Add wrapper to module list and port annotations to annolist...idk though, ports are still an issue. Manual Port() declarations?
 * 3. Return WDefInstance as output to mapper
 */
