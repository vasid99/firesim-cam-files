package midas.passes.fame

import firrtl._
import Mappers._
import traversals.Foreachers._
import ir._
import annotations._
import collection.mutable.ArrayBuffer
import midas.targetutils.{FirrtlCamModelAnnotation, FirrtlFAMEModelAnnotation}

import collection.mutable

class LabelCAMModels extends Transform{
  def inputForm = HighForm
  def outputForm = HighForm
  
  override def execute(state: CircuitState): CircuitState = {
    val circ = state.circuit
    val memModelAnnos = new ArrayBuffer[Annotation]
    val annotatedCams = state.annotations.collect({
      case FirrtlCamModelAnnotation(it) => it
    }).toSet
    def getModByName(n: String): DefModule = circ.modules.find(m => m.name==n).get
    
    println(s"[MIDAS 2.1] CAM Models To Extract: ${annotatedCams.size}")
    
    circ.modules.foreach({
      case m: Module =>
        val mtParent = ModuleTarget(circ.main, m.name)
        def onStmt(stmt: Statement): Statement = stmt.map(onStmt) match {
          case s: DefInstance if annotatedCams.contains(mtParent.instOf(s.name, s.module)) =>
            val itCam = mtParent.instOf(s.name, s.module)
            val modCam = getModByName(itCam.ofModule)
            val mtCam = itCam.ofModuleTarget
            memModelAnnos += FirrtlFAMEModelAnnotation(itCam)
            memModelAnnos ++= modCam.ports.map( p => CamPortAnnotation( mtCam.ref(p.name) ) )
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
    val circ = state.circuit
    val ns = Namespace(circ)
    val addedModules = new ArrayBuffer[Module]
    def getModByName(n: String): DefModule = circ.modules.find(m => m.name==n).get
    
    val modInsts = new mutable.LinkedHashMap[ModuleTarget,InstanceTarget]
    val annotatedCams = state.annotations.collect({
      case FirrtlCamModelAnnotation(it) =>
        modInsts += (it.ofModuleTarget -> it)
        it
    }).toSet
    
    val instancePorts = new mutable.LinkedHashMap[InstanceTarget, mutable.LinkedHashMap[String,ReferenceTarget]]
    state.annotations.collect({
      case anno: CamPortAnnotation =>
        val it = modInsts(anno.rt.moduleTarget) 
        if(!instancePorts.contains(it)) instancePorts(it) = new mutable.LinkedHashMap[String,ReferenceTarget]
        instancePorts(it) += (anno.labelName -> anno.rt.noComponents)
    })
    
    val transformedModules = circ.modules.map({
      case m: Module=>
        val mtParent = ModuleTarget(circ.main, m.name)
        def onStmt(stmt: Statement): Statement = stmt.map(onStmt) match {
          case s: DefInstance if annotatedCams.contains(mtParent.instOf(s.name, s.module)) =>
            val itCam = mtParent.instOf(s.name, s.module)
            val modCam = getModByName(itCam.ofModule)
            val mtCam = itCam.ofModuleTarget
            val itCamPorts = instancePorts(itCam)
            println("Targets created")
            
            val tagWidth = (
              getWidth(
                modCam.ports.find(
                  p => p.name==itCamPorts("io_in_tag").ref
                ).get.tpe match {
                  case b:BundleType => b.fields.collectFirst(
                    {case f:Field if f.name=="bits"=> f.tpe}
                  ).get
                }
              ) match {case IntWidth(num)=>num}
            ).toInt
            val datarr = modCam match {
              case m: Module => m.body match {
                case blk:Block => blk.stmts.collectFirst({
                  case s: DefMemory=>s
                }).get
              }
            }
            val depth = (datarr.depth).toInt
            val dataWidth = (getWidth(datarr.dataType) match {case IntWidth(num)=>num}).toInt
            println("Widths extracted")
            
            val c3circuit = chisel3.Driver.elaborate(() => new midas.models.cam.CamModel(depth, tagWidth, dataWidth))
            val chirrtl = firrtl.Parser.parse(chisel3.Driver.emit(c3circuit))
            val state = new firrtl.MiddleFirrtlCompiler().compile(firrtl.CircuitState(chirrtl, firrtl.ChirrtlForm, Nil), Nil)
            require(state.circuit.modules.length==1)
            val camModel = state.circuit.modules.collectFirst({
              case m: Module => m.copy(name=modCam.name+"_Model")
            }).get
            println("Elaborated model obtained")
            
            val camModelInst = WDefInstance(NoInfo, itCam.instance, camModel.name, UnknownType)
            val resetConnects = Seq(
              Connect(NoInfo, WSubField(WRef(itCamPorts("reset").ref), "ready"), WSubField(WSubField(WSubField(WSubField(WRef(camModelInst.name), "io"), "in"), "tag"), "ready"))
            )
            val interconnects = Seq(
              Connect(NoInfo, WSubField(WRef(camModelInst.name), "clock"), WRef("hostClock")),
              Connect(NoInfo, WSubField(WRef(camModelInst.name), "reset"), WRef("hostReset")),
              Connect(NoInfo, WSubField(WSubField(WSubField(WRef(camModelInst.name), "io"), "in"), "tag"), WRef(itCamPorts("io_in_tag").ref)),
              Connect(NoInfo, WSubField(WSubField(WSubField(WRef(camModelInst.name), "io"), "in"), "wrData"), WRef(itCamPorts("io_in_wrData").ref)),
              Connect(NoInfo, WSubField(WSubField(WSubField(WRef(camModelInst.name), "io"), "in"), "en"), WRef(itCamPorts("io_in_en").ref)),
              Connect(NoInfo, WSubField(WSubField(WSubField(WRef(camModelInst.name), "io"), "in"), "wr"), WRef(itCamPorts("io_in_wr").ref)),
              Connect(NoInfo, WRef(itCamPorts("io_out_rdData").ref), WSubField(WSubField(WSubField(WRef(camModelInst.name), "io"), "out"), "rdData")),
              Connect(NoInfo, WRef(itCamPorts("io_out_hit").ref), WSubField(WSubField(WSubField(WRef(camModelInst.name), "io"), "out"), "hit"))
            ) ++ resetConnects
            val camModelWrapper = Module(modCam.info, modCam.name+"_ModelWrapper", modCam.ports, Block(camModelInst+:interconnects))
            println("Wrapper module created")
            
            addedModules ++= Seq(camModel, camModelWrapper)
            WDefInstance(s.info, itCam.instance, camModelWrapper.name, UnknownType)
          case s=>s
        }
        m.copy(body=m.body.map(onStmt))
      case m=>m
    })
    
    state.copy(circuit = circ.copy(modules = transformedModules ++ addedModules))
  }
}

/* wrapped modules
            println("Instance found")
            // wrapper module around CAM RTL
            val itWrapper = mtParent.instOf(s.name, s.module)
            val modWrapper = getModByName(itWrapper.ofModule)
            val mtWrapper = ModuleTarget(circ.main, modWrapper.name)
            println("Wrapper information extracted")
            
            // wrapped CAM RTL module
            val itWrapped = modWrapper match {
                              case m: Module => m.body match {
                                case blk: Block => blk.stmts.collectFirst({
                                  case i: DefInstance => mtWrapper.instOf(i.name, i.module)
                                }).get
                              }
                            }
            val modWrapped = getModByName(itWrapped.ofModule)
            val mtWrapped = ModuleTarget(circ.main, modWrapped.name)
            println("Wrapped information extracted")
            
            // add FAME annotations
            memModelAnnos ++= Seq( FirrtlFAMEModelAnnotation(itWrapper) , FirrtlFAMEModelAnnotation(itWrapped) )
            println("FAME annotations added")
            // assign port annotations
            memModelAnnos ++= modWrapper.ports.map(p => CamPortAnnotation( mtWrapper.ref(p.name) , mtWrapped.ref(p.name) ))
            println("Port annotations added")*/
