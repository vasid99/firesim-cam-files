package midas.models.cam

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._

class CamRTLIO(tagWidth: Int, dataWidth: Int) extends Bundle{
  val in = Input(new Bundle{
    val tag = UInt(tagWidth.W)
    val wrData = UInt(dataWidth.W)
    val en = Bool()
    val wr = Bool()
  })
  val out = Output(new Bundle{
    val rdData = UInt(dataWidth.W)
    val hit = Bool()
  })
  
  override def cloneType = (new CamRTLIO(tagWidth, dataWidth)).asInstanceOf[this.type]
}

class CamRTL(depth: Int, tagWidth: Int, dataWidth: Int) extends Module{ /// TODO MultiIOModule? Maybe only for multiple channels...?
  val io = IO(new CamRTLIO(tagWidth, dataWidth))
  
  // tag and data arrays
  val tags = Reg(Vec(depth, UInt(tagWidth.W)))//Mem(depth, UInt(tagWidth.W))
  val data = Mem(depth, UInt(dataWidth.W))
  
  // hit processing
  val tagMatch = tags map { case t => t === io.in.tag }
  val hitIdx = OHToUInt(tagMatch)
  val hitDetected = tagMatch.orR
  
  // (write) miss processing
  val rpolicy = new SetAssocLRU(1, depth, "plru")// TODO accept as constructor arg
  val setIndex = WireDefault(0.U)
  val replIdx = rpolicy.way(setIndex)
  
  // data array port
  val memaddr = Mux(hitDetected, hitIdx, replIdx)
  val dataport = data(memaddr)
  
  // CAM operations
  when(io.in.en){
    // update replcement policy for hit
    when(hitDetected){ rpolicy.access(setIndex, hitIdx) }
    
    // perform read/write
    when(io.in.wr){
      dataport := io.in.wrData
      tags(memaddr) := io.in.tag
      io.out.rdData := DontCare
    }.otherwise{
      io.out.rdData := dataport
    }
    
    // output hit/miss status
    io.out.hit := hitDetected
  }.otherwise{
    // assign all outputs to DontCare
    io.out.rdData := DontCare
    io.out.hit := DontCare
  }
  
  def read(tag: UInt): UInt = {
    io.in.en := true.B
    io.in.wr := false.B
    io.in.tag := tag // doesn't make separate port every time => can cause issues when read function used more than once in an instance. That's why you use it only once
    io.in.wrData := DontCare
    io.out.rdData
  }
  
  def write(tag: UInt, data: UInt): Unit = {
    io.in.en := true.B
    io.in.wr := true.B
    io.in.tag := tag
    io.in.wrData := data
  }
  
  def hit: Bool = io.out.hit
}

class CamModelIO(tagWidth: Int, dataWidth: Int) extends Bundle{
  val in = new Bundle{
    val tag = Flipped(Decoupled(UInt(tagWidth.W)))
    val wrData = Flipped(Decoupled(UInt(dataWidth.W)))
    val en = Flipped(Decoupled(Bool()))
    val wr = Flipped(Decoupled(Bool()))
  }
  val out = new Bundle{
    val rdData = Decoupled(UInt(dataWidth.W))
    val hit = Decoupled(Bool())
  }
  
  override def cloneType = (new CamModelIO(tagWidth, dataWidth)).asInstanceOf[this.type]
}

class CamModel(depth: Int, tagWidth: Int, dataWidth: Int) extends Module{
  val io = IO(new CamModelIO(tagWidth, dataWidth))
  
  // state variables
  lazy val s_wait :: s_searching :: s_searchdone :: s_memwait :: s_done :: Nil = Enum(5)
  val state = RegInit(s_wait)
  val nextstate = WireDefault(state)
  
  // registers to capture inputs
  val inputs = Reg(new Bundle{
    val tag = UInt(tagWidth.W)
    val wrData = UInt(dataWidth.W)
    val en = Bool()
    val wr = Bool()
  })
  
  // tag search
  val tagCtr = Reg(UInt(log2Ceil(depth).W))
  tagCtr := Mux(state===s_searching, tagCtr+1.U, 0.U)
  val hitDetected = WireDefault(false.B)
  val hitIdx = RegEnable(tagCtr, hitDetected)
  val hit = Reg(Bool())
  
  // memory arrays
  val tags = SyncReadMem(depth, UInt(tagWidth.W))
  val data = SyncReadMem(depth, UInt(dataWidth.W))
  
  // memory ports
  val memaddr = Wire(UInt(log2Ceil(depth).W))
  val tagport = tags(memaddr)
  val dataport = data(memaddr)
  val rdData = Reg(UInt(dataWidth.W))
  val memactive = state===s_memwait
  
  // replacement policy
  val rpolicy = new SetAssocLRU(1, depth, "plru")
  val setIndex = WireDefault(0.U)
  val replIdx = rpolicy.way(setIndex)
  
  // input ready-valid
  val iReady = Wire(Bool())
  io.in.tag.ready    := iReady
  io.in.wrData.ready := iReady
  io.in.en.ready     := iReady
  io.in.wr.ready     := iReady
  val iValid = io.in.tag.valid && io.in.wrData.valid && io.in.en.valid && io.in.wr.valid
  //io.in.getElements.foreach({case sig => sig.ready := iReady})
  //val iValid = (io.in.getElements foldLeft WireDefault(true.B))({case (inp, value) => value && inp.valid})
  def iFire = iValid && iReady
  
  // output ready-valid
  val oReady = io.out.rdData.ready && io.out.hit.ready
  val oValid = Wire(Bool())
  io.out.rdData.valid := oValid
  io.out.hit.valid    := oValid
  def oFire = oValid && oReady
  
  // input capture
  when(iFire){
    inputs.tag    := io.in.tag.bits
    inputs.wrData := io.in.wrData.bits
    inputs.en     := io.in.en.bits
    inputs.wr     := io.in.wr.bits
  }

  // hit-bit register update
  when(state===s_wait){
    hit := false.B
  }.elsewhen(state===s_searching||state===s_searchdone){
    hit := hitDetected
  }

  // memory address assignment
  when(state===s_searching){
    memaddr := tagCtr
  }.elsewhen(state===s_memwait){
    when(hit){
      memaddr := hitIdx
    }.otherwise{
      when(inputs.wr){
        memaddr := replIdx
      }.otherwise{
        memaddr := DontCare
      }
    }
  }.otherwise{
    memaddr := DontCare
  }

  // data operations
  when(memactive){
    when(inputs.wr){
      dataport := inputs.wrData
      tagport := inputs.tag
      /// TODO replacement policy update?
    }.otherwise{
      rdData := Mux(hit, data.read(memaddr), DontCare)
    }
    when(hit){
      rpolicy.access(setIndex, hitIdx)
    }
  }

  // assigning outputs
  iReady := state===s_wait
  
  io.out.rdData.bits := rdData
  io.out.hit.bits := hit
  oValid := state===s_done

  // state update
  when(state===s_wait){
    when(iFire && io.in.en.bits){
      nextstate := s_searching /// TODO or should to go to s_done?
    }
  }.elsewhen(state===s_searching){
    when(tagport===inputs.tag){
      nextstate := s_memwait
      hitDetected := true.B
    }.elsewhen(tagCtr===(depth-1).U){
      nextstate := s_searchdone
    }
  }.elsewhen(state===s_searchdone){
    nextstate := s_memwait
    hitDetected := tagport===inputs.tag
  }.elsewhen(state===s_memwait){
    when(memactive){
      nextstate := s_done
    }
  }.elsewhen(state===s_done){
    when(oFire){
      nextstate := s_wait
    }
  }
  state := nextstate
}

object Cam {
  def apply(depth: Int, tagWidth: Int, dataWidth: Int) = Module(new CamRTL(depth, tagWidth, dataWidth))
}
