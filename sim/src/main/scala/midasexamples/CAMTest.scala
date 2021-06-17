package firesim.midasexamples

import chisel3._
import chisel3.experimental.annotate

import freechips.rocketchip.config.Parameters

import midas.widgets.PeekPokeBridge
import midas.targetutils._

import midas.models.cam._

class CAMTestDUT(depth:Int, tagWidth:Int, dataWidth:Int) extends Module{
  val io = IO(new CamRTLIO(tagWidth, dataWidth))
  val cam = Cam(depth, tagWidth, dataWidth)
  annotate(CamModelAnnotation(cam))
  
	io.out.rdData := DontCare
	io.out.hit := DontCare
	
  when(io.in.en){
		when(io.in.wr){
			cam.write(io.in.tag, io.in.wrData)
		}.otherwise{
			io.out.rdData := cam.read(io.in.tag)
		}
		io.out.hit := cam.hit
  }
}

class CAMTest(implicit p: Parameters) extends PeekPokeMidasExampleHarness(() => new CAMTestDUT(16,8,32))
