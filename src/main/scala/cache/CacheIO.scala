package cache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

// addressWidth and dataWidth are unchanged, so using Cache1Config is reasonable
// CPU request Cache
class Request extends Bundle with Cache1Config {
  val address = UInt(addressWidth.W)
  val writeEnable = Bool()
  val writeData = UInt(dataWidth.W)
}

class Response extends Bundle with Cache1Config {
  val readData = UInt(dataWidth.W)
}

class CacheIO extends Bundle with Cache1Config {
  // Decoupled: from master device
  // Flipped: flip input & output
  val request = Flipped(Decoupled(new Request))
  val response = Decoupled(new Response)

  val numHits = Output(UInt(32.W))
  val numCycles = Output(UInt(32.W))
  
  override def cloneType =
    new CacheIO().asInstanceOf[this.type]
}