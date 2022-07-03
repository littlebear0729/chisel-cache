package cache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

// width: capacity of units
// depth: number of units
class MemoryIO(width: Int, depth: Int) extends Bundle {
    val writeEnable = Input(Bool()) // true for write, false for read
    val address = Input(UInt(log2Ceil(depth).W)) // address, calculating the bits
    // log2Ceil: log, rounded to ceiling
    val writeData  = Input(UInt(width.W)) // the data needs to write
    val readData = Output(UInt(width.W)) // the read data
  
    override def cloneType = new MemoryIO(width, depth).asInstanceOf[this.type]
}