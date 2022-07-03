package cache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import common.CurrentCycle

class Memory(width: Int, depth: Int) extends Module {
    val io = IO(new MemoryIO(width, depth))

    // Register Vector
    // Multiple (depth) registers
    // Each register's width = width.W, init value = 0.U
    val mem = RegInit(VecInit(Seq.fill(depth)(0.U(width.W))))

    // cycle n: send io address
    // cycle n+1: get read data
    io.readData := RegNext(mem(io.address % depth.U))

    when(io.writeEnable) {
        mem(io.address % depth.U) := io.writeData
    }
}