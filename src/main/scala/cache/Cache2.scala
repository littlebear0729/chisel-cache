package cache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import common.CurrentCycle

// matadata for each cache block
class Meta2 extends Bundle with Cache2Config {
  // cache block data valid
  val valid = Bool()
  // dirty: different data between cache and memory
  // check valid and dirty before data replace
  val dirty = Bool()
  val address = UInt(addressWidth.W)
  val tag = UInt(tagBits.W)
  val cycle = UInt(32.W)
}

class Cache2 extends Module with Cache2Config with CurrentCycle {
  scala.Predef.printf(s"indexBits: ${indexBits}, offsetBits: ${offsetBits}\n")

  // assert(assoc == 1)

  val io = IO(new CacheIO)

  // one set, one cache block
  // width: blockSizeInBytes * 8 bits
  val dataArray = RegInit(
    VecInit(Seq.fill(assoc)(0.U((blockSizeInBytes * 8).W)))
  )
  val metaArray = RegInit(
    VecInit(
      Seq.fill(assoc)(
        {
          val meta = Wire(new Meta2())
          meta.valid := false.B
          meta.dirty := false.B
          meta.address := 0.U
          meta.tag := 0.U
          meta.cycle := 0.U
          meta
        }
      )
    )
  )
  val currentBlock = RegInit(0.U)

  // Idle: Get any request (both read & write)
  // ReadMiss: No required data in cache, fetch data from memory
  // ReadData: Cache hit, or data fetched from memory
  // WriteResponse:
  val sIdle :: sReadMiss :: sReadData :: sWriteResponse :: Nil = Enum(4)

  val regState = RegInit(sIdle)

  io.request.ready := false.B

  io.response.valid := false.B
  io.response.bits := DontCare

  val address = io.request.bits.address

  val tag = getTag(address)
  val index = 0

  // HIT cache!
  // ??Problem
  var exist = false.B
  for (i <- 0 until assoc) {
    exist = exist || (metaArray(i).valid && metaArray(i).tag === tag)
  }
  val hit = regState === sIdle && io.request.fire() && exist

  // Calculate hit rate
  val regNumHits = RegInit(0.U(32.W))

  io.numHits := regNumHits

  io.numCycles := currentCycle

  val addressReg = Reg(UInt(addressWidth.W))
  val tagReg = getTag(addressReg)
  val indexReg = 0

  val memory = Module(new Memory(dataWidth, 256))

  memory.io.writeEnable := false.B
  memory.io.address := DontCare
  memory.io.writeData := DontCare

  def writeback() {
    memory.io.writeEnable := true.B
    memory.io.address := metaArray(index).address
    memory.io.writeData := dataArray(index)
  }

  def refill() {
    memory.io.writeEnable := false.B
    memory.io.address := io.request.bits.address
  }

  def findPos(): UInt = {
    for (i <- 0 until assoc) {
      if (addressReg == metaArray(i).address && tagReg == metaArray(i).tag) {
        return i.U
      }
    }
  }

  def findEmptyPos(): UInt = {
    for (i <- 0 until assoc) {
      if (metaArray(i).cycle == 0.U) {
        return i.U
      }
    }
  }

  switch(regState) {
    is(sIdle) {
      io.request.ready := true.B

      when(io.request.fire()) {
        addressReg := io.request.bits.address

        // Read or Write?
        when(io.request.bits.writeEnable) {
          when(hit) {
            // HIT
            regNumHits := regNumHits + 1.U
            dataArray(findPos()) := io.request.bits.writeData

            regState := sWriteResponse
          }.otherwise {
            // MISS
            when(metaArray(index).valid && metaArray(index).dirty) {
              writeback()
            }

            var idx = findEmptyPos()
            metaArray(idx).valid := true.B
            metaArray(idx).dirty := true.B
            metaArray(idx).tag := tag
            metaArray(idx).address := address
            dataArray(idx) := io.request.bits.writeData

            regState := sWriteResponse
          }
        }.otherwise {
          when(hit) {
            regNumHits := regNumHits + 1.U

            regState := sReadData
          }.otherwise {
            when(metaArray(index).valid && metaArray(index).dirty) {
              writeback()
            }

            refill()

            regState := sReadMiss
          }
        }
      }
    }
    is(sReadMiss) {
      metaArray(indexReg).valid := true.B
      metaArray(indexReg).dirty := false.B
      metaArray(indexReg).tag := tagReg
      metaArray(indexReg).address := addressReg
      dataArray(indexReg) := memory.io.readData

      regState := sReadData
    }
    is(sReadData) {
      io.response.valid := true.B
      io.response.bits.readData := dataArray(indexReg)

      when(io.response.fire()) {
        regState := sIdle
      }
    }
    is(sWriteResponse) {
      io.response.valid := true.B

      when(io.response.fire()) {
        regState := sIdle
      }
    }
  }

  chisel3.printf(
    p"[${currentCycle}] regState: ${regState}, request.fire(): ${io.request
      .fire()}, response.fire(): ${io.response
      .fire()}, writeEnable: ${io.request.bits.writeEnable}, address: ${io.request.bits.address}, tag: ${tag}, index: ${index}, hit: ${hit}, regNumHits: ${regNumHits}, currentBlock: ${currentBlock}\n"
  )
  // chisel3.printf(
  //   p"[${currentCycle}] regState: ${regState}, data: ${io.response.bits.readData}, address: ${io.request.bits.address}, tag: ${tag}, index: ${index}, hit: ${hit}, regNumHits: ${regNumHits}\n"
  // )
}

object Cache2 extends App {
  (new ChiselStage).execute(
    Array("-X", "verilog", "-td", "source/"),
    Seq(
      ChiselGeneratorAnnotation(() => new Cache2())
    )
  )
}
