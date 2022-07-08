package cache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import common.CurrentCycle

// matadata for each cache block
class Meta1 extends Bundle with Cache1Config {
  // cache block data valid
  val valid = Bool()
  // dirty: different data: cache and memory
  // check valid and dirty before data replace
  val dirty = Bool()
  val address = UInt(addressWidth.W)
  val tag = UInt(tagBits.W)
}

class Cache1 extends Module with Cache1Config with CurrentCycle {
  scala.Predef.printf(s"tagBits: ${tagBits}, indexBits: ${indexBits}, offsetBits: ${offsetBits}\n")

  assert(assoc == 1)

  val io = IO(new CacheIO)

  // one set, one cache block
  // width: blockSizeInBytes * 8 bits
  val dataArray = RegInit(VecInit(Seq.fill(numSets)(0.U((blockSizeInBytes * 8).W))))
  val metaArray = RegInit(VecInit(Seq.fill(numSets)(
    {
      val meta = Wire(new Meta1())
      meta.valid := false.B
      meta.dirty := false.B
      meta.address := 0.U
      meta.tag := 0.U
      meta
    }
  )))

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
  val index = getIndex(address)

  // HIT cache!
  val hit = regState === sIdle && io.request.fire() && metaArray(index).valid && metaArray(index).tag === tag

  // Calculate hit rate
  val regNumHits = RegInit(0.U(32.W))

  io.numHits := regNumHits

  io.numCycles := currentCycle

  val addressReg = Reg(UInt(addressWidth.W))
  val tagReg = getTag(addressReg)
  val indexReg = getIndex(addressReg)

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

            dataArray(index) := io.request.bits.writeData

            regState := sWriteResponse
          }.otherwise {
            // MISS
            when(metaArray(index).valid && metaArray(index).dirty) {
              writeback()
            }

            metaArray(index).valid := true.B
            metaArray(index).dirty := true.B
            metaArray(index).tag := tag
            metaArray(index).address := address
            dataArray(index) := io.request.bits.writeData

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
    p"[${currentCycle}] regState: ${regState}, request.fire(): ${io.request.fire()}, response.fire(): ${io.response.fire()}, writeEnable: ${io.request.bits.writeEnable}, address: ${io.request.bits.address}, tag: ${tag}, index: ${index}, hit: ${hit}, regNumHits: ${regNumHits}\n"
  )
  // chisel3.printf(
  //   p"[${currentCycle}] regState: ${regState}, data: ${io.response.bits.readData}, address: ${io.request.bits.address}, tag: ${tag}, index: ${index}, hit: ${hit}, regNumHits: ${regNumHits}\n"
  // )
}

object Cache1 extends App {
  (new ChiselStage).execute(
    Array("-X", "verilog", "-td", "source/"),
    Seq(
      ChiselGeneratorAnnotation(() => new Cache1())
    )
  )
}