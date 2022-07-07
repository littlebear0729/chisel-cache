package cache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import common.CurrentCycle

// matadata for each cache block
class Meta3 extends Bundle with Cache3Config {
  // cache block data valid
  val valid = Bool()
  // dirty: different data between cache and memory
  // check valid and dirty before data replace
  val dirty = Bool()
  val address = UInt(addressWidth.W)
  val tag = UInt(tagBits.W)
  // hit counter for LRU algorithm
  val counter = UInt(32.W)
}

class Cache3 extends Module with Cache3Config with CurrentCycle {
  scala.Predef.printf(s"tagBits: ${tagBits}, indexBits: ${indexBits}, offsetBits: ${offsetBits}\n")

  val io = IO(new CacheIO)

  // one set, one cache block
  // width: blockSizeInBytes * 8 bits
  val dataArray = RegInit(
    VecInit(Seq.fill(assoc * numSets)(0.U((blockSizeInBytes * 8).W)))
  )
  val metaArray = RegInit(
    VecInit(
      Seq.fill(assoc * numSets)(
        {
          val meta = Wire(new Meta3())
          meta.valid := false.B
          meta.dirty := false.B
          meta.address := 0.U
          meta.tag := 0.U
          meta.counter := 0.U
          meta
        }
      )
    )
  )

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
  var exist = false.B
  for (i <- 0 until assoc) {
    exist = exist || (metaArray(i.U * numSets.U + index).valid && metaArray(
      i.U * numSets.U + index
    ).tag === tag)
  }
  val hit = regState === sIdle && io.request.fire() && exist

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

  def findPos(): UInt = {
    // find a position in cache with given address and tag
    var idx = 0.U
    for (i <- 0 until assoc) {
      idx = Mux(metaArray(i.U * numSets.U + index).tag === tagReg, i.U * numSets.U + index, idx)
    }
    return idx
  }

  def findEmptyPos(): UInt = {
    // find a empty position in order to write new data in
    var idx = 0.U
    for (i <- 0 until assoc) {
      idx = Mux(metaArray(i.U * numSets.U + index).counter === 0.U, i.U * numSets.U + index, idx)
    }
    return idx
  }

  def findOldPos(): UInt = {
    // find the oldest cache position to over-write
    val minMeta = metaArray.reduceLeft { (x, y) =>
      Mux(x.counter < y.counter, x, y)
    }
    var idx = 0.U
    for (i <- 0 until assoc) {
      idx = Mux(metaArray(i.U * numSets.U + index).counter === minMeta.counter, i.U * numSets.U + index, idx)
    }
    return idx
  }

  switch(regState) {
    is(sIdle) {
      io.request.ready := true.B

      when(io.request.fire()) {
        addressReg := io.request.bits.address

        // Read or Write?
        when(io.request.bits.writeEnable) {
          // Write
          when(hit) {
            // HIT
            regNumHits := regNumHits + 1.U
            val idx = findPos()
            metaArray(idx).counter := metaArray(idx).counter + 1.U
            // ---If write hit, write to exist cache block (and write back to memory).---
            dataArray(idx) := io.request.bits.writeData

            regState := sWriteResponse
          }.otherwise {
            // MISS
            // ---If write miss, find a avaliable(empty or oldest) cache block to write.---
            var idx = findPos()
            when(metaArray(idx).valid && metaArray(idx).dirty) {
              writeback()
            }
            
            val idx2 = findEmptyPos()
            when(idx2 === 0.U) {
              idx = findOldPos()
              when(metaArray(idx).valid && metaArray(idx).dirty) {
                writeback()
              }
            }.otherwise {
              idx = idx2
            }
            metaArray(idx).valid := true.B
            metaArray(idx).dirty := true.B
            metaArray(idx).tag := tag
            metaArray(idx).address := address
            dataArray(idx) := io.request.bits.writeData

            regState := sWriteResponse
          }
        }.otherwise {
          // Read
          when(hit) {
            //HIT
            regNumHits := regNumHits + 1.U
            val idx = findPos()
            metaArray(idx).counter := metaArray(idx).counter + 1.U
            regState := sReadData
          }.otherwise {
            //MISS
            val idx = findPos()
            when(metaArray(idx).valid && metaArray(idx).dirty) {
              writeback()
            }

            refill()

            regState := sReadMiss
          }
        }
      }
    }
    is(sReadMiss) {
      // ---If read miss, read data from memory and save it to cache available block.---
      // var idx = findPos()
      // val idx2 = findEmptyPos()
      // when(idx2 === 0.U) {
      //   idx = findOldPos()
      // }.otherwise {
      //   idx = idx2
      // }
      metaArray(indexReg).valid := true.B
      metaArray(indexReg).dirty := false.B
      metaArray(indexReg).tag := tagReg
      metaArray(indexReg).address := addressReg
      dataArray(indexReg) := memory.io.readData

      regState := sReadData
    }
    is(sReadData) {
      // ---Pass cached data from cache to response bits.---
      val idx = findPos()
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
      .fire()}, writeEnable: ${io.request.bits.writeEnable}, address: ${io.request.bits.address}, tag: ${tag}, index: ${index}, hit: ${hit}, regNumHits: ${regNumHits}\n"
  )
  // chisel3.printf(
  //   p"[${currentCycle}] regState: ${regState}, data: ${io.response.bits.readData}, address: ${io.request.bits.address}, tag: ${tag}, index: ${index}, hit: ${hit}, regNumHits: ${regNumHits}\n"
  // )
}

object Cache3 extends App {
  (new ChiselStage).execute(
    Array("-X", "verilog", "-td", "source/"),
    Seq(
      ChiselGeneratorAnnotation(() => new Cache3())
    )
  )
}
