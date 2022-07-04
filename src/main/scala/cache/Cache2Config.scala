package cache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

trait Cache2Config {
    val addressWidth = 32

    // 16 * 8 bits
    val blockSizeInBytes = 16
    val dataWidth = blockSizeInBytes * 8
    // 相联度
    // 1: 直接映射
    val assoc = 128 // 1~8
    // sets of caches
    val numSets = 1

    // cache capacity
    // assoc <=> numSets balance
    val capacityInBytes = blockSizeInBytes * assoc * numSets // 16 * assoc * 128 bytes = 2^4 * assoc * 2^7 bytes = 2^11 * assoc bytes = 2 * assoc kB

    val offsetBits = log2Ceil(blockSizeInBytes) // log2(16) = 4
    val indexBits = log2Ceil(numSets) // log2(1) = 0
    val tagBits = addressWidth - indexBits - offsetBits // 21

    def getTag(address: UInt): UInt = {
        return address(addressWidth - 1, indexBits + offsetBits)
    }

    def getOffset(address: UInt): UInt = {
        return address(offsetBits - 1, 0)
    }
}