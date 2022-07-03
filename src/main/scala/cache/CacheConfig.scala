package cache

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

trait CacheConfig {
    val addressWidth = 32

    // 16 * 8 bits
    val blockSizeInBytes = 16
    // 相连度
    // 1: 直接映射
    val assoc = 1 // 1~8
    // sets of caches
    val numSets = 128

    val dataWidth = blockSizeInBytes * 8

    // cache capacity
    // assoc <=> numSets balance
    val capacityInBytes = blockSizeInBytes * assoc * numSets // 16 * assoc * 128 bytes = 2^4 * assoc * 2^7 bytes = 2^11 * assoc bytes = 2 * assoc kB

    val offsetBits = log2Ceil(blockSizeInBytes) // log2(16) = 4
    val indexBits = log2Ceil(numSets) // log2(128) = 7
    val tagBits = addressWidth - indexBits - offsetBits // 21

    def getTag(address: UInt): UInt = {
        return address(addressWidth - 1, indexBits + offsetBits)
    }

    def getIndex(address: UInt): UInt = {
        return address(indexBits + offsetBits - 1, offsetBits)
    }

    def getOffset(address: UInt): UInt = {
        return address(offsetBits - 1, 0)
    }
}