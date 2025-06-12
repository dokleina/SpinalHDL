package spinal.lib.sim

import spinal.core._
import spinal.lib._
import spinal.core.sim._

object PackedBundle {
  implicit class SimPackedBundleEnricher(pb : PackedBundle) {
    def toBigInt : BigInt = {
      BigIntsPacked(pb.mappings.map(_._2.asBits.toBigInt):_*)
    }

    def fromBigInt(inInt : BigInt) : Unit = {
      BigIntUnpacked(inInt).zip(pb.mappings.map(_._2.asBits)).foreach {
        case(newVal, bits) =>
          bits #= newVal
      }
    }

    /**
      * Packs a set of element BigInt values according to the PackedBundle's element mapping.
      * The returned BigInt result will be identical to the PackedBundle's `packed` function.
      *
      * @param elements Element values
      * @return BigInt representing packed result
      */
    def BigIntsPacked(elements : BigInt*) : BigInt = {
      pb.mappings.zip(elements).map { case((range, data), value) =>
        val mask = (1 << range.size) - 1

        if (range.step > 0) {
          // LSB first
          val trunc = value & mask
          trunc << range.low
        } else {
          // MSB first
          val trunc = (value >> (data.getBitsWidth - range.size).max(0)) & mask
          trunc << (range.low + (range.size - data.getBitsWidth).max(0))
        }
      }.sum
    }

    /**
      * Unpacks a BigInt value according to the PackedBundle's element mapping.
      * The return list of BigInts will be identical to the PackedBundle's `unpack` function.
      *
      * @param inInt Value to unpack
      * @return List of BigInts representing unpacked element values
      */
    def BigIntUnpacked(inInt : BigInt) : Seq[BigInt] = {
      pb.mappings.map { case(range, data) =>
        val mask = (1 << range.size.min(data.getBitsWidth)) - 1

        if (range.step > 0) {
          // LSB first
          (inInt >> range.low) & mask
        } else {
          // MSB first
          val extracted = (inInt >> (range.low + (range.size - data.getBitsWidth).max(0))) & mask
          extracted << (data.getBitsWidth - range.size).max(0)
        }
      }
    }
  }
}
