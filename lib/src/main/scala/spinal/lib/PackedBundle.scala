package spinal.lib

import spinal.core._

import scala.collection.mutable.ArrayBuffer

abstract class IMappingBuilder {
  def addData(d : Data): Unit
  def width: Int

  def mapping: Seq[(Range, Data)]
}

sealed trait TagPackableBundle extends SpinalTag

/**
  * Similar to Bundle but with bit packing capabilities.
  * Use pack implicit functions to assign fields to bit locations
  * - pack(Range, [Endianness]) - Packs the data into Range aligning to bit Endianness if too wide
  * - packFrom(Position) - Packs the data starting (LSB) at Position. Uses full data length
  * - packTo(Position) - Packs the data ending (MSB) at Position. Uses full data length
  *
  * Providing no location tag will place the next data value immediately after the last.
  *
  * @example {{{
  *     val regWord = new PackedBundle {
  *       val init = Bool().packFrom(0) // Bit 0
  *       val stop = Bool() // Bit 1
  *       val result = Bits(16 bit).packTo(31) // Bits 16 to 31
  *     }
  * }}}
  *
  */
class PackedBundle extends Bundle {

  class TagBitPackExact(val range: Range, val endianness: Endianness) extends SpinalTag

  /**
    * Builds and caches the range mappings for PackedBitBundle's elements.
    * Tracks the width required for all mappings.
    * Does not check for overlap of elements.
    */
  protected class MappingBuilder extends IMappingBuilder {
    var lastPos = 0
    var highBit = 0

    val mapping = ArrayBuffer[(Range, Data)]()

    def addData(d : Data): Unit = {
      val r = d.getTag(classOf[TagBitPackExact]) match {
        case t: Some[TagBitPackExact] =>
          t.get.range
        case None =>
          (lastPos+d.getBitsWidth-1) downto (lastPos)
      }
      lastPos = r.high

      // Update the bit width
      highBit = highBit.max(r.high)

      mapping.append(r -> d)
    }

    def width = highBit + 1
  }

  val mapBuilder : IMappingBuilder = new MappingBuilder()

  /**
    * Gets the mappings of Range to Data for this PackedBundle
    * @return Seq of (Range,Data) for all elements
    */
  def mappings = mapBuilder.mapping

  override def asBits: Bits = {
    val maxWidth = mappings.map(_._1.high).max + 1
    val packed = B(0, maxWidth bit)
    for ((range, data) <- mappings) {
      val endianness: Endianness = data.getTag(classOf[TagBitPackExact]) match {
        case t: Some[TagBitPackExact] => t.get.endianness
        case _ => LITTLE
      }
      endianness match {
        case LITTLE =>
          packed(range) := data.asBits.takeLow(range.size.min(data.getBitsWidth)).resize(range.size)
        case BIG =>
          packed(range) := data.asBits.takeHigh(range.size.min(data.getBitsWidth)).resizeLeft(range.size)
      }
    }
    packed
  }

  override def assignFromBits(bits: Bits): Unit = assignFromBits(bits, bits.getBitsWidth, 0)

  override def assignFromBits(bits: Bits, hi: Int, lo: Int): Unit = {
    for((elRange, el) <- mappings) {
      val endianness: Endianness = el.getTag(classOf[TagBitPackExact]) match {
        case t: Some[TagBitPackExact] => t.get.endianness
        case _ => LITTLE
      }
      if (!(lo >= elRange.high || hi < elRange.low)) {
        val diff = (elRange.size - el.getBitsWidth).max(0)
        endianness match {
          case LITTLE =>
            val boundedBitsRange = hi.min(elRange.high-diff) downto lo.max(elRange.low)
            el.assignFromBits(bits(boundedBitsRange).resize(el.getBitsWidth))
          case BIG =>
            val boundedBitsRange = hi.min(elRange.high) downto lo.max(elRange.low+diff)
            el.assignFromBits(bits(boundedBitsRange).resizeLeft(el.getBitsWidth))
        }
      }
    }
  }

  override def getBitsWidth: Int = mapBuilder.width

  implicit class DataPositionEnrich[T <: Data](t: T) {
    /**
      * Place the data at the given range. Extra bits will be lost (unassigned or read) if the data does not fit with the range.
      * @param range Range to place the data
      * @param endianness Bit direction to align data within the range
      * @return Self
      */
    def pack(range: Range, endianness: Endianness = LITTLE): T = {
      t.addTag(new TagBitPackExact(range, endianness))
      t
    }

    /**
      * Packs data starting (LSB) at the bit position
      * @param pos Starting bit position of the data
      * @return Self
      */
    def packFrom(pos: Int): T = {
      t.pack(pos + t.getBitsWidth - 1 downto pos)
    }

    /**
      * Packs data ending (MSB) at the bit position
      * @param pos Ending bit position of the data
      * @return Self
      */
    def packTo(pos: Int): T = {
      t.pack(pos downto pos - t.getBitsWidth + 1)
    }

    /**
      * Packs a single bit at the bit position
      *
      * @param pos
      * @return
      */
    def at(pos: Int): T = {
      t.pack(pos downto pos)
    }
  }

  override def valCallbackRec(ref: Any, name: String): Unit = {
    super.valCallbackRec(ref, name)

    // Process the data
    ref match {
      case d: Data =>
        mapBuilder.addData(d)
      case _ =>
    }
  }
}

/**
  * An enhanced form of PackedBundle with Word-centric packing.
  * Offers all the same implicit packing assignment functions, but applies packing to an assigned word.
  * - inWord(WordIndex) - Indicates which word to pack into. Must be used after a pack assigment. If no pack range was given then the entire data length will be assumed. Ranges that exceed the word will wrap into subsequent words.
  *
  * Like PackedBundle, providing no pack or word assignments will place data immediately after the last.
  *
  * @example {{{
  *     val wordPacked = PackedWordBundle(8 bits) {
  *       val aNumber = UInt(8 bits).word(0) // Bits 7 downto 0
  *       val bNumber = UInt(8 bits).pack(0 to 7).word(1) // Bits 8 to 15
  *       val large   = Bits(18 bits).word(2) // Bits 33 downto 16
  *       val flag    = Bool() // Bit 34
  * }}}
  * @param wordWidth Width of a word, as BitCount
  */
class PackedWordBundle(wordWidth : BitCount) extends PackedBundle {

  implicit class WordEnrich[T <: Data](t: T) {

    def inWord(index : Int) = {
      val bitPackExact = t.getTag(classOf[TagBitPackExact])

      if (bitPackExact.isDefined) {
        // Update the BitPackExact if it exists on the Data
        t.removeTag(bitPackExact.get)
        val oldRange = bitPackExact.get.range
        val basePos = index * wordWidth.value

        val newRange = {
          if (oldRange.step > 0) {
            oldRange.low + basePos to oldRange.high + basePos
          } else {
            oldRange.high + basePos downto oldRange.low + basePos
          }
        }

        t.pack(newRange)
      } else {
        // Add the full range of the Data starting at the given word index
        val basePos = index * wordWidth.value
        t.packFrom(basePos)
      }
    }
  }
}