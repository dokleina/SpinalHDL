package spinal.lib

import spinal.core._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


trait IDataPacker {
  /**
    * Handler method for new
    * @param d
    * @param range
    */
  def handleData(d : Data)

  def getBitsWidth : Int
}

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
class PackedBundle(endianness : Endianness = LITTLE) extends Bundle {

  protected class BitPacker extends IDataPacker {

    private val packMap = new mutable.LinkedHashMap[Data, (Range, Endianness)]()
    private var nextBit = 0
    private var highBit = 0

    override def handleData(d: Data): Unit = {
      val packTuple : (Range, Endianness) = d.getTag(classOf[BitPacker.TagBitPackExact]) match {
        case Some(tag) => tag.range -> tag.endianness
        case None => nextBit until nextBit + d.getBitsWidth -> endianness
      }
      nextBit = packTuple._1.max + 1
      highBit = highBit.max(nextBit)

      packMap.put(d, packTuple)
    }

    override def asBits : Bits = {
      // `nextBit` will be the length of the
      val packed = B(0, highBit bit)
      packMap.foreach { case(data : Data, packTuple : (Range, Endianness)) =>
        val range = packTuple._1
        val dataSize = range.size.min(data.getBitsWidth)
        packed(range) := packTuple._2 match {
          case LITTLE => data.asBits.takeLow(dataSize).resize(range.size)
          case BIG => data.asBits.takeHigh(dataSize).resizeLeft(range.size)
        }

      }
      packed
    }

    override def getBitsWidth: Int = ???


  }

  protected object BitPacker {
    def MakePackTag(range : Range, endianness : Endianness) : TagBitPackExact = {
      new TagBitPackExact(range, endianness)
    }

    class TagBitPackExact(val range: Range, val endianness: Endianness) extends SpinalTag
  }

  protected val builder = new DataBitPacker()

  private def computePackMapping(): Seq[(Range, Data)] = {
    var lastPos = 0
    elements.map(_._2).map(d => {
      val r = d.getTag(classOf[TagBitPackExact]) match {
        case t: Some[TagBitPackExact] =>
          t.get.range
        case None =>
          (lastPos+d.getBitsWidth-1) downto (lastPos)
      }
      lastPos = r.high
      (r, d)
    }).toSeq
  }

  override def asBits: Bits = {
    val mappings = computePackMapping()
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
    val mappings = computePackMapping()
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

  override def getBitsWidth: Int = computePackMapping().map(_._1.high).max+1

  implicit class DataPositionEnrich[T <: Data](t: T) {
    /**
      * Place the data at the given range. Extra bits will be lost (unassigned or read) if the data does not fit with the range.
      * @param range Range to place the data
      * @param endianness Bit direction to align data within the range
      * @return Self
      */
    def pack(range: Range, endianness: Endianness = LITTLE): T = {
      t.addTag(new TagBitPackExact(range, endianness))
      t.addTag(builder.make)
      t
    }

    /**
      * Packs data starting (LSB) at the bit position
      * @param pos Starting bit position of the data
      * @return Self
      */
    def packFrom(pos: Int): T = {
      t.addTag(builder.MakeBitsTag)
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
  }

  override def valCallbackRec(ref: Any, name: String): Unit = {
    super.valCallbackRec(ref, name)
    ref match {
      case ref: Data =>
        // ToDo: Call factory
      case _ =>
    }
  }
}

class PackedWordBundle(wordWidth : BitCount, endianness : Endianness = LITTLE) extends PackedBundle(endianness) {

  protected class WordPacker(wordWidth : Int) extends IDataPacker {
    val mappedElements = mutable.LinkedHashMap[Data, ArrayBuffer[(Int, Range)]]()
    val mappedWords    = ArrayBuffer[ArrayBuffer[(Data, Range)]]()

    override def handleData(d : Data) = {

    }

    private def mapToWord(index : Int, d : Data, r : Range) = {
      // Ensure the range fits
      require(r.min < wordWidth && r.max < wordWidth && r.min > 0)

      if(index > (mappedWords.length-1)) {
        // Insert empty words
        mappedWords.appendAll(List.tabulate(index - mappedWords.length + 1)(_ => ArrayBuffer[(Data, Range)]()))
      }

      mappedWords(index).append((d, r))
    }

    def totalWords = mappedWords.length
    def totalBits = totalWords * wordWidth
  }

  protected object WordPacker {

  }

  implicit class DataWordEnrich[T <: Data](t : T) {

    /**
      * Packs the data into the word given by the index.
      * If the data does not fit entirely within the word it will wrap into subsequent words until all bits are packed.
      * For explicit control of multiword pack, use `words`.
      * @param index Word index to pack the data into
      * @return Self
      */
    def word(index : Int): T = {

      t
    }

    /**
      * Packs a single data into multiple words with the given Word Index / Range pair.
      * @param pairList List of Word Index / Range pairs
      * @return Self
      */
    def words(pairList : List[(Int, Range)]) : T = {

      t
    }

    /**
      * Syntax shortcut to `words(List[(Int, Range)]`.
      * @param pairs List of Word Index / Range tuples
      * @return Self
      */
    def words(pairs : (Int, Range)*) : T = words(pairs.toList)

    /**
      * Places the data
      * @param bit
      * @return
      */
    def at(bit : Int) : T = {

      t
    }
  }
}
