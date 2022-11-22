package spinal.tester

import spinal.core._
import spinal.lib._

object PackPlay {
  case class TestBundle() extends Bundle {
    val r = UInt(5 bits)
    val g = UInt(6 bits)
    val b = UInt(5 bits)
    val a = UInt(16 bits)
    val invert = Bool()
    val ignore = Bool()
  }

  class TestBundlePacked extends PackedWordBundle(16 bits) {
    val r = UInt(5 bits) word 0
    val g = UInt(6 bits) word 1
    val b = UInt(5 bits) word 2
    val a = UInt(16 bits) words(
      3 -> (7 downto 0),
      4 -> (15 downto 8)
    )
    val c = UInt(5 bits) word 0
    val invert = Bool() at 0 word 5
    val ignore = Bool() at 1 word 5
  }

  class TestFieldBundle extends Packed(16 bits) {
    val r = (5 downto 0) word 0
    val g = (6 downto 0) word 1
    val b = (5 downto 0) word 2
  }

  class TopLevel extends Component {
    var io = new Bundle {
      val inData = in(new TestBundle())
      val outData = out(new TestBundle())
    }

    io.outData := RegNext(io.inData)
  }


  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel())
  }
}
