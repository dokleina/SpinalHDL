package spinal.lib

import sim.{ScoreboardInOrder, StreamDriver, StreamMonitor}

import spinal.core._
import spinal.core.sim._
import spinal.lib.{PackedBundle, PackedWordBundle}
import spinal.tester.SpinalAnyFunSuite
import scala.util.Random

class SpinalSimStreamUnpackTester extends SpinalAnyFunSuite {

  case class UnpackTestBundle() extends Bundle {
    val r = UInt(5 bits)
    val g = UInt(6 bits)
    val b = UInt(5 bits)
    val a = UInt(16 bits)
  }

  case class UnpackTestUnit(var r: Int, var g: Int, var b: Int, var a: Int)

  class IStreamUnpackFixture(val offset: Int, val contiguousLayout: Boolean) extends Component {
    val io = new Bundle {
      val inStream = slave(new Stream(Bits(8 bits)))
      val start = in Bool()

      val outData = out(UnpackTestBundle())
      val done = out Bool()
    }
  }

  case class StreamUnpackStartBitFixture(override val offset: Int = 0, override val contiguousLayout : Boolean = false) extends IStreamUnpackFixture(offset, contiguousLayout) {
    val output = UnpackTestBundle()
    val stream = io.inStream.stage()

    val layout = List(
      output.r -> (0 + offset),
      output.g -> (8 + offset),
      output.b -> (16 + offset)
    ) ++ {
      if (contiguousLayout) {
        List(
          output.a -> 24
        )
      } else {
        List(
          output.a( 7 downto 0) -> 24,
          output.a(15 downto 8) -> 32
        )
      }
    }

    val unpacker = StreamUnpacker[Bits](
      stream,
      layout
    )

    unpacker.io.start := io.start
    io.outData := output

    io.done := unpacker.io.done
  }

  case class StreamUnpackSliceFixture(override val offset: Int = 0) extends IStreamUnpackFixture(offset, false) {
    val output = UnpackTestBundle()
    val stream = io.inStream.stage()

    val layout: Map[Data, Map[Int, (Range, Range)]] = Map(
      output.r -> Map(
        0 -> ((offset to offset + 4), (0 to 4))
      ),
      output.g -> Map(
        1 -> ((offset to offset + 5), (0 to 5))
      ),
      output.b -> Map(
        2 -> ((offset to offset + 4), (0 to 4))
      ),
      output.a -> Map(
        3 -> ((0 to 7), (0 to 7)),
        4 -> ((0 to 7), (8 to 15))
      )
    )

    val unpacker = StreamUnpacker[Bits](
      stream,
      layout
    )

    unpacker.io.start := io.start
    io.outData := output

    io.done := unpacker.io.done
  }

  case class StreamUnpackBundleFixture() extends IStreamUnpackFixture(0, false) {
    val output = UnpackTestBundle()
    val stream = io.inStream.stage()

    val unpacker = StreamUnpacker[Bits](
      stream,
      output
    )

    unpacker.io.start := io.start
    io.outData := output

    io.done := unpacker.io.done
  }

  case class StreamUnpackPackedBundleFixture(override val offset: Int = 0) extends IStreamUnpackFixture(offset, false) {
    val packedBundle = new PackedWordBundle(8 bits) {
      val r = UInt(5 bits) packFrom(offset) inWord(0)
      val g = UInt(6 bits) packFrom(offset) inWord(1)
      val b = UInt(5 bits) packFrom(offset) inWord(2)
      val a = UInt(16 bits) inWord(3)
    }

    val stream = io.inStream.stage()

    val unpacker = StreamUnpacker[Bits](
      stream,
      packedBundle
    )

    val output = UnpackTestBundle()
    output.assignFromBits(packedBundle.asBits)

    unpacker.io.start := io.start
    io.outData := output

    io.done := unpacker.io.done
  }

  def simDriver(doRandom : Boolean)(dut: IStreamUnpackFixture): Unit = {
    dut.clockDomain.forkStimulus(10)

    dut.io.start #= false

    val scoreboard = ScoreboardInOrder[UnpackTestUnit]()

    // Wait until out of reset
    dut.clockDomain.waitSampling()

    // Input generator
    fork {
      // Generate the single-bit only cases
      for (i <- 0 to 40) {
        val allBundleBits = if(i == 0) BigInt(0) else BigInt(1) << (i-1)

        // Send bytes to the DUT
        for (b <- 0 to 4) {
          val byteToSend = (allBundleBits >> (b * 8)) & 0xFF

          // Wait some random number of cycles if we need to
          // but not on the first byte
          if (doRandom && b > 0) dut.clockDomain.waitSampling(Random.nextInt(4))

          dut.io.inStream.payload #= byteToSend
          dut.io.inStream.valid #= true
          dut.io.start #= b == 0  // Issue Start on the first byte

          dut.clockDomain.waitSampling()

          dut.io.inStream.payload #= 0
          dut.io.inStream.valid #= false
          dut.io.start #= false
        }

        // Wait until the DUT is done
        dut.clockDomain.waitSamplingWhere(dut.io.done.toBoolean)
        // Space out the cases by a few cycles
        dut.clockDomain.waitSampling(5)
      }
    }

    // Output checker
    {
      // Stream Monitor
      var curWord = 0
      var curR = 0
      var curG = 0
      var curB = 0
      var curA = 0
      StreamMonitor(dut.io.inStream, dut.clockDomain)(p => {
        curWord match {
          case 0 =>
            curR = (p.toInt >> dut.offset) & 0x1F
          case 1 =>
            curG = (p.toInt >> dut.offset) & 0x3F
          case 2 =>
            curB = (p.toInt >> dut.offset) & 0x1F
          case 3 =>
            curA = p.toInt & 0xFF
          case 4 =>
            curA += (p.toInt & 0xFF) << 8
            scoreboard.pushRef(UnpackTestUnit(curR, curG, curB, curA))
        }

        curWord = (curWord + 1) % 5
      })

      // Unpacked output
      for(i <- 0 to 40) {
        dut.clockDomain.waitSamplingWhere(dut.io.done.toBoolean)
        scoreboard.pushDut(UnpackTestUnit(
          dut.io.outData.r.toInt,
          dut.io.outData.g.toInt,
          dut.io.outData.b.toInt,
          dut.io.outData.a.toInt
        ))
        scoreboard.check()
      }
    }

    // Wait a bit before ending
    dut.clockDomain.waitSampling(10)

    simSuccess()
  }

  def simDriverBundle(doRandom : Boolean)(dut: IStreamUnpackFixture): Unit = {
    dut.clockDomain.forkStimulus(10)

    dut.io.start #= false

    val scoreboard = ScoreboardInOrder[UnpackTestUnit]()

    // Wait until out of reset
    dut.clockDomain.waitSampling()

    // Input generator
    fork {
      // Generate the single-bit only cases
      for (i <- 0 to 32) {
        val allBundleBits = if(i == 0) BigInt(0) else BigInt(1) << (i-1)

        // Send bytes to the DUT
        for (b <- 0 to 3) {
          val byteToSend = (allBundleBits >> (b * 8)) & 0xFF

          // Wait some random number of cycles if we need to
          // but not on the first byte
          if (doRandom && b > 0) dut.clockDomain.waitSampling(Random.nextInt(4))

          dut.io.inStream.payload #= byteToSend
          dut.io.inStream.valid #= true
          dut.io.start #= b == 0  // Issue Start on the first byte

          dut.clockDomain.waitSampling()

          dut.io.inStream.payload #= 0
          dut.io.inStream.valid #= false
          dut.io.start #= false
        }

        // Wait until the DUT is done
        dut.clockDomain.waitSamplingWhere(dut.io.done.toBoolean)
        // Space out the cases by a few cycles
        dut.clockDomain.waitSampling(5)
      }
    }

    // Output checker
    {
      // Stream Monitor
      var curWord = 0
      var curR = 0
      var curG = 0
      var curB = 0
      var curA = 0
      StreamMonitor(dut.io.inStream, dut.clockDomain)(p => {
        curWord match {
          case 0 =>
            curR =   p.toInt & 0x1F
            curG =  (p.toInt & 0xE0) >> 5
          case 1 =>
            curG += (p.toInt & 0x07) << 3
            curB =  (p.toInt & 0xF8) >> 3
          case 2 =>
            curA =   p.toInt & 0xFF
          case 3 =>
            curA += (p.toInt & 0xFF) << 8
            scoreboard.pushRef(UnpackTestUnit(curR, curG, curB, curA))
        }

        curWord = (curWord + 1) % 4
      })

      // Unpacked output
      for(i <- 0 to 32) {
        dut.clockDomain.waitSamplingWhere(dut.io.done.toBoolean)
        scoreboard.pushDut(UnpackTestUnit(
          dut.io.outData.r.toInt,
          dut.io.outData.g.toInt,
          dut.io.outData.b.toInt,
          dut.io.outData.a.toInt
        ))
        scoreboard.check()
      }
    }

    // Wait a bit before ending
    dut.clockDomain.waitSampling(10)

    simSuccess()
  }

  test("start bit layout: aligned, always ready") {
    SimConfig.compile(StreamUnpackStartBitFixture())
      .doSim(simDriver(doRandom = false) _)
  }

  test("start bit layout: aligned, random ready") {
    SimConfig.compile(StreamUnpackStartBitFixture())
      .doSim(simDriver(doRandom = true) _)
  }

  test("start bit layout: unaligned, always ready") {
    SimConfig.compile(StreamUnpackStartBitFixture(2))
      .doSim(simDriver(doRandom = false) _)
  }

  test("start bit layout: unaligned, random ready") {
    SimConfig.compile(StreamUnpackStartBitFixture(2))
      .doSim(simDriver(doRandom = true) _)
  }

  test("start bit layout: contiguous") {
    SimConfig.compile(StreamUnpackStartBitFixture(contiguousLayout = true))
      .doSim(simDriver(doRandom = false) _)
  }

  test("slice layout: aligned, always ready") {
    SimConfig.compile(StreamUnpackSliceFixture())
      .doSim(simDriver(doRandom = false) _)
  }

  test("slice layout: aligned, random ready") {
    SimConfig.compile(StreamUnpackSliceFixture())
      .doSim(simDriver(doRandom = true) _)
  }

  test("slice layout: unaligned, always ready") {
    SimConfig.compile(StreamUnpackSliceFixture(2))
      .doSim(simDriver(doRandom = false) _)
  }

  test("slice layout: unaligned, random ready") {
    SimConfig.compile(StreamUnpackSliceFixture(2))
      .doSim(simDriver(doRandom = true) _)
  }

  test("bundle layout: always ready") {
    SimConfig.compile(StreamUnpackBundleFixture())
      .doSim(simDriverBundle(doRandom = false) _)
  }

  test("bundle layout: random ready") {
    SimConfig.compile(StreamUnpackBundleFixture())
      .doSim(simDriverBundle(doRandom = true) _)
  }

  test("packed bundle layout: aligned, always ready") {
    SimConfig.compile(StreamUnpackPackedBundleFixture())
      .doSim(simDriver(doRandom = false) _)
  }

  test("packed bundle layout: aligned, random ready") {
    SimConfig.compile(StreamUnpackPackedBundleFixture())
      .doSim(simDriver(doRandom = true) _)
  }

  test("packed bundle layout: unaligned, always ready") {
    SimConfig.compile(StreamUnpackPackedBundleFixture(2))
      .doSim(simDriver(doRandom = false) _)
  }

  test("packed bundle layout: unaligned, random ready") {
    SimConfig.compile(StreamUnpackPackedBundleFixture(2))
      .doSim(simDriver(doRandom = true) _)
  }
}
