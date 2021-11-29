import chisel3._
import chisel3.util._

class AcceleratorC extends Module {
  val io = IO(new Bundle {
    val start: Bool = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))
  })

  val idle :: read :: friendsTop :: friendsRight :: friendsBottom :: friendsLeft :: writeBlack :: writeWhite :: Nil = Enum(8)

  //States and registers
  val stateReg = RegInit(idle)

  //Support registers
  val addressReg = RegInit(0.U(16.W))
  val dataReg = RegInit(0.U(32.W))
  val registers = Reg(Vec(16,UInt(32.W)))
  val regCount = RegInit(0.U(32.W))

  //Default values
  io.writeEnable := false.B
  io.address := 0.U(16.W)
  io.dataWrite := dataReg
  io.done := false.B
  regCount := regCount + 1.U(32.W)

  switch(stateReg) {
    //We make sure all the registers are initialized correctly
    is(idle) {
      when(io.start) {
        stateReg := read        //For standard run
        registers(1) := 0.U(32.W) //0
        registers(2) := 1.U(32.W) //1
        registers(3) := 0.U(32.W) //0
        registers(4) := 20.U(32.W) //0
        registers(5) := 1.U(32.W) //0
        registers(6) := 0.U(32.W) //0
        registers(10) := 0.U(32.W)
        registers(11) := 0.U(32.W)
      }
    }
    //readRoi reads every pixel and saves them as bits in valReg(line)
    is(read) {
      io.address := registers(4)
      when (io.dataRead === 0.U(32.W)) {
        stateReg := writeBlack
      } .elsewhen(io.dataRead === 255.U(32.W)) {
        registers(5) := registers(4) - 20.U(32.W)
        stateReg := friendsBottom
      }
    }
    //writeRoi writes the content of valRegs
    is(friendsBottom) {
      io.address := registers(5)
      when(io.dataRead === 0.U(32.W)) {
        stateReg := writeBlack
      } .otherwise {
        registers(5) := registers(4) - 1.U(32.W)
        stateReg := friendsLeft
      }
    }
    is(friendsLeft) {
      io.address := registers(5)
      when(io.dataRead === 0.U(32.W)) {
        stateReg := writeBlack
      } .otherwise {
        registers(5) := registers(4) + 20.U(32.W)
        stateReg := friendsTop
      }
    }
    is(friendsTop) {
      io.address := registers(5)
      when(io.dataRead === 0.U(32.W)) {
        stateReg := writeBlack
      } .otherwise {
        registers(5) := registers(4) + 1.U(32.W)
        stateReg := friendsRight
      }
    }
    is(friendsRight) {
      io.address := registers(5)
      when(io.dataRead === 0.U(32.W)) {
        stateReg := writeBlack
      } .otherwise {
        stateReg := writeWhite
      }
    }
    is(writeBlack) {
      io.writeEnable := true.B
      io.address := registers(4) + 400.U(32.W)
      io.dataWrite := 0.U(32.W)

      when (registers(1) < 20.U(32.W)) {
        when(registers(2) < 19.U(32.W)) {
          registers(2) := registers(2) + 1.U(32.W)
          registers(4) := registers(1) + registers(2) * 20.U(32.W)
          stateReg := read
        }.otherwise {
          registers(2) := 0.U(32.W)
          registers(1) := registers(1) + 1.U(32.W)
          registers(4) := registers(1) + registers(2) * 20.U(32.W)
          stateReg := read
        }
      } .otherwise {
        io.done := true.B
      }
    }
    is(writeWhite) {
      io.writeEnable := true.B
      io.address := registers(4) + 400.U(32.W)
      io.dataWrite := 255.U(32.W)

      when (registers(1) < 20.U(32.W)) {
        when(registers(2) < 19.U(32.W)) {
          registers(2) := registers(2) + 1.U(32.W)
          registers(4) := registers(1) + registers(2) * 20.U(32.W)
          stateReg := read
        }.otherwise {
          registers(2) := 0.U(32.W)
          registers(1) := registers(1) + 1.U(32.W)
          registers(4) := registers(1) + registers(2) * 20.U(32.W)
          stateReg := read
        }
      } .otherwise {
        io.done := true.B
      }
    }
  }
}
