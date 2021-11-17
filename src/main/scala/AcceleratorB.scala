import chisel3._
import chisel3.util._

class AcceleratorB extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))
  })

  val idle :: border :: read :: writeBlack :: Nil = Enum(4)

  //States and registers
  val stateReg = RegInit(idle)

  //Support registers
  val addressReg = RegInit(0.U(16.W))
  val dataReg = RegInit(0.U(32.W))
  val registers = Reg(Vec(16,UInt(32.W)))

  //Default values
  io.writeEnable := false.B
  io.address := 0.U(16.W)
  io.dataWrite := dataReg
  io.done := false.B

  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := border
        registers(1) := 0.U(32.W)
        registers(2) := 0.U(32.W)
        registers(3) := 0.U(32.W)
        registers(4) := 0.U(32.W)
        registers(5) := 0.U(32.W)
        registers(6) := 0.U(32.W)
      }
    }
    is(border) {
      switch(registers(6)) {
        is(0.U(32.W)) {
          registers(4) := registers(1)
          stateReg := writeBlack
        }
        is(1.U(32.W)) {
          registers(4) := registers(1) * 20.U(32.W)
          stateReg := writeBlack
        }
        is(2.U(32.W)) {
          registers(4) := 19.U(32.W) + (registers(1) * 20.U(32.W))
          stateReg := writeBlack
        }
        is(3.U(32.W)) {
          registers(4) := registers(1) + (19.U(32.W) * 20.U(32.W))
          stateReg := writeBlack
        }
        is (4.U(32.W)) {
          stateReg := writeBlack
        }
      }
    }
    is(read) {
      when(registers(6) === 4.U(32.W)) {
        registers(1) := 1.U(32.W)
        registers(6) := registers(6) + 1.U(32.W)
      }
      registers(4) := registers(1) + registers(2) * 20.U(32.W)
      io.address := registers(4)
      registers(5) := io.dataRead

      when(registers(5) === 0.U(32.W)) {
        stateReg := writeBlack
      }
      when(registers(1) < 18.U(32.W)) {
        when(registers(2) < 18.U(32.W)) {
          registers(2) := registers(2) + 1.U(32.W)
        }.otherwise {
          registers(2) := 1.U(32.W)
          registers(1) := registers(1) + 1.U(32.W)
        }
      }.otherwise{
        io.done := true.B
      }

      stateReg := writeBlack
    }
    is(writeBlack){
      io.writeEnable := true.B
      io.address := registers(4) + 400.U(32.W)
      io.dataWrite := 0.U(32.W)
      when(registers(6) < 4.U(32.W)) {
        registers(1) := registers(1) + 1.U(32.W)
        when(registers(1) < 20.U(32.W)) {
          stateReg := border
        } .otherwise {
          registers(1) := 0.U(32.W)
          registers(6) := registers(6) + 1.U(32.W)
          stateReg := border
        }
      }

      when(registers(6) > 3.U) {
        registers(1) := 1.U(32.W)
        registers(2) := 1.U(32.W)
        stateReg := read
      }
    }

  }


}
