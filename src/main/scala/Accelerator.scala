import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))

  })

  //Write here your code

  //States and registers
  val idle :: borderCheck :: read :: blackPx :: checkLeft :: checkRight :: checkUp :: checkDown :: write :: incY :: incX :: Nil = Enum (11)
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

  //FSMD switch
  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := borderCheck
        registers(1) := 0.U(32.W)
        registers(2) := 0.U(32.W)
      }
    }
    is(borderCheck) {
      registers(3) := 20.U(32.W) * registers(2) + registers(1)
      //registers(3) := registers(3)
      registers(4) := 20.U(32.W) * registers(2) + registers(1) + 400.U(32.W)
      registers(5) := 0.U(32.W)
      registers(6) := 0.U(32.W)
      registers(7) := 0.U(32.W)
      when(registers(1) === 0.U(32.W)) {
        stateReg := write
      } .elsewhen(registers(2) === 0.U(32.W)) {
        stateReg := write
      } .elsewhen(registers(1) === 19.U(32.W)) {
        stateReg := write
      } .elsewhen(registers(2) === 19.U(32.W)) {
        stateReg := write
      } .otherwise {
        io.address := registers(3)
        registers(6) := io.dataRead
        stateReg := blackPx
      }
    }
    is (blackPx) {
      when(registers(6) === 0.U(32.W)) {
        registers(5) := 0.U(32.W)
        stateReg := write
      } .otherwise {
        io.address := registers(3) - 1.U(32.W)
        registers(7) := io.dataRead
        stateReg := checkLeft
      }
    }
    is(checkLeft) {
      io.address := registers(3) + 1.U(32.W)
      registers(7) := io.dataRead
      when(registers(7) === 0.U(32.W)) {
        stateReg := write
      }.otherwise {
        stateReg := checkRight
      }
    }
    is(checkRight) {
      io.address := registers(3) - 20.U(32.W)
      registers(7) := io.dataRead
      when(registers(7) === 0.U(32.W)) {
        stateReg := write
      }.otherwise {
        stateReg := checkUp
      }
    }
    is(checkUp) {
      io.address := registers(3) + 20.U(32.W)
      registers(7) := io.dataRead
      when(registers(7) === 0.U(32.W)) {
        stateReg := write
      }.otherwise {
        stateReg := checkDown
      }
    }
    is(checkDown) {
      when(registers(7) === 0.U(32.W)) {
        stateReg := write
      }.otherwise {
        registers(5) := 1.U(32.W)
        stateReg := write
      }
    }

    is (write) {
      io.writeEnable := true.B
      io.address := registers(4)
      when (registers(5) === 0.U(32.W)) {
        io.dataWrite := 0.U(32.W)
      } .otherwise {
        io.dataWrite := 255.U(32.W)
      }
      stateReg := incY
    }
    is (incY) {
      when(registers(2) < 19.U(32.W)) {
        registers(2) := registers(2) + 1.U(32.W)
        stateReg := borderCheck
      } .otherwise {
        stateReg := incX
      }
    }
    is (incX) {
      when(registers(1) < 19.U(32.W)) {
        registers(1) := registers(1) + 1.U(32.W)
        registers(2) := 0.U(32.W)
        stateReg := borderCheck
      } .otherwise {
        io.done := true.B
      }
    }
  }


}
