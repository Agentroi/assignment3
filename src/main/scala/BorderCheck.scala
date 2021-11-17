import chisel3._
import chisel3.util._

class BorderCheck extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))

  })

  //State and registers
  val idle :: erode :: write :: Nil = Enum(3)
  val stateReg = RegInit(idle)

  //Support registers
  val addressReg = RegInit(0.U(16.W))
  val dataReg = RegInit(0.U(32.W))
  val registers = Reg(Vec(16,UInt(32.W)))

  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := erode
        registers(1) := 0.U(32.W)
        registers(2) := 0.U(32.W)
        registers(3) := 0.U(32.W)
        io.writeEnable := true.B
      }
    }
    is(erode) {
      switch(registers(3)) {
        is(0.U(32.W)) {
          io.address := registers(1)
          stateReg := write
        }
        is(1.U(32.W)) {
          io.address := registers(1) * 20.U(32.W)
          stateReg := write
        }
        is(2.U(32.W)) {
          io.address := 19.U(32.W) + (registers(1) * 20.U(32.W))
          stateReg := write
        }
        is(3.U(32.W)) {
          io.address := registers(1) + (19.U(32.W) * 20.U(32.W))
          stateReg := write
        }
      }
    }
    is(write) {
      io.address := registers(2)
      io.dataWrite := 0.U(32.W)
      registers(1) := registers(1) + 1.U(32.W)
      when(registers(1) < 20.U) {
        stateReg := erode
      } .otherwise {
        registers(1) := 0.U(32.W)
        registers(3) := registers(3) + 1.U(32.W)
        stateReg := erode
        when (registers(3) > 3.U(32.W)) {
          io.done := true.B
        }
      }

    }
  }
}
