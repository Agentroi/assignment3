import chisel3._
import chisel3.util._

class AcceleratorRoi extends Module {
  val io = IO(new Bundle {
    val start: Bool = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))
  })

  val idle  :: readRoi :: writeRoi :: Nil = Enum(3)

  //States and registers
  val stateReg = RegInit(idle)

  //Support registers
  val addressReg = RegInit(0.U(16.W))
  val dataReg = RegInit(0.U(32.W))
  val valReg = Reg(Vec(20,UInt(20.W)))
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
        stateReg := readRoi        //For standard run
        registers(1) := 0.U(32.W) //0
        registers(2) := 1.U(32.W) //1
        registers(3) := 0.U(32.W) //0
        registers(4) := 0.U(32.W) //0
        registers(5) := 1.U(32.W) //0
        registers(6) := 0.U(32.W) //0
        registers(10) := 0.U(32.W)
        registers(11) := 0.U(32.W)
      }
    }

    //readRoi reads every pixel and saves them as bits in valReg(line)
    is(readRoi) {
      registers(4) := registers(1) + registers(2) * 20.U(32.W)
      io.address := registers(4)
      when (io.dataRead === 0.U(32.W)) {
        valReg(registers(2)) := Cat(valReg(registers(2)),0.U(1.W))
      } .elsewhen(io.dataRead === 255.U(32.W)) {
        valReg(registers(2)) := Cat(valReg(registers(2)),1.U(1.W))
      }
      io.dataWrite := valReg(registers(1))
      when (registers(1) < 20.U(32.W)) {
        when(registers(2) < 19.U(32.W)) {
          registers(2) := registers(2) + 1.U(32.W)
          stateReg := readRoi
        }.otherwise {
          registers(2) := 0.U(32.W)
          registers(1) := registers(1) + 1.U(32.W)
          stateReg := readRoi
        }
      } .otherwise {
        registers(1) := 19.U(32.W)
        registers(2) := 0.U(32.W)
        stateReg := writeRoi
      }
    }

    //writeRoi writes the content of valRegs
    is(writeRoi) {
      io.writeEnable := true.B
      io.address := (registers(2) * 20.U(32.W)) - registers(1) + 400.U(32.W)
      //when(valReg(registers(2)+1.U(32.W))(19.U(32.W) - registers(1)) === 1.U(1.W)) {
      when(valReg(registers(2))(registers(1)-1.U(32.W)) === 1.U(1.W) &&
        valReg(registers(2)-1.U(32.W))(registers(1)-1.U(32.W)) === 1.U(1.W) &&
        valReg(registers(2)+1.U(32.W))(registers(1)-1.U(32.W)) === 1.U(1.W) &&
        valReg(registers(2))(registers(1)-2.U(32.W)) === 1.U(1.W) &&
        valReg(registers(2))(registers(1)) === 1.U(1.W)) {
        io.dataWrite := 255.U(32.W)
      } .otherwise {
        io.dataWrite := 0.U(32.W)
      }
      when (registers(1) > 0.U(32.W)) {
        when(registers(2) < 21.U(32.W)) {
          registers(2) := registers(2) + 1.U(32.W)
          stateReg := writeRoi
        }.otherwise {
          registers(2) := 1.U(32.W)
          registers(1) := registers(1) - 1.U(32.W)
          stateReg := writeRoi
        }
      } .otherwise {
        io.done := true.B
      }
    }
  }
}
