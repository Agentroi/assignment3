import chisel3._
import chisel3.util._

class AcceleratorA extends Module {
  val io = IO(new Bundle {
    val start: Bool = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))
  })

  val idle  :: writeBlack :: writeWhite :: readRight :: readBottom :: readLeft :: readTop :: readCenter :: leftBorder :: rightBorder :: topBorder :: bottomBorder :: Nil = Enum(12)
  //  000   :: 001        :: 010        :: 011       :: 100        :: 101      :: 110     :: 111

  //States and registers
  val stateReg = RegInit(idle)

  //Support registers
  val addressReg = RegInit(0.U(16.W))
  val dataReg = RegInit(0.U(32.W))
  val registers = Reg(Vec(16,UInt(32.W)))
  val regCount = RegInit(0.U(32.W))

  val isBlack = Wire(Bool())
  val writingBlack = Wire(Bool())
  val writingWhite = Wire(Bool())

  //Default values
  io.writeEnable := false.B
  io.address := 0.U(16.W)
  io.dataWrite := dataReg
  io.done := false.B
  regCount := regCount + 1.U(32.W)
  isBlack := false.B
  writingBlack := false.B
  writingWhite := false.B

  switch(stateReg) {

    //We make sure all the registers are initialized correctly
    is(idle) {
      when(io.start) {
        stateReg := readCenter        //For standard run
        registers(1) := 0.U(32.W) //0
        registers(2) := 0.U(32.W) //1
        registers(3) := 0.U(32.W) //0
        registers(4) := 0.U(32.W) //0
        registers(5) := 1.U(32.W) //0
        registers(6) := 0.U(32.W) //0
        registers(10) := 0.U(32.W)
        registers(11) := 0.U(32.W)
      }
    }

    is(topBorder) {
      when (registers(1) < 20.U(32.W)) {
        io.address := registers(1) + 400.U(32.W)
        io.writeEnable := true.B
        io.dataWrite := 0.U(32.W)
        registers(1) := registers(1) + 1.U(32.W)
      } .otherwise {
        io.writeEnable := false.B
        registers(2) := registers(2) + 1.U(32.W)
        stateReg := rightBorder
      }
    }

    is(rightBorder) {
      when (registers(2) < 19.U(32.W)) {
        io.address := 19.U(32.W) + (registers(2) * 20.U(32.W)) + 400.U(32.W)
        io.writeEnable := true.B
        io.dataWrite := 0.U(32.W)
        registers(2) := registers(2) + 1.U(32.W)
      } .otherwise {
        io.writeEnable := false.B
        registers(2) := registers(2) + 1.U(32.W)
        stateReg := bottomBorder
      }
    }

    is(bottomBorder) {
      when (registers(1) >= 0.U(32.W)) {
        io.address := registers(1) + (registers(2) * 20.U(32.W)) + 400.U(32.W)
        io.writeEnable := true.B
        io.dataWrite := 0.U(32.W)
        registers(2) := registers(1) - 1.U(32.W)
      } .otherwise {
        io.writeEnable := false.B
        registers(2) := registers(2) - 1.U(32.W)
        stateReg := bottomBorder
      }
    }

    is(leftBorder) {
      when (registers(2) > 0.U(32.W)) {
        io.address :=  400.U(32.W) + (registers(2) * 20.U(32.W))
        io.writeEnable := true.B
        io.dataWrite := 0.U(32.W)
        registers(2) := registers(2) - 1.U(32.W)
      } .otherwise {
        io.writeEnable := false.B
        registers(1) := 1.U(32.W)
        stateReg := readCenter
      }
    }

    //readRoi reads every pixel and saves them as bits in valReg(line)
    is(readCenter) {
      io.writeEnable := false.B
      when (registers(2) === 19.U(32.W)) {
        io.done := true.B
      } .otherwise {
        writingWhite := false.B
        writingBlack := false.B
        //set address
        registers(4) := registers(1) + (registers(2) * 20.U(32.W))
        io.address := registers(1) + (registers(2) * 20.U(32.W))

        //Pixel is black
        when (io.dataRead === 0.U(32.W)) {
          stateReg := writeBlack

        //Pixel is white
        } .otherwise {
          //set address for right of center
          registers(1) := registers(1) + 1.U(32.W)
          registers(4) := (registers(1) + 1.U(32.W)) + registers(2) * 20.U(32.W)

          stateReg := readRight
        }
      }
    }

    //Writes black to
    is(writeBlack) {
      writingBlack := true.B
      io.address := registers(4) + 400.U(32.W)
      io.writeEnable := true.B
      io.dataWrite := 0.U(32.W)

      //set address for next pixel
      //if x == 19 -> x = 0
      when (registers(1) === 18.U(32.W)) {
        registers(1) := 1.U(32.W)
        registers(2) := registers(2) + 1.U(32.W)
        //otherwise increment
      } .otherwise {
        registers(1) := registers(1) + 1.U(32.W)
      }

      isBlack := false.B
      stateReg := readCenter
    }

    is(writeWhite) {
      writingWhite := true.B
      io.address := registers(4) + 400.U(32.W)
      io.writeEnable := true.B
      io.dataWrite := 255.U(32.W)


      //set address for next pixel
      //if x == 19 -> x = 0
      when (registers(1) === 18.U(32.W)) {
        registers(1) := 1.U(32.W)
        registers(2) := registers(2) + 1.U(32.W)
        //otherwise increment
      } .otherwise {
        registers(1) := registers(1) + 1.U(32.W)
      }

      stateReg := readCenter
    }

    is(readRight) {
      when (registers(1) < 20.U(32.W)) {
        //Load dataread to register
        io.address := registers(4)

        //when pixel is black
        when(io.dataRead === 0.U(32.W)) {
          //set address back to center
          registers(1) := registers(1) - 1.U(32.W)
          registers(4) := (registers(1) - 1.U(32.W)) + registers(2) * 20.U(32.W)

          stateReg := writeBlack
          isBlack := true.B

        }
      }
      //when pixel is white
      when (isBlack === false.B) {
        //Set address for bottom
        registers(1) := registers(1) - 1.U(32.W)
        registers(2) := registers(2) + 1.U(32.W)
        registers(4) := (registers(1) - 1.U(32.W)) + (registers(2) + 1.U(32.W)) * 20.U(32.W)

        stateReg := readBottom
      }
    }

    is(readBottom) {
      when(registers(2) < 20.U(32.W)) {
        //Load dataread to register
        io.address := registers(4)

        //when pixel is black
        when(io.dataRead === 0.U(32.W)) {
          //set address back to center
          registers(2) := registers(2) - 1.U(32.W)
          registers(4) := registers(1) + (registers(2) - 1.U(32.W)) * 20.U(32.W)

          stateReg := writeBlack
          isBlack := true.B

          //when pixel is white
        }
      }
      when (isBlack === false.B) {
        //Set address for left
        registers(1) := registers(1) - 1.U(32.W)
        registers(2) := registers(2) - 1.U(32.W)
        registers(4) := (registers(1) - 1.U(32.W)) + (registers(2) - 1.U(32.W)) * 20.U(32.W)

        stateReg := readLeft
      }
    }

    is(readLeft) {
      when(registers(1) >= 0.U(32.W)) {
        //Load dataread to register
        io.address := registers(4)

        //when pixel is black
        when(io.dataRead === 0.U(32.W)) {
          //set address back to center
          registers(1) := registers(1) + 1.U(32.W)
          registers(4) := (registers(1) + 1.U(32.W)) + registers(2) * 20.U(32.W)

          stateReg := writeBlack
          isBlack := true.B
        }

      }
      when (isBlack === false.B) {
        //Set address for top
        registers(1) := registers(1) + 1.U(32.W)
        registers(2) := registers(2) - 1.U(32.W)
        registers(4) := (registers(1) + 1.U(32.W)) + (registers(2) - 1.U(32.W)) * 20.U(32.W)

        stateReg := readTop
      }
    }

    is(readTop) {
      when(registers(2) >= 0.U(32.W)) {
        //Load dataread to register
        io.address := registers(4)

        //when pixel is black
        when(io.dataRead === 0.U(32.W)) {
          stateReg := writeBlack
          isBlack := true.B

          //set address back to center
          registers(2) := registers(2) + 1.U(32.W)
          registers(4) := registers(1) + (registers(2) + 1.U(32.W)) * 20.U(32.W)
        }
      }
      //when pixel is white
      when (isBlack === false.B) {
        //set address back to center
        registers(2) := registers(2) + 1.U(32.W)
        registers(4) := registers(1) + (registers(2) + 1.U(32.W)) * 20.U(32.W)
        stateReg := writeWhite
      }
    }
  }
}
