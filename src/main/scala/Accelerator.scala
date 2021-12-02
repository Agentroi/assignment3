import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start: Bool = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))
  })

  val idle  :: writeBlack :: writeWhite :: readRight :: readBottom :: readLeft :: readTop :: readCenter :: leftBorder :: rightBorder :: topBorder :: bottomBorder :: Nil = Enum(12)
  //  0000  :: 0001       :: 0010       :: 0011      :: 0100       :: 0101     :: 0110    :: 0111       :: 1000       :: 1001        :: 1010      :: 1011

  //States and registers
  val stateReg = RegInit(idle)

  //Support registers
  val addressReg = RegInit(0.U(16.W))
  val dataReg = RegInit(0.U(32.W))
  val registers = Reg(Vec(16,UInt(32.W)))
  val cache = Reg(Vec(19,UInt(32.W)))
  val regCount = RegInit(0.U(32.W))

  val isBlack = Wire(Bool())

  //Default values
  io.writeEnable := false.B
  io.address := 0.U(16.W)
  io.dataWrite := dataReg
  io.done := false.B
  regCount := regCount + 1.U(32.W)
  isBlack := false.B

  switch(stateReg) {

    //We make sure all the registers are initialized correctly
    is(idle) {
      when(io.start) {
        stateReg := topBorder        //For standard run
        registers(1) := 0.U(32.W) //0
        registers(2) := 0.U(32.W) //1
        registers(3) := 0.U(32.W) //0
        registers(4) := 0.U(32.W) //0
        registers(5) := 0.U(32.W) //0
        registers(6) := 0.U(32.W) //0
        registers(7) := 0.U(32.W)
        registers(8) := 2.U(32.W)
        registers(10) := 0.U(32.W)
        registers(11) := 0.U(32.W)


        //Initialize cache
        cache(1) := 1.U(32.W)
        cache(2) := 1.U(32.W)
        cache(3) := 1.U(32.W)
        cache(4) := 1.U(32.W)
        cache(5) := 1.U(32.W)
        cache(6) := 1.U(32.W)
        cache(7) := 1.U(32.W)
        cache(8) := 1.U(32.W)
        cache(9) := 1.U(32.W)
        cache(10) := 1.U(32.W)
        cache(11) := 1.U(32.W)
        cache(12) := 1.U(32.W)
        cache(13) := 1.U(32.W)
        cache(14) := 1.U(32.W)
        cache(15) := 1.U(32.W)
        cache(16) := 1.U(32.W)
        cache(17) := 1.U(32.W)
        cache(18) := 1.U(32.W)

      }
    }

    is(topBorder) {
      io.writeEnable := true.B
      when (registers(1) < 19.U(32.W)) {
        io.address := registers(1) + 400.U(32.W)
        io.dataWrite := 0.U(32.W)
        registers(1) := registers(1) + 1.U(32.W)
      } .otherwise {
        stateReg := rightBorder
      }
    }

    is(rightBorder) {
      io.writeEnable := true.B
      when (registers(2) < 19.U(32.W)) {
        io.address := registers(1) + (registers(2) * 20.U(32.W)) + 400.U(32.W)
        io.dataWrite := 0.U(32.W)
        registers(2) := registers(2) + 1.U(32.W)
      } .otherwise {
        stateReg := bottomBorder
      }
    }

    is(bottomBorder) {
      io.writeEnable := true.B
      when (registers(1) > 0.U(32.W)) {
        io.address := registers(1) + (registers(2) * 20.U(32.W)) + 400.U(32.W)
        io.dataWrite := 0.U(32.W)
        registers(1) := registers(1) - 1.U(32.W)
      } .otherwise {
        stateReg := leftBorder
      }
    }

    is(leftBorder) {
      io.writeEnable := true.B
      when (registers(2) > 0.U(32.W)) {
        io.address :=  400.U(32.W) + (registers(2) * 20.U(32.W))
        io.dataWrite := 0.U(32.W)
        registers(2) := registers(2) - 1.U(32.W)
      } .otherwise {
        registers(1) := 1.U(32.W)
        registers(2) := 1.U(32.W)
        stateReg := readCenter
      }
    }

    //Decides if a pixel should be eroded or if neighbors need to be checked
    is(readCenter) {
      io.writeEnable := false.B
      isBlack := false.B
      registers(6) := 0.U(32.W)

      //The end of the image is reached
      when (registers(2) === 19.U(32.W)) {
        io.done := true.B
      } .otherwise {
        //set address
        registers(4) := registers(1) + (registers(2) * 20.U(32.W))
        io.address := registers(1) + (registers(2) * 20.U(32.W))

        //Pixel is black
        when (io.dataRead === 0.U(32.W)) {
          stateReg := writeBlack
          //set cache index to black
          cache(registers(1)) := 0.U(32.W)

          //Pixel is white
        } .otherwise {

          //Above pixel was black
          when(cache(registers(1)) === 0.U(32.W)) {
            registers(6) := 1.U(32.W)
            stateReg := writeBlack

          //When left pixel was black
          } .elsewhen(registers(1) > 1.U(32.W) && cache(registers(1) - 1.U(32.W)) === 0.U(32.W)) {
            registers(6) := 1.U(32.W)
            stateReg := writeBlack

            //Above pixel was white
          }.otherwise {
            //set address for right of center
            registers(1) := registers(1) + 1.U(32.W)
            registers(4) := (registers(1) + 1.U(32.W)) + registers(2) * 20.U(32.W)

            stateReg := readRight
          }
          //Use reg(8) to remember that current pixel is white
          registers(8) := 1.U(32.W)
        }
      }
    }

    //Writes black to pixel in outImage
    is(writeBlack) {
      //set address
      io.address := registers(4) + 400.U(32.W)
      io.writeEnable := true.B
      io.dataWrite := 0.U(32.W)

      //When the pixel was orignal white
      when (registers(8) =/= 2.U(32.W)) {
        //set cache index to white
        cache(registers(1)) := registers(8)
        registers(8) := 2.U(32.W)
      }

      //If current address is product of being neighbor to a black pixel
      when (registers(5) > 0.U(32.W)) {
        registers(5) := 0.U(32.W)
        cache(registers(1)) := 2.U(32.W)

        //set address for next pixel
        //if x == 18 -> x = 1
        when(registers(1) === 18.U(32.W)) {
          registers(1) := 1.U(32.W)
          registers(2) := registers(2) + 1.U(32.W)

          //otherwise increment
        } .otherwise {
          registers(1) := registers(1) + 1.U(32.W)
        }
        stateReg := readCenter

        //Else when pixel is orignal-black and is not at the edge we write the neighbor black too
      }.elsewhen(registers(6) === 0.U(32.W) && registers(1) < 18.U(32.W)) {
        registers(1) := registers(1) + 1.U(32.W)
        registers(4) := (registers(1) + registers(2) * 20.U(32.W))
        registers(5) := 1.U(32.W)
        stateReg := writeBlack

        //Pixel is original-white or at the edge
      } .otherwise {
        when(registers(1) === 18.U(32.W)) {
          registers(1) := 1.U(32.W)
          registers(2) := registers(2) + 1.U(32.W)

          //otherwise increment
        } .otherwise {
          registers(1) := registers(1) + 1.U(32.W)
        }
        //If the neighbor to the right was checked and was black, then write the neighbor black
        when(registers(7) > 0.U(32.W) && registers(1) < 18.U(32.W)) {
          registers(7) := registers(7) - 1.U(32.W)
          registers(1) := registers(1) + 1.U(32.W)
          registers(4) := (registers(1) + 1.U(32.W)) + registers(2) * 20.U(32.W)
          stateReg := writeBlack

          //else we return to readCenter
        } .otherwise {
          when(registers(7) > 0.U(32.W)) {
            registers(7) := 0.U(32.W)
          }
          stateReg := readCenter
        }
      }
    }

    //writes white to pixel in outImage
    is(writeWhite) {
      //set address
      io.address := registers(4) + 400.U(32.W)
      io.writeEnable := true.B
      io.dataWrite := 255.U(32.W)
      //save pixel as white in cache
      cache(registers(1)) := 1.U(32.W)

      //set address for next pixel
      //if x == 18 -> x = 1
      when (registers(1) === 18.U(32.W)) {
        registers(1) := 1.U(32.W)
        registers(2) := registers(2) + 1.U(32.W)

        //otherwise increment
      } .otherwise {
        registers(1) := registers(1) + 1.U(32.W)
      }

      stateReg := readCenter
    }

    //reads the right neighbor and saves it in reg(7)
    is(readRight) {
      when (registers(1) < 20.U(32.W)) {
        //set address
        io.address := registers(4)

        //when pixel is black
        when(io.dataRead === 0.U(32.W)) {
          registers(7) := 2.U(32.W)
          //set address back to center
          registers(1) := registers(1) - 1.U(32.W)
          registers(4) := (registers(1) - 1.U(32.W)) + registers(2) * 20.U(32.W)

          stateReg := writeBlack
          isBlack := true.B
          //reg(6) symbolizes that the pixel was orignal white
          registers(6) := 1.U(32.W)

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

    //reads the bottom neighbor and saves it to cache
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
          //reg(6) symbolizes that pixel was original white
          registers(6) := 1.U(32.W)

          //when pixel is white
        }
      }
      when (isBlack === false.B) {
        //set address back to center
        registers(2) := registers(2) - 1.U(32.W)
        registers(4) := registers(1) + (registers(2) - 1.U(32.W)) * 20.U(32.W)


        //when pixel is next to left border or left neighbor is unknown
        when (registers(1) === 1.U(32.W) || cache(registers(1) - 1.U(32.W)) === 2.U(32.W)) {
          registers(1) := registers(1) - 1.U(32.W)
          registers(2) := registers(2) - 1.U(32.W)
          registers(4) := (registers(1) - 1.U(32.W)) + (registers(2) - 1.U(32.W)) * 20.U(32.W)
          stateReg := readLeft
          //else when pixel is next to top border or top neighbor is unknown
        } .elsewhen((registers(2) - 1.U(32.W)) === 1.U(32.W) || cache(registers(1)) === 2.U(32.W)) {
          registers(2) := registers(2) - 2.U(32.W)
          registers(4) := registers(1) + (registers(2) - 2.U(32.W)) * 20.U(32.W)
          stateReg := readTop
        } .otherwise {
          stateReg := writeWhite
        }
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
          registers(6) := 1.U(32.W)
        }

      }
      when (isBlack === false.B) {
        //Set address for top if top pixel was also unknown
        when (cache(registers(1) + 1.U(32.W)) === 2.U(32.W)) {
          registers(1) := registers(1) + 1.U(32.W)
          registers(2) := registers(2) - 1.U(32.W)
          registers(4) := (registers(1) + 1.U(32.W)) + (registers(2) - 1.U(32.W)) * 20.U(32.W)
          stateReg := readTop
        } .otherwise {
          //set address back to center
          registers(1) := registers(1) + 1.U(32.W)
          registers(4) := (registers(1) + 1.U(32.W)) + registers(2) * 20.U(32.W)
          stateReg := writeWhite
        }
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
          registers(6) := 1.U(32.W)

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