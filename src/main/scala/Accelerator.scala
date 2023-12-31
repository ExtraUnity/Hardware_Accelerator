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

  //State enum and register
  val idle :: x_loop :: y_loop :: black_check :: check_left :: check_right :: check_down :: check_up :: Nil = Enum (8)
  val stateReg = RegInit(idle)
  val x = RegInit(0.U(16.W))
  val y = RegInit(0.U(16.W))
  val dataReg = RegInit(0.U(32.W))

  //Default values
  io.writeEnable := false.B
  io.address := 0.U(32.W)
  io.dataWrite := 0.U(32.W)
  io.done := 0.U(32.W)

  //FSMD switch
  switch(stateReg) {
    is(idle) {
      when(io.start) {
        io.done := false.B
        x := 0.U(16.W)
        stateReg := x_loop
      }
    }

    is(x_loop) {
      when(x > 19.U(16.W)) {
        io.done := true.B
        stateReg := idle
      } .otherwise {
        y := 0.U(16.W)
        stateReg := y_loop
      }
    }

    is(y_loop) {
      when(y > 19.U(16.W)) {
        x := x + 1.U(16.W)
        stateReg := x_loop

      } .otherwise {
        when(x === 0.U(16.W) || x === 19.U(16.W) || y === 0.U(16.W) || y === 19.U(16.W)) {
        io.address := y*20.U(16.W)+x+400.U(16.W)
        io.writeEnable := true.B
        io.dataWrite := 0.U(32.W)
        y := y + 1.U(16.W)
        stateReg := y_loop

        } .otherwise {
          io.address := y*20.U(16.W) + x
          dataReg := io.dataRead
          stateReg := black_check
        }
      }
    }

    is(black_check) {
      when(dataReg === 0.U(32.W)) {
        io.address := y*20.U(16.W)+x+400.U(16.W)
        io.writeEnable := true.B
        io.dataWrite := 0.U(32.W)
        y := y + 1.U(16.W)
        stateReg := y_loop

      } .otherwise {
        io.address := y*20.U(16.W)+x-1.U(16.W)
        dataReg := io.dataRead
        stateReg := check_left
      }
    }

    is(check_left) {
      when(dataReg === 0.U(32.W)) {
        io.address := y*20.U(16.W)+x+400.U(16.W)
        io.writeEnable := true.B
        io.dataWrite := 0.U(32.W)
        y := y + 1.U(16.W)
        stateReg := y_loop

      } .otherwise {
        io.address := y*20.U(16.W)+x+1.U(16.W)
        dataReg := io.dataRead
        stateReg := check_right
      }
    }

    is(check_right) {
      when(dataReg === 0.U(32.W)) {
        io.address := y*20.U(16.W)+x+400.U(16.W)
        io.writeEnable := true.B
        io.dataWrite := 0.U(32.W)
        y := y + 1.U(16.W)
        stateReg := y_loop

      } .otherwise {
        io.address := (y+1.U(16.W))*20.U(16.W)+x
        dataReg := io.dataRead
        stateReg := check_down
      }
    }

    is(check_down) {
      when(dataReg === 0.U(32.W)) {
        io.address := y*20.U(16.W)+x+400.U(16.W)
        io.writeEnable := true.B
        io.dataWrite := 0.U(32.W)
        y := y + 1.U(16.W)
        stateReg := y_loop

      } .otherwise {
        io.address := (y-1.U(16.W))*20.U(16.W)+x
        dataReg := io.dataRead
        stateReg := check_up
      }
    }

    is(check_up) {
        io.address := y*20.U(16.W)+x+400.U(16.W)
        io.writeEnable := true.B
        y := y + 1.U(16.W)
        stateReg := y_loop
      
        when(dataReg === 0.U(32.W)) {
          io.dataWrite := 0.U(32.W)
        } .otherwise {   
          io.dataWrite := 255.U(32.W)
        }
    }


  }
}
