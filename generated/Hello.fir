;buildInfoPackage: chisel3, version: 3.3.2, scalaVersion: 2.12.11, sbtVersion: 1.3.10
circuit Hello : 
  module DataMemory : 
    input clock : Clock
    input reset : Reset
    output io : {flip address : UInt<16>, dataRead : UInt<32>, flip writeEnable : UInt<1>, flip dataWrite : UInt<32>, flip testerEnable : UInt<1>, flip testerAddress : UInt<16>, testerDataRead : UInt<32>, flip testerWriteEnable : UInt<1>, flip testerDataWrite : UInt<32>}
    
    cmem memory : UInt<32>[65536] @[DataMemory.scala 18:20]
    when io.testerEnable : @[DataMemory.scala 22:24]
      read mport _T = memory[io.testerAddress], clock @[DataMemory.scala 24:37]
      io.testerDataRead <= _T @[DataMemory.scala 24:23]
      io.dataRead <= UInt<32>("h00") @[DataMemory.scala 26:17]
      when io.testerWriteEnable : @[DataMemory.scala 27:32]
        write mport _T_1 = memory[io.testerAddress], clock
        _T_1 <= io.testerDataWrite
        io.testerDataRead <= io.testerDataWrite @[DataMemory.scala 30:25]
        skip @[DataMemory.scala 27:32]
      skip @[DataMemory.scala 22:24]
    else : @[DataMemory.scala 32:16]
      read mport _T_2 = memory[io.address], clock @[DataMemory.scala 34:31]
      io.dataRead <= _T_2 @[DataMemory.scala 34:17]
      io.testerDataRead <= UInt<32>("h00") @[DataMemory.scala 36:23]
      when io.writeEnable : @[DataMemory.scala 37:26]
        write mport _T_3 = memory[io.address], clock
        _T_3 <= io.dataWrite
        io.dataRead <= io.dataWrite @[DataMemory.scala 40:19]
        skip @[DataMemory.scala 37:26]
      skip @[DataMemory.scala 32:16]
    
  module Hello : 
    input clock : Clock
    input reset : UInt<1>
    output io : {done : UInt<1>, flip start : UInt<1>, flip testerDataMemEnable : UInt<1>, flip testerDataMemAddress : UInt<16>, testerDataMemDataRead : UInt<32>, flip testerDataMemWriteEnable : UInt<1>, flip testerDataMemDataWrite : UInt<32>}
    
    inst dataMemory of DataMemory @[Hello.scala 33:26]
    dataMemory.clock <= clock
    dataMemory.reset <= reset
    reg stateReg : UInt<2>, clock with : (reset => (reset, UInt<2>("h00"))) @[Hello.scala 37:25]
    reg addressReg : UInt<16>, clock with : (reset => (reset, UInt<16>("h00"))) @[Hello.scala 40:27]
    reg dataReg : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[Hello.scala 41:24]
    dataMemory.io.writeEnable <= UInt<1>("h00") @[Hello.scala 44:29]
    dataMemory.io.address <= UInt<16>("h00") @[Hello.scala 45:25]
    dataMemory.io.dataWrite <= dataReg @[Hello.scala 46:27]
    io.done <= UInt<1>("h00") @[Hello.scala 47:11]
    node _T = eq(UInt<2>("h00"), stateReg) @[Conditional.scala 37:30]
    when _T : @[Conditional.scala 40:58]
      when io.start : @[Hello.scala 52:22]
        stateReg <= UInt<2>("h01") @[Hello.scala 53:18]
        addressReg <= UInt<16>("h00") @[Hello.scala 54:20]
        skip @[Hello.scala 52:22]
      skip @[Conditional.scala 40:58]
    else : @[Conditional.scala 39:67]
      node _T_1 = eq(UInt<2>("h01"), stateReg) @[Conditional.scala 37:30]
      when _T_1 : @[Conditional.scala 39:67]
        dataMemory.io.address <= addressReg @[Hello.scala 59:29]
        node _T_2 = bits(dataMemory.io.dataRead, 7, 0) @[Hello.scala 60:56]
        node _T_3 = not(_T_2) @[Hello.scala 60:33]
        node _T_4 = cat(UInt<24>("h00"), _T_3) @[Cat.scala 29:58]
        dataReg <= _T_4 @[Hello.scala 60:15]
        stateReg <= UInt<2>("h02") @[Hello.scala 61:16]
        skip @[Conditional.scala 39:67]
      else : @[Conditional.scala 39:67]
        node _T_5 = eq(UInt<2>("h02"), stateReg) @[Conditional.scala 37:30]
        when _T_5 : @[Conditional.scala 39:67]
          node _T_6 = add(addressReg, UInt<16>("h0190")) @[Hello.scala 65:43]
          node _T_7 = tail(_T_6, 1) @[Hello.scala 65:43]
          dataMemory.io.address <= _T_7 @[Hello.scala 65:29]
          dataMemory.io.writeEnable <= UInt<1>("h01") @[Hello.scala 66:33]
          node _T_8 = add(addressReg, UInt<16>("h01")) @[Hello.scala 67:32]
          node _T_9 = tail(_T_8, 1) @[Hello.scala 67:32]
          addressReg <= _T_9 @[Hello.scala 67:18]
          node _T_10 = eq(addressReg, UInt<16>("h018f")) @[Hello.scala 68:23]
          when _T_10 : @[Hello.scala 68:40]
            stateReg <= UInt<2>("h03") @[Hello.scala 69:18]
            skip @[Hello.scala 68:40]
          else : @[Hello.scala 70:19]
            stateReg <= UInt<2>("h01") @[Hello.scala 71:18]
            skip @[Hello.scala 70:19]
          skip @[Conditional.scala 39:67]
        else : @[Conditional.scala 39:67]
          node _T_11 = eq(UInt<2>("h03"), stateReg) @[Conditional.scala 37:30]
          when _T_11 : @[Conditional.scala 39:67]
            io.done <= UInt<1>("h01") @[Hello.scala 76:15]
            stateReg <= UInt<2>("h03") @[Hello.scala 77:16]
            skip @[Conditional.scala 39:67]
    dataMemory.io.testerAddress <= io.testerDataMemAddress @[Hello.scala 82:31]
    io.testerDataMemDataRead <= dataMemory.io.testerDataRead @[Hello.scala 83:28]
    dataMemory.io.testerDataWrite <= io.testerDataMemDataWrite @[Hello.scala 84:33]
    dataMemory.io.testerEnable <= io.testerDataMemEnable @[Hello.scala 85:30]
    dataMemory.io.testerWriteEnable <= io.testerDataMemWriteEnable @[Hello.scala 86:35]
    
