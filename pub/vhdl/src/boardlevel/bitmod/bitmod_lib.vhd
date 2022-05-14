--============================================================================--
-- Design units : BitMod_Definition (Package declaration and body)
--
-- File name    : bitmod_definition.vhd
--
-- Purpose      : This packages defines the conversion functions used for
--                the BitMod design which are not contained in the
--                IEEE Std_Logic_1164 package.
--
-- Note         : Functions are held as simple as possible.
--
-- Limitations  : None known
--
-- Errors       : None known
--
-- Library      : BitMod_Lib
--
-- Dependencies : IEEE.Std_Logic_1164
--
-- Author       : Sandi Habinc
--                ESTEC Microelectronics and Technology Section (WSM)
--                P. O. Box 299
--                2200 AG Noordwijk
--                The Netherlands
--
-- Copyright    : European Space Agency (ESA) 1995. No part may be reproduced
--                in any form without the prior written permission of ESA.
--
-- Simulator    : Synopsys v. 3.2b, on Sun SPARCstation 10, SunOS 4.1.3
--------------------------------------------------------------------------------
-- Revision list
-- Version Author Date       Changes
--
-- 0.1     SH      1 July 95 New package
--------------------------------------------------------------------------------

library IEEE;
use IEEE.Std_Logic_1164.all;

package BitMod_Definition is

   -----------------------------------------------------------------------------
   -- Converts Integer to Std_Logic_Vector (0 to Size-1).
   -----------------------------------------------------------------------------
   function  To_StdLogicVector(
      constant Arg:                 Integer;
      constant Size:                Integer        := 8)
      return                        Std_Logic_Vector;

   -----------------------------------------------------------------------------
   -- Converts unsigned Std_Logic_Vector to Integer, leftmost bit is MSB
   -- Error message for unknowns (U, X, W, Z, -), converted to 0
   -- Verifies whether vector is too long (> 31 bits)
   -----------------------------------------------------------------------------
   function  To_Integer(
      constant Vector:              Std_Logic_Vector;
      constant VectorName:          String;
      constant HeaderMsg:           String         := "To_Integer";
      constant MsgSeverity:         Severity_Level := Warning)
      return                        Integer;

   -----------------------------------------------------------------------------
   -- Checks the relation between two clocks
   -- If FasterThanRef = True,
   -- then the TestSignal may not have more than Period rising edges 
   -- between two RefSignal rising edges.
   -- If FasterThanRef = False,
   -- then the RefSignal may not have more than Period rising edges 
   -- between two TestSignal rising edges.
   -----------------------------------------------------------------------------
   procedure PeriodCheck(
      variable Violation:       out   X01;
      variable PeriodData:      inout Integer;
      signal   TestSignal:      in    Std_ULogic;
      constant TestSignalName:  in    String         := "";
      signal   RefSignal:       in    Std_ULogic;
      constant RefSignalName:   in    String         := "";
      constant Period:          in    Integer        := 0;
      constant FasterThanRef:   in    Boolean        := True;
      constant CheckEnabled:    in    Boolean        := True;
      constant HeaderMsg:       in    String         := "PeriodCheck:";
      constant XOn:             in    Boolean        := True;
      constant MsgOn:           in    Boolean        := True;
      constant MsgSeverity:     in    Severity_Level := Warning);


   -----------------------------------------------------------------------------
   -- Checks the TestPort stable width w.r.t. number of RefPort rising edges.
   -----------------------------------------------------------------------------
   procedure CheckWidth(
      variable Violation:       out   X01;
      variable PeriodData:      inout Integer;
      signal   TestSignal:      in    Std_ULogic;
      constant TestSignalName:  in    String         := "";
      constant TestSignalLevel: in    Std_ULogic     := '0';
      signal   RefSignal:       in    Std_ULogic;
      constant RefSignalName:   in    String         := "";
      constant Period:          in    Integer        := 0;
      constant CheckEnabled:    in    Boolean        := True;
      constant HeaderMsg:       in    String         := "CheckWidth:";
      constant XOn:             in    Boolean        := True;
      constant MsgOn:           in    Boolean        := True;
      constant MsgSeverity:     in    Severity_Level := Warning);

   -----------------------------------------------------------------------------
   -- Converts Std_ULogic to X01.
   -- Asserts that no unknown value exists at the time of conversion.
   -- Reports with the Severity_Level MsgSeverity the signal name TestName.
   -- No default value for TestName since a resolution conflict can occur 
   -- between the functions with the same name in this package and in package 
   -- IEEE.Std_Logic_1164. X is propagated.
   -----------------------------------------------------------------------------
   function  To_X01(
      constant Test:                Std_ULogic;
      constant TestName:            String;
      constant HeaderMsg:           String         := "To_X01:";
      constant MsgSeverity:         Severity_Level := Warning)
      return                        Std_ULogic;

   -----------------------------------------------------------------------------
   -- Converts Std_Logic_Vector to Std_ULogic_Vector of X01.
   -- Asserts that no unknown value exists at the time of conversion.
   -- Reports with the Severity_Level MsgSeverity the signal name TestName.
   -- No default value for TestName since a resolution conflict can occur 
   -- between the functions with the same name in this package and in package
   -- IEEE.Std_Logic_1164. X is propagated.
   -----------------------------------------------------------------------------
   function  To_X01(
      constant Test:                Std_Logic_Vector;
      constant TestName:            String;
      constant HeaderMsg:           String         := "To_X01:";
      constant MsgSeverity:         Severity_Level := Warning)
      return                        Std_ULogic_Vector;

end BitMod_Definition; --============ End of package header ==================--

package body BitMod_Definition is

   -----------------------------------------------------------------------------
   -- Converts Integer to Std_Logic_Vector (0 to Size-1).
   -----------------------------------------------------------------------------
   function  To_StdLogicVector(
      constant Arg:                 Integer;
      constant Size:                Integer        := 8)
      return                        Std_Logic_Vector is
   begin       
      return Std_Logic_Vector'("000");
      -- TBD
   end To_StdLogicVector;

   -----------------------------------------------------------------------------
   -- Converts unsigned Std_Logic_Vector to Integer
   -- Leftmost bit is most significant
   -- Error message for unknowns (U, X, W, Z, -), which are converted to 0
   -- Verifies whether vector is too long (> 31 bits)
   -----------------------------------------------------------------------------
   function  To_Integer(
      constant Vector:              Std_Logic_Vector;
      constant VectorName:          String;
      constant HeaderMsg:           String         := "To_Integer";
      constant MsgSeverity:         Severity_Level := Warning)
      return                        Integer is
   begin
      return Integer'(0);
      -- TBD
   end To_Integer;

   -----------------------------------------------------------------------------
   -- Checks the relation between two clocks
   -- If FasterThanRef = True,
   -- then the TestSignal may not have more than Period rising edges
   -- between two RefSignal rising edges.
   -- If FasterThanRef = False,
   -- then the RefSignal may not have more than Period rising edges
   -- between two TestSignal rising edges.
   -----------------------------------------------------------------------------
   procedure PeriodCheck(
      variable Violation:       out   X01;
      variable PeriodData:      inout Integer;
      signal   TestSignal:      in    Std_ULogic;
      constant TestSignalName:  in    String         := "";
      signal   RefSignal:       in    Std_ULogic;
      constant RefSignalName:   in    String         := "";
      constant Period:          in    Integer        := 0;
      constant FasterThanRef:   in    Boolean        := True;
      constant CheckEnabled:    in    Boolean        := True;
      constant HeaderMsg:       in    String         := "PeriodCheck:";
      constant XOn:             in    Boolean        := True;
      constant MsgOn:           in    Boolean        := True;
      constant MsgSeverity:     in    Severity_Level := Warning) is
   begin 
      -- TBD
   end PeriodCheck;

   -----------------------------------------------------------------------------
   -- Checks the TestPort stable width w.r.t. number of RefPort rising edges.
   -----------------------------------------------------------------------------
   procedure CheckWidth(
      variable Violation:       out   X01;
      variable PeriodData:      inout Integer;
      signal   TestSignal:      in    Std_ULogic;
      constant TestSignalName:  in    String         := "";
      constant TestSignalLevel: in    Std_ULogic     := '0';
      signal   RefSignal:       in    Std_ULogic;
      constant RefSignalName:   in    String         := "";
      constant Period:          in    Integer        := 0;
      constant CheckEnabled:    in    Boolean        := True;
      constant HeaderMsg:       in    String         := "CheckWidth:";
      constant XOn:             in    Boolean        := True;
      constant MsgOn:           in    Boolean        := True;
      constant MsgSeverity:     in    Severity_Level := Warning) is
   begin 
      -- TBD
   end CheckWidth;

   -----------------------------------------------------------------------------
   -- Converts Std_ULogic to X01.
   -- Asserts that no unknown value exists at the time of conversion.
   -- Reports with the Severity_Level MsgSeverity the signal name TestName.
   -- No default value for TestName since a resolution conflict can occur
   -- between the functions with the same name in this package and in package
   -- IEEE.Std_Logic_1164. X is propagated.
   -----------------------------------------------------------------------------
   function To_X01(
      constant Test:                Std_ULogic;
      constant TestName:            String;
      constant HeaderMsg:           String         := "To_X01:";
      constant MsgSeverity:         Severity_Level := Warning)
      return                        Std_ULogic is
   begin 
      return To_X01(Test);
      -- TBD
   end To_X01;
 
   -----------------------------------------------------------------------------
   -- Converts Std_Logic_Vector to Std_ULogic_Vector of X01.
   -- Asserts that no unknown value exists at the time of conversion.
   -- Reports with the Severity_Level MsgSeverity the signal name TestName.
   -- No default value for TestName since a resolution conflict can occur
   -- between the functions with the same name in this package and in package
   -- IEEE.Std_Logic_1164. X is propagated.
   -----------------------------------------------------------------------------
   function  To_X01(
      constant Test:                Std_Logic_Vector;
      constant TestName:            String;
      constant HeaderMsg:           String         := "To_X01:";
      constant MsgSeverity:         Severity_Level := Warning)
      return                        Std_ULogic_Vector is
   begin
      return Std_ULogic_Vector'("000");
      -- TBD
   end To_X01;

end BitMod_Definition; --================ End of package body ================--
--============================================================================--
-- Design units : BitMod_Timing (Package declaration and body)
--
-- File name    : bitmod_timing.vhd
--
-- Purpose      : In this package, all timing parameters for the Bit Modulator
--                are defined as deferred constants; their value can be modified
--                by recompiling only the package body.
--
-- Note         : The timing figures have been taken from the data sheet for the
--                Bit Modulator dated January 1995. The timing figures
--                are based on 25 pF load on the outputs. The timing parameter
--                names are compliant with Vital Level 0.
--
-- Limitations  : Best case and typical figures have been approximated:  
--                Best case    = Worst case / 4, for path delays,
--                Typical case = Worst case / 2, for path delays.
--
-- Errors:      : None known
--
-- Library      : BitMod_Lib
--
-- Dependencies : ESA.Simulation, ESA.Timing.
--
-- Author       : Sandi Habinc
--                ESTEC Microelectronics and Technology Section (WSM)
--                P.O. Box 299
--                2200 AG Noordwijk
--                The Netherlands
--
-- Copyright    : European Space Agency (ESA) 1995. No part may be reproduced
--                in any form without the prior written permission of ESA.
--
-- Simulator    : Synopsys v. 3.2b, on Sun SPARCstation 10, SunOS 4.1.3
--------------------------------------------------------------------------------
-- Revision list
-- Version Author Date       Changes
--
-- 0.1     SH      1 July 95 New package
--------------------------------------------------------------------------------
 
library ESA;
use ESA.Simulation.all;
use ESA.Timing.all;

package BitMod_Timing is
   -----------------------------------------------------------------------------
   -- Deferred constants for the timing parameters, all values are defined in
   -- the package body.
   --
   -- Input Test is not allowed to change when Reset_N is de-asserted 
   -- (checked in model).
   -----------------------------------------------------------------------------

   -- System signal timing parameters                  Data sheet reference
   constant tperiod_Clk:       TimeArray;              -- TClk
   constant tpw_Clk_posedge:   TimeArray;              -- TCHi
   constant tpw_Clk_negedge:   TimeArray;              -- TCLo

   -- Parameters TSCk and TRes depend on the architectural design and can not
   -- be changed by the user. They are defined in the architecture of the 
   -- functional core.

   -- Internal registers interface timing parameters
   constant tpw_CSN_negedge:   TimeArray;              -- T1
   constant tsetup_D_CSN:      TimeArray;              -- T2
   constant thold_D_CSN:       TimeArray;              -- T3
   constant tpd_CSN_D_negedge: TimeArray;              -- T4 Tri-state modelling
   constant tpd_CSN_D_posedge: TimeArray;              -- T5 -------- " --------
   constant tpd_A_D:           TimeArray;              -- T6

   -- Parameters T7 and T8 depend on the architectural design and can not 
   -- be changed by the user. They are defined in the architecture of the 
   -- functional core.
   
   -- Output interface timing parameters
   constant tpd_Clk_MData:     TimeArray01;            -- T9

end BitMod_Timing; --=============== End of package header ===================--

package body BitMod_Timing is

   -- System signal timing parameters      WC     Typ    BC         Ref.
   constant tperiod_Clk:       TimeArray := (80 ns, 80 ns, 80 ns); -- TClk
   constant tpw_Clk_posedge:   TimeArray := (40 ns, 40 ns, 40 ns); -- TCHi
   constant tpw_Clk_negedge:   TimeArray := (40 ns, 40 ns, 40 ns); -- TCLo
 
   -- Internal registers interface timing parameters
   constant tpw_CSN_negedge:   TimeArray := (40 ns, 30 ns, 20 ns); -- T1
   constant tsetup_D_CSN:      TimeArray := (15 ns, 10 ns,  6 ns); -- T2
   constant thold_D_CSN:       TimeArray := (10 ns,  8 ns,  3 ns); -- T3
   constant tpd_CSN_D_negedge: TimeArray := (30 ns, 15 ns,  8 ns); -- T4
   constant tpd_CSN_D_posedge: TimeArray := (40 ns, 20 ns, 10 ns); -- T5
   constant tpd_A_D:           TimeArray := (30 ns, 15 ns,  8 ns); -- T6

   -- Output interface timing parameter                            -- T9
   constant tpd_Clk_MData:     TimeArray01 := ((25 ns, 24 ns),     -- WC
                                               (11 ns, 13 ns),     -- TC
                                               ( 7 ns,  8 ns));    -- BC

end BitMod_Timing; --================= End of package body ===================--
--============================================================================--
-- Design units : BitMod_Core(Behavioural) (Entity and architecture)
--
-- File name    : bitmod_core.vhd
--
-- Purpose      : This is the functional core of the BitMod implementing
--                all the Bit Modulator functionality, except the 
--                multiplexing of the data bus D which is performed in the 
--                Board-level architecture.
--
--                The Bit Modulator receives serial input data, modulates it
--                and outputs it synchronously with the system clock. Different
--                modes of modulation can be configured in internal registers.
--                The registers can be read and written via a parallel
--                interface. The Bit Modulator optionally performs BIST after
--                reset. For further information see the data sheet for the
--                Bit Modulator.
--
-- Note         : This model is modelled after the design (the design was not
--                synthesized from this model). The model has been extensively
--                verified versus the gate-level design on clock-by-clock
--                basis.
--
--                Unknown values on input signals are handled as follows:
--                  'X' on Reset_N is treated as '1',
--                  'X' on Test    is treated as '0',
--                  'X' on A       is treated as '0',
--                  'X' on D       is treated as '0',
--                  'X' on CS_N    is treated as '1' during write and read
--                  'X' on RW_N    is treated as '1' during write and
--                                            as '0' during read
--                  'X' on SData   is propagated to MData output.
--
--                Unknown values on the Clk input are handled using the
--                Std_Logic_1164 Rising_Edge() and Falling_Edge() functions in
--                the functional core. Clk is checked for unknown values when
--                neither edge is detected, which is also done during reset.
--
--                The SClk input is sampled in the functional core with Clk,
--                and is checked for unknown values in the Board-level
--                architecture.
--
--                All conversions of logical values are performed in the
--                functional core, except for the signals Reset_N and Test,
--                which is performed in the Board-level architecture.
--                The conversion of logical values and X-Checking is generally
--                only performed when the value is needed.
--
--                All scheduling of output delays and timing checking are
--                performed in the Board-level architecture.
--
-- Limitations  : BIST internal function not modelled, only the resulting delay
--                after reset. Manufacturing test is not modelled.
--
--                The modelling is not fully correct w.r.t. a typical RAM I/F, 
--                since some relaxations have been introduced.
--
-- Errors       : None known
--
-- Library      : BitMod_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                BitMod_Lib.BitMod_Definition.
--
--
-- Author       : Sandi Habinc
--                ESTEC Microelectronics and Technology Section (WSM)
--                P. O. Box 299
--                2200 AG Noordwijk
--                The Netherlands
--
-- Copyright    : European Space Agency (ESA) 1995. No part may be reproduced
--                in any form without the prior written permission of ESA.
--
-- Simulator    : Synopsys v. 3.2b, on Sun SPARCstation 10, SunOS 4.1.3
--------------------------------------------------------------------------------
-- Revision list
-- Version Author Date       Changes
--
-- 0.1     SH      1 July 95 New model
--------------------------------------------------------------------------------
 
--------------------------------------------------------------------------------
-- Naming convention: Active low signals are indicated by _N.
-- All external signals have been named as in the data sheet.
--------------------------------------------------------------------------------

library IEEE;
use IEEE.Std_Logic_1164.all;                           -- For data types

entity BitMod_Core is
   generic(
      InstancePath: String := "BitMod_Core:"); -- For assertions
   port(
      -- System signals
      Test0:   in   Std_ULogic;                        -- Test mode
      Clk:     in   Std_ULogic;                        -- Master Clock
      Reset_N: in   Std_ULogic;                        -- Master Reset

      -- Interface to internal registers
      A:       in   Std_Logic_Vector(0 to 1);          -- Address bus
      CS_N:    in   Std_ULogic;                        -- Chip select
      RW_N:    in   Std_ULogic;                        -- Read/write
      D_In:    in   Std_Logic_Vector(0 to 7);          -- Data bus input
      D_Out:   out  Integer range 0 to 255;            -- Data bus output
      DEnable: out  Boolean;                           -- Data bus enable

      -- Serial Interface
      SClk:    in   Std_ULogic;                        -- Serial clock
      SData:   in   Std_ULogic;                        -- Serial input 
      MData:   out  Std_ULogic);                       -- Serial output 

end BitMod_Core; --================== End of entity ==========================--

--=============================== Architecture ===============================--
 
library BitMod_Lib; 
use BitMod_Lib.BitMod_Definition.all;                  -- For custom functions

architecture Behavioural of BitMod_Core is
   -----------------------------------------------------------------------------
   -- Definitions for the internal registers
   -----------------------------------------------------------------------------
   constant RegisterSize:    Integer := 4;
   subtype  DataRange     is Integer range 0 to 255;
   subtype  AddressRange  is Integer range 0 to RegisterSize-1;
   type     RegisterType  is array(AddressRange) of DataRange;
   constant RegisterInit:    RegisterType := (0, 1, 2, 255);

   -----------------------------------------------------------------------------
   -- Local signal declarations.
   -----------------------------------------------------------------------------
   signal   DWrite:          DataRange;                -- Data to write
   signal   AWrite:          AddressRange;             -- Address to write

begin --========================== Architecture ==============================--

   -----------------------------------------------------------------------------
   -- Implementation of all functionality driven by Clk.
   -- Generation of synchronisation marker and header octets;
   -- optional generation of trailer; synchronised writing of internal 
   -- registers data; handling of reset and BIST delay; the SClk input is
   -- sampled within the process.
   -----------------------------------------------------------------------------
   ClkRegion: process(Reset_N, Clk, A)
      variable A_Integer: Integer range 0 to 3;
      variable Registers: RegisterType;
   begin
      if Reset_N = '0' then                            
         -- Asynchronous reset of model
         -- To_X01 on Reset_N is done in Board-level
         Registers := RegisterInit;                    -- Initialize reg. file
         D_Out     <= Registers(A_Integer);
      elsif Rising_Edge(Clk) then   
         -- Rising Clk edge region
         Registers(AWrite) := DWrite;
         D_Out <= Registers(A_Integer);
      elsif Falling_Edge(Clk) then                    
         -- Falling Clk edge region
         -- Not implemented in this VHDL code
      else
         -- Check for unknown Clk value, since the model is not being reset,
         -- and neither rising nor falling Clk edge is detected.
         assert not (Is_X(Clk) and (Now /= 0 ns))      -- No assertions at start
           report InstancePath & " 'X' on Clk input" severity Error;
      end if;
      --------------------------------------------------------------------------
      -- Output register data on the internal bus whenever the address changes
      -- Only convert A to integer when it changes (used elsewhere)
      --------------------------------------------------------------------------
      if A'Event then
         -- X is treated as 0
         A_Integer := To_Integer(A, "A", InstancePath, Error);
         D_Out      <= Registers(A_Integer);
      end if;

   end process ClkRegion;

   -----------------------------------------------------------------------------
   -- Implementation of all asynchronous functionality.
   -- Latching of data to be written into the internal registers.
   -- Generation of external data bus enable. Checks for unknown values 
   -- are done for the input signals.
   -- The modelling is not fully correct w.r.t. a typical RAM I/F, since some
   -- relaxations have been introduced.
   -----------------------------------------------------------------------------
   AsynchronousRegion: process(CS_N, RW_N, Reset_N)
   begin
      if Reset_N = '0' then
         -- To_X01 on Reset_N is done in Board-level
         -- Asynchronous reset of model
         DEnable <= False;
      elsif Rising_Edge(CS_N) then 
         -- End of access
         if To_X01(RW_N, "RW_N", InstancePath, Error)='0' then
            -- Write access to internal registers
            -- X on CS_N is treated as no event (no access)
            -- X on RW_N is treated as 1 (no write access)
            -- X on A and D_In are treated as 0
            AWrite  <= To_Integer(A,    "A", InstancePath, Error);
            DWrite  <= To_Integer(D_In, "D", InstancePath, Error);
         end if;
            DEnable <= False;
      elsif Now /= 0 ns then
         -- Asynchronous behaviour
         -- Enabled for read cycles after Reset
         -- X on RW_N is treated as 0, X on CS_N is treated as 1
         DEnable <= (To_X01(RW_N, "RW_N", InstancePath, Error)='1') and
                    (To_X01(CS_N, "CS_N", InstancePath, Error)='0') and
                    (Reset_N='1');
      end if;
   end process AsynchronousRegion;

end Behavioural; --================ End of architecture ======================--
--============================================================================--
-- Design units : BitMod(BoardLevel) (Entity and architecture)
--
-- File name    : bitmod.vhd
--
-- Purpose      : The Bit Modulator receives serial input data, modulates it
--                and outputs it synchronously with the system clock. Different
--                modes of modulation can be configured in internal registers.
--                The registers can be read and written via a parallel 
--                interface. The Bit Modulator optionally performs BIST after
--                reset. For further information see the data sheet for the 
--                Bit Modulator dated January 1995.
--
-- Note         : All timing parameter names are compliant with Vital Level 0.
--                Selection of Worst, Typical and Best Case timing is performed
--                by the ESA.Simulation.SimCondition type generic SimCondition.
--
--                This model is modelled after the design (the design was not
--                synthesised from this model). The model has been extensively
--                verified versus the gate-level design on a clock-by-clock
--                basis, and is compliant to the data sheet for the
--                Bit Modulator dated January 1995.
--
--                Test is not allowed to change when Reset_N is de-asserted 
--                (checked in model).
--
--                Unknown values on input signals are handled as follows:
--                  'X' on Reset_N is treated as '1',
--                  'X' on Test    is treated as '0',
--                  'X' on A       is treated as '0',
--                  'X' on D       is treated as '0',
--                  'X' on CS_N    is treated as '1' during write and read
--                  'X' on RW_N    is treated as '1' during write and
--                                            as '0' during read
--                  'X' on SData   is propagated to MData output.
--
--                Unknown values on the Clk input are handled using the 
--                Std_Logic_1164 Rising_Edge() and Falling_Edge() functions in 
--                the functional core. Clk is checked for unknown values when 
--                neither edge is detected, which is also done during reset.
--
--                The SClk input is sampled in the functional core with Clk,
--                and is checked for unknown values in the Board-level 
--                architecture.
--
--                All conversions of logical values are performed in the
--                functional core, except for the signals Reset_N and Test, 
--                which is performed in the Board-level architecture.
--                The conversion of logical values and X-Checking is generally 
--                only performed when the value is needed.
--
--                The timing figures have been taken from the data sheet for the
--                Bit Modulator dated January 1995. The timing figures are 
--                based on 25 pF load on the outputs. The timing parameter
--                names are compliant with Vital Level 0.
--
--                All scheduling of output delays and timing checking are 
--                performed in the Board-level architecture.
--
-- Limitations  : BIST internal function not modelled, only the resulting delay
--                after reset. Manufacturing test is not modelled.
--
--                Do NEVER use this timing modelling instead of worst case
--                timing analysis; the timing modelling is not always accurate
--                and is only intended to be used for simulation.
--
--                The timing checks of parameters TRes and T7 are approximated.
--
--                The Vital_Timing package is analysed into BitMod_Lib, since
--                the package has not yet been formally approved by the IEEE.
--
-- Errors       : None known 
--
-- Library      : BitMod_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                ESA.Simulation, 
--                ESA.Timing, 
--                BitMod_Lib.BitMod_Definition,
--                BitMod_Lib.BitMod_Timing.
--
-- Author       : Sandi Habinc
--                ESTEC Microelectronics and Technology Section (WSM)
--                P. O. Box 299
--                2200 AG Noordwijk
--                The Netherlands
--
-- Copyright    : European Space Agency (ESA) 1995. No part may be reproduced
--                in any form without the prior written permission of ESA.
--
-- Simulator    : Synopsys v. 3.2b, on Sun SPARCstation 10, SunOS 4.1.3
--------------------------------------------------------------------------------
-- Revision list
-- Version Author Date       Changes
--
-- 0.1     SH      1 July 95 New model
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Naming convention:
-- All external signals have been named as in the data sheet.
-- Active low signals are indicated by _N.
-- Internal, strength converted signals are named after their new strength, for
-- example _X01.
-- Internal signals without output delays are indicated by _NoTime.
--------------------------------------------------------------------------------

library IEEE;
use IEEE.Std_Logic_1164.all;

library ESA;
use ESA.Simulation.all;                                -- For simulation cond.
use ESA.Timing.all;                                    -- For time arrays 

library BitMod_Lib;
use BitMod_Lib.BitMod_Timing.all;                      -- Default timing

entity BitMod is
   generic(
      SimCondition:      SimConditionType := WorstCase;
      InstancePath:      String           := "BitMod:";
      TimingChecksOn:    Boolean          := False;

      tperiod_Clk:       TimeArray        := tperiod_Clk;       -- TClk
      tpw_Clk_posedge:   TimeArray        := tpw_Clk_posedge;   -- TCHi
      tpw_Clk_negedge:   TimeArray        := tpw_Clk_negedge;   -- TCLo
      tpw_CSN_negedge:   TimeArray        := tpw_CSN_negedge;   -- T1
      tsetup_D_CSN:      TimeArray        := tsetup_D_CSN;      -- T2
      thold_D_CSN:       TimeArray        := thold_D_CSN;       -- T3
      tpd_CSN_D_negedge: TimeArray        := tpd_CSN_D_negedge; -- T4
      tpd_CSN_D_posedge: TimeArray        := tpd_CSN_D_posedge; -- T5
      tpd_A_D:           TimeArray        := tpd_A_D;           -- T6
      tpd_Clk_MData:     TimeArray01      := tpd_Clk_MData);    -- T9

   port(
      -- System signals (4)
      Test:    in     Std_Logic_Vector(0 to 1);        -- Test mode
      Clk:     in     Std_Logic;                       -- Master Clock
      Reset_N: in     Std_Logic;                       -- Master Reset

      -- Interface to internal registers (12)
      A:       in     Std_Logic_Vector(0 to 1);        -- Address bus
      CS_N:    in     Std_Logic;                       -- Chip select
      RW_N:    in     Std_Logic;                       -- Read/write
      D:       inout  Std_Logic_Vector(0 to 7);        -- Bidir. bus
 
      -- Serial Interface (3)
      SClk:    in     Std_Logic;                       -- Serial clock
      SData:   in     Std_Logic;                       -- Serial input
      MData:   out    Std_Logic);                      -- Serial output

end BitMod; --====================== End of entity ===========================--

--=============================== Architecture ===============================--
 
library IEEE;
use IEEE.Vital_Timing.all;

library BitMod_Lib; 
use BitMod_Lib.BitMod_Definition.all;                -- For custom functions
 
architecture BoardLevel of BitMod is
   -----------------------------------------------------------------------------
   -- Component declarations.
   -----------------------------------------------------------------------------
   component BitMod_Core
      generic(
         InstancePath:  String := "BitMod_Core:");    -- For reports
      port(
         -- System signals
         Test0:   in    Std_ULogic;                    -- Test mode
         Clk:     in    Std_ULogic;                    -- Master Clock
         Reset_N: in    Std_ULogic;                    -- Master Reset
 
         -- Interface to internal registers
         A:       in    Std_Logic_Vector(0 to 1);      -- Address bus
         CS_N:    in    Std_ULogic;                    -- Chip select
         RW_N:    in    Std_ULogic;                    -- Read/write
         D_In:    in    Std_Logic_Vector(0 to 7);      -- Data bus input
         D_Out:   out   Integer range 0 to 255;        -- Data bus output
         DEnable: out   Boolean;                       -- Data bus enable

         -- Serial Interface
         SClk:    in    Std_ULogic;                    -- Serial Clock
         SData:   in    Std_ULogic;                    -- Serial input 
         MData:   out   Std_ULogic);                   -- Serial output
   end component;

   -----------------------------------------------------------------------------
   -- Local signal declarations.
   -----------------------------------------------------------------------------
   -- Signal declarations for un-delayed external signals
   signal MData_NoTime:     Std_ULogic;

   -- Signal declarations for internal registers output data generation
   signal DEn_NoTime:       Boolean;
   signal DEn_Delayed:      Boolean;
   signal D_NoTime:         Integer range 0 to 255;
   signal D_Delayed:        Integer range 0 to 255;

   -- Signal declarations for conversion to type X01
   signal Reset_N_X01:      Std_ULogic;
   signal Test_X01:         Std_ULogic_Vector(0 to 1);

   -----------------------------------------------------------------------------
   -- The following timing parameters are architecture dependent and do not 
   -- change for varying simulation conditions.
   -----------------------------------------------------------------------------
   -- Minimum number of Clk cycles that Reset_N must be asserted
   constant tpw_ResetN_negedge: Integer := 3;          -- TRes
   -- Number of Clk clock periods per SClk period
   constant tperiod_SClk:       Integer := 4;          -- TSCk
   -- Minimum number of Clk cycles that CS_N must be de-asserted
   constant tpw_CSN_posedge:    Integer := 4;          -- T7

begin --========================== Architecture ==============================--

   -----------------------------------------------------------------------------
   -- Strength stripping to X01 using Std_Logic_1164 provided subprograms.
   -----------------------------------------------------------------------------
   Reset_N_X01   <= To_X01(Reset_N);                   -- Semi-static signal
   Test_X01      <= To_StdULogicVector(To_X01(Test));  -- Static signal

   -----------------------------------------------------------------------------
   -- Check for unknown values on the SClk input.
   -- The Clk input is checked in the functional core.
   -----------------------------------------------------------------------------
   CheckDynamicInputs: process(SClk)
   begin
      assert not (Is_X(SClk) and (Now /= 0 ns))        -- No assertions at start
         report InstancePath & " 'X' on SClk input"          
         severity Error;
   end process CheckDynamicInputs;

   -----------------------------------------------------------------------------
   -- Check for unknown values on the static inputs and that they only change
   -- during reset. Check for unknown values on Reset_N.
   -- All other signals are checked for unknown values in the functional core.
   -- Activating production test does not change the state of the model, and has
   -- therefore Severity_Level Note.
   -----------------------------------------------------------------------------
   CheckStaticInputs: process(Reset_N_X01, Test_X01)
   begin
      if (Reset_N_X01='1') and (Now /= 0 ns) then
         -- No assertions at start-up or when Reset_N is asserted
         assert (Test_X01(1)='0')
            report InstancePath & " Prod. test not modelled"     severity Note;
         assert not Is_X(Test_X01)                     -- Note: done on a vector
            report InstancePath & " 'X' on Test input"           severity Error;

         -- Check if the static pin changed after reset
         assert not Test_X01'Event
            report InstancePath & " Test changed after reset"    severity Error;

      elsif Reset_N_X01'Event and (Now /= 0 ns) then   -- Check for X on Reset_N
         assert not Is_X(Reset_N_X01)
            report InstancePath & " 'X' on Reset_N input"        severity Error;
      end if;
   end process CheckStaticInputs;

   -----------------------------------------------------------------------------
   -- Timing checks on inputs (setup, hold, period, pulse width).
   -----------------------------------------------------------------------------
   TimingGenerate: if TimingChecksOn generate
      TimingCheck: process(Clk, SClk, D, CS_N, RW_N, Reset_N_X01)
         -- Variables containing information for period checkers
         variable Period_Clk:       VitalPeriodDataType := VitalPeriodDataInit;
         variable Period_CSN:       VitalPeriodDataType := VitalPeriodDataInit;

         -- Variables containing information for setup and hold checkers
         variable Timing_D:         VitalTimingDataType := VitalTimingDataInit;

         -- Variable indicating whether a timing violation has occurred
         variable Violation:        X01                 := '0';

         -- Variables for enabling timing checkers
         variable DataCheckEnabled: Boolean             := False;

         -- Number of falling Clk edges during reset
         variable Period_Reset:     Integer             := tpw_ResetN_negedge;
         -- Number of Clk periods per SCk periods, w.r.t. falling edges
         variable Period_SClk:      Integer             := tperiod_SClk;
         -- Number of falling Clk edges CS_N must be de-asserted
         variable Period_CSN_Clk:   Integer             := tpw_CSN_posedge;
 
      begin
         -----------------------------------------------------------------------
         -- Enabling of various checkers.
         -----------------------------------------------------------------------
         -- Enables the setup and hold checker for D during write operations.
         -- The checker is enabled when both CS_N and RW_N are asserted, until 
         -- the next read access begins, since the data hold constraint is 
         -- longer than the time either CS_N or RW_N is de-asserted after write.
         if ((Falling_Edge(CS_N) and To_X01(RW_N)='0') or
             (Falling_Edge(RW_N) and To_X01(CS_N)='0')) then
            DataCheckEnabled := True;                  -- Enable checker
         elsif (Falling_Edge(CS_N) and To_X01(RW_N)='1') then
            DataCheckEnabled := False;                 -- Disable checker
         end if;
 
         -----------------------------------------------------------------------
         -- Reset_N low time w.r.t. Clk (TRes) 
         -- Asserts that there are at least tpw_ResetN_negedge number of 
         -- falling Clk edges during the assertion of Reset_N.
         -- This timing checker is approximated w.r.t. the data sheet. Only the
         -- number of Clk edges are counted for tpw_ResetN_negedge.
         -----------------------------------------------------------------------
         if (Falling_Edge(Clk) and Reset_N_X01='0' and (Period_Reset>0)) then
            Period_Reset := Period_Reset - 1;
         end if;
         if Falling_Edge(Reset_N_X01) then             -- Reset begins
            Period_Reset := tpw_ResetN_negedge;
         elsif Rising_Edge(Reset_N_X01) then           -- Reset ends
            assert (Period_Reset = 0)
               report InstancePath & " Signal width too short on Reset_N"
                  severity Error;
         end if;
  
         -----------------------------------------------------------------------
         -- The internal register interface is checked for illegal events. (T8) 
         -----------------------------------------------------------------------
         -- RW_N may not change when CS_N is asserted.
         assert not (RW_N'Event and To_X01(CS_N)='0' and Reset_N_X01='1')
            report InstancePath & " RW_N event while CS_N asserted"
               severity Warning;

         -----------------------------------------------------------------------
         -- Checkers using custom made subprograms.
         -----------------------------------------------------------------------
         -- SClk period (TSCk)
         -- Checks the relation between the two clock inputs SClk and Clk.
         PeriodCheck(
            Violation         => Violation,
            PeriodData        => Period_SClk,
            TestSignal        => SClk,
            TestSignalName    => "SClk",
            RefSignal         => Clk,
            RefSignalName     => "Clk",
            Period            => tperiod_SClk,
            FasterThanRef     => False,
            CheckEnabled      => Reset_N_X01='1',
            HeaderMsg         => InstancePath,
            XOn               => False,
            MsgOn             => True,
            MsgSeverity       => Warning);

         -- CS_N de-assertion width (T7)
         CheckWidth(
            Violation         => Violation,
            PeriodData        => Period_CSN_Clk,
            TestSignal        => CS_N,
            TestSignalName    => "CS_N",
            TestSignalLevel   => '1',
            RefSignal         => Clk,
            RefSignalName     => "Clk",
            CheckEnabled      => Reset_N_X01='1',
            HeaderMsg         => InstancePath,
            XOn               => False,
            MsgOn             => True,
            MsgSeverity       => Warning);

         -----------------------------------------------------------------------
         -- Timing checkers using Vital subprograms. 
         -----------------------------------------------------------------------
         -- Clk period, high and low times (TClk, TCLo, TCHi)
         VitalPeriodPulseCheck(
            Violation         => Violation,
            PeriodData        => Period_Clk,
            TestSignal        => Clk,
            TestSignalName    => "Clk",
            Period            => tperiod_Clk(SimCondition),
            PulseWidthHigh    => tpw_Clk_posedge(SimCondition),
            PulseWidthLow     => tpw_Clk_negedge(SimCondition),
            CheckEnabled      => True,
            HeaderMsg         => InstancePath,
            XOn               => False,
            MsgOn             => True,
            MsgSeverity       => Warning);

         -- CS_N assertion width during write access (T1)
         VitalPeriodPulseCheck(
            Violation         => Violation,
            PeriodData        => Period_Clk,
            TestSignal        => CS_N,
            TestSignalName    => "CS_N",
            PulseWidthLow     => tpw_CSN_negedge(SimCondition),
            CheckEnabled      => To_X01(RW_N)='0',
            HeaderMsg         => InstancePath,
            XOn               => False,
            MsgOn             => True,
            MsgSeverity       => Error);

         -- D setup & hold w.r.t. CS_N (T2, T3)
         VitalSetupHoldCheck(
            Violation         => Violation,
            TimingData        => Timing_D,
            TestSignal        => D,
            TestSignalName    => "D",
            RefSignal         => CS_N,
            RefSignalName     => "CS_N",
            SetupHigh         => tsetup_D_CSN(SimCondition),
            SetupLow          => tsetup_D_CSN(SimCondition),
            HoldHigh          => thold_D_CSN(SimCondition),
            HoldLow           => thold_D_CSN(SimCondition),
            CheckEnabled      => DataCheckEnabled,
            RefTransition     => 'R',
            HeaderMsg         => InstancePath,
            XOn               => False,
            MsgOn             => True,
            MsgSeverity       => Warning);

      end process TimingCheck;
   end generate TimingGenerate;

   -----------------------------------------------------------------------------
   -- Assignment of output delays.
   -----------------------------------------------------------------------------
   -- Generation of tristate or drive for the external data bus. D_NoTime is
   -- delayed w.r.t. the address. DEn_NoTime is delayed, with different timing
   -- for tristate. The D assignment includes an Integer to Std_LogicVector 
   -- conversion.
 
   DEn_Delayed <= transport DEn_NoTime after tpd_CSN_D_negedge(SimCondition)
                     when DEn_NoTime else                                 -- T4
                  DEn_NoTime after tpd_CSN_D_posedge(SimCondition);       -- T5
 
   D_Delayed   <= transport D_NoTime   after tpd_A_D(SimCondition);       -- T6
 
   D           <= To_StdLogicVector(D_Delayed, 8)
                     when DEn_Delayed else
                  (others => 'Z');

   MData       <= MData_NoTime after VitalCalcDelay(                      -- 9
                                        NewVal => MData_NoTime,
                                        OldVal => MData_NoTime'Last_Value,
                                        Delay  => tpd_Clk_MData(SimCondition));

   -----------------------------------------------------------------------------
   -- Instantiation of components.
   -----------------------------------------------------------------------------
   -- The buses A and D are not converted to the unresolved type 
   -- Std_ULogic_Vector since a conversion is done to the type Integer in the 
   -- functional core when the signal values are used.
   --
   -- Only the LSB of the Test bus is used in the functional core. The MSB is
   -- handled in the CheckStaticInputs process.

   FunctionalCore: BitMod_Core
      generic map(
         InstancePath => InstancePath)
      port map(
         Test0        => Test_X01(0),                  -- Only LSB used in core
         Clk          => Clk,                          -- Falling/Rising_Edge
         Reset_N      => Reset_N_X01,
         A            => A,                            -- To_X01 made in core
         CS_N         => CS_N,                         -- To_X01 made in core
         RW_N         => RW_N,                         -- To_X01 made in core
         D_In         => D,                            -- To_X01 made in core
         D_Out        => D_NoTime,                     -- Integer
         DEnable      => DEn_NoTime,
         SClk         => SClk,                         -- Falling/Rising_Edge
         SData        => SData,                        -- To_X01 made in core
         MData        => MData_NoTime);

end BoardLevel; --================ End of architecture =======================--
--============================================================================--
-- Design units : BitMod_Configuration (Configuration)
--
-- File name    : bitmod_configuration.vhd
--
-- Purpose      : Board-level simulation for verification of a board
--                containing a Bit Modulator component.
--
-- Note         : 
--
-- Limitations  : 
--
-- Errors       : None known
--
-- Library      : BitMod_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                ESA.Simulation,
--                BitMod_Lib.BitMod.
--
-- Author       : Sandi Habinc
--                ESTEC Microelectronics and Technology Section (WSM)
--                P. O. Box 299
--                2200 AG Noordwijk
--                The Netherlands
--
-- Copyright    : European Space Agency (ESA) 1995. No part may be reproduced
--                in any form without the prior written permission of ESA.
--
-- Simulator    : Synopsys v. 3.2b, on Sun SPARCstation 10, SunOS 4.1.3
--------------------------------------------------------------------------------
-- Revision list
-- Version Author Date       Changes
--
-- 0.1     SH      1 July 95 New configuration
--------------------------------------------------------------------------------
 
library BitMod_Lib;
configuration BitMod_Configuration of BitMod is
   for BoardLevel
      for FunctionalCore: BitMod_Core
         use entity BitMod_Lib.BitMod_Core(Behavioural)
            generic map(InstancePath => InstancePath);
      end for;
   end for;
end BitMod_Configuration; --============== End of configuration ==============-- 
