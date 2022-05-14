--============================================================================--
-- Design units : MISR_Definition (Package declaration and body)
--
-- File name    : misr_definition.vhd
--
-- Purpose      : This packages defines the MISR subprogram used by the
--                Test Generator for the verification of the Bit Modulator.
--
-- Note         :
--
-- Limitations  : None known
--
-- Errors       : None known
--
-- Library      : BitMod_TB_Lib
--
-- Dependencies : IEEE.Std_Logic_1164
--
-- Author       : Sandi Habinc, 
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
 
--============================================================================--
-- Multiple-Input Signature Register (MISR)
-- This procedure implements a variable length MISR. The length is determined by
-- the length of Input, ranging from 4 to 100. The Input can be sampled on 
-- either Clk edge or both, delayed by Sense, selected with the Rising and
-- Falling parameters. If neither option is selected, events on Input will 
-- determine the sampling point. Events happening in the same simulation cycle,
-- differing only in delta cycles, will be sampled when the last event has 
-- occurred, and the MISR will then be shifted.
--
-- The Reset input will reset the MISR to all-ones. When sampling is made with 
-- Clk, it will re-start on the next relevant edge after the asserting edge of 
-- Reset. If Reset is detected between an Clk edge and the sampling point, the 
-- MISR will be reset and the sample will be ignored. When asynchronous sampling
-- is used, the next event on Input will be the first sample after reset. The 
-- MISR signal can be read at any point and should be compared with a 
-- predetermined signature.
--
-- The MISR is implemented as a primitive polynomial with up to five terms. The
-- terms are taken from the text book Built-In Test for VLSI: Pseudorandom 
-- Techniques, by Bardell et al. The elements in the Input vector are expanded 
-- to four bits, each Std_Logic value having a unique bit pattern, 
-- the intermediate vector is then divided in four and each part is shifted into
-- the MISR separately. The procedure can be used as a concurrent subprogram, 
-- not needing any surrounding process or block.
--
-- Inputs:  Clk,   sample clock used with Rising and Falling
--          Reset, reset of MISR when an event is detected and Reset is True
--          Input, input vector to the MISR, same length as MISR
--          Rising/Falling:
--          False  False  sample at each Input event
--          False  True   sample Input after Sense on falling Clk edge
--          True   False  sample Input after Sense on rising Clk edge
--          True   True   sample Input after Sense on rising or falling Clk edge
--          Sense, positive time after the Clk edge when Input is sampled
--          HeaderMsg, message header
-- In/Outs: MISR, Multiple-Input Signature Register
--
-- Author:  Sandi Habinc, ESTEC Microelectronics and Technology Section (WSM)
--          P.O. Box 299, 2200 AG Noordwijk, The Netherlands
--------------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
 
package MISR_Definition is
   procedure MISR(
      signal   Clk:       in    Std_ULogic;            -- Sample clock
      signal   Reset:     in    Boolean;               -- Reset of MISR
      signal   Input:     in    Std_Logic_Vector;      -- Input vector
      signal   MISR:      inout Std_Logic_Vector;      -- MISR
      constant Rising:    in    Boolean := True;       -- See above
      constant Falling:   in    Boolean := False;      -- See above
      constant HeaderMsg: in    String  := "MISR:";    -- Message header
      constant Sense:     in    Time    := 0 ns);      -- Sense time after clock

end MISR_Definition; --=============== End of package header =================--

package body MISR_Definition is

   -----------------------------------------------------------------------------
   -- Local declarations of minimum and maximum MISR lengths.
   -----------------------------------------------------------------------------
   constant MaxLen:   Integer := 100;
   constant MinLen:   Integer := 4;

   -----------------------------------------------------------------------------
   -- Local declarations defining subtypes and types needed for the definition
   -- of the Std_Logic to 4bit vector transfer function. Each Std_Logic value
   -- has a unique 4bit vector associated.
   -----------------------------------------------------------------------------
   subtype  Index  is Integer range 0 to 3;            -- Definition of table
   subtype  Vector is Std_Logic_Vector(Index);         -- with a 4bit vector for
   type     VTable is array (Std_Logic) of Vector;     -- each Std_Logic value
   constant Std4:     VTable := ('U' => "0001", 'X' => "0010", '0' => "0100",
                                 '1' => "1000", 'Z' => "0011", 'W' => "0110",
                                 'L' => "1100", 'H' => "0111", '-' => "1110");

   -----------------------------------------------------------------------------
   -- Expands every Std_Logic_Vector element to four bits returning a
   -- Std_Logic_Vector with four times the length of the input.
   -- Input:  V Std_Logic_Vector defined (0 to n)
   -- Output:   Std_Logic_Vector defined (0 to n) with same length as V
   -----------------------------------------------------------------------------
   function To01(V:         Std_Logic_Vector;
                 HeaderMsg: String := "MISR:") return Std_Logic_Vector is
      variable R: Std_Logic_Vector(0 to (V'Length*4-1));
   begin
      assert (V'Length<=MaxLen) and (V'Length>=MinLen) -- Check length
         report HeaderMsg&" Only vectors of Length 4 to 100 are supported."
         severity Failure;

      for i in V'Range loop
         R(i*4+0 to i*4+3):= Std4(V(i));               -- Expand input
      end loop;
      return R;
   end To01;

   -----------------------------------------------------------------------------
   -- Logical and operation between Std_ULogic scalar and Std_Logic_Vector.
   -- Input:  B Std_ULogic scalar, V Std_Logic_Vector vector
   -- Output: Std_Logic_Vector with same array constraints as V
   -----------------------------------------------------------------------------
   function "and" (B: Std_ULogic;
                   V: Std_Logic_Vector) return Std_Logic_Vector is
      variable R: Std_Logic_Vector(V'Range);
   begin
      for i in V'Range loop
         R(i):= B and V(i);                            -- logical and
      end loop;
      return R;                                        -- return vector
   end "and";

   -----------------------------------------------------------------------------
   -- Expands the Input four times using To01, the resulting vector is then
   -- shifted into the MISR in four parts. The resulting MISR value is returned.
   -----------------------------------------------------------------------------
   function Shift(MISR:  Std_Logic_Vector;
                  Input: Std_Logic_Vector;
                  Poly:  Std_Logic_Vector) return Std_Logic_Vector is
      variable M: Std_Logic_Vector(MISR'Range)           := MISR;
      variable T: Std_Logic_Vector(0 to 4*MISR'Length-1) := To01(Input);
   begin
      for i in 0 to 3 loop                             -- Four MISR shifts
         M := ('0'&M(0 to MISR'Length-2))              xor
               T(MISR'Length*i to MISR'Length*(i+1)-1) xor
               (M(MISR'Length-1) and Poly);            -- Scalar and vector
      end loop;
      return M;                                        -- Return resulting MISR
   end Shift;

   -----------------------------------------------------------------------------
   -- Returns a Std_Logic_Vector of length L (1 to 100) containing a primitive
   -- polynomial with up to five terms (always including x+1).
   -- Input:  L, the length of the resulting polynomial Std_Logic_Vector
   -- Output: polynomial Std_Logic_Vector defined (0 to n)
   -----------------------------------------------------------------------------
   function Polynomial(L:         Natural;
                       HeaderMsg: String:="MISR:") return Std_Logic_Vector is
      variable P:        Std_Logic_Vector(0 to L-1) := (others => '0');
      subtype  Degree is Integer range 0 to MaxLen;    -- Exponent range
      type     Terms  is array (1 to 3) of Degree;     -- Triplet
      type     TTable is array (1 to MaxLen) of Terms; -- Triplets
      constant Table:    TTable := (                   -- Look-up table
         (0,0,0),   (1,0,0),   (1,0,0),   (1,0,0),   (2,0,0),   (1,0,0),   --  1
         (1,0,0),   (6,5,1),   (4,0,0),   (3,0,0),   (2,0,0),   (7,4,3),   --  7
         (4,3,1),   (12,11,1), (1,0,0),   (5,3,2),   (3,0,0),   (7,0,0),   -- 13
         (6,5,1),   (3,0,0),   (2,0,0),   (1,0,0),   (5,0,0),   (4,3,1),   -- 19
         (3,0,0),   (8,7,1),   (8,7,1),   (3,0,0),   (2,0,0),   (16,15,1), -- 25
         (3,0,0),   (28,27,1), (13,0,0),  (15,14,1), (2,0,0),   (11,0,0),  -- 31
         (12,10,2), (6,5,1),   (4,0,0),   (21,19,2), (3,0,0),   (23,22,1), -- 37
         (6,5,1),   (27,26,1), (4,3,1),   (21,20,1), (5,0,0),   (28,27,1), -- 43
         (9,0,0),   (27,26,1), (16,15,1), (3,0,0),   (16,15,1), (37,36,1), -- 49
         (24,0,0),  (22,21,1), (7,0,0),   (19,0,0),  (22,21,1), (1,0,0),   -- 55
         (16,15,0), (57,56,1), (1,0,0),   (4,3,1),   (18,0,0),  (10,9,1),  -- 61
         (10,9,1),  (9,0,0),   (29,27,2), (16,15,1), (6,0,0),   (53,47,6), -- 67
         (25,0,0),  (16,15,1), (11,10,1), (36,35,1), (31,30,1), (20,19,1), -- 73
         (9,0,0),   (38,37,1), (4,0,0),   (38,35,3), (46,45,1), (13,0,0),  -- 79
         (28,27,1), (13,12,1), (13,0,0),  (72,71,1), (38,0,0),  (19,18,1), -- 85
         (84,83,1), (13,12,1), (2,0,0),   (12,0,0),  (11,0,0),  (49,47,2), -- 91
         (6,0,0),   (11,0,0),  (47,45,2), (37,0,0));                       -- 97
   begin
      assert (L <= MaxLen) and (L > 0)                 -- Check length
         report HeaderMsg&" Only polynomial degree of 1 to 100 is supported."
         severity Failure;
      for i in 1 to 3 loop
         P(Table(L)(i)) := '1';                        -- Insert terms
      end loop;
      return '1'&P(1 to L-1);                          -- Return polynomial
   end Polynomial;

   -----------------------------------------------------------------------------
   -- See definition in the package header.
   -----------------------------------------------------------------------------
   procedure MISR(
      signal   Clk:       in    Std_ULogic;            -- Sample clock
      signal   Reset:     in    Boolean;               -- Reset of MISR
      signal   Input:     in    Std_Logic_Vector;      -- Input vector
      signal   MISR:      inout Std_Logic_Vector;      -- MISR
      constant Rising:    in    Boolean := True;       -- See above
      constant Falling:   in    Boolean := False;      -- See above
      constant HeaderMsg: in    String  := "MISR:";    -- Message header
      constant Sense:     in    Time    := 0 ns) is    -- Sense time after clock

      constant L:    Integer                    := Input'Length;
      constant Poly: Std_Logic_Vector           := Polynomial(L);
      constant Ones: Std_Logic_Vector(0 to L-1) := (others => '1');
      variable Temp: Std_Logic_Vector(0 to L-1) := Input; -- Last Input
      variable Last: Time                       := 0 ns;  -- Last sim cycle
   begin
      assert (L <= MaxLen) and (L >= MinLen)           -- Check length
         report HeaderMsg&" Only MISRs of length 4 to 100 are supported."
         severity Failure;
      assert L = MISR'Length                           -- MISR=Input length
         report HeaderMsg&" The length of the Input and the MISR differs."
         severity Failure;

      while True loop                                  -- Loop never exited
         if not Rising and not Falling then
            -- Clk input has no influence on the MISR in asynchronous mode.
            -- Sampling only the final input vector in each simulation cycle.
            wait on Input, Reset;                      -- Asynchronous sample
            if not (Reset'Event and Reset) and         -- Check not reset
               Input'Event then                        -- Check for Input event
               if Now = Last then                      -- New delta cycle
                  Temp := Input;                       -- Store until next event
               else                                    -- New sim cycle
                  MISR <= Shift(MISR, Temp, Poly);     -- Shift MISR 4 times
                  Temp := Input;                       -- Store until next event
                  Last := Now;                         -- Store sim time
               end if;
            end if;
         else
            wait on Clk, Reset;                        -- Synchronous sample
            if not (Reset'Event and Reset) and         -- Check not reset
               ((Rising and Rising_Edge(Clk)) or       -- Check rising edge
                (Falling and Falling_Edge(Clk))) then  -- Check falling edge
               wait on Reset until Reset for Sense;    -- Delay until sample
               MISR <= Shift(MISR, Input, Poly);       -- Shift MISR 4 times
            end if;
         end if;
         if Reset'Event and Reset then
            MISR <= Ones;                              -- Reset MISR
            Temp := Ones;                              -- Reset Temp
         end if;
      end loop;
   end MISR;
end MISR_Definition;  --================ End of package body =================--
--============================================================================--
-- Design units : TestDefinition (Package declaration and body)
--
-- File name    : testdefinition.vhd
--
-- Purpose      : This packages defines the MISR subprograms used by the
--                Test Generator for the verification of the Bit Modulator.
--
-- Note         : 
--
-- Limitations  : None known
--
-- Errors       : None known
--
-- Library      : BitMod_TB_Lib
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

package TestDefinition is
   -----------------------------------------------------------------------------
   -- Reset of MISR
   -----------------------------------------------------------------------------
   procedure ResetMISR(signal MISRReset: out Boolean);
 
   -----------------------------------------------------------------------------
   -- Check of MISR
   -----------------------------------------------------------------------------
   procedure CheckMISR(
      constant InVec:      in    Std_Logic_Vector(0 to 15);
      constant RefVec:     in    String(1 to 4);
      variable TestFailed: inout Boolean;
      constant HeaderMsg:  in    String := "");
 
end TestDefinition; --=============== End of package header ==================--

package body TestDefinition is
   -----------------------------------------------------------------------------
   -- Reset of MISR
   -----------------------------------------------------------------------------
   procedure ResetMISR(signal MISRReset: out Boolean) is
   begin
      -- TBD
   end ResetMISR;
 
   -----------------------------------------------------------------------------
   -- Check of MISR
   -----------------------------------------------------------------------------
   procedure CheckMISR(
      constant InVec:      in    Std_Logic_Vector(0 to 15);
      constant RefVec:     in    String(1 to 4);
      variable TestFailed: inout Boolean;
      constant HeaderMsg:  in    String := "") is
   begin
      -- TBD
   end CheckMISR;
 
end TestDefinition; --=============== End of package body ====================--
--============================================================================--
-- Design units : TestGenerator (Entity)
--
-- File name    : testgenerator.vhd
--
-- Purpose      : 
--
-- Note         : 
--
-- Limitations  : 
--
-- Errors       : None known
--
-- Library      : BitMod_TB_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                ESA.Simulation,
--                BitMod_TB_Lib.MISR_Definition,
--                BitMod_TB_Lib.TestDefinition.
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

library ESA;
use ESA.Simulation.all;                                -- For simulation cond.

entity TestGenerator is
   generic(
      SimCondition:   SimConditionType := WorstCase;
      InstancePath:   String           := "TestGenerator:");
   port(
      -- System signals (4)
      Test:    inout  Std_Logic_Vector(0 to 1);        -- Test mode
      Clk:     inout  Std_Logic := '0';                -- Master Clock
      Reset_N: inout  Std_Logic;                       -- Master Reset

      -- Interface to internal registers (12)
      A:       inout  Std_Logic_Vector(0 to 1);        -- Address bus
      CS_N:    inout  Std_Logic;                       -- Chip select
      RW_N:    inout  Std_Logic;                       -- Read/write
      D:       inout  Std_Logic_Vector(0 to 7);        -- Bidir. bus
 
      -- Serial Interface (3)
      SClk:    inout  Std_ULogic := '0';               -- Serial clock
      SData:   inout  Std_ULogic := '0';               -- Serial input 
      MData:   in     Std_Logic);                      -- Serial output 

end TestGenerator; --=================== End of entity =======================--
--============================================================================--
-- Design units : TestGenerator(Functional) (Architecture)
--
-- File name    : testgenerator_functional.vhd
--
-- Purpose      :
--
-- Note         :
--
-- Limitations  :
--
-- Errors       : None known
--
-- Library      : BitMod_TB_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                ESA.Simulation,
--                BitMod_TB_Lib.MISR_Definition,
--                BitMod_TB_Lib.TestDefinition.
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
 
--=============================== Architecture ===============================--
library BitMod_TB_Lib;
use BitMod_TB_Lib.TestDefinition.all;
use BitMod_TB_Lib.MISR_Definition.all;

architecture Functional of TestGenerator is
   signal   MISRegister: Std_Logic_Vector(0 to 15);
   signal   MISRInput:   Std_Logic_Vector(0 to 15);
   signal   MISRReset:   Boolean;
   constant Sense:       Time := 45 ns;

begin --========================== Architecture ==============================--
 
   -- This test suite will test the full functionality of the model, except
   -- the the mode when BIST is activated. The activation of the BIST would
   -- preclude the test suite to be evaluated using fault simulation. The
   -- BIST is tested in the architecture X_Handling.
   --
   -- Only the following three Std_Logic values are applied to the inputs,
   -- to be able to execute the test for the Gate-level model as well:
   -- '0', '1', 'Z'.
   --
   -- The ouputs are sampled and compressed using a MISR, which is compared to 
   -- an expected signature at the end of each sub-test.
   -----------------------------------------------------------------------------
   TestSuite: process
      variable TestFailed: Boolean := False;
   begin
      ResetMISR(MISRReset);
         -- Not implemented in this VHDL code example
      CheckMISR(MISRegister, "1234", TestFailed, 
                InstancePath&"Functional:TestSuite:");
      assert not TestFailed
         report InstancePath&"Functional:TestSuite: Test failed."
         severity Error;
      assert TestFailed
         report InstancePath&"Functional:TestSuite: Test passed."
         severity Note;
      assert False
         report InstancePath&"Functional:TestSuite: End of test."
         severity Failure;
      wait;
   end process TestSuite;

   -----------------------------------------------------------------------------
   -- MISR
   -----------------------------------------------------------------------------
   MISRInput <= MData & SData & SClk & D & A & CS_N & RW_N & Reset_N;
 
   MISR(Clk          => Clk,
        Reset        => MISRReset,
        Input        => MISRInput,
        MISR         => MISRegister,
        Rising       => True,
        Falling      => False,
        HeaderMsg    => InstancePath&"Functional:MISR:",
        Sense        => Sense);

end Functional; --================ End of architecture =======================--
--============================================================================--
-- Design units : TestGenerator(Timing) (architecture)
--
-- File name    : testgenerator_timing.vhd
--
-- Purpose      :
--
-- Note         :
--
-- Limitations  :
--
-- Errors       : None known
--
-- Library      : BitMod_TB_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                ESA.Simulation,
--                BitMod_TB_Lib.TestDefinition.
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

--=============================== Architecture ===============================--
architecture Timing of TestGenerator is
begin --========================== Architecture ==============================--
   -- This test suite will test all timing constraint checkers 
   -- (both with and without timing violations).
   --
   -- The test suite can be executed for different simulation conditions using 
   -- the SimCondition generic in the entity.
   --
   -- A listing of simulator outputs (assertion messages) is provided in the
   -- reference files: worstcase.ref, typcase.ref and bestcase.ref.
   --
   -- No verification of the signal output is performed since the main purpose
   -- of the test suite is to verify the behaviour of the timing constraint
   -- checkers.
   --
   -- The timing figures have been taken from the data sheet for the
   -- Bit Modulater dated January 1995. The timing figures
   -- are based on 25 pF load on the outputs. The timing parameter
   -- names are compliant with Vital Level 0.
end Timing; --=================== End of architecture ========================--
--============================================================================--
-- Design units : TestGenerator(X_Handling) (Architecture)
--
-- File name    : testgenerator_x_handling.vhd
--
-- Purpose      :
--
-- Note         :
--
-- Limitations  :
--
-- Errors       : None known
--
-- Library      : BitMod_TB_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                ESA.Simulation,
--                BitMod_TB_Lib.MISR_Definition,
--                BitMod_TB_Lib.TestDefinition.
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
 
--=============================== Architecture ===============================--
library BitMod_TB_Lib;
use BitMod_TB_Lib.MISR_Definition.all;
use BitMod_TB_Lib.TestDefinition.all;
 
architecture X_Handling of TestGenerator is
begin --========================== Architecture ==============================--
   -- This test suite will test the following:
   --   all inputs will be applied all nine Std_Logic values;
   --   all checkers for unknown values on inputs;
   --   the handling of each unknown input value will be checked;
   --   the propagation of each unknown input value will be checked.
   --
   -- This test suite will also test the BIST functionality of the mode, 
   -- and should not be evaluated using fault simulation since it would activate
   -- portions of the component not modelled for Board-level simulation.
   --
   -- The ouputs are sampled and compressed using a MISR, which is compared to 
   -- the expected signature at the end of each sub-test.
   -- The ouputs are sampled and compressed using a MISR, which is compared to 
   -- an expected signature at the end of each sub-test.
end X_Handling; --================ End of architecture =======================--
--============================================================================--
-- Design units : TestBench(Structural) (Entity and architecture)
--
-- File name    : testbench.vhd
--
-- Purpose      : This is the structural test bench binding the Bit Modulator
--                and the Test Generator together.
--
-- Note         : No generics have been declared for the component, the
--                association of values to the generics will be done in the
--                configuration declarations of the test bench.
--
-- Limitations  : None known
--
-- Errors       : None known
--
-- Library      : BitMod_TB_Lib
--
-- Dependencies : IEEE.Std_Logic_1164.
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
--------------------------------------------------------------------------------

entity TestBench is
end TestBench; --=================== End of entity ===========================--

--=============================== Architecture ===============================--
library IEEE;
use IEEE.Std_Logic_1164.all;

architecture Structural of TestBench is
   -----------------------------------------------------------------------------
   -- Component declarations.
   -----------------------------------------------------------------------------
   component BitMod
      port(
         -- System signals (4)
         Test:    in     Std_Logic_Vector(0 to 1);     -- Test mode
         Clk:     in     Std_Logic;                    -- Master Clock
         Reset_N: in     Std_Logic;                    -- Master Reset

         -- Interface to internal registers (12)
         A:       in     Std_Logic_Vector(0 to 1);     -- Address bus
         CS_N:    in     Std_Logic;                    -- Chip select
         RW_N:    in     Std_Logic;                    -- Read/write
         D:       inout  Std_Logic_Vector(0 to 7);     -- Bidir. bus

         -- Serial Interface (3)
         SClk:    in     Std_ULogic;                   -- Serial clock
         SData:   in     Std_ULogic;                   -- Serial input 
         MData:   out    Std_Logic);                   -- Serial output 
   end component;

   component TestGenerator
      port(
         -- System signals (4)
         Test:    inout  Std_Logic_Vector(0 to 1);     -- Test mode
         Clk:     inout  Std_Logic;                    -- Master Clock
         Reset_N: inout  Std_Logic;                    -- Master Reset

         -- Interface to internal registers (12)
         A:       inout  Std_Logic_Vector(0 to 1);     -- Address bus
         CS_N:    inout  Std_Logic;                    -- Chip select
         RW_N:    inout  Std_Logic;                    -- Read/write
         D:       inout  Std_Logic_Vector(0 to 7);     -- Bidir. bus

         -- Serial Interface (3)
         SClk:    inout  Std_ULogic;                   -- Serial clock
         SData:   inout  Std_ULogic;                   -- Serial inputData
         MData:   in     Std_Logic);                   -- Serial output 
   end component;

   -----------------------------------------------------------------------------
   -- Local signal declarations.
   -----------------------------------------------------------------------------
   signal Test:     Std_Logic_Vector(0 to 1);          -- Test mode
   signal Clk:      Std_Logic;                         -- Master Clock
   signal Reset_N:  Std_Logic;                         -- Master Reset
   signal A:        Std_Logic_Vector(0 to 1);          -- Address bus
   signal CS_N:     Std_Logic;                         -- Chip select
   signal RW_N:     Std_Logic;                         -- Read/write
   signal D:        Std_Logic_Vector(0 to 7);          -- Bidir. bus
   signal SClk:     Std_ULogic;                        -- Serial Clock
   signal SData:    Std_ULogic;                        -- Serial input
   signal MData:    Std_Logic;                         -- Serial output

begin --========================== Architecture ==============================--

   -----------------------------------------------------------------------------
   -- Instantiation of components.
   -----------------------------------------------------------------------------
   Test_Object: BitMod
      port map(
         Test     => Test,
         Clk      => Clk,
         Reset_N  => Reset_N,
         A        => A,
         CS_N     => CS_N,
         RW_N     => RW_N,
         D        => D,
         SClk     => SClk,
         SData    => SData,
         MData    => MData);

   Test_Generator: TestGenerator
      port map(
         Test     => Test,
         Clk      => Clk,
         Reset_N  => Reset_N,
         A        => A,
         CS_N     => CS_N,
         RW_N     => RW_N,
         D        => D,
         SClk     => SClk,
         SData    => SData,
         MData    => MData);

end Structural; --================ End of architecture =======================--
--============================================================================--
-- Design units : FunctionalTest (Configuration)
--
-- File name    : functionaltest.vhd
--
-- Purpose      : Configuration selecting the functional test suite and
--                the Bit Modulator with the timing checkers deactivated.
--
-- Note         :
--
-- Limitations  :
--
-- Errors       : None known
--
-- Library      : BitMod_TB_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                ESA.Simulation,
--                BitMod_Lib.BitMod,
--                BitMod_TB_Lib.TestGenerator.
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
library BitMod_TB_Lib;
library ESA;
use ESA.Simulation.all;
use ESA.Timing.all;

configuration FunctionalTest of TestBench is
   for Structural
      for Test_Object: BitMod
         use configuration BitMod_Lib.BitMod_Configuration
            generic map(SimCondition   => WorstCase,
                        InstancePath   => ":TestBench:Test_Object:",
                        TimingChecksOn => False);
      end for;
      for Test_Generator: TestGenerator
         use entity BitMod_TB_Lib.TestGenerator(Functional)
            generic map(SimCondition   => WorstCase,
                        InstancePath   => ":TestBench:Test_Generator:");

      end for;
   end for;
end FunctionalTest; --============== End of configuration ====================--
--============================================================================--
-- Design units : X_HandlingTest (Configuration)
--
-- File name    : x_handlingtest.vhd
--
-- Purpose      : Configuration selecting the test suite verifying the
--                checker for unknown inputs and the BIST mode.
--
-- Note         :
--
-- Limitations  :
--
-- Errors       : None known
--
-- Library      : BitMod_TB_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                ESA.Simulation,
--                BitMod_Lib.BitMod,
--                BitMod_TB_Lib.TestGenerator.
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
library BitMod_TB_Lib;
library ESA;
use ESA.Simulation.all;
use ESA.Timing.all;

configuration X_HandlingTest of TestBench is
   for Structural
      for Test_Object: BitMod
         use configuration BitMod_Lib.BitMod_Configuration
            generic map(SimCondition   => WorstCase,
                        InstancePath   => ":TestBench:Test_Object:",
                        TimingChecksOn => False);
      end for;
      for Test_Generator: TestGenerator
         use entity BitMod_TB_Lib.TestGenerator(X_Handling)
            generic map(SimCondition   => WorstCase,
                        InstancePath   => ":TestBench:Test_Generator:");
      end for;
   end for;
end X_HandlingTest; --=============== End of configuration ===================--
--============================================================================--
-- Design units : WorstCaseTest (Configuration)
--
-- File name    : worstcasetest.vhd
--
-- Purpose      : Configuration selecting the timing test under Worst Case
--                simulation conditions.
--
-- Note         :
--
-- Limitations  :
--
-- Errors       : None known
--
-- Library      : BitMod_TB_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                ESA.Simulation,
--                BitMod_Lib.BitMod,
--                BitMod_TB_Lib.TestGenerator.
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
library BitMod_TB_Lib;
library ESA;
use ESA.Simulation.all;
use ESA.Timing.all;

configuration WorstCaseTest of TestBench is
   for Structural
      for Test_Object: BitMod
         use configuration BitMod_Lib.BitMod_Configuration
            generic map(SimCondition   => WorstCase,
                        InstancePath   => ":TestBench:Test_Object:",
                        TimingChecksOn => True);
      end for;
      for Test_Generator: TestGenerator
         use entity BitMod_TB_Lib.TestGenerator(Timing)
            generic map(SimCondition   => WorstCase,
                        InstancePath   => ":TestBench:Test_Generator:");
      end for;
   end for;
end WorstCaseTest; --================ End of configuration ===================--
--============================================================================--
-- Design units : TypCaseTest (Configuration)
--
-- File name    : typcasetest.vhd
--
-- Purpose      : Configuration selecting the timing test under Typical Case
--                simulation conditions.
--
-- Note         :
--
-- Limitations  :
--
-- Errors       : None known
--
-- Library      : BitMod_TB_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                ESA.Simulation,
--                BitMod_Lib.BitMod,
--                BitMod_TB_Lib.TestGenerator.
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
library BitMod_TB_Lib;
library ESA;
use ESA.Simulation.all;
use ESA.Timing.all;

configuration TypCaseTest of TestBench is
   for Structural
      for Test_Object: BitMod
         use configuration BitMod_Lib.BitMod_Configuration
            generic map(SimCondition   => TypCase,
                        InstancePath   => ":TestBench:Test_Object:",
                        TimingChecksOn => True);
      end for;
      for Test_Generator: TestGenerator
         use entity BitMod_TB_Lib.TestGenerator(Timing)
            generic map(SimCondition   => TypCase,
                        InstancePath   => ":TestBench:Test_Generator:");
      end for;
   end for;
end TypCaseTest; --================== End of configuration ===================--
--============================================================================--
-- Design units : BestCaseTest (Configuration)
--
-- File name    : bestcasetest.vhd
--
-- Purpose      : Configuration selecting the timing test under Best Case
--                simulation conditions.
--
-- Note         :
--
-- Limitations  :
--
-- Errors       : None known
--
-- Library      : BitMod_TB_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                ESA.Simulation,
--                BitMod_Lib.BitMod,
--                BitMod_TB_Lib.TestGenerator.
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
library BitMod_TB_Lib;
library ESA;
use ESA.Simulation.all;
use ESA.Timing.all;

configuration BestCaseTest of TestBench is
   for Structural
      for Test_Object: BitMod
         use configuration BitMod_Lib.BitMod_Configuration
            generic map(SimCondition   => BestCase,
                        InstancePath   => ":TestBench:Test_Object:",
                        TimingChecksOn => True);
      end for;
      for Test_Generator: TestGenerator
         use entity BitMod_TB_Lib.TestGenerator(Timing)
            generic map(SimCondition   => BestCase,
                        InstancePath   => ":TestBench:Test_Generator:");
      end for;
   end for;
end BestCaseTest; --================ End of configuration ====================--
