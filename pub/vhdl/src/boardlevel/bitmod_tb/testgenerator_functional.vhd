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
