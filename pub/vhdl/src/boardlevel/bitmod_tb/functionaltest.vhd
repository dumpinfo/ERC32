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
