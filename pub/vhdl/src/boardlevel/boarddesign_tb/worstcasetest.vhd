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
-- Library      : BoardDesign_TB_Lib
--
-- Dependencies : ESA.Simulation
--                BoardDesign_Lib,
--                BoardDesign_TB_Lib.
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
library ESA;
use ESA.Simulation.all;
use ESA.Timing.all;

library BoardDesign_Lib;
library BoardDesign_TB_Lib;
 
configuration WorstCaseTest of TestBench is
   for Structural 
      for Test_Object: BoardDesign
         use configuration BoardDesign_Lib.BoardDesign_Configuration
            generic map(
               SimCondition   => WorstCase,
               InstancePath   => ":TestBench:Test_Object:",
               TimingChecksOn => True);
      end for;
      for Test_Generator: TestGenerator
         use entity BoardDesign_TB_Lib.TestGenerator(Timing)
            generic map(
               SimCondition   => WorstCase,
               InstancePath   => ":TestBench:Test_Generator:");
      end for;
   end for;
end WorstCaseTest; --================ End of configuration ===================--
