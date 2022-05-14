--============================================================================--
-- Design units : TestGenerator(Timing) (Architecture)
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
-- Library      : BoardDesign_TB_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                ESA.Simulation.
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
   TestSuite: process
   begin
      wait;
   end process TestSuite;
end Timing; --=================== End of architecture ========================--
