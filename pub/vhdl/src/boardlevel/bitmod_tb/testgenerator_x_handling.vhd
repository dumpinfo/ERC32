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
