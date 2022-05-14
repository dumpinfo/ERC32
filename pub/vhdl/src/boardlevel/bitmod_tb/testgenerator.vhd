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
