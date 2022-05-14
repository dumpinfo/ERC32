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
 
library ESA;
use ESA.Simulation.all;
 
library IEEE;
use IEEE.Std_Logic_1164.all;

entity TestGenerator is
   generic(
      SimCondition:   SimConditionType := WorstCase;
      InstancePath:   String           := "TestGenerator:");
   port(
      Test:    out    Std_Logic_Vector(0 to 1);        -- Board Test mode
      Clk:     out    Std_Logic;                       -- Board Master Clock
      Reset_N: out    Std_Logic;                       -- Board Master Reset
      A:       out    Std_Logic_Vector(0 to 1);        -- Board Address bus
      D:       inout  Std_Logic_Vector(0 to 7);        -- Board Bidir. data bus
      RW_N:    out    Std_Logic;                       -- Board Read/write
      CS0_N:   out    Std_Logic;                       -- Chip select, IC0
      CS1_N:   out    Std_Logic;                       -- Chip select, IC1
      CS2_N:   out    Std_Logic;                       -- Chip select, IC2
      SClk:    out    Std_ULogic;                      -- Serial Clock
      DataIn:  out    Std_ULogic;                      -- Serial input Data
      DataOut: in     Std_Logic);                      -- Serial output data
end TestGenerator; --=================== End of entity =======================--
