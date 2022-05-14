--============================================================================--
-- Design units : BoardDesign (Entity)
--
-- File name    : boarddesign.vhd
--
-- Purpose      : This is an example of a simple board with three serially 
--                connected Bit Modulators instantiated.
--
-- Note         : No generics have been declared for the component, the
--                association of values to the generics will be done in the
--                configuration declaration.
--
-- Limitations  : None known
--
-- Errors       : None known
--
-- Library      : BoardDesign_Timing
--
-- Dependencies : IEEE.Std_Logic_1164, ESA.Simulation, ESA.Timing,
--                BoardDesign_Lib.BoardDesign_Timing. 
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

library ESA;
use ESA.Simulation.all;
use ESA.Timing.all;
 
library IEEE;
use IEEE.Std_Logic_1164.all;

library BoardDesign_Lib;
use BoardDesign_Lib.BoardDesign_Timing.all;

entity BoardDesign is
   generic(
      SimCondition:   SimConditionType := WorstCase;
      InstancePath:   String           := "BoardDesign:";
      TimingChecksOn: Boolean          := False;
      tpd_Clk_MData:  TimeArray01      := tpd_Clk_MData);
   port(
      Test:    in     Std_Logic_Vector(0 to 1);        -- Board Test mode
      Clk:     in     Std_Logic;                       -- Board Master Clock
      Reset_N: in     Std_Logic;                       -- Board Master Reset
      A:       in     Std_Logic_Vector(0 to 1);        -- Board Address bus
      D:       inout  Std_Logic_Vector(0 to 7);        -- Board Bidir. data bus
      RW_N:    in     Std_Logic;                       -- Board Read/write
      CS0_N:   in     Std_Logic;                       -- Chip select, IC0
      CS1_N:   in     Std_Logic;                       -- Chip select, IC1
      CS2_N:   in     Std_Logic;                       -- Chip select, IC2
      SClk:    in     Std_ULogic;                      -- Serial Clock
      DataIn:  in     Std_ULogic;                      -- Serial input data
      DataOut: out    Std_Logic);                      -- Serial output data
end BoardDesign; --================== End of entity ==========================--
