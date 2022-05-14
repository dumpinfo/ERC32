--============================================================================--
-- Design units : BoardDesign(Unconfigured) (Architecture)
--
-- File name    : unconfigured.vhd
--
-- Purpose      : The architecture only contains information on connectivity,
--                no timing related information or selection of
--                entity/architecture is made here. It is deferred to a
--                configuration declaration.
--
-- Note         : No generics have been declared for the component, the
--                association of values to the generics will be done in the
--                configuration declaration.
--
-- Limitations  : None known
--
-- Errors       : None known
--
-- Library      : BoardDesign_Lib
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

architecture Unconfigured of BoardDesign is
   -- Only declare the ports for the component. The generics of the entity
   -- will be associated in the configuration declaration.
   component BitMod
      port(
         Test:    in     Std_Logic_Vector(0 to 1);     -- Test mode
         Clk:     in     Std_Logic;                    -- Master Clock
         Reset_N: in     Std_Logic;                    -- Master Reset
         A:       in     Std_Logic_Vector(0 to 1);     -- Address bus
         CS_N:    in     Std_Logic;                    -- Chip select, act. low
         RW_N:    in     Std_Logic;                    -- Read/write, read = 1
         D:       inout  Std_Logic_Vector(0 to 7);     -- Bidir. data bus
         SClk:    in     Std_ULogic;                   -- Serial Clock
         SData:   in     Std_ULogic;                   -- Serial input Data
         MData:   out    Std_Logic);                   -- Modulated output Data
   end component;

   signal Data0:    Std_ULogic;                        -- Serial Data
   signal Data1:    Std_ULogic;                        -- Serial Data

begin --========================== Architecture ==============================--
 
   IC0: BitMod
      port map(
         Test     => Test,
         Clk      => Clk,
         Reset_N  => Reset_N,
         A        => A,
         CS_N     => CS0_N,
         RW_N     => RW_N,
         D        => D,
         SClk     => SClk,
         SData    => DataIn,
         MData    => Data0);

   IC1: BitMod
      port map(
         Test     => Test,
         Clk      => Clk,
         Reset_N  => Reset_N,
         A        => A,
         CS_N     => CS1_N,
         RW_N     => RW_N,
         D        => D,
         SClk     => SClk,
         SData    => Data0,
         MData    => Data1);

   IC2: BitMod
      port map(
         Test     => Test,
         Clk      => Clk,
         Reset_N  => Reset_N,
         A        => A,
         CS_N     => CS2_N,
         RW_N     => RW_N,
         D        => D,
         SClk     => SClk,
         SData    => Data1,
         MData    => DataOut);
end Unconfigured;  --================ End of architecture ====================--
