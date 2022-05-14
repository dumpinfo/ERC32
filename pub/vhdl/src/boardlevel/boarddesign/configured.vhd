--============================================================================--
-- Design units : BoardDesign(Configured) (Architecture)
--
-- File name    : Configured.vhd
--
-- Purpose      : The architecture contains information on connectivity,
--                and timing related information and selection of
--                entity/architecture is made here.
--
-- Note         : 
--
-- Limitations  : None known
--
-- Errors       : None known
--
-- Library      : BoardDesign_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                BitMod_Lib.
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
library BitMod_Lib;

library IEEE;
use IEEE.Vital_Timing.all;

-- The timing package contents are not made visible since this would potentially
-- conflict with any contenst of timing packages of other models. (This is not
-- illustrated in this example).

architecture Configured of BoardDesign is
   -----------------------------------------------------------------------------
   -- Component declarations.
   -----------------------------------------------------------------------------
   -- The generic declaration needs only to include those generics which will
   -- be associated in the architecture. The rest will take default values
   -- as defined for the entity referenced in the _for all_ statement. 
   --
   -- The timing generic delcaration need to have the default value
   -- as defined for the entity, since one instantiation will have
   -- unmodified timing (IC2). The default value is fetched from the
   -- timing package, with a full named selection not to conflict with
   -- any other paramters of other models.
   --
   component BitMod
      generic(
         SimCondition:   SimConditionType := WorstCase;
         InstancePath:   String           := "BitMod:";
         TimingChecksOn: Boolean          := False;
         tpd_Clk_MData:  TimeArray01      := 
                               BitMod_Lib.BitMod_Timing.tpd_Clk_MData);
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

   -----------------------------------------------------------------------------
   -- Local signal declarations.
   -----------------------------------------------------------------------------
   signal Data0: Std_ULogic;                           -- Serial Data
   signal Data1: Std_ULogic;                           -- Serial Data

   -----------------------------------------------------------------------------
   -- Configuration of components.
   -----------------------------------------------------------------------------
   -- All instances of the component BitMod are tied to the entity
   -- configured by BitMod_Configuration in this configuration 
   -- specification.
   for all: BitMod
         use configuration BitMod_Lib.BitMod_Configuration;

begin --========================== Architecture ==============================--
   -----------------------------------------------------------------------------
   -- Instantiation of components.
   -----------------------------------------------------------------------------
   IC0: BitMod                                 -- Annotated timing
      generic map(
         SimCondition   => SimCondition,
         InstancePath   => InstancePath&"IC0:",
         TimingChecksOn => TimingChecksOn,
         tpd_Clk_MData  => 

            ((BitMod_Lib.BitMod_Timing.tpd_Clk_MData(WorstCase)(tr01) + 5 ns,
              BitMod_Lib.BitMod_Timing.tpd_Clk_MData(WorstCase)(tr10)+ 5 ns),

             (BitMod_Lib.BitMod_Timing.tpd_Clk_MData(TypCase)(tr01)+ 5 ns,
              BitMod_Lib.BitMod_Timing.tpd_Clk_MData(TypCase)(tr10)+ 5 ns),

             (BitMod_Lib.BitMod_Timing.tpd_Clk_MData(BestCase)(tr01)+ 5 ns,
              BitMod_Lib.BitMod_Timing.tpd_Clk_MData(BestCase)(tr10)+ 5 ns)))
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

   IC1: BitMod                                 -- Absolute timing
      generic map(
         SimCondition   => SimCondition,
         InstancePath   => InstancePath&"IC1:",
         TimingChecksOn => TimingChecksOn,
         tpd_Clk_MData  => ((5 ns, 5 ns), (5 ns, 5 ns), (5 ns, 5 ns)))
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

   IC2: BitMod                                 -- Unmodified timing
      generic map(
         SimCondition   => SimCondition,
         InstancePath   => InstancePath&"IC2:",
         TimingChecksOn => TimingChecksOn)
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
end Configured; --================== End of architecture =====================--
