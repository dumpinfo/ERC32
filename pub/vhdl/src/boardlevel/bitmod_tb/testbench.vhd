--============================================================================--
-- Design units : TestBench(Structural) (Entity and architecture)
--
-- File name    : testbench.vhd
--
-- Purpose      : This is the structural test bench binding the Bit Modulator
--                and the Test Generator together.
--
-- Note         : No generics have been declared for the component, the
--                association of values to the generics will be done in the
--                configuration declarations of the test bench.
--
-- Limitations  : None known
--
-- Errors       : None known
--
-- Library      : BitMod_TB_Lib
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

entity TestBench is
end TestBench; --=================== End of entity ===========================--

--=============================== Architecture ===============================--
library IEEE;
use IEEE.Std_Logic_1164.all;

architecture Structural of TestBench is
   -----------------------------------------------------------------------------
   -- Component declarations.
   -----------------------------------------------------------------------------
   component BitMod
      port(
         -- System signals (4)
         Test:    in     Std_Logic_Vector(0 to 1);     -- Test mode
         Clk:     in     Std_Logic;                    -- Master Clock
         Reset_N: in     Std_Logic;                    -- Master Reset

         -- Interface to internal registers (12)
         A:       in     Std_Logic_Vector(0 to 1);     -- Address bus
         CS_N:    in     Std_Logic;                    -- Chip select
         RW_N:    in     Std_Logic;                    -- Read/write
         D:       inout  Std_Logic_Vector(0 to 7);     -- Bidir. bus

         -- Serial Interface (3)
         SClk:    in     Std_ULogic;                   -- Serial clock
         SData:   in     Std_ULogic;                   -- Serial input 
         MData:   out    Std_Logic);                   -- Serial output 
   end component;

   component TestGenerator
      port(
         -- System signals (4)
         Test:    inout  Std_Logic_Vector(0 to 1);     -- Test mode
         Clk:     inout  Std_Logic;                    -- Master Clock
         Reset_N: inout  Std_Logic;                    -- Master Reset

         -- Interface to internal registers (12)
         A:       inout  Std_Logic_Vector(0 to 1);     -- Address bus
         CS_N:    inout  Std_Logic;                    -- Chip select
         RW_N:    inout  Std_Logic;                    -- Read/write
         D:       inout  Std_Logic_Vector(0 to 7);     -- Bidir. bus

         -- Serial Interface (3)
         SClk:    inout  Std_ULogic;                   -- Serial clock
         SData:   inout  Std_ULogic;                   -- Serial inputData
         MData:   in     Std_Logic);                   -- Serial output 
   end component;

   -----------------------------------------------------------------------------
   -- Local signal declarations.
   -----------------------------------------------------------------------------
   signal Test:     Std_Logic_Vector(0 to 1);          -- Test mode
   signal Clk:      Std_Logic;                         -- Master Clock
   signal Reset_N:  Std_Logic;                         -- Master Reset
   signal A:        Std_Logic_Vector(0 to 1);          -- Address bus
   signal CS_N:     Std_Logic;                         -- Chip select
   signal RW_N:     Std_Logic;                         -- Read/write
   signal D:        Std_Logic_Vector(0 to 7);          -- Bidir. bus
   signal SClk:     Std_ULogic;                        -- Serial Clock
   signal SData:    Std_ULogic;                        -- Serial input
   signal MData:    Std_Logic;                         -- Serial output

begin --========================== Architecture ==============================--

   -----------------------------------------------------------------------------
   -- Instantiation of components.
   -----------------------------------------------------------------------------
   Test_Object: BitMod
      port map(
         Test     => Test,
         Clk      => Clk,
         Reset_N  => Reset_N,
         A        => A,
         CS_N     => CS_N,
         RW_N     => RW_N,
         D        => D,
         SClk     => SClk,
         SData    => SData,
         MData    => MData);

   Test_Generator: TestGenerator
      port map(
         Test     => Test,
         Clk      => Clk,
         Reset_N  => Reset_N,
         A        => A,
         CS_N     => CS_N,
         RW_N     => RW_N,
         D        => D,
         SClk     => SClk,
         SData    => SData,
         MData    => MData);

end Structural; --================ End of architecture =======================--
