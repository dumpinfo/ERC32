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
--============================================================================--
-- Design units : TestBench(Structural) (Entity and architecture)
--
-- File name    : testbench.vhd
--
-- Purpose      : This is the structural test bench binding the BoardDesign
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
   -- Only declare the ports for the component. The generics of the entity
   -- will be associated in the configuration declaration.
   component BoardDesign
      port(
         Test:    in     Std_Logic_Vector(0 to 1);     -- Board Test mode
         Clk:     in     Std_Logic;                    -- Board Master Clock
         Reset_N: in     Std_Logic;                    -- Board Master Reset
         A:       in     Std_Logic_Vector(0 to 1);     -- Board Address bus
         D:       inout  Std_Logic_Vector(0 to 7);     -- Board Bidir. data bus
         RW_N:    in     Std_Logic;                    -- Board Read/write
         CS0_N:   in     Std_Logic;                    -- Chip select, IC0
         CS1_N:   in     Std_Logic;                    -- Chip select, IC1
         CS2_N:   in     Std_Logic;                    -- Chip select, IC2
         SClk:    in     Std_ULogic;                   -- Serial Clock
         DataIn:  in     Std_ULogic;                   -- Serial input Data
         DataOut: out    Std_Logic);                   -- Serial output data
   end component;

   component TestGenerator
      port(
         Test:    out    Std_Logic_Vector(0 to 1);     -- Board Test mode
         Clk:     out    Std_Logic;                    -- Board Master Clock
         Reset_N: out    Std_Logic;                    -- Board Master Reset
         A:       out    Std_Logic_Vector(0 to 1);     -- Board Address bus
         D:       inout  Std_Logic_Vector(0 to 7);     -- Board Bidir. data bus
         RW_N:    out    Std_Logic;                    -- Board Read/write
         CS0_N:   out    Std_Logic;                    -- Chip select, IC0
         CS1_N:   out    Std_Logic;                    -- Chip select, IC1
         CS2_N:   out    Std_Logic;                    -- Chip select, IC2
         SClk:    out    Std_ULogic;                   -- Serial Clock
         DataIn:  out    Std_ULogic;                   -- Serial input Data
         DataOut: in     Std_Logic);                   -- Serial output data
   end component;

   -----------------------------------------------------------------------------
   -- Local signal declarations.
   -----------------------------------------------------------------------------
   signal Test:          Std_Logic_Vector(0 to 1);     -- Board Test mode
   signal Clk:           Std_Logic;                    -- Board Master Clock
   signal Reset_N:       Std_Logic;                    -- Board Master Reset
   signal A:             Std_Logic_Vector(0 to 1);     -- Board Address bus
   signal D:             Std_Logic_Vector(0 to 7);     -- Board Bidir. data bus
   signal RW_N:          Std_Logic;                    -- Board Read/write
   signal CS0_N:         Std_Logic;                    -- Chip select, IC0
   signal CS1_N:         Std_Logic;                    -- Chip select, IC1
   signal CS2_N:         Std_Logic;                    -- Chip select, IC2
   signal SClk:          Std_ULogic;                   -- Serial Clock
   signal DataIn:        Std_ULogic;                   -- Serial input Data
   signal DataOut:       Std_Logic;                    -- Serial output data


begin --========================== Architecture ==============================--
 
   -----------------------------------------------------------------------------
   -- Instantiation of components.
   -----------------------------------------------------------------------------
   Test_Object: BoardDesign
      port map(
         Test     => Test,
         Clk      => Clk,
         Reset_N  => Reset_N,
         A        => A,
         CS0_N    => CS0_N,
         CS1_N    => CS1_N,
         CS2_N    => CS2_N,
         RW_N     => RW_N,
         D        => D,
         SClk     => SClk,
         DataIn   => DataIn,
         DataOut  => DataOut);

   Test_Generator: TestGenerator
      port map(
         Test     => Test,
         Clk      => Clk,
         Reset_N  => Reset_N,
         A        => A,
         CS0_N    => CS0_N,
         CS1_N    => CS1_N,
         CS2_N    => CS2_N,
         RW_N     => RW_N,
         D        => D,
         SClk     => SClk,
         DataIn   => DataIn,
         DataOut  => DataOut);

end Structural; --================ End of architecture =======================--
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
--============================================================================--
-- Design units : TypCaseTest (Configuration)
--
-- File name    : typcasetest.vhd
--
-- Purpose      : Configuration selecting the timing test under Typical Case
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

configuration TypCaseTest of TestBench is
   for Structural
      for Test_Object: BoardDesign
         use configuration BoardDesign_Lib.BoardDesign_Configuration
            generic map(
               SimCondition   => TypCase,
               InstancePath   => ":TestBench:Test_Object:",
               TimingChecksOn => True);
      end for;
      for Test_Generator: TestGenerator
         use entity BoardDesign_TB_Lib.TestGenerator(Timing)
            generic map(
               SimCondition   => TypCase,
               InstancePath   => ":TestBench:Test_Generator:");
      end for;
   end for;
end TypCaseTest; --================== End of configuration ===================--
--============================================================================--
-- Design units : BestCaseTest (Configuration)
--
-- File name    : testbench.vhd
--
-- Purpose      : Configuration selecting the timing test under Best Case
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
-- Dependencies : ESA.Simulation,
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

configuration BestCaseTest of TestBench is
   for Structural
      for Test_Object: BoardDesign
         use configuration BoardDesign_Lib.BoardDesign_Configuration
            generic map(
               SimCondition   => BestCase,
               InstancePath   => ":TestBench:Test_Object:",
               TimingChecksOn => True);
      end for;
      for Test_Generator: TestGenerator
         use entity BoardDesign_TB_Lib.TestGenerator(Timing)
            generic map(
               SimCondition   => BestCase,
               InstancePath   => ":TestBench:Test_Generator:");
      end for;
   end for;
end BestCaseTest; --================= End of configuration ===================--
 
