--============================================================================--
-- Design units : BoardDesign_Timing (Package declaration and body)
--
-- File name    : boarddesign_timing.vhd
--
-- Purpose      : In this package, all timing parameters for the Board Design
--                are defined as deferred constants; their value can be modified
--                by recompiling only the package body.
--
-- Note         : The timing figures are based on 25 pF load on the outputs. 
--                The timing parameter names are compliant with Vital Level 0.
--
-- Limitations  : Best case and typical figures have been approximated:  
--                Best case    = Worst case / 4, for path delays,
--                Typical case = Worst case / 2, for path delays.
--
-- Errors:      : None known
--
-- Library      : BoardDesign_Lib
--
-- Dependencies : ESA.Simulation, ESA.Timing.
--
-- Author       : Sandi Habinc
--                ESTEC Microelectronics and Technology Section (WSM)
--                P.O. Box 299
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
-- 0.1     SH      1 July 95 New package
--------------------------------------------------------------------------------
 
library ESA;
use ESA.Simulation.all;
use ESA.Timing.all;

package BoardDesign_Timing is
   -----------------------------------------------------------------------------
   -- Deferred constants for the timing parameters, all values are defined in
   -- the package body.
   -----------------------------------------------------------------------------
   -- Output interface timing parameters
   constant tpd_Clk_MData: TimeArray01;
end BoardDesign_Timing; --============= End of package header ================--

package body BoardDesign_Timing is
   -- Output interface timing parameter
   constant tpd_Clk_MData: TimeArray01 := ((25 ns, 24 ns),     -- WC
                                           (11 ns, 13 ns),     -- TC
                                           ( 7 ns,  8 ns));    -- BC
end BoardDesign_Timing; --=============== End of package body ================--
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
--============================================================================--
-- Design units : BoardDesign_Configuration (Configuration)
--
-- File name    : boarddesign_configuration.vhd
--
-- Purpose      : Configure the board with explicit timing of the design.  
--                If other timing is needed, a new configuration could be 
--                derived. SimCondition and TimingChecksOn are propagated down 
--                the hierarchy.
--
-- Note         : Only the timing package is made visible, not the contents.
--                In the general case it is not possible to make the contents
--                visible to the configuration, since it there will be more than
--                one component in the design, each having its timing package,
--                and could result in naming  conflicts for timing generics.
--                Therefore is the incremented default timing generics for
--                instance IC0 made visible only where used. If the default
--                timing parameter is not needed, the package does not to be
--                specified in the use clause above, which is the case for IC2.
--
-- Limitations  :
--
-- Errors       : None known
--
-- Library      : BoardDesign_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                IEEE.Vital_Timing.all,
--                ESA.Simulation,
--                ESA.Timing,
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
-- 0.1     SH      1 July 95 New configuration
--------------------------------------------------------------------------------

library ESA;
use ESA.Simulation.all;
use ESA.Timing.all;

library IEEE;
use IEEE.Vital_Timing.all;

library BitMod_Lib;

configuration BoardDesign_Configuration of BoardDesign is
   for Unconfigured
      for IC0: BitMod                                  -- Annotated timing
         use configuration BitMod_Lib.BitMod_Configuration
            generic map(
               SimCondition   => SimCondition,
               InstancePath   => InstancePath&"IC0:",
               TimingChecksOn => TimingChecksOn,
               tpd_Clk_MData  =>
               ((BitMod_Lib.BitMod_Timing.tpd_Clk_MData(WorstCase)(tr01)+ 5 ns,
                BitMod_Lib.BitMod_Timing.tpd_Clk_MData(WorstCase)(tr10)+ 5 ns),
 
               (BitMod_Lib.BitMod_Timing.tpd_Clk_MData(TypCase)(tr01)+ 5 ns,
                BitMod_Lib.BitMod_Timing.tpd_Clk_MData(TypCase)(tr10)+ 5 ns),
 
               (BitMod_Lib.BitMod_Timing.tpd_Clk_MData(BestCase)(tr01)+ 5 ns,
                BitMod_Lib.BitMod_Timing.tpd_Clk_MData(BestCase)(tr10)+ 5 ns)));
      end for;

      for IC1: BitMod                                  -- Absolute timing
         use configuration BitMod_Lib.BitMod_Configuration
            generic map(
               SimCondition   => SimCondition,
               InstancePath   => InstancePath&"IC1:",
               TimingChecksOn => TimingChecksOn,
               tpd_Clk_MData  => ((5 ns, 5 ns), (5 ns, 5 ns), (5 ns, 5 ns)));
      end for;

      for IC2: BitMod                                  -- Unmodified timing
         use configuration BitMod_Lib.BitMod_Configuration
            generic map(
               SimCondition   => SimCondition,
               InstancePath   => InstancePath&"IC2:",
               TimingChecksOn => TimingChecksOn);
      end for;
   end for;
end BoardDesign_Configuration; --============= End of configuration ==========--
