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
