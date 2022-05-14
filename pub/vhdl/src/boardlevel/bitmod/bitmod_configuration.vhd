--============================================================================--
-- Design units : BitMod_Configuration (Configuration)
--
-- File name    : bitmod_configuration.vhd
--
-- Purpose      : Board-level simulation for verification of a board
--                containing a Bit Modulator component.
--
-- Note         : 
--
-- Limitations  : 
--
-- Errors       : None known
--
-- Library      : BitMod_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                ESA.Simulation,
--                BitMod_Lib.BitMod.
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
 
library BitMod_Lib;
configuration BitMod_Configuration of BitMod is
   for BoardLevel
      for FunctionalCore: BitMod_Core
         use entity BitMod_Lib.BitMod_Core(Behavioural)
            generic map(InstancePath => InstancePath);
      end for;
   end for;
end BitMod_Configuration; --============== End of configuration ==============-- 
