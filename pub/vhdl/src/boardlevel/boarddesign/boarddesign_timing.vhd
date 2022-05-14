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
