--============================================================================--
-- Design units : BitMod_Timing (Package declaration and body)
--
-- File name    : bitmod_timing.vhd
--
-- Purpose      : In this package, all timing parameters for the Bit Modulator
--                are defined as deferred constants; their value can be modified
--                by recompiling only the package body.
--
-- Note         : The timing figures have been taken from the data sheet for the
--                Bit Modulator dated January 1995. The timing figures
--                are based on 25 pF load on the outputs. The timing parameter
--                names are compliant with Vital Level 0.
--
-- Limitations  : Best case and typical figures have been approximated:  
--                Best case    = Worst case / 4, for path delays,
--                Typical case = Worst case / 2, for path delays.
--
-- Errors:      : None known
--
-- Library      : BitMod_Lib
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

package BitMod_Timing is
   -----------------------------------------------------------------------------
   -- Deferred constants for the timing parameters, all values are defined in
   -- the package body.
   --
   -- Input Test is not allowed to change when Reset_N is de-asserted 
   -- (checked in model).
   -----------------------------------------------------------------------------

   -- System signal timing parameters                  Data sheet reference
   constant tperiod_Clk:       TimeArray;              -- TClk
   constant tpw_Clk_posedge:   TimeArray;              -- TCHi
   constant tpw_Clk_negedge:   TimeArray;              -- TCLo

   -- Parameters TSCk and TRes depend on the architectural design and can not
   -- be changed by the user. They are defined in the architecture of the 
   -- functional core.

   -- Internal registers interface timing parameters
   constant tpw_CSN_negedge:   TimeArray;              -- T1
   constant tsetup_D_CSN:      TimeArray;              -- T2
   constant thold_D_CSN:       TimeArray;              -- T3
   constant tpd_CSN_D_negedge: TimeArray;              -- T4 Tri-state modelling
   constant tpd_CSN_D_posedge: TimeArray;              -- T5 -------- " --------
   constant tpd_A_D:           TimeArray;              -- T6

   -- Parameters T7 and T8 depend on the architectural design and can not 
   -- be changed by the user. They are defined in the architecture of the 
   -- functional core.
   
   -- Output interface timing parameters
   constant tpd_Clk_MData:     TimeArray01;            -- T9

end BitMod_Timing; --=============== End of package header ===================--

package body BitMod_Timing is

   -- System signal timing parameters      WC     Typ    BC         Ref.
   constant tperiod_Clk:       TimeArray := (80 ns, 80 ns, 80 ns); -- TClk
   constant tpw_Clk_posedge:   TimeArray := (40 ns, 40 ns, 40 ns); -- TCHi
   constant tpw_Clk_negedge:   TimeArray := (40 ns, 40 ns, 40 ns); -- TCLo
 
   -- Internal registers interface timing parameters
   constant tpw_CSN_negedge:   TimeArray := (40 ns, 30 ns, 20 ns); -- T1
   constant tsetup_D_CSN:      TimeArray := (15 ns, 10 ns,  6 ns); -- T2
   constant thold_D_CSN:       TimeArray := (10 ns,  8 ns,  3 ns); -- T3
   constant tpd_CSN_D_negedge: TimeArray := (30 ns, 15 ns,  8 ns); -- T4
   constant tpd_CSN_D_posedge: TimeArray := (40 ns, 20 ns, 10 ns); -- T5
   constant tpd_A_D:           TimeArray := (30 ns, 15 ns,  8 ns); -- T6

   -- Output interface timing parameter                            -- T9
   constant tpd_Clk_MData:     TimeArray01 := ((25 ns, 24 ns),     -- WC
                                               (11 ns, 13 ns),     -- TC
                                               ( 7 ns,  8 ns));    -- BC

end BitMod_Timing; --================= End of package body ===================--
