 
--====================================================================--
-- Design unit  : Simulation (Package declaration)
--
-- File name    : simulation.vhd
--
-- Purpose      : In this package the enumerated type SimConditionType,
--                to be used to select Worst, Typical or Best Case
--                values for timing parameters in VHDL models for
--                board-level simulation.
--
--                The simulation condition will normally be selected
--                by a generic parameter in the top-level entity
--
-- Note         : A type TimeArray has been defined, which can be used
--                for defining the timing parameters.
--
-- Errors:      : None known
--
-- Library      : ESA
--
-- Dependencies : None
--
-- Author       : Sandi Habinc, Peter Sinander
--                ESTEC Onboard Data Division (WD)
--                P.O. Box 299
--                2200 AG Noordwijk
--                The Netherlands
--
-- Simulator    : Synopsys v. 3.0c, on Sun Sparcstation 10, SunOS 4.1.3
------------------------------------------------------------------------
-- Revision list
-- Version Author Date       Changes
--
-- 1.0     PSI     1 Sep 94  New version
------------------------------------------------------------------------
 
 
package Simulation is
 
   -- Definition of the SimConditionType type
   type SimConditionType is (WorstCase, TypCase, BestCase);
 
 
   -- Definition of Time array type which can be used for the timing
   -- parameters
   type TimeArray is array(SimConditionType) of Time;
 
end Simulation;  --======= End of package Simulation =================--
 
 

--============================================================================--
-- Design unit  : Timing (Package declaration)
--
-- File name    : timing.vhd
--
-- Purpose      : This package defines three array types, indexed by the ESA 
--                SimConditionType, needed for timing generics when using 
--                Vital Delay Types. The types are intended to be used 
--                in VHDL models for board-level simulation. This package
--                should not be modified or moved to a different library.
--
-- Note:          The type TimeArray has been defined in ESA.Simulation.
--
--                This package does not define any types related to the 
--                Vital Delay Array Types, since it is not possible to 
--                define a constrained array of unconstrained arrays. Such
--                declarations should be done in the timing package of the
--                component.
--
-- Errors:      : None known
--
-- Library      : ESA
--
-- Dependencies : ESA.Simulation, IEEE.Vital_Timing.
--
-- Author       : Sandi Habinc, Peter Sinander
--                ESTEC Microelectronics and Technology Section (WSM)
--                P.O. Box 299
--                2200 AG Noordwijk
--                The Netherlands
--
-- Simulator    : Synopsys v. 3.2c, on Sun Sparcstation 10, SunOS 4.1.3
--------------------------------------------------------------------------------
-- Revision list
-- Version Author Date       Changes
--
-- 1.0     SH      1 July 95 New version
--------------------------------------------------------------------------------
library ESA;
use ESA.Simulation.all;

library IEEE;
use IEEE.Vital_Timing.all;

package Timing is

   -- Definition of Time array types, which can be used for the timing
   -- parameters with Vital Delay Types. 
   type TimeArray01   is array (SimConditionType) of VitalDelayType01;
   type TimeArray01Z  is array (SimConditionType) of VitalDelayType01Z;
   type TimeArray01ZX is array (SimConditionType) of VitalDelayType01ZX;

end Timing; --==================== End of package body =======================--
