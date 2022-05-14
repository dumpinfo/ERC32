 
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
 
 

