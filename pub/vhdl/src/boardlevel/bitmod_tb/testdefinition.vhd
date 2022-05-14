--============================================================================--
-- Design units : TestDefinition (Package declaration and body)
--
-- File name    : testdefinition.vhd
--
-- Purpose      : This packages defines the MISR subprograms used by the
--                Test Generator for the verification of the Bit Modulator.
--
-- Note         : 
--
-- Limitations  : None known
--
-- Errors       : None known
--
-- Library      : BitMod_TB_Lib
--
-- Dependencies : IEEE.Std_Logic_1164
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
-- 0.1     SH      1 July 95 New package
--------------------------------------------------------------------------------

library IEEE;
use IEEE.Std_Logic_1164.all;

package TestDefinition is
   -----------------------------------------------------------------------------
   -- Reset of MISR
   -----------------------------------------------------------------------------
   procedure ResetMISR(signal MISRReset: out Boolean);
 
   -----------------------------------------------------------------------------
   -- Check of MISR
   -----------------------------------------------------------------------------
   procedure CheckMISR(
      constant InVec:      in    Std_Logic_Vector(0 to 15);
      constant RefVec:     in    String(1 to 4);
      variable TestFailed: inout Boolean;
      constant HeaderMsg:  in    String := "");
 
end TestDefinition; --=============== End of package header ==================--

package body TestDefinition is
   -----------------------------------------------------------------------------
   -- Reset of MISR
   -----------------------------------------------------------------------------
   procedure ResetMISR(signal MISRReset: out Boolean) is
   begin
      -- TBD
   end ResetMISR;
 
   -----------------------------------------------------------------------------
   -- Check of MISR
   -----------------------------------------------------------------------------
   procedure CheckMISR(
      constant InVec:      in    Std_Logic_Vector(0 to 15);
      constant RefVec:     in    String(1 to 4);
      variable TestFailed: inout Boolean;
      constant HeaderMsg:  in    String := "") is
   begin
      -- TBD
   end CheckMISR;
 
end TestDefinition; --=============== End of package body ====================--
