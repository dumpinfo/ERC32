--============================================================================--
-- Design units : BitMod_Definition (Package declaration and body)
--
-- File name    : bitmod_definition.vhd
--
-- Purpose      : This packages defines the conversion functions used for
--                the BitMod design which are not contained in the
--                IEEE Std_Logic_1164 package.
--
-- Note         : Functions are held as simple as possible.
--
-- Limitations  : None known
--
-- Errors       : None known
--
-- Library      : BitMod_Lib
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

package BitMod_Definition is

   -----------------------------------------------------------------------------
   -- Converts Integer to Std_Logic_Vector (0 to Size-1).
   -----------------------------------------------------------------------------
   function  To_StdLogicVector(
      constant Arg:                 Integer;
      constant Size:                Integer        := 8)
      return                        Std_Logic_Vector;

   -----------------------------------------------------------------------------
   -- Converts unsigned Std_Logic_Vector to Integer, leftmost bit is MSB
   -- Error message for unknowns (U, X, W, Z, -), converted to 0
   -- Verifies whether vector is too long (> 31 bits)
   -----------------------------------------------------------------------------
   function  To_Integer(
      constant Vector:              Std_Logic_Vector;
      constant VectorName:          String;
      constant HeaderMsg:           String         := "To_Integer";
      constant MsgSeverity:         Severity_Level := Warning)
      return                        Integer;

   -----------------------------------------------------------------------------
   -- Checks the relation between two clocks
   -- If FasterThanRef = True,
   -- then the TestSignal may not have more than Period rising edges 
   -- between two RefSignal rising edges.
   -- If FasterThanRef = False,
   -- then the RefSignal may not have more than Period rising edges 
   -- between two TestSignal rising edges.
   -----------------------------------------------------------------------------
   procedure PeriodCheck(
      variable Violation:       out   X01;
      variable PeriodData:      inout Integer;
      signal   TestSignal:      in    Std_ULogic;
      constant TestSignalName:  in    String         := "";
      signal   RefSignal:       in    Std_ULogic;
      constant RefSignalName:   in    String         := "";
      constant Period:          in    Integer        := 0;
      constant FasterThanRef:   in    Boolean        := True;
      constant CheckEnabled:    in    Boolean        := True;
      constant HeaderMsg:       in    String         := "PeriodCheck:";
      constant XOn:             in    Boolean        := True;
      constant MsgOn:           in    Boolean        := True;
      constant MsgSeverity:     in    Severity_Level := Warning);


   -----------------------------------------------------------------------------
   -- Checks the TestPort stable width w.r.t. number of RefPort rising edges.
   -----------------------------------------------------------------------------
   procedure CheckWidth(
      variable Violation:       out   X01;
      variable PeriodData:      inout Integer;
      signal   TestSignal:      in    Std_ULogic;
      constant TestSignalName:  in    String         := "";
      constant TestSignalLevel: in    Std_ULogic     := '0';
      signal   RefSignal:       in    Std_ULogic;
      constant RefSignalName:   in    String         := "";
      constant Period:          in    Integer        := 0;
      constant CheckEnabled:    in    Boolean        := True;
      constant HeaderMsg:       in    String         := "CheckWidth:";
      constant XOn:             in    Boolean        := True;
      constant MsgOn:           in    Boolean        := True;
      constant MsgSeverity:     in    Severity_Level := Warning);

   -----------------------------------------------------------------------------
   -- Converts Std_ULogic to X01.
   -- Asserts that no unknown value exists at the time of conversion.
   -- Reports with the Severity_Level MsgSeverity the signal name TestName.
   -- No default value for TestName since a resolution conflict can occur 
   -- between the functions with the same name in this package and in package 
   -- IEEE.Std_Logic_1164. X is propagated.
   -----------------------------------------------------------------------------
   function  To_X01(
      constant Test:                Std_ULogic;
      constant TestName:            String;
      constant HeaderMsg:           String         := "To_X01:";
      constant MsgSeverity:         Severity_Level := Warning)
      return                        Std_ULogic;

   -----------------------------------------------------------------------------
   -- Converts Std_Logic_Vector to Std_ULogic_Vector of X01.
   -- Asserts that no unknown value exists at the time of conversion.
   -- Reports with the Severity_Level MsgSeverity the signal name TestName.
   -- No default value for TestName since a resolution conflict can occur 
   -- between the functions with the same name in this package and in package
   -- IEEE.Std_Logic_1164. X is propagated.
   -----------------------------------------------------------------------------
   function  To_X01(
      constant Test:                Std_Logic_Vector;
      constant TestName:            String;
      constant HeaderMsg:           String         := "To_X01:";
      constant MsgSeverity:         Severity_Level := Warning)
      return                        Std_ULogic_Vector;

end BitMod_Definition; --============ End of package header ==================--

package body BitMod_Definition is

   -----------------------------------------------------------------------------
   -- Converts Integer to Std_Logic_Vector (0 to Size-1).
   -----------------------------------------------------------------------------
   function  To_StdLogicVector(
      constant Arg:                 Integer;
      constant Size:                Integer        := 8)
      return                        Std_Logic_Vector is
   begin       
      return Std_Logic_Vector'("000");
      -- TBD
   end To_StdLogicVector;

   -----------------------------------------------------------------------------
   -- Converts unsigned Std_Logic_Vector to Integer
   -- Leftmost bit is most significant
   -- Error message for unknowns (U, X, W, Z, -), which are converted to 0
   -- Verifies whether vector is too long (> 31 bits)
   -----------------------------------------------------------------------------
   function  To_Integer(
      constant Vector:              Std_Logic_Vector;
      constant VectorName:          String;
      constant HeaderMsg:           String         := "To_Integer";
      constant MsgSeverity:         Severity_Level := Warning)
      return                        Integer is
   begin
      return Integer'(0);
      -- TBD
   end To_Integer;

   -----------------------------------------------------------------------------
   -- Checks the relation between two clocks
   -- If FasterThanRef = True,
   -- then the TestSignal may not have more than Period rising edges
   -- between two RefSignal rising edges.
   -- If FasterThanRef = False,
   -- then the RefSignal may not have more than Period rising edges
   -- between two TestSignal rising edges.
   -----------------------------------------------------------------------------
   procedure PeriodCheck(
      variable Violation:       out   X01;
      variable PeriodData:      inout Integer;
      signal   TestSignal:      in    Std_ULogic;
      constant TestSignalName:  in    String         := "";
      signal   RefSignal:       in    Std_ULogic;
      constant RefSignalName:   in    String         := "";
      constant Period:          in    Integer        := 0;
      constant FasterThanRef:   in    Boolean        := True;
      constant CheckEnabled:    in    Boolean        := True;
      constant HeaderMsg:       in    String         := "PeriodCheck:";
      constant XOn:             in    Boolean        := True;
      constant MsgOn:           in    Boolean        := True;
      constant MsgSeverity:     in    Severity_Level := Warning) is
   begin 
      -- TBD
   end PeriodCheck;

   -----------------------------------------------------------------------------
   -- Checks the TestPort stable width w.r.t. number of RefPort rising edges.
   -----------------------------------------------------------------------------
   procedure CheckWidth(
      variable Violation:       out   X01;
      variable PeriodData:      inout Integer;
      signal   TestSignal:      in    Std_ULogic;
      constant TestSignalName:  in    String         := "";
      constant TestSignalLevel: in    Std_ULogic     := '0';
      signal   RefSignal:       in    Std_ULogic;
      constant RefSignalName:   in    String         := "";
      constant Period:          in    Integer        := 0;
      constant CheckEnabled:    in    Boolean        := True;
      constant HeaderMsg:       in    String         := "CheckWidth:";
      constant XOn:             in    Boolean        := True;
      constant MsgOn:           in    Boolean        := True;
      constant MsgSeverity:     in    Severity_Level := Warning) is
   begin 
      -- TBD
   end CheckWidth;

   -----------------------------------------------------------------------------
   -- Converts Std_ULogic to X01.
   -- Asserts that no unknown value exists at the time of conversion.
   -- Reports with the Severity_Level MsgSeverity the signal name TestName.
   -- No default value for TestName since a resolution conflict can occur
   -- between the functions with the same name in this package and in package
   -- IEEE.Std_Logic_1164. X is propagated.
   -----------------------------------------------------------------------------
   function To_X01(
      constant Test:                Std_ULogic;
      constant TestName:            String;
      constant HeaderMsg:           String         := "To_X01:";
      constant MsgSeverity:         Severity_Level := Warning)
      return                        Std_ULogic is
   begin 
      return To_X01(Test);
      -- TBD
   end To_X01;
 
   -----------------------------------------------------------------------------
   -- Converts Std_Logic_Vector to Std_ULogic_Vector of X01.
   -- Asserts that no unknown value exists at the time of conversion.
   -- Reports with the Severity_Level MsgSeverity the signal name TestName.
   -- No default value for TestName since a resolution conflict can occur
   -- between the functions with the same name in this package and in package
   -- IEEE.Std_Logic_1164. X is propagated.
   -----------------------------------------------------------------------------
   function  To_X01(
      constant Test:                Std_Logic_Vector;
      constant TestName:            String;
      constant HeaderMsg:           String         := "To_X01:";
      constant MsgSeverity:         Severity_Level := Warning)
      return                        Std_ULogic_Vector is
   begin
      return Std_ULogic_Vector'("000");
      -- TBD
   end To_X01;

end BitMod_Definition; --================ End of package body ================--
