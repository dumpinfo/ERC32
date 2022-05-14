--============================================================================--
-- Design units : MISR_Definition (Package declaration and body)
--
-- File name    : misr_definition.vhd
--
-- Purpose      : This packages defines a MISR subprogram to be used for
--                verification of models.
--
-- Note         : (see further below)
--
-- Limitations  : None known
--
-- Errors       : None known
--
-- Library      : (independent)
--
-- Dependencies : IEEE.Std_Logic_1164
--
-- Author       : Sandi Habinc, sandi@ws.estec.esa.nl
--                ESTEC Microelectronics and Technology Section (WSM)
--                P.O. Box 299
--                2200 AG Noordwijk
--                The Netherlands
--
-- Copyright    : European Space Agency (ESA) 1996. No part may be reproduced
--                in any form without the prior written permission of ESA.
--
-- Simulator    : Synopsys v. 3.3b, on Sun SPARCstation 5, SunOS 4.1.3_U1
--------------------------------------------------------------------------------
-- Revision list
-- Version Author Date       Changes
--
-- 0.1     SH      1 July 95 New package
-- 0.2     SH      1 May  96 MISR made independent of bit ordering of inputs
--------------------------------------------------------------------------------

--============================================================================--
-- Multiple-Input Signature Register (MISR)
-- This procedure implements a variable length MISR. The length is determined by
-- the length of Input, ranging from 4 to 100. The Input can be sampled on
-- either Clk edge or both, delayed by Sense, selected with the Rising and
-- Falling parameters. If neither option is selected, events on Input will
-- determine the sampling point. Events happening in the same simulation cycle,
-- differing only in delta cycles, will be sampled when the last event has
-- occurred, and the MISR will then be shifted.
--
-- The rising edge of the Reset input will reset the MISR to all-ones. 
-- When sampling is made with Clk, it will re-start on the next relevant Clk 
-- edge after the rising Reset edge. If Reset is detected between a Clk edge 
-- and the sampling point, the MISR will be reset and the sample will be 
-- ignored. If there is a relevant Clk edge in the same delta cycle as the 
-- rising Reset edge, the sample will be ignored. But if the relevant Clk edge 
-- is in a delta cycle after the Rising Reset edge, then the sample will be 
-- made.  When asynchronous sampling is used, the first event on Input after the
-- rising Reset edge (in the next or following delta cycles) will trigger the 
-- first sample and shift the MISR. The MISR signal can be read at any point and
-- should be compared with a predetermined signature.
--
-- The MISR is implemented as a primitive polynomial with up to five terms. The
-- terms are taken from the text book Built-In Test for VLSI: Pseudorandom
-- Techniques, by Bardell et al. The elements in the Input vector are expanded
-- to four bits, each Std_Logic value having a unique bit pattern,
-- the intermediate vector is then divided in four and each part is shifted into
-- the MISR separately. The procedure can be used as a concurrent subprogram,
-- not needing any surrounding process or block.
--
-- The MISR is shifted from left to right, independently of falling or rising
-- bit order definition (i.e. to or downto).
--
-- Inputs:  Clk,   sample clock used with Rising and Falling
--          Reset, reset of MISR when an event is detected and Reset is True
--          Input, input vector to the MISR, same length and order as MISR
--          Rising/Falling:
--          False  False  sample at each Input event
--          False  True   sample Input after Sense on falling Clk edge
--          True   False  sample Input after Sense on rising Clk edge
--          True   True   sample Input after Sense on rising or falling Clk edge
--          Sense, positive time after the Clk edge when Input is sampled
--          HeaderMsg, message header
-- In/Outs: MISR, Multiple-Input Signature Register
--
-- Author:  Sandi Habinc, ESTEC Microelectronics and Technology Section (WSM)
--          P.O. Box 299, 2200 AG Noordwijk, The Netherlands
--------------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;

package MISR_Definition is
   procedure MISR(
      signal   Clk:       in    Std_ULogic;            -- Sample clock
      signal   Reset:     in    Boolean;               -- Reset of MISR
      signal   Input:     in    Std_Logic_Vector;      -- Input vector
      signal   MISR:      inout Std_Logic_Vector;      -- MISR
      constant Rising:    in    Boolean := True;       -- See above
      constant Falling:   in    Boolean := False;      -- See above
      constant HeaderMsg: in    String  := "MISR:";    -- Message header
      constant Sense:     in    Time    := 0 ns);      -- Sense time after clock

end MISR_Definition; --=============== End of package header =================--

package body MISR_Definition is

   -----------------------------------------------------------------------------
   -- Local declarations of minimum and maximum MISR lengths.
   -----------------------------------------------------------------------------
   constant MaxLen:   Integer := 100;
   constant MinLen:   Integer := 4;

   -----------------------------------------------------------------------------
   -- Local declarations defining subtypes and types needed for the definition
   -- of the Std_Logic to 4bit vector transfer function. Each Std_Logic value
   -- has a unique 4bit vector associated.
   -----------------------------------------------------------------------------
   subtype  Index  is Integer range 0 to 3;            -- Definition of table
   subtype  Vector is Std_Logic_Vector(Index);         -- with a 4bit vector for
   type     VTable is array (Std_Logic) of Vector;     -- each Std_Logic value
   constant Std4:     VTable := ('U' => "0001", 'X' => "0010", '0' => "0100",
                                 '1' => "1000", 'Z' => "0011", 'W' => "0110",
                                 'L' => "1100", 'H' => "0111", '-' => "1110");

   -----------------------------------------------------------------------------
   -- Expands every Std_Logic_Vector element to four bits returning a
   -- Std_Logic_Vector with four times the length of the input.
   -- The expansion is always made from left to right, bit order independent.
   -- Input:  V Std_Logic_Vector
   -- Output:   Std_Logic_Vector (0 to (V'Length*4-1))
   -----------------------------------------------------------------------------
   function To01(V:         Std_Logic_Vector;
                 HeaderMsg: String := "MISR:") return Std_Logic_Vector is
      variable R: Std_Logic_Vector(0 to (V'Length*4-1));
      variable J: Integer := 0;
   begin
      assert (V'Length<=MaxLen) and (V'Length>=MinLen) -- Check length
         report HeaderMsg&" Only vectors of Length 4 to 100 are supported."
         severity Failure;

      for i in V'Range loop
         R(J*4+0 to J*4+3):= Std4(V(i));               -- Expand input
         J := J + 1;
      end loop;
      return R;
   end To01;

   -----------------------------------------------------------------------------
   -- Logical and operation between Std_ULogic scalar and Std_Logic_Vector.
   -- Input:  B Std_ULogic scalar, V Std_Logic_Vector vector
   -- Output: Std_Logic_Vector with same array definition as V
   -----------------------------------------------------------------------------
   function "and" (B: Std_ULogic;
                   V: Std_Logic_Vector) return Std_Logic_Vector is
      variable R: Std_Logic_Vector(V'Range);
   begin
      for i in V'Range loop
         R(i):= B and V(i);                            -- logical and
      end loop;
      return R;                                        -- return vector
   end "and";

   -----------------------------------------------------------------------------
   -- Expands the Input four times using To01, the resulting vector is then
   -- shifted into the MISR in four parts. The resulting MISR value is returned.
   -- The MISR is shifted from left to right, independently of bit ordering.
   -- Input is applied to the MISR from left to right, independently of bit
   -- ordering. Note that Input is expanded four times before applied to MISR.
   -- Input:  MISR and Input may have rising or falling bit ordering
   --         Poly shall be defined as (0 to Input'Length-1)
   -- Output: Std_Logic_Vector defined as (0 to Input'Length-1)
   -----------------------------------------------------------------------------
   function Shift(MISR:  Std_Logic_Vector;
                  Input: Std_Logic_Vector;
                  Poly:  Std_Logic_Vector;
                  HeaderMsg: String:="MISR:") return Std_Logic_Vector is
      variable M: Std_Logic_Vector(0 to   Input'Length-1) := MISR;
      variable T: Std_Logic_Vector(0 to 4*Input'Length-1) := To01(Input);
   begin
      for i in 0 to 3 loop                             -- Four MISR shifts
         M := ('0'&M(0 to Input'Length-2))               xor
               T(Input'Length*i to Input'Length*(i+1)-1) xor
               (M(Input'Length-1) and Poly);           -- Scalar and vector
      end loop;
      return M;                                        -- Return resulting MISR
   end Shift;

   -----------------------------------------------------------------------------
   -- Returns a Std_Logic_Vector of length L (1 to 100) containing a primitive
   -- polynomial with up to five terms (always including x+1).
   -- Input:  L, the length of the resulting polynomial Std_Logic_Vector
   -- Output: polynomial Std_Logic_Vector (0 to L-1)
   -----------------------------------------------------------------------------
   function Polynomial(L:         Natural;
                       HeaderMsg: String:="MISR:") return Std_Logic_Vector is
      variable P:        Std_Logic_Vector(0 to L-1) := ('1', others => '0');
      subtype  Degree is Integer range 0 to MaxLen;    -- Exponent range
      type     Terms  is array (1 to 3) of Degree;     -- Triplet
      type     TTable is array (1 to MaxLen) of Terms; -- Triplets
      constant Table:    TTable := (                   -- Look-up table
         (0,0,0),   (1,0,0),   (1,0,0),   (1,0,0),   (2,0,0),   (1,0,0),   --  1
         (1,0,0),   (6,5,1),   (4,0,0),   (3,0,0),   (2,0,0),   (7,4,3),   --  7
         (4,3,1),   (12,11,1), (1,0,0),   (5,3,2),   (3,0,0),   (7,0,0),   -- 13
         (6,5,1),   (3,0,0),   (2,0,0),   (1,0,0),   (5,0,0),   (4,3,1),   -- 19
         (3,0,0),   (8,7,1),   (8,7,1),   (3,0,0),   (2,0,0),   (16,15,1), -- 25
         (3,0,0),   (28,27,1), (13,0,0),  (15,14,1), (2,0,0),   (11,0,0),  -- 31
         (12,10,2), (6,5,1),   (4,0,0),   (21,19,2), (3,0,0),   (23,22,1), -- 37
         (6,5,1),   (27,26,1), (4,3,1),   (21,20,1), (5,0,0),   (28,27,1), -- 43
         (9,0,0),   (27,26,1), (16,15,1), (3,0,0),   (16,15,1), (37,36,1), -- 49
         (24,0,0),  (22,21,1), (7,0,0),   (19,0,0),  (22,21,1), (1,0,0),   -- 55
         (16,15,0), (57,56,1), (1,0,0),   (4,3,1),   (18,0,0),  (10,9,1),  -- 61
         (10,9,1),  (9,0,0),   (29,27,2), (16,15,1), (6,0,0),   (53,47,6), -- 67
         (25,0,0),  (16,15,1), (11,10,1), (36,35,1), (31,30,1), (20,19,1), -- 73
         (9,0,0),   (38,37,1), (4,0,0),   (38,35,3), (46,45,1), (13,0,0),  -- 79
         (28,27,1), (13,12,1), (13,0,0),  (72,71,1), (38,0,0),  (19,18,1), -- 85
         (84,83,1), (13,12,1), (2,0,0),   (12,0,0),  (11,0,0),  (49,47,2), -- 91
         (6,0,0),   (11,0,0),  (47,45,2), (37,0,0));                       -- 97
   begin
      assert (L <= MaxLen) and (L > 0)                 -- Check length
         report HeaderMsg&" Only polynomial degree of 1 to 100 is supported."
         severity Failure;
      for i in 1 to 3 loop
         P(Table(L)(i)) := '1';                        -- Insert terms
      end loop;
      return P;                                        -- Return polynomial
   end Polynomial;

   -----------------------------------------------------------------------------
   -- See definition in the package header.
   -----------------------------------------------------------------------------
   procedure MISR(
      signal   Clk:       in    Std_ULogic;            -- Sample clock
      signal   Reset:     in    Boolean;               -- Reset of MISR
      signal   Input:     in    Std_Logic_Vector;      -- Input vector
      signal   MISR:      inout Std_Logic_Vector;      -- MISR
      constant Rising:    in    Boolean := True;       -- See above
      constant Falling:   in    Boolean := False;      -- See above
      constant HeaderMsg: in    String  := "MISR:";    -- Message header
      constant Sense:     in    Time    := 0 ns) is    -- Sense time after clock

      constant Poly: Std_Logic_Vector              := Polynomial(Input'Length);
      constant Ones: Std_Logic_Vector(Input'Range) := (others => '1');
      variable Temp: Std_Logic_Vector(Input'Range) := Input; -- Last Input
      variable Last: Time                          := 0 ns;  -- Last sim cycle
   begin
      assert (Input'Length <= MaxLen) and (Input'Length >= MinLen)
         report HeaderMsg&" Only MISRs of length 4 to 100 are supported."
         severity Failure;
      assert Input'Length = MISR'Length
         report HeaderMsg&" The length of the Input and the MISR differs."
         severity Failure;
      assert Input'Left   = MISR'Left
         report HeaderMsg&" The left index of the Input and the MISR differs."
         severity Failure;
      assert Input'Right = MISR'Right
         report HeaderMsg&" The right index of the Input and the MISR differs."
         severity Failure;

      while True loop                                  -- Loop never exited
         if not Rising and not Falling then
            -- Clk input has no influence on the MISR in asynchronous mode.
            -- Sampling only the final input vector in each simulation cycle.
            wait on Input, Reset;                      -- Asynchronous sample
            if not (Reset'Event and Reset) and         -- Check not reset
               Input'Event then                        -- Check for Input event
               if Now = Last then                      -- New delta cycle
                  Temp := Input;                       -- Store until next event
               else                                    -- New sim cycle
                  MISR <= Shift(MISR, Temp, Poly, HeaderMsg);
                  Temp := Input;                       -- Store until next event
                  Last := Now;                         -- Store sim time
               end if;
            end if;
         else
            wait on Clk, Reset;                        -- Synchronous sample
            if not (Reset'Event and Reset) and         -- Check not reset
               ((Rising and Rising_Edge(Clk)) or       -- Check rising edge
                (Falling and Falling_Edge(Clk))) then  -- Check falling edge
               wait on Reset until Reset for Sense;    -- Delay until sample
               MISR <= Shift(MISR, Input, Poly, HeaderMsg);
            end if;
         end if;
         if Reset'Event and Reset then
            MISR <= Ones;                              -- Reset MISR
            Temp := Ones;                              -- Reset Temp
         end if;
      end loop;
   end MISR;
end MISR_Definition;  --================ End of package body =================--
