---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
 
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
 
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
 
--  You should have received a copy of the GNU Library General Public
--  License along with this library; if not, write to the Free
--  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

---------------------------------------------------------------------------
-- Title:
-- File name:
-- VHDL unit:                  (Type)
-- Purpose and functionality:  (Text)
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--  SAAB ERICSSON SPACE AB                                               --
--  Delsjomotet                      Phone Int: +46 31 35 00 00          --
--  S-405 15 GOTHENBURG              Fax   Int: +46 31 35 95 20          --
--  Sweden                           Telex:     27950 saabsp s           --
---------------------------------------------------------------------------
 

--
--

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;

package MECPackage is

    type UX01_Vector is array (natural range <>) of UX01;

--- Subtype used in the EDAC
   subtype CheckBits   is std_logic_Vector ( 6 downto 0 );
   subtype SyndromBits is std_logic_Vector ( 7 downto 0 );
   subtype DataBits    is std_logic_Vector ( 31 downto 0 );

    function ParityGen(Val : Std_Logic_Vector) return UX01;

    function ParityCheck(Val: Std_Logic_Vector; Parity: UX01)
                         return UX01;

    function Decrement(Val : Std_Logic_Vector) return Std_Logic_Vector;

--    function Hex2Vec(HexInput: string; Width: positive)
--                     return Std_Logic_Vector;

--    function To_UX01_Vector (Input: Std_Logic_Vector)
--                             return UX01_vector;
-- This function is not used and therfore removed

    function Bool_To_UX01 (Val : Boolean) return UX01;

    function Vector_OR ( Data : Std_Logic_Vector ) return UX01;

--    function Bit_And_Vector ( Bit : UX01; Vec : Std_Logic_Vector) return Std_Logic_Vector;
-- This function is not used and therfore removed

    function Compare (A,B : Std_Logic_Vector) return UX01;

    --- Generate check bits
    function ChkGen ( Data : in DataBits ) return CheckBits;

    --- Generate syndrom bits
    function SyndromGen ( ChkBits_In : CheckBits;
                          Chk_Ok     : CheckBits;
                          Parity     : std_logic ) return SyndromBits;

    function SyndromGenS ( ChkBits_In : CheckBits;
                           Chk_Ok     : CheckBits;
                           Chk_Ok7    : std_logic;
                           Parity     : std_logic ) return SyndromBits;
                           
                       
     function Correct_Data ( Syndrom : in  SyndromBits;
                             Data   : in  Std_logic_vector ) return std_logic_vector;

     function NCError_Gen ( Syndrom : in  SyndromBits ) return std_logic;

     function CError_Gen ( Syndrom : in  SyndromBits ) return std_logic;

end MECPackage;

---------------------------------------------------------------------------
package body MECPackage is

    type Logic_UX01_Table is array (Std_ulogic'low to Std_ulogic'high) of UX01;

    constant Cvt_To_UX01 : Logic_UX01_Table := (
                         'U',  -- 'U'
                         'X',  -- 'X'
                         '0',  -- '0'
                         '1',  -- '1'
                         'X',  -- 'Z'
                         'X',  -- 'W'
                         '0',  -- 'L'
                         '1',  -- 'H'
                         'X'   -- '-'
                        );

---------------------------------------------------------------------------
    function ParityGen(Val : Std_Logic_Vector) return UX01 is

        --Normalize the indexing
        alias Data     : Std_Logic_Vector(Val'length downto 1) is Val;
        variable Result : UX01 := '1';
    begin
        for K in 1 to Data'length loop
            Result := Result xor Data(K);
        end loop;
        return Result;
    end ParityGen;

---------------------------------------------------------------------------
    function ParityCheck(Val: Std_Logic_Vector; Parity: UX01)
                                               return UX01 is

        --Normalize the indexing
        alias Data     : Std_Logic_Vector(Val'length downto 1) is Val;
        variable Result : UX01 := '1';
    begin
        for K in 1 to Data'length loop
            Result :=  Result xor Data(K);
        end loop;
        return (Result xor Parity);
    end ParityCheck;

---------------------------------------------------------------------------
    function Decrement(Val : Std_Logic_Vector) return Std_Logic_Vector is

        -- normalize the indexing
        alias Input     : Std_Logic_Vector(Val'length downto 1) is Val;
        variable Result : Std_Logic_Vector(Input'range) := Input;
    begin
        for K in 1 to Input'length loop
            Result(K) := not Input(K);
            exit when Input(K) = '1';
        end loop;
        return Result;
    end Decrement;

---------------------------------------------------------------------------
--    function Hex2Vec(HexInput: string; Width: positive)
--             return Std_Logic_Vector is
--
--        -- normalize the indexing
--        alias Hex : String(HexInput'length downto 1) is HexInput;
--        variable Temp   : Std_Logic_Vector(HexInput'length*4 downto 0)
--                          := (others => '0');
--        variable Result : Std_Logic_Vector(Width-1 downto 0)
--                          := (others => '0');
--        variable J,K : natural;
--    begin
--
--        J := 0;
--
--        for K in 1 to HexInput'length loop
--
--            if not (Hex(K) = '_') then
--
--                case Hex(K) is
--                    when '0'     => Temp(J+3 downto J) := "0000";
--
--                    when '1'     => Temp(J+3 downto J) := "0001";
--
--                    when '2'     => Temp(J+3 downto J) := "0010";
--
--                    when '3'     => Temp(J+3 downto J) := "0011";
--
--                    when '4'     => Temp(J+3 downto J) := "0100";
--
--                    when '5'     => Temp(J+3 downto J) := "0101";
--
--                    when '6'     => Temp(J+3 downto J) := "0110";
--
--                    when '7'     => Temp(J+3 downto J) := "0111";
--
--                    when '8'     => Temp(J+3 downto J) := "1000";
--
--                    when '9'     => Temp(J+3 downto J) := "1001";
--
--                    when 'a'|'A' => Temp(J+3 downto J) := "1010";
--
--                    when 'b'|'B' => Temp(J+3 downto J) := "1011";
--
--                    when 'c'|'C' => Temp(J+3 downto J) := "1100";
--
--                    when 'd'|'D' => Temp(J+3 downto J) := "1101";
--
--                    when 'e'|'E' => Temp(J+3 downto J) := "1110";
--
--                    when 'f'|'F' => Temp(J+3 downto J) := "1111";
--
--                    when others  => Temp(J+3 downto J) := "XXXX";
--                end case;
--
--                J := J+4;
--
--            end if;
--        end loop;
--
--        Result := Temp(Width-1 downto 0);
--        return Result;
--
--    end Hex2Vec;
--
---------------------------------------------------------------------------
    function Bool_To_UX01 (Val : Boolean) return UX01 is
    begin
      if Val then
        return '1';
      else
        return '0';
      end if;
    end;

--------------------------------------------------------------------------------
    function Vector_OR ( Data : Std_Logic_Vector ) return UX01 is
      variable Result : UX01 := '0';
    begin
      for K in Data'low to Data'high loop
        Result :=  Result or Data(K);
      end loop;
      return Result;
    end;

---------------------------------------------------------------------------
    function Compare (A,B : Std_Logic_Vector) return UX01 is
      variable xor_vector : Std_Logic_Vector(A'Length downto 1);
      variable result : UX01 := '0';
    begin
      -- assume that the length of A and B is equal
      xor_vector := A xor B;
      return Vector_OR (xor_vector);
    end;

---------------------------------------------------------------------------
 function ChkGen ( Data : in DataBits ) return CheckBits is

  alias D31 : std_logic is Data(31);
  alias D30 : std_logic is Data(30);
  alias D29 : std_logic is Data(29);
  alias D28 : std_logic is Data(28);
  alias D27 : std_logic is Data(27);
  alias D26 : std_logic is Data(26);
  alias D25 : std_logic is Data(25);
  alias D24 : std_logic is Data(24);
  alias D23 : std_logic is Data(23);
  alias D22 : std_logic is Data(22);
  alias D21 : std_logic is Data(21);
  alias D20 : std_logic is Data(20);
  alias D19 : std_logic is Data(19);
  alias D18 : std_logic is Data(18);
  alias D17 : std_logic is Data(17);
  alias D16 : std_logic is Data(16);
  alias D15 : std_logic is Data(15);
  alias D14 : std_logic is Data(14);
  alias D13 : std_logic is Data(13);
  alias D12 : std_logic is Data(12);
  alias D11 : std_logic is Data(11);
  alias D10 : std_logic is Data(10);
  alias D09 : std_logic is Data(9);
  alias D08 : std_logic is Data(8);
  alias D07 : std_logic is Data(7);
  alias D06 : std_logic is Data(6);
  alias D05 : std_logic is Data(5);
  alias D04 : std_logic is Data(4);
  alias D03 : std_logic is Data(3);
  alias D02 : std_logic is Data(2);
  alias D01 : std_logic is Data(1);
  alias D00 : std_logic is Data(0);

begin
  return (
  0 => D31 xor D30 xor D29 xor D28 xor D24 xor D21 xor D20 xor D19 xor
        D15 xor D11 xor D10 xor D09 xor D08 xor D05 xor D04 xor D01,

  1 => D30 xor D28 xor D25 xor D24 xor D20 xor D17 xor D16 xor D15 xor
        D13 xor D12 xor D09 xor D08 xor D07 xor D06 xor D04 xor D03,

  2 => not (D31 xor D26 xor D22 xor D19 xor D18 xor D16 xor D15 xor D14 xor
        D10 xor D08 xor D06 xor D05 xor D04 xor D03 xor D02 xor D01),

  3 => D31 xor D30 xor D27 xor D23 xor D22 xor D19 xor D15 xor D14 xor
        D13 xor D12 xor D10 xor D09 xor D08 xor D07 xor D04 xor D00,

  4 => not (D30 xor D29 xor D27 xor D26 xor D25 xor D24 xor D21 xor D19 xor
        D17 xor D12 xor D10 xor D09 xor D04 xor D03 xor D02 xor D00),

  5 => D31 xor D26 xor D25 xor D23 xor D21 xor D20 xor D18 xor D14 xor
        D13 xor D11 xor D10 xor D09 xor D08 xor D06 xor D05 xor D00,

  6 => D31 xor D30 xor D29 xor D28 xor D27 xor D23 xor D22 xor D19 xor

        D18 xor D17 xor D16 xor D15 xor D11 xor D07 xor D02 xor D01  );
 end;

---------------------------------------------------------------------------
---- 'ChkBits_In' are the checkbits read from the memory
---- 'Chk_Ok' are the checkbits generated from the data read from memory
---- 'Parity' is the parity read from the memory
function SyndromGenS ( ChkBits_In : CheckBits;
                       Chk_Ok     : CheckBits;
                       Chk_Ok7    : std_logic;
                       Parity     : std_logic ) return SyndromBits is
  
begin
        -- One can generate a parity bit from the word instead of
        -- using the generated checkbits as here
	return (
	   7 => Chk_Ok7 xor Parity,
	   6 => Chk_Ok(6) xor Chkbits_In(6),
	   5 => Chk_Ok(5) xor Chkbits_In(5),
	   4 => Chk_Ok(4) xor Chkbits_In(4),
	   3 => Chk_Ok(3) xor Chkbits_In(3),
	   2 => Chk_Ok(2) xor Chkbits_In(2),
	   1 => Chk_Ok(1) xor Chkbits_In(1),
	   0 => Chk_Ok(0) xor Chkbits_In(0) );
end ;


function SyndromGen ( ChkBits_In : CheckBits;
                      Chk_Ok     : CheckBits;
                      Parity     : std_logic ) return SyndromBits is

begin
        -- One can generate a parity bit from the word instead of
        -- using the generated checkbits as here
        return (
           7 => Chk_Ok(6) xor Chk_Ok(5) xor not(Chk_Ok(4)) xor    --gb 950527 bit 4 inverted
                Chk_Ok(3) xor not(Chk_Ok(2)) xor Chk_Ok(1) xor    --gb 950527 bit 4 inverted
                Chk_Ok(0) xor Parity,
           6 => Chk_Ok(6) xor Chkbits_In(6),
           5 => Chk_Ok(5) xor Chkbits_In(5),
           4 => Chk_Ok(4) xor Chkbits_In(4),
           3 => Chk_Ok(3) xor Chkbits_In(3),
           2 => Chk_Ok(2) xor Chkbits_In(2),
           1 => Chk_Ok(1) xor Chkbits_In(1),
           0 => Chk_Ok(0) xor Chkbits_In(0) );
end ;

-------------------------------------------------------------------------
function Correct_Data ( Syndrom : in  SyndromBits;
                         Data   : in  Std_Logic_Vector ) return std_logic_vector is
begin
   case Syndrom is
      when "00111000" => return ( Data xor "000000000000000000000000000000001");
      when "01000101" => return ( Data xor "000000000000000000000000000000010");
      when "01010100" => return ( Data xor "000000000000000000000000000000100");
      when "00010110" => return ( Data xor "000000000000000000000000000001000");
      when "00011111" => return ( Data xor "000000000000000000000000000010000");
      when "00100101" => return ( Data xor "000000000000000000000000000100000");
      when "00100110" => return ( Data xor "000000000000000000000000001000000");
      when "01001010" => return ( Data xor "000000000000000000000000010000000");
      when "00101111" => return ( Data xor "000000000000000000000000100000000");
      when "00111011" => return ( Data xor "000000000000000000000001000000000");
      when "00111101" => return ( Data xor "000000000000000000000010000000000");
      when "01100001" => return ( Data xor "000000000000000000000100000000000");
      when "00011010" => return ( Data xor "000000000000000000001000000000000");
      when "00101010" => return ( Data xor "000000000000000000010000000000000");
      when "00101100" => return ( Data xor "000000000000000000100000000000000");
      when "01001111" => return ( Data xor "000000000000000001000000000000000");
      when "01000110" => return ( Data xor "000000000000000010000000000000000");
      when "01010010" => return ( Data xor "000000000000000100000000000000000");
      when "01100100" => return ( Data xor "000000000000001000000000000000000");
      when "01011101" => return ( Data xor "000000000000010000000000000000000");
      when "00100011" => return ( Data xor "000000000000100000000000000000000");
      when "00110001" => return ( Data xor "000000000001000000000000000000000");
      when "01001100" => return ( Data xor "000000000010000000000000000000000");
      when "01101000" => return ( Data xor "000000000100000000000000000000000");
      when "00010011" => return ( Data xor "000000001000000000000000000000000");
      when "00110010" => return ( Data xor "000000010000000000000000000000000");
      when "00110100" => return ( Data xor "000000100000000000000000000000000");
      when "01011000" => return ( Data xor "000001000000000000000000000000000");
      when "01000011" => return ( Data xor "000010000000000000000000000000000");
      when "01010001" => return ( Data xor "000100000000000000000000000000000");
      when "01011011" => return ( Data xor "001000000000000000000000000000000");
      when "01101101" => return ( Data xor "010000000000000000000000000000000");
      when "00000000" => return ( Data xor "100000000000000000000000000000000");
      when "10000000" => return ( Data xor "000000000000000000000000000000000");
      when others => if VecUnknown(syndrom) then
                        return  "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
                     else
                        return data;
                     end if;
   end case;
end Correct_Data;

------------------------------------------------------------------------
function NCError_Gen ( Syndrom : in  SyndromBits ) return std_logic is
begin
   case Syndrom is
     when "00111000" | "01000101" | "01010100" | "00010110" | "00011111" | "00100101" |
          "00100110" | "01001010" | "00101111" | "00111011" | "00111101" | "01100001" |
          "00011010" | "00101010" | "00101100" | "01001111" | "01000110" | "01010010" |
          "01100100" | "01011101" | "00100011" | "00110001" | "01001100" | "01101000" |
          "00010011" | "00110010" | "00110100" | "01011000" | "01000011" | "01010001" |
          "01011011" | "01101101" | "00000000" | "10000001" | "10000010" | "10000100" |
          "10001000" | "10010000" | "10100000" | "11000000" | "10000000" => return '0';
      when others => return '1';
   end case;
end NCError_Gen;

----------------------------------------------------------------------
function CError_Gen ( Syndrom : in  SyndromBits ) return std_logic is
begin
   case Syndrom is
     when "00111000" | "01000101" | "01010100" | "00010110" | "00011111" | "00100101" |
          "00100110" | "01001010" | "00101111" | "00111011" | "00111101" | "01100001" |
          "00011010" | "00101010" | "00101100" | "01001111" | "01000110" | "01010010" |
          "01100100" | "01011101" | "00100011" | "00110001" | "01001100" | "01101000" |
          "00010011" | "00110010" | "00110100" | "01011000" | "01000011" | "01010001" |
          "01011011" | "01101101" | "00000000" | "10000001" | "10000010" | "10000100" |
          "10001000" | "10010000" | "10100000" | "11000000" => return '1';
      when others =>     return '0';
   end case;
end CError_Gen;

end MECPackage;
---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      UART component
-- File name:                  uart.vhd
-- VHDL unit:                  uart
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned."-";
use IEEE.std_logic_unsigned."+";

entity uart is
port(
     MCK  : in std_logic;   -- Master clock provided
     TBR1 : in std_logic;   -- Transmitter Buffer Register (0)
     TBR2 : in std_logic;   -- Transmitter Buffer Register (1)
     TBR3 : in std_logic;   -- Transmitter Buffer Register (2)
     TBR4 : in std_logic;   -- Transmitter Buffer Register (3)
     TBR5 : in std_logic;   -- Transmitter Buffer Register (4)
     TBR6 : in std_logic;   -- Transmitter Buffer Register (5)
     TBR7 : in std_logic;   -- Transmitter Buffer Register (6)
     TBR8 : in std_logic;   -- Transmitter Buffer Register (7)
          
     TBRL_N : in std_logic;   -- Transmitter Buffer Register Load
     CRL  : in std_logic;   -- Control Register Load
     SBS  : in std_logic;   -- Stop Bit Select
          
     PI   : in std_logic;   -- Parity Inhibit
     EPE  : in std_logic;   -- Even Parity Enable (when PI is low)
          
     DRR  : in std_logic;   -- Data Received Reset
          
     TRC  : in std_logic;   -- Transmitter Register Clk
          
     RRC  : in std_logic;   -- Receiver Register Clk
          
     RRI  : in std_logic;   -- Receiver Register Input
          
     MR   : in std_logic;   -- Master Reset
          
     RBR1 : out std_logic;  -- Receiver Buffer Register (0)
     RBR2 : out std_logic;  -- Receiver Buffer Register (1)
     RBR3 : out std_logic;  -- Receiver Buffer Register (2)
     RBR4 : out std_logic;  -- Receiver Buffer Register (3)
     RBR5 : out std_logic;  -- Receiver Buffer Register (4)
     RBR6 : out std_logic;  -- Receiver Buffer Register (5)
     RBR7 : out std_logic;  -- Receiver Buffer Register (6)
     RBR8 : out std_logic;  -- Receiver Buffer Register (7)
          
     DR   : out std_logic;  -- Data Received
          
     TBRE : out std_logic;  -- Transmitter Buffer Register Empty
     TRE  : out std_logic;  -- Transmitter Register Empty
          
     FE   : out std_logic;  -- Frame Error
     PE   : out std_logic;  -- Parity Error
     OE   : out std_logic;  -- Overrun Error
          
     TRO  : out std_logic   -- Transmitter Register Output
     );
end uart;

architecture VHDL_RTL of uart is

 constant ZD0           : std_logic_vector(3 downto 0) := "0000";
 constant ZD1           : std_logic_vector(3 downto 0) := "0001";
 constant ZD2           : std_logic_vector(3 downto 0) := "0010";
 constant ZD3           : std_logic_vector(3 downto 0) := "0011";
 constant ZD4           : std_logic_vector(3 downto 0) := "0100";
 constant ZD5           : std_logic_vector(3 downto 0) := "0101";
 constant ZD6           : std_logic_vector(3 downto 0) := "0110";
 constant ZD7           : std_logic_vector(3 downto 0) := "0111";
 constant ZD8           : std_logic_vector(3 downto 0) := "1000";
 constant ZD9           : std_logic_vector(3 downto 0) := "1001";
 constant ZD10          : std_logic_vector(3 downto 0) := "1010";
 constant ZD11          : std_logic_vector(3 downto 0) := "1011";
 constant ZD12          : std_logic_vector(3 downto 0) := "1100";
 constant ZD13          : std_logic_vector(3 downto 0) := "1101";
 constant ZD14          : std_logic_vector(3 downto 0) := "1110";
 constant ZD15          : std_logic_vector(3 downto 0) := "1111";

 -- programmation registers 
 signal PIReg            : std_logic;       -- Parity inhbit
 signal EPEReg           : std_logic;       -- EvenParity Enable
 signal SBSReg           : std_logic;       -- 2 Stop bits detected

 -- Emission signals
 signal SEmi             : std_logic_Vector(3 downto 0); -- fsm state
 signal Next_SEmi        : std_logic_Vector(3 downto 0); -- next fsm state

 signal CountClkEmi      : std_logic_Vector(3 downto 0);  -- counter used to divide TRC by 16
 signal EnCountClkEmi    : std_logic;                     -- Enable of the CountClkEmi counter
 signal ZeroCountClkEmi  : std_logic;                     -- CountClkEmi = 0000 and TRC enable

 signal TransBufReg      : std_logic_Vector(7 downto 0);  -- temporary buffer register
 signal TransmitReg      : std_logic_Vector(9 downto 0);  -- Transmit Register
 signal LoadTransmitReg  : std_logic;                     -- load Transmit Reg
 signal ShiftTransmitReg : std_logic;                     -- shift Transmit Reg
 signal EmiPar           : std_logic;                     -- Emitted Parity
 signal FlagTBRE         : std_logic;                     -- A new Data is loaded in the Transmit
                                                         -- buffer register
 signal SetFlagTBRE      : std_logic;                     -- set Flag TBRE 

 signal TREActive        : std_logic;   -- active if fsm EMi in idle or end state
 signal TREStopBit       : std_logic;   -- active during last Stop Bit

 -- Reception Signals

 signal RRI_R            : std_logic;                     -- RRI after resynchro
 signal SRec             : std_logic_Vector(3 downto 0);  -- fsm state
 signal Next_SRec        : std_logic_Vector(3 downto 0);  -- next fsm state

 signal CountClkRec      : std_logic_Vector(3 downto 0);  --  Reception Clk Counter
 signal EnCountClkRec    : std_logic;                     -- Enable rec Clk counting

 signal DataBitSampled      : std_logic; --  Active when a data bit is received
 signal StartBitSampled     : std_logic; --  Active when start bit is received
 signal ParBitSampled       : std_logic; --  Active when Parity bit is received
 signal FirstStopBitSampled : std_logic; --  Active when 1st stop bit is received
 signal SampleRec           : std_logic; --  Active when data is sampled on RRI_R

 signal ParecReg         : std_logic;        -- parity received register
 signal PEReg            : std_logic;        -- Parity Error register
 signal OEReg            : std_logic;        -- Overrun  Error register      
 signal DRReg            : std_logic;        -- Data ready register      
 signal FEReg            : std_logic;        -- Frame  Error register      
  
 signal ReceiveReg       : std_logic_Vector(7 downto 0);   -- receive register    
 signal RecBufReg        : std_logic_Vector(7 downto 0);   -- receive buffer register


begin

----------------------------------------------------------------------------
--These outputs are used inside, too.
    TBRE  <= FlagTBRE;
-- TRE is generated when emi fsm is idle
-- or during the second half of the last stop bit
--
    TRE  <= TREActive or ( TREStopBit and not(CountClkEmi(3)) ) ;
    TRO  <= TransmitReg(0);

    RBR1 <= RecBufReg(0);
    RBR2 <= RecBufReg(1);
    RBR3 <= RecBufReg(2);
    RBR4 <= RecBufReg(3);
    RBR5 <= RecBufReg(4);
    RBR6 <= RecBufReg(5);
    RBR7 <= RecBufReg(6);
    RBR8 <= RecBufReg(7);
    
    FE   <= FEReg;
    OE   <= OEReg;
    DR   <= DRReg;
    PE   <= PEReg;



----------------------------------------------------------------------------
-- Programming part
-- Synchronous Memorization of the parity, number of stop bits
-- upon receipt of the CRL signal compliant with the MCK clock
----------------------------------------------------------------------------
WriteControlRegister: process(MCK, MR)
begin
  if (MR = '1') then    
    PIReg      <= '0';
    EPEReg     <= '0';
    SBSReg     <= '0';
  
  elsif (MCK'event and MCK = '1') then
    -- when PI  = 0 and EPE = 1 => parity EVEN
    -- when PI  = 0 and EPE = 0 => parity ODD
    -- when PI  = 1 and EPE = x => no parity, PE = 0
    -- when SBS = 0             => 1 stop bit
    -- when SBS = 1             => 2 stop bits
    if CRL = '1' then
      PIReg  <= PI;
      EPEReg <= EPE;
      SBSReg <= SBS;
    end if;
  end if;
end process;


----------------------------------------------------------------------------
--Transmitting part
----------------------------------------------------------------------------
-- This counter is used to divide the TRC input by 16 to generate pulses on
-- TRO
-- It starts upon activation of EnCountClkEmi by the Emission fsm
-- It counts each time a TRC pulse is received
--
-- The ShiftTransmitReg shifts the transmission buffer in emission
-- one clock period after CountClkEmi = 0000 and TRC = '1'
--
TransmitClock: process(MCK, MR)
begin
  if (MR = '1') then
    CountClkEmi  <= ZD15 ;
    ShiftTransmitReg <= '0';

  elsif (MCK'event and MCK = '1') then
    if EnCountClkEmi = '0' then
        CountClkEmi <=  ZD15;         
    elsif EnCountClkEmi = '1' and TRC = '1' then 
        CountClkEmi <= CountClkEmi - 1;
    end if;
    
    ShiftTransmitReg <= ZeroCountClkEmi;
  end if;
end process;

ZeroCountClkEmi <= '1' when CountClkEmi = ZD0 and TRC = '1' else '0';

----------------------------------------------------------------------------
-- Synchronous load of the transmit buffer register
-- on receipt of TBRL signal
-- A Flag is reset to indicate that the TB Register is not empty
--
WriteTransBufReg: process (MCK, MR)
begin
  if (MR = '1') then
   FlagTBRE <= '1';
   TransBufReg <= "00000000";
  elsif (MCK'event and MCK = '1') then     
    if TBRL_N = '0' then
      TransBufReg(7) <= TBR8;
      TransBufReg(6) <= TBR7;
      TransBufReg(5) <= TBR6;
      TransBufReg(4) <= TBR5;
      TransBufReg(3) <= TBR4;
      TransBufReg(2) <= TBR3;
      TransBufReg(1) <= TBR2;
      TransBufReg(0) <= TBR1;
      FlagTBRE <= '0';          -- a new data to emit is received
    end if;
    if SetFlagTBRE = '1' then
      FlagTBRE <= '1';          -- TB Register empty
    end if;
    
  end if; 
end process;



----------------------------------------------------------------------------
-- Synchronous load of the transmit register by the fsm
-- Start bit, parity and stop bit values are added 
-- Synchronous shift of the register
--
-- There is a trick with the TransmitReg register
-- its size is only 10 bits ( 1 start bit + 8 data + parity )
-- the stop bit at 1 is generated by inputing 1 
-- in bit 9 of the register !!
--
WriteTransReg: process (MCK, MR)
begin
  if (MR = '1') then
   TransmitReg <= "1111111111";         -- shall be set to 1 
  elsif (MCK'event and MCK = '1') then  
     
    if LoadTransmitReg = '1' then
      TransmitReg(0) <= '0';             -- Start Bit
      TransmitReg(1) <= TransBufReg(0);  -- LSB
      TransmitReg(2) <= TransBufReg(1);  
      TransmitReg(3) <= TransBufReg(2);  
      TransmitReg(4) <= TransBufReg(3);  
      TransmitReg(5) <= TransBufReg(4);  
      TransmitReg(6) <= TransBufReg(5);  
      TransmitReg(7) <= TransBufReg(6);  
      TransmitReg(8) <= TransBufReg(7);  -- MSB
      if PIReg = '1' then    
         TransmitReg(9)  <= '1';        -- No parity (Stop Bit)
      elsif EPEReg = '1' then    
         TransmitReg(9) <= EmiPar;      -- Even Parity
      else
         TransmitReg(9) <= not EmiPar;  -- Odd Parity 
      end if;
      
     elsif  ShiftTransmitReg = '1' then
      TransmitReg(0)  <=  TransmitReg(1); 
      TransmitReg(1)  <=  TransmitReg(2); 
      TransmitReg(2)  <=  TransmitReg(3); 
      TransmitReg(3)  <=  TransmitReg(4); 
      TransmitReg(4)  <=  TransmitReg(5); 
      TransmitReg(5)  <=  TransmitReg(6); 
      TransmitReg(6)  <=  TransmitReg(7); 
      TransmitReg(7)  <=  TransmitReg(8); 
      TransmitReg(8)  <=  TransmitReg(9); 
      TransmitReg(9)  <=  '1';            -- furture stop bit

    end if;
  end if;
  
end process;

EmiPar <= TransBufReg(0) xor TransBufReg(1) xor TransBufReg(2) xor TransBufReg(3) xor
          TransBufReg(4) xor TransBufReg(5) xor TransBufReg(6) xor TransBufReg(7);
          

----------------------------------------------------------------------------
-- Emission FSM
-- 
EMIfsm1 : process(MCK, MR)
begin
  if(MR = '1') then
     SEmi <= ZD0;
  elsif (MCK'event and MCK = '1') then  
     SEmi <= Next_SEmi;
  end if;
end process;

EMIfsm2 : process(SEmi, LoadTransmitReg, EnCountClkEmi, TRC,
                  SetFlagTBRE, SBSReg, PIReg, FlagTBRE,
                  ZeroCountClkEmi)
begin

Next_SEmi <= SEmi;
LoadTransmitReg <= '0';
EnCountClkEmi   <= '0';
SetFlagTBRE     <= '0';
TREActive         <= '0';
TREStopBit      <= '0';

  case SEmi is
  -- Idle State
    when ZD0 =>
      if FlagTBRE = '0' and TRC = '1' then
        Next_SEmi <= ZD1;
      end if;
      TREActive <= '1';          -- Transmitted Register is empty
  
  -- Load Transmit Register
  -- Clear TRBL Flag
  -- Emit Start Bit 
    when ZD1 =>
      Next_SEmi <= ZD2;
      LoadTransmitReg <= '1';
      SetFlagTBRE     <= '1';
      EnCountClkEmi   <= '1';
      
  -- Continue to Emit Start Bit
  -- (used since LoadTransmitReg shall last only 1 MCK period)
    when ZD2 =>
      if TRC = '1' then   
        Next_SEmi <= ZD3;
      end if;
      EnCountClkEmi   <= '1';
      
         
  -- Start Bit Emission
    when ZD3 =>
      if ZeroCountClkEmi = '1' then       
        Next_SEmi <= ZD4;
      end if;
      EnCountClkEmi   <= '1';
      
  -- data bit emission 1
    when ZD4 =>
      if ZeroCountClkEmi = '1' then       
        Next_SEmi <= ZD5;
      end if;
      EnCountClkEmi   <= '1';
      
  -- data bit emission 2
    when ZD5 =>
      if ZeroCountClkEmi = '1' then       
        Next_SEmi <= ZD6;
      end if;
      EnCountClkEmi   <= '1';
      
  -- data bit emission 3
    when ZD6 =>
      if ZeroCountClkEmi = '1' then       
        Next_SEmi <= ZD7;
      end if;
      EnCountClkEmi   <= '1';
      
  -- data bit emission 4
    when ZD7 =>
      if ZeroCountClkEmi = '1' then       
        Next_SEmi <= ZD8;
      end if;
      EnCountClkEmi   <= '1';
      
  -- data bit emission 5
    when ZD8 =>
      if ZeroCountClkEmi = '1' then       
        Next_SEmi <= ZD9;
      end if;
      EnCountClkEmi   <= '1';
      
  -- data bit emission 6
    when ZD9 =>
      if ZeroCountClkEmi = '1' then       
        Next_SEmi <= ZD10;
      end if;
      EnCountClkEmi   <= '1';
      
  -- data bit emission 7
    when ZD10 =>
      if ZeroCountClkEmi = '1' then       
        Next_SEmi <= ZD11;
      end if;
      EnCountClkEmi   <= '1';
      
  -- data bit emission 8
    when ZD11 =>
      if ZeroCountClkEmi = '1' then
        if PIReg = '1' then
          if SBSReg = '0' then
            Next_SEmi <= ZD14;  -- no parity 1 stop bit jump to last stop bit
          else            
            Next_SEmi <= ZD13;  -- no parity 2 stop bits
          end if;
        else
          Next_SEmi <= ZD12;    -- parity
        end if;
      end if;
      EnCountClkEmi   <= '1';

  -- parity
    when ZD12 =>
      if ZeroCountClkEmi = '1' then
        if SBSreg = '0' then
          Next_SEmi <= ZD14;       -- 1 stop bit jump to last stop bit
        else       
          Next_SEmi <= ZD13;       -- 2 stop bits
        end if;
      end if;
      EnCountClkEmi   <= '1';

  -- first stop bit in case of 2 stop bits
    when ZD13 =>
      if ZeroCountClkEmi = '1' then       
        Next_SEmi <= ZD14;       -- 2 stop bits
      end if;
      EnCountClkEmi   <= '1';
      
  -- 2nd stop bit or 1st stop bit if only 1
    when ZD14 =>
      if ZeroCountClkEmi = '1' then       
        Next_SEmi <= ZD15;
      end if;
      TREStopBit      <= '1';       -- TRE is generated in this period
      EnCountClkEmi   <= '1';
      
  -- end state
  -- 
    when ZD15 =>
      if FlagTBRE = '0' then
        Next_SEmi <= ZD2;
        LoadTransmitReg <= '1';
        SetFlagTBRE     <= '1';
        EnCountClkEmi   <= '1';
      else
        Next_SEmi <= ZD0;
      end if;
      TREActive <= '1';          -- Transmitted Register is empty
       
    when others =>
      Next_SEmi <= ZD0;
    end case;
    
 
end process;





----------------------------------------------------------------------------
--RECEIVING PART
----------------------------------------------------------------------------
-- this counter is used to synchronize the data In sample w.r.t. the
-- receive clock RRC
-- The data is sampled when the counter is 0111 and RRC = 1
-- Resynchronization of RRI_R
--
ReceiveClk: process(MCK, MR)
begin
  if (MR = '1') then
    CountClkRec  <= "0000";
    RRI_R <= '1';
  
  elsif (MCK'event and MCK = '1') then
    if (EnCountClkRec = '0') then
      CountClkRec  <= "0000";
    elsif EnCountClkRec = '1' and RRC = '1' then
      CountClkRec <= CountClkRec + 1;
    end if;
    
    RRI_R <= RRI;
    
  end if;
end process;

ReceiveClk1: process(CountClkRec, RRC)
begin
   if RRC = '1' and CountClkRec = "0111" then
     SampleRec <= '1';
   else
     SampleRec <= '0';
   end if;
end process;


----------------------------------------------------------------------------
-- finite state for the receive part
-- this fsm starts upon receipt of a start bit
-- 
----------------------------------------------------------------------------
-- Emission FSM
-- 
RECfsm1 : process(MCK, MR)
begin
  if(MR = '1') then
     SRec <= ZD0;
  elsif (MCK'event and MCK = '1') then  
     SRec <= Next_SRec;
  end if;
end process;


RECfsm2: process(SRec, SampleRec, RRI_R,
                 PIReg, SBSReg, EPEReg)
begin

EnCountClkRec          <= '0';
DataBitSampled        <= '0';
StartBitSampled       <= '0';
ParBitSampled         <= '0';
FirstStopBitSampled   <= '0';
Next_SRec             <= Srec;

  case SRec is
  
  -- idle State
  -- waiting for start bit
   when ZD0 =>
     if RRI_R = '0' then
       Next_SRec <= ZD1;
     end if;
     
  -- start Counting
  -- falling edge detected on RRI_R
  -- and wait for middle of start bit
    when ZD1 => 
     if SampleRec = '1' then
       if RRI_R = '0' then
         Next_SRec <= ZD2;    -- ok start bit sampled at 0
       else
         Next_SRec <= ZD0;    -- wrong start bit
       end if;
       StartBitSampled <= '1';    -- init parity computation
     end if;
     EnCountClkRec <= '1';
     
  -- to middle of  data bit 1
    when ZD2 => 
     if SampleRec = '1' then
       Next_SRec <= ZD3;
       DataBitSampled      <= '1';
     end if;
     EnCountClkRec <= '1';

  -- to middle of  data bit 2
    when ZD3 => 
     if SampleRec = '1' then
       Next_SRec <= ZD4;
       DataBitSampled      <= '1';
     end if;
     EnCountClkRec <= '1';

  -- to middle of  data bit 3
    when ZD4  => 
     if SampleRec = '1' then
       Next_SRec <= ZD5;
       DataBitSampled      <= '1';
     end if;
     EnCountClkRec <= '1';

  -- to middle of  data bit 4
    when ZD5 => 
     if SampleRec = '1' then
       Next_SRec <= ZD6;
       DataBitSampled      <= '1';
     end if;
     EnCountClkRec <= '1';

  -- to middle of  data bit 5
    when ZD6 => 
     if SampleRec = '1' then
       Next_SRec <= ZD7;
       DataBitSampled      <= '1';
     end if;
     EnCountClkRec    <= '1';
     
  -- to middle of  data bit 6
    when ZD7 => 
     if SampleRec = '1' then
       Next_SRec <= ZD8;
       DataBitSampled      <= '1';
     end if;
     EnCountClkRec <= '1';
     
  -- to middle of  data bit 7
    when ZD8 => 
     if SampleRec = '1' then
       Next_SRec <= ZD9;
       DataBitSampled      <= '1';
     end if;
     EnCountClkRec <= '1';
     
  -- to middle of  data bit 8
    when ZD9 => 
     if SampleRec = '1' then
        if PIReg = '1' then
          Next_SRec <= ZD11;    -- no parity -> 1st stop bit
        else
          Next_SRec <= ZD10;    -- parity
        end if;
       DataBitSampled      <= '1';
     end if;
     EnCountClkRec <= '1';

  -- to middle of  parity bit if any
    when ZD10 => 
     if SampleRec = '1' then
       Next_SRec <= ZD11;       -- 1 st stop bit
       ParBitSampled      <= '1';
     end if;
     EnCountClkRec <= '1';

  -- to middle of  first stop bit
    when ZD11 => 
     if SampleRec = '1' then
       if SBSReg = '1' then
         Next_SRec <= ZD12;    -- second stop bit
       else
         Next_SRec <= ZD13;    -- only 1 stop bit
       end if;
       FirstStopBitSampled      <= '1';
     end if;
     EnCountClkRec <= '1';

  -- to middle of  the second stop bit (if 2 stop bits)
    when ZD12 => 
     if SampleRec = '1' then
       Next_SRec <= ZD13;
     end if;
     EnCountClkRec <= '1';
    
  -- end
     when ZD13 =>
  -- return to ZD0 only if a level 1 was detected
       if RRI_R = '1' then
         Next_Srec <= ZD0;  -- TBC
       end if;
     
     when others =>
       Next_Srec <= ZD0;
    end case;

end process;

----------------------------------------------------------------------------
-- Receive Shift Register 
-- The register is shifted at each sample
--
RecRegProc : process(MCK, MR)
begin
  if (MR = '1') then
    ReceiveReg   <= "00000000";
  
  elsif (MCK'event and MCK = '1') then
    if DataBitSampled = '1' then
      ReceiveReg(0) <= ReceiveReg(1);
      ReceiveReg(1) <= ReceiveReg(2);
      ReceiveReg(2) <= ReceiveReg(3);
      ReceiveReg(3) <= ReceiveReg(4);
      ReceiveReg(4) <= ReceiveReg(5);
      ReceiveReg(5) <= ReceiveReg(6);
      ReceiveReg(6) <= ReceiveReg(7);
      ReceiveReg(7) <= RRI_R;
    end if;
  end if;
end process;

----------------------------------------------------------------------------
-- Receive Buffer Register 
-- This register is loaded by the fsm during the first stop bit
--
ReceiveProc: process(MCK, MR)
begin
  if (MR = '1') then
    RecBufReg   <= "00000000";  
  elsif (MCK'event and MCK = '1') then
    if FirstStopBitSampled = '1' then
      RecBufReg <= ReceiveReg;
    end if;
  end if;
end process;

----------------------------------------------------------------------------
-- This process manages the Parity for the receive part
-- when the receiving process starts  the ParecReg is set to 1 or 0 depending
--  on the parity to compute
-- Then for each data bit received parity is computed and memorized
-- When the parity bit is received it is compared to the ParecReg regiter (parity ODD/EVEN)
-- if the result is false it is set to 1.
-- The value of this register is loaded in PEReg at the same time as RBR reg
-- 
ParityCompute: process(MCK, MR)
begin
  if (MR = '1') then
    ParecReg <= '0';
  
  elsif (MCK'event and MCK = '1') then
    if StartBitSampled = '1' then     -- initialise parity computation
    	ParecReg <= not EPEReg;    -- 0 for Even Parity 1 for Odd parity
    	
    elsif DataBitSampled = '1' then  -- compute parity
    	ParecReg <= ParecReg xor RRI_R;
    	
    elsif ParBitSampled = '1' then  -- test parity
        if Parecreg = RRI_R then
          ParecReg <= '0';       -- Parity good 
        else
          ParecReg <= '1';       -- Parity wrong
        end if;
    end if;
  end if; 

end process;

----------------------------------------------------------------------------
-- Computation of the Frame Error Error, Parity Error
-- Frame Errot is Active if the first stop bit is not at 1
--
FEprocess: process(MCK, MR)
begin
  if (MR = '1') then
    FEReg <= '0';
    PEReg <= '0';
  elsif (MCK'event and MCK = '1') then
  
-- NOTE : TO BE CORRECTED DR and FE shall be generated later

    if FirstStopBitSampled = '1' then
    
      if RRI_R = '0' then
     --rc   FEReg <= '1';    -- error first stop bit is zero
        FEReg <= '1' or FEReg;    -- error first stop bit is zero
      else
        FEReg <= '0';    -- no error    
      end if;
      
      if PIReg = '1' then
        PEReg <= '0';     -- no parity => no parity error
      else
     --rc   PEReg <= Parecreg;
        PEReg <= Parecreg or PEReg; -- hold the error until MR activation
      end if;
      
    end if;
  end if;
end process;

----------------------------------------------------------------------------
-- Data ready and Overrun Error management
-- Data Ready is cleared by the DRR signal and set when data is transferred (1st stop bit)
-- Overrun error is set to 1 if DRreg is active when First Stop Bit

OEProcess: process(MCK, MR)
begin
  if (MR = '1') then
    DRReg <= '0';
    OEReg <= '0';
  elsif (MCK'event and MCK = '1') then
    if DRR = '0' then
      DRReg <= '0';                        -- DR clear on DRR high
    elsif FirstStopBitSampled = '1' then
      DRReg <= '1';
    end if;
    
-- NOTE : TO BE CORRECTED DR and FE shall be generated later

    if FirstStopBitSampled = '1' then
      if DRReg = '1' then
        OEReg <= '1';                      -- OE error
      else
        OEReg <= '0';
      end if;
    end if;
  end if;
end process;

end VHDL_RTL;
---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      UART control
-- File name:                  \mec\source\uartctl.vhd
-- VHDL unit:                  UARTControl
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--


library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

use IEEE.STD_LOGIC_UNSIGNED.all;

entity UARTControl is
    port (
          Clk_Int         : in Std_Logic;
          Reset_Int_N     : in Std_Logic;
          DoNotRdUART     : in Std_Logic;
          
          GUARTAReg_N      : in Std_Logic;
          GUARTBReg_N      : in Std_Logic;
          GUARTStatusReg_N : in Std_Logic;
          
          UARTAReg_N      : in Std_Logic;
          UARTBReg_N      : in Std_Logic;
          UARTStatusReg_N : in Std_Logic;
          MECControlReg_N : in Std_Logic;
          Wr_Int_N        : in Std_Logic;
          Rd_Int_In       : in Std_Logic;
          D_Int_In        : in Std_Logic_Vector(31 downto 0);
                    
          Intr_UARTA_Data  : out Std_Logic;
          Intr_UARTB_Data  : out Std_Logic;
          UARTs_Quiet     : out Std_Logic;
          Intr_UART_Err   : out Std_Logic;
          
          D_UARTA_Out     : out Std_Logic_Vector(31 downto 0);
          DPar_UARTA_Out  : out Std_Logic;
          D_UARTB_Out     : out Std_Logic_Vector(31 downto 0);
          DPar_UARTB_Out  : out Std_Logic;
          D_UARTS_Out     : out Std_Logic_Vector(31 downto 0);
          DPar_UARTS_Out  : out Std_Logic;
          
          D_Ready_A       : in Std_Logic;
          D_Ready_B       : in Std_Logic;
          O_Err_A         : in Std_Logic;
          O_Err_B         : in Std_Logic;
          Frame_Err_A     : in Std_Logic;
          Frame_Err_B     : in Std_Logic;
          Par_Err_A       : in Std_Logic;
          Par_Err_B       : in Std_Logic;
          TrHoRegEm_A     : in Std_Logic;
          TrHoRegEm_B     : in Std_Logic;
          TrSeRegEm_A     : in Std_Logic;
          TrSeRegEm_B     : in Std_Logic;
          D_UART_A_Out    : in Std_Logic_Vector(7 downto 0);
          D_UART_B_Out    : in Std_Logic_Vector(7 downto 0);

          MReset_A        : out Std_Logic;
          MReset_B        : out Std_Logic;
          D_ReadyRes_A_N  : out Std_Logic;
          D_ReadyRes_B_N  : out Std_Logic;
          Wr_Data_A_N     : out Std_Logic;
          Wr_Data_B_N     : out Std_Logic;
          Wr_Control      : out Std_Logic;
          SBSel           : out Std_Logic;
          EvenOdd_N       : out Std_Logic;
          ParInh          : out Std_Logic;
          D_UART_In       : out Std_Logic_Vector(7 downto 0)
         );
end UARTControl;

architecture Mini_Spec of UARTControl is

    signal UART_A_Reg1         : Std_Logic_Vector(7 downto 0);
    signal UART_A_Par1         : Std_Logic;
    signal UART_B_Reg1         : Std_Logic_Vector(7 downto 0);
    signal UART_B_Par1         : Std_Logic;
    signal UART_Status_Reg1    : Std_Logic_Vector(23 downto 0);
    signal UART_Status_Par1    : Std_Logic;
    
    signal UART_A_Reg          : Std_Logic_Vector(7 downto 0);
    signal UART_A_Par          : Std_Logic;
    signal UART_B_Reg          : Std_Logic_Vector(7 downto 0);
    signal UART_B_Par          : Std_Logic;
    signal UART_Status_Reg     : Std_Logic_Vector(23 downto 0);
    signal UART_Status_Par     : Std_Logic;
    
    signal TrSeRegEm_A_rck     : Std_Logic;
    signal TrSeRegEm_B_rck     : Std_Logic;
    signal TrSeRegEm_A_redge   : Std_Logic;
    signal TrSeRegEm_B_redge   : Std_Logic;
    
    signal D_Ready_A_rck       : Std_Logic;
    signal D_Ready_B_rck       : Std_Logic;
    signal D_Ready_A_redge     : Std_Logic;
    signal D_Ready_B_redge     : Std_Logic;
    
    signal Intr_UART_Err_rck   : Std_Logic;
        
    signal D7                  : Std_Logic;
    signal D23                 : Std_Logic;
        
begin

----------------------------------------------------------------------------
    Data_Hold: process
    begin
		wait until Clk_Int'event and Clk_Int = '0';
        D7        <=     D_Int_In(7);    
        D23       <=     D_Int_In(23);    
        D_UART_In <=     D_Int_In(7 downto 0);    
        ParInh    <= not D_Int_In(20);
        EvenOdd_N <= not D_Int_In(21);
        SBSel     <=     D_Int_In(22);
    end process;
    

----------------------------------------------------------------------------
    UARTs_Quiet    <= TrHoRegEm_A and TrHoRegEm_B and TrSeRegEm_A and TrSeRegEm_B;

----------------------------------------------------------------------------
		Sync_Intr_UART: process(Reset_Int_N, Clk_Int)
		begin
		  if Reset_Int_N ='0' then
		  	TrSeRegEm_A_rck   <= '1';
		  	D_Ready_A_rck     <= '0';
		  	TrSeRegEm_B_rck   <= '1';
		  	D_Ready_B_rck     <= '0';
		  	Intr_UART_Err_rck <= '1';
		  
	  	elsif Clk_Int'event and Clk_Int = '1' then
		  	TrSeRegEm_A_rck   <= TrSeRegEm_A;
		  	D_Ready_A_rck     <= D_Ready_A;
		  	TrSeRegEm_B_rck   <= TrSeRegEm_B;
		  	D_Ready_B_rck     <= D_Ready_B;
		  	Intr_UART_Err_rck <= O_Err_A or Par_Err_A or Frame_Err_A or 
                             O_Err_B or Par_Err_B or Frame_Err_B;
		  end if;
		  
  	end process;
  		
	 TrSeRegEm_A_redge <= TrSeRegEm_A and not(TrSeRegEm_A_rck);
	 TrSeRegEm_B_redge <= TrSeRegEm_B and not(TrSeRegEm_B_rck);
	 
	 D_Ready_A_redge <= D_Ready_A and not(D_Ready_A_rck);
	 D_Ready_B_redge <= D_Ready_B and not(D_Ready_B_rck);
	 
   Intr_UARTA_Data <= (D_Ready_A_redge or TrSeRegEm_A_redge) and
                      not(Frame_Err_A or Par_Err_A or O_Err_A);
                      
   Intr_UARTB_Data <= (D_Ready_B_redge or TrSeRegEm_B_redge) and 
                      not(Frame_Err_B or Par_Err_B or O_Err_B);
  
   Intr_UART_Err   <= (O_Err_A or Par_Err_A or Frame_Err_A or 
                       O_Err_B or Par_Err_B or Frame_Err_B) and not Intr_UART_Err_rck;



----------------------------------------------------------------------------

    -- MReset high for 2 Clk period, It is used by the UARTs as 
    -- an asynchronous reset
    -- The UARTs are working on the rising edge so the 
    -- aynchronous reset is generated on ck+
    MasterReset: process(Reset_Int_N, Clk_Int)
    type states is (S0, S1);
    variable state :states;
    begin
      if Reset_Int_N = '0' then
        MReset_A <= '1';
        MReset_B <= '1';
        state    := S1;
          
      elsif Clk_Int'event and Clk_Int='1' then
        case state is
           when S0 => if (UARTStatusReg_N = '0') and (Wr_Int_N = '0') then
                         MReset_A <= D7;
                         MReset_B <= D23;
                         state := S1;
                       else
                         MReset_A <= '0';
                         MReset_B <= '0';
                         state := S0;
                       end if;
                       
           when S1 => state:=S0;
         end case;
      end if;
    end process;


----------------------------------------------------------------------------
    DataReadyReset: process(Reset_Int_N, Clk_Int)
    begin
                
     if Reset_Int_N = '0' then
         D_ReadyRes_A_N <= '1';
         D_ReadyRes_B_N <= '1';
          
     elsif Clk_Int'event and Clk_Int = '1' then
     
       if Rd_Int_In = '1' and DoNotRdUART = '0' and 
          GUARTAReg_N = '0' and D_Ready_A = '1' then
         D_ReadyRes_A_N <= '0';
         D_ReadyRes_B_N <= '1';
       elsif Rd_Int_In = '1' and DoNotRdUART = '0' and 
             GUARTBReg_N = '0' and D_Ready_B = '1' then
         D_ReadyRes_A_N <= '1';
         D_ReadyRes_B_N <= '0';
       else
         D_ReadyRes_A_N <= '1';
         D_ReadyRes_B_N <= '1';
       end if;
            
     end if;
        
    end process;
    

----------------------------------------------------------------------------
    WriteStrobes: process(Reset_Int_N, Clk_Int)
    begin
     if Reset_Int_N = '0' then
        Wr_Data_A_N <= '1';
        Wr_Data_B_N <= '1';
        Wr_Control  <= '0';

     elsif Clk_Int'event and Clk_Int='0' then
        Wr_Data_A_N <= Wr_Int_N or UARTAReg_N;
        Wr_Data_B_N <= Wr_Int_N or UARTBReg_N;
        Wr_Control  <= not (Wr_Int_N or MECControlReg_N);
     end if;
    end process;

----------------------------------------------------------------------------
    -- Sample signals for UART A RX/TX Register
    UART_A_Reg1(7 downto 0)      <= D_UART_A_Out;
  --  UART_A_Reg1(8)               <= D_Ready_A and not(Frame_Err_A or Par_Err_A or O_Err_A);
  --  UART_A_Reg1(9)               <= TrSeRegEm_A;
  --  UART_A_Reg1(10)              <= TrHoRegEm_A;
    UART_A_Par1                  <= ParityGen(UART_A_Reg1);
            
    -- Sample signals for UART B RX/TX Register
    UART_B_Reg1(7 downto 0)      <= D_UART_B_Out;
  --  UART_B_Reg1(8)               <= D_Ready_B and not(Frame_Err_B or Par_Err_B or O_Err_B);
  --  UART_B_Reg1(9)               <= TrSeRegEm_B;
  --  UART_B_Reg1(10)              <= TrHoRegEm_B;
    UART_B_Par1                  <= ParityGen(UART_B_Reg1);
           
    -- Sample signals for UART Status Register
    UART_Status_Reg1(0)          <= D_Ready_A and not(Frame_Err_A or Par_Err_A or O_Err_A);
    UART_Status_Reg1(1)          <= TrSeRegEm_A;
    UART_Status_Reg1(2)          <= TrHoRegEm_A;
    UART_Status_Reg1(3)          <= '0';
    UART_Status_Reg1(4)          <= Frame_Err_A;
    UART_Status_Reg1(5)          <= Par_Err_A;
    UART_Status_Reg1(6)          <= O_Err_A;
    UART_Status_Reg1(15 downto 7)<= "000000000";
    UART_Status_Reg1(16)         <= D_Ready_B and not(Frame_Err_B or Par_Err_B or O_Err_B);
    UART_Status_Reg1(17)         <= TrSeRegEm_B;
    UART_Status_Reg1(18)         <= TrHoRegEm_B;
    UART_Status_Reg1(19)         <= '0';
    UART_Status_Reg1(20)         <= Frame_Err_B;
    UART_Status_Reg1(21)         <= Par_Err_B;
    UART_Status_Reg1(22)         <= O_Err_B;
    UART_Status_Reg1(23)         <= '0';
    UART_Status_Par1             <= ParityGen(UART_Status_Reg1);

    DataSampleProc1: process(Clk_Int)
    begin
     if Clk_Int'event and Clk_Int = '1' then
     
       if Rd_Int_In = '1' and GUARTAReg_N = '0' then
          UART_A_Reg      <= UART_A_Reg;
          UART_A_Par      <= UART_A_Par;
       else
          UART_A_Reg      <= UART_A_Reg1;
          UART_A_Par      <= UART_A_Par1;
       end if;
            
     end if;
        
    end process;
    
    DataSampleProc2: process(Clk_Int)
    begin
     if Clk_Int'event and Clk_Int = '1' then
     
       if Rd_Int_In = '1' and GUARTBReg_N = '0' then
          UART_B_Reg      <= UART_B_Reg;
          UART_B_Par      <= UART_B_Par;
       else
          UART_B_Reg      <= UART_B_Reg1;
          UART_B_Par      <= UART_B_Par1;
       end if;
            
     end if;
        
    end process;
    
    DataSampleProc3: process(Clk_Int)
    begin
     if Clk_Int'event and Clk_Int = '1' then
     
       if Rd_Int_In = '1' and GUARTStatusReg_N = '0' then
          UART_Status_Reg(6 downto 0)    <= UART_Status_Reg(6 downto 0);
          UART_Status_Reg(22 downto 16)  <= UART_Status_Reg(22 downto 16);
          UART_Status_Par                <= UART_Status_Par;
       else
          UART_Status_Reg(6 downto 0)    <= UART_Status_Reg1(6 downto 0);
          UART_Status_Reg(22 downto 16)  <= UART_Status_Reg1(22 downto 16);
          UART_Status_Par                <= UART_Status_Par1;
       end if;
            
     end if;
        
    end process;
    
        
 process(UART_A_Reg,UART_A_Par,UART_B_Reg,UART_B_Par,
         UART_Status_Reg,UART_Status_Par)    
 begin
    D_UARTA_Out(7 downto 0)   <= UART_A_Reg;
    D_UARTA_Out(31 downto 8)  <= "000000000000000000000000";
    DPar_UARTA_Out            <= UART_A_Par;

    D_UARTB_Out(7 downto 0)   <= UART_B_Reg;
    D_UARTB_Out(31 downto 8)  <= "000000000000000000000000";
    DPar_UARTB_Out            <= UART_B_Par;
            
    D_UARTS_Out(6 downto 0)   <= UART_Status_Reg(6 downto 0);
    D_UARTS_Out(15 downto 7)  <= "000000000";
    D_UARTS_Out(22 downto 16) <= UART_Status_Reg(22 downto 16);
    D_UARTS_Out(31 downto 23) <= "000000000";
    DPar_UARTS_Out            <= UART_Status_Par;
            
 end process;


end Mini_Spec ;



---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      UART control
-- File name:                  \mec\source\uarts.vhd
-- VHDL unit:                   UARTs 
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;



entity UARTs is
port (
          Clk_Int          : in Std_Logic;
          UART_Clk_En      : in Std_Logic;
          Reset_Int_N      : in Std_Logic;
                    
          DoNotRdUART      : in Std_Logic;
           
          GUARTAReg_N      : in Std_Logic;
          GUARTBReg_N      : in Std_Logic;
          GUARTStatusReg_N : in Std_Logic;
                    
          UARTAReg_N       : in Std_Logic;
          UARTBReg_N       : in Std_Logic;
          UARTStatusReg_N  : in Std_Logic;
          MECControlReg_N  : in Std_Logic;
          Wr_Int_N         : in Std_Logic;
          Rd_Int_In        : in Std_Logic;
          D_Int_In         : in Std_Logic_Vector(31 downto 0);
          RxA_In           : in Std_Logic;
          RxB_In           : in Std_Logic;

          TxA_Out          : out Std_Logic;
          TxB_Out          : out Std_Logic;
          Intr_UARTA_Data  : out Std_Logic;
          Intr_UARTB_Data  : out Std_Logic;
          UARTs_Quiet      : out Std_Logic;
          Intr_UART_Err    : out Std_Logic;
          D_UARTA_Out      : out Std_Logic_Vector(31 downto 0);
          DPar_UARTA_Out   : out Std_Logic;
          D_UARTB_Out      : out Std_Logic_Vector(31 downto 0);
          DPar_UARTB_Out   : out Std_Logic;
          D_UARTS_Out      : out Std_Logic_Vector(31 downto 0);
          DPar_UARTS_Out   : out Std_Logic
     );
end UARTs;

architecture Mini_spec of UARTs is

component UART 
    port (
          MCK           : in std_logic;   -- MEC clock
          TBR1          : in Std_Logic;   -- D_UART_In(0)
          TBR2          : in Std_Logic;   -- D_UART_In(1)
          TBR3          : in Std_Logic;   -- D_UART_In(2)
          TBR4          : in Std_Logic;   -- D_UART_In(3)
          TBR5          : in Std_Logic;   -- D_UART_In(4)
          TBR6          : in Std_Logic;   -- D_UART_In(5)
          TBR7          : in Std_Logic;   -- D_UART_In(6)
          TBR8          : in Std_Logic;   -- D_UART_In(7)

          TBRL_N          : in Std_Logic;   -- Wr_Data_N
          CRL           : in Std_Logic;   -- Wr_Control
          SBS           : in Std_Logic;   -- SBSel
          
          PI            : in Std_Logic;   -- ParInh
          EPE           : in Std_Logic;   -- EvenOdd_N
          
          DRR           : in Std_Logic;   -- D_ReadyRes_N
          
          TRC           : in Std_Logic;   -- UART_Clk_En_T
          
          RRC           : in Std_Logic;   -- UART_Clk_En_R
          
          RRI           : in Std_Logic;   -- Rx_In
          
          MR            : in Std_Logic;   -- MReset
          
          RBR1          : out Std_Logic;  -- D_UART_Out(0)
          RBR2          : out Std_Logic;  -- D_UART_Out(1)
          RBR3          : out Std_Logic;  -- D_UART_Out(2)
          RBR4          : out Std_Logic;  -- D_UART_Out(3)
          RBR5          : out Std_Logic;  -- D_UART_Out(4)
          RBR6          : out Std_Logic;  -- D_UART_Out(5)
          RBR7          : out Std_Logic;  -- D_UART_Out(6)
          RBR8          : out Std_Logic;  -- D_UART_Out(7)
          
          DR            : out Std_Logic;  -- D_Ready
          
          TBRE          : out Std_Logic;  -- TrHoRegEm
          TRE           : out Std_Logic;  -- TrSeRegEm
          
          FE            : out Std_Logic;  -- Frame_Err
          PE            : out Std_Logic;  -- Par_Err
          OE            : out Std_Logic;  -- O_Err
          
          TRO           : out Std_Logic  -- Tx_Out
         );
end component;

component UARTControl 
port (
          Clk_Int         : in Std_Logic;
          Reset_Int_N     : in Std_Logic;
          DoNotRdUART     : in Std_Logic;
           
          GUARTAReg_N      : in Std_Logic;
          GUARTBReg_N      : in Std_Logic;
          GUARTStatusReg_N : in Std_Logic;
                    
          UARTAReg_N      : in Std_Logic;
          UARTBReg_N      : in Std_Logic;
          UARTStatusReg_N : in Std_Logic;
          MECControlReg_N : in Std_Logic;
          Wr_Int_N        : in Std_Logic;
          Rd_Int_In       : in Std_Logic;
          D_Int_In        : in Std_Logic_Vector(31 downto 0);
          
          Intr_UARTA_Data  : out Std_Logic;
          Intr_UARTB_Data  : out Std_Logic;
          UARTs_Quiet     : out Std_Logic;
          Intr_UART_Err   : out Std_Logic;
          
          D_UARTA_Out     : out Std_Logic_Vector(31 downto 0);
          DPar_UARTA_Out  : out Std_Logic;
          D_UARTB_Out     : out Std_Logic_Vector(31 downto 0);
          DPar_UARTB_Out  : out Std_Logic;
          D_UARTS_Out     : out Std_Logic_Vector(31 downto 0);
          DPar_UARTS_Out  : out Std_Logic;

          D_Ready_A     : in Std_Logic;
          D_Ready_B     : in Std_Logic;
          O_Err_A       : in Std_Logic;
          O_Err_B       : in Std_Logic;
          Frame_Err_A   : in Std_Logic;
          Frame_Err_B   : in Std_Logic;
          Par_Err_A     : in Std_Logic;
          Par_Err_B     : in Std_Logic;
          TrHoRegEm_A   : in Std_Logic;
          TrHoRegEm_B   : in Std_Logic;
          TrSeRegEm_A   : in Std_Logic;
          TrSeRegEm_B   : in Std_Logic;
          D_UART_A_Out  : in Std_Logic_Vector(7 downto 0);
          D_UART_B_Out  : in Std_Logic_Vector(7 downto 0);

          MReset_A      : out Std_Logic;
          MReset_B      : out Std_Logic;
          D_ReadyRes_A_N: out Std_Logic;
          D_ReadyRes_B_N: out Std_Logic;
          Wr_Data_A_N   : out Std_Logic;
          Wr_Data_B_N   : out Std_Logic;
          Wr_Control    : out Std_Logic;
          SBSel         : out Std_Logic;
          EvenOdd_N     : out Std_Logic;
          ParInh        : out Std_Logic;
          D_UART_In     : out Std_Logic_Vector(7 downto 0)
         );
end component;

signal D_Ready_A   : Std_Logic;
signal D_Ready_B   : Std_Logic;
signal O_Err_A     : Std_Logic;
signal O_Err_B     : Std_Logic;
signal Frame_Err_A : Std_Logic;
signal Frame_Err_B : Std_Logic;
signal Par_Err_A   : Std_Logic;
signal Par_Err_B   : Std_Logic;
signal TrHoRegEm_A : Std_Logic;
signal TrHoRegEm_B : Std_Logic;
signal TrSeRegEm_A : Std_Logic;
signal TrSeRegEm_B : Std_Logic;
signal D_UART_A_Out : Std_Logic_Vector(7 downto 0);
signal D_UART_B_Out : Std_Logic_Vector(7 downto 0);
signal MReset_A      : Std_Logic;
signal MReset_B      : Std_Logic;
signal D_ReadyRes_A_N : Std_Logic;
signal D_ReadyRes_B_N : Std_Logic;
signal Wr_Data_A_N   : Std_Logic;
signal Wr_Data_B_N   : Std_Logic;
signal Wr_Control    : Std_Logic;
signal SBSel         : Std_Logic;
signal EvenOdd_N     : Std_Logic;
signal ParInh        : Std_Logic;
signal D_UART_In     : Std_Logic_Vector(7 downto 0);
signal TxA           : Std_logic;
signal TxB           : Std_logic;

begin

TxA_Out <= TxA;
TxB_Out <= TxB;

UARTControl_1 : UARTControl 
port map  (
          Clk_Int => Clk_Int,
          Reset_Int_N => Reset_Int_N,
          DoNotRdUART => DoNotRdUART,
          
          GUARTAReg_N => GUARTAReg_N,
          GUARTBReg_N => GUARTBReg_N,
          GUARTStatusReg_N => GUARTStatusReg_N,
          
          UARTAReg_N => UARTAReg_N,
          UARTBReg_N => UARTBReg_N,
          UARTStatusReg_N => UARTStatusReg_N,
          
          MECControlReg_N => MECControlReg_N,
          Wr_Int_N => Wr_Int_N,
          Rd_Int_In => Rd_Int_In,
          D_Int_In => D_Int_In,

          Intr_UARTA_Data => Intr_UARTA_Data,
          Intr_UARTB_Data => Intr_UARTB_Data,
          UARTs_Quiet => UARTs_Quiet,
          Intr_UART_Err => Intr_UART_Err,
          D_UARTA_Out => D_UARTA_Out,
          DPar_UARTA_Out => DPar_UARTA_Out,
          D_UARTB_Out => D_UARTB_Out,
          DPar_UARTB_Out => DPar_UARTB_Out,
          D_UARTS_Out => D_UARTS_Out,
          DPar_UARTS_Out => DPar_UARTS_Out,

          D_Ready_A => D_Ready_A,
          D_Ready_B => D_Ready_B,
          O_Err_A => O_Err_A,
          O_Err_B => O_Err_B,
          Frame_Err_A => Frame_Err_A,
          Frame_Err_B => Frame_Err_B,
          Par_Err_A => Par_Err_A,
          Par_Err_B => Par_Err_B,
          TrHoRegEm_A => TrHoRegEm_A,
          TrHoRegEm_B => TrHoRegEm_B,
          TrSeRegEm_A => TrSeRegEm_A,
          TrSeRegEm_B => TrSeRegEm_B,
          D_UART_A_Out => D_UART_A_Out,
          D_UART_B_Out => D_UART_B_Out,

          MReset_A => MReset_A,
          MReset_B => MReset_B,
          D_ReadyRes_A_N => D_ReadyRes_A_N,
          D_ReadyRes_B_N => D_ReadyRes_B_N,
          Wr_Data_A_N => Wr_Data_A_N,
          Wr_Data_B_N => Wr_Data_B_N,
          Wr_Control => Wr_Control,
          SBSel => SBSel,
          EvenOdd_N => EvenOdd_N,
          ParInh => ParInh,
          D_UART_In => D_UART_In
         );
         
UART_A : UART 
port map  (
          MCK  => Clk_Int,
          
          TBR1 => D_UART_In(0),
          TBR2 => D_UART_In(1),
          TBR3 => D_UART_In(2),
          TBR4 => D_UART_In(3),
          TBR5 => D_UART_In(4),
          TBR6 => D_UART_In(5),
          TBR7 => D_UART_In(6),
          TBR8 => D_UART_In(7),

          TBRL_N => Wr_Data_A_N,
          CRL  => Wr_Control,
          SBS  => SBSel,
          
          PI   => ParInh,
          EPE  => EvenOdd_N,
          
          DRR  => D_ReadyRes_A_N,
          
          TRC  => UART_Clk_En,
          
          RRC  => UART_Clk_En,
          
          RRI  => RxA_In,
          
          MR   => MReset_A,
          
          RBR1 => D_UART_A_Out(0),
          RBR2 => D_UART_A_Out(1),
          RBR3 => D_UART_A_Out(2),
          RBR4 => D_UART_A_Out(3),
          RBR5 => D_UART_A_Out(4),
          RBR6 => D_UART_A_Out(5),
          RBR7 => D_UART_A_Out(6),
          RBR8 => D_UART_A_Out(7),
          
          DR   => D_Ready_A, 
                   
          TBRE => TrHoRegEm_A,
          TRE  => TrSeRegEm_A,
          
          FE   => Frame_Err_A,
          PE   => Par_Err_A,
          OE   => O_Err_A,
          
          TRO  => TxA
        );
UART_B : UART 
port map  (
          MCK  => Clk_Int,
          
          TBR1 => D_UART_In(0),
          TBR2 => D_UART_In(1),
          TBR3 => D_UART_In(2),
          TBR4 => D_UART_In(3),
          TBR5 => D_UART_In(4),
          TBR6 => D_UART_In(5),
          TBR7 => D_UART_In(6),
          TBR8 => D_UART_In(7),

          TBRL_N => Wr_Data_B_N,
          CRL  => Wr_Control,
          SBS  => SBSel,

          PI   => ParInh,
          EPE  => EvenOdd_N,
          
          DRR  => D_ReadyRes_B_N,
          
          TRC  => UART_Clk_En,
          
          RRC  => UART_Clk_En,
          
          RRI  => RxB_In,
          
          MR   => MReset_B,
          
          RBR1 => D_UART_B_Out(0),
          RBR2 => D_UART_B_Out(1),
          RBR3 => D_UART_B_Out(2),
          RBR4 => D_UART_B_Out(3),
          RBR5 => D_UART_B_Out(4),
          RBR6 => D_UART_B_Out(5),
          RBR7 => D_UART_B_Out(6),
          RBR8 => D_UART_B_Out(7),
          
          DR   => D_Ready_B,
          
          TBRE => TrHoRegEm_B,
          TRE  => TrSeRegEm_B,
          
          FE   => Frame_Err_B,
          PE   => Par_Err_B,
          OE   => O_Err_B,
          
          TRO  => TxB        );

end Mini_Spec;
---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      general purpose counter
-- File name:                  gpt.vhd
-- VHDL unit:                  GenPurpTimer
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.STD_LOGIC_UNSIGNED."-";

entity GenPurpTimer  is
    port (
    			 MECHalt_Int_N : IN Std_Logic;
           D_Int_In      : IN Std_Logic_Vector(31 downto 0);
           DPar_Int_In   : IN Std_Logic;
           Clk_Int       : IN Std_Logic;
           Reset_Int_N   : IN Std_Logic;
           Wr_Int_N      : IN Std_Logic;
           CounterLoad   : IN Std_Logic;
           ReLoad        : IN Std_Logic;
           ScalerLoad    : IN Std_Logic;
           ScalerHold    : IN Std_Logic;
           GPT_CountReg_N  : IN Std_Logic;
           GPT_ScalerReg_N : IN Std_Logic;
           ParErrGPTim   : OUT Std_Logic;
           CPar_GPTim    : OUT Std_Logic;
           SPar_GPTim    : OUT Std_Logic;
           GPTimTO       : OUT Std_Logic;
           S_GPTim       : OUT Std_Logic_Vector(16 downto 1);
           C_GPTim       : OUT Std_Logic_Vector(31 downto 0) );
end GenPurpTimer ;


architecture Mini_Spec of GenPurpTimer  is

    constant ScalerWidth : integer := 16; 

    signal ScalerReg       : Std_Logic_Vector(ScalerWidth downto 0);
    signal ScalerCountDff  : Std_Logic_Vector(ScalerWidth downto 0);
    signal ScalerCount     : Std_Logic_Vector(ScalerWidth downto 0);
    signal ScalerCounti    : Std_Logic_Vector(ScalerWidth downto 0);
    signal CounterReg      : Std_Logic_Vector(32 downto 0);
    signal CounterCount    : Std_Logic_Vector(32 downto 0);
    signal CounterCounti   : Std_Logic_Vector(32 downto 0);
    signal CounterCountDff : Std_Logic_Vector(32 downto 0);
    signal DPar_Scaler     : Std_Logic;
    signal DPar_Count      : Std_Logic;
    
    signal TimeOut_Generatedi    : Std_Logic;
    signal TimeOut_Generated_rck : Std_Logic;


begin

----------------------------------------------------------------------------
    Registers: process(Reset_Int_N,Clk_int)
    begin
      if Reset_Int_N = '0' then      --Reset Registers
        ScalerReg     <= (others => '1');
        CounterReg    <= (others => '1');
      elsif Clk_Int'event and Clk_Int='0' then 
        if Wr_Int_N = '0' then -- Write edge
        
          --Write Scaler Register
          if GPT_ScalerReg_N = '0' then     
            ScalerReg    <= DPar_Int_In & D_Int_In(ScalerWidth-1 downto 0);
          end if;
          
          --Write Counter Register
          if GPT_CountReg_N = '0' then    
            CounterReg    <= DPar_Int_In & D_Int_In;
          end if;
          
        end if;
      end if;
    end process;
    
     -- Check Parity
    ParErrGPTim <= 
        ParityCheck(ScalerCountDff(ScalerWidth-1 downto 0), 
                    ScalerCountDff(ScalerWidth)) or 
        ParityCheck(CounterCountDff(31 downto 0),CounterCountDff(32));

     -- Read Timer 
        
    C_GPTim    <= CounterCountDff(31 downto 0);
    CPar_GPTim <= CounterCountDff(32);
    S_GPTim    <= ScalerCountDff(ScalerWidth-1 downto 0);
    SPar_GPTim <= ScalerCountDff(ScalerWidth) ;


----------------------------------------------------------------------------
    Scaler: process(Reset_Int_N,Clk_Int)
    begin
      if Reset_Int_N = '0' then                         
        ScalerCountDff        <= (others => '1');
        CounterCountDff       <= (others => '1');
        TimeOut_Generated_rck <= '0';
        
      elsif Clk_Int'event and Clk_Int='1' then
      
        if not (MECHalt_Int_N = '0')  then
          CounterCountDff   <= CounterCounti;
          ScalerCountDff    <= ScalerCounti;
        end if;
        
        TimeOut_Generated_rck <= TimeOut_Generatedi;
     end if;
    end process;


    ScalerCounti  <= ScalerCountDff when ScalerHold = '1' else
                     ScalerReg(ScalerWidth) & 
                     ScalerReg(ScalerWidth-1 downto 0) when 
                               ((CounterLoad or ScalerLoad or ScalerCount(ScalerWidth)) = '1') else 
                     (DPar_Scaler & ScalerCount(ScalerWidth-1 downto 0));
 
    CounterCounti <= CounterCountDff when ScalerHold = '1' else 
                     CounterReg when ((CounterLoad or 
                                      (CounterCount(32) and ReLoad)) = '1') else
                     (DPar_Count & CounterCount(31 downto 0)) when 
                                     (ScalerCount(ScalerWidth) and not CounterCount(32)) = '1' else 
                     CounterCountDff;

    TimeOut_Generatedi <= 
                    '1' when (CounterCount(32) = '1') else
                    '0';

    GPTimTO      <= TimeOut_Generatedi and not TimeOut_Generated_rck;
    
    ScalerCount  <= ('0' & ScalerCountDff(ScalerWidth-1 downto 0)) - 1 when
		not is_x(ScalerCountDff(ScalerWidth-1 downto 0)) else
		"XXXXXXXXXXXXXXXXX";
    
    DPar_Scaler  <= ParityGen(ScalerCount(ScalerWidth-1 downto 0) );
                      
    CounterCount <= ('0' & CounterCountDff(31 downto 0)) - 1 when
		not is_x(CounterCountDff(31 downto 0)) else
		"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
    
    DPar_Count   <= ParityGen(CounterCount(31 downto 0));

end Mini_Spec ;

---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      RTC counter
-- File name:                  rtc.vhd
-- VHDL unit:                  RTCTimer
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.STD_LOGIC_UNSIGNED."-";

entity RTCTimer is
    port (
    			 MECHalt_Int_N   : IN Std_Logic;
           D_Int_In        : IN Std_Logic_Vector(31 downto 0);
           DPar_Int_In     : IN Std_Logic;
           Clk_Int         : IN Std_Logic;
           Reset_Int_N     : IN Std_Logic;
           Wr_Int_N        : IN Std_Logic;
           CounterLoad_RTC : IN Std_Logic;
           ReLoad_RTC      : IN Std_Logic;
           ScalerLoad_RTC  : IN Std_Logic;
           ScalerHold_RTC  : IN Std_Logic;
           RTC_CountReg_N  : IN Std_Logic;
           RTC_ScalerReg_N : IN Std_Logic;
           ParErr_RTCTim   : OUT Std_Logic;
           SPar_RTCTim     : OUT Std_Logic;
           CPar_RTCTim     : OUT Std_Logic;
           RTCTimTO        : OUT Std_Logic;
           S_RTCTim        : OUT Std_Logic_Vector(8 downto 1);
           C_RTCTim        : OUT Std_Logic_Vector(31 downto 0)
          );
end RTCTimer;


architecture Mini_Spec of RTCTimer is

    constant ScalerWidth : integer := 8; 

    signal TimeOut_Generated : Std_Logic ;

    signal ScalerReg       : Std_Logic_Vector(ScalerWidth downto 0);
    signal ScalerCountDff  : Std_Logic_Vector(ScalerWidth downto 0);
    signal ScalerCount     : Std_Logic_Vector(ScalerWidth downto 0);
    signal ScalerCounti    : Std_Logic_Vector(ScalerWidth downto 0);
    signal CounterReg      : Std_Logic_Vector(32 downto 0);
    signal CounterCount    : Std_Logic_Vector(32 downto 0);
    signal CounterCounti   : Std_Logic_Vector(32 downto 0);
    signal CounterCountDff : Std_Logic_Vector(32 downto 0);
    signal DPar_Scaler     : Std_Logic;
    signal DPar_Count      : Std_Logic;
    
    signal TimeOut_Generatedi    : Std_Logic;
    signal TimeOut_Generated_rck : Std_Logic;


begin

----------------------------------------------------------------------------
    Registers: process(Reset_Int_N, Clk_Int)
    begin
    if Reset_Int_N = '0' then
        ScalerReg     <= (others => '1');
        CounterReg    <= (others => '1');
        
    elsif Clk_Int'event and Clk_Int='0' then 
      if Wr_Int_N = '0' then -- Write edge
          if RTC_ScalerReg_N = '0' then     
            ScalerReg    <= DPar_Int_In & D_Int_In(ScalerWidth-1 downto 0);
          end if;
          
          if RTC_CountReg_N = '0' then    
            CounterReg    <= DPar_Int_In & D_Int_In;
          end if;
      end if;
      
    end if;
    end process;
    
     -- Check Parity
    ParErr_RTCTim <= 
        ParityCheck(ScalerCountDff(ScalerWidth-1 downto 0), 
                    ScalerCountDff(ScalerWidth)) or 
        ParityCheck(CounterCountDff(31 downto 0),CounterCountDff(32));

     -- Read Timer 
        
    C_RTCTim    <= CounterCountDff(31 downto 0);
    CPar_RTCTim <= CounterCountDff(32);
    S_RTCTim    <= ScalerCountDff(ScalerWidth-1 downto 0);
    SPar_RTCTim <= ScalerCountDff(ScalerWidth);


----------------------------------------------------------------------------
    Scaler: process(Reset_Int_N, Clk_Int)
    begin
    
    if Reset_Int_N = '0' then                         
        ScalerCountDff         <= (others => '1');
        CounterCountDff        <= (others => '1');
        TimeOut_Generated_rck  <= '0';
        
    elsif Clk_Int'event and Clk_Int ='1' then
	    
	    if not (MECHalt_Int_N = '0')  then
	        CounterCountDff   <= CounterCounti;
          ScalerCountDff    <= ScalerCounti;
      end if;
      
      TimeOut_Generated_rck <= TimeOut_Generatedi;
      
    end if;
    end process;


    ScalerCounti  <= ScalerCountDff when ScalerHold_RTC = '1' else
                     ScalerReg(ScalerWidth) & 
                          ScalerReg(ScalerWidth-1 downto 0) when 
                                 ((CounterLoad_RTC or ScalerLoad_RTC or ScalerCount(ScalerWidth)) = '1') else
                    (DPar_Scaler & ScalerCount(ScalerWidth-1 downto 0));

    CounterCounti <= CounterCountDff when ScalerHold_RTC = '1' else 
                     CounterReg when ((CounterLoad_RTC or 
                                       (CounterCount(32) and ReLoad_RTC)) = '1') else
                     (DPar_Count & CounterCount(31 downto 0)) when 
                                        (ScalerCount(ScalerWidth) and not (CounterCount(32))) = '1' else
                     CounterCountDff;

    TimeOut_Generatedi <= 
                    '1' when (CounterCount(32) = '1') else
                    '0';

    RTCTimTO     <= TimeOut_Generatedi and not TimeOut_Generated_rck;
    
    ScalerCount  <= ('0' & ScalerCountDff(ScalerWidth-1 downto 0)) - 1 when
                not is_x(ScalerCountDff(ScalerWidth-1 downto 0)) else
                "XXXXXXXXX";
    
    DPar_Scaler  <= ParityGen(ScalerCount(ScalerWidth-1 downto 0));
                        
    CounterCount <= ('0' & CounterCountDff(31 downto 0)) - 1 when
                not is_x(CounterCountDff(31 downto 0)) else
                "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
    
    DPar_Count   <= ParityGen(CounterCount(31 downto 0));

end Mini_Spec ;








---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      Timer control
-- File name:                  timmux.vhd
-- VHDL unit:                  TimerControl
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

use IEEE.STD_LOGIC_UNSIGNED.all;

entity TimerControl is
    port (
           SPar_GPTim_A      : in Std_Logic;
           CPar_GPTim_A      : in Std_Logic;
           SPar_RTCTim       : in Std_Logic;
           CPar_RTCTim       : in Std_Logic;
           S_RTCTim          : in Std_Logic_Vector(8 downto 1);
           C_RTCTim          : in Std_Logic_Vector(31 downto 0);
           S_GPTim_A         : in Std_Logic_Vector(16 downto 1);
           C_GPTim_A         : in Std_Logic_Vector(31 downto 0);
           ParErr_RTCTim     : in Std_Logic;
           ParErrGPTim_A     : in Std_Logic;

           ParityError_Tim   : out Std_Logic;
           D_Tim_Out         : out Std_Logic_Vector(31 downto 0);
           DPar_Tim_Out      : out Std_Logic;

           D_Int_In          : in Std_Logic_Vector(31 downto 0);
           Clk_Int           : in Std_Logic;
           Reset_Int_N       : in Std_Logic;
           Wr_Int_N          : in Std_Logic;
           GPT_A_CountReg_N  : in Std_Logic;
           GPT_A_ScalerReg_N : in Std_Logic;
           RTC_CountReg_N    : in Std_Logic;
           RTC_ScalerReg_N   : in Std_Logic;
           TimerControlReg_N : in Std_Logic;

           CounterLoad_A     : out Std_Logic;
           ReLoad_A          : out Std_Logic;
           ScalerLoad_A      : out Std_Logic;
           ScalerHold_A      : out Std_Logic;
           CounterLoad_RTC   : out Std_Logic;
           ReLoad_RTC        : out Std_Logic;
           ScalerLoad_RTC    : out Std_Logic;
           ScalerHold_RTC    : out Std_Logic);
end TimerControl;


architecture Mini_Spec of TimerControl is

signal TimerCtrl_Reg     : Std_Logic_Vector(11 downto 0);
signal TimerCtrl_Par     : Std_Logic;
signal TimerCtrlParCheck : Std_Logic;

begin
----------------------------------------------------------------------------
    Mux: process(CPar_RTCTim, C_RTCTim, RTC_CountReg_N, 
                 SPar_RTCTim, S_RTCTim, RTC_ScalerReg_N, 
                 CPar_GPTim_A, C_GPTim_A, GPT_A_CountReg_N, 
                 SPar_GPTim_A, S_GPTim_A, GPT_A_ScalerReg_N)
    begin
      for i in 0 to 7 loop
        D_Tim_Out(i) <= (C_RTCTim(i) and not (RTC_CountReg_N)) or
                        (S_RTCTim(i+1) and not (RTC_ScalerReg_N)) or
                        (C_GPTim_A(i) and not (GPT_A_CountReg_N)) or
                        (S_GPTim_A(i+1) and not (GPT_A_ScalerReg_N));
 
      end loop;
      
      for i in 8 to 15 loop
        D_Tim_Out(i) <= (C_RTCTim(i) and not (RTC_CountReg_N)) or
                        (C_GPTim_A(i) and not (GPT_A_CountReg_N)) or
                        (S_GPTim_A(i+1) and not (GPT_A_ScalerReg_N));
 
      end loop;
      
      for i in 16 to 31 loop
        D_Tim_Out(i) <= (C_RTCTim(i) and not (RTC_CountReg_N)) or
                        (C_GPTim_A(i) and not (GPT_A_CountReg_N));
 
      end loop;
      
      DPar_Tim_Out <= (CPar_RTCTim and not (RTC_CountReg_N)) or
                      (SPar_RTCTim and not (RTC_ScalerReg_N)) or
                      (CPar_GPTim_A and not (GPT_A_CountReg_N)) or
                      (SPar_GPTim_A and not (GPT_A_ScalerReg_N));
    end process;
    
    TimerCtrl_Reg(7 downto 4)  <= "0000";

    TimControlReg : process(Reset_Int_N, Clk_Int)
    begin
  --  wait until Clk_Int'event and Clk_Int='0';
    if Reset_Int_N = '0' then
        TimerCtrl_Reg(3 downto 0)  <= "0000";
        TimerCtrl_Reg(11 downto 8) <= "0001";
        TimerCtrl_Par <= '0';
        
    elsif Clk_Int'event and Clk_Int='0' then
    
      if Wr_Int_N = '0' and TimerControlReg_N = '0' then
        TimerCtrl_Reg(3 downto 0)  <= D_Int_In(3 downto 0);
        TimerCtrl_Reg(11 downto 8) <= D_Int_In(11 downto 8);
        TimerCtrl_Par              <= not (D_Int_In(0) xor D_Int_In(2) xor 
                                           D_Int_In(8) xor D_Int_In(10)); 
      else
        TimerCtrl_Reg(1)  <= '0';
        TimerCtrl_Reg(3)  <= '0';
    	  TimerCtrl_Reg(9)  <= '0';
      	TimerCtrl_Reg(11) <= '0';
      end if;
      
    end if;
    end process;

   	ReLoad_A        <=     TimerCtrl_Reg(0);
    CounterLoad_A   <=     TimerCtrl_Reg(1);        
    ScalerHold_A    <= not TimerCtrl_Reg(2);
   	ScalerLoad_A    <=     TimerCtrl_Reg(3);
    ReLoad_RTC      <=     TimerCtrl_Reg(8);
   	CounterLoad_RTC <=     TimerCtrl_Reg(9);        
    ScalerHold_RTC  <= not TimerCtrl_Reg(10);
    ScalerLoad_RTC  <=     TimerCtrl_Reg(11);


    TimerCtrlParCheck <= TimerCtrl_Par xor 
                         not (TimerCtrl_Reg(0) xor TimerCtrl_Reg(2) xor 
                              TimerCtrl_Reg(8) xor TimerCtrl_Reg(10));


    ParityError_Tim <= TimerCtrlParCheck or ParErr_RTCTim or ParErrGPTim_A; 

end Mini_Spec ;

---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      Timer
-- File name:                  \mec\source\timers.vhd
-- VHDL unit:                  Timers
-- Purpose and functionality:  (Text)
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;
  

entity Timers is
    port (
          Clk_Int           : in Std_Logic;
          Reset_Int_N       : in Std_Logic;
          MECHalt_Int_N     : in Std_Logic;
          Wr_Int_N          : in Std_Logic;
          GPT_A_CountReg_N  : in Std_Logic;
          GPT_A_ScalerReg_N : in Std_Logic;
          RTC_CountReg_N    : in Std_Logic;
          RTC_ScalerReg_N   : in Std_Logic;
          TimerControlReg_N : in Std_Logic;
          D_Int_In          : in Std_Logic_Vector(31 downto 0);
          DPar_Int_In       : in Std_Logic;
          ParityError_Tim   : out Std_Logic;
          D_Tim_Out         : out Std_Logic_Vector(31 downto 0);
          DPar_Tim_Out      : out Std_Logic;
          GPTimTO_A         : out Std_Logic;
          RTCTimTO          : out Std_Logic
         );
end Timers;

architecture Mini_Spec of Timers is
component TimerControl 
port (
           SPar_GPTim_A : in Std_Logic;
           CPar_GPTim_A : in Std_Logic;
           SPar_RTCTim : in Std_Logic;
           CPar_RTCTim : in Std_Logic;
           S_RTCTim : in Std_Logic_Vector(8 downto 1);
           C_RTCTim : in Std_Logic_Vector(31 downto 0);
           S_GPTim_A : in Std_Logic_Vector(16 downto 1);
           C_GPTim_A : in Std_Logic_Vector(31 downto 0);
           ParErr_RTCTim : in Std_Logic;
           ParErrGPTim_A : in Std_Logic;

           ParityError_Tim : OUT Std_Logic;
           D_Tim_Out : OUT Std_Logic_Vector(31 downto 0);
           DPar_Tim_Out : OUT Std_Logic;

           D_Int_In      : in Std_Logic_Vector(31 downto 0);
           Clk_Int       : in Std_Logic;
           Reset_Int_N   : in Std_Logic;
           Wr_Int_N      : in Std_Logic;
           GPT_A_CountReg_N  : in Std_Logic;
           GPT_A_ScalerReg_N : in Std_Logic;
           RTC_CountReg_N    : in Std_Logic;
           RTC_ScalerReg_N   : in Std_Logic;
           TimerControlReg_N : in Std_Logic;

           CounterLoad_A : OUT Std_Logic;
           ReLoad_A      : OUT Std_Logic;
           ScalerLoad_A  : OUT Std_Logic;
           ScalerHold_A : OUT Std_Logic;
           CounterLoad_RTC : OUT Std_Logic;
           ReLoad_RTC      : OUT Std_Logic;
           ScalerLoad_RTC  : OUT Std_Logic;
           ScalerHold_RTC : OUT Std_Logic );
end component;
component RTCTimer 
port (
--
           MECHalt_Int_N : in Std_Logic;
--
           D_Int_In      : in Std_Logic_Vector(31 downto 0);
           DPar_Int_In   : in Std_Logic;
           Clk_Int       : in Std_Logic;
           Reset_Int_N   : in Std_Logic;
           Wr_Int_N      : in Std_Logic;
           CounterLoad_RTC : in Std_Logic;
           ReLoad_RTC      : in Std_Logic;
           ScalerLoad_RTC  : in Std_Logic;
           ScalerHold_RTC : in Std_Logic;
           RTC_CountReg_N  : in Std_Logic;
           RTC_ScalerReg_N : in Std_Logic;
           ParErr_RTCTim  : OUT Std_Logic;
           SPar_RTCTim  : OUT Std_Logic;
           CPar_RTCTim  : OUT Std_Logic;
           RTCTimTO    : OUT Std_Logic;
           S_RTCTim     : OUT Std_Logic_Vector(8 downto 1);
           C_RTCTim     : OUT Std_Logic_Vector(31 downto 0)
          );
end component;
component GenPurpTimer 
port (
--
           MECHalt_Int_N : in Std_Logic;
--
           D_Int_In      : in Std_Logic_Vector(31 downto 0);
           DPar_Int_In   : in Std_Logic;
           Clk_Int       : in Std_Logic;
           Reset_Int_N   : in Std_Logic;
           Wr_Int_N      : in Std_Logic;
           CounterLoad   : in Std_Logic;
           ReLoad        : in Std_Logic;
           ScalerLoad    : in Std_Logic;
           ScalerHold    : in Std_Logic;
           GPT_CountReg_N  : in Std_Logic;
           GPT_ScalerReg_N : in Std_Logic;
           ParErrGPTim   : OUT Std_Logic;
           CPar_GPTim    : OUT Std_Logic;
           SPar_GPTim    : OUT Std_Logic;
           GPTimTO       : OUT Std_Logic;
           S_GPTim       : OUT Std_Logic_Vector(16 downto 1);
           C_GPTim       : OUT Std_Logic_Vector(31 downto 0)
          );
end component;

    signal ParErrGPTim_A : Std_Logic;
    signal SPar_GPTim_A  : Std_Logic;
    signal CPar_GPTim_A  : Std_Logic;
    signal S_GPTim_A     : Std_Logic_Vector(16 downto 1);
    signal C_GPTim_A     : Std_Logic_Vector(31 downto 0);
  --  signal ParErrGPTim_B : Std_Logic;
  --  signal SPar_GPTim_B  : Std_Logic;
  --  signal CPar_GPTim_B  : Std_Logic;
  --  signal S_GPTim_B     : Std_Logic_Vector(16 downto 1);
  --  signal C_GPTim_B     : Std_Logic_Vector(31 downto 0);
    signal ParErr_RTCTim : Std_Logic;
    signal SPar_RTCTim   : Std_Logic;
    signal CPar_RTCTim   : Std_Logic;
    signal S_RTCTim      : Std_Logic_Vector(8 downto 1);
    signal C_RTCTim      : Std_Logic_Vector(31 downto 0);

    signal CounterLoad_A   : Std_Logic;
    signal ReLoad_A        : Std_Logic;
    signal ScalerLoad_A    : Std_Logic;
    signal ScalerHold_A    : Std_Logic;
    signal CounterLoad_B   : Std_Logic;
    signal ReLoad_B        : Std_Logic;
    signal ScalerLoad_B    : Std_Logic;
    signal ScalerHold_B    : Std_Logic;
    signal CounterLoad_RTC : Std_Logic;
    signal ReLoad_RTC      : Std_Logic;
    signal ScalerLoad_RTC  : Std_Logic;
    signal ScalerHold_RTC  : Std_Logic;

begin
TimerControl_1 : TimerControl 
port map  (
           SPar_GPTim_A => SPar_GPTim_A,
           CPar_GPTim_A => CPar_GPTim_A,
         --  SPar_GPTim_B => SPar_GPTim_B,
         --  CPar_GPTim_B => CPar_GPTim_B,
           SPar_RTCTim => SPar_RTCTim,
           CPar_RTCTim => CPar_RTCTim,
           S_RTCTim => S_RTCTim,
           C_RTCTim => C_RTCTim,
           S_GPTim_A => S_GPTim_A,
           C_GPTim_A => C_GPTim_A,
         --  S_GPTim_B => S_GPTim_B,
         --  C_GPTim_B => C_GPTim_B,
           ParErr_RTCTim => ParErr_RTCTim,
           ParErrGPTim_A => ParErrGPTim_A,
         --  ParErrGPTim_B => ParErrGPTim_B,

           ParityError_Tim => ParityError_Tim,
           D_Tim_Out => D_Tim_Out,
           DPar_Tim_Out => DPar_Tim_Out,

           D_Int_In => D_Int_In,
           Clk_Int => Clk_Int,
           Reset_Int_N => Reset_Int_N,
           Wr_Int_N => Wr_Int_N,
           GPT_A_CountReg_N => GPT_A_CountReg_N,
           GPT_A_ScalerReg_N => GPT_A_ScalerReg_N,
           RTC_CountReg_N => RTC_CountReg_N,
           RTC_ScalerReg_N => RTC_ScalerReg_N,
           TimerControlReg_N => TimerControlReg_N,

           CounterLoad_A => CounterLoad_A,
           ReLoad_A => ReLoad_A,
           ScalerLoad_A => ScalerLoad_A,
           ScalerHold_A => ScalerHold_A,
           CounterLoad_RTC => CounterLoad_RTC,
           ReLoad_RTC => ReLoad_RTC,
           ScalerLoad_RTC => ScalerLoad_RTC,
           ScalerHold_RTC => ScalerHold_RTC ); 

RTCTimer_1 : RTCTimer 
port map  (
           MECHalt_Int_N => MECHalt_Int_N,
           D_Int_In => D_Int_In,
           DPar_Int_In => DPar_Int_In,
           Clk_Int => Clk_Int,
           Reset_Int_N => Reset_Int_N,
           Wr_Int_N => Wr_Int_N,
           CounterLoad_RTC => CounterLoad_RTC,
           ReLoad_RTC => ReLoad_RTC,
           ScalerLoad_RTC => ScalerLoad_RTC,
           ScalerHold_RTC => ScalerHold_RTC,
           RTC_CountReg_N => RTC_CountReg_N,
           RTC_ScalerReg_N => RTC_ScalerReg_N,
           ParErr_RTCTim => ParErr_RTCTim,
           SPar_RTCTim => SPar_RTCTim,
           CPar_RTCTim => CPar_RTCTim,
           RTCTimTO => RTCTimTO,
           S_RTCTim => S_RTCTim,
           C_RTCTim => C_RTCTim );

GenPurpTimer_A : GenPurpTimer 
port map  (
           MECHalt_Int_N => MECHalt_Int_N,
           D_Int_In => D_Int_In,
           DPar_Int_In => DPar_Int_In,
           Clk_Int => Clk_Int,
           Reset_Int_N => Reset_Int_N,
           Wr_Int_N => Wr_Int_N,
           CounterLoad => CounterLoad_A,
           ReLoad => ReLoad_A,
           ScalerLoad => ScalerLoad_A,
           ScalerHold => ScalerHold_A,
           GPT_CountReg_N => GPT_A_CountReg_N,
           GPT_ScalerReg_N => GPT_A_ScalerReg_N,
           ParErrGPTim => ParErrGPTim_A,
           CPar_GPTim => CPar_GPTim_A,
           SPar_GPTim => SPar_GPTim_A,
           GPTimTO => GPTimTO_A,
           S_GPTim => S_GPTim_A,
           C_GPTim => C_GPTim_A );

end Mini_Spec;
---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:
-- File name:
-- VHDL unit:                  (Type)
-- Purpose and functionality:  (Text)
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;


entity TAP is
    port(
         TRST_In_N : in Std_Logic;
         TCK_In    : in Std_Logic;
         TDI_In    : in Std_Logic;
         TMS_In    : in Std_Logic;
         TDO_Out   : out Std_Logic
        );
end TAP;

architecture Mini_Spec of TAP is

    type TAP_State is (Reset, Idle,
      Sel_DR_Scan, Capture_DR, Shift_DR, Exit1_DR, Pause_DR, Exit2_DR, Update_DR,
      Sel_IR_Scan, Capture_IR, Shift_IR, Exit1_IR, Pause_IR, Exit2_IR, Update_IR);

    signal State : TAP_State;
    signal TDOInternal   : Std_Logic;

begin

  TAPControl: process (TRST_In_N,TCK_In)

    variable TMSInternal   : Std_Logic;
    variable TDIInternal   : Std_Logic;
    variable Bypass_Shift  : Std_Logic;
    variable IR_Reg        : Std_Logic_Vector (0 to 1);
    variable IR_Shift      : Std_Logic_Vector (0 to 1);
    variable IntScan_Reg   : Std_Logic_Vector (0 to 2);
    variable IntScan_Shift : Std_Logic_Vector (0 to 2);

  begin

    if TRST_In_N = '0' then   -- Asynchronous reset
      State <= Reset;
      IntScan_Reg := "101";

    elsif TCK_In'event and TCK_In = '1' then   
        TMSInternal := TMS_In;
        TDIInternal := TDI_In;

        case State is
        
          when Reset =>
            IR_Reg       := "00";   -- Bypass instruction
            IR_Shift     := "00";
            Bypass_Shift := '0';
            if TMSInternal = '1' then
              State <= Reset;
            else
              State <= Idle;
            end if;
            
          when Idle =>
            if TMSInternal = '1' then
              State <= Sel_DR_Scan;
            end if;
            
          when Sel_DR_Scan =>
            if TMSInternal = '1' then
              State <= Sel_IR_Scan;
            else
              State <= Capture_DR;
            end if;

          when Capture_DR =>
            case IR_Reg is
              when "11" => -- Capture bypass register
                TDOInternal <= Bypass_Shift;
              when "01" => -- Capture internal scan
                IntScan_Shift := IntScan_Reg;
                TDOInternal <= IntScan_Shift (0);
              when "10" => -- Capture boundary scan is not implemented.
                null;
              when others =>
                null;
            end case;

            if TMSInternal = '1' then
              State <= Exit1_DR;
            else
              State <= Shift_DR;
            end if;

          when Shift_DR =>
            case IR_Reg is
              when "11" =>                --Bypass
                Bypass_Shift := TDIInternal;
                TDOInternal  <= Bypass_Shift;
              when "01" =>                --Internal Scan
                IntScan_Shift(0 to 1) := IntScan_Shift(1 to 2);
                IntScan_Shift(2) := TDIInternal;
                TDOInternal <= IntScan_Shift(0);
              when "10" =>                --Boundary scan is not implemented.
                null;
              when others =>
                null;
            end case;

            if TMSInternal = '1' then
              State <= Exit1_DR;
            else
              State <= Shift_DR;
            end if;

          when Exit1_DR =>
            if TMSInternal = '1' then
              State <= Update_DR;
            else
              State <= Pause_DR;
            end if;

          when Pause_DR =>
            if TMSInternal = '1' then
              State <= Exit2_DR;
            else
              State <= Pause_DR;
            end if;

          when Exit2_DR =>
            if TMSInternal = '1' then
              State <= Update_DR;
            else
              State <= Shift_DR;
            end if;

          when Update_DR =>
            case IR_Reg is
              when "01" =>   -- Update chip from internal scan register
                IntScan_Reg := IntScan_Shift;
              when "10" =>   -- Update chip from boundary scan register is
                null;        -- not implemented.
              when others =>
                null;
            end case;
            if TMSInternal = '1' then
              State <= Sel_DR_Scan;
            else
              State <= Idle;
            end if;

          when Sel_IR_Scan =>
            if TMSInternal = '1' then
              State <= Reset;
            else
              State <= Capture_IR;
            end if;

          when Capture_IR =>
            IR_Shift := IR_Reg;
            if TMSInternal = '1' then
              State <= Exit1_IR;
            else
              State <= Shift_IR;
            end if;
            TDOInternal <= IR_Shift (0);

          when Shift_IR =>
            IR_Shift(0) := IR_Shift(1);
            IR_Shift(1) := TDIInternal;
            TDOInternal <= IR_Shift (0);
            if TMSInternal = '1' then
              State <= Exit1_IR;
            else
              State <= Shift_IR;
            end if;

          when Exit1_IR =>
            if TMSInternal = '1' then
              State <= Update_IR;
            else
              State <= Pause_IR;
            end if;

          when Pause_IR =>
            if TMSInternal = '1' then
              State <= Exit2_IR;
            else
              State <= Pause_IR;
            end if;

          when Exit2_IR =>
            if TMSInternal = '1' then
              State <= Update_IR;
            else
              State <= Shift_IR;
            end if;

          when Update_IR =>
            IR_Reg := IR_Shift;
            if TMSInternal = '1' then
              State <= Sel_DR_Scan;
            else
              State <= Idle;
            end if;
        end case;

     end if;
  end process;
  
  


OUTPUTS : process (TCK_In)
  begin
   if TCK_In'event and TCK_In ='0' then
     if State = Shift_DR or State = Shift_IR then
       TDO_Out <= TDOInternal;
     else
       TDO_Out <= '0';  
     end if;
   end if;
  end process;
  
  
end Mini_Spec;

---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:
-- File name:                  FAULT.VHD
-- VHDL unit:                  Fault_Handler(Mini_Spec)
-- Purpose and functionality:  To handle asynchronous and synchronous fault.
--                             To manage the SFFR register.
--                             To generate a load signal (Load_Fault_N) to
--                             enable loading of the FFAR register.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--


library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.std_logic_1164.all;
entity Fault_Handler is
    port (
           Clk_Int                 : in  Std_Logic;
           Rd_Int_In               : in  Std_Logic;
           NewCycle                : in  Std_Logic;
           SFSRTmp_UD              : in  Std_Logic;
	         Reset_Int_N             : in  Std_Logic;
           Reset_Out_N             : in  std_logic;
           SysFSReg_N              : in  Std_Logic;
           Wr_Int_N                : in  Std_Logic;
           Instr_Fetch             : in  Std_Logic;
           Null_Int_rck            : in  Std_Logic;
	         SysBus_Error            : in  Std_Logic;
	         BusTimeOut              : in  Std_Logic;
	         NCError_ck1             : in  Std_Logic;
	         Unimpl_Address_Out      : in  Std_Logic;
	         Illegal_Address_Out     : in  Std_Logic;
           MemAccess_Violation_Out : in  Std_Logic;
	         APar_Error              : in  Std_Logic;
	         DPar_Error_ck1          : in  Std_Logic;
	         CtlPar_Error            : in  Std_Logic;
           DMATimeOut              : in  Std_Logic;
           DMAInPrgs               : in  Std_Logic;
           Intr_UART_Err           : in  Std_Logic;
           CError_ck1              : in  Std_Logic;
           CError_g_rck            : out  Std_Logic;
           WDInterrupt             : in  Std_Logic;
           AccessType              : in  Std_Logic_Vector(3 downto 0);
           Load_FFAR_N             : out Std_Logic;
           D_Fault_Out             : out std_logic_Vector(31 downto 0);
           DPar_Fault_Out          : out Std_Logic;
           ParityErrorFault        : out Std_Logic;
           Intr_DMAAcc             : buffer Std_Logic;
           MExc_Int                : out Std_Logic;
           Write_Inhibit           : out Std_Logic
           );
end Fault_Handler;



architecture Mini_Spec of Fault_Handler is

  signal SFSR_Reg     : Std_Logic_Vector(15 downto 0);
  signal SFSR_Par     : Std_Logic;

  function Syn_Fault_Nr ( Vec : Std_Logic_Vector ) return Std_Logic_Vector is
    variable FaultType : Std_logic_Vector(3 downto 0);
  begin
    if Vec(0) = '1' then
      FaultType := "0000";
    elsif Vec(1) = '1' then
      FaultType := "0001";
    elsif Vec(2) = '1' then
      FaultType := "0010";
    elsif Vec(3) = '1' then
      FaultType := "0011";
    elsif Vec(4) = '1' then
      FaultType := "0100";
    elsif Vec(5) = '1' then
      FaultType := "0101";
    elsif Vec(6) = '1' then
      FaultType := "0110";
    elsif Vec(7) = '1' then
      FaultType := "0111";
    elsif Vec(8) = '1' then
      FaultType := "1000";
    elsif Vec(9) = '1' then
      FaultType := "1001";
    else
      FaultType := "1111";
    end if;
       
    return FaultType;
  end Syn_Fault_Nr;

  function Trap(Vec : Std_logic_Vector) return Std_Logic is
  begin
    return Vec(0) or Vec(1) or Vec(2) or Vec(3) or 
           Vec(4) or Vec(5) or Vec(6) or Vec(7) or 
           Vec(8) or Vec(9);
  end Trap;

  signal Syn_Fault_Type          : Std_Logic_Vector(3 downto 0);
  signal Syn_Data_Exc_N          : Std_Logic;
  
  signal UnImplArea_Int_rck      : Std_Logic;
  signal MECRegAcc_Error_Int_rck : Std_Logic;
  signal ProtectArea_rck         : Std_Logic;
  signal AddrPar_Error_rck       : Std_Logic;
  signal CtlPar_Error_rck       : Std_Logic;
  signal Instr_Fetch_Int_rck     : Std_Logic;
  signal BusTimeOut_foo          : Std_Logic;
  signal BusTimeOut_g            : Std_Logic;
  signal SysBus_Error_foo        : Std_Logic;
  signal SysBus_Error_g          : Std_Logic;
  signal SFSRTmp_UD_rck          : Std_Logic; 
  signal AccessType_rck          : Std_Logic_Vector(3 downto 0); 
  
  signal SFSR_Tmp             : Std_Logic_Vector(15 downto 0); 
  signal Update_SFSR          : Std_Logic; 
  signal SynFaultVec          : Std_Logic_Vector(0 to 9); 

  signal Intr_DMAAcc_WEE      : Std_Logic;
   
  signal CError_rck           : Std_Logic;
   
  signal NCError_foo_rck      : Std_Logic; 
  signal DPar_Error_foo_rck   : Std_Logic; 
  
  signal NCError_g            : Std_Logic; 
  signal DPar_Error_g         : Std_Logic; 
  signal DPar_Error_g_rck     : Std_Logic; 
  signal DPar_Error_g_fck     : Std_Logic; 
  
  signal CError_ck0           : Std_Logic; 
  signal NCError_ck0          : Std_Logic; 
  signal DPar_Error_ck0       : Std_Logic; 
  
  signal NCError_g0           : Std_Logic; 
  signal DPar_Error_g0        : Std_Logic; 
  
  signal DMATimeOut_rck       : Std_Logic; 
  signal DMATimeOut_rck1      : Std_Logic; 
  signal DMATimeOut_rckedge   : Std_Logic; 
  
  signal Intr_DMAAcc_Int      : Std_Logic; 
  signal Intr_DMAAcc_rck      : Std_Logic; 
  signal Intr_DMAAcc_rck1     : Std_Logic; 

  signal MExc_Int_loc         : Std_Logic; 
  signal Write_Inhibit_loc    : Std_Logic; 
  signal NonDataError_rck     : Std_Logic; 
  signal ResetRckSample_N     : Std_Logic; 
  
begin  
  
  SynFaultVec  <= CtlPar_Error_rck & DPar_Error_g & AddrPar_Error_rck & 
				          ProtectArea_rck & UnImplArea_Int_rck & '0' &
				          MECRegAcc_Error_Int_rck & NCError_g & BusTimeOut_g &  
				          SysBus_Error_g; 

  Syn_Fault_Type <= Syn_Fault_Nr(SynFaultVec);
				                            
  -- Sampled on CK+ 	
  	             
  MExc_Int_loc_Gen: process(SysBus_Error_g, BusTimeOut_g, Unimpl_Address_Out, 
                            Illegal_Address_Out, APar_Error, CtlPar_Error, DPar_Error_g)
                   
  begin   
      
  MExc_Int_loc      <= SysBus_Error_g or
                       BusTimeOut_g or 
                       Unimpl_Address_Out or 
                       Illegal_Address_Out or 
                       APar_Error or 
                       CtlPar_Error or DPar_Error_g;
      
  end process;

  -- DPar_Error_g to handle properly the data parity error detected during a store
  -- Data parity detected during a load is handle in the same way than NCerror.
  MExc_Int <= MemAccess_Violation_Out or MExc_Int_loc or DPar_Error_ck1; 
                    
                    
  -- Sampled on CK+ 		             
                   
  Intr_DMAAccGen: process(DMAInPrgs, SysBus_Error_g, BusTimeOut_g, MECRegAcc_Error_Int_rck, 
                          UnImplArea_Int_rck,   
                          AddrPar_Error_rck, CtlPar_Error_rck, ProtectArea_rck)
                   
  begin   
  -- Load and Store Cycle last 2 cycles at least, so there is not problems to
  -- rise the interrupt one clock later
      
  Intr_DMAAcc_WEE   <= DMAInPrgs and (SysBus_Error_g or BusTimeOut_g or 
                       MECRegAcc_Error_Int_rck or UnImplArea_Int_rck or
                       AddrPar_Error_rck or CtlPar_Error_rck or ProtectArea_rck);
      
  end process;

  Intr_DMAAcc_Int   <= Intr_DMAAcc_WEE or (DMAInPrgs and (NCError_g or DPar_Error_g));
                   
  -- Sampled on falling edge of CK2 following CK-	
                                  
  Write_Inhibit_loc_Gen: process(Unimpl_Address_Out, Illegal_Address_Out, 
                                 APar_Error, CtlPar_Error)
  begin   
      
  Write_Inhibit_loc <= Illegal_Address_Out or 
                       Unimpl_Address_Out or APar_Error or
                       CtlPar_Error;
      
  end process;
  
  Write_Inhibit  <= MemAccess_Violation_Out or Write_Inhibit_loc or 
                    NCError_g0 or DPar_Error_g0; 
  
  
  Syn_Data_Exc_N <= not(not(Instr_Fetch_Int_rck) and Trap(SynFaultVec)) or 
                    Null_Int_rck;

-------------------------------------------------------------
-- Process that handles address exceptions and latch them ---
-------------------------------------------------------------


  ResetRckSample_N <= not Null_Int_rck and Reset_Int_N;

  
  rck_sampling1: process(ResetRckSample_N, Clk_Int)
  begin
    if ResetRckSample_N = '0' then
      UnImplArea_Int_rck      <= '0';
      MECRegAcc_Error_Int_rck <= '0';
      AddrPar_Error_rck       <= '0';
      CtlPar_Error_rck        <= '0';
      ProtectArea_rck         <= '0';
     
      NonDataError_rck        <= '0';
				                         
    elsif Clk_Int'event and Clk_Int = '1' then
      UnImplArea_Int_rck      <= Illegal_Address_Out;
      MECRegAcc_Error_Int_rck <= Unimpl_Address_Out;
      AddrPar_Error_rck       <= APar_Error;
      CtlPar_Error_rck        <= CtlPar_Error;
      ProtectArea_rck         <= MemAccess_Violation_Out;
     
      NonDataError_rck        <= CtlPar_Error_rck or AddrPar_Error_rck or
				                         ProtectArea_rck   or UnImplArea_Int_rck or 
				                         MECRegAcc_Error_Int_rck or 
				                         BusTimeOut_g or SysBus_Error_g;
    end if;
    
  end process;





  rck_sampling2: process
  begin
    wait until Clk_Int'event and Clk_Int = '1';
				                        
    Instr_Fetch_Int_rck     <= Instr_Fetch;
    AccessType_rck          <= AccessType;
    
    SFSRTmp_UD_rck          <= SFSRTmp_UD;
                
    DMATimeOut_rck          <= DMATimeOut;
    Intr_DMAAcc_rck         <= Intr_DMAAcc_Int;
    
    DMATimeOut_rck1         <= DMATimeOut_rck;
    Intr_DMAAcc_rck1        <= Intr_DMAAcc_rck;

  end process;


  DMATimeOut_rckedge   <= DMATimeOut_rck    and not DMATimeOut_rck1;
  
  Intr_DMAAcc          <= Intr_DMAAcc_rck   and not Intr_DMAAcc_rck1;


  -------------------------------------------------------------------------------
  -- SFSR Content calculation
  -------------------------------------------------------------------------------  
  SFSR_Tmp(0)  <= '0';  
  SFSR_Tmp(1)  <= '0';  
  
  SFSR_Tmp(7)  <= '0';  
  SFSR_Tmp(11) <= '0';  
  
  SFSRGen: process   
  begin   
    -- Handle all error inputs
    wait until Clk_Int'event and Clk_Int = '1';
      
          -- Synchronous Data exception
      if (DMAInPrgs = '0' and  Syn_Data_Exc_N = '0' and SFSRTmp_UD = '1') or
          -- Asynchronous Fault
         (SFSR_Reg(2) = '0' and 
           (DMATimeOut_rckedge = '1'  or Intr_DMAAcc_Int = '1' or 
            Intr_UART_Err = '1' or WDInterrupt = '1' or 
            CError_ck1 = '1')) then
        Update_SFSR <= '1';
        
      else
        Update_SFSR <= '0';

      end if;


          -- Synchronous Data exception
      if (DMAInPrgs = '0' and  Syn_Data_Exc_N = '0' and SFSRTmp_UD = '1') or
          -- Asynchronous Fault
         (SFSR_Reg(2) = '0' and 
           (DMATimeOut_rckedge = '1'  or Intr_DMAAcc_Int = '1' or 
            Intr_UART_Err = '1' or WDInterrupt = '1' or 
            CError_ck1 = '1')) then
        SFSR_Tmp(6 downto 3)   <= Syn_Fault_Type;
        SFSR_Tmp(15 downto 12) <= AccessType_rck;
        

      end if;


      -- Synchronous Data exception
      if DMAInPrgs = '0' and  Syn_Data_Exc_N = '0' and SFSRTmp_UD = '1'then
      
        SFSR_Tmp(2)            <= '1'; -- Set Synchronous Data Fault Valid
        SFSR_Tmp(8)            <= '0'; -- Clear Asynchronous Fault Valid
        SFSR_Tmp(10 downto 9)  <= SFSR_Reg(10 downto 9);
        
      -- Asynchronous Fault
      elsif  SFSR_Reg(2) = '0' and 
             (DMATimeOut_rckedge = '1'  or Intr_DMAAcc_Int = '1' or 
              Intr_UART_Err = '1' or WDInterrupt = '1' or 
              CError_ck1 = '1') then
        
        SFSR_Tmp(2)             <= SFSR_Reg(2);
        SFSR_Tmp(8)             <= '1';
        
        if WDInterrupt = '1' then
          SFSR_Tmp(10 downto 9) <= "00";
          
        elsif DMATimeOut_rckedge = '1' or Intr_DMAAcc_Int = '1' then
          SFSR_Tmp(10 downto 9) <= "01";
          
        elsif Intr_UART_Err = '1' then
          SFSR_Tmp(10 downto 9) <= "10";
          
        else
          SFSR_Tmp(10 downto 9) <= "11";
          
        end if;
                
      end if;

  end process;
  
  SFSR_Reg(0) <= '0';  
  SFSR_Reg(1) <= '0';  
  
  SFSRRegs: process(Reset_Int_N, Clk_Int)
  begin
    if Reset_Int_N = '0' then
      SFSR_Reg( 2)          <= '0';
      SFSR_Reg( 6 downto 3) <= "1111";
      SFSR_Reg(15 downto 7) <= "000000000";
      SFSR_Par              <= '1';
      
    elsif Clk_Int'event and Clk_Int = '0' then
    
      if Wr_Int_N = '0' and SysFSReg_N = '0' then
        -- Write only on failing edge
        -- Clear the register on write
        SFSR_Reg( 2)          <= '0';
        SFSR_Reg( 6 downto 3) <= "1111";
        SFSR_Reg(15 downto 7) <= "000000000";
        SFSR_Par              <= '1';
        
      elsif Update_SFSR = '1' then  
        SFSR_Reg(15 downto 2) <= SFSR_Tmp(15 downto 2); 
        SFSR_Par              <= ParityGen(SFSR_Tmp(15 downto 2));
          
      end if;
      
    end if;
  end process; 
  

  ---------------------------------------------------------------------------------  
  --  Load FFAR and FFDR Generation
  ---------------------------------------------------------------------------------    
--  LoadGen: process(NonDataError_rck, DPar_Error_g, NCError_g, SFSR_Reg, CError_rck, 
--                     Rd_Int_In, DPar_Error_g_rck)
  LoadGen: process(NonDataError_rck, DPar_Error_foo_rck, NCError_foo_rck, SFSR_Reg, 
                   CError_rck, Rd_Int_In, DPar_Error_g_rck)
                   
  begin   
      
    --  This modification has not been synthesized. A modification by hand has been 
    --  performed in the netlist
    --  if (NonDataError_rck = '1' or DPar_Error_g = '1' or NCError_g = '1') or
    
      if (NonDataError_rck = '1' or DPar_Error_foo_rck = '1' or NCError_foo_rck = '1') or
         (Rd_Int_In = '0' and DPar_Error_g_rck = '1') then         
        Load_FFAR_N <= '0';
        
      elsif  SFSR_Reg(2) = '0' and SFSR_Reg(1) = '0' and SFSR_Reg(8) = '0' and 
             CError_rck = '1' then         
        Load_FFAR_N <= '0';

      else
        Load_FFAR_N <= '1';
                
      end if;
      
  end process;

  
  ------
  D_Fault_Out(15 downto 0)  <= SFSR_Reg;
  D_Fault_Out(31 downto 16) <= "0000000000000000";
  DPar_Fault_Out            <= SFSR_Par;

  ------
  ParityErrorFault <= ParityGen(SFSR_Reg) xor SFSR_Par;

  ---
  Rcksampling: process(Reset_Out_N, Clk_Int)
  begin
    if Reset_Out_N = '0' then
      CError_rck   <= '0';
      
    elsif Clk_Int'event and Clk_Int = '1' then
      CError_rck   <= CError_ck0;
      
    end if;
  end process; 
  
  BTOsampling: process(Reset_Out_N, Clk_Int)
  begin
    if Reset_Out_N = '0' then
      BusTimeOut_foo   <= '0';
      SysBus_Error_foo <= '0';
      
    elsif Clk_Int'event and Clk_Int = '1' then
      if SFSRTmp_UD = '1' or  Null_Int_rck = '1' then
         BusTimeOut_foo   <= '0';
         SysBus_Error_foo <= '0';
      else
         BusTimeOut_foo   <= BusTimeOut or BusTimeOut_foo;
         SysBus_Error_foo <= SysBus_Error or SysBus_Error_foo;
      end if;
      
    end if;
  end process; 
  
  
  BusTimeOut_g   <= BusTimeOut_foo   or BusTimeOut;
  SysBus_Error_g <= SysBus_Error_foo or SysBus_Error;
  

  ErrorSampling: process(Reset_Out_N, Clk_Int)
  begin
    if Reset_Out_N = '0' then
      NCError_foo_rck        <= '0';        
      DPar_Error_foo_rck     <= '0';
      
    elsif Clk_Int'event and Clk_Int = '1' then
      if DMAInPrgs = '0' and  SFSRTmp_UD = '1' then
        NCError_foo_rck      <= '0';        
        DPar_Error_foo_rck   <= '0';
      elsif DMAInPrgs = '1' and  SFSRTmp_UD_rck = '1' then
        NCError_foo_rck      <= '0';        
        DPar_Error_foo_rck   <= '0';
      else          
        NCError_foo_rck      <= NCError_ck1 or NCError_foo_rck;        
        DPar_Error_foo_rck   <= DPar_Error_ck1 or DPar_Error_foo_rck;

      end if;
      
    end if;
  end process; 

  ErrorLatching: process(Clk_Int, CError_ck1, NCError_ck1, DPar_Error_ck1)
  begin
    if Clk_Int = '0' then
        CError_ck0       <= CError_ck1;        
        NCError_ck0      <= NCError_ck1;        
        DPar_Error_ck0   <= DPar_Error_ck1;
    end if;
  end process; 

  
  
  NCError_g0       <= NCError_ck0 or NCError_foo_rck; 
         
  DPar_Error_g0    <= DPar_Error_ck0 or DPar_Error_foo_rck;

  NCError_g        <= NCError_ck1 or NCError_foo_rck; 
         
  DPar_Error_g     <= DPar_Error_ck1 or DPar_Error_foo_rck;

  CError_g_rck     <= CError_ck1;


  --  This modification has not been synthesized. A modification by hand has been 
  --  performed in the netlist
  --Fck_smp: process(Reset_Out_N, Clk_Int)
  --begin
  --  if Reset_Out_N = '0' then
  --    DPar_Error_g_fck     <= '0';
  --  elsif Clk_Int'event and Clk_Int = '0' then
  --    DPar_Error_g_fck   <= DPar_Error_g;
  --  end if;
  --end process; 

  Rck_smp: process(Reset_Out_N, Clk_Int)
  begin
    if Reset_Out_N = '0' then
      DPar_Error_g_rck     <= '0';
    elsif Clk_Int'event and Clk_Int = '1' then
   --   DPar_Error_g_rck   <= DPar_Error_g_fck;
      DPar_Error_g_rck   <= DPar_Error_g;
    end if;
  end process; 

  
end Mini_Spec ;

---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      error handler
-- File name:                  errhandl.vhd
-- VHDL unit:                  Error_Handler
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.std_logic_1164.all;


entity Error_Handler is
    port (
           Clk_Int         : IN  std_logic;
           D_Int_In        : IN  std_logic_Vector(31 downto 0);
           DPar_Int_In     : IN  std_logic;
           CPUHalt_OUT_N   : IN  std_logic;
           Reset_OUT_N     : IN  std_logic;
           Reset_Cause     : IN  Std_Logic_Vector(1 downto 0);
           ResetOutDetect  : IN  std_logic;
           NoClkDetect     : IN  std_logic;
           ErrResStatReg_N : IN  std_logic;
           Wr_Int_N        : IN  std_logic;
           ErrorCtrl       : IN  Std_Logic_Vector(9 downto 0);
           IUErr_IN_N      : IN  std_logic;
           IUHWErr_IN_N    : IN  std_logic;
           IUCmpErr_IN_N   : IN  std_logic;
           FPUHWErr_IN_N   : IN  std_logic;
           FPUCmpErr_IN_N  : IN  std_logic;
           MecHWErr_Int_N  : IN  std_logic;
           SWHalt_En       : IN  std_logic;
           SysErr_OUT_N    : OUT std_logic;
           MecHWErr_OUT_N  : OUT std_logic;
           ErrorReset_N    : OUT std_logic;
           ErrorHalt_N     : OUT std_logic;
           D_Error_Out     : OUT std_logic_Vector(31 downto 0);
           DPar_Error_Out  : OUT std_logic;
           SysAv_OUT       : OUT std_logic;
           Int_MaskErr     : OUT std_logic;
           ParityErrorErrHandl : OUT std_logic
           );
end Error_Handler;

architecture Mini_Spec of Error_Handler is

   signal SysAv_OUT_loc     : Std_Logic;
  
   signal ERSR_Tmp          : Std_Logic_Vector(5 downto 0);
   signal ERSR_Shadow       : Std_Logic_Vector(5 downto 0);
 
   signal ERSR_Reg          : Std_Logic_Vector(15 downto 0);
   signal ERSR_Par12_0      : Std_Logic;
   signal ERSR_Par15_13     : Std_Logic;
   
   signal Errors_Sig        : Std_Logic_Vector(5 downto 0);
   signal Errors_ERSR       : Std_Logic_Vector(5 downto 0);
   signal Errors            : Std_Logic_Vector(5 downto 0);
   signal MaskedErrors_ERSR : Std_Logic_Vector(5 downto 0);
   signal MaskedErrors      : Std_Logic_Vector(5 downto 0);
   
   signal EMR_Reg           : Std_Logic_Vector(5 downto 0);
   signal ResetOrHalt_N     : Std_Logic_Vector(5 downto 0);
   
   signal Int_MaskErr_rck1     : Std_Logic;
   signal Int_MaskErr_rck2     : Std_Logic;
   signal Int_MaskErr_fck      : Std_Logic;

begin

   EMR_Reg(0)       <= ErrorCtrl(0);
   EMR_Reg(1)       <= ErrorCtrl(2);
   EMR_Reg(2)       <= ErrorCtrl(4);
   EMR_Reg(3)       <= '1';
   EMR_Reg(4)       <= ErrorCtrl(6);
   EMR_Reg(5)       <= ErrorCtrl(8);
   
   ResetOrHalt_N(0) <= ErrorCtrl(1);
   ResetOrHalt_N(1) <= ErrorCtrl(3);
   ResetOrHalt_N(2) <= ErrorCtrl(5);
   ResetOrHalt_N(3) <= '0';
   ResetOrHalt_N(4) <= ErrorCtrl(7);
   ResetOrHalt_N(5) <= ErrorCtrl(9);   
   
----------------------------------------------------------
-- Process that controls the registers ---------
----------------------------------------------------------
  ERSR_Reg(11 downto 6)  <= "000000";

  Regs: process
  begin
    wait until Clk_Int'event and Clk_Int = '0';
    
    if ResetOutDetect = '1' then    -- Reset
      ERSR_Shadow            <= "000000";
      ERSR_Reg(5 downto 0)   <= "000000";
      ERSR_Reg(12)           <= '0';
      ERSR_Par12_0           <= '1';
    
      if Reset_Cause = "00" then  -- System Reset
        ERSR_Reg(13)           <= '0';
        ERSR_Reg(15 downto 14) <= "00";
        ERSR_Par15_13          <= '1';
                
      else
        ERSR_Reg(13)           <= '0';
        ERSR_Reg(15 downto 14) <= Reset_Cause;
        ERSR_Par15_13          <= (Reset_Cause(1) xor ERSR_Reg(15)) xor 
                                  (Reset_Cause(0) xor ERSR_Reg(14)) xor ERSR_Par15_13;
      end if;
      
    else
    
      if Wr_Int_N = '1' then
        ERSR_Shadow            <= ERSR_Shadow or ERSR_Tmp;
        ERSR_Reg(5 downto 0)   <= ERSR_Reg(5 downto 0) or ERSR_Tmp;
        ERSR_Par12_0           <= ParityGen((ERSR_Reg(5 downto 0) or ERSR_Tmp) & ERSR_Reg(12));
                
      elsif Wr_Int_N = '0' then
        if ErrResStatReg_N = '0' then -- Write to ERSR
	        if SWHalt_En = '1' then
            ERSR_Shadow           <= D_Int_In(5 downto 0) or ERSR_Tmp;
            ERSR_Reg(5 downto 0)  <= D_Int_In(5 downto 0) or ERSR_Tmp;
            ERSR_Reg(12)          <= D_Int_In(12);
            ERSR_Par12_0          <= ParityGen((D_Int_In(12) & "00000" & D_Int_In(5 downto 0)) or 
                                               ("000000" & ERSR_Tmp));
                        
          else -- always allow writing to system availabilty flag
            ERSR_Reg(12) <= D_Int_In(12);
            ERSR_Par12_0 <= ERSR_Par12_0 xor (ERSR_Reg(12) xor D_Int_In(12));
          end if;
        end if;
        
      end if;
      	
      if (CPUHalt_OUT_N = '0') then -- IU/FPU Halted
          ERSR_Reg(13)  <= '1';
          ERSR_Par15_13 <= ERSR_Par15_13 xor (not(ERSR_Reg(13)));
      end if;

      
    end if;

  end process;
  
  -----
  SamplPin: process(Reset_OUT_N, Clk_Int)
  begin

   if Reset_OUT_N = '0' then
    
      ERSR_Tmp <= "000000";
      
   elsif Clk_Int'event and Clk_Int = '1' then
   
    if IUErr_IN_N = '0' then
      ERSR_Tmp(0) <= '1';
    else
      ERSR_Tmp(0) <= '0';
    end if;
    
    if IUHWErr_IN_N = '0' then
      ERSR_Tmp(1) <= '1';
    else
      ERSR_Tmp(1) <= '0';
    end if;
    
    if IUCmpErr_IN_N = '0' and (ERSR_Reg(1) = '0') then
      ERSR_Tmp(2) <= '1';
    else
      ERSR_Tmp(2) <= '0';
    end if;
    
    if FPUHWErr_IN_N = '0' then
      ERSR_Tmp(3) <= '1';
    else
      ERSR_Tmp(3) <= '0';
    end if;
    
    if FPUCmpErr_IN_N = '0' and (ERSR_Reg(3) = '0') then
      ERSR_Tmp(4) <= '1';
    else
      ERSR_Tmp(4) <= '0';
    end if;
    
    if MecHWErr_Int_N = '0' then
      ERSR_Tmp(5) <= '1';
    else
      ERSR_Tmp(5) <= '0';
    end if;
    
  end if;
    
  end process;
  
  ERSR_Reg(11 downto 6)  <= "000000";

  -----
  D_Error_OutPROC: process(ERSR_Reg, ERSR_Par12_0, ERSR_Par15_13)
  begin
      D_Error_Out(5 downto 0)   <= ERSR_Reg(5 downto 0);
      D_Error_Out(11 downto 6)  <= "000000";
      D_Error_Out(15 downto 12) <= ERSR_Reg(15 downto 12);
      D_Error_Out(31 downto 16) <= "0000000000000000";
      
      DPar_Error_Out            <= not (ERSR_Par12_0 xor ERSR_Par15_13);
  end process;
  
  -----
  ParityErrorErrHandl <= (ParityGen(ERSR_Reg(12 downto  0)) xor ERSR_Par12_0) or
                         (ParityGen(ERSR_Reg(15 downto 13)) xor ERSR_Par15_13);


  -------------------------------------------------
  -- Processes that generates the output signals --
  -------------------------------------------------
  
  Errors_ERSR       <= ERSR_Shadow and not(EMR_Reg);
  Errors            <= ERSR_Tmp    and not(EMR_Reg);
  
  MaskedErrors_ERSR <= ERSR_Shadow and EMR_Reg;
  MaskedErrors      <= ERSR_Tmp    and EMR_Reg;
  
  -----
  ErrHand: process
   begin
    wait until Clk_Int'event and Clk_Int = '0'; 
    
    Int_MaskErr_fck          <= Vector_OR(MaskedErrors) or Vector_OR(MaskedErrors_ERSR);
    
    Errors_Sig(2 downto 0)   <= Errors(2 downto 0) or Errors_ERSR(2 downto 0);
    Errors_Sig(5 downto 4)   <= Errors(5 downto 4) or Errors_ERSR(5 downto 4);
    
    ErrorReset_N             <= not (Vector_OR(Errors      and ResetOrHalt_N) or 
                                     Vector_OR(Errors_ERSR and ResetOrHalt_N));
                                   
  end process;
  
  Errors_Sig(3) <= '0';
   
  -----
  HaltHand: process
  begin
  wait until Clk_Int'event and Clk_Int = '0'; 
      
      ErrorHalt_N  <= not (Vector_OR(Errors      and not (ResetOrHalt_N)) or
                           Vector_OR(Errors_ERSR and not (ResetOrHalt_N))) ;
  end process;
    
  -----
  SigClkRis: process(Reset_OUT_N, Clk_Int)
  begin
   if Reset_OUT_N = '0' then
     Int_MaskErr_rck1 <= '0';
     Int_MaskErr_rck2 <= '0';
     MecHWErr_OUT_N   <= '1';
     SysErr_OUT_N     <= '1';
   elsif Clk_Int'event and Clk_Int = '1' then
     Int_MaskErr_rck1 <= Int_MaskErr_fck;
     Int_MaskErr_rck2 <= Int_MaskErr_rck1;
     MecHWErr_OUT_N   <= not(ERSR_Shadow(5));
     SysErr_OUT_N     <= not(Vector_OR(Errors_Sig) and Vector_OR(Errors_ERSR));
   end if;
  end process;

  Int_MaskErr <= Int_MaskErr_rck1 and not Int_MaskErr_rck2;

  process(Reset_OUT_N, Clk_Int)
   begin
   if Reset_OUT_N = '0' then
     SysAv_OUT_loc <= '0';
   elsif Clk_Int'event and Clk_Int = '1' then
     SysAv_OUT_loc <= ERSR_Reg(12) and CPUHalt_OUT_N;
   end if;
  end process;
  
  SysAv_OUT <= SysAv_OUT_loc and not NoClkDetect;

end Mini_Spec ;


---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      interrupt handler
-- File name:                  inthandl.vhd
-- VHDL unit:                  inT_HANDLER
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.std_logic_1164.all;


entity inT_HANDLER is
    port (
      Clk_Int           : in  std_logic;
      MHold_Out_ck1_N   : in Std_Logic;
      D_Int_In          : in  std_logic_Vector(31 downto 0);
      DPar_Int_In       : in  std_logic;
      Reset_Int_N       : in  std_logic;
      Reset_Out_N       : in  std_logic;
      IntShapeReg_N     : in  std_logic;
      IntPendReg_N      : in  std_logic;
      IntMaskReg_N      : in  std_logic;
      IntClearReg_N     : in  std_logic;
      IntForceReg_N     : in  std_logic;
      Wr_Int_N          : in  std_logic;
      ExtInt_In_fck     : in  std_logic_vector(4 downto 0);
      ExtInt_In_rck     : in  std_logic_vector(4 downto 0);
      IntAck_In         : in  std_logic;
      A_Int_Trap        : in  std_logic_vector(3 downto 0);
      In_Int            : in  std_logic_vector(9 downto 0);
      IntrTest_En       : in  std_logic;
      
      ErrResStatReg_N   : in std_logic;
      D_Error_Out       : in std_logic_Vector(31 downto 0);
      DPar_Error_Out    : in std_logic;

      SysFSReg_N        : in  std_logic;
      D_Fault_Out       : in std_logic_Vector(31 downto 0);
      DPar_Fault_Out    : in std_logic;

      ParityErrorErrHandl : in std_logic;
      ParityErrorFault    : in std_logic;
      ParityErrorWDog     : in std_logic;
                   
      AnyInterrupt        : out std_logic;
      ExtIntAck_Out       : out std_logic;
      IRL_Out             : out std_logic_vector(3 downto 0);
      
      ParityErrorIntErr   : out std_logic;
      D_IntErr_Out        : out std_logic_Vector(31 downto 0);
      DPar_IntErr_out     : out std_logic            
    );
end inT_HANDLER;



architecture Mini_Spec of inT_HANDLER is



  ------------------------------------------------------------------------
  ----                     Function declarations         -----------------
  -----------------------------------------------------------------------
  function BitValOfIFR(IFR_Reg   : std_logic_vector;
                       IRL_AckNr : std_logic_vector
                      ) return std_logic is
    variable Res : std_logic;
    variable IRL_AckNr_LOC : std_logic_vector(3 downto 0);
  begin
    IRL_AckNr_LOC(3 downto 0) := IRL_AckNr(3 downto 0);
    case IRL_AckNr_LOC(3 downto 0) is
      when "1111" => Res := IFR_Reg(15);
      when "1110" => Res := IFR_Reg(14);
      when "1101" => Res := IFR_Reg(13);
      when "1100" => Res := IFR_Reg(12);
      when "1011" => Res := IFR_Reg(11);
      when "1010" => Res := IFR_Reg(10);
      when "1001" => Res := IFR_Reg(9);
      when "1000" => Res := IFR_Reg(8);
      when "0111" => Res := IFR_Reg(7);
      when "0110" => Res := IFR_Reg(6);
      when "0101" => Res := IFR_Reg(5);
      when "0100" => Res := IFR_Reg(4);
      when "0011" => Res := IFR_Reg(3);
      when "0010" => Res := IFR_Reg(2);
      when "0001" => Res := IFR_Reg(1);
      when others => Res := '0';
    end case;
     
    return Res;
  end BitValOfIFR;
  
  
  signal IRL_loc : std_logic_Vector(3 downto 0);

  --- The ISR, IPR, IMR and the IFR register
  signal ISR_Reg : std_logic_Vector(15 downto 0);
  signal ISR_Par : std_logic;
  signal IPR_Reg : std_logic_Vector(15 downto 0);
  signal IPR_Reg_Int : std_logic_Vector(15 downto 1);
  signal IPR_Par : std_logic;
  signal IMR_Reg : std_logic_Vector(15 downto 0);
  signal IMR_Par : std_logic;
  
  signal IFR_Reg : std_logic_Vector(15 downto 0);
  signal IFR_Par : std_logic;
    
  signal IPR_Reg_Din : std_logic_Vector(15 downto 1);
  
  signal IPRClear        : Std_Logic_Vector(15 downto 1);

  signal ICR_Reg         : Std_Logic_Vector(15 downto 1);
  signal ICRrck          : Std_Logic_Vector(15 downto 1);
  
  signal ExtIntDirty      : std_logic_vector(4 downto 0);
  signal ExtIntClean      : Std_Logic_Vector(4 downto 0);
  signal ExtInt_High      : Std_Logic_Vector(4 downto 0);

  signal ExtInt_High_rck  : Std_Logic_Vector(4 downto 0);
  signal ExtInt_PULS      : Std_Logic_Vector(4 downto 0);

  signal IRL_AckNr        : Std_Logic_Vector(3 downto 0);
  signal IntAck_Int_rck   : Std_Logic;
    
  signal ICRAdvReset      : Std_Logic;
  
  signal ExtIntAckQual1      : Std_Logic_Vector(6 downto 0);
  
  signal ExtIntAckQual2_2    : Std_Logic;
  signal ExtIntAckQual2_3    : Std_Logic;
  signal ExtIntAckQual2_10   : Std_Logic;
  signal ExtIntAckQual2_11   : Std_Logic;
  signal ExtIntAckQual2_14   : Std_Logic;
  
  signal ExtIntAck_Out_loc   : Std_Logic;
  signal IPRRstEn            : Std_Logic;
  
begin

	  -----
  Glitch: process(Clk_Int)
  begin
    if (Clk_Int'event and Clk_Int = '0') then
    	ExtIntDirty <= ExtInt_In_fck;
    end if;
  end process;
    
  -----
  Glitch_bis: process(Clk_Int)
  begin
   	if (Clk_Int'event and Clk_Int = '1') then
    	if ExtInt_In_rck(0) = ExtIntDirty(0) then
      	ExtIntClean(0) <= ExtIntDirty(0);
    	end if;
    	
    	if ExtInt_In_rck(1) = ExtIntDirty(1) then
      	ExtIntClean(1) <= ExtIntDirty(1);
    	end if;
    	
    	if ExtInt_In_rck(2) = ExtIntDirty(2) then
      	ExtIntClean(2) <= ExtIntDirty(2);
    	end if;
    	
    	if ExtInt_In_rck(3) = ExtIntDirty(3) then
      	ExtIntClean(3) <= ExtIntDirty(3);
      end if;
    
    	if ExtInt_In_rck(4) = ExtIntDirty(4) then
        ExtIntClean(4) <= ExtIntDirty(4);
    	end if;
    	
    end if;
    
  end process;

	
  ----------------------------------------------------------------------------
  A_Int_Trap_Smpl: process
  begin
    wait until (Clk_Int'event and Clk_Int = '1');
    if IPRRstEn = '1' then
      IRL_AckNr <= A_Int_Trap;
    else
      IRL_AckNr <= "0000";
    end if;
    IntAck_Int_rck <= IntAck_In and MHold_Out_ck1_N;
  end process;
  
  IPRRstEn_Gen: process(IntAck_In, IntAck_Int_rck, MHold_Out_ck1_N)
  begin
    if IntAck_In = '1' and IntAck_Int_rck = '0' and MHold_Out_ck1_N = '1' then
      IPRRstEn <= '1';
    else
      IPRRstEn <= '0';
    end if;
  end process;
  
  ----------------------------------------------------------------------------
  Regs_ISR_IMR: process(Reset_Int_N, Clk_Int)
  begin
    if Reset_Int_N = '0' then      -- Reset Registers
      ISR_Reg(12 downto 0) <= "0000000000000";
      ISR_Par              <= '1';
      IMR_Reg(14 downto 1) <= "11111111111111";
      IMR_Par              <= '1';
      
    elsif Clk_Int'event and Clk_Int = '0' then
        -- ISR
        if IntShapeReg_N = '0' and Wr_Int_N = '0' then        -- Write to ISR
          ISR_Reg(12 downto 0) <= D_Int_In(12 downto 0);
          ISR_Par              <= DPar_Int_In;
        end if;
        
        -- IMR
        if IntMaskReg_N = '0' and Wr_Int_N = '0' then         -- Write to IMR 
          IMR_Reg(14 downto 1) <= D_Int_In(14 downto 1);
          IMR_Par              <= DPar_Int_In;
        end if;
              
    end if;
    
  end process;

  ----------------------------------------------------------------------------
  IFR_Hdling: process(Reset_Int_N, Clk_Int)
    variable Tmp : std_logic;
  begin
    if Reset_Int_N = '0' then
      IFR_Reg(15 downto 1) <= "000000000000000";
      IFR_Par              <= '1';
      
    elsif  Clk_Int'event and Clk_Int = '0' then

      if IRL_AckNr /= "0000" then
          Tmp := BitValOfIFR(IFR_Reg, IRL_AckNr);
          if IntrTest_En = '1' and Tmp = '1' then
            case IRL_AckNr is
              when "1111" => IFR_Reg(15) <= '0';
                             IFR_Par <= not(IFR_Par);
              when "1110" => IFR_Reg(14) <= '0';
                             IFR_Par <= not(IFR_Par);
              when "1101" => IFR_Reg(13) <= '0';
                             IFR_Par <= not(IFR_Par);
              when "1100" => IFR_Reg(12) <= '0';
                             IFR_Par <= not(IFR_Par);
              when "1011" => IFR_Reg(11) <= '0';
                             IFR_Par <= not(IFR_Par);
              when "1010" => IFR_Reg(10) <= '0';
                             IFR_Par <= not(IFR_Par);
              when "1001" => IFR_Reg(9) <= '0';
                             IFR_Par <= not(IFR_Par);
              when "1000" => IFR_Reg(8) <= '0';
                             IFR_Par <= not(IFR_Par);
              when "0111" => IFR_Reg(7) <= '0';
                             IFR_Par <= not(IFR_Par);
              when "0110" => IFR_Reg(6) <= '0';
                             IFR_Par <= not(IFR_Par);
              when "0101" => IFR_Reg(5) <= '0';
                             IFR_Par <= not(IFR_Par);
              when "0100" => IFR_Reg(4) <= '0';
                             IFR_Par <= not(IFR_Par);
              when "0011" => IFR_Reg(3) <= '0';
                             IFR_Par <= not(IFR_Par);
              when "0010" => IFR_Reg(2) <= '0';
                             IFR_Par <= not(IFR_Par);
              when "0001" => IFR_Reg(1) <= '0';
                             IFR_Par <= not(IFR_Par);
              when others => NULL;
            end case;
          end if;
        
      elsif Wr_Int_N = '0' then
          if IntForceReg_N = '0' then
            IFR_Reg(15 downto 1) <= D_Int_In(15 downto 1);
            IFR_Par              <= DPar_Int_In;
          end if;
        
      end if;
      
    end if;
    
  end process;
  
  ----------------------------------------------------------------------------
  ICR_Hdling1: process(Clk_Int, Reset_Int_N)
  begin
    if Reset_Int_N = '0' then
      ICR_Reg <= "000000000000000";      
            
    elsif Clk_Int'event and Clk_Int = '0' then
    
      if Wr_Int_N = '0' and IntClearReg_N = '0' then
          ICR_Reg <= D_Int_In(15 downto 1); 
      elsif ICRAdvReset = '1' then
          ICR_Reg <= "000000000000000";
      end if;
      
    end if;

  end process;
  
  ICR_Hdling2: process(Reset_Int_N, Clk_Int)
  begin
    
    if Reset_Int_N = '0' then
      ICRAdvReset <= '0';
            
    elsif Clk_Int'event and Clk_Int = '0' then
    
      if Wr_Int_N = '0' and IntClearReg_N = '0' then
        ICRAdvReset <= '1';
      else      
        ICRAdvReset <= '0';
      end if;
      
    end if;

  end process;
  
      
  ----------------------------------------------------------------------------
  ICR_Sync: process
  begin
    wait until Clk_Int'event and Clk_Int = '1';        
         ICRrck <= ICR_Reg;
       
  end process;
  
  ----------------------------------------------------------------------------
  IPRClear_Hdling: process(IRL_AckNr)
  begin
    if IRL_AckNr /= "0000" then
      IPRClear <= (others => '0');
          case IRL_AckNr is
            when "1111" => IPRClear(15) <= '1';
            when "1110" => IPRClear(14) <= '1';
            when "1101" => IPRClear(13) <= '1';
            when "1100" => IPRClear(12) <= '1';
            when "1011" => IPRClear(11) <= '1';
            when "1010" => IPRClear(10) <= '1';
            when "1001" => IPRClear(9)  <= '1';
            when "1000" => IPRClear(8)  <= '1';
            when "0111" => IPRClear(7)  <= '1';
            when "0110" => IPRClear(6)  <= '1';
            when "0101" => IPRClear(5)  <= '1';
            when "0100" => IPRClear(4)  <= '1';
            when "0011" => IPRClear(3)  <= '1';
            when "0010" => IPRClear(2)  <= '1';
            when "0001" => IPRClear(1)  <= '1';
            when others => NULL;
          end case;
      
    else
      IPRClear <= (others => '0');
      
    end if;

  end process;

  ----------------------------------------------------------------------------
  ISR_Reg(13)  <= '0';
  ISR_Reg(14)  <= '0';
  ISR_Reg(15)  <= '0';
      
  IPR_Reg(0)   <= '0';
      
  IMR_Reg(0)   <= '0';
  IMR_Reg(15)  <= '0';
      
  IFR_Reg(0)   <= '0';
      
  IntHdl_Mux: process(IntShapeReg_N,   ISR_Reg,     ISR_Par,
                      IntPendReg_N,    IPR_Reg,     IPR_Par,
                      IntMaskReg_N,    IMR_Reg,     IMR_Par,
                      IntForceReg_N,   IFR_Reg,     IFR_Par,
                      ErrResStatReg_N, D_Error_Out, DPar_Error_Out,
                      SysFSReg_N,      D_Fault_Out, DPar_Fault_Out)
  begin
  
  
  	for i in 0 to 1 loop
    		D_IntErr_Out(i)   <= (ISR_Reg(i)     and not IntShapeReg_N) or 
    		                     (IPR_Reg(i)     and not IntPendReg_N) or
    		                     (IMR_Reg(i)     and not IntMaskReg_N) or
    		                     (IFR_Reg(i)     and not IntForceReg_N) or
    		                     (D_Error_Out(i) and not ErrResStatReg_N);
    end loop;
    		                       
  	for i in 2 to 5 loop
    		D_IntErr_Out(i)   <= (ISR_Reg(i)     and not IntShapeReg_N) or 
    		                     (IPR_Reg(i)     and not IntPendReg_N) or
    		                     (IMR_Reg(i)     and not IntMaskReg_N) or
    		                     (IFR_Reg(i)     and not IntForceReg_N) or
    		                     (D_Error_Out(i) and not ErrResStatReg_N) or 
    		                     (D_Fault_Out(i) and not SysFSReg_N);
    end loop;
    		                       
    		                       
    for i in 6 to 11 loop
    		D_IntErr_Out(i)   <= (ISR_Reg(i)     and not IntShapeReg_N) or 
    		                     (IPR_Reg(i)     and not IntPendReg_N) or
    		                     (IMR_Reg(i)     and not IntMaskReg_N) or
    		                     (IFR_Reg(i)     and not IntForceReg_N) or
    		                     (D_Fault_Out(i) and not SysFSReg_N);
    end loop;
    		                       
    		                       
    for i in 12 to 15 loop
    		D_IntErr_Out(i)   <= (ISR_Reg(i)     and not IntShapeReg_N) or 
    		                     (IPR_Reg(i)     and not IntPendReg_N) or
    		                     (IMR_Reg(i)     and not IntMaskReg_N) or
    		                     (IFR_Reg(i)     and not IntForceReg_N) or
    		                     (D_Error_Out(i) and not ErrResStatReg_N) or 
    		                     (D_Fault_Out(i) and not SysFSReg_N);
    end loop;
    		

    DPar_IntErr_Out   <= (ISR_Par        and not IntShapeReg_N) or 
    		                 (IPR_Par        and not IntPendReg_N) or
    		                 (IMR_Par        and not IntMaskReg_N) or
    		                 (IFR_Par        and not IntForceReg_N) or
                         (DPar_Error_Out and not ErrResStatReg_N) or 
    		                 (DPar_Fault_Out and not SysFSReg_N);
  end process;
  
  D_IntErr_Out(31 downto 16) <= "0000000000000000";
    
  ----------------------------------------------------------------------------
   
  ParityError_Or: process(ParityErrorErrHandl, ParityErrorFault,
                          ParityErrorWDog, 
                          ISR_Reg, ISR_Par,
                          IMR_Reg, IMR_Par,
                          IFR_Reg, IFR_Par)
  begin
     ParityErrorIntErr <= ParityErrorErrHandl or ParityErrorFault or
                          ParityErrorWDog or
                          (ParityGen(ISR_Reg) xor ISR_Par) or
                          (ParityGen(IMR_Reg) xor IMR_Par) or
                          (ParityGen(IFR_Reg) xor IFR_Par); 
  end process;


  ----------------------------------------------------------------------------
  ExtIntAckQual1    <= IRL_AckNr & ISR_Reg(7 downto 5);

  ExtIntAckQual2_2  <= (not(IntrTest_En and IFR_Reg(2))) ;
  ExtIntAckQual2_3  <= (not(IntrTest_En and IFR_Reg(3))) ;
  ExtIntAckQual2_10 <= (not(IntrTest_En and IFR_Reg(10))) ;
  ExtIntAckQual2_11 <= (not(IntrTest_En and IFR_Reg(11))) ;
  ExtIntAckQual2_14 <= (not(IntrTest_En and IFR_Reg(14))) ;

  ExtIntAck_Out_PROC: process(ExtIntAckQual1, 
                              ExtIntAckQual2_2, ExtIntAckQual2_3,
                              ExtIntAckQual2_10, ExtIntAckQual2_11, ExtIntAckQual2_14)
  begin
           case ExtIntAckQual1 is
             when "0010001" => 
                 ExtIntAck_Out_loc <= ExtIntAckQual2_2;
             when "0011010" => 
                 ExtIntAck_Out_loc <= ExtIntAckQual2_3;
             when "1010011" => 
                 ExtIntAck_Out_loc <= ExtIntAckQual2_10;
             when "1011100" => 
                 ExtIntAck_Out_loc <= ExtIntAckQual2_11;
             when "1110101" => 
                 ExtIntAck_Out_loc <= ExtIntAckQual2_14;
             when others => 
                 ExtIntAck_Out_loc <= '0';
           end case;
  end process;
  
  Delayprocess: process(Clk_Int, Reset_Out_N)
  begin
  
    if Reset_Out_N = '0' then
      ExtIntAck_Out <= '0';
    elsif Clk_Int'event and Clk_Int = '1' then
      ExtIntAck_Out <= ExtIntAck_Out_loc;
    end if;
   
  end process;

  -----------------------------------------------------------
  -- Processes that process the external interrupt signals --
  -----------------------------------------------------------
  ExtInt_High(4) <= not(ExtIntClean(4) xor ISR_Reg(12));
  ExtInt_High(3) <= not(ExtIntClean(3) xor ISR_Reg(11));
  ExtInt_High(2) <= not(ExtIntClean(2) xor ISR_Reg(10));
  ExtInt_High(1) <= not(ExtIntClean(1) xor ISR_Reg(9));
  ExtInt_High(0) <= not(ExtIntClean(0) xor ISR_Reg(8));

  -----
  ExtInt_HighSmpling: process
  begin
    wait until Clk_Int'event and Clk_Int = '1';
    ExtInt_High_rck(4) <= ExtInt_High(4) or not ISR_Reg(4);
    ExtInt_High_rck(3) <= ExtInt_High(3) or not ISR_Reg(3);
    ExtInt_High_rck(2) <= ExtInt_High(2) or not ISR_Reg(2);
    ExtInt_High_rck(1) <= ExtInt_High(1) or not ISR_Reg(1);
    ExtInt_High_rck(0) <= ExtInt_High(0) or not ISR_Reg(0);
  end process;


  ----- pulses detecting rising edge of signals ExtInt_High(*)
  -- No pulse is generated when level triggered interruptions
  ExtInt_PULS(4) <= ExtInt_High(4) and not(ExtInt_High_rck(4));
  ExtInt_PULS(3) <= ExtInt_High(3) and not(ExtInt_High_rck(3));
  ExtInt_PULS(2) <= ExtInt_High(2) and not(ExtInt_High_rck(2));
  ExtInt_PULS(1) <= ExtInt_High(1) and not(ExtInt_High_rck(1));
  ExtInt_PULS(0) <= ExtInt_High(0) and not(ExtInt_High_rck(0));



  ----- D inputs of IPR DFFs.


  
  IPR_Reg_Din(1) <= '0' when ((IPRClear(1) = '1' and IFR_Reg(1) = '0' and IntrTest_En = '1') or
                              (IPRClear(1) = '1' and IntrTest_En = '0')) and In_Int(0) = '0' else
                    '0' when ICR_Reg(1) = '1' and In_Int(0) = '0' else
                    IPR_Reg_Int(1) or In_Int(0);
                    
  IPR_Reg_Din(2) <= '0' when ((IPRClear(2) = '1' and IFR_Reg(2) = '0' and IntrTest_En = '1') or
                              (IPRClear(2) = '1' and IntrTest_En = '0')) and 
                             ((ExtInt_PULS(0) = '0' and ISR_Reg(0) = '1')) else
                    '0' when ICR_Reg(2) = '1' and (ExtInt_PULS(0) = '0' and ISR_Reg(0) = '1') else
                    IPR_Reg_Int(2) or ExtInt_PULS(0);
                    
  IPR_Reg_Din(3) <=  '0' when ((IPRClear(3) = '1' and IFR_Reg(3) = '0' and IntrTest_En = '1') or
                               (IPRClear(3) = '1' and IntrTest_En = '0')) and 
                              ((ExtInt_PULS(1) = '0' and ISR_Reg(1) = '1'))  else
                    '0' when ICR_Reg(3) = '1' and (ExtInt_PULS(1) = '0' and ISR_Reg(1) = '1') else
                    IPR_Reg_Int(3) or ExtInt_PULS(1);
                    
  IPR_Reg_Din(4) <= '0' when ((IPRClear(4) = '1' and IFR_Reg(4) = '0' and IntrTest_En = '1') or
                              (IPRClear(4) = '1' and IntrTest_En = '0')) and In_Int(1) = '0' else
                    '0' when ICR_Reg(4) = '1' and In_Int(1) = '0' else
                    IPR_Reg_Int(4) or In_Int(1);
                    
  IPR_Reg_Din(5) <= '0' when ((IPRClear(5) = '1' and IFR_Reg(5) = '0' and IntrTest_En = '1') or
                              (IPRClear(5) = '1' and IntrTest_En = '0')) and In_Int(2) = '0' else
                    '0' when ICR_Reg(5) = '1' and In_Int(2) = '0' else
                    IPR_Reg_Int(5) or In_Int(2);
                                        
  IPR_Reg_Din(6) <= '0' when ((IPRClear(6) = '1' and IFR_Reg(6) = '0' and IntrTest_En = '1') or
                              (IPRClear(6) = '1' and IntrTest_En = '0')) and In_Int(3) = '0' else
                    '0' when ICR_Reg(6) = '1' and In_Int(3) = '0' else
                    IPR_Reg_Int(6) or In_Int(3);
                    
  IPR_Reg_Din(7) <= '0' when ((IPRClear(7) = '1' and IFR_Reg(7) = '0' and IntrTest_En = '1') or
                              (IPRClear(7) = '1' and IntrTest_En = '0')) and In_Int(4) = '0' else
                    '0' when ICR_Reg(7) = '1' and In_Int(4) = '0' else
                    IPR_Reg_Int(7) or In_Int(4);
                    
  IPR_Reg_Din(8) <= '0' when ((IPRClear(8) = '1' and IFR_Reg(8) = '0' and IntrTest_En = '1') or
                              (IPRClear(8) = '1' and IntrTest_En = '0')) and 
                             (In_Int(5)) = '0' else
                    '0' when ICR_Reg(8) = '1' and (In_Int(5)) = '0' else
                    IPR_Reg_Int(8) or (In_Int(5));
                    
  IPR_Reg_Din(9) <= '0' when ((IPRClear(9) = '1' and IFR_Reg(9) = '0' and IntrTest_En = '1') or
                              (IPRClear(9) = '1' and IntrTest_En = '0')) and In_Int(6) = '0' else
                    '0' when ICR_Reg(9) = '1' and In_Int(6) = '0' else
                    IPR_Reg_Int(9) or In_Int(6);
                    
  IPR_Reg_Din(10) <= '0' when ((IPRClear(10) = '1' and IFR_Reg(10) = '0' and IntrTest_En = '1') or
                               (IPRClear(10) = '1' and IntrTest_En = '0')) and 
                              ((ExtInt_PULS(2) = '0' and ISR_Reg(2) = '1')) else
                     '0' when ICR_Reg(10) = '1' and (ExtInt_PULS(2) = '0' and ISR_Reg(2) = '1') else
                     IPR_Reg_Int(10) or ExtInt_PULS(2);
                     
  IPR_Reg_Din(11) <= '0' when ((IPRClear(11) = '1' and IFR_Reg(11) = '0' and IntrTest_En = '1') or
                               (IPRClear(11) = '1' and IntrTest_En = '0')) and 
                              ((ExtInt_PULS(3) = '0' and ISR_Reg(3) = '1'))  else
                     '0' when ICR_Reg(11) = '1' and (ExtInt_PULS(3) = '0' and ISR_Reg(3) = '1') else
                     IPR_Reg_Int(11) or ExtInt_PULS(3); 
                     
  IPR_Reg_Din(12) <= '0' when ((IPRClear(12) = '1' and IFR_Reg(12) = '0' and IntrTest_En = '1') or
                               (IPRClear(12) = '1' and IntrTest_En = '0')) and In_Int(7) = '0' else
                     '0' when ICR_Reg(12) = '1' and In_Int(7) = '0' else
                     IPR_Reg_Int(12) or In_Int(7);
                     
  IPR_Reg_Din(13) <= '0' when ((IPRClear(13) = '1' and IFR_Reg(13) = '0' and IntrTest_En = '1') or
                               (IPRClear(13) = '1' and IntrTest_En = '0')) and In_Int(8) = '0' else
                     '0' when ICR_Reg(13) = '1' and In_Int(8) = '0' else
                     IPR_Reg_Int(13) or In_Int(8);
                     
  IPR_Reg_Din(14) <= '0' when ((IPRClear(14) = '1' and IFR_Reg(14) = '0' and IntrTest_En = '1') or
                               (IPRClear(14) = '1' and IntrTest_En = '0')) and 
                              ((ExtInt_PULS(4) = '0' and ISR_Reg(4) = '1'))  else
                     '0' when ICR_Reg(14) = '1' and (ExtInt_PULS(4) = '0' and ISR_Reg(4) = '1') else
                     IPR_Reg(14) or ExtInt_PULS(4);
                     
  IPR_Reg_Din(15) <= '0' when ((IPRClear(15) = '1' and IFR_Reg(15) = '0' and IntrTest_En = '1') or
                               (IPRClear(15) = '1' and IntrTest_En = '0')) and In_Int(9) = '0' else
                     '0' when ICR_Reg(15) = '1' and In_Int(9) = '0' else
                     IPR_Reg_Int(15) or In_Int(9);
    

  -----
  IPR_Intprocess: process(Clk_Int, Reset_Out_N)
  begin
      
    if Reset_Out_N = '0' then
      IPR_Reg_Int(15 downto 1) <= "000000000000000";
    
    elsif Clk_Int'event and Clk_Int = '1' then
      IPR_Reg_Int(15 downto 1) <= IPR_Reg_Din(15 downto 1);
    end if;
   
  end process;

  IPR_process: process(IPR_Reg_Int, ISR_Reg, ExtInt_High)
  begin
  
    IPR_Reg(1)            <= IPR_Reg_Int(1);
      
    if ISR_Reg(0) = '0' then
      IPR_Reg(2) <= ExtInt_High(0);
    else
      IPR_Reg(2) <= IPR_Reg_Int(2);
    end if;
    
    if ISR_Reg(1) = '0' then
      IPR_Reg(3) <= ExtInt_High(1);
    else
      IPR_Reg(3) <= IPR_Reg_Int(3);
    end if;
    
    IPR_Reg(9 downto 4)   <= IPR_Reg_Int(9 downto 4);
    
    if ISR_Reg(2) = '0' then
      IPR_Reg(10) <= ExtInt_High(2);
    else
      IPR_Reg(10) <= IPR_Reg_Int(10);
    end if;
    
    if ISR_Reg(3) = '0' then
      IPR_Reg(11) <= ExtInt_High(3);
    else
      IPR_Reg(11) <= IPR_Reg_Int(11);
    end if;
    
    IPR_Reg(13 downto 12) <= IPR_Reg_Int(13 downto 12);
    
    if ISR_Reg(4) = '0' then
      IPR_Reg(14) <= ExtInt_High(4);
    else
      IPR_Reg(14) <= IPR_Reg_Int(14);
    end if;
    
    IPR_Reg(15)           <= IPR_Reg_Int(15);
   
  end process;



  
 IPR_Par <= ParityGen(IPR_Reg);
   
      
  --------------------------------------------------------
  -- Process that controls which IRL line to be active ---
  --------------------------------------------------------
  OUT_process: process(Reset_Out_N, Clk_Int)
    variable int_active : std_logic_vector(15 downto 1);
    variable int_masked : std_logic_vector(15 downto 1);
  begin
    if Reset_Out_N = '0' then 
      AnyInterrupt <= '0';
      IRL_loc      <= "0000";
   
    elsif Clk_Int'event and Clk_Int = '0' then
    
      if IntrTest_En = '1' then
        int_active(15 downto 1)  := IPR_Reg(15 downto 1) or IFR_Reg(15 downto 1);
      else
        int_active(15 downto 1)  := IPR_Reg(15 downto 1);
      end if;
     
      int_masked(1)            := int_active(1) and not IMR_Reg(1) and 
                                  not (IPRClear(1));
                               
      int_masked(2)            := int_active(2) and not IMR_Reg(2) and 
                                  not (IPRClear(2) and ISR_Reg(0));
                               
      int_masked(3)            := int_active(3) and not IMR_Reg(3) and 
                                  not (IPRClear(3) and ISR_Reg(1));
                               
      int_masked(9 downto 4)   := int_active(9 downto 4) and not IMR_Reg(9 downto 4) and 
                                  not (IPRClear(9 downto 4));
                               
      int_masked(10)           := int_active(10) and not IMR_Reg(10) and 
                                  not (IPRClear(10) and ISR_Reg(2));
                               
      int_masked(11)           := int_active(11) and not IMR_Reg(11) and 
                                  not (IPRClear(11) and ISR_Reg(3));
                               
      int_masked(13 downto 12) := int_active(13 downto 12) and not IMR_Reg(13 downto 12) and 
                                  not (IPRClear(13 downto 12));
                               
      int_masked(14)           := int_active(14) and not IMR_Reg(14) and 
                                  not (IPRClear(14) and ISR_Reg(4));
                               
      int_masked(15)           := int_active(15) and not (IPRClear(15));

 
                                 
      AnyInterrupt <= Vector_Or(int_masked);
      
      if int_masked(15) = '1' then
        IRL_loc <= "1111";
      elsif int_masked(14) = '1' then
        IRL_loc <= "1110";
      elsif int_masked(13) = '1' then
        IRL_loc <= "1101";
      elsif int_masked(12) = '1' then
        IRL_loc <= "1100";
      elsif int_masked(11) = '1' then
        IRL_loc <= "1011";
      elsif int_masked(10) = '1' then
        IRL_loc <= "1010";
      elsif int_masked(9) = '1' then
        IRL_loc <= "1001";
      elsif int_masked(8) = '1' then
        IRL_loc <= "1000";
      elsif int_masked(7) = '1' then
        IRL_loc <= "0111";
      elsif int_masked(6) = '1' then
        IRL_loc <= "0110";
      elsif int_masked(5) = '1' then
        IRL_loc <= "0101";
      elsif int_masked(4) = '1' then
        IRL_loc <= "0100";
      elsif int_masked(3) = '1' then
        IRL_loc <= "0011";
      elsif int_masked(2) = '1' then
        IRL_loc <= "0010";
      elsif int_masked(1) = '1' then
        IRL_loc <= "0001";
      else
        IRL_loc <= "0000";
      end if;
    
    end if;
      
  end process;
  
  IRL_Out <= IRL_loc;

end Mini_Spec ;

---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      watch dog
-- File name:                  wdog.vhd
-- VHDL unit:                  WatchDog
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.STD_LOGIC_UNSIGNED."-";


entity WatchDog is
    port(
         Reset_Out_N     : in Std_Logic;
         Clk_Int         : in Std_Logic;
         WDClk_In        : in Std_Logic;
         WDStrobe        : in Std_Logic;
         Wr_Int_N        : in Std_Logic;
         WDProgramReg_N  : in Std_Logic;
         WDTDSetReg_N    : in Std_Logic;
         D_Int_In        : in Std_Logic_Vector(31 downto 0);
         DPar_Int_In     : in Std_Logic;
         D_Wdog_Out      : out Std_Logic_Vector(31 downto 0);
         DPar_Wdog_Out   : out Std_Logic;
         WDInterrupt     : out Std_Logic;
         WDReset_N       : out Std_Logic;
         ParityErrorWDog : out Std_Logic
        );
end WatchDog;


architecture Mini_Spec of WatchDog is

--    type WatchDogControllerType is (WDInit,WDDisabled,WDEnabled,
--                                    WDResetTimerEnabled,WDHalted);

    
    signal State               : Std_Logic_Vector(2 downto 0);
    signal StatePar            : Std_Logic;

    signal D_Wdog_Out_loc      : Std_Logic_Vector(31 downto 0);
    
    signal WDProAndTOReg       : Std_Logic_Vector(31 downto 0);
    signal WDProAndTORegPar    : Std_Logic;
    signal ScalerCount         : Std_Logic_Vector(7 downto 0);
    signal CounterCount        : Std_Logic_Vector(15 downto 0);
    signal LoadCounter         : Std_Logic;
    signal LoadResetCounter    : Std_Logic;
    signal WDProgram           : Std_Logic;
    signal WDProgramReset      : Std_Logic;
    signal WDProgram_Temp      : Std_Logic;
    signal WDProgram_ClkWD     : Std_Logic;
    signal WDTrapDoorSet       : Std_Logic;
    signal WDTrapDoorSetReset  : Std_Logic;
    signal WDTrapDoorSet_Temp  : Std_Logic;
    signal WDTrapDoorSet_ClkWD : Std_Logic;
    signal Timeout             : Std_Logic;
    signal CounterClk          : Std_Logic;
    
    signal WDProgramReset_fck      : Std_Logic;
    signal WDTrapDoorSetReset_fck  : Std_Logic;
    
    signal WDInterrupt_ClkWD : Std_Logic;
    signal WDInterrupt_fck   : Std_Logic;
    
    signal WDInterrupt_rck1  : Std_Logic;
    signal WDInterrupt_rck2  : Std_Logic;
    
    signal RstWDMachines_1_N : Std_Logic;
    signal RstWDMachines_N   : Std_Logic;
    
    signal WDParCheck_fck1   : Std_Logic;
    signal WDParCheck_fck    : Std_Logic;
    
   
begin

  -----
    fckSync1: process(Reset_Out_N, Clk_Int)
    begin
                   
     if Reset_Out_N = '0' then               
        WDParCheck_fck1  <= '0';
        WDParCheck_fck   <= '0';
        
      elsif Clk_Int'event and Clk_Int = '0' then
        WDParCheck_fck1   <= ParityCheck(State, StatePar);
        WDParCheck_fck    <= WDParCheck_fck1;
     end if;
     
    end process;

  ParityErrorWDog <= ParityCheck(WDProAndTOReg, WDProAndTORegPar) or
                     WDParCheck_fck;

  -----
  WriteRegs: process(Reset_Out_N, Clk_Int)
  begin
    if Reset_Out_N = '0' then      --Reset Registers
      WDProAndTOReg    <= "11111111111111111111111111111111";
      WDProAndTORegPar <= '1';
      WDTrapDoorSet    <= '0';
      WDProgram        <= '0';
          
    elsif (Clk_Int'event and Clk_Int = '0') then
 
      if Wr_Int_N = '0' then
        --Write Watchdog Program and Timeout Acknowledge Register
        if (WDProgramReg_N = '0') then
          WDProAndTOReg    <= D_Int_In;
          WDProAndTORegPar <= DPar_Int_In;
          WDProgram        <= '1';

        --Write Watchdog Trap Door Set
        elsif (WDTDSetReg_N = '0') then
          WDTrapDoorSet <= '1';
        end if;

      else -- No write 
        -- Insure that the signals is valid from Clk- to Clk-
        WDTrapDoorSet <= '0';
        WDProgram     <= '0';

      end if;
    end if;
  end process;
  
  --### Generate reset signal for Watchdog Part
  RstWDMachinesGen: process(Reset_Out_N, WDClk_In)
  begin
    if Reset_Out_N = '0' then
      RstWDMachines_1_N <= '0';
      RstWDMachines_N   <= '0';
    elsif WDClk_In'event and WDClk_In = '1' then
      if WDStrobe = '1' then
         RstWDMachines_1_N <= '1';
         RstWDMachines_N   <= RstWDMachines_1_N;
      end if;
    end if;
  end process;
 
  -----
  fck_sync1:process(Clk_Int, Reset_Out_N)
  begin
    
      if Reset_Out_N = '0' then
        WDProgramReset_fck     <= '0';
        WDTrapDoorSetReset_fck <= '0';
        
      elsif Clk_Int'event and Clk_Int = '0' then
        WDProgramReset_fck     <= WDProgramReset;
        WDTrapDoorSetReset_fck <= WDTrapDoorSetReset;
      end if;
      
  end process;
    
  ----
    
  WDProgram_Sync1:process(Clk_Int, Reset_Out_N)
  begin
      if Reset_Out_N = '0' then    
        WDProgram_Temp <= '0';
      elsif Clk_Int'event and Clk_Int = '1' then
        if WDProgramReset_fck = '1' then    
          WDProgram_Temp <= '0';
        else
          WDProgram_Temp <= WDProgram or WDProgram_Temp;
        end if;
      end if;
  end process;

  -----

  WDProgram_Sync2:process(RstWDMachines_N, WDClk_In)
  begin
   if RstWDMachines_N = '0' then
      WDProgram_ClkWD <= '0';
      
   elsif WDClk_In'event and WDClk_In = '0' then
   
    if WDStrobe = '1' then
        WDProgram_ClkWD <= WDProgram_Temp;
    end if;
    
   end if;
  end process;


  -----

  WDTrapDoorSet_Sync1: process(Clk_Int, Reset_Out_N)
  begin
      if Reset_Out_N = '0' then
        WDTrapDoorSet_Temp <= '0';
      elsif Clk_Int'event and Clk_Int = '1' then
        if WDTrapDoorSetReset_fck = '1' then
          WDTrapDoorSet_Temp <= '0';
        else
          WDTrapDoorSet_Temp <= WDTrapDoorSet or WDTrapDoorSet_Temp;
        end if;
        
     end if;
  end process;

  -----
  WDTrapDoorSet_Sync2:process(RstWDMachines_N, WDClk_In)
  begin
    if RstWDMachines_N = '0' then
      WDTrapDoorSet_ClkWD <= '0';
      
    elsif WDClk_In'event and WDClk_In = '0' then
    
     if WDStrobe = '1' then
      WDTrapDoorSet_ClkWD <= WDTrapDoorSet_Temp;
     end if; 
    
    end if;
  end process;

  -----



  WatchDogController: process(RstWDMachines_N, WDClk_In)
  begin
    
  if RstWDMachines_N = '0' then
      State              <= "000" ;   --WDInit
      StatePar           <= '1' ;   --WDInit
      LoadCounter        <= '0';
      LoadResetCounter   <= '0';
      WDInterrupt_ClkWD  <= '0';
      WDReset_N          <= '1';
      WDProgramReset     <= '0'; 
      WDTrapDoorSetReset <= '0';
    
  elsif WDClk_In'event and WDClk_In = '1' then
  
    if WDStrobe = '1' then

      LoadCounter        <= '0';
      LoadResetCounter   <= '0';
      WDInterrupt_ClkWD  <= '0';
      WDReset_N          <= '1';
      WDProgramReset     <= '0'; 
      WDTrapDoorSetReset <= '0';

      case State is
      
        --WDInit
        when "000"  =>           
           --WDEnabled           
           if (WDProgram_ClkWD = '1') then
             State          <= "001";  
             StatePar       <= '0';  
             LoadCounter    <= '1'; 
             WDProgramReset <= '1'; 
             
           --WDDisabled
           elsif (WDTrapDoorSet_ClkWD = '1') then
             State              <= "100";
             StatePar           <= '0';  
             WDTrapDoorSetReset <= '1';
             
           --WDResetTimerEnabled
           elsif (Timeout = '1') then
             State           <= "010";  
             StatePar        <= '0';  
            LoadResetCounter <= '1';
             
           --WDInit
           else
             State      <= "000";  
             StatePar   <= '1';  
           end if;


        --WDDisabled
        when "100" =>   
              
           --WDEnabled
           if (WDProgram_ClkWD = '1') then
             State       <= "001";  
             StatePar    <= '0';  
             LoadCounter <= '1';
             
           --WDDisabled
           else
             State      <= "100";  
             StatePar   <= '0';  
           end if;


        --WDEnabled
        when "001" =>  
        
           --WDResetTimerEnabled
           if (Timeout = '1') then
             State            <= "010";  
             StatePar         <= '0';  
             LoadResetCounter <= '1';
             
           --WDEnabled
           elsif (WDProgram_ClkWD = '1') then
             State          <= "001";  
             StatePar       <= '0';  
             LoadCounter    <= '1';
             WDProgramReset <= '1';
             
           --WDEnabled
           else
             State      <= "001";  
             StatePar   <= '0';  
           end if;


        --WDResetTimerEnabled
        when "010" => 
         
           --WDEnabled
           if (WDProgram_ClkWD = '1') then
             State          <= "001"; 
             StatePar       <= '0';  
             LoadCounter    <= '1';
             WDProgramReset <= '1';
              
           --WDHalted
           elsif (Timeout = '1') then
             State      <= "011";
             StatePar   <= '1';  
               
           --WDResetTimerEnabled
           else
             State            <= "010";  
             StatePar         <= '0';  
            WDInterrupt_ClkWD <= '1';
           end if;


        --WDHalted
        when  "011" =>  
           --WDHalted
           State      <= "011";  
           StatePar   <= '1';  
           WDReset_N  <= '0';
           
        when others => -- generate a parity error
        
           State      <= "111";
           StatePar   <= '1';  
                                 
        end case;
        
     end if;
     
  end if;
  end process;

  -----
  Scaler: process(RstWDMachines_N, WDClk_In)
  begin
  if RstWDMachines_N = '0' then
      ScalerCount <= WDProAndTOReg(23 downto 16);
      CounterClk  <= '0';

  elsif WDClk_In'event and WDClk_In = '1' then
    if WDStrobe = '1' then

      if (LoadCounter = '1') or (LoadResetCounter = '1') then
        ScalerCount <= WDProAndTOReg(23 downto 16);
        CounterClk  <= '0';
	    else
        if (ScalerCount = "00000000") then
          ScalerCount <= WDProAndTOReg(23 downto 16);
          CounterClk  <= '1';
        else
	        ScalerCount <= ScalerCount - 1;
          CounterClk  <= '0';
        end if;
	    end if;
	    
	  end if;
	  
	end if;
  end process;

  -----
  Counter: process(RstWDMachines_N, WDClk_In)
  begin

  if RstWDMachines_N = '0' then
      CounterCount <= WDProAndTOReg(15 downto 0);
      Timeout      <= '0';
    
  elsif WDClk_In'event and WDClk_In = '1' then
    if WDStrobe = '1' then
      -- Synchronous Load
      if (LoadCounter = '1') then
        CounterCount <= WDProAndTOReg(15 downto 0);
        Timeout      <= '0';

      elsif (LoadResetCounter = '1') then
        CounterCount <= "00000000" & WDProAndTOReg(31 downto 24);
        Timeout      <= '0';

      elsif CounterClk = '1' then
        if (CounterCount = "0000000000000000") then
          Timeout      <= '1';
        else
          Timeout      <= '0';
          CounterCount <= CounterCount - 1;
        end if;
      else -- Always assert TimeOut
        Timeout <= '0';
      end if;
       
   end if;
   
  end if;
  end process;





  -----
  fck_sync2:process(Clk_Int, Reset_Out_N)
  begin
    
      if Reset_Out_N = '0' then
        WDInterrupt_fck <= '0';
      elsif Clk_Int'event and Clk_Int = '0' then
        WDInterrupt_fck <= WDInterrupt_ClkWD;
      end if;
      
  end process;
  
  rck_sync2:process(Clk_Int, Reset_Out_N)
  begin
    
      if Reset_Out_N = '0' then
        WDInterrupt_rck1 <= '0';
        WDInterrupt_rck2 <= '0';
        D_Wdog_Out_loc(23 downto 0) <= "000000000000000000000000";
        
      elsif Clk_Int'event and Clk_Int = '1' then
        WDInterrupt_rck1 <= WDInterrupt_fck;
        WDInterrupt_rck2 <= WDInterrupt_rck1;
        D_Wdog_Out_loc(15 downto 0)  <= CounterCount;
        D_Wdog_Out_loc(23 downto 16) <= ScalerCount;
      end if;
      
  end process;
  
  D_Wdog_Out_loc(31 downto 24) <= "00000000";

  D_Wdog_Out                   <= D_Wdog_Out_loc;
  
  DPar_Wdog_Out                <= ParityGen(D_Wdog_Out_loc);
  

  WDInterrupt <= WDInterrupt_rck1 and not WDInterrupt_rck2;
  
end Mini_Spec ;




---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      
-- File name:                  inTERR.VHD
-- VHDL unit:                  InterruptAndErrorHandling
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library IEEE;
use IEEE.std_logic_1164.all;
use work.all;
library MECLibrary;
use MECLibrary.all;
entity InterruptAndErrorHandling is
    port (
           Clk_Int         : in  std_logic;
           Rd_Int_In       : in Std_Logic;
           MHold_Out_ck1_N : in Std_Logic;
	         SFSRTmp_UD      : in  Std_Logic;
           WDClk_In        : in  std_logic;
           WDStrobe        : in  Std_Logic;
           NewCycle        : in  std_logic;
           D_Int_In        : in  std_logic_Vector(31 downto 0);
           DPar_Int_In     : in  std_logic;
           WDProgramReg_N  : in  std_logic;
           WDTDSetReg_N    : in  std_logic;
           CPUHalt_OUT_N   : in  std_logic;
           Reset_OUT_N     : in  std_logic;
           Reset_Int_N     : in  std_logic;
           Reset_Cause     : in  Std_Logic_Vector(1 downto 0);
           ResetOutDetect  : in std_logic;
           NoClkDetect     : in std_logic;
           ErrResStatReg_N : in  std_logic;
           Wr_Int_N        : in  std_logic;
           ErrorCtrl       : in  Std_Logic_Vector(9 downto 0);
           IUErr_in_N      : in  std_logic;
           IUHWErr_in_N    : in  std_logic;
           IUCmpErr_in_N   : in  std_logic;
           FPUHWErr_in_N   : in  std_logic;
           FPUCmpErr_in_N  : in  std_logic;
           SysFSReg_N      : in  std_logic;
           Instr_Fetch             : in  std_logic;
           Null_Int_rck            : in  std_logic;
	         SysBus_Error            : in  std_logic;
	         BusTimeOut              : in  std_logic;
	         NCError_ck1             : in  std_logic;
	         Unimpl_Address_Out      : in  std_logic;
	         
	         CPED                    : in  std_logic;
	         
	         Illegal_Address_Out     : in  std_logic;
           MemAccess_Violation_Out : in  std_logic;
	         APar_Error              : in  std_logic;
	         DPar_Error_ck1          : in  std_logic;
	         CtlPar_Error            : in  std_logic;
           DMATimeOut      : in  std_logic;
           DMAInPrgs       : in  std_logic;
           Intr_UART_Err   : in  std_logic;
           Intr_UARTA_Data : in  std_logic;
           Intr_UARTB_Data : in  std_logic;
           CError_ck1      : in  std_logic;
           AccessType      : in  Std_Logic_Vector(3 downto 0);
           IntShapeReg_N   : in  std_logic;
	         IntPendReg_N    : in  std_logic;
	         IntMaskReg_N    : in  std_logic;
	         IntClearReg_N   : in  std_logic;
	         IntForceReg_N   : in  std_logic;
           ExtInt_In_fck   : in  std_logic_vector(4 downto 0);
           ExtInt_In_rck   : in  std_logic_vector(4 downto 0);
	         IntAck_In       : in  std_logic;
	         A_Int_Trap      : in  std_logic_vector(3 downto 0);
           RTCTimTO        : in  std_logic;
           GPTimTO_A       : in  std_logic;

	         IntrTest_En     : in  std_logic;
           SWHalt_En       : in  std_logic;
           
           D_Wdog_Out      : out Std_Logic_Vector(31 downto 0);
           DPar_Wdog_Out   : out Std_Logic;
           
           AnyInterrupt    : out std_logic;
	         ExtIntAck_Out   : out std_logic;
	         IRL_Out         : out std_logic_vector(3 downto 0);
           Write_Inhibit   : out std_logic;
           MExc_Int        : out std_logic;
           Load_FFAR_N     : out std_logic;
           SysErr_OUT_N    : out std_logic;
           MecHWErr_OUT_N  : out std_logic;
           WDReset_N       : out std_logic;
           ErrorReset_N    : out std_logic;
           ErrorHalt_N     : out std_logic;
           ParityErrorIntErr : out std_logic;
           D_IntErr_Out    : out std_logic_Vector(31 downto 0);
           DPar_IntErr_OUT : out std_logic;
           SysAv_OUT       : out std_logic );
end InterruptAndErrorHandling;

architecture Mini_Spec of InterruptAndErrorHandling is
component Fault_Handler
port (
           Clk_Int                 : in  Std_Logic;
           Rd_Int_In               : in  Std_Logic;
           NewCycle                : in  Std_Logic;
           SFSRTmp_UD              : in  Std_Logic;
	         Reset_Int_N             : in  Std_Logic;
           Reset_Out_N             : in  std_logic;
           SysFSReg_N              : in  Std_Logic;
           Wr_Int_N                : in  Std_Logic;
           Instr_Fetch             : in  Std_Logic;
           Null_Int_rck            : in  Std_Logic;
	         SysBus_Error            : in  Std_Logic;
	         BusTimeOut              : in  Std_Logic;
	         NCError_ck1             : in  Std_Logic;
	         Unimpl_Address_Out      : in  Std_Logic;
	         Illegal_Address_Out     : in  Std_Logic;
           MemAccess_Violation_Out : in  Std_Logic;
	         APar_Error              : in  Std_Logic;
	         DPar_Error_ck1          : in  Std_Logic;
	         CtlPar_Error            : in  Std_Logic;
           DMATimeOut              : in  Std_Logic;
           DMAInPrgs               : in  Std_Logic;
           Intr_UART_Err           : in  Std_Logic;
           CError_ck1              : in  Std_Logic;
           CError_g_rck            : out Std_Logic;
           WDInterrupt             : in  Std_Logic;
           AccessType              : in  Std_Logic_Vector(3 downto 0);
           Load_FFAR_N             : out Std_Logic;
           D_Fault_Out             : out std_logic_Vector(31 downto 0);
           DPar_Fault_Out          : out Std_Logic;
           ParityErrorFault        : out Std_Logic;
           Intr_DMAAcc             : buffer Std_Logic;
           MExc_Int                : out Std_Logic;
           Write_Inhibit           : out Std_Logic
           );

end component;
component WatchDog 
port(
         Reset_Out_N    : in std_logic;
         Clk_Int        : in std_logic;
         WDClk_In       : in std_logic;
         WDStrobe       : in Std_Logic;
         Wr_Int_N       : in std_logic;
         WDProgramReg_N : in std_logic;
         WDTDSetReg_N   : in std_logic;
         D_Int_In       : in Std_Logic_Vector(31 downto 0);
         DPar_Int_In    : in std_logic;
         D_Wdog_Out     : out Std_Logic_Vector(31 downto 0);
         DPar_Wdog_Out  : out Std_Logic;
         WDInterrupt    : out std_logic;
         WDReset_N      : out std_logic;
         ParityErrorWDog: out std_logic
        );
end component;
component inT_HANDLER 
port (
	       Clk_Int           : in  std_logic;
         MHold_Out_ck1_N   : in Std_Logic;
	       D_Int_In          : in  std_logic_Vector(31 downto 0);
	       DPar_Int_In       : in  std_logic;
	       Reset_Int_N       : in  std_logic;
	       Reset_Out_N       : in  std_logic;
	       IntShapeReg_N     : in  std_logic;
	       IntPendReg_N      : in  std_logic;
	       IntMaskReg_N      : in  std_logic;
	       IntClearReg_N     : in  std_logic;
	       IntForceReg_N     : in  std_logic;
	       Wr_Int_N          : in  std_logic;
         ExtInt_In_fck     : in  std_logic_vector(4 downto 0);
         ExtInt_In_rck     : in  std_logic_vector(4 downto 0);
	       IntAck_In         : in  std_logic;
	       A_Int_Trap        : in  std_logic_vector(3 downto 0);
	       In_Int            : in  std_logic_vector(9 downto 0);
	       IntrTest_En       : in  std_logic;
	       	       
         ErrResStatReg_N   : in std_logic;
         D_Error_Out       : in std_logic_Vector(31 downto 0);
         DPar_Error_Out    : in std_logic;

         SysFSReg_N        : in  std_logic;
         D_Fault_Out       : in std_logic_Vector(31 downto 0);
         DPar_Fault_Out    : in std_logic;

         ParityErrorErrHandl : in std_logic;
         ParityErrorFault    : in std_logic;
         ParityErrorWDog     : in std_logic;
                   
         AnyInterrupt      : out std_logic;
	       ExtIntAck_Out     : out std_logic;
	       IRL_Out           : out std_logic_vector(3 downto 0);
	       
         ParityErrorIntErr   : out std_logic;
         D_IntErr_Out        : out std_logic_Vector(31 downto 0);
         DPar_IntErr_out     : out std_logic
          );
end component;

component Error_Handler 
port (
           Clk_Int         : in  std_logic;
           D_Int_In        : in  std_logic_Vector(31 downto 0);
           DPar_Int_In     : in  std_logic;
           CPUHalt_OUT_N   : in  std_logic;
           Reset_OUT_N     : in  std_logic;
           Reset_Cause     : in  Std_Logic_Vector(1 downto 0);
           ResetOutDetect  : in  Std_Logic;
           NoClkDetect     : in  Std_Logic;
           ErrResStatReg_N : in  std_logic;
           Wr_Int_N        : in  std_logic;
           ErrorCtrl       : in  Std_Logic_Vector(9 downto 0);
           IUErr_in_N      : in  std_logic;
           IUHWErr_in_N    : in  std_logic;
           IUCmpErr_in_N   : in  std_logic;
           FPUHWErr_in_N   : in  std_logic;
           FPUCmpErr_in_N  : in  std_logic;
           MecHWErr_Int_N  : in  std_logic;
           SWHalt_En       : in  std_logic;
           SysErr_OUT_N    : out std_logic;
           MecHWErr_OUT_N  : out std_logic;
           ErrorReset_N    : out std_logic;
           ErrorHalt_N     : out std_logic;
           D_Error_Out     : out std_logic_Vector(31 downto 0);
           DPar_Error_Out  : out std_logic;
           SysAv_OUT       : out std_logic;
           Int_MaskErr     : out std_logic;
           ParityErrorErrHandl : out std_logic
           );
end component;

signal ParityErrorWDog     : std_logic;
signal WDInterrupt         : std_logic;
signal D_IntHandl_Out      : Std_Logic_Vector(31 downto 0);
signal DPar_IntHandl_Out   : std_logic;
signal ParityErrorIntHandl : std_logic;
signal D_Fault_Out         : Std_Logic_Vector(31 downto 0);
signal DPar_Fault_Out      : std_logic;
signal ParityErrorFault    : std_logic; 
signal D_Error_Out         : std_Logic_Vector(31 downto 0);
signal DPar_Error_Out      : std_logic;
signal Int_MaskErr         : std_logic;
signal ParityErrorErrHandl : std_logic;
signal Intr_DMAAcc         : std_logic;
signal CError_g_rck        : std_logic;
signal MECHWErr_In_N       : std_logic;


begin

MECHWErr_In_N <= not CPED;

Fault_Handler_1 : Fault_Handler
port map  (
           Clk_Int => Clk_Int,
           Rd_Int_In => Rd_Int_In,
           NewCycle => NewCycle,
           SFSRTmp_UD => SFSRTmp_UD,
	         Reset_Int_N => Reset_Int_N,
	         Reset_Out_N => Reset_Out_N,
           SysFSReg_N => SysFSReg_N,
           Wr_Int_N => Wr_Int_N,
           Instr_Fetch => Instr_Fetch,
           Null_Int_rck => Null_Int_rck,
	         SysBus_Error => SysBus_Error,
	         BusTimeOut => BusTimeOut,
	         NCError_ck1 => NCError_ck1,
	         Unimpl_Address_Out => Unimpl_Address_Out,
	         Illegal_Address_Out => Illegal_Address_Out,
           MemAccess_Violation_Out => MemAccess_Violation_Out,
	         APar_Error => APar_Error,
	         DPar_Error_ck1 => DPar_Error_ck1,
	         CtlPar_Error => CtlPar_Error,
           DMATimeOut => DMATimeOut,
           DMAInPrgs => DMAInPrgs,
           Intr_UART_Err => Intr_UART_Err,
           CError_ck1 => CError_ck1,
           CError_g_rck => CError_g_rck,
           WDInterrupt => WDInterrupt,
           AccessType => AccessType,
           Load_FFAR_N => Load_FFAR_N,
           D_Fault_Out => D_Fault_Out,
           DPar_Fault_Out => DPar_Fault_Out,
           ParityErrorFault => ParityErrorFault,
           Intr_DMAAcc => Intr_DMAAcc,
           MExc_Int => MExc_Int,
           Write_Inhibit => Write_Inhibit
         );

WatchDog_1 : WatchDog 
port map (
         Reset_Out_N => Reset_Out_N,
         Clk_Int => Clk_Int,
         WDClk_In => WDClk_In,
         WDStrobe => WDStrobe,
         Wr_Int_N => Wr_Int_N,
         WDProgramReg_N => WDProgramReg_N,
         WDTDSetReg_N => WDTDSetReg_N,
         D_Int_In => D_Int_In,
         DPar_Int_In => DPar_Int_In,
         D_Wdog_Out => D_Wdog_Out,
         DPar_Wdog_Out => DPar_Wdog_Out,
         WDInterrupt => WDInterrupt,
         WDReset_N => WDReset_N,
         ParityErrorWDog => ParityErrorWDog
         );

inT_HANDLER_1 : inT_HANDLER 
port map (
	      Clk_Int => Clk_Int,
	      MHold_Out_ck1_N => MHold_Out_ck1_N,
	      D_Int_In => D_Int_In,
	      DPar_Int_In => DPar_Int_In,
	      Reset_Int_N => Reset_Int_N,
	      Reset_Out_N => Reset_Out_N,
	      IntShapeReg_N => IntShapeReg_N,
	      IntPendReg_N => IntPendReg_N,
	      IntMaskReg_N => IntMaskReg_N,
	      IntClearReg_N => IntClearReg_N,
	      IntForceReg_N => IntForceReg_N,
	      Wr_Int_N => Wr_Int_N,
	      ExtInt_In_fck => ExtInt_In_fck,
	      ExtInt_In_rck => ExtInt_In_rck,
	      IntAck_In => IntAck_In,
	      A_Int_Trap => A_Int_Trap,
	      In_Int(9) => WDInterrupt,
        In_Int(8) => RTCTimTO,
        In_Int(7) => GPTimTO_A,
        In_Int(6) => DMATimeOut,
        In_Int(5) => Intr_DMAAcc,
        In_Int(4) => Intr_UART_Err,
        In_Int(3) => CError_g_rck,
        
        In_Int(2) => Intr_UARTB_Data,
        
        In_Int(1) => Intr_UARTA_Data,
        In_Int(0) => Int_MaskErr,
	      IntrTest_En => IntrTest_En,
	      
        ErrResStatReg_N => ErrResStatReg_N,
        D_Error_Out => D_Error_Out,
        DPar_Error_Out => DPar_Error_Out,

        SysFSReg_N => SysFSReg_N,
        D_Fault_Out => D_Fault_Out,
        DPar_Fault_Out => DPar_Fault_Out,
	   	   
        ParityErrorErrHandl => ParityErrorErrHandl,
        ParityErrorFault => ParityErrorFault,
        ParityErrorWDog => ParityErrorWDog,
	   	   
        AnyInterrupt => AnyInterrupt,
	      ExtIntAck_Out => ExtIntAck_Out,
	      IRL_Out => IRL_Out,
	      
        ParityErrorIntErr => ParityErrorIntErr,
        D_IntErr_Out => D_IntErr_Out,
        DPar_IntErr_OUT => DPar_IntErr_OUT
        );


Error_Handler_1 : Error_Handler 
port map  (
           Clk_Int => Clk_Int,
           D_Int_In => D_Int_In,
           DPar_Int_In => DPar_Int_In,
           CPUHalt_OUT_N => CPUHalt_OUT_N,
           Reset_OUT_N => Reset_OUT_N,
           Reset_Cause => Reset_Cause,
           ResetOutDetect => ResetOutDetect,
           NoClkDetect => NoClkDetect,
           ErrResStatReg_N => ErrResStatReg_N,
           Wr_Int_N => Wr_Int_N,
           ErrorCtrl => ErrorCtrl,
           IUErr_in_N => IUErr_in_N,
           IUHWErr_in_N => IUHWErr_in_N,
           IUCmpErr_in_N => IUCmpErr_in_N,
           FPUHWErr_in_N => FPUHWErr_in_N,
           FPUCmpErr_in_N => FPUCmpErr_in_N,
           MecHWErr_Int_N => MECHWErr_In_N,
           SWHalt_En => SWHalt_En,
           SysErr_OUT_N => SysErr_OUT_N,
           MecHWErr_OUT_N => MecHWErr_OUT_N,
           ErrorReset_N => ErrorReset_N,
           ErrorHalt_N => ErrorHalt_N,
           D_Error_Out => D_Error_Out,
           DPar_Error_Out => DPar_Error_Out,
           SysAv_OUT => SysAv_OUT,
           Int_MaskErr => Int_MaskErr,
           ParityErrorErrHandl => ParityErrorErrHandl
         );

end Mini_Spec;


---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      power down control
-- File name:                  pwrdown.vhd
-- VHDL unit:                  PowerDownModeCtl
-- Purpose and functionality:  (Text)
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--


library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;


entity PowerDownModeCtl is
    port (
          Reset_Int_N  : in Std_Logic;
          Clk_Int      : in Std_Logic;
          Wr_Int_N     : in Std_Logic;
          PwrDReg_N    : in Std_Logic;
          PowerDownEn  : in Std_Logic;
          AnyInterrupt : in Std_Logic;
          PowerDown    : out Std_Logic
         );

end PowerDownModeCtl;

architecture Mini_Spec of PowerDownModeCtl is

begin

    PowerDownGenerator: process(Reset_Int_N, Clk_Int)
    begin
    if Reset_Int_N = '0' then
        PowerDown <= '0';
            
    elsif Clk_Int'event and Clk_Int = '0' then
            
        --Powerdown disable
        if (AnyInterrupt = '1') then
            PowerDown <= '0';
            
        --Powerdown enable
        elsif (PwrDReg_N = '0') and (PowerDownEn = '1') and 
              (Wr_Int_N = '0') then
            PowerDown <= '1';
        end if;
        
     end if;
        
    end process;   
end Mini_Spec;

---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      MEC Startup and Control / Reset Handler
-- File name:                  \mec\source\reshandl.vhd
-- VHDL unit:                  StartUpCtlAndRes
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE; 
use IEEE.std_logic_1164.all; 
use IEEE.STD_LOGIC_UNSIGNED."+";

entity StartUpCtlAndRes is
  port (
          Clk_Int         : in std_logic;
          Clk2_In         : in std_logic;
          Wr_Int_N        : in std_logic;
          MECControlReg_N : in std_logic;
          SWRReg_N        : in std_logic;
          SysReset_In_N   : in std_logic;
          WDReset_N       : in std_logic;
          ErrorReset_N    : in std_logic;
          DPar_Int_In     : in std_logic;

          RO              : out std_logic;
          Reset_N_ext     : out std_logic; 
          RI              : buffer std_logic; --Reset_Int_N
          PowerDownEn     : out std_logic;
          AccessProtEn    : out std_logic;
          WDClkSupply     : out std_logic;
          Baudrate        : out std_logic;
          UARTClkSupply   : out std_logic;
          ErrorCtrl       : out std_logic_vector(9 downto 0);
          DMAEnable_Int   : out std_logic;
          DMAParity_En    : out std_logic;
          DMATimeOut_En   : out std_logic;
          DPar_CtlSup_Out : out std_logic;
          ParErrCtlSup    : out std_logic;
          Reset_Cause     : out std_logic_vector(1 downto 0);

          D_Int_In        : in std_logic_vector(31 downto 0);

          D_CtlSup_Out    : out std_logic_vector(31 downto 0);
          UARTScalerReg   : out std_logic_vector(7 downto 0);
          BusTimeOut_En   : out std_logic;
          ResetOutDetect  : out std_logic;
          NoClkDetect     : out std_logic;
          RstCK2Div       : out std_logic
       );

end StartUpCtlAndRes;



architecture Mini_Spec of StartUpCtlAndRes is

  signal SWResetEnable    : std_logic;
  signal SWReset_N        : std_logic;
  signal MECControlReg    : std_logic_vector(31 downto 0);
  signal MECControlRegPar : std_logic;
    
  signal ResetIntCount      : std_logic_vector(4 downto 0);
  signal ResetOutCount      : std_logic_vector(4 downto 0);
  
  signal ErrorReset_N_rck   : std_logic; 
  signal SWReset_N_rck      : std_logic;
  signal WDReset_N_rck_sync : std_logic;
  signal WDReset_N_rck      : std_logic;
  signal ErrorReset_N_rck_1 : std_logic;
  signal SWReset_N_rck_1    : std_logic;
  signal WDReset_N_rck_1    : std_logic;

  signal ErrorReset_N_rck_stb : std_logic;
  signal WDReset_N_rck_stb    : std_logic;
  signal SWReset_N_rck_stb    : std_logic;
  
  signal SysReset_Int_N_rck2_1 : std_logic;
  signal SysReset_Int_N_rck2_2 : std_logic;
  signal SysReset_Int_N_rck2_3 : std_logic;
  
  signal SysReset_Int_N_1rck   : std_logic;
  signal SysReset_Int_N_2rck   : std_logic;
  
  signal ResetIntCounter_Start_N : std_logic;
  signal ResetOutCounter_Start_N : std_logic;
  
  signal ResetOutCounter_Start_N_fck     : std_logic;
  signal ResetOutCounter_Start_N_fck_del : std_logic;
    

begin

  --###
  MECControlRegister: process(RI, Clk_Int)
  begin

  if RI = '0' then
      MECControlReg     <= (others => '0');
      MECControlReg(2)  <= '1';
      MECControlReg(4)  <= '1';
      MECControlReg(16) <= '1';
      MECControlReg(18) <= '1';
      MECControlReg(20) <= '1';
      MECControlReg(21) <= '1';
      MECControlReg(23) <= '1';
      MECControlReg(24) <= '1';
      MECControlRegPar  <= '1';
      
  elsif Clk_Int'event and Clk_Int = '0' then
  
     if MECControlReg_N = '0' and Wr_Int_N = '0' then
        MECControlReg    <= D_Int_In;
        MECControlRegPar <= DPar_Int_In;
     end if;
    
   end if;
  end process;

  --###
  D_CtlSup_Out    <= MECControlReg;
  DPar_CtlSup_Out <= MECControlRegPar;

  --### Direct Outputs from MECControlRegister
  PowerDownEn   <= MECControlReg(0);
  SWResetEnable <= MECControlReg(1);
  BusTimeOut_En <= MECControlReg(2);
  AccessProtEn  <= MECControlReg(3);
  WDClkSupply   <= MECControlReg(4);
  ErrorCtrl     <= MECControlReg(14 downto 5);
  DMAEnable_Int <= MECControlReg(16);
  DMAParity_En  <= MECControlReg(17);
  DMATimeOut_En <= MECControlReg(18);

  Baudrate      <= MECControlReg(19);
  UARTClkSupply <= MECControlReg(23);
  UARTScalerReg <= MECControlReg(31 downto 24);

  --###
  ParErrCtlSup <= ParityCheck(MECControlReg,MECControlRegPar);

  --###
  SWResetReg: process
  begin  
    wait until Clk_Int'event and Clk_Int = '0';

      if (SWRReg_N = '0') and (Wr_Int_N = '0') and (SWResetEnable = '1') then
        SWReset_N <= '0';      
       elsif ResetOutCount(4) = '1' then
        SWReset_N <= '1';        
      end if;
      
  end process;

  --
  SYSRESETsamplingRising: process
  begin
    wait until Clk2_In'event and Clk2_In = '1';
    SysReset_Int_N_rck2_1 <= SysReset_In_N;
    SysReset_Int_N_rck2_2 <= SysReset_Int_N_rck2_1;
    SysReset_Int_N_rck2_3 <= SysReset_Int_N_rck2_2;
  end process;
      
  RstCK2Div    <= SysReset_Int_N_rck2_2 and not SysReset_Int_N_rck2_3;

--  ResetIntCounter_Start_N <= ErrorReset_N_rck_stb and SysReset_Int_N_2rck ;
--                             
--  
--  --###
--  ResetIntCounter: process(ResetIntCounter_Start_N, Clk_Int)
--  begin
--        
--    if ResetIntCounter_Start_N = '0' then
--      ResetIntCount <= "00000";
--   elsif Clk_Int'event and Clk_Int = '1' then
--      if ResetIntCount(4) /= '1' then
--        ResetIntCount <= ResetIntCount + 1;
--      end if;
--    end if;
--        
--  end process;
--    
--  --###
--  RI <= ResetIntCount(4);
      
  --### 
  WDResetSync: process(Clk_Int)
  begin
    if Clk_Int'event and Clk_Int = '1' then
      WDReset_N_rck_sync <= WDReset_N;
    end if;
  end process;
      
  --### Synchronize signals on CLK+ if not in reset.
  SynchSig: process(Clk_Int, SysReset_Int_N_2rck)
  begin
  
    if SysReset_Int_N_2rck = '0' then
      ErrorReset_N_rck <= '1';
      SWReset_N_rck    <= '1';
      WDReset_N_rck    <= '1';
  
    elsif Clk_Int'event and Clk_Int = '1' then
      if ResetOutCount(4) = '1' then -- ResetOutCount(4) = Reset_Out_N
        if ErrorReset_N_rck_stb = '1' then
          ErrorReset_N_rck <= ErrorReset_N;
        end if;
        if SWReset_N_rck_stb = '1' then
          SWReset_N_rck    <= SWReset_N;
        end if;
        if WDReset_N_rck_stb = '1' then
          WDReset_N_rck    <= WDReset_N_rck_sync;
        end if;
      end if;
    end if;
  end process;
      
  --###
  Reset_Cause <= "10" when ErrorReset_N_rck = '0' else -- HW error reset
                 "11" when WDReset_N_rck = '0'   else -- WD timeout reset
                 "01" when SWReset_N_rck = '0'    else -- SW triggered reset
                 "00";                                 -- System Reset
                   
  --### Generate strobes for Reset_Out_N machine.
  SamplSig: process
  begin
    wait until Clk_Int'event and Clk_Int = '1';
    ErrorReset_N_rck_1 <= ErrorReset_N_rck;
    SWReset_N_rck_1    <= SWReset_N_rck;
    WDReset_N_rck_1    <= WDReset_N_rck;
    
  end process;
  
  ErrorReset_N_rck_stb <= ErrorReset_N_rck or not(ErrorReset_N_rck_1);
  WDReset_N_rck_stb    <= WDReset_N_rck or not(WDReset_N_rck_1);
  SWReset_N_rck_stb    <= SWReset_N_rck or not(SWReset_N_rck_1);

  ResetOutCounter_Start_N <= ErrorReset_N_rck_stb and WDReset_N_rck_stb and 
                             SWReset_N_rck_stb and SysReset_Int_N_2rck ;
                             
  --
  ResetOutCounter: process (ResetOutCounter_Start_N, Clk_Int)
  begin
        
   if ResetOutCounter_Start_N ='0'  then
      ResetOutCount <= "00000";
   elsif Clk_Int'event and Clk_Int = '1' then   
     if (ResetOutCount(4) /= '1') and not is_x(ResetOutCount) then
       ResetOutCount <= ResetOutCount + 1;
     end if;
     
   end if;
        
  end process;


  --
  process(SysReset_Int_N_rck2_3, Clk_Int)
  begin
  
   if SysReset_Int_N_rck2_3 ='0'  then
    SysReset_Int_N_1rck <= '0';
    SysReset_Int_N_2rck <= '0';
    
   elsif Clk_Int'event and Clk_Int = '1' then
    SysReset_Int_N_1rck <= '1';
    SysReset_Int_N_2rck <= SysReset_Int_N_1rck;
   end if;
  end process;

  
  ResetOutDetectGen: process 
  begin
  wait until Clk_Int'event and Clk_Int = '0';    
    ResetOutCounter_Start_N_fck     <= ResetOutCounter_Start_N;
    ResetOutCounter_Start_N_fck_del <= ResetOutCounter_Start_N_fck;
        
  end process;

  ResetOutDetect <= ResetOutCounter_Start_N_fck  and not ResetOutCounter_Start_N_fck_del;


  --### 
  NoClkDetectGen: process(WDReset_N, Clk_Int)
  begin
    if WDReset_N = '0' then
      NoClkDetect <= '1';
    elsif Clk_Int'event and Clk_Int = '1' then
      NoClkDetect <= '0';
    end if;
  end process;
      
    
  --###
  RO <= ResetOutCount(4);
  
  RI <= ResetOutCount(4);
  
  Reset_N_ext <= ResetOutCount(4) and WDReset_N;
  
    
end Mini_Spec;

---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      MEC System Clock Generator
-- File name:                  \mec\source\sysclks.vhd
-- VHDL unit:                  SystemClocks
-- Purpose and functionality:  (Text)
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE; 
use IEEE.std_logic_1164.all; 

use IEEE.STD_LOGIC_UNSIGNED.all;

entity SystemClocks is
  port(
       Reset_Int_N    : in std_logic;
       Clk2_In        : in std_logic;
       WDClk_In       : in std_logic;
       WDClkSupply    : in std_logic;
       UARTClkSupply  : in std_logic;
       Baudrate       : in std_logic;
       UARTs_Quiet    : in std_logic;
       MECHalt_Int_N  : in std_logic;
       UARTScalerReg  : in std_logic_vector(7 downto 0);
       RstCK2Div      : in std_logic;      
       Clk_Int        : in std_logic;      
       Clk2_Int       : out std_logic;
       CKO            : buffer std_logic;
       WDStrobe       : out std_logic;
       UART_Clk_En    : out std_logic
      );
end SystemClocks;


architecture Mini_Spec of SystemClocks is

  signal UARTClk4            : std_logic;
  signal UARTScalerCount     : std_logic_vector(7 downto 0);
  
  signal WDcount             : std_logic_vector(3 downto 0);
  signal UARTcount           : std_logic_vector(1 downto 0);

  signal ResetWDclkDivider_1 : std_logic := '0';
  signal ResetWDclkDivider   : std_logic := '0';
  
  signal Clk_UART_Int        : std_logic;
  signal UARTScalEn          : std_logic;
  
  signal WDClk_In_fck        : std_logic;
  signal WDClk_In1_rck       : std_logic;
  signal WDClk_In2_rck       : std_logic;
  
  signal Clk_UART_Int_rck    : std_logic;
  
  signal ResetUARTCounter    : std_logic;
  signal WDcount3_del        : std_logic;

  signal CKO_N               : std_logic := '0';

begin

  --### Internal MEC clock
  Clk2_Int <= Clk2_In;
  
  CKO <= not(CKO_N);

  --### System Clock
  SystemClocks: process
  begin
    wait until Clk2_In'event and Clk2_In = '1';
    if RstCK2Div = '1' then
      CKO_N <= '0';
    else
      CKO_N <= not(CKO_N);
    end if;
  end process;
    
  --### Generate reset signal for Watchdog Clock divider
  ResetWDclkDividerGen: process(Reset_Int_N, WDClk_In)
  begin
    if Reset_Int_N = '0' then
      ResetWDclkDivider_1 <= '0';
      ResetWDclkDivider   <= '0';
    elsif WDClk_In'event and WDClk_In = '1' then
      ResetWDclkDivider_1 <= '1';
      ResetWDclkDivider   <= ResetWDclkDivider_1;
    end if;
  end process;
  
  --### Watchdog Clock divider
  WDDivideBy16: process(ResetWDclkDivider, WDClk_In)
  begin
    if ResetWDclkDivider = '0' then
      WDcount      <= "0000";
      WDcount3_del <= '0';
    elsif WDClk_In'event and WDClk_In = '1' then
      WDcount      <= WDcount + 1;
      WDcount3_del <= WDcount(3);
    end if;
  end process;


  --### WDClkMux
  WDStrobe <= (WDcount(3) and not WDcount3_del) and MECHalt_Int_N when WDClkSupply = '1' else
              MECHalt_Int_N;
            

--------- UART Clock --------

 fck_sync: process(Reset_Int_N, Clk_Int)
 begin
   if Reset_Int_N = '0' then
     WDClk_In_fck <= '0';
   elsif Clk_Int'event and Clk_Int = '0' then
     WDClk_In_fck <= WDClk_In or (UARTs_Quiet and not(MECHalt_Int_N));
   end if;
    
 end process;
  
 rck_sync: process(Reset_Int_N, Clk_Int)
 begin
   
   if Reset_Int_N = '0' then
      WDClk_In1_rck <= '0';
      WDClk_In2_rck <= '0';
   elsif Clk_Int'event and Clk_Int = '1' then
      WDClk_In1_rck <= WDClk_In_fck;
      WDClk_In2_rck <= WDClk_In1_rck;
   end if;
    
  end process;

  UARTScalEn   <=  not (UARTs_Quiet and not(MECHalt_Int_N)) when UARTClkSupply = '1' else
                   WDClk_In1_rck and not WDClk_In2_rck;

  -- MECHalt_Int_N and UARTs_Quiet are generated on ck+
  ResetUARTCounter <= not Reset_Int_N or (UARTs_Quiet and not(MECHalt_Int_N));
  
  UARTScaler: process
  begin
  wait until Clk_Int'event and Clk_Int = '1';
    if ResetUARTCounter = '1' then                         
      UARTScalerCount <= "00000000";
      UARTClk4        <= '0';
    elsif UARTScalEn = '1' then
      if UARTScalerCount = "00000000" then
        UARTScalerCount <= UARTScalerReg;
        UARTClk4        <= '1';
      else
        UARTScalerCount <= UARTScalerCount - 1;
        UARTClk4        <= '0';
      end if;
    end if;
  end process;

  
  UARTClockDivider: process
  begin
  
  wait until Clk_Int'event and Clk_Int = '1';
    if ResetUARTCounter = '1' then
      UARTcount <= "00";
    elsif UARTScalEn = '1' then
      if UARTClk4 = '1' then
        UARTcount <= UARTcount + 1;
      end if;
    end if;
  end process;
    
  --### UARTClkMux
              
  Clk_UART_Int <= UARTcount(0) when Baudrate = '1' else
                  UARTcount(1);


  rck_sync1: process(Reset_Int_N, Clk_Int)
  begin
--  wait until Clk_Int'event and Clk_Int = '1';
  if Reset_Int_N = '0' then
      Clk_UART_Int_rck <= '0';
  elsif Clk_Int'event and Clk_Int = '1' then
      Clk_UART_Int_rck <= Clk_UART_Int;
  end if;
    
  end process;

  UART_Clk_En <= Clk_UART_Int and not Clk_UART_Int_rck;

end Mini_Spec ;
---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      MEC Control And Support Functions
-- File name:                  \mec\source\supfunc.vhd
-- VHDL unit:                  MECControlAndSupportFunctions
-- Purpose and functionality:  (Text)
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE; 
use IEEE.std_logic_1164.all; 


entity MecControlAndSupportFunctions is
    port (
           D_Int_In            : in Std_Logic_Vector(31 downto 0);
           Clk2_In             : in Std_Logic;
           MECControlReg_N     : in Std_Logic;
           PwrDReg_N           : in Std_Logic;
           SWRReg_N            : in Std_Logic;
           ErrorReset_N        : in Std_Logic;
           WDReset_N           : in Std_Logic;
           SysReset_In_N       : in Std_Logic;
           Wr_Int_N            : in Std_Logic;
           WDClk_In            : in Std_Logic;
           DPar_Int_In         : in Std_Logic;
           MECHalt_Int_N       : in Std_Logic;
           UARTs_Quiet         : in Std_Logic;
           AnyInterrupt        : in Std_Logic;
           D_CtlSup_Out    : out Std_Logic_Vector(31 downto 0);
           Clk_Int         : in Std_Logic;
           Reset_Int_N     : buffer Std_Logic;
           UART_Clk_En     : out Std_Logic;
           Clk2_Int        : out Std_Logic;
           Reset_Out_N     : out Std_Logic;
           Reset_N_ext     : out Std_Logic;
           Reset_Cause     : out Std_Logic_Vector(1 downto 0);
           ResetOutDetect  : out std_logic;
           NoClkDetect     : out std_logic;
           WDStrobe        : out Std_Logic;
           Clk_Out         : buffer Std_Logic;
           PowerDown       : out Std_Logic;
           AccessProtEn    : out Std_Logic;
           ErrorCtrl       : out Std_Logic_Vector(9 downto 0);
           DMAEnable_Int   : out Std_Logic;
           DMAParity_En    : out Std_Logic;
           DMATimeOut_En   : out Std_Logic;
           DPar_CtlSup_Out : out Std_Logic;
           BusTimeOut_En   : out std_logic;
           ParErrCtlSup    : out Std_Logic
         );

end MecControlAndSupportFunctions;

architecture mini_spec OF MecControlAndSupportFunctions is
component SystemClocks 
port(
         Reset_Int_N   : in Std_Logic;
         Clk2_In       : in Std_Logic;
         WDClk_In      : in Std_Logic;
         WDClkSupply   : in Std_Logic;
         UARTClkSupply : in Std_Logic;
         Baudrate      : in Std_Logic;
         UARTs_Quiet   : in Std_Logic;
         MECHalt_Int_N : in Std_Logic;
         UARTScalerReg : in  std_logic_vector(7 downto 0);
         RstCK2Div     : in Std_Logic;
         Clk_Int       : in Std_Logic;
         Clk2_Int      : out Std_Logic;
         CKO           : buffer Std_Logic;
         WDStrobe      : out Std_Logic;
         UART_Clk_En   : out Std_Logic );
end component;

component StartUpCtlAndRes 
port (
          Clk_Int            : in Std_Logic;
          Clk2_In            : in Std_Logic;
          Wr_Int_N           : in Std_Logic;
          MECControlReg_N    : in Std_Logic;
          SWRReg_N           : in Std_Logic;
          SysReset_In_N      : in Std_Logic;
          WDReset_N          : in Std_Logic;
          ErrorReset_N       : in Std_Logic;
          DPar_Int_In        : in Std_Logic;

          RO             : out Std_Logic;
          Reset_N_ext    : out Std_Logic;
          RI             : buffer Std_Logic;
          PowerDownEn    : out Std_Logic;
          AccessProtEn   : out Std_Logic;
          WDClkSupply    : out Std_Logic;
          Baudrate       : out Std_Logic;
          UARTClkSupply  : out Std_Logic;
          ErrorCtrl      : out Std_Logic_Vector(9 downto 0);
          DMAEnable_Int  : out Std_Logic;
          DMAParity_En   : out Std_Logic;
          DMATimeOut_En  : out Std_Logic;
          DPar_CtlSup_Out: out Std_Logic;
          ParErrCtlSup   : out Std_Logic;
          Reset_Cause    : out Std_Logic_Vector(1 downto 0);

          D_Int_In       : in Std_Logic_Vector(31 downto 0);

          D_CtlSup_Out   : out Std_Logic_Vector(31 downto 0);
          UARTScalerReg  : out Std_Logic_Vector(7 downto 0);
          BusTimeOut_En  : out std_logic;
          ResetOutDetect : out std_logic;
          NoClkDetect    : out std_logic;
          RstCK2Div      : out Std_Logic
          );
end component;

component PowerDownModeCtl 
port (
          Reset_Int_N  : in Std_Logic;
          Clk_Int      : in Std_Logic;
          Wr_Int_N     : in Std_Logic;
          PwrDReg_N    : in Std_Logic;
          PowerDownEn  : in Std_Logic;
          AnyInterrupt : in Std_Logic;
          PowerDown    : out Std_Logic
         );
end component;

signal Baudrate       : Std_Logic;
signal PowerDownEn    : Std_Logic;
signal UARTClkSupply  : Std_Logic;
signal UARTScalerReg  : Std_Logic_Vector(7 downto 0);
signal WDClkSupply    : Std_Logic;
signal RstCK2Div      : Std_Logic;
signal CKO            : Std_Logic;
signal Clk2_Int_loc   : Std_Logic;

               
begin

Clk_Out <= not(CKO);
Clk2_Int <= Clk2_Int_loc;

SCK : SystemClocks 
port map (
         Reset_Int_N => Reset_Int_N,
         Clk2_In => Clk2_In,
         WDClk_In => WDClk_In,
         WDClkSupply => WDClkSupply,
         UARTClkSupply => UARTClkSupply,
         Baudrate => Baudrate,
         UARTs_Quiet => UARTs_Quiet,
         MECHalt_Int_N => MECHalt_Int_N,
         UARTScalerReg => UARTScalerReg,
         RstCK2Div  => RstCK2Div,
         Clk_Int => Clk_Int,
         Clk2_Int => Clk2_Int_loc,
         CKO => CKO,
         WDStrobe => WDStrobe,
         UART_Clk_En => UART_Clk_En
         ); 


SCR : StartUpCtlAndRes 
port map  (
          Clk_Int => Clk_Int,
          -- connexion to Clk2_Int_loc, in order to generate RstCK2Div 
          -- without skew for the clock divider
          Clk2_In => Clk2_Int_loc,
          Wr_Int_N => Wr_Int_N,
          MECControlReg_N => MECControlReg_N,
          SWRReg_N => SWRReg_N,
          SysReset_In_N => SysReset_In_N,
          WDReset_N => WDReset_N,
          ErrorReset_N => ErrorReset_N,
          DPar_Int_In => DPar_Int_In,

          RO          => Reset_Out_N,
          Reset_N_ext => Reset_N_ext,
          RI          => Reset_Int_N,
          PowerDownEn => PowerDownEn,
          AccessProtEn => AccessProtEn,
          WDClkSupply => WDClkSupply,
          Baudrate    => Baudrate,
          UARTClkSupply => UARTClkSupply,
          ErrorCtrl => ErrorCtrl,
          DMAEnable_Int => DMAEnable_Int,
          DMAParity_En => DMAParity_En,
          DMATimeOut_En => DMATimeOut_En,
          DPar_CtlSup_Out => DPar_CtlSup_Out,
          ParErrCtlSup => ParErrCtlSup,
          Reset_Cause => Reset_Cause,

          D_Int_In     => D_Int_In,

          D_CtlSup_Out => D_CtlSup_Out,
          UARTScalerReg => UARTScalerReg,
          BusTimeOut_En   => BusTimeOut_En,
          ResetOutDetect   => ResetOutDetect,
          NoClkDetect   => NoClkDetect,
          RstCK2Div    => RstCK2Div
          );

PDC : PowerDownModeCtl 
port map  (
          Reset_Int_N => Reset_Int_N,
          Clk_Int => Clk_Int,
          Wr_Int_N => Wr_Int_N,
          PwrDReg_N => PwrDReg_N,
          PowerDownEn => PowerDownEn,
          AnyInterrupt => AnyInterrupt,
          PowerDown => PowerDown
         );
end Mini_Spec;
---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      
-- File name:                  
-- VHDL unit:                  
-- Purpose and functionality:  buffer description for synthesis purpose
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;


entity LBuff is
    port (
          Clk1      : in Std_Logic;
          Clk2      : in Std_Logic;
          LBuf      : out Std_Logic
         );

end LBuff;

architecture Mini_Spec of LBuff is

begin

LBuf <= Clk1 and Clk2;

end Mini_Spec;





-----------------------------------
library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

  entity GPBuff is
     port (
          GP_In    : in Std_Logic;
          GP_Out   : out Std_Logic
         );

  end GPBuff;

  architecture Mini_Spec of GPBuff is

  begin

  GP_Out <= GP_In;

  end Mini_Spec;


---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      Bus access controller fsm
-- File name:                  \mec\source\fsmpck.vhd
-- VHDL unit:                  AccessControl_fsm
-- Purpose and functionality:  (Text)
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--
library IEEE;
use IEEE.Std_Logic_1164.all;

package pk is 
                           
type ACCSEQstate is (Idle, 
                     BHoldIn, BHold1, BHoldCPU, 
                     DMAMx1, DMAMx2, DMAMxOut, 
                     DMAWaitAS, DMAOut1, DMAOut,  
                     MxSt,
                     Mx, MxDE,
                     MLdOut, 
                     LdEcAc, LdEcAc1, 
                     LdWSNN, 
                     LdIO, 
                     LdOut, 
                     LdDMAIn1, LdDMAIn2,
                     St1, 
                     StEcAc, StEcAc1, 
                     StWSNN, StWSNN1, 
                     StOut,
                     LdR81Decod, LdR81, LdR8WSNN1, LdR82, LdR8WSNN2,   
                     LdR83, LdR8WSNN3, LdR84, LdR8WSNN4, LdR8Out, 
                     SSW1, SSW2, SSW3, SSWWSNN, SSW4, SSWWSNNWR, 
                     SSWOut);
end pk;


library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;
use MECLibrary.pk.all;

library IEEE;
use IEEE.Std_Logic_1164.all;


entity AccessControl_fsm is
        port (
              Clk_Int           : in Std_Logic;
              DPar_Error_Wr_ck1 : in Std_Logic;
              FHold_N           : in Std_Logic;
              ST_OUT            : out Std_Logic_vector (43 downto 0);
              DBEn_fck          : out Std_Logic;
              MemData_Valid_fck : out Std_Logic;
              IOWr_Out_N_fck    : out Std_Logic;
              MemWr_Out_N_fck   : out Std_Logic;
              NewCycle          : out Std_Logic;
              EndOfCycle        : out Std_Logic;
              Wr_En_N           : out Std_Logic;
              DMAMxEn           : out Std_Logic;
              Reset_Out_N       : in Std_Logic;
              DMA_Mode          : in Std_Logic;
              IO_Access         : in Std_Logic;
              Exch_Access       : in Std_Logic;
              IO_Area_Int_fck   : in Std_Logic;
                 
              WSUNOrZR          : in Std_Logic;
              WSDEUXorUNorZR    : in Std_Logic;
              
              ExchWSONE         : in Std_Logic;
              
              LdRAMWSNN_N       : in Std_Logic;
              
              StRAMWSNN_N       : in Std_Logic;
              
              SSWRdWSNN_N       : in Std_Logic;
              SSWWrWSNN_N       : in Std_Logic;
              
              LdROM8WSNN_N      : in Std_Logic;
              
              B_MECLoad         : in Std_Logic;
              B_Load            : in Std_Logic;
              B_Store           : in Std_Logic;
              B_LoadROM8        : in Std_Logic;
              B_StoreSubWord    : in Std_Logic;
              
              PROM_IO_Ext_Access : in Std_Logic;
              Ext_Access_fck    : in Std_Logic;
              CPUHalt_N         : in Std_Logic;
              BusRequest_Int    : in Std_Logic;
              MExcReqDetect     : in Std_Logic;
              DMAAS_In          : in Std_Logic;
              MExcVarFSM2       : in Std_Logic;
              BHold_Out_N       : in Std_Logic;
              DMAInPrgs_fck     : in Std_Logic;
              BusRdy_In_N_ck0   : in Std_Logic;
              Null_Int_ck0      : in Std_Logic;
              Mem_Area_Int_fck  : in Std_Logic;
              PROM_Area_Out_fck : in Std_Logic;
              Error_detect_FSM  : in Std_Logic;
              MExcVarFoo        : in Std_Logic;
              BusTimeOut        : in Std_Logic
             );
end AccessControl_fsm;

architecture Mini_Spec of AccessControl_fsm is
    
    
    signal SeqState : ACCSEQstate;


begin


----------------------------------------------------------------------------
--Access Controller
----------------------------------------------------------------------------
    AccessController: process(Reset_Out_N, Clk_Int)
    begin
        
      if Reset_Out_N = '0' then
        SeqState           <= Idle;
        IOWr_Out_N_fck     <= '1';
        MemWr_Out_N_fck    <= '1';
        Wr_En_N            <= '1';
        NewCycle           <= '0';
        EndOfCycle         <= '0';
        
        DBEn_fck           <= '0';
        MemData_Valid_fck  <= '0';
        DMAMxEn            <= '0';
        
      elsif Clk_Int'event and Clk_Int = '0' then

        IOWr_Out_N_fck     <= '1';
        MemWr_Out_N_fck    <= '1';
        Wr_En_N            <= '1';
        NewCycle           <= '0';
        EndOfCycle         <= '0';
        
        DBEn_fck           <= '0';
        MemData_Valid_fck  <= '0';
        DMAMxEn            <= '0';
        
        case SeqState is
        
         -----------------------------------------------------------------------  
          when Idle    =>
         
          if Reset_Out_N = '1' then
            
            if BHold_Out_N = '0' then
                SeqState <= BHoldIn;
                 
            elsif  B_Load = '1' then
              
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;
                   
                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;
            
                else
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';
                   
                   if DMA_Mode = '1' then
                     DMAMxEn         <= IO_Access;
                   end if;

                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;
                    
                   MemData_Valid_fck  <= not IO_Access;

                 end if;                                 

            elsif  B_Store = '1' then
            
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
                   
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;
            
                else               
                   SeqState          <= St1;
                   Wr_En_N           <= '0';
                   
                end if;

            elsif  B_LoadROM8 = '1' then
                SeqState <= LdR81Decod;

            elsif  B_StoreSubWord = '1' then
                SeqState         <= SSW1;
              
            elsif  B_MECLoad = '1' then
                SeqState          <= MLdOut;
                   
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
            
          end if;

         -----------------------------------------------------------------------  
          when BHoldIn  =>
          
            if DMA_Mode = '1' then
                SeqState <= DMAWaitAS;
                NewCycle <= '1';

            elsif not(CPUHalt_N = '1' and BusRequest_Int = '0') then
                SeqState <= BHold1;
                
            elsif CPUHalt_N = '1' and BusRequest_Int = '0' then 
                SeqState <= BHoldCPU;
                NewCycle <= '1';
               
            end if;
          
         -----------------------------------------------------------------------  
          when BHold1  =>
                          
            if DMA_Mode = '1' then
                SeqState <= DMAWaitAS;
                NewCycle <= '1';
                
            elsif CPUHalt_N = '1' and BusRequest_Int = '0' then 
                SeqState <= BHoldCPU;
                NewCycle <= '1';
                            
            end if;
            
         -----------------------------------------------------------------------  
          when BHoldCPU  =>
          
            if BHold_Out_N = '0' then
                SeqState <= BHoldIn;
                 
            elsif FHold_N = '0' then
                SeqState <= BHoldCPU;
                NewCycle <= '1';
                
            elsif B_Load = '1' then
            
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;
            
                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;
            
                else
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';
                   
                   if DMA_Mode = '1' then
                     DMAMxEn         <= IO_Access;
                   end if;

                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;
                    
                   MemData_Valid_fck <= not IO_Access;

                 end if;                                 

            elsif  B_Store = '1' then
            
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
             
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;
            
                else               
                   SeqState          <= St1;
                   Wr_En_N           <= '0';
                   
                end if;

            elsif  B_LoadROM8 = '1' then
                SeqState <= LdR81Decod;

            elsif  B_StoreSubWord = '1' then
                SeqState         <= SSW1;
              
            elsif  B_MECLoad = '1' then
                SeqState          <= MLdOut;
                   
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
          
         -----------------------------------------------------------------------  
          when DMAWaitAS  =>
          
            if MExcReqDetect = '1' then
                SeqState <= DMAMx1;
                NewCycle <= '1';
                
            elsif DMA_Mode = '0' then
                SeqState <= DMAOut1;
             
            elsif DMAAS_In = '0' then          
                SeqState <= DMAWaitAS;
                NewCycle <= '1';
             
            elsif  B_Load = '1' then
              
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;
            
                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;
            
                else
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';
                   
                   if DMA_Mode = '1' then
                     DMAMxEn          <= IO_Access;
                   end if;

                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;
                    
                   MemData_Valid_fck  <= not IO_Access;

                end if;
                
            elsif  B_Store = '1' then
            
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then           
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
            
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;
            
                else               
                   SeqState         <= St1;
                   Wr_En_N          <= '0';
                   
                end if;

            elsif  B_LoadROM8 = '1' then
                SeqState <= LdR81Decod;

            elsif  B_StoreSubWord = '1' then
                SeqState  <= SSW1;
             
            elsif  B_MECLoad = '1' then
                SeqState          <= MLdOut;
                   
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
          
         -----------------------------------------------------------------------  
          when DMAOut1 =>
          
            SeqState <= DMAOut;
            NewCycle <= CPUHalt_N and not BusRequest_Int;
                                
         -----------------------------------------------------------------------  
          when DMAOut =>
          
            if DMA_Mode = '1' then
                SeqState <= DMAWaitAS;  
                NewCycle <= '1';
                
            elsif BHold_Out_N = '0' then
                SeqState <= BHoldIn;
                
            elsif FHold_N = '0' then
                SeqState <= BHoldCPU;
                NewCycle <= '1';
                
            elsif  B_Load = '1' then 
                        
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;
                   
                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;
            
                else               
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';

                   if DMA_Mode = '1' then
                     DMAMxEn         <= IO_Access;
                   end if;

                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;

                   MemData_Valid_fck  <= not IO_Access;

                end if;
               
            elsif  B_Store = '1' then
            
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then           
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
            
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;
            
                else               
                   SeqState          <= St1;
                   Wr_En_N           <= '0';
                   
                end if;
            
            elsif  B_LoadROM8 = '1' then
                SeqState <= LdR81Decod;

            elsif  B_StoreSubWord = '1' then
                SeqState  <= SSW1;
             
            elsif  B_MECLoad = '1' then 
                SeqState          <= MLdOut;
                   
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
            
         -----------------------------------------------------------------------  
          when DMAMx1  =>
             SeqState <= DMAMx2;
             NewCycle <= '1';
             DMAMxEn  <= '1';
                              
         -----------------------------------------------------------------------  
          when DMAMx2  =>
             SeqState <= DMAMxOut;
             NewCycle <= '1';
                    
         -----------------------------------------------------------------------  
          when DMAMxOut  =>
             SeqState <= DMAOut1;
                                 
         -----------------------------------------------------------------------  
          when Mx =>
          
            if DMA_Mode = '1' then
                SeqState <= DMAWaitAS;                
                NewCycle <= '1';
               
            elsif BHold_Out_N = '0' then
                SeqState <= BHoldIn;
                
            elsif FHold_N = '0' then
                SeqState <= BHoldCPU;
                NewCycle <= '1';
                
            elsif  B_Load = '1' then
              
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;
               
                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;
            
                else
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';
                   
                   if DMA_Mode = '1' then
                     DMAMxEn         <= IO_Access;
                   end if;
                    
                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;

                   MemData_Valid_fck  <= not IO_Access;

                end if;
                                 
            elsif  B_Store = '1' then
            
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then           
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
                   
            
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;
             
                else               
                   SeqState          <= St1;
                   Wr_En_N           <= '0';
                   
                end if;

            elsif  B_LoadROM8 = '1' then
                SeqState <= LdR81Decod;

            elsif  B_StoreSubWord = '1' then
                SeqState  <= SSW1;
             
            elsif  B_MECLoad = '1' then
                SeqState          <= MLdOut;
                   
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
                              
         -----------------------------------------------------------------------  
          when MxSt =>
            SeqState   <= StOut;
            NewCycle   <= '1';
            EndOfCycle <= '1';
    
         -----------------------------------------------------------------------  
          when MLdOut =>
          
            if DMAInPrgs_fck = '1' then             
               SeqState <= LdDMAIn1;
               DMAMxEn  <= '1';
               
            elsif MExcVarFoo = '1' then
               SeqState   <= Mx;
               NewCycle   <= '1';
               EndOfCycle <= '1';
                              
            elsif DMA_Mode = '1' then
                SeqState <= DMAWaitAS;                
                NewCycle <= '1';
               
            elsif BHold_Out_N = '0' then
                SeqState <= BHoldIn;
                
            elsif FHold_N = '0' then
                SeqState <= BHoldCPU;
                NewCycle <= '1';
                
            elsif  B_Load = '1' then
              
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;
               
                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;
            
                else
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';
                   
                   if DMA_Mode = '1' then
                     DMAMxEn         <= IO_Access;
                   end if;
                    
                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;

                   MemData_Valid_fck  <= not IO_Access;

                end if;
                                 
            elsif  B_Store = '1' then
            
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then           
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
            
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;
            
                else               
                   SeqState          <= St1;
                   Wr_En_N           <= '0';
                   
                end if;

            elsif  B_LoadROM8 = '1' then
                SeqState          <= LdR81Decod;

            elsif  B_StoreSubWord = '1' then
                SeqState          <= SSW1;
               
            elsif  B_MECLoad = '1' then
                SeqState          <= MLdOut;
                   
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
                                                
         -----------------------------------------------------------------------  
          when LdOut =>
          
            if DMAInPrgs_fck = '1' then             
                SeqState   <= LdDMAIn1;
                DMAMxEn    <= not IO_Access;
                EndOfCycle <= '1';
                         
            elsif Error_detect_FSM = '1'  then
                SeqState   <= MxDE;
                EndOfCycle <= '1';
                 
                         
            elsif MExcVarFoo = '1' then
                SeqState   <= Mx;
                NewCycle   <= '1';
                EndOfCycle <= '1';
                 
            elsif BHold_Out_N = '0' then
                SeqState <= BHoldIn;
                
            elsif FHold_N = '0' then
                SeqState <= BHoldCPU;
                NewCycle <= '1';
                
            elsif  B_Load = '1' then              
            
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;
                   
                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;

                else               
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';
                  
                   if DMA_Mode = '1' then
                     DMAMxEn         <= IO_Access;
                   end if;

                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;

                   MemData_Valid_fck  <= not IO_Access;

               end if;
               
            elsif  B_Store = '1' then
            
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then           
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
                   
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;
           
                else               
                   SeqState          <= St1;
                   Wr_En_N           <= '0';
                   
                end if;
            
            elsif  B_LoadROM8 = '1' then
                SeqState          <= LdR81Decod;

            elsif  B_StoreSubWord = '1' then
                SeqState  <= SSW1;
                
            elsif  B_MECLoad = '1' then                             
                SeqState          <= MLdOut;
                   
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
                                
         -----------------------------------------------------------------------            
         when MxDE =>
          
               SeqState   <= Mx;
               NewCycle   <= '1';
               EndOfCycle <= '1';

         -----------------------------------------------------------------------  
          when LdEcAc =>
                    
               SeqState          <= LdEcAc1;
                                                               
         -----------------------------------------------------------------------  
          when LdEcAc1 =>
                    
            if  BusRdy_In_N_ck0 = '1' and BusTimeOut = '0' then                    
               SeqState          <= LdEcAc1;
               
            elsif BusTimeOut = '0' and 
                  (not((BusRdy_In_N_ck0 = '0' and WSUNOrZR = '1')) or ExchWSONE = '1') then
               SeqState          <= LdWSNN;
               
            else
               SeqState          <= LdOut;
               EndOfCycle        <= '1';

               if DMA_Mode = '0' then
                 NewCycle   <= '1';
               end if;

               MemData_Valid_fck  <= not IO_Access;

            end if;
                                            
          -----------------------------------------------------------------------
          when LdWSNN =>
          
            if BusTimeOut = '0' and 
                   (not((Exch_Access = '1' and BusRdy_In_N_ck0 = '0' and WSUNOrZR = '1') or
                        (Ext_Access_fck = '1' and BusRdy_In_N_ck0 = '0') or
                        ((Mem_Area_Int_fck = '1' or PROM_Area_Out_fck = '1') and WSUNOrZR = '1') or
                        (IO_Area_Int_fck = '1' and BusRdy_In_N_ck0 = '0' and WSUNOrZR = '1')) 
                ) then          
               SeqState          <= LdWSNN;
                              
                                               
            elsif IO_Access = '1' then           
               SeqState          <= LdIO;

               if DMA_Mode = '0' then
                 EndOfCycle   <= '1';
               end if;
               
               MemData_Valid_fck  <= '1';
               
            else            
               SeqState          <= LdOut;
               EndOfCycle        <= '1';
                   
               if DMA_Mode = '0' then
                 NewCycle   <= '1';
               end if;
               
               MemData_Valid_fck  <= not IO_Access;

           end if;                                 
                                                                                                 
          -----------------------------------------------------------------------
          when LdIO =>
          
           if Error_detect_FSM = '1' and DMA_Mode = '0' then
              SeqState   <= MxDE;
              EndOfCycle <= '1';
                 
           elsif MExcVarFoo = '1' and DMA_Mode = '0' then
              SeqState   <= Mx;
              NewCycle   <= '1';
              EndOfCycle <= '1';
                 
           else
             SeqState          <= LdOut;
             EndOfCycle        <= not IO_Access;
                   
             if DMA_Mode = '1'  then 
               DMAMxEn <= '1';
             end if;
            
             if DMA_Mode = '0' then
               NewCycle   <= '1';
             end if;
             
             MemData_Valid_fck  <= not IO_Access;

           end if;

         -----------------------------------------------------------------------  
          when LdDMAIn1 =>
                SeqState   <= LdDMAIn2;
                NewCycle   <= '1';
                EndOfCycle <= '1';
          
         -----------------------------------------------------------------------  
          when LdDMAIn2 =>
          
            if DMAAS_In = '0' and DMA_Mode = '1' then           
                SeqState <= DMAWaitAS;
                NewCycle <= '1';
             
            elsif DMA_Mode = '0'  then 
                SeqState <= DMAOut1;
                                                                          
            elsif  B_Load = '1' then              
            
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;
                   
                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;

                else               
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';
                   
                   if DMA_Mode = '1' then
                     DMAMxEn          <= IO_Access;
                   end if;

                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;

                   MemData_Valid_fck  <= not IO_Access;

                end if;
               
            elsif  B_Store = '1' then
            
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then           
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
            
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;
            
                else               
                   SeqState          <= St1;
                   Wr_En_N           <= '0';
                   
                end if;
            
            elsif  B_LoadROM8 = '1' then
                SeqState          <= LdR81Decod;

            elsif  B_StoreSubWord = '1' then
                SeqState          <= SSW1;
                    
            elsif  B_MECLoad = '1' then                             
                SeqState          <= MLdOut;
                   
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
                                                
         -----------------------------------------------------------------------  
          when St1 =>
          
            if DPar_Error_Wr_ck1 = '1' then
                 SeqState   <= MxSt;
                 
            elsif Null_Int_ck0 = '0' then 
            
              if MExcVarFSM2 = '1' and DMA_Mode = '0' then
                 SeqState   <= MxSt;
              else
                SeqState    <= StOut;
                NewCycle    <= '1';
                EndOfCycle  <= '1';
                                                
                if DMA_Mode = '1' then 
                   DMAMxEn  <= '1';
                end if;
                
              end if;

            else

             if FHold_N = '0' then
                SeqState <= BHoldCPU;
                NewCycle <= '1';
                
             elsif B_Load = '1' then
              
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;
               
                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;
            
                else
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';
                    
                   if DMA_Mode = '1' then
                     DMAMxEn         <= IO_Access;
                   end if;
                    
                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;

                   MemData_Valid_fck  <= not IO_Access;

                end if;
                                 
             elsif  B_Store = '1' then
            
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then           
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
                   
            
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;
            
                else               
                   SeqState          <= St1;
                   Wr_En_N           <= '0';
                   
                end if;

             elsif  B_LoadROM8 = '1' then
                SeqState          <= LdR81Decod;

             elsif  B_StoreSubWord = '1' then
                SeqState <= SSW1;
              
             elsif  B_MECLoad = '1' then
                SeqState          <= MLdOut;
                   
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
            
          end if;

         -----------------------------------------------------------------------
         when StEcAc =>
          
            if Null_Int_ck0 = '0' then 
              SeqState          <= StEcAc1;               
                                 
            else

             if FHold_N = '0' then
                SeqState <= BHoldCPU;
                NewCycle <= '1';
                
             elsif  B_Load = '1' then
              
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;
               
                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;
            
                else
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';
                    
                   if DMA_Mode = '1' then
                     DMAMxEn         <= IO_Access;
                   end if;
                    
                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;

                   MemData_Valid_fck  <= not IO_Access;

                end if;
                                 
             elsif  B_Store = '1' then
            
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then           
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
                   
            
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;
            
                else               
                   SeqState          <= St1;
                   Wr_En_N           <= '0';
                   
                end if;

             elsif  B_LoadROM8 = '1' then
                SeqState          <= LdR81Decod;

             elsif  B_StoreSubWord = '1' then
                SeqState <= SSW1;
              
             elsif  B_MECLoad = '1' then
                SeqState          <= MLdOut;
                   
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
            
          end if;

         -----------------------------------------------------------------------  
          when StEcAc1 =>
                    
            if  BusRdy_In_N_ck0 = '1' and BusTimeOut = '0' then                    
               SeqState          <= StEcAc1;
               
            elsif BusTimeOut = '0' and
                  (not((BusRdy_In_N_ck0 = '0' and WSUNOrZR = '1')) or ExchWSONE = '1') then
               SeqState          <= StWSNN1;
               IOWr_Out_N_fck    <= '0';
               Wr_En_N           <= '0';
               
            else
               SeqState          <= St1;
               Wr_En_N           <= '0';
               
                   
            end if;

         -----------------------------------------------------------------------  
          when StWSNN =>
            
           if Null_Int_ck0 = '0' then 
             if not((Ext_Access_fck = '1' and BusRdy_In_N_ck0 = '0') or
                     (IO_Area_Int_fck = '1' and BusRdy_In_N_ck0 = '0' and WSUNOrZR = '1') or
                     ((Mem_Area_Int_fck = '1' or PROM_Area_Out_fck = '1') and WSUNOrZR = '1')) then 
                 SeqState          <= StWSNN1;
                 IOWr_Out_N_fck    <= '0';
                 MemWr_Out_N_fck   <= '0';
                 Wr_En_N           <= '0';
               
              else          
                 SeqState          <= St1;
                 Wr_En_N           <= '0';
                   
              end if;
                                                                                                 
            else

             if FHold_N = '0' then
                SeqState <= BHoldCPU;
                NewCycle <= '1';
                
             elsif  B_Load = '1' then
              
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;
               
                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;
            
                else
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';
                    
                   if DMA_Mode = '1' then
                     DMAMxEn         <= IO_Access;
                   end if;
                    
                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;

                   MemData_Valid_fck  <= not IO_Access;

                end if;
                                 
             elsif  B_Store = '1' then
            
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then           
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
                   
            
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;
            
                else               
                   SeqState          <= St1;
                   Wr_En_N           <= '0';
                   
                end if;

             elsif  B_LoadROM8 = '1' then
                SeqState          <= LdR81Decod;

             elsif  B_StoreSubWord = '1' then
                SeqState <= SSW1;
              
             elsif  B_MECLoad = '1' then
                SeqState          <= MLdOut;
                   
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
            
          end if;

         -----------------------------------------------------------------------  
          when StWSNN1 =>
            
            if BusTimeOut = '0' and 
               not((Exch_Access = '1' and BusRdy_In_N_ck0 = '0' and WSUNOrZR = '1') or
                   (Ext_Access_fck = '1' and BusRdy_In_N_ck0 = '0') or
                   (IO_Area_Int_fck = '1' and BusRdy_In_N_ck0 = '0' and WSUNOrZR = '1') or
                   ((Mem_Area_Int_fck = '1' or PROM_Area_Out_fck = '1') and WSUNOrZR = '1')) then 
               SeqState          <= StWSNN1;
               IOWr_Out_N_fck    <= '0';
               MemWr_Out_N_fck   <= '0';
               Wr_En_N           <= '0';
               
            else          
               SeqState          <= St1;
               Wr_En_N           <= '0';
                   
            end if;
                                                                                                 
         -----------------------------------------------------------------------  
          when StOut =>
          
            if DMAInPrgs_fck = '1' and DMAAS_In = '0' and DMA_Mode = '1' then           
                SeqState <= DMAWaitAS;
                NewCycle <= '1';
             
            elsif DMAInPrgs_fck = '1' and DMA_Mode = '0'  then 
                SeqState <= DMAOut1;
                                                                                                              
            elsif BHold_Out_N = '0' then
                SeqState <= BHoldIn;
                
            elsif FHold_N = '0' then
                SeqState <= BHoldCPU;
                NewCycle <= '1';
                
            elsif  B_Load = '1' then
              
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;

                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;
            
                else
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';
                   
                   if DMA_Mode = '1' then
                     DMAMxEn         <= IO_Access;
                   end if;
                    
                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;

                   MemData_Valid_fck  <= not IO_Access;

                end if;
                                 
            elsif  B_Store = '1' then
            
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then           
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
            
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;

                else               
                   SeqState          <= St1;
                   Wr_En_N           <= '0';
                      
                end if;
                   
            elsif  B_LoadROM8 = '1' then
                SeqState          <= LdR81Decod;

            elsif  B_StoreSubWord = '1' then
                SeqState          <= SSW1;
             
            elsif  B_MECLoad = '1' then
                SeqState          <= MLdOut;
                   
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
                                                                                                  
         -----------------------------------------------------------------------
          when LdR81Decod =>
           if Ext_Access_fck = '1' or
              not(Ext_Access_fck = '0' and WSUNOrZR = '1')  then
               SeqState   <= LdR8WSNN1;
           else
               SeqState   <= LdR81;
           end if;
                                 
         -----------------------------------------------------------------------
          when LdR81 =>
           if LdROM8WSNN_N = '0' then
               SeqState   <= LdR8WSNN2;
           else
               SeqState   <= LdR82;
           end if;
                                 
         -----------------------------------------------------------------------
         when LdR8WSNN1 =>
           if BusTimeOut = '0' and 
              not((Ext_Access_fck = '1' and BusRdy_In_N_ck0 = '0') or
                  (Ext_Access_fck = '0' and WSUNOrZR = '1'))  then
               SeqState   <= LdR8WSNN1;
           else
               SeqState   <= LdR81;
           end if;
                                 
         -----------------------------------------------------------------------  
          when LdR82 =>
           if LdROM8WSNN_N = '0' then
               SeqState    <= LdR8WSNN3;
           else
               SeqState    <= LdR83;
           end if;
                                                                 
         -----------------------------------------------------------------------  
          when LdR8WSNN2 =>
           if BusTimeOut = '0' and 
              not((Ext_Access_fck = '1' and BusRdy_In_N_ck0 = '0') or
                  (Ext_Access_fck = '0' and WSDEUXorUNorZR = '1'))  then
               SeqState    <= LdR8WSNN2;
           else
               SeqState    <= LdR82;
           end if;
                                                                 
         -----------------------------------------------------------------------  
          when LdR83 =>
           if LdROM8WSNN_N = '0' then
               SeqState    <= LdR8WSNN4;
           else
               SeqState    <= LdR84;
               EndOfCycle  <= '1';
           end if;
                                 
         -----------------------------------------------------------------------  
          when LdR8WSNN3 =>
           if BusTimeOut = '0' and
              not((Ext_Access_fck = '1' and BusRdy_In_N_ck0 = '0') or
                  (Ext_Access_fck = '0' and WSDEUXorUNorZR = '1'))  then
               SeqState   <= LdR8WSNN3;
           else
               SeqState   <= LdR83;
           end if;
                                                                 
         -----------------------------------------------------------------------  
          when LdR84 =>
           SeqState     <= LdR8Out;
               
           if DMA_Mode = '0' then
             NewCycle   <= '1';
           end if;
               
         -----------------------------------------------------------------------  
          when LdR8WSNN4 =>
           if BusTimeOut = '0' and
              not((Ext_Access_fck = '1' and BusRdy_In_N_ck0 = '0') or
                  (Ext_Access_fck = '0' and WSDEUXorUNorZR = '1') )  then
               SeqState          <= LdR8WSNN4;
               
           else
               SeqState          <= LdR84;
               EndOfCycle        <= '1';
               
           end if;
                                                                 
         -----------------------------------------------------------------------  
          when LdR8Out =>
            if DMAInPrgs_fck = '1' then             
               SeqState <= LdDMAIn1;
               DMAMxEn  <= '1';
                                   
            elsif MExcVarFoo = '1' then
               SeqState   <= Mx;
               NewCycle   <= '1';
               EndOfCycle <= '1';
                              
            elsif BHold_Out_N = '0' then
                SeqState <= BHoldIn;
                
            elsif FHold_N = '0' then
                SeqState <= BHoldCPU;
                NewCycle <= '1';
                
            elsif  B_Load = '1' then
              
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;
               
                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;
            
                else
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';
                   
                   if DMA_Mode = '1' then
                     DMAMxEn         <= IO_Access;
                   end if;
                   
                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;

                   MemData_Valid_fck  <= not IO_Access;

                end if;
                                 
            elsif  B_Store = '1' then
                               
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then           
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
                   
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;
            
                else               
                   SeqState          <= St1;
                   Wr_En_N           <= '0';
                   
                end if;
                   
            elsif  B_LoadROM8 = '1' then
                SeqState          <= LdR81Decod;
                
            elsif  B_StoreSubWord = '1' then
                SeqState          <= SSW1;
              
            elsif  B_MECLoad = '1' then
                SeqState          <= MLdOut;
                
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
                                                          
         -----------------------------------------------------------------------  
          when SSW1 =>
          
            if Null_Int_ck0 = '0' then 
             SeqState          <= SSW2;
            
            else

             if FHold_N = '0' then
                SeqState <= BHoldCPU;
                NewCycle <= '1';
                
             elsif  B_Load = '1' then
              
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;
               
                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;
            
                else
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';
                    
                   if DMA_Mode = '1' then
                     DMAMxEn         <= IO_Access;
                   end if;
                    
                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;

                   MemData_Valid_fck  <= not IO_Access;

                end if;
                                 
             elsif  B_Store = '1' then
            
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then           
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
                   
            
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;
            
                else               
                   SeqState          <= St1;
                   Wr_En_N           <= '0';
                   
                end if;

             elsif  B_LoadROM8 = '1' then
                SeqState          <= LdR81Decod;

             elsif  B_StoreSubWord = '1' then
                SeqState <= SSW1;
              
             elsif  B_MECLoad = '1' then
                SeqState          <= MLdOut;
                   
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
            
          end if;
            
         -----------------------------------------------------------------------  
          when SSW2 =>
           if (SSWRdWSNN_N = '0' or Ext_Access_fck = '1') and DMA_Mode = '0' then          
             SeqState         <= SSWWSNN;
           else
             SeqState           <= SSW3;
             MemData_Valid_fck  <= not DMA_Mode;
           end if;
                                
         -----------------------------------------------------------------------  
           when SSWWSNN =>
            if BusTimeOut = '0' and DMA_Mode = '0' and 
               not((Ext_Access_fck = '1' and BusRdy_In_N_ck0 = '0') or
                   (Mem_Area_Int_fck = '1' and WSUNOrZR = '1'))  then          
               SeqState          <= SSWWSNN;
                
            else
               SeqState           <= SSW3;
               MemData_Valid_fck  <= not DMA_Mode;
                                 
            end if;
                                
         -----------------------------------------------------------------------  
          when SSW3 =>
            if (SSWWrWSNN_N = '0' or Ext_Access_fck = '1') and DMA_Mode = '0' then          
               SeqState          <= SSWWSNNWR;
               MemWr_Out_N_fck   <= DMA_Mode;
               Wr_En_N           <= DMA_Mode;
               DBEn_fck          <= not DMA_Mode;
            else
               SeqState          <= SSW4;
               Wr_En_N           <= DMA_Mode;
               DBEn_fck          <= not DMA_Mode;
            end if;
                                
         -----------------------------------------------------------------------  
           when SSWWSNNWR =>
            if BusTimeOut = '0' and DMA_Mode = '0' and 
               not((Ext_Access_fck = '1' and BusRdy_In_N_ck0 = '0') or
                   (Mem_Area_Int_fck = '1' and WSUNOrZR = '1'))  then
               SeqState          <= SSWWSNNWR;
               MemWr_Out_N_fck   <= DMA_Mode;
               Wr_En_N           <= DMA_Mode;
               DBEn_fck          <= not DMA_Mode;
               
            else
               SeqState          <= SSW4;                                 
               Wr_En_N           <= DMA_Mode;
               DBEn_fck          <= not DMA_Mode;
                                 
            end if;
                                                                
         -----------------------------------------------------------------------  
           when SSW4 =>
            if MExcVarFoo = '1' and DMA_Mode = '0' then 
               SeqState   <= MxSt;
            else
              SeqState   <= SSWOut;
              NewCycle   <= '1';
              EndOfCycle <= '1';
               
              if DMA_Mode = '1'  then 
                 DMAMxEn <= '1';
              end if;
            end if;
                                 
         -----------------------------------------------------------------------  
          when SSWOut =>
            if DMA_Mode = '1' then
                SeqState <= DMAWaitAS;                
                NewCycle <= '1';
               
            elsif BHold_Out_N = '0' then
                SeqState <= BHoldIn;
                
            elsif FHold_N = '0' then
                SeqState <= BHoldCPU;
                NewCycle <= '1';
               
            elsif  B_Load = '1' then
              
                if PROM_IO_Ext_Access = '1' or LdRAMWSNN_N = '0' then
                   SeqState          <= LdWSNN;
               
                elsif Exch_Access = '1' then
                   SeqState          <= LdEcAc;
            
                else
                   SeqState          <= LdOut;
                   EndOfCycle        <= '1';
                   
                   if DMA_Mode = '1' then
                     DMAMxEn           <= IO_Access;
                   end if;
                   
                   if DMA_Mode = '0' then
                     NewCycle   <= '1';
                   end if;

                   MemData_Valid_fck  <= not IO_Access;

                end if;
                                 
            elsif  B_Store = '1' then
                               
                if PROM_IO_Ext_Access = '1' or StRAMWSNN_N = '0' then           
                   SeqState          <= StWSNN;
                   IOWr_Out_N_fck    <= '0';
                   MemWr_Out_N_fck   <= '0';
                   Wr_En_N           <= PROM_IO_Ext_Access;
            
                elsif Exch_Access = '1' then
                   SeqState          <= StEcAc;
            
                else               
                   SeqState          <= St1;
                   Wr_En_N           <= '0';
                   
                end if;
                   
            elsif  B_LoadROM8 = '1' then
                SeqState          <= LdR81Decod;

            elsif  B_StoreSubWord = '1' then
                SeqState <= SSW1;
            
            elsif  B_MECLoad = '1' then
                SeqState          <= MLdOut;
                   
                if DMA_Mode = '0' then
                  NewCycle   <= '1';
                end if;

            end if;
            
         ---------------------------------------------
          when others  => NULL;
          
        end case;
        
      end if;
      
    end process;
  
            

process(SeqState) 
begin

case SeqState is
when Idle       => ST_OUT <= "10000000000000000000000000000000000000000000";
when BHoldIn    => ST_OUT <= "01000000000000000000000000000000000000000000";
when BHold1     => ST_OUT <= "00100000000000000000000000000000000000000000";
when BHoldCPU   => ST_OUT <= "00010000000000000000000000000000000000000000";
when DMAMx1     => ST_OUT <= "00001000000000000000000000000000000000000000";
when DMAMx2     => ST_OUT <= "00000100000000000000000000000000000000000000";
when DMAMxOut   => ST_OUT <= "00000010000000000000000000000000000000000000";
when DMAWaitAS  => ST_OUT <= "00000001000000000000000000000000000000000000";
when DMAOut1    => ST_OUT <= "00000000100000000000000000000000000000000000";
when DMAOut     => ST_OUT <= "00000000010000000000000000000000000000000000";
when MxSt       => ST_OUT <= "00000000001000000000000000000000000000000000";
when Mx         => ST_OUT <= "00000000000100000000000000000000000000000000";
when MxDE       => ST_OUT <= "00000000000010000000000000000000000000000000";
when MLdOut     => ST_OUT <= "00000000000001000000000000000000000000000000";
when LdEcAc     => ST_OUT <= "00000000000000100000000000000000000000000000";
when LdEcAc1    => ST_OUT <= "00000000000000010000000000000000000000000000";
when LdWSNN     => ST_OUT <= "00000000000000001000000000000000000000000000";
when LdIO       => ST_OUT <= "00000000000000000100000000000000000000000000";
when LdOut      => ST_OUT <= "00000000000000000010000000000000000000000000";
when LdDMAIn1   => ST_OUT <= "00000000000000000001000000000000000000000000";
when LdDMAIn2   => ST_OUT <= "00000000000000000000100000000000000000000000";
when St1        => ST_OUT <= "00000000000000000000010000000000000000000000";
when StEcAc     => ST_OUT <= "00000000000000000000001000000000000000000000";
when StEcAc1    => ST_OUT <= "00000000000000000000000100000000000000000000";
when StWSNN     => ST_OUT <= "00000000000000000000000010000000000000000000";
when StWSNN1    => ST_OUT <= "00000000000000000000000001000000000000000000";
when StOut      => ST_OUT <= "00000000000000000000000000100000000000000000";
when LdR81Decod => ST_OUT <= "00000000000000000000000000010000000000000000";
when LdR81      => ST_OUT <= "00000000000000000000000000001000000000000000";
when LdR8WSNN1  => ST_OUT <= "00000000000000000000000000000100000000000000";
when LdR82      => ST_OUT <= "00000000000000000000000000000010000000000000";
when LdR8WSNN2  => ST_OUT <= "00000000000000000000000000000001000000000000";
when LdR83      => ST_OUT <= "00000000000000000000000000000000100000000000";
when LdR8WSNN3  => ST_OUT <= "00000000000000000000000000000000010000000000";
when LdR84      => ST_OUT <= "00000000000000000000000000000000001000000000";
when LdR8WSNN4  => ST_OUT <= "00000000000000000000000000000000000100000000";
when LdR8Out    => ST_OUT <= "00000000000000000000000000000000000010000000";
when SSW1       => ST_OUT <= "00000000000000000000000000000000000001000000";
when SSW2       => ST_OUT <= "00000000000000000000000000000000000000100000";
when SSW3       => ST_OUT <= "00000000000000000000000000000000000000010000";
when SSWWSNN    => ST_OUT <= "00000000000000000000000000000000000000001000";
when SSW4       => ST_OUT <= "00000000000000000000000000000000000000000100";
when SSWWSNNWR  => ST_OUT <= "00000000000000000000000000000000000000000010";
when SSWOut     => ST_OUT <= "00000000000000000000000000000000000000000001";
               
end case;

end process;
                                    

end Mini_Spec;

---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      Bus access controller
-- File name:                  \mec\source\accctrl.vhd
-- VHDL unit:                  AccessControl
-- Purpose and functionality:  access controller
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;
use MECLibrary.pk.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

  entity CtrlBuff is
     port (
          D_In    : in Std_Logic;
          D_Out   : out Std_Logic
         );

  end CtrlBuff;

  architecture Mini_Spec of CtrlBuff is

  begin

  D_Out <= D_In;

  end Mini_Spec;


library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;
use MECLibrary.pk.all;

library IEEE;
use IEEE.Std_Logic_1164.all;


  entity ABuff is
     port (
          A_In    : in Std_Logic_Vector(7 downto 0);
          A_Out   : buffer Std_Logic_Vector(7 downto 0)
         );

  end ABuff;
  
  architecture Mini_Spec of ABuff is

  begin

  A_Out <= A_In;

  end Mini_Spec;
  
  

library IEEE;
use IEEE.std_logic_1164.all;


  entity AddBuff is
     port (
          D_In    : in Std_Logic_Vector(31 downto 0);
          D_Out   : out Std_Logic_Vector(31 downto 0)
         );

  end AddBuff;

  architecture Mini_Spec of AddBuff is

  begin

  D_Out <= not D_In;

  end Mini_Spec;


library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;
use MECLibrary.pk.all;

library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_unsigned."-";


entity AccessControl is
        port (
              Clk_aux           : in Std_Logic;
              Clk_Int           : in Std_Logic;
              Clk2_Int          : in Std_Logic;
              ParityEnable      : in Std_Logic;
              MHold_Out_ck1_N   : in Std_Logic;
              LDSTO_Int_In      : in Std_Logic;
              
              DMAParity_En      : in Std_Logic;
              
              MECRegSel_N       : in Std_Logic;
              BAError           : in Std_Logic;              
              BusTimeOut_En     : in Std_Logic;              
              RdRst             : in Std_Logic;
              BusRdy_In_loc_N   : in Std_Logic;
              Reset_Int_N       : in Std_Logic;
              Rd_In             : in Std_Logic;
              Rd_Int_In         : in Std_Logic;
              DPar_Int_In       : in Std_Logic;
              PROM8_In_N        : in Std_Logic;
              WSConfigReg_N     : in Std_Logic;
              Reset_Out_N       : in Std_Logic;
              Error_detect_FSM  : in Std_Logic;
              CError_fck        : in Std_Logic;
              NCError_fck       : in Std_Logic;
              DPar_Error_fck    : in Std_Logic;
              DPar_Error_Wr_ck1 : in Std_Logic;
                                          
              BusRequest_Int    : in Std_Logic;
              DMA_Mode          : in Std_Logic;
              DMAAS_In          : in Std_Logic;
              A_In              : in Std_Logic_Vector(31 downto 0);
              D_Int_In          : in Std_Logic_Vector(31 downto 0);
              Size_In           : in Std_Logic_Vector(1 downto 0);
              Size_Int_In       : in Std_Logic_Vector(1 downto 0);
              BusErr_In_N       : in Std_Logic;
              Mexc_Int          : in Std_Logic;
              MexcRequest       : in Std_Logic;
              Write_Inhibit     : in Std_Logic;
              INull_In          : in Std_Logic;  
              ExtHold_In_N      : in Std_Logic;
              ExtCCV_In         : in Std_Logic;
              BHold_Out_N       : in Std_Logic;
              CPUHalt_N         : in Std_Logic;

              StrobeGate     : Out Std_Logic;
              SFSRTmp_UD     : Out Std_Logic;
              DPar_Error_Rst : Out Std_Logic;
              ALE_Out_N      : Out Std_Logic;
              MemBEn_Out_N   : Out Std_Logic;
              RAMBEn_Out_N   : Out Std_Logic;
              ROMBEn_Out_N   : Out Std_Logic;
              DDir_Out       : Out Std_Logic;
              MemWr_Out_N    : Out Std_Logic;
              CBWr_Out_N     : Out Std_Logic;
              OE_Out_N       : Out Std_Logic;
              MHold_Out_N_WOEE  : Out Std_Logic;
              MDS_Out_N      : Out Std_Logic;
              IOWr_Out_N     : Out Std_Logic;
              IOBEn_Out_N    : Out Std_Logic;
              DOE_Out_N      : Out Std_Logic;
              AOE_Out_N      : Out Std_Logic;
              COE_Out_N      : Out Std_Logic;
              ParErrWSReg    : Out Std_Logic;
              Wr_Int_N       : buffer Std_Logic;
              DBEn           : Out Std_Logic;
              CBEn           : Out Std_Logic;
              MemData_Valid  : Out Std_Logic;
              MemData_Valid_Ld  : Out Std_Logic;
              Null_Int_rck      : Out Std_Logic;
              DMAInPrgs         : Out Std_Logic;
              NewCycle          : Out Std_Logic;
              DRdy_Out_N     : Out Std_Logic;
              ByteSel        : Out Std_Logic_Vector(3 downto 0);
              IUDataLE       : Out Std_Logic;
              MemDataLE      : Out Std_Logic;
              ForceNonMemDataLE    : Out Std_Logic;
              BA_Out               : Out Std_Logic_Vector(1 downto 0);
              StoreByte_Reg_Read_N : Out Std_Logic;
              CorrectedData_Reg_N  : Out Std_Logic;
              ROM8Reg_N      : Out Std_Logic;
              ROM8Reg_LE     : Out Std_Logic;
              SysBus_Error   : Out Std_Logic;
              BusTimeOut     : buffer Std_Logic;
              Mexc_Out_N     : Out Std_Logic;
              DParIOEn       : Out Std_Logic;
              A_to_AD        : out Std_Logic_Vector(31 downto 0);
              A_to_ML        : out Std_Logic_Vector(31 downto 0);
              DoNotRdUART    : Out Std_Logic;
              LastCycle      : Out Std_Logic
             );
end AccessControl;

architecture Mini_Spec of AccessControl is

component LBuff
    port (
          Clk1      : in Std_Logic;
          Clk2      : in Std_Logic;
          LBuf      : out Std_Logic
         );
end component;


component CtrlBuff
    port (
          D_In  : in Std_Logic;
          D_Out : out Std_Logic
         );
end component;


component ABuff
    port (
          A_In  : in Std_Logic_Vector(7 downto 0);
          A_Out : buffer Std_Logic_Vector(7 downto 0)
         );
end component;


component AddBuff
    port (
          D_In  : in Std_Logic_Vector(31 downto 0);
          D_Out : out Std_Logic_Vector(31 downto 0)
         );
end component;


component AccessControl_fsm
        port (
              Clk_Int           : in Std_Logic;
              DPar_Error_Wr_ck1 : in Std_Logic;
              FHold_N           : in Std_Logic;
              ST_OUT            : out Std_Logic_vector (43 downto 0);
              DBEn_fck          : out Std_Logic;
              MemData_Valid_fck : out Std_Logic;
              IOWr_Out_N_fck    : out Std_Logic;
              MemWr_Out_N_fck   : out Std_Logic;
              NewCycle          : out Std_Logic;
              EndOfCycle        : out Std_Logic;
              Wr_En_N           : out Std_Logic;
              DMAMxEn           : out Std_Logic;
              Reset_Out_N       : in Std_Logic;
              DMA_Mode          : in Std_Logic;
              IO_Access         : in Std_Logic;
              Exch_Access       : in Std_Logic;
              IO_Area_Int_fck   : in Std_Logic;
                                     
              WSUNOrZR          : in Std_Logic;
              WSDEUXorUNorZR    : in Std_Logic;

              ExchWSONE         : in Std_Logic;
        
              LdRAMWSNN_N       : in Std_Logic;
              
              StRAMWSNN_N       : in Std_Logic;
              
              SSWRdWSNN_N       : in Std_Logic;
              SSWWrWSNN_N       : in Std_Logic;
              
              LdROM8WSNN_N      : in Std_Logic;
              
              B_MECLoad         : in Std_Logic;
              B_Load            : in Std_Logic;
              B_Store           : in Std_Logic;
              B_LoadROM8        : in Std_Logic;
              B_StoreSubWord    : in Std_Logic;
              
              PROM_IO_Ext_Access : in Std_Logic;
              Ext_Access_fck    : in Std_Logic;
              CPUHalt_N         : in Std_Logic;
              BusRequest_Int    : in Std_Logic;
              MExcReqDetect     : in Std_Logic;
              DMAAS_In          : in Std_Logic;
              MExcVarFSM2       : in Std_Logic;
              BHold_Out_N       : in Std_Logic;
              DMAInPrgs_fck     : in Std_Logic;
              BusRdy_In_N_ck0   : in Std_Logic;
              Null_Int_ck0      : in Std_Logic;
              Mem_Area_Int_fck  : in Std_Logic;
              PROM_Area_Out_fck : in Std_Logic;
              Error_detect_FSM  : in Std_Logic;
              MExcVarFoo        : in Std_Logic;
              BusTimeOut        : in Std_Logic
             );
end component;


    signal A_Loc_Invert   : Std_Logic_Vector(31 downto 0);

    type BusAccessType is (Idle,MECLoad,MECStore,Load,Store,LoadROM8,
                           StoreSubWord);

    type mdsstate is (Idle, WaitMDS_Exch, WaitMDS, MDSis0);

    signal WaitStateConfigReg   : Std_Logic_Vector(31 downto 0);
    signal WaitStateConfigRegPar: Std_Logic;
    signal BusAccessEnum        : BusAccessType;
    signal BusAccessEnum_c      : BusAccessType;
    
    signal Reset_Out_N_del_rck  : Std_Logic; 
    signal CBEnForDParIOEn      : Std_Logic;
     
    signal CError_fck_fck2      : Std_Logic; 
    
    signal Rst_IU_MemBEnBase_N  : Std_Logic; 
    signal NoBenDueToMexc_N     : Std_Logic; 
    
    signal DOE_Int_N_fck        : Std_Logic; 
    signal DOE_Int_N_rck        : Std_Logic;
    signal PartOfDOE_N          : Std_Logic;
     
    signal BusTOEnable          : Std_Logic;
    signal BusTOEnable_loc      : Std_Logic;
    
    signal BusTimeOut_fck       : Std_Logic;
    signal MemData_Valid_Ld_qual: Std_Logic;
    
    signal DMAAS_In_buff        : Std_Logic;
      
    signal MExcVarFoo           : Std_Logic;
    signal MexcVarFSM1          : Std_Logic;
    signal MexcVarFSM2          : Std_Logic;
    
    signal MexcVar_loc          : Std_Logic;
    
    signal S_LdOut_rck            : Std_Logic;
    signal S_LdR84_rck            : Std_Logic;
    signal S_LdR84_fck2           : Std_Logic;
    signal S_LdR8Out_rck          : Std_Logic;
    signal S_LdIO_N_rck           : Std_Logic;
    signal S_MxDE_rck             : Std_Logic;
    signal S_LdDMAIn1_rck         : Std_Logic;
    signal S_BHoldCPU_rck         : Std_Logic;
   
    signal WriteAccess_N          : Std_Logic;
    signal WriteAccess_N_fck      : Std_Logic;
    
    signal CErrorMask             : Std_Logic;
    signal CErrorMask_rck         : Std_Logic;
    signal CErrorDuringLd_fck     : Std_Logic;
    signal CErrorDuringLd_fck_rck  : Std_Logic;
    signal CErrorDuringLd_dfck     : Std_Logic;
    signal CErrorDuringLd_dfck_rck : Std_Logic;
    signal CError_rck              : Std_Logic;
    signal NCError_rck             : Std_Logic;
    
    signal Error_rck               : Std_Logic;
    
    signal PROM_IO_Ext_Access     : Std_Logic;
    signal Ext_Access             : Std_Logic;
    signal IO_Access              : Std_Logic;
    signal IO_Access_fck          : Std_Logic;
    signal RAM_Access             : Std_Logic;
    signal ROM_Access             : Std_Logic;
    
    signal RAM_Access_sel         : Std_Logic;
    signal PROM_Access_sel        : Std_Logic;
    signal IO_Access_sel          : Std_Logic;
    signal Ext_Access_sel         : Std_Logic;
    signal PROM_IO_Ext_Access_sel : Std_Logic;
        
    signal Rd_In_Latch            : Std_Logic;
    signal Size_In_Latch_1        : Std_Logic;
   
    signal WScounterLoadEN        : Std_Logic;
    signal WScounterLoadEN_loc    : Std_Logic;
    signal SSWRead                : Std_Logic;
    
    signal ForceNonMemDataLE_fck2 : Std_Logic;
    signal ForceNonMemDataLE_loc  : Std_Logic;
    
    signal WaitStateLDval_c       : Std_Logic_Vector(3 downto 0) := "0000";
    signal WaitStateLDval         : Std_Logic_Vector(3 downto 0) := "0000";
    signal WaitState              : Std_Logic_Vector(3 downto 0) := "0000";
    
    signal BA_Out_fck             : Std_Logic_Vector(1 downto 0);
    signal BA_Out_rck             : Std_Logic_Vector(1 downto 0);
    
    signal OE_SSW_N               : std_logic;
    signal OE_SSW_N_fck           : std_logic;
    signal OE_SSW_N_rck           : std_logic;
    signal OECond                 : std_logic;

    signal MHold_Err_N_rst        : std_logic;
    signal MHold_Err_N            : std_logic;
    signal MHold_WS_N             : std_logic;
    signal GlobalHold_N           : std_logic;
    
    signal MDS_WS_LdR84_N         : std_logic;
    signal MDS_WS_LdR84_N_rck     : std_logic;
    signal MDS_WS_N_ld            : std_logic;
    signal LoadOp                 : std_logic;
    
    signal State                  : mdsstate;
       
    signal SeqState            : std_logic_vector (43 downto 0);
    
    signal BusRdy_In_N_ck0     : std_logic;
    
    signal MHold_WS_N_rck      : std_logic;    

    signal MemData_Valid_fck   : std_logic;
    
    signal MemWr_Out_N_CK      : std_logic;
    signal MemWr_Out_N_fck     : std_logic;
    signal MemWr_Out_N_rck     : std_logic;
    
    signal IOWr_Out_N_CK       : std_logic;
    signal IOWr_Out_N_fck      : std_logic;
    signal IOWr_Out_N_rck      : std_logic;
    
    signal Wr_Int_N_fck        : std_logic;
    
    signal IOWr_Cond1          : std_logic;
    signal IOWr_Cond2          : std_logic;
    
    signal MemWr_Cond1         : std_logic;
    signal MemWr_Cond2         : std_logic;
    
    signal CBEn_N_fck          : std_logic;
    
    signal CBWrCond_N          : std_logic;

    signal OE_loc_N            : std_logic;
    signal OE_N_v              : std_logic;
    signal OE_loc_N_fck2       : std_logic;
    
    signal PartOfOE_N_fck2     : std_logic;
    signal PartOfOE_N_rck2     : std_logic;
    
    signal DBEn_fck            : std_logic;
    signal DBEn_rck            : std_logic;
    signal ALE_St_N            : std_logic;
        
    signal DMA_OE_Window      : std_logic;
    signal DRdy_Out_N_loc     : std_logic;
    signal DRdy_Out_N_loc_rck : std_logic;
    signal DRdy_Out_N_drck : std_logic;
       
    signal ResetDMAInPrgs     : std_logic;
    signal DMAInPrgs_loc      : std_logic;
    signal DMAInPrgs_fck      : std_logic;
    signal DMA_Mode_fck       : std_logic;
    signal DMA_Mode_rck       : std_logic;
    
    signal Mexc_Out_N_del     : std_logic;
    signal Mexc_Out_N_loc     : std_logic;
    
    signal ROM8Reg_N_loc      : std_logic;
        
    signal StoreByte_Reg_Read_N_loc : std_logic;
    signal StoreByte_Reg_Read_N_rck : std_logic;
    
    signal SSWIdentifier_fck  : std_logic;
    signal SSWIdentifier_rck  : std_logic;
    
    signal Null_Int_ck0_loc   : std_logic;
    signal Null_Int_ck0       : std_logic;
    signal Null_Int_loc1_rck  : std_logic;
    signal Null_Int_loc_rck   : std_logic;
    signal Null_Int_fck       : std_logic;
    
    signal OE_Mask            : std_logic;
    
    signal DParIOEn_N_loc     : std_logic;
    signal DParIOEn_N_rck     : std_logic;
    
    signal DBEn_Int           : std_logic;
    signal DParIOEn_Int       : std_logic;
        
    signal MexcVarRst         : std_logic;
    signal MexcVarSmple       : std_logic;
    signal MexcVar            : std_logic;
    signal MexcVar_fck        : std_logic;
        
    signal MHold_Err_N_En     : std_logic;
        
    signal DBEnLdR8Window_N       : std_logic;
    signal DBEnLdR8Window_N_rck   : std_logic;
    signal DBEnLdR8Window_N_fck2  : std_logic;

    signal ByteSel_stsw_loc   : Std_Logic_Vector(3 downto 0);
    signal ByteSel_stsw_rck   : Std_Logic_Vector(3 downto 0);    
    signal ByteSel_ldr8       : Std_Logic_Vector(3 downto 0);
    
    signal Count              : Std_Logic_Vector(7 downto 0);

    signal Ext_Area_Sel      : std_logic;
    signal IO_Area_Int_g     : std_logic;
    signal Mem_Area_Int_g    : std_logic;
    signal Register_Area_Out_g : std_logic;
    signal PROM_Area_Out_g   : std_logic;
    signal Exch_Area_Int_g   : std_logic;
    signal IO_Select_Int_g   : Std_Logic_Vector(1 downto 0);
    
    signal GeneralExt_Access : std_logic;
    signal IO_Select_Int     : Std_Logic_Vector(1 downto 0);
    signal IO_Area_Int       : std_logic;
    signal Mem_Area_Int      : std_logic;
    signal Exch_Access       : std_logic;
    signal PROM_Area_Out     : std_logic;
    signal PROM_Area_Out_F   : std_logic;
    
    signal IO_Area_Int_fck   : std_logic;
    signal Mem_Area_Int_fck  : std_logic;
    signal Ext_Access_fck    : std_logic;
    signal PROM_Area_Out_fck : std_logic;
    
    signal WSLdVal_SSWRd     : Std_Logic_Vector(1 downto 0) := "00";
    signal WSLdVal_SSWWr     : Std_Logic_Vector(1 downto 0) := "00";
    
    signal WSLdVal_LdRAM     : Std_Logic_Vector(1 downto 0) := "00";
    signal WSLdVal_StRAM     : Std_Logic_Vector(1 downto 0) := "00";
    
    signal WSLdVal_LdROM     : Std_Logic_Vector(3 downto 0) := "0000";
    signal WSLdVal_StROM     : Std_Logic_Vector(3 downto 0) := "0000";
    
    signal WSLdVal_EXCH      : Std_Logic_Vector(3 downto 0) := "0000";
        
    signal WSUNOrZR          : std_logic;
    signal WSDEUXorUNorZR    : std_logic;
    signal ExchWSONE         : std_logic;
    
    signal BusRdy_In_N       : std_logic;
    
    signal LdRAMWSNN_N       : std_logic;
    signal LdROM8WSNN_N      : std_logic;
    
    signal StRAMWSNN_N       : std_logic;
    
    signal SSWRdWSNN_N       : std_logic;
    signal SSWWrWSNN_N       : std_logic;
    
    signal L_Buf2            : std_logic;
    signal L_Buf3            : std_logic;
    signal L_Buf4            : std_logic;
    signal L_Enable          : std_logic;
    signal L_Enable_Buf1     : std_logic;
    signal L_Enable_Buf2     : std_logic;
    signal L_Enable_Buf3     : std_logic;
    
    signal B_Load_c          : std_logic;
    signal B_Store_c         : std_logic;
    signal B_StoreSubWord_c  : std_logic;
    signal B_LoadROM8_c      : std_logic;
    signal B_MECLoad_c       : std_logic;
    signal B_MECStore_c      : std_logic;
        
    signal B_Load            : std_logic;
    signal B_Store           : std_logic;
    signal B_StoreSubWord    : std_logic;
    signal B_LoadROM8        : std_logic;
    signal B_MECLoad         : std_logic;
    
    signal B_Load_fck2            : std_logic;
    signal B_StoreSubWord_fck2    : std_logic;
    signal B_MECLoad_fck2         : std_logic;
    
    signal IOBenCond         : std_logic;
    signal IOBEn_Out_N_loc   : std_logic;
    signal IOBEn_Out_N_v     : std_logic;
    
    signal MemBenCond        : std_logic;
    signal MemBEn_Out_N_loc  : std_logic;
    signal MemBEn_Out_N_v  : std_logic;
    
    signal Wr_En_N           : std_logic;
    
    signal NullEn            : std_logic;
    signal NullEn_fck        : std_logic;
        
    signal RstACMachine      : std_logic;
    signal Reset_Out_N_fck1  : std_logic;
    signal Reset_Out_N_fck2  : std_logic;
    
    signal NewCycle_Int      : std_logic;
    signal NewCycle_loc      : std_logic;
    signal NewCycleOOR       : std_logic;
    
    signal BTOToFSM              : std_logic;
    signal FStCyDoubleWord       : std_logic;
    signal FStCyDoubleWord1      : std_logic;
    signal FStCyDoubleWord_fck   : std_logic;
    signal FStCyDoubleWord1_fck  : std_logic;
    signal SStCyDoubleWord_fck   : std_logic;
    
    signal MExcReqDetect         : std_logic;
    signal MExcRequest_fck       : std_logic;
    
    signal DMAMxEn                : std_logic;
    
    signal IU_MemBEnBase_N_fck2   : std_logic;
    signal IU_MemBEnBase_N_rck2   : std_logic;
    signal IU_MemBEnBase_N        : std_logic;
    
    signal DMA_MemBEnBase_N_fck2  : std_logic;
    signal DMA_MemBEnBase_N_rck2  : std_logic;
    signal DMA_MemBEnBase_N       : std_logic;
    
    signal ResetMemBEnWindow      : std_logic;
    signal MemBEnWindow           : std_logic;
    
    signal EndOfCycle             :std_logic;
    signal Register_Access        :std_logic;
    
    signal SynthBull1             :std_logic;
    signal SynthBull2             :std_logic;
    
    signal NewCycleForSDW        :std_logic;
    signal NewCycle_Bull         :std_logic;
    
    signal FHold_N               :std_logic;
    signal FHold_N_fck           :std_logic;
    
    signal MexcFirAccLdSto_fck   :std_logic;
    signal MexcFirAccLdSto_rck   :std_logic;
    signal MexcFirAccLdSto_drck  :std_logic;
    
    signal MEC_A             : Std_Logic_Vector(7 downto 0) := "00000000";
    signal MEC_A_Slow        : Std_Logic_Vector(7 downto 0) := "00000000";

    signal High_A            : Std_Logic_Vector(7 downto 0) := "00000000";
    signal High_A_Slow       : Std_Logic_Vector(7 downto 0) := "00000000";
    
    signal ALsb_DFF          : Std_Logic_Vector(1 downto 0);
    

    signal S_Idle, 
           S_BHoldIn, S_BHold1, S_BHoldCPU, 
           S_DMAMx1, S_DMAMx2, S_DMAMxOut, 
           S_DMAWaitAS, S_DMAOut1, S_DMAOut,  
           S_MxSt,
           S_Mx, S_MxDE,
           S_MLdOut, 
           S_LdEcAc, S_LdEcAc1, 
           S_LdWSNN, 
           S_LdIO, 
           S_LdOut, 
           S_LdDMAIn1, 
           S_LdDMAIn2, 
           S_St1, 
           S_StEcAc, S_StEcAc1, 
           S_StWSNN, S_StWSNN1, 
           S_StOut,
           S_LdR81Decod, S_LdR81, S_LdR8WSNN1, S_LdR82, S_LdR8WSNN2, 
           S_LdR83, S_LdR8WSNN3, S_LdR84, S_LdR8WSNN4, 
           S_LdR8Out, 
           S_SSW1, S_SSW2, S_SSW3, S_SSWWSNN, S_SSW4, S_SSWWSNNWR, 
           S_SSWOut : std_logic;
                         

begin


    Ad1_Buff : AddBuff
    
    port map (
              D_In  => A_In,
              D_Out => A_Loc_Invert
             );

    A_to_AD <= not A_Loc_Invert;
    
    A_to_ML <= not A_Loc_Invert;

----------------------------------------------------------------------------
-- Access Controler FSM
----------------------------------------------------------------------------
    DMAAS_In_buffer : CtrlBuff
    port map (D_In  => DMAAS_In,
              D_Out => DMAAS_In_Buff
              );
              
        
   RstACMachine   <= Reset_Out_N_fck1;
                
   AC_FSM : AccessControl_fsm
    port map (
              Clk_Int           =>  Clk_Int,
              DPar_Error_Wr_ck1 =>  DPar_Error_Wr_ck1,
              FHold_N           =>  FHold_N,
              ST_OUT            =>  SeqState,
              DBEn_fck          =>  DBEn_fck,
              MemData_Valid_fck =>  MemData_Valid_fck,
              IOWr_Out_N_fck    =>  IOWr_Out_N_fck,
              MemWr_Out_N_fck   =>  MemWr_Out_N_fck,
              NewCycle          =>  NewCycle_loc,
              EndOfCycle        =>  EndOfCycle,
              Wr_En_N           =>  Wr_En_N,
              DMAMxEn           =>  DMAMxEn,
              Reset_Out_N       =>  RstACMachine,
              DMA_Mode          =>  DMA_Mode,
              IO_Access         =>  IO_Access,
              Exch_Access       =>  Exch_Access,
              IO_Area_Int_fck   =>  IO_Area_Int_fck,
                                          
              WSUNOrZR          =>  WSUNOrZR,
              WSDEUXorUNorZR    =>  WSDEUXorUNorZR,
              
              ExchWSONE         =>  ExchWSONE,
              
              LdRAMWSNN_N       =>  LdRAMWSNN_N,

              StRAMWSNN_N       =>  StRAMWSNN_N,
              
              SSWRdWSNN_N       =>  SSWRdWSNN_N,
              SSWWrWSNN_N       =>  SSWWrWSNN_N,
              
              LdROM8WSNN_N      =>  LdROM8WSNN_N,

              B_MECLoad         =>  B_MECLoad,
              B_Load            =>  B_Load,
              B_Store           =>  B_Store,
              B_LoadROM8        =>  B_LoadROM8,
              B_StoreSubWord    =>  B_StoreSubWord,
              
              PROM_IO_Ext_Access     =>  PROM_IO_Ext_Access,
              Ext_Access_fck    =>  Ext_Access_fck,
              CPUHalt_N         =>  CPUHalt_N,
              BusRequest_Int    =>  BusRequest_Int,
              MExcReqDetect     =>  MExcReqDetect,
              DMAAS_In          =>  DMAAS_In_buff,
              MExcVarFSM2       =>  MExcVarFSM2,
              BHold_Out_N       =>  BHold_Out_N,
              DMAInPrgs_fck     =>  DMAInPrgs_fck,
              BusRdy_In_N_ck0   =>  BusRdy_In_N_ck0,
              Null_Int_ck0      =>  Null_Int_ck0,
              Mem_Area_Int_fck  =>  Mem_Area_Int_fck,
              PROM_Area_Out_fck =>  PROM_Area_Out_fck,
              Error_detect_FSM  =>  Error_detect_FSM,
              MExcVarFoo        =>  MExcVarFoo,
              BusTimeOut        =>  BTOToFSM
             );


     S_Idle       <= SeqState(42);
     S_BHoldIn    <= SeqState(42);
     S_BHold1     <= SeqState(41);
     S_BHoldCPU   <= SeqState(40);
     S_DMAMx1     <= SeqState(39);
     S_DMAMx2     <= SeqState(38);
     S_DMAMxOut   <= SeqState(37);
     S_DMAWaitAS  <= SeqState(36);
     S_DMAOut1    <= SeqState(35);
     S_DMAOut     <= SeqState(34);
     S_MxSt       <= SeqState(33);
     S_Mx         <= SeqState(32);
     S_MxDE       <= SeqState(31);
     S_MLdOut     <= SeqState(30);
     S_LdEcAc     <= SeqState(29);
     S_LdEcAc1    <= SeqState(28);
     S_LdWSNN     <= SeqState(27);
     S_LdIO       <= SeqState(26);
     S_LdOut      <= SeqState(25);
     S_LdDMAIn1   <= SeqState(24);
     S_LdDMAIn2   <= SeqState(23);
     S_St1        <= SeqState(22);
     S_StEcAc     <= SeqState(21);
     S_StEcAc1    <= SeqState(20);
     S_StWSNN     <= SeqState(19);
     S_StWSNN1    <= SeqState(18);
     S_StOut      <= SeqState(17);
     S_LdR81Decod <= SeqState(16);
     S_LdR81      <= SeqState(15);
     S_LdR8WSNN1  <= SeqState(14);
     S_LdR82      <= SeqState(13);
     S_LdR8WSNN2  <= SeqState(12);
     S_LdR83      <= SeqState(11);
     S_LdR8WSNN3  <= SeqState(10);
     S_LdR84      <= SeqState(9);
     S_LdR8WSNN4  <= SeqState(8);
     S_LdR8Out    <= SeqState(7);
     S_SSW1       <= SeqState(6);
     S_SSW2       <= SeqState(5);
     S_SSW3       <= SeqState(4);
     S_SSWWSNN    <= SeqState(3);
     S_SSW4       <= SeqState(2);
     S_SSWWSNNWR  <= SeqState(1);
     S_SSWOut     <= SeqState(0);


----------------------------------------------------------------------------
--Area decoding
----------------------------------------------------------------------------

    L_Buff2 : LBuff
    port map (
              Clk1 => Clk_Int,
              Clk2 => Clk_Int,
              LBuf => L_Buf2
              );
              
    Latch_P2 : process (L_Buf2, Rd_In, Size_In)
    begin
      if L_Buf2 = '0' then
        Rd_In_Latch     <= Rd_In;
        Size_In_Latch_1 <= Size_In(1);
      end if;
    end process;
    
    -----
    L_Buff3 : LBuff
    port map (
              Clk1 => Clk_Int,
              Clk2 => Clk_Int,
              LBuf => L_Buf3
              );
               
    Latch_P3 : process (L_Buf3, A_in)
    begin
      if L_Buf3 = '0' then
        MEC_A <= A_In(23 downto 16);
      end if;
    end process;
    
    MEC_A_Buff : ABuff
    
    port map (
              A_In  => MEC_A,
              A_Out => MEC_A_Slow
             );

    -----
    L_Buff4 : LBuff
    port map (
              Clk1 => Clk_Int,
              Clk2 => Clk_Int,
              LBuf => L_Buf4
              );
              
    Latch_P4 : process (L_Buf4, A_in)
    begin
      if L_Buf4 = '0' then
        High_A <= A_In(31 downto 24);
      end if;
    end process;
    
    High_A_Buff : ABuff
    
    port map (
              A_In  => High_A,
              A_Out => High_A_Slow
             );

    -----

    AddressDecoder: process(High_A, High_A_Slow, MEC_A, MEC_A_Slow)
    variable IO_Ext_Area_Sel : std_logic;
    variable RAM_Ext_Area_Sel : std_logic;
    variable PROM_Ext_Area_Sel : std_logic;
    variable PROM_Access_sel_var : std_logic;
    begin
        
        Ext_Area_Sel     <= High_A_Slow(7); --1XXX_XXXX
        
        --0001_0100 to 0111_1111
        IO_Ext_Area_Sel  := not(High_A(7)) and 
                            (High_A(6) or High_A(5) or 
                            (High_A(4) and (High_A(3) or High_A(2)) ));                                                      
                           
                            
        if High_A(7 downto 4) ="0000" and (High_A(3)='1' or High_A(2)='1') then
             RAM_Ext_Area_Sel := '1';             
        else
             RAM_Ext_Area_Sel := '0';
        end if;

        
        if High_A ="00000001" and not(MEC_A(7 downto 4)="1111") then
             PROM_Ext_Area_Sel := '1';             
        else
             PROM_Ext_Area_Sel := '0';
        end if;
        
        Ext_Access_sel <= PROM_Ext_Area_Sel or RAM_Ext_Area_Sel or IO_Ext_Area_Sel or High_A(7);
        

        ---------------------------
                                
                            
        if High_A_Slow(7 downto 2) ="000100"  then
             IO_Area_Int_g <= '1';             
        else
             IO_Area_Int_g <= '0';
        end if;

        
        if High_A_Slow(7) = '0' and not (High_A_Slow(6 downto 4) = "000")  then
             IO_Access_sel <= '1';             
        else
             IO_Access_sel <= '0';
        end if;
        
        IO_Select_Int_g  <= High_A_Slow (1 downto 0);
        
        ---------------------------
         
        
        if High_A_Slow(7 downto 1) ="0000001"  then
             Mem_Area_Int_g <= '1';             
        else
             Mem_Area_Int_g <= '0';
        end if;
        
        
        if High_A_Slow(7 downto 4) ="0000" and not (High_A_Slow(3 downto 1) ="000")  then
             RAM_Access_sel <= '1';             
        else
             RAM_Access_sel <= '0';
        end if;
                
        ---------------------------
                 
        if High_A_Slow ="00000001" and MEC_A_Slow(7 downto 3)="11111"  then
             Register_Area_Out_g <= '1';             
        else
             Register_Area_Out_g <= '0';
        end if;
        
        if High_A_Slow ="00000001" and MEC_A_Slow(7 downto 3)="11110"  then
             Exch_Area_Int_g <= '1';             
        else
             Exch_Area_Int_g <= '0';
        end if;
        
        ---------------------------
                            
        if High_A_Slow ="00000000" then
             PROM_Area_Out_g <= '1';             
        else
             PROM_Area_Out_g <= '0';
        end if;
        
        if High_A_Slow(7 downto 1) ="0000000" and 
           not ((High_A_Slow(0)='1') and (MEC_A_slow(7 downto 4)="1111"))  then
             PROM_Access_sel_var := '1';             
        else
             PROM_Access_sel_var := '0';
        end if;
        
            
            
        PROM_Access_sel        <= PROM_Access_sel_var;
             
        PROM_IO_Ext_Access_sel <= PROM_Access_sel_var or High_A(7) or High_A(6) or 
                                  High_A(5) or High_A(4) or High_A(3) or High_A(2);
             

    end process; -- AdressDecoder
    
    
      L_Enable_P: process(DMA_Mode_fck, NewCycle_Int, DMAAS_In)
      begin
          
        if (DMA_Mode_fck = '0' and NewCycle_Int = '1')  or
           (DMA_Mode_fck = '1' and DMAAS_In = '1') then
          L_Enable <= '1';
         else
          L_Enable <= '0';
        end if; 
      
      end process;

    
      L_Enable_Buff1: process(L_Enable, Clk_Int)
      begin
          
         L_Enable_Buf1 <= L_Enable and Clk_Int;
      
      end process;
    
    -----------------------------------------------------------------------------------------
    BusAccessTypeCombi: process(Register_Area_Out_g, Rd_In_Latch, 
                                PROM_Access_sel, PROM8_In_N, 
                                RAM_Access_sel, Size_In_Latch_1) 
    begin
    
         if PROM_Access_sel = '1' and PROM8_In_N = '0' and Rd_In_Latch = '1' then
           BusAccessEnum_c  <= LoadROM8;
           B_LoadROM8_c     <= '1';
           B_Load_c         <= '0';
           
         elsif Register_Area_Out_g = '0' and Rd_In_Latch = '1' then
           BusAccessEnum_c  <= Load;
           B_LoadROM8_c     <= '0';
           B_Load_c         <= '1';
           
         else
           B_Load_c         <= '0';
           B_LoadROM8_c     <= '0';
         end if;

                          
         if RAM_Access_sel = '1' and Size_In_Latch_1 = '0' and 
            Rd_In_Latch = '0' then
           BusAccessEnum_c  <= StoreSubWord;
           B_StoreSubWord_c <= '1';
           B_Store_c        <= '0';
           
         elsif Rd_In_Latch = '0' then
           BusAccessEnum_c  <= Store;
           B_StoreSubWord_c <= '0';
           B_Store_c        <= '1';
           
         else
           B_Store_c        <= '0';
           B_StoreSubWord_c <= '0';
         end if;


         if Register_Area_Out_g = '1' and Rd_In_Latch = '1' then
           BusAccessEnum_c  <= MECLoad;
           B_MECLoad_c      <= '1';
         else
           B_MECLoad_c      <= '0';
         end if;

    end process;


    BusAccessTypeGenerator: process(L_Enable_Buf1, Reset_Out_N, BusAccessEnum_c, 
                                    B_LoadROM8_c, B_Load_c,
                                    B_StoreSubWord_c, B_Store_c,
                                    B_MECLoad_c) 
    begin
     if Reset_Out_N = '0' then
           BusAccessEnum  <= Idle;
           B_Load         <= '0';
           B_Store        <= '0';
           B_StoreSubWord <= '0';
           B_LoadROM8     <= '0';
           B_MECLoad      <= '0';
     elsif L_Enable_Buf1 = '1'  then
           BusAccessEnum  <= BusAccessEnum_c;
           B_LoadROM8     <= B_LoadROM8_c;
           B_Load         <= B_Load_c;
           B_StoreSubWord <= B_StoreSubWord_c;
           B_Store        <= B_Store_c;
           B_MECLoad      <= B_MECLoad_c;
     end if;

    end process;


  ctrl_XBen1_N : process (B_Load, B_Store, B_StoreSubWord, B_LoadROM8, B_MECLoad)
  begin
    if  ((B_Load = '1') and (B_Store = '1' or B_StoreSubWord = '1' or 
                             B_LoadROM8 = '1' or B_MECLoad = '1' )) or 
                              
        ((B_Store = '1') and (B_StoreSubWord = '1' or 
                              B_LoadROM8 = '1' or B_MECLoad = '1' )) or
                              
        ((B_StoreSubWord = '1') and (B_LoadROM8 = '1' or B_MECLoad = '1' )) or
        
        ((B_LoadROM8 = '1') and (B_MECLoad = '1' )) then
      assert FALSE
      report " WARNING B_Load and B_Store or etc asserted "
      severity failure;
    end if;
                    
  end process;


    ZoneGenerator1: process(L_Enable_Buf1, Reset_Out_N, Register_Area_Out_g,
                           RAM_Access_sel, PROM_Access_sel, 
                           IO_Access_sel, Ext_Access_sel, PROM_IO_Ext_Access_sel) 
    begin
    
    if Reset_Out_N = '0' then
       Register_Access    <= '0';
       RAM_Access         <= '0';
       ROM_Access         <= '0';
       IO_Access          <= '0';
       Ext_Access         <= '0';      
       PROM_IO_Ext_Access <= '0';      
           
    elsif L_Enable_Buf1 = '1'  then
       Register_Access    <= Register_Area_Out_g;
       RAM_Access         <= RAM_Access_sel;
       ROM_Access         <= PROM_Access_sel;
       IO_Access          <= IO_Access_sel;
       Ext_Access         <= Ext_Access_sel;      
       PROM_IO_Ext_Access <= PROM_IO_Ext_Access_sel;      
    end if;

    end process;

    
    L_Enable_Buff2: process(L_Enable, Clk_Int)
    begin
          
      L_Enable_Buf2 <= L_Enable and Clk_Int;
      
    end process;

    
    ZoneGenerator2: process(L_Enable_Buf2, Reset_Out_N,
                            Ext_Area_Sel, IO_Select_Int_g, 
                            IO_Area_Int_g, Mem_Area_Int_g,
                            Exch_Area_Int_g, PROM_Area_Out_g)
    begin
    
    if Reset_Out_N = '0' then
       GeneralExt_Access    <= '0';
       IO_Select_Int        <= "00";
       IO_Area_Int          <= '0';
       Mem_Area_Int         <= '0';      
       Exch_Access          <= '0';      
       PROM_Area_Out_F      <= '0';      
     
    elsif L_Enable_Buf2 = '1'  then
       GeneralExt_Access    <= Ext_Area_Sel;
       IO_Select_Int        <= IO_Select_Int_g;
       IO_Area_Int          <= IO_Area_Int_g;
       Mem_Area_Int         <= Mem_Area_Int_g;      
       Exch_Access          <= Exch_Area_Int_g;      
       PROM_Area_Out_F      <= PROM_Area_Out_g;      
    end if;

    end process;


PROM_Area_buffer : CtrlBuff
    port map (D_In  => PROM_Area_Out_F,
              D_Out => PROM_Area_Out
              );
              
    
    TOFSM: process(WSLdVal_LdRAM, Mem_Area_Int,
                   WSLdVal_LdROM, PROM_Area_Out_F,
                   WSLdVal_StRAM, PROM_Area_Out,
                   WSLdVal_StROM,
                   WSLdVal_SSWRd, 
                   WSLdVal_SSWWr,
                   WSLdVal_EXCH,
                   Ext_Access, WScounterLoadEN,
                   WaitState) 
    begin
    
      if (WSLdVal_LdRAM /= "00" and Mem_Area_Int = '1') then
         LdRAMWSNN_N    <= '0';
      else
         LdRAMWSNN_N    <= '1';
      end if;

      if (WSLdVal_StRAM /= "00" and Mem_Area_Int = '1') then
         StRAMWSNN_N    <= '0';
      else
         StRAMWSNN_N    <= '1';
      end if;

      
      if WSLdVal_SSWRd /= "00" and Mem_Area_Int = '1' then
         SSWRdWSNN_N    <= '0';
      else
         SSWRdWSNN_N    <= '1';
      end if;
      
      if WSLdVal_SSWWr /= "00" and Mem_Area_Int = '1' then
         SSWWrWSNN_N    <= '0';
      else
         SSWWrWSNN_N    <= '1';
      end if;
      
      if (Ext_Access = '0' and (WSLdVal_LdROM /= "0000" and WSLdVal_LdROM /= "0001")) or 
         (Ext_Access = '1') then
         LdROM8WSNN_N    <= '0';
      else
         LdROM8WSNN_N    <= '1';
      end if;
      
      
      if WaitState = "0001" or WaitState = "0000" then
         WSUNOrZR    <= '1';
      else
         WSUNOrZR    <= '0';
      end if;
      
      if WaitState = "0010" or WaitState = "0001" or WaitState = "0000" then
         WSDEUXorUNorZR    <= '1';
      else
         WSDEUXorUNorZR    <= '0';
      end if;
      
      
      if (WScounterLoadEN = '1' and WSLdVal_EXCH = "0001") then
         ExchWSONE    <= '1';
      else
         ExchWSONE    <= '0';
      end if;
      
    end process;
      
    -----
    FCDW_DFF : process(Reset_Out_N, Clk_Int)
    begin
      if Reset_Out_N = '0' then 
         FStCyDoubleWord   <= '0';
        
      elsif  Clk_Int'event and Clk_Int = '1' then
      
        if ((S_StOut = '1' or S_MxSt = '1') and FStCyDoubleWord = '1') or 
           Null_Int_ck0 = '1' then     
           FStCyDoubleWord   <= '0';
        elsif S_St1 = '1' and Size_Int_In = "11" and 
              RD_Int_in = '0' and SStCyDoubleWord_fck = '0' then
           FStCyDoubleWord   <= '1';
        end if;
      
      end if;
    end process;
    

    FCDW_DFF1 : process(Reset_Out_N, Clk_Int)
    begin
      if Reset_Out_N = '0' then 
         FStCyDoubleWord1   <= '0';
        
      elsif  Clk_Int'event and Clk_Int = '1' then
      
        if (S_StOut = '1' and FStCyDoubleWord1 = '1') or 
           Null_Int_ck0 = '1' then     
           FStCyDoubleWord1   <= '0';
        elsif S_St1 = '1' and Size_Int_In = "11" and 
              RD_Int_in = '0' and SStCyDoubleWord_fck = '0' then
           FStCyDoubleWord1   <= '1';
        end if;
      
      end if;
    end process;
    
    -----
    SCDW_DFF : process(Reset_Out_N, Clk_Int)
    begin
      if Reset_Out_N = '0' then 
         SStCyDoubleWord_fck   <= '0';
        
      elsif  Clk_Int'event and Clk_Int = '0' then
      
        if NewCycleForSDW = '1' and FStCyDoubleWord1_fck = '1' then
           SStCyDoubleWord_fck   <= '1';
        elsif S_StWSNN = '0' and S_StWSNN1 = '0' and S_St1 = '0' then     
           SStCyDoubleWord_fck   <= '0';
        end if;
      
      end if;
    end process;

    
    -----
    Ad_DFF : process(Reset_Out_N, Clk_Int)
    begin
      if Reset_Out_N = '0' then 
         ALsb_DFF       <= "00";
         WriteAccess_N  <= '1';
        
      elsif  Clk_Int'event and Clk_Int = '1' then
      
        if (DMA_Mode_fck = '0' and NewCycle_Int = '1') or
           (DMA_Mode_fck = '1' and DMAAS_In = '1') then     
           ALsb_DFF       <= A_In(1 downto 0);
           WriteAccess_N  <= Rd_In;
       end if;
      
      end if;
    end process;

    
----------------------------------------------------------------------------
--Waitstate Generator
----------------------------------------------------------------------------
    WriteRegs: process(Reset_Int_N, Clk_Int)
    begin
      if Reset_Int_N = '0' then      
          WaitStateConfigReg    <= (others => '1');
          WaitStateConfigRegPar <= '1';

      elsif Clk_Int'event and Clk_Int = '0' then
      
        if WSConfigReg_N = '0' and Wr_Int_N = '0' then
          WaitStateConfigReg    <= D_Int_In(31 downto 0);
          WaitStateConfigRegPar <= DPar_Int_In;
        end if;
         
      end if;
    end process;


    ParityChecker: process(WaitStateConfigReg,WaitStateConfigRegPar, SeqState, BAError)
    begin
        ParErrWSReg <= ParityCheck(WaitStateConfigReg,WaitStateConfigRegPar) or
                       not ParityGen(SeqState) or BAError;
    end process;


    WScounterLoadEN <= NewCycle_Int or WScounterLoadEN_loc or S_Idle;

    Counter: process
    begin
                
    wait until Clk_Int'event and Clk_Int = '0';
    
      if WScounterLoadEN = '1' then
         WaitState <= WaitStateLDval;
      elsif WaitState /= "0000" then
         WaitState <= WaitState - 1;
      end if;
     
    end process;
    
        
    L_Enable_Buff3: process(L_Enable, Clk_Int)
    begin
      L_Enable_Buf3 <= L_Enable and Clk_Int;
    end process;
    

    WSloadValue_combi: process( Rd_In_Latch, Size_In_Latch_1, RAM_Access, SSWRead, WaitStateConfigReg, 
                                IO_Select_Int, Mem_Area_Int_g, 
                                PROM_Area_Out_g, IO_Area_Int_g)
    begin
        -- Store Sub Word  
        if (Mem_Area_Int_g = '1') and 
           (Rd_In_Latch = '0') and (Size_In_Latch_1 = '0') then
                   
             if SSWRead = '1' then
               WaitStateLDval_c <= '0' & '0' & WaitStateConfigReg(1 downto 0);
             else
               WaitStateLDval_c <= '0' & '0' & WaitStateConfigReg(3 downto 2);
             end if;
             
        -- RAM access (Load and Store)
        elsif (Mem_Area_Int_g = '1') then
           
            if (Rd_In_Latch = '1') then               
              WaitStateLDval_c <= '0' & '0' & WaitStateConfigReg(1 downto 0);
            else
              WaitStateLDval_c <= '0' & '0' & WaitStateConfigReg(3 downto 2);
            end if;
            
        -- IO Access (Load and Store)      
        elsif IO_Area_Int_g = '1' then 
              
             case  IO_Select_Int is
               when "11"   => WaitStateLDval_c <= WaitStateConfigReg(31 downto 28); 
               when "10"   => WaitStateLDval_c <= WaitStateConfigReg(27 downto 24);
               when "01"   => WaitStateLDval_c <= WaitStateConfigReg(23 downto 20);
               when "00"   => WaitStateLDval_c <= WaitStateConfigReg(19 downto 16);
               when others => WaitStateLDval_c <= WaitStateConfigReg(19 downto 16);
             end case; 
             
        -- PROM Access (Load and Store)      
        elsif PROM_Area_Out_g = '1' then
            
            if (Rd_In_Latch = '1') then
               WaitStateLDval_c <= WaitStateConfigReg(7 downto 4);
            else
               WaitStateLDval_c <= WaitStateConfigReg(11 downto 8);
            end if;
              
        -- Exchange Access
        else
            WaitStateLDval_c <= WaitStateConfigReg(15 downto 12);
                
        end if;
      
    end process;

        
    WSloadValue: process(L_Enable_Buf3, Reset_Out_N,WaitStateLDval_c)
    begin
      
    if Reset_Out_N = '0'  then
       WaitStateLDval <= "1111";
    elsif L_Enable_Buf3 = '1'  then
       WaitStateLDval <= WaitStateLDval_c;
    end if;

    end process;

        
   WSLdVal_SSWRd <= WaitStateConfigReg(1 downto 0);
   WSLdVal_SSWWr <= WaitStateConfigReg(3 downto 2);
           
   WSLdVal_LdRAM <= WaitStateConfigReg(1 downto 0);
   WSLdVal_StRAM <= WaitStateConfigReg(3 downto 2);
             
   WSLdVal_LdROM <= WaitStateConfigReg(7 downto 4);
   WSLdVal_StROM <= WaitStateConfigReg(11 downto 8);
               
   WSLdVal_EXCH  <= WaitStateConfigReg(15 downto 12);
                
----------------------------------------------------------------------------
--Bus TimeOut Timer
----------------------------------------------------------------------------
    BusTimeOutCounter: process(Reset_Out_N, Clk_Int)
    begin
      if Reset_Out_N = '0' then 
         Count      <= "11111111";
         BusTimeOut <= '0';
         
      elsif Clk_Int'event and Clk_Int = '1' then 
              
         if BusTOEnable = '0' then 
           Count      <= "11111111";
           BusTimeOut <= '0';
         elsif Count /= "00000000"  and BusTimeOut_En = '1' then
           Count      <= Count-1;
           BusTimeOut <= '0';
         elsif Count = "00000000" then
           Count      <= "11111111";
           BusTimeOut <= '1';           
         end if;
         
      end if;
    end process;

----------------------------------------------------------------------------
--INull_Int Generator
----------------------------------------------------------------------------
    NullEnGen: process(Clk_Int,Reset_Int_N)
    begin
        if Reset_Int_N = '0' then
          NullEn <= '0';
        elsif Clk_Int'event and Clk_Int = '1' then 
          if NewCycle_Int = '1' and RD_In = '0' then 
            NullEn <= '1';
          elsif NullEn = '1' then 
            NullEn <= '0';
          end if;
        end if;
    end process;


    GlobalHold_NGen: process(MHold_Out_ck1_N,BHold_Out_N)
    begin
       GlobalHold_N <= MHold_Out_ck1_N and BHold_Out_N;
    end process;
        
    
    INull_IntGenerator: process(Clk_Int,INull_In,GlobalHold_N, RD_Int_In, 
                                NullEn_fck, Null_Int_ck0, DMA_Mode)
    begin
        if Clk_Int = '0' then
          if DMA_Mode = '1' then 
            Null_Int_ck0_loc <= '0';
          elsif GlobalHold_N = '1' and RD_Int_In = '1' then
            Null_Int_ck0_loc <= INull_In;
          elsif GlobalHold_N = '1' and RD_Int_In = '0' then
            Null_Int_ck0_loc <= INull_In and NullEn_fck;
          end if;
        end if;
    end process;
        
    Null_Int_ck0 <= Null_Int_ck0_loc or MexcFirAccLdSto_fck;


    INullSync: process
    begin
      wait until Clk_Int'event and Clk_Int = '1';      
              
        if DMA_Mode = '1' then 
          Null_Int_loc1_rck <= '0';
        elsif GlobalHold_N = '1' and RD_Int_In = '1' then
          Null_Int_loc1_rck <= INull_In;
        elsif GlobalHold_N = '1' and RD_Int_In = '0' then
          Null_Int_loc1_rck <= INull_In and NullEn;
        end if;
                                       
    end process;

    Null_Int_loc_rck <= Null_Int_loc1_rck or MexcFirAccLdSto_drck;
    
    Null_Int_rck     <= Null_Int_loc1_rck or MexcFirAccLdSto_drck;


    --The store access following an erronous load access during LdSto is aborted
    MexcFirAccLdStoGen: process(Clk_Int,Reset_Int_N)
    begin
    
        if Reset_Int_N = '0' then
            MexcFirAccLdSto_rck <= '0';
        elsif Clk_Int'event and Clk_Int = '1' then 
              
          if LDSTO_Int_In = '1' and S_Mx = '1' and Rd_Int_In = '1' then 
            MexcFirAccLdSto_rck <= '1';
          elsif S_SSW1 = '1' or  S_St1 = '1' or S_StEcAc = '1' or S_StWSNN = '1' then
            MexcFirAccLdSto_rck <= '0';
          end if;
        end if;
                                       
    end process;


    FckGen: process(Clk_Int,Reset_Int_N)
    begin
    
        if Reset_Int_N = '0' then
            MexcFirAccLdSto_fck <= '0';
        elsif Clk_Int'event and Clk_Int = '0' then 
            MexcFirAccLdSto_fck <= MexcFirAccLdSto_rck;
        end if;
                                       
    end process;

    rckGen: process(Clk_Int,Reset_Int_N)
    begin
    
        if Reset_Int_N = '0' then
            MexcFirAccLdSto_drck <= '0';
        elsif Clk_Int'event and Clk_Int = '1' then 
            MexcFirAccLdSto_drck <= MexcFirAccLdSto_fck;
        end if;
                                       
    end process;


    
----------------------------------------------------------------------------
--BusRdy_N latching
----------------------------------------------------------------------------


    BusRdy_In_N  <= BusRdy_In_loc_N and
                    not (Null_Int_loc_rck and not MHold_WS_N_rck);
    
        
    
    BusRdy_NLatch: process(Clk_Int,BusRdy_In_N)
    begin
        if Clk_Int = '0' then
            BusRdy_In_N_ck0 <= BusRdy_In_N;
        end if;
    end process;
    
   
----------------------------------------------------------------------------
-- Combinatorial logic generated  by FSM state decoding
----------------------------------------------------------------------------
   
    process(S_Idle, 
            S_BHoldIn, S_BHold1, S_BHoldCPU, 
            S_DMAMx1, S_DMAMx2,S_DMAMxOut, 
            S_DMAWaitAS, S_DMAOut1, S_DMAOut,  
            S_Mx, S_MxSt,
            S_MLdOut, 
            S_LdEcAc, S_LdEcAc1, 
            S_LdWSNN, 
            S_LdIO, 
            S_LdOut, 
            S_LdDMAIn1, S_LdDMAIn1_rck, 
            S_LdDMAIn2, 
            S_St1, 
            S_StEcAc, S_StEcAc1, 
            S_StWSNN, S_StWSNN1, 
            S_StOut,
            S_LdR81Decod, S_LdR81, S_LdR8WSNN1, S_LdR82, S_LdR8WSNN2, 
            S_LdR83, S_LdR8WSNN3, S_LdR84, S_LdR8WSNN4, S_LdR84_rck,
            S_LdR8Out, S_LdR8Out_rck, 
            S_SSW1, S_SSW2, S_SSW3, S_SSWWSNN, S_SSW4, S_SSWWSNNWR, 
            S_SSWOut , 
            DMA_Mode_fck, RAM_Access, ROM_Access, IO_Access, IO_Access_fck, Exch_Access,
            GeneralExt_Access, Register_Access,
            MexcVar_fck, DPar_Error_fck, Null_Int_fck, PROM8_In_N, MexcVar_loc,
            NCError_fck, CError_fck, Size_Int_In, ALsb_DFF, BTOToFSM, FStCyDoubleWord_fck, 
            DMAMxEn)
            
        function EDACByteSel(Size,A: Std_Logic_Vector(1 downto 0))
                         return Std_Logic_Vector is

            variable ByteSel: Std_Logic_Vector(3 downto 0);
        begin

            if Size = "00" then
                 if A = "00"  then
                    ByteSel := "1000";
                 elsif A = "01"  then
                    ByteSel := "0100";
                 elsif A = "10"  then
                    ByteSel := "0010";
                 elsif A = "11"  then
                    ByteSel := "0001";
                 end if;
            else
                 if A = "00"  then
                    ByteSel := "1100";
                 else
                    ByteSel := "0011";
                 end if;
            end if;
            
            return ByteSel;
        end EDACByteSel;
            
    begin
        
         
    if S_BHoldIn   = '1' or 
       S_BHold1    = '1' or 
       S_BHoldCPU  = '1' or       
       S_DMAWaitAS = '1' or       
       S_DMAMx1    = '1' or       
       S_DMAMx2    = '1' or       
       S_DMAMxOut  = '1' or       
       S_DMAOut1   = '1' or       
       S_DMAOut    = '1' or       
       S_LdDMAIn1  = '1' or       
       S_LdDMAIn2  = '1' or       
       S_StOut     = '1' or       
       S_SSWOut    = '1' or       
       S_MxDE      = '1' or       
       S_MxSt      = '1' or       
       S_Mx        = '1' then  
      MexcVarSmple  <= '0';
    else      
      MexcVarSmple  <= '1';   
    end if;
    
        
    if S_St1     = '1' or      
       S_StWSNN  = '1' or      
       S_StWSNN1 = '1' or      
       S_StEcAc  = '1' or      
       (S_SSW1   = '1' and DMA_Mode_fck = '0') then 
      Wr_Int_N_fck  <= '0';   
    else      
      Wr_Int_N_fck  <= '1';   
    end if;


    if S_MLdOut  = '1' or 
       (S_LdOut  = '1' and IO_Access_fck = '0') or       
       S_LdIO    = '1' or       
       S_DMAMx1  = '1' or       
       S_St1     = '1' or       
       S_LdR8Out = '1' then      
      DRdy_Out_N_loc  <= not DMA_Mode_fck;
    else      
      DRdy_Out_N_loc  <= '1';   
    end if;

    
    if S_MLdOut   = '1' or
       (S_LdOut   = '1' and IO_Access_fck = '0') or
       S_LdIO     = '1' or
       S_MxDE     = '1' or 
       S_St1      = '1' or
       (S_SSW4    = '1' and DMA_Mode_fck = '0') or       
       S_LdR8Out  = '1' then       
      MHold_Err_N_En  <= '1';
    else      
      MHold_Err_N_En  <= '0';
    end if;
  
    
    if S_Mx   = '1' or       
       S_MxSt = '1' then       
      MHold_Err_N_rst  <= '1';
    else      
      MHold_Err_N_rst  <= '0';
    end if;
  

    if S_LdEcAc  = '1' or
       S_LdEcAc1 = '1' or
       S_LdWSNN  = '1' then       
      LoadOp  <= '1';
    else      
      LoadOp  <= '0';   
    end if;

    
    if S_LdEcAc     = '1' or
       S_LdEcAc1    = '1' or
       S_LdWSNN     = '1' or 
       S_LdIO       = '1' or 
       S_StWSNN     = '1' or 
       S_StWSNN1    = '1' or 
       S_StEcAc     = '1' or 
       S_StEcAc1    = '1' or 
       (S_StOut     = '1' and FStCyDoubleWord_fck = '1' and Register_Access = '0') or
       S_SSW1       = '1' or        
       S_SSW2       = '1' or       
       S_SSW3       = '1' or       
       S_SSWWSNN    = '1' or       
       S_SSWWSNNWR  = '1' or       
       S_LdR81Decod = '1' or       
       S_LdR81      = '1' or       
       S_LdR8WSNN1  = '1' or       
       S_LdR82      = '1' or       
       S_LdR8WSNN2  = '1' or       
       S_LdR83      = '1' or       
       S_LdR8WSNN3  = '1' or       
       S_LdR84      = '1' or       
       S_LdR8WSNN4  = '1' then       
      MHold_WS_N  <= '0';
    else      
      MHold_WS_N  <= '1';
    end if;


    if S_LdEcAc    = '1' or
       S_LdEcAc1   = '1' or
       S_LdWSNN    = '1' or 
       S_StWSNN1   = '1' or 
       S_StEcAc1   = '1' or       
       (S_SSWWSNN   = '1' and DMA_Mode_fck = '0') or    
       (S_SSWWSNNWR = '1' and DMA_Mode_fck = '0') or       
       S_LdR8WSNN1 = '1' or       
       S_LdR8WSNN2 = '1' or       
       S_LdR8WSNN3 = '1' or       
       S_LdR8WSNN4 = '1' then       
      BusTOEnable_loc     <= '1';
    else      
      BusTOEnable_loc     <= '0';
    end if;

    
    if S_LdEcAc   = '1' or
       S_LdEcAc1  = '1' or
       S_LdWSNN   = '1' or       
       S_LdDMAIn1 = '1' or       
       (S_LdOut   = '1' and IO_Access_fck = '1' and DMA_Mode_fck = '1') or       
       S_LdIO     = '1' then       
      DParIOEn_N_loc  <= '0';
    else      
      DParIOEn_N_loc  <= '1';   
    end if;


    if S_LdWSNN     = '1' or       
       S_SSWWSNN    = '1' then       
      ForceNonMemDataLE_loc  <= '1';   
    else      
      ForceNonMemDataLE_loc  <= '0';   
    end if;


    if (S_LdOut  = '1' and IO_Access_fck = '0') or
        S_LdIO    = '1' or
        S_SSW3    = '1' then       
      MemDataLE  <= '0';   
    else      
      MemDataLE  <= '1';   
    end if;

    
    if S_LdDMAIn1  = '1' or 
       S_StOut     = '1' or    
       S_SSWOut    = '1' or          
       S_DMAMx2    = '1' or          
       S_MxSt      = '1' or          
       S_Mx        = '1' then    
      MexcVarRst  <= '1';
    else      
      MexcVarRst  <= '0';   
    end if;


    if S_StOut     = '1' or    
       S_SSWOut    = '1' or    
       S_MxSt      = '1' then    
      DPar_Error_Rst  <= '1';
    else      
      DPar_Error_Rst  <= '0';   
    end if;


    if S_LdDMAIn1  = '1' or 
       (S_StOut    = '1' and DMA_Mode_fck = '1') or    
       S_DMAMx2    = '1' or          
       S_MxSt      = '1' or          
       S_Mx        = '1' then    
      SFSRTmp_UD  <= '1';
    else      
      SFSRTmp_UD  <= '0';   
    end if;

        
    if S_Idle     = '1'  or 
       S_LdEcAc   = '1'  or       
       S_LdEcAc1  = '1'  or       
       S_StEcAc   = '1'  or       
       S_StEcAc1  = '1'  or       
       (S_SSW2    = '1' and DMA_Mode_fck = '0') or       
       (S_SSW3    = '1' and DMA_Mode_fck = '0') or       
       S_LdR81    = '1'  or       
       S_LdR82    = '1'  or       
       S_LdR83    = '1'  then       
      WScounterLoadEN_loc  <= '1';
    else      
      WScounterLoadEN_loc  <= '0';   
    end if;
    

    if S_Mx = '1' or DMAMxEn = '1' or S_MxSt = '1' then       
      Mexc_Out_N_loc  <= not (MexcVar_fck or NCError_fck or DPar_Error_fck);
    else      
      Mexc_Out_N_loc  <= '1';
    end if;

   
    if (S_SSW1     = '1' and DMA_Mode_fck = '0') or      
       S_StEcAc    = '1' or      
       S_St1       = '1' or      
       S_StWSNN    = '1' then     
      IUDataLE  <= '1';
    else      
      IUDataLE  <= '0';
    end if;
   
          
    if (S_SSW2    = '1' and DMA_Mode_fck = '0') or
       (S_SSWWSNN = '1' and DMA_Mode_fck = '0') then      
      OE_SSW_N_fck  <= '0';
    else      
      OE_SSW_N_fck  <= '1';
    end if;
   
   
    if S_SSW3 = '1' and DMA_Mode_fck = '0' then      
      SSWRead     <= '0';
      CErrorMask  <= '1';      
    else      
      SSWRead     <= '1';
      CErrorMask  <= '0';   
    end if;
    
   
    if S_SSW3      = '1' or      
       S_SSWWSNNWR = '1' or      
       S_SSW4      = '1' then      
      StoreByte_Reg_Read_N_loc  <= '0';
    else      
      StoreByte_Reg_Read_N_loc  <= '1';
    end if;
     
   
    if S_SSW2      = '1' or  
       S_SSW3      = '1' or  
       S_SSWWSNN   = '1' or  
       S_SSW4      = '1' or  
       S_SSWWSNNWR = '1' then      
      ByteSel_stsw_loc  <= EDACByteSel(Size_Int_In,ALsb_DFF);
    else      
      ByteSel_stsw_loc  <= "1111";
    end if;

   
    if (S_SSW4      = '1' and DMA_Mode_fck = '0') or  
       (S_SSWWSNNWR = '1' and DMA_Mode_fck = '0') or      
       S_StWSNN     = '1' or 
       S_StWSNN1    = '1' or 
       S_StEcAc     = '1' or
       S_StEcAc1    = '1' or   
       (S_St1       = '1' and Register_Access = '0') then       
      CBEn_N_fck  <= '0';
    else      
      CBEn_N_fck  <= '1';
    end if;
    
   
    if S_LdR84 = '1' then      
      MDS_WS_LdR84_N  <= '0';
    else      
      MDS_WS_LdR84_N  <= '1';
    end if;


    if S_LdR81 = '1' then      
      ByteSel_ldr8  <= "1000";
    elsif S_LdR82 = '1' then      
      ByteSel_ldr8  <= "0100";
    elsif S_LdR83 = '1' then      
      ByteSel_ldr8  <= "0010";
    elsif S_LdR84 = '1' then      
      ByteSel_ldr8  <= "0001";
    else
      ByteSel_ldr8  <= "1111";

    end if;


    if S_LdR81 = '1' or S_LdR8WSNN2 = '1' then      
      BA_Out_fck  <= "01";
    elsif S_LdR82 = '1' or S_LdR8WSNN3 = '1' then      
      BA_Out_fck  <= "10";
    elsif S_LdR83 = '1' or S_LdR8WSNN4 = '1' then      
      BA_Out_fck  <= "11";
  --  elsif S_LdR84 = '1' then      
    else      
      BA_Out_fck  <= "00";
    end if;

    
    if S_LdR81 = '1' or
       S_LdR82 = '1' or
       S_LdR83 = '1' or
       S_LdR84 = '1' then
      ROM8Reg_LE  <= '1';
    else
      ROM8Reg_LE  <= '0';
    end if;


    if S_LdR84        = '1' or 
       S_LdR84_rck    = '1' or 
       (S_LdDMAIn1    = '1' and IO_Access_fck = '0') or 
       S_LdDMAIn1_rck = '1' then      
      OE_Mask  <= '1';
    elsif 
       S_LdR8Out      = '1' or 
       S_LdR8Out_rck  = '1' or 
       (S_LdOut       = '1' and IO_Access_fck = '0') then      
      OE_Mask  <= DMA_Mode_fck;
    else
      OE_Mask  <= '0';
    end if;


    if S_LdR84     = '1' or 
       S_LdDMAIn1  = '1' then      
      DBEnLdR8Window_N  <= '0';
    elsif 
       S_LdR8Out   = '1' or 
       (S_LdOut    = '1' and IO_Access_fck = '0') then      
      DBEnLdR8Window_N  <= not DMA_Mode_fck;
    else
      DBEnLdR8Window_N  <= '1';
    end if;


    if S_LdR84      = '1' or 
       (S_LdDMAIn1  = '1' and Register_Access = '0') then      
      ROM8Reg_N_loc  <= '0';
    elsif 
       S_LdR8Out    = '1' then            
      ROM8Reg_N_loc  <= not DMA_Mode_fck;
    else
      ROM8Reg_N_loc  <= '1';
    end if;


    if S_BHoldIn = '1' or
       S_BHold1  = '1' then      
      LastCycle  <= '1';
    else
      LastCycle  <= '0';
    end if;


    if S_DMAOut = '1' then      
      ResetDMAInPrgs  <= '1';
    else
      ResetDMAInPrgs  <= '0';
    end if;
    
    
    if S_MLdOut   = '1' or      
       S_DMAOut1  = '1' or      
       (S_LdOut   = '1' and IO_Access_fck = '0') or      
       S_LdDMAIn1 = '1' or      
       S_StOut    = '1' or      
       S_LdR84    = '1' or      
       S_SSW2     = '1' then      
      ResetMemBEnWindow  <= '1';
    else
      ResetMemBEnWindow  <= '0';
    end if;
        
    end process;
              
            
----------------------------------------------------------------------------   
-- Miscellaneous combinatorial logic
----------------------------------------------------------------------------      
   PartOfDOE_N_Gen2: process(S_SSW3, S_SSWWSNN, S_SSW4, S_SSWWSNNWR)
   begin
   
    if S_SSW3      = '1' or      
       S_SSWWSNN   = '1' or      
       S_SSW4      = '1' or      
       S_SSWWSNNWR = '1' then      
      DOE_Int_N_fck  <= '1';
    else      
      DOE_Int_N_fck  <= '0';
    end if;
    
   end process;

   
   PartOfDOE_N_Gen1: process(DOE_Int_N_rck, BusRequest_Int, DOE_Int_N_fck)
   begin
      PartOfDOE_N <= DOE_Int_N_rck or BusRequest_Int or DOE_Int_N_fck; 
   end process;

   
   DOE_Out_N            <= S_SSW2 or PartOfDOE_N; 
                      
   AOE_Out_N            <= BusRequest_Int;
                   
   COE_Out_N            <= BusRequest_Int;

   DRdy_Out_N           <= DRdy_Out_N_loc_rck;
                
   BusTOEnable          <= BusTOEnable_loc or ((BusTimeOut_fck or MexcVar_fck) and not MexcVarRst);
    
   BTOToFSM             <= BusTimeOut_fck;
   
   StoreByte_Reg_Read_N <= StoreByte_Reg_Read_N_rck;

   CorrectedData_Reg_N  <= not (CErrorDuringLd_fck_rck) and 
                           not ((S_LdOut_rck or 
                                ((S_LdDMAIn1 or S_LdDMAIn1_rck) and 
                                 not Register_Access and not B_LoadROM8)) and DMAInPrgs_loc); 
    
   CErrorDuringLd_fck   <= CError_fck and not CErrorMask_rck;

   ByteSel              <= ByteSel_stsw_rck when SSWIdentifier_rck = '1' else 
                           ByteSel_ldr8;
                                      
   Mexc_Out_N           <= Mexc_Out_N_loc;
    
   MemData_Valid        <= MemData_Valid_fck and not(BusTimeOut or MexcVar_loc);
    
   MemData_Valid_Ld_qual <= (not(BusTimeOut or MexcVar_loc) and not SSWIdentifier_fck);
     
   MemData_Valid_Ld     <= MemData_Valid_fck and MemData_Valid_Ld_qual; 
   
   
   ErrRck_Gen: process(MexcVar_loc, CError_rck, NCError_rck)
   begin
     Error_rck <= MexcVar_loc or CError_rck or NCError_rck;
   end process;
   
   
   DoNotRdUART    <= CError_fck or NCError_fck or DPar_Error_fck or Error_rck or Null_Int_ck0;
  
   DMAInPrgs      <= DMAInPrgs_loc;
                                             
   NewCycleOOR    <= Reset_Out_N_fck1 and not Reset_Out_N_fck2;
   
      
   NCBull_Gen: process(NewCycleOOR, NewCycle_loc)
   begin
     NewCycle_Bull <= NewCycleOOR or NewCycle_loc;
   end process;
   
   
   NewCycle_Int   <= NewCycle_Bull or 
                     (not (WriteAccess_N and WriteAccess_N_fck) and Null_Int_ck0);
   
   NewCycle       <= NewCycle_Int;
            
   NewCycleForSDW <= NewCycleOOR or NewCycle_loc;
   
   ALE_Out_N      <= (not NewCycle_Bull) and 
                     ((WriteAccess_N and WriteAccess_N_fck) or DMA_Mode_fck);
 
   MExcReqDetect  <= MExcRequest and not MExcRequest_fck;
   
   FHold_N        <= ((ExtHold_In_N and ExtCCV_In) or DMA_Mode) and 
                     (FHold_N_fck or Rd_Int_In);
   
   StrobeGate     <= S_BHoldCPU or S_BHoldCPU_rck or Null_Int_ck0 or 
                     S_MxDE or S_Mx;
   
   ForceNonMemDataLE  <=  ForceNonMemDataLE_fck2 or ForceNonMemDataLE_loc;
                                              
   DDir_Out       <= (not Rd_Int_In and OE_SSW_N_rck);                                       

   ---------------------------------------------------------------------------------------------  
   -- MemBen_N , IOBen_N generation 
   ---------------------------------------------------------------------------------------------     
   fck2Sync1: process(Reset_Out_N, Clk2_Int)
   begin
     if Reset_Out_N = '0' then
         S_LdR84_fck2   <= '0';
     elsif Clk2_Int'event and Clk2_Int = '0' then
         S_LdR84_fck2  <= S_LdR84;
     end if;    
   end process;


   Reset_Out_N_Sync: process(Reset_Out_N, Clk_Int)
   begin
     if Reset_Out_N = '0' then
         Reset_Out_N_del_rck    <= '0';
     elsif Clk_Int'event and Clk_Int = '1' then
         Reset_Out_N_del_rck  <= Reset_Out_N;
     end if;    
   end process;
   
   NoBenDueToMexcGen : process(Reset_Out_N, Clk_Int)
   begin
          
     if Reset_Out_N = '0' then
         NoBenDueToMexc_N <= '1';
          
     elsif Clk_Int'event and Clk_Int = '1' then 
      
        if MexcVarRst = '1' or Null_Int_ck0 = '1' then 
            NoBenDueToMexc_N <= '1';
        elsif MexcVarSmple = '1' then
            NoBenDueToMexc_N <= NoBenDueToMexc_N and not Mexc_Int;
        end if;
        
     end if;
   end process;

   Rst_IU_MemBEnBase_N <= Reset_Out_N_del_rck and BHold_Out_N and S_LdIO_N_rck;
   
   ----
   SynthBull1Gen : process(NoBenDueToMexc_N, S_LdR84_fck2, IU_MemBEnBase_N_fck2, CError_fck_fck2)
   begin
          
     if NoBenDueToMexc_N = '1' and S_LdR84_fck2 = '0' and 
        IU_MemBEnBase_N_fck2 = '1' and CError_fck_fck2 = '0' then
       SynthBull1 <= '1';
     else        
       SynthBull1 <= '0';
     end if;
   end process;

   --------  
   IU_MemBEnBase_N_fck2Gen: process(Rst_IU_MemBEnBase_N, Clk2_Int)
   begin
     if Rst_IU_MemBEnBase_N = '0' then
         IU_MemBEnBase_N_fck2    <= '1';
         
     elsif Clk2_Int'event and Clk2_Int = '0' then
       if Register_Access = '0' and SynthBull1 = '1' then
         IU_MemBEnBase_N_fck2  <= '0';
       elsif EndOfCycle = '1' or Null_Int_ck0 = '1' then 
         IU_MemBEnBase_N_fck2  <= '1';
       end if;  
          
     end if;    
   end process;

   IU_MemBEnBase_N_rck2Gen: process(Reset_Out_N, Clk2_Int)
   begin
     if Reset_Out_N = '0' then
      IU_MemBEnBase_N_rck2  <= '1';
     elsif Clk2_Int'event and Clk2_Int = '1' then
      IU_MemBEnBase_N_rck2  <= IU_MemBEnBase_N_fck2;
     end if;    
   end process;
      
   IU_MemBEnBase_N  <= IU_MemBEnBase_N_fck2 and IU_MemBEnBase_N_rck2;
         
   --------
   MemBEnWindowGen: process(Reset_Out_N, Clk_Int)
   begin
     if Reset_Out_N = '0' then
         MemBEnWindow    <= '0';
         
     elsif Clk_Int'event and Clk_Int = '1' then
       if DMAAS_In = '1' then
         MemBEnWindow  <= '1';
       elsif ResetMemBEnWindow = '1' then 
         MemBEnWindow  <= '0';
       end if;  
          
     end if;    
   end process;

   SynthBull2Gen : process(B_StoreSubWord, MemBEnWindow, DMA_MemBEnBase_N_fck2)
   begin
          
     if B_StoreSubWord = '0' and MemBEnWindow = '1' and DMA_MemBEnBase_N_fck2 = '1'  then
       SynthBull2 <= '1';
     else        
       SynthBull2 <= '0';
     end if;
   end process;


   --------
   DMA_MemBEnBase_N_fck2Gen: process(Reset_Out_N_del_rck, Clk2_Int)
   begin
     if Reset_Out_N_del_rck = '0' then
         DMA_MemBEnBase_N_fck2    <= '1';
         
     elsif Clk2_Int'event and Clk2_Int = '0' then
       if Register_Access = '0' and SynthBull2 = '1' then
         DMA_MemBEnBase_N_fck2  <= '0';
       elsif MemBEnWindow = '0' or EndOfCycle = '1' then 
         DMA_MemBEnBase_N_fck2  <= '1';
       end if;  
          
     end if;    
   end process;

   DMA_MemBEnBase_N_rck2Gen: process(Reset_Out_N, Clk2_Int)
   begin
     if Reset_Out_N = '0' then
       DMA_MemBEnBase_N_rck2  <= '1';
     elsif Clk2_Int'event and Clk2_Int = '1' then
       DMA_MemBEnBase_N_rck2  <= DMA_MemBEnBase_N_fck2;
     end if;    
   end process;
      
   DMA_MemBEnBase_N  <= DMA_MemBEnBase_N_fck2 and DMA_MemBEnBase_N_rck2;
   
         
   --------MemBEn_N generation
   MemBEnCond_Gen: process(RAM_Access, ROM_Access)
   begin
     MemBenCond <= not(RAM_Access or ROM_Access) ;
   end process;
   

   MemBEn_N_Gen: process(BHold_Out_N, MemBenCond, IU_MemBEnBase_N, DMA_MemBEnBase_N)
   begin
     if BHold_Out_N = '1' then
        MemBEn_Out_N_loc <= IU_MemBEnBase_N or MemBenCond;
     else
        MemBEn_Out_N_loc <= DMA_MemBEnBase_N or MemBenCond;
     end if;    
   end process;
   
   MemBEn_Out_N  <= MemBEn_Out_N_loc;
   
   RAMBEn_N_Gen: process(BHold_Out_N, RAM_Access, IU_MemBEnBase_N, DMA_MemBEnBase_N)
   begin
     if BHold_Out_N = '1' then
        RAMBEn_Out_N  <= IU_MemBEnBase_N or not RAM_Access;
     else
        RAMBEn_Out_N  <= DMA_MemBEnBase_N or not RAM_Access;
     end if;    
   end process;
   
   ROMBEn_N_Gen: process(BHold_Out_N, ROM_Access, IU_MemBEnBase_N, DMA_MemBEnBase_N)
   begin
     if BHold_Out_N = '1' then
        ROMBEn_Out_N  <= IU_MemBEnBase_N or not ROM_Access;
     else
        ROMBEn_Out_N  <= DMA_MemBEnBase_N or not ROM_Access;
     end if;    
   end process;

  
   ------OE_N generation
    OE_NGen: process(Reset_Out_N, Clk2_Int)
    begin
      
      if Reset_Out_N = '0' then 
          PartOfOE_N_fck2  <= '1';
                        
      elsif Clk2_Int'event and Clk2_Int = '0' then  
       if SSWRead = '0' then
          PartOfOE_N_fck2  <= '1';
       else
          PartOfOE_N_fck2  <= OE_SSW_N_rck;                                       
       end if;
       
      end if; 
       
    end process;
    
    rck2Sync: process(Reset_Out_N, Clk2_Int)
    begin
                   
    if Reset_Out_N = '0' then 
        PartOfOE_N_rck2   <= '1';
    elsif Clk2_Int'event and Clk2_Int = '1' then
        PartOfOE_N_rck2   <= PartOfOE_N_fck2;
    end if;
     
   end process;

   OE_SSW_N  <= (PartOfOE_N_fck2 and PartOfOE_N_rck2); 
                                          
   OECond_Gen: process(Rd_Int_In, PartOfOE_N_fck2, PartOfOE_N_rck2)
   begin
      OECond  <= (not Rd_Int_In and PartOfOE_N_fck2 and PartOfOE_N_rck2); 
   end process;
   
   OE_N_Gen: process(BHold_Out_N, OECond, IU_MemBEnBase_N, DMA_MemBEnBase_N)
   begin
     if BHold_Out_N = '1' then
        OE_Out_N  <= IU_MemBEnBase_N or OECond;
     else
        OE_Out_N  <= DMA_MemBEnBase_N or OECond;
     end if;    
   end process;
                                             
                                              
   --------IOBEn_N generation

   IOBEnCond_Gen: process(IO_Access, Exch_Access, GeneralExt_Access)
   begin
     IOBenCond <= not(IO_Access or Exch_Access or GeneralExt_Access);
   end process;
   

   IOBEn_N_Gen: process(BHold_Out_N, IOBenCond, IU_MemBEnBase_N, DMA_MemBEnBase_N)
   begin
     if BHold_Out_N = '1' then
        IOBEn_Out_N_loc  <= IU_MemBEnBase_N or IOBenCond;
     else
        IOBEn_Out_N_loc  <= DMA_MemBEnBase_N or IOBenCond;
     end if;    
   end process;
   
   IOBEn_Out_N  <= IOBEn_Out_N_loc;
   
   ---------Checking (no functionality)
   ctrl_XBen2_N : process (IOBEn_Out_N_loc, MemBEn_Out_N_loc, OE_loc_N)
   begin
  
   OE_N_v          <= OE_loc_N after 1 ns;
   IOBEn_Out_N_v   <= IOBEn_Out_N_loc after 1 ns;
   MemBEn_Out_N_v  <= MemBEn_Out_N_loc after 1 ns;
   
    if  (OE_N_v = '0'  and IOBEn_Out_N_v = '1' and MemBEn_Out_N_v = '1')  then
      assert FALSE
      report " WARNING MemBEn_Out_N and OE_loc_N or etc asserted "
      severity warning;
    end if;
                    
   end process;
  
   ---------------------------------------------------------------------------------------------  
    MexcVarGen : process(Reset_Out_N, Clk_Int)
    begin
          
      if Reset_Out_N = '0' then
          MexcVar_loc <= '0';
          
      elsif Clk_Int'event and Clk_Int = '1' then 
      
        if (MexcVarRst = '1' or Null_Int_ck0 = '1') and MExcReqDetect = '0' then 
            MexcVar_loc <= '0';
        elsif DPar_Error_fck  = '1' or NCError_fck = '1' or MExcReqDetect = '1' then
            MexcVar_loc <= '1';
        elsif MexcVarSmple = '1' then
            MexcVar_loc <= MexcVar_loc or Mexc_Int;
        end if;
        
      end if;
    end process;
                
    MexcVar     <= MexcVar_loc;  
    
    MExcVarFSM2 <= MexcVar_loc; 
    
    MExcVarFoo  <= MexcVar_loc;
    
   ---------------------------------------------------------------------------------------------
   -- MHold_N generation
   ---------------------------------------------------------------------------------------------
   MHold_WS_NGen: process(Reset_Out_N, Clk_Int)
    begin
        
     if Reset_Out_N = '0' then 
        MHold_WS_N_rck  <= '1';      
     
     elsif Clk_Int'event and Clk_Int = '1' then  
        -- The AND with S_Sxx is required due to load with WS, other the FSM has to leave
        -- the WS state as soon as Null is received                    
        if (Null_Int_ck0 = '1' and (S_StWSNN = '1' or 
                                    S_StEcAc = '1'or S_SSW1 = '1'))then 
           MHold_WS_N_rck  <= '1';      
        else     
           MHold_WS_N_rck  <= MHold_WS_N;      
        end if;
         
      end if;
    end process;

   ------
   MHold_err_NGen : process(Reset_Out_N, Clk_Int)
   begin
       if Reset_Out_N = '0' then
          MHold_Err_N <= '1';
          
       elsif Clk_Int'event and Clk_Int = '1' then 
                            
         if MHold_Err_N_rst = '1' or DMAInPrgs_fck = '1' or Null_Int_ck0 = '1' then
            MHold_Err_N <= '1';
            
         -- NCError_fck due to SSW
         elsif ((MexcVar_loc = '1' or Mexc_Int = '1' or DPar_Error_fck  = '1' or 
                 MexcRequest = '1' or S_MxDE = '1' or NCError_fck = '1') and
                 MHold_Err_N_En = '1' and MexcVarRst = '0') then 
            MHold_Err_N <= '0';                             
         end if;
         
       end if;
   end process;

   ------            
   MHold_Out_NGen: process(MHold_WS_N_rck, MHold_Err_N, DMAInPrgs_loc, Null_Int_loc_rck,
                           S_MxDE)
   begin
   -- S_MxDE because data error are removed on fck2 after the first ck-,    
      MHold_Out_N_WOEE <= ((MHold_WS_N_rck and 
                           ((MHold_Err_N) or Null_Int_loc_rck)) or 
                           DMAInPrgs_loc) and not S_MxDE;
   end process;
   
         
   ---------------------------------------------------------------------------------------------
   MDS_Out_N_Gen: process(MDS_WS_LdR84_N_rck, MHold_Err_N, MDS_WS_N_ld, DMAInPrgs_loc,  
                          Null_Int_loc_rck, S_MxDE_rck)
   begin
      MDS_Out_N  <= (MDS_WS_LdR84_N_rck and MHold_Err_N and MDS_WS_N_ld and 
                     not S_MxDE_rck) or Null_Int_loc_rck or DMAInPrgs_loc; 
   end process;
   

   ---------------------------------------------------------------------------------------------  
   DBEn_N_locGen: process(Reset_Out_N, B_MECLoad_fck2, B_StoreSubWord_fck2,
                          RdRst, DMAInPrgs_loc, DMA_OE_Window, IO_Access,
                          DBEn_fck, DBEn_rck, Clk_aux, DBEnLdR8Window_N_fck2, 
                          S_LdDMAIn1, S_LdDMAIn1_rck)
    begin
      if Reset_Out_N = '0' or RdRst= '1' or
         (DMAInPrgs_loc = '1' and DMA_OE_Window = '0') then 
          DBEn_Int    <= '0';
      else  
          DBEn_Int    <=  (not DBEnLdR8Window_N_fck2 and not Clk_aux) or 
                          (B_MECLoad_fck2 and not Clk_aux) or
                          (B_StoreSubWord_fck2 and (DBEn_fck or DBEn_rck)) or 
                          ((S_LdDMAIn1 or S_LdDMAIn1_rck) and not IO_Access);
      end if;    
    end process;
  
   DBEn_NGen: process(DBEn_Int, CErrorDuringLd_dfck, CErrorDuringLd_dfck_rck)
    begin
      if CErrorDuringLd_dfck = '1' and CErrorDuringLd_dfck_rck = '0' then 
          DBEn    <= '1';
      else 
          DBEn    <= DBEn_Int;
      end if;    
    end process;
  
                   
   ---------------------------------------------------------------------------------------------  
   DParIOEn_NlocGen: process(Reset_Out_N, DMAInPrgs_loc, DMA_OE_Window,  
                             DBEn_fck, DBEn_rck, RdRst, ParityEnable,
                             Clk_aux, DBEnLdR8Window_N_fck2, DParIOEn_N_loc, DParIOEn_N_rck,
                             B_MECLoad_fck2, B_Load_fck2, B_StoreSubWord_fck2)
   begin
     if Reset_Out_N = '0' or RdRst= '1' or
        (DMAInPrgs_loc = '1' and DMA_OE_Window = '0') then 
         DParIOEn_Int <= '0';
        
     else 
         DParIOEn_Int <= (not DBEnLdR8Window_N_fck2 and not (DParIOEn_N_loc and DParIOEn_N_rck and Clk_aux)) or
                         (
                          ((B_MECLoad_fck2) or (B_Load_fck2 and not ParityEnable)) and 
                          not ((DParIOEn_N_loc and DParIOEn_N_rck and Clk_aux))
                          ) or 
                         (B_StoreSubWord_fck2 and (DBEn_fck or DBEn_rck));
                                        
     end if;    
   end process;

  
   DParIOEn_NGen: process(DParIOEn_Int, CErrorDuringLd_dfck, CErrorDuringLd_dfck_rck, 
                          CBEnForDParIOEn)
   begin
     if CErrorDuringLd_dfck = '1' and CErrorDuringLd_dfck_rck = '0' then  
         DParIOEn    <= '1';
     else 
         DParIOEn    <= (DParIOEn_Int or CBEnForDParIOEn);
     end if;    
   end process;
   
      
   CBEnForDParIOEnGen : process(Reset_Out_N, Clk_Int)
   begin          
    if Reset_Out_N = '0' then
      CBEnForDParIOEn <= '0';
    elsif Clk_Int'event and Clk_Int = '1' then          
      if DMA_Mode = '1' and DMAParity_En = '0' then
        CBEnForDParIOEn <=  not CBEn_N_fck;
      else
        CBEnForDParIOEn <=  '0';
      end if;
    end if;
   end process;

   ---------------------------------------------------------------------------------------------  
   CBEnGen : process(Reset_Out_N, Clk_Int)
   begin          
    if Reset_Out_N = '0' then
       CBEn <= '0';
    elsif Clk_Int'event and Clk_Int = '1' then          
       CBEn <=  not CBEn_N_fck and not Null_Int_ck0;
    end if;
   end process;

   ---------------------------------------------------------------------------------------------  
    DMAInPrggeneration : process(Reset_Out_N, Clk_Int)
    begin
     if Reset_Out_N = '0' then
        DMAInPrgs_loc <= '0';
        
     elsif Clk_Int'event and Clk_Int = '1' then
        if DMA_Mode = '1' and DMA_Mode_rck = '0' then
          DMAInPrgs_loc <= '1';
        elsif ResetDMAInPrgs = '1' then
          DMAInPrgs_loc <= '0';
        end if;
     end if;
    end process;

   ---------------------------------------------------------------------------------------------  
    DMA_OE_Valid : process(Reset_Out_N, Clk_Int)
    begin
     if Reset_Out_N = '0' then
        DMA_OE_Window <= '0';
        
     elsif Clk_Int'event and Clk_Int = '1' then
        if DMAAS_In = '1' then
          DMA_OE_Window <= '1';
        elsif DRdy_Out_N_drck = '0' then
          DMA_OE_Window <= '0';
        end if;
     end if;
    end process;

   ---------------------------------------------------------------------------------------------  
    BA_Out_rckGen : process(Reset_Out_N, Clk_Int)
    begin
      if Reset_Out_N = '0' then 
        BA_Out_rck <= "00";
      elsif Clk_Int'event and Clk_Int = '1' then
        BA_Out_rck <= BA_Out_fck;
      end if;
    end process;

   --------------------------------------------------------------------------------------------
    BAMux: process(Rd_Int_In, ALsb_DFF, BA_Out_rck)
    begin
      if Rd_Int_In = '0' then 
        BA_Out <= ALsb_DFF;
      else
        BA_Out <= BA_Out_rck;
      end if;
    end process;

   ---------------------------------------------------------------------------------------------  
   -- Write Strobe Generation
   ---------------------------------------------------------------------------------------------  
   IOWr_Cond1Gen: process(Exch_Access, IO_Access, GeneralExt_Access, 
                         WaitStateLDval)
    begin
    if ((Exch_Access = '1') and (WaitStateLDval /= "0000")) or 
       (WaitStateLDval /= "0000") or 
       (GeneralExt_Access = '1' or IO_Access = '1') then            
        IOWr_Cond1  <= '1';
    else 
        IOWr_Cond1  <= '0';
    end if;
       
    end process;

   IOWr_Cond2Gen: process(Exch_Access, IO_Access, GeneralExt_Access)
    begin
    if Exch_Access = '1' or GeneralExt_Access = '1' or IO_Access = '1' then            
        IOWr_Cond2  <= '1';
    else 
        IOWr_Cond2  <= '0';
    end if;
       
    end process;

   IOWr_Out_N_CKGen: process(S_St1, Exch_Access, Register_Access,
                             IOWr_Out_N_fck, IOWr_Out_N_rck,
                             IOWr_Cond1, IOWr_Cond2, Clk_aux)
    begin
    if IOWr_Cond1 = '1' and IOWr_Cond2 = '1' then            
        IOWr_Out_N_CK  <= IOWr_Out_N_fck and IOWr_Out_N_rck;
    elsif Exch_Access = '1' and (S_St1 = '1' and Register_Access = '0') then        
        IOWr_Out_N_CK  <= Clk_aux;
    else 
        IOWr_Out_N_CK  <= '1';
    end if;
       
    end process;

   ----------  
   MemWr_Out_Con1Gen: process(WaitStateLDval, Ext_Access)
    begin
    if (WaitStateLDval /= "0000") or (Ext_Access = '1') then
        MemWr_Cond1 <= '1';
    else 
        MemWr_Cond1 <= '0';
    end if;
      
    end process;

   MemWr_Out_Cond2Gen: process(RAM_Access, ROM_Access)
    begin
    if (RAM_Access = '1' or ROM_Access = '1') then 
        MemWr_Cond2  <= '1';
    else 
        MemWr_Cond2  <= '0';
    end if;
      
    end process;

   MemWr_Out_N_CKGen: process(S_St1, S_SSW4, Register_Access,
                              MemWr_Out_N_fck, MemWr_Out_N_rck, 
                              MemWr_Cond1, MemWr_Cond2, Clk_aux, DMAInPrgs_loc)
    begin
    if MemWr_Cond1 = '1' and MemWr_Cond2 = '1' then
        MemWr_Out_N_CK <= MemWr_Out_N_fck and MemWr_Out_N_rck;
    elsif ((S_St1 = '1' and Register_Access = '0') or (S_SSW4 = '1' and DMAInPrgs_loc = '0')) and
          (MemWr_Cond2 = '1') then 
        MemWr_Out_N_CK <= Clk_aux;
    else 
        MemWr_Out_N_CK <= '1';
    end if;
      
    end process;
    
   ----------  
    CBWrCond_Gen: process(RAM_Access, ROM_Access, PROM8_In_N, Exch_Access)
    begin
     CBWrCond_N  <= not(RAM_Access or (ROM_Access and PROM8_In_N) or Exch_Access);
    end process;

    
   ----------  
    -- Write_Inhibit contient NCerror et Dparerror ainsi que MexcVar, mais Write_Inhibit 
    -- arrive plus rapidement et dure moins longtemps d'ou les deux.
    -- INull est asserte a 36ns sur ck+
    fck3Sync: process
    begin
    wait until Clk2_Int'event and Clk2_Int = '0';
      if Null_Int_ck0 = '1' then
        MemWr_Out_N <= '1';
        CBWr_Out_N  <= '1';
        IOWr_Out_N  <= '1';
      else
        MemWr_Out_N <= MemWr_Out_N_CK or Write_Inhibit or MexcVar or Wr_En_N;
        CBWr_Out_N  <= CBWrCond_N     or Write_Inhibit or MexcVar or Wr_En_N;
        IOWr_Out_N  <= IOWr_Out_N_CK  or Write_Inhibit or MexcVar or Wr_En_N;      
      end if;                                                  
    end process;


   ---------------------------------------------------------------------------------------------  
    fckSync1: process(Reset_Out_N, Clk_Int)
    begin
                   
     if Reset_Out_N = '0' then               
        DMA_Mode_fck         <= '0';
        MexcVar_fck          <= '0';
        Null_Int_fck         <= '0';
        DMAInPrgs_fck        <= '0';   
        BusTimeOut_fck       <= '0';
        IO_Access_fck        <= '0';
        IO_Area_Int_fck      <= '0';
        Mem_Area_Int_fck     <= '0';
        Ext_Access_fck       <= '0';
        PROM_Area_Out_fck    <= '0';
        MExcRequest_fck      <= '0';
        SSWIdentifier_fck    <= '0';
        NullEn_fck           <= '0';
        CErrorDuringLd_dfck  <= '0';
        FStCyDoubleWord_fck  <= '0';
        FStCyDoubleWord1_fck <= '0';
        WriteAccess_N_fck    <= '1';
        FHold_N_fck          <= '1';
        
      elsif Clk_Int'event and Clk_Int = '0' then
        DMA_Mode_fck         <= DMA_Mode;
        MexcVar_fck          <= MexcVar;
        Null_Int_fck         <= Null_Int_loc_rck;
        DMAInPrgs_fck        <= DMAInPrgs_loc;      
        BusTimeOut_fck       <= BusTimeOut;
        IO_Access_fck        <= IO_Access;
        IO_Area_Int_fck      <= IO_Area_Int;
        Mem_Area_Int_fck     <= Mem_Area_Int;
        Ext_Access_fck       <= Ext_Access;
        PROM_Area_Out_fck    <= PROM_Area_Out;
        MExcRequest_fck      <= MExcRequest;
        SSWIdentifier_fck    <= not DMA_mode and B_StoreSubWord;
        NullEn_fck           <= NullEn;
        CErrorDuringLd_dfck  <= CErrorDuringLd_fck;
        FStCyDoubleWord_fck  <= FStCyDoubleWord;
        FStCyDoubleWord1_fck <= FStCyDoubleWord1;
        WriteAccess_N_fck    <= WriteAccess_N;
        FHold_N_fck          <= ((ExtHold_In_N and ExtCCV_In) or DMA_Mode);
                
     end if;
     
    end process;
        
    
    fckSync2: process
    begin
                   
    wait until Clk_Int'event and Clk_Int = '0';     
        Reset_Out_N_fck1      <= Reset_Out_N;
        Reset_Out_N_fck2      <= Reset_Out_N_fck1;
     
    end process;

   
   ---------------------------------------------------------------------------------------------  
  RCKSync1: process(Reset_Out_N, Clk_Int)
    begin
        
     if Reset_Out_N = '0' then 
        
        CError_rck                <= '0';              
        NCError_rck               <= '0';              
        CErrorMask_rck            <= '1'; --In order to devalidate DBen_N during reset              
        MemWr_Out_N_rck           <= '1';      
        IOWr_Out_N_rck            <= '1';      
        DBEnLdR8Window_N_rck      <= '1';                       
        DRdy_Out_N_loc_rck        <= '1';      
        DRdy_Out_N_drck           <= '1';      
        DParIOEn_N_rck            <= '1';      
        DOE_Int_N_rck             <= '0';   --Normal state is '0'      
        SysBus_Error              <= '0';      
        MDS_WS_LdR84_N_rck        <= '1';      
        Mexc_Out_N_del            <= '1';      
        Wr_Int_N                  <= '1';               
        ROM8Reg_N                 <= '1';      
        StoreByte_Reg_Read_N_rck  <= '1';
        ByteSel_stsw_rck          <= "1111";                  
        SSWIdentifier_rck         <= '0';                  
        OE_SSW_N_rck              <= '1';
        DMA_Mode_rck              <= '0';
        CErrorDuringLd_dfck_rck   <= '0';
        CErrorDuringLd_fck_rck    <= '0';
        S_LdOut_rck               <= '0';
        S_LdR84_rck               <= '0';
        S_LdR8Out_rck             <= '0';
        S_LdIO_N_rck              <= '1';
        S_MxDE_rck                <= '0';
        S_LdDMAIn1_rck            <= '0';
        S_BHoldCPU_rck            <= '0';
        DBEn_rck                  <= '0';              

      elsif Clk_Int'event and Clk_Int = '1' then    
        CError_rck                <= CError_fck;              
        NCError_rck               <= NCError_fck;              
        CErrorMask_rck            <= CErrorMask;              
        MemWr_Out_N_rck           <= MemWr_Out_N_fck;      
        IOWr_Out_N_rck            <= IOWr_Out_N_fck;      
        DBEnLdR8Window_N_rck      <= DBEnLdR8Window_N;                       
        DRdy_Out_N_loc_rck        <= DRdy_Out_N_loc;      
        DRdy_Out_N_drck           <= DRdy_Out_N_loc_rck;      
        DParIOEn_N_rck            <= DParIOEn_N_loc;      
        DOE_Int_N_rck             <= DOE_Int_N_fck;      
        SysBus_Error              <= not BusRdy_In_N and not BusErr_In_N and 
                                     not Null_Int_ck0;      
        MDS_WS_LdR84_N_rck        <= MDS_WS_LdR84_N;      
        Mexc_Out_N_del            <= Mexc_Out_N_loc;      
        Wr_Int_N                  <= Wr_Int_N_fck or Write_Inhibit or 
                                     MECRegSel_N or Null_Int_ck0;               
        ROM8Reg_N                 <= ROM8Reg_N_loc;      
        StoreByte_Reg_Read_N_rck  <= StoreByte_Reg_Read_N_loc;
        ByteSel_stsw_rck          <= ByteSel_stsw_loc;                  
        SSWIdentifier_rck         <= SSWIdentifier_fck;                  
        OE_SSW_N_rck              <= OE_SSW_N_fck;
        DMA_Mode_rck              <= DMA_Mode;
        CErrorDuringLd_dfck_rck   <= CErrorDuringLd_dfck;
        CErrorDuringLd_fck_rck    <= CErrorDuringLd_fck;
        S_LdOut_rck               <= S_LdOut;
        S_LdR84_rck               <= S_LdR84;
        S_LdR8Out_rck             <= S_LdR8Out;
        S_LdIO_N_rck              <= not S_LdIO;
        S_MxDE_rck                <= S_MxDE;
        S_LdDMAIn1_rck            <= S_LdDMAIn1;
        S_BHoldCPU_rck            <= S_BHoldCPU;
        DBEn_rck                  <= DBEn_fck;              
                     
      end if;
    
   end process;

   ---------------------------------------------------------------------------------------------  
    fck2Sync: process
    begin
      wait until Clk2_Int'event and Clk2_Int = '0';

        OE_loc_N_fck2          <= OE_loc_N;
        DBEnLdR8Window_N_fck2  <= DBEnLdR8Window_N_rck;
        B_MECLoad_fck2         <= B_MECLoad;
        B_Load_fck2            <= B_Load;
        B_StoreSubWord_fck2    <= B_StoreSubWord;
        ForceNonMemDataLE_fck2 <= ForceNonMemDataLE_loc;
        CError_fck_fck2        <= CError_fck;
                              
    end process;
    
   ---------------------------------------------------------------------------------------------  
    MDSGenerator: process(Reset_Out_N, Clk_Int)
    begin
      if Reset_Out_N = '0' then
        State <= Idle;
        
      elsif Clk_Int'event and Clk_Int = '1' then
        if DMAInPrgs_loc = '1' or MHold_WS_N = '1' then
          State <= Idle;
            
        else
          case State is
            when Idle    =>
              if LoadOp = '1' then
            
                --exchange access
                if Exch_Access = '1' then
                  if WaitState = "0000" then
                    State <= WaitMDS;
                  else
                    State <= WaitMDS_Exch;
                  end if;
                
                --IO access ready
                elsif (Ext_Access = '0' and IO_Access = '1' and (WaitState = "0001" or WaitState = "0000") and 
                       BusRdy_In_N = '0') or
                      (Ext_Access = '1' and IO_Access = '1' and BusRdy_In_N = '0') then
                  State <= MDSis0;
                
                --IO access not ready 
                elsif (Ext_Access = '0' and IO_Access = '1' and not((WaitState = "0001" or WaitState = "0000") 
                       and BusRdy_In_N = '0')) or
                      (Ext_Access = '1' and IO_Access = '1' and not (BusRdy_In_N = '0')) then
                  State <= WaitMDS;
                
                -- RAM or ROM access ready
                elsif (Ext_Access = '0' and (WaitState = "0001" or WaitState = "0000")) or
                      (Ext_Access = '1' and BusRdy_In_N = '0') then
                  State <= MDSis0;
              
                -- RAM or ROM access not ready
                elsif (Ext_Access = '0' and not(WaitState = "0001" or WaitState = "0000")) or 
                      (Ext_Access = '1' and BusRdy_In_N = '1') then
                  State <= WaitMDS;
                
                end if;
                 
             end if;
           
           when WaitMDS_Exch  =>
              State <= WaitMDS; -- one extra Wait Cycle before WaitMDS, 
                                -- in order to be sure to wait 2 clock cycles before getting BusRdy_In_N
             
           when WaitMDS =>
             if (Ext_Access = '1' and BusRdy_In_N = '0') or
                (IO_Area_Int = '1' and (WaitState = "0001" or WaitState = "0000") and 
                 BusRdy_In_N = '0') or
                (Exch_Access = '1' and WaitState = "0000" and BusRdy_In_N = '0') or
                (Exch_Access = '1' and WaitState = "0001" and BusRdy_In_N = '0' and 
                 WScounterLoadEN = '0') or 
                ((Mem_Area_int = '1' or PROM_Area_Out = '1') and (WaitState = "0001" or WaitState = "0000") and 
                 Ext_Access = '0') then
               State <= MDSis0;
             end if;
           
           when MDSis0  =>
              State <= Idle;
           when others  => NULL;
          
         end case;
        
       end if;
         
      end if;
      
    end process;
    
    -----
    MDS_WS_N_ld <= '0' when State = MDSis0 else
                       '1';

    -----



end Mini_Spec;
       
---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      Address decoder and write protection Unit
-- File name:                  \mec\source\adr_dec.vhd
-- VHDL unit:                  AddressDecoder
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;


  entity ADecBuff is
     port (
          A_In    : in Std_Logic_Vector(9 downto 0);
          A_Out   : buffer Std_Logic_Vector(9 downto 0)
         );

  end ADecBuff;
  
  architecture Mini_Spec of ADecBuff is

  begin

  A_Out <= A_In;

  end Mini_Spec;



library MECLibrary;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.conv_integer;


entity AddressDecoder is
    port ( Reset_Out_N         : in Std_Logic;
           Clk_Int             : in Std_Logic;
           Clk2_Int            : in Std_Logic;
           CPUHalt_Out_N       : in Std_Logic;
           NewCycle            : in Std_Logic;
           RdRst               : in std_logic;
           StrobeGate          : in std_logic;
           
           Size_In             : in Std_Logic_Vector(1 downto 0);
           A_In                : in Std_Logic_Vector(31 downto 2);
           ASI_In              : in Std_Logic_Vector(3 downto 0);
           ASI_Int_In          : in Std_Logic_Vector(3 downto 0);
           WRT_In              : in Std_Logic;
           WRT_Int_In          : in Std_Logic;
           
           DMAAS_In            : in Std_Logic;
           DMA_Mode            : in Std_Logic;
           Prom8_In_N          : in Std_Logic;
           RomWrt_In_N         : in Std_Logic;

           Mem_Cfg_reg         : in Std_Logic_Vector(31 downto 0);
           IO_Cfg_reg          : in Std_Logic_Vector(31 downto 0);
           
           AccessProtEn        : in Std_Logic;
           Reset_Int_N         : in Std_Logic;
           D_Int_In            : in Std_Logic_Vector(31 downto 0);
           DPar_Int_In         : in Std_Logic;
           Wr_Int_N            : in Std_Logic;
          
          
           MemAccess_Violation_Out : out Std_Logic;
           D_MemAcc            : out Std_Logic_Vector(31 downto 0);
           DPar_MemAcc         : out Std_Logic;
           ParErr_MemAcc       : out Std_Logic;
           
           -- Output signals.
           MemCs_Out_N         : out Std_Logic_Vector(9 downto 0);
           ExMCS_Out_N         : out Std_Logic;
           IOSel_Out_N         : out Std_Logic_Vector(3 downto 0);
           RomCs_Out_N         : out Std_Logic;

           Illegal_Address_Out : out Std_Logic;
           AccessType          : out Std_Logic_Vector(3 downto 0);
           Register_Area_Out   : buffer Std_Logic;

           ParityEnable        : out Std_Logic;
           EDACEnable          : out Std_Logic;

           MECControlReg_N     : buffer Std_Logic;
           SWRReg_N            : out Std_Logic;
           PWRDReg_N           : out Std_Logic;
           MemConfigReg_N      : buffer Std_Logic;
           IOConfigReg_N       : buffer Std_Logic;
           WSConfigReg_N       : out Std_Logic;
           MemAccessReg_N      : out Std_Logic;
           IntShapeReg_N       : out Std_Logic;
           IntPendReg_N        : out Std_Logic;
           IntMaskReg_N        : out Std_Logic;
           IntClearReg_N       : out Std_Logic;
           IntForceReg_N       : out Std_Logic;
           WDProgramReg_N      : out Std_Logic;
           WDTDSetReg_N        : out Std_Logic;
           RTC_CountReg_N      : out Std_Logic;
           RTC_ScalerReg_N     : out Std_Logic;
           GPT_A_CountReg_N    : out Std_Logic;
           GPT_A_ScalerReg_N   : out Std_Logic;
           TimerControlReg_N   : out Std_Logic;
           SysFSReg_N          : out Std_Logic;
           FFAReg_N            : out Std_Logic;
           ErrResStatReg_N     : out Std_Logic;
           TestControl_Reg_N   : out Std_Logic;
           GUARTAReg_N         : out Std_Logic;
           GUARTBReg_N         : out Std_Logic;
           GUARTStatusReg_N    : out Std_Logic;
           UARTAReg_N          : out Std_Logic;
           UARTBReg_N          : out Std_Logic;
           UARTStatusReg_N     : out Std_Logic;
           MECRegSel_N         : out Std_Logic;
           Unimpl_Address_Out  : out Std_Logic
           );
end AddressDecoder;

----------------------------------------------------------------------------
architecture Mini_Spec of AddressDecoder is

  component ADecBuff
    port (
          A_In  : in Std_Logic_Vector(9 downto 0);
          A_Out : buffer Std_Logic_Vector(9 downto 0)
         );
  end component;


  component LBuff
       port (
             Clk1  : in Std_Logic;
             Clk2  : in Std_Logic;
             LBuf  : out Std_Logic
            );
  end component;

  constant PSize1 : Std_Logic_Vector(2 downto 0) := "000";    --  128k
  constant PSize2 : Std_Logic_Vector(2 downto 0) := "001";    --  256k
  constant PSize3 : Std_Logic_Vector(2 downto 0) := "010";    --  512k
  constant PSize4 : Std_Logic_Vector(2 downto 0) := "011";    --    1M
  constant PSize5 : Std_Logic_Vector(2 downto 0) := "100";    --    2M
  constant PSize6 : Std_Logic_Vector(2 downto 0) := "101";    --    4M
  constant PSize7 : Std_Logic_Vector(2 downto 0) := "110";    --    8M
  constant PSize8 : Std_Logic_Vector(2 downto 0) := "111";    --   16M

  constant MSize1 : Std_Logic_Vector(2 downto 0) := "000";    -- 256k
  constant MSize2 : Std_Logic_Vector(2 downto 0) := "001";    -- 512k
  constant MSize3 : Std_Logic_Vector(2 downto 0) := "010";    --   1M
  constant MSize4 : Std_Logic_Vector(2 downto 0) := "011";    --   2M
  constant MSize5 : Std_Logic_Vector(2 downto 0) := "100";    --   4M
  constant MSize6 : Std_Logic_Vector(2 downto 0) := "101";    --   8M
  constant MSize7 : Std_Logic_Vector(2 downto 0) := "110";    --  16M
  constant MSize8 : Std_Logic_Vector(2 downto 0) := "111";    --  32M

  constant XSize1 : Std_Logic_Vector(2 downto 0)  := "000";   --   4k
  constant XSize2 : Std_Logic_Vector(2 downto 0)  := "001";   --   8k
  constant XSize3 : Std_Logic_Vector(2 downto 0)  := "010";   --  16k
  constant XSize4 : Std_Logic_Vector(2 downto 0)  := "011";   --  32k
  constant XSize5 : Std_Logic_Vector(2 downto 0)  := "100";   --  64k
  constant XSize6 : Std_Logic_Vector(2 downto 0)  := "101";   -- 128k
  constant XSize7 : Std_Logic_Vector(2 downto 0)  := "110";   -- 256k
  constant XSize8 : Std_Logic_Vector(2 downto 0)  := "111";   -- 512k

  constant IOSize1 : Std_Logic_Vector(3 downto 0) := "0000";  --  512
  constant IOSize2 : Std_Logic_Vector(3 downto 0) := "0001";  --   1k
  constant IOSize3 : Std_Logic_Vector(3 downto 0) := "0010";  --   2k
  constant IOSize4 : Std_Logic_Vector(3 downto 0) := "0011";  --   4k
  constant IOSize5 : Std_Logic_Vector(3 downto 0) := "0100";  --   8k
  constant IOSize6 : Std_Logic_Vector(3 downto 0) := "0101";  --  16k
  constant IOSize7 : Std_Logic_Vector(3 downto 0) := "0110";  --  32k
  constant IOSize8 : Std_Logic_Vector(3 downto 0) := "0111";  --  64k
  constant IOSize9 : Std_Logic_Vector(3 downto 0) := "1000";  -- 128k
  constant IOSize10: Std_Logic_Vector(3 downto 0) := "1001";  -- 256k
  constant IOSize11: Std_Logic_Vector(3 downto 0) := "1010";  -- 512k
  constant IOSize12: Std_Logic_Vector(3 downto 0) := "1011";  --   1M
  constant IOSize13: Std_Logic_Vector(3 downto 0) := "1100";  --   2M
  constant IOSize14: Std_Logic_Vector(3 downto 0) := "1101";  --   4M
  constant IOSize15: Std_Logic_Vector(3 downto 0) := "1110";  --   8M
  constant IOSize16: Std_Logic_Vector(3 downto 0) := "1111";  --  16M


  signal AccessType_s: Std_Logic_Vector(3 downto 0);

  signal CS_BUS     : Std_Logic_Vector (39 downto 0);
  signal CS_BUS_aux : Std_Logic_Vector (39 downto 0);

  signal Prom_Area_Out_p,
         Exch_Area_Int_p,
         Register_Area_Out_p,
         Mem_Area_Int_p,
         IO_Area_Int_p,
         Prom_Select_Out_p,
         PromA_illegal_Out_p,
         ExchA_illegal_Out_p,
         Exchange_Select_Out_p,
         MemA_illegal_Out_p,
         IOA_illegal_out_p,
         Prom_Ext_Area_sel_p,
         Ram_Ext_Area_sel_p,
         IO_Ext_Area_sel_p,
         Ext_Area_Sel_p : Std_Logic;
         
  signal  Mem_Area_Int     : Std_Logic; 
          
  signal  MemA_Int_p       : Std_Logic_Vector(2 downto 0);
  signal  MemA_Int         : Std_Logic_Vector(2 downto 0);


  signal  IO_Select_Int_p  : Std_Logic_Vector (1 downto 0);
  signal  IO_Select_out_p  : Std_Logic_Vector(3 downto 0);

  signal  Byte_Access      : Std_Logic; 
  signal  Word_Access      : Std_Logic;
  signal  DblWord_Access   : Std_Logic;
           
  signal High_A            : Std_Logic_Vector(7 downto 0);
  signal MEC_A             : Std_Logic_Vector(7 downto 0);
  signal Prom_Adr          : Std_Logic_Vector(21 downto 0);
  signal Xchg_Adr          : Std_Logic_Vector(16 downto 0);
  signal Ram_Adr           : Std_Logic_Vector(22 downto 0);
  signal IO_Adr            : Std_Logic_Vector(21 downto 0);
  signal Low_A             : Std_Logic_Vector(7 downto 0);
  signal MEC_r             : Std_Logic_Vector(5 downto 0);
  
  signal DMA_Mode_Latched  : Std_Logic;
     
  signal CPUHalt_Out_N_fck :  Std_Logic;
  
  signal A_Int_CS          : Std_Logic_Vector (31 downto 2);
  signal A_Int_CS_Buff     : Std_Logic_Vector (31 downto 2);
  signal ASI_Int_CS        : Std_Logic_Vector (3 downto 0);
  signal Size_Int_CS       : Std_Logic_Vector (1 downto 0);
  signal Wrt_Int_CS        : Std_Logic;
    
  signal CSvalue           : std_logic_vector(2 downto 0);
  
  signal IO_dec            : Std_Logic;
  signal NewCycle_rck  : Std_Logic;
  
  signal RomCs_Out_loc_N   : Std_Logic;
  signal ExMCS_Out_loc_N   : Std_Logic;

  signal IOA_Area_sel      : std_logic_vector(4 downto 0);
  
  signal IOSel_Out_loc_N   : std_logic_vector(3 downto 0);
  signal IO_Sel_Gate       : std_logic;

  signal Ram_limit         : std_logic;
  signal LatchReset_N      : std_logic;
  
  --latches
  signal MemSelectReset_N  : std_logic;
  signal MemSelectRedReset_N  : std_logic;
  signal CSEnable_N_p      : Std_Logic;
      
  signal MemSelectOut_p  : Std_Logic_Vector(7 downto 0); 
  signal MemSelectEn_N_p : Std_Logic;
  signal MemSelectEn_N   : Std_Logic;
   
  signal MemSelectR1_p   : Std_Logic; 
  signal MemSelectR2_p   : Std_Logic; 

  signal MemCs_Out_N_0    : std_logic;
  signal MemCs_Out_N_1    : std_logic;
  signal MemCs_Out_N_2    : std_logic;
  signal MemCs_Out_N_3    : std_logic;
  signal MemCs_Out_N_4    : std_logic;
  signal MemCs_Out_N_5    : std_logic;
  signal MemCs_Out_N_6    : std_logic;
  signal MemCs_Out_N_7    : std_logic;
  signal MemCs_Out_N_8    : std_logic;
  signal MemCs_Out_N_9    : std_logic;
  
  signal L_Enable        : std_logic;
  
  signal L_Enable_Buf1   : std_logic;
  signal L_Enable_Buf3   : std_logic;
  signal L_Enable_Buf4   : std_logic;
  signal L_Enable_Buf5   : std_logic;
  signal L_Enable_Buf6   : std_logic;
  signal L_Enable_Buf7   : std_logic;
  signal L_Enable_Buf8   : std_logic;
  
  signal L_Buf1          : std_logic;
  signal L_Buf2          : std_logic;
  signal L_Buf3          : std_logic;
  signal L_Buf4          : std_logic;
  signal L_Buf5          : std_logic;
  
  signal Low_A_r         : std_logic;
  signal RegAccessViolation      : std_logic;
  
  signal ParityEnable_c  : std_logic;
  signal EDACEnable_c    : std_logic;
  
  signal MemoryAccess    : Std_Logic;
  signal CheckAccessEn0  : Std_Logic;
  signal CheckAccessEn1  : Std_Logic;
    
  signal  APSBaseReg0    : Std_Logic_Vector(24 downto 0);
  signal  APSBaseReg0Par : Std_Logic;
  signal  APSBaseReg0_N  : Std_Logic;
  
  signal  APSEndReg0     : Std_Logic_Vector(22 downto 0);
  signal  APSEndReg0Par  : Std_Logic;
  signal  APSEndReg0_N   : Std_Logic;

  signal  APSBaseReg1    : Std_Logic_Vector(24 downto 0);
  signal  APSBaseReg1Par : Std_Logic;
  signal  APSBaseReg1_N  : Std_Logic;
  
  signal  APSEndReg1     : Std_Logic_Vector(22 downto 0);
  signal  APSEndReg1Par  : Std_Logic;
  signal  APSEndReg1_N   : Std_Logic;
            
  signal ParErr_APSBaseReg0     : std_logic;
  signal ParErr_APSEndReg0      : std_logic;
  signal ParErr_APSBaseReg1     : std_logic;
  signal ParErr_APSEndReg1      : std_logic;
    
  signal Clk_Int_N              : std_logic;
  signal UARTCond               : std_logic;
  
  signal MemAccess_Violation0_p : std_logic;
  signal MemAccess_Violation0   : std_logic;
  
  signal MemAccess_Violation1_p : std_logic;
  signal MemAccess_Violation1   : std_logic;
  
              
begin

   -----
   Byte_Access       <= not(Size_Int_CS(0) or Size_Int_CS(1));
   Word_Access       <= Size_Int_CS(1) and not(Size_Int_CS(0));
   DblWord_Access    <= Size_Int_CS(1) and Size_Int_CS(0);
   
   Clk_Int_N <= not Clk_Int;
    
   ------
   L_Buff1 : LBuff
   port map (
             Clk1 =>  Clk_Int_N,
             Clk2 =>  Clk_Int_N,
             LBuf => L_Buf1
             );
              
   Latch_P1 : process (L_Buf1, A_in)
   begin
    if L_Buf1 = '1' then
      A_Int_CS(8 downto 2) <= A_In(8 downto 2);
    end if;
   end process;

   --
   L_Buff2 : LBuff
   port map (
             Clk1 =>  Clk_Int_N,
             Clk2 =>  Clk_Int_N,
             LBuf => L_Buf2
             );
              
   Latch_P2 : process (L_Buf2, A_in)
   begin
    if L_Buf2 = '1' then
      A_Int_CS(15 downto 9) <= A_In(15 downto 9);
    end if;
   end process;

   --
   L_Buff3 : LBuff
   port map (
             Clk1 =>  Clk_Int_N,
             Clk2 =>  Clk_Int_N,
             LBuf => L_Buf3
             );
              
   Latch_P3 : process (L_Buf3, A_in)
   begin
    if L_Buf3 = '1' then
      A_Int_CS(23 downto 16) <= A_In(23 downto 16);
    end if;
  end process;

   --
   L_Buff4 : LBuff
   port map (
             Clk1 =>  Clk_Int_N,
             Clk2 =>  Clk_Int_N,
             LBuf => L_Buf4
             );
              
   Latch_P4 : process (L_Buf4, A_in)
   begin
    if L_Buf4 = '1' then
      A_Int_CS(31 downto 24) <= A_In(31 downto 24);
    end if;
   end process;

   --
   L_Buff5 : LBuff
   port map (
             Clk1 =>  Clk_Int_N,
             Clk2 =>  Clk_Int_N,
             LBuf => L_Buf5
             );
              
   Latch_P5 : process (L_Buf5, ASI_in, Size_In, Wrt_In)
   begin
    if L_Buf5 = '1' then
      ASI_Int_CS  <= ASI_In;
      Size_Int_CS <= Size_In;
	    Wrt_Int_CS  <= Wrt_In;
    end if;
   end process;
  
    -----
    DMA_Mode_LatchedProc: process
    begin
      wait until Clk_Int'event and Clk_Int = '0';
      DMA_Mode_Latched <= DMA_Mode;
      CPUHalt_Out_N_fck <= CPUHalt_Out_N;
    end process;
    

    -----
    High_A  <= A_Int_CS(31 downto 24);
    MEC_A   <= A_Int_CS(23 downto 16);
    Low_A   <= A_Int_CS(15 downto 8);
    MEC_r   <= A_Int_CS(7 downto 2);         -- 228 st
    Prom_Adr<= A_Int_CS(23 downto 2);        -- 16M
    Xchg_Adr<= A_Int_CS(18 downto 2);        -- 512k
    Ram_Adr <= A_Int_CS(24 downto 2);        -- 32M
    IO_Adr  <= A_Int_CS(23 downto 2);        -- 16M


    -----
    AddressDecoder: process(High_A, MEC_A)
    begin
        
        Ext_Area_Sel_p  <= High_A(7); --1XXX_XXXX
        
        IO_Select_Int_p <= High_A (1 downto 0);
                
        IO_Ext_Area_Sel_p <= not(High_A(7)) and --0001_0100 to 0111_1111
                            (High_A(6) or High_A(5) or 
                            (High_A(4) and (High_A(3) or High_A(2)) ));
             
             
        if High_A(7 downto 2) ="000100"  then
             IO_Area_Int_p <= '1';             
        else
             IO_Area_Int_p <= '0';
        end if;
         
        if High_A(7 downto 4) ="0000" and (High_A(3)='1' or High_A(2)='1') then
             Ram_Ext_Area_Sel_p <= '1';             
        else
             Ram_Ext_Area_Sel_p <= '0';
        end if;
        
        if High_A(7 downto 1) ="0000001"  then
             Mem_Area_Int_p <= '1';             
        else
             Mem_Area_Int_p <= '0';
        end if;
         
        if High_A ="00000001" and MEC_A(7 downto 3)="11111"  then
             Register_Area_Out_p <= '1';             
        else
             Register_Area_Out_p <= '0';
        end if;
        
        if High_A ="00000001" and MEC_A(7 downto 3)="11110"  then
             Exch_Area_Int_p <= '1';             
        else
             Exch_Area_Int_p <= '0';
        end if;
        
        if High_A ="00000001" and not(MEC_A(7 downto 4)="1111") then
             Prom_Ext_Area_Sel_p <= '1';             
        else
             Prom_Ext_Area_Sel_p <= '0';
        end if;
                    
        if High_A ="00000000" then
             Prom_Area_Out_p <= '1';             
        else
             Prom_Area_Out_p <= '0';
        end if;
        
    end process; -- AdressDecoder
    
    
    L_Enable_P: process(DMA_Mode_Latched, NewCycle, DMAAS_In)
    begin
          
      if (DMA_Mode_Latched = '0' and NewCycle = '1')  or
         (DMA_Mode_Latched = '1' and DMAAS_In = '1') then
        L_Enable <= '1';
       else
        L_Enable <= '0';
      end if; 
      
    end process;

    
    L_Enable_Buff1: process(L_Enable, Clk_Int)
    begin
          
       L_Enable_Buf1 <= L_Enable and Clk_Int;
      
    end process;
    
    L_Enable_Buff3: process(L_Enable, Clk_Int)
    begin
          
       L_Enable_Buf3 <= L_Enable and Clk_Int;
      
    end process;
    
    L_Enable_Buff4: process(L_Enable, Clk_Int)
    begin
          
       L_Enable_Buf4 <= L_Enable and Clk_Int;
      
    end process;
    
    L_Enable_Buff5: process(L_Enable, Clk_Int)
    begin
          
       L_Enable_Buf5 <= L_Enable and Clk_Int;
      
    end process;
    
    L_Enable_Buff6: process(L_Enable, Clk_Int)
    begin
          
       L_Enable_Buf6 <= L_Enable and Clk_Int;
      
    end process;
    
    L_Enable_Buff7: process(L_Enable, Clk_Int)
    begin
          
       L_Enable_Buf7 <= L_Enable and Clk_Int;
      
    end process;
    
    L_Enable_Buff8: process(L_Enable, Clk_Int)
    begin
          
       L_Enable_Buf8 <= L_Enable and Clk_Int;
      
    end process;
                   
      
    MemSelectReset_N  <= Reset_Out_N and not RdRst and not MemSelectEn_N;


    MemCSLatch: process(MemSelectReset_N, MemSelectOut_p, 
                        L_Enable_Buf1, CSEnable_N_p)  
    begin
     
      if MemSelectReset_N = '0'  then
         MemCs_Out_N_0   <= '1';
         MemCs_Out_N_1   <= '1';
         MemCs_Out_N_2   <= '1';
         MemCs_Out_N_3   <= '1';
         MemCs_Out_N_4   <= '1';
         MemCs_Out_N_5   <= '1';
         MemCs_Out_N_6   <= '1';
         MemCs_Out_N_7   <= '1';

      elsif L_Enable_Buf1 = '1' then
        
         MemCs_Out_N_0   <= MemSelectOut_p(0) or CSEnable_N_p;
         MemCs_Out_N_1   <= MemSelectOut_p(1) or CSEnable_N_p;
         MemCs_Out_N_2   <= MemSelectOut_p(2) or CSEnable_N_p;
         MemCs_Out_N_3   <= MemSelectOut_p(3) or CSEnable_N_p;
         MemCs_Out_N_4   <= MemSelectOut_p(4) or CSEnable_N_p;
         MemCs_Out_N_5   <= MemSelectOut_p(5) or CSEnable_N_p;
         MemCs_Out_N_6   <= MemSelectOut_p(6) or CSEnable_N_p;
         MemCs_Out_N_7   <= MemSelectOut_p(7) or CSEnable_N_p;

      end if;

    end process; -- MemCSLatch
                   

    MemSelectRedReset_N  <= Reset_Out_N and not RdRst; 


    MemCSRLatch: process(MemSelectRedReset_N, L_Enable_Buf1, MemSelectR1_p, 
                         MemSelectR2_p, CSEnable_N_p)  
    begin
     
      if MemSelectRedReset_N = '0'  then
         MemCs_Out_N_8   <= '1';
         MemCs_Out_N_9   <= '1';

      elsif L_Enable_Buf1 = '1' then
        
         MemCs_Out_N_8   <= MemSelectR1_p or CSEnable_N_p;
         MemCs_Out_N_9   <= MemSelectR2_p or CSEnable_N_p;

      end if;

    end process; -- MemCSLatch

    MemCs_Out_N <= MemCs_Out_N_9 & MemCs_Out_N_8 & MemCs_Out_N_7 & MemCs_Out_N_6 & 
                   MemCs_Out_N_5 & MemCs_Out_N_4 & MemCs_Out_N_3 & MemCs_Out_N_2 & 
                   MemCs_Out_N_1 & MemCs_Out_N_0;
                   
                   

    LatchReset_N  <= Reset_Out_N and not RdRst;
                   
    TESTproc2: process(LatchReset_N, L_Enable_Buf3, 
                       Prom_Select_Out_p, Exchange_Select_Out_p, 
                       PromA_illegal_Out_p,
                       ExchA_illegal_Out_p, MemA_illegal_Out_p,
                       IOA_illegal_out_p, Register_Area_Out_p,
                       Ext_Area_Sel_p, Word_Access,
                       IO_Select_out_p, DMA_Mode)  
    begin
      if LatchReset_N = '0' then
         RomCs_Out_loc_N        <= '1';
         ExMCS_Out_loc_N        <= '1';
         Illegal_Address_Out    <= '0'; 
         IOSel_Out_loc_N        <= "1111";

      elsif L_Enable_Buf3 = '1' then
         RomCs_Out_loc_N        <= Prom_Select_Out_p;
         ExMCS_Out_loc_N        <= Exchange_Select_Out_p;
         Illegal_Address_Out    <= PromA_illegal_Out_p or ExchA_illegal_Out_p or 
                                   MemA_illegal_Out_p or IOA_illegal_out_p or 
                                   (DMA_Mode and not(Word_Access)); 
         IOSel_Out_loc_N        <= IO_Select_out_p;

      end if; 
    end process;
    
    ExMCS_Out_N  <= ExMCS_Out_loc_N;
    
    RomCs_Out_N  <= RomCs_Out_loc_N or NewCycle_rck;
     
    TESTproc2bis: process(Reset_Out_N, L_Enable_Buf3, 
                          Register_Area_Out_p, MemSelectEn_N_p)  
    begin
      if Reset_Out_N = '0' then
         Register_Area_Out      <= '0'; 
         MemSelectEn_N          <= '1'; 
      elsif L_Enable_Buf3 = '1' then
         Register_Area_Out      <= Register_Area_Out_p; 
         MemSelectEn_N          <= MemSelectEn_N_p; 
      end if; 
    end process;
     


   rck_Process : process
   begin
     wait until Clk_Int'Event and Clk_Int = '1';
     NewCycle_rck <= NewCycle;
   end process rck_Process;

   -- Process for generating a gate signal for IO_Sel
   Gate_Process : process(NewCycle, NewCycle_rck, StrobeGate)
   begin
     IO_Sel_Gate <= NewCycle or NewCycle_rck or StrobeGate;
   end process Gate_Process;

   IOSel_Out_N(0) <= IO_Sel_Gate or IOSel_Out_loc_N(0);
   IOSel_Out_N(1) <= IO_Sel_Gate or IOSel_Out_loc_N(1);
   IOSel_Out_N(2) <= IO_Sel_Gate or IOSel_Out_loc_N(2);
   IOSel_Out_N(3) <= IO_Sel_Gate or IOSel_Out_loc_N(3);


----------------------------------------------------------------------------
    EDACEnGen: process( PROM_Area_Out_p, Prom_Ext_Area_Sel_p,
                        Exch_Area_Int_p, Prom8_In_N, Mem_Cfg_reg, Register_Area_Out_p,
                        Mem_Area_Int_p, Ram_Ext_Area_Sel_p, IO_Area_Int_p, IO_Select_Int_p,
                        IO_Cfg_reg, IO_Ext_Area_Sel_p) 
    begin     
     
      -- PROM
      if ((PROM_Area_Out_p = '1') or (Prom_Ext_Area_Sel_p = '1')) then
          ParityEnable_c <= Prom8_In_N;        
          EDACEnable_c   <= Prom8_In_N;        

      -- Exchange memory
      elsif Exch_Area_Int_p = '1' then                       
          ParityEnable_c <= Mem_Cfg_reg(27) or Mem_Cfg_reg(28);
          EDACEnable_c   <= Mem_Cfg_reg(28);

      -- MEC Registers
      elsif Register_Area_Out_p = '1' then
          ParityEnable_c <= '1';
          EDACEnable_c   <= '0';
            
      -- RAM
      elsif ((Mem_Area_Int_p = '1') or (Ram_Ext_Area_Sel_p = '1')) then  
          ParityEnable_c <= Mem_Cfg_reg(13) or Mem_Cfg_reg(14);
          EDACEnable_c   <= Mem_Cfg_reg(14);
            
      -- IO
      elsif IO_Area_Int_p = '1' then 
          
        case IO_Select_Int_p is
          when "00" =>   ParityEnable_c <= IO_Cfg_reg(5);
                         EDACEnable_c   <= '0';
          when "01" =>   ParityEnable_c <= IO_Cfg_reg(13);
                         EDACEnable_c   <= '0';
          when "10" =>   ParityEnable_c <= IO_Cfg_reg(21);
                         EDACEnable_c   <= '0';
          when others => ParityEnable_c <= IO_Cfg_reg(29);
                         EDACEnable_c   <= '0';
       end case; -- Prom_Size
          
      elsif IO_Ext_Area_Sel_p = '1' then
        ParityEnable_c <= IO_Cfg_reg(29);
        EDACEnable_c   <= '0';
          
      else
        ParityEnable_c <= '0';
        EDACEnable_c   <= '0';
      end if;        
    end process;
    
    EDACEnLatchGen: process(Clk_Int, Reset_Out_N)
    begin     
     
      if Reset_Out_N = '0' then
        ParityEnable <= '0';
        EDACEnable   <= '0';
        
      elsif Clk_Int'event and Clk_Int = '0' then
           if (DMA_Mode_Latched = '0' and NewCycle = '1')  or
              (DMA_Mode_Latched = '1' and DMAAS_In = '1') then
             ParityEnable <= ParityEnable_c;        
             EDACEnable   <= EDACEnable_c;  
               
           end if; 
           
      end if; 
    end process;
    

----------------------------------------------------------------------------
--      Boot PROM Area 0x0000_0000 to 0x0007_FFFF
--      External output Prom_Select_Out
--      External input Prom_Size
--
  Prom_Decoder: process(Prom_Area_Out_p, Prom_Ext_Area_Sel_p, 
                        Mem_Cfg_reg, Prom_Adr, Wrt_Int_CS,
                        RomWrt_In_N, Byte_Access, Word_Access)
    variable limit : std_logic;
  begin
  
      case Mem_Cfg_reg(20 downto 18) is
        when PSize1 => limit := Prom_Adr(21) or Prom_Adr(20) or Prom_Adr(19) or
                                Prom_Adr(18) or Prom_Adr(17) or Prom_Adr(16) or
                                Prom_Adr(15);                                 
        when PSize2 => limit := Prom_Adr(21) or Prom_Adr(20) or Prom_Adr(19) or
                                Prom_Adr(18) or Prom_Adr(17) or Prom_Adr(16);
        when PSize3 => limit := Prom_Adr(21) or Prom_Adr(20) or Prom_Adr(19) or
                                Prom_Adr(18) or Prom_Adr(17);                 
        when PSize4 => limit := Prom_Adr(21) or Prom_Adr(20) or Prom_Adr(19) or
                                Prom_Adr(18);
        when PSize5 => limit := Prom_Adr(21) or Prom_Adr(20) or Prom_Adr(19);
        when PSize6 => limit := Prom_Adr(21) or Prom_Adr(20);
        when PSize7 => limit := Prom_Adr(21);
        when others => limit := '0'; -- Prom_Size1;
      end case; -- Prom_Size
      

    if Prom_Area_Out_p = '1' then

      -- Illegal access when;
      -- 1. address > max size of PROM
      -- 2. Write and Write inhibit set
      -- 3. Write and 8-bit wide and non byte access
      -- 4. Write and 40-bit wide and non word access
      if limit = '1' or
         ( (Wrt_Int_CS = '1') and ((Mem_Cfg_reg(16) /= '1') or (RomWrt_In_N /= '0'))) or
         ( (Wrt_Int_CS = '1') and (Mem_Cfg_reg(17) = '0') and (Byte_Access /= '1'))  or
         ( (Wrt_Int_CS = '1') and (Mem_Cfg_reg(17) = '1') and (Word_Access /= '1') ) then
          PromA_illegal_Out_p <= '1';
          Prom_Select_Out_p   <= '1';
	    else
          Prom_Select_Out_p   <= '0';
          PromA_illegal_Out_p <= '0';
      end if; --limit

    elsif Prom_Ext_Area_Sel_p = '1' then
      -- Illegal access when;
      -- 1. Write and Write inhibit set
      -- 2. Write and 8-bit wide and non byte access
      -- 3. Write and 40-bit wide and non word access
      if ( (Wrt_Int_CS = '1') and ((Mem_Cfg_reg(16) /= '1') or (RomWrt_In_N /= '0'))) or
         ( (Wrt_Int_CS = '1') and (Mem_Cfg_reg(17) = '0') and (Byte_Access /= '1'))  or
         ( (Wrt_Int_CS = '1') and (Mem_Cfg_reg(17) = '1') and (Word_Access /= '1') ) then
          PromA_illegal_Out_p <= '1';
    	else
          PromA_illegal_Out_p <= '0';
      end if; --limit
      Prom_Select_Out_p <= '1';

    else
      Prom_Select_Out_p   <= '1';
      PromA_illegal_Out_p <= '0';

    end if; -- Prom_Area_Sel

  end process; -- PROM_Decoder
----------------------------------------------------------------------------
--      eXchange Area 0x01F0_0000 to 0x01F7_FFFF
--      External output Xchange_CS
--      External input Xchg_Size
--
  Xchg_Decoder_P: process(Exch_Area_Int_p,  Mem_Cfg_reg, Xchg_Adr,
                          Word_Access)
    variable limit : std_logic;
  begin
  
      case  Mem_Cfg_reg(26 downto 24) is
      	when XSize1 => limit := Xchg_Adr(16) or Xchg_Adr(15) or Xchg_Adr(14) or
      	                        Xchg_Adr(13) or Xchg_Adr(12) or Xchg_Adr(11) or 
      	                        Xchg_Adr(10);
      	when XSize2 => limit := Xchg_Adr(16) or Xchg_Adr(15) or Xchg_Adr(14) or
      	                        Xchg_Adr(13) or Xchg_Adr(12) or Xchg_Adr(11);
      	when XSize3 => limit := Xchg_Adr(16) or Xchg_Adr(15) or Xchg_Adr(14) or
      	                        Xchg_Adr(13) or Xchg_Adr(12);
      	when XSize4 => limit := Xchg_Adr(16) or Xchg_Adr(15) or Xchg_Adr(14) or
      	                        Xchg_Adr(13);
      	when XSize5 => limit := Xchg_Adr(16) or Xchg_Adr(15) or Xchg_Adr(14);
      	when XSize6 => limit := Xchg_Adr(16) or Xchg_Adr(15);
      	when XSize7 => limit := Xchg_Adr(16);
        when others => limit := '0';
      end case; -- Xchg_Size


    if Exch_Area_Int_p = '1' then
    
      if (limit = '1') or (Word_Access /= '1') or (Mem_Cfg_reg(29) /= '1') then
        ExchA_illegal_Out_p   <= '1';
        Exchange_Select_Out_p <= '1';
      else
        ExchA_illegal_Out_p   <= '0';
        Exchange_Select_Out_p <= '0';
      end if; --limit
    else
      ExchA_illegal_Out_p   <= '0';
      Exchange_Select_Out_p <= '1';

    end if; -- Xchg_Area_Sel

  end process; -- Xchg_Decoder

----------------------------------------------------------------------------
--      MEC RegisterArea 0x01F8_0000 to 0x01FF_FFFF 
----------------------------------------------------------------------------

----------------------------------------------------------------------------
  UARTCond  <= not (DMA_Mode_Latched or ASI_Int_CS(0) or Wrt_Int_CS);

  CS_Bus_Decoder: process( MEC_r, Wrt_Int_CS, UARTCond)
  begin    
        --MECControlReg_N
        if MEC_r = "000000" then
          CS_Bus_aux(0)  <= '0'; 
        else
          CS_Bus_aux(0)  <= '1';
        end if;

        --SWRReg_N
        if MEC_r = "000001" then
          CS_Bus_aux(1)  <= not Wrt_Int_CS;
        else
          CS_Bus_aux(1)  <= '1';
        end if;

        --PWRDReg_N
        if MEC_r = "000010" then
          CS_Bus_aux(2)  <= not Wrt_Int_CS;
        else
          CS_Bus_aux(2)  <= '1';
        end if;
         
        --MemConfigReg_N
        if MEC_r = "000100" then
          CS_Bus_aux(3)  <= '0';
        else
          CS_Bus_aux(3)  <= '1';
        end if;
        
        --IOConfigReg_N
        if MEC_r = "000101" then
          CS_Bus_aux(4)  <= '0';
        else
          CS_Bus_aux(4)  <= '1';
        end if;
        
        --WSConfigReg_N
        if MEC_r = "000110" then
          CS_Bus_aux(5)  <= not Wrt_Int_CS;
        else
          CS_Bus_aux(5)  <= '1';
        end if;

        --APSBaseReg0_N
        if MEC_r = "001000" then
          CS_Bus_aux(6)  <= '0';
        else
          CS_Bus_aux(6)  <= '1';
        end if;
         
         --APSEndReg0_N
        if MEC_r = "001001" then
          CS_Bus_aux(7)  <= '0';
        else
          CS_Bus_aux(7)  <= '1';
        end if;

        --APSBaseReg1_N
        if MEC_r = "001010" then
          CS_Bus_aux(8)  <= '0';
        else
          CS_Bus_aux(8)  <= '1';
        end if;
         
         --APSEndReg1_N
        if MEC_r = "001011" then
          CS_Bus_aux(9)  <= '0';
        else
          CS_Bus_aux(9)  <= '1';
        end if;


        CS_Bus_aux(10)  <= '1';


        --IntShapeReg_N
        if MEC_r = "010001" then
          CS_Bus_aux(11)  <= '0';
        else
          CS_Bus_aux(11)  <= '1';
        end if;

        --IntPendReg_N
        if MEC_r = "010010" then
          CS_Bus_aux(12)  <= Wrt_Int_CS;
        else
          CS_Bus_aux(12)  <= '1';
        end if;

        --IntMaskReg_N
        if MEC_r = "010011" then
          CS_Bus_aux(13)  <= '0';
        else
          CS_Bus_aux(13)  <= '1';
        end if;

        -- IntClearReg_N
        if MEC_r = "010100" then
          CS_Bus_aux(14)  <= not Wrt_Int_CS;
        else
          CS_Bus_aux(14)  <= '1';
        end if;

        --IntForceReg_N
        if MEC_r = "010101" then
          CS_Bus_aux(15)  <= '0';
        else
          CS_Bus_aux(15)  <= '1';
        end if;

        --WDProgramReg_N
        if MEC_r = "011000" then
          CS_Bus_aux(16)  <= '0';  --wdog
        else
          CS_Bus_aux(16)  <= '1';
        end if;

        --WDTDSetReg_N
        if MEC_r = "011001" then
          CS_Bus_aux(17)  <= not Wrt_Int_CS;
        else
          CS_Bus_aux(17)  <= '1';
        end if;

        --RTC_CountReg_N
        if MEC_r = "100000" then
          CS_Bus_aux(18)  <= '0';
        else
          CS_Bus_aux(18)  <= '1';
        end if;

        --RTC_ScalerReg_N
        if MEC_r = "100001" then
          CS_Bus_aux(19)  <= '0';
        else
          CS_Bus_aux(19)  <= '1';
        end if;

        --GPT_A_CountReg_N
        if MEC_r = "100010" then
          CS_Bus_aux(20)  <= '0';
        else
          CS_Bus_aux(20)  <= '1';
        end if;

        --GPT_A_ScalerReg_N
        if MEC_r = "100011" then
          CS_Bus_aux(21)  <= '0';
        else
          CS_Bus_aux(21)  <= '1';
        end if;

        CS_Bus_aux(22)  <= '1';
        CS_Bus_aux(23)  <= '1';

        --TimerControlReg_N
        if MEC_r = "100110" then
          CS_Bus_aux(24)  <= not Wrt_Int_CS;
        else
          CS_Bus_aux(24)  <= '1';
        end if;

        --SysFSReg_N
        if MEC_r = "101000" then
          CS_Bus_aux(25)  <= '0';
        else
          CS_Bus_aux(25)  <= '1';
        end if;

        --FFAReg_N
        if MEC_r = "101001" then
          CS_Bus_aux(26)  <= Wrt_Int_CS;
        else
          CS_Bus_aux(26)  <= '1';
        end if;

          CS_Bus_aux(27)  <= '1';

          CS_Bus_aux(28)  <= '1';

        --ErrResStatReg_N
        if MEC_r = "101100" then
          CS_Bus_aux(29)  <= '0';
        else
          CS_Bus_aux(29)  <= '1';
        end if;

          CS_Bus_aux(30)  <= '1';

          CS_Bus_aux(31)  <= '1';
        
          CS_Bus_aux(32)  <= '1';

          CS_Bus_aux(33)  <= '1';

        --TestControl_Reg_N
        if MEC_r = "110100" then
          CS_Bus_aux(34)  <= '0';
        else
          CS_Bus_aux(34)  <= '1';
        end if;

          CS_Bus_aux(35)  <= '1';
        
        --UARTAReg_N
        if MEC_r = "111000" then
          CS_Bus_aux(36)  <= UARTCond;
        else
          CS_Bus_aux(36)  <= '1';
        end if;
        
        --UARTBReg_N
        if MEC_r = "111001" then
          CS_Bus_aux(37)  <= UARTCond;
        else
          CS_Bus_aux(37)  <= '1';
        end if;
        
        --UARTStatusReg_N
        if MEC_r = "111010" then
          CS_Bus_aux(38)  <= '0';
        else
          CS_Bus_aux(38)  <= '1';
        end if;
  
 
  end process; 

----------------------------------------------------------------------------
  CS_Bus_Decoder2: process( MEC_r, Wrt_Int_CS,
                            DMA_Mode_Latched,
                            DblWord_Access, Word_Access, ASI_Int_CS, 
                            CPUHalt_Out_N_fck)
    variable MEC_rVar : integer;
  begin    
      
	if is_x(MEC_r) then
	   MEC_rVar := -1;
	else
          MEC_rVar := CONV_inTEGER(MEC_r);
	end if;
        case MEC_rVar is
          when 16#0_0# =>  CS_Bus_aux(39) <= '1';
          when 16#0_1# =>  CS_Bus_aux(39) <= Wrt_Int_CS;
          when 16#0_2# =>  CS_Bus_aux(39) <= Wrt_Int_CS;
          
          when 16#0_4# =>  CS_Bus_aux(39) <= '1';
          when 16#0_5# =>  CS_Bus_aux(39) <= '1';
          when 16#0_6# =>  CS_Bus_aux(39) <= Wrt_Int_CS;
          
          when 16#0_8# =>  CS_Bus_aux(39) <= '1';
          when 16#0_9# =>  CS_Bus_aux(39) <= '1';
          when 16#0_A# =>  CS_Bus_aux(39) <= '1';
          when 16#0_B# =>  CS_Bus_aux(39) <= '1';
          
          when 16#1_1# =>  CS_Bus_aux(39) <= '1';
          when 16#1_2# =>  CS_Bus_aux(39) <= not Wrt_Int_CS;
          when 16#1_3# =>  CS_Bus_aux(39) <= '1';
          when 16#1_4# =>  CS_Bus_aux(39) <= Wrt_Int_CS;
          when 16#1_5# =>  CS_Bus_aux(39) <= '1';
          
          when 16#1_8# =>  CS_Bus_aux(39) <= '1';  --Wdog
          
          when 16#1_9# =>  CS_Bus_aux(39) <= Wrt_Int_CS;
          
          when 16#2_0# =>  CS_Bus_aux(39) <= '1';
          when 16#2_1# =>  CS_Bus_aux(39) <= '1';
                    
          when 16#2_2# =>  CS_Bus_aux(39) <= '1';          
          when 16#2_3# =>  CS_Bus_aux(39) <= '1';
          
          
          when 16#2_6# =>  CS_Bus_aux(39) <= Wrt_Int_CS;
          when 16#2_8# =>  CS_Bus_aux(39) <= '1';
          when 16#2_9# =>  CS_Bus_aux(39) <= not Wrt_Int_CS;
          when 16#2_C# =>  CS_Bus_aux(39) <= '1';
          
          when 16#3_4# =>  CS_Bus_aux(39) <= '1';
          
          when 16#3_8# =>  if (DMA_Mode_Latched = '0' and ASI_Int_CS(0) = '0' and
                               Wrt_Int_CS = '0') then -- User mode read forbidden
                             CS_Bus_aux(39) <= '0';
                           else
                             CS_Bus_aux(39) <= '1';
                           end if;
          when 16#3_9# =>  if (DMA_Mode_Latched = '0' and ASI_Int_CS(0) = '0' and
                               Wrt_Int_CS = '0') then -- User mode read forbidden
                             CS_Bus_aux(39) <= '0';
                           else
                             CS_Bus_aux(39) <= '1';
                           end if;
          when 16#3_A# =>  CS_Bus_aux(39)  <= '1';
  
          when others => CS_Bus_aux(39)    <= '0';
    
        end case; -- MEC_r
        
  
  end process;
  
 ---------------------------------------------------------------------------
  MEC_Decoder_P1: process( Low_A, Wrt_Int_CS,
                           DMA_Mode_Latched,
                           DblWord_Access, Word_Access, ASI_Int_CS, 
                           CPUHalt_Out_N_fck)
  begin
  
    Low_A_r <= Low_A(0) or Low_A(1) or Low_A(2) or Low_A(3) or
               Low_A(4) or Low_A(5) or Low_A(6) or Low_A(7);
               
	       
    if  (Wrt_Int_CS = '1' and 
        (Word_Access = '0' or
        (DMA_Mode_Latched = '0' and ASI_Int_CS(0) = '0') or
	      (DMA_Mode_Latched = '1' and CPUHalt_Out_N_fck = '1')
	      ) ) then
	       
         RegAccessViolation <= '1';
     else
         RegAccessViolation <= '0';
     end if;

  end process; -- MEC_Decoder_P1



  MEC_Decoder_P: process(Register_Area_Out_p, Low_A_r, CS_Bus_aux, 
                         RegAccessViolation, MEC_A, DblWord_Access)
  begin

   	for i in 0 to 38 loop
    	 CS_Bus(i) <= CS_Bus_aux(i);

    end loop;
    -- equal to 0 if error    		
   	CS_Bus(39) <= (Low_A_r or not Register_Area_Out_p or
    		            (not DblWord_Access and not RegAccessViolation and CS_Bus_aux(39))) and
                  (Low_A_r or not Register_Area_Out_p or not MEC_A(2)) and
                  (Low_A_r or not Register_Area_Out_p or not MEC_A(1)) and
                  (Low_A_r or not Register_Area_Out_p or not MEC_A(0));
    		             
    		             
  end process;

  
----------------------------------------------------------------------------
--Transparant output latch


  MEC_Decoder_latch_P1: process(CS_BUS, Reset_Out_N, Low_A_r, Register_Area_Out_p,
                                L_Enable_Buf4)
  begin
    if Reset_Out_N = '0' then
      MECControlReg_N     <= '1';
      SWRReg_N            <= '1';
      PWRDReg_N           <= '1';
      MemConfigReg_N      <= '1';
      IOConfigReg_N       <= '1';
      WSConfigReg_N       <= '1';
      MemAccessReg_N      <= '1';
      APSBaseReg0_N       <= '1';
      APSEndReg0_N        <= '1';
      MECRegSel_N         <= '1';
          
    elsif L_Enable_Buf4 ='1' then

      MECControlReg_N     <= CS_Bus(0);
      SWRReg_N            <= CS_Bus(1);
      PWRDReg_N           <= CS_Bus(2);
      MemConfigReg_N      <= CS_Bus(3);
      IOConfigReg_N       <= CS_Bus(4);
      WSConfigReg_N       <= CS_Bus(5);
      APSBaseReg0_N       <= CS_Bus(6);
      APSEndReg0_N        <= CS_Bus(7);
      MemAccessReg_N      <= CS_Bus(6) and CS_Bus(7) and CS_Bus(8) and CS_Bus(9);
      MECRegSel_N         <= Low_A_r or not Register_Area_Out_p;
    
    end if;
  end process;

  MEC_Decoder_latch_P2: process(CS_BUS, Reset_Out_N, L_Enable_Buf5)
  begin
    if Reset_Out_N = '0' then
      IntShapeReg_N       <= '1';
      IntPendReg_N        <= '1';
      IntMaskReg_N        <= '1';
      IntClearReg_N       <= '1';
      IntForceReg_N       <= '1';
      WDProgramReg_N      <= '1';
      WDTDSetReg_N        <= '1';
      RTC_CountReg_N      <= '1';
      RTC_ScalerReg_N     <= '1';
          
    elsif L_Enable_Buf5 ='1' then

      IntShapeReg_N       <= CS_Bus(11);
      IntPendReg_N        <= CS_Bus(12);
      IntMaskReg_N        <= CS_Bus(13);
      IntClearReg_N       <= CS_Bus(14);
      IntForceReg_N       <= CS_Bus(15);
      WDProgramReg_N      <= CS_Bus(16);
      WDTDSetReg_N        <= CS_Bus(17);
      RTC_CountReg_N      <= CS_Bus(18);
      RTC_ScalerReg_N     <= CS_Bus(19);
    
    end if;
  end process;

  MEC_Decoder_latch_P3: process(CS_BUS, Reset_Out_N, L_Enable_Buf6,
                                MemAccess_Violation0_p, MemAccess_Violation1_p,
                                Low_A_r, Register_Area_Out_p)
  begin
    if Reset_Out_N = '0' then
      MemAccess_Violation0 <= '0';
      MemAccess_Violation1 <= '0';
      GPT_A_CountReg_N     <= '1';
      GPT_A_ScalerReg_N    <= '1';
      TimerControlReg_N    <= '1';
      SysFSReg_N           <= '1';
      FFAReg_N             <= '1';
      ErrResStatReg_N      <= '1';
      GUARTAReg_N          <= '1';
      GUARTBReg_N          <= '1';
    
    elsif L_Enable_Buf6 ='1' then
      MemAccess_Violation0 <= MemAccess_Violation0_p;
      MemAccess_Violation1 <= MemAccess_Violation1_p;
      GPT_A_CountReg_N     <= CS_Bus(20);
      GPT_A_ScalerReg_N    <= CS_Bus(21);
      TimerControlReg_N    <= CS_Bus(24);
      SysFSReg_N           <= CS_Bus(25);
      FFAReg_N             <= CS_Bus(26);
      ErrResStatReg_N      <= CS_Bus(29);
      GUARTAReg_N          <= CS_Bus(36) or Low_A_r or not Register_Area_Out_p;
      GUARTBReg_N          <= CS_Bus(37) or Low_A_r or not Register_Area_Out_p;
    
    end if;
  end process;


  
  MEC_Decoder_latch_P4: process(CS_BUS, Reset_Out_N, L_Enable_Buf7, Mem_Area_Int_p,
                                Low_A_r, Register_Area_Out_p)
  begin
    if Reset_Out_N = '0' then
      APSBaseReg1_N       <= '1';
      APSEndReg1_N        <= '1';
      TestControl_Reg_N   <= '1';
      UARTAReg_N          <= '1';
      UARTBReg_N          <= '1';
      UARTStatusReg_N     <= '1';
      GUARTStatusReg_N    <= '1';
      Unimpl_Address_Out  <= '0';
      Mem_Area_Int        <= '0';
    
    elsif L_Enable_Buf7 ='1' then

      APSBaseReg1_N       <= CS_Bus(8);
      APSEndReg1_N        <= CS_Bus(9);
    --  This modification is performed by hand in the netlist
    --  TestControl_Reg_N   <= CS_Bus(34);
      TestControl_Reg_N   <= CS_Bus(34) or Low_A_r or not Register_Area_Out_p;
      UARTAReg_N          <= CS_Bus(36);
      UARTBReg_N          <= CS_Bus(37);
      UARTStatusReg_N     <= CS_Bus(38);
      GUARTStatusReg_N    <= CS_Bus(38) or Low_A_r or not Register_Area_Out_p;
      Unimpl_Address_Out  <= not(CS_Bus(39));
      Mem_Area_Int        <= Mem_Area_Int_p;
    
    end if;
  end process;  
  

  
----------------------------------------------------------------------------
--      RAM Area 0x0200_0000 to 0x03FF_FFFF
--      External output RAM_CS
--      External input RAM_Size
--  
  MemA_Decoder: process(Mem_Cfg_reg, A_Int_CS)
  begin
  
      case Mem_Cfg_reg(12 downto 10) is
      
        when MSize1 => MemA_Int_p  <= A_Int_CS(17 downto 15);
        when MSize2 => MemA_Int_p  <= A_Int_CS(18 downto 16);
        when MSize3 => MemA_Int_p  <= A_Int_CS(19 downto 17);
        when MSize4 => MemA_Int_p  <= A_Int_CS(20 downto 18);
        when MSize5 => MemA_Int_p  <= A_Int_CS(21 downto 19);
        when MSize6 => MemA_Int_p  <= A_Int_CS(22 downto 20);
        when MSize7 => MemA_Int_p  <= A_Int_CS(23 downto 21);
        when others => MemA_Int_p  <= A_Int_CS(24 downto 22);
                       
      end case; -- Ram_Size
  
  end process;

----------------------------------------------------------------------------
  CSvalue_Decoder_P: process(Mem_Cfg_reg, MemA_Int_p)
  begin
        case  Mem_Cfg_Reg(1 downto 0) is
          when "11"   => Csvalue <= MemA_Int_p;
          when "10"   => Csvalue(2) <= '0';
                         Csvalue(1 downto 0) <= MemA_Int_p (2 downto 1);
          when "01"   => Csvalue(2 downto 1) <= "00";
                         Csvalue(0) <= MemA_Int_p(2);
          when others => Csvalue <= "000";
        end case; 
  
  end process;
  
----------------------------------------------------------------------------
 RCS_Decoder: process(Mem_Cfg_reg, CSvalue)
  begin

        if Mem_Cfg_reg(2) = '1' and CSvalue = Mem_Cfg_reg(5 downto 3) then 
           MemSelectR1_p   <= '0';
           MemSelectR2_p   <= '1';
           MemSelectEn_N_p <= '1';
        elsif Mem_Cfg_reg(6) = '1' and CSvalue = Mem_Cfg_reg(9 downto 7) then 
           MemSelectR1_p   <= '1';
           MemSelectR2_p   <= '0';
           MemSelectEn_N_p <= '1';
        else
           MemSelectR1_p   <= '1';
           MemSelectR2_p   <= '1';
           MemSelectEn_N_p <= '0';
        end if;
        
  end process; -- RCS_Decoder
  
----------------------------------------------------------------------------
 CS_Decoder: process(CSvalue)
  begin

     case CSvalue(2 downto 0) is
       when "000"  => MemSelectOut_p <= "11111110";
       when "001"  => MemSelectOut_p <= "11111101";
       when "010"  => MemSelectOut_p <= "11111011";
       when "011"  => MemSelectOut_p <= "11110111";
       when "100"  => MemSelectOut_p <= "11101111";
       when "101"  => MemSelectOut_p <= "11011111";
       when "110"  => MemSelectOut_p <= "10111111";
       when others => MemSelectOut_p <= "01111111";
    end case;
        
  end process; -- CS_Decoder
  
----------------------------------------------------------------------------
  Ram_limit_Decoder: process(Mem_Cfg_reg, Ram_Adr)
  begin

      case Mem_Cfg_reg(12 downto 10) is
      
        when MSize1 => Ram_limit <= Ram_Adr(22) or Ram_Adr(21) or Ram_Adr(20) or
                                    Ram_Adr(19) or Ram_Adr(18) or Ram_Adr(17) or
                                    Ram_Adr(16);
                       
        when MSize2 => Ram_limit <= Ram_Adr(22) or Ram_Adr(21) or Ram_Adr(20) or
                                    Ram_Adr(19) or Ram_Adr(18) or Ram_Adr(17);
                       
        when MSize3 => Ram_limit <= Ram_Adr(22) or Ram_Adr(21) or Ram_Adr(20) or
                                    Ram_Adr(19) or Ram_Adr(18);
                       
        when MSize4 => Ram_limit <= Ram_Adr(22) or Ram_Adr(21) or Ram_Adr(20) or
                                    Ram_Adr(19); 
                       
        when MSize5 => Ram_limit <= Ram_Adr(22) or Ram_Adr(21) or Ram_Adr(20);
                       
        when MSize6 => Ram_limit <= Ram_Adr(22) or Ram_Adr(21);
                       
        when MSize7 => Ram_limit <= Ram_Adr(22);
                       
        when others => Ram_limit <= '0';
                       
      end case; -- Ram_Size
  
  end process; -- Ram_limit_Decoder
  
----------------------------------------------------------------------------
 MemA_illegal_Out_p_Decoder: process(Mem_Area_Int_p, Ram_limit)
  begin

    if Mem_Area_Int_p = '1' and Ram_limit = '1' then
        MemA_illegal_Out_p <= '1';
    else
        MemA_illegal_Out_p <= '0';
    end if;
        
      
  end process; -- MemA_illegal_Out_p_Decoder
  
----------------------------------------------------------------------------
 CSEnable_Decoder: process(Mem_Area_Int_p, Ram_limit)
  begin

    if (Mem_Area_Int_p = '1' and Ram_limit = '1') or Mem_Area_Int_p = '0' then
        CSEnable_N_p <= '1';
    else
        CSEnable_N_p <= '0';
    end if;
        
      
  end process; -- CSEnable_Decoder
 
----------------------------------------------------------------------------
  IO_Decoder: process(IOA_Area_sel, IO_Adr)
  begin
    
    case IOA_Area_sel(3 downto 0) is
      when IOSize1 => IO_dec <= IO_Adr(21) or IO_Adr(20) or IO_Adr(19) or IO_Adr(18) or
                                IO_Adr(17) or IO_Adr(16) or IO_Adr(15) or IO_Adr(14) or
                                IO_Adr(13) or IO_Adr(12) or IO_Adr(11) or IO_Adr(10) or
                                IO_Adr(9)  or IO_Adr(8)  or IO_Adr(7);
                                
      when IOSize2 => IO_dec <= IO_Adr(21) or IO_Adr(20) or IO_Adr(19) or IO_Adr(18) or
                                IO_Adr(17) or IO_Adr(16) or IO_Adr(15) or IO_Adr(14) or
                                IO_Adr(13) or IO_Adr(12) or IO_Adr(11) or IO_Adr(10) or
                                IO_Adr(9)  or IO_Adr(8);
                                
      when IOSize3 => IO_dec <= IO_Adr(21) or IO_Adr(20) or IO_Adr(19) or IO_Adr(18) or
                                IO_Adr(17) or IO_Adr(16) or IO_Adr(15) or IO_Adr(14) or
                                IO_Adr(13) or IO_Adr(12) or IO_Adr(11) or IO_Adr(10) or
                                IO_Adr(9);
                                
      when IOSize4 => IO_dec <= IO_Adr(21) or IO_Adr(20) or IO_Adr(19) or IO_Adr(18) or
                                IO_Adr(17) or IO_Adr(16) or IO_Adr(15) or IO_Adr(14) or
                                IO_Adr(13) or IO_Adr(12) or IO_Adr(11) or IO_Adr(10);
                                
      when IOSize5 => IO_dec <= IO_Adr(21) or IO_Adr(20) or IO_Adr(19) or IO_Adr(18) or
                                IO_Adr(17) or IO_Adr(16) or IO_Adr(15) or IO_Adr(14) or
                                IO_Adr(13) or IO_Adr(12) or IO_Adr(11);
                                
      when IOSize6 => IO_dec <= IO_Adr(21) or IO_Adr(20) or IO_Adr(19) or IO_Adr(18) or
                                IO_Adr(17) or IO_Adr(16) or IO_Adr(15) or IO_Adr(14) or
                                IO_Adr(13) or IO_Adr(12);
                                
      when IOSize7 => IO_dec <= IO_Adr(21) or IO_Adr(20) or IO_Adr(19) or IO_Adr(18) or
                                IO_Adr(17) or IO_Adr(16) or IO_Adr(15) or IO_Adr(14) or
                                IO_Adr(13);
                                
      when IOSize8 => IO_dec <= IO_Adr(21) or IO_Adr(20) or IO_Adr(19) or IO_Adr(18) or
                                IO_Adr(17) or IO_Adr(16) or IO_Adr(15) or IO_Adr(14);
                                
      when IOSize9 => IO_dec <= IO_Adr(21) or IO_Adr(20) or IO_Adr(19) or IO_Adr(18) or
                                IO_Adr(17) or IO_Adr(16) or IO_Adr(15);
                                
      when IOSize10=> IO_dec <= IO_Adr(21) or IO_Adr(20) or IO_Adr(19) or IO_Adr(18) or
                                IO_Adr(17) or IO_Adr(16);
                                
      when IOSize11=> IO_dec <= IO_Adr(21) or IO_Adr(20) or IO_Adr(19) or IO_Adr(18) or
                                IO_Adr(17);
                                
      when IOSize12=> IO_dec <= IO_Adr(21) or IO_Adr(20) or IO_Adr(19) or IO_Adr(18);
      
      when IOSize13=> IO_dec <= IO_Adr(21) or IO_Adr(20) or IO_Adr(19);
      
      when IOSize14=> IO_dec <= IO_Adr(21) or IO_Adr(20);
      
      when IOSize15=> IO_dec <= IO_Adr(21);
      
      when others  => IO_dec <= '0';
    end case; 

  end process; -- IO_decoder


  IOA_legality_1: process(IO_Area_Int_p, IO_Select_Int_p, IO_Cfg_reg)
  begin
 
     case IO_Select_Int_p is
      when "00"   => IOA_Area_sel <= IO_Cfg_reg(4 downto 0);
      when "01"   => IOA_Area_sel <= IO_Cfg_reg(12 downto 8);
      when "10"   => IOA_Area_sel <= IO_Cfg_reg(20 downto 16);
      when others => IOA_Area_sel <= IO_Cfg_reg(28 downto 24);

     end case; 

  end process; 

  
  IOA_legality_2: process(IO_Area_Int_p, IO_dec, IOA_Area_sel)
  begin
    if IO_Area_Int_p = '1' and ( IO_dec = '1' or IOA_Area_sel(4) = '0') then
       IOA_illegal_out_p <= '1';
    else
       IOA_illegal_out_p <= '0';
    end if; 

  end process; 

----------------------------------------------------------------------------

  IO_Decoder_P: process(IO_Area_Int_p, IO_Select_Int_p, IOA_illegal_out_p)
  begin
    if IO_Area_Int_p = '1' and IOA_illegal_out_p = '0' then
    
     case IO_Select_Int_p is
      when "00" =>    IO_Select_out_p   <= "1110";
      when "01" =>    IO_Select_out_p   <= "1101";
      when "10" =>    IO_Select_out_p   <= "1011";
      when "11" =>    IO_Select_out_p   <= "0111";
      when others =>  IO_Select_out_p   <= "1111";                     

     end case; -- IO_Select_Out

     else
       IO_Select_out_p   <= "1111";

    end if; -- IO_Area_Int

  end process; -- IO_Decoder_P

----------------------------------------------------------------------------

  AccessTypeEncoder : process(ASI_Int_In, Wrt_Int_In, DMA_mode,
                              IO_Area_Int_p, Exch_Area_Int_p, IO_Ext_Area_Sel_p)
  begin
    if DMA_mode = '1' then

	    if (IO_Area_Int_p = '1' or Exch_Area_Int_p = '1' or IO_Ext_Area_Sel_p = '1') then
          if Wrt_Int_In = '0' then
          -- DMA load IO/Exchange Access
          	  AccessType_s <= "0111";
          else
          -- DMA store IO/Exchange Access
          	  AccessType_s <= "1111";
	        end if;

      else
          if Wrt_Int_In = '0' then
          -- DMA load RAM/ROM/Register Access
          	  AccessType_s <= "0100";
          else --elsif Wrt_Int_In = '1' then
          -- DMA store RAM/ROM/Register Access
          	  AccessType_s <= "1100";
	        end if;

	    end if;
 
    else 
	     if (IO_Area_Int_p = '1' or Exch_Area_Int_p = '1' or IO_Ext_Area_Sel_p = '1') then
          if Wrt_Int_In = '0' then
            case ASI_Int_In is               
              when "1000" | "1010" =>
              -- IU I/O/Exchange User load/execute access
                AccessType_s <= "0101";
              when "1001" | "1011" =>
              -- IU I/O/Exchange Supervisor load/execute access
                AccessType_s <= "0110";
              when others =>
              -- Not excpected Access
            	  AccessType_s <= (others => 'X');
            end case;

          else

            case ASI_Int_In is 
              when "1000" | "1010" =>
              -- IU I/O/Exchange User store access
                AccessType_s <= "1101";
              when "1001" | "1011" =>
              -- IU I/O/Exchange Supervisor store access
                AccessType_s <= "1110";
              when others =>
              -- Not excpected Access
          	    AccessType_s <= (others => 'X');
            end case;

	        end if;

      else
           --    Register_Area_out = '1') then
          if Wrt_Int_In = '0' then
            case ASI_Int_In is
              when "1000" =>
              -- IU RAM/PROM/Register User Instruction load/execute access
	            AccessType_s <= "0010";
              when "1001" =>
              -- IU RAM/PROM/Register Supervisor Instruction load/execute access
	            AccessType_s <= "0011";
              when "1010" =>
              -- IU RAM/PROM/Register User Data load access
	            AccessType_s <= "0000";
          
              when "1011" =>
              -- IU RAM/PROM/Register Supervisor Data load access
	            AccessType_s <= "0001";
              when others =>
              -- Not excpected Access
          	    AccessType_s <= (others => 'X');               
            end case;

          else 
            case ASI_Int_In is
              when "1000" =>
              -- IU RAM/PROM/Register User Instruction store access
	            AccessType_s <= "1010";          
              when "1001" =>
              -- IU RAM/PROM/Register Supervisor Instruction store access
	            AccessType_s <= "1011";          
              when "1010" =>
              -- IU RAM/PROM/Register User Data store access
                AccessType_s <= "1000";         
              when "1011" =>
              -- IU RAM/PROM/Register Supervisor Data store access
                AccessType_s <= "1001";         
              when others =>
              -- Not excpected Access
          	    AccessType_s <= (others => 'X');               
            end case;
          end if;

	    end if;

	end if;

  end process AccessTypeEncoder;


  AccessTypeLatch: process(Reset_out_N, L_Enable_Buf8, AccessType_s)
  begin
    if Reset_Out_N = '0' then
      AccessType <= "0000";
    elsif L_Enable_Buf8 = '1' then
      AccessType <= AccessType_s;
    end if;
  end process;
                 

-----------------------------------------------------------------------------  
 MemoryAccess  <= Reset_Int_N and Mem_Area_Int; 
        
----------------------------------------------------------------------------
   ReadMemAccess: process(APSBaseReg0_N, APSBaseReg0, APSBaseReg0Par,
                          APSEndReg0_N,  APSEndReg0,  APSEndReg0Par,
                          APSBaseReg1_N, APSBaseReg1, APSBaseReg1Par,
                          APSEndReg1_N,  APSEndReg1,  APSEndReg1Par) 

   begin

  	for i in 0 to 22 loop
    		D_MemAcc(i)   <= (APSBaseReg0(i) and not APSBaseReg0_N) or 
    		                 (APSEndReg0(i)  and not APSEndReg0_N) or
    		                 (APSBaseReg1(i) and not APSBaseReg1_N) or
    		                 (APSEndReg1(i)  and not APSEndReg1_N);
    end loop;
    		                 
  	for i in 23 to 24 loop
    		D_MemAcc(i)   <= (APSBaseReg0(i) and not APSBaseReg0_N) or 
    		                 (APSBaseReg1(i) and not APSBaseReg1_N);
    end loop;
    
    		                       
    D_MemAcc(31 downto 25)   <= "0000000"; 


    DPar_MemAcc   <= (APSBaseReg0Par and not APSBaseReg0_N) or 
    		             (APSEndReg0Par  and not APSEndReg0_N) or
    		             (APSBaseReg1Par and not APSBaseReg1_N) or
    		             (APSEndReg1Par  and not APSEndReg1_N);

  end process;


----------------------------------------------------------------------------
  WriteMemAccess: process(Clk_Int, Reset_Int_N)
  begin
  
    if Reset_Int_N = '0' then -- no protection after reset
       APSBaseReg0    <= "0000000000000000000000000";
       APSEndReg0     <= "00000000000000000000000";
       APSBaseReg1    <= "0000000000000000000000000";
       APSEndReg1     <= "00000000000000000000000";
       APSBaseReg0Par <= '1';
       APSEndReg0Par  <= '1';
       APSBaseReg1Par <= '1';
       APSEndReg1Par  <= '1';
       
    elsif Clk_Int'event and Clk_Int = '0' then
      if  Wr_Int_N = '0' then
        if APSBaseReg0_N = '0' then
           APSBaseReg0(24 downto 0)  <= D_Int_In(24 downto 0);
           APSBaseReg0Par            <= DPar_Int_In;

        elsif APSEndReg0_N = '0' then
         APSEndReg0(22 downto 0)     <= D_Int_In(22 downto 0);
         APSEndReg0Par               <= DPar_Int_In;
         
        elsif APSBaseReg1_N = '0' then
         APSBaseReg1(24 downto 0)    <= D_Int_In(24 downto 0);
         APSBaseReg1Par              <= DPar_Int_In;
         
        elsif APSEndReg1_N = '0' then
         APSEndReg1(22 downto 0)     <= D_Int_In(22 downto 0);
         APSEndReg1Par               <= DPar_Int_In;
        end if;
        
      end if;
      
    end if;
    
  end process; -- MemAccRegProc


----------------------------------------------------------------------------
  ParErr_APSBaseReg0  <= paritycheck(APSBaseReg0, APSBaseReg0Par); 
  ParErr_APSEndReg0   <= paritycheck(APSEndReg0 , APSEndReg0Par); 
  ParErr_APSBaseReg1  <= paritycheck(APSBaseReg1, APSBaseReg1Par); 
  ParErr_APSEndReg1   <= paritycheck(APSEndReg1 , APSEndReg1Par);
   
  ParErr_MemAcc       <= ParErr_APSBaseReg0 or ParErr_APSEndReg0 or
                         ParErr_APSBaseReg1 or ParErr_APSEndReg1; 
  
----------------------------------------------------------------------------
  CheckAccessEn0_Gen : process (MemoryAccess, ASI_Int_In, APSBaseReg0, AccessProtEn, WRT_Int_In) 
  begin

    if MemoryAccess = '1' and WRT_Int_In = '1' and
       ((ASI_Int_In = "1010" and APSBaseReg0(23) = '1') or 
        (ASI_Int_In = "1011" and APSBaseReg0(24) = '1')) then 
       CheckAccessEn0 <= '1';
    else 
       CheckAccessEn0 <= '0';
    end if;

  end process; -- 

----------------------------    
  MemAccess_Violation0_Gen : process (CheckAccessEn0, A_Int_CS,  
                                      APSBaseReg0, APSEndReg0, AccessProtEn) 
  begin

  if CheckAccessEn0 = '1' then 
      
      if (A_Int_CS(24 downto 2) >= APSBaseReg0(22 downto 0)) and 
         (A_Int_CS(24 downto 2) <  APSEndReg0 (22 downto 0)) then
         
          if AccessProtEn = '1' then 
             MemAccess_Violation0_p <= '1';
          else 
             MemAccess_Violation0_p <= '0';
          end if;

      else
            
         if AccessProtEn = '1' then           
            MemAccess_Violation0_p <= '0';
          else
            MemAccess_Violation0_p <= '1';
          end if;         
         
      end if;
      
  else
   
    MemAccess_Violation0_p <= '0';
      
  end if;

  end process;

----------------------------------------------------------------------------
  CheckAccessEn1_Gen : process (MemoryAccess, ASI_Int_In, APSBaseReg1, WRT_Int_In) 
  begin

    if MemoryAccess = '1'  and WRT_Int_In = '1' and 
       ((ASI_Int_In = "1010" and APSBaseReg1(23) = '1') or 
        (ASI_Int_In = "1011" and APSBaseReg1(24) = '1')) then 
       CheckAccessEn1 <= '1';
    else 
       CheckAccessEn1 <= '0';
    end if;

  end process; -- 

----------------------------    
  MemAccess_Violation1_Gen : process (CheckAccessEn1, A_Int_CS,  
                                      APSBaseReg1, APSEndReg1, AccessProtEn) 
  begin

  if CheckAccessEn1 = '1' then 

      if (A_Int_CS(24 downto 2) >= APSBaseReg1(22 downto 0)) and 
         (A_Int_CS(24 downto 2) <  APSEndReg1(22 downto 0)) then
         
          if AccessProtEn = '1' then 
             MemAccess_Violation1_p <= '1';
          else 
             MemAccess_Violation1_p <= '0';
          end if;

      else
          if AccessProtEn = '1' then           
            MemAccess_Violation1_p <= '0';
          else
            MemAccess_Violation1_p <= '1';
          end if;
         
      end if;
      
  else
   
    MemAccess_Violation1_p <= '0';
      
  end if;

  end process;

                                                      ---------------------------------------------------------------------------

  MemAccess_Violation_Gen: process(AccessProtEn, APSBaseReg0, APSBaseReg1,
                                   MemAccess_Violation0, MemAccess_Violation1)
  begin
    if AccessProtEn = '1' or
       (AccessProtEn ='0' and ((APSBaseReg1(23) = '0' and APSBaseReg1(24) = '0') or 
                               (APSBaseReg0(23) = '0' and APSBaseReg0(24) = '0'))) then
       MemAccess_Violation_Out <= MemAccess_Violation0 or MemAccess_Violation1;
    
--    elsif AccessProtEn ='0' and (APSBaseReg0(23) = '1' or APSBaseReg0(24) = '1') and 
--          (APSBaseReg1(23) = '1' or APSBaseReg1(24) = '1') then
    else
       MemAccess_Violation_Out <= MemAccess_Violation0 and MemAccess_Violation1;
    
    end if;
  end process;





end Mini_Spec;
---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:		                   Bus Arbiter
-- File name:                  Baunit.vhd
-- VHDL unit:                  BusArbiter
-- Purpose and functionality:  This unit makes the decision on which unit 
--                             that is the MEC master. Either the IU/FPU or
--                             the DMA unit controls the MEC if not SYSHalt
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;


entity BusArbiter is
    port (Clk_Int          : in std_logic ;
          Reset_Int_N      : in std_logic ;
          LOCK_Int_In      : in std_logic ;
          PowerDown        : in std_logic ;
          DMAREQ_In_N      : in std_logic ;
          DMAEnable_Int    : in std_logic ;
          DMATimeOut_En    : in std_logic ;
          ErrorHalt_N      : in std_logic ;
          SysHalt_In_N     : in std_logic ;
          NewCycle         : in std_logic ;
          LastCycle        : in std_logic ;

          BAError          : out std_logic ;
          RdRst            : out std_logic ;
          BusRequest_Int   : out std_logic ;
          Bhold_Out_N      : out std_logic ;
          DMAGNT_Out_N     : out std_logic ;
          DMA_Mode         : out std_logic ;
          MEXCRequest      : out std_logic ;
          DMATimeOut       : out std_logic ;
          CPUHalt_Out_N    : out std_logic ;
          MECHalt_Int_N    : out std_logic );

end BusArbiter;

---------------------------------------------------------------------------
architecture Mini_Spec of BusArbiter is
---------------------------------------------------------------------------

    signal CounterCount   : Integer range 0 to 1023;
    signal DMACycle       : std_logic ;
    signal IUHaltCycle    : std_logic ;
    signal NonDMACycle    : std_logic ;
    signal NonHaltCycle   : std_logic ;

    signal DMAGNT_Int_N   : std_logic;
    signal CPUHalt_Int_N  : std_logic;
    signal DMATimeOut_Int : std_logic;
    
    signal SysHalt_Clk_N  : std_logic;
    
    signal IU_Mode        : std_logic;


    type BusArbiter_NextState is (IUGranted, IUNonGrantPending, IUPowerDown, 
                                  IUHalted, DMAGranted, DMARelease);

    signal NextStateEnum : BusArbiter_NextState;
    
    signal NextState     : std_logic_vector(2 downto 0);
    
    signal NextStatePar  : std_logic;

---------------------------------------------------------------------------
begin

    StateEnumGen: process(NextState) 
    begin
             case  NextState is
               when "000"          => NextStateEnum <= IUGranted; 
               when "001"          => NextStateEnum <= IUNonGrantPending; 
               when "010"          => NextStateEnum <= IUPowerDown; 
               when "011"          => NextStateEnum <= IUHalted; 
               when "100"          => NextStateEnum <= DMAGranted; 
               when "101"          => NextStateEnum <= DMARelease; 
      	
               when others         => NextStateEnum <= IUGranted;
             end case; 
             
    end process;

      BAError       <= ParityCheck(NextState,NextStatePar);

      DMACycle      <= not(DMAREQ_In_N) and Newcycle and not(Lock_Int_In) and 
                       DMAEnable_Int;

      NonDMACycle   <= DMAREQ_In_N and Newcycle and not(Lock_Int_In);

      IUHaltCycle   <= (not(ErrorHalt_N) or not(SysHalt_Clk_N)) and 
                       Newcycle and not(Lock_Int_In);

      NonHaltCycle  <= ErrorHalt_N  and SysHalt_Clk_N;

--      RdRst         <= To_StdLogic(NextState = IUPowerDown) or
--                       To_StdLogic(NextState = IUHalted);

--      RdRst         <= To_StdLogic(NextState = "010") or
--                       To_StdLogic(NextState = "011");

--      DMA_Mode      <= To_StdLogic(NextState = DMAGranted);
--      DMA_Mode      <= To_StdLogic(NextState = "100");

      MECHalt_Int_N <= SysHalt_Clk_N and CPUHalt_Int_N;

      DMAGNT_Out_N  <= DMAGNT_Int_N;
      
      CPUHalt_Out_N <= CPUHalt_Int_N;
      
      DMATimeOut    <= DMATimeOut_Int;


      Sync_SysHalt : process 
      begin
      wait until Clk_Int'event and Clk_Int = '1';
         SysHalt_Clk_N <= SysHalt_In_N;
      end process Sync_SysHalt;



--------------------------------------------------------------------------
      Bus_Arbiter_Control:
      process(Reset_Int_N, Clk_Int)
      begin
       --------------------------------------------------------------
       -- Goto init state (in reset state)
       -- Reset Timeout timer counter
          --------------------------------------------------------------
       if Reset_Int_N = '0' then

            Bhold_Out_N  	    <= '1';
            BusRequest_Int    <= '0';
            DMAGNT_Int_N      <= '1';
            CPUHalt_Int_N     <= '1';

            NextState         <= "000"; -- IUGranted;
            NextStatePar      <= '1';
            DMA_Mode          <= '0';
            IU_Mode           <= '1';
            RdRst             <= '0';
              
       elsif Clk_Int'event and Clk_Int = '1' then
 
            IU_Mode           <= '0';
            DMA_Mode          <= '0';
            RdRst             <= '0';
         
            case NextState is
            
            ---------------------------------------------------------------
            -- BusArbiter init State, Session Timeout counter value 3FFh
            ---------------------------------------------------------------

            when "000" =>   -- IUGranted
              -------------------------------------------------------------
              -- Goto to IUNonGrantPending state
              -- Reset Timeout timer counter
              -------------------------------------------------------------
              if (DMACycle = '1' or IUHaltCycle = '1' or PowerDown = '1')  
                 then

                  Bhold_Out_N  	  <= '0'; -- stops the IU processing
                  BusRequest_Int  <= '0'; -- wait with IU threestating
                  DMAGNT_Int_N    <= '1'; -- moved to pending state
                  CPUHalt_Int_N   <= '1'; -- moved to pending state

                  NextState       <= "001"; -- IUNonGrantPending;
                  NextStatePar    <= '0';

              -------------------------------------------------------------
              -- Stay in IUGranted state
              -- Reset Timeout timer counter
              -------------------------------------------------------------
              else

                  Bhold_Out_N  	  <= '1';
                  BusRequest_Int  <= '0';
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= '1';

                  NextState       <= "000"; -- IUGranted;
                  NextStatePar    <= '1';
                  IU_Mode         <= '1';
                 
              end if;
              

            when "001" =>
              -------------------------------------------------------------
              -- Goto to DMAGranted state
              -- Run Timeout timer counter
              -------------------------------------------------------------
              if (LastCycle = '1' and DMAREQ_In_N = '0')
                 then

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '1'; -- IU threestating
                  DMAGNT_Int_N    <= '0';
                  CPUHalt_Int_N   <= ErrorHalt_N  and SysHalt_Clk_N;

                  NextState       <= "100"; -- DMAGranted;
                  NextStatePar    <= '0';
                  DMA_Mode        <= '1';

              -------------------------------------------------------------
              -- Goto IUHalted state
              -- Reset Timeout timer counter
              -------------------------------------------------------------
              elsif Lastcycle = '1' and 
                    (ErrorHalt_N = '0' or SysHalt_Clk_N = '0')  then

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '1'; -- IU threestating for direct DMA
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= ErrorHalt_N  and SysHalt_Clk_N;
                  
                  RdRst           <= '1';

                  NextState       <= "011"; -- IUHalted;
                  NextStatePar    <= '1';

              --------------------------------------------------------------
              -- Goto IUPowerDown state
              -- Reset Timeout timer counter
              --------------------------------------------------------------
              elsif Lastcycle = '1' and PowerDown = '1' then

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '1';
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= '1';
                  
                  RdRst           <= '1';

                  NextState       <= "010"; -- IUPowerDown;
                  NextStatePar    <= '0';

              --------------------------------------------------------------
              -- Goto IUGranted state
              -- Reset Timeout timer counter
              --------------------------------------------------------------
              elsif not(DMAREQ_In_N = '0') and not(PowerDown = '1') and 
                    not(ErrorHalt_N = '0' or SysHalt_Clk_N = '0')
                    then

                  Bhold_Out_N  	  <= '1';
                  BusRequest_Int  <= '0';
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= '1';

                  NextState       <= "000"; -- IUGranted;
                  NextStatePar    <= '1';
                  IU_Mode         <= '1';

              ---------------------------------------------------------------
              -- Stay in IUNonGrantPending state
              -- Reset Timeout timer counter
              ---------------------------------------------------------------
              else

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '0'; -- wait with IU threestating
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= '1';

                  NextState       <= "001"; -- IUNonGrantPending;
                  NextStatePar    <= '0';
              end if;

            ------------------------------------------------------------------
            -- BusArbiter in IU halt State, Session Timeout counter value 3FFh
            ------------------------------------------------------------------

            when "011" =>
               ---------------------------------------------------------------
               -- Goto to DMAGranted state
               -- Run Timeout timer counter
               ---------------------------------------------------------------
               if DMAREQ_In_N = '0' then

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '1';
                  DMAGNT_Int_N    <= '0';
                  CPUHalt_Int_N   <= ErrorHalt_N  and SysHalt_Clk_N;

                  NextState       <= "100"; -- DMAGranted;
                  NextStatePar    <= '0';
                  DMA_Mode        <= '1';

               ---------------------------------------------------------------
               -- Goto PowerDown state
               -- Reset Timeout timer counter
               ---------------------------------------------------------------
               elsif not(DMAREQ_In_N = '0') and NonHaltCycle = '1' and 
                     (PowerDown = '1') then

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '1';
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= '1';
                  
                  RdRst           <= '1';

                  NextState       <= "010"; -- IUPowerDown;
                  NextStatePar    <= '0';

               ---------------------------------------------------------------
               -- Goto IUGranted state
               -- Reset Timeout timer counter
               ---------------------------------------------------------------
               elsif not(DMAREQ_In_N = '0') and NonHaltCycle = '1' and 
                     not(Powerdown = '1')  then

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '0';
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= '1';

                  NextState       <= "000"; -- IUGranted;
                  NextStatePar    <= '1';
                  IU_Mode         <= '1';

               ---------------------------------------------------------------
               -- Stay in IUhalted state
               -- Reset Timeout timer counter
               ---------------------------------------------------------------
               else

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '0';
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= ErrorHalt_N and SysHalt_Clk_N;
                  
                  RdRst           <= '1';

                  NextState       <= "011"; -- IUHalted;
                  NextStatePar    <= '1';

                end if;

            ------------------------------------------------------------------
            -- BusArbiter in IU Power Down State, 
            -- Session Timeout counter value 3FFh
            ------------------------------------------------------------------

            when "010" =>
              ----------------------------------------------------------------
              -- Goto to DMAGranted state
              -- Run Timeout timer counter
              ----------------------------------------------------------------
              if  DMAREQ_In_N = '0' then

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '1';
                  DMAGNT_Int_N    <= '0';
                  CPUHalt_Int_N   <= ErrorHalt_N and SysHalt_Clk_N;

                  NextState       <= "100"; -- DMAGranted;
                  NextStatePar    <= '0';
                  DMA_Mode        <= '1';

               ---------------------------------------------------------------
               -- Goto IUHalted state
               -- Reset Timeout timer counter
               ---------------------------------------------------------------
               elsif not(DMAREQ_In_N = '0') and 
                     (ErrorHalt_N = '0' or SysHalt_Clk_N = '0')  then

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '1';
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= ErrorHalt_N and SysHalt_Clk_N;
                  
                  RdRst           <= '1';

                  NextState       <= "011"; -- IUHalted;
                  NextStatePar    <= '1';

               ----------------------------------------------------------------
               -- Goto IUGranted state
               -- Reset Timeout timer counter
               ----------------------------------------------------------------
               elsif not(DMAREQ_In_N = '0') and NonHaltCycle = '1' and 
                     not(PowerDown = '1')  then

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '0';
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= '1';

                  NextState       <= "000"; -- IUGranted;
                  NextStatePar    <= '1';
                  IU_Mode         <= '1';

               ----------------------------------------------------------------
               -- Stay in IUPowerDown state
               -- Reset Timeout timer counter
               ----------------------------------------------------------------
               else

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '1';
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= '1';
                  
                  RdRst           <= '1';

                  NextState       <= "010"; -- IUPowerDown;
                  NextStatePar    <= '0';

                end if;

            -------------------------------------------------------------------
            -- BusArbiter in DMAGranted State, Session Timeout counter active
            -------------------------------------------------------------------

            when "100" =>
               ----------------------------------------------------------------
               -- Goto IUHalted state
               -- Reset Timeout timer counter
               ----------------------------------------------------------------
               if  NonDMACycle = '1' and IUHaltCycle = '1' then

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '1';
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= ErrorHalt_N and SysHalt_Clk_N;
                  
                  RdRst           <= '1';

                  NextState       <= "011"; -- IUHalted;
                  NextStatePar    <= '1';
                  
               -----------------------------------------------------------------
               -- Goto IUPowerDown state
               -- Reset Timeout timer counter
               -----------------------------------------------------------------
               elsif NonDMACycle = '1' and NonHaltCycle = '1' and 
                     (PowerDown = '1') then

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '1';
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= '1';
                  
                  RdRst           <= '1';

                  NextState       <= "010"; -- IUPowerDown;
                  NextStatePar    <= '0';

               -----------------------------------------------------------------
               -- Goto DMARelease state
               -- Reset Timeout timer counter
               -----------------------------------------------------------------
               elsif NonDMACycle = '1' and NonHaltCycle = '1' and 
                     (not(PowerDown = '1') or (DMATimeOut_Int = '1')) 
                     then

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '1';
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= '1';

                  NextState       <= "101"; -- DMARelease;
                  NextStatePar    <= '1';

               -----------------------------------------------------------------
               -- Stay in DMAGranted state
               -- Run Timeout timer counter
               -----------------------------------------------------------------
               else

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '1';
                  DMAGNT_Int_N    <= '0';
                  CPUHalt_Int_N   <= ErrorHalt_N and SysHalt_Clk_N;

                  NextState       <= "100"; -- DMAGranted;
                  NextStatePar    <= '0';
                  DMA_Mode        <= '1';

                end if;

            --------------------------------------------------------------------
            -- BusArbiter in DMARelease State, Session Timeout counter inactive
            --------------------------------------------------------------------

            when "101" =>
              ------------------------------------------------------------------
              -- Goto IUHalted state
              -- Reset Timeout timer counter
              ------------------------------------------------------------------
              if (ErrorHalt_N = '0' or SysHalt_Clk_N = '0') then

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '0';
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= ErrorHalt_N and SysHalt_Clk_N;
                  
                  RdRst           <= '1';

                  NextState       <= "011"; -- IUHalted;
                  NextStatePar    <= '1';

               -----------------------------------------------------------------
               -- Goto IUGranted state
               -- Reset Timeout timer counter
               -----------------------------------------------------------------
               else

                  Bhold_Out_N  	  <= '0';
                  BusRequest_Int  <= '0';
                  DMAGNT_Int_N    <= '1';
                  CPUHalt_Int_N   <= '1';

                  NextState       <= "000"; -- IUGranted;
                  NextStatePar    <= '1';
                  IU_Mode         <= '1';

               end if;

            when others => -- generate a parity error
            
                  NextState       <= "111";
                  NextStatePar    <= '1';
            
             end case;

        end if;
      end process;  -- BusArbiter

---------------------------------------------------------------------------


    Counter: process(Reset_Int_N, Clk_Int)

    begin
    -- Reset Counter
    if Reset_Int_N = '0' then
        CounterCount   <= 1023;
        DMATimeOut_Int <= '0';
        MEXCRequest    <= '0';

    elsif Clk_Int'event and Clk_Int = '0' then
        
        -- Clear Counter
        if (not(DMAGNT_Int_N = '0') or not(DMATimeOut_En = '1')) then
            CounterCount   <= 1023;
            DMATimeOut_Int <= '0';
            MEXCRequest    <= '0';

         -- Count if enabled
        elsif  DMATimeOut_En = '1' and DMAGNT_Int_N = '0' then
        	
            -- Signal DMASession TimeOut if reached
            if (CounterCount = 0) then
                DMATimeOut_Int <= '1';

            -- Signal MEXCRequest if timeout + 1 is reached
            elsif (CounterCount = 1) then
                DMATimeOut_Int <= '0';
                MEXCRequest    <= '1';
                CounterCount   <= CounterCount - 1;
                
            -- Decrement(CounterCount)
            else
                DMATimeOut_Int <= '0';
	              CounterCount   <= CounterCount - 1;
            end if;
        end if;
          
	  end if;
    end process; --Counter
    
    
---------------------------------------------------------------------------
end mini_spec;

---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      MEC EDAC and data bus handling
-- File name:                  \mec\source\eadac2.vhd
-- VHDL unit:                  EDAC
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--


library IEEE;
use IEEE.std_logic_1164.all;

  ---
  entity DBuff is
     port (
          D_In    : in Std_Logic_Vector(31 downto 0);
          D_Out   : buffer Std_Logic_Vector(31 downto 0)
         );

  end DBuff;

  architecture Mini_Spec of DBuff is

  begin

  D_Out <= D_In;

  end Mini_Spec;

---
library MECLibrary;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.std_logic_1164.all;


entity EDAC is
    port (
           Clk_Int              : IN  std_logic;
           Clk_Int_Adv          : IN  std_logic;
           Clk2_Int_Adv         : IN  std_logic;
           Clk2_Int             : IN  std_logic;
           
           ExtHold_In_N         : IN  std_logic;
           ExtCCV_In            : IN  std_logic;
           BHold_Out_N          : IN  std_logic;
           
           Register_Area_Out    : IN  std_logic;
           CorrectedData_Reg_N  : IN  std_logic;
           DPar_Int_Out         : IN  std_logic;
           
           NOPar_In_N           : IN  std_logic;
           ParityEnable         : IN  std_logic;
           
           NewCycle             : IN  std_logic;
           DMA_Mode             : IN  std_logic;
           DMAParity_En         : IN  std_logic;
           
           MHold_Out_N_WOEE     : IN  std_logic;
           DParIO_In            : IN  std_logic;
           Rd_Int_In            : IN  std_logic;
           MemData_Valid        : IN  std_logic;
           MemData_Valid_Ld     : IN  std_logic;
           Null_Int_rck         : IN  std_logic;
           DMAInPrgs            : IN  std_logic;
           EDACEnable           : IN  std_logic;
           IUDataLE             : IN  std_logic;
           MemDataLE            : IN  std_logic;
           ForceNonMemDataLE    : IN  std_logic;
           D_In                 : IN  std_logic_Vector(31 downto 0);
           CB_In                : IN  std_logic_Vector(6 downto 0);
           Reset_Int_N          : IN  std_logic;
           Reset_Out_N          : IN  std_logic;
           TestControl_Reg_N    : IN  std_logic;
           Wr_Int_N             : IN  std_logic;
                      
           StoreByte_Reg_Read_N : IN  std_logic;
           ByteSel              : IN  std_logic_Vector(3 downto 0);
           Rom8Reg_N            : IN  std_logic;
           Rom8Reg_LE           : IN  std_logic;
           DPar_Error_Rst       : IN  std_logic;
                      
           D_HIGHLD             : buffer std_logic_Vector(31 downto 0);
           CorrectDataLatch     : OUT std_logic_Vector(31 downto 0);

           EdacRegSel           : OUT std_logic;
           IntrTest_En          : OUT std_logic;
           SWHalt_En            : OUT std_logic;
           D_EDAC               : OUT std_logic_Vector(31 downto 0);
           DParErr_EDAC         : OUT std_logic;
           CB_Out               : OUT std_logic_Vector(6 downto 0);
           CError_ck1           : OUT std_logic;
           NCError_ck1          : OUT std_logic;
           Error_detect_FSM     : OUT std_logic;
           CError_fck           : OUT std_logic;
           NCError_fck          : OUT std_logic;
           MHold_Out_N          : OUT std_logic;
           MHold_Out_ck1_N      : OUT std_logic;
           
           DParIO_Out           : OUT std_logic;
           DPar_int_in          : buffer std_logic;
           
           DPar_Error_Wr_ck1    : OUT std_logic;
           DPar_Error_fck       : OUT std_logic;
           DPar_Error_ck1       : OUT std_logic
           );
end EDAC;

architecture Mini_Spec of EDAC is

   function mux2 ( Sel : Std_Logic;
                   In1 : std_logic_Vector;
                   In2 : std_logic_Vector ) return std_logic_Vector is
   begin
     if    Sel = '0' then
        return In1;
     else 
        return In2;
     end if;
   end;

   function mux2 ( Sel : std_logic;
                   In1 : std_logic;
                   In2 : std_logic ) return std_logic is
   begin
     if    Sel = '0' then
        return In1;
     else 
        return In2;
     end if;
   end;
   
    signal DPar_Int_In_F : std_logic;
    
    Signal ParityTestEn  : Std_Logic;
    
    Signal MHold_Out_loc1_N  : Std_Logic;
    Signal MHold_Out_loc2_N  : Std_Logic;
    
    Signal IUDLE         : Std_Logic;
    
    Signal NCError_rck     : Std_Logic;
    Signal NCError_fck_loc : Std_Logic;
    Signal EDACEnable_drck : Std_Logic;
    signal D_LOWLD         : std_logic_Vector(31 downto 0);

    signal IUDataLatch     : std_logic_Vector(31 downto 0);
    
    signal IUDataLatch_F   : std_logic_Vector(31 downto 0);
    
    signal DParIO_HIGHLD    : std_logic;
    signal D_Int_Parity     : std_logic;
    signal DPar_int_in_loc  : std_logic;
    signal DPar_Error_Rd_fck: std_logic;
        
    signal Error_detect_N   : Std_Logic;
    
    signal IUDLRst          : Std_Logic;
    signal DPar_Error_Wr_ZR : Std_Logic;
    signal DPar_Error_Wr_ck1_ZR : Std_Logic;

    signal TCR_Reg          : std_logic_Vector(20 downto 0);
    signal TCR_RegPar       : std_logic;
    signal TCR_RegTmp       : Std_Logic_Vector(31 downto 0);
    
    Signal Mem_Test         : Std_Logic;
    Signal CBOut_Latched    : Std_Logic_Vector(7 downto 0);
    Signal ChkGen_Data      : std_logic_Vector(32 downto 0);
    
    Signal MemDataLE_loc       : Std_Logic; 
    Signal MDLE                : Std_Logic; 
    
    Signal MemDataLatchPar     : Std_Logic; 
       
    Signal MemDataLatch_loc    : Std_Logic_Vector(31 downto 0);
    Signal CBLatch             : Std_Logic_Vector(7 downto 0);
    Signal CBLatch_1           : Std_Logic_Vector(3 downto 0);
    Signal CBLatch_2           : Std_Logic_Vector(7 downto 4);
    
    Signal MemDataLatch_F    : Std_Logic_Vector(31 downto 0);
    Signal CBLatch_F         : Std_Logic_Vector(7 downto 0);
    Signal CBLatch_F_1       : Std_Logic_Vector(3 downto 0);
    Signal CBLatch_F_2       : Std_Logic_Vector(7 downto 4);

    Signal SSWMemData        : Std_Logic_Vector(31 downto 0);
    
    Signal IUDataParLatch    : Std_Logic;

    Signal Rom8_Reg          : Std_Logic_Vector(31 downto 0);
    Signal Rom8_Par          : Std_Logic;
    Signal CB_Out_Int        : Std_Logic_Vector(7 downto 0);
        
    Signal SSWGen_Chk        : Std_Logic_Vector(7 downto 0);
    Signal CB_bull           : Std_Logic_Vector(7 downto 0);
    Signal BullEn            : Std_Logic;

    Signal CorrectData       : std_logic_Vector(32 downto 0);
    Signal CorrectDataParity : std_logic;
           
    Signal EDACErrorEn       : Std_Logic;
    
    Signal CError_detect     : Std_Logic;
    
    Signal NCError_detect    : Std_Logic;
    Signal NCError_ck1_loc   : Std_Logic;
    
    Signal DPar_Error_Rst_rck : Std_Logic;
    
    Signal PErrorEn_fck2_loc  : Std_Logic;
    Signal PErrorEn_fck2      : Std_Logic;
    
    Signal ParErrorEn         : Std_Logic;
    Signal DParErrorRd_detect : Std_Logic;
    
    Signal EErrorEn_fck2_loc  : Std_Logic;
    Signal EErrorEn_fck2      : Std_Logic;
    
    Signal CondEError         : Std_Logic;
    Signal CondPError         : Std_Logic;
        
    Signal Clk_Int_OnCK2fck   : Std_Logic;
    Signal Clk_MDL_loc_In     : Std_Logic;
    Signal Clk_MDL_F_In       : Std_Logic;
    
    Signal Clk_MDL_loc        : Std_Logic;
    Signal Clk_MDL_F          : Std_Logic;
    
    Signal DPar_Error_Wr_ck1_loc  : Std_Logic;
    Signal DPar_Error_Wr_rckd : Std_Logic;
    
    Signal DParError_Rd_ck1   : Std_Logic;
    
    Signal LEnable            : Std_Logic;
    Signal LReset_N           : Std_Logic;
    
    Signal DPar_EDAC_loc      : Std_Logic;
    Signal EdacRegSel_loc     : Std_Logic;
    
    Signal CorrectedDataPar   : Std_Logic;
    
    Signal EDACEnable_rck     : Std_Logic;
    Signal ParityEnable_rck   : Std_Logic;
    
    Signal Rom8_Par_Int       : Std_Logic;  
      
    Signal DPar_Error_ck1_loc    : Std_Logic;    

            
    component DBuff
    port (
          D_In  : in Std_Logic_Vector(31 downto 0);
          D_Out : buffer Std_Logic_Vector(31 downto 0)
         );
    end component;

    component GPBuff
    port (
          GP_In  : in Std_Logic;
          GP_Out : out Std_Logic
         );
    end component;
    
begin
  
  IntrTest_En      <= TCR_Reg(19);
  SWHalt_En        <= TCR_Reg(20);
  ParityTestEn     <= TCR_Reg(18);
  CB_Out           <= CB_Out_Int(6 downto 0);
   
  Data_Buff1 : DBuff
  port map (D_In  => D_In,
            D_Out => D_LOWLD
            );


              
  Data_Buff2 : DBuff
  port map (D_In  => D_LOWLD,
            D_Out => D_HIGHLD
            );


              
  DPar_Buff1 : GPBuff
    port map (
              GP_In  => DParIO_In,
              GP_Out => DParIO_HIGHLD
              );
              
              
              
  DataParityBuff: process(D_HIGHLD)
  begin
    D_Int_Parity  <= ParityGen(D_HIGHLD);
  end process;
  

  -------------------------------------------------------------------------------------  
  IUDataParity: process( D_Int_Parity, EdacRegSel_loc, DPar_EDAC_loc,
                         Register_Area_Out, DPar_Int_Out,
                         CorrectedData_Reg_N, CorrectDataParity,
                         Rd_Int_In, CB_Out_Int, ParityTestEn)
  begin
    
    if CorrectedData_Reg_N = '0' then
        DParIO_Out <= CorrectDataParity;
    elsif EdacRegSel_loc = '1' then
        DParIO_Out <= DPar_EDAC_loc xor ParityTestEn;
    elsif Register_Area_Out = '1' then
        DParIO_Out <= DPar_Int_Out xor ParityTestEn;
    elsif Rd_Int_In = '1'  then
        DParIO_Out <= D_Int_Parity;
    else
        DParIO_Out <= CB_Out_Int(7);
    end if; 
    
  end process;


  -------------------------------------------------------------------------------------    
  InternalDataParity: process( NOPar_In_N, D_Int_Parity, DParIO_HIGHLD, 
                               DMA_Mode, DMAParity_En)
  begin
     -- Parity generation for MEC internal use (registers) 
      if (NOPar_In_N = '0') or (DMA_Mode = '1' and DMAParity_En = '0') then
        DPar_Int_in_loc <= D_Int_Parity;
      else
        DPar_int_in_loc <= DParIO_HIGHLD;
      end if;
  
  end process;
  
  
  DPar_int_in <= DPar_int_in_loc;

  DPar_Buff : GPBuff
    port map (
              GP_In  => DPar_int_in_loc,
              GP_Out => DPar_Int_In_F
              );
              

---------------------------------------------------------------------
-- Process that control writing to the TCR and TDR registers ---------
----------------------------------------------------------------------
    WriteRegs: process(Reset_Int_N, Clk_Int)
    begin
     if Reset_Int_N = '0' then
          TCR_Reg(6 downto 0)  <= "0000000";
          TCR_Reg(20 downto 17)  <= "0000";
          TCR_RegPar            <= '1';
          
     elsif Clk_Int'event and Clk_Int = '0' then
       if Wr_Int_N = '0' then
        
          if TestControl_Reg_N = '0' then
            TCR_Reg(6 downto 0)   <= D_HIGHLD(6 downto 0);
            TCR_Reg(20 downto 17) <= D_HIGHLD(20 downto 17);
            TCR_RegPar            <= DPar_Int_In_F;
          end if;
          
       end if;
      end if;
    end process;

    TCR_Reg(16 downto 7)  <= "0000000000";

   -----------------------------------------------------------------
   -- IU Data latch handling (data latched comes from IU)
   -----------------------------------------------------------------
   
   process
   begin
     wait until Clk_Int'event and Clk_Int = '1';
       DPar_Error_Rst_rck <= DPar_Error_Rst;
   end process;


   IUDLRst <=  not Reset_Out_N or DPar_Error_Rst_rck or Null_Int_rck;

         
   ----      
   IUDLEPROC: process(Clk_Int, IUDataLE)
   begin
     IUDLE <= Clk_Int and IUDataLE;
   end process;

   ----   
   IUDataLatch_FPROC1: process(IUDLRst, IUDLE, D_LOWLD)
   begin
     if IUDLRst = '1' then      
       IUDataLatch_F(7 downto 0)  <= "00000000";
       IUDataLatch_F(8)           <= '0';
       IUDataLatch_F(31 downto 9) <= "00000000000000000000000";
     elsif IUDLE = '1' then 
       IUDataLatch_F(7 downto 0)  <= D_LOWLD(7 downto 0);
       IUDataLatch_F(8)           <= D_LOWLD(8);
       IUDataLatch_F(31 downto 9) <= D_LOWLD(31 downto 9);
     end if;
   end process;
   

   -----
   IUDataLatchPROC1: process(IUDLRst, IUDLE, D_HIGHLD)
   begin
     if IUDLRst = '1' then      
       IUDataLatch <= "00000000000000000000000000000000";
     elsif IUDLE = '1' then 
       IUDataLatch <= D_HIGHLD;
     end if;
   end process;
   

   ----
   IUDataLatchPROC5: process(IUDLRst, IUDLE, DPar_Int_In_F)
   begin
     if IUDLRst = '1' then      
       IUDataParLatch <= '1';
     elsif IUDLE = '1' then 
       IUDataParLatch <= DPar_Int_In_F;
     end if;
   end process;


   ----------------------------------------------------------------
   -- ROM8Register handling (data comes from memory)
   ----------------------------------------------------------------
         
   Rom8_RegPROC: process
   begin
   wait until Clk_Int'event and Clk_Int = '1';
       
     if ROM8Reg_LE = '1' then
         
         if ByteSel(0) = '1' then    -- load byte 0
           Rom8_Reg( 7 downto  0) <= D_LOWLD(7 downto 0);
         end if;
         
         if ByteSel(1) = '1' then -- load byte 1
           Rom8_Reg(15 downto  8) <= D_LOWLD(7 downto 0);
         end if;
         
         if ByteSel(2) = '1' then -- load byte 2
           Rom8_Reg(23 downto 16) <= D_LOWLD(7 downto 0);
         end if;
         
         if ByteSel(3) = '1' then -- load byte 3 (ba = 00), first byte fetched
           Rom8_Reg(31 downto 24) <= D_LOWLD(7 downto 0);
         end if;
                
     end if;
   end process;
   
   
   Rom8_RegParPROC: process
   begin
   wait until Clk_Int'event and Clk_Int = '0';
       
     if ROM8Reg_LE = '1' then
         
         if ByteSel(1) = '1' then -- load byte 1
	         Rom8_Par_Int  <= ParityGen(Rom8_Reg(15 downto  8)) xor not Rom8_Par_Int;
         end if;
         
         if ByteSel(2) = '1' then -- load byte 2
	         Rom8_Par_Int  <= ParityGen(Rom8_Reg(23 downto 16)) xor not Rom8_Par_Int;
         end if;
         
         if ByteSel(3) = '1' then -- load byte 3
	         Rom8_Par_Int  <= ParityGen(Rom8_Reg(31 downto 24));
         end if;
                
     end if;
   end process;

	 Rom8_Par  <= ParityGen(Rom8_Reg(7 downto 0)) xor not Rom8_Par_Int;

   -----------------------------------------------------------------
   -- Memory Data latch handling (data latched comes from memory)
   -----------------------------------------------------------------
   -- MemDataLatch_loc holds the data when MemDataLE = 1;
   
    fck2Sync: process
    begin
      wait until Clk2_Int_Adv'event and Clk2_Int_Adv = '0';
 
        Clk_Int_OnCK2fck   <= Clk_Int_Adv;
              
    end process;
   
   Clk_MDL_loc_In  <= Clk_Int_Adv or Clk_Int_OnCK2fck;
   
   
   Clk_MDL_loc_Inbuffer : GPBuff
    port map (GP_In  => Clk_MDL_loc_In,
              GP_Out => Clk_MDL_loc
              );


      
   MDLEPROC1: process(Clk_MDL_loc, MemDataLE)
   begin
     MemDataLE_loc <= not(Clk_MDL_loc or MemDataLE);
   end process;

   MDLEPROC2: process(MemDataLE_loc, ForceNonMemDataLE)
   begin
     MDLE <= MemDataLE_loc or ForceNonMemDataLE;
   end process;


   ----
  
   MemDatat_LatchPROC1: process(MDLE, D_LOWLD, Reset_Out_N)
   begin
    if Reset_Out_N = '0' then
       MemDataLatch_loc(7 downto 0)  <= "00000000";       
       MemDataLatch_loc(8)           <= '0';       
       MemDataLatch_loc(31 downto 9) <= "00000000000000000000000";       
    elsif MDLE = '1' then
       MemDataLatch_loc(7 downto 0)  <= D_LOWLD(7 downto 0);       
       MemDataLatch_loc(8)           <= D_LOWLD(8);       
       MemDataLatch_loc(31 downto 9) <= D_LOWLD(31 downto 9);       
    end if;
   end process;
    

   ----
   
   CB_LatchPROC9: process(MDLE, CB_In, Reset_Out_N)
   begin
    if Reset_Out_N = '0' then
       CBLatch_1       <= "0000";
    elsif MDLE = '1' then
       CBLatch_1       <= CB_In(3 downto 0);
    end if;
   end process;
    
   ----
   
   CBandDatat_LatchPROC10: process(MDLE, CB_In, DParIO_HIGHLD, Reset_Out_N)
   begin
    if Reset_Out_N = '0' then
       CBLatch_2             <= "0000";
    elsif MDLE = '1' then
       CBLatch_2(6 downto 4) <= CB_In(6 downto 4);
       CBLatch_2(7)          <= DParIO_HIGHLD;
    end if;
   end process;
    
   CBLatch <= CBLatch_2 & CBLatch_1;

   -----------------------------------------------------------------
   -- Memory Data latch handling (data latched comes from memory)
   -- Specific one for error detection
   -----------------------------------------------------------------
   -- MemDataLatch_loc holds the data when MemDataLE = 1;
   -- It is not necessary to hold the data when CError because 
   -- the correction cycle lasts only one extra cycle.
   
   MemData_LatchPROC1F: process(MDLE, D_In, Reset_Out_N)
   begin
    if Reset_Out_N = '0' then
       MemDataLatch_F <= "00000000000000000000000000000000";       
    elsif MDLE = '1' then
       MemDataLatch_F <= D_In;
    end if;
   end process;
    

   ----

   CBandDatat_LatchPROC9F: process(MDLE, CB_In, Reset_Out_N)
   begin
    if Reset_Out_N = '0' then
       CBLatch_F_1       <= "0000";
    elsif MDLE = '1' then
       CBLatch_F_1       <= CB_In(3 downto 0);
    end if;
   end process;
    
   ----
      
   CBandDatat_LatchPROC10F: process(MDLE, CB_In, DParIO_In, Reset_Out_N)
   begin
    if Reset_Out_N = '0' then
       CBLatch_F_2       <= "0000";
    elsif MDLE = '1' then
       CBLatch_F_2(6 downto 4) <= CB_In(6 downto 4);
       CBLatch_F_2(7)          <= DParIO_In;
    end if;
   end process;
    
    ---
    CBLatch_F <= CBLatch_F_2 & CBLatch_F_1;
    
        
----------------------------------------------------------------------
-- Process for the generation of the Checkbits on the output
----------------------------------------------------------------------
    BullEn_PROC: process(Mem_Test, ByteSel)
    begin
       BullEn <= not (ByteSel(0) and ByteSel(1) and ByteSel(2) and ByteSel(3)) or Mem_Test;
    end process;

    ------
    IUChk_Out_Gen: process (IUDataLatch_F, ChkGen_Data, BullEn, CB_bull)
     Variable IUGen_Chk  : std_logic_vector(7 downto 0);

    begin

      IUGen_Chk(6 downto 0) := ChkGen (IUDataLatch_F);
      IUGen_Chk(7)          := ChkGen_Data(32);
      
      CB_Out_Int <= mux2 (BullEn, IUGen_Chk, CB_bull);
      
    end process;


    SSWChk_Out_Gen: process ( ByteSel, IUDataLatch, SSWMemData)

     Variable Word_Data  : DataBits;
     Variable Word_Par   : std_logic;

    begin

      ---- 4 Byte multiplexor between IUDataLatch and  SSWMemData(31 downto 0)
      
      
      Word_Data(7  downto 0 ) := mux2(ByteSel(0),SSWMemData(7  downto 0 ),IUDataLatch(7  downto 0 ));
      Word_Data(15 downto 8 ) := mux2(ByteSel(1),SSWMemData(15 downto 8 ),IUDataLatch(15 downto 8 ));
      Word_Data(23 downto 16) := mux2(ByteSel(2),SSWMemData(23 downto 16),IUDataLatch(23 downto 16));
      Word_Data(31 downto 24) := mux2(ByteSel(3),SSWMemData(31 downto 24),IUDataLatch(31 downto 24));

      Word_Par                := ParityGen(Word_Data);

      ---- Assign ChkGen_Data with the data to the Checkbits generator
      ChkGen_Data(31 downto 0) <= Word_Data;
      ChkGen_Data(32)          <= Word_Par;

      ---- Generate CheckBits
      SSWGen_Chk(6 downto 0) <= ChkGen (Word_Data);
      SSWGen_Chk(7)          <= Word_Par;
      
    end process;

    ------
    SSWCB_Out_PROC: process(Mem_Test, SSWGen_Chk, TCR_Reg)
    begin
        CB_bull <= mux2 (Mem_Test, SSWGen_Chk, TCR_Reg(7 downto 0));
    end process;


    ----------------------------------------------------------------------
    -- Correct Data   
    ----------------------------------------------------------------------
    CorrectDataGen: process (CBLatch, MemDataLatch_loc)

    variable Ok_Chk     : CheckBits;
    variable In_Chk     : CheckBits;
    variable Syn_DataP  : std_logic_Vector(32 downto 0);
    variable Synd       : SyndromBits;
    variable DinParity  : std_logic;
    
    begin

        Syn_DataP(31 downto 0) := MemDataLatch_loc(31 downto 0);                         
        Syn_DataP(32)          := CBLatch(7);
        DinParity              := not ParityGen(MemDataLatch_loc);
                
        ---- Generate checkbits and then syndrom bits
        Ok_Chk  := ChkGen(MemDataLatch_loc(31 downto 0));
        In_Chk  := CBLatch(6 downto 0);
        Synd    := SyndromGenS(In_Chk, Ok_Chk, DinParity, CBLatch(7));
        
        CorrectData <= Correct_Data (Synd, Syn_DataP);
               
    end process;

    
    ----------------------------------------------------------------------
    -- CError and NCerror Calculation 
    ----------------------------------------------------------------------
    cond_Gen: process(EDACEnable, DMAInPrgs, NOPar_In_N, ParityEnable)
    begin
    
    CondEError <= EDACEnable and not DMAInPrgs;
    
    CondPError <= not EDACEnable and NOPar_In_N and ParityEnable and  
                  not DMAInPrgs;
      
    end process;
    
    
    rckSync: process
    begin
      wait until Clk2_Int'event and Clk2_Int = '0';
 
        EErrorEn_fck2_loc   <= MemData_Valid_Ld and CondEError;
        PErrorEn_fck2_loc   <= MemData_Valid_Ld and CondPError;
       
    end process;
    
    
    EErrorEn_fck2  <= EErrorEn_fck2_loc and not Null_Int_rck;
    PErrorEn_fck2  <= PErrorEn_fck2_loc and not Null_Int_rck;

    
    EDACErrorEn_Gen: process(MemData_Valid, EDACEnable, Null_Int_rck)
    begin
    
      EDACErrorEn <= MemData_Valid and EDACEnable and not Null_Int_rck;
      
    end process;


    ParErrorEnGen: process(EDACEnable, NOPar_In_N, ParityEnable, MemData_Valid, Null_Int_rck)
    begin
  
      ParErrorEn <=  not EDACEnable and NOPar_In_N and ParityEnable and MemData_Valid and 
                     not Null_Int_rck;

    end process;
  
                
    NC_C_Error_Detect: process (CBLatch_F, MemDataLatch_F, EDACErrorEn, ParErrorEn)

    variable Ok_Chk       : CheckBits;
    variable Syndrom      : SyndromBits;
    variable CError_loc   : std_logic;
    variable DinParity    : std_logic;
    
    begin
            
      Ok_Chk         := ChkGen(MemDataLatch_F(31 downto 0)); 
      
      -- Quand on n'a pas d'erreur Synd7 = 1, 
      -- or  Synd7= cb7 xor DinParity = cb7 xor not ParityGen(MemDataLatch_F)
      
      DinParity      := not ParityGen(MemDataLatch_F);
            
      Syndrom        := SyndromGenS (CBLatch_F(6 downto 0), Ok_Chk, DinParity, CBLatch_F(7));
        
      CError_loc     := CError_Gen (Syndrom);
            
      DParErrorRd_detect  <= (not Syndrom(7) and ParErrorEn);
      
      CError_detect       <= CError_loc and EDACErrorEn;
      
      NCError_detect      <= (Not (CError_loc or 
                                  (Syndrom(7) and 
                                   not (Syndrom(6) or Syndrom(5) or
                                        Syndrom(4) or Syndrom(3) or
                                        Syndrom(2) or Syndrom(1) or
                                        Syndrom(0)))))  and EDACErrorEn;
                                  
    end process;


    Error_detect_FSM_Gen: process (CBLatch_F, MemDataLatch_F, EErrorEn_fck2, PErrorEn_fck2)

    variable Ok_Chk       : CheckBits;
    variable Syndrom      : SyndromBits;
    variable CError_loc   : std_logic;
    variable DinParity    : std_logic;
    
    begin
            
      Ok_Chk         := ChkGen(MemDataLatch_F(31 downto 0)); 
            
      DinParity      := not ParityGen(MemDataLatch_F);
      
      Syndrom        := SyndromGenS (CBLatch_F(6 downto 0), Ok_Chk, DinParity, CBLatch_F(7));
                  
      Error_detect_N <= Not ((not Syndrom(7) and PErrorEn_fck2) or
                             ((not(Syndrom(7) and 
                               not (Syndrom(6) or Syndrom(5) or
                                    Syndrom(4) or Syndrom(3) or
                                    Syndrom(2) or Syndrom(1) or
                                    Syndrom(0)))) and EErrorEn_fck2));

    end process;

    
    fckSync1a: process(Reset_Out_N, Clk_Int)
    begin
                   
     if Reset_Out_N = '0' then               
        CError_fck       <= '0';
        NCError_fck_loc  <= '0';
     elsif Clk_Int'event and Clk_Int = '0' then    
        CError_fck       <= CError_detect;
        NCError_fck_loc  <= NCError_detect;
     end if;
     
    end process;
    
    NCError_fck   <= NCError_fck_loc;

    rckSync1a: process(Reset_Out_N, Clk_Int)
    begin
                   
     if Reset_Out_N = '0' then               
        NCError_rck      <= '0';
        EDACEnable_drck  <= '0';
     elsif Clk_Int'event and Clk_Int = '1' then    
        NCError_rck      <= NCError_fck_loc;
        EDACEnable_drck  <= EDACEnable_rck;
        
     end if;
     
    end process;


    -- CError_ck1 is used in the AC for selection of correct data register
    CError_latch: process(Clk_Int, CError_detect)
    begin
    
      if Clk_Int = '1' then     
        CError_ck1 <= CError_detect;        
      end if;
      
    end process;


    NCError_latch: process(Clk_Int, NCError_detect)
    begin
    
      if Clk_Int = '1' then     
        NCError_ck1_loc <= NCError_detect;        
      end if;
      
    end process;
                      
    NCError_ck1 <= NCError_ck1_loc;        
                      
    
    Error_detect_FSM  <= not Error_detect_N;                                
    


    DParError_Rd_ck1_latch: process(Clk_Int, DParErrorRd_detect)
    begin
    
      if Clk_Int = '1' then     
        DParError_Rd_ck1 <= DParErrorRd_detect;        
      end if;
      
    end process;
    
    
    MHold_Out_loc1_N  <= MHold_Out_N_WOEE and Error_detect_N;
    
    MHold_Out_N       <= MHold_Out_loc1_N;

    MHold_Out_loc1_N_Buff1 : GPBuff
    port map (
              GP_In  => MHold_Out_loc1_N,
              GP_Out => MHold_Out_loc2_N
              );
              
    MHold_Out_ck1_N_latch: process(Clk_Int, MHold_Out_loc2_N, BHold_Out_N,
                                   ExtHold_In_N, ExtCCV_In)
    begin
    
      if Clk_Int = '1' then     
       --  This modification is performed by hand in the netlist
       -- MHold_Out_ck1_N <= MHold_Out_loc2_N;
        MHold_Out_ck1_N <= MHold_Out_loc2_N and BHold_Out_N and 
                           ExtHold_In_N and ExtCCV_In;                    
  
      end if;
      
    end process;
                        
  --------------------------------------------------------------------------
  -- Detection of Data Parity error during a store 
  --------------------------------------------------------------------------
  

  DPar_Error_Wr_ZR_Gen: process(NOPar_In_N, DMA_Mode, DMAParity_En)
  begin      

     if (NOPar_In_N = '0') or (DMA_Mode = '1' and DMAParity_En = '0') then
        DPar_Error_Wr_ZR  <= '1';
     else
        DPar_Error_Wr_ZR  <= '0';
     end if;
      
  end process;

  DPar_Error_Wr_ck1_Gen: process(DPar_Error_Wr_ZR, IUDataLatch, IUDataParLatch)
  begin      

     if DPar_Error_Wr_ZR = '1' then
        DPar_Error_Wr_ck1_loc  <= '0';
     else
        DPar_Error_Wr_ck1_loc  <= parityCheck(IUDataLatch, IUDataParLatch); -- data comes from IU
     end if;
      
  end process;


  DPar_Error_Wr_ZR_Gen1: process(DPar_Error_Wr_ZR, DMA_Mode, Null_Int_rck)
  begin      

     if DPar_Error_Wr_ZR = '1' or DMA_Mode = '1' or Null_Int_rck = '1' then
        DPar_Error_Wr_ck1_ZR  <= '1';
     else
        DPar_Error_Wr_ck1_ZR  <= '0';
     end if;
      
  end process;


  -- This is a data parity detection dedicated to the FSM of the access controler
  DPar_Error_Wr_ck1_Gen1: process(DPar_Error_Wr_ck1_ZR, D_LOWLD, DParIO_HIGHLD)
  begin      

     if DPar_Error_Wr_ck1_ZR = '1' then
        DPar_Error_Wr_ck1  <= '0';
     else
        DPar_Error_Wr_ck1  <= parityCheck(D_LOWLD, DParIO_HIGHLD); -- data comes from IU
     end if;
      
  end process;

  
  --------------------------------------------------------------------------------
  -- Generation of DPar_Error = DPar_ErrorRd + DPar_ErrorWr
  --------------------------------------------------------------------------------
  
       
  --to the access controller and to fault handler    
   DPar_Error_ck1_loc   <= DPar_Error_Wr_ck1_loc or DParError_Rd_ck1;   

   
    DPar_Error_fck_Gen: process(Reset_Out_N, Clk_Int)
    begin
     if Reset_Out_N = '0' then
       DPar_Error_fck <= '0';
     elsif Clk_Int'event and Clk_Int = '0' then
       DPar_Error_fck <= DPar_Error_ck1_loc;      
     end if;

    end process;


   DPar_Error_ck1   <= DPar_Error_ck1_loc;   
                                            
    
    ------------------------------------------------------------------------
    -- Data handling in DMA Mode. During DMA load, the data is output by the MEC
    ------------------------------------------------------------------------
   rsync: process(Reset_Out_N, Clk_Int)
   begin
    if Reset_Out_N = '0' then 
        EDACEnable_rck   <= '0';
        ParityEnable_rck <= '0';
    elsif Clk_Int'event and Clk_Int ='1' then
        EDACEnable_rck   <= EDACEnable;
        ParityEnable_rck <= ParityEnable;
    end if;
       
    end process;
    
    
    MDLPar_PROC: process(MemDataLatch_loc)
    begin
        MemDataLatchPar  <= ParityGen(MemDataLatch_loc(31 downto 0));
    end process;
     
   
    DMAData_PROC: process(EDACEnable_rck, EDACEnable_drck, ParityEnable_rck, NCError_rck,
                          NCError_fck_loc,
                          CorrectData, MemDataLatch_loc, CBLatch_2, MemDataLatchPar)
    begin
 
    if (EDACEnable_rck = '1' or EDACEnable_drck = '1') and 
        NCError_fck_loc = '0' and NCError_rck = '0' then
        CorrectDataLatch(31 downto 0) <= CorrectData(31 downto 0); 
        CorrectDataParity             <= CorrectData(32);        
    else
        CorrectDataLatch(31 downto 0) <= MemDataLatch_loc(31 downto 0); 
        
        if ParityEnable_rck = '1' then       
          CorrectDataParity  <= CBLatch_2(7);
        else
          CorrectDataParity  <= MemDataLatchPar;
        end if;
        
    end if;
      
    end process;

    ------------------------------------------------------------------------
    -- Data coming from memory in order to rebuild the word to be written
    -- in case of SSW operation.
    -- When EDAC is enable, this data is corrected. Otherwise, it is the data
    -- latched in MemDataLatch_loc
    ------------------------------------------------------------------------
    SSWMemData_PROC: process(EDACEnable, CorrectData, MemDataLatch_loc)
    begin
 
    if EDACEnable = '1' then
        SSWMemData <= CorrectData(31 downto 0);        
    else
        SSWMemData <= MemDataLatch_loc(31 downto 0);        
    end if;
      
    end process;


   ---------------------------------------------------------------------------
   -- Data mux
   ---------------------------------------------------------------------------

    TCR_RegTmp            <= "00000000000" & TCR_Reg(20 downto 0);

    ReadRegs: process(TestControl_Reg_N, TCR_RegTmp, TCR_RegPar,
                      StoreByte_Reg_Read_N, ChkGen_Data,
                      Rom8Reg_N, Rom8_Par, Rom8_Reg
                      )

    begin
    
      if StoreByte_Reg_Read_N = '0' then    -- Read the data from the bytemux
         D_EDAC         <= ChkGen_Data(31 downto 0);
         DPar_EDAC_loc  <= ChkGen_Data(32);
         
      elsif Rom8Reg_N = '0' then 
         D_EDAC         <= Rom8_Reg;
         DPar_EDAC_loc  <= Rom8_Par;

      else 
         D_EDAC         <= TCR_RegTmp;
         DPar_EDAC_loc  <= TCR_RegPar;

      end if;

    end process;
    
    DParErr_EDAC   <= ParityCheck(TCR_Reg(20 downto 0),TCR_RegPar);
    
    EdacRegSel_loc <= not(TestControl_Reg_N and 
                          Rom8Reg_N and StoreByte_Reg_Read_N);

    EdacRegSel     <= EdacRegSel_loc;

   ---------------------------------------------------------------------------
   ----           Process that controls the CB test select signal  ----------
   ---------------------------------------------------------------------------
    
    Mem_Test_gen: process(TCR_Reg)
    begin
      if TCR_Reg(17) = '1' then
         Mem_Test <= '1';
      else
         Mem_Test <= '0';
      end if;   
    end process;
    
end Mini_Spec ;

---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      Memory and IO configuration registers
-- File name:                  \mec\source\memiocnf.vhd
-- VHDL unit:                  Mem_io_config_E
-- Purpose and functionality:  MEC description
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--


library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;


entity Mem_io_config_E is
    port ( Mem_cfg_reg          : BUFFER Std_Logic_Vector(31 downto 0);
           IO_cfg_reg           : BUFFER Std_Logic_Vector(31 downto 0);
           D_MemIoConfig_out    : OUT Std_Logic_Vector(31 downto 0);
           DPar_MemIOConfig_out : OUT Std_Logic;
           ParErrMemIOConfig    : OUT Std_Logic;
           D_Int_In             : IN Std_Logic_Vector(31 downto 0);
           MemConfigReg_N       : IN Std_Logic;
           IOConfigReg_N        : IN Std_Logic;
           Wr_Int_N             : IN Std_Logic;
           DPar_Int_In          : IN Std_Logic;
           CLK_Int              : IN Std_Logic;
           Reset_Int_N          : IN Std_Logic;
           Load_FFAR_N          : IN Std_Logic;
           FFAReg_N             : IN Std_Logic;
           PROM8_In_N           : IN Std_Logic;
           A_Int_In             : IN Std_Logic_Vector(31 downto 0)
           );
end Mem_io_config_E;

architecture Mini_Spec of Mem_io_config_E is

  Signal PROM8Reg         : Std_Logic;
  Signal Mem_Cfg_regPar_G : Std_Logic;
  Signal Mem_Cfg_regPar   : Std_Logic;
  Signal IO_Cfg_regPar    : Std_Logic;
  Signal FFAR_Reg         : Std_Logic_Vector(31 downto 0);
  Signal FFAR_RegPar         : Std_Logic;
  
  Signal FFAR_TempReg1    : Std_Logic_Vector(31 downto 0);
  Signal FFAR_TempPar1    : Std_Logic;

  Signal FFAR_TempReg2    : Std_Logic_Vector(31 downto 0);
  Signal FFAR_TempPar2    : Std_Logic;

begin

    Mem_Cfg_reg(15)           <= '0';
    Mem_Cfg_reg(17)           <= PROM8_In_N;
    Mem_Cfg_reg(23 downto 21) <= "000";
    Mem_Cfg_reg(31 downto 30) <= "00";
      
    IO_cfg_reg(6)             <= '0';
    IO_cfg_reg(7)             <= '0';
    IO_cfg_reg(14)            <= '0';
    IO_cfg_reg(15)            <= '0';
    IO_cfg_reg(22)            <= '0';
    IO_cfg_reg(23)            <= '0';
    IO_cfg_reg(30)            <= '0';
    IO_cfg_reg(31)            <= '0';
      
  -----
  Registers: process(Reset_Int_N, Clk_Int)
  begin  
    
  if Reset_Int_N = '0' then
    Mem_cfg_reg(14 downto 0)  <= "000000000000000";
    Mem_cfg_reg(16)           <= '1'; -- PROM write function enabled
    Mem_Cfg_reg(20 downto 18) <= "000";
    Mem_Cfg_reg(29 downto 24) <= "000000";
    Mem_Cfg_regPar            <= PROM8_In_N;
      
    PROM8Reg                  <= PROM8_In_N;
      
    IO_cfg_reg(5 downto 0)    <= "000000";
    IO_cfg_reg(13 downto 8)   <= "000000";
    IO_cfg_reg(21 downto 16)  <= "000000";
    IO_cfg_reg(29 downto 24)  <= "000000";
    IO_Cfg_regPar             <= '1';
      
  elsif Clk_Int'event and Clk_Int = '0' then
  
    if Wr_Int_N = '0' then
       if MemConfigReg_N  = '0' then
         Mem_cfg_reg(14 downto 0)  <= D_Int_In(14 downto 0);
         Mem_cfg_reg(16)           <= D_Int_In(16);
         Mem_Cfg_reg(20 downto 18) <= D_Int_In(20 downto 18);
         Mem_Cfg_reg(29 downto 24) <= D_Int_In(29 downto 24);
         Mem_Cfg_regPar            <= DPar_Int_in xor (D_Int_In(17) xor PROM8_In_N);
            
         PROM8Reg                  <= PROM8_In_N;
       end if;
                        
       if IOConfigReg_N  = '0' then
         IO_cfg_reg(5 downto 0)    <= D_Int_In(5 downto 0);
         IO_cfg_reg(13 downto 8)   <= D_Int_In(13 downto 8);
         IO_cfg_reg(21 downto 16)  <= D_Int_In(21 downto 16);
         IO_cfg_reg(29 downto 24)  <= D_Int_In(29 downto 24);
         IO_Cfg_regPar             <= DPar_Int_in;
       end if;
          
    end if;
    
  end if;
    
    
  end process;

    
  Mem_Cfg_regPar_G <= Mem_Cfg_regPar xor (PROM8Reg xor PROM8_In_N);

  -----
  D_MemIoConfigPROC: process(MemConfigReg_N, Mem_cfg_reg, Mem_Cfg_regPar_G,
                             IOConfigReg_N, IO_cfg_reg, IO_Cfg_regPar,
                             FFAReg_N, FFAR_reg, FFAR_RegPar)
  begin
  
 	for i in 0 to 31 loop
 	
    D_MemIoConfig_out(i) <= (Mem_cfg_reg(i) and not MemConfigReg_N) or
                            (IO_cfg_reg(i) and not IOConfigReg_N) or
    	                      (FFAR_reg(i) and not FFAReg_N);
  end loop;
    		
  DPar_MemIOConfig_out <= (Mem_Cfg_regPar_G and not MemConfigReg_N) or
    		                  (IO_Cfg_regPar and not IOConfigReg_N) or
    		                  (FFAR_RegPar and not FFAReg_N);

    
  end process;

  -----
  FFAR_Regs1: process(Reset_Int_N, Clk_Int)
  begin
  if Reset_Int_N = '0' then
      FFAR_TempReg1 <= (others => '0');
      FFAR_TempPar1 <= '1';
            
  elsif Clk_Int'event and Clk_Int = '1' then
      FFAR_TempReg1 <= A_Int_In;
      FFAR_TempPar1 <= ParityGen(A_Int_In);
     
  end if;
        
  end process;
  
  FFAR_Regs2: process(Reset_Int_N, Clk_Int)
  begin
  if Reset_Int_N = '0' then
      FFAR_TempReg2 <= (others => '0');
      FFAR_TempPar2 <= '1';
            
  elsif Clk_Int'event and Clk_Int = '1' then
     FFAR_TempReg2 <= FFAR_TempReg1;
      FFAR_TempPar2 <= FFAR_TempPar1;
     
  end if;
        
  end process;
  
  FFAR_Regs: process(Reset_Int_N, Clk_Int)
  begin
    if Reset_Int_N = '0' then
      FFAR_Reg    <= (others => '0');
      FFAR_RegPar <= '1';
      
  elsif Clk_Int'event and Clk_Int = '0' then
  
    if Load_FFAR_N = '0' then
      FFAR_Reg    <= FFAR_TempReg2;
      FFAR_RegPar <= FFAR_TempPar2;
    end if;
    
  end if;

  end process;
  

  -----
  Parity_Chk_P : Process (Mem_cfg_reg, Mem_Cfg_regPar_G,
                          IO_cfg_reg, IO_Cfg_regPar,
                          FFAR_Reg, FFAR_RegPar)
    variable ParErrMemConfig : Std_Logic;
    variable ParErrIOConfig  : Std_Logic;
    variable ParErrFFAR      : Std_Logic;
  begin
    ParErrMemConfig    := parityCheck(Mem_cfg_reg, Mem_Cfg_regPar_G);
    ParErrIOConfig     := parityCheck(IO_cfg_reg, IO_Cfg_regPar);
    ParErrFFAR         := parityCheck(FFAR_Reg, FFAR_RegPar);
    ParErrMemIOConfig  <= ParErrMemConfig or ParErrIOConfig or ParErrFFAR;
  end process;



end Mini_Spec;


---------------------------------------------------------------------------
-- Title:                      Address and data input latches
-- File name:                  \mec\source\mec_lat.vhd
-- VHDL unit:                  Mec_latch_E
---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      Address and control input latches
-- File name:                  \mec\source\mec_lat.vhd
-- VHDL unit:                  Mec_latch_E
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--


library MECLibrary;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;


entity Mec_latch_E is
    port ( 
           A_int_in             : buffer Std_Logic_Vector(31 downto 0);
           ASI_int_in           : buffer Std_Logic_Vector(3 downto 0);
           SIZE_int_in          : buffer Std_Logic_Vector(1 downto 0);
           
           APar_Error           : out Std_Logic;
           
           CtlPar_Error         : out Std_Logic;

           RD_In                : in  Std_Logic;
           Rd_Int_In            : out Std_Logic;
           WRT_In               : in  Std_Logic;
           WRT_Int_In           : out Std_Logic;
           DXFER_In             : in  Std_Logic;
           LOCK_In              : in  Std_Logic;
           LOCK_Int_In          : out Std_Logic;
           LDSTO_Int_In         : out Std_Logic;

           A_in                 : in Std_Logic_Vector(31 downto 0);
           APar_in              : in Std_Logic;
           ASI_in               : in Std_Logic_Vector(3 downto 0);
           Size_in              : in Std_Logic_Vector(1 downto 0);
           LDSTO_In             : in Std_Logic;
           IMPar_In             : in Std_Logic;
           WE_In_N              : in Std_Logic;
           ASPar_in             : in Std_Logic;
           NewCycle             : in Std_Logic;
           NOPar_In_N           : in Std_Logic;
           DMA_Mode             : in Std_Logic;
           DMAInPrgs            : in Std_Logic;
           DMAParity_En         : in Std_Logic;
           Reset_Int_N          : in Std_Logic;
           Reset_Out_N          : in Std_Logic;
           Clk_Int              : in Std_Logic;
           DMAAS_In             : in Std_Logic
           );

end MEC_latch_E;

architecture Mini_Spec of MEC_latch_E is

component GPBuff
    port (
          GP_In  : in Std_Logic;
          GP_Out : out Std_Logic
         );
end component;



component LBuff
    port (
          Clk1 : in Std_Logic;
          Clk2 : in Std_Logic;
          LBuf : out Std_Logic
         );
end component;


 
 signal  LDSTO_Int_loc  : Std_Logic;
 signal  WE_Int_In_N    : Std_Logic;
 signal  APar_Int_In    : Std_Logic;
 signal  ASPar_Int_In   : Std_Logic;
 signal  IMPar_Int_In   : Std_Logic;
 signal  Wrt_Internal   : Std_Logic;
 signal  Wrt_Latched    : Std_Logic;
 signal  Rd_Internal    : Std_Logic;
 signal  DXFER_Internal : Std_Logic;
 signal  LOCK_Internal  : Std_Logic;
 
 signal  DMA_Mode_fck   : Std_Logic;
   
 
--   attribute async_set_reset of Reset_Out_N : signal is "true"; --SYNOP  
		
begin


-- Parity checking on Address and Control signals
  ParCheck : process( Reset_Int_N, A_int_in, APar_int_in, ASI_int_in, Size_int_in,
                      ASPar_int_in, DXFER_Internal, LDSTO_Int_loc, LOCK_Internal,
                      RD_Internal, WE_Int_In_N, Wrt_Internal ,IMPar_Int_In,
                      NOPar_In_N, DMA_Mode, DMAInPrgs, DMAParity_En )
  begin
  
    if (NOPar_In_N = '0') or 
     --  This modification is performed by hand in the netlist
     --  (DMA_Mode = '1' and DMAParity_En = '0') then
       ((DMA_Mode = '1' or DMAInPrgs = '1') and DMAParity_En = '0') then
      -- No checking
      APar_Error   <= '0';
      CtlPar_Error <= '0';   
    else
      -- Check parity of MEC_in busses.
      APar_Error    <= Reset_Int_N and (
                       parityCheck(A_int_in, APar_int_in) or
                       parityCheck((ASI_int_in & Size_int_in), ASPar_int_in));
                        
      CtlPar_Error  <= Reset_Int_N and (
                       parityCheck((DXFER_Internal & LDSTO_Int_loc & LOCK_Internal &
                       Rd_Internal & WE_Int_In_N & Wrt_Internal),IMPar_Int_In));
    end if;
    
  end process; --ParCheck


-- Register clocking data at positive edge.
  Register_P : Process(Reset_Out_N, Clk_Int)
    -- Need to delay DMA_Mode one half cycle to disable a latching
    -- when DMA releases the bus
  begin
      
  if Reset_Out_N = '0' then 
      A_int_in       <= (others => '0');
      APar_Int_in    <= '1';  -- Odd parity
      ASI_int_in     <= (others => '0');
      Size_int_in    <= (others => '0');
      ASPar_Int_in   <= '1'; -- Odd parity
      Wrt_Internal   <= '0';
      Wrt_Latched    <= '0';
      DXFER_Internal <= '0';
      LOCK_Internal  <= '0';
      LDSTO_Int_loc  <= '0';
      WE_Int_In_N    <= '1';
      IMPar_Int_In   <= '0';
      Rd_Internal    <= '0';
      
  elsif Clk_Int'event and Clk_Int = '1' then
  
    if (DMA_Mode_fck = '0' and NewCycle = '1') or
       (DMA_Mode_fck = '1' and DMAAS_In = '1') then

        -- Do not update wrt in the second cycle in a double word access
        if (Size_in /= "11") or (Size_int_in /= "11") then
          Wrt_Latched <= Wrt_In;
        end if;

        Wrt_Internal   <= Wrt_In;
        A_int_in       <= A_in;
        APar_Int_in    <= APar_in;  -- Odd parity
        ASI_int_in     <= ASI_in;
        Size_int_in    <= Size_in;
        ASPar_Int_in   <= ASPar_in; -- Odd parity
        DXFER_Internal <= DXFER_In;
        LOCK_Internal  <= LOCK_In;
        LDSTO_Int_loc  <= LDSTO_In;
        WE_Int_In_N    <= WE_In_N;
        IMPar_Int_In   <= IMPar_In;
        Rd_Internal    <= RD_In;

    end if;
    
  end if;
  end process; --Register_P


  LOCK_Int_In    <= LOCK_Internal;
  Wrt_Int_In     <= Wrt_Latched;
  Rd_Int_In      <= Rd_Internal;
  LDSTO_Int_In   <= LDSTO_Int_loc;


  process
  begin    
    wait until (Clk_Int'event and Clk_Int = '0');
    DMA_Mode_fck <= DMA_Mode;
  end process;

  
end Mini_Spec;

---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      System Bus Interface
-- File name:                  \mec\source\sysbif.vhd
-- VHDL unit:                  SystemBusInterface
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--


library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE; 
use IEEE.std_logic_1164.all; 

entity SystemBusInterface is
    port (
          Clk_Int           : in Std_Logic;
          Clk2_Int          : in Std_Logic;
          BusTimeOut_En     : in Std_Logic;
          PowerDown         : in Std_Logic;
          MemData_Valid     : out Std_Logic;
                    
          Lock_In           : in Std_Logic;
          Reset_Int_N       : in Std_Logic;
          APar_In           : in Std_Logic;
          DParIO_in         : in Std_Logic;
          ASPar_In          : in Std_Logic;
          DMAAS_in          : in Std_Logic;
          DRdy_Out_N        : out Std_Logic;
          inull_In          : in Std_Logic;
          Wrt_In            : in Std_Logic;
          D_In              : in Std_Logic_Vector(31 downto 0);
          RomWrt_In_N       : in Std_Logic;
          NoPar_In_N        : in Std_Logic;
          BusErr_In_N       : in Std_Logic;
          DMAReq_In_N       : in Std_Logic;
          ExtHold_In_N      : in Std_Logic;
          ExtCCV_In         : in Std_Logic;
          ASI_In            : in Std_Logic_Vector(3 downto 0);
          LdSto_In          : in Std_Logic;
          CB_In             : in Std_Logic_Vector(6 downto 0);
          PROM8_In_N        : in Std_Logic;
          BusRdy_In_N       : in Std_Logic;
          A_In              : in Std_Logic_Vector(31 downto 0);
          
          A_Int_in          : out Std_Logic_Vector(31 downto 0);
          SFSRTmp_UD        : Out Std_Logic;
          
          WE_In_N           : in Std_Logic;
          Rd_In             : in Std_Logic;
          DXfer_In          : in Std_Logic;
          IMPar_In          : in Std_Logic;
          Size_In           : in Std_Logic_Vector(1 downto 0);
          Reset_Out_N       : in Std_Logic;
          AccessProtEn      : in Std_Logic;
          DMAEnable_Int     : in Std_Logic;
          DMATimeOut_En     : in Std_Logic;
          DMAParity_En      : in Std_Logic;
          DPar_Int_Out      : in Std_Logic;
          MExc_Int          : in Std_Logic;
          ErrorHalt_N       : in Std_Logic;
          SysHalt_In_N      : in Std_Logic;
          Write_Inhibit     : in Std_Logic;
          Load_FFAR_N       : in  Std_Logic;
          NewCycle          : out Std_Logic;
          CBWr_Out_N        : out Std_Logic;
          D_Int_In          : buffer Std_Logic_Vector(31 downto 0);
          Rd_Int_In         : out Std_Logic;
          MemBEn_Out_N      : out Std_Logic;
          RAMBEn_Out_N      : out Std_Logic;
          ROMBEn_Out_N      : out Std_Logic;
          COE_Out_N         : out Std_Logic;
          ExMCS_Out_N       : out Std_Logic;
          IOBEn_Out_N       : out Std_Logic;
          OE_Out_N          : out Std_Logic;
          MemWr_Out_N       : out Std_Logic;
          DParIO_Out        : out Std_Logic;
          CB_Out            : out Std_Logic_Vector(6 downto 0);
          DDir_Out          : out Std_Logic;
          IOWr_Out_N        : out Std_Logic;
          CBEn              : out Std_Logic;
          DMAGnt_Out_N      : out Std_Logic;
          BHold_Out_N       : out Std_Logic;
          AOE_Out_N         : out Std_Logic;
          MemCS_Out_N       : out Std_Logic_Vector(9 downto 0);
          Wr_Int_N          : buffer Std_Logic;
          MDS_Out_N         : out Std_Logic;
          MHold_Out_N       : out Std_Logic;
          DPar_Int_In       : buffer Std_Logic;
          MExc_Out_N        : out Std_Logic;
          BA_Out            : out Std_Logic_Vector(1 downto 0);
          MECControlReg_N   : buffer Std_Logic;
          ALE_Out_N         : out Std_Logic;
          RomCS_Out_N       : out Std_Logic;
          IOSel_Out_N       : out Std_Logic_Vector(3 downto 0);
          DOE_Out_N         : out Std_Logic;
          MemConfigReg_N    : buffer Std_Logic;
          IOConfigReg_N     : buffer Std_Logic;
	        IntShapeReg_N     : out Std_Logic;
	        IntPendReg_N      : out Std_Logic;
	        IntMaskReg_N      : out Std_Logic;
	        IntClearReg_N     : out Std_Logic;
	        IntForceReg_N     : out Std_Logic;
	        WDProgramReg_N    : out Std_Logic;
	        WDTDSetReg_N      : out Std_Logic;
          RTC_CountReg_N    : out Std_Logic;
          RTC_ScalerReg_N   : out Std_Logic; 
          GPT_A_CountReg_N  : out Std_Logic; 
          GPT_A_ScalerReg_N : out Std_Logic; 
          TimerControlReg_N : out Std_Logic;
           
          GUARTAReg_N       : out Std_Logic;
          GUARTBReg_N       : out Std_Logic;
          GUARTStatusReg_N  : out Std_Logic;
          
          MECRegSel_N       : out Std_Logic;
          
          UARTAReg_N        : out Std_Logic;
          UARTBReg_N        : out Std_Logic;
          UARTStatusReg_N   : out Std_Logic;
          SysFSReg_N        : out Std_Logic;
          ErrResStatReg_N   : out Std_Logic;
	        FFAReg_N          : out Std_Logic;
          TestControl_Reg_N : out Std_Logic; 
          MemAccessReg_N    : out Std_Logic;
          StoreByte_Reg_Read_N : out Std_Logic;
          CorrectedData_Reg_N  : out Std_Logic;
          ROM8Reg_N            : out Std_Logic;
          SWRReg_N             : out Std_Logic;
          PWRDReg_N            : out Std_Logic;
          IntrTest_En          : out Std_Logic;
          SWHalt_En            : out Std_Logic;
          MemAccess_Violation_Out    : out Std_Logic;
          AccessType          : out Std_Logic_Vector(3 downto 0);
          Unimpl_Address_Out  : out Std_Logic;
          Illegal_Address_Out : out Std_Logic;
          APar_Error          : out Std_Logic;
          DPar_Error_ck1      : out Std_Logic;
          CtlPar_Error        : out Std_Logic;
          NCError_ck1         : out Std_Logic;
          CError_ck1          : out Std_Logic;
          DMATimeOut          : out Std_Logic;
          DBEn                : out Std_Logic;
          DParIOEn            : out Std_Logic;
          SysBus_Error        : out Std_Logic;
          BusTimeOut          : buffer Std_Logic;
          CorrectDataLatch    : out Std_Logic_Vector(31 downto 0);
          D_EDAC              : out Std_Logic_Vector(31 downto 0);
          DParErr_EDAC        : out Std_Logic;
          D_MemIoConfig_out   : out Std_Logic_Vector(31 downto 0);
          DPar_MemIOConfig_out: out Std_Logic;
          ParErrMemIOConfig   : out Std_Logic;
          D_MemAcc            : out Std_Logic_Vector(31 downto 0);
          DPar_MemAcc         : out Std_Logic;
          ParErr_MemAcc       : out Std_Logic;
          MECHalt_Int_N       : out Std_Logic;
          Instr_Fetch         : out Std_Logic;
          Null_Int_rck        : out Std_Logic;
          CPUHalt_Out_N       : out Std_Logic;
          ParityEnable        : out Std_Logic;
          EDACEnable          : out Std_Logic;
          MHold_Out_ck1_N     : out Std_Logic;
          DoNotRdUART         : out Std_Logic;
          DMAInPrgs           : out  std_logic;
          
          MECRegister_Access  : out Std_Logic);
end SystemBusInterface;

---------------------------------------------------------------------------
architecture mini_spec of SystemBusInterface is
component AddressDecoder
    port ( Reset_Out_N         : in Std_Logic;
           Clk_Int             : in Std_Logic;
           Clk2_Int            : in Std_Logic;
           CPUHalt_Out_N       : in Std_Logic;
           NewCycle            : in Std_Logic;
           RdRst               : in std_logic;
           StrobeGate          : in std_logic;
           
           Size_In             : in Std_Logic_Vector(1 downto 0);
           A_In                : in Std_Logic_Vector(31 downto 2);
           ASI_In              : in Std_Logic_Vector(3 downto 0);
           ASI_Int_In          : in Std_Logic_Vector(3 downto 0);
           WRT_In              : in Std_Logic;
           WRT_Int_In          : in Std_Logic;
           
           DMAAS_In            : in Std_Logic;
           DMA_Mode            : in Std_Logic;
           Prom8_In_N          : in Std_Logic;
           RomWrt_In_N         : in Std_Logic;

           Mem_Cfg_reg         : in Std_Logic_Vector(31 downto 0);
           IO_Cfg_reg          : in Std_Logic_Vector(31 downto 0);
           
           AccessProtEn      : in Std_Logic;
           Reset_Int_N       : in Std_Logic;
           D_Int_In          : in Std_Logic_Vector(31 downto 0);
           DPar_Int_In       : in Std_Logic;
           Wr_Int_N          : in Std_Logic;
          
          
           MemAccess_Violation_Out : out Std_Logic;
           D_MemAcc          : out Std_Logic_Vector(31 downto 0);
           DPar_MemAcc       : out Std_Logic;
           ParErr_MemAcc     : out Std_Logic;
           
           -- Output signals.
           MemCs_Out_N         : out Std_Logic_Vector(9 downto 0);
           ExMCS_Out_N         : out Std_Logic;
           IOSel_Out_N         : out Std_Logic_Vector(3 downto 0);
           RomCs_Out_N         : out Std_Logic;

           Illegal_Address_Out : out Std_Logic;
           AccessType          : out Std_Logic_Vector(3 downto 0);
           Register_Area_Out   : buffer Std_Logic;

           ParityEnable        : out Std_Logic;
           EDACEnable          : out Std_Logic;

           MECControlReg_N     : buffer Std_Logic;
           SWRReg_N            : out Std_Logic;
           PWRDReg_N           : out Std_Logic;
           MemConfigReg_N      : buffer Std_Logic;
           IOConfigReg_N       : buffer Std_Logic;
           WSConfigReg_N       : out Std_Logic;
           MemAccessReg_N      : out Std_Logic;
           IntShapeReg_N       : out Std_Logic;
           IntPendReg_N        : out Std_Logic;
           IntMaskReg_N        : out Std_Logic;
           IntClearReg_N       : out Std_Logic;
           IntForceReg_N       : out Std_Logic;
           WDProgramReg_N      : out Std_Logic;
           WDTDSetReg_N        : out Std_Logic;
           RTC_CountReg_N      : out Std_Logic;
           RTC_ScalerReg_N     : out Std_Logic;
           GPT_A_CountReg_N    : out Std_Logic;
           GPT_A_ScalerReg_N   : out Std_Logic;
           TimerControlReg_N   : out Std_Logic;
           SysFSReg_N          : out Std_Logic;
           FFAReg_N            : out Std_Logic;
           ErrResStatReg_N     : out Std_Logic;
           TestControl_Reg_N   : out Std_Logic;
           
           GUARTAReg_N         : out Std_Logic;
           GUARTBReg_N         : out Std_Logic;
           GUARTStatusReg_N     : out Std_Logic;
           
           UARTAReg_N          : out Std_Logic;
           UARTBReg_N          : out Std_Logic;
           UARTStatusReg_N     : out Std_Logic;
           
           MECRegSel_N         : out Std_Logic;

           Unimpl_Address_Out  : out Std_Logic
           );
end component;

component Mec_latch_E
port ( 
           A_int_in             : buffer Std_Logic_Vector(31 downto 0);
           ASI_int_in           : buffer Std_Logic_Vector(3 downto 0);
           SIZE_int_in          : buffer Std_Logic_Vector(1 downto 0);
           
           APar_Error           : out Std_Logic;
           
           CtlPar_Error         : out Std_Logic;

           RD_In                : in  Std_Logic;
           Rd_Int_In            : out Std_Logic;
           WRT_In               : in  Std_Logic;
           WRT_Int_In           : out Std_Logic;
           DXFER_In             : in  Std_Logic;
           LOCK_In              : in  Std_Logic;
           LOCK_Int_In          : out Std_Logic;
           LDSTO_Int_In         : out Std_Logic;

           A_in                 : in Std_Logic_Vector(31 downto 0);
           APar_in              : in Std_Logic;
           ASI_in               : in Std_Logic_Vector(3 downto 0);
           Size_in              : in Std_Logic_Vector(1 downto 0);
           LDSTO_In             : in Std_Logic;
           IMPar_In             : in Std_Logic;
           WE_In_N              : in Std_Logic;
           ASPar_in             : in Std_Logic;
           NewCycle             : in Std_Logic;
           NOPar_In_N           : in Std_Logic;
           DMA_Mode             : in Std_Logic;
           DMAInPrgs            : in Std_Logic;
           DMAParity_En         : in Std_Logic;
           Reset_Int_N          : in Std_Logic;
           Reset_Out_N          : in Std_Logic;
           Clk_Int              : in Std_Logic;
           DMAAS_In             : in Std_Logic
           );
end component;

component BusArbiter
port (    Clk_Int          : in Std_Logic;
          Reset_Int_N      : in Std_Logic;
          LOCK_Int_In      : in Std_Logic;
          PowerDown        : in Std_Logic;
          DMAREQ_In_N      : in Std_Logic;
          DMAEnable_Int    : in Std_Logic;
          DMATimeOut_En    : in Std_Logic;
          ErrorHalt_N      : in Std_Logic;
          SysHalt_In_N     : in Std_Logic;
          NewCycle         : in Std_Logic;
          LastCycle        : in Std_Logic;
          BAError          : out std_logic;
          RdRst            : out Std_Logic;
          BusRequest_Int   : out Std_Logic;
          BHold_Out_N      : out Std_Logic;
          DMAGNT_Out_N     : out Std_Logic;
          DMA_Mode         : out Std_Logic;
          MEXCRequest      : out Std_Logic;
          DMATimeOut       : out Std_Logic;
          CPUHalt_Out_N    : out Std_Logic;
          MECHalt_Int_N    : out Std_Logic );
end component;

component EDAC
port (
           Clk_Int              : IN  std_logic;
           Clk_Int_Adv          : IN  std_logic;
           Clk2_Int_Adv         : IN  std_logic;
           Clk2_Int             : IN  std_logic;
                 
           ExtHold_In_N         : IN  std_logic;
           ExtCCV_In            : IN  std_logic;
           BHold_Out_N          : IN  std_logic;
           
           Register_Area_Out    : IN  std_logic;
           CorrectedData_Reg_N  : IN  std_logic;
           DPar_Int_Out         : IN  std_logic;
           
           NOPar_In_N           : IN  std_logic;
           ParityEnable         : IN  std_logic;
           
           NewCycle             : IN  std_logic;
           DMA_Mode             : IN  std_logic;
           DMAParity_En         : IN  std_logic;
           
           MHold_Out_N_WOEE     : IN  std_logic;
           DParIO_In            : IN  std_logic;
           Rd_Int_In            : IN  std_logic;
           MemData_Valid        : IN  std_logic;
           MemData_Valid_Ld     : IN  std_logic;
           Null_Int_rck         : IN  std_logic;
           DMAInPrgs            : IN  std_logic;
           EDACEnable           : IN  std_logic;
           IUDataLE             : IN  std_logic;
           MemDataLE            : IN  std_logic;
           ForceNonMemDataLE    : IN  std_logic;
           D_In                 : IN  std_logic_Vector(31 downto 0);
           CB_In                : IN  std_logic_Vector(6 downto 0);
           Reset_Int_N          : IN  std_logic;
           Reset_Out_N          : IN  std_logic;
           TestControl_Reg_N    : IN  std_logic;
           Wr_Int_N             : IN  std_logic;
           
           StoreByte_Reg_Read_N : IN  std_logic;
           ByteSel              : IN  std_logic_Vector(3 downto 0);
           Rom8Reg_N            : IN  std_logic;
           Rom8Reg_LE           : IN  std_logic;
           DPar_Error_Rst       : IN  std_logic;
                      
           D_HIGHLD             : buffer std_logic_Vector(31 downto 0);
           CorrectDataLatch     : OUT std_logic_Vector(31 downto 0);

           EdacRegSel           : OUT std_logic;
           IntrTest_En          : OUT std_logic;
           SWHalt_En            : OUT std_logic;
           D_EDAC               : OUT std_logic_Vector(31 downto 0);
           DParErr_EDAC         : OUT std_logic;
           CB_Out               : OUT std_logic_Vector(6 downto 0);
           CError_ck1           : OUT std_logic;
           NCError_ck1          : OUT std_logic;
           Error_detect_FSM     : OUT std_logic;
           CError_fck           : OUT std_logic;
           NCError_fck          : OUT std_logic;
           MHold_Out_N          : OUT std_logic;
           MHold_Out_ck1_N      : OUT std_logic;
           
           DParIO_Out           : OUT std_logic;

           DPar_int_in          : buffer std_logic;
           
           DPar_Error_Wr_ck1    : OUT std_logic;
           DPar_Error_fck       : OUT std_logic;
           DPar_Error_ck1       : OUT std_logic
	   );
end component;


component AccessControl
port (
              Clk_aux           : in Std_Logic;
              Clk_Int           : in Std_Logic;
              Clk2_Int          : in Std_Logic;
              ParityEnable      : in Std_Logic;
              MHold_Out_ck1_N   : in Std_Logic;
              LDSTO_Int_In      : in Std_Logic;
              
              DMAParity_En      : in Std_Logic;
              
              MECRegSel_N       : in Std_Logic;
              BAError           : in Std_Logic;              
              BusTimeOut_En     : in Std_Logic;              
              RdRst             : in Std_Logic;
              BusRdy_In_loc_N   : in Std_Logic;
              Reset_Int_N       : in Std_Logic;
              RD_In             : in Std_Logic;
              RD_Int_In         : in Std_Logic;
              DPar_Int_In       : in Std_Logic;
              PROM8_In_N        : in Std_Logic;
              WSConfigReg_N     : in Std_Logic;
              Reset_Out_N       : in Std_Logic;
              Error_detect_FSM  : in Std_Logic;
              CError_fck        : in Std_Logic;
              NCError_fck       : in Std_Logic;
              DPar_Error_fck    : in Std_Logic;
              DPar_Error_Wr_ck1 : in Std_Logic;
              
              BusRequest_Int    : in Std_Logic;
              DMA_Mode          : in Std_Logic;
              DMAAS_In          : in Std_Logic;
              A_In              : in Std_Logic_Vector(31 downto 0);
              D_Int_In          : in Std_Logic_Vector(31 downto 0);
              Size_In           : in Std_Logic_Vector(1 downto 0);
              Size_Int_In       : in Std_Logic_Vector(1 downto 0);
              BusErr_In_N       : in Std_Logic;
              MExc_Int          : in Std_Logic;
              MExcRequest       : in Std_Logic;
              Write_Inhibit     : in Std_Logic;
              inull_In          : in Std_Logic;  
              ExtHold_In_N      : in Std_Logic;
              ExtCCV_In         : in Std_Logic;
              BHold_Out_N       : in Std_Logic;
              CPUHalt_N         : in Std_Logic;

              StrobeGate        : Out Std_Logic;
              SFSRTmp_UD        : Out Std_Logic;
              DPar_Error_Rst    : out Std_Logic;
              ALE_Out_N         : out Std_Logic;
              MemBEn_Out_N      : out Std_Logic;
              RAMBEn_Out_N      : out Std_Logic;
              ROMBEn_Out_N      : out Std_Logic;
              DDir_Out          : out Std_Logic;
              MemWr_Out_N       : out Std_Logic;
              CBWr_Out_N        : out Std_Logic;
              OE_Out_N          : out Std_Logic;
              MHold_Out_N_WOEE  : out Std_Logic;
              MDS_Out_N         : out Std_Logic;
              IOWr_Out_N        : out Std_Logic;
              IOBEn_Out_N       : out Std_Logic;
              DOE_Out_N         : out Std_Logic;
              AOE_Out_N         : out Std_Logic;
              COE_Out_N         : out Std_Logic;
              ParErrWSReg       : out Std_Logic;
              Wr_Int_N          : buffer Std_Logic;
              DBEn              : out Std_Logic;
              DParIOEn          : out Std_Logic;
              CBEn              : out Std_Logic;
              MemData_Valid     : out Std_Logic;
              MemData_Valid_Ld  : out Std_Logic;
              Null_Int_rck      : Out Std_Logic;
              DMAInPrgs         : Out Std_Logic;
              NewCycle          : out Std_Logic;
              DRdy_Out_N        : out Std_Logic;
              ByteSel           : out Std_Logic_Vector(3 downto 0);
              IUDataLE          : out Std_Logic;
              MemDataLE         : out Std_Logic;
              ForceNonMemDataLE : Out Std_Logic;
              BA_Out               : out Std_Logic_Vector(1 downto 0);
              StoreByte_Reg_Read_N : out Std_Logic;
              CorrectedData_Reg_N  : out Std_Logic;
              ROM8Reg_N            : out Std_Logic;
              ROM8Reg_LE           : out Std_Logic;
              SysBus_Error         : out Std_Logic;
              BusTimeOut     : buffer Std_Logic;
              Mexc_Out_N     : out Std_Logic;
              
              A_to_AD        : out Std_Logic_Vector(31 downto 0);
              A_to_ML        : out Std_Logic_Vector(31 downto 0);
                            
              DoNotRdUART    : out Std_Logic;
                                          
              LastCycle      : out Std_Logic
              );
end component;


component Mem_io_config_E
port ( Mem_cfg_reg          : buffer Std_Logic_Vector(31 downto 0);
       IO_cfg_reg           : buffer Std_Logic_Vector(31 downto 0);
       D_MemIOConfig_out    : out Std_Logic_Vector(31 downto 0);
       DPar_MemIOConfig_out : out Std_Logic;
       ParErrMemIOConfig    : out Std_Logic;
       D_Int_In             : in Std_Logic_Vector(31 downto 0);
       MemConfigReg_N       : in Std_Logic;
       IOConfigReg_N        : in Std_Logic;
       Wr_Int_N             : in Std_Logic;
       DPar_Int_In          : in Std_Logic;
       CLK_Int              : in Std_Logic;
       Reset_Int_N          : in Std_Logic;
       Load_FFAR_N          : in Std_Logic;
       FFAReg_N             : in Std_Logic;
       PROM8_In_N           : in Std_Logic;
       A_Int_In             : in Std_Logic_Vector(31 downto 0)
     );
end component;


    signal Clk_Int_Adv  : Std_Logic;
    signal Clk2_Int_Adv : Std_Logic;
    
    signal MHold_Out_ck1_N_int: Std_Logic;
    signal DPar_Error_Wr_ck1  : Std_Logic;
    
    signal RdRst             : Std_Logic;
    signal DPar_Int_In_F     : Std_Logic;
    signal IUDataLatch       : Std_Logic_Vector(31 downto 0);
    signal Mem_cfg_reg       : Std_Logic_Vector(31 downto 0);
    signal IO_cfg_reg        : Std_Logic_Vector(31 downto 0);
    signal Size_Int_In       : Std_Logic_Vector(1 downto 0);
    signal Size_Int_CS       : Std_Logic_Vector(1 downto 0);
    signal ASI_Int_In        : Std_Logic_Vector(3 downto 0);
    signal ASI_Int_CS        : Std_Logic_Vector(3 downto 0);
    signal Wrt_Int_CS        : Std_Logic;
    signal WSConfigReg_N     : Std_Logic;
    signal Register_Area_Out : Std_Logic;
    signal Mem_Area_Int      : Std_Logic;
    signal PROM_Area_Out     : Std_Logic;
    signal Exch_Area_Int     : Std_Logic;
    signal IO_Area_Int       : Std_Logic;
    signal IO_Select_Int     : Std_Logic_Vector(1 downto 0);
    signal ParErrWSReg       : Std_Logic;
    signal MemA_Int          : Std_Logic_Vector(2 downto 0);
    signal PageA_Int         : Std_Logic_Vector(2 downto 0);
    signal MemAccess_Select  : Std_Logic_Vector(2 downto 0);
    signal Prom_Ext_Area_sel : Std_Logic;
    signal Ram_Ext_Area_sel  : Std_Logic;
    signal IO_Ext_Area_sel   : Std_Logic;
    signal Ext_Area_Sel      : Std_Logic;
    signal MemData_Valid_Int : Std_Logic;
    signal ByteSel           : Std_Logic_Vector(3 downto 0);
    signal Rom8Reg_LE        : Std_Logic;
    signal BusRequest_Int    : Std_Logic;           
    signal LOCK_Int_In       : Std_Logic;
    signal LDSTO_Int_In      : Std_Logic;
    signal IUDataLE          : Std_Logic;
    signal WRT_Int_In        : Std_Logic;
    signal MexcRequest       : Std_Logic;
    signal NewCycle_Int      : Std_Logic;
    signal FFAReg_N_Int      : Std_Logic;
    
    signal TestControl_Reg_N_Int    : Std_Logic; 
    signal MemAccessReg_N_Int       : Std_Logic;
    signal StoreByte_Reg_Read_N_Int : Std_Logic;
    signal CorrectedData_Reg_N_Int  : Std_Logic;
    signal ROM8Reg_N_Int            : Std_Logic;
    signal DMA_Mode_Int             : Std_Logic;
    signal CError_ck1_Int           : Std_Logic;
    signal CError_fck               : Std_Logic;
    signal NCError_fck              : Std_Logic;
    signal Null_Internal            : Std_Logic;
    signal MHOLD_Out_Int_N          : Std_Logic;
    signal MEXC_Out_Int_N           : Std_Logic;
    signal ParityEnable_Int         : Std_Logic;
    signal EDACEnable_Int           : Std_Logic;
    signal Rd_Internal              : Std_Logic;
    signal CPUHalt_Internal         : Std_Logic;
    signal BHOLD_Out_Int_N          : Std_Logic;
    signal LastCycle                : Std_Logic;
    signal DPar_Error_Rst           : std_logic;
    signal DPar_Error_Rst_Int       : std_logic;
    
    signal MemDataLE          : std_logic;
    signal MemData_Valid_Ld   : std_logic;
    signal DMAInPrgs_Int      : std_logic;
    signal MHold_Out_N_WOEE   : std_logic;
    signal ForceNonMemDataLE  : std_logic;
    signal NCError_F_Int      : std_logic;
    signal CError_F           : std_logic;
    signal Error_detect_FSM   : std_logic;
    signal DPar_Error_fck     : std_logic;
    signal BAError            : std_logic;
    signal EdacRegSel         : std_logic;
    signal StrobeGate         : std_logic;
    signal MECRegSel_N_Int    : std_logic;
    
    signal A_Int_In_loc       : std_logic_Vector(31 downto 0);
    
    signal A_to_ML            : std_logic_Vector(31 downto 0);
    signal A_to_AD            : std_logic_Vector(31 downto 0);
    
    signal CorrectDataLatch_loc  : std_logic_Vector(31 downto 0);
   
    
begin

  
  DMAInPrgs <= DMAInPrgs_Int;
  
  MECRegSel_N <= MECRegSel_N_Int;
  
  A_Int_In <= A_Int_In_loc;

  DPar_Error_Rst <= DPar_Error_Rst_Int;
  NewCycle <= NewCycle_Int;
  FFAReg_N <= FFAReg_N_Int;
  TestControl_Reg_N <= TestControl_Reg_N_Int;
  MemAccessReg_N <= MemAccessReg_N_Int;
  StoreByte_Reg_Read_N <= StoreByte_Reg_Read_N_Int;
  CorrectedData_Reg_N <= CorrectedData_Reg_N_Int;
  ROM8Reg_N <= ROM8Reg_N_Int;
  CError_ck1  <= CError_ck1_Int;    
  Instr_Fetch <= not(ASI_Int_In(1));
  Null_Int_rck <= Null_Internal;
  MEXC_Out_N <= MEXC_Out_Int_N;
  Rd_Int_In <= Rd_Internal;
  CPUHalt_Out_N <= CPUHalt_Internal;
  EDACEnable <= EDACEnable_Int;
  ParityEnable <= ParityEnable_Int;
  BHold_Out_N <= BHold_Out_Int_N; 
  MECRegister_Access <= Register_Area_Out;
  CorrectDataLatch <= CorrectDataLatch_loc;
  MHold_Out_ck1_N <= MHold_Out_ck1_N_int;
  MemData_Valid <= MemData_Valid_Int;
  
  
AD : AddressDecoder
port map  ( 
           Reset_Out_N => Reset_Out_N,
           Clk_Int => Clk_Int,
           Clk2_Int => Clk2_Int,
           CPUHalt_Out_N => CPUHalt_Internal,
           NewCycle => NewCycle_Int,
           RdRst => RdRst,
           StrobeGate => StrobeGate,
           
           Size_in => Size_in,
	         A_In => A_to_AD(31 downto 2),
	         ASI_Int_In => ASI_Int_In,
	         ASI_In => ASI_In,--2301
	         WRT_In => WRT_In,--2301
	         WRT_Int_In => WRT_Int_In,
           DMAAS_In => DMAAS_In,
           DMA_Mode => DMA_Mode_Int,
           Prom8_In_N => Prom8_In_N,
           RomWrt_In_N => RomWrt_In_N,
  	       Mem_Cfg_reg => Mem_Cfg_reg,
  	       IO_Cfg_reg => IO_Cfg_reg,
  	       
           AccessProtEn   => AccessProtEn,
           Reset_Int_N => Reset_Int_N,
           D_Int_In => D_Int_In,
           DPar_Int_In => DPar_Int_In,
           Wr_Int_N => Wr_Int_N,
           
           MemAccess_Violation_Out => MemAccess_Violation_Out,
           D_MemAcc => D_MemAcc,
           DPar_MemAcc => DPar_MemAcc,
           ParErr_MemAcc => ParErr_MemAcc,
          	       
	         MemCs_Out_N => MemCs_Out_N,
	         ExMCS_Out_N => ExMCS_Out_N,
	         IOSel_Out_N => IOSel_Out_N,
	         RomCs_Out_N => RomCs_Out_N,
	         Illegal_Address_Out => Illegal_Address_Out,
	         AccessType => AccessType,
	         Register_Area_Out => Register_Area_Out,
	         ParityEnable => ParityEnable_Int,
	         EDACEnable => EDACEnable_Int,
	         MECControlReg_N => MECControlReg_N,
	         SWRReg_N => SWRReg_N,
	         PWRDReg_N => PWRDReg_N,
	         MemConfigReg_N => MemConfigReg_N,
	         IOConfigReg_N => IOConfigReg_N,
	         WSConfigReg_N => WSConfigReg_N,
	         MemAccessReg_N => MemAccessReg_N_Int,
	         IntShapeReg_N => IntShapeReg_N,
	         IntPendReg_N => IntPendReg_N,
	         IntMaskReg_N => IntMaskReg_N,
	         IntClearReg_N => IntClearReg_N,
	         IntForceReg_N => IntForceReg_N,
	         WDProgramReg_N => WDProgramReg_N,
	         WDTDSetReg_N => WDTDSetReg_N,
           RTC_CountReg_N => RTC_CountReg_N,
           RTC_ScalerReg_N => RTC_ScalerReg_N,
           GPT_A_CountReg_N => GPT_A_CountReg_N,
           GPT_A_ScalerReg_N => GPT_A_ScalerReg_N,
           TimerControlReg_N => TimerControlReg_N,
	         SysFSReg_N => SysFSReg_N,
	         FFAReg_N => FFAReg_N_Int,
	         ErrResStatReg_N => ErrResStatReg_N,
           TestControl_Reg_N => TestControl_Reg_N_Int,
           
	         GUARTAReg_N => GUARTAReg_N,
	         GUARTBReg_N => GUARTBReg_N,
	         GUARTStatusReg_N => GUARTStatusReg_N,
	         
	         UARTAReg_N => UARTAReg_N,
	         UARTBReg_N => UARTBReg_N,
	         UARTStatusReg_N => UARTStatusReg_N,
	         
	         MECRegSel_N => MECRegSel_N_Int,
	         
	         Unimpl_Address_Out => Unimpl_Address_Out);

MLAT : Mec_latch_E
port map  ( 
           A_int_in => A_Int_In_loc,
           ASI_int_in => ASI_int_in,
           Size_Int_In => Size_Int_In,
                      
           APar_Error => APar_Error,
           CtlPar_Error => CtlPar_Error,
           
           RD_In => RD_In,
           Rd_Int_In => RD_Internal,
           WRT_In => WRT_In,
           WRT_Int_In => WRT_Int_In,
           DXFER_In => DXFER_In,
           LOCK_In => LOCK_In,
           LOCK_Int_In => LOCK_Int_In,
           LDSTO_Int_In => LDSTO_Int_In,
           A_in => A_to_ML,
           APar_in => APar_in,
           ASI_in => ASI_in,
           Size_in => Size_in,
           LDSTO_In => LDSTO_In,
           IMPar_In => IMPar_In,
           WE_In_N => WE_In_N,
           ASPar_in => ASPar_in,
           NewCycle => NewCycle_Int,
           NOPar_In_N => NOPar_In_N,
           DMA_Mode => DMA_Mode_Int,
           DMAInPrgs => DMAInPrgs_Int,
           DMAParity_En => DMAParity_En,
           Reset_Int_N => Reset_Int_N,
           Reset_Out_N => Reset_Out_N,
           Clk_Int => Clk_Int,
           DMAAS_In => DMAAS_In
           );

BA : BusArbiter
port map  (
          Clk_Int => Clk_Int,
          Reset_Int_N => Reset_Int_N,
          LOCK_Int_In => LOCK_Int_In,
          PowerDown => PowerDown,
          DMAREQ_In_N => DMAREQ_In_N,
          DMAEnable_Int => DMAEnable_Int,
          DMATimeOut_En => DMATimeOut_En,
          ErrorHalt_N => ErrorHalt_N,
          SysHalt_In_N => SysHalt_In_N,
          NewCycle => NewCycle_Int,
          RdRst => RdRst,
          LastCycle => LastCycle,
          BAError => BAError,
          BusRequest_Int => BusRequest_Int,
          BHold_Out_N => BHold_Out_Int_N,
          DMAGNT_Out_N => DMAGNT_Out_N,
          DMA_Mode => DMA_Mode_Int,
          MEXCRequest => MEXCRequest,
          DMATimeOut => DMATimeOut,
          CPUHalt_Out_N => CPUHalt_Internal,
          MECHalt_Int_N => MECHalt_Int_N
         );


EDAC_1 : EDAC
port map  (
           Clk_Int     => Clk_Int,
           Clk_Int_Adv => Clk_Int,
           
           Clk2_Int_Adv => Clk2_Int,
           Clk2_Int     => Clk2_Int,
           	         
	         ExtHold_In_N => ExtHold_In_N,
	         ExtCCV_In => ExtCCV_In,
           BHold_Out_N => BHold_Out_Int_N,
           
	         Register_Area_Out => Register_Area_Out,
	         CorrectedData_Reg_N => CorrectedData_Reg_N_Int,
           DPar_Int_Out => DPar_Int_Out,
           
           NOPar_In_N => NOPar_In_N,
           ParityEnable => ParityEnable_Int,
                      
           NewCycle => NewCycle_Int,
           DMA_Mode => DMA_Mode_Int,
           DMAParity_En => DMAParity_En,
           
           MHold_Out_N_WOEE => MHold_Out_N_WOEE,
           DParIO_In => DParIO_In,
           Rd_Int_In => RD_Internal,
           MemData_Valid => MemData_Valid_Int,
           MemData_Valid_Ld => MemData_Valid_Ld,
           Null_Int_rck => Null_Internal,
           DMAInPrgs => DMAInPrgs_Int,
           EDACEnable => EDACEnable_Int,
           
           IUDataLE => IUDataLE,
           MemDataLE => MemDataLE,
           ForceNonMemDataLE => ForceNonMemDataLE,
           D_In => D_In,
           CB_In => CB_In(6 downto 0),
           Reset_Int_N => Reset_Int_N,
           Reset_Out_N => Reset_Out_N,
           TestControl_Reg_N => TestControl_Reg_N_Int,
           Wr_Int_N => Wr_Int_N,
                      
           StoreByte_Reg_Read_N => StoreByte_Reg_Read_N_Int,
           ByteSel => ByteSel,
           Rom8Reg_N => Rom8Reg_N_Int,
           Rom8Reg_LE => Rom8Reg_LE,
           DPar_Error_Rst => DPar_Error_Rst_Int,
           
           D_HIGHLD => D_Int_In,
           CorrectDataLatch => CorrectDataLatch_loc,
                      
           EdacRegSel => EdacRegSel,
           IntrTest_En => IntrTest_En,
           SWHalt_En => SWHalt_En,
           D_EDAC => D_EDAC,
           DParErr_EDAC => DParErr_EDAC,
           CB_Out => CB_Out,
           
           CError_ck1 => CError_ck1_Int,
           NCError_ck1 => NCError_ck1,
           Error_detect_FSM => Error_detect_FSM,
           CError_fck => CError_fck,
           NCError_fck => NCError_fck,
           
           MHold_Out_N => MHold_Out_N,
           MHold_Out_ck1_N => MHold_Out_ck1_N_int,
           
           DParIO_Out => DParIO_Out,
           
           DPar_int_in => DPar_int_in,
           
           DPar_Error_Wr_ck1 => DPar_Error_Wr_ck1,
           DPar_Error_fck => DPar_Error_fck,
           DPar_Error_ck1 => DPar_Error_ck1
         );

AC : AccessControl
port map  (
              Clk_aux => Clk_Int,
              Clk_Int => Clk_Int,
              Clk2_Int => Clk2_Int,
              ParityEnable => ParityEnable_Int,
              MHold_Out_ck1_N => MHold_Out_ck1_N_int,
              LDSTO_Int_In => LDSTO_Int_In,
              
              DMAParity_En => DMAParity_En,
              
              MECRegSel_N => MECRegSel_N_Int,
              BAError => BAError,
              BusTimeOut_En => BusTimeOut_En,           
              RdRst => RdRst,
              BusRdy_In_loc_N => BusRdy_In_N,
              Reset_Int_N => Reset_Int_N,
              RD_In => RD_In,
              RD_Int_In => RD_Internal,
              DPar_Int_In => DPar_Int_In,
              PROM8_In_N => PROM8_In_N,
              WSConfigReg_N => WSConfigReg_N,
              Reset_Out_N => Reset_Out_N,
              
              Error_detect_FSM => Error_detect_FSM,
              CError_fck => CError_fck,
              NCError_fck => NCError_fck,
              
              DPar_Error_fck => DPar_Error_fck,
              DPar_Error_Wr_ck1 => DPar_Error_Wr_ck1,
              
              StrobeGate => StrobeGate,
              BusRequest_Int => BusRequest_Int,
              DMA_Mode => DMA_Mode_Int,
              DMAAS_In => DMAAS_In,
              A_In => A_In(31 downto 0),
              D_Int_In => D_Int_in,
              Size_In => Size_In,
              Size_Int_In => Size_Int_In,
              BusErr_In_N => BusErr_In_N,
              MExc_Int => MExc_Int,
              MExcRequest => MExcRequest,
              Write_Inhibit => Write_Inhibit,
              inull_In => inull_In,
              ExtHold_In_N => ExtHold_In_N,
              ExtCCV_In => ExtCCV_In,
              DPar_Error_Rst => DPar_Error_Rst_Int,
              ALE_Out_N => ALE_Out_N,
              MemBEn_Out_N => MemBEn_Out_N,
              RAMBEn_Out_N => RAMBEn_Out_N,
              ROMBEn_Out_N => ROMBEn_Out_N,
              DDir_Out => DDir_Out,
              MemWr_Out_N => MemWr_Out_N,
              CBWr_Out_N => CBWr_Out_N,
              OE_Out_N => OE_Out_N,
              MHold_Out_N_WOEE => MHold_Out_N_WOEE,
              MDS_Out_N => MDS_Out_N,
              IOWr_Out_N => IOWr_Out_N,
              IOBEn_Out_N => IOBEn_Out_N,
              DOE_Out_N => DOE_Out_N,
              AOE_Out_N => AOE_Out_N,
              COE_Out_N => COE_Out_N,
              ParErrWSReg => ParErrWSReg,
              Wr_Int_N => Wr_Int_N,
              DBEn => DBEn,
              DParIOEn => DParIOEn,
              CBEn => CBEn,
              MemData_Valid => MemData_Valid_Int,
              MemData_Valid_Ld => MemData_Valid_Ld,
              Null_Int_rck => Null_Internal,
              DMAInPrgs => DMAInPrgs_Int,
              NewCycle => NewCycle_Int,
              DRdy_Out_N => DRdy_Out_N,
              ByteSel => ByteSel,
              IUDataLE => IUDataLE,
              MemDataLE => MemDataLE,
              ForceNonMemDataLE => ForceNonMemDataLE,
              BA_Out => BA_Out,
              StoreByte_Reg_Read_N => StoreByte_Reg_Read_N_Int,
              CorrectedData_Reg_N => CorrectedData_Reg_N_Int,
              ROM8Reg_N => ROM8Reg_N_Int,
              ROM8Reg_LE => ROM8Reg_LE,
              SysBus_Error => SysBus_Error,
              BusTimeOut => BusTimeOut,
              Mexc_Out_N => MEXC_Out_Int_N,
              BHold_Out_N => BHold_Out_Int_N,
              CPUHalt_N => CPUHalt_Internal,
              SFSRTmp_UD => SFSRTmp_UD,
              A_to_AD => A_to_AD,
              A_to_ML => A_to_ML,
              DoNotRdUART => DoNotRdUART,
              LastCycle => LastCycle              
            );




MIOP : Mem_io_config_E
port map  ( 
           Mem_cfg_reg => Mem_cfg_reg,
           IO_cfg_reg => IO_cfg_reg,
           D_MemIoConfig_out => D_MemIoConfig_out,
           DPar_MemIOConfig_out => DPar_MemIOConfig_out,
           ParErrMemIOConfig => ParErrMemIOConfig,
           D_Int_In => D_Int_In,
           MemConfigReg_N => MemConfigReg_N,
           IOConfigReg_N => IOConfigReg_N,
           Wr_Int_N => Wr_Int_N,
           DPar_Int_In => DPar_Int_In,
           CLK_Int => CLK_Int,
           Reset_Int_N => Reset_Int_N,
           Load_FFAR_N => Load_FFAR_N,
           FFAReg_N => FFAReg_N_Int,
           PROM8_In_N => PROM8_In_N,
           A_Int_In => A_Int_In_loc
         );

end mini_spec;
---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      Register Read Data Multiplexer
-- File name:                  \mec\source\datamux.vhd
-- VHDL unit:                  DataMux
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library IEEE;
use IEEE.Std_Logic_1164.all;

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

entity DataMux is
    port (
           Clk_Int            : in std_logic;
           Reset_Out_N        : in std_logic;
           
           MECRegSel_N        : in std_logic;
           -- From module uarts
           UARTAReg_N         : in std_logic;
           UARTBReg_N         : in std_logic;
           UARTStatusReg_N    : in std_logic;
        
           D_UARTA_Out        : in Std_Logic_Vector(31 downto 0);
           DPar_UARTA_Out     : in Std_Logic;
           D_UARTB_Out        : in Std_Logic_Vector(31 downto 0);
           DPar_UARTB_Out     : in Std_Logic;
           D_UARTS_Out        : in Std_Logic_Vector(31 downto 0);
           DPar_UARTS_Out     : in Std_Logic;
           
           DPar_Wdog_Out      : in Std_Logic;
           D_Wdog_Out         : in Std_Logic_Vector(31 downto 0);
           WDProgramReg_N     : in Std_Logic;

           -- From module timers
           GPT_A_ScalerReg_N  : in std_logic;
           GPT_A_CountReg_N   : in std_logic;
           RTC_ScalerReg_N    : in std_logic;
           RTC_CountReg_N     : in std_logic;
           D_Tim_Out          : in Std_Logic_Vector(31 downto 0);
           DPar_Tim_Out       : in std_logic;
           ParityError_Tim    : in std_logic;

           -- From module mecctrl
           MECControlReg_N    : in std_logic;
           D_CtlSup_Out       : in Std_Logic_Vector(31 downto 0);
           DPar_CtlSup_Out    : in std_logic;
           ParErrCtlSup       : in std_logic;

           -- From module interr
           ErrResStatReg_N    : in std_logic;
           SysFSReg_N         : in std_logic;
           IntShapeReg_N      : in std_logic;
           IntPendReg_N       : in std_logic; 
           IntMaskReg_N       : in std_logic;
           IntForceReg_N      : in std_logic;
           D_IntErr_Out       : in std_logic_Vector(31 downto 0);
           DPar_IntErr_out    : in std_logic;
           ParityErrorIntErr  : in std_logic;

           -- From module sysbif
           FFAReg_N             : in std_logic;
           TestControl_Reg_N    : in std_logic;
           MemConfigReg_N       : in std_logic;
           IOConfigReg_N        : in std_logic;           
           MemAccessReg_N       : in std_logic;
           CorrectedData_Reg_N  : in std_logic;
           StoreByte_Reg_Read_N : in std_logic;
           Rom8Reg_N            : in std_logic;
           
           CorrectDataLatch     : in Std_Logic_Vector(31 downto 0);
           
           D_EDAC               : in std_logic_vector(31 downto 0);
           DParErr_EDAC         : in std_logic;
           
           D_MemIoConfig_out    : in std_logic_vector(31 downto 0);
           DPar_MemIOConfig_out : in std_logic;
           ParErrMemIOConfig    : in std_logic;
           
           D_MemAcc             : in std_logic_vector(31 downto 0);
           DPar_MemAcc          : in std_logic;
           ParErr_MemAcc        : in std_logic;

           Unimpl_Address_Out   : in std_logic;
           
           CPED                 : out std_logic;
           
           D_Out                : out std_logic_Vector(31 downto 0);
           DPar_Int_Out         : out std_logic
         );

end DataMux;

----------------------------------------------------------------------------
architecture Mini_Spec of DataMux is

   function mux2 ( Sel : std_logic;
                   In1 : std_logic;
                   In2 : std_logic ) return std_logic is
   begin
     if    Sel = '0' then
        return In1;
     else 
        return In2;
     end if;
   end;
   
      
  signal TimerReg           : std_logic;
  signal InterrReg          : std_logic;
  signal CPED_loc           : std_logic;
  
  signal D_Foo1_Out          : std_logic_vector(31 downto 0);
  signal DPar_Foo1_Out       : std_logic;
  signal Foo1Reg             : std_logic;
  
  signal D_Foo2_Out          : std_logic_vector(31 downto 0);
  signal DPar_Foo2_Out       : std_logic;
  signal Foo2Reg             : std_logic;
  
  signal EdacReg             : std_logic;
  signal MIOCfgReg           : std_logic;

begin


    FooMux1:process(WDProgramReg_N  , D_Wdog_Out   , DPar_Wdog_Out,
                    UARTAReg_N      , D_UARTA_Out  , DPar_UARTA_Out,
                    UARTBReg_N      , D_UARTB_Out  , DPar_UARTB_Out,
                    UARTStatusReg_N , D_UARTS_Out  , DPar_UARTS_Out)
    begin
    
    		for i in 0 to 10 loop
    		  D_Foo1_Out(i)   <= (D_Wdog_Out(i)     and not WDProgramReg_N) or
    		                     (D_UARTA_Out(i)    and not UARTAReg_N) or
    		                     (D_UARTB_Out(i)    and not UARTBReg_N) or
    		                     (D_UARTS_Out(i)    and not UARTStatusReg_N);
    		end loop;
    		
    		for i in 11 to 23 loop
    		  D_Foo1_Out(i)   <= (D_Wdog_Out(i)     and not WDProgramReg_N) or
    		                     (D_UARTS_Out(i)    and not UARTStatusReg_N);
    		end loop;
    		
    		for i in 24 to 31 loop
    		  D_Foo1_Out(i)   <= '0';
    		end loop;
    		
        DPar_Foo1_Out     <= (DPar_Wdog_Out     and not WDProgramReg_N) or
    	                       (DPar_UARTA_Out    and not UARTAReg_N) or
    	                       (DPar_UARTB_Out    and not UARTBReg_N) or
    	                       (DPar_UARTS_Out    and not UARTStatusReg_N);
    		                   
    end process;

    FooMux2:process(TimerReg        , D_Tim_Out         , DPar_Tim_Out,
                    MIOCfgReg       , D_MemIoConfig_out , DPar_MemIOConfig_out,
                    MemAccessReg_N  , D_MemAcc          , DPar_MemAcc,
                    MECControlReg_N , D_CtlSup_Out      , DPar_CtlSup_Out)
    begin
    
    		for i in 0 to 31 loop
    		  D_Foo2_Out(i)   <= (D_Tim_Out(i)         and TimerReg) or 
    		                     (D_MemIoConfig_out(i) and MIOCfgReg) or
    		                     (D_MemAcc(i)          and not MemAccessReg_N) or
    		                     (D_CtlSup_Out(i)      and not MECControlReg_N);
   		end loop;
    		
        DPar_Foo2_Out     <= (DPar_Tim_Out         and TimerReg) or 
    	                       (DPar_MemIOConfig_out and MIOCfgReg) or
    	                       (DPar_MemAcc          and not MemAccessReg_N) or
    	                       (DPar_CtlSup_Out      and not MECControlReg_N);
    		                   
    end process;

    Reg_p: process(UARTAReg_N, UARTBReg_N, UARTStatusReg_N,
                   GPT_A_CountReg_N, RTC_CountReg_N, WDProgramReg_N,
                   GPT_A_ScalerReg_N, RTC_ScalerReg_N,
                   ErrResStatReg_N, SysFSReg_N, IntShapeReg_N,
                   IntPendReg_N, IntMaskReg_N, IntForceReg_N,
                   FFAReg_N, TestControl_Reg_N,
                   MemConfigReg_N, IOConfigReg_N,
                   MemAccessReg_N, Rom8Reg_N, MECControlReg_N,
                   CorrectedData_Reg_N, StoreByte_Reg_Read_N)
                         
    begin
    
      Foo1Reg    <= not(UARTAReg_N and 
                        UARTBReg_N and UARTStatusReg_N and WDProgramReg_N);
        
      Foo2Reg    <= not(GPT_A_CountReg_N and RTC_CountReg_N and
                        GPT_A_ScalerReg_N and RTC_ScalerReg_N and
                        MECControlReg_N and FFAReg_N and MemConfigReg_N and 
                        IOConfigReg_N and MemAccessReg_N );
                     
      TimerReg   <= not(GPT_A_CountReg_N and RTC_CountReg_N and
                        GPT_A_ScalerReg_N and RTC_ScalerReg_N);
                     
      MIOCfgReg  <= not(FFAReg_N and MemConfigReg_N and IOConfigReg_N);
                     
      InterrReg  <= not(ErrResStatReg_N and SysFSReg_N and 
                        IntShapeReg_N and IntPendReg_N and IntMaskReg_N and IntForceReg_N);
                     
      EdacReg    <= not(TestControl_Reg_N and 
                        Rom8Reg_N and StoreByte_Reg_Read_N);
                     
    end process;
    
    



    MainMux1: process(Foo2Reg,   D_Foo2_Out,
                      Foo1Reg,   D_Foo1_Out,
                      InterrReg, D_IntErr_Out,
                      EdacReg,   D_EDAC,
                      CorrectedData_Reg_N, CorrectDataLatch,
                      Unimpl_Address_Out)
    begin
      if CorrectedData_Reg_N = '0' then 
    	  	 D_Out         <= CorrectDataLatch(31 downto 0);
         
      elsif Unimpl_Address_Out = '1' then 
    		   D_Out         <= "00000000000000000000000000000000";
         
      elsif EdacReg = '1' then 
    		   D_Out         <= D_EDAC;
         
      else
      
    	    for i in 0 to 15 loop
             D_Out(i) <= (D_Foo2_Out(i)   and Foo2Reg) or 
    		                 (D_Foo1_Out(i)   and Foo1Reg) or 
    		                 (D_IntErr_Out(i) and InterrReg);
    	    end loop;
    		                   
    	    for i in 16 to 31 loop
             D_Out(i) <= (D_Foo2_Out(i)   and Foo2Reg) or 
    		                 (D_Foo1_Out(i)   and Foo1Reg);
    	    end loop;
    		
    		               
      end if;
    
    end process;
        
    MainMux2: process(Foo2Reg,   DPar_Foo2_Out,
                      Foo1Reg,   DPar_Foo1_Out,
                      InterrReg, DPar_IntErr_OUT,
                      Unimpl_Address_Out)
    begin
                   		
      DPar_Int_Out <= Unimpl_Address_Out or
                      (DPar_Foo2_Out   and Foo2Reg) or 
    	                (DPar_Foo1_Out   and Foo1Reg) or 
    	                (DPar_IntErr_out and InterrReg);
    		               
    end process;

        
 
    CPED_Or: process(ParityError_Tim, ParErrCtlSup, ParErr_MemAcc,
                     ParityErrorIntErr, DParErr_EDAC, ParErrMemIOConfig)
    begin
      CPED_loc  <= (ParityError_Tim or 
                    ParErrCtlSup    or ParityErrorIntErr or
                    DParErr_EDAC    or ParErrMemIOConfig or
                    ParErr_MemAcc);
    end process;

  
        
    fsync: process(Reset_Out_N, Clk_Int)
    begin
    if Reset_Out_N = '0' then
        CPED <= '0';
    elsif Clk_Int'event and Clk_Int = '0' then
        CPED <= CPED_loc;
    end if;
    end process; 

 
    
end Mini_Spec;

---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      MEC Generic component
-- File name:                  \mec\source\mecgen.vhd
-- VHDL unit:                  MEmoryController
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing generics.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE; 
use IEEE.std_logic_1164.all; 

ENTITY MECfunc IS
  PORT(
    Prom8_In_N         : in     Std_Logic ;
    BusRdy_In_N        : in     Std_Logic ;
    BusErr_In_N        : in     Std_Logic ;
    ASPar_In           : in     Std_Logic ;
    Size_In            : in     std_Logic_Vector(1 downto 0) ;
    ASI_In             : in     std_Logic_Vector(3 downto 0) ;
    APar_In            : in     Std_Logic ;
    A_In               : in     std_Logic_Vector(31 downto 0) ;
    TDI_In             : in     Std_Logic ;
    TCk_In             : in     Std_Logic ;
    TRst_In_N          : in     Std_Logic ;
    TMS_In             : in     Std_Logic ;
    IMPar_In           : in     Std_Logic ;
    Wrt_In             : in     Std_Logic ;
    WE_In_N            : in     Std_Logic ;
    Rd_In              : in     Std_Logic ;
    Lock_In            : in     Std_Logic ;
    LdSto_In           : in     Std_Logic ;
    DXfer_In           : in     Std_Logic ;
    INull_In           : in     Std_Logic ;
    DMAAS_In           : in     Std_Logic ;
    NoPar_In_N         : in     Std_Logic ;
    RomWrt_In_N        : in     Std_Logic ;
    WDClk_In           : in     Std_Logic ;
    DRdy_Out_N         : out    Std_Logic ;
    TDO_Out            : out    Std_Logic ;
    DMAGnt_Out_N       : out    Std_Logic ;
    Clk2_In            : in     Std_Logic ;
    DMAReq_In_N        : in     Std_Logic ;
    CB_In              : in     std_Logic_Vector(6 downto 0) ;
    CB_Out             : out    std_Logic_Vector(6 downto 0) ;
    CBEn_N             : out    Std_Logic ;
    IUErr_In_N         : in     Std_Logic ;
    IUHWErr_In_N       : in     Std_Logic ;
    FPUHWErr_In_N      : in     Std_Logic ;
    IUCmpErr_In_N      : in     Std_Logic ;
    FPUCmpErr_In_N     : in     Std_Logic ;
    MECHWErr_Out_N     : out    Std_Logic ;
    SysErr_Out_N       : out    Std_Logic ;
    SysAv_Out          : out    Std_Logic ;
    TxB_Out            : out    Std_Logic ;
    TxA_Out            : out    Std_Logic ;
    RxA_In             : in     Std_Logic ;
    RxB_In             : in     Std_Logic ;
    Clk_Out            : buffer Std_Logic ;
    CPUHalt_Out_N      : out    Std_Logic ;
    Reset_Out_N        : out    Std_Logic ;
    SysHalt_In_N       : in     Std_Logic ;
    SysReset_In_N      : in     Std_Logic ;
    IntAck_In          : in     Std_Logic ;
    AOE_Out_N          : out    Std_Logic ;
    COE_Out_N          : out    Std_Logic ;
    DOE_Out_N          : out    Std_Logic ;
    BHold_Out_N        : out    Std_Logic ;
    MDS_Out_N          : out    Std_Logic ;
    MExc_Out_N         : out    Std_Logic ;
    MHold_Out_N        : out    Std_Logic ;
    IRL_Out            : out    std_Logic_Vector(3 downto 0) ;
    ExtInt_In          : in     std_Logic_Vector(4 downto 0) ;
    ExtIntAck_Out      : out    Std_Logic ;
    ExMCS_Out_N        : out    Std_Logic ;
    IOBEn_Out_N        : out    Std_Logic ;
    IOWR_Out_N         : out    Std_Logic ;
    IOSel_Out_N        : out    std_Logic_Vector(3 downto 0) ;
    BA_Out             : out    std_Logic_Vector(1 downto 0) ;
    MemBEn_Out_N       : out    std_logic ;
    RAMBEn_Out_N       : out    std_logic ;
    ROMBEn_Out_N       : out    std_logic ;
    CBWr_Out_N         : out    Std_Logic ;
    MemWr_Out_N        : out    Std_Logic ;
    OE_Out_N           : out    Std_Logic ;
    DDir_Out           : out    Std_Logic ;
    MemCS_Out_N        : out    std_Logic_Vector(9 downto 0) ;
    RomCS_Out_N        : out    Std_Logic ;
    ALE_Out_N          : out    Std_Logic ;
    DParIO_Out         : out    Std_Logic ;
    D_Out              : out    std_Logic_Vector(31 downto 0) ;
    DParIO_In          : in     Std_Logic ;
    D_In               : in     std_Logic_Vector(31 downto 0) ;
    DBEn_N             : out    Std_Logic ;
    DParIOEn_N         : out    Std_Logic ; 
    ExtHold_In_N       : in     Std_Logic ;
    ExtCCV_In          : in     Std_Logic ;
    
    ParityEnable       : out    UX01 ; -- Only used for D setup time checking
    EDACEnable         : out    UX01 ; -- Only used for D setup time checking
    MemData_Valid      : out    UX01 ; -- Only used for D setup time checking
    Wr_Int_N           : buffer UX01 ; -- Only used for D setup time checking
    MECRegister_Access : out    UX01
    );
END MECfunc ;

---------------------------------------------------------------------------
architecture mini_spec of MECFunc is
component UARTs
port (
          Clk_Int         : in Std_Logic;
          UART_Clk_En     : in Std_Logic;
          Reset_Int_N     : in Std_Logic;
          
          DoNotRdUART     : in Std_Logic;
           
          GUARTAReg_N       : in Std_Logic;
          GUARTBReg_N       : in Std_Logic;
          GUARTStatusReg_N : in Std_Logic;
                    
          UARTAReg_N      : in Std_Logic;
          UARTBReg_N      : in Std_Logic;
          UARTStatusReg_N : in Std_Logic;
          MECControlReg_N : in Std_Logic;
          Wr_Int_N        : in Std_Logic;
          Rd_Int_In       : in Std_Logic;
          D_Int_In        : in Std_Logic_Vector(31 downto 0);
          RxA_In          : in Std_Logic;
          RxB_In          : in Std_Logic;
          TxA_Out         : out Std_Logic;
          TxB_Out         : out Std_Logic;
          Intr_UARTA_Data  : out Std_Logic;
          Intr_UARTB_Data  : out Std_Logic;
          UARTs_Quiet     : out Std_Logic;
          Intr_UART_Err   : out Std_Logic;
          D_UARTA_Out      : out Std_Logic_Vector(31 downto 0);
          DPar_UARTA_Out   : out Std_Logic;
          D_UARTB_Out      : out Std_Logic_Vector(31 downto 0);
          DPar_UARTB_Out   : out Std_Logic;
          D_UARTS_Out      : out Std_Logic_Vector(31 downto 0);
          DPar_UARTS_Out   : out Std_Logic);
end component;

component Timers
port (
          Clk_Int           : in Std_Logic;
          Reset_Int_N       : in Std_Logic;
          MECHalt_Int_N     : in Std_Logic;
          Wr_Int_N          : in Std_Logic;
          GPT_A_CountReg_N  : in Std_Logic;
          GPT_A_ScalerReg_N : in Std_Logic;
          RTC_CountReg_N    : in Std_Logic;
          RTC_ScalerReg_N   : in Std_Logic;
          TimerControlReg_N : in Std_Logic;
          D_Int_In          : in Std_Logic_Vector(31 downto 0);
          DPar_Int_In       : in Std_Logic;
          ParityError_Tim   : out Std_Logic;
          D_Tim_Out         : out Std_Logic_Vector(31 downto 0);
          DPar_Tim_Out      : out Std_Logic;
          GPTimTO_A         : out Std_Logic;
          RTCTimTO          : out Std_Logic );
end component;

component TAP 
port(
         TRst_In_N : in Std_Logic;
         TCK_In    : in Std_Logic;
         TDI_In    : in Std_Logic;
         TMS_In    : in Std_Logic;
         TDO_Out   : out Std_Logic
        );
end component;


component SystemBusInterface
port (
          Clk_Int           : in Std_Logic;
          Clk2_Int          : in Std_Logic;
          BusTimeOut_En     : in Std_Logic;
          PowerDown         : in Std_Logic;
          MemData_Valid     : out Std_Logic;
          Lock_In           : in Std_Logic;
          Reset_Int_N       : in Std_Logic;
          APar_In           : in Std_Logic;
          DParIO_in         : in Std_Logic;
          ASPar_In          : in Std_Logic;
          DMAAS_in          : in Std_Logic;
          DRdy_Out_N        : out Std_Logic;
          INull_In          : in Std_Logic;
          Wrt_In            : in Std_Logic;
          D_In              : in Std_Logic_Vector(31 downto 0);
          RomWrt_In_N       : in Std_Logic;
          NoPar_In_N        : in Std_Logic;
          BusErr_In_N       : in Std_Logic;
          DMAReq_In_N       : in Std_Logic;
          ExtHold_In_N      : in Std_Logic;
          ExtCCV_In         : in Std_Logic;
          ASI_In            : in Std_Logic_Vector(3 downto 0);
          LdSto_In          : in Std_Logic;
          CB_In             : in Std_Logic_Vector(6 downto 0);
          PROM8_In_N        : in Std_Logic;
          BusRdy_In_N       : in Std_Logic;
          A_In              : in Std_Logic_Vector(31 downto 0);
          
          A_Int_in          : out Std_Logic_Vector(31 downto 0);
          SFSRTmp_UD        : Out Std_Logic;
          
          WE_In_N           : in Std_Logic;
          Rd_In             : in Std_Logic;
          DXfer_In          : in Std_Logic;
          IMPar_In          : in Std_Logic;
          Size_In           : in Std_Logic_Vector(1 downto 0);
          Reset_Out_N       : in Std_Logic;
          AccessProtEn      : in Std_Logic;
          DMAEnable_Int     : in Std_Logic;
          DMATimeOut_En     : in Std_Logic;
          DMAParity_En      : in Std_Logic;
          DPar_Int_Out      : in Std_Logic;
          MExc_Int          : in Std_Logic;
          ErrorHalt_N       : in Std_Logic;
          SysHalt_In_N      : in Std_Logic;
          Write_Inhibit     : in Std_Logic;
          Load_FFAR_N       : in Std_Logic;
          NewCycle          : out Std_Logic;
          CBWr_Out_N        : out Std_Logic;
          D_Int_In          : buffer Std_Logic_Vector(31 downto 0);
          Rd_Int_In         : out Std_Logic;
          MemBEn_Out_N      : out Std_Logic;
          RAMBEn_Out_N      : out Std_Logic;
          ROMBEn_Out_N      : out Std_Logic;
          COE_Out_N         : out Std_Logic;
          ExMCS_Out_N       : out Std_Logic;
          IOBEn_Out_N       : out Std_Logic;
          OE_Out_N          : out Std_Logic;
          MemWr_Out_N       : out Std_Logic;
          DParIO_Out        : out Std_Logic;
          CB_Out            : out Std_Logic_Vector(6 downto 0);
          DDir_Out          : out Std_Logic;
          IOWr_Out_N        : out Std_Logic;
          CBEn              : out Std_Logic;
          DMAGnt_Out_N      : out Std_Logic;
          BHold_Out_N       : out Std_Logic;
          AOE_Out_N         : out Std_Logic;
          MemCS_Out_N       : out Std_Logic_Vector(9 downto 0);
          Wr_Int_N          : buffer Std_Logic;
          MDS_Out_N         : out Std_Logic;
          MHold_Out_N       : out Std_Logic;
          DPar_Int_In       : buffer Std_Logic;
          MExc_Out_N        : out Std_Logic;
          BA_Out            : out Std_Logic_Vector(1 downto 0);
          MECControlReg_N   : buffer Std_Logic;
          ALE_Out_N         : out Std_Logic;
          RomCS_Out_N       : out Std_Logic;
          IOSel_Out_N       : out Std_Logic_Vector(3 downto 0);
          DOE_Out_N         : out Std_Logic;
          MemConfigReg_N    : buffer Std_Logic;
          IOConfigReg_N     : buffer Std_Logic;
	        IntShapeReg_N       : out Std_Logic;
	        IntPendReg_N        : out Std_Logic;
	        IntMaskReg_N        : out Std_Logic;
	        IntClearReg_N       : out Std_Logic;
	        IntForceReg_N       : out Std_Logic;
	        WDProgramReg_N      : out Std_Logic;
	        WDTDSetReg_N        : out Std_Logic;
          RTC_CountReg_N      : out Std_Logic; 
          RTC_ScalerReg_N     : out Std_Logic; 
          GPT_A_CountReg_N    : out Std_Logic; 
          GPT_A_ScalerReg_N   : out Std_Logic; 
          TimerControlReg_N   : out Std_Logic; 
           
          GUARTAReg_N         : out Std_Logic;
          GUARTBReg_N         : out Std_Logic;
	        GUARTStatusReg_N     : out Std_Logic;
           
          MECRegSel_N         : out Std_Logic;
          
	        UARTAReg_N          : out Std_Logic;
	        UARTBReg_N          : out Std_Logic;
	        UARTStatusReg_N     : out Std_Logic;
	        SysFSReg_N          : out Std_Logic;
	        ErrResStatReg_N     : out Std_Logic;
	        FFAReg_N            : out Std_Logic;
          TestControl_Reg_N   : out Std_Logic;
	        MemAccessReg_N      : out Std_Logic;
          StoreByte_Reg_Read_N : out Std_Logic;
          CorrectedData_Reg_N  : out Std_Logic;
          ROM8Reg_N            : out Std_Logic;
	        SWRReg_N             : out Std_Logic;
	        PWRDReg_N            : out Std_Logic;
          IntrTest_En          : out Std_Logic;
          SWHalt_En            : out Std_Logic;
	        MemAccess_Violation_Out    : out Std_Logic;
	        AccessType           : out Std_Logic_Vector(3 downto 0);
	        Unimpl_Address_Out   : out Std_Logic;
	        Illegal_Address_Out  : out Std_Logic;
          APar_Error           : out Std_Logic;
          DPar_Error_ck1       : out Std_Logic;
          CtlPar_Error         : out Std_Logic;
          NCError_ck1          : out Std_Logic;
          CError_ck1           : out Std_Logic;
          DMATimeOut           : out Std_Logic;
          DBEn                 : out Std_Logic;
          DParIOEn             : out Std_Logic;
          SysBus_Error         : out Std_Logic;
          BusTimeOut           : buffer Std_Logic;
          
          CorrectDataLatch     : out Std_Logic_Vector(31 downto 0);
          
          D_EDAC               : out Std_Logic_Vector(31 downto 0);
          DParErr_EDAC         : out Std_Logic;
          
          D_MemIoConfig_out    : out Std_Logic_Vector(31 downto 0);
          DPar_MemIOConfig_out : out Std_Logic;
          ParErrMemIOConfig    : out Std_Logic;
          
          D_MemAcc             : out Std_Logic_Vector(31 downto 0);
          DPar_MemAcc          : out Std_Logic;
          ParErr_MemAcc        : out Std_Logic;
          
          MECHalt_Int_N        : out Std_Logic;
          Instr_Fetch          : out Std_Logic;
          Null_Int_rck         : out Std_Logic;
          CPUHalt_Out_N        : out Std_Logic;
          ParityEnable         : out Std_Logic;
          EDACEnable           : out Std_Logic;
          MHold_Out_ck1_N      : out Std_Logic;
          DoNotRdUART          : out Std_Logic;
          DMAInPrgs            : out  std_logic;
          
          MECRegister_Access   : out Std_Logic);
end component;

component MECControlAndSupportFunctions
port (
           D_Int_In            : in Std_Logic_Vector(31 downto 0);
           Clk2_In             : in Std_Logic;
           MECControlReg_N     : in Std_Logic;
           PwrDReg_N           : in Std_Logic;
           SWRReg_N            : in Std_Logic;
           ErrorReset_N        : in Std_Logic;
           WDReset_N           : in Std_Logic;
           SysReset_In_N       : in Std_Logic;
           Wr_Int_N            : in Std_Logic;
           WDClk_In            : in Std_Logic;
           DPar_Int_In         : in Std_Logic;
           MECHalt_Int_N       : in Std_Logic;
           UARTs_Quiet         : in Std_Logic;
           AnyInterrupt        : in Std_Logic;
           D_CtlSup_Out       : out Std_Logic_Vector(31 downto 0);
           Clk_Int            : in Std_Logic;
           Reset_Int_N        : buffer Std_Logic;
           UART_Clk_En        : out Std_Logic;
           Clk2_Int           : out Std_Logic;
           Reset_Out_N        : out Std_Logic;
           Reset_N_ext        : out Std_Logic;
           Reset_Cause        : out Std_Logic_Vector(1 downto 0);
           ResetOutDetect     : out std_logic;
           NoClkDetect        : out std_logic;
           WDStrobe           : out Std_Logic;
           Clk_Out            : buffer Std_Logic;
           PowerDown          : out Std_Logic;
           AccessProtEn       : out Std_Logic;
           ErrorCtrl          : out Std_Logic_Vector(9 downto 0);
           DMAEnable_Int      : out Std_Logic;
           DMAParity_En       : out Std_Logic;
           DMATimeOut_En      : out Std_Logic;
           DPar_CtlSup_Out    : out Std_Logic;
           BusTimeOut_En      : out std_logic;
           ParErrCtlSup       : out Std_Logic );
end component;

component InterruptAndErrorHandling
port (
           Clk_Int         : in  Std_Logic;
           Rd_Int_In       : in Std_Logic;
           MHold_Out_ck1_N : in Std_Logic;
	         SFSRTmp_UD      : in  Std_Logic;
           WDClk_In        : in  Std_Logic;
           WDStrobe        : in Std_Logic;
           NewCycle        : in  Std_Logic;
           D_Int_In        : in  std_logic_Vector(31 downto 0);
           DPar_Int_In     : in  Std_Logic;
           WDProgramReg_N  : in  Std_Logic;
           WDTDSetReg_N    : in  Std_Logic;
           CPUHalt_Out_N   : in  Std_Logic;
           Reset_Out_N     : in  Std_Logic;
           Reset_Int_N     : in  Std_Logic;
           Reset_Cause     : in  Std_Logic_Vector(1 downto 0);
           ResetOutDetect  : in std_logic;
           NoClkDetect     : in std_logic;
           ErrResStatReg_N : in  Std_Logic;
           Wr_Int_N        : in  Std_Logic;
           ErrorCtrl       : in  Std_Logic_Vector(9 downto 0);
           IUErr_IN_N      : in  Std_Logic;
           IUHWErr_IN_N    : in  Std_Logic;
           IUCmpErr_IN_N   : in  Std_Logic;
           FPUHWErr_IN_N   : in  Std_Logic;
           FPUCmpErr_IN_N  : in  Std_Logic;
           SysFSReg_N      : in  Std_Logic;
           Instr_Fetch     : in  Std_Logic;
           Null_Int_rck    : in  Std_Logic;
	         SysBus_Error    : in  Std_Logic;
	         BusTimeOut      : in  Std_Logic;
	         NCError_ck1     : in  Std_Logic;
	         Unimpl_Address_Out         : in  Std_Logic;
	   
	         CPED                       : in  Std_Logic;
	   
	         Illegal_Address_Out        : in  Std_Logic;
           MemAccess_Violation_Out    : in  Std_Logic;
	         APar_Error      : in  Std_Logic;
	         DPar_Error_ck1  : in  Std_Logic;
	         CtlPar_Error    : in  Std_Logic;
           DMATimeOut      : in  Std_Logic;
           DMAInPrgs       : in  Std_Logic;
           Intr_UART_Err   : in  Std_Logic;
           Intr_UARTA_Data  : in  Std_Logic;
           Intr_UARTB_Data  : in  Std_Logic;
           CError_ck1      : in  Std_Logic;
           AccessType      : in  Std_Logic_Vector(3 downto 0);
           IntShapeReg_N   : in  Std_Logic;
	         IntPendReg_N    : in  Std_Logic;
	         IntMaskReg_N    : in  Std_Logic;
	         IntClearReg_N   : in  Std_Logic;
	         IntForceReg_N   : in  Std_Logic;
           ExtInt_In_fck   : in  std_logic_vector(4 downto 0);
           ExtInt_In_rck   : in  std_logic_vector(4 downto 0);
	         IntAck_In       : in  Std_Logic;
	         A_Int_Trap      : in  std_logic_vector(3 downto 0);
           RTCTimTO        : in  Std_Logic;
           GPTimTO_A       : in  Std_Logic;
	         IntrTest_En     : in  Std_Logic;
           SWHalt_En       : in  Std_Logic;
           
           D_Wdog_Out      : out Std_Logic_Vector(31 downto 0);
           DPar_Wdog_Out   : out Std_Logic;
           
           AnyInterrupt    : out Std_Logic;
	         ExtIntAck_Out   : out Std_Logic;
	         IRL_Out         : out std_logic_vector(3 downto 0);
           Write_Inhibit   : out Std_Logic;
           MExc_Int        : out Std_Logic;
           Load_FFAR_N     : out Std_Logic;
           SysErr_Out_N    : out Std_Logic;
           MECHWErr_Out_N  : out Std_Logic;
           WDReset_N       : out Std_Logic;
           ErrorReset_N    : out Std_Logic;
           ErrorHalt_N     : out Std_Logic;
           ParityErrorIntErr : out Std_Logic;
           D_IntErr_Out      : out std_logic_Vector(31 downto 0);
           DPar_IntErr_Out   : out Std_Logic;
           SysAv_Out         : out Std_Logic );
end component;

component DataMux
port (
           Clk_Int            : in std_logic;
           Reset_Out_N        : in std_logic;
           MECRegSel_N        : in std_logic;
           -- From module uarts
           UARTAReg_N         : in Std_Logic;
           UARTBReg_N         : in Std_Logic;
           UARTStatusReg_N    : in Std_Logic;
           
           D_UARTA_Out        : in Std_Logic_Vector(31 downto 0);
           DPar_UARTA_Out     : in Std_Logic;
           D_UARTB_Out        : in Std_Logic_Vector(31 downto 0);
           DPar_UARTB_Out     : in Std_Logic;
           D_UARTS_Out        : in Std_Logic_Vector(31 downto 0);
           DPar_UARTS_Out     : in Std_Logic;
           
           DPar_Wdog_Out      : in Std_Logic;
           D_Wdog_Out         : in Std_Logic_Vector(31 downto 0);
           WDProgramReg_N     : in Std_Logic;
           
           -- From module timers
           GPT_A_CountReg_N    : in Std_Logic;
           GPT_A_ScalerReg_N   : in Std_Logic;
           RTC_CountReg_N      : in Std_Logic;
           RTC_ScalerReg_N     : in Std_Logic;
           D_Tim_Out           : in Std_Logic_Vector(31 downto 0);
           DPar_Tim_Out        : in Std_Logic;
           ParityError_Tim     : in Std_Logic;
           -- From module mecctrl
           MECControlReg_N     : in Std_Logic;
           D_CtlSup_Out        : in Std_Logic_Vector(31 downto 0);
           DPar_CtlSup_Out     : in Std_Logic;
           ParErrCtlSup        : in Std_Logic;
            -- From module interr
           ErrResStatReg_N     : in Std_Logic;
           SysFSReg_N          : in Std_Logic;
           IntShapeReg_N       : in Std_Logic;
	         IntPendReg_N        : in Std_Logic;
	         IntMaskReg_N        : in Std_Logic;
	         IntForceReg_N       : in Std_Logic;
           D_IntErr_Out        : in std_logic_Vector(31 downto 0);
           DPar_IntErr_Out     : in Std_Logic;
           ParityErrorIntErr   : in Std_Logic;
           -- From module sysbif
	         FFAReg_N             : in Std_Logic;
           TestControl_Reg_N    : in Std_Logic;
	         MemConfigReg_N       : in Std_Logic;
	         IOConfigReg_N        : in Std_Logic;           
	         MemAccessReg_N       : in Std_Logic;
           CorrectedData_Reg_N  : in Std_Logic;
           StoreByte_Reg_Read_N : in Std_Logic;
           Rom8Reg_N            : in Std_Logic;
           CorrectDataLatch     : in Std_Logic_Vector(31 downto 0);
           D_EDAC               : in std_logic_vector(31 downto 0);
           DParErr_EDAC         : in std_logic;
           D_MemIoConfig_out    : in std_logic_vector(31 downto 0);
           DPar_MemIOConfig_out : in std_logic;
           ParErrMemIOConfig    : in std_logic;
           D_MemAcc             : in std_logic_vector(31 downto 0);
           DPar_MemAcc          : in std_logic;
           ParErr_MemAcc        : in std_logic;
	         Unimpl_Address_Out   : in Std_Logic;
           CPED                 : out Std_Logic;
           D_Out                : out std_logic_Vector(31 downto 0);
           DPar_Int_Out         : out Std_Logic
           );
end component;


--------------------------------------------------------------
-- Local defined default values on not implemented part
--------------------------------------------------------------
-- Internal signals

    signal CPED                : Std_Logic;
    signal MECRegSel_N         : Std_Logic;
    signal UART_Clk_En_Int     : Std_Logic;
    signal MHold_Out_ck1_N     : Std_Logic;
    signal IOConfigReg_N       : Std_Logic;
    signal MemConfigReg_N      : Std_Logic;
    signal MECControlReg_N     : Std_Logic;
    signal IntShapeReg_N       : Std_Logic;
    signal IntPendReg_N        : Std_Logic;
    signal IntMaskReg_N        : Std_Logic;
    signal IntClearReg_N       : Std_Logic;
    signal IntForceReg_N       : Std_Logic;
    signal WDProgramReg_N      : Std_Logic;
    signal WDTDSetReg_N        : Std_Logic;
    signal RTC_CountReg_N      : Std_Logic; 
    signal RTC_ScalerReg_N     : Std_Logic; 
    signal GPT_A_CountReg_N    : Std_Logic; 
    signal GPT_A_ScalerReg_N   : Std_Logic; 
    signal TimerControlReg_N   : Std_Logic; 
    signal UARTAReg_N          : Std_Logic;
    signal UARTBReg_N          : Std_Logic;
    signal UARTStatusReg_N     : Std_Logic;
    signal SysFSReg_N          : Std_Logic;
    signal ErrResStatReg_N     : Std_Logic;
    signal MemAccess_Violation_Out    : Std_Logic;
    signal D_CtlSup_Out        : Std_Logic_Vector(31 downto 0);
    signal D_SysBusIf_Out      : Std_Logic_Vector(31 downto 0);
    signal DPar_CtlSup_Out     : Std_Logic;
    signal D_Int_In            : Std_Logic_Vector(31 downto 0);
    signal Clk2_Int            : Std_Logic;
    signal PwrDReg_N           : Std_Logic;
    signal SWRReg_N            : Std_Logic;
    signal WDReset_N           : Std_Logic;
    signal DPar_Int_In         : Std_Logic;
    signal Clk_Int             : Std_Logic;
    signal Reset_Int_N         : Std_Logic;
    signal WDStrobe            : Std_Logic;
    signal PowerDown           : Std_Logic;
    signal ParErrCtlSup        : Std_Logic;
    signal ParErrSysBusIf      : Std_Logic;
    signal Rd_Int_In           : Std_Logic;
    signal DMATimeOut_En       : Std_Logic;
    signal DMAParity_En        : Std_Logic;
    signal DMAEnable_Int       : Std_Logic;
    signal DPar_Int_Out        : Std_Logic;
    signal MEXC_Int            : Std_Logic;
    signal MECHalt_Int_N       : Std_Logic;
    signal DMAInPrgs           : Std_Logic;
    signal NewCycle            : Std_Logic;
    signal ErrorCtrl           : Std_Logic_Vector(9 downto 0);
    signal SysBus_Error        : Std_Logic;
    signal BusTimeOut          : Std_Logic;
    signal NCError_ck1         : Std_Logic;
    signal Unimpl_Address_Out  : Std_Logic;
    signal Illegal_Address_Out : Std_Logic;
    signal APar_Error          : Std_Logic;
    signal CtlPar_Error        : Std_Logic;
    signal DMATimeOut          : Std_Logic;
    signal CError_ck1          : Std_Logic;
    signal AccessType          : Std_Logic_Vector(3 downto 0);
    signal RTCTimTO            : Std_Logic;
    signal GPTimTO_A           : Std_Logic;
    signal IntrTest_En         : Std_Logic;
    signal SWHalt_En           : Std_Logic;
    signal AnyInterrupt        : Std_Logic;
    signal Write_Inhibit       : Std_Logic;
    signal Load_FFAR_N         : Std_Logic;
    signal ErrorReset_N        : Std_Logic;
    signal ErrorHalt_N         : Std_Logic;
    signal Reset_Cause         : Std_Logic_Vector(1 downto 0);
    signal ResetOutDetect      : Std_Logic;
    signal NoClkDetect         : Std_Logic;
    signal DPar_SysBusIf_Out   : Std_Logic;
    signal Rom8Reg_N           : Std_Logic;
    signal StoreByte_Reg_Read_N : Std_Logic;
    signal CorrectedData_Reg_N  : Std_Logic;
    signal MemAccessReg_N       : Std_Logic;
    signal TestControl_Reg_N    : Std_Logic;
    signal FFAReg_N             : Std_Logic;
    signal ParityErrorIntErr    : Std_Logic;
    signal DPar_IntErr_Out      : Std_Logic;
    signal D_IntErr_Out         : Std_logic_Vector(31 downto 0);
    signal ParityError_Tim      : Std_Logic;
    signal DPar_Tim_Out         : Std_Logic;
    signal D_Tim_Out            : Std_logic_Vector(31 downto 0);
    signal DPar_UART_Out        : Std_Logic;
    signal D_UART_Out           : Std_logic_Vector(31 downto 0);
    signal Intr_UART_Err        : Std_Logic;
    signal Intr_UARTA_Data      : Std_Logic;
    signal Intr_UARTB_Data      : Std_Logic;
    signal UARTs_Quiet          : Std_Logic;
    signal Instr_Fetch          : Std_Logic;
    signal Null_Int_rck         : Std_Logic;
    signal Clk_Int_Out          : Std_Logic;
    signal CPUHalt_Int_Out_N    : Std_Logic;
    signal Reset_Int_Out_N      : Std_Logic;
    signal AccessProtEn         : Std_Logic;
    signal BusTimeOut_En        : Std_logic;
    signal D_UARTA_Out          : Std_Logic_Vector(31 downto 0);
    signal DPar_UARTA_Out       : Std_Logic;
    signal D_UARTB_Out          : Std_Logic_Vector(31 downto 0);
    signal DPar_UARTB_Out       : Std_Logic;
    signal D_UARTS_Out          : Std_Logic_Vector(31 downto 0);
    signal DPar_UARTS_Out       : Std_Logic;
    signal CorrectDataLatch     : Std_Logic_Vector(31 downto 0);
    signal D_EDAC               : Std_Logic_Vector(31 downto 0);
    signal DParErr_EDAC         : Std_Logic;
    signal D_MemIoConfig_out    : Std_Logic_Vector(31 downto 0);
    signal DPar_MemIOConfig_out : Std_Logic;
    signal ParErrMemIOConfig    : Std_Logic;
    signal D_MemAcc          : Std_Logic_Vector(31 downto 0);
    signal DPar_MemAcc       : Std_Logic;
    signal ParErr_MemAcc     : Std_Logic;
    signal DPar_Error_ck1    : Std_Logic;
    signal DoNotRdUART       : Std_Logic;
    signal A_Int_in          : Std_Logic_Vector(31 downto 0);
    signal SFSRTmp_UD        : Std_Logic;
    signal DPar_Wdog_Out     : Std_Logic;
    signal D_Wdog_Out        : Std_Logic_Vector(31 downto 0);
    signal GUARTAReg_N       : Std_Logic;
    signal GUARTBReg_N       : Std_Logic;
    signal GUARTStatusReg_N  : Std_Logic;
    signal Clk_Out_loc       : Std_Logic;
    signal Reset_N_ext       : Std_Logic;
    signal DBEn              : Std_Logic;
    signal DParIOEn          : Std_Logic;
    signal CBEn              : Std_Logic;

begin

  CPUHalt_Out_N <= CPUHalt_Int_Out_N;
  Reset_Out_N   <= Reset_N_ext;
  Clk_Out       <= Clk_Out_loc;
  
  DBEn_N       <= not DBEn;
  DParIOEn_N   <= not DParIOEn;
  CBEn_N       <= not CBEn;
  
  
UA : UARTs
port map  (
          Clk_Int => Clk_Out,
          UART_Clk_En => UART_Clk_En_Int,
          Reset_Int_N => Reset_Int_N,
          DoNotRdUART => DoNotRdUART,
          GUARTAReg_N => GUARTAReg_N,
          GUARTBReg_N => GUARTBReg_N,
          GUARTStatusReg_N => GUARTStatusReg_N,
          UARTAReg_N => UARTAReg_N,
          UARTBReg_N => UARTBReg_N,
          UARTStatusReg_N => UARTStatusReg_N,
          MECControlReg_N => MECControlReg_N,
          Wr_Int_N => Wr_Int_N,
          Rd_Int_In => Rd_Int_In,
          D_Int_In => D_Int_In,
          RxA_In => RxA_In,
          RxB_In => RxB_In,
          TxA_Out => TxA_Out,
          TxB_Out => TxB_Out,
          Intr_UARTA_Data => Intr_UARTA_Data,
          Intr_UARTB_Data => Intr_UARTB_Data,
          UARTs_Quiet => UARTs_Quiet,
          Intr_UART_Err => Intr_UART_Err,
          D_UARTA_Out => D_UARTA_Out,
          DPar_UARTA_Out => DPar_UARTA_Out,
          D_UARTB_Out => D_UARTB_Out,
          DPar_UARTB_Out => DPar_UARTB_Out,
          D_UARTS_Out => D_UARTS_Out,
          DPar_UARTS_Out => DPar_UARTS_Out
         );

TIM : Timers
port map  (
          Clk_Int => Clk_Out,
          Reset_Int_N => Reset_Int_N,
          MECHalt_Int_N => MECHalt_Int_N,
          Wr_Int_N => Wr_Int_N,
          GPT_A_CountReg_N => GPT_A_CountReg_N,
          GPT_A_ScalerReg_N => GPT_A_ScalerReg_N,
          RTC_CountReg_N => RTC_CountReg_N,
          RTC_ScalerReg_N => RTC_ScalerReg_N,
          TimerControlReg_N => TimerControlReg_N,
          D_Int_In => D_Int_In,
          DPar_Int_In => DPar_Int_In,
          ParityError_Tim => ParityError_Tim,
          D_Tim_Out => D_Tim_Out,
          DPar_Tim_Out => DPar_Tim_Out,
          GPTimTO_A => GPTimTO_A,
          RTCTimTO => RTCTimTO
         );

SBI : SystemBusInterface
port map  (
          Clk_Int => Clk_Out,
          Clk2_Int => Clk2_Int,
          BusTimeOut_En => BusTimeOut_En,
          PowerDown => PowerDown,
          MemData_Valid => MemData_Valid,
          Lock_In => Lock_In,
          Reset_Int_N => Reset_Int_N,
          APar_In => APar_In,
          DParIO_in => DParIO_in,
          ASPar_In => ASPar_In,
          DMAAS_in => DMAAS_in,
          DRdy_Out_N => DRdy_Out_N,
          INull_In => INull_In,
          Wrt_In => Wrt_In,
          D_In => D_In,
          RomWrt_In_N => RomWrt_In_N,
          NoPar_In_N => NoPar_In_N,
          BusErr_In_N => BusErr_In_N,
          DMAReq_In_N => DMAReq_In_N,
          ExtHold_In_N => ExtHold_In_N,
          ExtCCV_In => ExtCCV_In,
          ASI_In => ASI_In,
          LdSto_In => LdSto_In,
          CB_In => CB_In,
          PROM8_In_N => PROM8_In_N,
          BusRdy_In_N => BusRdy_In_N,
          A_In => A_In,
          A_Int_in => A_Int_in,
          SFSRTmp_UD => SFSRTmp_UD,
          WE_In_N => WE_In_N,
          Rd_In => Rd_In,
          DXfer_In => DXfer_In,
          IMPar_In => IMPar_In,
          Size_In => Size_In,
          Reset_Out_N => Reset_Int_Out_N,
          AccessProtEn => AccessProtEn,
          DMAEnable_Int => DMAEnable_Int,
          DMATimeOut_En => DMATimeOut_En,
          DMAParity_En => DMAParity_En,
          DPar_Int_Out => DPar_Int_Out,
          MExc_Int => MExc_Int,
          ErrorHalt_N => ErrorHalt_N,
          SysHalt_In_N => SysHalt_In_N,
          NewCycle => NewCycle,
          Write_Inhibit => Write_Inhibit,
          Load_FFAR_N => Load_FFAR_N,
          CBWr_Out_N => CBWr_Out_N,
          D_Int_In => D_Int_In,
          Rd_Int_In => Rd_Int_In,
          MemBEn_Out_N => MemBEn_Out_N,
          RAMBEn_Out_N => RAMBEn_Out_N,
          ROMBEn_Out_N => ROMBEn_Out_N,
          COE_Out_N => COE_Out_N,
          ExMCS_Out_N => ExMCS_Out_N,
          IOBEn_Out_N => IOBEn_Out_N,
          OE_Out_N => OE_Out_N,
          MemWr_Out_N => MemWr_Out_N,
          DParIO_Out => DParIO_Out,
          CB_Out => CB_Out,
          DDir_Out => DDir_Out,
          IOWr_Out_N => IOWr_Out_N,
          CBEn => CBEn,
          DMAGnt_Out_N => DMAGnt_Out_N,
          BHold_Out_N => BHold_Out_N,
          AOE_Out_N => AOE_Out_N,
          MemCS_Out_N => MemCS_Out_N,
          Wr_Int_N => Wr_Int_N,
          MDS_Out_N => MDS_Out_N,
          MHold_Out_N => MHold_Out_N,
          DPar_Int_In => DPar_Int_In,
          MExc_Out_N => MExc_Out_N,
          BA_Out => BA_Out,
          MECControlReg_N => MECControlReg_N,
          ALE_Out_N => ALE_Out_N,
          RomCS_Out_N => RomCS_Out_N,
          IOSel_Out_N => IOSel_Out_N,
          DOE_Out_N => DOE_Out_N,
          MemConfigReg_N => MemConfigReg_N,
          IOConfigReg_N => IOConfigReg_N,
          IntShapeReg_N => IntShapeReg_N,
          IntPendReg_N => IntPendReg_N,
          IntMaskReg_N => IntMaskReg_N,
          IntClearReg_N => IntClearReg_N,
          IntForceReg_N => IntForceReg_N,
          WDProgramReg_N => WDProgramReg_N,
          WDTDSetReg_N => WDTDSetReg_N,
          RTC_CountReg_N => RTC_CountReg_N,
          RTC_ScalerReg_N => RTC_ScalerReg_N,
          GPT_A_CountReg_N => GPT_A_CountReg_N,
          GPT_A_ScalerReg_N => GPT_A_ScalerReg_N,
          TimerControlReg_N => TimerControlReg_N,
          GUARTAReg_N => GUARTAReg_N,
          GUARTBReg_N => GUARTBReg_N,
          GUARTStatusReg_N => GUARTStatusReg_N,
          MECRegSel_N => MECRegSel_N,
          UARTAReg_N => UARTAReg_N,
          UARTBReg_N => UARTBReg_N,
          UARTStatusReg_N => UARTStatusReg_N,
          SysFSReg_N => SysFSReg_N,
          ErrResStatReg_N => ErrResStatReg_N,
          FFAReg_N => FFAReg_N,
          TestControl_Reg_N => TestControl_Reg_N,
          MemAccessReg_N => MemAccessReg_N,
          StoreByte_Reg_Read_N => StoreByte_Reg_Read_N,
          CorrectedData_Reg_N => CorrectedData_Reg_N,
          ROM8Reg_N => ROM8Reg_N,
          SWRReg_N => SWRReg_N,
          PWRDReg_N => PWRDReg_N,
          IntrTest_En => IntrTest_En,
          SWHalt_En => SWHalt_En,
          MemAccess_Violation_Out => MemAccess_Violation_Out,
          AccessType => AccessType,
          Unimpl_Address_Out => Unimpl_Address_Out,
          Illegal_Address_Out => Illegal_Address_Out,
          APar_Error => APar_Error,
          DPar_Error_ck1 => DPar_Error_ck1,
          CtlPar_Error => CtlPar_Error,
          NCError_ck1 => NCError_ck1,
          CError_ck1 => CError_ck1,
          DMATimeOut => DMATimeOut,
          DBEn => DBEn,
          DParIOEn  => DParIOEn,
          SysBus_Error => SysBus_Error,
          BusTimeOut => BusTimeOut,
          CorrectDataLatch => CorrectDataLatch,
          D_EDAC => D_EDAC,
          DParErr_EDAC => DParErr_EDAC,
          D_MemIoConfig_out => D_MemIoConfig_out,
          DPar_MemIOConfig_out => DPar_MemIOConfig_out,
          ParErrMemIOConfig => ParErrMemIOConfig,
          D_MemAcc => D_MemAcc,
          DPar_MemAcc => DPar_MemAcc,
          ParErr_MemAcc => ParErr_MemAcc,
          MECHalt_Int_N => MECHalt_Int_N,
          Instr_Fetch => Instr_Fetch,
          Null_Int_rck => Null_Int_rck,
          CPUHalt_Out_N => CPUHalt_Int_Out_N,
          ParityEnable => ParityEnable,
          EDACEnable => EDACEnable,
          MHold_Out_ck1_N => MHold_Out_ck1_N,
          DoNotRdUART => DoNotRdUART,
          DMAInPrgs => DMAInPrgs,
          MECRegister_Access => MECRegister_Access);

MCSF : MECControlAndSupportFunctions
port map  (
           D_Int_In => D_Int_In,
           Clk2_In => Clk2_In,
           MECControlReg_N => MECControlReg_N,
           PwrDReg_N => PwrDReg_N,
           SWRReg_N => SWRReg_N,
           ErrorReset_N => ErrorReset_N,
           WDReset_N => WDReset_N,
           SysReset_In_N => SysReset_In_N,
           Wr_Int_N => Wr_Int_N,
           WDClk_In => WDClk_In,
           DPar_Int_In => DPar_Int_In,
           MECHalt_Int_N => MECHalt_Int_N,
           UARTs_Quiet => UARTs_Quiet,
           AnyInterrupt => AnyInterrupt,
           D_CtlSup_Out => D_CtlSup_Out,
           Clk_Int => Clk_Out,
           Reset_Int_N => Reset_Int_N,
           UART_Clk_En => UART_Clk_En_Int,
           Clk2_Int => Clk2_Int,
           Reset_Out_N => Reset_Int_Out_N,
           Reset_N_ext => Reset_N_ext,
           Reset_Cause => Reset_Cause,
           ResetOutDetect => ResetOutDetect,
           NoClkDetect => NoClkDetect,
           WDStrobe => WDStrobe,
           Clk_Out => Clk_Out_loc,
           PowerDown => PowerDown,
           AccessProtEn => AccessProtEn,
           ErrorCtrl => ErrorCtrl,
           DMAEnable_Int => DMAEnable_Int,
           DMAParity_En => DMAParity_En,
           DMATimeOut_En => DMATimeOut_En,
           DPar_CtlSup_Out => DPar_CtlSup_Out,
           BusTimeOut_En => BusTimeOut_En,
           ParErrCtlSup => ParErrCtlSup
         );


IAEH : InterruptAndErrorHandling
port map  (
           Clk_Int => Clk_Out,
           Rd_Int_In => Rd_Int_In,
           MHold_Out_ck1_N => MHold_Out_ck1_N,
           SFSRTmp_UD => SFSRTmp_UD,
           WDClk_In => WDClk_In,
           WDStrobe => WDStrobe,
           NewCycle => NewCycle,
           D_Int_In => D_Int_In,
           DPar_Int_In => DPar_Int_In,
           WDProgramReg_N => WDProgramReg_N,
           WDTDSetReg_N => WDTDSetReg_N,
           CPUHalt_Out_N => CPUHalt_Int_Out_N,
           Reset_Out_N => Reset_Int_Out_N,
           Reset_Int_N => Reset_Int_N,
           Reset_Cause => Reset_Cause,
           ResetOutDetect => ResetOutDetect,
           NoClkDetect => NoClkDetect,
           ErrResStatReg_N => ErrResStatReg_N,
           Wr_Int_N => Wr_Int_N,
           ErrorCtrl => ErrorCtrl,
           IUErr_IN_N => IUErr_IN_N,
           IUHWErr_IN_N => IUHWErr_IN_N,
           IUCmpErr_IN_N => IUCmpErr_IN_N,
           FPUHWErr_IN_N => FPUHWErr_IN_N,
           FPUCmpErr_IN_N => FPUCmpErr_IN_N,
           SysFSReg_N => SysFSReg_N,
           Instr_Fetch => Instr_Fetch,
           Null_Int_rck => Null_Int_rck,
	         SysBus_Error => SysBus_Error,
	         BusTimeOut => BusTimeOut,
	         NCError_ck1 => NCError_ck1,
	         Unimpl_Address_Out => Unimpl_Address_Out,
	         CPED => CPED,
	         Illegal_Address_Out => Illegal_Address_Out,
           MemAccess_Violation_Out => MemAccess_Violation_Out,
	         APar_Error => APar_Error,
	         DPar_Error_ck1 => DPar_Error_ck1,
	         CtlPar_Error => CtlPar_Error,
           DMATimeOut => DMATimeOut,
           DMAInPrgs   => DMAInPrgs,
           Intr_UART_Err => Intr_UART_Err,
           Intr_UARTA_Data => Intr_UARTA_Data,
           Intr_UARTB_Data => Intr_UARTB_Data,
           CError_ck1 => CError_ck1,
           AccessType => AccessType,
           IntShapeReg_N => IntShapeReg_N,
	         IntPendReg_N => IntPendReg_N,
	         IntMaskReg_N => IntMaskReg_N,
	         IntClearReg_N => IntClearReg_N,
	         IntForceReg_N => IntForceReg_N,
	         ExtInt_In_fck => ExtInt_In,
	         ExtInt_In_rck => ExtInt_In,
	         IntAck_In => IntAck_In,
	         A_Int_Trap => A_In(7 downto 4),
           RTCTimTO => RTCTimTO,
           GPTimTO_A => GPTimTO_A,
	         IntrTest_En => IntrTest_En,
           SWHalt_En => SWHalt_En,
	         D_Wdog_Out => D_Wdog_Out,
           DPar_Wdog_Out => DPar_Wdog_Out,
           AnyInterrupt => AnyInterrupt,
	         ExtIntAck_Out => ExtIntAck_Out,
	         IRL_Out => IRL_Out,
           Write_Inhibit => Write_Inhibit,
           MExc_Int => MExc_Int,
           Load_FFAR_N => Load_FFAR_N,
           SysErr_Out_N => SysErr_Out_N,
           MECHWErr_Out_N => MECHWErr_Out_N,
           WDReset_N => WDReset_N,
           ErrorReset_N => ErrorReset_N,
           ErrorHalt_N => ErrorHalt_N,
           ParityErrorIntErr => ParityErrorIntErr,
           D_IntErr_Out => D_IntErr_Out,
           DPar_IntErr_Out => DPar_IntErr_Out,
           SysAv_Out => SysAv_Out
         );


DM : DataMux
port map  (
           Clk_Int => Clk_Out,
           Reset_Out_N => Reset_Int_Out_N,
           MECRegSel_N => MECRegSel_N,
           UARTAReg_N => UARTAReg_N,
           UARTBReg_N => UARTBReg_N,
           UARTStatusReg_N => UARTStatusReg_N,
           D_UARTA_Out => D_UARTA_Out,
           DPar_UARTA_Out => DPar_UARTA_Out,
           D_UARTB_Out => D_UARTB_Out,
           DPar_UARTB_Out => DPar_UARTB_Out,
           D_UARTS_Out => D_UARTS_Out,
           DPar_UARTS_Out => DPar_UARTS_Out,
           DPar_Wdog_Out  => DPar_Wdog_Out,
           D_Wdog_Out     => D_Wdog_Out,
           WDProgramReg_N => WDProgramReg_N,
           GPT_A_CountReg_N => GPT_A_CountReg_N,
           GPT_A_ScalerReg_N => GPT_A_ScalerReg_N,
           RTC_CountReg_N => RTC_CountReg_N,
           RTC_ScalerReg_N => RTC_ScalerReg_N,
           D_Tim_Out => D_Tim_Out,
           DPar_Tim_Out => DPar_Tim_Out,
           ParityError_Tim => ParityError_Tim,
           MECControlReg_N => MECControlReg_N,
           D_CtlSup_Out => D_CtlSup_Out,
           DPar_CtlSup_Out => DPar_CtlSup_Out,
           ParErrCtlSup => ParErrCtlSup,
           ErrResStatReg_N => ErrResStatReg_N,
           SysFSReg_N => SysFSReg_N,
           IntShapeReg_N => IntShapeReg_N,
	         IntPendReg_N => IntPendReg_N,
	         IntMaskReg_N => IntMaskReg_N,
	         IntForceReg_N => IntForceReg_N,
           D_IntErr_Out => D_IntErr_Out,
           DPar_IntErr_Out => DPar_IntErr_Out,
           ParityErrorIntErr => ParityErrorIntErr,
	         FFAReg_N => FFAReg_N,
           TestControl_Reg_N => TestControl_Reg_N,
	         MemConfigReg_N => MemConfigReg_N,
	         IOConfigReg_N => IOConfigReg_N,
	         MemAccessReg_N => MemAccessReg_N,
           CorrectedData_Reg_N => CorrectedData_Reg_N,
           StoreByte_Reg_Read_N => StoreByte_Reg_Read_N,
           Rom8Reg_N => Rom8Reg_N,
           CorrectDataLatch => CorrectDataLatch,
           D_EDAC => D_EDAC,
           DParErr_EDAC => DParErr_EDAC,
           D_MemIoConfig_out => D_MemIoConfig_out,
           DPar_MemIOConfig_out => DPar_MemIOConfig_out,
           ParErrMemIOConfig => ParErrMemIOConfig,
           D_MemAcc => D_MemAcc,
           DPar_MemAcc => DPar_MemAcc,
           ParErr_MemAcc => ParErr_MemAcc,
	         Unimpl_Address_Out => Unimpl_Address_Out, 
           CPED => CPED,
           D_Out => D_Out,
           DPar_Int_Out => DPar_Int_Out 
         );
         
         
TAP_1 : TAP 
port map (
         TRst_In_N => TRst_In_N,
         TCK_In => TCK_In,
         TDI_In => TDI_In,
         TMS_In => TMS_In,
         TDO_Out => TDO_Out
         );


end mini_spec;










---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  Saab Ericsson Space AB and it must not be disclosed, copied, altered --
--  or used without the written permission of Saab Ericsson Space AB.    --
---------------------------------------------------------------------------
-- Title:                      MEC Clock Buffer
-- File name:                  \mec\source\test_buff.vhd
-- VHDL unit:                  Test_buffer
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing and environmental
--                             textio inputs included. The buffer is used to 
--                             avoid visibility on the timing at top level.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--  SAAB ERICSSON SPACE AB                                               --
--  Delsjomotet                      Phone Int: +46 31 35 00 00          --
--  S-405 15 GOTHENBURG              Fax   Int: +46 31 35 95 20          --
--  Sweden                           Telex:     27950 saabsp s           --
---------------------------------------------------------------------------
--
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdTiming.all;

------------------------------------------------------------------
-- Entity declaration of 'Test_Buffer'.
------------------------------------------------------------------

ENTITY Test_Buffer IS
  GENERIC(
    t55   : Time := 1000 ns ;
    t56   : Time := 8 ns ;
    t57   : Time := 4 ns ;
    t58   : Time := 8 ns ;
    t59   : Time := 4 ns ;
    t60   : Time := 8 ns ;
    t61   : Time := 4 ns ;
    t62   : Time := 8 ns ;
    CHECK : Boolean := False ) ;
  PORT(
    TDI       : IN     STD_Logic ;
    TCk       : IN     STD_Logic ;
    TRst_N    : IN     STD_Logic ;
    TMS       : IN     STD_Logic ;
    TDO       : OUT    STD_Logic ;
    TDO_Out   : IN     UX01 ;
    TDI_In    : OUT    UX01 ;
    TCk_In    : OUT    UX01 ;
    TRst_In_N : OUT    UX01 ;
    TMS_In    : OUT    UX01 ) ;
END Test_Buffer ;

------------------------------------------------------------------
-- Architectural body of 'Test_Buffer'.
------------------------------------------------------------------

ARCHITECTURE Test_Buffer_body OF Test_Buffer IS
 
-- constant t55  : Time := 1000 ns ;  -- TCk Cycle Time
-- constant t56  : Time := 8 ns ;     -- TRst_N Input Setup Time, ref edge TCk+
-- constant t57  : Time := 4 ns ;     -- TRst_N Input Hold Time, ref edge TCk+
-- constant t58  : Time := 8 ns ;     -- TMS Input Setup Time, ref edge TCk+
-- constant t59  : Time := 4 ns ;     -- TMS Input Hold Time, ref edge TCk+
-- constant t60  : Time := 8 ns ;     -- TDI Input Setup Time, ref edge TCk+
-- constant t61  : Time := 4 ns ;     -- TDI Input Hold Time, ref edge TCk+
-- constant t62  : Time := 8 ns ;     -- TDO Output Delay Time, ref edge TCk+

-- Local constants for pulse width checking
constant TckPWH : time := (t55)/2;
constant TckPWL : time := (t55)/2;
constant TckPWU : time := 1 ns; -- Default to allow check
constant TckPWX : time := 1 ns; -- Default to allow check

begin

-- Setup, Hold and Pulse width violation checks added
------------------------------------------------------------------

           TCk_In      <= To_UX01(TCk);

-- PulsecheckTck
        PulseCheck(     Pin => Tck,
			LEVEL => '1',
			WIDTH => TCkPWH,
			SENSE => MINIMUM,
			PATH => "TCkPWH-MIN VIOLATION");

        PulseCheck(     Pin => Tck,
			LEVEL => '0',
			WIDTH => TCkPWL,
			SENSE => MINIMUM,
			PATH => "TCkPWL-MIN VIOLATION");

        PulseCHECK(     Pin => Tck,
			LEVEL => 'U',
			WIDTH => TckPWU,
			SENSE => MAXIMUM,
			PATH => "Tck NOT DRIVEN VIOLATION");
	
        PulseCHECK(     Pin => Tck,
			LEVEL => 'X',
			WIDTH => TckPWX,
			SENSE => MAXIMUM,
			PATH => "TCk DRIVEN X VIOLATION");
------------------------------------------------------------------
           TRst_In_N   <= To_UX01(TRst_N);

-- SetupHoldcheckTRst
	SetupHoldCheck(	Data => TRst_N,
			Ref => Tck,
			EDGE => RISING,
			SETUP => t56, HOLD => t57,
			PATH => "TRst_N SetupHold Violation",
			DelayedData => TRst_N);
------------------------------------------------------------------
           TMS_In      <= To_UX01(TMS);

-- SetupHoldcheckTMS
	SetupHoldCheck(	Data => TMS,
			Ref => Tck,
			EDGE => RISING,
			SETUP => t58, HOLD => t59,
			PATH => "TMS SetupHold Violation",
			DelayedData => TMS);
------------------------------------------------------------------
           TDI_In      <= To_UX01(TDI);

-- SetupHoldcheckTDI
	SetupHoldCheck(	Data => TDI,
			Ref => Tck,
			EDGE => RISING,
			SETUP => t60, HOLD => t61,
			PATH => "TDI SetupHold Violation",
			DelayedData => TDI);
------------------------------------------------------------------
           TDO         <= TDO_OUT after t62;

END Test_Buffer_body ;
---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  Saab Ericsson Space AB and it must not be disclosed, copied, altered --
--  or used without the written permission of Saab Ericsson Space AB.    --
---------------------------------------------------------------------------
-- Title:                      MEC SystemError Buffer
-- File name:                  \mec\source\Systemer.vhd
-- VHDL unit:                  SystemError_buffer
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing and environmental
--                             textio inputs included. It is used as a 
--                             comparator selector and it is used to avoid
--                             visibility of the timing at top level.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--  SAAB ERICSSON SPACE AB                                               --
--  Delsjomotet                      Phone Int: +46 31 35 00 00          --
--  S-405 15 GOTHENBURG              Fax   Int: +46 31 35 95 20          --
--  Sweden                           Telex:     27950 saabsp s           --
---------------------------------------------------------------------------
--
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdTiming.all;

------------------------------------------------------------------
-- Entity declaration of 'SystemError_Buffer'.
------------------------------------------------------------------

ENTITY SystemError_Buffer IS
  GENERIC(
    t47   : Time := 4 ns ;
    t46   : Time := 8 ns ;
    t48   : Time := 8 ns ;
    CHECK : Boolean := False ) ;
  PORT(
    IUErr_N         : IN     STD_Logic ;
    IUHWErr_N       : IN     STD_Logic ;
    FPUHWErr_N      : IN     STD_Logic ;
    IUCmpErr_N      : IN     STD_Logic ;
    FPUCmpErr_N     : IN     STD_Logic ;
    MecHWErr_N      : OUT    STD_Logic ;
    SysErr_N        : OUT    STD_Logic ;
    SysAv           : OUT    STD_Logic ;
    IUErr_In_N      : OUT    UX01 ;
    IUHWErr_In_N    : OUT    UX01 ;
    FPUHWErr_In_N   : OUT    UX01 ;
    IUCmpErr_In_N   : OUT    UX01 ;
    FPUCmpErr_In_N  : OUT    UX01 ;
    MecHWErr_Out_N  : IN     UX01 ;
    SysErr_Out_N    : IN     UX01 ;
    SysAv_Out       : IN     UX01 ;
    Clk_Out_Int     : IN     UX01 ;
    Clk_Out         : IN     UX01 ) ;
END SystemError_Buffer ;

------------------------------------------------------------------
-- Architectural body of 'SystemError_Buffer'.
------------------------------------------------------------------

ARCHITECTURE SystemError_Buffer_body OF SystemError_Buffer IS

-- constant t46  : Time := 8 ns ;     -- xERR_N Input Setup Time, ref edge Clk+
-- constant t47  : Time := 4 ns ;     -- xERR_N Input Hold Time, ref edge Clk+
-- constant t48  : Time := 8 ns ;     -- xERR_N Output Delay Time, ref edge Clk+

   SIGNAL SysErr_In_N        : UX01 := '1' ;
   SIGNAL SysAv_In           : UX01 := '0' ;
   SIGNAL SECompErr_Internal : UX01 := '0';

BEGIN
-- Setup and Hold time violation checks added
------------------------------------------------------------------
           IUErr_In_N      <= To_UX01(IUErr_N);

-- SetupHoldcheckIUErr
	SetupHoldCheck(	Data => IUErr_N,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP => t46, HOLD => t47,
			PATH => "IUErr_N SetupHold Violation",
			DelayedData => IUErr_N);
------------------------------------------------------------------
           IUHWErr_In_N    <= To_UX01(IUHWErr_N);

-- SetupHoldCheckIUHWErr
	SetupHoldCheck(	Data => IUHWErr_N,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP => t46, HOLD => t47,
			PATH => "IUHWErr_N SetupHold Violation",
			DelayedData => IUHWErr_N);
------------------------------------------------------------------
           IUCmpErr_In_N   <= To_UX01(IUCmpErr_N);

-- SetupHoldCheckIUCMPErr 
	SetupHoldCheck(	Data => IUCmpErr_N,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP => t46, HOLD => t47,
			PATH => "IUCmpErr_N SetupHold Violation",
			DelayedData => IUCmpErr_N);
------------------------------------------------------------------
           FPUHWErr_In_N   <= To_UX01(FPUHWErr_N);

-- SetupHoldCheckFPUHWErr 
	SetupHoldCheck(	Data => FPUHWErr_N,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP => t46, HOLD => t47,
			PATH => "FPUHWErr_N SetupHold Violation",
			DelayedData => FPUHWErr_N);
------------------------------------------------------------------
           FPUCmpErr_In_N  <= To_UX01(FPUCmpErr_N);

-- SetupHoldCheckFPUCmpErr 
	SetupHoldCheck(	Data => FPUCmpErr_N,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP => t46, HOLD => t47,
			PATH => "FPUCmpErr_N SetupHold Violation",
			DelayedData => FPUCmpErr_N);
			
------------------------------------------------------------------
     MecHWErr_N         <= MecHWErr_Out_N after t48;
     

SyserrorSignalProcess: Process(SysErr_Out_N, SysAv_Out)
begin
  SysErr_N  <= SysErr_Out_N after t48;
  SysAv     <= SysAv_Out after t48;

end process; -- SyserrorSignalProcess


END SystemError_Buffer_body ;
---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  Saab Ericsson Space AB and it must not be disclosed, copied, altered --
--  or used without the written permission of Saab Ericsson Space AB.    --
---------------------------------------------------------------------------
-- Title:                      MEC System Control Buffer
-- File name:                  \mec\source\Sysctrl_.vhd
-- VHDL unit:                  Sysctrl_buffer
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing and environmental
--                             textio inputs included. It is used as a 
--                             comparator selector and it is used to avoid
--                             visibility of the timing at top level.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
------------------------------------------------------------------------------
--  SAAB ERICSSON SPACE AB                                               --
--  Delsjomotet                      Phone Int: +46 31 35 00 00          --
--  S-405 15 GOTHENBURG              Fax   Int: +46 31 35 95 20          --
--  Sweden                           Telex:     27950 saabsp s           --
---------------------------------------------------------------------------
--
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdTiming.all;

------------------------------------------------------------------
-- Entity declaration of 'SysCtrl_Buffer'.
------------------------------------------------------------------

ENTITY SysCtrl_Buffer IS
  GENERIC(
    t1    : Time := 44 ns ;
    t69   : Time := 3 ns ;
    t70   : Time := 3 ns ;
    t30   : Time := 15 ns ;
    t31   : Time := 4 ns ;
    t32   : Time := 4 ns ;
    t74   : Time := 4 ns ;
    t75   : Time := 4 ns ;
    CHECK : Boolean := False ) ;
  PORT(
    Clk                : OUT  STD_Logic_Vector(1 DOWNTO 0) ;
    CPUHalt_N          : OUT  STD_Logic ;
    Reset_N            : OUT  STD_Logic ;
    SysHalt_N          : IN     STD_Logic ;
    SysReset_N         : IN     STD_Logic ;
    SysHalt_In_N       : OUT    UX01 ;
    SysReset_In_N      : OUT    UX01 ;
    Clk_Out_Int        : OUT    UX01 ;
    Clk_Out            : IN     UX01 ;
    CPUHalt_Out_N      : IN     UX01 ;
    Reset_Out_N        : IN     UX01) ;
END SysCtrl_Buffer ;

------------------------------------------------------------------
-- Architectural body of 'SysCtrl_Buffer'.
------------------------------------------------------------------

ARCHITECTURE SysCtrl_Buffer_body OF SysCtrl_Buffer IS


 constant ClkPWH : time := (t1)/2;
 constant ClkPWL : time := (t1)/2;
 constant ClkPWU : time := 1 ns; -- Default to allow check
 constant ClkPWX : time := 1 ns; -- Default to allow check

 SIGNAL CPUHalt_In_N       : UX01 := '1' ;
 SIGNAL Reset_In_N         : UX01 := '1' ;
 SIGNAL SysReset_Int_N     : UX01 := '1' ;
 SIGNAL SysHalt_Int_N      : UX01 := '1' ;

BEGIN
-- Setup, Hold and Pulse width violation checks added
------------------------------------------------------------------

           Clk_Out_Int <= Clk_Out;

--PulseCheckClk
        PulseCheck(     Pin => Clk_Out,
			LEVEL => '1',
			WIDTH => ClkPWH,
			SENSE => MINIMUM,
			PATH => "ClkPWH-MIN VIOLATION");

        PulseCheck(     Pin => Clk_Out,
			LEVEL => '0',
			WIDTH => ClkPWL,
			SENSE => MINIMUM,
			PATH => "ClkPWL-MIN VIOLATION");

        PulseCHECK(     Pin => Clk_Out,
			LEVEL => 'U',
			WIDTH => ClkPWU,
			SENSE => MAXIMUM,
			PATH => "Clk NOT DRIVEN VIOLATION");
	
        PulseCHECK(     Pin => Clk_Out,
			LEVEL => 'X',
			WIDTH => ClkPWX,
			SENSE => MAXIMUM,
			PATH => "Clk DRIVEN X VIOLATION");
------------------------------------------------------------------

           SysReset_In_N  <= To_UX01(SysReset_N);

-- SetupHoldCheckSysReset
	SetupHoldCheck(	Data => SysReset_N,
			Ref => Clk_Out,
			EDGE => RISING,
			SETUP => t30, HOLD => t31,
			PATH => "SysReset_N SetupHold Violation",
			DelayedData => SysReset_N);
------------------------------------------------------------------

           SysHalt_In_N  <= To_UX01(SysHalt_N);
 
-- SetupHoldCheckSysHalt
	SetupHoldCheck(	Data => SysHalt_N,
			Ref => Clk_Out,
			EDGE => RISING,
			SETUP => t30, HOLD => t31,
			PATH => "SysHalt_N SetupHold Violation",
			DelayedData => SysHalt_N);
------------------------------------------------------------------


SysctrlSignalProcess: Process(Reset_Out_N, CPUHalt_Out_N, Clk_Out)
begin
       Reset_N         <= Reset_Out_N after t32;
       CPUHalt_N       <= CPUHalt_Out_N after t32;
       Clk             <= (Clk_OUT & Clk_OUT);

end process; -- SysctrlSignalProcess


END SysCtrl_Buffer_body ;




---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  Saab Ericsson Space AB and it must not be disclosed, copied, altered --
--  or used without the written permission of Saab Ericsson Space AB.    --
---------------------------------------------------------------------------
-- Title:                      MEC Setup and check Buffer
-- File name:                  \mec\source\SETUPAND.vhd
-- VHDL unit:                  Setupandcheck_buffer
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing and environmental
--                             textio inputs included. The buffer is used to 
--                             avoid visibility on the timing at top level.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--  SAAB ERICSSON SPACE AB                                               --
--  Delsjomotet                      Phone Int: +46 31 35 00 00          --
--  S-405 15 GOTHENBURG              Fax   Int: +46 31 35 95 20          --
--  Sweden                           Telex:     27950 saabsp s           --
---------------------------------------------------------------------------
--
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdTiming.all;

------------------------------------------------------------------
-- Entity declaration of 'Setupandcheck_buffer'.
------------------------------------------------------------------

ENTITY Setupandcheck_buffer IS
  GENERIC(
    t49   : Time := 8 ns ;
    t50   : Time := 4 ns ;
    CHECK : Boolean := False ) ;
  PORT(
    Clk_Out_Int : IN     UX01 ;
    Prom8_In_N  : OUT    UX01 ;
    NoPar_In_N  : OUT    UX01 ;
    RomWrt_In_N : OUT    UX01 ;
    Prom8_N     : IN     STD_Logic ;
    NoPar_N     : IN     STD_Logic ;
    RomWrt_N    : IN     STD_Logic ) ;
END Setupandcheck_buffer ;

------------------------------------------------------------------
-- Architectural body of 'Setupandcheck_buffer'.
------------------------------------------------------------------

ARCHITECTURE Setupandcheck_buffer_body OF Setupandcheck_buffer IS
-- constant t49  : Time := 8 ns ;     -- CMode_N, NoPar_N, RomWrt_N, Prom8_N
                                      -- Input Setup Time, ref edge Clk+     
-- constant t50  : Time := 4 ns ;     -- CMode_N, NoPar_N, RomWrt_N, Prom8_N
                                      -- Input Hold Time, ref edge Clk+

BEGIN
-- Setup and Hold time violation checks added
------------------------------------------------------------------

           Prom8_In_N  <= To_UX01(Prom8_N);
 
-- SetupHoldCheckProm8
	SetupHoldCheck(	Data => Prom8_N,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP => t49, HOLD => t50,
			PATH => "Prom8_N SetupHold Violation",
			DelayedData => Prom8_N);
------------------------------------------------------------------

           NoPar_In_N  <= To_UX01(NoPar_N);
 
-- SetupHoldCheckNoPar
	SetupHoldCheck(	Data => NoPar_N,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP => t49, HOLD => t50,
			PATH => "NoPar_N SetupHold Violation",
			DelayedData => NoPar_N);
------------------------------------------------------------------

	   RomWrt_In_N  <= To_UX01(RomWrt_N);
 
-- SetupHoldCheckRomWrt 
	SetupHoldCheck(	Data => RomWrt_N,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP => t49, HOLD => t50,
			PATH => "RomWrt_N SetupHold Violation",
			DelayedData => RomWrt_N);
------------------------------------------------------------------
END Setupandcheck_buffer_body ;
---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  Saab Ericsson Space AB and it must not be disclosed, copied, altered --
--  or used without the written permission of Saab Ericsson Space AB.    --
---------------------------------------------------------------------------
-- Title:                      MEC Serial Interface Buffer
-- File name:                  \mec\source\Serial_B.vhd
-- VHDL unit:                  Serial_buffer
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing and environmental
--                             textio inputs included. It is used as a 
--                             comparator selector and it is used to avoid
--                             visibility of the timing at top level.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdTiming.all;

------------------------------------------------------------------
-- Entity declaration of 'Serial_Buffer'.
------------------------------------------------------------------

ENTITY Serial_Buffer IS
  GENERIC(
    CHECK : Boolean := False ) ;
  PORT(
    TxB             : OUT    STD_Logic ;
    TxA             : OUT    STD_Logic ;
    RxA             : IN     STD_Logic ;
    RxB             : IN     STD_Logic ;
    TxB_Out         : IN     UX01 ;
    TxA_Out         : IN     UX01 ;
    RxA_In          : OUT    UX01 ;
    RxB_In          : OUT    UX01 ;
    SerialClk_Int   : IN     UX01 ) ;
END Serial_Buffer ;


------------------------------------------------------------------
-- Architectural body of 'Serial_Buffer'.
------------------------------------------------------------------

ARCHITECTURE Serial_Buffer_body OF Serial_Buffer IS
   SIGNAL TxB_In          : UX01 := '0' ;
   SIGNAL TxA_In          : UX01 := '0' ;
   SIGNAL UARTCompErr_Internal : UX01 := '0'; 

BEGIN
-- Setup and Hold time violation checks added
------------------------------------------------------------------

           RxA_In      <= To_UX01(RxA);
 
-- SetupHoldCheckRxA 
	SetupHoldCheck(	Data => RxA,
			Ref => SerialClk_Int,
			EDGE => RISING,
			SETUP => 0 ns, HOLD => 0 ns,
			PATH => "RxA SetupHold Violation",
			DelayedData => RxA);
------------------------------------------------------------------
           RxB_In      <= To_UX01(RxB);
 
-- SetupHoldCheckRxB
	SetupHoldCheck(	Data => RxB,
			Ref => SerialClk_Int,
			EDGE => RISING,
			SETUP => 0 ns, HOLD => 0 ns,
			PATH => "RxB SetupHold Violation",
			DelayedData => RxB);
------------------------------------------------------------------


SerialProcess: Process(TxA_Out, TxB_Out, SerialClk_Int)
begin
   TxB     <= TxB_Out;
   TxA     <= TxA_Out ;
end process; --SerialSignalProcess

END Serial_Buffer_body ;
---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  Saab Ericsson Space AB and it must not be disclosed, copied, altered --
--  or used without the written permission of Saab Ericsson Space AB.    --
---------------------------------------------------------------------------
-- Title:                      MEC Mem Buffer
-- File name:                  \mec\source\Mem_Buff.vhd
-- VHDL unit:                  Mem_buffer
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing and environmental
--                             textio inputs included. It is used as a 
--                             comparator selector and it is used to avoid
--                             visibility of the timing at top level.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--  SAAB ERICSSON SPACE AB                                               --
--  Delsjomotet                      Phone Int: +46 31 35 00 00          --
--  S-405 15 GOTHENBURG              Fax   Int: +46 31 35 95 20          --
--  Sweden                           Telex:     27950 saabsp s           --
---------------------------------------------------------------------------
--
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdTiming.all;
------------------------------------------------------------------
-- Entity declaration of 'Mem_Buffer'.
------------------------------------------------------------------

ENTITY Mem_Buffer IS
  GENERIC(
    t8    : Time := 15 ns ;
    t9    : Time := 20 ns ;
    t10   : Time := 15 ns ;
    t11   : Time := 1 ns ;
    t12   : Time := 8 ns ;
    t13   : Time := 35 ns ;
    t14   : Time := 35 ns ;
    t15   : Time := 35 ns ;
    t16   : Time := 35 ns ;
    t17   : Time := 15 ns ;
    t74   : Time := 4 ns ;
    t75   : Time := 4 ns ;
    CHECK : Boolean := False ) ;
  PORT(
    BA             : out    Std_Logic_Vector(1 downto 0) ;
    RAMBEn_N       : out    Std_Logic ;
    ROMBEn_N       : out    Std_Logic ;
    MemBEn_N       : out    Std_Logic ;
    MemWr2_N       : out    Std_Logic_Vector(1 downto 0) ;
    MemWr1_N       : out    Std_Logic_Vector(1 downto 0) ;
    OE_N           : out    Std_Logic_Vector(1 downto 0) ;
    DDir           : out    Std_Logic ;
    DDir_N         : out    Std_Logic ;
    MemCS_N        : out    Std_Logic_Vector(9 downto 0) ;
    RomCS_N        : out    Std_Logic ;
    ALE_N          : out    Std_Logic ;
    BA_Out         : in     Std_Logic_Vector(1 downto 0) ;
    MemBEn_Out_N   : in     UX01 ;
    RAMBEn_Out_N   : in     UX01 ;
    ROMBEn_Out_N   : in     UX01 ;
    CBWr_Out_N     : in     UX01 ;
    MemWr_Out_N    : in     UX01 ;
    OE_Out_N       : in     UX01 ;
    DDir_Out       : in     UX01 ;
    MemCS_Out_N    : in     Std_Logic_Vector(9 downto 0) ;
    RomCS_Out_N    : in     UX01 ;
    ALE_Out_N      : in     UX01 ;
    Clk_Out_Int    : in     UX01;
    Clk_Out        : in     UX01;
    A_In           : in     Std_Logic_Vector(31 downto 0) ) ;
END Mem_Buffer ;

------------------------------------------------------------------
-- Architectural body of 'Mem_Buffer'.
------------------------------------------------------------------

ARCHITECTURE Mem_Buffer_body OF Mem_Buffer IS


BEGin
-- Setup and Hold time violation checks added
------------------------------------------------------------------



MemSignalProcess: Process( BA_OUT, ALE_OUT_N, RomCS_OUT_N, MemCS_OUT_N, 
                           OE_OUT_N, MemWr_OUT_N, CBWr_OUT_N, 
                           MemBEn_OUT_N, DDir_Out, Clk_Out_Int, A_In)
  variable AddrChange,AddrSetup,CS_Delay : time := 0 ns;
begin
  
  if A_In'event then
   AddrChange := now;
  end if;

  if rising_edge(Clk_Out_Int) then
    AddrSetup := now - AddrChange;
    if AddrSetup > t9 + abs(t10 - t12) then -- CS generation before edge
      CS_Delay := t10;
    else
      CS_Delay := t9 + t12 - AddrSetup;
    end if;
  end if;


  BA           <= BA_OUT after t17;
  ALE_N        <= ALE_OUT_N after t8;
  RomCS_N      <= RomCS_OUT_N after CS_Delay;
  MemCS_N      <= MemCS_OUT_N after CS_Delay;
  OE_N         <= (OE_OUT_N & OE_OUT_N) after t16;
  RAMBEn_N     <= RAMBEn_OUT_N after t13;
  ROMBEn_N     <= ROMBEn_OUT_N after t13;
  MemBEn_N     <= MemBEn_OUT_N after t13;
  DDir         <= DDir_OUT after t14;
  DDir_N       <= not DDir_OUT after t14;
  MemWr1_N     <= (MemWr_OUT_N & MemWr_OUT_N) after t15;
  MemWr2_N     <= (CBWr_OUT_N & CBWr_OUT_N) after t15;

end process; -- MemSignalProcess


END Mem_Buffer_body ;

---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  Saab Ericsson Space AB and it must not be disclosed, copied, altered --
--  or used without the written permission of Saab Ericsson Space AB.    --
---------------------------------------------------------------------------
-- Title:                      MEC IU Output Buffer
-- File name:                  \mec\source\IU_Out_B.vhd
-- VHDL unit:                  IU_Out_buffer
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing and environmental
--                             textio inputs included. It is used as a 
--                             comparator selector and it is used to avoid
--                             visibility of the timing at top level.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--  SAAB ERICSSON SPACE AB                                               --
--  Delsjomotet                      Phone Int: +46 31 35 00 00          --
--  S-405 15 GOTHENBURG              Fax   Int: +46 31 35 95 20          --
--  Sweden                           Telex:     27950 saabsp s           --
---------------------------------------------------------------------------
--
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdTiming.all;
------------------------------------------------------------------
-- Entity declaration of 'IU_Out_Buffer'.
------------------------------------------------------------------

ENTITY IU_Out_Buffer IS
  GENERIC(
    t25   : Time := 8 ns ;
    t26   : Time := 8 ns ;
    t27   : Time := 8 ns ;
    t28   : Time := 8 ns ;
    t29   : Time := 4 ns ;
    t43   : Time := 8 ns ;
    t37   : Time := 8 ns ;
    t73   : Time := 28 ns ;
    t74   : Time := 4 ns ;
    t75   : Time := 4 ns ;
    CHECK : Boolean := False ) ;
  PORT(
    AOE_N             : out    Std_Logic ;
    COE_N             : out    Std_Logic ;
    DOE_N             : out    Std_Logic ;
    BHold_N           : out    Std_Logic ;
    MDS_N             : out    Std_Logic ;
    MExc_N            : out    Std_Logic ;
    MHold_N           : out    Std_Logic ;
    IRL               : out    Std_Logic_Vector(3 downto 0) ;
    IntAck_In         : out    Std_Logic;
    INTAck            : in     UX01 ;
    AOE_Out_N         : in     UX01 ;
    COE_Out_N         : in     UX01 ;
    DOE_Out_N         : in     UX01 ;
    BHold_Out_N       : in     UX01 ;
    MDS_Out_N         : in     UX01 ;
    MExc_Out_N        : in     UX01 ;
    MHold_Out_N       : in     UX01 ;
    IRL_Out           : in     Std_Logic_Vector(3 downto 0) ;
    Clk_Out_Int       : in     UX01);
END IU_Out_Buffer ;

------------------------------------------------------------------
-- Architectural body of 'IU_Out_Buffer'.
------------------------------------------------------------------

ARCHITECTURE IU_Out_Buffer_body OF IU_Out_Buffer IS

begin

-------------------------------------------------------------------
IntAck_In     <= To_UX01(INTAck);

-- SetupHoldCheckDMAAS 
	SetupHoldCheck(	Data => INTAck,
			Ref => Clk_Out_Int,
			EDGE => RISinG,
			SETUP =>  t28, HOLD => t29,
			PATH => "DMAAS SetupHold Violation",
			DelayedData => INTAck);
-------------------------------------------------------------------

 AOE_N        <= AOE_Out_N after t27;
 COE_N        <= COE_Out_N after t27;
 BHold_N      <= BHold_Out_N after t26;
 MDS_N        <= MDS_Out_N after   t37;
 MHold_N      <= MHold_Out_N after t26;
 IRL          <= IRL_Out after t43;
 MExc_N       <= MExc_Out_N after  t25;
 DOE_N        <= DOE_Out_N after t27;
       
END IU_Out_Buffer_body ;

---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  Saab Ericsson Space AB and it must not be disclosed, copied, altered --
--  or used without the written permission of Saab Ericsson Space AB.    --
---------------------------------------------------------------------------
-- Title:                      MEC IU Input Buffer
-- File name:                  \mec\source\IU_IN_Bu.vhd
-- VHDL unit:                  IU_In_buffer
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing and environmental
--                             textio inputs included. The buffer is used to 
--                             avoid visibility on the timing at top level.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--  SAAB ERICSSON SPACE AB                                               --
--  Delsjomotet                      Phone Int: +46 31 35 00 00          --
--  S-405 15 GOTHENBURG              Fax   Int: +46 31 35 95 20          --
--  Sweden                           Telex:     27950 saabsp s           --
---------------------------------------------------------------------------
--
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdTiming.all;
------------------------------------------------------------------
-- Entity declaration of 'IU_In_Buffer'.
------------------------------------------------------------------

ENTITY IU_In_Buffer IS
  GENERIC(
    t4    : Time := 15 ns ;
    t5    : Time := 4 ns ;
    t6    : Time := 15 ns ;
    t7    : Time := 4 ns ;
    t53   : Time := 8 ns ;
    t54   : Time := 2 ns ;
    t35   : Time := 8 ns ;
    t36   : Time := 4 ns ;
    t33   : Time := 8 ns ;
    t34   : Time := 2 ns ;
    CHECK : Boolean := False ) ;
  PORT(
    A        : IN     STD_Logic_Vector(31 DOWNTO 0) ;
    Size     : IN     STD_Logic_Vector(1 DOWNTO 0) ;
    ASPar    : IN     STD_Logic ;
    APar     : IN     STD_Logic ;
    ASI      : IN     STD_Logic_Vector(3 DOWNTO 0) ;
    IMPar    : IN     STD_Logic ;
    Rd       : IN     STD_Logic ;
    Lock     : IN     STD_Logic ;
    LdSto    : IN     STD_Logic ;
    INull    : IN     STD_Logic ;
    ASPar_In : OUT    UX01 ;
    Size_In  : OUT    STD_Logic_Vector(1 DOWNTO 0) ;
    ASI_In   : OUT    STD_Logic_Vector(3 DOWNTO 0) ;
    APar_In  : OUT    UX01 ;
    A_In     : OUT    STD_Logic_Vector(31 DOWNTO 0) ;
    IMPar_In : OUT    UX01 ;
    Wrt_In   : OUT    UX01 ;
    Rd_In    : OUT    UX01 ;
    Lock_In  : OUT    UX01 ;
    LdSto_In : OUT    UX01 ;
    INull_In : OUT    UX01 ;
    WE_In_N  : OUT    UX01 ;
    DXfer    : IN     STD_Logic ;
    DXfer_In : OUT    UX01 ;
    Wrt      : IN     STD_Logic ;
    WE_N     : IN     STD_Logic;
    Clk_Out_Int  : IN     UX01 ) ;
END IU_In_Buffer ;


------------------------------------------------------------------
-- Architectural body of 'IU_In_Buffer'.
------------------------------------------------------------------

ARCHITECTURE IU_In_Buffer_body OF IU_In_Buffer IS
-- constant t4   : Time := 8 ns ;     -- Address Input Setup Time, ref edge Clk+
-- constant t5   : Time := 4 ns ;     -- Address Input Hold Time, ref edge Clk+
-- constant t6   : Time := 8 ns ;     -- ASI(7:0), Size(1:0), RD, WRT, WE*, LOCK,
                                      -- DMMAS Input Setup Time, ref edge Clk+
-- constant t7   : Time := 4 ns ;     -- ASI(7:0), Size(1:0), RD, WRT, WE*, LOCK,
                                      -- DMMAS Input Hold Time, ref edge Clk+
-- constant t33  : Time := 8 ns ;     -- DXfer Input Setup Time, ref edge Clk+
-- constant t34  : Time := 2 ns ;     -- DXfer Input Hold Time, ref edge Clk+
-- constant t35  : Time := 8 ns ;     -- INull Input Setup Time, ref edge Clk+
-- constant t36  : Time := 4 ns ;     -- INull Input Hold Time, ref edge Clk+
-- constant t53  : Time := 8 ns ;     -- DParI, DParIO, APar, ASPar, IMPar
                                      -- Input Setup Time, ref edge Clk+
-- constant t54  : Time := 2 ns ;     -- DParI, DParIO, APar, ASPar, IMPar
                                      -- Input Hold Time, ref edge Clk+
  
BEGIN
-- Setup and holdtime checkers to be added

           A_In        <= To_UX01(A);

-- SetupHoldCheckA 
	SetupHoldCheck(	Data => A,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP =>  t4, HOLD => t5,
			PATH => "Adrress SetupHold Violation",
			DelayedData => A);
------------------------------------------------------------------

           APar_In     <= To_UX01(APar);

-- SetupHoldCheckAPar 
	SetupHoldCheck(	Data => APar,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP =>  t53, HOLD => t54,
			PATH => "APar SetupHold Violation",
			DelayedData => APar);
------------------------------------------------------------------

           ASI_In      <= To_UX01(ASI);

-- SetupHoldCheckASI 
	SetupHoldCheck(	Data => ASI,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP =>  t6, HOLD => t7,
			PATH => "ASI SetupHold Violation",
			DelayedData => ASI);
------------------------------------------------------------------
 
           Size_In     <= To_UX01(Size);

-- SetupHoldCheckSize 
	SetupHoldCheck(	Data => Size,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP =>  t6, HOLD => t7,
			PATH => "Size SetupHold Violation",
			DelayedData => Size);
------------------------------------------------------------------

           ASPar_In    <= To_UX01(ASPar);

-- SetupHoldCheckASPar 
	SetupHoldCheck(	Data => ASPar,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP =>  t53, HOLD => t54,
			PATH => "ASPar SetupHold Violation",
			DelayedData => ASPar);
------------------------------------------------------------------

           INull_In    <= To_UX01(INull);

-- SetupHoldCheckINull 
	SetupHoldCheck(	Data => INull,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP =>  t35, HOLD => t36,
			PATH => "INull SetupHold Violation",
			DelayedData => INull);
------------------------------------------------------------------

           DXfer_In    <= To_UX01(DXFer);

-- SetupHoldCheckDXfer 
	SetupHoldCheck(	Data => DXfer,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP =>  t33, HOLD => t34,
			PATH => "DXfer SetupHold Violation",
			DelayedData => DXfer);
------------------------------------------------------------------

          LdSto_In    <= To_UX01(LdSto);

-- SetupHoldCheckLdSto 
	SetupHoldCheck(	Data => LdSto,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP =>  t6, HOLD => t7,
			PATH => "LdSto SetupHold Violation",
			DelayedData => LdSto);
------------------------------------------------------------------

           Lock_In     <= To_UX01(Lock);

-- SetupHoldCheckLock 
	SetupHoldCheck(	Data => Lock,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP =>  t6, HOLD => t7,
			PATH => "Lock SetupHold Violation",
			DelayedData => Lock);
------------------------------------------------------------------

           Rd_In       <= To_UX01(Rd);

-- SetupHoldCheckRd
	SetupHoldCheck(	Data => Rd,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP =>  t6, HOLD => t7,
			PATH => "Rd SetupHold Violation",
			DelayedData => Rd);
------------------------------------------------------------------

           WE_In_N     <= To_UX01(WE_N);

-- SetupHoldCheckWE 
	SetupHoldCheck(	Data => WE_N,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP =>  t6, HOLD => t7,
			PATH => "WE_N SetupHold Violation",
			DelayedData => WE_N);
------------------------------------------------------------------

           Wrt_In      <= To_UX01(Wrt);

-- SetupHoldCheckWrt 
	SetupHoldCheck(	Data => Wrt,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP =>  t6, HOLD => t7,
			PATH => "Wrt SetupHold Violation",
			DelayedData => Wrt);
------------------------------------------------------------------

           IMPar_In    <= To_UX01(IMPar);

-- SetupHoldCheckIMPar 
	SetupHoldCheck(	Data => IMPar,
			Ref => Clk_Out_Int,
			EDGE => RISING,
			SETUP =>  t53, HOLD => t54,
			PATH => "IMPar SetupHold Violation",
			DelayedData => IMPar);
------------------------------------------------------------------

END IU_In_Buffer_body ;
---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  Saab Ericsson Space AB and it must not be disclosed, copied, altered --
--  or used without the written permission of Saab Ericsson Space AB.    --
---------------------------------------------------------------------------
-- Title:                      MEC IO Buffer
-- File name:                  \mec\source\IO_Buffe.vhd
-- VHDL unit:                  IO_buffer
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing and environmental
--                             textio inputs included. It is used as a
--                             comparator selector and it is used to avoid
--                             visibility of the timing at top level.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--  SAAB ERICSSON SPACE AB                                               --
--  Delsjomotet                      Phone Int: +46 31 35 00 00          --
--  S-405 15 GOTHENBURG              Fax   Int: +46 31 35 95 20          --
--  Sweden                           Telex:     27950 saabsp s           --
---------------------------------------------------------------------------
--
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdTiming.all;

------------------------------------------------------------------
-- Entity declaration of 'IO_Buffer'.
------------------------------------------------------------------

ENTITY IO_Buffer IS
  GENERIC(
    t13   : Time := 8 ns ;
    t15   : Time := 8 ns ;
    t18   : Time := 30 ns ;
    t9    : Time := 15 ns ;
    t10   : Time := 5 ns ;
    t11   : Time := 1 ns ;
    t12   : Time := 8 ns ;
    t74   : Time := 4 ns ;
    t75   : Time := 4 ns ;
    CHECK : Boolean := False ) ;
  PORT(
    ExMCS_N       : OUT    Std_Logic ;
    IOBEn_N       : OUT    Std_Logic ;
    IOWR_N        : OUT    Std_Logic ;
    IOSel_N       : OUT    Std_Logic_Vector(3 downto 0) ;
    ExMCS_Out_N   : IN     UX01 ;
    IOBEn_Out_N   : IN     UX01 ;
    IOWR_Out_N    : IN     UX01 ;
    IOSel_Out_N   : IN     Std_Logic_Vector(3 downto 0) ;
    A_In          : IN     Std_Logic_Vector(31 downto 0) ;
    Clk_Out_Int   : IN     UX01 ;
    Clk_Out       : IN     UX01 ) ;
END IO_Buffer ;

------------------------------------------------------------------
-- Architectural body of 'IO_Buffer'.
------------------------------------------------------------------

ARCHITECTURE IO_Buffer_body OF IO_Buffer IS
BEGIN

-- Setup and Hold time violation checks added
------------------------------------------------------------------


IOSignalProcess: Process(IOSel_Out_N, IOWR_Out_N, IOBen_Out_N,
                         ExMCS_Out_N)
begin

  IOSel_N        <= IOSel_OUT_N after t18;
  IOWr_N         <= IOWr_OUT_N after  t15;
  IOBEn_N        <= IOBEn_OUT_N after t13;
  ExMCS_N        <= ExMCS_OUT_N after t18;

end process; -- IOSignalProcess


END IO_Buffer_body ;

---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  Saab Ericsson Space AB and it must not be disclosed, copied, altered --
--  or used without the written permission of Saab Ericsson Space AB.    --
---------------------------------------------------------------------------
-- Title:                      MEC External Interrupt Buffer
-- File name:                  \mec\source\EXTinT_B.vhd
-- VHDL unit:                  ExternalInterrupt_buffer
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing and environmental
--                             textio inputs included. It is used as a 
--                             comparator selector and it is used to avoid
--                             visibility of the timing at top level.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--  SAAB ERICSSON SPACE AB                                               --
--  Delsjomotet                      Phone Int: +46 31 35 00 00          --
--  S-405 15 GOTHENBURG              Fax   Int: +46 31 35 95 20          --
--  Sweden                           Telex:     27950 saabsp s           --
---------------------------------------------------------------------------
--
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdTiming.all;

------------------------------------------------------------------
-- Entity declaration of 'ExternalInterrupt_Buffer'.
------------------------------------------------------------------

ENTITY ExternalInterrupt_Buffer IS
  GENERIC(
    t44   : Time := 8 ns ;
    t45   : Time := 4 ns ;
    t43   : Time := 8 ns ;
    t74   : Time := 4 ns ;
    t75   : Time := 4 ns ;
    CHECK : Boolean := False ) ;
  PORT(
    ExtInt           : in     Std_Logic_Vector(4 DOWNTO 0) ;
    ExtIntAck        : out    Std_Logic ;
    ExtInt_In        : out    Std_Logic_Vector(4 DOWNTO 0) ;
    ExtIntAck_Out    : in     UX01 ;
    Clk_Out_Int      : in     UX01;
    Clk_Out          : in     UX01 ) ;
END ExternalInterrupt_Buffer ;

------------------------------------------------------------------
-- Architectural body of 'ExternalInterrupt_Buffer'.
------------------------------------------------------------------

ARCHITECTURE ExternalInterrupt_Buffer_body OF ExternalInterrupt_Buffer IS

   SIGNAL ExtInt_Int      : Std_Logic_Vector(4 DOWNTO 0) := "11111" ;

 signal    TPData_Internal      : Time := 0 ns;
 signal    TPDataPar_Internal   : Time := 0 ns;

BEGin
-- Setup, Hold time and pulse width violation checks added
------------------------------------------------------------------

           ExtInt_In   <= To_UX01(ExtInt);
           ExtInt_Int  <= To_UX01(ExtInt);

-- SetupHoldCheckExtInt 
	SetupHoldCheck(	Data => ExtInt_Int,
			Ref => Clk_Out,
			EDGE => RISinG,
			SETUP => t44, HOLD => t45,
			PATH => "ExtInt SetupHold Violation",
			DelayedData => ExtInt);
------------------------------------------------------------------


ExtinTProcess: Process(ExtIntAck_Out)
begin
   ExtIntAck         <= ExtIntAck_Out after t43;
end process; -- ExtinTProcess

END ExternalInterrupt_Buffer_body ;

---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  Saab Ericsson Space AB and it must not be disclosed, copied, altered --
--  or used without the written permission of Saab Ericsson Space AB.    --
---------------------------------------------------------------------------
-- Title:                      MEC DMA Buffer
-- File name:                  \mec\source\DMA_Buff.vhd
-- VHDL unit:                  DMA_buffer
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing and environmental
--                             textio inputs included. It is used as a 
--                             comparator selector and it is used to avoid
--                             visibility of the timing at top level.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--  SAAB ERICSSON SPACE AB                                               --
--  Delsjomotet                      Phone Int: +46 31 35 00 00          --
--  S-405 15 GOTHENBURG              Fax   Int: +46 31 35 95 20          --
--  Sweden                           Telex:     27950 saabsp s           --
---------------------------------------------------------------------------
--
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdTiming.all;
------------------------------------------------------------------
-- Entity declaration of 'DMA_Buffer'.
------------------------------------------------------------------

ENTITY DMA_Buffer IS
  GENERIC(
    t40   : Time := 15 ns ;
    t42   : Time := 4 ns ;
    t6    : Time := 8 ns ;
    t7    : Time := 4 ns ;
    t27   : Time := 8 ns ; 
    t38   : Time := 15 ns ;
    t39   : Time := 4 ns ;
    t37   : Time := 8 ns ;
    t68   : Time := 22 ns ;
    t74   : Time := 4 ns ;
    t75   : Time := 4 ns ;
    CHECK : Boolean := False ) ;
  PORT(
    BusRdy_In_N    : Out    UX01 ;
    BusErr_In_N    : Out    UX01 ;
    DMAAS_In       : Out    UX01 ;
    DRdy_Out_N     : in     UX01 ;
    DMAGnt_Out_N   : in     UX01 ;
    DMAReq_In_N    : Out    UX01 ;
    DMAAS          : in     Std_Logic ;
    DMAReq_N       : in     Std_Logic ;
    BusRdy_N       : in     Std_Logic ;
    BusErr_N       : in     Std_Logic ;
    DMAGnt_N       : Out    Std_Logic ;
    DRdy_N         : Out    Std_Logic ;
    Clk_Out_Int    : in     UX01 ;
    Clk_Out        : in     UX01 ;
    ExtHold_N      : in     Std_Logic ;
    ExtHold_In_N   : Out    UX01 ;
    ExtCCV         : in     Std_Logic ;
    ExtCCV_In      : Out    UX01 ) ;

END DMA_Buffer ;


------------------------------------------------------------------
-- Architectural body of 'DMA_Buffer'.
------------------------------------------------------------------

ARCHITECTURE DMA_Buffer_body OF DMA_Buffer IS

   signal TPDRDY_Internal    : Time := 0 ns;
   signal TPDMAGnt_Internal  : Time := 0 ns;

Begin

 DMAAS_In     <= To_UX01(DMAAS);

-- SetupHoldCheckDMAAS 
	SetupHoldCheck(	Data => DMAAS,
			Ref => Clk_Out_Int,
			EDGE => RISinG,
			SETUP =>  t74, HOLD => t75,
			PATH => "DMAAS SetupHold Violation",
			DelayedData => DMAAS);
			
	SetupHoldCheck(	Data => DMAAS,
			Ref => Clk_Out_Int,
			EDGE => FallinG,
			SETUP =>  t74, HOLD => t75,
			PATH => "DMAAS SetupHold Violation",
			DelayedData => DMAAS);
------------------------------------------------------------------
           BusRdy_In_N  <= To_UX01(BusRdy_N);

-- SetupHoldCheckBusRdy 
	SetupHoldCheck(	Data => BusRdy_N,
			Ref => Clk_Out_Int,
			EDGE => RISinG,
			SETUP =>  t38, HOLD => t39,
			PATH => "BusRdy_N SetupHold Violation",
			DelayedData => BusRdy_N);
------------------------------------------------------------------
           BusErr_In_N  <= To_UX01(BusErr_N);

-- SetupHoldCheckBusErr 
	SetupHoldCheck(	Data => BusErr_N,
			Ref => Clk_Out_Int,
			EDGE => RISinG,
			SETUP =>  t38, HOLD => t39,
			PATH => "BusErr_N SetupHold Violation",
			DelayedData => BusErr_N);
------------------------------------------------------------------
      DMAReq_In_N  <= To_UX01(DMAReq_N);

-- SetupHoldCheckDMAReq 
	SetupHoldCheck(	Data => DMAReq_N,
			Ref => Clk_Out_Int,
			EDGE => RISinG,
			SETUP =>  t40, HOLD => 0 ns,
			PATH => "DMAReq_N SetupHold Violation",
			DelayedData => DMAReq_N);
------------------------------------------------------------------
           ExtHold_In_N <= To_UX01(ExtHold_N);

-- SetupHoldCheckExtHold_N 
	SetupHoldCheck(	Data => ExtHold_N,
			Ref => Clk_Out_Int,
			EDGE => FALLinG,
			SETUP =>  t68, HOLD => 0 ns,
			PATH => "ExtHold_N SetupHold Violation",
			DelayedData => ExtHold_N);
------------------------------------------------------------------
           ExtCCV_In   <= To_UX01(ExtCCV);

-- SetupHoldCheckExtCCV 
	SetupHoldCheck(	Data => ExtCCV,
			Ref => Clk_Out_Int,
			EDGE => FALLinG,
			SETUP =>  t68, HOLD => 0 ns,
			PATH => "ExtCCV SetupHold Violation",
			DelayedData => ExtCCV);
------------------------------------------------------------------
 

DMASignalProcess: Process(DRdy_Out_N, DMAGnt_Out_N)
begin
 
    DRDY_N          <= DRDY_Out_N    after t37;
    DMAGnt_N        <= DMAGnt_Out_N  after t42;

end process; -- DMASignalProcess

END DMA_Buffer_body ;
---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  Saab Ericsson Space AB and it must not be disclosed, copied, altered --
--  or used without the written permission of Saab Ericsson Space AB.    --
---------------------------------------------------------------------------
-- Title:                      MEC Data Buffer
-- File name:                  \mec\source\data_buf.vhd
-- VHDL unit:                  Data_buffer
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing and environmental
--                             textio inputs included. It acts as a tristate 
--                             buffer controlled by MEC func, as a comparator
--                             selector and is used to avoid visibility of 
--                             the timing at top level.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Ver. No:  Author:            Date:       Changes made:                --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--  SAAB ERICSSON SPACE AB                                               --
--  Delsjomotet                      Phone Int: +46 31 35 00 00          --
--  S-405 15 GOTHENBURG              Fax   Int: +46 31 35 95 20          --
--  Sweden                           Telex:     27950 saabsp s           --
---------------------------------------------------------------------------
--
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdTiming.all;
use MMS.StdIoImp.all;
use MMS.StdSim.all;

------------------------------------------------------------------
-- Entity declaration of 'Data_Buffer'.
------------------------------------------------------------------

ENTITY Data_Buffer IS
  GENERIC(
    t19   : Time := 35 ns ;
    t20   : Time := 1 ns ;
    t21   : Time := 10 ns ;
    t22   : Time := 4 ns ;
    t66   : Time := 20 ns ;
    t51   : Time := 8 ns ;
    t53   : Time := 8 ns ;
    t54   : Time := 2 ns ;
    t71   : Time := 4 ns ;
    t73   : Time := 35 ns ;
    CHECK : Boolean := False ) ;
  PORT(
    DParIO          : INOUT  STD_Logic ;
    D               : INOUT  STD_Logic_Vector(31 DOWNTO 0) ;
    DParIO_Out      : IN     UX01 ;
    D_Out           : IN     STD_Logic_Vector(31 DOWNTO 0) ;
    DParIO_In       : OUT    UX01 ;
    D_In            : OUT    STD_Logic_Vector(31 DOWNTO 0) ;
    DBEn_N          : IN     UX01 ;
    DParIOEn_N      : IN     UX01 ;
    Clk_Out_Int     : IN     UX01 ;
    Clk_Out         : IN     UX01 ;
    ParityEnable    : IN     UX01 ;
    Reset_Out_N     : IN     UX01 ;
    EDACEnable      : IN     UX01 ;
    MemData_Valid   : IN     UX01 ;
    MHOLD_N         : IN     UX01 ;
    Wr_Int_N        : IN     UX01 ;
    MECRegister_Access : in  UX01;   -- Added 941012
    RomCS_Out_N : in  UX01   -- Added 961009 by J.Gaisler to improve DPAR timing
    );
END Data_Buffer ;

------------------------------------------------------------------
-- Architectural body of 'Data_Buffer'.
------------------------------------------------------------------

ARCHITECTURE Data_Buffer_body OF Data_Buffer IS


-- constant t19  : Time := 25 ns ;    -- Data Write Setup Time to generate parity from Data bus 
                                      -- before CLK-(Store MEC), ref edge Clk-
-- constant t20  : Time := 1 ns ;     -- Data Hold Time, ref edge Clk+
-- constant t21  : Time := 10 ns ;    -- MEC Data Output Delay Time, ref edge Clk+
-- constant t22  : Time := 4 ns ;     -- MEC Data Hold Time, ref edge Clk+
-- constant t51  : Time := 8 ns ;     -- DParI to DPARIO Output Internal Delay Time, 
                                      -- ref DParI Valid
-- constant t53  : Time := 8 ns ;     -- DParI, DParIO, APar, ASPar, IMPar
                                      -- Input Setup Time, ref edge Clk+
-- constant t54  : Time := 2 ns ;     -- DParI, DParIO, APar, ASPar, IMPar
                                      -- Input Hold Time, ref edge Clk+
-- constant t66  : Time := 20 ns ;    -- DPARIO Generation Time (Including Output
                                      -- Delay Time), ref Data Valid
-- constant t71  : Time := 4 ns ;     -- Data read setup time for latches, ref edge Clk+
-- constant t73  : Time := 35 ns ;    -- Data read setup time for checking EDAC checkbits and 
                                      -- generating MHOLD before next Clk-, ref edge Clk+
-- Local signals to avoid buffer assignmemt
 signal    D_Int       : Std_Logic_Vector(31 downto 0) := "00000000000000000000000000000000";
 signal    DParIO_Int  : UX01   := '1';

 signal    ParityEnable_Int     : UX01;
 signal    EDACEnable_Int       : UX01; 
 signal    Clk_Half_Period : Time := 20 ns; -- Clk = 25 MHz
 signal    Wr_Int_Happend : UX01 := '0';

BEGIN
-- Assure that ParityEnable and EDACEnable do not change during rising_edge(Clk_Out_Int)
  ParityEnable_Int <= ParityEnable after Clk_Half_Period;
  EDACEnable_Int   <= EDACEnable after Clk_Half_Period;

-- Setup and Hold time violation checks added
------------------------------------------------------------------

           D_IN        <= To_UX01(D);
           D_Int       <= To_UX01(D);

           DParIO_IN   <= To_UX01(DParIO);
           DParIO_Int  <= To_UX01(DParIO);

------------------------------------------------------------------

DataSignalprocess:Process(D_Out, DParIO_Out, DBEn_N, DParIOEn_N,MECRegister_Access)
begin 

  if DBEn_N = '0' then
     D                <= D_Out after t21;
   else
     D                <= (others => 'Z') after t22;
  end if;

  if ((DBEn_N = '0') or (DParIOEn_N = '0')) then

     if MECRegister_Access = '1' or ParityEnable_Int = '1' or RomCS_Out_N = '0' 
     then
       DParIO           <= transport DParIO_Out after t21;
     else
       DParIO           <= transport DParIO_Out after t66;
     end if;

   else 

     DParIO           <= transport 'Z' after t22;

  end if;

end process; -- DataSignalProcess


DataSetupHoldCheck : process
  variable LastEdge, DeltaT, D_Hold_Time, D_Setup_Time : time := 0 ns;
begin
  wait on D,Clk_Out_Int;
  if (not CHECK_ON) then 
    wait;
  end if;
  -- Setup checking
  if rising_edge(Clk_Out_Int) and (MemData_Valid = '1') then -- Check read data setup
    LastEdge := now;
    D_Setup_Time := t71;
    D_Hold_Time := MaxTime(t20,t22);
    DeltaT := D'last_event;
    assert not(DeltaT <= D_Setup_Time) 
      report "Setup violation on " &  "D Read Setup Violation" & " at time " & 
       ToString(now) & ". Observed : " & ToString(DeltaT) & ". Setup : " & ToString(D_Setup_Time)
      severity warning;
  elsif falling_edge(Clk_Out_Int) and ( 
      ( (Wr_Int_N = '0') and (MHOLD_N = '1') ) or    -- 0 waitstate
      ( (Wr_Int_Happend = '1') and (MHOLD_N = '1'))) then -- 1 -> waitstates -- Check write data setup
      D_Setup_Time := t19;
    LastEdge := now;
    D_Hold_Time := t20;
    DeltaT := D'last_event;
    assert not(DeltaT <= D_Setup_Time) 
      report "Setup violation on " &  "D Write Setup Violation" & " at time " & 
       ToString(now) & ". Observed : " & ToString(DeltaT) & ". Setup : " & ToString(D_Setup_Time)
      severity warning;
  end if;

  -- Hold checking
  if D'event then
    DeltaT := now - LastEdge;
    assert not(DeltaT <= D_Hold_Time)
     report "Hold violation on " & "D Hold Violation" & " at time " & ToString(now) &
            ". Observed : " & ToString(DeltaT) & ". Hold : " & ToString(D_Hold_Time)
     severity warning;
  end if;

end process; -- DataSetupHoldCheck

Wr_Happend:process
begin
  wait until falling_edge(Clk_Out_Int);
  if (Wr_Int_N = '0') and (MHOLD_N = '0') then -- write access with waitstates
    -- assert after 5 ns in order to avoid action in this cycle
    Wr_Int_Happend <= '1' after 5 ns;                                                                   
  elsif (Wr_Int_Happend = '1') and (MHOLD_N = '1') then -- last cycle in access with waitstates
    -- assert after 5 ns in order to avoid action in this cycle
    Wr_Int_Happend <= '0' after 5 ns;
  end if;
end process;

DPARIOSetupHoldCheck : process
  variable LastEdge, DeltaT, DPARIO_Hold_Time, DPARIO_Setup_Time : time := 0 ns;
begin
  wait on DPARIO,Clk_Out_Int;
  if (not CHECK_ON) then 
    wait;
  end if;
  -- Setup checking
  if rising_edge(Clk_Out_Int) and (MemData_Valid = '1') then -- Check read data setup
    LastEdge := now;
    DPARIO_Setup_Time := t71;
    DPARIO_Hold_Time := MaxTime(t20,t22);
    DeltaT := DPARIO'last_event;
    assert not(DeltaT < DPARIO_Setup_Time) 
      report "Setup violation on " &  "DPARIO Read Setup Violation" & " at time " & 
       ToString(now) & ". Observed : " & ToString(DeltaT) & ". Setup : " & ToString(DPARIO_Setup_Time)
      severity warning;
  -- Setup checking
  elsif falling_edge(Clk_Out_Int) and (
     ((Wr_Int_N = '0') and (MHOLD_N = '1') ) or    -- 0 waitstate
     ((Wr_Int_Happend = '1') and (MHOLD_N = '1'))) then -- 1 -> waitstates -- Check write DPARIO setup
      DPARIO_Setup_Time := t19;
      DPARIO_Hold_Time  := t20;
    LastEdge := now;
    DeltaT := DPARIO'last_event;
    assert not(DeltaT < DPARIO_Setup_Time) 
      report "Setup violation on " &  "DPARIO Write Setup Violation" & " at time " & 
       ToString(now) & ". Observed : " & ToString(DeltaT) & ". Setup : " & ToString(DPARIO_Setup_Time)
      severity warning;
  end if;

  -- Hold checking
  if DPARIO'event then
    DeltaT := now - LastEdge;
    assert not(DeltaT < DPARIO_Hold_Time)
     report "Hold violation on " & "DPARIO Hold Violation" & " at time " & ToString(now) &
            ". Observed : " & ToString(DeltaT) & ". Hold : " & ToString(DPARIO_Hold_Time)
     severity warning;
  end if;

end process; -- DPARIOSetupHoldCheck

GetClockTime: process
  variable edge : time := 0 ns;
begin
  wait until Reset_Out_N = '0';
  wait until Reset_Out_N = '1';
  wait until Clk_Out_Int = '1';
  edge := now;
  wait until Clk_Out_Int = '0';
  Clk_half_period <= now - edge;
end process;

END Data_Buffer_body ;
---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  Saab Ericsson Space AB and it must not be disclosed, copied, altered --
--  or used without the written permission of Saab Ericsson Space AB.    --
---------------------------------------------------------------------------
-- Title:                      MEC Clock Buffer
-- File name:                  \mec\source\clk_buff.vhd
-- VHDL unit:                  Clk_buffer
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing and environmental
--                             textio inputs included. The buffer is used to 
--                             avoid visibility on the timing at top level.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--  SAAB ERICSSON SPACE AB                                               --
--  Delsjomotet                      Phone Int: +46 31 35 00 00          --
--  S-405 15 GOTHENBURG              Fax   Int: +46 31 35 95 20          --
--  Sweden                           Telex:     27950 saabsp s           --
---------------------------------------------------------------------------
--
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdTiming.all;

------------------------------------------------------------------
-- Entity declaration of 'Clk_buffer'.
------------------------------------------------------------------

ENTITY Clk_buffer IS
  GENERIC(
    CHECK : Boolean := False ;
    t2    : Time := 10 ns ;
    t3    : Time := 22 ns ;
    t63   : Time := 1000 ns ) ;
  PORT(
    WDClk    : IN     STD_LOGIC ;
    Clk2     : IN     STD_LOGIC ;
    Clk2_In  : OUT    UX01 ;
    WDClk_In : OUT    UX01 ) ;
END Clk_buffer ;

------------------------------------------------------------------
-- Architectural body of 'Clk_buffer'.
------------------------------------------------------------------

ARCHITECTURE Clk_buffer_body OF Clk_buffer IS
-- constant    t2  : Time := 10 ns ;    -- Clk2 high to low/ low to high
-- constant    t3  : Time := 22 ns ;    -- Clk2 period time 2-44 MHz in steps of 2MHz
-- constant    t63 : Time := 1000 ns ;   -- WDClk Clock Period time

constant Clk2PWH : time := (t3)/2;
constant Clk2PWL : time := (t3)/2;
constant Clk2PWU : time := 1 ns; -- Default to allow check
constant Clk2PWX : time := 1 ns; -- Default to allow check
constant WDPWH   : time := (t63)/2;
constant WDPWL   : time := (t63)/2;
constant WDPWU   : time := 1 ns; -- Default to allow check
constant WDPWX   : time := 1 ns; -- Default to allow check

-- Local signals for pulse width checking
signal   Clk2_Int  : UX01;
signal   WDClk_Int : UX01;

begin

-- Pulse width violation checks added
------------------------------------------------------------------

        Clk2_In     <= To_UX01(Clk2);
        Clk2_Int    <= To_UX01(Clk2);

-- PulsecheckClk2	
        PulseCheck(     Pin => Clk2_Int,
			LEVEL => '1',
			WIDTH => Clk2PWH,
			SENSE => MINIMUM,
			PATH => "Clk2PWH-MIN VIOLATION");
	
        PulseCHECK(     Pin => Clk2_Int,
			LEVEL => '0',
			WIDTH => Clk2PWL,
			SENSE => MINIMUM,
			PATH => "Clk2PWL-MIN VIOLATION");

        PulseCHECK(     Pin => Clk2_Int,
			LEVEL => 'U',
			WIDTH => Clk2PWU,
			SENSE => MAXIMUM,
			PATH => "Clk2 NOT DRIVEN VIOLATION");
	
        PulseCHECK(     Pin => Clk2_Int,
			LEVEL => 'X',
			WIDTH => Clk2PWX,
			SENSE => MAXIMUM,
			PATH => "Clk DRIVEN X VIOLATION");
------------------------------------------------------------------
        WDClk_In    <= To_UX01(WDClk);
        WDClk_Int   <= To_UX01(WDClk);

-- PulsecheckWD 
        PulseCHECK(     Pin => WDClk_Int,
			LEVEL => '1',
			WIDTH => Clk2PWH,
			SENSE => MINIMUM,
			PATH => "WDPWH-MIN VIOLATION");

        PulseCHECK(     Pin => WDClk_Int,
			LEVEL => '1',
			WIDTH => WDPWH,
			SENSE => MAXIMUM,
			PATH => "WDPWH-MAX VIOLATION");
	
        PulseCHECK(     Pin => WDClk_Int,
			LEVEL => '0',
			WIDTH => Clk2PWL,
			SENSE => MINIMUM,
			PATH => "WDPWL-MIN VIOLATION");

        PulseCHECK(     Pin => WDClk_Int,
			LEVEL => '0',
			WIDTH => WDPWL,
			SENSE => MAXIMUM,
			PATH => "WDPWL-MAX VIOLATION");

        PulseCHECK(     Pin => WDClk_Int,
			LEVEL => 'U',
			WIDTH => Clk2PWU,
			SENSE => MINIMUM,
			PATH => "WDClk NOT DRIVEN VIOLATION");

        PulseCHECK(     Pin => WDClk_Int,
			LEVEL => 'U',
			WIDTH => WDPWU,
			SENSE => MAXIMUM,
			PATH => "WDClk NOT DRIVEN VIOLATION");
	
        PulseCHECK(     Pin => WDClk_Int,
			LEVEL => 'X',
			WIDTH => Clk2PWX,
			SENSE => MAXIMUM,
			PATH => "WDClk DRIVEN X VIOLATION");

        PulseCHECK(     Pin => WDClk_Int,
			LEVEL => 'X',
			WIDTH => WDPWX,
			SENSE => MAXIMUM,
			PATH => "WDClk DRIVEN X VIOLATION");
END Clk_buffer_body ;
---------------------------------------------------------------------------
--                Copyright SAAB ERICSSON SPACE AB                       --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  Saab Ericsson Space AB and it must not be disclosed, copied, altered --
--  or used without the written permission of Saab Ericsson Space AB.    --
---------------------------------------------------------------------------
-- Title:                      MEC Checkbit Buffer
-- File name:                  \mec\source\checkbit.vhd
-- VHDL unit:                  Checkbits_buffer
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing and environmental
--                             textio inputs included. It acts  as a tristate 
--                             buffer controlled by MEC func, as a comparator
--                             selector and is used to avoid visibility of 
--                             the timing at top level.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--  SAAB ERICSSON SPACE AB                                               --
--  Delsjomotet                      Phone Int: +46 31 35 00 00          --
--  S-405 15 GOTHENBURG              Fax   Int: +46 31 35 95 20          --
--  Sweden                           Telex:     27950 saabsp s           --
---------------------------------------------------------------------------
--
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdTiming.all;
use MMS.StdIoImp.all;
use MMS.StdSim.all;

------------------------------------------------------------------
-- Entity declaration of 'Checkbits_Buffer'.
------------------------------------------------------------------

ENTITY Checkbits_Buffer IS
  GENERIC(
    t20   : Time := 1 ns ;
    t23   : Time := 2 ns ;
    t24   : Time := 2 ns ;
    t71   : Time := 12 ns ;
    CHECK : Boolean := False) ;
  PORT(
    CB_Out        : IN     STD_Logic_Vector(6 DOWNTO 0) ;
    CB_In         : OUT    STD_Logic_Vector(6 DOWNTO 0) ;
    CB            : INOUT  STD_Logic_Vector(6 DOWNTO 0) ;
    CBEn_N        : IN     UX01 ;
    Clk_Out_Int   : IN     UX01  ;
    Clk_Out       : IN     UX01  ;
    Reset_Out_N   : IN     UX01 ;
    EDACEnable    : IN     UX01 ;
    ParityEnable  : IN     UX01 ;
    MemData_Valid : IN     UX01 );
END Checkbits_Buffer ;


------------------------------------------------------------------
-- Architectural body of 'Checkbits_Buffer'.
------------------------------------------------------------------

ARCHITECTURE Checkbits_Buffer_body OF Checkbits_Buffer IS


  signal CB_Int           : Std_Logic_Vector(6 downto 0):= "0000000";
  signal EDACEnable_Int   : UX01; 
  signal Clk_Half_Period  : Time := 20 ns;

BEGIN
-- Assure that ParityEnable and EDACEnable do not change during rising_edge(Clk_Out_Int)
  EDACEnable_Int   <= EDACEnable after Clk_Half_Period;

-- Setup and Hold time violation checks added
------------------------------------------------------------------
           CB_In       <= To_UX01(CB);
           CB_Int      <= To_UX01(CB);
------------------------------------------------------------------

  
CBSignalprocess:Process(CB_Out, CBEn_N)
begin
 
  if CBEn_N = '1' then
    CB  <= transport (others => 'Z') after t24;
  else
    CB  <= transport CB_OUT after t23;
  end if;
end process; --CBSignalProcess


CBSetupHoldCheck : process
  variable LastEdge, DeltaT, CB_Hold_Time, CB_Setup_Time : time := 0 ns;
begin
  wait on CB,Clk_Out_Int;
  if (not CHECK_ON) then 
    wait;
  end if;
  -- Setup checking
  if rising_edge(Clk_Out_Int) and (MemData_Valid = '1') then -- Check read data setup
    if EDACEnable_Int = '1' then      -- Time for generate MHOLD before next Clk-
      CB_Setup_Time := t71;
      CB_Hold_Time := t20;
    else                             -- With no EDAC Checking CB signals are ignored
      CB_Setup_Time := 0 ns;
      CB_Hold_Time :=  0 ns;
    end if;
    LastEdge := now;
    DeltaT := CB'last_event;
    assert not(DeltaT <= CB_Setup_Time) 
      report "Setup violation on " &  "CB Read Setup Violation" & " at time " & 
       ToString(now) & ". Observed : " & ToString(DeltaT) & ". Setup : " & ToString(CB_Setup_Time)
      severity warning;
  end if;

  -- Hold checking
  if CB'event then
    DeltaT := now - LastEdge;
    assert not(DeltaT <= CB_Hold_Time)
     report "Hold violation on " & "CB Hold Violation" & " at time " & ToString(now) &
            ". Observed : " & ToString(DeltaT) & ". Hold : " & ToString(CB_Hold_Time)
     severity warning;
  end if;

end process; -- CBSetupHoldCheck



GetClockTime: process
  variable edge : time := 0 ns;
begin
  wait until Reset_Out_N = '0';
  wait until Reset_Out_N = '1';
  wait until Clk_Out_Int = '1';
  edge := now;
  wait until Clk_Out_Int = '0';
  Clk_half_period <= now - edge;
end process;

END Checkbits_Buffer_body ;
---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      MEC Generic component
-- File name:                  \mec\source\mecgen.vhd
-- VHDL unit:                  MEmoryController
-- Purpose and functionality:  This unit is a one to one mapping of the top
--                             level, with the timing generics.
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdSim.all;
use MMS.StdTiming.all;
use MMS.StdIoImp.all;

------------------------------------------------------------------
-- entity declaration of 'MECGEN'.
------------------------------------------------------------------

entity MECGEN is
  generic(
    CHECK : Boolean := False ;
    t1   : Time := 44 ns ;    -- Clk period time
    t2   : Time := 10 ns ;    -- Clk2 high to low/ low to high
    t3   : Time := 22 ns ;    -- Clk2 period time
    t4   : Time := 8 ns ;     -- Address Input Setup Time, ref edge Clk+
    t5   : Time := 4 ns ;     -- Address Input Hold Time, ref edge Clk+
    t6   : Time := 8 ns ;     -- ASI(7:0), Size(1:0), RD, WRT, WE*, LOCK,
                              -- DMMAS Input Setup Time, ref edge Clk+
    t7   : Time := 4 ns ;     -- ASI(7:0), Size(1:0), RD, WRT, WE*, LOCK,
                              -- DMMAS Input Hold Time, ref edge Clk+
    t8   : Time := 8 ns ;     -- ALE_N, ref edge Clk-
    t9   : Time := 15 ns ;    -- MEC Chip Select Generation, ref Address Valid
    t10  : Time := 5 ns ;     -- MEC CS_N Clock to Output delay Time, ref edge Clk+
    t11  : Time := 1 ns ;     -- MEC CS_N Hold Time, ref edge Clk+
    t12  : Time := 8 ns ;     -- MEC CS_N Output Latch propagation delay Time, 
                              -- ref MEC CS_N Generated
    t13  : Time := 8 ns ;     -- xBEn_N Output Delay Time, ref Clk-
    t14  : Time := 8 ns ;     -- DDir Output Delay Time, ref Clk2-
    t15  : Time := 8 ns ;     -- xWR_N High to Low Output Delay, ref Clk-
    t16  : Time := 8 ns ;     -- OE_N Output Delay Time, ref Clk2
    t17  : Time := 8 ns ;     -- BA Valid, ref Clk+
    t18  : Time := 8 ns ;     -- IOSel Valid, ref Clk+
    t19  : Time := 25 ns ;    -- Data Write Setup Time to check parity on Data bus 
                              -- before CLK-(Store memory), ref edge Clk-
    t20  : Time := 1 ns ;     -- Data Hold Time, ref edge Clk+
    t21  : Time := 10 ns ;    -- MEC Data Output Delay Time, ref edge Clk+
    t22  : Time := 4 ns ;     -- MEC Data Hold Time, ref edge Clk+
    t23  : Time := 28 ns ;    -- CB Generation (Valid Data Latched on Clk+), ref edge Clk+
    t24  : Time := 1 ns ;     -- CB Hold Time, ref edge Clk+
    t25  : Time := 8 ns ;     -- MExc_N Output Delay Time, ref Clk-
    t26  : Time := 8 ns ;     -- xHold_N Output Delay Time, ref Clk+
    t27  : Time := 8 ns ;     -- AOE_N, COE_N, DOE_N, TOE_N Output Delay, ref Clk+
    t28  : Time := 8 ns ;     -- INST, INTACK Input Setup Time, ref edge Clk+
    t29  : Time := 4 ns ;     -- INST, INTACK Input Hold Time, ref edge Clk+
    t30  : Time := 15 ns ;    -- SysReset_N, SysHalt_N  Input Setup Time, ref edge Clk+
    t31  : Time := 4 ns ;     -- SysReset_N, SysHalt_N  Input Hold Time, ref edge Clk+
    t32  : Time := 4 ns ;     -- Reset_N, CPUHalt_N  Output Delay Time, ref edge Clk2
    t33  : Time := 8 ns ;     -- DXfer Input Setup Time, ref edge Clk+
    t34  : Time := 2 ns ;     -- DXfer Input Hold Time, ref edge Clk+
    t35  : Time := 8 ns ;     -- INull Input Setup Time, ref edge Clk+
    t36  : Time := 4 ns ;     -- INull Input Hold Time, ref edge Clk+
    t37  : Time := 8 ns ;     -- MDS_N, DRDY_N Output Delay Time, ref edge Clk+
    t38  : Time := 15 ns ;    -- BusRdy_N, BusErr_N Input Setup Time, ref edge Clk+
    t39  : Time := 4 ns ;     -- BusRdy_N, BusErr_N Input Hold Time, ref edge Clk+
    t40  : Time := 15 ns ;    -- DMAReq_N Input Setup Time, ref edge Clk-
    t42  : Time := 4 ns ;     -- DMAGnt_N Output Delay Time, ref edge Clk+
    t43  : Time := 8 ns ;     -- IRL(3:0), EXTIntack Output Delay Time, ref edge Clk-          
    t44  : Time := 8 ns ;     -- EXTInt(4:0) Input Setup Time, ref edge Clk+
    t45  : Time := 4 ns ;     -- EXTInt(4:0) Input Hold Time, ref edge Clk+
    t46  : Time := 8 ns ;     -- xERR_N Input Setup Time, ref edge Clk+
    t47  : Time := 4 ns ;     -- xERR_N Input Hold Time, ref edge Clk+
    t48  : Time := 8 ns ;     -- xERR_N Output Delay Time, ref edge Clk+
    t49  : Time := 8 ns ;     -- NoPar_N, RomWrt_N, Prom8_N
                              -- Input Setup Time, ref edge Clk+     
    t50  : Time := 4 ns ;     -- NoPar_N, RomWrt_N, Prom8_N
                              -- Input Hold Time, ref edge Clk+
    t51  : Time := 8 ns ;     -- DParI to DPARIO Output Internal Delay Time, 
                              -- ref DParI Valid
    t52  : Time := 4 ns ;     -- DPARIO Output Delay Time, ref edge Clk+
    t53  : Time := 8 ns ;     -- DParI, DParIO, APar, ASPar, IMPar
                              -- Input Setup Time, ref edge Clk+
    t54  : Time := 2 ns ;     -- DParI, DParIO, APar, ASPar, IMPar
                              -- Input Hold Time, ref edge Clk+
    t55  : Time := 1000 ns ;  -- TCk Cycle Time
    t56  : Time := 8 ns ;     -- TRst_N Input Setup Time, ref edge TCk+
    t57  : Time := 4 ns ;     -- TRst_N Input Hold Time, ref edge TCk+
    t58  : Time := 8 ns ;     -- TMS Input Setup Time, ref edge TCk+
    t59  : Time := 4 ns ;     -- TMS Input Hold Time, ref edge TCk+
    t60  : Time := 8 ns ;     -- TDI Input Setup Time, ref edge TCk+
    t61  : Time := 4 ns ;     -- TDI Input Hold Time, ref edge TCk+
    t62  : Time := 8 ns ;     -- TDO Output Delay Time, ref edge TCk+
    t63  : Time := 1000 ns ;  -- WDClk Clock Period time
                              -- before next Clk-
    t66  : Time := 20 ns ;    -- DPARIO Generation Time (Including Output
                              -- Delay Time), ref Data Valid
    t68  : Time := 20 ns ;    -- ExtHold, ExtCCV set up
    t69  : Time := 3 ns ;     -- Clk2 to Clk Low to High Output 
                              -- Delay Time, ref edge Clk2+
    t70  : Time := 3 ns ;     -- Clk2 to Clk High to Low Output 
                              -- Delay Time, ref edge Clk2+
    t71  : Time := 4 ns ;     -- Data read setup time for latches, ref edge Clk+
    t73  : Time := 35 ns ;    -- Data read setup time for checking EDAC checkbits and 
                              -- generating MHOLD before next Clk-, ref edge Clk+
    t74  : Time := 4 ns;      -- MEC Output comparision input setup time
    t75  : Time := 4 ns;      -- MEC Output comparision input hold  time
    t77  : Time := 8 ns );     -- Tx output Delay to High Z after three-stated

  port(
    CB           : inout  Std_Logic_Vector(6 downto 0) ;
    BusRdy_N     : in     Std_Logic ;
    BusErr_N     : in     Std_Logic ;
    ExtInt       : in     Std_Logic_Vector(4 downto 0) ;
    ASPar        : in     Std_Logic ;
    Size         : in     Std_Logic_Vector(1 downto 0) ;
    ASI          : in     Std_Logic_Vector(3 downto 0) ;
    APar         : in     Std_Logic ;
    A            : in     Std_Logic_Vector(31 downto 0) ;
    TDI          : in     Std_Logic ;
    TCk          : in     Std_Logic ;
    TRst_N       : in     Std_Logic ;
    TMS          : in     Std_Logic ;
    IMPar        : in     Std_Logic ;
    Wrt          : in     Std_Logic ;
    Rd           : in     Std_Logic ;
    Lock         : in     Std_Logic ;
    LdSto        : in     Std_Logic ;
    DXfer        : in     Std_Logic ;
    INull        : in     Std_Logic ;
    DMAAS        : in     Std_Logic ;
    IUErr_N      : in     Std_Logic ;
    IUHWErr_N    : in     Std_Logic ;
    FPUHWErr_N   : in     Std_Logic ;
    IUCmpErr_N   : in     Std_Logic ;
    FPUCmpErr_N  : in     Std_Logic ;
    SysHalt_N    : in     Std_Logic ;
    WDClk        : in     Std_Logic ;
    DRdy_N       : out    Std_Logic ;
    DParIO       : inout  Std_Logic ;
    D            : inout  Std_Logic_Vector(31 downto 0) ;
    TDO          : out    Std_Logic ;
    
    Clk          : out    Std_Logic_Vector(1 downto 0) ;
    
    AOE_N        : out    Std_Logic ;
    COE_N        : out    Std_Logic ;
    DOE_N        : out    Std_Logic ;
    BHold_N      : out    Std_Logic ;
    MDS_N        : out    Std_Logic ;
    MExc_N       : out    Std_Logic ;
    MHold_N      : out    Std_Logic ;
    BA           : out    Std_Logic_Vector(1 downto 0) ;
    DMAGnt_N     : out    Std_Logic ;
    ExtIntAck    : out    Std_Logic ;
    IRL          : out    Std_Logic_Vector(3 downto 0) ;
    CPUHalt_N    : out    Std_Logic ;
    TxB          : out    Std_Logic ;
    TxA          : out    Std_Logic ;
    Clk2         : in     Std_Logic ;
    SysReset_N   : in     Std_Logic ;
    Reset_N      : out    Std_Logic ;
    ExMCS_N      : out    Std_Logic ;
    IOBEn_N      : out    Std_Logic ;
    IOWR_N       : out    Std_Logic ;
    IOSel_N      : out    Std_Logic_Vector(3 downto 0) ;
    
    RAMBEn_N     : out    Std_Logic;
    ROMBEn_N     : out    Std_Logic;
    MemBEn_N     : out    Std_Logic;
    MemWr2_N     : out    Std_Logic_Vector(1 downto 0);
    MemWr1_N     : out    Std_Logic_Vector(1 downto 0);
    OE_N         : out    Std_Logic_Vector(1 downto 0);
    DDir         : out    Std_Logic ;
    DDir_N       : out    Std_Logic;
    
    DMAReq_N     : in     Std_Logic ;
    WE_N         : in     Std_Logic ;
    Prom8_N      : in     Std_Logic ;
    NoPar_N      : in     Std_Logic ;
    RomWrt_N     : in     Std_Logic ;
    MecHWErr_N   : out    Std_Logic ;
    SysErr_N     : out    Std_Logic ;
    SysAv        : out    Std_Logic ;
    MemCS_N      : out    Std_Logic_Vector(9 downto 0) ;
    RomCS_N      : out    Std_Logic ;
    INTAck       : in     Std_Logic ;
    ALE_N        : out    Std_Logic ;
    RxA          : in     Std_Logic ;
    RxB          : in     Std_Logic ;
    ExtHold_N    : in     Std_Logic ;
    ExtCCV       : in     Std_Logic
    ) ;
end MECGEN ;


------------------------------------------------------------------
-- Architectural body of 'MECGEN'.
------------------------------------------------------------------

architecture Schematic of MECGEN is

 component MECfunc
  port(
    Prom8_In_N         : in     UX01 ;
    BusRdy_In_N        : in     UX01 ;
    BusErr_In_N        : in     UX01 ;
    ASPar_In           : in     UX01 ;
    Size_In            : in     Std_Logic_Vector(1 downto 0) ;
    ASI_In             : in     Std_Logic_Vector(3 downto 0) ;
    APar_In            : in     UX01 ;
    A_In               : in     Std_Logic_Vector(31 downto 0) ;
    TDI_In             : in     UX01 ;
    TCk_In             : in     UX01 ;
    TRst_In_N          : in     UX01 ;
    TMS_In             : in     UX01 ;
    IMPar_In           : in     UX01 ;
    Wrt_In             : in     UX01 ;
    WE_In_N            : in     UX01 ;
    Rd_In              : in     UX01 ;
    Lock_In            : in     UX01 ;
    LdSto_In           : in     UX01 ;
    DXfer_In           : in     UX01 ;
    INull_In           : in     UX01 ;
    DMAAS_In           : in     UX01 ;
    NoPar_In_N         : in     UX01 ;
    RomWrt_In_N        : in     UX01 ;
    WDClk_In           : in     UX01 ;
    DRdy_Out_N         : out    UX01 ;
    TDO_Out            : out    UX01 ;
    DMAGnt_Out_N       : out    UX01 ;
    Clk2_In            : in     UX01 ;
    DMAReq_In_N        : in     UX01 ;
    CB_In              : in     Std_Logic_Vector(6 downto 0) ;
    CB_Out             : out    Std_Logic_Vector(6 downto 0) ;
    CBEn_N             : out    UX01 ;
    IUErr_In_N         : in     UX01 ;
    IUHWErr_In_N       : in     UX01 ;
    FPUHWErr_In_N      : in     UX01 ;
    IUCmpErr_In_N      : in     UX01 ;
    FPUCmpErr_In_N     : in     UX01 ;
    MecHWErr_Out_N     : out    UX01 ;
    SysErr_Out_N       : out    UX01 ;
    SysAv_Out          : out    UX01 ;
    TxB_Out            : out    UX01 ;
    TxA_Out            : out    UX01 ;
    RxA_In             : in     UX01 ;
    RxB_In             : in     UX01 ;
    Clk_Out            : buffer UX01 ;
    CPUHalt_Out_N      : out    UX01 ;
    Reset_Out_N        : out    UX01 ;
    SysHalt_In_N       : in     UX01 ;
    SysReset_In_N      : in     UX01 ;
    IntAck_In          : in     UX01 ;
    AOE_Out_N          : out    UX01 ;
    COE_Out_N          : out    UX01 ;
    DOE_Out_N          : out    UX01 ;
    BHold_Out_N        : out    UX01 ;
    MDS_Out_N          : out    UX01 ;
    MExc_Out_N         : out    UX01 ;
    MHold_Out_N        : out    UX01 ;
    IRL_Out            : out    Std_Logic_Vector(3 downto 0) ;
    ExtInt_In          : in     Std_Logic_Vector(4 downto 0) ;
    ExtIntAck_Out      : inout    UX01 ;
    ExMCS_Out_N        : out    UX01 ;
    IOBEn_Out_N        : out    UX01 ;
    IOWR_Out_N         : out    UX01 ;
    IOSel_Out_N        : out    Std_Logic_Vector(3 downto 0) ;
    BA_Out             : out    Std_Logic_Vector(1 downto 0) ;
    MemBEn_Out_N       : out    UX01 ;
    RAMBEn_Out_N       : out    UX01 ;
    ROMBEn_Out_N       : out    UX01 ;
    CBWr_Out_N         : out    UX01 ;
    MemWr_Out_N        : out    UX01 ;
    OE_Out_N           : out    UX01 ;
    DDir_Out           : out    UX01 ;
    MemCS_Out_N        : out    Std_Logic_Vector(9 downto 0) ;
    RomCS_Out_N        : out    UX01 ;
    ALE_Out_N          : out    UX01 ;
    DParIO_Out         : out    UX01 ;
    D_Out              : out    Std_Logic_Vector(31 downto 0) ;
    DParIO_In          : in     UX01 ;
    D_In               : in     Std_Logic_Vector(31 downto 0) ;
    DBEn_N             : out    UX01 ;
    DParIOEn_N         : out    UX01 ;
        
    ExtHold_In_N       : in     UX01 ;
    ExtCCV_In          : in     UX01 ;
    
    ParityEnable       : out    UX01 ; -- Only used for D setup time checking
    EDACEnable         : out    UX01 ; -- Only used for D setup time checking
    MemData_Valid      : out    UX01 ; -- Only used for D setup time checking
    Wr_Int_N           : buffer UX01 ; -- Only used for D setup time checking
    MECRegister_Access : out    UX01
    );
 end component  ;
 component Clk_buffer
  generic(
    CHECK : Boolean := False ;
    t2    : Time := 10 ns ;
    t3    : Time := 22 ns ;
    t63   : Time := 1000 ns ) ;
  port(
    WDClk    : in     Std_Logic ;
    Clk2     : in     Std_Logic ;
    Clk2_In  : out    UX01 ;
    WDClk_In : out    UX01 ) ;
 end component  ;
 component Test_Buffer
  generic(
    t55   : Time := 1000 ns ;
    t56   : Time := 8 ns ;
    t57   : Time := 4 ns ;
    t58   : Time := 8 ns ;
    t59   : Time := 4 ns ;
    t60   : Time := 8 ns ;
    t61   : Time := 4 ns ;
    t62   : Time := 8 ns ;
    CHECK : Boolean := False ) ;
  port(
    TDI       : in     Std_Logic ;
    TCk       : in     Std_Logic ;
    TRst_N    : in     Std_Logic ;
    TMS       : in     Std_Logic ;
    TDO       : out    Std_Logic ;
    TDO_Out   : in     UX01 ;
    TDI_In    : out    UX01 ;
    TCk_In    : out    UX01 ;
    TRst_In_N : out    UX01 ;
    TMS_In    : out    UX01 ) ;
 end component  ;
 component IU_In_Buffer
  generic(
    t4    : Time := 8 ns ;
    t5    : Time := 4 ns ;
    t6    : Time := 8 ns ;
    t7    : Time := 4 ns ;
    t53   : Time := 8 ns ;
    t54   : Time := 2 ns ;
    t35   : Time := 8 ns ;
    t36   : Time := 4 ns ;
    t33   : Time := 8 ns ;
    t34   : Time := 2 ns ;
    CHECK : Boolean := False ) ;
  port(
    A        : in     Std_Logic_Vector(31 downto 0) ;
    Size     : in     Std_Logic_Vector(1 downto 0) ;
    ASPar    : in     Std_Logic ;
    APar     : in     Std_Logic ;
    ASI      : in     Std_Logic_Vector(3 downto 0) ;
    IMPar    : in     Std_Logic ;
    Rd       : in     Std_Logic ;
    Lock     : in     Std_Logic ;
    LdSto    : in     Std_Logic ;
    INull    : in     Std_Logic ;
    ASPar_In : out    UX01 ;
    Size_In  : out    Std_Logic_Vector(1 downto 0) ;
    ASI_In   : out    Std_Logic_Vector(3 downto 0) ;
    APar_In  : out    UX01 ;
    A_In     : out    Std_Logic_Vector(31 downto 0) ;
    IMPar_In : out    UX01 ;
    Wrt_In   : out    UX01 ;
    Rd_In    : out    UX01 ;
    Lock_In  : out    UX01 ;
    LdSto_In : out    UX01 ;
    INull_In : out    UX01 ;
    WE_In_N  : out    UX01 ;
    DXfer    : in     Std_Logic ;
    DXfer_In : out    UX01 ;
    Wrt      : in     Std_Logic ;
    WE_N     : in     Std_Logic ;
    Clk_Out_Int  : in     UX01 ) ;
 end component  ;
 component Setupandcheck_buffer
  generic(
    t49   : Time := 8 ns ;
    t50   : Time := 4 ns ;
    CHECK : Boolean := False ) ;
  port(
    Clk_Out_Int : in     UX01 ;
    Prom8_In_N  : out    UX01 ;
    NoPar_In_N  : out    UX01 ;
    RomWrt_In_N : out    UX01 ;
    Prom8_N     : in     Std_Logic ;
    NoPar_N     : in     Std_Logic ;
    RomWrt_N    : in     Std_Logic ) ;
 end component  ;
 component DMA_Buffer
  generic(
    t40   : Time := 15 ns ;
    t42   : Time := 4 ns ;
    t6    : Time := 8 ns ;
    t7    : Time := 4 ns ;
    t27   : Time := 8 ns ;
    t38   : Time := 15 ns ;
    t39   : Time := 4 ns ;
    t37   : Time := 8 ns ;
    t68   : Time := 22 ns ;
    t74   : Time := 4 ns ;
    t75   : Time := 4 ns ;
    CHECK : Boolean := False ) ;
  port(
    BusRdy_In_N    : out    UX01 ;
    BusErr_In_N    : out    UX01 ;
    DMAAS_In       : out    UX01 ;
    DRdy_Out_N     : in     UX01 ;
    DMAGnt_Out_N   : in     UX01 ;
    DMAReq_In_N    : out    UX01 ;
    DMAAS          : in     Std_Logic ;
    DMAReq_N       : in     Std_Logic ;
    BusRdy_N       : in     Std_Logic ;
    BusErr_N       : in     Std_Logic ;
    DMAGnt_N       : out    Std_Logic ;
    DRdy_N         : out    Std_Logic ;
    Clk_Out_Int    : in     UX01 ;
    Clk_Out        : in     UX01 ;
    ExtHold_N      : in     Std_Logic ;
    ExtHold_In_N   : out    UX01 ; 
    ExtCCV         : in     Std_Logic ;
    ExtCCV_In      : out    UX01 ) ;
 end component  ;
 component Checkbits_Buffer
  GENERIC(
    t20   : Time := 1 ns ;
    t23   : Time := 2 ns ;
    t24   : Time := 2 ns ;
    t71   : Time := 12 ns ;
    CHECK : Boolean := False );
  PORT(
    CB_Out        : IN     Std_Logic_Vector(6 downto 0) ;
    CB_In         : OUT    Std_Logic_Vector(6 downto 0) ;
    CB            : INOUT  Std_Logic_Vector(6 downto 0) ;
    CBEn_N        : IN     UX01 ;
    Clk_Out_Int   : IN     UX01 ;
    Clk_Out       : IN     UX01 ;
    Reset_Out_N   : IN     UX01 ;
    EDACEnable    : IN     UX01 ;
    ParityEnable  : IN     UX01 ;
    MemData_Valid : IN     UX01 );
END component ;
 component SystemError_Buffer
  generic(
    t47   : Time := 4 ns ;
    t46   : Time := 8 ns ;
    t48   : Time := 8 ns ;
    CHECK : Boolean := False ) ;
  port(
    IUErr_N         : in     Std_Logic ;
    IUHWErr_N       : in     Std_Logic ;
    FPUHWErr_N      : in     Std_Logic ;
    IUCmpErr_N      : in     Std_Logic ;
    FPUCmpErr_N     : in     Std_Logic ;
    MecHWErr_N      : out    Std_Logic ;
    SysErr_N        : out    Std_Logic ;
    SysAv           : out    Std_Logic ;
    IUErr_In_N      : out    UX01 ;
    IUHWErr_In_N    : out    UX01 ;
    FPUHWErr_In_N   : out    UX01 ;
    IUCmpErr_In_N   : out    UX01 ;
    FPUCmpErr_In_N  : out    UX01 ;
    MecHWErr_Out_N  : in     UX01 ;
    SysErr_Out_N    : in     UX01 ;
    SysAv_Out       : in     UX01 ;
    Clk_Out_Int     : in     UX01 ;
    Clk_Out         : in     UX01) ;
 end component  ;
 component Serial_Buffer
  generic(
    CHECK : Boolean := False ) ;
  port(
    TxB             : out    Std_Logic ;
    TxA             : out    Std_Logic ;
    RxA             : in     Std_Logic ;
    RxB             : in     Std_Logic ;
    TxB_Out         : in     UX01 ;
    TxA_Out         : in     UX01 ;
    RxA_In          : out    UX01 ;
    RxB_In          : out    UX01 ;
    SerialClk_Int   : in     UX01 ) ;
 end component  ;
 component SysCtrl_Buffer
  generic(
    t1    : Time := 44 ns ;
    t69   : Time := 3 ns ;
    t70   : Time := 3 ns ;
    t30   : Time := 15 ns ;
    t31   : Time := 4 ns ;
    t32   : Time := 4 ns ;
    t74   : Time := 4 ns ;
    t75   : Time := 4 ns ;
    CHECK : Boolean := False ) ;
  port(
    Clk                : out    Std_Logic_Vector(1 downto 0);
    CPUHalt_N          : out    Std_Logic ;
    Reset_N            : out    Std_Logic ;
    SysHalt_N          : in     Std_Logic ;
    SysReset_N         : in     Std_Logic ;
    SysHalt_In_N       : out    UX01 ;
    SysReset_In_N      : out    UX01 ;
    Clk_Out_Int        : OUT    UX01 ;
    Clk_Out            : in     UX01 ;
    CPUHalt_Out_N      : in     UX01 ;
    Reset_Out_N        : in     UX01) ;
 end component  ;
 component IU_Out_Buffer
  generic(
    t25   : Time := 8 ns ;
    t26   : Time := 8 ns ;
    t27   : Time := 8 ns ;
    t28   : Time := 8 ns ;
    t29   : Time := 4 ns ;
    t43   : Time := 8 ns ;
    t37   : Time := 8 ns ;
    t73   : Time := 28 ns ;
    t74   : Time := 4 ns ;
    t75   : Time := 4 ns ;
    CHECK : Boolean := False ) ;
  port(
    AOE_N             : out    Std_Logic ;
    COE_N             : out    Std_Logic ;
    DOE_N             : out    Std_Logic ;
    BHold_N           : out    Std_Logic ;
    MDS_N             : out    Std_Logic ;
    MExc_N            : out    Std_Logic ;
    MHold_N           : out    Std_Logic ;
    IRL               : out    Std_Logic_Vector(3 downto 0) ;
    IntAck_In         : out    Std_Logic;
    INTAck            : in     UX01 ;
    AOE_Out_N         : in     UX01 ;
    COE_Out_N         : in     UX01 ;
    DOE_Out_N         : in     UX01 ;
    BHold_Out_N       : in     UX01 ;
    MDS_Out_N         : in     UX01 ;
    MExc_Out_N        : in     UX01 ;
    MHold_Out_N       : in     UX01 ;
    IRL_Out           : in     Std_Logic_Vector(3 downto 0) ;
    Clk_Out_Int       : in     UX01);

 end component  ;

 component Data_Buffer
  GENERIC(
    t19   : Time := 25 ns ;
    t20   : Time := 1 ns ;
    t21   : Time := 10 ns ;
    t22   : Time := 4 ns ;
    t66   : Time := 20 ns ;
    t51   : Time := 8 ns ;
    t53   : Time := 8 ns ;
    t54   : Time := 2 ns ;
    t71   : Time := 4 ns ;
    t73   : Time := 35 ns ;
    CHECK : Boolean := False ) ;
  PORT(
    DParIO          : INOUT  Std_Logic ;
    D               : INOUT  Std_Logic_Vector(31 downto 0) ;
    DParIO_Out      : IN     UX01 ;
    D_Out           : IN     Std_Logic_Vector(31 downto 0) ;
    DParIO_In       : OUT    UX01 ;
    D_In            : OUT    Std_Logic_Vector(31 downto 0) ;
    DBEn_N          : IN     UX01 ;
    DParIOEn_N      : IN     UX01 ;
    Clk_Out_Int     : IN     UX01 ;
    Clk_Out         : IN     UX01 ;
    ParityEnable    : IN     UX01 ;
    Reset_Out_N     : IN     UX01 ;
    EDACEnable      : IN     UX01 ;
    MemData_Valid   : IN     UX01 ;
    MHOLD_N         : IN     UX01 ;
    Wr_Int_N        : IN     UX01 ;
    MECRegister_Access : in  UX01 ;
    RomCS_Out_N : in  UX01
    );
 END component  ;
 component Mem_Buffer
  generic(
    t8    : Time := 8 ns ;
    t9    : Time := 15 ns ;
    t10   : Time := 5 ns ;
    t11   : Time := 1 ns ;
    t12   : Time := 8 ns ;
    t13   : Time := 8 ns ;
    t14   : Time := 8 ns ;
    t15   : Time := 8 ns ;
    t16   : Time := 8 ns ;
    t17   : Time := 8 ns ;
    t74   : Time := 4 ns ;
    t75   : Time := 4 ns ;
    CHECK : Boolean := False ) ;
  port(
    BA             : OUT    Std_Logic_Vector(1 downto 0) ;
    RAMBEn_N       : OUT    Std_Logic ;
    ROMBEn_N       : OUT    Std_Logic ;
    MemBEn_N       : OUT    Std_Logic ;
    MemWr2_N       : OUT    Std_Logic_Vector(1 downto 0) ;
    MemWr1_N       : OUT    Std_Logic_Vector(1 downto 0) ;
    OE_N           : OUT    Std_Logic_Vector(1 downto 0) ;
    DDir           : OUT    Std_Logic ;
    DDir_N         : OUT    Std_Logic ;
    MemCS_N        : OUT    Std_Logic_Vector(9 downto 0) ;
    RomCS_N        : OUT    Std_Logic ;
    ALE_N          : OUT    Std_Logic ;
    BA_Out         : IN     Std_Logic_Vector(1 downto 0) ;
    MemBEn_Out_N   : IN     UX01 ;
    RAMBEn_Out_N   : IN     UX01 ;
    ROMBEn_Out_N   : IN     UX01 ;
    CBWr_Out_N     : IN     UX01 ;
    MemWr_Out_N    : IN     UX01 ;
    OE_Out_N       : IN     UX01 ;
    DDir_Out       : IN     UX01 ;
    MemCS_Out_N    : IN     Std_Logic_Vector(9 downto 0) ;
    RomCS_Out_N    : IN     UX01 ;
    ALE_Out_N      : IN     UX01 ;
    Clk_Out_Int    : IN     UX01;
    Clk_Out        : IN     UX01;
    A_In           : in     Std_Logic_Vector(31 downto 0) ) ;
 end component  ;
 
 component IO_Buffer
  generic(
    t13   : Time := 8 ns ;
    t15   : Time := 8 ns ;
    t18   : Time := 30 ns ;
    t9    : Time := 15 ns ;
    t10   : Time := 5 ns ;
    t11   : Time := 1 ns ;
    t12   : Time := 8 ns ;
    t74   : Time := 4 ns ;
    t75   : Time := 4 ns ;
    CHECK : Boolean := False ) ;
  port(
    ExMCS_N       : out    Std_Logic ;
    IOBEn_N       : out    Std_Logic ;
    IOWR_N        : out    Std_Logic ;
    IOSel_N       : out    Std_Logic_Vector(3 downto 0) ;
    ExMCS_Out_N   : in     UX01 ;
    IOBEn_Out_N   : in     UX01 ;
    IOWR_Out_N    : in     UX01 ;
    IOSel_Out_N   : in     Std_Logic_Vector(3 downto 0) ;
    A_In          : in     Std_Logic_Vector(31 downto 0) ; -- added signal
    Clk_Out_Int   : in     UX01 ;
    Clk_Out       : in     UX01 ) ;
 end component  ;
 component ExternalInterrupt_Buffer
  generic(
    t44   : Time := 8 ns ;
    t45   : Time := 4 ns ;
    t43   : Time := 8 ns ;
    t74   : Time := 4 ns ;
    t75   : Time := 4 ns ;
    CHECK : Boolean := False ) ;
  port(
    ExtInt           : in     Std_Logic_Vector(4 downto 0) ;
    ExtIntAck        : out    Std_Logic ;
    ExtInt_In        : out    Std_Logic_Vector(4 downto 0) ;
    ExtIntAck_Out    : inout  UX01 ;
    Clk_Out_Int      : in     UX01 ;
    Clk_Out          : in     UX01 );
 end component  ;
 signal Clk2_In            : UX01 ;
 signal WDClk_In           : UX01 ;
 signal Wrt_In             : UX01 ;
 signal Tck_In             : UX01 ;
 signal A_In               : Std_Logic_Vector(31 downto 0) ;
 signal APar_In            : UX01 ;
 signal ASI_In             : Std_Logic_Vector(3 downto 0) ;
 signal Size_In            : Std_Logic_Vector(1 downto 0) ;
 signal ASPar_In           : UX01 ;
 signal SysErr_Out_N       : UX01 ;
 signal TDI_In             : UX01 ;
 signal TMS_In             : UX01 ;
 signal TRst_In_N          : UX01 ;
 signal SysAv_Out          : UX01 ;
 signal Prom8_In_N         : UX01 ;
 signal NoPar_In_N         : UX01 ;
 signal RomWrt_In_N        : UX01 ;
 signal DDir_Out_N         : UX01 ;
 signal IOBEn_Out_N        : UX01 ;
 signal IOWR_Out_N         : UX01 ;
 signal IMPar_In           : UX01 ;
 signal BusErr_In_N        : UX01 ;
 signal WE_In_N            : UX01 ;
 signal DMAAS_In           : UX01 ;
 signal DMAGnt_Out_N       : UX01 ;
 signal CBValid_Int        : UX01 ;
 signal DataValid_Int      : UX01 ;
 signal DParIOValid_Int    : UX01 ;
 signal CB_In              : Std_Logic_Vector(6 downto 0) ;
 signal CBEn_N             : UX01 ;
 signal IUErr_In_N         : UX01 ;
 signal IUHWErr_In_N       : UX01 ;
 signal FPUHWErr_In_N      : UX01 ;
 signal IUCmpErr_In_N      : UX01 ;
 signal FPUCmpErr_In_N     : UX01 ;
 signal MecHWErr_Out_N     : UX01 ;
 signal RxA_In             : UX01 ;
 signal RxB_In             : UX01 ;
 signal SysReset_In_N      : UX01 ;
 signal TxB_Out            : UX01 ;
 signal Clk_Out            : UX01 ;
 signal Reset_Out_N        : UX01 ;
 signal CPUHalt_Out_N      : UX01 ;
 signal SysHalt_In_N       : UX01 ;
 signal AOE_Out_N          : UX01 ;
 signal RD_In              : UX01 ;
 signal Lock_In            : UX01 ;
 signal LdSto_In           : UX01 ;
 signal DXfer_In           : UX01 ;
 signal IRL_Out            : Std_Logic_Vector(3 downto 0) ;
 signal INTAck_In          : UX01 ;
 signal DPARIO_In          : UX01 ;
 signal TDO_Out            : UX01 ;
 signal ExtIntAck_Out      : UX01 ;
 signal ExtInt_In          : Std_Logic_Vector(4 downto 0) ;
 signal IOSel_Out_N        : Std_Logic_Vector(3 downto 0) ;
 signal DOE_Out_N          : UX01 ;
 signal COE_Out_N          : UX01 ;
 signal ExMCS_Out_N        : UX01 ;
 signal ALE_Out_N          : UX01 ;
 signal BA_Out             : Std_Logic_Vector(1 downto 0) ;
 signal RomCS_Out_N        : UX01 ;
 signal MemCS_Out_N        : Std_Logic_Vector(9 downto 0) ;
 signal MHold_Out_N        : UX01 ;
 signal MExc_Out_N         : UX01 ;
 signal MDS_Out_N          : UX01 ;
 signal DParIO_Out         : UX01 ;
 signal D_In               : Std_Logic_Vector(31 downto 0) ;
 signal DBEn_N             : UX01 ;
 signal DParIOEn_N         : UX01 ;
 signal INull_In           : UX01 ;
 signal CBWR_Out_N         : UX01 ;
 signal MemBEn_Out_N       : UX01 ;
 signal RAMBEn_Out_N       : UX01 ;
 signal ROMBEn_Out_N       : UX01 ;
 signal DRdy_Out_N         : UX01 ;
 signal BusRdy_In_N        : UX01 ;
 signal DMAReq_In_N        : UX01 ;
 signal MemWR_Out_N        : UX01 ;
 signal TxA_Out            : UX01 ;
 signal D_Out              : Std_Logic_Vector(31 downto 0) ;
 signal SerialClk_Int      : UX01 ;
 signal CB_Out             : Std_Logic_Vector(6 downto 0) ;
 signal OE_Out_N           : UX01 ;
 signal ExtHold_In_N       : UX01 ;
 signal ExtCCV_In          : UX01 ;
 signal ParityEnable       : UX01 ;
 signal EDACEnable         : UX01 ;
 signal MemData_Valid      : UX01 ;
 signal Wr_Int_N           : UX01 ;
 signal Clk_Out_Int        : UX01 ;
 signal MECRegister_Access : UX01 ;
 signal BHold_Out_N        : UX01 ;

 
begin

 
 MECFUNCtionality: MECfunc
    port map(
      Prom8_In_N => Prom8_In_N,
      BusRdy_In_N => BusRdy_In_N,
      BusErr_In_N => BusErr_In_N,
      ASPar_In => ASPar_In,
      Size_In => Size_In,
      ASI_In => ASI_In,
      APar_In => APar_In,
      A_In => A_In,
      TDI_In => TDI_In,
      TCk_In => Tck_In,
      TRst_In_N => TRst_In_N,
      TMS_In => TMS_In,
      IMPar_In => IMPar_In,
      Wrt_In => Wrt_In,
      WE_In_N => WE_In_N,
      Rd_In => RD_In,
      Lock_In => Lock_In,
      LdSto_In => LdSto_In,
      DXfer_In => DXfer_In,
      INull_In => INull_In,
      DMAAS_In => DMAAS_In,
      NoPar_In_N => NoPar_In_N,
      RomWrt_In_N => RomWrt_In_N,
      WDClk_In => WDClk_In,
      DRdy_Out_N => DRdy_Out_N,
      TDO_Out => TDO_Out,
      DMAGnt_Out_N => DMAGnt_Out_N,
      Clk2_In => Clk2_In,
      DMAReq_In_N => DMAReq_In_N,
      CB_In => CB_In,
      CB_Out => CB_Out,
      CBEn_N => CBEn_N,
      IUErr_In_N => IUErr_In_N,
      IUHWErr_In_N => IUHWErr_In_N,
      FPUHWErr_In_N => FPUHWErr_In_N,
      IUCmpErr_In_N => IUCmpErr_In_N,
      FPUCmpErr_In_N => FPUCmpErr_In_N,
      MecHWErr_Out_N => MecHWErr_Out_N,
      SysErr_Out_N => SysErr_Out_N,
      SysAv_Out => SysAv_Out,
      TxB_Out => TxB_Out,
      TxA_Out => TxA_Out,
      RxA_In => RxA_In,
      RxB_In => RxB_In,
      Clk_Out => Clk_Out,
      CPUHalt_Out_N => CPUHalt_Out_N,
      Reset_Out_N => Reset_Out_N,
      SysHalt_In_N => SysHalt_In_N,
      SysReset_In_N => SysReset_In_N,
      IntAck_In => INTAck_In,
      AOE_Out_N => AOE_Out_N,
      COE_Out_N => COE_Out_N,
      DOE_Out_N => DOE_Out_N,
      BHold_Out_N => BHold_Out_N,
      MDS_Out_N => MDS_Out_N,
      MExc_Out_N => MExc_Out_N,
      MHold_Out_N => MHold_Out_N,
      IRL_Out => IRL_Out,
      ExtInt_In => ExtInt_In,
      ExtIntAck_Out => ExtIntAck_Out,
      ExMCS_Out_N => ExMCS_Out_N,
      IOBEn_Out_N => IOBEn_Out_N,
      IOWR_Out_N => IOWR_Out_N,
      IOSel_Out_N => IOSel_Out_N,
      BA_Out => BA_Out,
      MemBEn_Out_N => MemBEn_Out_N,
      RAMBEn_Out_N => RAMBEn_Out_N,
      ROMBEn_Out_N => ROMBEn_Out_N,
      CBWr_Out_N => CBWR_Out_N,
      MemWr_Out_N => MemWR_Out_N,
      OE_Out_N => OE_Out_N,
      DDir_Out => DDir_Out_N,
      MemCS_Out_N => MemCS_Out_N,
      RomCS_Out_N => RomCS_Out_N,
      ALE_Out_N => ALE_Out_N,
      DParIO_Out => DParIO_Out,
      D_Out => D_Out,
      DParIO_In => DPARIO_In,
      D_In => D_In,
      DBEn_N => DBEn_N,
      DParIOEn_N => DParIOEn_N,
      
      ExtHold_In_N => ExtHold_In_N,
      ExtCCV_In => ExtCCV_In,
      
      ParityEnable => ParityEnable,
      EDACEnable => EDACEnable,
      MemData_Valid => MemData_Valid,
      Wr_Int_N => Wr_Int_N,
      MECRegister_Access => MECRegister_Access
      ) ;
 Clock_buffer: Clk_buffer
    generic map(
      CHECK => CHECK,
      t2 => t2 ,
      t3 => t3 ,
      t63 => t63 )
    port map(
      WDClk => WDClk,
      Clk2 => Clk2,
      Clk2_In => Clk2_In,
      WDClk_In => WDClk_In ) ;

 Test_Port_Buffer: Test_Buffer
    generic map(
      t55 => t55 ,
      t56 => t56 ,
      t57 => t57 ,
      t58 => t58 ,
      t59 => t59 ,
      t60 => t60 ,
      t61 => t61 ,
      t62 => t62 ,
      CHECK => CHECK )
    port map(
      TDI => TDI,
      TCk => TCk,
      TRst_N => TRst_N,
      TMS => TMS,
      TDO => TDO,
      TDO_Out => TDO_Out,
      TMS_In => TMS_In,
      TCk_In => Tck_In,
      TRst_In_N => TRst_In_N,
      TDI_In => TDI_In ) ;

 IU_Input_Buffer: IU_In_Buffer
    generic map(
      t4 => t4 ,
      t5 => t5 ,
      t6 => t6 ,
      t7 => t7 ,
      t53 => t53 ,
      t54 => t54 ,
      t35 => t35 ,
      t36 => t36 ,
      t33 => t33 ,
      t34 => t34 ,
      CHECK => CHECK )
    port map(
      A => A,
      Size => Size,
      ASPar => ASPar,
      APar => APar,
      ASI => ASI,
      IMPar => IMPar,
      Rd => Rd,
      Lock => Lock,
      LdSto => LdSto,
      INull => INull,
      ASPar_In => ASPar_In,
      Size_In => Size_In,
      ASI_In => ASI_In,
      APar_In => APar_In,
      A_In => A_In,
      IMPar_In => IMPar_In,
      Wrt_In => Wrt_In,
      Rd_In => RD_In,
      Lock_In => Lock_In,
      LdSto_In => LdSto_In,
      INull_In => INull_In,
      WE_In_N => WE_In_N,
      DXfer => DXfer,
      DXfer_In => DXfer_In,
      Wrt => Wrt,
      WE_N => WE_N,
      Clk_Out_Int => Clk_Out_Int ) ;

 Setupandchecker_Buffer: Setupandcheck_buffer
    generic map(
      t49 => t49 ,
      t50 => t50 ,
      CHECK => CHECK )
    port map(
      Clk_Out_Int => Clk_Out_Int, 
      Prom8_In_N => Prom8_In_N,
      NoPar_In_N => NoPar_In_N,
      RomWrt_In_N => RomWrt_In_N,
      Prom8_N => Prom8_N,
      NoPar_N => NoPar_N,
      RomWrt_N => RomWrt_N ) ;

 DMAandBus_Buffer: DMA_Buffer
    generic map(
      t40 => t40 ,
      t42 => t42 ,
      t6 => t6 ,
      t7 => t7 ,
      t27 => t27 ,
      t38 => t38 ,
      t39 => t39 ,
      t37 => t37 ,
      t68 => t68 ,
      t74 => t74 ,
      t75 => t75 ,
      CHECK => CHECK )
    port map(
      BusRdy_In_N => BusRdy_In_N,
      BusErr_In_N => BusErr_In_N,
      DMAAS_In => DMAAS_In,
      DRdy_Out_N => DRdy_Out_N,
      DMAGnt_Out_N => DMAGnt_Out_N,
      DMAReq_In_N => DMAReq_In_N,
      DMAAS => DMAAS,
      DMAReq_N => DMAReq_N,
      BusRdy_N => BusRdy_N,
      BusErr_N => BusErr_N,
      DMAGnt_N => DMAGnt_N,
      DRdy_N => DRdy_N,
      Clk_Out_Int => Clk_Out_Int,
      Clk_Out => Clk_Out,
      ExtHold_N => ExtHold_N,
      ExtHold_In_N => ExtHold_In_N,
      ExtCCV => ExtCCV,
      ExtCCV_In => ExtCCV_In
       ) ;

 CB_Buffer: Checkbits_Buffer
    generic map(
      t20 => t20 ,
      t23 => t23 ,
      t24 => t24 ,
      t71 => t71 ,
      CHECK => CHECK)
    port map(
      CB_Out => CB_Out(6 downto 0),
      CB_In => CB_In,
      CB => CB,
      CBEn_N => CBEn_N,
      Clk_Out_Int => Clk_Out_Int,
      Clk_Out => Clk_Out,
      Reset_Out_N => Reset_Out_N,
      EDACEnable => EDACEnable,
      ParityEnable => ParityEnable,
      MemData_Valid => MemData_Valid
       ) ;

 Error_Buffer: SystemError_Buffer
    generic map(
      t47 => t47 ,
      t46 => t46 ,
      t48 => t48 ,
      CHECK => CHECK )
    port map(
      IUErr_N => IUErr_N,
      IUHWErr_N => IUHWErr_N,
      FPUHWErr_N => FPUHWErr_N,
      IUCmpErr_N => IUCmpErr_N,
      FPUCmpErr_N => FPUCmpErr_N,
      MecHWErr_N => MecHWErr_N,
      SysErr_N => SysErr_N,
      SysAv => SysAv,
      IUErr_In_N => IUErr_In_N,
      IUHWErr_In_N => IUHWErr_In_N,
      FPUHWErr_In_N => FPUHWErr_In_N,
      IUCmpErr_In_N => IUCmpErr_In_N,
      FPUCmpErr_In_N => FPUCmpErr_In_N,
      MecHWErr_Out_N => MecHWErr_Out_N,
      SysErr_Out_N => SysErr_Out_N,
      SysAv_Out => SysAv_Out,
      Clk_Out_Int => Clk_Out_Int,
      Clk_Out => Clk_Out
      ) ;

 UART_Buffer: Serial_Buffer
    generic map(
      CHECK => CHECK) 
    port map(
      TxB => TxB,
      TxA => TxA,
      RxA => RxA,
      RxB => RxB,
      TxB_Out => TxB_Out,
      TxA_Out => TxA_Out,
      RxA_In => RxA_In,
      RxB_In => RxB_In,
      SerialClk_Int => SerialClk_Int
      ) ;

 System_Buffer: SysCtrl_Buffer
    generic map(
      t1 => t1 ,
      t69 => t69 ,
      t70 => t70 ,
      t30 => t30 ,
      t32 => t32 ,
      t74 => t74 ,
      t75 => t75 ,
      CHECK => CHECK )
    port map(
      Clk => Clk,
      CPUHalt_N => CPUHalt_N,
      Reset_N => Reset_N,
      SysHalt_N => SysHalt_N,
      SysReset_N => SysReset_N,
      SysHalt_In_N => SysHalt_In_N,
      SysReset_In_N => SysReset_In_N,
      Clk_Out_Int => Clk_Out_Int,
      Clk_Out => Clk_Out,
      CPUHalt_Out_N => CPUHalt_Out_N,
      Reset_Out_N => Reset_Out_N
      ) ;

 IU_Output_Buffer: IU_Out_Buffer
    generic map(
      t25 => t25 ,
      t26 => t26 ,
      t27 => t27 ,
      t28 => t28 ,
      t29 => t29 ,
      t43 => t43 ,
      t37 => t37 ,
      t73 => t73 ,
      t74 => t74 ,
      t75 => t75 ,
      CHECK => CHECK )
    port map(
      AOE_N => AOE_N,
      COE_N => COE_N,
      DOE_N => DOE_N,
      BHold_N => BHold_N,
      MDS_N => MDS_N,
      MExc_N => MExc_N,
      MHold_N => MHold_N,
      IRL => IRL,
      IntAck_In => IntAck_In,
      INTAck => INTAck,
      AOE_Out_N => AOE_Out_N,
      COE_Out_N => COE_Out_N,
      DOE_Out_N => DOE_Out_N,
      BHold_Out_N => BHold_Out_N,
      MDS_Out_N => MDS_Out_N,
      MExc_Out_N => MExc_Out_N,
      MHold_Out_N => MHold_Out_N,
      IRL_Out => IRL_Out,
      Clk_Out_Int => Clk_Out_Int 
      ) ;

 D_Buffer: Data_Buffer
    generic map(
      t19 => t19 ,
      t20 => t20 ,
      t21 => t21 ,
      t22 => t22 ,
      t66 => t66 ,
      t51 => t51 ,
      t53 => t53 ,
      t54 => t54 ,
      t71 => t71 ,
      t73 => t73 ,
      CHECK => CHECK )
    port map(
      DParIO => DParIO,
      D => D,
      DParIO_Out => DParIO_Out,
      D_Out => D_Out,
      DParIO_In => DPARIO_In,
      D_In => D_In,
      DBEn_N => DBEn_N,
      DParIOEn_N => DParIOEn_N,
      Clk_Out_Int => Clk_Out_Int,
      Clk_Out => Clk_Out,
      Reset_Out_N => Reset_Out_N,
      ParityEnable => ParityEnable,
      EDACEnable => EDACEnable,
      MemData_Valid => MemData_Valid,
      MHOLD_N => MHold_Out_N,
      Wr_Int_N => Wr_Int_N,
      MECRegister_Access => MECRegister_Access,
      RomCS_Out_N => RomCS_Out_N
      ) ;

 Memory_Buffer: Mem_Buffer
    generic map(
      t8 => t8 ,
      t9 => t9 ,
      t10 => t10 ,
      t13 => t13 ,
      t14 => t14 ,
      t15 => t15 ,
      t16 => t16 ,
      t17 => t17 ,
      t74 => t74 ,
      t75 => t75 ,
      CHECK => CHECK
      )
    port map(
      BA => BA,
      RAMBEn_N => RAMBEn_N,
      ROMBEn_N => ROMBEn_N,
      MemBEn_N => MemBEn_N,
      MemWr2_N => MemWr2_N,
      MemWr1_N => MemWr1_N,
      OE_N => OE_N,
      DDir => DDir,
      DDir_N => DDir_N,
      MemCS_N => MemCS_N,
      RomCS_N => RomCS_N,
      ALE_N => ALE_N,
      BA_Out => BA_Out,
      MemBEn_Out_N => MemBEn_Out_N,
      RAMBEn_Out_N => RAMBEn_Out_N,
      ROMBEn_Out_N => ROMBEn_Out_N,
      CBWr_Out_N => CBWR_Out_N,
      MemWr_Out_N => MemWR_Out_N,
      OE_Out_N => OE_Out_N,
      DDir_Out => DDir_Out_N,
      MemCS_Out_N => MemCS_Out_N,
      RomCS_Out_N => RomCS_Out_N,
      ALE_Out_N => ALE_Out_N,
      Clk_Out_Int => Clk_Out_Int,
      Clk_Out => Clk_Out,
      A_In => A_In
      ) ;

 IOPort_Buffer: IO_Buffer
    generic map(
      t13 => t13 ,
      t15 => t15 ,
      t18 => t18 ,
      t9 => t9 ,
      t10 => t10 ,
      t11 => t11 ,
      t12 => t12 ,
      t74 => t74 ,
      t75 => t75 ,
      CHECK => CHECK )
    port map(
      ExMCS_N => ExMCS_N,
      IOBEn_N => IOBEn_N,
      IOWR_N => IOWR_N,
      IOSel_N => IOSel_N,
      ExMCS_Out_N => ExMCS_Out_N,
      IOBEn_Out_N => IOBEn_Out_N,
      IOWR_Out_N => IOWR_Out_N,
      IOSel_Out_N => IOSel_Out_N,
      Clk_Out_Int => Clk_Out_Int,
      Clk_Out => Clk_Out,
      A_In => A_In
      ) ;

 Ext_Interrupt_Buffer: ExternalInterrupt_Buffer
    generic map(
      t44 => t44 ,
      t45 => t45 ,
      t43 => t43 ,
      t74 => t74 ,
      t75 => t75 ,
      CHECK => CHECK )
    port map(
      ExtInt => ExtInt,
      ExtIntAck => ExtIntAck,
      ExtInt_In => ExtInt_In,
      ExtIntAck_Out => ExtIntAck_Out,
      Clk_Out_Int => Clk_Out_Int,
      Clk_Out => Clk_Out
      ) ;

END Schematic ; -- of MECGEN 


















---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      MEC Generic component
-- File name:                  mec.vhd
-- VHDL unit:                  MEmoryController
-- Purpose and functionality:  MEC description
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--

library MECLibrary;
use MECLibrary.all;
use MECLibrary.MECPackage.all;

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;
use MMS.StdSim.all;
use MMS.StdTiming.all;
use MMS.StdIoImp.all;
 
------------------------------------------------------------------
-- Declaration of 'MEC'.
------------------------------------------------------------------

ENTITY MEC IS
  GENERIC(
          T      : temperature := T_BOARD;
          V      : voltage     := V_BOARD;
          PROCES : proces_type := PROCES_BOARD;
          LOAD   : capacitance := LOAD_BOARD;
          CHECK  : Boolean     := False
          ) ;
    port ( 
     A       : in    Std_Logic_Vector(31 downto 0);
	   APar    : in    Std_Logic;
	   ASI     : in    Std_Logic_Vector(3 downto 0);
	   Size    : in    Std_Logic_Vector(1 downto 0);
	   ASPar   : in    Std_Logic;
	   D       : inout Std_Logic_Vector(31 downto 0);
	   DParIO  : inout Std_Logic;
	   DMAAS   : in    Std_Logic;
	   DRdy_N  : out   Std_Logic;
	   Inull   : in    Std_Logic;
	   DXfer   : in    Std_Logic;
	   LdSto   : in    Std_Logic;
	   Lock    : in    Std_Logic;
	   Rd      : in    Std_Logic;
	   WE_N    : in    Std_Logic;
	   Wrt     : in    Std_Logic;
	   IMPar   : in    Std_Logic;
	   ExtHold_N : in  Std_Logic;
	   ExtCCV  : in    Std_Logic;
	   AOE_N   : out   Std_Logic;
	   COE_N   : out   Std_Logic;
	   DOE_N   : out   Std_Logic;
	   BHold_N : out   Std_Logic;
	   MDS_N   : out   Std_Logic;
	   MExc_N  : out   Std_Logic;
	   MHold_N : out   Std_Logic;
	   BA      : out   Std_Logic_Vector(1 downto 0);
	   CB      : inout Std_Logic_Vector(6 downto 0);
	   ALE_N   : out   Std_Logic;
	   Prom8_N : in    Std_Logic;
	   RomCS_N : out   Std_Logic;
	   MemCS_N : out   Std_Logic_Vector(9 downto 0);
     OE_N     : out  Std_Logic_Vector(1 downto 0);
     MemWr1_N : out  Std_Logic_Vector(1 downto 0);
     MemWr2_N : out  Std_Logic_Vector(1 downto 0);
     DDir     : out  Std_Logic;
     DDir_N   : out  Std_Logic;
     RAMBEn_N : out  Std_Logic;
     ROMBEn_N : out  Std_Logic;
     MemBEn_N : out  Std_Logic;
	   IOSel_N  : out  Std_Logic_Vector(3 downto 0);
	   IOWr_N   : out  Std_Logic;
	   IOBEn_N  : out  Std_Logic;
	   ExMCS_N  : out  Std_Logic;
	   BusRdy_N : in   Std_Logic;
	   BusErr_N : in   Std_Logic;
	   DMAReq_N : in   Std_Logic;
	   DMAGnt_N : out  Std_Logic;
	   IRL      : out  Std_Logic_Vector(3 downto 0);
	   IntAck   : in   Std_Logic;
	   ExtInt   : in   Std_Logic_Vector(4 downto 0);
	   ExtIntAck   :   out Std_Logic;
	   SysReset_N  :   in Std_Logic;
	   Reset_N     :   out Std_Logic;
	   IUErr_N     :   in Std_Logic;
	   IUHWErr_N   :   in Std_Logic;
	   IUCmpErr_N  :   in Std_Logic;
	   FPUHWErr_N  :   in Std_Logic;
	   FPUCmpErr_N :   in Std_Logic;
	   MecHWErr_N  :   out Std_Logic;
	   SysErr_N    :   out Std_Logic;
	   SysAv       :   out Std_Logic;
	   SysHalt_N   :   in Std_Logic;
	   CPUHalt_N   :   out Std_Logic;
	   NoPar_N     :   in Std_Logic;
	   RomWrt_N    :   in Std_Logic;
	   TCk         :   in Std_Logic;
	   TRst_N      :   in Std_Logic;
	   TMS         :   in Std_Logic;
	   TDI         :   in Std_Logic;
	   TDO         :   out Std_Logic;
	   WDClk       :   in Std_Logic;
	   RxA         :   in Std_Logic;
	   RxB         :   in Std_Logic;
	   TxA         :   out Std_Logic;
	   TxB         :   out Std_Logic;
	   Clk2        :   in Std_Logic;
     SClk        :   inout Std_Logic_Vector(1 downto 0)
     );
end MEC;

------------------------------------------------------------------
-- Architectural body of 'MEC'.
------------------------------------------------------------------

ARCHITECTURE Schematic OF MEC IS  -- architecture WithTiming of MEC is
  constant MHS_MC : technology := (
     (0.9375, 0.0025, 0.0, 0.0), -- real values
     (2.0, -0.20, 0.0, 0.0),                    
     0.8, 1.3,                                  
     47.0);                                     
   
  constant TP : tp_array(1 to 69) := 
   GetTiming("t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 " & 
             "t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 " &
             "t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40 " &
             "t42 t43 t44 t45 t46 t47 t48 t49 t50 t51 t53 t54 " &
             "t55 t56 t57 t58 t59 t60 t61 t62 t63 t66 " &
             "t68 t69 t70 t71 t73 t74 t75 ", 69,
              "MECNom.tim");

  signal SClkL : std_logic_vector(1 downto 0);  -- Fix for CLK2/CLK delay
  constant t69L : time := CalcDelay(TP(63), MHS_MC, T, V, PROCES, LOAD);


-- for all: MEmoryControllerGeneric use entity MecLibrary.MECGen;


 COMPONENT MECGEN
  GENERIC(
    CHECK  : Boolean     := False ;
    t1   : Time := 44 ns ;    -- Clk period
    t2   : Time := 10 ns ;    -- Clk2 high to low/ low to high
    t3   : Time := 22 ns ;    -- Clk2 period time 2-44 MHz in steps of 2MHz
    t4   : Time := 8 ns ;     -- Address Input Setup Time, ref edge Clk+
    t5   : Time := 4 ns ;     -- Address Input Hold Time, ref edge Clk+
    t6   : Time := 8 ns ;     -- ASI(7:0), Size(1:0), RD, WRT, WE*, LOCK,
                              -- DMMAS Input Setup Time, ref edge Clk+
    t7   : Time := 4 ns ;     -- ASI(7:0), Size(1:0), RD, WRT, WE*, LOCK,
                              -- DMMAS Input Hold Time, ref edge Clk+
    t8   : Time := 8 ns ;     -- ALE_N, ref edge Clk-
    t9   : Time := 15 ns ;    -- MEC Chip Select Generation, ref Address Valid
    t10  : Time := 5 ns ;     -- MEC CS_N Clock to Output delay Time, ref edge Clk+
    t11  : Time := 1 ns ;     -- MEC CS_N Hold Time, ref edge Clk+
    t12  : Time := 8 ns ;     -- MEC CS_N Output Latch propagation delay Time, 
                              -- ref MEC CS_N Generated
    t13  : Time := 8 ns ;     -- xBEn_N Output Delay Time, ref Clk-
    t14  : Time := 8 ns ;     -- DDir Output Delay Time, ref Clk2-
    t15  : Time := 8 ns ;     -- xWR_N High to Low Output Delay, ref Clk-
    t16  : Time := 8 ns ;     -- OE_N Output Delay Time, ref Clk2
    t17  : Time := 8 ns ;     -- BA Valid, ref Clk+
    t18  : Time := 8 ns ;     -- IOSEL Valid, ref Clk+
    t19  : Time := 25 ns ;    -- Data Write Setup Time to check parity on Data bus 
                              -- before CLK-(Store memory), ref edge Clk-
    t20  : Time := 1 ns ;     -- Data Hold Time, ref edge Clk+
    t21  : Time := 10 ns ;    -- MEC Data Output Delay Time, ref edge Clk+
    t22  : Time := 4 ns ;     -- MEC Data Hold Time, ref edge Clk+
    t23  : Time := 28 ns ;    -- CB Generation (Valid Data Latched on Clk+), ref edge Clk+
    t24  : Time := 1 ns ;     -- CB Hold Time, ref edge Clk+
    t25  : Time := 8 ns ;     -- MExc_N Output Delay Time, ref Clk-
    t26  : Time := 8 ns ;     -- xHold_N Output Delay Time, ref Clk+
    t27  : Time := 8 ns ;     -- AOE_N, COE_N, DOE_N, TOE_N Output Delay, ref Clk+
    t28  : Time := 8 ns ;     -- inST, inTACK Input Setup Time, ref edge Clk+
    t29  : Time := 4 ns ;     -- inST, inTACK Input Hold Time, ref edge Clk+
    t30  : Time := 15 ns ;    -- SysReset_N, SysHalt_N  Input Setup Time, ref edge Clk+
    t31  : Time := 4 ns ;     -- SysReset_N, SysHalt_N  Input Hold Time, ref edge Clk+
    t32  : Time := 4 ns ;     -- Reset_N, CPUHalt_N  Output Delay Time, ref edge Clk2
    t33  : Time := 8 ns ;     -- DXfer Input Setup Time, ref edge Clk+
    t34  : Time := 2 ns ;     -- DXfer Input Hold Time, ref edge Clk+
    t35  : Time := 8 ns ;     -- inull Input Setup Time, ref edge Clk+
    t36  : Time := 4 ns ;     -- inull Input Hold Time, ref edge Clk+
    t37  : Time := 8 ns ;     -- MDS_N, DRDY_N Output Delay Time, ref edge Clk+
    t38  : Time := 15 ns ;    -- BusRdy_N, BusErr_N Input Setup Time, ref edge Clk+
    t39  : Time := 4 ns ;     -- BusRdy_N, BusErr_N Input Hold Time, ref edge Clk+
    t40  : Time := 15 ns ;    -- DMAReq_N Input Setup Time, ref edge Clk-
    t42  : Time := 4 ns ;     -- DMAGnt_N Output Delay Time, ref edge Clk+
    t43  : Time := 8 ns ;     -- IRL(3:0), EXTIntack Output Delay Time, ref edge Clk-          
    t44  : Time := 8 ns ;     -- EXTInt(4:0) Input Setup Time, ref edge Clk+
    t45  : Time := 4 ns ;     -- EXTInt(4:0) Input Hold Time, ref edge Clk+
    t46  : Time := 8 ns ;     -- xERR_N Input Setup Time, ref edge Clk+
    t47  : Time := 4 ns ;     -- xERR_N Input Hold Time, ref edge Clk+
    t48  : Time := 8 ns ;     -- xERR_N Output Delay Time, ref edge Clk+
    t49  : Time := 8 ns ;     -- CMode_N, NoPar_N, RomWrt_N, Prom8_N
                              -- Input Setup Time, ref edge Clk+     
    t50  : Time := 4 ns ;     -- CMode_N, NoPar_N, RomWrt_N, Prom8_N
                              -- Input Hold Time, ref edge Clk+
    t51  : Time := 8 ns ;     -- DParI to DPARIO Output Internal Delay Time, 
                              -- ref DParI Valid
    t53  : Time := 8 ns ;     -- DParI, DParIO, APar, ASPar, IMPar
                              -- Input Setup Time, ref edge Clk+
    t54  : Time := 2 ns ;     -- DParI, DParIO, APar, ASPar, IMPar
                              -- Input Hold Time, ref edge Clk+
    t55  : Time := 1000 ns ;  -- TCk Cycle Time
    t56  : Time := 8 ns ;     -- TRst_N Input Setup Time, ref edge TCk+
    t57  : Time := 4 ns ;     -- TRst_N Input Hold Time, ref edge TCk+
    t58  : Time := 8 ns ;     -- TMS Input Setup Time, ref edge TCk+
    t59  : Time := 4 ns ;     -- TMS Input Hold Time, ref edge TCk+
    t60  : Time := 8 ns ;     -- TDI Input Setup Time, ref edge TCk+
    t61  : Time := 4 ns ;     -- TDI Input Hold Time, ref edge TCk+
    t62  : Time := 8 ns ;     -- TDO Output Delay Time, ref edge TCk+
    t63  : Time := 1000 ns ;  -- WDClk Clock Period time
                              -- before next Clk-
    t66  : Time := 20 ns ;    -- DPARIO Generation Time (Including Output
                              -- Delay Time), ref Data Valid
    t68  : Time := 20 ns ;    -- ExtHold, ExtCCV set up
    t69  : Time := 3 ns ;     -- Clk2 to Clk Low to High Output 
                              -- Delay Time, ref edge Clk2+
    t70  : Time := 3 ns ;     -- Clk2 to Clk High to Low Output 
                              -- Delay Time, ref edge Clk2+
    t71  : Time := 4 ns ;     -- Data read setup time for latches, ref edge Clk+
    t73  : Time := 35 ns ;    -- 
                              -- 
    t74  : Time := 4 ns;      -- MEC Output comparision input setup time
    t75  : Time := 4 ns) ;    -- MEC Output comparision input hold  time
  PORT(
    CB           : inout  Std_Logic_Vector(6 downto 0) ;
    BusRdy_N     : in     Std_Logic ;
    BusErr_N     : in     Std_Logic ;
    ExtInt       : in     Std_Logic_Vector(4 downto 0) ;
    ASPar        : in     Std_Logic ;
    Size         : in     Std_Logic_Vector(1 downto 0) ;
    ASI          : in     Std_Logic_Vector(3 downto 0) ;
    APar         : in     Std_Logic ;
    A            : in     Std_Logic_Vector(31 downto 0) ;
    TDI          : in     Std_Logic ;
    TCk          : in     Std_Logic ;
    TRst_N       : in     Std_Logic ;
    TMS          : in     Std_Logic ;
    IMPar        : in     Std_Logic ;
    ExtHold_N    : in     Std_Logic ;
    ExtCCV       : in     Std_Logic ;
    Wrt          : in     Std_Logic ;
    Rd           : in     Std_Logic ;
    Lock         : in     Std_Logic ;
    LdSto        : in     Std_Logic ;
    DXfer        : in     Std_Logic ;
    inull        : in     Std_Logic ;
    DMAAS        : in     Std_Logic ;
    IUErr_N      : in     Std_Logic ;
    IUHWErr_N    : in     Std_Logic ;
    FPUHWErr_N   : in     Std_Logic ;
    IUCmpErr_N   : in     Std_Logic ;
    FPUCmpErr_N  : in     Std_Logic ;
    SysHalt_N    : in     Std_Logic ;
    WDClk        : in     Std_Logic ;
    DRdy_N       : out    Std_Logic ;
    DParIO       : inout  Std_Logic ;
    D            : inout  Std_Logic_Vector(31 downto 0) ;
    TDO          : out    Std_Logic ;
    Clk          : out    Std_Logic_Vector(1 downto 0) ;
    AOE_N        : out    Std_Logic ;
    COE_N        : out    Std_Logic ;
    DOE_N        : out    Std_Logic ;
    BHold_N      : out    Std_Logic ;
    MDS_N        : out    Std_Logic ;
    MExc_N       : out    Std_Logic ;
    MHold_N      : out    Std_Logic ;
    BA           : out    Std_Logic_Vector(1 downto 0) ;
    DMAGnt_N     : out    Std_Logic ;
    ExtIntAck    : out    Std_Logic ;
    IRL          : out    Std_Logic_Vector(3 downto 0) ;
    CPUHalt_N    : out    Std_Logic ;
    TxB          : out    Std_Logic ;
    TxA          : out    Std_Logic ;
    Clk2         : in     Std_Logic ;
    SysReset_N   : in     Std_Logic ;
    Reset_N      : out    Std_Logic ;
    ExMCS_N      : out    Std_Logic ;
    IOBEn_N      : out    Std_Logic ;
    IOWR_N       : out    Std_Logic ;
    IOSel_N      : out    Std_Logic_Vector(3 downto 0) ;
    
    RAMBEn_N     : out    Std_Logic;
    ROMBEn_N     : out    Std_Logic;
    MemBEn_N     : out    Std_Logic;
    MemWr2_N     : out    Std_Logic_Vector(1 downto 0);
    MemWr1_N     : out    Std_Logic_Vector(1 downto 0);
    OE_N         : out    Std_Logic_Vector(1 downto 0);
    DDir         : out    Std_Logic ;
    DDir_N       : out    Std_Logic;
    
    DMAReq_N     : in     Std_Logic ;
    WE_N         : in     Std_Logic ;
    Prom8_N      : in     Std_Logic ;
    NoPar_N      : in     Std_Logic ;
    RomWrt_N     : in     Std_Logic ;
    MecHWErr_N   : out    Std_Logic ;
    SysErr_N     : out    Std_Logic ;
    SysAv        : out    Std_Logic ;
    MemCS_N      : out    Std_Logic_Vector(9 downto 0) ;
    RomCS_N      : out    Std_Logic ;
    inTAck       : in     Std_Logic ;
    ALE_N        : out    Std_Logic ;
    RxA          : in     Std_Logic ;
    RxB          : in     Std_Logic ) ;
 END COMPONENT ;

BEGin

 MECGENeric: MECGEN
    GENERIC MAP(
      CHECK => CHECK ,
      t1    => TP(1),
      t2    => TP(2),
      t3    => TP(3),
      t4    => CalcDelay(TP(4), MHS_MC, T, V, PROCES),
      t5    => CalcDelay(TP(5), MHS_MC, T, V, PROCES),
      t6    => CalcDelay(TP(6), MHS_MC, T, V, PROCES),
      t7    => CalcDelay(TP(7), MHS_MC, T, V, PROCES),
      t8    => CalcDelay(TP(8), MHS_MC, T, V, PROCES, LOAD),
      t9    => CalcDelay(TP(9), MHS_MC, T, V, PROCES),
      t10   => CalcDelay(TP(10), MHS_MC, T, V, PROCES, LOAD),
      t11   => CalcDelay(TP(11), MHS_MC, T, V, PROCES),
      t12   => CalcDelay(TP(12), MHS_MC, T, V, PROCES, LOAD),
      t13   => CalcDelay(TP(13), MHS_MC, T, V, PROCES, LOAD),
      t14   => CalcDelay(TP(14), MHS_MC, T, V, PROCES, LOAD),
      t15   => CalcDelay(TP(15), MHS_MC, T, V, PROCES, LOAD),
      t16   => CalcDelay(TP(16), MHS_MC, T, V, PROCES, LOAD),
      t17   => CalcDelay(TP(17), MHS_MC, T, V, PROCES, LOAD),
      t18   => CalcDelay(TP(18), MHS_MC, T, V, PROCES, LOAD),
      t19   => CalcDelay(TP(19), MHS_MC, T, V, PROCES),
      t20   => CalcDelay(TP(20), MHS_MC, T, V, PROCES),
      t21   => CalcDelay(TP(21), MHS_MC, T, V, PROCES, LOAD),
      t22   => CalcDelay(TP(22), MHS_MC, T, V, PROCES),
      t23   => CalcDelay(TP(23), MHS_MC, T, V, PROCES),
      t24   => CalcDelay(TP(24), MHS_MC, T, V, PROCES),
      t25   => CalcDelay(TP(25), MHS_MC, T, V, PROCES, LOAD),
      t26   => CalcDelay(TP(26), MHS_MC, T, V, PROCES, LOAD),
      t27   => CalcDelay(TP(27), MHS_MC, T, V, PROCES, LOAD),
      t28   => CalcDelay(TP(28), MHS_MC, T, V, PROCES),
      t29   => CalcDelay(TP(29), MHS_MC, T, V, PROCES),
      t30   => CalcDelay(TP(30), MHS_MC, T, V, PROCES),
      t31   => CalcDelay(TP(31), MHS_MC, T, V, PROCES),
      t32   => CalcDelay(TP(32), MHS_MC, T, V, PROCES, LOAD),
      t33   => CalcDelay(TP(33), MHS_MC, T, V, PROCES),
      t34   => CalcDelay(TP(34), MHS_MC, T, V, PROCES),
      t35   => CalcDelay(TP(35), MHS_MC, T, V, PROCES),
      t36   => CalcDelay(TP(36), MHS_MC, T, V, PROCES),
      t37   => CalcDelay(TP(37), MHS_MC, T, V, PROCES, LOAD),
      t38   => CalcDelay(TP(38), MHS_MC, T, V, PROCES),
      t39   => CalcDelay(TP(39), MHS_MC, T, V, PROCES),
      t40   => CalcDelay(TP(40), MHS_MC, T, V, PROCES),
      t42   => CalcDelay(TP(41), MHS_MC, T, V, PROCES, LOAD),
      t43   => CalcDelay(TP(42), MHS_MC, T, V, PROCES, LOAD),
      t44   => CalcDelay(TP(43), MHS_MC, T, V, PROCES),
      t45   => CalcDelay(TP(44), MHS_MC, T, V, PROCES),
      t46   => CalcDelay(TP(45), MHS_MC, T, V, PROCES),
      t47   => CalcDelay(TP(46), MHS_MC, T, V, PROCES),
      t48   => CalcDelay(TP(47), MHS_MC, T, V, PROCES, LOAD),
      t49   => CalcDelay(TP(48), MHS_MC, T, V, PROCES),
      t50   => CalcDelay(TP(49), MHS_MC, T, V, PROCES),
      t51   => CalcDelay(TP(50), MHS_MC, T, V, PROCES, LOAD),
      t53   => CalcDelay(TP(51), MHS_MC, T, V, PROCES),
      t54   => CalcDelay(TP(52), MHS_MC, T, V, PROCES),
      t55   => TP(53),
      t56   => CalcDelay(TP(54), MHS_MC, T, V, PROCES),
      t57   => CalcDelay(TP(55), MHS_MC, T, V, PROCES),
      t58   => CalcDelay(TP(56), MHS_MC, T, V, PROCES),
      t59   => CalcDelay(TP(57), MHS_MC, T, V, PROCES),
      t60   => CalcDelay(TP(58), MHS_MC, T, V, PROCES),
      t61   => CalcDelay(TP(59), MHS_MC, T, V, PROCES),
      t62   => CalcDelay(TP(60), MHS_MC, T, V, PROCES, LOAD),
      t63   => TP(61),
      t66   => CalcDelay(TP(62), MHS_MC, T, V, PROCES),
      t68   => CalcDelay(TP(63), MHS_MC, T, V, PROCES),
      t69   => CalcDelay(TP(63), MHS_MC, T, V, PROCES, LOAD),
      t70   => CalcDelay(TP(64), MHS_MC, T, V, PROCES, LOAD),
      t71   => CalcDelay(TP(65), MHS_MC, T, V, PROCES),
      t73   => CalcDelay(TP(66), MHS_MC, T, V, PROCES, LOAD),
      t74   => CalcDelay(TP(68), MHS_MC, T, V, PROCES),
      t75   => CalcDelay(TP(69), MHS_MC, T, V, PROCES)
      )
    PORT MAP(
      CB => CB,
      BusRdy_N => BusRdy_N,
      BusErr_N => BusErr_N,
      ExtInt => ExtInt,
      ASPar => ASPar,
      Size => Size,
      ASI => ASI,
      APar => APar,
      A => A,
      TDI => TDI,
      TCk => TCk,
      TRst_N => TRst_N,
      TMS => TMS,
      IMPar => IMPar,
      ExtHold_N => ExtHold_N,
      ExtCCV => ExtCCV,
      Wrt => Wrt,
      Rd => Rd,
      Lock => Lock,
      LdSto => LdSto,
      DXfer => DXfer,
      inull => inull,
      DMAAS => DMAAS,
      IUErr_N => IUErr_N,
      IUHWErr_N => IUHWErr_N,
      FPUHWErr_N => FPUHWErr_N,
      IUCmpErr_N => IUCmpErr_N,
      FPUCmpErr_N => FPUCmpErr_N,
      SysHalt_N => SysHalt_N,
      WDClk => WDClk,
      DRdy_N => DRdy_N,
      DParIO => DParIO,
      D => D,
      TDO => TDO,
      Clk => SClk,
      AOE_N => AOE_N,
      COE_N => COE_N,
      DOE_N => DOE_N,
      BHold_N => BHold_N,
      MDS_N => MDS_N,
      MExc_N => MExc_N,
      MHold_N => MHold_N,
      BA => BA,
      DMAGnt_N => DMAGnt_N,
      ExtIntAck => ExtIntAck,
      IRL => IRL,
      CPUHalt_N => CPUHalt_N,
      TxB => TxB,
      TxA => TxA,
      Clk2 => Clk2,
      SysReset_N => SysReset_N,
      Reset_N => Reset_N,
      ExMCS_N => ExMCS_N,
      IOBEn_N => IOBEn_N,
      IOWR_N => IOWR_N,
      IOSel_N => IOSel_N,
      RAMBEn_N => RAMBEn_N,
      ROMBEn_N => ROMBEn_N,
      MemBEn_N => MemBEn_N,
      MemWr2_N => MemWr2_N,
      MemWr1_N => MemWr1_N,
      OE_N => OE_N,
      DDir => DDir,
      DDir_N => DDir_N,
      DMAReq_N => DMAReq_N,
      WE_N => WE_N,
      Prom8_N => Prom8_N,
      NoPar_N => NoPar_N,
      RomWrt_N => RomWrt_N,
      MecHWErr_N => MecHWErr_N,
      SysErr_N => SysErr_N,
      SysAv => SysAv,
      MemCS_N => MemCS_N,
      RomCS_N => RomCS_N,
      inTAck => IntAck,
      ALE_N => ALE_N,
      RxA => RxA,
      RxB => RxB ) ;

--      SClk <= SClkL after T69L;

END Schematic ; -- of MEC 
---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      MEC Configuration 
-- File name:                  \mec\source\mecconf.vhd
-- VHDL unit:                  MECConfiguration
-- Purpose and functionality:  
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--
library MECLibrary;
use MECLibrary.all;

configuration MECConfiguration of MEC is
  for Schematic
   
    
    for all: MECGen 
      use entity MECLibrary.MECGen(Schematic);
      for Schematic 

--------------------------------------------------------------------------------
--MEC Functionallity
--------------------------------------------------------------------------------
        for all: MECFunc 
          use entity MECLibrary.MECFunc(Mini_Spec);
          for Mini_Spec

--------------------------------------------------------------------------------
--UARTS
--------------------------------------------------------------------------------
            for all: uarts 
              use entity MECLibrary.uarts(mini_spec);
              for Mini_Spec
                for all: uartcontrol
                  use entity MECLibrary.uartcontrol(mini_spec);
                end for;
                for all: uart
                  use entity MECLibrary.uart(VHDL_RTL);
                end for;
              end for;                            
            end for;

--------------------------------------------------------------------------------
--Timers
--------------------------------------------------------------------------------
            for all: timers
              use entity MECLibrary.timers(mini_spec);
              for Mini_Spec
                for all: timercontrol
                  use entity MECLibrary.timercontrol(mini_spec);
                end for;
                for all: rtctimer
                  use entity MECLibrary.rtctimer(mini_spec);
                end for;
                for all: genpurptimer
                  use entity MECLibrary.genpurptimer(mini_spec);
                end for;
              end for;
            end for;

--------------------------------------------------------------------------------
--Test and Debug
--------------------------------------------------------------------------------
            for all: tap
              use entity MECLibrary.tap(mini_spec);
            end for;

--------------------------------------------------------------------------------
--System Bus Interface
--------------------------------------------------------------------------------
            for all: systembusinterface
              use entity MECLibrary.systembusinterface(mini_spec);
              for Mini_Spec
                for all: addressdecoder
                  use entity MECLibrary.addressdecoder(mini_spec);
                     for Mini_Spec
                        for all: LBuff
                           use entity MECLibrary.LBuff(mini_spec);
                        end for;
                     end for;
                end for;
                for all: mec_latch_e
                  use entity MECLibrary.mec_latch_e(mini_spec);
                end for;
                for all: busarbiter
                  use entity MECLibrary.busarbiter(mini_spec);
                end for;
                for all: edac
                  use entity MECLibrary.edac(mini_spec);
                end for;
                for all: accesscontrol
                  use entity MECLibrary.accesscontrol(mini_spec);
                     for Mini_Spec
                        for all: LBuff
                           use entity MECLibrary.LBuff(mini_spec);
                        end for;
                        for all: AccessControl_fsm
                           use entity MECLibrary.AccessControl_fsm(mini_spec);
                        end for;
                     end for;
                end for;
                for all: mem_io_config_e
                  use entity MECLibrary.mem_io_config_e(mini_spec);
                end for;
              end for;
            end for;


--------------------------------------------------------------------------------
--MEC Control and Support Functions
--------------------------------------------------------------------------------
            for all: meccontrolandsupportfunctions
            use entity MECLibrary.meccontrolandsupportfunctions(mini_spec);
              for Mini_Spec
                for all: systemclocks
                  use entity MECLibrary.systemclocks(mini_spec);
                end for;
                for all: startupctlandres
                  use entity MECLibrary.startupctlandres(mini_spec);
                end for;
                for all: powerdownmodectl
                  use entity MECLibrary.powerdownmodectl(mini_spec);
                end for;
              end for;
            end for;

--------------------------------------------------------------------------------
--Interrupt and Errorhandling
--------------------------------------------------------------------------------
            for all: interruptanderrorhandling
            use entity MECLibrary.interruptanderrorhandling(mini_spec);
              for Mini_Spec
                for all: fault_handler
                  use entity MECLibrary.fault_handler(mini_spec);
                end for;

                for all: watchdog
                  use entity MECLibrary.watchdog(mini_spec);
                end for;

                for all: int_handler
                  use entity MECLibrary.int_handler(mini_spec);
                end for;

                for all: error_handler
                  use entity MECLibrary.error_handler(mini_spec);
                end for;
              end for;
            end for;

--------------------------------------------------------------------------------
--Data Mux
--------------------------------------------------------------------------------
            for all: datamux
              use entity MECLibrary.datamux(mini_spec);
            end for;
          end for;     --End MECFunc(Mini_Spec)
        end for;       --End MECFunc 


--------------------------------------------------------------------------------
--Buffers
--------------------------------------------------------------------------------
        for all: clk_buffer 
          use entity MECLibrary.clk_buffer(clk_buffer_body);
        end for;

        for all: test_buffer 
          use entity MECLibrary.test_buffer(test_buffer_body);
        end for;

        for all: iu_in_buffer 
          use entity MECLibrary.iu_in_buffer(iu_in_buffer_body);
        end for; 

        for all: setupandcheck_buffer 
          use entity MECLibrary.setupandcheck_buffer(setupandcheck_buffer_body);
        end for;

        for all: dma_buffer 
          use entity MECLibrary.dma_buffer(dma_buffer_body);
        end for;

        for all: checkbits_buffer 
          use entity MECLibrary.checkbits_buffer(checkbits_buffer_body);
        end for;

        for all: systemerror_buffer 
          use entity MECLibrary.systemerror_buffer(systemerror_buffer_body);
        end for;

        for all: serial_buffer 
          use entity MECLibrary.serial_buffer(serial_buffer_body);
        end for;

        for all: sysctrl_buffer 
          use entity MECLibrary.sysctrl_buffer(sysctrl_buffer_body);
        end for;

        for all: iu_out_buffer 
          use entity MECLibrary.iu_out_buffer(iu_out_buffer_body);
        end for;

        for all: data_buffer 
          use entity MECLibrary.data_buffer(data_buffer_body);
        end for;

        for all: mem_buffer 
          use entity MECLibrary.mem_buffer(mem_buffer_body);
        end for;

        for all: io_buffer 
          use entity MECLibrary.io_buffer(io_buffer_body);
        end for;

        for all: externalinterrupt_buffer 
          use entity MECLibrary.externalinterrupt_buffer(externalinterrupt_buffer_body);
        end for;




      end for;         --End MECGen(Schematic)
    end for;           --End MECGen
  end for;             --End MEC(Schematic)
end MECConfiguration;  --End MEC configuration
  
  
  
  
  
  
---------------------------------------------------------------------------
--                Copyright MATRA MARCONI SPACE FRANCE                   --
---------------------------------------------------------------------------
--  The ownership and copyright of this document belong to               --
--  MATRA MARCONI SPACE FRANCE and it must not be disclosed, copied,     --
--  altered or used without the written                                  --
--  permission of MATRA MARCONI SPACE FRANCE.                            --
---------------------------------------------------------------------------
-- Title:                      MEC Generic component
-- File name:                  mec_pck.vhd
-- VHDL unit:                  MEmoryController
-- Purpose and functionality:  Pinout definition
-- Reference:                  (RDx)
-- Analysis Dependencies:      (N/A)
-- Limitations:                (N/A)
-- Fidelity:                   (N/A)
-- Discrepancies:              (N/A)
-- Usage:                      (N/A)
-- I/O:                        (N/A)
-- Operations:                 (N/A)
-- Assertions:                 (N/A)
-- Development Platform:       (N/A)
-- Analyzer:                   (No Dependencies)
-- Synthesis:                  (No Dependencies)
---------------------------------------------------------------------------
-- Revision history: (all revisions included)                            --
---------------------------------------------------------------------------
-- Version No:    Author:            Modification Date:    Changes made: --
---------------------------------------------------------------------------
-- v1.0 Rev A    Remi CISSOU           1996-04-22           New issue
--
---------------------------------------------------------------------------
--


library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdSim.all;

use work.all;

package meccomppck is

  component mec
  GENERIC(
          T      : temperature := T_BOARD;
          V      : voltage     := V_BOARD;
          PROCES : proces_type := PROCES_BOARD;
          LOAD   : capacitance := LOAD_BOARD;
          CHECK  : Boolean     := False
          ) ;
    port ( 
     A       : in Std_Logic_Vector(31 downto 0);
	   APar    : in Std_Logic;
	   ASI     : in Std_Logic_Vector(3 downto 0);
	   Size    : in Std_Logic_Vector(1 downto 0);
	   ASPar   : in Std_Logic;
	   D       : inout Std_Logic_Vector(31 downto 0);
	   DParIO  : inout Std_Logic;
	   DMAAS   : in Std_Logic;
	   DRDY_N  : inout Std_Logic;
	   INull   : in Std_Logic;
	   DXfer   : in Std_Logic;
	   LdSto   : in Std_Logic;
	   Lock    : in Std_Logic;
	   Rd      : in Std_Logic;
	   WE_N    : in Std_Logic;
	   Wrt     : in Std_Logic;
	   IMPar   : in Std_Logic;
	   ExtHold_N : in Std_Logic;
	   ExtCCV  : in Std_Logic;
	   AOE_N   : inout Std_Logic;
	   COE_N   : inout Std_Logic;
	   DOE_N   : inout Std_Logic;
	   BHold_N : inout Std_Logic;
	   MDS_N   : inout Std_Logic;
	   MExc_N  : inout Std_Logic;
	   MHold_N : inout Std_Logic;
	   BA      : inout Std_Logic_Vector(1 downto 0);
	   CB      : inout Std_Logic_Vector(6 downto 0);
	   ALE_N   : inout Std_Logic;
	   Prom8_N : in Std_Logic;
	   RomCS_N : inout Std_Logic;
	   MemCS_N : inout Std_Logic_Vector(9 downto 0);
     OE_N     : inout Std_Logic_Vector(1 downto 0);
     MemWr1_N : inout Std_Logic_Vector(1 downto 0);
     MemWr2_N : inout Std_Logic_Vector(1 downto 0);
     DDir     : inout Std_Logic;
     DDir_N   : inout Std_Logic;
     RAMBEn_N : inout Std_Logic;
     ROMBEn_N : inout Std_Logic;
     MemBEn_N : inout Std_Logic;
	   IOSel_N  : inout Std_Logic_Vector(3 downto 0);
	   IOWr_N   : inout Std_Logic;
	   IOBEn_N  : inout Std_Logic;
	   ExMCS_N  : inout Std_Logic;
	   BusRdy_N : in Std_Logic;
	   BusErr_N : in Std_Logic;
	   DMAReq_N : in Std_Logic;
	   DMAGnt_N : inout Std_Logic;
	   IRL      : inout Std_Logic_Vector(3 downto 0);
	   IntAck   : in Std_Logic;
	   ExtInt   : in Std_Logic_Vector(4 downto 0);
	   ExtIntAck   : inout Std_Logic;
	   SysReset_N  : in Std_Logic;
	   Reset_N     : inout Std_Logic;
	   IUErr_N     : in Std_Logic;
	   IUHWErr_N   : in Std_Logic;
	   IUCmpErr_N  : in Std_Logic;
	   FPUHWErr_N  : in Std_Logic;
	   FPUCmpErr_N : in Std_Logic;
	   MecHWErr_N  : out Std_Logic;
	   SysErr_N    : inout Std_Logic;
	   SysAv       : inout Std_Logic;
	   SysHalt_N   : in Std_Logic;
	   CPUHalt_N   : inout Std_Logic;
	   NoPar_N     : in Std_Logic;
	   RomWrt_N    : in Std_Logic;
	   TCk         : in Std_Logic;
	   TRst_N      : in Std_Logic;
	   TMS         : in Std_Logic;
	   TDI         : in Std_Logic;
	   TDO         : out Std_Logic;
	   WDClk       : in Std_Logic;
	   RxA         : in Std_Logic;
	   RxB         : in Std_Logic;
	   TxA         : inout Std_Logic;
	   TxB         : inout Std_Logic;
	   Clk2        : in Std_Logic;
     SClk        : inout Std_Logic_Vector(1 downto 0)
     );
  end component;
  

end meccomppck;
