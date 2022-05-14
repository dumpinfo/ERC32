------------------------------------------------------------
-- File name : stdioimp.vhd
-- Title : StdIoImp
-- project : SPARC
-- Library : MMS
-- Author(s) : E. Laubacher
-- Purpose : package for improved IO declarations
-- notes : 
------------------------------------------------------------
-- Modification history :
------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
------------------------------------------------------------
-- v 1.0        | EL     | 92/06/14    | first version
--..........................................................
-- v 1.1        | MR     | 93/10/13    | 2nd version
-- + bug fix in functions ToString when the parameter is either
--   a bit_vector or a std_ulogic_vector or a std_logic_vector.
--   Error in the range declaration.
------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE

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
 
------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use std.textio.all;
package StdIoImp is

	subtype digit is character;	-- case insensitive ('a' = 'A', ... 'f' = 'F')
	subtype base_type is integer range 2 to 16;

	------------------------------------------------------------
	-- Conventions : 
	--   1/ A string is finished by NUL or last character
	--        following characters should not be considered
	--	 2/ Positions in a string refer to the range (1 to S'length)
	--   3/ Blank means ' ' or HT
	------------------------------------------------------------
	-- Function StrLen returns the length of string S
	-- Function StrEqu returns TRUE if S1 equal S2
	--   the comparison goes to the length of the longest string
	--   with N /= 0, the comparison is limited to N characters
	--  Ex : StrEqu("abc", "ab") -> FALSE
	--  Ex : StrEqu("abc", "ab", 2) -> TRUE
	--  Ex : StrEqu("abc", "abd", 2) -> TRUE
	--  Function StrNcEqu is similar but case insensitive
	------------------------------------------------------------

	function StrLen (S : string) return natural;
	function StrEqu (S1, S2 : string; N : natural := 0) return boolean;
	function StrNcEqu (S1, S2 : string; N : natural := 0) return boolean;

	------------------------------------------------------------
	-- Procedure Shift shifts left the string S for N characters,
	--   NUL characters <0> are used for filling
	-- Function UpTo returns the number of character up to 
	--   substring PATTERN included
	--   returns StrLen(S) if pattern not found
	--   the pattern "" is found at position 0
	--
	--  Ex : Shift("ABCDEF", 2)   -> "CDEF<0><0>"
	--  Ex : Shift("ABCDEF")   -> "BCDEF<0>"
	--  Ex : UpTo("ABCDEF", "CD")   ->  4
	--  Ex : UpTo("ABCDEF", "YZ")  -> 6
	--  Ex : UpTo("ABCDEF", "")  -> 0
	--  Ex : Shift("ABCDEF", UpTo("ABCDEF", "CD")) -> "EF<0><0><0><0>"
	------------------------------------------------------------

	procedure Shift(variable S : inout string; N : natural := 1);
	function UpTo(S : string; PATTERN : string) return integer;

	------------------------------------------------------------
	-- function WordLen returns the length of first word 
	-- (separator = blank or NUL)
	-- returns 0 if leading blank or empty string
	-- function NextWord returns the index of the next word in a string
	-- from a given position, the string being considered circular
	-- if S'length < INDEX, then behave as if INDEX = 1
	-- The condition StrLen(S) < INDEX is not verified
	--  test it before use to avoid this case if necessary
	-- for blank or empty string, return 0
	---------------

	function WordLen(S : string) return natural;
	function NextWord(S : string; INDEX : positive) return natural;

	------------------------------------------------------------
	-- Function DigitValue return digit value; 
	--	 assertion error if digit not in base BASE
	-- Function IsDigit returns TRUE iff VALUE is a digit in base BASE
	-- Function IsBlank returns TRUE iff string empty or blank
	-- Function LeadBlank returns the number of leading blanks
	--	for empty string, returns 0
	--  for blank string, returns StrLen(S)
	--  (use IsBlank to avoid these unpleasant cases)
	------------------------------------------------------------

	function DigitValue (VALUE : digit; BASE : base_type := 10) return integer;
	function IsDigit (VALUE : character; BASE : base_type := 10) return boolean;
	function IsBlank (S : string) return boolean;
	function LeadBlank (S : string) return integer;

	-----------------------------------------------------------------------
	-- Functions ToUpper return the upper-case equivalent of the parameter
	-- Functions ToLower return the lower-case equivalent of the parameter
	-- Constant ToDigit gives the equivalent upper-case digit of an integer
	-- Function Mirror reverts a string. Ex : ABCD -> DCBA
	--  Return parameter has same size than operand
	-----------------------------------------------------------------------
	function ToUpper (VALUE : character) return character;
	function ToUpper (S : string) return string;
	function ToLower (VALUE : character) return character;
	function ToLower (S : string) return string;
	function ToDigit (VALUE : natural) return character;
	function Mirror (S : string) return string;

	-----------------------------------------------------------------------
	-- Functions ToString return the string equivalent of VALUE
	-- Fhe size of the return string is adapted to VALUE
	-- For integer, real and time, a format can be given to control the result
	-- These functions are well suited for assertions and Print
	-----------------------------------------------------------------------

	function ToString (VALUE : integer; FORMAT : string := "%d") return string;
	-----------------------------------------------------------------------
	--	format (C-style)|	decimal	|	lower-case 	|		upper-case 	| binary	|
	--	format (C-style)|					|	 	hexa			|		 	hexa			|					|
	-----------------------------------------------------------------------
	--	automatic				|		%d		|			%x			|			%X				| 	%b		|
	--	minimum length	|		%3d		|			%3x			|			%3X				|		%3b		|
	--	fill with 0			|		%03d	|			%03x		|			%03X			|		%03b	|
	-----------------------------------------------------------------------	

	function ToString (VALUE : real; FORMAT : string := "%") return string;
	---------------------------------
	--	format (C-style)|						|
	---------------------------------
	--	exponent				|		%				|
	--	minimum length	|		%3			|
	--	radix length		|		%.2			|	-- 0 stands for exponent
	--	fill with 0			|		%03.2		|
	---------------------------------

	function ToString (VALUE : time; FORMAT : string := "% ns") return string;
	-----------------------------------
	--	format (C-style)|							|
	-----------------------------------
	--	automatic				|		% <unit>	|	 -- unit case insensitive
	--	minimum length	|		%3 ns			|
	-----------------------------------

	function ToString (VALUE : boolean) return string;
	function ToString (VALUE : bit) return string;
	function ToString (VALUE : std_ulogic) return string;
	function ToString (VALUE : bit_vector) return string;
	function ToString (VALUE : std_ulogic_vector) return string;
	function ToString (VALUE : std_logic_vector) return string;

	------------------------------------------------------------
	-- Functions FromString transform a string in a value
	-- For vectors, the size of the vector must be given
	-- For integer, real, time, boolean, the value read must
	--  be followed by end of string or blank
	-- Any error in the input provokes an assertion error
	-- These functions are suited to read a value in a file along with Scan
	------------------------------------------------------------
	function FromString (S : string; BASE : base_type := 10) return integer;
	function FromString (S : string) return real;
	function FromString (S : string) return time;
	function FromString (S : string) return boolean;	-- case insensitive
	function FromString (S : string) return bit;
	function FromString (S : string) return std_ulogic;
	function FromString (S : string; N : natural) return bit_vector;
	function FromString (S : string; N : natural) return std_ulogic_vector;
	function FromString (S : string; N : natural) return std_logic_vector;

	------------------------------------------------------------
	-- Print string on standard output or in a file
	-- An entire line is appended to the file
	------------------------------------------------------------
	procedure Print (S : string);
	procedure Print (F : out text; S : string); 

	------------------------------------------------------------
	-- Scan string from standard input or from a file
	-- Gets a whole line and write it in the string
	-- There is an assertion error if the string is too short
	------------------------------------------------------------
	procedure Scan (S : out string);
	procedure Scan (variable F : in text; S : out string); 

	------------------------------------------------------------
	-- Function GetTiming gets a timing array from a file
	-- The file format must be lines of couples name value
	-- Ex : TPLH  12 ns
	-- Blanks, comments in VHDL style (--) are authorized
	-- Actually when a line does not fit, it is simply skipped
	-- Anything following the value is ignored
	-- The function is case insensitive regarding TP_NAME
	-- TP_NAME gives the names to search for (separated by blanks)
	-- The order needs not to be the same in TP_NAME and in file,
	--  but the function is much faster if it is so (especially if N is high !).
	------------------------------------------------------------
	type tp_array is array (natural range <>) of time;

	function GetTiming (	TP_NAME : string;	-- names of values to load
												N : natural;			-- number of values to load
												FILE_NAME : string) return tp_array;	

	------------------------------------------------------------
	-- The function GetVal gets an integer in the same way
	--  It is very useful for loading a value of any type
	--   by using its position and the attribute 'val
	--  With INF and SUP, assertion error if value outside [INF, SUP]
	--   and return value = INF
	-- In case of error, GetVal returns 0 (first form) or INF (second form)
	------------------------------------------------------------
	function GetVal (FILE_NAME, FIELD : string) return integer;
	function GetVal (	FILE_NAME, FIELD : string; 
										INF, SUP : integer) return integer;

end StdIoImp;	-- package

------------------------------------------------------------
------------------------------------------------------------

package body StdIoImp is

	constant BUF_SIZE : natural := 256;	-- supposed greater than necessary
	constant ZERO_BUFFER : string (1 to BUF_SIZE) := (others => '0');

	----------------------------

	function StrLen (S : string) return natural is
		constant L : integer := S'length;
		alias Si : string(1 to L) is S;
	begin
		for i in 1 to L loop
			if Si(i) = NUL then return i - 1; end if;
		end loop;
		return L;
	end StrLen;	-- function

	----------------------------
	-- the function StrEqu is written for optimal speed
	function StrEqu (S1, S2 : string; N : natural := 0) return boolean is
		constant L1 : integer := S1'length + 1;
		constant L2 : integer := S2'length + 1;
		constant Si1 : string(1 to L1) := S1 & NUL;
		constant Si2 : string(1 to L2) := S2 & NUL;
		variable MinBound : natural := L1;
	begin
		if L2 < MinBound then MinBound := L2; end if;
		if N > 0 and N < MinBound then MinBound := N; end if;
		for i in 1 to MinBound loop
			if Si1(i) /= Si2(i) then return FALSE;
			elsif Si1(i) = NUL then return TRUE;
			end if;
		end loop;
		return TRUE;
	end StrEqu;	-- function

	----------------------------
	-- the function StrNcEqu is written for optimal speed
	function StrNcEqu (S1, S2 : string; N : natural := 0) return boolean is
		constant L1 : integer := S1'length + 1;
		constant L2 : integer := S2'length + 1;
		constant Si1 : string(1 to L1) := S1 & NUL;
		constant Si2 : string(1 to L2) := S2 & NUL;
		variable MinBound : natural := L1;
	begin
		if L2 < MinBound then MinBound := L2; end if;
		if N > 0 and N < MinBound then MinBound := N; end if;
		for i in 1 to MinBound loop
			if ToUpper(Si1(i)) /= ToUpper(Si2(i)) then return FALSE;
			elsif Si1(i) = NUL then return TRUE;
			end if;
		end loop;
		return TRUE;
	end StrNcEqu;	-- function

	----------------------------

	procedure Shift(variable S : inout string; N : natural := 1) is
		constant L : integer := StrLen(S);
		alias Si : string (1 to S'length) is S;
		variable Res : string (1 to S'length) := (others => NUL);
	begin
		if L > 0 then
			if N < L then Res(1 to L - N) := Si(N + 1 to L); end if;
			Si := Res;
		end if;
	end Shift;	-- procedure

	----------------------------

	function UpTo(S : string; PATTERN : string) return integer is
		alias Si : string(1 to S'length) is S;
		constant L1 : integer := StrLen(Si);
		constant L2 : integer := StrLen(PATTERN);
	begin
		if L1 < L2 then return L1; end if;
		for i in 1 to L1 - L2 + 1 loop
			if StrEqu(Si(i to L1), PATTERN, L2) then return i + L2 - 1; end if;
		end loop;
		return L1;
	end UpTo;	-- function

	----------------------------

	function WordLen(S : string) return natural is
		constant L : natural := S'length;
		alias Si : string (1 to L) is S;
	begin
		for i in 1 to L loop
			if Si(i) = NUL or Si(i) = ' ' or Si(i) = HT then return i - 1; end if;
		end loop;
		return L;
	end WordLen;	-- function

	----------------------------

	function NextWord(S : string; INDEX : positive) return natural is
		constant L : natural := S'length;
		alias Si : string (1 to L) is S;
		variable InBlank : boolean := FALSE;
		variable LeftBound : natural;
	begin
		if L = 0 then return 0; end if;
		if L < INDEX then LeftBound := 1;
		else LeftBound := INDEX;
		end if;
		for i in LeftBound to L loop
			case S(i) is
				when NUL 			=> exit;	-- respect string convention
				when ' ' | HT => InBlank := TRUE;
				when others 	=> if InBlank then return i; end if;
			end case;
		end loop;
		InBlank := TRUE;
		for i in 1 to LeftBound loop
			case S(i) is
				when NUL 			=> exit;	-- respect string convention
				when ' ' | HT => InBlank := TRUE;
				when others 	=> if InBlank then return i; end if;
			end case;
		end loop;
		return 0;
	end NextWord;	-- function

	----------------------------

	type to_integer_table is array (character'low to character'high) of integer;
	-- TO_INTEGER gives digit_value without control; 1000 for non-digit
	constant TO_INTEGER : to_integer_table := (
		'0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6,
		'7' => 7, '8' => 8, '9' => 9, 
		'A' => 10, 'B' => 11, 'C' => 12, 'D' => 13, 'E' => 14, 'F' => 15, 
		'a' => 10, 'b' => 11, 'c' => 12, 'd' => 13, 'e' => 14, 'f' => 15, 
		others => 1000);

	function DigitValue (VALUE : digit; BASE : base_type := 10) return integer is
		constant RES : integer := TO_INTEGER(VALUE);
	begin
		assert RES < BASE report "Incorrect digit in that base !" severity error;
		return RES;
	end DigitValue;

	----------------------------

	function IsDigit (VALUE : character; BASE : base_type := 10) return boolean is
	begin
		return TO_INTEGER(VALUE) < BASE;
	end IsDigit;	-- function

	----------------------------

	function IsBlank (S : string) return boolean is
	begin
		return LeadBlank(S) = StrLen(S);
	end IsBlank;	-- function

	----------------------------

	function LeadBlank (S : string) return integer is
		constant L : integer := S'length;
		alias Si : string (1 to L) is S;
	begin
		for i in 0 to L - 1 loop
			if not (Si(i + 1) = ' ' or Si(i + 1) = HT) then return i; end if;
		end loop;
		return L;
	end LeadBlank;	-- function

	----------------------------
	constant a_A : integer := character'pos('a') - character'pos('A');

	function ToUpper (VALUE : character) return character is
	begin
		if 'a' <= VALUE and VALUE <= 'z' then
			return character'val(character'pos(VALUE) - a_A);
		else return VALUE;
		end if;
	end ToUpper;	-- function

	----------------------------

	function ToUpper (S : string) return string is
		variable Res : string (S'range);
	begin
		for i in S'range loop
			Res(i) := ToUpper(S(i));
		end loop;
		return Res;
	end ToUpper;	-- function

	----------------------------

	function ToLower (VALUE : character) return character is
	begin
		if 'A' <= VALUE and VALUE <= 'Z' then
			return character'val(character'pos(VALUE) + a_A);
		else return VALUE;
		end if;
	end ToLower;	-- function

	----------------------------

	function ToLower (S : string) return string is
		variable Res : string (S'range);
	begin
		for i in S'range loop
			Res(i) := ToLower(S(i));
		end loop;
		return Res;
	end ToLower;	-- function

	----------------------------

	function ToDigit (VALUE : natural) return character is
		constant TO_DIGIT_TABLE : string (1 to 16) := "0123456789ABCDEF"; 
	begin   
		if VALUE >= 16 then
			assert FALSE report "Not a digit !" severity error;
			return ' ';
		else return TO_DIGIT_TABLE(VALUE + 1);
		end if;
	end ToDigit;    -- function

	----------------------------

	function Mirror (S : string) return string is
		alias Si : string (1 to S'length) is S;
		constant L : integer := StrLen(S);
		variable Res : string (1 to S'length) := (others => NUL);
	begin
		for i in 1 to L loop
			Res(i) := Si(L - i + 1);
		end loop;
		return Res;
	end Mirror;	-- function

	----------------------------

	-- local procedure definition for extracting a natural from a string
	procedure GetNatural (variable S : inout string;
									variable N : out natural;
									BASE : base_type := 10) is
		constant L : integer := S'length;
		alias Si : string (1 to L) is S;
		variable Index : integer := 0;
		variable Ni : natural := 0;
	begin
		for i in 1 to L loop
			exit when not IsDigit(Si(i), BASE);
			Ni := Ni * BASE + DigitValue(Si(i), BASE);
			Index := Index + 1;
		end loop;
		N := Ni;
		Shift(Si, Index);
	end GetNatural;	-- procedure

	----------------------------

	function ToString (VALUE : integer; FORMAT : string := "%d") return string is
		variable Si : string (1 to FORMAT'length) := FORMAT;
		variable FieldWidth : natural := 0;
		type case_type is (UPPER, LOWER);
		variable MyCase : case_type;
		variable BaseValue : base_type := 10;
		variable Res : string(BUF_SIZE downto 1)  := (others => ' ');
		variable Index : integer := 1;
		variable VAbs : integer := abs(VALUE);
		variable FillZero : boolean := FALSE;
	begin
		assert StrLen(Si) > 1 and Si(1) = '%' 
			report "Wrong format in string conversion !" severity error;
		Shift(Si);
		if Si(1) = '0' then
			FillZero := TRUE;
			Res := ZERO_BUFFER;
		end if;
		if IsDigit(Si(1)) then GetNatural(Si, FieldWidth); end if;
		assert FieldWidth < Res'length 
				report "Wrong format in string conversion !" severity error;
		case Si(1) is
			when 'b' => MyCase := UPPER; BaseValue := 2;
			when 'd' => MyCase := UPPER; BaseValue := 10;
			when 'x' => MyCase := LOWER; BaseValue := 16;
			when 'X' => MyCase := UPPER; BaseValue := 16;
			when others => assert FALSE
				report "Wrong format in string conversion !" severity error;
		end case;
		assert StrLen(Si) = 1
			report "Wrong format in string conversion !" severity error;
		
		loop
			Res(Index) := ToDigit(VAbs mod BaseValue);
			VAbs := VAbs / BaseValue;
			Index := Index + 1;
			exit when VAbs = 0;
		end loop;
		
		if VALUE < 0 then
			if FillZero and Index <= FieldWidth then
				Index := FieldWidth + 1;
				Res(Index - 1) := '-';
			else
				Res(Index) := '-';
				Index := Index + 1;
			end if;
		end if;
		if Index <= FieldWidth then Index := FieldWidth + 1; end if;
		if MyCase = LOWER then return ToLower(Res(Index - 1 downto 1));
		else return Res(Index - 1 downto 1);
		end if;
	end ToString;	-- function

	----------------------------

	function ToString (VALUE : real; FORMAT : string := "%") return string is
		variable Si : string (1 to FORMAT'length) := FORMAT;
		variable FieldWidth : natural := 0;
		variable Precision : natural := 0;
		variable Index : integer := 1;
		variable FillZero : boolean := FALSE;
		variable L : line;
	begin
		assert StrLen(Si) > 0 and Si(1) = '%' 
			report "Wrong format in string conversion !" severity error;
		Shift(Si);
		FillZero := Si(1) = '0';
		if IsDigit(Si(1)) then GetNatural(Si, FieldWidth); end if;
		if Si(1) = '.' then
			Shift(Si);
			if IsDigit(Si(1)) then GetNatural(Si, Precision); end if;
		end if;
		assert StrLen(Si) = 0
			report "Wrong format in string conversion !" severity error;
		write(L => L, VALUE => VALUE, FIELD => FieldWidth, DIGITS => Precision);
		if FillZero and L(1) = ' ' then
			for i in 1 to L'length loop
				case L(i) is
					when '-' => L(i) := '0'; exit;
					when ' ' => L(i) := '0';
					when others => exit;
				end case;
			end loop;
			if VALUE < 0.0 then L(1) := '-'; end if;
		end if;
		return L.all;
	end ToString;	-- function

	----------------------------

	function ToString (VALUE : time; FORMAT : string := "% ns") return string is
		variable Si : string (1 to FORMAT'length) := FORMAT;
		variable FieldWidth : natural := 0;
		variable Unit : time := ns;
		variable L : line;
	begin
		assert StrLen(Si) > 0 and Si(1) = '%' 
			report "Wrong format in string conversion !" severity error;
		Shift(Si);
		if IsDigit(Si(1)) then GetNatural(Si, FieldWidth); end if;
		assert Si(1) = ' ' or Si(1) = HT
			report "No blank between time value and unit !"
			severity error;
		Shift(Si, LeadBlank(Si));
		if StrEqu(Si, "fs") then Unit := 1.0E-6 ns;	-- avoid warning
		elsif StrNcEqu(Si, "ps") then Unit := 1.0E-3 ns;	-- avoid warning
		elsif StrNcEqu(Si, "ns") then Unit := ns;	-- conform to textio
		elsif StrNcEqu(Si, "us") then Unit := us;
		elsif StrNcEqu(Si, "ms") then Unit := ms;
		elsif StrNcEqu(Si, "sec") then Unit := sec;
--		elsif StrNcEqu(Si, "min") then Unit := min;
--		elsif StrNcEqu(Si, "hr") then Unit := hr;
		else 
			assert FALSE report "Wrong format in string conversion !" severity error;
		end if;
		write(L => L, VALUE => VALUE, FIELD => FieldWidth, UNIT => Unit);
		return L.all;
	end ToString;	-- function

	----------------------------

	function ToString (VALUE : boolean) return string is
	begin	
		if VALUE then return "TRUE";
		else return "FALSE";
		end if;
	end ToString;	-- function

	----------------------------

	type bit_to_char_table is 
			array(bit'low to bit'high) of character;
	constant BIT_TO_CHAR : bit_to_char_table := ('0', '1');
	type std_ulogic_to_char_table is 
			array(std_ulogic'low to std_ulogic'high) of character;
	constant STD_ULOGIC_TO_CHAR : std_ulogic_to_char_table :=
			('U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-');

	function ToString (VALUE : std_ulogic) return string	is
	begin	
		return STD_ULOGIC_TO_CHAR(VALUE) & "";
	end ToString;	-- function

	----------------------------

	function ToString (VALUE : bit) return string is
	begin
		return BIT_TO_CHAR(VALUE) & "";
	end ToString;	-- function

	----------------------------

	function ToString (VALUE : bit_vector) return string is
    constant L : integer := VALUE'length;
	  alias Vec : bit_vector(1 to L) is VALUE;
		variable Res : string (1 to L);
	begin
		for i in Res'range loop
			Res(i) := BIT_TO_CHAR(Vec(i));
		end loop;
		return Res;
	end ToString;	-- function

	----------------------------

	function ToString (VALUE : std_ulogic_vector) return string is
    constant L : integer := VALUE'length;
	  alias Vec : std_ulogic_vector(1 to L) is VALUE;
		variable Res : string (1 to L);
	begin
		for i in Res'range loop
			Res(i) := STD_ULOGIC_TO_CHAR(Vec(i));
		end loop;
		return Res;
	end ToString;	-- function

	----------------------------

	function ToString (VALUE : std_logic_vector) return string is
    constant L : integer := VALUE'length;
	  alias Vec : std_logic_vector(1 to L) is VALUE;
		variable Res : string (1 to L);
	begin
		for i in Res'range loop
			Res(i) := STD_ULOGIC_TO_CHAR(Vec(i));
		end loop;
		return Res;
	end ToString;	-- function

	----------------------------

	function FromString (S : string; BASE : base_type := 10) return integer is
		variable L : natural := S'length;
		variable Si : string (1 to L) := S;
		variable Res : integer := 0;
		variable Invert : boolean := FALSE;
		variable Index : natural := LeadBlank(Si) + 1;
	begin
		if Index <= L and Si(Index) = '-' then
			Invert := TRUE;
			Index := Index + 1;
		end if;
		if Index > L or not IsDigit(Si(Index), BASE) then 
			assert FALSE report "Not an integer string !" severity error;
			return Res;
		end if;
		while Index <= L and IsDigit(Si(Index), BASE) loop
			Res := Res * BASE + DigitValue(Si(Index), BASE);
			Index := Index + 1;
		end loop;
		assert Index > L or Si(Index) = NUL or Si(Index) = ' ' or Si(Index) = HT
				report "Not an integer string !" severity error;
		if Invert then return -Res;
		else return Res;
		end if;
	end FromString;	-- function

	----------------------------

	function FromString (S : string) return real is
		variable L : line;
		variable Success : boolean;
		variable Res : real := 0.0;
	begin
		Write(L, S);
		Read(L, Res, Success);
		assert Success report "Not a real string !" severity error;
		return Res;
	end FromString;	-- function

	----------------------------

	function FromString (S : string) return time is
		variable L : line;
		variable Success : boolean;
		variable Res : time := 0 ns;
	begin
		Write(L, S);
		Read(L, Res, Success);
		assert Success report "Not a time string !" severity error;
		return Res;
	end FromString;	-- function

	----------------------------

	function FromString (S : string) return boolean is
		variable Si : string (1 to S'length) := S;
		variable Res : boolean := FALSE;
	begin
		Shift(Si, LeadBlank(Si));
		if StrLen(Si) >= 4 and Si(1 to 4) = "TRUE" then
			Res := TRUE;
			Shift(Si, 4);
		elsif StrLen(Si) >= 5 and Si(1 to 5) = "FALSE" then
			Res := FALSE;
			Shift(Si, 5);
		else
			assert FALSE report "Not a boolean string !" severity error;
		end if;
		assert StrLen(Si) = 0 or Si(1) = ' ' or Si(1) = HT
			report "Not an boolean string !" severity error;
		return Res;
	end FromString;	-- function

	----------------------------

	function ToBit(VALUE : character) return bit is
		variable Res : bit := '0';
	begin
		case VALUE is
			when '0' => Res := '0';
			when '1' => Res := '1';
			when others => assert FALSE
					report "character is not a bit value !" severity error;
		end case;
	return Res;
	end ToBit;	-- local function

	----------------------------

	function ToStdUlogic(VALUE : character) return std_ulogic is
		variable Res : std_ulogic := 'U';
	begin
		case VALUE is
			when 'U' => Res := 'U';
			when 'X' => Res := 'X';
			when '0' => Res := '0';
			when '1' => Res := '1';
			when 'Z' => Res := 'Z';
			when 'L' => Res := 'L';
			when 'H' => Res := 'H';
			when 'W' => Res := 'W';
			when '-' => Res := '-';
			when others => assert FALSE
					report "character is not a std_ulogic value !" severity error;
		end case;
	return Res;
	end ToStdUlogic;	-- local function

	----------------------------

	function FromString (S : string) return bit is
		alias Si : string (1 to S'length) is S;
	begin
		assert not IsBlank(Si) report "Empty or blank string !" severity error;
		return ToBit(Si(LeadBlank(Si) + 1));
	end FromString;	-- function

	----------------------------
	
	function FromString (S : string) return std_ulogic is
		alias Si : string (1 to S'length) is S;
	begin
		assert not IsBlank(Si) report "Empty or blank string !" severity error;
		return ToStdUlogic(Si(LeadBlank(Si) + 1));
	end FromString;	-- function

	----------------------------

	function FromString (S : string; N : natural) return bit_vector is
		alias Si : string (1 to S'length) is S;
		variable Res : bit_vector (0 to N - 1) := (others => '0');
		variable Index : integer := LeadBlank(Si) + 1;
	begin
		if N > StrLen(Si) - Index + 1 then
			assert FALSE report "String too small !" severity error;
		else
			for i in 0 to N - 1 loop
				Res(i) := ToBit(Si(Index));
				Index := Index + 1;
			end loop;
		end if;
		return Res;
	end FromString;	-- function

	----------------------------

	function FromString (S : string; N : natural) return std_ulogic_vector is
		alias Si : string (1 to S'length) is S;
		variable Res : std_ulogic_vector (0 to N - 1) := (others => 'U');
		variable Index : integer := LeadBlank(Si) + 1;
	begin
		if N > StrLen(Si) - Index + 1 then
			assert FALSE report "String too small !" severity error;
		else
			for i in 0 to N - 1 loop
				Res(i) := ToStdUlogic(Si(Index));
				Index := Index + 1;
			end loop;
		end if;
		return Res;
	end FromString;	-- function

	----------------------------

	function FromString (S : string; N : natural) return std_logic_vector is
		alias Si : string (1 to S'length) is S;
		variable Res : std_logic_vector (0 to N - 1) := (others => 'U');
		variable Index : integer := LeadBlank(Si) + 1;
	begin
		if N > StrLen(Si) - Index + 1 then
			assert FALSE report "String too small !" severity error;
		else
			for i in 0 to N - 1 loop
				Res(i) := ToStdUlogic(Si(Index));
				Index := Index + 1;
			end loop;
		end if;
		return Res;
	end FromString;	-- function

	----------------------------

	procedure Print (S : string) is
	begin
		Print(OUTPUT, S);
	end Print;	-- procedure

	----------------------------

	procedure Print (F : out text; S : string) is
		alias Si : string (1 to S'length) is S;
		variable L : line := new string'(Si(1 to Strlen(Si)));
	begin
		writeline(F, L);
	end Print;	-- procedure

	----------------------------

	procedure Scan (S : out string) is
	begin
		Scan(INPUT, S);
	end Scan;	-- procedure

	----------------------------

	procedure Scan (variable F : in text; S : out string) is 
		alias Si : string (1 to S'length) is S;
		variable L : line;
		constant S_LENGTH : natural := S'length;
		variable LLength : natural;
	begin
	 	readline(F, L);
	 	if L = NULL then
	 		assert FALSE report "Read error !" severity error;
	 		return;
	 	end if;
	 	LLength := L'length;
		if LLength < S_LENGTH then Si(1 to LLength + 1) := L.all & NUL;
		elsif LLength = S_LENGTH then Si(1 to LLength) := L.all;
		else
			Si := L(1 to S_LENGTH);
	 		assert FALSE report "String too short to contain input line !" 
	 			severity error;
	 	end if;
	end Scan;	-- procedure

	----------------------------

	---------------
	-- NbWord returns the number of words in a string (separator : blank)
	---------------
	function NbWord(S : string) return integer is
		variable InBlank : boolean := TRUE;
		variable Res : integer := 0;
	begin
		for i in S'range loop
			case S(i) is
				when NUL 			=> exit;	-- respect string convention
				when ' ' | HT => InBlank := TRUE;
				when others 	=> 
					if InBlank then Res := Res + 1; end if;
					InBlank := FALSE;
			end case;
		end loop;
		return Res;
	end NbWord;	-- local function

	---------------

	function GetTiming (	TP_NAME : string;
												N : natural;
												FILE_NAME : string) return tp_array is
		file F : text is in FILE_NAME;
		constant TP_NAME_L : natural := TP_NAME'length;
		alias TpName : string (1 to TP_NAME_L) is TP_NAME;
		variable MyLine : string(1 to BUF_SIZE);
		variable IndexS : natural := LeadBlank(TpName) + 1;
		variable IndexTp, OldIndexTp : natural := 1;
		variable NextTpLength : integer;
		variable Res : tp_array (1 to N) := (others => 0 ns);
		variable WriteCount : bit_vector(1 to N) := (others => '0');
		constant ALL_WRITTEN : bit_vector(1 to N) := (others => '1');
		variable Found : boolean;
	begin
		if N /= NbWord(TpName) then
			assert FALSE report "TP_NAME string incorrect !" severity error;
			return Res;
		end if;
		ForeachLineInFile : while not endfile(F) loop
			Scan(F, MyLine);
			Shift(MyLine, LeadBlank(MyLine));
			next when MyLine(1) = NUL or MyLine(1 to 2) = "--";
			OldIndexTp := IndexTp;
			ForeachTpInTpName : loop
				NextTpLength := WordLen(MyLine);
				Found := StrNcEqu(MyLine, TpName(IndexS to TP_NAME_L), NextTpLength);
				if Found then
					if WriteCount(IndexTp) > '0' then
						assert FALSE report "Field " & MyLine(1 to NextTpLength) & 
							" already loaded !" severity error;
						exit;
					end if;
					Shift(MyLine, NextTpLength);
					Res(IndexTp) := FromString(MyLine);
					WriteCount(IndexTp) := '1';
				end if;
				IndexTp := IndexTp mod N + 1;
				IndexS := NextWord(TpName, IndexS);
				exit when Found or IndexTp = OldIndexTp;
				-- if the line not relevant, it is skipped
			end loop ForeachTpInTpName;
		end loop ForeachLineInFile;
		if WriteCount /= ALL_WRITTEN then
			Diagnosis : for i in 1 to N loop
				assert WriteCount(i) /= '0' report "TP number " & ToString(i) & 
					" not found !" severity error;
			end loop Diagnosis;
		end if;
		return Res;
	end GetTiming;	-- function

	----------------------------

	function GetVal (FILE_NAME, FIELD : string) return integer is
		file F : text is in FILE_NAME;
		variable S : string(1 to BUF_SIZE);
		variable MyVal : integer := 0;
		constant L : integer := StrLen(FIELD);
	begin
		loop
			if endfile(F) then
				assert FALSE report FIELD & " not found in file " & FILE_NAME
					severity error;
				exit;
			end if;
			Scan(F, S);
			Shift(S, LeadBlank(S));
			if StrNcEqu(S, FIELD, L) then
				Shift(S, L);
				MyVal := FromString(S);
				exit;
			end if;
		end loop;
		return MyVal;
	end GetVal;	-- function

	----------------------------

	function GetVal (	FILE_NAME, FIELD : string; 
										INF, SUP : integer) return integer is
		file F : text is in FILE_NAME;
		variable S : string(1 to BUF_SIZE);
		variable MyVal : integer := INF;
		constant L : integer := StrLen(FIELD);
	begin
		loop
			if endfile(F) then
				assert FALSE report FIELD & " not found in file " & FILE_NAME
					severity error;
				exit;
			end if;
			Scan(F, S);
			Shift(S, LeadBlank(S));
			if StrNcEqu(S, FIELD, L) then
				Shift(S, L);
				MyVal := FromString(S);
				if MyVal < INF or MyVal > SUP then
					assert FALSE report "Value " & ToString(MyVal) & " out of range [" & 
						ToString(INF) & ", " & ToString(SUP) & "] in file " & FILE_NAME
						severity error;
					MyVal := INF;
				end if;
				exit;
			end if;
		end loop;
		return MyVal;
	end GetVal;	-- function

	----------------------------

end StdIoImp;	-- package body

------------------------------------------------------------
-- File name : stdsim.vhd
-- Title : StdSim
-- project : SPARC
-- Library : MMS
-- Author(s) : E. Laubacher
-- Purpose : package for standard simulations declarations
-- notes :  to change the simulation conditions, alter 
-- 	the stdsim.dft file in your working directory
------------------------------------------------------------
-- Modification history :
------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
------------------------------------------------------------
-- v 1.0        | EL     | 92/08/11    | first version
------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
------------------------------------------------------------

package StdSim is

	----------------------------------
	-- definition of types necessary for simulation control
	----------------------------------

	type environment is (COMMERCIAL, INDUSTRIAL, MILITARY);
	type sim_type is (SPECIFIC, MINIMUM, TYPICAL, MAXIMUM);
	type proces_type is (TYPICAL, BEST, WORST);
	type temperature is range -55 to +125
		units
			Celsius;
		end units;
	type voltage is range 4500 to 5500
		units
			mV;
			V = 1000 mV;
		end units;
	type capacitance is range integer'low to integer'high
		units
			fF;
			pF = 1000 fF;
			nF = 1000 pF;
			uF = 1000 nF;
		end units;

	----------------------------------
	-- Deferred constants
	-- CHECK_ON to turn on or off the timing checkers (SetupHoldCheck, 
	--   PulseWidthCheck)
	-- ENVIRONMENT_BOARD and SIM_BOARD are visible for information purpose only 
	-- BOARD conditions for temperature, voltage, process, capacitance
	--  Capacitance is generally set globally, and not pin-by-pin
	--   to avoid numerous unuseful parameters
	----------------------------------

	constant CHECK_ON : boolean;
	constant ENVIRONMENT_BOARD : environment;
	constant SIM_BOARD : sim_type;
	constant T_BOARD : temperature;
	constant V_BOARD : voltage;
	constant PROCES_BOARD : proces_type;
	constant LOAD_BOARD : capacitance;

end StdSim;	-- package

------------------------------------------------------------

library MMS;
use MMS.StdIoImp.GetVal;

package body StdSim is
	------------------------------------
	-- values loaded from file "stdsim.dft"
	------------------------------------
	constant CHECK_ON : boolean := boolean'val(GetVal("stdsim.dft", "CHECK_ON", 
		boolean'pos(boolean'low), boolean'pos(boolean'high)));

	constant ENVIRONMENT_BOARD : environment := 
		environment'val(GetVal("stdsim.dft", "ENVIRONMENT_BOARD",
			environment'pos(environment'low), environment'pos(environment'high)));

	constant SIM_BOARD : sim_type := 
		sim_type'val(GetVal("stdsim.dft", "SIM_BOARD",
			sim_type'pos(sim_type'low), sim_type'pos(sim_type'high)));

	constant LOAD_BOARD : capacitance := 
		GetVal("stdsim.dft", "LOAD_BOARD") * pF;

	------------------------------------

	function SelectT (ENV : environment; SIM : sim_type) return temperature is
	begin
		case SIM is
			when SPECIFIC =>
				return GetVal("stdsim.dft", "T_BOARD_SPECIFIC") * Celsius;
			when MINIMUM =>
				case ENV is
					when COMMERCIAL => return 0 Celsius;
					when INDUSTRIAL => return -40 Celsius;
					when MILITARY => return -55 Celsius;
				end case;
			when TYPICAL => return 25 Celsius;
			when MAXIMUM =>
				case ENV is
					when COMMERCIAL => return 70 Celsius;
					when INDUSTRIAL => return 85 Celsius;
					when MILITARY => return 125 Celsius;
				end case;
		end case;
	end SelectT;	-- function

	function SelectV (SIM : sim_type) return voltage is
	begin
		case SIM is
			when SPECIFIC => return GetVal("stdsim.dft", "V_BOARD_SPECIFIC") * mV;
			when MINIMUM => return 5500 mV;
			when TYPICAL => return 5000 mV;
			when MAXIMUM => return 4500 mV;
		end case;
	end SelectV;	-- function

	function SelectProces (SIM : sim_type) return proces_type is
	begin
		case SIM is
			when SPECIFIC => 
				return proces_type'val(GetVal("stdsim.dft", "PROCES_BOARD_SPECIFIC",
				proces_type'pos(proces_type'low), proces_type'pos(proces_type'high)));
			when MINIMUM => return BEST;
			when TYPICAL => return TYPICAL;
			when MAXIMUM => return WORST;
		end case;
	end SelectProces;	-- function

	------------------------------------
	-- Calculation of board conditions
	------------------------------------

	constant T_BOARD : temperature := SelectT(ENVIRONMENT_BOARD, SIM_BOARD);
	constant V_BOARD : voltage := SelectV(SIM_BOARD);
	constant PROCES_BOARD : proces_type := SelectProces(SIM_BOARD);

end StdSim;	-- package body
------------------------------------------------------------
-- File name : stdtiming.vhd
-- Title : StdTiming
-- project : SPARC
-- Library : MMS
-- Author(s) : E. Laubacher
-- Purpose : package for standard timing declarations
-- notes : 
------------------------------------------------------------
-- Modification history :
------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
------------------------------------------------------------
-- v 1.0        | EL     | 92/06/14    | first version
--..........................................................
-- v 1.1        | MR     | 94/03/04    | 2nd version
-- + modification of function CalcDelay: new equation with
--   default capacitance of 50 pf, and timing parameters
--   in worst case conditions.
-- + 2 procedures for conditional setup/hold timing checking
--   added.
--..........................................................
-- v 1.2        | MR     | 94          |
-- + modification of function CalcDelay: equation with "ns"
--   instead of "ps".
------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
------------------------------------------------------------

library IEEE;
use IEEE.Std_Logic_1164.all;
library MMS;
use MMS.StdSim.all;
use MMS.StdIoImp.all;
package StdTiming is

	----------------------------------
	-- Technology definitions
	----------------------------------

	type curve_parameter is array(natural range <>) of real;
	constant TECHNO_PREC : natural := 4;
	subtype slope_type is real;	-- pS/pF

	type technology is record
		TCurve : curve_parameter(0 to TECHNO_PREC - 1);
		VCurve : curve_parameter(0 to TECHNO_PREC - 1);
		KPMin : real;
		KPMax : real;
		LoadSlope : slope_type;	-- slope in ps/PF
	end record;	-- technology

	-- Note : KPTyp = 1.0

	----------------------------------
	-- usefull type for timing violation procedures
	----------------------------------

	type edge_type is (RISING, FALLING);
	type sense_type is (MINIMUM, MAXIMUM);

	----------------------------------
	-- Min and max functions on type time
	----------------------------------

	function MinTime(A, B : time) return time;
	function MaxTime(A, B : time) return time;

	-----------------------------------------------------------
	-- Function Name : CalcDelay
	-- Purpose : Reference delay calculation
	--
	-- Use :
	-- 	CalcDelay(BASE => TPLH,
	--						TECH => MHS_MC,
	--						T => 125 C,
	--						V => 5500 mV,
	--						PROCES => TYPICAL,
	--						LOAD => 50 pF);
	--
	-- Note : for setup, hold or pulse-width, the load has no meaning :
	--  just use the default value 50 pF
	--  The BASE value is given in the worst conditions: 125 Celcius,
	--  4.5 Volts and worst case process.
	-----------------------------------------------------------
	function CalcDelay(
		BASE : time;
		TECH : technology;
		T : temperature;
		V : voltage;
		PROCES : proces_type;
		LOAD : capacitance := 50 pF) return time;

	-----------------------------------------------------------
	-- Procedure Name : SetupHoldCheck
	-- Purpose : Check of setup-hold condition
	--  must be called concurrently (lasts Infinitely)
	-- 	Negative values accepted
	-- 	Verifies that the data does not change during the setup-hold interval
	-- 	The data is not allowed to change at the edge of the interval
	-- 	A 0-value for both edges produces no check
	--  The data is delayed at calling, since it is impossible in a procedure
	--  Beware that HOLD >= Tcycle leads to an incorrect behavior
	--
	-- Use (general form) :	
	-- 	SetupHoldCheck(	Data => Data,
	-- 				        	Ref => Clk,
	--   				     	 EDGE => FALLING,
	--          				SETUP => TSU, HOLD => THO,
	--           				PATH => "SEQUENCER/REG",
	--									DelayedData => Data'delayed(abs(HOLD)));
	--
	-----------------------------------------------------------
	procedure SetupHoldCheck (
		signal Data : std_ulogic;	-- as a subtype works for std_logic as well
		signal Ref : std_ulogic;
		constant EDGE : edge_type := RISING;
		constant SETUP, HOLD : time := 0 ns;
		constant PATH : string := "";
		signal DelayedData : std_ulogic);
			-- DelayedData must be set to Data'Delayed(abs(HOLD)) 
			--  for negative hold processing
	
	procedure SetupHoldCheck (
		signal Data : std_ulogic_vector;	
		signal Ref : std_ulogic;
		constant EDGE : edge_type := RISING;
		constant SETUP, HOLD : time := 0 ns;
		constant PATH : string := "";
		signal DelayedData : std_ulogic_vector);
			-- DelayedData must be set to Data'Delayed(abs(HOLD)) 
			--  for negative hold processing
	
	procedure SetupHoldCheck (
		signal Data : std_logic_vector;	-- not a subtype of std_ulogic_vector
		signal Ref : std_ulogic;
		constant EDGE : edge_type := RISING;
		constant SETUP, HOLD : time := 0 ns;
		constant PATH : string := "";
		signal DelayedData : std_logic_vector);
			-- DelayedData must be set to Data'Delayed(abs(HOLD)) 
			--  for negative hold processing
	
	-----------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Conditional Setup/Hold timing checkers: the boolean signal EN_CHECKING  
  -- enables checking if it is TRUE. Similar to the procedure SetupHoldCheck.
  -----------------------------------------------------------------------------
  procedure CondSetupHoldCheck (signal Data   : std_ulogic;	
                                   -- as a subtype works for std_logic as well 
                                signal Ref    : std_ulogic;
                                constant EDGE : edge_type := RISING;
                                constant SETUP, HOLD : time := 0 ns;
                                constant PATH : string := "";
                                signal DelayedData : std_ulogic;
                                signal EN_CHECKING : boolean);

  procedure CondSetupHoldCheck (signal Data   : std_logic_vector;	
                                signal Ref    : std_ulogic;
                                constant EDGE : edge_type := RISING;
                                constant SETUP, HOLD : time := 0 ns;
                                constant PATH : string := "";
                                signal DelayedData : std_logic_vector;
                                signal EN_CHECKING : boolean);

	-----------------------------------------------------------
	-- Procedure Name : PulseCheck
	-- Purpose : Check of pulse width condition
	--  must be called concurrently (lasts Infinitely)
	-- 	Verifies that Pin stays longer than WIDTH in state LEVEL
	--  WIDTH is an acceptable value
	-- 	A 0-value produces no check
	--  No check before first event on Pin
	--
	-- Use :	PulseCheck (
	--					Pin => Clk,
	-- 					LEVEL => '1',
	--    			WIDTH => 15 ns,
	--          SENSE => MINIMUM,
	--    			PATH => "SEQUENCER/REG" );
	--
	-----------------------------------------------------------
	procedure PulseCheck (
		signal Pin : std_ulogic;
		constant LEVEL : std_ulogic;
		constant WIDTH : time := 0 ns;
		constant SENSE : sense_type := MINIMUM;
		constant PATH : string := "");
	
	-----------------------------------------------------------

	function TpDelay(	NewData : std_ulogic;
										TPLH, TPHL : time := 0 ns) return time;

	function TpDelay(	OldData, NewData : std_ulogic;
										TPLH, TPHL : time := 0 ns;
										TPZH, TPZL : time := 0 ns;
										TPHZ, TPLZ : time := 0 ns) return time;
										
	function TpDelay(	OldData, NewData : std_ulogic_vector;
										TPD : time := 0 ns;
										TPZD, TPDZ : time := 0 ns) return time;
										
	function TpDelay(	OldData, NewData : std_logic_vector;
										TPD : time := 0 ns;
										TPZD, TPDZ : time := 0 ns) return time;
										
end StdTiming;	-- package

-------------------------------------------------------------------------
-------------------------------------------------------------------------

Library IEEE;
use IEEE.Std_Logic_1164.all;
package body StdTiming is 

	-----------------------------------------------------------
	function MinTime (A, B : time) return time is
	begin
		if A <= B then return A;
		else return B;
		end if;
	end MinTime;	-- function

	-----------------------------------------------------------
	function MaxTime (A, B : time) return time is
	begin
		if A >= B then return A;
		else return B;
		end if;
	end MaxTime;	-- function

	-----------------------------------------------------------
	procedure SetupHoldCheck (
		signal Data : std_ulogic;
		signal Ref : std_ulogic;
		constant EDGE : edge_type := RISING;
		constant SETUP, HOLD : time := 0 ns;
		constant PATH : string := "";
		signal DelayedData : std_ulogic) is
			-- DelayedData must be set to Data'Delayed(abs(HOLD)) 
			--  for negative hold processing
		
		variable LastEdge : time;
		variable DeltaT : time;
		variable EdgeDetect : boolean;
	begin
		if (not CHECK_ON) or (SETUP = 0 ns and HOLD = 0 ns) then wait; end if;
		if SETUP + HOLD <= 0 ns then
			assert FALSE report "Impossible check on " & PATH &  
				". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
				severity warning;
			wait;
		end if;
		SkipInitProblems : loop
			wait on Ref until (EDGE = RISING and rising_edge(Ref)) or 
												(EDGE = FALLING and falling_edge(Ref));
				-- nothing before first edge of Ref
			LastEdge := now;
			if HOLD >= 0 ns then exit when Data'event or Data'last_event > 0 ns;
			else exit when DelayedData'event or DelayedData'last_event > 0 ns;
			end if;
		end loop SkipInitProblems;

		Infinite : loop
			EdgeDetect := (EDGE = RISING and rising_edge(Ref)) or 
										(EDGE = FALLING and falling_edge(Ref));
			--------------------
			if SETUP > 0 ns and HOLD >= 0 ns and EdgeDetect then
				DeltaT := Data'last_event;
				assert not(DeltaT <= SETUP) 
					report "Setup violation on " & PATH & " at time " & ToString(now) &
						". Observed : " & ToString(DeltaT) & ". Setup : " & ToString(SETUP)
					severity warning;
			end if;
			if HOLD > 0 ns and SETUP >= 0 ns and Data'event then
				DeltaT := now - LastEdge;
				assert not(DeltaT <= HOLD)
					report "Hold violation on " & PATH & " at time " & ToString(now) &
						". Observed : " & ToString(DeltaT) & ". Hold : " & ToString(HOLD)
					severity warning;
			end if;
			--------------------
			if SETUP < 0 ns and Data'event then
				DeltaT := now - LastEdge;
				assert not(-SETUP <= DeltaT and DeltaT <= HOLD)
					report "Timing violation on " & PATH & " at time " & ToString(now)
						& ". Observed : " & ToString(DeltaT) & 
						". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
					severity warning;
			end if;	-- nothing if no data event
			--------------------
			if HOLD < 0 ns and EdgeDetect then
				DeltaT := HOLD - DelayedData'last_event;
				assert not(-SETUP <= DeltaT and DeltaT <= HOLD)
					report "Timing violation on " & PATH & " at time " & ToString(now)
						& ". Observed : " & ToString(DeltaT) & 
						". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
					severity warning;
			end if;	-- nothing if no edge detected
			--------------------

			if EdgeDetect then LastEdge := now; end if;
			wait on Ref, Data;
		end loop Infinite;
	end SetupHoldCheck;	-- procedure
	
	--------------------------------------------

	procedure SetupHoldCheck (
		signal Data : std_ulogic_vector;	
		signal Ref : std_ulogic;
		constant EDGE : edge_type := RISING;
		constant SETUP, HOLD : time := 0 ns;
		constant PATH : string := "";
		signal DelayedData : std_ulogic_vector) is
			-- DelayedData must be set to Data'Delayed(abs(HOLD)) 
		
		variable LastEdge : time := 0 ns;
		variable DeltaT : time;
		variable EdgeDetect : boolean;
	begin
		if (not CHECK_ON) or (SETUP = 0 ns and HOLD = 0 ns) then wait; end if;
		if SETUP + HOLD <= 0 ns then
			assert FALSE report "Impossible check on " & PATH &  
				". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
				severity warning;
			wait;
		end if;
		SkipInitProblems : loop
			wait on Ref until (EDGE = RISING and rising_edge(Ref)) or 
												(EDGE = FALLING and falling_edge(Ref));
				-- nothing before first edge of Ref
			LastEdge := now;
			if HOLD >= 0 ns then exit when Data'event or Data'last_event > 0 ns;
			else exit when DelayedData'event or DelayedData'last_event > 0 ns;
			end if;
		end loop SkipInitProblems;

		Infinite : loop
			EdgeDetect := (EDGE = RISING and rising_edge(Ref)) or 
										(EDGE = FALLING and falling_edge(Ref));
			--------------------
			if SETUP > 0 ns and HOLD >= 0 ns and EdgeDetect then
				DeltaT := Data'last_event;
				assert not(DeltaT <= SETUP) 
					report "Setup violation on " & PATH & " at time " & ToString(now) &
						". Observed : " & ToString(DeltaT) & ". Setup : " & ToString(SETUP)
					severity warning;
			end if;
			if HOLD > 0 ns and SETUP >= 0 ns and Data'event then
				DeltaT := now - LastEdge;
				assert not(DeltaT <= HOLD)
					report "Hold violation on " & PATH & " at time " & ToString(now) &
						". Observed : " & ToString(DeltaT) & ". Hold : " & ToString(HOLD)
					severity warning;
			end if;
			--------------------
			if SETUP < 0 ns and Data'event then
				DeltaT := now - LastEdge;
				assert not(-SETUP <= DeltaT and DeltaT <= HOLD)
					report "Timing violation on " & PATH & " at time " & ToString(now)
						& ". Observed : " & ToString(DeltaT) & 
						". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
					severity warning;
			end if;	-- nothing if no data event
			--------------------
			if HOLD < 0 ns and EdgeDetect then
				DeltaT := HOLD - DelayedData'last_event;
				assert not(-SETUP <= DeltaT and DeltaT <= HOLD)
					report "Timing violation on " & PATH & " at time " & ToString(now)
						& ". Observed : " & ToString(DeltaT) & 
						". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
					severity warning;
			end if;	-- nothing if no edge detected
			--------------------
	
			if EdgeDetect then LastEdge := now; end if;
			wait on Ref, Data;
		end loop Infinite;
	end SetupHoldCheck;	-- procedure

	--------------------------------------------

	procedure SetupHoldCheck (
		signal Data : std_logic_vector;	
		signal Ref : std_ulogic;
		constant EDGE : edge_type := RISING;
		constant SETUP, HOLD : time := 0 ns;
		constant PATH : string := "";
		signal DelayedData : std_logic_vector) is
			-- DelayedData must be set to Data'Delayed(abs(HOLD)) 
		
		variable LastEdge : time := 0 ns;
		variable DeltaT : time;
		variable EdgeDetect : boolean;
	begin
		if (not CHECK_ON) or (SETUP = 0 ns and HOLD = 0 ns) then wait; end if;
		if SETUP + HOLD <= 0 ns then
			assert FALSE report "Impossible check on " & PATH &  
				". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
				severity warning;
			wait;
		end if;
		SkipInitProblems : loop
			wait on Ref until (EDGE = RISING and rising_edge(Ref)) or 
												(EDGE = FALLING and falling_edge(Ref));
				-- nothing before first edge of Ref
			LastEdge := now;
			if HOLD >= 0 ns then exit when Data'event or Data'last_event > 0 ns;
			else exit when DelayedData'event or DelayedData'last_event > 0 ns;
			end if;
		end loop SkipInitProblems;

		Infinite : loop
			EdgeDetect := (EDGE = RISING and rising_edge(Ref)) or 
										(EDGE = FALLING and falling_edge(Ref));
			--------------------
			if SETUP > 0 ns and HOLD >= 0 ns and EdgeDetect then
				DeltaT := Data'last_event;
				assert not(DeltaT <= SETUP) 
					report "Setup violation on " & PATH & " at time " & ToString(now) &
						". Observed : " & ToString(DeltaT) & ". Setup : " & ToString(SETUP)
					severity warning;
			end if;
			if HOLD > 0 ns and SETUP >= 0 ns and Data'event then
				DeltaT := now - LastEdge;
				assert not(DeltaT <= HOLD)
					report "Hold violation on " & PATH & " at time " & ToString(now) &
						". Observed : " & ToString(DeltaT) & ". Hold : " & ToString(HOLD)
					severity warning;
			end if;
			--------------------
			if SETUP < 0 ns and Data'event then
				DeltaT := now - LastEdge;
				assert not(-SETUP <= DeltaT and DeltaT <= HOLD)
					report "Timing violation on " & PATH & " at time " & ToString(now)
						& ". Observed : " & ToString(DeltaT) & 
						". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
					severity warning;
			end if;	-- nothing if no data event
			--------------------
			if HOLD < 0 ns and EdgeDetect then
				DeltaT := HOLD - DelayedData'last_event;
				assert not(-SETUP <= DeltaT and DeltaT <= HOLD)
					report "Timing violation on " & PATH & " at time " & ToString(now)
						& ". Observed : " & ToString(DeltaT) & 
						". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
					severity warning;
			end if;	-- nothing if no edge detected
			--------------------
	
			if EdgeDetect then LastEdge := now; end if;
			wait on Ref, Data;
		end loop Infinite;
	end SetupHoldCheck;	-- procedure
	
  -------------------------------------  
  procedure CondSetupHoldCheck ( signal Data   : std_ulogic;	
                                 signal Ref    : std_ulogic;
                                 constant EDGE : edge_type := RISING;
                                 constant SETUP, HOLD : time := 0 ns;
                                 constant PATH : string := "";
                                 signal DelayedData : std_ulogic;
                                 signal EN_CHECKING : boolean) is
    variable LastEdge : time;
    variable DeltaT : time;
    variable EdgeDetect : boolean;
  begin
    if (not CHECK_ON) or (SETUP = 0 ns and HOLD = 0 ns) then wait; end if;
    if SETUP + HOLD <= 0 ns then
       assert FALSE report "Impossible check on " & PATH &  
        ". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
       severity warning;
       wait;
    end if;
    SkipInitProblems : loop
      wait on Ref until (EDGE = RISING and rising_edge(Ref)) or 
                        (EDGE = FALLING and falling_edge(Ref));
         -- nothing before first edge of Ref
      LastEdge := now;
      wait for SETUP;
      if HOLD >= 0 ns then exit when Data'event or Data'last_event > 0 ns;
      else exit when DelayedData'event or DelayedData'last_event > 0 ns;
      end if;
    end loop SkipInitProblems;

    Infinite : loop
      EdgeDetect := (EDGE = RISING and rising_edge(Ref)) or 
                    (EDGE = FALLING and falling_edge(Ref));
                    
      if EN_CHECKING then
        --------------------
        if SETUP > 0 ns and HOLD >= 0 ns and EdgeDetect then
          DeltaT := Data'last_event;
          assert not(DeltaT <= SETUP) 
            report "Setup violation on " & PATH & " at time " & ToString(now) &
             ". Observed : " & ToString(DeltaT) & ". Setup : " & ToString(SETUP)
            severity warning;
        end if;
        if HOLD > 0 ns and SETUP >= 0 ns and Data'event then
          DeltaT := now - LastEdge;
          assert not(DeltaT <= HOLD)
          report "Hold violation on " & PATH & " at time " & ToString(now) &
            ". Observed : " & ToString(DeltaT) & ". Hold : " & ToString(HOLD)
          severity warning;
        end if;
        --------------------
        if SETUP < 0 ns and Data'event then
          DeltaT := now - LastEdge;
          assert not(-SETUP <= DeltaT and DeltaT <= HOLD)
          report "Timing violation on " & PATH & " at time " & ToString(now)
            & ". Observed : " & ToString(DeltaT) & 
              ". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
          severity warning;
        end if;	-- nothing if no data event
        --------------------
        if HOLD < 0 ns and EdgeDetect then
          DeltaT := HOLD - DelayedData'last_event;
          assert not(-SETUP <= DeltaT and DeltaT <= HOLD)
          report "Timing violation on " & PATH & " at time " & ToString(now)
            & ". Observed : " & ToString(DeltaT) & 
              ". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
					severity warning;
        end if;	-- nothing if no edge detected
        --------------------
      end if;

      if EdgeDetect then LastEdge := now; end if;
      wait on Ref, Data;
    end loop Infinite;
  end CondSetupHoldCheck;	-- procedure

  -------------------------------------  
  procedure CondSetupHoldCheck (signal Data   : std_logic_vector;	
                                signal Ref    : std_ulogic;
                                constant EDGE : edge_type := RISING;
                                constant SETUP, HOLD : time := 0 ns;
                                constant PATH : string := "";
                                signal DelayedData : std_logic_vector;
                                signal EN_CHECKING : boolean) is
    variable LastEdge : time;
    variable DeltaT : time;
    variable EdgeDetect : boolean;
  begin
    if (not CHECK_ON) or (SETUP = 0 ns and HOLD = 0 ns) then wait; end if;
    if SETUP + HOLD <= 0 ns then
       assert FALSE report "Impossible check on " & PATH &  
        ". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
       severity warning;
       wait;
    end if;
    SkipInitProblems : loop
      wait on Ref until (EDGE = RISING and rising_edge(Ref)) or 
                        (EDGE = FALLING and falling_edge(Ref));
         -- nothing before first edge of Ref
      LastEdge := now;
      if HOLD >= 0 ns then exit when Data'event or Data'last_event > 0 ns;
      else exit when DelayedData'event or DelayedData'last_event > 0 ns;
      end if;
    end loop SkipInitProblems;

    Infinite : loop
      EdgeDetect := (EDGE = RISING and rising_edge(Ref)) or 
                    (EDGE = FALLING and falling_edge(Ref));
                    
      if EN_CHECKING then
        --------------------
        if SETUP > 0 ns and HOLD >= 0 ns and EdgeDetect then
          DeltaT := Data'last_event;
          assert not(DeltaT <= SETUP) 
            report "Setup violation on " & PATH & " at time " & ToString(now) &
             ". Observed : " & ToString(DeltaT) & ". Setup : " & ToString(SETUP)
            severity warning;
        end if;
        if HOLD > 0 ns and SETUP >= 0 ns and Data'event then
          DeltaT := now - LastEdge;
          assert not(DeltaT <= HOLD)
          report "Hold violation on " & PATH & " at time " & ToString(now) &
            ". Observed : " & ToString(DeltaT) & ". Hold : " & ToString(HOLD)
          severity warning;
        end if;
        --------------------
        if SETUP < 0 ns and Data'event then
          DeltaT := now - LastEdge;
          assert not(-SETUP <= DeltaT and DeltaT <= HOLD)
          report "Timing violation on " & PATH & " at time " & ToString(now)
            & ". Observed : " & ToString(DeltaT) & 
              ". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
          severity warning;
        end if;	-- nothing if no data event
        --------------------
        if HOLD < 0 ns and EdgeDetect then
          DeltaT := HOLD - DelayedData'last_event;
          assert not(-SETUP <= DeltaT and DeltaT <= HOLD)
          report "Timing violation on " & PATH & " at time " & ToString(now)
            & ". Observed : " & ToString(DeltaT) & 
              ". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
					severity warning;
        end if;	-- nothing if no edge detected
        --------------------
      end if;

      if EdgeDetect then LastEdge := now; end if;
      wait on Ref, Data;
    end loop Infinite;
  end CondSetupHoldCheck;	-- procedure
  	
	-----------------------------------------------------------
	procedure PulseCheck (
		signal Pin : std_ulogic;
		constant LEVEL : std_ulogic;
		constant WIDTH : time := 0 ns;
		constant SENSE : sense_type := MINIMUM;
		constant PATH : string := "") is
	
		variable PreviousEvent : time := 0 ns;
		variable Detection : boolean := FALSE;
		variable DeltaT : time;
	begin
		if (not CHECK_ON) or WIDTH = 0 ns then wait; end if;
		if WIDTH < 0 ns then
			assert FALSE
				report "Impossible check on " & PATH & ". WIDTH : " & ToString(WIDTH)
				severity warning;
			wait;
		end if;
		wait on Pin;	-- no check before first event on Pin
		loop
			DeltaT := now - PreviousEvent;
			case SENSE is
				when MINIMUM => 
					assert not(Detection and (DeltaT < WIDTH))
						report "Min pulse width violation on " & PATH & " for level " & 
							ToString(LEVEL) & " at time " & ToString(now) & ". Observed : "
								& ToString(DeltaT) & ". Required : " & ToString(WIDTH)
						severity warning;
				when MAXIMUM => 
					assert not(Detection and (DeltaT > WIDTH))
						report "Max pulse width violation on " & PATH & " for level " & 
							ToString(LEVEL) & " at time " & ToString(now) & ". Observed : "
								& ToString(DeltaT) & ". Required : " & ToString(WIDTH)
						severity warning;
			end case;
			PreviousEvent := now;
			Detection := (Pin = LEVEL) and (PreviousEvent /= 0 ns);
			wait on Pin;
		end loop;	-- Infinite loop
	end PulseCheck;	-- procedure
	
	-----------------------------------------------------------
	-- function CurveValue : used by CalcDelay

	function CurveValue (
		REF : curve_parameter;
		VALUE : real) return real is
		variable Res : real := 0.0;
	begin
		for i in Ref'high downto Ref'low loop
			Res := Ref(i) + Res*Value;
		end loop;
		return Res;
	end CurveValue;
	
	-----------------------------------------------------------

	function CalcDelay (
		BASE : time;
		TECH : technology;
		T : temperature;
		V : voltage;
		PROCES : proces_type;
		LOAD : capacitance := 50 pF) return time is

    constant CAPADEFAULT : capacitance := 50 pF;
    variable Kp : real;
	begin
		case PROCES is
			when WORST => Kp := TECH.KpMax;
			when BEST => Kp := TECH.KpMin;
			when TYPICAL => Kp := 1.0;
		end case;
		return	((BASE + 1 ns * ((LOAD - CAPADEFAULT)/ (TECH.LoadSlope * 1 pF) ) ) *
					   CurveValue(TECH.TCurve, real(T / 1 Celsius) ) *
					   CurveValue(TECH.VCurve, real(0.001 * (V / 1 mV) ) ) *
					 	 Kp) / 
					 	(TECH.KpMax * 
						 CurveValue(TECH.TCurve, real(125 Celsius / 1 Celsius) ) *
						 CurveValue(TECH.VCurve, real(0.001 * (4500 mV / 1 mV) ) ) 
						);
	end CalcDelay;

	-----------------------------------------------------------
	function TpDelay (NewData : std_ulogic;
										TPLH, TPHL : time := 0 ns) return time is
	begin
		case NewData is 
			when '0' | 'L' => return TPHL;
			when '1' | 'H' => return TPLH;
			when others => return MaxTime(TPLH, TPHL);
		end case;
	end TpDelay;	-- function

	---------------------

	function TpDelay (OldData, NewData : std_ulogic;
										TPLH, TPHL : time := 0 ns;
										TPZH, TPZL : time := 0 ns;
										TPHZ, TPLZ : time := 0 ns) return time is
	begin
		case OldData is 
			when '0' | 'L' =>
				case NewData is 
					when '1' | 'H' => return TPLH;
					when 'Z' => return TPLZ;
					when others => return TPLH;
				end case;
			when '1' | 'H' =>
				case NewData is 
					when '0' | 'L' => return TPHL;
					when 'Z' => return TPHZ;
					when others => return TPHL;
				end case;
			when 'Z' => 
				case NewData is 
					when '0' | 'L' => return TPZL;
					when '1' | 'H' => return TPZH;
					when others => return MaxTime(TPZH, TPZL);
				end case;
			when others =>
				case NewData is 
					when '0' | 'L' => return TPHL;
					when '1' | 'H' => return TPLH;
					when 'Z' => return MaxTime(TPLZ, TPHZ);
					when others => return 0 ns;
				end case;
		end case;
	end TpDelay;	-- function
		
	---------------------
	
	function TpDelay (OldData, NewData : std_ulogic_vector;
										TPD : time := 0 ns;
										TPZD, TPDZ : time := 0 ns) return time is
		variable AllZ : std_ulogic_vector(OldData'range) := (others => 'Z');
	begin
		if OldData = AllZ then
			return TPZD;
		elsif NewData = AllZ then
			return TPDZ;
		else return TPD;
		end if;
	end TpDelay;	-- function
	
	---------------------
	
	function TpDelay (OldData, NewData : std_logic_vector;
										TPD : time := 0 ns;
										TPZD, TPDZ : time := 0 ns) return time is
		variable AllZ : std_logic_vector(OldData'range) := (others => 'Z');
	begin
		if OldData = AllZ then
			return TPZD;
		elsif NewData = AllZ then
			return TPDZ;
		else return TPD;
		end if;
	end TpDelay;	-- function
		
	-----------------------------------------------------------

end StdTiming;	-- package body

------------------------------------------------------------
-- File name : stdrtl.vhd
-- Title : StdRtl
-- project : SPARC
-- Library : MMS
-- Author(s) : E. Laubacher
-- Purpose : package for Register Transfer Level declarations
-- notes : 
------------------------------------------------------------
-- Modification history :
------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
------------------------------------------------------------
-- v 1.0        | EL     | 92/06/14    | first version
------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

package StdRtl is

	type unknown_table is array(std_ulogic'low to std_ulogic'high) of boolean;
	---------------------------------------------------------------
	-- *Unknown return FALSE iff parameter contains only ('0', '1', 'L', 'H')
	---------------------------------------------------------------
	constant BitUnknown : unknown_table;
	function VecUnknown (A : std_ulogic_vector) return boolean;
	function VecUnknown (A : std_logic_vector) return boolean;

	type X_table is array(std_ulogic'low to std_ulogic'high) of boolean;
	---------------------------------------------------------------
	-- *X return TRUE iff parameter contains 'X' or 'W'
	---------------------------------------------------------------
	constant BitX : X_table;
	function VecX (A : std_ulogic_vector) return boolean;
	function VecX (A : std_logic_vector) return boolean;

	type bit_to_std_ulogic_table is 
		array(bit'low to bit'high) of std_ulogic;
	---------------------------------------------------------------
	-- simple conversions
	---------------------------------------------------------------
	constant ToStdUlogic : bit_to_std_ulogic_table;
	function ToStdUlogicVector(A : bit_vector) return std_ulogic_vector;
	function ToStdLogicVector(A : bit_vector) return std_logic_vector;
	function ToStdUlogicVector(A : std_logic_vector) return std_ulogic_vector;
	function ToStdLogicVector(A : std_ulogic_vector) return std_logic_vector;

	type std_ulogic_to_bit_table is 
		array(std_ulogic'low to std_ulogic'high) of bit;
	---------------------------------------------------------------
	-- simple conversions
	-- assert not VecUnknown(A)
	---------------------------------------------------------------
	constant ToBit : std_ulogic_to_bit_table;
	function ToBitVector(A : std_ulogic_vector) return bit_vector;
	function ToBitVector(A : std_logic_vector) return bit_vector;

	---------------------------------------------------------------
	-- returns equivalent of A in 2-complement
	-- if one parameter, size fits exactly for result
	-- if two parameters, the second parameter is the size
	---------------------------------------------------------------
	function ToBitVector (A : integer) return bit_vector;
	function ToStdUlogicVector (A : integer) return std_ulogic_vector;
	function ToStdLogicVector (A : integer) return std_logic_vector;
	function ToBitVector (A : integer; L : positive) return bit_vector;
	function ToStdUlogicVector (A : integer; L : positive)
						return std_ulogic_vector;
	function ToStdLogicVector (A : integer; L : positive)
						return std_logic_vector;

	---------------------------------------------------------------
	-- ToInteger returns integer equivalent of A in 2-complement
	-- The following usage is valid for a 32-bit implementation only
	-- assert A'length <= 32
	-- assert not VecUnknown(A)
	---------------------------------------------------------------
	function ToInteger (A : bit_vector) return integer;
	function ToInteger (A : std_ulogic_vector) return integer;
	function ToInteger (A : std_logic_vector) return integer;

	---------------------------------------------------------------
	-- ToNatural returns integer equivalent of A in unsigned notation
	-- The following usage is valid for a 32-bit implementation only
	-- assert A'length < 32
	-- assert not VecUnknown(A)
	---------------------------------------------------------------
	function ToNatural (A : bit_vector) return natural;
	function ToNatural (A : std_ulogic_vector) return natural;
	function ToNatural (A : std_logic_vector) return natural;

	---------------------------------------------------------------
	-- Overloaded arithmetic operators in 2-complement
	-- Return boolean or vector of the size of the largest operand
	-- Therefore, "+" and "-" also work in unsigned
	-- Comparison operators are not overloaded as implicit versions exist, and
	--  some compilers issue an error if an explicit version is also available
	--  So the following correspondence applies :
	--   ">"   -->  Greater
	--   ">="  -->  GreaterEqual
	--   "<"   -->  Less
	--   "<="  -->  LessEqual
	--
	-- the comparison can be done in unsigned by concatenating a '0' at left
	--  ex : Less('0' & A, '0' & B)
	-- All binary operators assert operands not unknown
	---------------------------------------------------------------
	function "+" (A, B : bit_vector) return bit_vector;
	function "+" (A, B : std_ulogic_vector) return std_ulogic_vector;
	function "+" (A, B : std_logic_vector) return std_logic_vector;
	function "+" (A : bit_vector; B : integer) return bit_vector;
	function "+" (A : std_ulogic_vector; B : integer) return std_ulogic_vector;
	function "+" (A : std_logic_vector; B : integer) return std_logic_vector;
	function "+" (A : integer; B : bit_vector) return bit_vector;
	function "+" (A : integer; B : std_ulogic_vector) return std_ulogic_vector;
	function "+" (A : integer; B : std_logic_vector) return std_logic_vector;
	function "-" (A, B : bit_vector) return bit_vector;
	function "-" (A, B : std_ulogic_vector) return std_ulogic_vector;
	function "-" (A, B : std_logic_vector) return std_logic_vector;
	function "-" (A : bit_vector; B : integer) return bit_vector;
	function "-" (A : std_ulogic_vector; B : integer) return std_ulogic_vector;
	function "-" (A : std_logic_vector; B : integer) return std_logic_vector;
	function "-" (A : integer; B : bit_vector) return bit_vector;
	function "-" (A : integer; B : std_ulogic_vector) return std_ulogic_vector;
	function "-" (A : integer; B : std_logic_vector) return std_logic_vector;
	function Less (A, B : bit_vector) return boolean;
	function Less (A, B : std_ulogic_vector) return boolean;
	function Less (A, B : std_logic_vector) return boolean;
	function LessEqual (A, B : bit_vector) return boolean;
	function LessEqual (A, B : std_ulogic_vector) return boolean;
	function LessEqual (A, B : std_logic_vector) return boolean;
	function Greater (A, B : bit_vector) return boolean;
	function Greater (A, B : std_ulogic_vector) return boolean;
	function Greater (A, B : std_logic_vector) return boolean;
	function GreaterEqual (A, B : bit_vector) return boolean;
	function GreaterEqual (A, B : std_ulogic_vector) return boolean;
	function GreaterEqual (A, B : std_logic_vector) return boolean;
	function "abs" (A : bit_vector) return bit_vector;
	function "abs" (A : std_ulogic_vector) return std_ulogic_vector;
	function "abs" (A : std_logic_vector) return std_logic_vector;
	function "-" (A : bit_vector) return bit_vector;
	function "-" (A : std_ulogic_vector) return std_ulogic_vector;
	function "-" (A : std_logic_vector) return std_logic_vector;

	---------------------------------------------------------------
	-- shift functions : shift vector left (ShiftL) of right (ShiftR)
	-- if no FILL value is given, ShiftR use sign for filling
	-- result has same length as parameter, outputing bits are dropped
	---------------------------------------------------------------
	function ShiftL (	A : bit_vector;
										N : natural := 1;
										FILL : bit := '0') return bit_vector;
	function ShiftL (	A : std_ulogic_vector;
										N : natural := 1;
										FILL : std_ulogic := '0') return std_ulogic_vector;
	function ShiftL (	A : std_logic_vector;
										N : natural := 1;
										FILL : std_logic := '0') return std_logic_vector;
	function ShiftR (	A : bit_vector;
										N : natural := 1) return bit_vector;
	function ShiftR (	A : bit_vector;
										N : natural := 1;
										FILL : bit) return bit_vector;
	function ShiftR (	A : std_ulogic_vector;
										N : natural := 1) return std_ulogic_vector;
	function ShiftR (	A : std_ulogic_vector;
										N : natural := 1;
										FILL : std_ulogic) return std_ulogic_vector;
	function ShiftR (	A : std_logic_vector;
										N : natural := 1) return std_logic_vector;
	function ShiftR (	A : std_logic_vector;
										N : natural := 1;
										FILL : std_logic) return std_logic_vector;

	---------------------------------------------------------------
	-- extend functions : extend vector A to N bits
	-- if no FILL value is given, Extend use sign for filling
	-- if N < A'length, most significant bits of A are dropped
	---------------------------------------------------------------
	function Extend (	A : bit_vector;
										N : positive) return bit_vector;
	function Extend (	A : bit_vector;
										N : positive;
										FILL : bit) return bit_vector;
	function Extend (	A : std_ulogic_vector;
										N : positive) return std_ulogic_vector;
	function Extend (	A : std_ulogic_vector;
										N : positive;
										FILL : std_ulogic) return std_ulogic_vector;
	function Extend (	A : std_logic_vector;
										N : positive) return std_logic_vector;
	function Extend (	A : std_logic_vector;
										N : positive;
										FILL : std_logic) return std_logic_vector;

	---------------------------------------------------------------
	-- sign function : return left element of vector
	---------------------------------------------------------------
	function Sign(A : bit_vector) return bit;
	function Sign(A : std_ulogic_vector) return std_ulogic;
	function Sign(A : std_logic_vector) return std_logic;

	---------------------------------------------------------------
	-- multiply subprograms
	-- default sign type is 2-complement
	-- to get unsigned calculation, add parameter UNSIGNED
	--  example : Res := Mul(A, B);	-- 2-complement
	--						Res := Mul(A, B, UNSIGNED);	-- unsigned
	-- Mul returns a vector of size A'length + B'length
	-- Div performs euclidian division. 
	-- 	A = B*Q + R
	--	Sign(R) = sign(A)
	--	Abs(R) < Abs(B)
	-- Div computes the quotient Q of length Dividand'length + 1
	-- Div computes the remainder R of the same length as the divisor B
	-- Division by zero issues a warning and returns "0...0" for bit_vector
	--   or "X...X" for std_*logic
	-- The size of parameters is checked
	-- All subprograms assert parameters not unknown
	---------------------------------------------------------------
	type format_type is (SIGNED, UNSIGNED);
	function Mul (A, B : bit_vector; FORMAT : format_type := SIGNED)
								return bit_vector;
	function Mul (A, B : std_ulogic_vector; FORMAT : format_type := SIGNED)
								return std_ulogic_vector;
	function Mul (A, B : std_logic_vector; FORMAT : format_type := SIGNED)
								return std_logic_vector;
	procedure Div (A, B : in bit_vector; Q, R : out bit_vector; 
								 FORMAT : in format_type := SIGNED);
	procedure Div (A, B : in std_ulogic_vector; Q, R : out std_ulogic_vector;
								 FORMAT : in format_type := SIGNED);
	procedure Div (A, B : in std_logic_vector; Q, R : out std_logic_vector;
								 FORMAT : in format_type := SIGNED);
end StdRtl;	-- package

------------------------------------------------------------
------------------------------------------------------------

package body StdRtl is

	----------------------------
	constant BitUnknown : unknown_table := (
		'U' => TRUE,
		'X' => TRUE,
		'0' => FALSE,
		'1' => FALSE,
		'Z' => TRUE,
		'W' => TRUE,
		'L' => FALSE,
		'H' => FALSE,
		'-' => TRUE);
	----------------------------

	function VecUnknown (A : std_ulogic_vector) return boolean is
	begin
		for i in A'range loop
			if BitUnknown(A(I)) then return TRUE; end if;
		end loop;
		return FALSE;
	end VecUnknown;	-- function

	----------------------------

	function VecUnknown (A : std_logic_vector) return boolean is
	begin
		for i in A'range loop
			if BitUnknown(A(I)) then return TRUE; end if;
		end loop;
		return FALSE;
	end VecUnknown;	-- function

	----------------------------

	constant BitX : X_table := (
		'U' => FALSE,
		'X' => TRUE,
		'0' => FALSE,
		'1' => FALSE,
		'Z' => FALSE,
		'W' => TRUE,
		'L' => FALSE,
		'H' => FALSE,
		'-' => FALSE);

	----------------------------

	function VecX (A : std_ulogic_vector) return boolean is
	begin
		for i in A'range loop
			if BitX(A(I)) then return TRUE; end if;
		end loop;
		return FALSE;
	end VecX;	-- function


	----------------------------

	function VecX (A : std_logic_vector) return boolean is
	begin
		for i in A'range loop
			if BitX(A(I)) then return TRUE; end if;
		end loop;
		return FALSE;
	end VecX;	-- function

	----------------------------

	constant ToStdUlogic : bit_to_std_ulogic_table := ('0' => '0', '1' => '1');

	----------------------------

	function ToStdUlogicVector(A : bit_vector) return std_ulogic_vector is
		variable Res : std_ulogic_vector (A'range);
	begin
		for i in A'range loop
			Res(i) := ToStdUlogic(A(i));
		end loop;
		return Res;
	end ToStdUlogicVector;

	----------------------------

	function ToStdLogicVector(A : bit_vector) return std_logic_vector is
		variable Res : std_logic_vector (A'range);
	begin
		for i in A'range loop
			Res(i) := ToStdUlogic(A(i));
		end loop;
		return Res;
	end ToStdLogicVector;

	----------------------------

	function ToStdUlogicVector(A : std_logic_vector) return std_ulogic_vector is
		variable Res : std_ulogic_vector (A'range);
	begin
		for i in A'range loop
			Res(i) := A(i);
		end loop;
		return Res;
	end ToStdUlogicVector;

	----------------------------

	function ToStdLogicVector(A : std_ulogic_vector) return std_logic_vector is
		variable Res : std_logic_vector (A'range);
	begin
		for i in A'range loop
			Res(i) := A(i);
		end loop;
		return Res;
	end ToStdLogicVector;

	----------------------------
	constant ToBit : std_ulogic_to_bit_table := (
		'U' => '0',
		'X' => '0',
		'0' => '0',
		'1' => '1',
		'Z' => '0',
		'W' => '0',
		'L' => '0',
		'H' => '1',
		'-' => '0');

	----------------------------

	function ToBitVector(A : std_ulogic_vector) return bit_vector is
		variable Res : bit_vector (A'range);
	begin
		assert not VecUnknown(A) report "Unknown value in conversion !"
			severity warning;
		for i in A'range loop
			Res(i) := ToBit(A(i));
		end loop;
		return Res;
	end ToBitVector;

	----------------------------

	function ToBitVector(A : std_logic_vector) return bit_vector is
		variable Res : bit_vector (A'range);
	begin
		assert not VecUnknown(A) report "Unknown value in conversion !"
			severity warning;
		for i in A'range loop
			Res(i) := ToBit(A(i));
		end loop;
		return Res;
	end ToBitVector;

	----------------------------

	function ToBitVector (A : integer) return bit_vector is
		variable Res : bit_vector (63 downto 0);
		variable Buf : integer;
		variable I : integer := 0;	-- index 
	begin
		if A >= 0 then
			Buf := A;
			Res := (others => '0');
			loop
				exit when Buf = 0;
				if Buf rem 2 = 1 then Res(I) := '1'; end if;
				Buf := Buf / 2;
				I := I + 1;
			end loop;
		else
			Buf := -A - 1;
			Res := (others => '1');
			loop
				if Buf rem 2 = 1 then Res(I) := '0'; end if;
				Buf := Buf / 2;
				I := I + 1;
				exit when Buf = 0;
			end loop;
		end if;
		return Res(I downto 0);
	end ToBitVector;

	----------------------------

	function ToStdUlogicVector (A : integer) return std_ulogic_vector is
	begin
		return ToStdUlogicVector(ToBitVector(A));
	end ToStdUlogicVector;

	----------------------------

	function ToStdLogicVector (A : integer) return std_logic_vector is
	begin
		return ToStdLogicVector(ToBitVector(A));
	end ToStdLogicVector;

	----------------------------

	function ToBitVector (A : integer; L : positive) return bit_vector is
		variable Res : bit_vector (L- 1 downto 0);
		variable Buf : integer;
	begin
		if A >= 0 then
			Buf := A;
			Res := (others => '0');
			for i in 0 to L - 1 loop
				if Buf rem 2 = 1 then Res(i) := '1'; end if;
				Buf := Buf / 2;
				exit when Buf = 0;
			end loop;
		else
			Buf := -A - 1;
			Res := (others => '1');
			for i in 0 to L - 1 loop
				if Buf rem 2 = 1 then Res(i) := '0'; end if;
				Buf := Buf / 2;
				exit when Buf = 0;
			end loop;
		end if;
		return Res;
	end ToBitVector;

	----------------------------

	function ToStdUlogicVector (A : integer; L : positive)
		return std_ulogic_vector is
	begin
		return ToStdUlogicVector(ToBitVector(A, L));
	end ToStdUlogicVector;

	----------------------------

	function ToStdLogicVector (A : integer; L : positive) 
		return std_logic_vector is
	begin
		return ToStdLogicVector(ToBitVector(A, L));
	end ToStdLogicVector;

	----------------------------

	function ToInteger (A : bit_vector) return integer is
		constant L : integer := A'length;
		alias Ai : bit_vector(L - 1 downto 0) is A;
		variable Res : integer := 0;
	begin
		assert L <= 32 report "Integer conversion overflow" severity warning;
		if Ai(Ai'left) = '1' then Res := -1; end if;	-- negative
		for i in L - 1 downto 0 loop	
			Res := Res * 2;
			if Ai(i) = '1' then Res := Res + 1; end if;
		end loop;
		return Res;
	end ToInteger;

	----------------------------

	function ToInteger (A : std_ulogic_vector) return integer is
	begin
		-- assert in ToBitVector
		return ToInteger(ToBitVector(A));
	end ToInteger;
	----------------------------

	function ToInteger (A : std_logic_vector) return integer is
	begin
		-- assert in ToBitVector
		return ToInteger(ToBitVector(A));
	end ToInteger;

	----------------------------

	function ToNatural (A : bit_vector) return natural is
		constant L : integer := A'length;
		alias Ai : bit_vector(L - 1 downto 0) is A;
		variable Res : natural := 0;
	begin
		assert L < 32 report "Integer conversion overflow" severity warning;
		for i in L - 1 downto 0 loop	
			Res := Res * 2;
			if Ai(i) = '1' then Res := Res + 1; end if;
		end loop;
		return Res;
	end ToNatural;

	----------------------------

	function ToNatural (A : std_ulogic_vector) return natural is
	begin
		-- assert in ToBitVector
		return ToNatural(ToBitVector(A));
	end ToNatural;

	----------------------------

	function ToNatural (A : std_logic_vector) return natural is
	begin
		-- assert in ToBitVector
		return ToNatural(ToBitVector(A));
	end ToNatural;

	----------------------------
	-- local function
	----------------------------
	
	function Max (A, B : integer) return integer is
	begin
		if A >= B then return A;
		else return B;
		end if;
	end Max;	-- function

	----------------------------

	function "+" (A, B : bit_vector) return bit_vector is
		constant A_L : integer := A'length;
		constant B_L : integer := B'length;
		alias Ai : bit_vector (A_L - 1 downto 0) is A;
		alias Bi : bit_vector (B_L - 1 downto 0) is B;
		variable Res : bit_vector (Max(A_L, B_L) - 1 downto 0);
		variable A1, A2, C : bit := '0';
	begin
		for i in 0 to Res'length - 1 loop
			if i < A_L then A1 := Ai(i); end if;	-- else keep last value (sign)
			if i < B_L then A2 := Bi(i); end if;	-- else keep last value (sign)
			Res(i) := A1 xor A2 xor C;
			C := (A1 and A2) or (A1 and C) or (A2 and C);
		end loop;
		return Res;
	end "+";	-- function

	----------------------------

	function "+" (A, B : std_ulogic_vector) return std_ulogic_vector is
	begin
		-- assertion in ToBitVector
		return ToStdUlogicVector(ToBitVector(A) + ToBitVector(B));
	end "+";	-- function

	----------------------------

	function "+" (A, B : std_logic_vector) return std_logic_vector is
	begin
		-- assertion in ToBitVector
		return ToStdLogicVector(ToBitVector(A) + ToBitVector(B));
	end "+";	-- function

	----------------------------

	function "+" (A : bit_vector; B : integer) return bit_vector is
	begin
		return A + ToBitVector(B);
	end "+";	-- function

	----------------------------

	function "+" (A : std_ulogic_vector; B : integer) return std_ulogic_vector is
	begin
		-- assertion in ToBitVector
		return ToStdUlogicVector(ToBitVector(A) + ToBitVector(B));
	end "+";	-- function

	----------------------------

	function "+" (A : std_logic_vector; B : integer) return std_logic_vector is
	begin
		-- assertion in ToBitVector
		return ToStdLogicVector(ToBitVector(A) + ToBitVector(B));
	end "+";	-- function

	----------------------------

	function "+" (A : integer; B : bit_vector) return bit_vector is
	begin
		return ToBitVector(A) + B;
	end "+";	-- function

	----------------------------

	function "+" (A : integer; B : std_ulogic_vector) return std_ulogic_vector is
	begin
		-- assertion in ToBitVector
		return ToStdUlogicVector(ToBitVector(A) + ToBitVector(B));
	end "+";	-- function

	----------------------------

	function "+" (A : integer; B : std_logic_vector) return std_logic_vector is
	begin
		-- assertion in ToBitVector
		return ToStdLogicVector(ToBitVector(A) + ToBitVector(B));
	end "+";	-- function

	----------------------------

	function "-" (A, B : bit_vector) return bit_vector is
		constant A_L : integer := A'length;
		constant B_L : integer := B'length;
		alias Ai : bit_vector (A_L - 1 downto 0) is A;
		alias Bi : bit_vector (B_L - 1 downto 0) is B;
		variable Res : bit_vector (Max(A_L, B_L) - 1 downto 0);
		variable A1, A2, C : bit := '0';
	begin
		for i in 0 to Res'length - 1 loop
			if i < A_L then A1 := Ai(i); end if;	-- else keep last value (sign)
			if i < B_L then A2 := Bi(i); end if;	-- else keep last value (sign)
			Res(i) := A1 xor A2 xor C;
			C := ((not A1) and A2) or ((not A1) and C) or (A2 and C);
		end loop;
		return Res;
	end "-";	-- function

	----------------------------

	function "-" (A, B : std_ulogic_vector) return std_ulogic_vector is
	begin
		-- assertion in ToBitVector
		return ToStdUlogicVector(ToBitVector(A) - ToBitVector(B));
	end "-";	-- function

	----------------------------

	function "-" (A, B : std_logic_vector) return std_logic_vector is
	begin
		-- assertion in ToBitVector
		return ToStdLogicVector(ToBitVector(A) - ToBitVector(B));
	end "-";	-- function

	----------------------------

	function "-" (A : bit_vector; B : integer) return bit_vector is
	begin
		return A - ToBitVector(B);
	end "-";	-- function

	----------------------------

	function "-" (A : std_ulogic_vector; B : integer) return std_ulogic_vector is
	begin
		-- assertion in ToBitVector
		return ToStdUlogicVector(ToBitVector(A) - ToBitVector(B));
	end "-";	-- function

	----------------------------

	function "-" (A : std_logic_vector; B : integer) return std_logic_vector is
	begin
		-- assertion in ToBitVector
		return ToStdLogicVector(ToBitVector(A) - ToBitVector(B));
	end "-";	-- function

	----------------------------

	function "-" (A : integer; B : bit_vector) return bit_vector is
	begin
		return ToBitVector(A) - B;
	end "-";	-- function

	----------------------------

	function "-" (A : integer; B : std_ulogic_vector) return std_ulogic_vector is
	begin
		-- assertion in ToBitVector
		return ToStdUlogicVector(ToBitVector(A) - ToBitVector(B));
	end "-";	-- function

	----------------------------

	function "-" (A : integer; B : std_logic_vector) return std_logic_vector is
	begin
		-- assertion in ToBitVector
		return ToStdLogicVector(ToBitVector(A) - ToBitVector(B));
	end "-";	-- function

	----------------------------

	function Less (A, B : bit_vector) return boolean is
	begin
		return Sign((Sign(A) & A) - (Sign(B) & B)) = '1';
	end Less;	-- function
	----------------------------

	function Less (A, B : std_ulogic_vector) return boolean is
	begin
		-- assertion in ToBitVector
		return Less(ToBitVector(A), ToBitVector(B));
	end Less;	-- function

	----------------------------

	function Less (A, B : std_logic_vector) return boolean is
	begin
		-- assertion in ToBitVector
		return Less(ToBitVector(A), ToBitVector(B));
	end Less;	-- function

	----------------------------

	function LessEqual (A, B : bit_vector) return boolean is
	begin
		return not Greater(A, B);
	end LessEqual;	-- function

	----------------------------

	function LessEqual (A, B : std_ulogic_vector) return boolean is
	begin
		-- assertion in ToBitVector
		return LessEqual(ToBitVector(A), ToBitVector(B));
	end LessEqual;	-- function

	----------------------------

	function LessEqual (A, B : std_logic_vector) return boolean is
	begin
		-- assertion in ToBitVector
		return LessEqual(ToBitVector(A), ToBitVector(B));
	end LessEqual;	-- function

	----------------------------

	function Greater (A, B : bit_vector) return boolean is
	begin
		return Sign((Sign(B) & B) - (Sign(A) & A)) = '1';
	end Greater;	-- function

	----------------------------

	function Greater (A, B : std_ulogic_vector) return boolean is
	begin
		-- assertion in ToBitVector
		return Greater(ToBitVector(A), ToBitVector(B));
	end Greater;	-- function

	----------------------------

	function Greater (A, B : std_logic_vector) return boolean is
	begin
		-- assertion in ToBitVector
		return Greater(ToBitVector(A), ToBitVector(B));
	end Greater;	-- function

	----------------------------

	function GreaterEqual (A, B : bit_vector) return boolean is
	begin
		return not Less(A, B);
	end GreaterEqual;	-- function

	----------------------------

	function GreaterEqual (A, B : std_ulogic_vector) return boolean is
	begin
		-- assertion in ToBitVector
		return GreaterEqual(ToBitVector(A), ToBitVector(B));
	end GreaterEqual;	-- function

	----------------------------

	function GreaterEqual (A, B : std_logic_vector) return boolean is
	begin
		-- assertion in ToBitVector
		return GreaterEqual(ToBitVector(A), ToBitVector(B));
	end GreaterEqual;	-- function

	----------------------------

	function "abs" (A : bit_vector) return bit_vector is
	begin
		if A(A'left) = '1' then return "0" - A;
		else return A;
		end if;
	end "abs";	-- function

	----------------------------

	function "abs" (A : std_ulogic_vector) return std_ulogic_vector is
	begin
		if A(A'left) = '1' then return "0" - A;
		else return A;
		end if;
	end "abs";	-- function

	----------------------------

	function "abs" (A : std_logic_vector) return std_logic_vector is
	begin
		if A(A'left) = '1' then return "0" - A;
		else return A;
		end if;
	end "abs";	-- function

	----------------------------

	function "-" (A : bit_vector) return bit_vector is
	begin
		return "0" - A;
	end "-";	-- function
	----------------------------

	function "-" (A : std_ulogic_vector) return std_ulogic_vector is
	begin
		return "0" - A;
	end "-";	-- function

	----------------------------

	function "-" (A : std_logic_vector) return std_logic_vector is
	begin
		return "0" - A;
	end "-";	-- function

	----------------------------

	function ShiftL (	A : bit_vector;
										N : natural := 1;
										FILL : bit := '0') return bit_vector is
		constant L : integer :=  A'length;
		variable Res : bit_vector(L - 1 downto 0) := (others => FILL);
		alias Ai : bit_vector(L - 1 downto 0) is A;
	begin
		if N < L then Res(L - 1 downto N) := Ai(L - 1 -N downto 0); end if;
		return Res;
	end ShiftL;	-- function

	----------------------------

	function ShiftL (	A : std_ulogic_vector;
										N : natural := 1;
										FILL : std_ulogic := '0') return std_ulogic_vector is
		constant L : integer :=  A'length;
		variable Res : std_ulogic_vector(L - 1 downto 0) := (others => FILL);
		alias Ai : std_ulogic_vector(L - 1 downto 0) is A;
	begin
		if N < L then Res(L - 1 downto N) := Ai(L - 1 -N downto 0); end if;
		return Res;
	end ShiftL;	-- function

	----------------------------

	function ShiftL (	A : std_logic_vector;
										N : natural := 1;
										FILL : std_logic := '0') return std_logic_vector is
		constant L : integer :=  A'length;
		variable Res : std_logic_vector(L - 1 downto 0) := (others => FILL);
		alias Ai : std_logic_vector(L - 1 downto 0) is A;
	begin
		if N < L then Res(L - 1 downto N) := Ai(L - 1 -N downto 0); end if;
		return Res;
	end ShiftL;	-- function

	----------------------------

	function ShiftR (	A : bit_vector;
										N : natural := 1) return bit_vector is
	begin
		return ShiftR(A, N, A(A'left));	-- signed
	end ShiftR;	-- function

	----------------------------

	function ShiftR (	A : bit_vector;
										N : natural := 1;
										FILL : bit) return bit_vector is
		constant L : integer :=  A'length;
		variable Res : bit_vector(L - 1 downto 0) := (others => FILL);
		alias Ai : bit_vector(L - 1 downto 0) is A;
	begin
		if N < L then Res(L - 1 - N downto 0) := Ai(L - 1 downto N); end if;
		return Res;
	end ShiftR;	-- function

	----------------------------

	function ShiftR (	A : std_ulogic_vector;
										N : natural := 1) return std_ulogic_vector is
	begin
		return ShiftR(A, N, A(A'left));	-- signed
	end ShiftR;	-- function
	----------------------------

	function ShiftR (	A : std_ulogic_vector;
										N : natural := 1;
										FILL : std_ulogic) return std_ulogic_vector is
		constant L : integer :=  A'length;
		variable Res : std_ulogic_vector(L - 1 downto 0) := (others => FILL);
		alias Ai : std_ulogic_vector(L - 1 downto 0) is A;
	begin
		if N < L then Res(L - 1 - N downto 0) := Ai(L - 1 downto N); end if;
		return Res;
	end ShiftR;	-- function

	----------------------------

	function ShiftR (	A : std_logic_vector;
										N : natural := 1) return std_logic_vector is
	begin
		return ShiftR(A, N, A(A'left));	-- signed
	end ShiftR;	-- function

	----------------------------

	function ShiftR (	A : std_logic_vector;
										N : natural := 1;
										FILL : std_logic) return std_logic_vector is
		constant L : integer :=  A'length;
		variable Res : std_logic_vector(L - 1 downto 0) := (others => FILL);
		alias Ai : std_logic_vector(L - 1 downto 0) is A;
	begin
		if N < L then Res(L - 1 - N downto 0) := Ai(L - 1 downto N); end if;
		return Res;
	end ShiftR;	-- function

	----------------------------

	function Extend (	A : bit_vector;
										N : positive) return bit_vector is
	begin
		return Extend(A, N, A(A'left));	-- signed
	end Extend;	-- function

	----------------------------

	function Extend (	A : bit_vector;
										N : positive;
										FILL : bit) return bit_vector is
		constant L : integer :=  A'length;
		variable Res : bit_vector(N - 1 downto 0) := (others => FILL);
		alias Ai : bit_vector(L - 1 downto 0) is A;
	begin
		if N < L then Res := Ai(N - 1 downto 0);
		else Res(L - 1 downto 0) := Ai;
		end if;
		return Res;
	end Extend;	-- function

	----------------------------

	function Extend (	A : std_ulogic_vector;
										N : positive) return std_ulogic_vector is
	begin
		return Extend(A, N, A(A'left));	-- signed
	end Extend;	-- function

	----------------------------

	function Extend (	A : std_ulogic_vector;
										N : positive;
										FILL : std_ulogic) return std_ulogic_vector is
		constant L : integer :=  A'length;
		variable Res : std_ulogic_vector(N - 1 downto 0) := (others => FILL);
		alias Ai : std_ulogic_vector(L - 1 downto 0) is A;
	begin
		if N < L then Res := Ai(N - 1 downto 0);
		else Res(L - 1 downto 0) := Ai;
		end if;
		return Res;
	end Extend;	-- function

	----------------------------

	function Extend (	A : std_logic_vector;
										N : positive) return std_logic_vector is
	begin
		return Extend(A, N, A(A'left));	-- signed
	end Extend;	-- function

	----------------------------

	function Extend (	A : std_logic_vector;
										N : positive;
										FILL : std_logic) return std_logic_vector is
		constant L : integer :=  A'length;
		variable Res : std_logic_vector(N - 1 downto 0) := (others => FILL);
		alias Ai : std_logic_vector(L - 1 downto 0) is A;
	begin
		if N < L then Res := Ai(N - 1 downto 0);
		else Res(L - 1 downto 0) := Ai;
		end if;
		return Res;
	end Extend;	-- function


	----------------------------

	function Sign(A : bit_vector) return bit is
	begin
		return A(A'left);
	end Sign;	--function

	----------------------------

	function Sign(A : std_ulogic_vector) return std_ulogic is
	begin
		return A(A'left);
	end Sign;	--function

	----------------------------

	function Sign(A : std_logic_vector) return std_logic is
	begin
		return A(A'left);
	end Sign;	--function

	----------------------------

	function Mul (A, B : bit_vector; FORMAT : format_type := SIGNED)
								return bit_vector is
		constant A_L : integer := A'length;
		constant B_L : integer := B'length;
		constant RES_L : integer := A_L + B_L;
		alias Ai : bit_vector (A_L - 1 downto 0) is A;
		alias Bi : bit_vector (B_L - 1 downto 0) is B;
		variable Res : bit_vector (RES_L - 1 downto 0) := (others => '0');
		variable Fill : bit;
	begin
		if (FORMAT = SIGNED) then Fill := Sign(Ai);
		else Fill := '0';
		end if;
		for i in 0 to B_L - 2 loop
			if Bi(i) = '1' then
				Res := Res + ShiftL(Extend(Ai, RES_L, Fill), i);
			end if;
		end loop;
		if Sign(Bi) = '1' then 
			if FORMAT = SIGNED then 
				Res := Res - ShiftL(Extend(Ai, RES_L, Fill), B_L - 1);
			else 
				Res := Res + ShiftL(Extend(Ai, RES_L, Fill), B_L - 1);
			end if;
		end if;
		return Res;
	end Mul;	-- function

	----------------------------

	function Mul (A, B : std_ulogic_vector; FORMAT : format_type := SIGNED)
								return std_ulogic_vector is
	begin
		-- assert in ToBitVector
		return ToStdUlogicVector(Mul(ToBitVector(A), ToBitVector(B), FORMAT));
	end Mul;	--function

	----------------------------

	function Mul (A, B : std_logic_vector; FORMAT : format_type := SIGNED)
								return std_logic_vector is
	begin
		-- assert in ToBitVector
		return ToStdLogicVector(Mul(ToBitVector(A), ToBitVector(B), FORMAT));
	end Mul;	--function

	----------------------------

	procedure Div (A, B : in bit_vector; Q, R : out bit_vector; 
								 FORMAT : in format_type := SIGNED) is
		constant A_L : integer := A'length;
		constant B_L : integer := B'length;
		variable Ai : bit_vector (A_L - 1 downto 0);
		variable Bi : bit_vector (B_L - 1 downto 0);
		variable Qi : bit_vector (A_L downto 0);
		variable Ri : bit_vector (B_L - 1 downto 0);		
		variable LeftI : integer;	-- left index of dividand
	begin
		assert Q'length = Qi'length report "wrong quotient size !" severity error;
		assert R'length = Ri'length report "wrong remainder size !" severity error;
		if B = Extend("0", B'length) then
		 	assert FALSE report "Division by zero !" severity warning;
			Q := Extend("0", Qi'length);
			R := Extend("0", Ri'length);		 	
			return;
		end if;

		if FORMAT = SIGNED then
			Ai := abs(A);
			Bi := abs(B);
		else
			Ai := A;
			Bi := B;
		end if;
		---------------------
		LeftI := A_L - 1;
		for i in A_L - 1 downto 0 loop
			if GreaterEqual('0' & Ai(LeftI downto i), '0' & Bi) then 
				Qi(i) := '1';
				Ai(LeftI downto i) := 
					Ai(LeftI downto i) - Extend(Bi, LeftI - i + 1, '0');
				LeftI := LeftI - 1;
			end if;
		end loop;
		Ri := Extend(Ai, B_L, '0');
		Qi(A_L) := '0';
			---------------------
		if FORMAT = SIGNED then 
			if Sign(A) /= Sign(B) then Qi := -Qi; end if;
			if Sign(A) = '1' then Ri := -Ri; end if;
		end if;
		Q := Qi;
		R := Ri;
	end Div;	-- procedure
	----------------------------

	procedure Div (A, B : in std_ulogic_vector; Q, R : out std_ulogic_vector;
								 FORMAT : in format_type := SIGNED) is
		variable Qi : bit_vector(A'length downto 0);
		variable Ri : bit_vector(B'length - 1 downto 0);
	begin
		-- assert in ToBitVector
		Div(ToBitVector(A), ToBitVector(B), Qi, Ri, FORMAT);
		if B = Extend("0", B'length) then
			Q := Extend("X", Q'length);
			R := Extend("X", R'length);
		else
			Q := ToStdUlogicVector(Qi);
			R := ToStdUlogicVector(Ri);
		end if;
	end Div;	-- procedure

	----------------------------

	procedure Div (A, B : in std_logic_vector; Q, R : out std_logic_vector;
								 FORMAT : in format_type := SIGNED) is
		variable Qi : bit_vector(A'length downto 0);
		variable Ri : bit_vector(B'length - 1 downto 0);
	begin
		-- assert in ToBitVector
		Div(ToBitVector(A), ToBitVector(B), Qi, Ri, FORMAT);
		if B = Extend("0", B'length) then
			Q := Extend("X", Q'length);
			R := Extend("X", R'length);
		else
			Q := ToStdLogicVector(Qi);
			R := ToStdLogicVector(Ri);
		end if;
	end Div;	-- procedure
	----------------------------

end StdRtl;	-- package body

------------------------------------------------------------
-- File name : stdmem.vhd
-- Title : StdMem
-- project : SPARC
-- Library : MMS
-- Author(s) : E. Laubacher
-- Purpose : package for memory modelling
-- notes : 
------------------------------------------------------------
-- Modification history :
-- 93/01/12 : New version of std_logic_1164. The function
--            Convert_to_X01 is now To_X01.
------------------------------------------------------------
-- Version No : | Author       | Mod. Date : | Changes made :
------------------------------------------------------------
-- v 1.0        | EL           | 92/06/14    | 1st version
--..........................................................
-- v 1.1 = v 1.0
--..........................................................
-- v 1.2        | MR - RC      | 92/10/06    | 2nd version
-- + bug in the procedure MemRead and MemWrite; the variable
--   RangeHigh takes a negative value (-1) during execution.
--..........................................................
-- v 1.3        | MR           | 93/01/12    | 3rd version
-- + New version of std_logic_1164. The function Convert_to_X01 
--   is now To_X01.
--..........................................................
-- v 1.4        | MR           | 93/06/01    | 4th version
-- + vicious bug discovered and fixed in procedure MemWrite: 
--   wrong handling of chained list of page records (pbs when
--   attempting to write data on 2 different and adjacent pages
--   with one call to MemWrite).
------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
library MMS;
use MMS.StdIoImp.all;
use std.textio.all;

package StdMem is
	constant PAGE_SIZE : integer := 128;
	subtype bit_width is integer range 1 to 15;
		-- 2 bits are necessary for each bit and an integer contains 32 bits
		-- 15 instead of 16 because using the sign bit requires strong calculation
	subtype address_type is natural;
	type word is range integer'low to integer'high;
	type page_array is array (0 to PAGE_SIZE - 1) of word;
	type page;
	type p_page is access page;
	type page is record
		BaseAddress : address_type;
		Value : page_array;
		NextPage : p_page;
	end record;	-- page

	type memory_type is record
		BaseAddress : address_type;
		Length : positive;
		Width : bit_width;
		Chain : p_page;
	end record;	 -- memory_type

	--------------------------------
	-- MemCreate : dynamically allocated memory declaration
	-- The given values will be used for checkings
	-- BASE_ADDRESS is the beginning address value
	--  the address space is therefore [BASE_ADDRESS, BASE_ADDRESS + LENGTH - 1]
	-- The memory contains 'U' after declaration
	--------------------------------
	procedure MemCreate (
		variable MemRef : inout memory_type;
		LENGTH : positive;
		BASE_ADDRESS : address_type := 0;
		WIDTH : bit_width := 8);	-- default is byte
	
	--------------------------------
	-- MemReset : puts a dynamically allocated memory in the same state
	--  that follows declaration with MemCreate 
	--------------------------------
	procedure MemReset (variable MemRef : inout memory_type);

	--------------------------------
	-- MemRead, MemWrite : access to a dynamically allocated memory
	-- The memory must be firstly declared with MemCreate
	-- Access outside adress space provoques an assertion violation
	-- Memory values are stored in UX01 format
	-- When writing, IEEE.std_logic_1164. To_X01 is used
	--  ('U' -> 'X' on purpose !)
	-- Multiple word access (ADDRESS, ADDRESS + 1, ADDRESS + 2 ...) is possible,
	--  check Data'size = Memory_width * NB_WORD
	-- The word with lowest adress corresponds to the left part of Data
	-- The word with highest adress corresponds to the right part of Data
	-- (Big Indian convention)
	--------------------------------

	procedure MemRead (
		variable MemRef : memory_type;
		ADDRESS : address_type; 
		variable Data : out std_ulogic_vector;
		NB_WORD : positive := 1);

	procedure MemRead (
		variable MemRef : memory_type;
		ADDRESS : address_type; 
		variable Data : out std_logic_vector;
		NB_WORD : positive := 1);

	procedure MemWrite (
			variable MemRef : inout memory_type;
			ADDRESS : address_type;
			DATA : std_ulogic_vector;
			NB_WORD : positive := 1);

	procedure MemWrite (
			variable MemRef : inout memory_type;
			ADDRESS : address_type;
			DATA : std_logic_vector;
			NB_WORD : positive := 1);

	--------------------------------
	-- MemWriteVal : fast access with packets of naturals
	-- Nothing done if NB_WORD = 0
	--------------------------------
	type packet_type is array (integer range <>) of natural;

	procedure MemWriteVal (
			variable MemRef : inout memory_type;
			ADDRESS : address_type;
			PACKET : packet_type;
			NB_WORD : natural);

	--------------------------------
	-- MemLoad and MemDump allow easy file IOs
	--  The file format is :
	--   1/ comments follow VHDL rule (--), except that a blank is necessary
	--       between a declaration and a comment
	--   2/ delimiter is carriage return, or blank between consecutive words
	--   3/ ADDRESS x  changes the current address to x
	--   4/ BASE x changes the base to x (only applicable to values)
	--   5/ It is not possible to load X or U values
	--   6/ Values are truncated if necessary
	--   7/ FORMAT corresponds to function ToString for integers, except that 
	--       empty string means raw binary format
	--   8/ LINE_CYCLE is the period for indication of address before line
	--       put 0 to have only starting address
	--   9/ Loading is case insensitive
	--  Any inconsistency stops the loading
	--  MemLoad has been designed for speed
	--  MemDump is rather slow (default format is faster)
	--  Ex : 16 KByte Load : 40 seconds (base 16, 16 values per line)
	--  Ex : 16 KByte Dump : 3 minutes
	--------------------------------

	procedure MemLoad (
			variable MemRef : inout memory_type;
			FILE_NAME  : string);

	procedure MemDump (
			variable MemRef : inout memory_type;
			FILE_NAME  : string := "STD_OUTPUT";
			ADDRESS : address_type := 0;	-- 0 means from lowest address 
			NB_WORD : natural := 0;	-- 0 means up to highest address
			FORMAT : string := "";	-- "" means raw binary format
			NB_COLUMN : positive := 1;
			LINE_CYCLE : natural := 0; -- 0 stands means only once
			TITLE : string := "");
			
	--------------------------------

end StdMem;	-- package

------------------------------------------------------------

library MMS;
use MMS.stdrtl.all;
package Body StdMem is

	function ToWord (VALUE : std_ulogic_vector) return word is
		variable ValueX01 : std_ulogic_vector (VALUE'length -1 downto 0)
 			:=To_X01(VALUE);
 		variable Result : word := 0;
	begin
		for i in ValueX01'range loop
			Result := Result * 4 + UX01'pos(ValueX01(i));
		end loop;
	return Result;
	end ToWord;	-- function

	--------------------------------

	function ToUX01 (VALUE : word; WIDTH : positive) return std_ulogic_vector is
 		variable Result : std_ulogic_vector (WIDTH - 1 downto 0);
 		variable MyValue : word := VALUE;
	begin
		for i in 0 to WIDTH - 1 loop
			Result(i) := UX01'val(MyValue mod 4);
			MyValue := MyValue / 4;
		end loop;
	return Result;
	end ToUX01;	-- function

	--------------------------------

	constant U_PAGE_ARRAY : page_array := (others => 0);	-- 0 <==> 'U'

	--------------------------------

	procedure MemCreate (
		variable MemRef : inout memory_type;
		LENGTH : positive;
		BASE_ADDRESS : address_type := 0;
		WIDTH : bit_width := 8) is
	begin
		MemRef := (BASE_ADDRESS, LENGTH, WIDTH, NULL);
	end MemCreate;	-- procedure

	--------------------------------

	procedure MemReset (variable MemRef : inout memory_type) is
		variable Index : p_page := MemRef.Chain;
		variable EmptyPage : p_page;
	begin
		MemRef.Chain := NULL;
		while Index /= NULL loop
			EmptyPage := Index;
			Index := Index.NextPage;
			Deallocate(EmptyPage);
		end loop;
	end MemReset;	-- procedure

	--------------------------------
	-- local procedure to test address space
	procedure CheckAddressSpace (
		variable MemRef : memory_type;
		ADDRESS : address_type; 
		NB_WORD : positive := 1;
		Correct : out boolean) is
	begin
		Correct := (ADDRESS >= MemRef.BaseAddress) and
			(ADDRESS + NB_WORD - 1 < MemRef.BaseAddress + MemRef.Length);
	end CheckAddressSpace;	-- local procedure
			
	--------------------------------
	-- local procedure to get page containing address or just before it
	-- if not possible return NULL in Index
	procedure GetPageBefore (
			variable Index : inout p_page;
			ADDRESS : address_type;
			Inside : out boolean) is
		variable NextPage : p_page;
	begin
		Inside := FALSE;
		if Index = NULL or ADDRESS < Index.BaseAddress then
			Index := NULL;
			return;
		end if;
		loop
			NextPage := Index.NextPage;
			exit when NextPage = NULL or ADDRESS < NextPage.BaseAddress;
			Index := NextPage;
		end loop;
		Inside := (ADDRESS < Index.BaseAddress + PAGE_SIZE);
	end GetPageBefore;	-- local procedure

	--------------------------------

	procedure MemRead (
			variable MemRef : memory_type;
			ADDRESS : address_type; 
			variable Data : out std_ulogic_vector;
			NB_WORD : positive := 1) is
		alias Dout : std_ulogic_vector(DATA'length -1 downto 0) is DATA;
		constant WIDTH : bit_width := MemRef.Width;
		variable Index : p_page := MemRef.Chain;
		variable RangeLow : natural := WIDTH * NB_WORD;
		variable RangeHigh : natural := RangeLow + WIDTH - 1;
		variable Correct, Inside : boolean;
	begin
		Data := Extend("UU", Data'length);
		Correct := Dout'length = WIDTH * NB_WORD;
		assert Correct report "Wrong Width in memory read !" severity error;
		if not Correct then return; end if;
		CheckAddressSpace(MemRef, ADDRESS, NB_WORD, Correct);
		assert Correct report "Wrong address in memory read !" severity error;
		if not Correct then return; end if;

		EachAddress : for LocalAddress in ADDRESS to ADDRESS + NB_WORD - 1 loop
		  RangeHigh := RangeHigh - WIDTH;
			RangeLow := RangeLow - WIDTH;
			GetPageBefore(Index, LocalAddress, Inside);
			if Index /= NULL and Inside then
				Dout(RangeHigh downto RangeLow) :=
					ToUX01(Index.Value(LocalAddress - Index.BaseAddress), WIDTH);
			end if;	-- else "UU"
		end loop EachAddress;
	end MemRead;	-- procedure

	--------------------------------

	procedure MemRead (
			variable MemRef : memory_type;
			ADDRESS : address_type; 
			variable Data : out std_logic_vector;
			NB_WORD : positive := 1) is
		variable Result : std_ulogic_vector(Data'range);
	begin
		MemRead(MemRef, ADDRESS, Result, NB_WORD);
		Data := ToStdLogicVector(Result);
	end MemRead;	-- procedure

	--------------------------------

	procedure MemWrite (
			variable MemRef : inout memory_type;
			ADDRESS : address_type;
			DATA : std_ulogic_vector;
			NB_WORD : positive := 1) is
		alias D_IN : std_ulogic_vector(DATA'length -1 downto 0) is DATA;
		constant WIDTH : bit_width := MemRef.Width;
		variable Index : p_page := MemRef.Chain;
		variable RangeLow : natural := WIDTH * NB_WORD;
		variable RangeHigh : natural := RangeLow + WIDTH - 1;
		variable Correct, Inside : boolean;
		variable Relative : address_type;
	begin
		Correct := D_IN'length = WIDTH * NB_WORD;
		assert Correct report "Wrong Width in memory write !" severity error;
		if not Correct then return; end if;
		CheckAddressSpace(MemRef, ADDRESS, NB_WORD, Correct);
		assert Correct report "Wrong address in memory write !" severity error;
		if not Correct then return; end if;

		GetPageBefore(Index, ADDRESS, Inside);
		if Index = NULL then	-- first page
			MemRef.Chain := new page'(ADDRESS - ADDRESS mod PAGE_SIZE, 
				U_PAGE_ARRAY, NULL);	-- insertion
			Index := MemRef.Chain;
			Inside := TRUE;
		end if;

		EachAddress : for LocalAddress in ADDRESS to ADDRESS + NB_WORD - 1 loop
			RangeHigh := RangeHigh - WIDTH;
			RangeLow := RangeLow - WIDTH;		  
			if not Inside then
			  if (Index.NextPage = NULL or
			      (Index.NextPage /= NULL and 
			       Index.NextPage.BaseAddress > LocalAddress) ) then
		  		Index.NextPage := new page'(LocalAddress - LocalAddress mod PAGE_SIZE,
			 		U_PAGE_ARRAY, Index.NextPage);	-- insertion
				end if;
				Index := Index.NextPage;	-- for following calculations
			end if;
			Relative := LocalAddress - Index.BaseAddress;
			Index.Value(Relative) := 
				ToWord(D_IN(RangeHigh downto RangeLow));
			Inside := Relative + 1 < PAGE_SIZE;
		end loop EachAddress;
	end MemWrite;	-- procedure

	--------------------------------

	procedure MemWrite (
			variable MemRef : inout memory_type;
			ADDRESS : address_type;
			DATA : std_logic_vector;
			NB_WORD : positive := 1) is
	begin
		MemWrite(MemRef, ADDRESS, ToStdUlogicVector(DATA), NB_WORD);
	end MemWrite;	-- procedure

	--------------------------------
	-- For values < 255, direct coding
	-- so efficient for bytes
	--------------------------------
	constant MAX_FAST : integer := 255;
	type fast_word_type is array (integer range 0 to MAX_FAST) of word;
	constant FAST_WORD : fast_word_type := (
		16#2AAAAAAA#, 16#2AAAAAAB#, 16#2AAAAAAE#, 16#2AAAAAAF#,
		16#2AAAAABA#, 16#2AAAAABB#, 16#2AAAAABE#, 16#2AAAAABF#, 
		16#2AAAAAEA#, 16#2AAAAAEB#, 16#2AAAAAEE#, 16#2AAAAAEF#, 
		16#2AAAAAFA#, 16#2AAAAAFB#, 16#2AAAAAFE#, 16#2AAAAAFF#, 
		16#2AAAABAA#, 16#2AAAABAB#, 16#2AAAABAE#, 16#2AAAABAF#, 
		16#2AAAABBA#, 16#2AAAABBB#, 16#2AAAABBE#, 16#2AAAABBF#,
		16#2AAAABEA#, 16#2AAAABEB#, 16#2AAAABEE#, 16#2AAAABEF#, 
		16#2AAAABFA#, 16#2AAAABFB#, 16#2AAAABFE#, 16#2AAAABFF#, 
		16#2AAAAEAA#, 16#2AAAAEAB#, 16#2AAAAEAE#, 16#2AAAAEAF#, 
		16#2AAAAEBA#, 16#2AAAAEBB#, 16#2AAAAEBE#, 16#2AAAAEBF#, 
		16#2AAAAEEA#, 16#2AAAAEEB#, 16#2AAAAEEE#, 16#2AAAAEEF#, 
		16#2AAAAEFA#, 16#2AAAAEFB#, 16#2AAAAEFE#, 16#2AAAAEFF#, 
		16#2AAAAFAA#, 16#2AAAAFAB#, 16#2AAAAFAE#, 16#2AAAAFAF#, 
		16#2AAAAFBA#, 16#2AAAAFBB#, 16#2AAAAFBE#, 16#2AAAAFBF#, 
		16#2AAAAFEA#, 16#2AAAAFEB#, 16#2AAAAFEE#, 16#2AAAAFEF#, 
		16#2AAAAFFA#, 16#2AAAAFFB#, 16#2AAAAFFE#, 16#2AAAAFFF#, 
		16#2AAABAAA#, 16#2AAABAAB#, 16#2AAABAAE#, 16#2AAABAAF#, 
		16#2AAABABA#, 16#2AAABABB#, 16#2AAABABE#, 16#2AAABABF#, 
		16#2AAABAEA#, 16#2AAABAEB#, 16#2AAABAEE#, 16#2AAABAEF#, 
		16#2AAABAFA#, 16#2AAABAFB#, 16#2AAABAFE#, 16#2AAABAFF#, 
		16#2AAABBAA#, 16#2AAABBAB#, 16#2AAABBAE#, 16#2AAABBAF#, 
		16#2AAABBBA#, 16#2AAABBBB#, 16#2AAABBBE#, 16#2AAABBBF#, 
		16#2AAABBEA#, 16#2AAABBEB#, 16#2AAABBEE#, 16#2AAABBEF#, 
		16#2AAABBFA#, 16#2AAABBFB#, 16#2AAABBFE#, 16#2AAABBFF#, 
		16#2AAABEAA#, 16#2AAABEAB#, 16#2AAABEAE#, 16#2AAABEAF#, 
		16#2AAABEBA#, 16#2AAABEBB#, 16#2AAABEBE#, 16#2AAABEBF#, 
		16#2AAABEEA#, 16#2AAABEEB#, 16#2AAABEEE#, 16#2AAABEEF#, 
		16#2AAABEFA#, 16#2AAABEFB#, 16#2AAABEFE#, 16#2AAABEFF#, 
		16#2AAABFAA#, 16#2AAABFAB#, 16#2AAABFAE#, 16#2AAABFAF#, 
		16#2AAABFBA#, 16#2AAABFBB#, 16#2AAABFBE#, 16#2AAABFBF#, 
		16#2AAABFEA#, 16#2AAABFEB#, 16#2AAABFEE#, 16#2AAABFEF#, 
		16#2AAABFFA#, 16#2AAABFFB#, 16#2AAABFFE#, 16#2AAABFFF#, 
		16#2AAAEAAA#, 16#2AAAEAAB#, 16#2AAAEAAE#, 16#2AAAEAAF#, 
		16#2AAAEABA#, 16#2AAAEABB#, 16#2AAAEABE#, 16#2AAAEABF#, 
		16#2AAAEAEA#, 16#2AAAEAEB#, 16#2AAAEAEE#, 16#2AAAEAEF#, 
		16#2AAAEAFA#, 16#2AAAEAFB#, 16#2AAAEAFE#, 16#2AAAEAFF#, 
		16#2AAAEBAA#, 16#2AAAEBAB#, 16#2AAAEBAE#, 16#2AAAEBAF#, 
		16#2AAAEBBA#, 16#2AAAEBBB#, 16#2AAAEBBE#, 16#2AAAEBBF#, 
		16#2AAAEBEA#, 16#2AAAEBEB#, 16#2AAAEBEE#, 16#2AAAEBEF#, 
		16#2AAAEBFA#, 16#2AAAEBFB#, 16#2AAAEBFE#, 16#2AAAEBFF#, 
		16#2AAAEEAA#, 16#2AAAEEAB#, 16#2AAAEEAE#, 16#2AAAEEAF#, 
		16#2AAAEEBA#, 16#2AAAEEBB#, 16#2AAAEEBE#, 16#2AAAEEBF#, 
		16#2AAAEEEA#, 16#2AAAEEEB#, 16#2AAAEEEE#, 16#2AAAEEEF#, 
		16#2AAAEEFA#, 16#2AAAEEFB#, 16#2AAAEEFE#, 16#2AAAEEFF#, 
		16#2AAAEFAA#, 16#2AAAEFAB#, 16#2AAAEFAE#, 16#2AAAEFAF#, 
		16#2AAAEFBA#, 16#2AAAEFBB#, 16#2AAAEFBE#, 16#2AAAEFBF#, 
		16#2AAAEFEA#, 16#2AAAEFEB#, 16#2AAAEFEE#, 16#2AAAEFEF#, 
		16#2AAAEFFA#, 16#2AAAEFFB#, 16#2AAAEFFE#, 16#2AAAEFFF#, 
		16#2AAAFAAA#, 16#2AAAFAAB#, 16#2AAAFAAE#, 16#2AAAFAAF#, 
		16#2AAAFABA#, 16#2AAAFABB#, 16#2AAAFABE#, 16#2AAAFABF#,
		16#2AAAFAEA#, 16#2AAAFAEB#, 16#2AAAFAEE#, 16#2AAAFAEF#, 
		16#2AAAFAFA#, 16#2AAAFAFB#, 16#2AAAFAFE#, 16#2AAAFAFF#, 
		16#2AAAFBAA#, 16#2AAAFBAB#, 16#2AAAFBAE#, 16#2AAAFBAF#, 
		16#2AAAFBBA#, 16#2AAAFBBB#, 16#2AAAFBBE#, 16#2AAAFBBF#, 
		16#2AAAFBEA#, 16#2AAAFBEB#, 16#2AAAFBEE#, 16#2AAAFBEF#, 
		16#2AAAFBFA#, 16#2AAAFBFB#, 16#2AAAFBFE#, 16#2AAAFBFF#, 
		16#2AAAFEAA#, 16#2AAAFEAB#, 16#2AAAFEAE#, 16#2AAAFEAF#, 
		16#2AAAFEBA#, 16#2AAAFEBB#, 16#2AAAFEBE#, 16#2AAAFEBF#, 
		16#2AAAFEEA#, 16#2AAAFEEB#, 16#2AAAFEEE#, 16#2AAAFEEF#, 
		16#2AAAFEFA#, 16#2AAAFEFB#, 16#2AAAFEFE#, 16#2AAAFEFF#, 
		16#2AAAFFAA#, 16#2AAAFFAB#, 16#2AAAFFAE#, 16#2AAAFFAF#, 
		16#2AAAFFBA#, 16#2AAAFFBB#, 16#2AAAFFBE#, 16#2AAAFFBF#, 
		16#2AAAFFEA#, 16#2AAAFFEB#, 16#2AAAFFEE#, 16#2AAAFFEF#, 
		16#2AAAFFFA#, 16#2AAAFFFB#, 16#2AAAFFFE#, 16#2AAAFFFF#);

	--------------------------------

	procedure MemWriteVal (
			variable MemRef : inout memory_type;
			ADDRESS : address_type;
			PACKET : packet_type;
			NB_WORD : natural) is
		alias PacketIn : packet_type(0 to PACKET'length - 1) is PACKET;
		constant WIDTH : integer := MemRef.Width;
		variable Add, Relative : address_type;
		variable Index : p_page := MemRef.Chain;
		variable Correct, Inside : boolean;
	begin
		if NB_WORD = 0 then return; end if;
		CheckAddressSpace(MemRef, ADDRESS, NB_WORD, Correct);
		assert Correct report "Wrong address in MemWriteVal !" severity error;
		if not Correct then return; end if;
		Correct := (NB_WORD <= PacketIn'length);
		assert Correct report "Packet too small in MemWriteVal !" severity error;
		if not Correct then return; end if;

		GetPageBefore(Index, ADDRESS, Inside);
		if Index = NULL then	-- first page
			MemRef.Chain := new page'(ADDRESS - ADDRESS mod PAGE_SIZE, 
				U_PAGE_ARRAY, MemRef.Chain);	-- insertion
			Index := MemRef.Chain;
		end if;

		for i in 0 to NB_WORD - 1 loop
			Add := ADDRESS + i;
			if not Inside then
				Index.NextPage := new page'(Add - Add mod PAGE_SIZE, 
					U_PAGE_ARRAY, Index.NextPage); -- insertion
				Index := Index.NextPage;	-- for following calculations
			end if;
			Relative := Add - Index.BaseAddress;
			if PacketIn(i) > MAX_FAST then
				Index.Value(Relative) := ToWord(ToStdUlogicVector(PacketIn(i)));
			else Index.Value(Relative) := FAST_WORD(PacketIn(i));
			end if;
			Inside := Relative + 1 < PAGE_SIZE;
		end loop;
	end MemWriteVal;	-- procedure

	--------------------------------

	procedure MemLoad (
			variable MemRef : inout memory_type;
			FILE_NAME  : string) is
		constant PACKET_SIZE : positive := PAGE_SIZE;
		file F : text is in FILE_NAME;
		variable MyLine : string (1 to 256);
		variable Address : address_type := MemRef.BaseAddress;
		variable Base : base_type := 10;
		variable Value : integer;
		variable FirstWord, Index, L : natural;
		constant WIDTH : bit_width := MemRef.Width;
		variable Packet : packet_type (0 to PACKET_SIZE - 1);
		variable PacketIndex : natural := 0;
		variable LastWord : boolean;
	begin
		ForeachLine : while not endfile(F) loop
			Scan(F, MyLine);
			FirstWord := LeadBlank(MyLine) + 1;
			Index := FirstWord;
			next when MyLine(Index) = NUL or MyLine(Index to Index + 1) = "--";
			LastWord := FALSE;
			L := StrLen(MyLine);

			--------------	
			if StrNcEqu(MyLine(Index to L), "ADDRESS ", 8) then
				MemWriteVal(MemRef, Address - PacketIndex, Packet, PacketIndex);
				PacketIndex := 0;
				Index := Index + 8;
				Value := FromString(MyLine(Index to L));
				exit when Value < MemRef.BaseAddress or 
					Value >= MemRef.BaseAddress + MemRef.Length;
				Address := Value;
				Index := NextWord(MyLine, Index);	-- skip to next word
				LastWord := (Index = FirstWord or MyLine(Index to Index + 1) = "--");
				exit when not LastWord;
			--------------	
			elsif StrNcEqu(MyLine(Index to L), "BASE ", 5) then
				Index := Index + 5;
				Value := FromString(MyLine(Index to L));
				exit when	Value < base_type'low or Value > base_type'high;
				Base := Value;
				Index := NextWord(MyLine, Index);	-- skip to next word
				LastWord := (Index = FirstWord or MyLine(Index to Index + 1) = "--");
				exit when not LastWord;
			--------------	
			else	-- supposed value
				ForeachWordInLine : while not LastWord loop
					exit when not IsDigit(MyLine(Index), Base);
					Packet(PacketIndex) := FromString(MyLine(Index to L), Base);
					PacketIndex := PacketIndex + 1;
					Address := Address + 1;
					Index := NextWord(MyLine, Index);	-- skip to next word
					LastWord := (Index = FirstWord or MyLine(Index to Index + 1) = "--");
					if PacketIndex = PACKET_SIZE then
						MemWriteVal(MemRef, Address - PacketIndex, Packet, PacketIndex);
						PacketIndex := 0;
					end if;
					exit when LastWord or PacketIndex >= PACKET_SIZE;
				end loop ForeachWordInLine;
			end if;
			--------------	
			exit when not LastWord;
		end loop ForeachLine;
		MemWriteVal(MemRef, Address - PacketIndex, Packet, PacketIndex);
		assert LastWord report "incorrect line : " & MyLine(1 to L) severity error;
	end MemLoad;	-- procedure

	--------------------------------
	-- local function ToStringDump is adapted to UX01 format (for MemDump)

	function ToStringDump(VALUE : std_ulogic_vector; FORMAT : string)
				return string is
		constant ALL_U : std_ulogic_vector (VALUE'range) := (others => 'U');
		variable L : line;
	begin
		if StrLen(FORMAT) = 0 then return ToString(VALUE); end if;
		if not VecUnknown(VALUE) then
			return ToString(ToNatural(VALUE), FORMAT);
		else 
			write(L, ToString(0, FORMAT));
			for i in L'range loop
				L(i) := ' ';
			end loop;
			if VALUE = ALL_U then L(L'right) := 'U';
			else L(L'right) := 'X';
			end if;
			return L.all;
		end if;
	end ToStringDump;	-- local function
	
	--------------------------------
	
	procedure MemDump (
			variable MemRef : inout memory_type;
			FILE_NAME  : string := "STD_OUTPUT";
			ADDRESS : address_type := 0; 
			NB_WORD : natural := 0;
			FORMAT : string := "";
			NB_COLUMN : positive := 1;
			LINE_CYCLE : natural := 0;
			TITLE : string := "") is
		file F : text is out FILE_NAME;
		variable AddDump : address_type := ADDRESS;
		variable Value : std_ulogic_vector(1 to MemRef.Width);
		variable Length : natural := NB_WORD;
		variable Column : integer := 1;
		variable L : line;
		variable LineIndex : integer := 0;
		variable Correct : boolean;
	begin
		if StrLen(TITLE) > 0 then Print(F, "-- " & TITLE); end if;
		if StrLen(FORMAT) > 0 then
			case FORMAT(FORMAT'right) is
				when 'x' | 'X' => Print(F, "BASE 16");
				when 'b' => Print(F, "BASE 2");
				when others =>
			end case;
		end if;
		if ADDRESS = 0 then AddDump := MemRef.BaseAddress; end if;
		if NB_WORD = 0 then
			Length := AddDump - MemRef.BaseAddress + MemRef.Length;
		end if;
		CheckAddressSpace(MemRef, AddDump, Length, Correct);
		assert Correct report "Incorrect address space for dump !" severity error;
		if not Correct then return; end if;
		--------------
		for i in AddDump to AddDump + Length - 1 loop
			if LineIndex = 0 and Column = 1 then
				Print(F, "ADDRESS " & ToString(i));
			end if;
			MemRead(MemRef, i, Value);
			write(L, ToStringDump(Value, FORMAT));
			Column := Column mod NB_COLUMN + 1;
			if Column = 1 then
				writeline(F, L);
				LineIndex := LineIndex + 1;
				if LineIndex = LINE_CYCLE and LINE_CYCLE /= 0 then
					LineIndex := 0;
				end if;
			else write(L, ' ');
			end if;
		end loop;
		if L /= NULL then writeline(F, L); end if;
	end MemDump;	-- procedure

	--------------------------------

end StdMem;	-- package body
