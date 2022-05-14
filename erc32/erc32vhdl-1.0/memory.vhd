--
-- Simple RAM and ROM models
--
-- Written by Jiri Gaisler, ESA/ESTEC, 1996
--

library ieee;
   use ieee.std_logic_1164.all;
   use ieee.STD_LOGIC_ARITH.all;
   use std.textio.all;


entity FLASHPROM is
    port (
        A : in std_logic_vector(18 downto 0);
        CE : in std_logic;
        OE : in std_logic;
        WE : in std_logic;
        D : inout std_logic_vector(7 downto 0)
);
end FLASHPROM;

architecture BEHAVIORAL of FLASHPROM is

  signal CSN : std_logic;
  signal OEN : std_logic;

  subtype BYTE is std_logic_vector(7 downto 0);

  signal DRIVE : std_logic;
  signal DINT : std_logic_vector(0 to 7);

  function TO_INT(V:std_logic_vector) return integer is
  variable RES : integer := 0;
  variable IND : integer := 1;
  begin
    for I in V'reverse_range loop
      if V(I) = '1' then
        RES := RES + IND;
      end if;
      IND := IND*2;
    end loop;
    return(RES);
  end;
 
        procedure CHAR2QUADBITS(C: character;
                                RESULT: out bit_vector(3 downto 0);
                                GOOD: out boolean;
                                ISSUE_ERROR: in boolean) is
        begin
                case C is
                        when '0' => RESULT :=  x"0"; GOOD := true;
                        when '1' => RESULT :=  x"1"; GOOD := true;
                        when '2' => RESULT :=  X"2"; GOOD := true;
                        when '3' => RESULT :=  X"3"; GOOD := true;
                        when '4' => RESULT :=  X"4"; GOOD := true;
                        when '5' => RESULT :=  X"5"; GOOD := true;
                        when '6' => RESULT :=  X"6"; GOOD := true;
                        when '7' => RESULT :=  X"7"; GOOD := true;
                        when '8' => RESULT :=  X"8"; GOOD := true;
                        when '9' => RESULT :=  X"9"; GOOD := true;
                        when 'A' => RESULT :=  X"A"; GOOD := true;
                        when 'B' => RESULT :=  X"B"; GOOD := true;
                        when 'C' => RESULT :=  X"C"; GOOD := true;
                        when 'D' => RESULT :=  X"D"; GOOD := true;
                        when 'E' => RESULT :=  X"E"; GOOD := true;
                        when 'F' => RESULT :=  X"F"; GOOD := true;
 
                        when 'a' => RESULT :=  X"A"; GOOD := true;
                        when 'b' => RESULT :=  X"B"; GOOD := true;
                        when 'c' => RESULT :=  X"C"; GOOD := true;
                        when 'd' => RESULT :=  X"D"; GOOD := true;
                        when 'e' => RESULT :=  X"E"; GOOD := true;
                        when 'f' => RESULT :=  X"F"; GOOD := true;
                        when others =>
                           if ISSUE_ERROR then
                                   assert false report
                                        "HREAD Error: Read a '" & C &
                                           "', expected a Hex character (0-F).";
                           end if;
                           GOOD := false;
                end case;
        end;

        procedure HREAD(L:inout line; VALUE:out bit_vector)  is
                variable OK: boolean;
                variable C:  character;
                constant NE: integer := VALUE'length/4;
                variable BV: bit_vector(0 to VALUE'length-1);
                variable S:  string(1 to NE-1);
        begin
                if VALUE'length mod 4 /= 0 then
                        assert false report
                                "HREAD Error: Trying to read vector " &
                                   "with an odd (non multiple of 4) length";
                        return;
                end if;
 
                loop                                    -- skip white space
                        read(L,C);
                        exit when ((C /= ' ') and (C /= CR) and (C /= HT));
                end loop;
 
                CHAR2QUADBITS(C, BV(0 to 3), OK, true);
                if not OK then
                        return;
                end if;
 
                read(L, S, OK);
                if not OK then
                        assert false
                                report "HREAD Error: Failed to read the STRING";
                        return;
                end if;
 
                for I in 1 to NE-1 loop
                        CHAR2QUADBITS(S(I), BV(4*I to 4*I+3), OK, true);
                        if not OK then
                                return;
                        end if;
                end loop;
                VALUE := BV;
        end HREAD;

        procedure HREAD(L:inout line; VALUE:out std_ulogic_vector) is
                variable TMP: bit_vector(VALUE'length-1 downto 0);
        begin
                HREAD(L, TMP);
                VALUE := TO_X01(TMP);
        end HREAD;


        procedure HREAD(L:inout line; VALUE:out std_logic_vector) is
                variable TMP: std_ulogic_vector(VALUE'length-1 downto 0);
        begin
                HREAD(L, TMP);
                VALUE := std_logic_vector(TMP);
        end HREAD;
 



begin
 
  CSN <= CE;
  OEN <= OE;

  MEMORY : process(CSN,A)
  type  MEMA is array (0 to 8191) of BYTE;
  variable  MEM : MEMA;
  variable L1,L2 : line;
  variable FIRST : boolean := true;
  variable LEN : integer := 0;
  variable BUF : BYTE;
  variable CH : character;
  file TCF : text is in "prom.dat";
  begin
    if FIRST then
--      WriteC("PROM8 : Reading prom8.srec ...");
      L1:= new string'("");
      while not endfile(TCF) loop
        readline(TCF,L1);
        while (L1'length /= 0) loop
          while (not (L1'length=0)) and (L1(L1'left) = ' ') loop
            std.textio.read(L1,CH);
          end loop;

          if L1'length > 0 then
            if not (L1'length=0) then
              HREAD(L1,BUF);
              MEM(LEN) := BUF;
              LEN := LEN +1;
            end if;
          end if;
        end loop;
      end loop;


      FIRST := false;
--      WriteC("PROM8 : Done! Loaded " & Tohex(Len) & " bytes ...");
    elsif TO_X01(CSN) = '0' and not IS_X(A) then
      DINT <= MEM(TO_INT(A) mod MEM'length) after 150 NS;
    end if;
  end process MEMORY;
 
  DRIVE <= (TO_X01(CSN or OEN)) after 15 NS;
  D <= TO_X01(DINT) when DRIVE  = '0' else
       "ZZZZZZZZ" when DRIVE = '1' else
        "XXXXXXXX";
  

end BEHAVIORAL;

configuration CFG_FLASHPROM_BEHAVIORAL of FLASHPROM is
   for BEHAVIORAL

   end for;

end CFG_FLASHPROM_BEHAVIORAL;


library ieee;
   use ieee.std_logic_1164.all;
   use ieee.STD_LOGIC_ARITH.all;
   use std.textio.all;


entity RAM8 is
      port (  
	A : in std_logic_vector(18 downto 0);
        D : inout std_logic_vector(7 downto 0);
        CE1 : in std_logic;
        WE : in std_logic;
        OE : in std_logic
); end RAM8;     


architecture BEHAVIORAL of RAM8 is
signal CS1N : std_logic;
signal CS2 : std_logic;
signal OEN : std_logic;
signal WN : std_logic;

  function TO_INT(V:std_logic_vector) return integer is
  variable RES : integer := 0;
  variable IND : integer := 1;
  begin
    for I in V'reverse_range loop
      if V(I) = '1' then
        RES := RES + IND;
      end if;
      IND := IND*2;
    end loop;
    return(RES);
  end;
 
  subtype BYTE is std_logic_vector(7 downto 0);
  type MEM is array(0 to 2047) of BYTE;
  signal DINT,DI,DO : BYTE;

begin

  CS2 <= '1';
  CS1N <= TO_X01(CE1);
  WN <= TO_X01(WE);
  DI <= D;
  OEN <= OE;

  RAM : process(CS1N,CS2,WN,D,A,OEN)
  variable MEMA : MEM;
  begin
    if (TO_X01((not CS1N) and CS2) = '1') then
      if (TO_X01(WN) = '1') and not IS_X(A(10 downto 0)) then
        DINT <= MEMA(TO_INT(A(10 downto 0))) after 20 NS;
      end if;
      if WN'event and (TO_X01(WN) = '1') and not IS_X(A(10 downto 0)) then
        MEMA(TO_INT(A(10 downto 0))) := std_logic_vector(DI);
      end if;
    end if;
  end process;
 
  BUFS : process(CS1N,CS2,WN,DINT,OEN)
  variable DRIVEB : std_logic;
  begin
    DRIVEB := TO_X01((not CS1N) and CS2 and (not OEN) and WN);
    case DRIVEB is
      when '1' => D <= DINT after 5 NS;
      when '0' => D <= "ZZZZZZZZ" after 5 NS;
      when others => D <= "XXXXXXXX" after 5 NS;
    end case;
  end process;

end BEHAVIORAL;


library ieee;
   use ieee.std_logic_1164.all;
   use ieee.STD_LOGIC_ARITH.all;
   use std.textio.all;

package MEMPACK is

  component RAM8
    port (
	A : in std_logic_vector(18 downto 0);
        D : inout std_logic_vector(7 downto 0);
        CE1 : in std_logic;
        WE : in std_logic;
        OE : in std_logic
    );
  end component;

  component FLASHPROM
    port (
        A : in std_logic_vector(18 downto 0);
        CE : in std_logic;
        OE : in std_logic;
        WE : in std_logic;
        D : inout std_logic_vector(7 downto 0)
    );
  end component;

end MEMPACK;

