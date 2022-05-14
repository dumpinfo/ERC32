-------------------------------------------------------------------------------
-- File name : board.vhd
-- Title : ERC32 board design
-- project : SPARC 
-- Library : IURT_LIB, FPURT_LIB, MECLIBRARY, FACTLIB
-- Author(s) : Jiri Gaisler
-- Purpose : Example of an ERC32 board model
--
-- notes :   
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        |  JG    | 96-10-04    | first version
--.............................................................................
-------------------------------------------------------------------------------
-- Copyright ESA/ESTEC
-------------------------------------------------------------------------------
---------|---------|---------|---------|---------|---------|---------|--------|

library IEEE;
use IEEE.Std_Logic_1164.all;
library IURT_LIB;
use IURT_LIB.IURTCompPck.all;
library FPURT_LIB;
use FPURT_LIB.FPURTCompPck.all;
library MECLibrary;
use MECLibrary.MECCompPck.all;
library factlib;
use factlib.FACT_Comp_Pck.all;
library memory;
use memory.mempack.all;

library MMS;
use MMS.stdioimp.all;
use STD.TEXTIO.all;

entity board is
end board;


architecture Simulation of board is

  constant ResetPeriod : time := 1000 ns;
  constant Clock2Period : time := 36 ns;	-- 14 MHz system clock
  

  -------- SIGNAL DEFINITION ----------
  signal GND : std_logic := '0';
  signal VCC : std_logic := '1';
  
  
  --  Memory Subsystems Interface Signals  
  signal A        : std_logic_vector(31 downto 0); -- Address bus
  signal APAR     : std_logic; -- Address bus PARity
  signal AOE_N    : std_logic; -- Address Output Enable
  signal ASI      : std_logic_vector( 7 downto 0); -- Address Space Identifier
  signal ASPAR    : std_logic;
  signal BHOLD_N  : std_logic; -- Bus Hold
  signal COE_N    : std_logic; -- Control Output Enable
  signal D        : std_logic_vector(31 downto 0); -- Data Bus
  signal DPAR     : std_logic; -- Data bus PARity
  signal DOE_N    : std_logic; -- Data Output Enable
  signal DXFER    : std_logic; -- Data Transfer
  signal IMPAR    : std_logic; -- Integer Unit to MEC parity
  signal INULL    : std_logic; -- Integer Unit Nullify Cycle
  signal LDSTO    : std_logic; -- Atomic Load-Store
  signal LOCK     : std_logic; -- Bus LOCK
  signal MDS_N    : std_logic; -- Memory Data Strobe
  signal MEXC_N   : std_logic; -- Memory Exception
  signal MHOLDA_N : std_logic; -- Memory Hold A
  signal RD       : std_logic; -- Read Access
  signal SIZE     : std_logic_vector( 1 downto 0); -- Bus Transaction Size
  signal WE_N     : std_logic; -- Write Enable
  signal WRT      : std_logic; -- Advanced Write
    
  -- Interrupt and Control Signals
  signal IUERR_N  : std_logic; -- Error State
  signal IUHWERR_N  : std_logic; -- IU hardware error
  signal IUMCERR_N  : std_logic; -- IU comaprison error
  signal RESET_N  : std_logic; -- Integer Unit Reset
  signal INTACK   : std_logic; -- Interrupt Acknowledge
  signal IRL      : std_logic_vector( 3 downto 0); -- Interrupt
                                                   -- Request Level
    
  -- Floating Point / Coprocessor Interfaces
  signal INST     : std_logic; -- Instruction Fetch
  signal FP_N     : std_logic; -- Floating-Point Unit Present
  signal FINS1    : std_logic; -- Floating-Point Instruction in Buffer 1
  signal FINS2    : std_logic; -- Floating-Point Instruction in Buffer 2
  signal FHOLD_N  : std_logic; -- Floating-Point Hold
  signal FEXC_N   : std_logic; -- Floating-Point Exception
  signal FXACK    : std_logic; -- Floating-Point Exception Acknowledge
  signal FCCV     : std_logic; -- FP Condition Codes Valid
  signal FCC      : std_logic_vector( 1 downto 0); -- FP Condition Codes
  signal FLUSH    : std_logic; -- Floating-Point/Coproc. Instruction Flush
  signal FNULL    : std_logic; -- Fpu output signal.
  signal FIPAR    : std_logic; -- Fpu to Iu control PARity
  signal IFPAR    : std_logic; -- Iu to Fpu control PARity
  signal FPUHWERR_N : std_logic; -- Error state
  signal FPUMCERR_N   : std_logic; -- FPURT Master/Checker Mode 
  signal HALT_N       : std_logic; -- Halt mode
  
--  signal CP_N     : std_logic; -- Coprocessor Unit Present
--  signal CINS1    : std_logic; -- Coprocessor Instruction in Buffer 1
--  signal CINS2    : std_logic; -- Coprocessor Instruction in Buffer 2
  signal CHOLD_N  : std_logic := '1'; -- Coprocessor Hold
--  signal CEXC_N   : std_logic; -- Coprocessor Exception
--  signal CXACK    : std_logic; -- Coprocessor Exception Acknowledge
  signal CCCV     : std_logic := '1'; -- Coprocessor Condition Codes Valid
  signal CCC      : std_logic_vector( 1 downto 0) := "00"; -- Coproc. Cond.
                                                             -- Codes
    
  -- Various MEC signals
  signal SCLK   : std_logic_vector(1 downto 0);
  signal EXTINT   : std_logic_vector(4 downto 0) := "11111";
  signal SYSRESET_N   : std_logic;
  signal DDIR_N   : std_logic;
  signal DDIR   : std_logic;
  signal CLK2      : std_logic := '0';  -- MEC Clock pin
  signal BA   : std_logic_vector(1 downto 0);
  signal CB   : std_logic_vector(6 downto 0);
  signal ALE_N   : std_logic;
  signal ROMCS_N   : std_logic;
  signal MEMBEN_N   : std_logic;
  signal MEMCS_N   : std_logic_vector(9 downto 0);
  signal OE_N   : std_logic_vector(1 downto 0);
  signal MEMWR1_N   : std_logic_vector(1 downto 0);
  signal MEMWR2_N   : std_logic_vector(1 downto 0);
  signal IOSEL_N   : std_logic_vector(3 downto 0);
  signal IOWR_N   : std_logic;
  signal IOBEN_N   : std_logic;
  signal EXMCS_N   : std_logic;
  signal SYSERR_N   : std_logic;
  signal SYSAV   : std_logic;
  signal RAMBEN_N   : std_logic;
  signal ROMBEN_N   : std_logic;

  -- Memory signals

  signal DB   : std_logic_vector(31 downto 0);
  signal AL   : std_logic_vector(25 downto 0);

  -- Configuration of components
--  for all : IURT use entity IURT_LIB.IURT(WithTiming);
--  for all : FPURT use entity FPURT_LIB.FPURT(WithTiming);
--  for all : MEC use entity MECLibrary.MEC(WithTiming);
  
begin

  ClockGenerator : CLK2 <= not CLK2 after Clock2Period/2; 
  ResetGenerator : SYSRESET_N <= '0', '1' after ResetPeriod;

  
  MyIU: IURT
    port map(
      CLK => SCLK(0), -- clock signal
    
      --  Memory Subsystems Interface Signals  
      A        => A,
      APAR     => APAR,
      AOE_N    => AOE_N,
      ASI      => ASI,
      ASPAR    => ASPAR,
      BHOLD_N  => BHOLD_N,
      COE_N    => COE_N, 
      D        => D,
      DPAR     => DPAR,
      DOE_N    => DOE_N,
      DXFER    => DXFER,
      IFT_N    => GND,
      INULL    => INULL,
      LDSTO    => LDSTO,
      LOCK     => LOCK,
      MAO      => GND, 
      MDS_N    => MDS_N, 
      MEXC_N   => MEXC_N,
      MHOLDA_N => MHOLDA_N,
      MHOLDB_N => VCC,
      RD       => RD,
      SIZE     => SIZE,
      WE_N     => WE_N,
      WRT      => WRT,
      IMPAR    => IMPAR,
    
      -- Interrupt and Control Signals
      ERROR_N    => IUERR_N,
      HWERROR_N  => IUHWERR_N,
      FLOW_N     => VCC,
      MCERR_N    => IUMCERR_N,
      N601MODE_N => VCC,
      CMODE_N    => VCC,
      FPSYN      => VCC,
      INTACK     => INTACK,
      IRL        => IRL,
      RESET_N    => RESET_N,
      TOE_N      => GND,
      HALT_N     => HALT_N,
    
      -- Floating Point / Coprocessor Interfaces
      FCC     => FCC, 
      FCCV    => FCCV, 
      FEXC_N  => FEXC_N,
      FHOLD_N => FHOLD_N,
      FIPAR   => FIPAR,
      FINS1   => FINS1,
      FINS2   => FINS2,
      FLUSH   => FLUSH,
      FP_N    => FP_N,
      FXACK   => FXACK,
      INST    => INST,
      IFPAR   => IFPAR,
      CCC     => CCC,  
      CCCV    => VCC,
      CEXC_N  => VCC,
      CHOLD_N => VCC,
      CINS1   => open,
      CINS2   => open,
      CP_N    => VCC,
      CXACK   => open,
        
      -- TAP signals
      TCLK    => GND,
      TRST_N  => GND,
      TMS     => GND,
      TDI     => GND,
      TDO     => open
    );  


  MyFPU: FPURT
    port map(
      CLK => SCLK(0), -- clock signal
    
      -- Integer Unit Interface Signals
      FP_N    => FP_N,    -- Floating-point (Fp) Present
      FCC     => FCC,     -- Fp Condition Codes
      FCCV    => FCCV,    -- Fp Condition Codes Valid
      FHOLD_N => FHOLD_N, -- Fp Hold
      FEXC_N  => FEXC_N,  -- Fp EXCeption
      FIPAR   => FIPAR,   -- Fpu to Iu control PARity
      FXACK   => FXACK,   -- Fp eXception ACKnowledge
      INST    => INST,    -- INSTruction fetch
      FINS1   => FINS1,   -- Fp INStruction in buffer 1
      FINS2   => FINS2,   -- Fp INStruction in buffer 2
      FLUSH   => FLUSH,   -- Fp instruction fLUSH
      IFPAR   => IFPAR,   -- Iu to Fpu control PARity
    
      -- System/Memory Interface Signals
      A          => A,     -- Address bus
      APAR       => APAR,  -- Address bus PARity
      D          => D,     -- Data bus
      DPAR       => DPAR,  -- Data bus PARity
      DOE_N      => DOE_N, -- Data Output Enable
      COE_N      => COE_N,    -- Control Output Enable
      MHOLDA_N   => MHOLDA_N, -- Memory HOLD
      MHOLDB_N   => VCC, -- Memory HOLD
      BHOLD_N    => BHOLD_N,  -- Bus HOLD
      MDS_N      => MDS_N,    -- Memory Data Strobe
      FNULL      => FNULL,    -- Fpu NULLify cycle
      RESET_N    => RESET_N,  -- Reset signal
      HWERROR_N  => FPUHWERR_N, -- Hardware error detected
      CMODE_N    => VCC,   -- master/Checker MODE
      MCERR_N    => FPUMCERR_N, -- Comparison Error
      N602MODE_N => VCC,	 -- Normal 602 MODE Operation
      HALT_N     => HALT_N,  -- Halt mode
    
      -- Coprocessor Interface Signals
      CHOLD_N => CHOLD_N, -- Coprocessor hold.
      CCCV    => CCCV, -- Coprocessor Condition Code Valid.

      -- Test Access Port (TAP) signals
      TCLK   => GND,   -- Test CLocK
      TRST_N => GND, -- Test ReSeT
      TMS    => GND,    -- Test Mode Select
      TDI    => GND,    -- Test Data In
      TDO    => open     -- Test Data Out
    );


  MyMEC : 
    MEC PORT MAP(
	A => A, 
	APAR => APAR, 
	ASI => ASI(3 downto 0), 
	SIZE => SIZE, 
	ASPAR => ASPAR, 
	SCLK => SCLK, 
	DMAAS => GND, 
	INULL => INULL, 
	EXTHOLD_N => FHOLD_N, 
	EXTCCV => FCCV, 
	DXFER => DXFER, 
	LDSTO => LDSTO, 
	LOCK => LOCK, 
	RD => RD, 
	WE_N => WE_N, 
	WRT => WRT, 
	IMPAR => IMPAR, 
	PROM8_N => GND, 
	BUSRDY_N => GND, 
	BUSERR_N => VCC, 
	DMAREQ_N => VCC, 
	INTACK => INTACK, 
	EXTINT => EXTINT, 
	SYSRESET_N => SYSRESET_N, 
	IUERR_N => IUERR_N, 
	IUHWERR_N => IUHWERR_N, 
	IUCMPERR_N => IUMCERR_N, 
	FPUHWERR_N => FPUHWERR_N, 
	FPUCMPERR_N => FPUMCERR_N, 
	DDIR_N => DDIR_N, 
	SYSHALT_N => VCC, 
	NOPAR_N => VCC, 
	ROMWRT_N => VCC, 
	TCK => GND, 
	TRST_N => GND, 
	TMS => GND, 
	TDI => GND, 
	WDCLK => SCLK(0), 
	RXA => VCC, 
	RXB => VCC, 
	CLK2 => CLK2, 
	D => D, 
	DPARIO => DPAR, 
	DRDY_N => open, 
	AOE_N => AOE_N, 
	COE_N => COE_N, 
	DOE_N => DOE_N, 
	BHOLD_N => BHOLD_N, 
	MDS_N => MDS_N, 
	MEXC_N => MEXC_N, 
	MHOLD_N => MHOLDA_N, 
	BA => BA, 
	CB => CB, 
	ALE_N => ALE_N, 
	ROMCS_N => ROMCS_N, 
	MEMCS_N => MEMCS_N, 
	OE_N => OE_N, 
	MEMWR1_N => MEMWR1_N, 
	MEMWR2_N => MEMWR2_N, 
	MEMBEN_N => MEMBEN_N, 
	DDIR => DDIR, 
	IOSEL_N => IOSEL_N, 
	IOWR_N => IOWR_N, 
	IOBEN_N => IOBEN_N, 
	EXMCS_N => EXMCS_N, 
	DMAGNT_N => open, 
	IRL => IRL, 
	EXTINTACK => open, 
	RESET_N => RESET_N, 
	SYSERR_N => SYSERR_N, 
	SYSAV => SYSAV, 
	CPUHALT_N => HALT_N, 
	TDO => open, 
	TXA => open, 
	TXB => open, 
	RAMBEN_N => RAMBEN_N, 
	ROMBEN_N => ROMBEN_N
);

  addresslatch1: AC377
    port map(
	CP => SCLK(1),
	CE_N => ALE_N,
	D    => A(9 downto 2),
	Q    => AL(9 downto 2)
    );

  addresslatch2: AC377
    port map(
	CP => SCLK(1),
	CE_N => ALE_N,
	D    => A(17 downto 10),
	Q    => AL(17 downto 10)
    );


  addresslatch3: AC377
    port map(
	CP => SCLK(1),
	CE_N => ALE_N,
	D    => A(25 downto 18),
	Q    => AL(25 downto 18)
    );

  databuffer0 : AC245
    port map(
	OE_N => RAMBEN_N,
	T_R_N => DDIR,
	A => D(7 downto 0),
	B => DB(7 downto 0)
    );
  databuffer1 : AC245
    port map(
	OE_N => RAMBEN_N,
	T_R_N => DDIR,
	A => D(15 downto 8),
	B => DB(15 downto 8)
    );

  databuffer2 : AC245
    port map(
	OE_N => RAMBEN_N,
	T_R_N => DDIR,
	A => D(23 downto 16),
	B => DB(23 downto 16)
    );
  databuffer3 : AC245
    port map(
	OE_N => RAMBEN_N,
	T_R_N => DDIR,
	A => D(31 downto 24),
	B => DB(31 downto 24)
    );


  ram0 : ram8
    port map (
	A   => AL(20 downto 2),
	D   => DB(7 downto 0),
	CE1 => MEMCS_N(0),
	WE  => MEMWR1_N(0),
	OE  => OE_N(0)
    );


  ram1 : ram8
    port map (
	A   => AL(20 downto 2),
	D   => DB(15 downto 8),
	CE1 => MEMCS_N(0),
	WE  => MEMWR1_N(0),
	OE  => OE_N(0)
    );

  ram2 : ram8
    port map (
	A   => AL(20 downto 2),
	D   => DB(23 downto 16),
	CE1 => MEMCS_N(0),
	WE  => MEMWR1_N(0),
	OE  => OE_N(0)
    );

  ram3 : ram8
    port map (
	A   => AL(20 downto 2),
	D   => DB(31 downto 24),
	CE1 => MEMCS_N(0),
	WE  => MEMWR1_N(0),
	OE  => OE_N(0)
    );

  chkram0 : ram8
    port map (
	A   => AL(20 downto 2),
	D(6 downto 0) => CB(6 downto 0),
	D(7) => DPAR,
	CE1 => MEMCS_N(0),
	WE  => MEMWR1_N(0),
	OE  => OE_N(0)
    );


  bootprom : flashprom
    port map (
	A(18 downto 2) => AL(18 downto 2),
	A(1 downto 0) => BA,
	CE  => ROMCS_N,
	WE  => MEMWR1_N(0),
	OE  => OE_N(0),
	D   => D(7 downto 0)
    );

----- Reporting --------

  startmsg : process(SYSRESET_N, MEMWR1_N(0))
  begin
    if SYSRESET_N = '1'  and SYSRESET_N'event then
      print("Running system test ...");
    end if;

    if MEMWR1_N(0) = '1'  and MEMWR1_N(0)'event and 
       (AL = "10000000001111110111110000") and
       (DB = "11000000000010000000001000100000")
    then
      print("TEST OK");
    end if;
  end process;

  AL(1 downto 0) <= "00";	-- To improve readability in waveform viewer...
  
end Simulation;


---------|---------|---------|---------|---------|---------|---------|--------|
---------|---------|---------|---------|---------|---------|---------|--------|
---------|---------|---------|---------|---------|---------|---------|--------|

configuration board_cfg of board is
  for Simulation
  end for;
end board_cfg;
