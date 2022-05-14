-------------------------------------------------------------------------------
-- File name : iurt_gen_ent.vhd
-- Title : IURTGeneric (entity)
-- project : SPARC 
-- Library : IURTLIB
-- Author(s) : Maxime ROCCA
-- Purpose :  definition of entity IURTGeneric
-- notes :   
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        |  MR    | 94-03-04    | first version
-- Compliant with the IU-RT Device Specification, issue 4.
--.............................................................................
-- v 1.1        |  MR    | 94-05-27    | 2nd version
-- + modify timing checkers
-------------------------------------------------------------------------------
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
 
-------------------------------------------------------------------------------
---------|---------|---------|---------|---------|---------|---------|--------|

library IEEE;
use IEEE.Std_Logic_1164.all;
library MMS;
use MMS.StdIoImp.all;
use MMS.StdSim.all;
use MMS.StdTiming.all;


entity IURTGeneric is
  generic( -- Fake default timing values.
     tCY   : time := 50 ns; -- Clock cycle
     tCHL  : time := 22 ns; -- Clock high and low
     tAD   : time := 8 ns; -- A,ASI,SIZE,RD,WRT,WE_N,LOCK,LDSTO output delay 
     tAH   : time := 7 ns; -- A,ASI,SIZE,RD,WRT,WE_N,LOCK,LDSTO output valid
     tDOD  : time := 8 ns; -- D output delay
     tDOH  : time := 7 ns; -- D output valid
     tDIS  : time := 5 ns; -- D input setup
     tDIH  : time := 1 ns; -- D input hold
     tMES  : time := 5 ns; -- MEXC_N input setup
     tMEH  : time := 1 ns; -- MEXC_N input hold
     tHS   : time := 5 ns; -- BHOLD_N,MHOLDA_N,MHOLDB_N,FHOLD,CHOLD input setup
     tHH   : time := 1 ns; -- BHOLD_N,MHOLDA_N,MHOLDB_N,FHOLD,CHOLD input hold
     tHOD  : time := 8 ns; -- XHOLD_N to address/control output delay
     tHOH  : time := 8 ns; -- XHOLD_N to address/control output valid
     tOE   : time := 8 ns; -- AOE_N, COE_N, DOE_N to output enable delay
     tOD   : time := 8 ns; -- AOE_N, COE_N, DOE_N to output disable delay
     tTOE  : time := 8 ns; -- TOE_N to output enable delay
     tTOD  : time := 8 ns; -- TOE_N to output disable delay
     tSSD  : time := 8 ns; -- INST, FXACK, CXACK, INTACK output delay
     tSSH  : time := 7 ns; -- INST, FXACK, CXACK, INTACK output valid
     tRS   : time := 5 ns; -- RESET_N input setup
     tRH   : time := 1 ns; -- RESET_N input hold
     tFD   : time := 8 ns; -- FINS, CINS output delay
     tFH   : time := 7 ns; -- FINS, CINS output valid
     tFIS  : time := 5 ns; -- FCC, CCC input setup
     tFIH  : time := 1 ns; -- FCC, CCC input hold
     tDXD  : time := 8 ns; -- DXFER output delay
     tDXH  : time := 7 ns; -- DXFER output valid
     tHDXD : time := 8 ns; -- XHOLD_N asserted to DXFER output delay
     tHDXH : time := 7 ns; -- XHOLD_N asserted to DXFER output valid
     tNUD  : time := 8 ns; -- INULL output delay
     tNUH  : time := 7 ns; -- INULL output valid
     tMDS  : time := 5 ns; -- MDS_N input setup
     tMDH  : time := 1 ns; -- MDS_N input hold
     tFLS  : time := 8 ns; -- FLUSH output delay
     tFLH  : time := 7 ns; -- FLUSH output valid
     tCCVS : time := 5 ns; -- FCCV, CCCV input setup
     tCCVH : time := 1 ns; -- FCCV, CCCV input hold
     tXES  : time := 5 ns; -- FEXC_N, CEXC_N input setup
     tXEH  : time := 1 ns; -- FEXC_N, CEXC_N input hold
     tMAD  : time := 8 ns; -- MAO asserted to address/control output delay
     tMAH  : time := 8 ns; -- MAO asserted to address/control output valid
     tETD  : time := 8 ns; -- HWERROR_N output delay
     tETH  : time := 7 ns; -- HWERROR_N output valid
     tXAPD : time := 8 ns; -- APAR & ASPAR output delay
     tXAPH : time := 7 ns; -- APAR & ASPAR output valid
     tDPOD : time := 8 ns; -- DPAR output delay
     tDPOH : time := 7 ns; -- DPAR output valid
     tDPIS : time := 5 ns; -- DPAR input setup
     tDPIH : time := 1 ns; -- DPAR input hold
     tIFPD : time := 8 ns; -- IFPAR output delay
     tIFPH : time := 7 ns; -- IFPAR output valid
     tFIPS : time := 8 ns; -- FIPAR output delay
     tFIPH : time := 7 ns; -- FIPAR output valid
     tIMPD : time := 8 ns; -- IMPAR output delay
     tIMPH : time := 7 ns; -- IMPAR output valid
     tMCED : time := 8 ns; -- MCERR_N output delay
     tMCEV : time := 7 ns; -- MCERR_N output valid
     tSTATS : time := 5 ns; -- N601MODE_N, FLOW_N, CMODE_N, FP_N input setup
     tHAS  : time := 5 ns; -- HALT_N input setup
     tHAH  : time := 1 ns; -- HALT_N input hold
     tHAE  : time := 8 ns; -- HALT_N asserted to output enable delay
     tHAD  : time := 8 ns; -- HALT_N asserted to output disable delay
     
     tTCY  : time := 50 ns; -- TCLK Clock Cycle
     tTMS  : time := 5 ns; -- TMS setup
     tTMH  : time := 1 ns; -- TMS hold
     tTDIS : time := 5 ns; -- TDI setup
     tTDIH : time := 1 ns; -- TDI hold
     tTRS  : time := 5 ns; -- TRST_N setup
     tTRH  : time := 1 ns; -- TRST_N hold
     tTDOD : time := 8 ns; -- TDO output delay
     tTDOH : time := 7 ns  -- TDO output valid
  );
  
  port(
    -- Note: signals which are functionally output signals but are actually
    -- inout signals because of the Master/Checker mode have an "*" in the 
    -- comments defining their function.
    
    CLK   : in std_logic; -- clock signal
    
    --  Memory Subsystems Interface Signals  
    A        : inout std_logic_vector(31 downto 0); --* Address bus
    APAR     : inout std_logic; --* Address bus Parity
    AOE_N    : in    std_logic; -- Address Output Enable
    ASI      : inout std_logic_vector(7 downto 0); --* Address Space Identifier
    ASPAR    : inout std_logic; --* ASI & SIZE Parity
    BHOLD_N  : in    std_logic; -- Bus Hold
    COE_N    : in    std_logic; -- Control Output Enable
    D        : inout std_logic_vector(31 downto 0); -- Data Bus
    DPAR     : inout std_logic; -- Data Bus Parity
    DOE_N    : in    std_logic; -- Data Output Enable
    DXFER    : inout std_logic; --* Data Transfer
    IFT_N    : in    std_logic; -- Instruction Cache Flush Trap
    INULL    : inout std_logic; --* Integer Unit Nullify Cycle
    LDSTO    : inout std_logic; --* Atomic Load-Store
    LOCK     : inout std_logic; --* Bus Lock
    MAO      : in    std_logic; -- Memory Address Output
    MDS_N    : in    std_logic; -- Memory Data Strobe
    MEXC_N   : in    std_logic; -- Memory Exception
    MHOLDA_N : in    std_logic; -- Memory Hold A
    MHOLDB_N : in    std_logic; -- Memory Hold B
    RD       : inout std_logic; --* Read Access
    SIZE     : inout std_logic_vector(1 downto 0); --* Bus Transaction Size
    WE_N     : inout std_logic; --* Write Enable
    WRT      : inout std_logic; --* Advanced Write
    IMPAR    : inout std_logic; --* IU to MEC Control Parity
    
    -- Interrupt and Control Signals
    ERROR_N    : out std_logic; -- Error State
    HWERROR_N  : out std_logic; -- Hardware error detected
    FLOW_N     : in  std_logic; -- Enable flow control
    MCERR_N    : out std_logic; -- Comparison error
    N601MODE_N : in  std_logic; -- Normal 601Mode Operation
    CMODE_N    : in  std_logic; -- Checker mode
    FPSYN      : in  std_logic; -- Floating-Point Synomym Mode
    INTACK     : inout std_logic; --* Interrupt Acknowledge
    IRL        : in  std_logic_vector(3 downto 0); -- Interrupt Request Level
    RESET_N    : in  std_logic; -- Integer Unit Reset
    TOE_N      : in  std_logic; -- Test Mode Output Enable
    HALT_N     : in  std_logic; -- Halt
    
    -- Floating Point / Coprocessor Interfaces
    FCC     : in    std_logic_vector( 1 downto 0); -- FP Condition Codes
    FCCV    : in    std_logic; -- Floating-Point Condition Codes Valid
    FEXC_N  : in    std_logic; -- Floating-Point Exception
    FHOLD_N : in    std_logic; -- Floating-Point Hold
    FIPAR   : in    std_logic; -- FPU to IU Control Parity
    FINS1   : inout std_logic; --* Floating-Point Instruction in Buffer 1
    FINS2   : inout std_logic; --* Floating-Point Instruction in Buffer 2
    FLUSH   : inout std_logic; --* Floating-Point/Coproc. Instruction Flush
    FP_N    : in    std_logic; -- Floating-Point Unit Present
    FXACK   : inout std_logic; --* Floating-Point Exception Acknowledge
    INST    : inout std_logic; --* Instruction Fetch
    IFPAR   : inout std_logic; --* IU to FPU Control Parity
    CCC     : in    std_logic_vector( 1 downto 0); -- Coproc. Condition Codes
    CCCV    : in    std_logic; -- Coprocessor Condition Codes Valid
    CEXC_N  : in    std_logic; -- Coprocessor Exception
    CHOLD_N : in    std_logic; -- Coprocessor Hold
    CINS1   : inout std_logic; --* Coprocessor Instruction in Buffer 1
    CINS2   : inout std_logic; --* Coprocessor Instruction in Buffer 2
    CP_N    : in    std_logic; -- Coprocessor Unit Present
    CXACK   : inout std_logic; --* Coprocessor Exception Acknowledge

    -- TAP signals
    TCLK      : in  std_logic; -- Test Clock
    TRST_N    : in  std_logic; -- Test Reset
    TMS       : in  std_logic; -- Test Mode Select
    TDI       : in  std_logic; -- Test Data Input
    TDO       : out std_logic  -- Test Data Output
  );
begin
  --  PUT HERE SOME TIMING CHECKERS: SETUP & HOLD TIME + PULSE WIDTH CHECKERS.
  
  SigRESET_N  : SetupHoldCheck(RESET_N, CLK, EDGE => RISING,
                               SETUP => tRS,  HOLD => tRH, 
                               PATH => "RESET_N", 
                               DelayedData => RESET_N'Delayed(abs(tRH)));
    
  SigN601MODE_N  : SetupHoldCheck(N601MODE_N, CLK, EDGE => RISING,
                                  SETUP => tSTATS,  HOLD => 0 ns, 
                                  PATH => "N601MODE_N", 
                                  DelayedData => N601MODE_N'Delayed(0 ns));
  
  SigFLOW_N   : SetupHoldCheck(FLOW_N, CLK, EDGE => RISING,
                               SETUP => tSTATS,  HOLD => 0 ns, 
                               PATH => "FLOW_N", 
                               DelayedData => FLOW_N'Delayed(0 ns));
  
  SigCMODE_N  : SetupHoldCheck(CMODE_N, CLK, EDGE => RISING,
                               SETUP => tSTATS,  HOLD => 0 ns, 
                               PATH => "CMODE_N", 
                               DelayedData => CMODE_N'Delayed(0 ns));
                               
  SigHALT_N   : SetupHoldCheck(HALT_N, CLK, EDGE => FALLING,
                               SETUP => tHAS,  HOLD => tHAH, 
                               PATH => "HALT_N", 
                               DelayedData => HALT_N'Delayed(abs(tHAH)));
      
  SigTMS     : SetupHoldCheck(TMS, TCLK, EDGE => RISING,
                              SETUP => tTMS,  HOLD => tTMH, 
                              PATH => "TMS", 
                              DelayedData => TMS'Delayed(abs(tTMH)));
                                 
  SigTDI     : SetupHoldCheck(TDI, TCLK, EDGE => RISING,
                              SETUP => tTDIS,  HOLD => tTDIH, 
                              PATH => "TDI", 
                              DelayedData => TDI'Delayed(abs(tTDIH)));
                                 
  SigTRST_N  : SetupHoldCheck(TRST_N, TCLK, EDGE => RISING,
                              SETUP => tTRS,  HOLD => tTRH, 
                              PATH => "TRST_N", 
                              DelayedData => TRST_N'Delayed(abs(tTRH)));
    
  
  CLKHigh : PulseCheck(CLK, LEVEL => '1', WIDTH => tCHL, 
                       SENSE => MINIMUM, PATH => "CLK");
                                
  CLKlow  : PulseCheck(CLK, LEVEL => '0', WIDTH => tCHL, 
                       SENSE => MINIMUM, PATH => "CLK");
    
  ---
  CLKCycle : process    -- Passive process.
    variable DeltaT   : time := 0 ns;
    variable LastEdge : time := -1 sec;
  begin
    if not(CHECK_ON) then
      wait; -- the process dies here....
    end if;
    
    wait on CLK until rising_edge(CLK);
    
    DeltaT   := now - LastEdge;
    LastEdge := now;
    
    assert (DeltaT >= tCY)
          report "Clock cycle violation: minimal value is " &
                 ToString(tCY) & "; value observed: " &
                 ToString(DeltaT) & "."
          severity warning;
    
  end process CLKCycle; 

  ----
  RESET_Nwidth : process    -- Passive process.
    constant MIN_NB_RESET_CYCLES : natural := 10;
    variable CountNbResetCycle : natural := 0;
  begin
    if not(CHECK_ON) then
      wait; -- the process dies here....
    end if;
    
    wait on CLK until rising_edge(CLK);
    
    if RESET_N = '1' then
      if CountNbResetCycle /= 0 and CountNbResetCycle < MIN_NB_RESET_CYCLES then
        assert FALSE
          report "Pulse width violation for RESET_N: should stay low for at " & 
                 "least " & ToString(MIN_NB_RESET_CYCLES) & 
                 " rising clock edges!"
          severity warning;
      end if;
      CountNbResetCycle := 0;
    elsif RESET_N = '0' then
      CountNbResetCycle := CountNbResetCycle + 1;
    end if;
    
  end process RESET_Nwidth; 
  
end IURTGeneric;
-------------------------------------------------------------
-- File containing timing values for the IURT VHDL model.
-- 
-- ALL THE TIMING PARAMETERS are given at 125 degrees C,  
-- 4.5 Volts and in worst case process conditions.
-- WARNING: minimal values for output signal propagation 
-- delay in data sheets are usually given in best conditions,  
-- i.e -55 Celsius, 5.5 Volts and best case process conditions.
-- They must be re-calculated for worst case conditions.
-------------------------------------------------------------

package IURTTimPar is

  constant tCY    : time := 40 ns;
  constant tCHL   : time := 18 ns;
  constant tAD    : time := 33 ns; 
  constant tAH    : time := 16 ns;
  constant tDOD   : time := 29 ns;
  constant tDOH   : time :=  9 ns; 
  constant tDIS   : time :=  3 ns; 
  constant tDIH   : time :=  4 ns;
  constant tMES   : time := 15 ns;
  constant tMEH   : time :=  2 ns;
  constant tHS    : time :=  7 ns;
  constant tHH    : time :=  4 ns;
  constant tHOD   : time := 22 ns;
  constant tHOH   : time :=  8 ns; -- WARNING:
                                   -- For tHOH, the IURT spec. gives 0 ns.
                                   -- This does not seem to be compatible with
                                   -- the FPURT spec. for the hold time on A 
                                   -- bus (6 ns).
  constant tOE    : time := 15 ns;
  constant tOD    : time := 15 ns;
  constant tTOE   : time := 21 ns;
  constant tTOD   : time := 21 ns;
  constant tSSD   : time := 20 ns;
  constant tSSH   : time :=  7 ns;
  constant tRS    : time := 15 ns;
  constant tRH    : time :=  3 ns;
  constant tFD    : time := 27 ns;
  constant tFH    : time :=  9 ns;
  constant tFIS   : time := 10 ns;
  constant tFIH   : time :=  4 ns;
  constant tDXD   : time := 28 ns;
  constant tDXH   : time :=  5 ns;
  constant tHDXD  : time := 20 ns;
  constant tHDXH  : time :=  6 ns;-- WARNING:
                                   -- For tHDXH, the IURT spec. gives 0 ns.
                                   -- This does not seem to be compatible with
                                   -- the MEC spec. for the hold time on DXFER.
  constant tNUD   : time := 20 ns;
  constant tNUH   : time :=  7 ns;
  constant tMDS   : time :=  5 ns;
  constant tMDH   : time :=  4 ns;
  constant tFLS   : time := 15 ns;
  constant tFLH   : time :=  7 ns;
  constant tCCVS  : time :=  7 ns;
  constant tCCVH  : time :=  4 ns;
  constant tXES   : time := 10 ns;
  constant tXEH   : time :=  4 ns;
  constant tMAD   : time := 20 ns; 
  constant tMAH   : time :=  5 ns;
  constant tETD   : time := 25 ns;
  constant tETH   : time :=  7 ns;
  constant tXAPD  : time := 33 ns;
  constant tXAPH  : time := 16 ns;
  constant tDPOD  : time := 29 ns;
  constant tDPOH  : time :=  9 ns;
  constant tDPIS  : time :=  3 ns;
  constant tDPIH  : time :=  4 ns;
  constant tIFPD  : time := 29 ns;
  constant tIFPH  : time :=  7 ns;
  constant tFIPS  : time := 10 ns;
  constant tFIPH  : time :=  4 ns;
  constant tIMPD  : time := 33 ns;
  constant tIMPH  : time := 16 ns;
  constant tMCED  : time := 15 ns;
  constant tMCEV  : time :=  7 ns;
  constant tSTATS : time := 10 ns;
  constant tHAS   : time :=  7 ns;
  constant tHAH   : time :=  4 ns;
  constant tHAE   : time := 14 ns; -- instead of 18
  constant tHAD   : time := 14 ns; -- instead of 18
   
  constant tTCY   : time := 100 ns;  
  constant tTMS   : time := 20 ns;  
  constant tTMH   : time :=  4 ns;  
  constant tTDIS  : time := 20 ns;  
  constant tTDIH  : time :=  4 ns;  
  constant tTRS   : time := 20 ns;  
  constant tTRH   : time :=  4 ns;  
  constant tTDOD  : time := 30 ns;   
  constant tTDOH  : time := 12 ns; 

end IURTTimPar;
-------------------------------------------------------------------------------
-- File name : iurt_gen_beh.vhd
-- Title : IURTGeneric (architecture Behavior)
-- project : SPARC 
-- Library : IURT_LIB
-- Author(s) : Maxime ROCCA, Jiri GAISLER
-- Purpose : definition of architecture Behavior for IURTGeneric. It is a beha-
--           -vioral description of the IURT at the highest level.
-- notes : The entity IURTGeneric is defined in the file iurt_gen_ent.vhd.
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        |  MR    | 94-03-04     | first version
-- Compliant with the the IURT Device Specification, issue 4++ (i.e certain
-- features implemented in this version of the IU-RT model will only appear in
-- the next issue of the IU-RT Device Specification), except for coprocessor
-- interface.
--.............................................................................
-- v 1.1        |  MR    | 94-05-03     | 2nd version
-- + IURT model is made sensitive to CCCV and CHOLD_N
-- + modif. condition for parity bit checking
-- + bug fix for memory exception (MEXC_N)
-- + bug fix about XHOLD on annulling FP branch
-- + bug fix on ASI generation for load/store instructions in alternate space.
-- + bug fix on return address for trap in particular case.
-- + bug fix for cache miss on branching instruction.
-- + bug fix for memory exception in case of STD or LDD.
-- + modif. definition of IMPAR
-- + change name of tap controller
-- + modelling of buffers slightly modified.
-- + modification concerning parity bit checking
--.............................................................................
-- v 1.2        |  MR    | 94-05-27     | 3rd version
-- + modification of timing checkers
--.............................................................................
-- v 1.3        |  RC    | 95-12-11     | 4th version
-- + modification of the instruction pipeline in order to handle correctly 
--   JMPL and RETT instructions and the address generation in case of CALL
-- v 1.4        |  JG    | 96-03-04     | 5th version
-- + bug fix for memory exception in second data cycle for STD
-- + bug fix for trap handling in LD and LDD
-- v 1.5        |  JG    | 96-09-24     | 6th version
-- + bug fix to supress false warnings
-- + bug fix for INULL geneation and LD/ST hardware interlock
-------------------------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
-- Copyright ESA/ESTEC
-------------------------------------------------------------------------------
---------|---------|---------|---------|---------|---------|---------|--------|

library IEEE;
use IEEE.Std_Logic_1164.all;
library MMS;
use MMS.StdRtl.all;
use MMS.StdIoImp.all;
use MMS.StdTiming.all;
library SPARC_LIB;
use SPARC_LIB.SparcPck.all;
use SPARC_LIB.TAPCompPck.all;


architecture vhdl_behavioral of IURTGeneric is

  -- constant for modelling
  constant TRI_STATE32 : std_logic_vector(31 downto 0)
                         := "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";

  -- Pipeline registers:
  --   ID: Instruction Decode stage
  --   EX: EXecute stage
  --   WR: WRite stage
  signal ID, EX, WR : Instruction;
  
  -- Intermediary signals for hold signals:                                 
  -- SIGNALlat = output of a latch (transparent clock high) with signal
  --             SIGNAL as input to the latch.
  -- HOLDsig_N: all hold signals are ANDed together and generate HOLDsig_N.
  signal HOLDsig_N   : std_logic;
  signal FHOLD_Nlat  : std_logic;
  signal CHOLD_Nlat  : std_logic;
  signal BHOLD_Nlat  : std_logic;
  signal MHOLDA_Nlat : std_logic;
  signal MHOLDB_Nlat : std_logic;
  signal FCCVlat     : std_logic;
  signal CCCVlat     : std_logic;
    
  -- Signals inside model corresponding to output or bidirectional port signals.
  -- A signal SIGin corresponds to the output port signal SIG.
  signal Ain      : std_logic_vector(31 downto 0);
  signal APARin   : std_logic;
  signal ASIin    : std_logic_vector(7 downto 0);
  signal Din      : std_logic_vector(31 downto 0);
  signal DPARin   : std_logic;
  signal DXFERin  : std_logic;
  signal INULLin  : std_logic;
  signal LDSTOin  : std_logic;
  signal LOCKin   : std_logic;
  signal RDin     : std_logic;
  signal SIZEin   : std_logic_vector(1 downto 0);
  signal ASPARin  : std_logic;
  signal WE_Nin   : std_logic;
  signal WRTin    : std_logic;
  signal IMPARin  : std_logic;
  signal FINS1in  : std_logic;
  signal FINS2in  : std_logic;
  signal FLUSHin  : std_logic;
  signal FXACKin  : std_logic;
  signal INSTin   : std_logic;
  signal IFPARin  : std_logic;
  signal INTACKin : std_logic;
  signal ERROR_Nin   : std_logic;
  signal HWERROR_Nin : std_logic;
  signal MCERR_Nin   : std_logic;
  
  -- Signals used for the IO buffers modelling
  signal AOE_Ndel : std_logic; 
  signal DOE_Ndel : std_logic; 
  signal COE_Ndel : std_logic;
  signal TOE_Ndel : std_logic; 
  signal HALTsampled : std_logic;
  signal DTHCsig  : std_logic;
  signal ATHCsig  : std_logic;
  signal CTHCsig  : std_logic;
  signal ACTHCsig : std_logic;
  signal THCsig   : std_logic;

  -- Specific signals for inter-process communication used for
  -- timing violation checking.
  signal CheckTimBidir     : boolean := FALSE; -- Checking performed if TRUE
  signal CheckTimBidir_1_i : boolean := FALSE;
  signal CheckTimBidir_1   : boolean := FALSE; -- Checking performed if TRUE
  signal CheckTimBidir_2_i : boolean := FALSE; 
  signal CheckTimBidir_2   : boolean := FALSE; -- Checking performed if TRUE
  
  signal Chk_MEXC_N_en   : boolean;
  signal Chk_BHOLD_N_en  : boolean;
  signal Chk_MHOLDA_N_en : boolean;
  signal Chk_MHOLDB_N_en : boolean;
  signal Chk_FHOLD_N_en  : boolean;
  signal Chk_FCC_en      : boolean;
  signal Chk_MDS_N_en    : boolean;
  signal Chk_FCCV_en     : boolean;
  signal Chk_FEXC_N_en   : boolean;
  signal Chk_FIPAR_en    : boolean;
  signal Chk_FP_N_en     : boolean;
  
    signal Buf1IsValid_Spy : boolean;     -- Flag for validity of buffer 1.
    signal Buf2IsValid_Spy : boolean;     -- Flag for validity of buffer 2.
    signal IOPcase_Spy : boolean; -- set to true if IOP to be scheduled.
  
    signal WR1x, wr2x, wr3x : Instruction;
    signal CurrentAddrx : std_logic_vector(31 downto 0);
  -- Configuration of components
--  for all : TAP_iufpu use entity SPARC_LIB.tap_iufpu(vhdl_behavioral);
     
begin

  IUmodel: process
  
    ------ Declaration zone for process IUmodel ------
    variable PowerUP : boolean := TRUE; -- Start-up flag for initialization.
    variable StartUp : boolean := TRUE; -- Another flag for start-up.
      
    variable Mode : ModeType := RESET_MODE; -- State (or mode) of the 
                                            -- processor when powered up.

    variable TrapMode : TrapModeType := NOTRAP; -- Flag used for pipeline
                                                -- progression.
    
    -- State registers declaration
    variable PSR : std_logic_vector(31 downto 0) :=
	"00000000000000000000000000000000"; -- Processor State Register.
      alias impl : std_logic_vector(3 downto 0) is PSR(31 downto 28);
      alias ver  : std_logic_vector(3 downto 0) is PSR(27 downto 24);
      alias icc  : std_logic_vector(3 downto 0) is PSR(23 downto 20);
      alias Reserved_PSR : std_logic_vector(5 downto 0) is PSR(19 downto 14);
      alias EC   : std_logic is PSR(13);
      alias EF   : std_logic is PSR(12);
      alias PIL  : std_logic_vector(3 downto 0) is PSR(11 downto  8);
      alias S    : std_logic is PSR(7);
      alias PS   : std_logic is PSR(6);
      alias ET   : std_logic is PSR(5);
      alias CWP  : std_logic_vector(4 downto 0) is PSR(4 downto 0);
      
    variable TBR : std_logic_vector(31 downto 0) :=
	"00000000000000000000000000000000"; -- Trap Base Register.
      alias TBA : std_logic_vector(19 downto 0) is TBR(31 downto 12);
      alias tt  : std_logic_vector( 7 downto 0) is TBR(11 downto  4);
      -- bit 3 through 0 of TBR are kept to 0
      
    variable WIM : std_logic_vector(31 downto 0); -- Window Invalid Mask Reg.
    variable   Y : std_logic_vector(31 downto 0); -- Y Register.
    
    -- Register file: 8 global registers + 16*NWINDOWS windowed registers.
    variable RegFile: RegisterFile(16*NWINDOWS+7 downto 0);
    
    -- Address registers (related to Program Counters)
                   -- Current  Address.
    variable SA1CurrentAddr : std_logic_vector(31 downto 0);
    variable CurrentAddr : std_logic_vector(31 downto 0);
                   -- Previous  Address.
    variable PrevAddr : std_logic_vector(31 downto 0);
                   -- Previous to previous Address.
    variable pPrevAddr : std_logic_vector(31 downto 0);
                   -- Save registers for store or load or loadstore
    variable SaveCurrentAddr : std_logic_vector(31 downto 0);
    variable SavePrevAddr : std_logic_vector(31 downto 0);
        
    -- Pipeline buffer declaration + convenient temp. variable
    variable nID : Instruction; -- next Instruction Decode: temp. variable.
    variable nIDisValid  : boolean; -- Flag for validity of nID.
    variable InstBuffer1 : Instruction; -- Instruction Buffer 1.
    variable Buf1IsValid : boolean;     -- Flag for validity of buffer 1.
    variable InstBuffer2 : Instruction; -- Instruction Buffer 2.
    variable Buf2IsValid : boolean;     -- Flag for validity of buffer 2.
    variable IOPcase : boolean; -- set to true if IOP to be scheduled.
    variable nIDTemp : Instruction; -- Temp. variable for nID
    
    -- WR1 and WR2 are fictitious pipeline stages.
    -- WR1: "stage" just after WR stage
    -- WR2: "stage" just after WR1 stage
    -- WR3: "stage" just after WR2 stage
    variable WR1 : Instruction;
    variable WR2 : Instruction;
    variable WR3 : Instruction;
        
    -- Declaration of data objects for trap handling  
    variable TrapVector : TrapVectorType;
    variable NextAddressForTraps : std_logic_vector(31 downto 0); -- Addr. to be
                                                                  -- put in r18.
    variable Preserve_SavePrevAddr : std_logic_vector(31 downto 0); 
                                -- Save value to be put in r18 in certain cases
    
    -- Data objects for "anticipated" execution of OPcc instructions.
    variable ResultOpcc : std_logic_vector(31 downto 0); -- computed result
    variable iccTemp    : std_logic_vector(3 downto 0);  -- modified temp. icc
    variable YTemp      : std_logic_vector(31 downto 0); -- Temporary Y reg.
            
    -- Data objects for STORE instructions.
    variable StoreData : std_logic_vector(31 downto 0);
    variable DestReg   : natural;
    
    -- Data object for LOAD instructions.
    variable LoadDataAddr : std_logic_vector(31 downto 0);
    
    -- Data object for SWAP instructions
    variable SwapData : std_logic_vector(31 downto 0);
    
    -- Variables used for proper address generation when data cache miss or 
    -- memory exception bus hold.
    variable LDdataAD1 : std_logic_vector(31 downto 0);
    
    -- Flag for branches (Bicc, FBfcc)
    variable TakenBr : boolean := FALSE; -- Taken branch flag.

    -- Variables for output signals: corresponding signal name is just before
    -- "var" in variable name.
    -- pSIGNALvar = value of SIGNAL in previous cycle.
    -- pTOpSIGNALvar = value of SIGNAL in the cycle before the previous cycle.
    variable ASIvar       : natural; 
    variable pASIvar      : natural; 
    variable pTOpASIvar   : natural;
    variable SizeVar      : natural;
    variable pSizeVar     : natural;
    variable pTOpSizeVar  : natural;
    variable INTACKvar    : std_logic; 
    variable pINTACKvar   : std_logic; 
    variable ERROR_Nvar   : std_logic;
    variable RDvar        : std_logic;
    variable pRDvar       : std_logic;
    variable pTOpRDvar    : std_logic;
    variable WE_Nvar      : std_logic; 
    variable pWE_Nvar     : std_logic;
    variable pTOpWE_Nvar  : std_logic;
    variable WRTvar       : std_logic;
    variable pWRTvar      : std_logic;
    variable pTOpWRTvar   : std_logic;
    variable DXFERvar     : std_logic;
    variable pDXFERvar    : std_logic;
    variable pTOpDXFERvar : std_logic;
    variable LDSTOvar     : std_logic;
    variable pLDSTOvar    : std_logic;
    variable pTOpLDSTOvar : std_logic;
    variable INULLvar     : std_logic;
    variable pINULLvar    : std_logic;
    variable LOCKvar      : std_logic;
    variable pLOCKvar     : std_logic;
    variable pTOpLOCKvar  : std_logic;
    variable INSTvar      : std_logic;
    variable pINSTvar     : std_logic;
    variable FLUSHvar     : std_logic;
    variable pFLUSHvar    : std_logic;
    variable FXACKvar     : std_logic;
    variable pFXACKvar    : std_logic;
    variable FINS1var     : std_logic;
    variable pFINS1var    : std_logic;
    variable FINS2var     : std_logic;
    variable pFINS2var    : std_logic;
    variable HWERROR_Nvar  : std_logic;    
    variable pHWERROR_Nvar : std_logic;
    
    -- Flag for INTACK, FLUSH, FXACK signals
    variable TriggerINTACK : boolean := FALSE;
    variable TriggerFXACK  : boolean := FALSE;
    variable TriggerFLUSH  : boolean := FALSE;
    
    -- Variables for input signals: IRL bus, MDS_N
    variable IRLvar    : natural;
    variable pIRLvar   : natural;
    variable MDS_Nvar   : std_logic;
    variable MEXC_Nvar  : std_logic;
    variable FEXC_Nvar  : std_logic;
    variable FCCVvar    : std_logic;
    variable FHOLD_Nvar : std_logic;
      
    -- Variable to freeze the pipeline
    variable FrozenPipe : boolean := FALSE;
    
    -- Flags related to ERROR or RESET mode.
    variable FrozenBusses : boolean := TRUE; -- TRUE when in RESET_MODE except
                                             -- for the 1st clock cycle of reset
    variable ResetHasBeenDetected : boolean := FALSE;
    variable LocalCounterDuringReset : integer := -1;
    variable GenerateASIsig   : boolean := FALSE;
    variable GenerateERRORsig : boolean := FALSE;
    variable TrigERRcountdown : integer := -1;
    
    -- Master/Checker related variable
    variable CMDmismatch : natural := 0;

    -- Variable for Memory access exceptions
    variable PendingInstAccExc : integer := -2;

    -- Variables for Program Flow Control Scheme
    variable Checksum  : std_logic_vector(31 downto 0);
    variable CHKSUMsig : std_logic_vector(15 downto 0);
    variable ChecksumCompare : boolean;
        
    -- Variables for parity bit checking
    variable ParBitViolCount : integer := -1;
    variable DPARviol        : boolean := FALSE;
    variable FIPARviol       : boolean := FALSE;

    -- Other variables 
    variable Vec8bits : std_logic_vector(7 downto 0);
    variable Vec2bits : std_logic_vector(1 downto 0);
    variable PendingERROR : boolean := FALSE;
    variable TempINST : Instruction;
    
  begin
    ------ The body of this "if" statement is executed only once ------
    ------ for initialization purposes.                          ------
    if PowerUP then
      PowerUP := FALSE;
      
      RegFile(0) := (others => '0'); -- Global register zero.
                                     -- This register should NEVER be written.

      impl := PSR_IMPL; -- These bits are not affected by a WRPSR. They should
                        -- NOT be affected by a write.
                        
      ver  := PSR_VER; -- These bits are not affected by a WRPSR. They should
                       -- NOT be affected by a write.

      TBR(3 downto 0) := TBR3_DOWNTO_0; -- These bits are always 0. They should
                                 -- NOT be affected by a write.
                                 
      for i in NWINDOWS to 31 loop
        WIM(i) := '0'; -- Unimplemented windows are read as ZEROS and not 
      end loop;        -- affected by WRWIM.
      
      -- Checking timing parameters.
      if tAH >= tAD then
        assert FALSE report "[IURTGeneric(Behave)]: parameter tAD must be " &
                             "greater than tAH after re-computation!"
                      severity failure;
      end if;
   
      if tXAPH >= tXAPD then
        assert FALSE report "[IURTGeneric(Behave)]: parameter tXAPD must be " &
                             "greater than tXAPH after re-computation!"
                      severity failure;
      end if;
   
      if tDOH >= tDOD then
        assert FALSE report "[IURTGeneric(Behave)]: parameter tDOD must be " &
                             "greater than tDOH after re-computation!"
                      severity failure;
      end if;
   
      if tDPOH >= tDPOD then
        assert FALSE report "[IURTGeneric(Behave)]: parameter tDPOD must be " &
                             "greater than tDPOH after re-computation!"
                      severity failure;
      end if;
   
      if tIMPH >= tIMPD then
        assert FALSE report "[IURTGeneric(Behave)]: parameter tIMPD must be " &
                             "greater than tIMPH after re-computation!"
                      severity failure;
      end if;
   
      if tIFPH >= tIFPD then
        assert FALSE report "[IURTGeneric(Behave)]: parameter tIMPD must be " &
                             "greater than tIMPH after re-computation!"
                      severity failure;
      end if;
   
    end if;
    -------------------------------------------------------------------

    wait on CLK,
            MHOLDA_N, MHOLDB_N, BHOLD_N, MAO, FHOLD_N, FCCV,
            MHOLDA_Nlat, MHOLDB_Nlat, BHOLD_Nlat, FHOLD_Nlat, FCCVlat,
            HOLDsig_N,
            TOE_N,
            DTHCsig, ATHCsig, CTHCsig, ACTHCsig, THCsig,
            Ain, APARin, ASIin, Din, DPARin, DXFERin, INULLin, LDSTOin, LOCKin,
            RDin, SIZEin, ASPARin, WE_Nin, WRTin, IMPARin, FINS1in, FINS2in,
            FLUSHin, FXACKin, INSTin, IFPARin, INTACKin, ERROR_Nin, HWERROR_Nin,
            MCERR_Nin;
    
    
    ---- The 3 possible modes for the IU ----
    case Mode is
      when ERROR_MODE   =>        
        FrozenBusses := TRUE;
        if rising_edge(CLK) then
          if TrigERRcountdown = 0 then
            GenerateERRORsig := TRUE;
            ERROR_Nvar := '0';
            TrigERRcountdown := TrigERRcountdown - 1;
          elsif TrigERRcountdown > 0 then
            TrigERRcountdown := TrigERRcountdown - 1;
          end if;
        end if;
        
      when RESET_MODE   => 
        if ResetHasBeenDetected = TRUE then
          FrozenBusses := TRUE;
        end if;
        
        if (rising_edge(CLK) and Reset_N = '1'and not(StartUp)) then 
          Mode := EXECUTE_MODE; -- reset trap serviced here.
          TrapVector(RESET_TRAP)    := FALSE;
          TrapVector(DETECTED_TRAP) := FALSE;
          FrozenBusses := FALSE;
          ResetHasBeenDetected := FALSE;
          S  := '1';
          ET := '0';
          pINSTvar  := INSTvar;
          INSTvar   := '1'; -- First word on the bus is an instruction.
          pINULLvar := INULLvar;
          INULLvar  := '0';
          PrevAddr  := CurrentAddr;
          CurrentAddr := CurrentAddr + 4;
          
          CheckTimBidir <= TRUE; -- destined to the conditional timing checkers
          
        end if;
        
      when EXECUTE_MODE =>
      
      --------------------------------------------
      -- Only purpose of this: setup/hold timing checkers
      if rising_edge(CLK) then
        if EX.Mnemo = LDFSR or EX.Mnemo = LDF or WR.Mnemo = LDDF or 
           IsStoreInst(EX.Mnemo) or IsStoreInst(WR.Mnemo) or 
           IsStoreDoubleInst(WR1.Mnemo) or 
           WR.Mnemo = LDSTUB or WR.Mnemo = LDSTUBA or 
           WR1.Mnemo = LDSTUB or WR1.Mnemo = LDSTUBA or
           WR.Mnemo = SWAP or WR.Mnemo = SWAPA or 
           WR1.Mnemo = SWAP or WR1.Mnemo = SWAPA then
          CheckTimBidir <= FALSE;
        else
          CheckTimBidir <= TRUE;
        end if;
      end if;
      --------------------------------------------
      
      --^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        if (rising_edge(CLK) and not(FrozenPipe)) then
          --.... Actions related to WRite (and WR1) stage(s) of pipeline ....
          DataFetchForLoadAndLdstInst(LoadDataAddr, D, CWP, WR, WR1,
                                      RegFile, 
                                      SwapData);          
           
      
          --......... Actions related to EXecute stage of pipeline ..........
          ExecutionBody(FP_N, IFT_N,
                        EX, ResultOpcc, YTemp, iccTemp,
                        Y, PSR, TBR, WIM, RegFile, Mode,
                        TrapVector);
                        
          --..... Evaluates latched FEXC_N pin for FP exceptions ............
          --..... A FP trap is recognized as a FP instr. goes into ..........
          --..... EX stage (ID.Mnemo is a FP instruction) TBC ...............
          if FEXC_Nvar = '0' and IsFPinst(EX.Mnemo) and FP_N = '0' then
            TrapVector(DETECTED_TRAP) := TRUE;
            TrapVector(FP_EXCEPTION)  := TRUE;
          end if;

          --....... Program Flow Control scheme .......
          if FLOW_N = '0' then
            if EX.Mnemo = SETHI and EX.rd = 0 then
              if EX.disp22 = 0 then
                ChecksumCompare := FALSE; -- disable next checksum comparison.
              elsif ChecksumCompare then
                CHKSUMsig := Checksum(31 downto 16) xor Checksum(15 downto 0);
                if EX.BitInstr(21 downto 16) = "011111" and 
                   CHKSUMsig /= EX.BitInstr(15 downto 0) then
                  TrapVector(DETECTED_TRAP)    := TRUE;
                  TrapVector(PROGRAM_FLOW_ERR) := TRUE;
                end if;
              elsif not(ChecksumCompare) then
                ChecksumCompare := TRUE;
              end if;
              Checksum := (others => '0'); -- zero checksum.
            elsif EX.Mnemo /= IOP and EX.Mnemo /= ANNULLED and 
                  EX.Mnemo /= NOTHING then
              Checksum := CheckSum xor EX.BitInstr(31 downto 0);
              if EX.Mnemo = RETT then
                ChecksumCompare := FALSE; -- disable next checksum comparison.
              end if;
            end if;
          end if;

          --....... Parity bit checking --> generate hardware error .......
          -- WARNING: fuzzy behavior of signal HWERROR_N.
          -- What if violation while on HOLD???
          -- Should there be a violation on DPAR when LDFSR, LDF or LDDF???
          if N601MODE_N = '1' then
            if ParBitViolCount = 1 and EX.Mnemo /= IOP then
              TrapVector(DETECTED_TRAP) := TRUE; 
              if DPARviol then
                TrapVector(RESTART_IMPRECISE) := TRUE;
                DPARviol := FALSE;
              end if; 
              if FIPARviol then
                TrapVector(NON_RESTART_IMPRECISE) := TRUE;
                FIPARviol := FALSE;
              end if;
            end if;
            
            if ParBitViolCount >= 0 and EX.Mnemo /= IOP then
              ParBitViolCount := ParBitViolCount - 1;
            end if;
          
            if (FIPAR /= OddParityOf(FEXC_N & FCC & FCCVvar & FHOLD_Nvar) and
                FIPAR /= 'Z' and FEXC_N /= 'Z' and FCC /= "ZZ" and
                FCCVvar /= 'Z' and FHOLD_Nvar /= 'Z' and
                ParBitViolCount = -1) then
              ParBitViolCount := 2;
              FIPARviol := TRUE;
            end if;
            if DPAR /= OddParityOf(D) and DPAR /= 'Z' and D /= TRI_STATE32 and
               ParBitViolCount = -1 and
               not(WR.Mnemo = LDFSR or WR.Mnemo = LDF or 
                   WR.Mnemo = LDDF or WR1.Mnemo = LDDF or 
                   IsStoreInst(WR.Mnemo) or IsStoreInst(WR1.Mnemo) or 
                   IsStoreDoubleInst(WR2.Mnemo) or 
                   WR1.Mnemo = LDSTUB or WR1.Mnemo = LDSTUBA or 
                   WR2.Mnemo = LDSTUB or WR2.Mnemo = LDSTUBA or
                   WR1.Mnemo = SWAP or WR1.Mnemo = SWAPA or 
                   WR2.Mnemo = SWAP or WR2.Mnemo = SWAPA
                  ) then
              ParBitViolCount := 2;
              DPARviol := TRUE;
            end if;
          end if;

          --...................... TRAPS  ..................................
          TrapHandler(EX, WR, WR1, WR2,
                      pIRLvar, IRLvar,
                      TBR, PSR, TrapVector, Mode,
                      TrapMode, pPrevAddr, PrevAddr, CurrentAddr, RegFile);
           
           
          if Mode = ERROR_MODE then -- This "if" is for a spec. bug fix!
            Mode := EXECUTE_MODE;
            PendingERROR := TRUE;
          elsif PendingERROR then
            PendingERROR := FALSE;
            Mode := ERROR_MODE;
            TrigERRcountdown := 2;
          end if;
          
                      
          if TrapVector(DETECTED_TRAP) then
          
            if tt = TrapTypeTable(FP_EXCEPTION) then
              TriggerFXACK := TRUE;
            end if;
            
            TriggerFLUSH := TRUE;

----------------------------------------------------------------------
-- Moved generation of INTACK one clock forward (bug in spec!)
-- Jiri Gaisler, 30-01-96

            TriggerINTACK := TRUE; -- Flag for INTACK signal
----------------------------------------------------------------------
            
            -- Reset trap vector to FALSE: the trap has been serviced in
            -- TrapHandler.
            TrapVector := (others => FALSE);
            
            pTOpSizeVar := pSizeVar;
            pSizeVar := SizeVar;
            pTOpASIvar := pASIvar;
            pASIvar := ASIvar;
            
            -- Disable next checksum comparison for PFC.
            ChecksumCompare := FALSE;

          else
          --......... Actions related to Instruction Fetch cycle ............
            -- '''''''' conditional fetch. ''''''''''
            nIDisValid := FALSE;
            
            if not(ID.Annul) then
            
              if INSTvar = '1' then
              
                nID := Transcribe(D); -- Fetches the bus only if it is an 
                                      -- instr.
                nID.Address := PrevAddr; -- Address of the instr. in nID.
                  -- NextAddress of instr. in nID will be loaded later in the
                  -- pipeline. See parts "Nominal increment of address" and
                  -- "pipeline progression".
                nIDisValid := TRUE;             -- following the one that is
                                                -- in nID.
              end if;
              
            else   -- No fetching if delay slot inst. is annulled. Note:  
                   -- in this case, ID.Mnemo MUST BE a branching instruction.
              nID.Mnemo := ANNULLED;
              nID.Annul := FALSE;
              nID.Address := PrevAddr;
              -- here dequeue from InstBuf. the delay slot instruction if any.
              if Buf1IsValid then
                GetFromBufferQueue(Buf1IsValid, InstBuffer1,
                                   Buf2IsValid, InstBuffer2,
                                   nIDTemp); -- nIDTemp is annulled in this 
                                             -- case: nothing is done with it.
                if Buf2IsValid then -- for debugging purposes
                  assert FALSE report "(IURTGen architecture): error in VHDL " &
                                      "model; the FIFO should be EMPTY!!!"
                               severity error;
                end if;
              end if;
            end if;
            
            --'''''' IOP scheduling. WARNING: DELICATE mechanism here!!!'''''''
            IOPscheduling(InstBuffer1, nID, ID, EX, WR,
                          IOPcase); -- IOPcase flag is used in next IF.
                        
            if IOPcase then
              if nIDisValid then
                PutInBufferQueue(nID, Buf1IsValid, InstBuffer1,
                                      Buf2IsValid, InstBuffer2);
              end if;
              nID.Mnemo   := IOP;
              nID.Address := ID.Address; -- This is used for of memory 
                                         -- exception: MEXC_N (not clear).
            else
              if Buf1IsValid then
                GetFromBufferQueue(Buf1IsValid, InstBuffer1,
                                   Buf2IsValid, InstBuffer2,
                                   nIDTemp);
                if nIDisValid then
                  PutInBufferQueue(nID, Buf1IsValid, InstBuffer1,
                                        Buf2IsValid, InstBuffer2);
                end if;
                nID := nIDTemp;
              end if;
            end if;
        
            -- '''''''' Assignment of field NextAddress of ID + ''''''''''
            -- '''''''' Nominal increment of CurrentAddr ''''''''''
            -- '''''''' + ASI assignment ''''''''''
            
                    -- Address of inst. following the inst. that is in EX 
                    -- in next cycle: NextAddressForTraps is used in part
                    -- pipeline progression for EX.         
            NextAddressForTraps := PrevAddr; 
                                        
            pPrevAddr := PrevAddr;
            PrevAddr := CurrentAddr;
            CurrentAddr := CurrentAddr + 4;
            pTOpSizeVar := pSizeVar;
            pSizeVar := SizeVar; -- SizeVar is modified when data xfer, if any.
            pTOpASIvar := pASIvar;
            pASIvar := ASIvar;
            if (
                 not(IsLoadDoubleInst(EX.Mnemo)) and
                 not(IsStoreInst(EX.Mnemo)) and 
                 not(IsStoreDoubleInst(WR.Mnemo)) and
                 EX.Mnemo /= LDSTUB and EX.Mnemo /= LDSTUBA and
                 WR.Mnemo /= LDSTUB and WR.Mnemo /= LDSTUBA and
                 EX.Mnemo /= SWAP and EX.Mnemo /= SWAPA and
                 WR.Mnemo /= SWAP and WR.Mnemo /= SWAPA and 
                 EX.Mnemo /= WRPSR and 
                 not((ID.Mnemo = JMPL or EX.Mnemo = JMPL) and 
                     InstBuffer1.Mnemo = RETT) and
                 not(WR.Mnemo = JMPL and ID.Mnemo = RETT) 
               ) then
              if S = '0' then
                ASIvar := USER_INST;
              elsif S = '1' then
                ASIvar := SUPERVISOR_INST;
              else
                ASIvar := 0;
                assert FALSE report "(IURTGen architecture): unknown value " &
                                    "for S bit!"
                             severity warning;
              end if;
            end if;
            
              -- particular case for ASI generation
            if (ID.Mnemo = JMPL and InstBuffer1.Mnemo = RETT) then
              if PS = '1' then
                ASIvar := SUPERVISOR_INST;
              elsif PS = '0' then
                ASIvar :=  USER_INST;
              end if;
            end if;
            
        
            --+++++++++++++++++++++++++++++++++++++++++++ 
            -- Addr. remains unchanged when EX=JMPL and an IOP is scheduled or
            -- when WR=load and ID=IOP i.e LDsingle w/ dep.or
            -- when (WR1.Mnemo=LDdouble) i.e LDdouble w/ dep. or
            -- when EX=CALL and ID=IOP i.e CALL w/ dep.or
            -- when EX=storeInst or
            -- when EX=loadstoreORswap inst. or
            -- when WR=loadstoreORswap inst.
            if ( 
                 (EX.Mnemo = JMPL and IOPcase) or
                 (IsLoadSingleInst(WR.Mnemo) and ID.Mnemo = IOP) or
                 (IsLoadDoubleInst(WR1.Mnemo) and ID.Mnemo = IOP) or
                 (EX.Mnemo = CALL and ID.Mnemo = IOP) or
                 IsStoreInst(EX.Mnemo) or
                 (EX.Mnemo = LDSTUB or EX.Mnemo = LDSTUBA) or
                 (EX.Mnemo = SWAP or EX.Mnemo = SWAPA) or
                 (WR.Mnemo = LDSTUB or WR.Mnemo = LDSTUBA) or
                 (WR.Mnemo = SWAP or WR.Mnemo = SWAPA)
               ) then
              CurrentAddr := PrevAddr;
            end if;
            --+++++++++++++++++++++++++++++++++++++++++++ 
                         
            -- ''''''' OPcc "anticipated" execution. '''''''''
            if IsOPcc(ID.Mnemo) then
              ExecuteOPcc(ID, RegFile, CWP, icc, Y, ResultOPcc, iccTemp, YTemp);
            end if;

            SA1CurrentAddr := CurrentAddr;
            
            -- ''''''''' Bicc, FBicc "anticipated" execution. '''''''''''
            -- ''''''''' CBicc are not implemented. ''''''''''''
            if IsBicc(nID.Mnemo) then
              ExecuteBicc(iccTemp, icc, nID, CurrentAddr, TakenBr);
            end if;
            
            if (IsFBfcc(nID.Mnemo) and FP_N = '0' and EF = '1' and 
                not(IsFCMP(EX.Mnemo)) ) then
                -- if trap condition (FP_N=1 or EF=0), does nothing here.
              ExecuteFBfcc(FCC, nID, CurrentAddr, TakenBr); 
            end if;
          
            -- '''''''''' Instruction CALL "anticipated" execution. '''''''''''
            -- '''''''''' Address calculation. '''''''''''''
            if nID.Mnemo = CALL then
              CurrentAddr := nID.Address + (nID.disp30 & "00");
            end if; -- DO NOT FORGET the paranthesis in the line above!!!
                      
          --......... Actions related to Instruction Decode cycle ...........
            -- '''''''' Load case: data address generation. '''''''
            if IsLoadInst(ID.Mnemo) then
              SaveCurrentAddr := CurrentAddr; -- Save current address.
              Preserve_SavePrevAddr := SavePrevAddr;
              SavePrevAddr := PrevAddr;       -- Save previous address.
              
              if S = '0' then
                ASIvar := USER_DATA;
              else
                ASIvar := SUPERVISOR_DATA;
              end if;
              if IsLoadInstASI(ID.Mnemo) then 
                ASIvar := ID.asi;
              end if;
              
              if IsLoadByteInst(ID.Mnemo) then
                SizeVar := BYTE;
              elsif IsLoadHalfwordInst(ID.Mnemo) then
                SizeVar := HALFWORD;
              elsif IsLoadDoubleInst(ID.Mnemo) then
                SizeVar := DOUBLEWORD;
              else
                SizeVar := WORDTYPE;
              end if;
              
              CurrentAddr := LoadORStoreORSwapAddrCalc(ID, CWP, RegFile);
              LoadDataAddr := CurrentAddr;
              LDdataAD1 := CurrentAddr;
            end if;
            
            if IsLoadDoubleInst(EX.Mnemo) then
              LoadDataAddr := LoadDataAddr + 4;
            end if;
            
            if (IsLoadSingleInst(EX.Mnemo) or 
                (IsLoadDoubleInst(WR.Mnemo) and EX.Mnemo /= ANNULLED)
               ) then
              if ( not((IsBicc(nID.Mnemo) or IsFBfcc(nID.Mnemo)) and TakenBr) 
                  and nID.Mnemo /= CALL ) then
                CurrentAddr := SaveCurrentAddr; -- Restore values of instruction
                                                -- address unless nID=Bicc or
                                                -- nID=FBfcc or nID=CALL
              end if;
              
              if S = '0' then
                ASIvar := USER_INST;
              else
                ASIvar := SUPERVISOR_INST;
              end if;
              
              SizeVar := WORDTYPE;
            end if;
            
            -- '''''''' Store case: data address generation. '''''''
            if IsStoreInst(ID.Mnemo) then
              SaveCurrentAddr := CurrentAddr;
              Preserve_SavePrevAddr := SavePrevAddr;
              SavePrevAddr := PrevAddr;
              
              if S = '0' then
                ASIvar := USER_DATA;
              else
                ASIvar := SUPERVISOR_DATA;
              end if;
              if IsStoreInstASI(ID.Mnemo) then
                ASIvar := ID.asi;
              end if;
              
              CurrentAddr := LoadORStoreORSwapAddrCalc(ID, CWP, RegFile);
	    end if;
              
            if IsStoreInst(ID.Mnemo) then
              if (ID.Mnemo = STB or EX.Mnemo = STBA) then
                SizeVar := BYTE;
              elsif (ID.Mnemo = STH or EX.Mnemo = STHA) then
                SizeVar := HALFWORD;
              elsif IsStoreDoubleInst(ID.Mnemo) then
                SizeVar := DOUBLEWORD;
              else
                SizeVar := WORDTYPE;
	      end if;
	    end if;


            if IsStoreInst(EX.Mnemo) then
              -- Size assignment + StoreData value assignment
              DestReg := GetIndex(EX.rd, CWP);
              if (EX.Mnemo = STB or EX.Mnemo = STBA) then
                StoreData(7 downto 0)   := RegFile(DestReg)(7 downto 0);
                StoreData(15 downto 8)  := RegFile(DestReg)(7 downto 0);
                StoreData(23 downto 16) := RegFile(DestReg)(7 downto 0);
                StoreData(31 downto 24) := RegFile(DestReg)(7 downto 0);
              elsif (EX.Mnemo = STH or EX.Mnemo = STHA) then
                StoreData(15 downto 0)  := RegFile(DestReg)(15 downto 0);
                StoreData(31 downto 16) := RegFile(DestReg)(15 downto 0);
              elsif IsStoreDoubleInst(EX.Mnemo) then
                if (EX.Mnemo = I_STD or EX.Mnemo = STDA) then
                  StoreData := RegFile((DestReg/2)*2);
                                       -- even-numbered register: 1st data.
                end if;
              else
                if not(IsStoreFP_CPInst(Ex.Mnemo)) then
                  StoreData := RegFile(DestReg);
                end if;
              end if;
            end if;
            
            if (WR.Mnemo = I_STD or WR.Mnemo = STDA) then
              DestReg := GetIndex(WR.rd, CWP);
              StoreData := RegFile((DestReg/2)*2 + 1); 
                                    -- odd-numbered register: 2nd data.
            end if;
                        
            if ((IsStoreSingleInst(WR.Mnemo) and EX.Mnemo /= ANNULLED) or 
                IsStoreDoubleInst(WR1.Mnemo)) then
              if ( not((IsBicc(nID.Mnemo) or IsFBfcc(nID.Mnemo)) and TakenBr) 
                  and nID.Mnemo /= CALL) then
                CurrentAddr := SaveCurrentAddr; -- Restore values of instruction
                                                -- addresses unless nID=Bicc or
                                                -- nID=FBfcc or nID=CALL
              end if;
              
              if S = '0' then -- restore value of the ASI bus
                ASIvar := USER_INST;
              else
                ASIvar := SUPERVISOR_INST;
              end if;
              
              SizeVar := WORDTYPE;
            end if;
            
            -- ''''''''  LDSTUB & SWAP. '''''''''
            if (ID.Mnemo = LDSTUB or ID.Mnemo = LDSTUBA or ID.Mnemo = SWAP or
                ID.Mnemo = SWAPA) then
              SaveCurrentAddr := CurrentAddr;
              Preserve_SavePrevAddr := SavePrevAddr;
              SavePrevAddr := PrevAddr;
              
              if S = '0' then
                ASIvar := USER_DATA;
              else
                ASIvar := SUPERVISOR_DATA;
              end if;
              if (ID.Mnemo = LDSTUBA or ID.Mnemo = SWAPA) then 
                ASIvar := ID.asi;
              end if;
              
              if (ID.Mnemo = LDSTUBA or ID.Mnemo = LDSTUB) then
                SizeVar := BYTE;
              else
                SizeVar := WORDTYPE;
              end if;
              
              CurrentAddr := LoadORStoreORSwapAddrCalc(ID, CWP, RegFile);
              LoadDataAddr := CurrentAddr;
            end if;
            
            if (EX.Mnemo = LDSTUB or ID.Mnemo = LDSTUBA) then
              StoreData  := (others => '1');
            elsif (WR.Mnemo = SWAP or WR.Mnemo = SWAPA) then
              StoreData := SwapData;
            end if;
            
            if (WR1.Mnemo = LDSTUB or WR1.Mnemo = LDSTUBA or WR1.Mnemo = SWAP or
                WR1.Mnemo = SWAPA) then
              if ( not((IsBicc(nID.Mnemo) or IsFBfcc(nID.Mnemo)) and TakenBr) 
                  and nID.Mnemo /= CALL) then
                CurrentAddr := SaveCurrentAddr; -- Restore values of instruction
                                                -- addresses unless nID=Bicc or
                                                -- nID=FBfcc or nID=CALL
              end if;
              
              if S = '0' then -- restore value of the ASI bus
                ASIvar := USER_INST;
              else
                ASIvar := SUPERVISOR_INST;
              end if;
              
              SizeVar := WORDTYPE;
            end if;
            
            -- ''''''''  JMPL & RETT: branching address calculation. '''''''''
            if ( ID.Mnemo = JMPL or ID.Mnemo = RETT ) then
              JmplRettAddrCalc(ID, CWP, RegFile, CurrentAddr);
            end if;
                        
          end if;
          
          --..... Assignment of variables corresponding to signals .....
          --..... triggered on the rising edge of the clock CLK.   .....
          
          -- Signal ASI --> ASIvar
          if TrapMode /= NOTRAP then 
            ASIvar := SUPERVISOR_INST;
          end if;
          
          -- Signal Size --> SizeVar
          if TrapMode /= NOTRAP then
            SizeVar := WORDTYPE;
          end if;
          
          -- Signal INTACK --> INTACKvar
          pINTACKvar := INTACKvar;
          if ( -- conditions for resetting

-- Fixed to reset one clock earlier, Jiri Gaisler 30-01-96

               ID.Mnemo = ANNULLED and EX.Mnemo = ANNULLED
             ) then
            INTACKvar := '0';
          end if;
          if ( -- conditions for setting
               TriggerINTACK
             ) then
            INTACKvar := '1';
            TriggerINTACK := FALSE;
          end if;
          
          
          -- Signal RD --> RDvar
          pTOpRDvar := pRDvar;
          pRDvar := RDvar;
          if TrapMode /= NOTRAP then
            RDvar := '1';
          else
            if ( -- conditions for setting
                 IsStoreSingleInst(WR.Mnemo) or
                 IsStoreDoubleInst(WR1.Mnemo) or
                 (WR1.Mnemo = LDSTUB or WR1.Mnemo = LDSTUBA) or
                 (WR1.Mnemo = SWAP or WR1.Mnemo = SWAPA)
               ) then
              RDvar := '1';
            end if;
            if ( -- conditions for resetting
                 IsStoreInst(ID.Mnemo) or
                 (EX.Mnemo = LDSTUB or EX.Mnemo = LDSTUBA) or
                 (EX.Mnemo = SWAP or EX.Mnemo = SWAPA)
               ) then
              RDvar := '0';
            end if;
          end if;
          
          -- Signal WE_N --> WE_Nvar
          pTOpWE_Nvar := pWE_Nvar;
          pWE_Nvar := WE_Nvar;
          if TrapMode /= NOTRAP then
            WE_Nvar := '1';
          else
            if ( -- conditions for setting
                 IsStoreSingleInst(WR.Mnemo) or
                 IsStoreDoubleInst(WR1.Mnemo) or
                 (WR1.Mnemo = LDSTUB or WR1.Mnemo = LDSTUBA) or
                 (WR1.Mnemo = SWAP or WR1.Mnemo = SWAPA)
               ) then
              WE_Nvar := '1';
            end if;
            if ( -- conditions for resetting
                 IsStoreInst(EX.Mnemo) or
                 (WR.Mnemo = LDSTUB or WR.Mnemo = LDSTUBA) or
                 (WR.Mnemo = SWAP or WR.Mnemo = SWAPA)
               ) then
              WE_Nvar := '0';
            end if;
          end if;
          
          -- Signal WRT --> WRTvar
          pTOpWRTvar := pWRTvar;
          pWRTvar := WRTvar;
          if TrapMode /= NOTRAP then
            WRTvar := '0';
          else
            if ( -- conditions for resetting
                 IsStoreInst(EX.Mnemo) or
                 (WR.Mnemo = LDSTUB or WR.Mnemo = LDSTUBA) or
                 (WR.Mnemo = SWAP or WR.Mnemo = SWAPA)
               ) then
              WRTvar := '0';
            end if;
            if ( -- conditions for setting
                 IsStoreInst(ID.Mnemo) or
                 (EX.Mnemo = LDSTUB or EX.Mnemo = LDSTUBA) or
                 (EX.Mnemo = SWAP or EX.Mnemo = SWAPA)
               ) then
              WRTvar := '1';
            end if;
          end if;
          
          -- Signal DXFER --> DXFERvar
          pTOpDXFERvar := pDXFERvar;
          pDXFERvar := DXFERvar;
          if TrapMode /= NOTRAP then
            DXFERvar := '0';
          else
            if ( -- conditions for resetting
                 IsLoadSingleInst(EX.Mnemo) or
                 IsLoadDoubleInst(WR.Mnemo) or
                 IsStoreSingleInst(WR.Mnemo) or
                 IsStoreDoubleInst(WR1.Mnemo) or
                 (WR1.Mnemo = LDSTUB or WR1.Mnemo = LDSTUBA) or
                 (WR1.Mnemo = SWAP or WR1.Mnemo = SWAPA)
               ) then
              DXFERvar := '0';
            end if;
            if ( -- conditions for setting overriding resetting
                 IsLoadInst(ID.Mnemo) or
                 IsStoreInst(ID.Mnemo) or
                 (ID.Mnemo = LDSTUB or ID.Mnemo = LDSTUBA) or
                 (ID.Mnemo = SWAP or ID.Mnemo = SWAPA)
               ) then
              DXFERvar := '1';
            end if;
          end if;
          
          -- Signal LDSTO --> LDSTOvar
          pTOpLDSTOvar := pLDSTOvar;
          pLDSTOvar := LDSTOvar;
          if TrapMode /= NOTRAP then 
            LDSTOvar := '0';
          else
            if ( -- conditions for resetting
                 (WR1.Mnemo = LDSTUB or WR1.Mnemo = LDSTUBA) or
                 (WR1.Mnemo = SWAP or WR1.Mnemo = SWAPA)
               ) then
              LDSTOvar := '0';
            end if;
            if ( -- conditions for setting overriding resetting
                 (ID.Mnemo = LDSTUB or ID.Mnemo = LDSTUBA) or
                 (ID.Mnemo = SWAP or ID.Mnemo = SWAPA)
               ) then
              LDSTOvar := '1';
            end if;
          end if;
          
          -- Signal INULL --> INULLvar
          pINULLvar := INULLvar;
          if TrapMode /= NOTRAP then 
            INULLvar := '1';
          else
            if ( -- conditions for resetting
                 EX.Mnemo = RETT or
                 (EX.Mnemo = JMPL and not(IOPcase)) or
                 (WR.Mnemo = JMPL and ID.Mnemo = IOP) or
                 (ID.Mnemo = ANNULLED and EX.Mnemo = ANNULLED) or
                 (WR.Mnemo = CALL and EX.Mnemo = IOP) or
                 (IsLoadSingleInst(WR1.Mnemo) and EX.Mnemo = IOP) or
                 (IsLoadDoubleInst(WR2.Mnemo) and EX.Mnemo = IOP) or
                 IsStoreInst(WR1.Mnemo) or
                 (WR2.Mnemo = LDSTUB or WR2.Mnemo = LDSTUBA) or
                 (WR2.Mnemo = SWAP or WR2.Mnemo = SWAPA)
               ) then
              INULLvar := '0';
            end if;
            if  ( -- conditions setting overriding resetting
                  ID.Mnemo = JMPL or
                  ID.Mnemo = RETT or
                  (EX.Mnemo = CALL and ID.Mnemo = IOP) or
                  (IsLoadSingleInst(WR.Mnemo) and ID.Mnemo = IOP and 
		   not isFPInst(WR.Mnemo) and not IsFPInst(ID.Mnemo)) or

--                  (IsLoadSingleInst(WR1.Mnemo) and 
--		((WR1.rd = EX.rs1) or ((WR1.rd = EX.rs2) and (EX.i = 0)))) or

                  (IsLoadDoubleInst(WR1.Mnemo) and ID.Mnemo = IOP and
                   not isFPInst(WR1.Mnemo) and not IsFPInst(ID.Mnemo)) or 
                  (IsStoreInst(WR.Mnemo) and EX.Mnemo /= ANNULLED) or
--		   not isFPInst(WR.Mnemo) and not IsFPInst(EX.Mnemo)) or
                  (WR1.Mnemo = LDSTUB or WR1.Mnemo = LDSTUBA) or
                  (WR1.Mnemo = SWAP or WR1.Mnemo = SWAPA)
                ) then
               INULLvar := '1';
            end if;
          end if;
            
          
          -- Signal LOCK --> LOCKvar
          pTOpLOCKvar := pLOCKvar;
          pLOCKvar := LOCKvar;
          if TrapMode /= NOTRAP then 
            LOCKvar := '0';
          else
            if ( -- conditions for resetting
                 IsLoadDoubleInst(EX.Mnemo) or
                 IsStoreSingleInst(EX.Mnemo) or
                 IsStoreDoubleInst(WR.Mnemo) or
                 (WR.Mnemo = LDSTUB or WR.Mnemo = LDSTUBA) or
                 (WR.Mnemo = SWAP or WR.Mnemo = SWAPA)
               ) then
              LOCKvar := '0';
            end if;
            if ( -- conditions for setting overriding resetting.
                 IsLoadDoubleInst(ID.Mnemo) or
                 IsStoreInst(ID.Mnemo) or
                 (ID.Mnemo = LDSTUB or ID.Mnemo = LDSTUBA) or
                 (ID.Mnemo = SWAP or ID.Mnemo = SWAPA)
               ) then
              LOCKvar := '1';
            end if;
          end if;
          
          -- Signal INST --> INSTvar
          pINSTvar := INSTvar;
          if TrapMode /= NOTRAP then 
            INSTvar := '0';
          else 
            if ( -- conditions for setting 
                 EX.Mnemo = RETT or
                 (EX.Mnemo = JMPL and not(IOPcase)) or
                 (WR.Mnemo = JMPL and ID.Mnemo = IOP) or
                 (ID.Mnemo = ANNULLED and EX.Mnemo = ANNULLED) or
                 (WR.Mnemo = CALL and EX.Mnemo = IOP) or
                 (IsLoadSingleInst(WR.Mnemo) and ID.Mnemo /= IOP) or
                 (IsLoadSingleInst(WR1.Mnemo) and EX.Mnemo = IOP) or
                 (IsLoadDoubleInst(WR1.Mnemo) and ID.Mnemo /= IOP) or
                 (IsLoadDoubleInst(WR2.Mnemo) and EX.Mnemo = IOP) or
                 IsStoreSingleInst(WR1.Mnemo) or
                 IsStoreDoubleInst(WR2.Mnemo) or
                 (WR2.Mnemo = LDSTUB or WR2.Mnemo = LDSTUBA) or
                 (WR2.Mnemo = SWAP or WR2.Mnemo = SWAPA) 
               ) then
              INSTvar := '1';
            end if;
            if ( -- conditions for resetting overriding setting.
                 ID.Mnemo = JMPL or
                 ID.Mnemo = RETT or
                 IsLoadInst(EX.Mnemo) or
                 (EX.Mnemo = CALL and ID.Mnemo = IOP) or
                 IsStoreInst(EX.Mnemo) or
                 (EX.Mnemo = LDSTUB or EX.Mnemo = LDSTUBA) or
                 (EX.Mnemo = SWAP or EX.Mnemo = SWAPA)
               ) then
              INSTvar := '0';
            end if;  
          end if;
          
          -- Signal FLUSH --> FLUSHvar & Signal FXACK --> FXACKvar
          pFLUSHvar := FLUSHvar;
          pFXACKvar := FXACKvar;
          if ( -- condition for setting.
               TriggerFLUSH
             ) then
            FLUSHvar := '1';
            TriggerFLUSH := FALSE;
          end if;
          
          if ( -- condition for setting.
               TriggerFXACK
             ) then
            FXACKvar := '1';
            TriggerFXACK := FALSE;
          end if;
          
          if ( -- condition for resetting.
               ID.Mnemo = ANNULLED and EX.Mnemo = ANNULLED 
             ) then
            FXACKvar := '0';
            FLUSHvar := '0';
          end if;
           
          
          -- Signal FINS1 --> FINS1var
          pFINS1var := FINS1var;
          FINS1var := '0';
          if (TrapMode = NOTRAP and IsFPinst(nID.Mnemo) and 
              not(IsFBfcc(nID.Mnemo)) and 
              (ID.Mnemo /= IOP or EX.Mnemo = JMPL or EX.Mnemo = RETT)) then
            FINS1var := '1';
          end if;
          
          -- Signal FINS2 --> FINS2var
          pFINS2var := FINS2var;
          FINS2var := '0';
          if (TrapMode = NOTRAP and IsFPinst(nID.Mnemo) and
              not(IsFBfcc(nID.Mnemo)) and 
              ID.Mnemo = IOP and EX.Mnemo /= JMPL and
              EX.Mnemo /= RETT) then
            FINS2var := '1';
          end if;
          
          -- Signal HWERROR_N --> HWERROR_Nvar
          pHWERROR_Nvar := HWERROR_Nvar;
          HWERROR_Nvar := '1';
          if ParBitViolCount = 0 then
            HWERROR_Nvar := '0';
          end if;
                    
          --........ Pipeline progression. ..........
          if TrapMode = NOTRAP then
            WR <= EX;
            EX <= ID;
            ID <= nID;
            
            -- loading of field NextAddress for traps.
            if ( (not(IsBicc(ID.Mnemo)) and 
                 not(IsFBfcc(ID.Mnemo) and FP_N = '0' and EF = '1') and
                 ID.Mnemo /= IOP and ID.Mnemo /= NOTHING and
                 ID.Mnemo /= XXX) or
                 (ID.Mnemo = ANNULLED and EX.Mnemo /= ANNULLED)
               ) then
              EX.NextAddress <= NextAddressForTraps;
            end if;
            if ID.Mnemo = IOP then
              EX.NextAddress <= EX.NextAddress;
            elsif ( IsLoadInst(WR.Mnemo) or 
                    (IsLoadInst(WR1.Mnemo) and EX.Mnemo = IOP) or
                    IsLoadDoubleInst(WR1.Mnemo) or
                    (IsLoadDoubleInst(WR2.Mnemo) and EX.Mnemo = IOP) or
                    IsStoreInst(WR1.Mnemo) or IsStoreDoubleInst(WR2.Mnemo) or
                    WR2.Mnemo = LDSTUB or WR2.Mnemo = LDSTUBA or 
                    WR2.Mnemo = SWAP or WR2.Mnemo = SWAPA
                  ) then
              if IsLoadInst(ID.Mnemo) or IsStoreInst(ID.Mnemo) or
                 (ID.Mnemo = LDSTUB or ID.Mnemo = LDSTUBA or ID.Mnemo = SWAP or
                  ID.Mnemo = SWAPA) then
                EX.NextAddress <= Preserve_SavePrevAddr;
              else
                EX.NextAddress <= SavePrevAddr;
              end if;
            elsif (WR.Mnemo = CALL and EX.Mnemo = IOP) then
              EX.NextAddress <= WR.Address + (WR.disp30 & "00");
            end if;
            
            -- Progression of fictitious pipeline stages.
            WR3 := WR2;
            WR2 := WR1;
            if (ID.Mnemo = ANNULLED and EX.Mnemo = ANNULLED) then
              WR1.Mnemo := ANNULLED;   -- Particuliar case: progression of the 
                                       -- pipeline when asynchronous trap.
              WR1.Annul := FALSE;
            else
              WR1 := WR;
            end if;
            
            -- Avoid progression in pipeline of instr. in WR stage in case
            -- of a trap.
            if (EX.Mnemo = ANNULLED and WR.Mnemo = ANNULLED) then
              WR1.Mnemo := ANNULLED;
              WR1.Annul := FALSE;
            end if;
          elsif TrapMode = SYNCH_TRAP then
            ID.Mnemo  <= ANNULLED;
            ID.Annul  <= FALSE;
            EX.Mnemo  <= ANNULLED;
            EX.Annul  <= FALSE;
            WR.Mnemo  <= ANNULLED;
            WR.Annul  <= FALSE;
            WR1.Mnemo := ANNULLED;
            WR1.annul := FALSE;
            WR2.Mnemo := ANNULLED;
            WR2.annul := FALSE;
            InstBuffer1.Mnemo := NOTHING; -- null cycle.
            InstBuffer1.Annul := FALSE;
            Buf1IsValid := TRUE;
            Buf2IsValid := FALSE;
            TrapMode := NOTRAP; 
          else -- TrapMode = ASYNCH_TRAP
            WR <= EX;
            ID.Mnemo  <= ANNULLED;
            ID.Annul  <= FALSE;
            EX.Mnemo  <= ANNULLED;
            EX.Annul  <= FALSE;
            WR1.Mnemo := ANNULLED;
            WR1.Annul := FALSE;
            WR2.Mnemo := ANNULLED;
            WR2.Annul := FALSE;
            
            InstBuffer1.Mnemo := NOTHING; -- null cycle.
            InstBuffer1.Annul := FALSE;
            Buf1IsValid := TRUE;
            Buf2IsValid := FALSE;
            TrapMode := NOTRAP;
--            
--            TriggerINTACK := TRUE; -- Flag for INTACK signal
-- INTACK generation moved! Jiri Gaisler, 31-01-96

          end if;
                    
        end if;     
      
      --^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      
      when others => NULL;
      
    end case; -- Mode


    
    -- **************** INTERFACE modeling **********************
    

    --''''''' Input signal sampling: XHOLD_N, FCCV, MDS_N, MEXC_N, '''''
    --''''''' HALT_N + actions to take in certain conditions.      '''''
    if falling_edge(CLK) then
      if ( MHOLDA_N = '0' or
           MHOLDB_N = '0' or
           BHOLD_N = '0' or
           FHOLD_N = '0' or
           FCCV = '0' or
           CHOLD_N = '0' or
           CCCV = '0' or
           HALT_N = '0' ) then
        FrozenPipe := TRUE;
      else
        FrozenPipe := FALSE;
      end if;
      
      MDS_Nvar   := MDS_N;
      FCCVvar    := FCCV;
      FHOLD_Nvar := FHOLD_N;
      
      -- mask the DPAR violation in the case of MHOLD_N
      if N601MODE_N = '1' and (MHOLDA_N = '0' or MHOLDB_N = '0') and   
         ParBitViolCount /= -1 and DPARviol then
        DPARviol := FALSE;
        if not(FIPARviol) then
          ParBitViolCount := -1;
        end if;
      end if;
      
    end if;
       
    -- Sampling of IRL bus for interrupts
    if (rising_edge(CLK) and not(FrozenPipe) and Mode = EXECUTE_MODE) then
      if VecUnknown(IRL) then
        IRLvar := 0; -- amounts to ignoring this interrupt request.
      else
        pIRLvar := IRLvar;
        IRLvar := ToNatural(IRL);
      end if;
    end if;
    
    -- Sample FEXC_N
    if rising_edge(CLK) then
      FEXC_Nvar := FEXC_N;
    end if;
    
    -- Reset pin sampling
    if rising_edge(CLK) and Reset_N = '0' then
      if not(ResetHasBeenDetected) then
        ResetHasBeenDetected := TRUE;
        FrozenBusses := FALSE;
        LocalCounterDuringReset := 0;
      end if;
      
      Mode := RESET_MODE;
      
      ChecksumCompare := TRUE;
      Checksum := (others => '0');
      
      TrapVector := (others => FALSE);
      TrapVector(DETECTED_TRAP) := TRUE;
      TrapVector(RESET_TRAP) := TRUE;
                  -- if Reset_N pin is not at '0' at
                  -- start-up, IU state is undefined.
                  
      StartUp := FALSE; 
      
      CurrentAddr := (others => '0');
      
      Din    <= (others => 'Z');
      DPARin <= 'Z';
      
      CheckTimBidir <= FALSE; -- destined for the conditional timing checkers
      
      ID.Mnemo  <= XXX;
      EX.Mnemo  <= XXX;
      WR.Mnemo  <= XXX;
      WR1.Mnemo := XXX;
      WR2.Mnemo := XXX;
      
      pSizeVar:= SizeVar;
      SizeVar := WORDTYPE;
      pINTACKvar := INTACKvar;
      INTACKvar  := '0'; 
      pRDvar     := RDvar;
      RDvar      := '1';
      pWE_Nvar   := WE_Nvar;
      WE_Nvar    := '1';
      pWRTvar    := WRTvar;
      WRTvar     := '0';
      pDXFERvar  := DXFERvar;
      DXFERvar   := '0';
      pLDSTOvar  := LDSTOvar;
      LDSTOvar   := '0';
      pINULLvar  := INULLvar;
      INULLvar   := '1';
      pLOCKvar   := LOCKvar;
      LOCKvar    := '0';
      pINSTvar   := INSTvar;
      INSTvar    := '1';
      pFLUSHvar  := FLUSHvar;
      FLUSHvar   := '0';
      pFXACKvar  := FXACKvar;
      FXACKvar   := '0';
      pFINS1var  := FINS1var;
      FINS1var   := '0';
      pFINS2var  := FINS2var;
      FINS2var   := '0';
      pHWERROR_Nvar := HWERROR_Nvar;
      HWERROR_Nvar  := '1';
      
      if LocalCounterDuringReset = 2 then -- Specific behavior of ASI and 
                                          -- ERROR_N pins during RESET_N = 0.
        pASIvar := ASIvar;
        ASIvar  := SUPERVISOR_INST;
        GenerateASIsig := TRUE;
        LocalCounterDuringReset := LocalCounterDuringReset + 1;
      elsif LocalCounterDuringReset = 3 then
        ERROR_Nvar := '1';
        GenerateERRORsig := TRUE;
        LocalCounterDuringReset := -1;
      elsif LocalCounterDuringReset >= 0 then
        LocalCounterDuringReset := LocalCounterDuringReset + 1;
      end if;
    end if;
    
    -- Memory exception sampling and handling
    if (rising_edge(CLK) and not(FrozenPipe)) then
      if PendingInstAccExc = 0 and not(ID.Mnemo = IOP) then
        TrapVector(DETECTED_TRAP) := TRUE;
        TrapVector(INST_ACCESS) := TRUE;
        PendingInstAccExc := PendingInstAccExc - 1;
      elsif PendingInstAccExc >= -1 and not(ID.Mnemo = IOP) then
        PendingInstAccExc := PendingInstAccExc - 1;
      end if;
    end if;
    
    if (rising_edge(CLK) and (MDS_Nvar = '0' or not(FrozenPipe))) then
      MEXC_Nvar := MEXC_N;
      if (MEXC_Nvar = '0' and 
          not(ID.Mnemo = ANNULLED and EX.Mnemo = ANNULLED) 
         ) then
        if (IsLoadInst(WR1.Mnemo) or IsStoreInst(WR1.Mnemo) or

-- trap detection during std is made sensitive to wr2.mnemo
-- J.Gaisler, 04-03-96
--          IsLoadDoubleInst(WR2.Mnemo) or IsStoreDoubleInst(WR3.Mnemo) or

            IsLoadDoubleInst(WR2.Mnemo) or IsStoreDoubleInst(WR2.Mnemo) or
            WR1.Mnemo = LDSTUB or WR1.Mnemo = LDSTUBA or
            WR1.Mnemo = SWAP or WR1.Mnemo = SWAPA
           ) then
          TrapVector(DETECTED_TRAP) := TRUE;
          TrapVector(DATA_ACCESS_EXCEPTION) := TRUE;
        elsif PendingInstAccExc = -2 then
          if (IsLoadInst(WR.Mnemo) or IsStoreInst(WR.Mnemo) or
              WR.Mnemo = LDSTUB or WR.Mnemo = LDSTUBA or WR.Mnemo = SWAP or
              WR.Mnemo = SWAPA) then
            PendingInstAccExc := 1;
          elsif EX.Annul then -- annulling branch: inst. acc. exc. annulled
            PendingInstAccExc := -2;
          else
            PendingInstAccExc := 0;
          end if;
        end if;
      end if;
    end if;

    --''''''''' Instruction or data fetch in case of cache miss. '''''''
    if (rising_edge(CLK) and MDS_Nvar = '0' and FrozenPipe and             
        MEXC_N /= '0') then
      if (
           (IsLoadInst(WR1.Mnemo) and not(IsLoadFP_CPInst(WR1.Mnemo))) or
           (WR2.Mnemo = LDD or WR2.Mnemo = LDDA) or
           (WR1.Mnemo = LDSTUB or WR1.Mnemo = LDSTUBA or WR1.Mnemo = SWAP 
            or WR1.Mnemo = SWAPA) 
         ) then -- Data cache miss.
          DataFetchWhenCacheMiss(LDdataAD1, D, CWP, WR1, WR2, RegFile);
      elsif (WR1.Mnemo = LDF or WR1.Mnemo = LDDF or WR1.Mnemo = LDFSR or
             WR2.Mnemo = LDDF) then
        NULL; -- cache miss on data for FPU; so do nothing here.
      else      -- Instruction cache miss.
        if (not(EX.Annul) and
	    not(WR.Mnemo = JMPL) and 
	    not(WR.Mnemo = RETT) and
            not((ID.Mnemo = ANNULLED or ID.Mnemo = NOTHING) and EX.Mnemo = ANNULLED)--gd 
--            not(ID.Mnemo = ANNULLED and EX.Mnemo = ANNULLED) --gd
           ) then
          if (not(Buf1IsValid) and not(Buf2IsValid)) then
            TempINST := Transcribe(D);
            ID <= TempINST;
            ID.Address <= pPrevAddr;
            
            TempINST.Address := pPrevAddr;
            --+++++++++++++++++++++++++++++++++++++++++++

            -- ''''''''' Bicc, FBicc "anticipated" execution. '''''''''''
            -- ''''''''' CBicc are not implemented. ''''''''''''
            if IsBicc(TempINST.Mnemo) then
              ExecuteBicc(iccTemp, icc, TempINST, SA1CurrentAddr, TakenBr);
              CurrentAddr := SA1CurrentAddr;
              ID <= TempINST;
            elsif (IsFBfcc(TempINST.Mnemo) and FP_N = '0' and EF = '1' and
                not(IsFCMP(WR.Mnemo)) ) then
                -- if trap condition (FP_N=1 or EF=0), does nothing here.
              ExecuteFBfcc(FCC, TempINST, SA1CurrentAddr, TakenBr);
              CurrentAddr := SA1CurrentAddr;
              ID <= TempINST;
            end if;

            -- '''''''''' Instruction CALL "anticipated" execution. '''''''''''
            -- '''''''''' Address calculation. '''''''''''''
            if TempINST.Mnemo = CALL then
              CurrentAddr := TempINST.Address + (TempINST.disp30 & "00");
            end if;


          -- Signal FINS1 --> FINS1var
          pFINS1var := FINS1var;
          FINS1var := '0';
          if (TrapMode = NOTRAP and IsFPinst(TempINST.Mnemo) and
              not(IsFBfcc(TempINST.Mnemo)) and
              (EX.Mnemo /= IOP or WR.Mnemo = JMPL or WR.Mnemo = RETT)) then
            FINS1var := '1';
            FINS1in  <= '1' after tFD;
            IFPARin <= 'X' after tIFPH,
                       OddParityOf(FINS1var & FINS2var &
                                   FLUSHvar & FXACKvar & INSTvar) after tIFPD;
          end if;

          -- Signal FINS2 --> FINS2var
          pFINS2var := FINS2var;
          FINS2var := '0';
          if (TrapMode = NOTRAP and IsFPinst(TempINST.Mnemo) and
              not(IsFBfcc(TempINST.Mnemo)) and
              EX.Mnemo = IOP and WR.Mnemo /= JMPL and WR.Mnemo /= RETT) then
            FINS2var := '1';
            FINS2in  <= '1' after tFD;
            IFPARin <= 'X' after tIFPH,
                       OddParityOf(FINS1var & FINS2var &
                                   FLUSHvar & FXACKvar & INSTvar) after tIFPD;

          end if;
            
          elsif (Buf1IsValid and not(Buf2IsValid)) then
            InstBuffer1 := Transcribe(D);
            InstBuffer1.Address := pPrevAddr;
          elsif (Buf1IsValid and Buf2IsValid) then
            InstBuffer2 := Transcribe(D);
            InstBuffer2.Address := pPrevAddr;
          end if;
        end if;
      end if;
    end if;

    --''''''''' DPAR checking when pipeline frozen and MDS_N '''''''
    if (N601MODE_N = '1' and 
        rising_edge(CLK) and MDS_Nvar = '0' and FrozenPipe and             
        MEXC_N /= '0') then
      if DPAR /= OddParityOf(D) and DPAR /= 'Z' and D /= TRI_STATE32 and
         ParBitViolCount = -1 and
         not(WR.Mnemo = LDFSR or WR.Mnemo = LDF or 
             WR.Mnemo = LDDF or WR1.Mnemo = LDDF or 
             IsStoreInst(WR.Mnemo) or IsStoreInst(WR1.Mnemo) or 
             IsStoreDoubleInst(WR2.Mnemo) or 
             WR1.Mnemo = LDSTUB or WR1.Mnemo = LDSTUBA or 
             WR2.Mnemo = LDSTUB or WR2.Mnemo = LDSTUBA or
             WR1.Mnemo = SWAP or WR1.Mnemo = SWAPA or 
             WR2.Mnemo = SWAP or WR2.Mnemo = SWAPA
            ) then
        ParBitViolCount := 2;
        DPARviol := TRUE;
      end if;
    end if;
    
    
    --''''''''' Nominal cases: pipeline not frozen. '''''''
    if (falling_edge(CLK) and 
        not(FrozenBusses) and (not(FrozenPipe) or Mode = RESET_MODE) ) then
      -- Data bus handling
      if ((WE_Nvar = '1') or
          IsStoreFP_CPInst(WR.Mnemo) or
          (IsStoreDoubleInst(WR1.Mnemo) and IsStoreFP_CPInst(WR1.Mnemo))
         ) then
        Din    <= (others => 'Z') after tDOH;
        DPARin <= 'Z' after tDPOH;
      elsif WE_Nvar = '0' then
        if pWE_Nvar = '0' then
          Din    <= (others => 'X') after tDOH, StoreData after tDOD;
          DPARin <= 'X' after tDPOH, OddParityOf(StoreData) after tDPOD;
        elsif pWE_Nvar = '1' then
          Din    <= StoreData after tDOD;
          DPARin <= OddParityOf(StoreData) after tDPOD;
        end if;
      else
        Din    <= (others => 'X'); -- should never get here.
        DPARin <= 'X';
      end if;
      
    end if;
    
    
    if rising_edge(CLK) and GenerateERRORsig then -- Generate ERROR_N during 
      ERROR_Nin <= ERROR_Nvar after tSSD;         -- RESET_N asserted.
      GenerateERRORsig := FALSE;
    end if;
     
    if rising_edge(CLK) and GenerateASIsig then -- Generate ASI during RESET_N.
      Vec8bits := Extend(ToStdLogicVector(ASIvar),ASI'length,'0');
      Vec2bits := Extend(ToStdLogicVector(SizeVar), 2);
      ASIin  <= (others => 'X') after tAH, Vec8bits after tAD;
      ASPARin <= 'X' after tXAPH, 
                 OddParityOf(Vec8bits & Vec2bits) after tXAPD;
      GenerateASIsig := FALSE;
    end if;
     
    if (rising_edge(CLK) and 
        not(FrozenBusses) and (not(FrozenPipe) or Mode = RESET_MODE) ) then
    
      Ain    <= (others => 'X') after tAH, CurrentAddr after tAD;
      APARin <= 'X' after tXAPH, OddParityOf(CurrentAddr) after tXAPD;
      
      if not(StartUp) then
        Vec8bits := Extend(ToStdLogicVector(ASIvar),ASI'length,'0');
        Vec2bits := Extend(ToStdLogicVector(SizeVar), 2);
        SIZEin <= (others => 'X') after tAH, Vec2bits after tAD;
        if not(Mode = RESET_MODE) then
          ASIin  <= (others => 'X') after tAH, Vec8bits after tAD;
          ASPARin <= 'X' after tXAPH, 
                     OddParityOf(Vec8bits & Vec2bits) after tXAPD;
        end if;
      end if;

      if (INTACKvar = '1' and pINTACKvar /= '1') then
        INTACKin <= INTACKvar after tSSD;
      elsif (INTACKvar = '0' and pINTACKvar /= '0') then
        INTACKin <= INTACKvar after tSSH;
      end if;
            
      if (RDvar = '1' and pRDvar /= '1') then
        RDin  <= RDvar after tAH;
      elsif (RDvar = '0' and pRDvar /= '0') then
        RDin  <= RDvar after tAD;
      end if;
      
      if (WE_Nvar = '1' and pWE_Nvar /= '1') then
        WE_Nin <= WE_Nvar after tAH;
      elsif (WE_Nvar = '0' and pWE_Nvar /= '0') then
        WE_Nin <= WE_Nvar after tAD;
      end if;
      
      if (WRTvar = '1' and pWRTvar /= '1') then
        WRTin <= WRTvar after tAD;
      elsif (WRTvar = '0' and pWRTvar /= '0') then
        WRTin <= WRTvar after tAH;
      end if;
      
      if (DXFERvar = '1' and pDXFERvar /= '1') then
        DXFERin <= DXFERvar after tDXD;
      elsif (WRTvar = '0' and pDXFERvar /= '0') then
        DXFERin <= DXFERvar after tDXH;
      end if;
      
      if (LDSTOvar = '1' and pLDSTOvar /= '1') then
        LDSTOin<= LDSTOvar after tAD;
      elsif (LDSTOvar = '0' and pLDSTOvar /= '0') then
        LDSTOin<= LDSTOvar after tAH;
      end if;
      
      if (INULLvar = '1' and pINULLvar /= '1') then
        INULLin <= INULLvar after tNUD;
      elsif (INULLvar = '0' and pINULLvar /= '0') then
        INULLin <= INULLvar after tNUH;
      end if;
      
      if (LOCKvar = '1' and pLOCKvar /= '1') then
        LOCKin <= LOCKvar after tAD;
      elsif (LOCKvar = '0' and pLOCKvar /= '0') then
        LOCKin <= LOCKvar after tAH;
      end if;
      
      if (INSTvar = '1' and pINSTvar /= '1') then
        INSTin <= INSTvar after tSSH;
      elsif (INSTvar = '0' and pINSTvar /= '0') then
        INSTin <= INSTvar after tSSD;
      end if;
      
      if (FLUSHvar = '1' and pFLUSHvar /= '1') then
        FLUSHin <= FLUSHvar after tFLS;
      elsif (FLUSHvar = '0' and pFLUSHvar /= '0') then
        FLUSHin <= FLUSHvar after tFLH;
      end if;
      
      if (FXACKvar = '1' and pFXACKvar /= '1') then
        FXACKin <= FXACKvar after tSSD;
      elsif (FXACKvar = '0' and pFXACKvar /= '0') then
        FXACKin <= FXACKvar after tSSH;
      end if;

      if (FINS1var = '1' and pFINS1var /= '1') then
        FINS1in <= '1' after tFD;
      elsif (FINS1var = '0' and pFINS1var /= '0') then
        FINS1in <= '0' after tFH;
      end if;
      
      if (FINS2var = '1' and pFINS2var /= '1') then
        FINS2in <= '1' after tFD;
      elsif (FINS2var = '0' and pFINS2var /= '0') then
        FINS2in <= '0' after tFH;
      end if;
      
      if HWERROR_Nvar = '1' and pHWERROR_Nvar /= '1' then
        HWERROR_Nin <= '1' after tETH;
      elsif HWERROR_Nvar = '0' and pHWERROR_Nvar /= '0' then
        HWERROR_Nin <= '0' after tETD;
      end if;
      
      IMPARin <= 'X' after tIMPH, 
                 OddParityOf(DXFERvar & LDSTOvar & LOCKvar & 
                             RDvar & WE_Nvar & WRTvar) after tIMPD;
                             
      IFPARin <= 'X' after tIFPH, 
                 OddParityOf(FINS1var & FINS2var & 
                             FLUSHvar & FXACKvar & INSTvar) after tIFPD;
      
    end if;
    
    --''''''''' CHECKER mode operation: generation of MCERR_N '''''''''''''
    if CMODE_N = '0' then
      if rising_edge(CLK) then
        if CMDmismatch = 1 or
           ( TOE_Ndel = '0' and HALTsampled = '1' and
             ( ((Ain /= A or APARin /= APAR or ASIin /= ASI) and 
                 AOE_Ndel = '0') or
               (ASPARin /= ASPAR and AOE_Ndel = '0' and COE_Ndel = '0') or
               ((SIZEin /= SIZE or RDin /= RD or WE_Nin /= WE_N or 
                 WRTin /= WRT or LOCKin /= LOCK or LDSTOin /= LDSTO or 
                 DXFERin /= DXFER or IMPARin /= IMPAR) and COE_Ndel = '0') or
               INULLin  /= INULL or
               FINS1in  /= FINS1 or
               FINS2in  /= FINS2 or
               FLUSHin  /= FLUSH or
               FXACKin  /= FXACK or
               INSTin   /= INST  or
               IFPARin  /= IFPAR or
               INTACKin /= INTACK 
             )
           )  then
           MCERR_Nin <= '0' after tMCED;
         else
           MCERR_Nin <= '1' after tMCEV;
         end if;
         
         if CMDmismatch /= 0 then
           CMDmismatch := CMDmismatch - 1;
         end if;
             
      elsif falling_edge(CLK) and pWE_Nvar = '0' then --$$$ TO BE FINALIZED
        if (Din /= D or DPARin /= DPAR) and DOE_Ndel = '0' and 
            TOE_Ndel = '0' and HALTsampled = '1' then
       --   if CLK'last_event >  then
           CMDmismatch := 1;
       --   else
       --    CMDmismatch := 2;
       --   end if;
        end if;
      end if;
    else
      MCERR_Nin <= '1' after tMCEV;
    end if;
    
    --''''''''' Events triggered on edges of XHOLD_N, MAO and FCCV. '''''''
    --''''''''' Address bus modifications. ''''''''''
    --''''''''' (Pipeline frozen) '''''''''''
    if (falling_edge(HOLDsig_N) and Mode = EXECUTE_MODE) then
      Ain     <= transport PrevAddr  after tHOD;
      APARin  <= transport OddParityOf(PrevAddr) after tHOD;
      Vec8bits := Extend(ToStdLogicVector(pASIvar), ASI'length, '0');
      Vec2bits := Extend(ToStdLogicVector(pSizeVar), 2, '0');
      ASIin   <= transport Vec8bits  after tHOD;
      SIZEin  <= transport Vec2bits  after tHOD;
      ASPARin <= transport OddParityOf(Vec8bits & Vec2bits) after tHOD;
      RDin    <= transport pRDvar    after tHOD;
      WE_Nin  <= transport pWE_Nvar  after tHOD;
      WRTin   <= transport pWRTvar   after tHOD;
      DXFERin <= transport pDXFERvar after tHDXD;
      LDSTOin <= transport pLDSTOvar after tHOD;
      LOCKin  <= transport pLOCKvar  after tHOD;
   --   INSTin  <= transport pINSTvar  after tHOD; -- <- should be commented??
      IMPARin <= OddParityOf(pDXFERvar & pLDSTOvar & pLOCKvar & 
                             pRDvar & pWE_Nvar & pWRTvar) after tHOD;
    elsif (rising_edge(HOLDsig_N) and Mode = EXECUTE_MODE) then
      if IsFCMP(WR.mnemo) and IsFBfcc(ID.mnemo) then
           -- Particular case here: FCMP - I1 - FBfcc
        TempINST := ID;
        ExecuteFBfcc(FCC, TempINST, CurrentAddr, TakenBr);
        ID <= TempINST;
      end if;
      Ain     <= transport CurrentAddr after tHOH;
      APARin  <= transport OddParityOf(CurrentAddr) after tHOH;
      Vec8bits := Extend(ToStdLogicVector(ASIvar), ASI'length, '0');
      Vec2bits := Extend(ToStdLogicVector(SizeVar), 2, '0');
      ASIin   <= transport Vec8bits after tHOH;
      SIZEin  <= transport Vec2bits after tHOH;
      ASPARin <= transport OddParityOf(Vec8bits & Vec2bits) after tHOH;
      RDin    <= transport RDvar    after tHOH;
      WE_Nin  <= transport WE_Nvar  after tHOH;
      WRTin   <= transport WRTvar   after tHOH;
      DXFERin <= transport DXFERvar after tHDXH;
      LDSTOin <= transport LDSTOvar after tHOH;
      LOCKin  <= transport LOCKvar  after tHOH;
   --   INSTin  <= transport INSTvar  after tHOH; -- <- should be commented??
      IMPARin <= OddParityOf(DXFERvar & LDSTOvar & LOCKvar & 
                             RDvar & WE_Nvar & WRTvar) after tHOH;
    end if;
    
    
    if (rising_edge(MAO) and Mode = EXECUTE_MODE) then
      Ain     <= transport pPrevAddr after tMAD;
      APARin  <= transport OddParityOf(pPrevAddr) after tMAD;
      Vec8bits := Extend(ToStdLogicVector(pTOpASIvar), ASI'length, '0');
      Vec2bits := Extend(ToStdLogicVector(pTOpSizeVar), 2, '0');
      ASIin   <= transport Vec8bits     after tMAD;
      SIZEin  <= transport Vec2bits     after tMAD;
      ASPARin <= transport OddParityOf(Vec8bits & Vec2bits) after tMAD;
      RDin    <= transport pTOpRDvar    after tMAD;
      WE_Nin  <= transport pTOpWE_Nvar  after tMAD;
      WRTin   <= transport pTOpWRTvar   after tMAD;
      LDSTOin <= transport pTOpLDSTOvar after tMAD;
      LOCKin  <= transport pTOpLOCKvar  after tMAD;
      DXFERin <= transport pTOpDXFERvar after tMAD;
      IMPARin <= OddParityOf(pTOpDXFERvar & pTOpLDSTOvar & pTOpLOCKvar & 
                             pTOpRDvar & pTOpWE_Nvar & 
                             pTOpWRTvar) after tMAD;
        
    elsif (falling_edge(MAO) and Mode = EXECUTE_MODE) then
      Ain     <= transport PrevAddr after tMAH;
      APARin  <= transport OddParityOf(PrevAddr) after tMAH;
      Vec8bits := Extend(ToStdLogicVector(pASIvar), ASI'length, '0');
      Vec2bits := Extend(ToStdLogicVector(pSizeVar), 2, '0');
      ASIin   <= transport Vec8bits after tMAH;
      SIZEin  <= transport Vec2bits after tMAH;
      ASPARin <= transport OddParityOf(Vec8bits & Vec2bits) after tMAH;  
      RDin    <= transport pRDvar    after tMAH;
      WE_Nin  <= transport pWE_Nvar  after tMAH;
      WRTin   <= transport pWRTvar   after tMAH;
      LDSTOin <= transport pLDSTOvar after tMAH;
      LOCKin  <= transport pLOCKvar  after tMAH;
      DXFERin <= transport pDXFERvar after tMAH;
      IMPARin <= OddParityOf(pDXFERvar & pLOCKvar & pLOCKvar & 
                             pRDvar & pWE_Nvar & pWRTvar) after tMAH;
    end if;
        
    --''''''''' Latch HOLD signals & generate global hold signal '''''''' 
    if (MHOLDA_N'event and CLK = '1') or rising_edge(CLK) then
      MHOLDA_Nlat <= MHOLDA_N;
    end if;
    
    if (MHOLDB_N'event and CLK = '1') or rising_edge(CLK) then
      MHOLDB_Nlat <= MHOLDB_N;
    end if;
    
    if (BHOLD_N'event and CLK = '1') or rising_edge(CLK) then
      BHOLD_Nlat <= BHOLD_N;
    end if;
    
    if (FHOLD_N'event and CLK = '1') or rising_edge(CLK) then
      FHOLD_Nlat <= FHOLD_N;
    end if;
    
    if (FCCV'event and CLK = '1') or rising_edge(CLK) then
      FCCVlat <= FCCV;
    end if;
    
    if (CHOLD_N'event and CLK = '1') or rising_edge(CLK) then
      CHOLD_Nlat <= CHOLD_N;
    end if;
    
    if (CCCV'event and CLK = '1') or rising_edge(CLK) then
      CCCVlat <= CCCV;
    end if;
    
    if (MHOLDA_Nlat'event or MHOLDB_Nlat'event or BHOLD_Nlat'event or
        FHOLD_Nlat'event or FCCVlat'event or CHOLD_Nlat'event or
        CCCVlat'event
       ) then
     HOLDsig_N <= MHOLDA_Nlat and MHOLDB_Nlat and BHOLD_Nlat and 
                  FHOLD_Nlat and FCCVlat and CHOLD_Nlat and CCCVlat;
    end if;

    --'''' Tristated Outputs modelling: effects of signals AOE_N, '''' 
    --'''' DOE_N, COE_N, TOE_N, HALT_N and CMODE on the output    ''''
    --'''' signals.  ''''
    if ATHCsig'event and ATHCsig = '1' then
      A     <= (others => 'Z');
      APAR  <= 'Z';
      ASI   <= (others => 'Z');
    elsif ATHCsig = '0' then
      A    <= Ain;
      APAR <= APARin;
      ASI  <= ASIin;
    elsif ATHCsig = 'U' then
      A    <= (others => 'U');
      APAR <= 'U';
      ASI  <= (others => 'U');
    elsif ATHCsig /= '1' then  -- should never get here.
      A    <= (others => 'X');
      APAR <= 'X';
      ASI  <= (others => 'X');
    end if;
    
    if DTHCsig'event and DTHCsig = '1' then
      D    <= (others => 'Z');
      DPAR <= 'Z';
    elsif DTHCsig = '0' then
      D    <= Din;
      DPAR <= DPARin;
    elsif DTHCsig = 'U' then 
      D    <= (others => 'U');
      DPAR <= 'U';
    elsif DTHCsig /= '1' then  -- should never get here.
      D    <= (others => 'X');
      DPAR <= 'X';
    end if;
    
    if CTHCsig'event and CTHCsig = '1' then
      SIZE  <= (others => 'Z');
      RD    <= 'Z';
      WE_N  <= 'Z';
      WRT   <= 'Z';
      LOCK  <= 'Z';
      LDSTO <= 'Z';
      DXFER <= 'Z';
      IMPAR <= 'Z';
    elsif CTHCsig = '0' then
      SIZE  <= SIZEin;
      RD    <= RDin;
      WE_N  <= WE_Nin;
      WRT   <= WRTin;
      LOCK  <= LOCKin;
      LDSTO <= LDSTOin;
      DXFER <= DXFERin;
      IMPAR <= IMPARin;
    elsif CTHCsig = 'U' then
      SIZE  <= "UU";
      RD    <= 'U';
      WE_N  <= 'U';
      WRT   <= 'U';
      LOCK  <= 'U';
      LDSTO <= 'U';
      DXFER <= 'U';
      IMPAR <= 'U';
    elsif CTHCsig /= '1' then -- should never get here.
      SIZE  <= "XX";
      RD    <= 'X';
      WE_N  <= 'X';
      WRT   <= 'X';
      LOCK  <= 'X';
      LDSTO <= 'X';
      DXFER <= 'X';
      IMPAR <= 'X';
    end if;
    
    if ACTHCsig'event and ACTHCsig = '1' then
      ASPAR <= 'Z';
    elsif ACTHCsig = '0' then
      ASPAR <= ASPARin;
    elsif ACTHCsig = 'U' then
      ASPAR <= 'U';
    elsif ACTHCsig /= '1' then
      ASPAR <= 'X';
    end if;
    
    if THCsig'event and THCsig = '1' then
      INULL  <= 'Z';
      FINS1  <= 'Z';
      FINS2  <= 'Z';
      FLUSH  <= 'Z';
      FXACK  <= 'Z';
      INST   <= 'Z';
      IFPAR  <= 'Z';
      INTACK <= 'Z';
    elsif THCsig = '0' then
      INULL  <= INULLin;
      FINS1  <= FINS1in;
      FINS2  <= FINS2in;
      FLUSH  <= FLUSHin;
      FXACK  <= FXACKin;
      INST   <= INSTin;
      IFPAR  <= IFPARin;
      INTACK <= INTACKin;
    elsif THCsig = 'U' then
      INULL  <= 'U';
      FINS1  <= 'U';
      FINS2  <= 'U';
      FLUSH  <= 'U';
      FXACK  <= 'U';
      INST   <= 'U';
      IFPAR  <= 'U';
      INTACK <= 'U';
    elsif THCsig /= '1' then
      INULL  <= 'X';
      FINS1  <= 'X';
      FINS2  <= 'X';
      FLUSH  <= 'X';
      FXACK  <= 'X';
      INST   <= 'X';
      IFPAR  <= 'X';
      INTACK <= 'X';
    end if;
    
    if TOE_N'event and TOE_N = '1' then
      ERROR_N   <= 'Z' after tTOD;
      MCERR_N   <= 'Z' after tTOD;
      HWERROR_N <= 'Z' after tTOD;
    elsif TOE_N'event and TOE_N = '0' then
      ERROR_N   <= ERROR_Nin after tTOE;
      MCERR_N   <= MCERR_Nin after tTOE;
      HWERROR_N <= HWERROR_Nin after tTOE;
    elsif TOE_N = '0' then
      ERROR_N   <= ERROR_Nin;
      MCERR_N   <= MCERR_Nin;
      HWERROR_N <= HWERROR_Nin;
    elsif TOE_N = 'U' then
      ERROR_N   <= 'U';
      MCERR_N   <= 'U';
      HWERROR_N <= 'U';
    elsif TOE_N /= '1' then
      ERROR_N   <= 'X';
      MCERR_N   <= 'X';
      HWERROR_N <= 'X';
    end if;
 
 
 
 Buf1IsValid_Spy <=  Buf1IsValid;  
 Buf2IsValid_Spy <=  Buf2IsValid;  
 IOPCASE_Spy <=  IOPCASE;  

    wr1x <= wr1;
    wr2x <= wr2;
    wr3x <= wr3;
    CurrentAddrx <= CurrentAddr;
  end process IUmodel;



  -------------------------------------------
  -- Processes to help model the tri-stated 
  -- output buffers.
  -------------------------------------------
  DTHCsigProcess  : DTHCsig  <= DOE_Ndel or TOE_Ndel or 
                                not(HALTsampled) or not(CMODE_N);
  ATHCsigProcess  : ATHCsig  <= AOE_Ndel or TOE_Ndel or 
                                not(HALTsampled) or not(CMODE_N);
  CTHCsigProcess  : CTHCsig  <= COE_Ndel or TOE_Ndel or 
                                not(HALTsampled) or not(CMODE_N);
  ACTHCsigProcess : ACTHCsig <= AOE_Ndel or COE_Ndel or TOE_Ndel or 
                                not(HALTsampled) or not(CMODE_N);
  THCsigProcess   : THCsig   <= TOE_Ndel or not(HALTsampled) or not(CMODE_N);
  
  SampleHALT_N: process
  begin
    wait on CLK;
    if falling_edge(CLK) then
      if HALT_N = '0' then
        HALTsampled <= '0' after tHAD;
      elsif HALT_N = '1' then
        HALTsampled <= '1' after tHAE;
      else
        HALTsampled <= HALT_N;
      end if;
    end if;
  end process SampleHALT_N;

  IntermediarySignals: process
  begin
        
    if AOE_N = '0' then
      AOE_Ndel <= '0' after tOE;
    elsif AOE_N = '1' then
      AOE_Ndel <= '1' after tOD;
    else
      AOE_Ndel <= AOE_N; -- should not happen in normal case
    end if;

    if DOE_N = '0' then
      DOE_Ndel <= '0' after tOE;
    elsif DOE_N = '1' then
      DOE_Ndel <= '1' after tOD;
    else
      DOE_Ndel <= DOE_N; -- should not happen in normal case
    end if;

    if COE_N = '0' then
      COE_Ndel <= '0' after tOE;
    elsif COE_N = '1' then
      COE_Ndel <= '1' after tOD;
    else
      COE_Ndel <= COE_N; -- should not happen in normal case
    end if;

    if TOE_N = '0' then
      TOE_Ndel <= '0' after tTOE;
    elsif TOE_N = '1' then
      TOE_Ndel <= '1' after tTOD;
    else
      TOE_Ndel <= TOE_N; -- should not happen in normal case
    end if;
    
    wait on AOE_N, DOE_N, COE_N, TOE_N;
    
  end process IntermediarySignals;

  -------------------------------------------
  -- TAP controller
  -------------------------------------------
  TAPctrller: TAP_iufpu
    generic map(
      tTDOD => tTDOD,
      tTDOH => tTDOH
    )
    
    port map(
      TRst_N => TRST_N,
      TCK    => TCLK,
      TDI    => TDI,
      TMS    => TMS,
      TDO    => TDO
    );

  --------------------------------------------------
  -- Setup/hold checkers 
  --------------------------------------------------
  
  ---
  CheckTimBidir_1_i <= transport CheckTimBidir after tDIH;
  CheckTimBidir_1   <= transport CheckTimBidir_1_i and not(RESET_N = '0') and 
                                 not(D = TRI_STATE32);
  
  DbusCheck :  DbusSetupHoldCheck(D, CLK, HOLDsig_N,
                                  SETUP => tDIS,  HOLD => tDIH, 
                                  PATH => "D", 
                                  DelayedData => D'Delayed(abs(tDIH)),
                                  EN_CHECKING => CheckTimBidir_1);
  
  ---
  CheckTimBidir_2_i <= transport CheckTimBidir after tDPIH;
  CheckTimBidir_2   <= transport CheckTimBidir_2_i and not(RESET_N = '0') and 
                                 not(DPAR = 'Z');

  DPARcheck :  DbusSetupHoldCheck(DPAR, CLK, HOLDsig_N,
                                  SETUP => tDPIS,  HOLD => tDPIH, 
                                  PATH => "DPAR", 
                                  DelayedData => DPAR'Delayed(abs(tDPIH)),
                                  EN_CHECKING => CheckTimBidir_2);
  
  ---
  Chk_MEXC_N_en <= not(RESET_N = '0') and not(MEXC_N = 'Z');
  
  SigMEXC_N   : CondSetupHoldCheck(MEXC_N, CLK, EDGE => RISING,
                                   SETUP => tMES,  HOLD => tMEH, 
                                   PATH => "MEXC_N", 
                                   DelayedData => MEXC_N'Delayed(abs(tMEH)),
                                   EN_CHECKING => Chk_MEXC_N_en); 
                                    
  ---
  Chk_BHOLD_N_en <= not(RESET_N = '0') and not(BHOLD_N = 'Z');
  
  SigBHOLD_N  : CondSetupHoldCheck(BHOLD_N, CLK, EDGE => FALLING,
                                   SETUP => tHS,  HOLD => tHH, 
                                   PATH => "BHOLD_N", 
                                   DelayedData => BHOLD_N'Delayed(abs(tHH)),
                                   EN_CHECKING => Chk_BHOLD_N_en); 
  
  ---
  Chk_MHOLDA_N_en <= not(RESET_N = '0') and not(MHOLDA_N = 'Z');
  
  SigMHOLDA_N : CondSetupHoldCheck(MHOLDA_N, CLK, EDGE => FALLING,
                                   SETUP => tHS,  HOLD => tHH, 
                                   PATH => "MHOLDA_N", 
                                   DelayedData => MHOLDA_N'Delayed(abs(tHH)),
                                   EN_CHECKING => Chk_MHOLDA_N_en);
  ---
  Chk_MHOLDB_N_en <= not(RESET_N = '0') and not(MHOLDB_N = 'Z');
  
  SigMHOLDB_N : CondSetupHoldCheck(MHOLDB_N, CLK, EDGE => FALLING,
                                   SETUP => tHS,  HOLD => tHH, 
                                   PATH => "MHOLDB_N", 
                                   DelayedData => MHOLDB_N'Delayed(abs(tHH)),
                                   EN_CHECKING => Chk_MHOLDB_N_en);
  
  ---
  Chk_FHOLD_N_en <= not(RESET_N = '0') and not(FHOLD_N = 'Z');
  
  SigFHOLD_N  : CondSetupHoldCheck(FHOLD_N, CLK, EDGE => FALLING,
                                   SETUP => tHS,  HOLD => tHH, 
                                   PATH => "FHOLD_N", 
                                   DelayedData => FHOLD_N'Delayed(abs(tHH)),
                                   EN_CHECKING => Chk_FHOLD_N_en);

  ---
--  SigCHOLD_N  : SetupHoldCheck(CHOLD_N, CLK, EDGE => FALLING,
--                               SETUP => tHS,  HOLD => tHH, 
--                               PATH => "CHOLD_N", 
--                               DelayedData => CHOLD_N'Delayed(abs(tHH)));

  ---
  Chk_FCC_en <= not(RESET_N = '0') and not(FCC = "ZZ");
  
  SigFCC      : CondSetupHoldCheck(FCC, CLK, EDGE => RISING,
                                   SETUP => tFIS,  HOLD => tFIH, 
                                   PATH => "FCC", 
                                   DelayedData => FCC'Delayed(abs(tFIH)),
                                   EN_CHECKING => Chk_FCC_en);
  
  ---
--  SigCCC      : SetupHoldCheck(CCC, CLK, EDGE => RISING,
--                               SETUP => tFIS,  HOLD => tFIH, 
--                               PATH => "CCC", 
--                               DelayedData => CCC'Delayed(abs(tFIH)));
  
  ---
  Chk_MDS_N_en <= not(RESET_N = '0') and not(MDS_N = 'Z');
  
  SigMDS_N    : CondSetupHoldCheck(MDS_N, CLK, EDGE => FALLING,
                                   SETUP => tMDS,  HOLD => tMDH, 
                                   PATH => "MDS_N",
                                   DelayedData => MDS_N'Delayed(abs(tMDH)),
                                   EN_CHECKING => Chk_MDS_N_en);
  
  ---
  Chk_FCCV_en <= not(RESET_N = '0') and not(FCCV = 'Z');
  
  SigFCCV     : CondSetupHoldCheck(FCCV, CLK, EDGE => FALLING,
                                   SETUP => tCCVS,  HOLD => tCCVH, 
                                   PATH => "FCCV", 
                                   DelayedData => FCCV'Delayed(abs(tCCVH)),
                                   EN_CHECKING => Chk_FCCV_en);
  
  ---
--  SigCCCV     : SetupHoldCheck(CCCV, CLK, EDGE => FALLING,
--                               SETUP => tCCVS,  HOLD => tCCVH, 
--                               PATH => "CCCV", 
--                               DelayedData => CCCV'Delayed(abs(tCCVH)));
  
  ---
  Chk_FEXC_N_en <= not(RESET_N = '0') and not(FEXC_N = 'Z');
  
  SigFEXC_N   : CondSetupHoldCheck(FEXC_N, CLK, EDGE => RISING,
                                   SETUP => tXES,  HOLD => tXEH, 
                                   PATH => "FEXC_N", 
                                   DelayedData => FEXC_N'Delayed(abs(tXEH)),
                                   EN_CHECKING => Chk_FEXC_N_en);
                               
  ---
--  SigCEXC_N   : SetupHoldCheck(CEXC_N, CLK, EDGE => RISING,
--                               SETUP => tXES,  HOLD => tXEH, 
--                               PATH => "CEXC_N", 
--                               DelayedData => CEXC_N'Delayed(abs(tXEH)));

  ---
  Chk_FIPAR_en <= not(RESET_N = '0') and not(FIPAR = 'Z');
  
  SigFIPAR    : CondSetupHoldCheck(FIPAR, CLK, EDGE => RISING,
                                   SETUP => tFIPS,  HOLD => tFIPH, 
                                   PATH => "FIPAR", 
                                   DelayedData => FIPAR'Delayed(abs(tFIPH)),
                                   EN_CHECKING => Chk_FIPAR_en);

  ---
  Chk_FP_N_en <= not(RESET_N = '0') and not(FP_N = 'Z');
  
  SigFP_N     : CondSetupHoldCheck(FP_N, CLK, EDGE => RISING,
                                   SETUP => tSTATS,  HOLD => 0 ns, 
                                   PATH => "FP_N", 
                                   DelayedData => FP_N'Delayed(0 ns),
                                   EN_CHECKING => Chk_FP_N_en);
  

  --'''''''''' THAT'S ALL FOLKS! ''''''''''''''


end vhdl_behavioral;
-------------------------------------------------------------------------------
-- File name : iurt_comp_pck.vhd
-- Title : IURTCompPck
-- project : SPARC 
-- Library : IURTLIB
-- Author(s) : Maxime ROCCA
-- Purpose : package to declare components of entities for the IURT.
--
-- notes :  include this package with a "use" clause whenever a component is 
--          instanciated. 
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        |  MR    | 94-03-04    | first version
--.............................................................................
-------------------------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
-------------------------------------------------------------------------------
---------|---------|---------|---------|---------|---------|---------|--------|

library IEEE;
use IEEE.Std_Logic_1164.all;
library MMS;
use MMS.StdSim.all;

package IURTCompPck is

  component IURTGeneric
    generic( -- Fake default timing values.
      tCY    : time := 50 ns; -- Clock cycle
      tCHL   : time := 22 ns; -- Clock high and low
      tAD    : time := 8 ns; -- A,ASI,SIZE,RD,WRT,WE_N,LOCK,LDSTO output delay 
      tAH    : time := 7 ns; -- A,ASI,SIZE,RD,WRT,WE_N,LOCK,LDSTO output valid
      tDOD   : time := 8 ns; -- D output delay
      tDOH   : time := 7 ns; -- D output valid
      tDIS   : time := 5 ns; -- D input setup
      tDIH   : time := 1 ns; -- D input hold
      tMES   : time := 5 ns; -- MEXC_N input setup
      tMEH   : time := 1 ns; -- MEXC_N input hold
      tHS    : time := 5 ns; -- BHOLD_N,MHOLDA_N,MHOLDB_N,FHOLD,CHOLD input stup
      tHH    : time := 1 ns; -- BHOLD_N,MHOLDA_N,MHOLDB_N,FHOLD,CHOLD input hold
      tHOD   : time := 8 ns; -- XHOLD_N to address/control output delay
      tHOH   : time := 8 ns; -- XHOLD_N to address/control output valid
      tOE    : time := 8 ns; -- AOE_N, COE_N, DOE_N to output enable delay
      tOD    : time := 8 ns; -- AOE_N, COE_N, DOE_N to output disable delay
      tTOE   : time := 8 ns; -- TOE_N to output enable delay
      tTOD   : time := 8 ns; -- TOE_N to output disable delay
      tSSD   : time := 8 ns; -- INST, FXACK, CXACK, INTACK output delay
      tSSH   : time := 7 ns; -- INST, FXACK, CXACK, INTACK output valid
      tRS    : time := 5 ns; -- RESET_N input setup
      tRH    : time := 1 ns; -- RESET_N input hold
      tFD    : time := 8 ns; -- FINS, CINS output delay
      tFH    : time := 7 ns; -- FINS, CINS output valid
      tFIS   : time := 5 ns; -- FCC, CCC input setup
      tFIH   : time := 1 ns; -- FCC, CCC input hold
      tDXD   : time := 8 ns; -- DXFER output delay
      tDXH   : time := 7 ns; -- DXFER output valid
      tHDXD  : time := 8 ns; -- XHOLD_N asserted to DXFER output delay
      tHDXH  : time := 7 ns; -- XHOLD_N asserted to DXFER output valid
      tNUD   : time := 8 ns; -- INULL output delay
      tNUH   : time := 7 ns; -- INULL output valid
      tMDS   : time := 5 ns; -- MDS_N input setup
      tMDH   : time := 1 ns; -- MDS_N input hold
      tFLS   : time := 8 ns; -- FLUSH output delay
      tFLH   : time := 7 ns; -- FLUSH output valid
      tCCVS  : time := 5 ns; -- FCCV, CCCV input setup
      tCCVH  : time := 1 ns; -- FCCV, CCCV input hold
      tXES   : time := 5 ns; -- FEXC_N, CEXC_N input setup
      tXEH   : time := 1 ns; -- FEXC_N, CEXC_N input hold
      tMAD   : time := 8 ns; -- MAO asserted to address/control output delay
      tMAH   : time := 8 ns; -- MAO asserted to address/control output valid
      tETD   : time := 8 ns; -- HWERROR_N output delay
      tETH   : time := 7 ns; -- HWERROR_N output valid
      tXAPD  : time := 8 ns; -- APAR & ASPAR output delay
      tXAPH  : time := 7 ns; -- APAR & ASPAR output valid
      tDPOD  : time := 8 ns; -- DPAR output delay
      tDPOH  : time := 7 ns; -- DPAR output valid
      tDPIS  : time := 5 ns; -- DPAR input setup
      tDPIH  : time := 1 ns; -- DPAR input hold
      tIFPD  : time := 8 ns; -- IFPAR output delay
      tIFPH  : time := 7 ns; -- IFPAR output valid
      tFIPS  : time := 8 ns; -- FIPAR output delay
      tFIPH  : time := 7 ns; -- FIPAR output valid
      tIMPD  : time := 8 ns; -- IMPAR output delay
      tIMPH  : time := 7 ns; -- IMPAR output valid
      tMCED  : time := 8 ns; -- MCERR_N output delay
      tMCEV  : time := 7 ns; -- MCERR_N output valid
      tSTATS : time := 5 ns; -- N601MODE_N, FLOW_N, CMODE_N, FP_N input setup
      tHAS   : time := 5 ns; -- HALT_N input setup
      tHAH   : time := 1 ns; -- HALT_N input hold
      tHAE   : time := 8 ns; -- HALT_N asserted to output enable delay
      tHAD   : time := 8 ns; -- HALT_N asserted to output disable delay
     
      tTCY  : time := 50 ns; -- TCLK Clock Cycle
      tTMS  : time := 5 ns; -- TMS setup
      tTMH  : time := 1 ns; -- TMS hold
      tTDIS : time := 5 ns; -- TDI setup
      tTDIH : time := 1 ns; -- TDI hold
      tTRS  : time := 5 ns; -- TRST_N setup
      tTRH  : time := 1 ns; -- TRST_N hold
      tTDOD : time := 8 ns; -- TDO output delay
      tTDOH : time := 7 ns  -- TDO output valid
    );
  
    port(
      CLK   : in std_logic; -- clock signal
    
      --  Memory Subsystems Interface Signals  
      A        : inout std_logic_vector(31 downto 0); --* Address bus
      APAR     : inout std_logic; --* Address bus Parity
      AOE_N    : in    std_logic; -- Address Output Enable
      ASI      : inout std_logic_vector(7 downto 0); --* Address Space Ident.
      ASPAR    : inout std_logic; --* ASI & SIZE Parity
      BHOLD_N  : in    std_logic; -- Bus Hold
      COE_N    : in    std_logic; -- Control Output Enable
      D        : inout std_logic_vector(31 downto 0); -- Data Bus
      DPAR     : inout std_logic; -- Data Bus Parity
      DOE_N    : in    std_logic; -- Data Output Enable
      DXFER    : inout std_logic; --* Data Transfer
      IFT_N    : in    std_logic; -- Instruction Cache Flush Trap
      INULL    : inout std_logic; --* Integer Unit Nullify Cycle
      LDSTO    : inout std_logic; --* Atomic Load-Store
      LOCK     : inout std_logic; --* Bus Lock
      MAO      : in    std_logic; -- Memory Address Output
      MDS_N    : in    std_logic; -- Memory Data Strobe
      MEXC_N   : in    std_logic; -- Memory Exception
      MHOLDA_N : in    std_logic; -- Memory Hold A
      MHOLDB_N : in    std_logic; -- Memory Hold B
      RD       : inout std_logic; --* Read Access
      SIZE     : inout std_logic_vector(1 downto 0); --* Bus Transaction Size
      WE_N     : inout std_logic; --* Write Enable
      WRT      : inout std_logic; --* Advanced Write
      IMPAR    : inout std_logic; --* IU to MEC Control Parity
    
      -- Interrupt and Control Signals
      ERROR_N    : out std_logic; -- Error State
      HWERROR_N  : out std_logic; -- Hardware error detected
      FLOW_N     : in  std_logic; -- Enable flow control
      MCERR_N    : out std_logic; -- Comparison error
      N601MODE_N : in std_logic; -- Normal 601Mode Operation
      CMODE_N    : in std_logic; -- Checker mode
      FPSYN      : in  std_logic; -- Floating-Point Synomym Mode
      INTACK     : inout std_logic; --* Interrupt Acknowledge
      IRL        : in  std_logic_vector(3 downto 0); -- Interrupt Request Level
      RESET_N    : in  std_logic; -- Integer Unit Reset
      TOE_N      : in  std_logic; -- Test Mode Output Enable
      HALT_N     : in  std_logic; -- Halt
    
      -- Floating Point / Coprocessor Interfaces
      FCC     : in    std_logic_vector( 1 downto 0); -- FP Condition Codes
      FCCV    : in    std_logic; -- Floating-Point Condition Codes Valid
      FEXC_N  : in    std_logic; -- Floating-Point Exception
      FHOLD_N : in    std_logic; -- Floating-Point Hold
      FIPAR   : in    std_logic; -- FPU to IU Control Parity
      FINS1   : inout std_logic; --* Floating-Point Instruction in Buffer 1
      FINS2   : inout std_logic; --* Floating-Point Instruction in Buffer 2
      FLUSH   : inout std_logic; --* Floating-Point/Coproc. Instruction Flush
      FP_N    : in    std_logic; -- Floating-Point Unit Present
      FXACK   : inout std_logic; --* Floating-Point Exception Acknowledge
      INST    : inout std_logic; --* Instruction Fetch
      IFPAR   : inout std_logic; --* IU to FPU Control Parity
      CCC     : in    std_logic_vector( 1 downto 0); -- Coproc. Condition Codes
      CCCV    : in    std_logic; -- Coprocessor Condition Codes Valid
      CEXC_N  : in    std_logic; -- Coprocessor Exception
      CHOLD_N : in    std_logic; -- Coprocessor Hold
      CINS1   : inout std_logic; --* Coprocessor Instruction in Buffer 1
      CINS2   : inout std_logic; --* Coprocessor Instruction in Buffer 2
      CP_N    : in    std_logic; -- Coprocessor Unit Present
      CXACK   : inout std_logic; --* Coprocessor Exception Acknowledge

      -- TAP signals
      TCLK      : in  std_logic; -- Test Clock
      TRST_N    : in  std_logic; -- Test Reset
      TMS       : in  std_logic; -- Test Mode Select
      TDI       : in  std_logic; -- Test Data Input
      TDO       : out std_logic  -- Test Data Output
    );
  end component; -- IURTGeneric


  component IURT
    generic(
            T : temperature := T_BOARD;
            V : voltage := V_BOARD;
            PROCES : proces_type := PROCES_BOARD;
            LOAD : capacitance := LOAD_BOARD
    );
    
    port(
      Clk   : in std_logic; -- clock signal
    
      --  Memory Subsystems Interface Signals  
      A        : inout std_logic_vector(31 downto 0); --* Address bus
      APAR     : inout std_logic; --* Address bus Parity
      AOE_N    : in    std_logic; -- Address Output Enable
      ASI      : inout std_logic_vector(7 downto 0); --* Address Space Ident.
      ASPAR    : inout std_logic; --* ASI & SIZE Parity
      BHOLD_N  : in    std_logic; -- Bus Hold
      COE_N    : in    std_logic; -- Control Output Enable
      D        : inout std_logic_vector(31 downto 0); -- Data Bus
      DPAR     : inout std_logic; -- Data Bus Parity
      DOE_N    : in    std_logic; -- Data Output Enable
      DXFER    : inout std_logic; --* Data Transfer
      IFT_N    : in    std_logic; -- Instruction Cache Flush Trap
      INULL    : inout std_logic; --* Integer Unit Nullify Cycle
      LDSTO    : inout std_logic; --* Atomic Load-Store
      LOCK     : inout std_logic; --* Bus Lock
      MAO      : in    std_logic; -- Memory Address Output
      MDS_N    : in    std_logic; -- Memory Data Strobe
      MEXC_N   : in    std_logic; -- Memory Exception
      MHOLDA_N : in    std_logic; -- Memory Hold A
      MHOLDB_N : in    std_logic; -- Memory Hold B
      RD       : inout std_logic; --* Read Access
      SIZE     : inout std_logic_vector(1 downto 0); --* Bus Transaction Size
      WE_N     : inout std_logic; --* Write Enable
      WRT      : inout std_logic; --* Advanced Write
      IMPAR    : inout std_logic; --* IU to MEC Control Parity
    
      -- Interrupt and Control Signals
      ERROR_N    : out std_logic; -- Error State
      HWERROR_N  : out std_logic; -- Hardware error detected
      FLOW_N     : in  std_logic; -- Enable flow control
      MCERR_N    : out std_logic; -- Comparison error
      N601MODE_N : in std_logic; -- Normal 601Mode Operation
      CMODE_N    : in std_logic; -- Checker mode
      FPSYN      : in  std_logic; -- Floating-Point Synomym Mode
      INTACK     : inout std_logic; --* Interrupt Acknowledge
      IRL        : in  std_logic_vector(3 downto 0); -- Interrupt Request Level
      RESET_N    : in  std_logic; -- Integer Unit Reset
      TOE_N      : in  std_logic; -- Test Mode Output Enable
      HALT_N     : in  std_logic; -- Halt
    
      -- Floating Point / Coprocessor Interfaces
      FCC     : in    std_logic_vector( 1 downto 0); -- FP Condition Codes
      FCCV    : in    std_logic; -- Floating-Point Condition Codes Valid
      FEXC_N  : in    std_logic; -- Floating-Point Exception
      FHOLD_N : in    std_logic; -- Floating-Point Hold
      FIPAR   : in    std_logic; -- FPU to IU Control Parity
      FINS1   : inout std_logic; --* Floating-Point Instruction in Buffer 1
      FINS2   : inout std_logic; --* Floating-Point Instruction in Buffer 2
      FLUSH   : inout std_logic; --* Floating-Point/Coproc. Instruction Flush
      FP_N    : in    std_logic; -- Floating-Point Unit Present
      FXACK   : inout std_logic; --* Floating-Point Exception Acknowledge
      INST    : inout std_logic; --* Instruction Fetch
      IFPAR   : inout std_logic; --* IU to FPU Control Parity
      CCC     : in    std_logic_vector( 1 downto 0); -- Coproc. Condition Codes
      CCCV    : in    std_logic; -- Coprocessor Condition Codes Valid
      CEXC_N  : in    std_logic; -- Coprocessor Exception
      CHOLD_N : in    std_logic; -- Coprocessor Hold
      CINS1   : inout std_logic; --* Coprocessor Instruction in Buffer 1
      CINS2   : inout std_logic; --* Coprocessor Instruction in Buffer 2
      CP_N    : in    std_logic; -- Coprocessor Unit Present
      CXACK   : inout std_logic; --* Coprocessor Exception Acknowledge

     -- TAP signals
     TCLK      : in  std_logic; -- Test Clock
     TRST_N    : in  std_logic; -- Test Reset
     TMS       : in  std_logic; -- Test Mode Select
     TDI       : in  std_logic; -- Test Data Input
     TDO       : out std_logic  -- Test Data Output
  );
  end component; -- IURT
  
end IURTCompPck;
-------------------------------------------------------------------------------
-- File name : iurt.vhd
-- Title : IURT
-- project : SPARC 
-- Library : IURT_LIB
-- Author(s) : Maxime ROCCA
-- Purpose :  definition of entity IURT (with simulation and timing parameters)
--
-- notes : to be used for board simulation (including timing).
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        |  MR    | 94-03-04    | first version
--.............................................................................
-- v 1.1        |  MR    | 94-05-03    | 2nd version
-- + modify value for the record technology
-------------------------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
-------------------------------------------------------------------------------
---------|---------|---------|---------|---------|---------|---------|--------|

library IEEE;
use IEEE.Std_Logic_1164.all;
library MMS;
use MMS.StdSim.all;
use MMS.StdTiming.all;
use MMS.StdIoImp.all;
library IURT_LIB;
use IURT_LIB.IURTCompPck.IURTGeneric;
use IURT_LIB.IURTTimPar.all;


entity IURT is
  generic(
          T : temperature := T_BOARD;
          V : voltage := V_BOARD;
          PROCES : proces_type := PROCES_BOARD;
          LOAD : capacitance := LOAD_BOARD
  );
  
  port(
    CLK   : in std_logic; -- clock signal
    
    --  Memory Subsystems Interface Signals  
    A        : inout std_logic_vector(31 downto 0); --* Address bus
    APAR     : inout std_logic; --* Address bus Parity
    AOE_N    : in    std_logic; -- Address Output Enable
    ASI      : inout std_logic_vector(7 downto 0); --* Address Space Identifier
    ASPAR    : inout std_logic; --* ASI & SIZE Parity
    BHOLD_N  : in    std_logic; -- Bus Hold
    COE_N    : in    std_logic; -- Control Output Enable
    D        : inout std_logic_vector(31 downto 0); -- Data Bus
    DPAR     : inout std_logic; -- Data Bus Parity
    DOE_N    : in    std_logic; -- Data Output Enable
    DXFER    : inout std_logic; --* Data Transfer
    IFT_N    : in    std_logic; -- Instruction Cache Flush Trap
    INULL    : inout std_logic; --* Integer Unit Nullify Cycle
    LDSTO    : inout std_logic; --* Atomic Load-Store
    LOCK     : inout std_logic; --* Bus Lock
    MAO      : in    std_logic; -- Memory Address Output
    MDS_N    : in    std_logic; -- Memory Data Strobe
    MEXC_N   : in    std_logic; -- Memory Exception
    MHOLDA_N : in    std_logic; -- Memory Hold A
    MHOLDB_N : in    std_logic; -- Memory Hold B
    RD       : inout std_logic; --* Read Access
    SIZE     : inout std_logic_vector(1 downto 0); --* Bus Transaction Size
    WE_N     : inout std_logic; --* Write Enable
    WRT      : inout std_logic; --* Advanced Write
    IMPAR    : inout std_logic; --* IU to MEC Control Parity
    
    -- Interrupt and Control Signals
    ERROR_N    : out std_logic; -- Error State
    HWERROR_N  : out std_logic; -- Hardware error detected
    FLOW_N     : in  std_logic; -- Enable flow control
    MCERR_N    : out std_logic; -- Comparison error
    N601MODE_N : in  std_logic; -- Normal 601Mode Operation
    CMODE_N    : in  std_logic; -- Checker mode
    FPSYN      : in  std_logic; -- Floating-Point Synomym Mode
    INTACK     : inout std_logic; --* Interrupt Acknowledge
    IRL        : in  std_logic_vector(3 downto 0); -- Interrupt Request Level
    RESET_N    : in  std_logic; -- Integer Unit Reset
    TOE_N      : in  std_logic; -- Test Mode Output Enable
    HALT_N     : in  std_logic; -- Halt
    
    -- Floating Point / Coprocessor Interfaces
    FCC     : in    std_logic_vector( 1 downto 0); -- FP Condition Codes
    FCCV    : in    std_logic; -- Floating-Point Condition Codes Valid
    FEXC_N  : in    std_logic; -- Floating-Point Exception
    FHOLD_N : in    std_logic; -- Floating-Point Hold
    FIPAR   : in    std_logic; -- FPU to IU Control Parity
    FINS1   : inout std_logic; --* Floating-Point Instruction in Buffer 1
    FINS2   : inout std_logic; --* Floating-Point Instruction in Buffer 2
    FLUSH   : inout std_logic; --* Floating-Point/Coproc. Instruction Flush
    FP_N    : in    std_logic; -- Floating-Point Unit Present
    FXACK   : inout std_logic; --* Floating-Point Exception Acknowledge
    INST    : inout std_logic; --* Instruction Fetch
    IFPAR   : inout std_logic; --* IU to FPU Control Parity
    CCC     : in    std_logic_vector( 1 downto 0); -- Coproc. Condition Codes
    CCCV    : in    std_logic; -- Coprocessor Condition Codes Valid
    CEXC_N  : in    std_logic; -- Coprocessor Exception
    CHOLD_N : in    std_logic; -- Coprocessor Hold
    CINS1   : inout std_logic; --* Coprocessor Instruction in Buffer 1
    CINS2   : inout std_logic; --* Coprocessor Instruction in Buffer 2
    CP_N    : in    std_logic; -- Coprocessor Unit Present
    CXACK   : inout std_logic; --* Coprocessor Exception Acknowledge

    -- TAP signals
    TCLK      : in  std_logic; -- Test Clock
    TRST_N    : in  std_logic; -- Test Reset
    TMS       : in  std_logic; -- Test Mode Select
    TDI       : in  std_logic; -- Test Data Input
    TDO       : out std_logic  -- Test Data Output
  );
end IURT;
 

architecture WithTiming of IURT is
  constant MHS_MC : technology := (
     (0.9375, 0.0025, 0.0, 0.0), -- real values
     (2.0, -0.20, 0.0, 0.0), 
     0.8, 1.3,
     47.0);
     
-- Configuration of components
--  for all : IURTGeneric use entity IURTLIB.IURTGeneric(Behavior);
    
begin
  MyIURTGeneric: IURTGeneric
    generic map(
      tCY   => tCY,
      tCHL  => tCHL,
      tAD   => CalcDelay(tAD  , MHS_MC, T, V, PROCES, LOAD),
      tAH   => CalcDelay(tAH  , MHS_MC, T, V, PROCES, LOAD),
      tDOD  => CalcDelay(tDOD , MHS_MC, T, V, PROCES, LOAD),
      tDOH  => CalcDelay(tDOH , MHS_MC, T, V, PROCES, LOAD),
      tDIS  => CalcDelay(tDIS , MHS_MC, T, V, PROCES),
      tDIH  => CalcDelay(tDIH , MHS_MC, T, V, PROCES),
      tMES  => CalcDelay(tMES , MHS_MC, T, V, PROCES),
      tMEH  => CalcDelay(tMEH , MHS_MC, T, V, PROCES),
      tHS   => CalcDelay(tHS  , MHS_MC, T, V, PROCES),
      tHH   => CalcDelay(tHH  , MHS_MC, T, V, PROCES),
      tHOD  => CalcDelay(tHOD , MHS_MC, T, V, PROCES, LOAD),
      tHOH  => CalcDelay(tHOH , MHS_MC, T, V, PROCES, LOAD),
      tOE   => CalcDelay(tOE  , MHS_MC, T, V, PROCES, LOAD),
      tOD   => CalcDelay(tOD  , MHS_MC, T, V, PROCES, LOAD),
      tTOE  => CalcDelay(tTOE , MHS_MC, T, V, PROCES, LOAD),
      tTOD  => CalcDelay(tTOD , MHS_MC, T, V, PROCES, LOAD),
      tSSD  => CalcDelay(tSSD , MHS_MC, T, V, PROCES, LOAD),
      tSSH  => CalcDelay(tSSH , MHS_MC, T, V, PROCES, LOAD),
      tRS   => CalcDelay(tRS  , MHS_MC, T, V, PROCES),
      tRH   => CalcDelay(tRH  , MHS_MC, T, V, PROCES),
      tFD   => CalcDelay(tFD  , MHS_MC, T, V, PROCES, LOAD),
      tFH   => CalcDelay(tFH  , MHS_MC, T, V, PROCES, LOAD),
      tFIS  => CalcDelay(tFIS , MHS_MC, T, V, PROCES),
      tFIH  => CalcDelay(tFIH , MHS_MC, T, V, PROCES),
      tDXD  => CalcDelay(tDXD , MHS_MC, T, V, PROCES, LOAD),
      tDXH  => CalcDelay(tDXH , MHS_MC, T, V, PROCES, LOAD),
      tHDXD => CalcDelay(tHDXD, MHS_MC, T, V, PROCES, LOAD),
      tHDXH => CalcDelay(tHDXH, MHS_MC, T, V, PROCES, LOAD),
      tNUD  => CalcDelay(tNUD , MHS_MC, T, V, PROCES, LOAD),
      tNUH  => CalcDelay(tNUH , MHS_MC, T, V, PROCES, LOAD),
      tMDS  => CalcDelay(tMDS , MHS_MC, T, V, PROCES),
      tMDH  => CalcDelay(tMDH , MHS_MC, T, V, PROCES),
      tFLS  => CalcDelay(tFLS , MHS_MC, T, V, PROCES, LOAD),
      tFLH  => CalcDelay(tFLH , MHS_MC, T, V, PROCES, LOAD),
      tCCVS => CalcDelay(tCCVS, MHS_MC, T, V, PROCES),
      tCCVH => CalcDelay(tCCVH, MHS_MC, T, V, PROCES),
      tXES  => CalcDelay(tXES , MHS_MC, T, V, PROCES),
      tXEH  => CalcDelay(tXEH , MHS_MC, T, V, PROCES),
      tMAD  => CalcDelay(tMAD , MHS_MC, T, V, PROCES, LOAD),
      tMAH  => CalcDelay(tMAH , MHS_MC, T, V, PROCES, LOAD),
      tETD  => CalcDelay(tETD , MHS_MC, T, V, PROCES, LOAD),
      tETH  => CalcDelay(tETH , MHS_MC, T, V, PROCES, LOAD),
      tXAPD => CalcDelay(tXAPD, MHS_MC, T, V, PROCES, LOAD),
      tXAPH => CalcDelay(tXAPH, MHS_MC, T, V, PROCES, LOAD),
      tDPOD => CalcDelay(tDPOD, MHS_MC, T, V, PROCES, LOAD),
      tDPOH => CalcDelay(tDPOH, MHS_MC, T, V, PROCES, LOAD),
      tDPIS => CalcDelay(tDPIS, MHS_MC, T, V, PROCES),
      tDPIH => CalcDelay(tDPIH, MHS_MC, T, V, PROCES),
      tIFPD => CalcDelay(tIFPD, MHS_MC, T, V, PROCES, LOAD),
      tIFPH => CalcDelay(tIFPH, MHS_MC, T, V, PROCES, LOAD),
      tFIPS => CalcDelay(tFIPS, MHS_MC, T, V, PROCES, LOAD),
      tFIPH => CalcDelay(tFIPH, MHS_MC, T, V, PROCES, LOAD),
      tIMPD => CalcDelay(tIMPD, MHS_MC, T, V, PROCES, LOAD),
      tIMPH => CalcDelay(tIMPH, MHS_MC, T, V, PROCES, LOAD),
      tMCED => CalcDelay(tMCED, MHS_MC, T, V, PROCES, LOAD),
      tMCEV => CalcDelay(tMCEV, MHS_MC, T, V, PROCES, LOAD),
      tSTATS => CalcDelay(tSTATS, MHS_MC, T, V, PROCES),
      tHAS  => CalcDelay(tHAS , MHS_MC, T, V, PROCES),
      tHAH  => CalcDelay(tHAH , MHS_MC, T, V, PROCES),
      tHAE  => CalcDelay(tHAE , MHS_MC, T, V, PROCES, LOAD),
      tHAD  => CalcDelay(tHAD , MHS_MC, T, V, PROCES, LOAD),

      tTCY  => tTCY,
      tTMS  => CalcDelay(tTMS , MHS_MC, T, V, PROCES),
      tTMH  => CalcDelay(tTMH , MHS_MC, T, V, PROCES),
      tTDIS => CalcDelay(tTDIS, MHS_MC, T, V, PROCES),
      tTDIH => CalcDelay(tTDIH, MHS_MC, T, V, PROCES),
      tTRS  => CalcDelay(tTRS , MHS_MC, T, V, PROCES),
      tTRH  => CalcDelay(tTRH , MHS_MC, T, V, PROCES),
      tTDOD => CalcDelay(tTDOD, MHS_MC, T, V, PROCES, LOAD),
      tTDOH => CalcDelay(tTDOH, MHS_MC, T, V, PROCES, LOAD)
    )
  
    port map(

      CLK,  -- clock signal
    
      --  Memory Subsystems Interface Signals  
      A, 
      APAR, 
      AOE_N, 
      ASI, 
      ASPAR, 
      BHOLD_N, 
      COE_N, 
      D, 
      DPAR, 
      DOE_N, 
      DXFER,  
      IFT_N,
      INULL, 
      LDSTO, 
      LOCK, 
      MAO, 
      MDS_N, 
      MEXC_N, 
      MHOLDA_N, 
      MHOLDB_N,
      RD,
      SIZE,
      WE_N,
      WRT,
      IMPAR,
      
      -- Interrupt and Control Signals
      ERROR_N,
      HWERROR_N,
      FLOW_N,
      MCERR_N,
      N601MODE_N,
      CMODE_N,
      FPSYN,
      INTACK,
      IRL,
      RESET_N,
      TOE_N,
      HALT_N,
    
      -- Floating Point / Coprocessor Interfaces
      FCC,
      FCCV,
      FEXC_N,
      FHOLD_N,
      FIPAR,
      FINS1,
      FINS2,
      FLUSH,
      FP_N,
      FXACK,
      INST,
      IFPAR,
      CCC,
      CCCV,
      CEXC_N,
      CHOLD_N,
      CINS1,
      CINS2,
      CP_N,
      CXACK,

      -- TAP signals
      TCLK,
      TRST_N,
      TMS,
      TDI,
      TDO
    );
    
end WithTiming;

