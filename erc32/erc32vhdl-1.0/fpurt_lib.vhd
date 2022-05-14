-------------------------------------------------------------------------------
-- File name : fpurt_gen_ent.vhd
-- Title : FPURTGeneric (entity)
-- project : SPARC
-- Library : FPURTLIB
-- Author(s) : Maxime ROCCA
-- Purpose : definition of entity FPURTGeneric
--
-- notes :   
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        |   MR   | 94-03-04    | first version
--.............................................................................
-- v 1.1        |   MR   | 94-05-27    | 2nd version
-- + modification of timing checkers.
------------------------------------------------------------------------------
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

entity FPURTGeneric is
  generic( -- Fake default timing values
    tCY      : time := 50 ns; -- Clock cycle
    tCHL     : time := 22 ns; -- CLock High and Low
    tAS      : time := 5 ns; -- A input setup
    tAH      : time := 1 ns; -- A input hold
    tDIS     : time := 5 ns; -- D input setup
    tDIH     : time := 1 ns; -- D input hold
    tDOD     : time := 7 ns; -- D output delay
    tDOH     : time := 6 ns; -- D data valid
    tDOFFL   : time := 7 ns; -- D output turn-off (FLUSH+)
    tDOHFL   : time := 6 ns; -- D output valid (FLUSH+)
    tDOFOE   : time := 7 ns; -- D output turn-off (DOE_N+)
    tDONOE   : time := 7 ns; -- D output turn-on (DOE_N-)
    tDOHOE   : time := 6 ns; -- D output valid (DOE_N-)
    tFIS     : time := 5 ns; -- FINS1/2 input setup
    tFIH     : time := 1 ns; -- FINS1/2 input hold
    tINS     : time := 5 ns; -- INST input setup
    tINH     : time := 1 ns; -- INST input hold
    tFXS     : time := 5 ns; -- FXACK input setup
    tFXH     : time := 1 ns; -- FXACK input hold
    tFLS     : time := 5 ns; -- FLUSH input setup
    tFLH     : time := 1 ns; -- FLUSH input hold
    tRES     : time := 5 ns; -- RESET_N input setup
    tREH     : time := 1 ns; -- RESET_N input hold
    tMHS     : time := 5 ns; -- MHOLD_N input setup
    tMHH     : time := 1 ns; -- MHOLD_N input hold
    tMDS     : time := 5 ns; -- MDS_N input setup
    tMDH     : time := 1 ns; -- MDS_N input hold
    tFHD     : time := 7 ns; -- FHOLD_N output delay
    tFHH     : time := 6 ns; -- FHOLD_N output valid
    tFHDFI   : time := 7 ns; -- FHOLD_N output delay (FINS1/2+)
    tFHDFL   : time := 7 ns; -- FHOLD_N output delay (FLUSH+)
    tFHDMH   : time := 7 ns; -- FHOLD_N output delay (MHOLD_N-)
    tFCCVD   : time := 7 ns; -- FCCV output delay
    tFCCVH   : time := 6 ns; -- FCCV output valid
    tFCCVDFL : time := 7 ns; -- FCCV output delay (FLUSH+)
    tFCCVDMH : time := 7 ns; -- FCCV output delay (MHOLD_N-)
    tFCCD    : time := 7 ns; -- FCC output delay
    tFCCH    : time := 6 ns; -- FCC output valid
    tFED     : time := 7 ns; -- FEXC_N output delay
    tFEH     : time := 6 ns; -- FEXC_N output valid
    tFND     : time := 7 ns; -- FNULL output delay
    tFNH     : time := 6 ns; -- FNULL output valid
    tAPS     : time := 7 ns; -- APAR input setup
    tAPH     : time := 6 ns; -- APAR input hold
    tDPIS    : time := 7 ns; -- DPAR input setup
    tDPIH    : time := 6 ns; -- DPAR input hold
    tDPOD    : time := 7 ns; -- DPAR output delay
    tDPOH    : time := 6 ns; -- DPAR output valid
    tIFS     : time := 7 ns; -- IFPAR input setup
    tIFH     : time := 6 ns; -- IFPAR input hold
    tFIPD    : time := 7 ns; -- FIPAR output delay
    tFIPH    : time := 6 ns; -- FIPAR output valid
    tMCD     : time := 7 ns; -- MCERR_N output delay
    tMCH     : time := 6 ns; -- MCERR_N output valid
    tCMS     : time := 5 ns; -- N602MODE_N, CMODE_N input setup
    tHAS     : time := 5 ns; -- HALT_N input setup
    tHAH     : time := 1 ns; -- HALT_N input hold
    tHAD     : time := 7 ns; -- HALT_N asserted to output disable delay
    tHAE     : time := 7 ns; -- HALT_N asserted to output enable delay
    tERD     : time := 7 ns; -- HWERROR_N output delay
    tERH     : time := 6 ns; -- HWERROR_N output valid
    
    tTCY     : time := 50 ns; -- TCLK Clock Cycle
    tTMS     : time := 5 ns; -- TMS setup
    tTMH     : time := 1 ns; -- TMS hold
    tTDIS    : time := 5 ns; -- TDI setup
    tTDIH    : time := 1 ns; -- TDI hold
    tTRS     : time := 5 ns; -- TRST_N setup
    tTRH     : time := 1 ns; -- TRST_N hold
    tTDOD    : time := 7 ns; -- TDO output delay
    tTDOH    : time := 6 ns  -- TDO output valid
  );
  
  port(
    -- Note: signals which are functionally output signals but are actually
    -- inout signals because of the Master/Checker mode have an "*" in the 
    -- comments defining their function.

    CLK   : in std_logic; -- clock signal
    
    -- Integer Unit Interface Signals
    FP_N    : inout std_logic; -- Floating-point (Fp) Present
    FCC     : inout std_logic_vector(1 downto 0); --* Fp Condition Codes
    FCCV    : inout std_logic; --* Fp Condition Codes Valid
    FHOLD_N : inout std_logic; --* Fp Hold
    FEXC_N  : inout std_logic; --* Fp EXCeption
    FIPAR   : inout std_logic; --* Fpu to Iu control PARity
    FXACK   : in  std_logic; -- Fp eXception ACKnowledge
    INST    : in  std_logic; -- INSTruction fetch
    FINS1   : in  std_logic; -- Fp INStruction in buffer 1
    FINS2   : in  std_logic; -- Fp INStruction in buffer 2
    FLUSH   : in  std_logic; -- Fp instruction fLUSH
    IFPAR   : in  std_logic; -- Iu to Fpu control PARity
    
    -- System/Memory Interface Signals
    A          : in  std_logic_vector(31 downto 0); -- Address bus
    APAR       : in  std_logic; -- Address bus PARity
    D          : inout std_logic_vector(31 downto 0); -- Data bus
    DPAR       : inout std_logic; -- Data bus PARity
    DOE_N      : in  std_logic; -- Data Output Enable
    COE_N      : in  std_logic; -- Control Output Enable
    MHOLDA_N   : in  std_logic; -- Memory HOLD
    MHOLDB_N   : in  std_logic; -- Memory HOLD
    BHOLD_N    : in  std_logic; -- Bus HOLD
    MDS_N      : in  std_logic; -- Memory Data Strobe
    FNULL      : inout std_logic; --* Fpu NULLify cycle
    RESET_N    : in  std_logic; -- Reset signal
    HWERROR_N  : out std_logic; -- Hardware error detected
    CMODE_N    : in  std_logic; -- master/Checker MODE
    MCERR_N    : out std_logic; -- Comparison Error
    N602MODE_N : in  std_logic; -- Normal 602MODE Operation
    HALT_N     : in  std_logic; -- Halt mode
    
    -- Coprocessor Interface Signals
    CHOLD_N : in std_logic; -- Coprocessor hold.
    CCCV    : in std_logic; -- Coprocessor Condition Code Valid.

    -- Test Access Port (TAP) signals
    TCLK   : in  std_logic; -- Test CLocK
    TRST_N : in  std_logic; -- Test ReSeT
    TMS    : in  std_logic; -- Test Mode Select
    TDI    : in  std_logic; -- Test Data In
    TDO    : out std_logic  -- Test Data Out
  );
  
begin
  -- PUT HERE TIMING CHECKERS: SETUP & HOLD TIME + PULSE WIDTH CHECKERS.
  SigRESET_N    : SetupHoldCheck(RESET_N, CLK, EDGE => RISING,
                                 SETUP => tRES,  HOLD => tREH, 
                                 PATH => "RESET_N", 
                                 DelayedData => RESET_N'Delayed(abs(tREH)));
                                 
  SigN602MODE_N : SetupHoldCheck(N602MODE_N, CLK, EDGE => RISING,
                                 SETUP => tCMS,  HOLD => 0 ns, 
                                 PATH => "N602MODE_N", 
                                 DelayedData => N602MODE_N'Delayed(0 ns));
                                 
  SigCMODE_N    : SetupHoldCheck(CMODE_N, CLK, EDGE => RISING,
                                 SETUP => tCMS,  HOLD => 0 ns, 
                                 PATH => "CMODE_N", 
                                 DelayedData => CMODE_N'Delayed(0 ns));
                                 
  SigHALT_N     : SetupHoldCheck(HALT_N, CLK, EDGE => FALLING,
                                 SETUP => tHAS,  HOLD => tHAH, 
                                 PATH => "HALT_N", 
                                 DelayedData => HALT_N'Delayed(abs(tHAH)));
                                 
  SigTMS        : SetupHoldCheck(TMS, TCLK, EDGE => RISING,
                                 SETUP => tTMS,  HOLD => tTMH, 
                                 PATH => "TMS", 
                                 DelayedData => TMS'Delayed(abs(tTMH)));
                                 
  SigTDI        : SetupHoldCheck(TDI, TCLK, EDGE => RISING,
                                 SETUP => tTDIS,  HOLD => tTDIH, 
                                 PATH => "TDI", 
                                 DelayedData => TDI'Delayed(abs(tTDIH)));
                                 
  SigTRST_N     : SetupHoldCheck(TRST_N, TCLK, EDGE => RISING,
                                 SETUP => tTRS,  HOLD => tTRH, 
                                 PATH => "TRST_N", 
                                 DelayedData => TRST_N'Delayed(abs(tTRH)));
                                 
  
  CLKHigh : PulseCheck(CLK, LEVEL => '1', WIDTH => tCHL, 
                       SENSE => MINIMUM, PATH => "CLK");
                                
  CLKlow  : PulseCheck(CLK, LEVEL => '0', WIDTH => tCHL, 
                       SENSE => MINIMUM, PATH => "CLK");
    
  ---
  CLKCycle : process    -- Passive process: checks minimal value for CLK cycle.
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
  RESET_Nwidth : process    -- Passive process: checking on RESET_N width.
    constant MIN_NB_RESET_CYCLES : natural := 10;
    variable CountNbResetCycle : natural := 0;
  begin
    if not(CHECK_ON) then
      wait; -- the process dies here....
    end if;
    
    wait on CLK until rising_edge(CLK);
    
    if RESET_N = '1' then
      if CountNbResetCycle < MIN_NB_RESET_CYCLES and CountNbResetCycle /= 0 then
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
  
end FPURTGeneric;
-------------------------------------------------------------
-- File containing timing values for the FPURT VHDL model.
-- 
-- ALL THE TIMING PARAMETERS are given at 125 degrees C,  
-- 4.5 Volts and in worst case process conditions.
-- WARNING: minimal values for output signal propagation 
-- delay in data sheets are usually given in best conditions,  
-- i.e -55 Celsius, 5.5 Volts and best case process conditions.
-- They must be re-calculated for worst case conditions.
-------------------------------------------------------------

package FPURTTimPar is

  constant tCY      : time := 40 ns;
  constant tCHL     : time := 18 ns;
  constant tAS      : time :=  3 ns;
  constant tAH      : time :=  6 ns;
  constant tDIS     : time :=  3 ns;
  constant tDIH     : time :=  4 ns;
  constant tDOD     : time := 29 ns;
  constant tDOH     : time :=  9 ns;
  constant tDOFFL   : time := 31 ns;
  constant tDOHFL   : time :=  0 ns;
  constant tDOFOE   : time := 15 ns;
  constant tDONOE   : time := 15 ns;
  constant tDOHOE   : time :=  0 ns;
  constant tFIS     : time :=  9 ns;
  constant tFIH     : time :=  3 ns;
  constant tINS     : time := 16 ns; 
  constant tINH     : time :=  2 ns; 
  constant tFXS     : time := 16 ns; 
  constant tFXH     : time :=  2 ns; 
  constant tFLS     : time := 21 ns;
  constant tFLH     : time :=  2 ns;
  constant tRES     : time := 15 ns;
  constant tREH     : time :=  3 ns;
  constant tMHS     : time :=  7 ns;
  constant tMHH     : time :=  4 ns;
  constant tMDS     : time :=  5 ns;
  constant tMDH     : time :=  4 ns;
  constant tFHD     : time := 29 ns;
  constant tFHH     : time := 12 ns;
  constant tFHDFI   : time := 16 ns;
  constant tFHDFL   : time := 28 ns;
  constant tFHDMH   : time := 36 ns;
  constant tFCCVD   : time := 29 ns;
  constant tFCCVH   : time := 12 ns;
  constant tFCCVDFL : time := 28 ns; 
  constant tFCCVDMH : time := 36 ns;
  constant tFCCD    : time := 26 ns;
  constant tFCCH    : time := 12 ns;
  constant tFED     : time := 26 ns;
  constant tFEH     : time := 12 ns;
  constant tFND     : time := 20 ns;
  constant tFNH     : time :=  7 ns;
  constant tAPS     : time :=  3 ns;
  constant tAPH     : time :=  6 ns;
  constant tDPIS    : time :=  3 ns;
  constant tDPIH    : time :=  4 ns;
  constant tDPOD    : time := 29 ns;
  constant tDPOH    : time := 10 ns;
  constant tIFS     : time :=  9 ns;
  constant tIFH     : time :=  2 ns; 
  constant tFIPD    : time := 28 ns; 
  constant tFIPH    : time := 12 ns;
  constant tMCD     : time := 15 ns;
  constant tMCH     : time :=  7 ns;
  constant tCMS     : time := 10 ns;
  constant tHAS     : time :=  7 ns;
  constant tHAH     : time :=  4 ns;
  constant tHAD     : time := 15 ns; -- instead of 18
  constant tHAE     : time := 15 ns; -- instead of 18
  constant tERD     : time := 25 ns;
  constant tERH     : time :=  7 ns;
   
  constant tTCY     : time := 100 ns;  
  constant tTMS     : time := 20 ns;  
  constant tTMH     : time :=  8 ns;  
  constant tTDIS    : time := 20 ns;  
  constant tTDIH    : time :=  8 ns;  
  constant tTRS     : time := 20 ns;  
  constant tTRH     : time :=  8 ns;  
  constant tTDOD    : time := 30 ns;   
  constant tTDOH    : time := 12 ns; 

end FPURTTimPar;
-------------------------------------------------------------------------------
-- File name : fpurt_comp_pck.vhd
-- Title : FPURTCompPck
-- project : SPARC
-- Library : FPURTLIB
-- Author(s) : Maxime ROCCA
-- Purpose : package to declare components of entities for the FPURT.
-- 
-- notes : Use this package whenever a component is instanciated.
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        |        | 94-03-04    | first version
--.............................................................................
-------------------------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
-------------------------------------------------------------------------------
---------|---------|---------|---------|---------|---------|---------|--------|

library IEEE;
use IEEE.Std_Logic_1164.all;
library MMS;
use MMS.StdSim.all;

package FPURTCompPck is

  component FPURTGeneric
    generic( -- Fake default timing values
      tCY      : time := 50 ns; -- Clock cycle
      tCHL     : time := 22 ns; -- CLock High and Low
      tAS      : time := 5 ns; -- A input setup
      tAH      : time := 1 ns; -- A input hold
      tDIS     : time := 5 ns; -- D input setup
      tDIH     : time := 1 ns; -- D input hold
      tDOD     : time := 7 ns; -- D output delay
      tDOH     : time := 6 ns; -- D data valid
      tDOFFL   : time := 7 ns; -- D output turn-off (FLUSH+)
      tDOHFL   : time := 6 ns; -- D output valid (FLUSH+)
      tDOFOE   : time := 7 ns; -- D output turn-off (DOE_N+)
      tDONOE   : time := 7 ns; -- D output turn-on (DOE_N-)
      tDOHOE   : time := 6 ns; -- D output valid (DOE_N-)
      tFIS     : time := 5 ns; -- FINS1/2 input setup
      tFIH     : time := 1 ns; -- FINS1/2 input hold
      tINS     : time := 5 ns; -- INST input setup
      tINH     : time := 1 ns; -- INST input hold
      tFXS     : time := 5 ns; -- FXACK input setup
      tFXH     : time := 1 ns; -- FXACK input hold
      tFLS     : time := 5 ns; -- FLUSH input setup
      tFLH     : time := 1 ns; -- FLUSH input hold
      tRES     : time := 5 ns; -- RESET_N input setup
      tREH     : time := 1 ns; -- RESET_N input hold
      tMHS     : time := 5 ns; -- MHOLD_N input setup
      tMHH     : time := 1 ns; -- MHOLD_N input hold
      tMDS     : time := 5 ns; -- MDS_N input setup
      tMDH     : time := 1 ns; -- MDS_N input hold
      tFHD     : time := 7 ns; -- FHOLD_N output delay
      tFHH     : time := 6 ns; -- FHOLD_N output valid
      tFHDFI   : time := 7 ns; -- FHOLD_N output delay (FINS1/2+)
      tFHDFL   : time := 7 ns; -- FHOLD_N output delay (FLUSH+)
      tFHDMH   : time := 7 ns; -- FHOLD_N output delay (MHOLD_N-)
      tFCCVD   : time := 7 ns; -- FCCV output delay
      tFCCVH   : time := 6 ns; -- FCCV output valid
      tFCCVDFL : time := 7 ns; -- FCCV output delay (FLUSH+)
      tFCCVDMH : time := 7 ns; -- FCCV output delay (MHOLD_N-)
      tFCCD    : time := 7 ns; -- FCC output delay
      tFCCH    : time := 6 ns; -- FCC output valid
      tFED     : time := 7 ns; -- FEXC_N output delay
      tFEH     : time := 6 ns; -- FEXC_N output valid
      tFND     : time := 7 ns; -- FNULL output delay
      tFNH     : time := 6 ns; -- FNULL output valid
      tAPS     : time := 7 ns; -- APAR input setup
      tAPH     : time := 6 ns; -- APAR input hold
      tDPIS    : time := 7 ns; -- DPAR input setup
      tDPIH    : time := 6 ns; -- DPAR input hold
      tDPOD    : time := 7 ns; -- DPAR output delay
      tDPOH    : time := 6 ns; -- DPAR output valid
      tIFS     : time := 7 ns; -- IFPAR input setup
      tIFH     : time := 6 ns; -- IFPAR input hold
      tFIPD    : time := 7 ns; -- FIPAR output delay
      tFIPH    : time := 6 ns; -- FIPAR output valid
      tMCD     : time := 7 ns; -- MCERR_N output delay
      tMCH     : time := 6 ns; -- MCERR_N output valid
      tCMS     : time := 5 ns; -- N602MODE_N, CMODE_N input setup
      tHAS     : time := 5 ns; -- HALT_N input setup
      tHAH     : time := 1 ns; -- HALT_N input hold
      tHAD     : time := 7 ns; -- HALT_N asserted to output disable delay
      tHAE     : time := 7 ns; -- HALT_N asserted to output enable delay
      tERD     : time := 7 ns; -- HWERROR_N output delay
      tERH     : time := 6 ns; -- HWERROR_N output valid
    
      tTCY     : time := 50 ns; -- TCLK Clock Cycle
      tTMS     : time := 5 ns; -- TMS setup
      tTMH     : time := 1 ns; -- TMS hold
      tTDIS    : time := 5 ns; -- TDI setup
      tTDIH    : time := 1 ns; -- TDI hold
      tTRS     : time := 5 ns; -- TRST_N setup
      tTRH     : time := 1 ns; -- TRST_N hold
      tTDOD    : time := 7 ns; -- TDO output delay
      tTDOH    : time := 6 ns  -- TDO output valid
    );
  
    port(
      Clk   : in std_logic; -- clock signal
    
      -- Integer Unit Interface Signals
      FP_N    : inout std_logic; --* Floating-point (Fp) Present
      FCC     : inout std_logic_vector(1 downto 0); --* Fp Condition Codes
      FCCV    : inout std_logic; --* Fp Condition Codes Valid
      FHOLD_N : inout std_logic; --* Fp Hold
      FEXC_N  : inout std_logic; --* Fp EXCeption
      FIPAR   : inout std_logic; --* Fpu to Iu control PARity
      FXACK   : in std_logic; -- Fp eXception ACKnowledge
      INST    : in std_logic; -- INSTruction fetch
      FINS1   : in std_logic; -- Fp INStruction in buffer 1
      FINS2   : in std_logic; -- Fp INStruction in buffer 2
      FLUSH   : in std_logic; -- Fp instruction fLUSH
      IFPAR   : in std_logic; -- Iu to Fpu control PARity
    
      -- System/Memory Interface Signals
      A          : in std_logic_vector(31 downto 0); -- Address bus
      APAR       : in std_logic; -- Address bus PARity
      D          : inout std_logic_vector(31 downto 0); -- Data bus
      DPAR       : inout std_logic; -- Data bus PARity
      DOE_N      : in std_logic;  -- Data Output Enable
      COE_N      : in std_logic;  -- Control Output Enable
      MHOLDA_N   : in std_logic;  -- Memory HOLD
      MHOLDB_N   : in std_logic;  -- Memory HOLD
      BHOLD_N    : in std_logic;  -- Bus HOLD
      MDS_N      : in std_logic;  -- Memory Data Strobe
      FNULL      : inout std_logic; --* Fpu NULLify cycle
      RESET_N    : in std_logic;  -- Reset signal
      HWERROR_N  : out std_logic; --Hardware error detected
      CMODE_N    : in std_logic;  -- master/Checker MODE
      MCERR_N    : out std_logic; -- Comparison Error
      N602MODE_N : in std_logic;  -- Normal 602MODE Operation
      HALT_N     : in std_logic;  -- Halt mode
    
      -- Coprocessor Interface Signals
      CHOLD_N  : in std_logic; -- Coprocessor hold.
      CCCV     : in std_logic; -- Coprocessor Condition Code Valid.

      -- Test Access Port (TAP) signals
      TCLK   : in std_logic;  -- Test CLocK
      TRST_N : in std_logic;  -- Test ReSeT
      TMS    : in std_logic;  -- Test Mode Select
      TDI    : in std_logic;  -- Test Data In
      TDO    : out std_logic  -- Test Data Out
    );
  end component; -- FPURTGeneric
  
  component FPURT
    generic(
          T : temperature := T_BOARD;
          V : voltage := V_BOARD;
          PROCES : proces_type := PROCES_BOARD;
          LOAD : capacitance := LOAD_BOARD
    );
  
    port(
      Clk   : in std_logic; -- clock signal
    
      -- Integer Unit Interface Signals
      FP_N    : inout std_logic; --* Floating-point (Fp) Present
      FCC     : inout std_logic_vector(1 downto 0); --* Fp Condition Codes
      FCCV    : inout std_logic; --* Fp Condition Codes Valid
      FHOLD_N : inout std_logic; --* Fp Hold
      FEXC_N  : inout std_logic; --* Fp EXCeption
      FIPAR   : inout std_logic; --* Fpu to Iu control PARity
      FXACK   : in std_logic; -- Fp eXception ACKnowledge
      INST    : in std_logic; -- INSTruction fetch
      FINS1   : in std_logic; -- Fp INStruction in buffer 1
      FINS2   : in std_logic; -- Fp INStruction in buffer 2
      FLUSH   : in std_logic; -- Fp instruction fLUSH
      IFPAR   : in std_logic; -- Iu to Fpu control PARity
    
      -- System/Memory Interface Signals
      A          : in std_logic_vector(31 downto 0); -- Address bus
      APAR       : in std_logic; -- Address bus PARity
      D          : inout std_logic_vector(31 downto 0); -- Data bus
      DPAR       : inout std_logic; -- Data bus PARity
      DOE_N      : in std_logic;  -- Data Output Enable
      COE_N      : in std_logic;  -- Control Output Enable
      MHOLDA_N   : in std_logic;  -- Memory HOLD
      MHOLDB_N   : in std_logic;  -- Memory HOLD
      BHOLD_N    : in std_logic;  -- Bus HOLD
      MDS_N      : in std_logic;  -- Memory Data Strobe
      FNULL      : inout std_logic; --* Fpu NULLify cycle
      RESET_N    : in std_logic;  -- Reset signal
      HWERROR_N  : out std_logic; -- Hardware error detected
      CMODE_N    : in std_logic;  -- master/Checker MODE
      MCERR_N    : out std_logic; -- Comparison Error
      N602MODE_N : in std_logic;  -- Normal 602MODE Operation
      HALT_N     : in std_logic;   -- Halt mode
    
      -- Coprocessor Interface Signals
      CHOLD_N  : in std_logic; -- Coprocessor hold.
      CCCV     : in std_logic; -- Coprocessor Condition Code Valid.

      -- Test Access Port (TAP) signals
      TCLK   : in std_logic;  -- Test CLocK
      TRST_N : in std_logic;  -- Test ReSeT
      TMS    : in std_logic;  -- Test Mode Select
      TDI    : in std_logic;  -- Test Data In
      TDO    : out std_logic  -- Test Data Out
    );
  end component; -- FPURT
  
end FPURTCompPck;
-------------------------------------------------------------------------------
-- File name : fpurt_pck.vhd
-- Title : FPURTPck
-- project : SPARC
-- Library : FPURTLIB
-- Author(s) : Maxime ROCCA, Jiri Gaisler
-- Purpose : 
--    Package containing SPARC FPURT specific VHDL constructs.
--
-- notes :   
--    To be included when anything defined in this file is used 
--    in another file.
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        |   MR   | 94-03-04    | first version
--.............................................................................
-- v 1.1        |   MR   | 94-05-03    | 2nd version
-- + VHDL bugs fixed.
--.............................................................................
-- v 1.2        |   JG   | 94-09-23    | 3nd version
-- Completely new implementation. Replaced MEIKO derived code with behavioral 
-- to allow free distribution. FPops with NaN as input do NOT behave as real 
-- device, but this shouldn't matter...
-------------------------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
-- Copyright ESA/ESTEC
-------------------------------------------------------------------------------
---------|---------|---------|---------|---------|---------|---------|--------|

library IEEE;
use IEEE.Std_Logic_1164.all;
use ieee.Std_logic_arith.all;

library SPARC_LIB;
use SPARC_LIB.SparcPck.all;

use STD.TEXTIO.all;

package FPURTPck is

  -- Pseudo-functions that are constant tables:
  -- IsFPopRdDouble(Mnemonic) returns TRUE if Mnemonic is a FPop with rd double.
  -- IsFPopSourceRegDouble(Mnemonic) returns TRUE if Mnemonic is a FPop with 
  -- double precision source registers.
  -- IsFPopUnimp(Mnemonic) returns TRUE if Mnemonic is an unimplemented FPop.
  constant IsFPopRdDouble : MnemoTableType;
  constant IsFPopSourceRegDouble : MnemoTableType;
  constant IsFPopUnimp : MnemoTableType;
  
  -- Pseudo-function that emulates the ROM microcode for the computational core
  -- of the FPU. It returns a 64-bit data corresponding to the address (i.e the
  -- index in the table ranging between 0 and 255).
      
  -- Floating-point instruction type.
  type FPInstruction is record
    Mnemo          : SuperInstMnemonic; -- menmonic of a FP instruction.
    BitInstruction : std_logic_vector(31 downto 0); -- 32-bit value for the
                                                    -- instruction.
    BitAddress     : std_logic_vector(31 downto 0); -- 32-bit value for the
                                                    -- address.
    rs1 : natural; -- source register 1.
    rs2 : natural; -- source register 2.
    rd  : natural; -- destination register.
  end record; -- FPInstruction
  
  -- Modes of the FPU
  type FPUmodeType is (RESET_MODE, ERROR_MODE, 
                       EXECUTION, 
                       PENDING_EXCEPTION, EXCEPTION);

  -- Floating-point exception types
  type FPexcMnemonic is (
    FP_EXC_DETECTED,    -- FP exception has been detected
    IEEE_EXC,           -- IEEE 754 exception
    UNFINISHED_FPOP,    -- Unfinished FPop exception
    UNIMPLEMENTED_FPOP, -- Unimplemented FPop exception
    SEQUENCE_ERROR,     -- Sequence error exception
    DATA_BUS_ERROR,     -- Data bus error
    RESTARTABLE_ER      -- Restartable error
  );
  
  type FPexcVectorType is array(FPexcMnemonic) of boolean;
  
  
  --=================== FUNCTIONS declarations ====================
  
  ---------------------------------------------------------------------------
  -- Decode A as a SPARC instruction and returns the info under the form of
  -- a FPinstruction record.
  ---------------------------------------------------------------------------
  function FPtranscribe(A : std_logic_vector) return FPInstruction;
  
  ---------------------------------------------------------------------------
  -- Returns a non null value if a condition to generate the FHOLD_N signal is 
  -- encountered.
  ---------------------------------------------------------------------------
  function FHOLDcondition(signal FINS1 : std_logic;
                          signal FINS2 : std_logic;
                          signal ID1   : FPInstruction;
                          signal ID2   : FPInstruction;
                          signal E     : FPInstruction;
                          signal W     : FPInstruction;
                          W1           : FPInstruction;
                          signal FQ    : FPInstruction
                         ) return natural;
  
  ---------------------------------------------------------------------------
  -- Used in FHOLDcondition: dependency between FPop and FP load.
  ---------------------------------------------------------------------------
  function FPop_LDF(signal FPinst : FPInstruction;
                    RegD          : natural
                   ) return boolean;

  ---------------------------------------------------------------------------
  -- Used in FHOLDcondition: dependency between FPop and FP store.
  ---------------------------------------------------------------------------
  function FPop_STF(signal FPinst : FPInstruction;
                    RegD          : natural
                   ) return boolean;

  ---------------------------------------------------------------------------
  -- Used in FHOLDcondition: dependency between FP load and FPop.
  ---------------------------------------------------------------------------
  function LDF_FPop(signal W : FPInstruction;
                    ID       : FPInstruction
                   ) return boolean;

  ---------------------------------------------------------------------------
  -- Used in FHOLDcondition: dependency betwee FP load double and FPop.
  ---------------------------------------------------------------------------
  function LDDF_FPop(W1 : FPInstruction;
                     ID : FPInstruction
                    ) return boolean;



  ---------------------------------------------------------------------------
  -- Execution body for the FPURT: instructions are dispatched, executed, and
  -- the procedure returns the result together with the number of cycles it
  -- takes to get it. This procedure is the computational core of the FPU.
  ---------------------------------------------------------------------------
  procedure ExecuteFPop(FPbitInst   : std_logic_vector;
                        RS1vec      : std_logic_vector;
                        RS2vec      : std_logic_vector;
                        RD          : std_logic_vector;
                        TEM2        : std_logic;
                        DEBUG_FLAG  : boolean := FALSE; -- debugging purpose.
                        TestNb      : integer; -- debugging purpose.
                        Result      : out std_logic_vector;
                        tfcc        : out std_logic_vector;
                        texc        : out std_logic_vector;
                        FPexcVector : out FPexcVectorType;
                        NbCycles    : out integer;
                        Fdebug      : out text; -- debugging purpose.
                        FCtDebug    : out text  -- debugging purpose.
                       );
  
end FPURTPck; -- package

--------------------------------------------------------------------------
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^--
--------------------------------------------------------------------------

package body FPURTPck is

  subtype single is unsigned(31 downto 0);
  subtype double is unsigned(63 downto 0);
  subtype fpexc is std_logic_vector(4 downto 0);


  ------------------------------
  constant IsFPopRdDouble : MnemoTableType := (
    FiTOd  => TRUE, FsTOd  => TRUE, FqTOd  => TRUE, FSQRTd => TRUE, 
    FADDd  => TRUE, FSUBd  => TRUE, FMULd  => TRUE, 
    FsMULd => TRUE, -- SPARC v.8 only for FsMULd 
    FDIVd  => TRUE,
    others => FALSE
  );
  
  ------------------------------
  constant IsFPopSourceRegDouble : MnemoTableType := (
    FdTOi  => TRUE, FdTOs  => TRUE, FdTOq  => TRUE, FSQRTd => TRUE, 
    FADDd  => TRUE, FSUBd  => TRUE, FMULd  => TRUE, 
    FdMULq => TRUE, -- SPARC v.8 only for FsMULd & FdMULq
    FDIVd  => TRUE, FCMPd  => TRUE, FCMPEd => TRUE, 
    others => FALSE
  );
  
  ------------------------------
  constant IsFPopUnimp : MnemoTableType := (
    FiTOq  => TRUE, FqTOi  => TRUE, FsTOq  => TRUE, FdTOq  => TRUE, 
    FqTOs  => TRUE, FqTOd  => TRUE, FSQRTq => TRUE, FADDq  => TRUE, 
    FSUBq  => TRUE, FMULq  => TRUE,
    FsMULd => TRUE, FdMULq => TRUE, -- SPARC v.8 only for FsMULd & FdMULq
    FDIVq  => TRUE, FCMPq  => TRUE, FCMPEq => TRUE,
    others => FALSE
  );
  
  ------------------------------
  function FPtranscribe(A : std_logic_vector) return FPInstruction is
    constant L : natural := A'length;
    variable Inst   : Instruction;
    variable Result : FPInstruction;
  begin
    assert L = 32 report "(FPtranscribe): invalid vector length!"
                  severity error;
    Inst := Transcribe(A);
    Result.Mnemo := Inst.Mnemo;
    Result.rs1 := Inst.rs1;
    Result.rs2 := Inst.rs2;
    Result.rd := Inst.rd;
    Result.BitInstruction := A;
    
    return Result;
    
  end FPtranscribe; -- function
  
  ------------------------------
  function FPop_LDF(signal FPinst : FPInstruction;
                    RegD          : natural
                   ) return boolean is
  begin
    if IsFPop(FPinst.Mnemo) and 
       (FPinst.rs2 = RegD or 
        (FPURs1IsIn(FPinst.Mnemo) and FPinst.rs1 = RegD) or
        (not(IsFCMP(FPinst.Mnemo)) and FPinst.rd = RegD) or 
        (IsFPopRdDouble(FPinst.Mnemo) and ( ((FPinst.rd/2)*2) = RegD or 
                                            ((FPinst.rd/2)*2 + 1) = RegD 
                                          ) 
        ) or
        (IsFPopSourceRegDouble(FPinst.Mnemo) and 
          ( ((FPinst.rs2/2)*2) = RegD or ((FPinst.rs2/2)*2 + 1) = RegD or
            (FPURs1IsIn(FPinst.Mnemo) and ( ((FPinst.rs1/2)*2) = RegD or 
                                            ((FPinst.rs1/2)*2 + 1) = RegD
                                          ) 
            )
          )
        )
       ) then
      return TRUE;
    end if;
    
    return FALSE;
    
  end FPop_LDF;
  
  ------------------------------
  function FPop_STF(signal FPinst : FPInstruction;
                    RegD          : natural
                   ) return boolean is
  begin
    if IsFPop(FPinst.Mnemo) and
        ((not(IsFCMP(FPinst.Mnemo)) and FPinst.rd = RegD) or
         (IsFPopRdDouble(FPinst.Mnemo) and ( ((FPinst.rd/2)*2) = RegD or 
                                             ((FPinst.rd/2)*2 + 1) = RegD 
                                           )
         )
        ) then
      return TRUE;
    end if;
    
    return FALSE;
    
  end FPop_STF;

  ------------------------------
  function LDF_FPop(signal W : FPInstruction;
                    ID       : FPInstruction
                   ) return boolean is
  begin
    if W.Mnemo = LDF and
       (W.rd = ID.rs2 or 
        (FPURs1IsIn(ID.Mnemo) and W.rd = ID.rs1) or
        (IsFPopSourceRegDouble(ID.Mnemo) and 
              (W.rd = ((ID.rs2/2)*2) or 
               W.rd = ((ID.rs2/2)*2 + 1) or
               (FPURs1IsIn(ID.Mnemo) and (W.rd = ((ID.rs1/2)*2) or
                                          W.rd = ((ID.rs1/2)*2 + 1) ) 
               )
              ) 
        )
       ) then
      return TRUE;
    end if;
    
    return FALSE;
    
  end LDF_FPop;
  
  ------------------------------
  function LDDF_FPop(W1 : FPInstruction;
                     ID : FPInstruction
                    ) return boolean is
  begin
    if W1.Mnemo = LDDF and -- dependency on both rd and rd+1 of LDDF
       ((W1.rd = ID.rs2 or 
         (FPURs1IsIn(ID.Mnemo) and W1.rd = ID.rs1) or
         (IsFPopSourceRegDouble(ID.Mnemo) and 
                 (W1.rd = ((ID.rs2/2)*2) or 
                  W1.rd = ((ID.rs2/2)*2 + 1) or
                  (FPURs1IsIn(ID.Mnemo) and (W1.rd = ((ID.rs1/2)*2) or
                                             W1.rd = ((ID.rs1/2)*2 + 1) ) 
                  )
                 ) 
         ) 
        ) or
        (W1.rd+1 = ID.rs2 or 
         (FPURs1IsIn(ID.Mnemo) and W1.rd+1 = ID.rs1) or
         (IsFPopSourceRegDouble(ID.Mnemo) and 
                 (W1.rd+1 = ((ID.rs2/2)*2) or 
                  W1.rd+1 = ((ID.rs2/2)*2 + 1) or
                  (FPURs1IsIn(ID.Mnemo) and (W1.rd+1 = ((ID.rs1/2)*2) or
                                             W1.rd+1 = ((ID.rs1/2)*2 + 1) ) 
                  )
                 ) 
         )
        )
       ) then
      return TRUE;
    end if;
    
    return FALSE;
    
  end LDDF_FPop;

  ------------------------------
  function FHOLDcondition(signal FINS1 : std_logic;
                          signal FINS2 : std_logic;
                          signal ID1   : FPInstruction;
                          signal ID2   : FPInstruction;
                          signal E     : FPInstruction;
                          signal W     : FPInstruction;
                          W1           : FPInstruction;
                          signal FQ    : FPInstruction
                         ) return natural is
    variable ID      : FPInstruction;
    variable W1rd_ev : natural;
    variable W1rd_od : natural;
    variable IDrd_ev : natural;
    variable IDrd_od : natural;
  begin
    if FINS1 = '1' then
      ID := ID1;
    elsif FINS2 = '1' then
      ID := ID2;
    else 
      return 0;
    end if;
    
    
    ----
    if (IsFPop(ID.Mnemo) or ID.Mnemo = LDFSR or ID.Mnemo = STFSR) and 
       (IsFPop(E.Mnemo) or IsFPop(W.Mnemo) or IsFPop(FQ.Mnemo)) then
      return 1;
    end if;
    
    ----
    if (ID.Mnemo = LDF or ID.Mnemo = LDDF) and 
       ( FPop_LDF(E, ID.rd) or FPop_LDF(W, ID.rd) or FPop_LDF(FQ, ID.rd)
       ) then
      return 1;
    end if;
    
    if ID.Mnemo = LDDF and 
       ( FPop_LDF(E, ID.rd + 1) or FPop_LDF(W, ID.rd + 1) or
         FPop_LDF(FQ, ID.rd + 1)
       ) then
      return 1;
    end if;
    
    ----
    if ID.Mnemo = STF and
       (FPop_STF(E, ID.rd) or FPop_STF(W, ID.rd) or FPop_STF(FQ, ID.rd)
       ) then
      return 1;
    end if;
    
    IDrd_ev := (ID.rd/2)*2;
    IDrd_od := IDrd_ev + 1;
    if ID.Mnemo = STDF and
       (FPop_STF(E, IDrd_ev) or FPop_STF(W, IDrd_ev) or FPop_STF(FQ, IDrd_ev) or
        FPop_STF(E, IDrd_od) or FPop_STF(W, IDrd_od) or FPop_STF(FQ, IDrd_od)
       ) then
      return 1;
    end if;

    ---
    if ID.Mnemo = STDFQ and
       (IsFPop(E.Mnemo) or IsFPop(W.Mnemo) or IsFPop(FQ.Mnemo)) then
      return 1;
    end if;
    
    ----
    if IsFPinst(ID.Mnemo) and not(IsFBfcc(ID.Mnemo)) and 
       (IsFPopUnimp(E.Mnemo) or IsFPopUnimp(W.Mnemo) or 
        IsFPopUnimp(FQ.Mnemo)) then
      return 1;
    end if;

    ----
    if (W.Mnemo = LDFSR or W1.Mnemo = LDFSR) and
       (ID.Mnemo = LDFSR or ID.Mnemo = STFSR or IsFPop(ID.Mnemo)) then
      return 2;
    end if;

    ---- $$$$ WARNING: this condition is fuzzy: to be finalized. $$$$$$$
    W1rd_ev := (W1.rd/2)*2;
    W1rd_od := W1rd_ev + 1;
    IDrd_ev := (ID.rd/2)*2;
    IDrd_od := IDrd_ev + 1;
    if (IsFPop(ID.Mnemo) and (LDF_FPop(W, ID) or LDDF_FPop(W1, ID)) ) or
       (ID.Mnemo = STF and W.Mnemo = LDF and W.rd = ID.rd) or
       (ID.Mnemo = STF and W1.Mnemo = LDDF and (ID.rd = W1rd_ev or 
                                                ID.rd = W1rd_od) ) or
       (ID.Mnemo = STDF and W.Mnemo = LDF and (W.rd = IDrd_ev or
                                               W.rd = IDrd_od) ) or 
       (ID.Mnemo = STDF and W1.Mnemo = LDDF and (W1rd_ev = IDrd_ev or
                                                 W1rd_ev = IDrd_od or
                                                 W1rd_od = IDrd_ev or
                                                 W1rd_od = IDrd_od) ) then
      return 2;
    end if;

    return 0;
      
  end FHOLDcondition;



  -----------------------------
  -- Behavioral FP functions to replace structural model
  -- Jiri Gaisler, 19-09-1996
  ------------------------------

  procedure ExecuteFPop(FPbitInst   : std_logic_vector;
                        RS1vec      : std_logic_vector;
                        RS2vec      : std_logic_vector;
                        RD          : std_logic_vector;
                        TEM2        : std_logic;
                        DEBUG_FLAG  : boolean := FALSE;
                        TestNb      : integer;
                        Result      : out std_logic_vector;
                        tfcc        : out std_logic_vector;
                        texc        : out std_logic_vector;
                        FPexcVector : out FPexcVectorType;
                        NbCycles    : out integer;
                        Fdebug      : out text;
                        FCtDebug    : out text
                       ) is

    type Ftype is (QNan, SNan, Zero, Inf, Norm);

    type Quad is record
	sign : std_logic;
	exp  : unsigned(14 downto 0);
	man  : unsigned(112 downto 0);
	v  : Ftype;
    end record;

    constant SPInf : std_logic_vector(30 downto 0) :=
	"1111111100000000000000000000000";
    constant DPInf : std_logic_vector(62 downto 0) :=
	"111111111110000000000000000000000000000000000000000000000000000";
    constant SPNaN : std_logic_vector(31 downto 0) :=
	"01111111111111110000000000000000";
    constant DPNaN : std_logic_vector(63 downto 0) :=
	"0111111111111111111000000000000000000000000000000000000000000000";
    variable cexc : std_logic_vector(4 downto 0);
    variable dres : std_logic_vector(63 downto 0);
    variable ns : boolean;
    variable qs1, qs2, qres : quad;

    function SIsInf(f1 : std_logic_vector) return boolean is
    begin
	return ((f1(30 downto 23) = std_logic_vector'("11111111")) and 
	        (f1(22 downto 0) = "00000000000000000000000"));
    end SIsInf;

    function DIsInf(f1 : std_logic_vector) return boolean is
    begin
	return ((f1(62 downto 52) = std_logic_vector'("11111111111")) and 
	        (f1(51 downto 0) = "0000000000000000000000000000000000000000000000000000"));
    end DIsInf;

    function SIsZero(f1 : std_logic_vector) return boolean is
    begin
	return (f1(30 downto 0) = ext("0",31));
    end SIsZero;

    function DIsZero(f1 : std_logic_vector) return boolean is
    begin
	return (f1(62 downto 0) = ext("0",63));
    end DIsZero;

    function SIsNaN(f1 : std_logic_vector) return boolean is
    begin
	return ((f1(30 downto 23) = std_logic_vector'("11111111")) and 
	        (f1(22 downto 0) /= "00000000000000000000000"));
    end SIsNaN;

    function DIsNaN(f1 : std_logic_vector) return boolean is
    begin
	return ((f1(62 downto 52) = std_logic_vector'("11111111111")) and 
	        (f1(51 downto 0) /= "0000000000000000000000000000000000000000000000000000"));
    end DIsNaN;

    function SToQuad(f1: std_logic_vector) return Quad is
    variable q1 : Quad;
    begin
	q1.sign := f1(31);
	q1.exp := unsigned(ext(f1(30 downto 23),15)) + 16383 - 127;
	q1.man := unsigned(ext(('1'& f1(22 downto 0) & 
				"00000000000000000000000000000000"),113));
	q1.v := Norm;
	if  SIsNaN(f1) then
	    if f1(22) = '1' then
	        q1.v := QNaN;
	    else
	        q1.v := SNaN;
	    end if;
	elsif SIsInf(f1) then
	    q1.v := Inf;
        elsif SIsZero(f1) then
	    q1.v := Zero;
	end if;
	return(q1);
    end SToQuad;

    function DToQuad(f1: std_logic_vector) return Quad is
    variable q1 : Quad;
    begin
	q1.sign := f1(63);
	q1.exp := unsigned(ext(f1(62 downto 52),15)) + 16383 - 1023;
	q1.man := unsigned(ext(('1'& f1(51 downto 0) & "000"),113));
	q1.v := Norm;
	if  DIsNaN(f1) then
	    if f1(51) = '1' then
	        q1.v := QNaN;
	    else
	        q1.v := SNaN;
	    end if;
	elsif DIsInf(f1) then
	    q1.v := Inf;
        elsif DIsZero(f1) then
	    q1.v := Zero;
	end if;
	return(q1);
    end DToQuad;

    procedure Normalize(q1 : inout quad) is
    variable i : integer := 0;
    begin
	if q1.v = norm then
	    while (i < 113) and (q1.man(112 - i) = '0') loop
	        i := i + 1;
	    end loop;
 	    if (i < 57) then
	        q1.man := shr(q1.man, conv_unsigned(57 - i,8));
	    else
	        q1.man := shl(q1.man, conv_unsigned(i - 57 ,8));
	    end if;
	    q1.exp := q1.exp + (57 - i);
	end if;
    end Normalize;

    procedure FPround(q : quad; res : inout std_logic_vector; SP : boolean) is
    variable i, intexp, iexp, isign, exprange, ig : integer;
    variable utmp : unsigned (14 downto 0);
    variable Pround : unsigned (112 downto 0) := (others => '0');
    variable q1 : quad := q;
    variable lexp, lman : integer;
    variable RDL : std_logic_vector(1 downto 0);
    begin
	if q1.v = Inf then
	    if SP then
		res(31 downto 0) := q1.sign & SPInf;
	    else
		res(63 downto 0) := q1.sign & DPInf;
	    end if;
	elsif q1.v = Zero then
	    if SP then
		res(31 downto 0) := q1.sign & ext("0",31);
	    else
		res(63 downto 0) := q1.sign & ext("0",63);
	    end if;
	elsif q1.v <= SNan then
	    if SP then
		res(31 downto 0) := SPNan;
	    else
		res(63 downto 0) := DPNan;
	    end if;
	else
	    if SP then
	        lexp := 8;
	        lman := 23;
	    else
	        lexp := 11;
	        lman := 52;
	    end if;
	    ig := 54 -lman;
	    Pround(ig) := '1';
	    isign := lman + lexp;
	    iexp := isign - 1;
	    exprange := 2;
	    for i in 2 to lexp loop
	        exprange := exprange * 2;
	    end loop;
	    exprange := exprange;
  	    i := 0;

	-- First, we normalize to quad

	    Normalize(q1);

	-- Round according to RD

            RDL := RD( 1 downto 0);

	    if ((q1.man(ig downto 1)) /= unsigned(ext("0",ig-1))) or
		cexc(0) = '1'
	     then
	        cexc(0) := '1';		-- Inexact ieee flag
	        case RDL(1 downto 0) is
		    when "00" =>		-- round to nearest
		 	if ((q1.man(ig downto 0)) > 
			     unsigned('1' & ext("0",ig))
		 	   ) or
			   (((q1.man(ig downto 0)) = 
			      unsigned('1' & ext("0",ig))
			     ) and 
			     (q1.man(ig+1) = '1')
			   ) then
			    q1.man := q1.man + Pround;
			    Normalize(q1);
			end if;
		    when "01" =>		-- round to zero (truncate)
			null;
		    when "10" =>		-- round to +Inf
			if q1.sign = '0' then
			    q1.man := q1.man + shl(Pround,"01");
			end if;
		    when "11" =>		-- round to -Inf
			if q1.sign = '1' then
			    q1.man := q1.man + shl(Pround,"01");
			end if;
		    when others =>
			null;
		end case;
	    end if;

	    res(isign) := q1.sign;
	    utmp := q1.exp + (-16383 + (exprange / 2) -1);
	    res(iexp downto lman) := std_logic_vector(utmp((lexp-1) downto 0));
	    res(lman-1 downto 0) := std_logic_vector(q1.man(54 downto (54 -lman +1)));

	-- Check ieee exceptions, improvements here are welcome
	    intexp := conv_integer(q1.exp);
	    if (intexp > ( 16383 + exprange/2 -1)) then
	        cexc(3) := '1';				-- overflow
		case RDL(1 downto 0) is
		    when "00" =>	-- round to nearest, return 1
	        	res(iexp downto lman) := '0' & sxt("1",lexp - 1);
	        	res(lman-1 downto 0) := ext("0",lman);
		    when "01" =>	-- round to zero, return largest norm
	        	res(iexp downto lman) := sxt("1",lexp - 1) & '0';
	        	res(lman-1 downto 0) := sxt("1",lman);
		    when "10" =>	-- round to +Inf
			if res(isign) = '1' then 	-- most negitive norm
	        	    res(iexp downto lman) := sxt("1",lexp - 1) & '0';
	        	    res(lman-1 downto 0) := sxt("1",lman);
			else				-- +Inf
	        	    res(iexp downto lman) := sxt("1",lexp);
	        	    res(lman-1 downto 0) := sxt("0",lman);
			end if;
		    when "11" =>	-- round to -Inf
			if res(isign) = '0' then 	-- most positive norm
	        	    res(iexp downto lman) := sxt("1",lexp - 1) & '0';
	        	    res(lman-1 downto 0) := sxt("1",lman);
			else				-- -Inf
	        	    res(iexp downto lman) := sxt("1",lexp);
	        	    res(lman-1 downto 0) := sxt("0",lman);
			end if;
		    when others =>
			null;
		end case;
	    elsif (intexp < (16383 - exprange/2 + 2)) then 
	        cexc(2) := '1';		-- underflow, return denorm
	        i := 16383 - intexp + (exprange / 2) - 1;
		res(iexp downto lman) := ext("0",lexp);
		res(lman-1 downto 0) := 
		    std_logic_vector(shr(q1.man(55 downto 55 - lman +1),(conv_unsigned(i,8))));
	    end if;
	end if;
	if (SP) then
	    res (63 downto 32) := res(31 downto 0);
	end if;
    end FPround;
		
	
    procedure fmul(rs1, rs2: Quad; res: out Quad) is
    variable q1: Quad;
    begin
	res.sign := rs1.sign xor rs2.sign;
	if (rs1.v = Zero) or (rs2.v = Zero) then
	    res.v := Zero;
	elsif (rs1.v = Inf) or (rs2.v = Inf) then
	    res.v := Inf;
	else
	  res.v := Norm;
	  res.exp := (rs1.exp - 16383) + rs2.exp - 55;
	  res.man := '0' & (rs1.man(55 downto 0) * rs2.man(55 downto 0));
	end if;
    end fmul;

    procedure fdiv(rs1, rs2: Quad; res: out Quad) is
    variable q1,q2: Quad;
    variable i : integer := 55;
    begin
	res.sign := rs1.sign xor rs2.sign;
	res.exp := (rs1.exp - rs2.exp) + 16383;
	res.man := unsigned(ext("0",113));
	if (rs1.v = Zero) then
	    res.v := Zero;
	elsif  (rs2.v = Inf) then
	    res.v := Zero;
	else
	    res.v := Norm;
	    q1.man := shl(rs1.man,"0111000");
	    q2.man := shl(rs2.man,"0111000");
	    while (i > 0) loop
	        if q2.man <= q1.man then
		    q1.man := q1.man - q2.man;
		    res.man(i) := '1';
	        end if;
	        i := i - 1;
	        q2.man := shr(q2.man, "01");
	    end loop;
	    if q1.man /= unsigned(ext("0",113)) then
		cexc(0) := '1';		-- Inexact
	    end if;
	end if;
    end fdiv;

    procedure fcmp(q1, q2: quad; tfcc: out std_logic_vector) is
    variable s1, s2 : signed(127 downto 0);
    begin
	if ((q1.v = Inf) and (q2.v = Inf) and (q1.sign = q2.sign)) then
	    tfcc := "11";
	else
	    s1 := signed(q1.sign & q1.exp & q1.man(111 downto 0));
	    s2 := signed(q2.sign & q2.exp & q2.man(111 downto 0));
	    if (s1 < s2) then
	        tfcc := "01";
	    elsif (s1 > s2) then
	        tfcc := "10";
	    else
	        tfcc := "00";
	    end if;
	end if;
    end fcmp;

    procedure faddsub(rs1, rs2: quad; res: out quad; doadd : boolean) is
    variable s1, s2, exp1, exp2, man1, man2: quad;
    begin
	if (rs1.v = Inf) then
	    res := rs1;
	    if (rs2.v = Inf) then
		if ((doadd and (rs1.sign /= rs2.sign)) or
		    (not doadd and (rs1.sign = rs2.sign))) then
		    cexc(4) := '1';
		    res.v := QNan;
		    res.sign := '0';
		end if;
	    end if;
	elsif (rs2.v = Inf) then
	    res.v := Inf;
	    if doadd then
	        res.sign := rs2.sign;
	    else
	        res.sign := not rs2.sign;
	    end if;
	elsif (rs1 = rs2) then
	    res := rs1;
	    if doadd then
		res.exp := rs1.exp + 1;
	    else
	        res.v := Zero;
	        if RD = "11" then
		    res.sign := '1';
	        else
		    res.sign := '0';
	        end if;
	    end if;
	elsif (rs1.v = Zero) then
	    res := rs2;
	    if (rs2.v = norm) and not doadd then
		res.sign := not rs2.sign;
	    end if;
	elsif (rs2.v = Zero) then
	    res := rs1;
	else
            if (rs1.exp < rs2.exp) then
                s1 := rs2;
                s2 := rs1;
	        if not doadd then
		    s1.sign := not s1.sign;
		end if;
            else
                s1 := rs1;
                s2 := rs2;
	        if not doadd then
		    s2.sign := not s2.sign;
		end if;
            end if;

		
	    s2.man := shr(s2.man, s1.exp - s2.exp);
            if (s1.sign = '1') then
                s1.man := 0 - s1.man;
            end if;
            if (s2.sign = '1') then
                s2.man := 0 - s2.man;
            end if;
            s1.man := s1.man + s2.man;
	    s1.sign := s1.man(112);
            if (s1.man(112) = '1') then
                s1.man := 0 - s1.man;
            end if;
	    res := s1;
	    if s1.man = unsigned(ext("0",113)) then
		res.v := Zero;
	    end if;
	end if;
    end;

    procedure fsqrt(rs1 : quad; res: out quad; sp : boolean) is
    variable s1, s2, s3, s4, s5 : quad;
    variable accuracy : integer;
    begin
	-- Sqrt calculations according newton/rapson xn = (x2 +s1)/2x

	if (rs1.v = Norm) then

	    s1 := rs1;
	    s2 := rs1;
	    if SP then
	        accuracy := 16838 - 130;
	    else
	        accuracy := 16838 - 1026;
	    end if;
	    s2.exp := s2.exp - 1;	-- s2 is seed, equal to s1/2
	    loop

		fmul(s2,s2,s3);
	        Normalize(s3);
	        faddsub(s3, s1, s4, false);
	        Normalize(s4);
		s4.exp := s4.exp -1;
	        fdiv(s4,s2,s3);
	        Normalize(s3);
	        faddsub(s2, s3, s5, false);
	        Normalize(s5);

--		s4 := s1;
--		s4.exp := s4.exp -1;
--	        fdiv(s4,s2,s3);
--	        Normalize(s3);
--		s4 := s2;
--		s4.exp := s4.exp -1;
--	        faddsub(s3, s4, s5, true);
--	        Normalize(s5);
	        if (s5.v /= norm) or (s3.v /= norm) or (s2 = s5) then
		    exit;
		end if;
		s2 := s5;
	    end loop;
--	    if s5.man(1 downto 0) = unsigned'("11") then
--		s5.man := s5.man + 1;
--	        Normalize(s5);
--	    elsif s5.man(1 downto 0) = unsigned'("01") then
--		s5.man(0) := '0';
--	    end if;
	    cexc := "00000";
	    res := s5;
	end if;
    end;

    procedure fitoq(rs1 : std_logic_vector; res: inout quad) is
    begin
	res.sign := rs1(31);
	if (rs1(31 downto 0) = ext("0",32)) then
	    res.v := Zero;
	else
	    res.v := Norm;
	end if;
	res.exp := conv_unsigned(16383,15);
	if (rs1(31) = '1') then
	    res.man := unsigned(ext("0",26)) &
		(0 - unsigned(rs1(31 downto 0))) &
		unsigned(ext("0",55));
	else
	    res.man := unsigned(ext("0",26)) &
	     	unsigned(rs1(31 downto 0)) &
		unsigned(ext("0",55));
	end if;
	Normalize(res);
    end fitoq;
  
    procedure fqtoi(q : quad; res : inout std_logic_vector) is
    variable tmp : std_logic_vector(0 downto 0);
    variable q1 : quad;
    begin
	q1 := q;
	if (q1.v = Inf) or (q1.exp > 16383 + 30) then
	    if q1.sign = '0' then
	        cexc(4) := '1';		-- Invalid
	    end if;
	    tmp(0) := not q1.sign;
	    res(31 downto 0) := q1.sign & sxt(tmp, 31);
	elsif (q1.v = Zero) then
	    res(31 downto 0) := ext("0",32);
	else
	    if (q1.exp < 16383) then
		res(31 downto 0) := ext("0",32);
		cexc(0) := '1';		-- Inexact
	    elsif (q1.exp > 16383 + 30) then
		res(31 downto 0) := '0' & sxt("1",31);
		if q1.sign = '1' then
		    res(31 downto 0) := not res(31 downto 0);
		end if;
		cexc(0) := '1';		-- Inexact
	    else
	        q1.man := shl(q1.man,q1.exp-16383);
		if q1.man(54 downto 0) /= unsigned(ext("0",55)) then
		    cexc(0) := '1';		-- Inexact
		end if;
	        if (q1.sign = '1') then
		    q1.man(55+31 downto 55) := 0 - q1.man(55+31 downto 55);
		end if;
	        res(31 downto 0) := std_logic_vector(q1.man(55+31 downto 55));
	    end if;
	end if;
	res (63 downto 32) := res(31 downto 0);
    end fqtoi;
	

  variable FpInst : std_logic_vector(9 downto 0);
  variable LRes : std_logic_vector(63 downto 0);
  variable opcase : std_logic_vector(2 downto 0);
  variable lfcc : std_logic_vector(1 downto 0) := "11";
  variable SP : boolean;
  variable Unimp : boolean := false;
  begin
        
    FpInst := FPbitInst(19) & FPbitInst(13 downto 5);
    FPexcVector := (others => FALSE);
    SP := FPbitInst(5) = '1';
    cexc := "00000";
    tfcc := "00";

    opcase := (FPbitInst(12 downto 11) & FPbitInst(5));
    case opcase is
	when "000" | "110" =>
	    	qs2 := DToQuad(RS2vec);
		if qs2.v = QNan then 
		    LRes(63 downto 0) := RS2vec;
		elsif qs2.v = SNan then 
		    LRes(63 downto 0) := DPNan; 
		    if FPbitInst(12) = '0' then
		        cexc(4) := '1';		-- Invalid exception
		    end if;
		end if;
	when "001" | "111" =>
	    	qs2 := SToQuad(RS2vec);
		if qs2.v = QNan then 
		    LRes(63 downto 0) := RS2vec(31 downto 0) & RS2vec(31 downto 0);
		elsif qs2.v = SNan then 
		    LRes(63 downto 0) := SPNan & SPNan; 
		    if FPbitInst(12) = '0' then
		        cexc(4) := '1';		-- Invalid exception
		    end if;
		end if;
	when "010" =>
	    	qs1 := DToQuad(RS1vec);
	    	qs2 := DToQuad(RS2vec);
		if (qs1.v = SNan) or (qs2.v = SNan) then 
		    LRes(63 downto 0) := DPNan;
		    cexc(4) := '1';		-- Invalid exception
		elsif qs1.v = QNan then
		    LRes(63 downto 0) := RS1vec;
		elsif qs2.v = QNan then
		    LRes(63 downto 0) := RS2vec;
		end if;
	when "011" =>
	    	qs1 := SToQuad(RS1vec);
	    	qs2 := SToQuad(RS2vec);
		if (qs1.v = SNan) or (qs2.v = SNan) then 
		    LRes(63 downto 0) := SPNan & SPNan;
		    cexc(4) := '1';		-- Invalid exception
		elsif qs1.v = QNan then
		    LRes(63 downto 0) := RS1vec(31 downto 0) & RS1vec(31 downto 0);
		elsif qs2.v = QNan then
		    LRes(63 downto 0) := RS2vec(31 downto 0) & RS2vec(31 downto 0);
		end if;
	when others => null;
    end case;

    case FpInst is
	when "0000001001" =>		-- fabss
	    if (qs2.v > SNaN) then
	        LRes(63 downto 32) := '0' & RS2vec(30 downto 0);
	    end if;
	    NbCycles := 2;
	when "0001000001" | "0001000010" =>		-- fadds/d
	    if (qs1.v > SNan) and (qs2.v > SNaN) then
	        faddsub(qs1, qs2, qres, true);
	        FPround(qres, LRes, SP);
	    end if;
	    NbCycles := 5;
	when "1001010001" | "1001010010" =>		-- fcmps/d
	    if (qs1.v <= SNaN) or (qs2.v <= SNaN) then
	        tfcc := "11";
	    else 
	        fcmp(qs1, qs2, tfcc);
	    end if;
	    NbCycles := 5;
	when "1001010101" | "1001010110" =>		-- fcmpes/d
	    if (qs1.v <= SNaN) or (qs2.v <= SNaN) then
	        tfcc := "11";
	    else 
	        fcmp(qs1, qs2, lfcc);
	    end if;
	    if lfcc = "11" then
		cexc(4) := '1';
	    end if;
	    NbCycles := 5;
	    tfcc := lfcc;
	when "0001001101" | "0001001110" =>		-- fdivs/d
	    if (qs1.v > SNaN) and (qs2.v > SNaN) then
	        if (qs1.v = Zero and qs2.v = Zero) or
		   ((qs1.v = Inf) and (qs1.v = Inf)) then
		    cexc(4) := '1';			-- Invalid operation
		    qres.v := SNaN;
	        elsif (qs2.v = Zero) then
	    	    cexc(1) := '1'; 		-- Divide by zero 
		    qres.v := SNaN;
	        else
	            fdiv(qs1, qs2, qres);
	        end if;
	        FPround(qres, LRes, SP);
	    end if;
	    if SP then
	        NbCycles := 21;
	    else
	        NbCycles := 36;
	    end if;
	when "0000000001" =>		-- fmovs
	    LRes(63 downto 0) := RS2vec(31 downto 0) & RS2vec(31 downto 0);
	    cexc := "00000";
	    NbCycles := 2;
	when "0001001001" | "0001001010" =>		-- fmuls/d
	    if ((qs1.v > SNaN) and (qs2.v > SNaN)) then
	        if (qs1.v = Zero and qs2.v = Inf) or
	           (qs1.v = Inf and qs2.v = Zero) then
		    qres.v := QNaN;
	        else
	            fmul(qs1, qs2, qres);
	        end if;
	        FPround(qres, LRes, SP);
	    end if;
	    if SP then
	        NbCycles := 6;
	    else
	        NbCycles := 10;
	    end if;
	when "0000000101" =>		-- fnegs
	    if qs2.v > SNaN then
	        LRes(63 downto 32) := not(RS2vec(31)) & RS2vec(30 downto 0);
		Lres(31 downto 0) := LRes(63 downto 32);
	    end if;
	    NbCycles := 2;
	when "0000101001" | "0000101010" =>		-- fsqrs/d
	    NbCycles := 66;
	    if (qs2.v > SNaN) then
		if (qs2.sign = '1') then
		    cexc(4) := '1';			-- Invalid operation
		    qres.v := SNaN;
	            NbCycles := 7;
	        elsif qs2.v = Zero then
	                NbCycles := 7;
		        qres := qs2;
	        elsif qs2.v = Inf then
	                NbCycles := 7;
		        qres := qs2;
	        elsif qs2.v = norm then
	            if SP then
	                NbCycles := 38;
	            else
	                NbCycles := 66;
	            end if;
	            fsqrt(qs2, qres,SP);
	        end if;
	        FPround(qres, LRes, SP);
	    end if;
	when "0001000101" | "0001000110" =>		-- fsub/d
	    if (qs1.v > SNan) and (qs2.v > SNaN) then
	    	faddsub(qs1, qs2, qres, false);
	    	FPround(qres, LRes, SP);
	    end if;
	    NbCycles := 5;
	when "0011010010"  =>				-- fdtoi
	    if (qs2.v <= SNan) then
		cexc(4) := '1';
		LRes(31 downto 0) := '1' & ext("0",31);
		if qs2.sign = '0' then
		    LRes(31 downto 0) := not LRes(31 downto 0);
		end if;
		LRes(63 downto 32) := LRes(31 downto 0);
	    else
	        fqtoi(qs2, LRes);
	    end if;
	    NbCycles := 8;
	when "0011000110"  =>				-- fdtos
	    if qs2.v <= SNan then
		LRes(63 downto 0) := SPNan & SPNan;
		if qs2.v = SNan then
		    cexc(4) := '1';
		end if;
	    else
	        FPround(qs2, LRes, true);
	    end if;
	    NbCycles := 4;
	when "0011001000" =>				-- fitod
	    cexc := "00000";
	    fitoq(rs2vec,qs2);
	    FPround(qs2, LRes, false);
	    NbCycles := 7;
	when "0011000100"  =>				-- fitos
	    cexc := "00000";
	    fitoq(rs2vec,qs2);
	    FPround(qs2, LRes, true);
	    NbCycles := 7;
	when "0011001001"  =>				-- fstod
	    if qs2.v = QNan then
		LRes(63 downto 0) := DPNan;
		LRes(51 downto 51 - 22) := RS2vec(22 downto 0);
	    else
	        if qs2.v = SNan then
		    cexc(4) := '1';
	        end if;
	        FPround(qs2, LRes, false);
	    end if;
	    NbCycles := 3;
	when "0011010001"  =>				-- fstoi
	    if (qs2.v <= SNan) then
		cexc(4) := '1';
		LRes(31 downto 0) := '1' & ext("0",31);
		if qs2.sign = '0' then
		    LRes(31 downto 0) := not LRes(31 downto 0);
		end if;
		LRes(63 downto 32) := LRes(31 downto 0);
	    else
	        fqtoi(qs2, LRes);
	    end if;
	    NbCycles := 7;
        when others => 
	    Unimp := true;
	    NbCycles := 2;
    end case;
    
    Result := LRes;
    texc := cexc;

    if cexc /= "00000" then
      FPexcVector(FP_EXC_DETECTED) := TRUE;
      FPexcVector(IEEE_EXC)        := TRUE;
    elsif Unimp then
      FPexcVector(FP_EXC_DETECTED)    := TRUE;
      FPexcVector(UNIMPLEMENTED_FPOP) := TRUE;
    end if;

  end ExecuteFPop; -- procedure
  
  
end FPURTPck; -- package body
-------------------------------------------------------------------------------
-- File name : fpurt_gen_beh.vhd
-- Title : FPURTGeneric (architecture Behave)
-- project : SPARC
-- Library : FPURT_LIB
-- Author(s) : Maxime ROCCA
-- Purpose : definition of architecture Behave for FPURTGeneric. It is a beha-
--           -vioral description of the FPURT at the highest level.
--
-- notes : The entity FPURTGeneric is defined in the file fpurt_gen_ent.vhd.
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author |   Date   | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        |   MR   | 94-03-04 | first version.
-- Compliant with the FPU-RT Device Specification, issue 4++ (i.e certain 
-- features implemented in this version of the FPU-RT model will only appear in 
-- the next issue of the FPU-RT Device Specification), except that the copro- 
-- -cessor interface is not implemented in this version.
--.............................................................................
-- v 1.1        |   MR   | 94-05-03 | 2nd version
-- + FPURT model is made sensitive to the signals CCCV and CHOLD_N
-- + bug fix about LDFSR handling when FINS1/2 is late.
-- + bug fix about FEXC_N when STDFQ while FQ empty
-- + bug fix on FNULL and FLUSH.
-- + bug fix on generation of FCCV when in exception mode
-- + generation of FHOLD_N when FCMP(E)q is in W stage (particular case)
-- + modif. logical condition in parity bit checking + modif. generation of 
--   FIPAR
-- + bug fix for XHOLD cases on FP store.
-- + bug fix: modif. condition for FCCV generation.
-- + change name of tap controller
-- + modelling of buffers slightly modified.
--.............................................................................
-- v 1.2        |   MR   | 94-05-27  | 3rd version
-- + modify setup and hold time checkers.
--.............................................................................
-- v 1.3        |   RC   | 95-12-11  | 4th version
-- + modify data drivers.
--.............................................................................
-- v 1.4        |   JG   | 96-02-27  | 5th version
-- + bug fix: INST latching
-- + bug fix: MDS properly handled
-- + bug fix: STDF and STDFQ driving of data bus during second data cycle
--.............................................................................
-- v 1.5        |   JG   | 96-03-13  | 6th version
-- + bug fix: de-assert FHOLD even if MHOLD is asserted
--.............................................................................
-- v 1.6        |   JG   | 96-04-24  | 7th version
-- + Changed FHOLD/FCCV generation to FPU rev.B imlpementation
-- + Modified pipeline handling during XHOLD
-- + Instruction timing should be identical to FPU rev.B
--.............................................................................
-- v 1.7        |   JG   | 96-10-01  | 8th version
-- + Fixed problem with ldf/fpop interlock
-------------------------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
-------------------------------------------------------------------------------
---------|---------|---------|---------|---------|---------|---------|--------|

library IEEE;
use IEEE.Std_Logic_1164.all;
library MMS;
use MMS.StdRtl.all;
use MMS.StdIoImp.all;
library SPARC_LIB;
use SPARC_LIB.SparcPck.all;
use SPARC_LIB.TAPCompPck.all;
library FPURT_LIB;
use FPURT_LIB.FPURTPck.all;

use STD.TEXTIO.all;

architecture vhdl_behavioral of FPURTGeneric is

  -- constant for modelling purposes
  constant TRI_STATE32 : std_logic_vector(31 downto 0) 
                         := "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
  
  -- Value for the field "ver" of the FSR.
  constant FSR_VER    : std_logic_vector(2 downto 0) := "100";
  constant FSR_UNUSED : std_logic := '0';
  constant FSR_RES    : std_logic_vector(1 downto 0) := "00";
  constant RPconst    : std_logic_vector(1 downto 0) := "00";
  constant NSconst    : std_logic := '0';

  -- Pipeline registers + floating-point queue (FQ):
  signal ID1, ID2, E, W : FPInstruction;
  signal FQ : FPInstruction;
  signal FQ_rck : FPInstruction; --gd 130995
  
  -- Address register
  signal DDA : std_logic_vector(31 downto 0);
  
  -- Internal signals corresponding to output or bidirectional port signals.
  -- A signal SIGin corresponds to the output port signal SIG.
  signal FP_Nin    : std_logic;
  signal FCCin     : std_logic_vector(1 downto 0);
  signal FCCVin    : std_logic;
  signal FHOLD_Nin : std_logic;
  signal FEXC_Nin  : std_logic;
  signal FIPARin   : std_logic;
  signal Din       : std_logic_vector(31 downto 0);
  signal DPARin    : std_logic;
  signal FNULLin   : std_logic;
  
  -- Signals used for the IO buffers modelling
  signal DHCmSig     : std_logic;
  signal CHCmSig     : std_logic;
  signal DOE_Ndel    : std_logic; 
  signal COE_Ndel    : std_logic;
  signal HALTsampled : std_logic;

  -- Signals used for setup/hold timing checkers: conditional checking.
  signal RESET_Nsamp     : std_logic;
  signal TrigChecking    : std_logic;
  signal TrigChecking_1  : std_logic;
  signal TrigChecking_2  : std_logic;
  signal CheckTimBidir_1   : boolean;
  signal CheckTimBidir_1_i : boolean;
  signal CheckTimBidir_2   : boolean;
  signal CheckTimBidir_2_i : boolean;
  signal XHOLD_N : std_logic;
  
  signal Chk_A_en        : boolean;
  signal Chk_FINS1_en    : boolean;
  signal Chk_FINS2_en    : boolean;
  signal Chk_INST_en     : boolean;
  signal Chk_FXACK_en    : boolean;
  signal Chk_FLUSH_en    : boolean;
  signal Chk_MHOLDA_N_en : boolean;
  signal Chk_MHOLDB_N_en : boolean;
  signal Chk_BHOLD_N_en  : boolean;
  signal Chk_MDS_N_en    : boolean;
  signal Chk_APAR_en     : boolean;
  signal Chk_IFPAR_en    : boolean;

  signal FSR_spy : std_logic_vector(31 downto 0); -- gd 130995
  signal HLDorHLTasserted_spy   : boolean := FALSE; -- gd 130995
  signal HLDorHLTasserted_rck_spy   : boolean := FALSE;
  signal FPUmode_spy : FPUmodeType := RESET_MODE; -- gd 130995
  signal W1_spy, W2_spy : FPInstruction; -- gd 130995

  signal Inst_rck : std_logic := '0'; -- gd 140995
  signal Inst_samp_rck : std_logic := '0'; -- gd 140995
  signal Inst_samp_fck : std_logic := '0'; -- gd 140995
  signal Inst_samp : std_logic := '0'; -- gd 140995
  signal FPregFile_spy : RegisterFile(31 downto 0);

    signal CycleCounterx : integer;
    signal FHOLDcondTypex : natural := 0;
    signal FHOLD_Nvar_spy : std_logic;

  -- Configuration of components
--  for all : TAP_iufpu use entity SPARC_LIB.tap_iufpu(vhdl_behavioral);

begin

  FPUmodel: process
  
    -- W1 & W2: fictitious pipeline stages after the W stage
    -- W1: "stage" just after W
    -- W2: "stage" just after W1.
    variable W1, W2 : FPInstruction; -- CAREFUL: W1 & W2 are variables.
  
    -- FPU mode
    variable FPUmode : FPUmodeType := RESET_MODE;
  
    -- Floating-point state register
    variable FSR : std_logic_vector(31 downto 0);
      alias RD   : std_logic_vector(1 downto 0) is FSR(31 downto 30);
      alias RP   : std_logic_vector(1 downto 0) is FSR(29 downto 28);
      alias TEM  : std_logic_vector(4 downto 0) is FSR(27 downto 23);
      alias NS   : std_logic is FSR(22);
      alias res  : std_logic_vector(1 downto 0) is FSR(21 downto 20);
      alias ver  : std_logic_vector(2 downto 0) is FSR(19 downto 17);
      alias FTT  : std_logic_vector(2 downto 0) is FSR(16 downto 14);
      alias QNE  : std_logic is FSR(13);
      alias Unused : std_logic is FSR(12);
      alias FSR_FCC : std_logic_vector(1 downto 0) is FSR(11 downto 10);
      alias AEXC : std_logic_vector(4 downto 0) is FSR(9 downto 5);
      alias CEXC : std_logic_vector(4 downto 0) is FSR(4 downto 0);
    
    -- Floating-point register file
    variable FPregFile : RegisterFile(31 downto 0);
  
    -- Data objects for FP load instructions
    variable Data1, Data2 : std_logic_vector(31 downto 0);
  
    -- Boolean flag for start-up.
    variable PowerUp : boolean := TRUE;
  
    -- Variables for output signals: for signal SIGNAL, the variable is
    -- SIGNALvar. pSIGNALvar = value of SIGNALvar in the previous cycle.
    variable FHOLD_Nvar    : std_logic;
    variable FHOLD_Nvar2    : std_logic := '1';
    variable pFHOLD_Nvar2   : std_logic;
    variable FEXC_Nvar     : std_logic;
    variable pFEXC_Nvar    : std_logic;
    variable FCCvar        : std_logic_vector(1 downto 0);
    variable pFCCvar       : std_logic_vector(1 downto 0);
    variable FCCVvar       : std_logic;
    variable pFCCVvar      : std_logic;
    variable FNULLvar      : std_logic;
    variable pFNULLvar     : std_logic;
    variable pHWERROR_Nvar : std_logic;
    variable HWERROR_Nvar  : std_logic;
  
    -- Variable for MDS_N signal
    variable MDS_Nvar : std_logic;
    
    -- Variable for FLUSH signal
    variable pFLUSHvar : std_logic;
   
    -- Boolean variables for signals holding (or freezing) the pipeline.
    variable HLDorHLTasserted   : boolean := FALSE; -- XHOLD_N or HALT_N active
    variable HLDorHLTasserted_rck : boolean := FALSE;
    variable FHOLD_Nasserted    : boolean := FALSE;
    variable FCCVlow            : boolean := FALSE;
  
    -- Variables for execution.
    variable RS1vec       : std_logic_vector(63 downto 0);
    variable RS2vec       : std_logic_vector(63 downto 0);
    variable FPbitInst    : std_logic_vector(31 downto 0);
    variable CycleCounter : integer;
    variable Result       : std_logic_vector(63 downto 0);
    variable tfcc         : std_logic_vector(1 downto 0);
    variable texc         : std_logic_vector(4 downto 0);
    variable FPexcVector  : FPexcVectorType := (others => FALSE);
    variable NbCycles     : integer;
    variable texcVar      : std_logic_vector(4 downto 0); -- temp. variable
  
    -- Variables for FP STORE instructions
    variable StoreData1    : std_logic_vector(31 downto 0);
    variable StoreData2    : std_logic_vector(31 downto 0);
    variable DriveData1    : boolean := FALSE;
    variable DriveData2    : boolean := FALSE;
    variable OldDriveData1 : boolean := FALSE;
    variable OldDriveData2 : boolean := FALSE;
    variable EmptyTheFQ    : boolean := FALSE;
    
    -- Variables related to the external hold cases on FP STORE
    variable SaveDriveData1 : boolean := FALSE;
    variable SaveDriveData2 : boolean := FALSE;
    variable SaveActive : boolean := FALSE;
  
    -- Variable related to the Master/checker
    variable CMmismatch : integer := 0;

    -- Variables related to the FHOLD_N generation
    variable FHOLDcondType : natural := 0;

    -- Variable related to the parity bits checking
    variable ParBitViolCount : integer := -1;
    variable DPARviol  : boolean := FALSE;
    variable IFPARviol : boolean := FALSE;
    
    -- Other variables
    variable fttSAVE : std_logic_vector(2 downto 0); -- temp variable for ftt
    variable QNEsave : std_logic; -- temp variable for QNE
    variable PendingSeqErr : integer := -1; -- used for sequence error cases.
    variable DeasFHOLDseqErr : boolean := FALSE; -- for sequence error cases.

    -- Variable for specific case
    variable LDFSRhold2ndCycle : integer := -1; -- used for hold wen LDFSR
  
    -- Files for debugging; only used when in debugging mode.
    file Fdebug   : text is out "DPRegFPU.dbg";
    file FCtDebug : text is out "CtRegFPU.dbg";

  begin
  
    ---------------------------------------------
    -- Initialization part: body of 
    -- this "if" statement is executed only once.
    if PowerUp then
      PowerUp := FALSE;
      
      FP_Nin <= '0';
      res := FSR_RES;
      Unused := FSR_UNUSED;
      ver := FSR_VER;
      
      -- Check certain timing parameters values
      if tDPOH >= tDPOD then
        assert FALSE report "[FPURTGeneric(Behave)]: parameter tDPOD must be " &
                             "greater than tDPOH after re-computation!"
                      severity failure;
      end if;
      
      if tDOH >= tDOD then
        assert FALSE report "[FPURTGeneric(Behave)]: parameter tDOD must be " &
                             "greater than tDOH after re-computation!"
                      severity failure;
      end if;
      
      if tFCCH >= tFCCD then
        assert FALSE report "[FPURTGeneric(Behave)]: parameter tFCCD must be " &
                             "greater than tFCCH after re-computation!"
                      severity failure;
      end if;
      
      if tDOHFL >= tDOFFL then
        assert FALSE report "[FPURTGeneric(Behave)]: parameter tDOFFL must " &
                            "be greater than tDOHFL after re-computation!"
                      severity failure;
      end if;
      
      if tFIPH >= tFIPD then
        assert FALSE report "[FPURTGeneric(Behave)]: parameter tFIPD must " &
                            "be greater than tFIPH after re-computation!"
                      severity failure;
      end if;
      
    end if;
    --------------------------------
  
    wait on Clk, FINS1, FINS2, FLUSH,
            DHCmSig, CHCmSig,
            FP_Nin, FCCin, FCCVin, FHOLD_Nin, FEXC_Nin, FIPARin, Din,
            DPARin, FNULLin;
    
    if (FPUmode /= RESET_MODE and FPUmode /= ERROR_MODE) then
      if rising_edge(Clk) then
      
        -- assign pSIGNAL variables
        pFEXC_Nvar := FEXC_Nvar;
        pFCCvar    := FCCvar;
        pHWERROR_Nvar := HWERROR_Nvar;
        
        -- on each rising clock edge
        DDA <= A;

        -- For setup/hold timing checkers.
        if E.Mnemo = LDF or E.Mnemo = LDFSR or 
           E.Mnemo = LDDF or W.Mnemo = LDDF then
          TrigChecking <= '1';
        else
          TrigChecking <= '0';
        end if;

	HLDorHLTasserted_rck := HLDorHLTasserted;

-- Fix for ldf/fpop interlock problem, 1-10-96 J.Gaisler

        if not(HLDorHLTasserted or FCCVlow or FHOLD_Nasserted) or
          ((HLDorHLTasserted or FCCVlow or FHOLD_Nasserted)
	   and IsFPop(E.Mnemo) and not (IsFPop(FQ.Mnemo) or IsFPop(W.Mnemo))
	   and CycleCounter = -1)
	then

          --''''' FP load '''''
        
          if FLUSH = '0' and FPUmode /= EXCEPTION then
            if W1.Mnemo = LDF then -- delayed loading into register file.
              FPregFile(W1.rd) := Data1;
            elsif W1.Mnemo = LDFSR then
              fttSAVE := ftt; -- ftt, res, Unused, ver, QNE, NS, RP of FSR 
              QNEsave := QNE; -- are NOT loadable.
              FSR := Data1; 
              ftt := fttSAVE;
              res := FSR_RES;
              Unused := FSR_UNUSED;
              ver := FSR_VER;
              FCCvar := FSR_FCC;
              RP  := RPconst;
              NS  := NSconst;
              QNE := QNEsave;
            end if;
            
            if W1.Mnemo = LDDF then
              FPregFile((W1.rd/2)*2) := Data1; -- Register alignment (even #)
            end if;
            
            if W2.Mnemo = LDDF then
-- MDS fix, J.Gaisler 27-02-96
              FPregFile((W2.rd/2)*2 + 1) := Data1; -- Register alignment (odd #)
            end if;
          end if;
        
          if (W.Mnemo = LDF or W.Mnemo = LDDF or W.Mnemo = LDFSR) then
            Data1 := D;
          end if;
        
          if W1.Mnemo = LDDF then
            Data1 := D;		-- MDS fix, J.Gaisler 27-02-96
          end if;
        end if;
        
         

        if not(HLDorHLTasserted or FCCVlow or FHOLD_Nasserted) then
        
	  -- Latch INST (J.Gaisler 27-02-96)

          Inst_samp <= INST;

          --~~~~~~ IFetch, IDecode, Execute, FP load/store ~~~~~~
          --''''' FP store '''''
          if EmptyTheFQ then
            EmptyTheFQ := FALSE;
            QNE := '0';
            FPUmode := EXECUTION;
          end if;
          
          --''''' Assignment of E '''''
          if FINS1 = '1' then
            if FPUmode = EXCEPTION and 
               not(ID1.Mnemo = STF   or ID1.Mnemo = STDF or 
                   ID1.Mnemo = STFSR or ID1.Mnemo = STDFQ) and 
               PendingSeqErr = -1 then
              PendingSeqErr := 2;
            end if;
            E <= ID1;
          elsif FINS2 = '1' then
            if FPUmode = EXCEPTION and 
               not(ID2.Mnemo = STF   or ID2.Mnemo = STDF or 
                   ID2.Mnemo = STFSR or ID2.Mnemo = STDFQ) and 
               PendingSeqErr = -1 then
              PendingSeqErr := 2;
            end if;
            E <= ID2;
          elsif ((E.Mnemo = LDF or E.Mnemo = LDDF or E.Mnemo = LDFSR or
                  E.Mnemo = STF or E.Mnemo = STDF or E.Mnemo = STFSR or
                  E.Mnemo = STDFQ) or
                 (W.Mnemo = LDDF or W.Mnemo = STF or W.Mnemo = STDF or 
                  W.Mnemo = STDFQ or W1.Mnemo = STDF or W1.Mnemo = STDFQ)
                ) then
            E.Mnemo <= IOP;
          elsif FINS1 = '0' and FINS2 = '0' then
            E.Mnemo <= NOTHING;
          else
            E.Mnemo <= XXX;
          end if;
                    
          --''''' Pipeline progression for W, W1, W2 '''''
          W2 := W1;
          W1 := W;
          W  <= E;
        
          --''''' Instruction fetch from D bus '''''
          if INST = '1' then
            ID2 <= ID1;
            ID1 <= FPtranscribe(D);
            ID1.BitAddress <= DDA;
          end if;
        
        end if;

        --''''' Sequence error condition '''''
        if (PendingSeqErr = 0) or 
           (E.Mnemo = STDFQ and QNE = '0' and not(FHOLD_Nasserted) and
            not(HLDorHLTasserted)) then
          FPexcVector(FP_EXC_DETECTED) := TRUE;
          FPexcVector(SEQUENCE_ERROR)  := TRUE;
          CycleCounter := 1; -- goes into the exception handling part.
        end if;
          
        if PendingSeqErr >= 0 then
          PendingSeqErr := PendingSeqErr - 1;
        end if;
          
        if W.Mnemo = STDFQ and FPUmode = PENDING_EXCEPTION then
          FEXC_Nvar := '1'; -- "Kinky" behavior in this case!!!
        end if;

        --~~~~~ Effect of FLUSH on the FPU: flush the pipeline ~~~~
        if (FLUSH = '1' and pFLUSHvar = '0' and 
            not(HLDorHLTasserted or FCCVlow or FHOLD_Nasserted) 
           ) then
          E.Mnemo  <= ANNULLED;
          W.Mnemo  <= ANNULLED;
          W1.Mnemo := ANNULLED;
          W2.Mnemo := ANNULLED;
        end if;
        pFLUSHvar  := FLUSH;
               
        --~~~~~~~ Generation of HWERROR_N: parity bit checking ~~~~~~~~
        -- WARNING: Fuzzy behavior!!!
        ---
        if N602MODE_N = '1' then 
          if (IFPAR /= OddParityOf(FINS1 & FINS2 & FLUSH & FXACK & INST) and
              IFPAR /=  'Z' and FINS1 /= 'Z' and FINS2 /= 'Z' and FLUSH /= 'Z'
              and FXACK /= 'Z' and INST /= 'Z') or 
             (APAR  /= OddParityOf(A) and APAR /= 'Z' and A /= TRI_STATE32) then
            ParBitViolCount := 1;
            IFPARviol := TRUE;
          end if;
          
          if (INST = '1' or W.mnemo = LDF or W2.Mnemo = LDDF) and 
             DPAR /= OddParityOf(D) and DPAR /= 'Z' and D /= TRI_STATE32 then
            ParBitViolCount := 1;
            DPARviol := TRUE;
          end if;
        end if;
        
        if FTT = "000" then  -- WARNING: the timing for signal HWERROR_N may
                             -- be wrong! Not well specified.
          HWERROR_Nvar := '1';
        end if;

        --~~~~~~ Floating-point queue (FQ) handling ~~~~~~
        --~~~~~~ and exception handling ~~~~~~
        if QNE = '0' then
          if IsFPop(W.Mnemo) and FPUmode = EXECUTION and FLUSH = '0' then
            if IsFPopSourceRegDouble(W.Mnemo) then
              RS1vec := FPregFile((W.rs1/2)*2) & FPregFile((W.rs1/2)*2+1);
              RS2vec := FPregFile((W.rs2/2)*2) & FPregFile((W.rs2/2)*2+1);
            else
              RS1vec := FPregFile(W.rs1) & FPregFile(W.rs1);
              RS2vec := FPregFile(W.rs2) & FPregFile(W.rs2);
            end if;
            FPbitInst := W.BitInstruction;
            if VecUnknown(FPbitInst) or VecUnknown(RS2vec) or 
               VecUnknown(RD) or BitUnknown(TEM(2)) or
               (FPURs1IsIn(W.Mnemo) and VecUnknown(RS1vec)) then
              Result := (others => 'X');
              tfcc := "XX";
              texc := "XXXXX";
              FPexcVector := (others => FALSE);
              NbCycles := 2; -- Arbitrary value
              assert FALSE report "(Architecture of FPURTGeneric): unknown " &
                                  "value encountered."
                           severity warning;
            else
               ExecuteFPop(FPbitInst, RS1vec, RS2vec, RD, TEM(2), FALSE, 0,
                           Result, tfcc, texc, FPexcVector, NbCycles, Fdebug, 
                           FCtDebug);
            end if;
                         
            if NbCycles = 2 or IsFPopUnimp(W.Mnemo) then
                      -- because of "kinky" behavior for 2-cycle
                      -- FPop, and unimplemented FPop instructions.
              CycleCounter := NbCycles;
            elsif NbCycles > 2 then
              CycleCounter := NbCycles - 1;
            end if;
            
            FQ  <= W;
            QNE := '1'; -- FQ not empty.
            if (HLDorHLTasserted or FCCVlow or FHOLD_Nasserted) then
              W.Mnemo <= NOTHING; -- The instr. in W has moved to the FQ.
            end if;
          else
            FQ.Mnemo <= NOTHING;
          end if;
        end if;

---------------------------------------------------------------
--  FIX: E and W stage is not sensitive to FHOLD/FCCV
--  J.Gaisler 19-04-96

        if (HLDorHLTasserted or FCCVlow or FHOLD_Nasserted)
	   and IsFPop(E.Mnemo) and not (IsFPop(FQ.Mnemo) or IsFPop(W.Mnemo))
	   and CycleCounter = -1
        then
          W2 := W1;
          W1 := W;
          W  <= E;
	  E.Mnemo <= NOTHING;
	end if;
---------------------------------------------------------------
          
        if CycleCounter >= 0 then
          CycleCounter := CycleCounter - 1;
        end if;
        
        if ParBitViolCount = 0 then -- parity bit violation handling.
          ParBitViolCount := -1;
          CycleCounter := 0;
          if DPARviol then
            DPARviol  := FALSE;
            FPexcVector(DATA_BUS_ERROR)  := TRUE;
            FPexcVector(FP_EXC_DETECTED) := TRUE;
            FPUmode := PENDING_EXCEPTION;
          end if;
          
          if IFPARviol then
            IFPARviol := FALSE;
            FPexcVector(RESTARTABLE_ER)  := TRUE;
            FPexcVector(FP_EXC_DETECTED) := TRUE;
            FPUmode   := PENDING_EXCEPTION;
          end if;
        elsif ParBitViolCount > 0 then
          ParBitViolCount := ParBitViolCount - 1;
        end if;

        if CycleCounter = 0 then
          
          if FPexcVector(FP_EXC_DETECTED) then -- Potential FP exception 
                                               -- detected
             if FPexcVector(IEEE_EXC) then
               -- assign CEXC & AEXC according to TEM
               CEXC := texc;
               texcVar := texc and TEM;
               if texcVar = "00000" then
                 AEXC := AEXC or CEXC;
                 FTT  := "000";  -- no exception flagged (AND-masked by TEM).
                 -- Assign FPregFile with Result because no exception occurred
                 -- Note: the register addressing is forced --> data alignment
                 --       in register file.
                 if IsFPopRdDouble(FQ.Mnemo) then
                   FPregFile((FQ.rd/2)*2) := Result(63 downto 32);
                   FPregFile((FQ.rd/2)*2+1) := Result(31 downto 0);
                 elsif not(IsFCMP(FQ.Mnemo)) then
                   FPregFile(FQ.rd) := Result(63 downto 32);
                 elsif IsFCMP(FQ.Mnemo) then 
                   FCCvar  := tfcc;
                   FSR_FCC := tfcc;
                 end if;
                        
                 QNE := '0'; -- The exec. of the instr. in the FQ is finished.
                 FQ.Mnemo <= NOTHING;
                 
               else
                 FTT       := "001";
                 FPUmode   := PENDING_EXCEPTION;
                 FEXC_Nvar := '0';
               end if;
                 
             elsif FPexcVector(UNFINISHED_FPOP) then -- should not occur
               FTT       := "010";
               FPUmode   := PENDING_EXCEPTION;
               FEXC_Nvar := '0';
               
             elsif FPexcVector(UNIMPLEMENTED_FPOP) then
               FTT       := "011";
               FPUmode   := PENDING_EXCEPTION;
               FEXC_Nvar := '0';
               
             elsif FPexcVector(SEQUENCE_ERROR) then
               FTT       := "100";
               FPUmode   := PENDING_EXCEPTION;
               FEXC_Nvar := '0';
               
             elsif FPexcVector(DATA_BUS_ERROR) then
               FTT       := "101"; -- Data bus error
               HWERROR_Nvar := '0';
               FEXC_Nvar := '0';
               
             elsif FPexcVector(RESTARTABLE_ER) then
               FTT       := "110"; -- Restartable error
               HWERROR_Nvar := '0';
               FEXC_Nvar := '0';
               
             end if;
             
             FPexcVector := (others => FALSE); 
               
          else
            CEXC := "00000";
            FTT  := "000";  
            -- Assign FPregFile with Result because no exception occurred
            -- Note: the register addressing is forced --> data alignment
            --       in register file.
            if IsFPopRdDouble(FQ.Mnemo) then
              FPregFile((FQ.rd/2)*2) := Result(63 downto 32);
              FPregFile((FQ.rd/2)*2+1) := Result(31 downto 0);
            elsif not(IsFCMP(FQ.Mnemo)) then
              FPregFile(FQ.rd) := Result(63 downto 32);
            elsif IsFCMP(FQ.Mnemo) then 
              FCCvar  := tfcc;
              FSR_FCC := tfcc;
            end if;
            
            QNE := '0'; -- The execution of the instr. in the FQ is finished.
            FQ.Mnemo <= NOTHING;
          end if;
                            
        end if;
        
        --~~~~~~~~~~ Samples FXACK to deassert FEXC_N ~~~~~~~~~
        if FXACK = '1' then
          FEXC_Nvar := '1';
          FPUmode := EXCEPTION;
        end if;
        
        --~~~~~~~~~ Instruction/data fetch when cache miss ~~~~~~~~~ 
        if MDS_Nvar = '0' then
          if Inst_samp = '1' then -- instruction fetch -- gd 140995
            ID1 <= FPtranscribe(D);
            ID1.BitAddress <= DDA;
          elsif FPUmode /= EXCEPTION then -- data fetch -- gd 140995

              if (W1.Mnemo = LDF) or (W1.Mnemo = LDDF) or
                 (W1.Mnemo = LDFSR) or (W2.Mnemo = LDDF) then
                Data1 := D;		-- MDS fix, J.Gaisler 27-02-96
              end if;

          end if;
        end if;
        
        --~~~~ DPAR checking when MDS_N: what if MEXC_N??? ~~~~
        if N602MODE_N = '1' and MDS_Nvar = '0' then
          if (INST = '1' or W.mnemo = LDF or W2.Mnemo = LDDF) and 
             DPAR /= OddParityOf(D) and DPAR /= 'Z' and D /= TRI_STATE32 then
            ParBitViolCount := 1;
            DPARviol := TRUE;
          end if;          
        end if; 
        
      FQ_rck <= FQ; -- GD 130995

      end if; -- rising edge of Clk.
              -- ^^^^^^^^^^^^^^^^^^^


      if falling_edge(Clk) then
      
        -- Sample signal MDS_N
        MDS_Nvar := MDS_N;
      
        -- assign pSIGNAL variables
        pFHOLD_Nvar2 := FHOLD_Nvar2;
        pFCCVvar    := FCCVvar;
        pFNULLvar   := FNULLvar;
            
        --~~~ Sampling of XHOLD_N input signals and FHOLD_Nvar, FCCVvar ~~~~
        if (MHOLDA_N = '0' or MHOLDB_N = '0' or BHOLD_N = '0' or 
            HALT_N = '0' or CHOLD_N = '0' or CCCV = '0'
           ) then
	  HLDorHLTasserted := true;
	else
	  HLDorHLTasserted := false;
	end if;

        if FHOLD_Nvar2 = '0' then
          FHOLD_Nasserted := TRUE;
        else
          FHOLD_Nasserted := FALSE;
        end if;
        
        if FCCVvar = '0' then
          FCCVlow := TRUE;
        else
          FCCVlow := FALSE;
        end if;
        
        --~~~ DPAR violation cancelled in case of MHOLD_N ~~~~
        if N602MODE_N = '1' and (MHOLDA_N = '0' or MHOLDB_N = '0') and 
           ParBitViolCount /= -1 and DPARviol then
          DPARviol := FALSE;
          if not(IFPARviol) then
            ParBitViolCount := -1;
          end if;
        end if;
        
        --~~~~~~ Handling of FP compare: FCCV signal ~~~~~~
        if (not FHOLD_Nasserted) and
	   (IsFCMP(E.Mnemo) or IsFCMP(W.Mnemo))
	     and ((FPUmode = EXECUTION) and (FLUSH = '0'))
        then
            FCCVvar := '0';
        end if;
              
        --~~~~~~ Generation of FHOLD_N signal on the falling clock edge ~~~
        if FPUmode = EXECUTION and
--	FIX: FHOLD is asserted even if MHOLD is active, J.Gaisler 27-02-96
           ( ( not(FHOLD_Nasserted) and not(FCCVlow)) 
             or (W.Mnemo = FCMPq or W.Mnemo = FCMPEq)
           ) then
          FHOLDcondType := FHOLDcondition(FINS1, FINS2, ID1, ID2, E, W, W1, FQ);
          if FHOLDcondType = 1 or (FHOLDcondType = 2 and not HLDorHLTasserted)
          then
            FHOLD_Nvar := '0';
          end if;
        end if;
        
        if PendingSeqErr = 1 then
          FHOLD_Nvar := '0'; -- case of sequence error.
        end if;
        
        --~~~~~ Deassertion of FHOLD_N and FCCV ~~~~~~~~

        if (CycleCounter = 0) then 
          if FHOLD_Nvar = '0' then
            FHOLD_Nvar := '1';
          end if;
          if FCCVvar = '0' and FCCVlow then
            FCCVvar := '1';
          end if;
        end if;

        if LDFSRhold2ndCycle >= 0 then
          LDFSRhold2ndCycle := LDFSRhold2ndCycle - 1;
        end if;
        
        if ((((W1.Mnemo = LDF or W2.Mnemo = LDDF) and FHOLD_Nasserted and 
             FHOLDcondType /= 1 ) or
           ( (W.Mnemo = LDF or W1.Mnemo = LDDF) and FHOLD_Nasserted and 
             FHOLDcondType /= 1 ) or
           ( (W2.Mnemo = LDFSR or (LDFSRhold2ndCycle = 0 and W1.Mnemo = LDFSR))
             and FHOLD_Nasserted and FHOLDcondType /= 1)))

        then
          FHOLD_Nvar := '1';
        end if;
        
        if DeasFHOLDseqErr then
          FHOLD_Nvar := '1'; -- case of sequence error.
          DeasFHOLDseqErr := FALSE;
        end if;
        if PendingSeqErr = 0 then
          DeasFHOLDseqErr := TRUE; -- case of sequence error.
        end if;
        
        if FHOLD_Nvar = '0' and W.Mnemo = LDFSR then
          LDFSRhold2ndCycle := 2;
        end if;

-----------------------------------------------------------------------------
--	FIX: if MHOLD and FHOLD are asserted simultaneously, then FHOLD will
--     	be kept low for one clock after the release of MHOLD.
--	MODIFIED 15-04-96 to incorporate new scheme in rev.B

        if not ( HLDorHLTasserted_rck and HLDorHLTasserted) then
	  FHOLD_Nvar2 := FHOLD_Nvar; 
	end if;
                
        FNULLvar := not(FCCVvar and FHOLD_Nvar2);
        
      end if; -- falling edge of Clk.
              -- ^^^^^^^^^^^^^^^^^^^^
              
    end if;
    
    
    
    --******************** INTERFACE modelling ******************
    
    --'''''' Reset pin sampling '''''
    if rising_edge(Clk) then
      if RESET_N = '0' then
        FPUmode := RESET_MODE;
              
        pFHOLD_Nvar2 := FHOLD_Nvar2;
        FHOLD_Nvar  := '1';
        pFEXC_Nvar  := FEXC_Nvar;
        FEXC_Nvar   := '1';
        pFCCvar     := FCCvar;
        FCCvar      := "00";
        pFCCVvar    := FCCVvar;
        FCCVvar     := '1';
        pFNULLvar   := FNULLvar;
        FNULLvar    := '0';
        pHWERROR_Nvar := HWERROR_Nvar;
        HWERROR_Nvar  := '1';
        
        MCERR_N <= '1' after tMCD;
	FHOLD_Nin <= '1';
      
        -- Set the writable fields of FSR to zero.
        RD      := "00";
        RP      := "00";
        TEM     := "00000";
        NS      := '0';
        QNE     := '0';
        FSR_FCC := "00";
        AEXC    := "00000";
        CEXC    := "00000";
        ftt     := "000";
      
        -- Reset the pipeline.
        ID1.Mnemo <= XXX;
        ID2.Mnemo <= XXX;
        E.Mnemo   <= XXX;
        W.Mnemo   <= XXX;
        FQ.Mnemo  <= XXX;
        W1.Mnemo  := XXX;
        W2.Mnemo  := XXX;
        
        -- Reset the address register
        DDA <= (others => '0');
        
      elsif RESET_N = '1' and FPUmode = RESET_MODE then
        FPUmode := EXECUTION;
        TrigChecking <= '1'; -- used for timing checkers.
      end if;
    end if;
    
    --''''''''' Assign output signals on their corresponding edges ''''''''
    if (rising_edge(FINS1) or rising_edge(FINS2)) and CLK = '0' and 
       FPUmode = EXECUTION and 
       ( (not(HLDorHLTasserted) and not(FHOLD_Nasserted) and not(FCCVlow)) or
         (W.Mnemo = FCMPq or W.Mnemo = FCMPEq)        
       ) then
      FHOLDcondType := FHOLDcondition(FINS1, FINS2, ID1, ID2, E, W, W1, FQ);
      if FHOLDcondType = 1 or FHOLDcondType = 2 then
        FHOLD_Nvar := '0';
        FHOLD_Nvar2 := '0';
--        FHOLD_Nin  <= '0' after tFHDFI;
        FNULLvar   := '1';
        if W.Mnemo = LDFSR then
          LDFSRhold2ndCycle := 2;
        end if;
      end if;
    end if;
    
    if rising_edge(FLUSH) then
      FHOLD_Nvar := '1';
--      FHOLD_Nin  <= transport '1' after tFHDFL;
      FCCVvar := '1';
      FCCVin  <= transport '1' after tFCCVDFL;
    end if;

    if falling_edge(Clk) then
    
        if not(HLDorHLTasserted or FCCVlow or FHOLD_Nasserted) then
          --''''' FP store '''''
          if (W.Mnemo = STF or W.Mnemo = STDF or 
              W.Mnemo = STDFQ or W.Mnemo = STFSR 
             ) then
            case W.Mnemo is
              when STF   =>
          --       OldStoreData := StoreData;
          --       StoreData  := FPregFile(W.rd);
                 StoreData1  := FPregFile(W.rd);
                 OldDriveData1 := DriveData1;
                 DriveData1 := TRUE;
              when STDF  =>
          --       OldStoreData := StoreData;
          --       StoreData  := FPregFile((W.rd/2)*2); -- Register alignment
                 StoreData1  := FPregFile((W.rd/2)*2); -- Register alignment
                 OldDriveData1 := DriveData1;
                 DriveData1 := TRUE;
              when STDFQ =>
          --       OldStoreData := StoreData;
          --       StoreData  := FQ.BitAddress;
                 StoreData1  := FQ_rck.BitAddress;
                 OldDriveData1 := DriveData1;
                 DriveData1 := TRUE;
              when STFSR =>
          --       OldStoreData := StoreData;
          --       StoreData  := FSR;
                 StoreData1  := FSR;
                 OldDriveData1 := DriveData1;
                 DriveData1 := TRUE;
              when others => NULL;
            end case;
          else
            OldDriveData1 := DriveData1;
            DriveData1 := FALSE;
          end if;
          
          if W1.Mnemo = STDFQ then 
            OldDriveData2 := DriveData2;
            DriveData2 := TRUE;
            StoreData2 := FQ.BitInstruction;
            EmptyTheFQ := TRUE;
          elsif W1.Mnemo = STDF then
            OldDriveData2 := DriveData2;
            DriveData2 := TRUE;
            StoreData2 := FPregFile((W.rd/2)*2 + 1); -- Register alignment
          else
            OldDriveData2 := DriveData2;
            DriveData2 := FALSE;
          end if;
          
        end if;

      if (FHOLD_Nvar2 = '1' and pFHOLD_Nvar2 /= '1') then
        FHOLD_Nin <= '1' after tFHH;
      elsif (FHOLD_Nvar2 = '0' and pFHOLD_Nvar2 /= '0') then
        FHOLD_Nin <= '0' after tFHD;
      end if;
      
      if (FCCVvar = '1' and pFCCVvar /= '1') then
        FCCVin <= '1' after tFCCVH;
      elsif (FCCVvar = '0' and pFCCVvar /= '0') then
        FCCVin <= '0' after tFCCVD;
      end if;
      
      ------------------------------------
      
      if DriveData1 and FLUSH = '0' then
        Din    <= StoreData1 after tDOD;
        DPARin <= OddParityOf(StoreData1) after tDPOD;
      elsif DriveData2 and FLUSH = '0' then
        Din    <= StoreData2 after tDOD;
        DPARin <= OddParityOf(StoreData2) after tDPOD;
      else
        Din    <= (others => 'Z') after tDOH;
        DPARin <= 'Z' after tDPOH;
      end if;
            
    end if;
    
    if rising_edge(FLUSH) and (DriveData1 or DriveData2) then
      Din    <= transport (others => 'X') after tDOHFL, 
                          (others => 'Z') after tDOFFL;
      DPARin <= transport 'X' after tDOHFL, 'Z' after tDOFFL;
    end if;
    
    
    if rising_edge(Clk) then
    
      if (FEXC_Nvar = '1' and pFEXC_Nvar /= '1') then
        FEXC_Nin <= '1' after tFEH;
      elsif (FEXC_Nvar = '0' and pFEXC_Nvar /= '0') then
        FEXC_Nin <= '0' after tFED;
      end if;
    
      if FCCvar /= pFCCvar then
        FCCin <= "XX" after tFCCH, FCCvar after tFCCD;
      end if;
      
      if (FNULLvar = '1' and pFNULLvar /= '1') then
        FNULLin <= '1' after tFND;
      elsif (FNULLvar = '0' and pFNULLvar /= '0') then
        FNULLin <= '0' after tFNH;
      end if;
      
      if (HWERROR_Nvar = '1' and pHWERROR_Nvar /= '1') then
        HWERROR_N <= '1' after tERH;
      elsif (HWERROR_Nvar = '0' and pHWERROR_Nvar /= '0') then
        HWERROR_N <= '0' after tERD;
      end if;
      
      FIPARin <= 'X' after tFIPH, 
                 OddParityOf(FEXC_Nvar & FCCvar &
                             FCCVvar & FHOLD_Nvar2) after tFIPD;

    end if;
    
    --''''''' Master/checker mode operation: generation of MCERR_N '''''''
    if CMODE_N = '0' then
      if rising_edge(CLK) then
        if CMmismatch = 1 or
           ((FCCin     /= FCC    or
             FEXC_Nin  /= FEXC_N or
             FNULLin   /= FNULL  or
             FIPARin   /= FIPAR  or
             FP_Nin    /= FP_N      
            ) and COE_Ndel = '0' and HALTsampled = '1') then
           MCERR_N <= '0' after tMCD;
         else
           MCERR_N <= '1' after tMCH;
         end if;
         
         if CMmismatch /= 0 then
           CMmismatch := CMmismatch - 1;
         end if;
             
      elsif falling_edge(CLK)  then
        if HALTsampled = '1' and
           ( ( (Din /= D or DPARin /= DPAR) and DOE_Ndel = '0' and 
               (OldDriveData1 or OldDriveData2) ) or
             ((FCCVin /= FCCV or FHOLD_Nin /= FHOLD_N) and COE_Ndel = '0')
           ) then
       --   if CLK'last_event >  then
          CMmismatch := 1;
       --   else
       --     CMmismatch := 2; -- This is for clock cycle dependency: TBD
       --   end if;
        end if;
      end if;
    else
      MCERR_N <= '1' after tMCH;
    end if;

    --'''' Tristated Outputs modelling: effects of signals COE_N, '''' 
    --'''' DOE_N, HALT_N and CMODE on the output signals. ''''
    if DHCmSig'event and DHCmSig = '1' then
      D    <= (others => 'Z');
      DPAR <= 'Z';
    elsif DHCmSig = '0' then
      D    <= Din;
      DPAR <= DPARin;
    elsif DHCmSig = 'U' then 
      D    <= (others => 'U');
      DPAR <= 'U';
    elsif DHCmSig /= '1' then  -- should never get here.
      D    <= (others => 'X');
      DPAR <= 'X';
    end if;

    if CHCmSig'event and CHCmSig = '1' then
      FCCV    <= 'Z';
      FCC     <= "ZZ";
      FEXC_N  <= 'Z';
      FHOLD_N <= 'Z';
      FNULL   <= 'Z';
      FIPAR   <= 'Z';
      FP_N    <= 'Z';
    elsif CHCmSig = '0' then
      FCCV    <= FCCVin;
      FCC     <= FCCin;
      FEXC_N  <= FEXC_Nin;
      FHOLD_N <= FHOLD_Nin;
      FNULL   <= FNULLin;
      FIPAR   <= FIPARin;
      FP_N    <= FP_Nin;
    elsif CHCmSig = 'U' then  
      FCCV    <= 'U';
      FCC     <= "UU";
      FEXC_N  <= 'U';
      FHOLD_N <= 'U';
      FNULL   <= 'U';
      FIPAR   <= 'U';
      FP_N    <= 'U';
    elsif CHCmSig /= '1' then  -- should never get here.
      FCCV    <= 'X';
      FCC     <= "XX";
      FEXC_N  <= 'X';
      FHOLD_N <= 'X';
      FNULL   <= 'X';
      FIPAR   <= 'X';
      FP_N    <= 'X';
    end if;
    
    FSR_spy <= FSR; -- gd 130995
    HLDorHLTasserted_spy <= HLDorHLTasserted; -- gd 130995
    HLDorHLTasserted_rck_spy <= HLDorHLTasserted_rck;
    FPUmode_spy <= FPUmode; -- gd 130995
    W1_spy <= W1; -- gd 130995
    W2_spy <= W2; -- gd 130995
    CycleCounterx <= CycleCounter;
    FHOLDcondTypex <= FHOLDcondType;

    FPregFile_spy <= FPregFile;

    FHOLD_Nvar_spy <= FHOLD_Nvar;

  end process FPUmodel;
  
  -------------------------------------------
  -------------------------------------------

  
  -------------------------------------------
  -- Processes to help model the tri-stated 
  -- output buffers.
  -------------------------------------------
  DHCmSigProcess: DHCmSig <= DOE_Ndel or not(HALTsampled) or not(CMODE_N);
  CHCmSigProcess: CHCmSig <= COE_Ndel or not(HALTsampled) or not(CMODE_N);
  
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
        
    if DOE_N = '0' then
      DOE_Ndel <= '0' after tDONOE;
    elsif DOE_N = '1' then
      DOE_Ndel <= '1' after tDOFOE;
    else
      DOE_Ndel <= DOE_N; -- should not happen in normal case
    end if;

    if COE_N = '0' then
      COE_Ndel <= '0' after tDONOE;
    elsif COE_N = '1' then
      COE_Ndel <= '1' after tDOFOE;
    else
      COE_Ndel <= COE_N; -- should not happen in normal case
    end if;
    
    wait on DOE_N, COE_N;

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
  -- Setup/hold checkers.
  --------------------------------------------------
  GenRESET_Nsamp: process
  begin
    if not(CHECK_ON) then
      wait; -- the process dies here....
    end if;
    
    wait on CLK until rising_edge(CLK);
    
    RESET_Nsamp <= RESET_N;
  end process GenRESET_Nsamp;

  ----
  XHOLD_N <= MHOLDA_N and MHOLDB_N and BHOLD_N and FHOLD_N and FCCV and 
             CHOLD_N and CCCV;
  
  ----
  TrigChecking_1 <= TrigChecking after tDIH;
  
  GenCheckTimBidir_1_i : process
    variable Temp : std_logic;
  begin
    if not(CHECK_ON) then
      wait; -- the process dies here....
    end if;
    
    wait on INST, RESET_Nsamp, TrigChecking_1;
    
    Temp := (INST and RESET_Nsamp) or TrigChecking_1;
    if Temp = '1' then
      CheckTimBidir_1_i <= TRUE;
    else
      CheckTimBidir_1_i <= FALSE;
    end if;
  end process GenCheckTimBidir_1_i;
  
  CheckTimBidir_1 <= transport CheckTimBidir_1_i and not(D = TRI_STATE32);

  DbusCheck :  DbusSetupHoldCheck(D, CLK, XHOLD_N,
                                  SETUP => tDIS,  HOLD => tDIH, 
                                  PATH => "D", 
                                  DelayedData => D'Delayed(abs(tDIH)),
                                  EN_CHECKING => CheckTimBidir_1);
  
  ----
  TrigChecking_2 <= TrigChecking after tDPIH;

  GenCheckTimBidir_2_i : process
    variable Temp : std_logic;
  begin
    if not(CHECK_ON) then
      wait; -- the process dies here....
    end if;
    
    wait on INST, RESET_Nsamp, TrigChecking_2;
    
    Temp := (INST and RESET_Nsamp) or TrigChecking_2;
    if Temp = '1' then
      CheckTimBidir_2_i <= TRUE;
    else
      CheckTimBidir_2_i <= FALSE;
    end if;
  end process GenCheckTimBidir_2_i;
  
  CheckTimBidir_2 <= transport CheckTimBidir_2_i and not(DPAR = 'Z');

  DPARcheck :  DbusSetupHoldCheck(DPAR, CLK, XHOLD_N,
                                  SETUP => tDPIS,  HOLD => tDPIH, 
                                  PATH => "DPAR", 
                                  DelayedData => DPAR'Delayed(abs(tDPIH)),
                                  EN_CHECKING => CheckTimBidir_2);

  ---
  Chk_A_en <= not(RESET_N = '0') and not(A = TRI_STATE32);
  
  SigA          : CondSetupHoldCheck(A, CLK, EDGE => RISING,
                                     SETUP => tAS,  HOLD => tAH, 
                                     PATH => "A", 
                                     DelayedData => A'Delayed(abs(tAH)),
                                     EN_CHECKING => Chk_A_en);
                                   
  ---
  Chk_FINS1_en <= not(RESET_N = '0') and not(FINS1 = 'Z');
  
  SigFINS1      : CondSetupHoldCheck(FINS1, CLK, EDGE => RISING,
                                     SETUP => tFIS,  HOLD => tFIH, 
                                     PATH => "FINS1", 
                                     DelayedData => FINS1'Delayed(abs(tFIH)),
                                     EN_CHECKING => Chk_FINS1_en);
                                 
  ---
  Chk_FINS2_en <= not(RESET_N = '0') and not(FINS2 = 'Z');
  
  SigFINS2      : CondSetupHoldCheck(FINS2, CLK, EDGE => RISING,
                                     SETUP => tFIS,  HOLD => tFIH, 
                                     PATH => "FINS2", 
                                     DelayedData => FINS2'Delayed(abs(tFIH)),
                                     EN_CHECKING => Chk_FINS2_en);
                                 
  ---
  Chk_INST_en <= not(RESET_N = '0') and not(INST = 'Z');
  
  SigINST       : CondSetupHoldCheck(INST, CLK, EDGE => RISING,
                                     SETUP => tINS,  HOLD => tINH, 
                                     PATH => "INST", 
                                     DelayedData => INST'Delayed(abs(tINH)),
                                     EN_CHECKING => Chk_INST_en);
                                 
  ---
  Chk_FXACK_en <= not(RESET_N = '0') and not(FXACK = 'Z');
  
  SigFXACK      : CondSetupHoldCheck(FXACK, CLK, EDGE => RISING,
                                     SETUP => tFXS,  HOLD => tFXH, 
                                     PATH => "FXACK", 
                                     DelayedData => FXACK'Delayed(abs(tFXH)),
                                     EN_CHECKING => Chk_FXACK_en);
                                 
  ---
  Chk_FLUSH_en <= not(RESET_N = '0') and not(FLUSH = 'Z');
  
  SigFLUSH      : CondSetupHoldCheck(FLUSH, CLK, EDGE => RISING,
                                     SETUP => tFLS,  HOLD => tFLH, 
                                     PATH => "FLUSH", 
                                     DelayedData => FLUSH'Delayed(abs(tFLH)),
                                     EN_CHECKING => Chk_FLUSH_en);
                                 
  ---
  Chk_MHOLDA_N_en <= not(RESET_N = '0') and not(MHOLDA_N = 'Z');
  
  SigMHOLDA_N   : CondSetupHoldCheck(MHOLDA_N, CLK, EDGE => FALLING,
                                     SETUP => tMHS,  HOLD => tMHH, 
                                     PATH => "MHOLDA_N", 
                                     DelayedData => MHOLDA_N'Delayed(abs(tMHH)),
                                     EN_CHECKING => Chk_MHOLDA_N_en);
                                 
  ---
  Chk_MHOLDB_N_en <= not(RESET_N = '0') and not(MHOLDB_N = 'Z');
  
  SigMHOLDB_N   : CondSetupHoldCheck(MHOLDB_N, CLK, EDGE => FALLING,
                                     SETUP => tMHS,  HOLD => tMHH, 
                                     PATH => "MHOLDB_N", 
                                     DelayedData => MHOLDB_N'Delayed(abs(tMHH)),
                                     EN_CHECKING => Chk_MHOLDB_N_en);
                                 
  ---
  Chk_BHOLD_N_en <= not(RESET_N = '0') and not(BHOLD_N = 'Z');
  
  SigBHOLD_N    : CondSetupHoldCheck(BHOLD_N, CLK, EDGE => FALLING,
                                     SETUP => tMHS,  HOLD => tMHH, 
                                     PATH => "BHOLD_N", 
                                     DelayedData => BHOLD_N'Delayed(abs(tMHH)),
                                     EN_CHECKING => Chk_BHOLD_N_en);
                                 
  ---
--  SigCHOLD_N    : SetupHoldCheck(CHOLD_N, CLK, EDGE => FALLING,
--                                 SETUP => tMHS,  HOLD => tMHH, 
--                                 PATH => "CHOLD_N", 
--                                 DelayedData => CHOLD_N'Delayed(abs(tMHH)));
                                 
  ---
--  SigCCCV       : SetupHoldCheck(CCCV, CLK, EDGE => FALLING,
--                                 SETUP => tMHS,  HOLD => tMHH, 
--                                 PATH => "CCCV", 
--                                 DelayedData => CCCV'Delayed(abs(tMHH)));
                                 
  ---
  Chk_MDS_N_en <= not(RESET_N = '0') and not(MDS_N = 'Z');
  
  SigMDS_N      : CondSetupHoldCheck(MDS_N, CLK, EDGE => FALLING,
                                     SETUP => tMDS,  HOLD => tMDH, 
                                     PATH => "MDS_N", 
                                     DelayedData => MDS_N'Delayed(abs(tMDH)),
                                     EN_CHECKING => Chk_MDS_N_en);
                                 
  ---
  Chk_APAR_en <= not(RESET_N = '0') and not(APAR = 'Z');
  
  SigAPAR       : CondSetupHoldCheck(APAR, CLK, EDGE => RISING,
                                     SETUP => tAPS,  HOLD => tAPH, 
                                     PATH => "APAR", 
                                     DelayedData => APAR'Delayed(abs(tAPH)),
                                     EN_CHECKING => Chk_APAR_en);
                                 
  
  ---
  Chk_IFPAR_en <= not(RESET_N = '0') and not(IFPAR = 'Z');
  
  SigIFPAR      : CondSetupHoldCheck(IFPAR, CLK, EDGE => RISING,
                                     SETUP => tIFS,  HOLD => tIFH, 
                                     PATH => "IFPAR", 
                                     DelayedData => IFPAR'Delayed(abs(tIFH)),
                                     EN_CHECKING => Chk_IFPAR_en);
                                 
  
end vhdl_behavioral;
-------------------------------------------------------------------------------
-- File name : fpurt.vhd
-- Title : FPURT
-- project : SPARC
-- Library : FPURT_LIB
-- Author(s) : Maxime ROCCA
-- Purpose : definition of entity FPURT (with simulation and timing parameters)
--           to be used for board simulation.
-- notes :   
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        |   MR   | 94-03-04    | first version.
--.............................................................................
-- v 1.1        |   MR   | 94-05-03    |
-- + modification of the value in the record technology.
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
library FPURT_LIB;
use FPURT_LIB.FPURTCompPck.FPURTGeneric;
use FPURT_LIB.FPURTTimPar.all;

entity FPURT is
  generic(
          T : temperature := T_BOARD;
          V : voltage := V_BOARD;
          PROCES : proces_type := PROCES_BOARD;
          LOAD : capacitance := LOAD_BOARD
  );
  
  port(
    CLK   : in std_logic; -- clock signal
    
    -- Integer Unit Interface Signals
    FP_N    : inout std_logic; --* Floating-point (Fp) Present
    FCC     : inout std_logic_vector(1 downto 0); --* Fp Condition Codes
    FCCV    : inout std_logic; --* Fp Condition Codes Valid
    FHOLD_N : inout std_logic; --* Fp Hold
    FEXC_N  : inout std_logic; --* Fp EXCeption
    FIPAR   : inout std_logic; --* Fpu to Iu control PARity
    FXACK   : in std_logic; -- Fp eXception ACKnowledge
    INST    : in std_logic; -- INSTruction fetch
    FINS1   : in std_logic; -- Fp INStruction in buffer 1
    FINS2   : in std_logic; -- Fp INStruction in buffer 2
    FLUSH   : in std_logic; -- Fp instruction fLUSH
    IFPAR   : in std_logic; -- Iu to Fpu control PARity
    
    -- System/Memory Interface Signals
    A          : in std_logic_vector(31 downto 0); -- Address bus
    APAR       : in std_logic; -- Address bus PARity
    D          : inout std_logic_vector(31 downto 0); -- Data bus
    DPAR       : inout std_logic; -- Data bus PARity
    DOE_N      : in std_logic;  -- Data Output Enable
    COE_N      : in std_logic;  -- Control Output Enable
    MHOLDA_N   : in std_logic;  -- Memory HOLD
    MHOLDB_N   : in std_logic;  -- Memory HOLD
    BHOLD_N    : in std_logic;  -- Bus HOLD
    MDS_N      : in std_logic;  -- Memory Data Strobe
    FNULL      : inout std_logic; --* Fpu NULLify cycle
    RESET_N    : in std_logic;  -- Reset signal
    HWERROR_N  : out std_logic; -- Hardware error detected
    CMODE_N    : in std_logic;  -- master/Checker MODE
    MCERR_N    : out std_logic; -- Comparison Error
    N602MODE_N : in std_logic;  -- Normal 602MODE Operation
    HALT_N     : in std_logic;  -- Halt mode
    
    -- Coprocessor Interface Signals
    CHOLD_N  : in std_logic; -- Coprocessor hold.
    CCCV     : in std_logic; -- Coprocessor Condition Code Valid.

    -- Test Access Port (TAP) signals
    TCLK   : in std_logic;  -- Test CLocK
    TRST_N : in std_logic;  -- Test ReSeT
    TMS    : in std_logic;  -- Test Mode Select
    TDI    : in std_logic;  -- Test Data In
    TDO    : out std_logic  -- Test Data Out
  );
end FPURT;


architecture WithTiming of FPURT is
  constant MHS_MC : technology := (
     (0.9375, 0.0025, 0.0, 0.0), -- real values
     (2.0, -0.20, 0.0, 0.0), 
     0.8, 1.3,
     47.0);

-- Configuration of components
--  for all : FPURTGeneric use entity FPURTLIB.FPURTGeneric(Behave);
       
begin
  MyFPURTGeneric: FPURTGeneric
    generic map(
      tCY      => tCY,  
      tCHL     => tCHL,
      tAS      => CalcDelay(tAS     , MHS_MC, T, V, PROCES),
      tAH      => CalcDelay(tAH     , MHS_MC, T, V, PROCES), 
      tDIS     => CalcDelay(tDIS    , MHS_MC, T, V, PROCES),  
      tDIH     => CalcDelay(tDIH    , MHS_MC, T, V, PROCES),
      tDOD     => CalcDelay(tDOD    , MHS_MC, T, V, PROCES, LOAD), 
      tDOH     => CalcDelay(tDOH    , MHS_MC, T, V, PROCES, LOAD),  
      tDOFFL   => CalcDelay(tDOFFL  , MHS_MC, T, V, PROCES, LOAD),
      tDOHFL   => CalcDelay(tDOHFL  , MHS_MC, T, V, PROCES, LOAD),
      tDOFOE   => CalcDelay(tDOFOE  , MHS_MC, T, V, PROCES, LOAD),
      tDONOE   => CalcDelay(tDONOE  , MHS_MC, T, V, PROCES, LOAD),
      tDOHOE   => CalcDelay(tDOHOE  , MHS_MC, T, V, PROCES, LOAD),
      tFIS     => CalcDelay(tFIS    , MHS_MC, T, V, PROCES),
      tFIH     => CalcDelay(tFIH    , MHS_MC, T, V, PROCES),
      tINS     => CalcDelay(tINS    , MHS_MC, T, V, PROCES),
      tINH     => CalcDelay(tINH    , MHS_MC, T, V, PROCES),
      tFXS     => CalcDelay(tFXS    , MHS_MC, T, V, PROCES),
      tFXH     => CalcDelay(tFXH    , MHS_MC, T, V, PROCES),
      tFLS     => CalcDelay(tFLS    , MHS_MC, T, V, PROCES),
      tFLH     => CalcDelay(tFLH    , MHS_MC, T, V, PROCES),
      tRES     => CalcDelay(tRES    , MHS_MC, T, V, PROCES),
      tREH     => CalcDelay(tREH    , MHS_MC, T, V, PROCES),
      tMHS     => CalcDelay(tMHS    , MHS_MC, T, V, PROCES),
      tMHH     => CalcDelay(tMHH    , MHS_MC, T, V, PROCES),
      tMDS     => CalcDelay(tMDS    , MHS_MC, T, V, PROCES),
      tMDH     => CalcDelay(tMDH    , MHS_MC, T, V, PROCES),
      tFHD     => CalcDelay(tFHD    , MHS_MC, T, V, PROCES, LOAD),
      tFHH     => CalcDelay(tFHH    , MHS_MC, T, V, PROCES, LOAD),
      tFHDFI   => CalcDelay(tFHDFI  , MHS_MC, T, V, PROCES, LOAD),
      tFHDFL   => CalcDelay(tFHDFL  , MHS_MC, T, V, PROCES, LOAD),
      tFHDMH   => CalcDelay(tFHDMH  , MHS_MC, T, V, PROCES, LOAD),
      tFCCVD   => CalcDelay(tFCCVD  , MHS_MC, T, V, PROCES, LOAD),
      tFCCVH   => CalcDelay(tFCCVH  , MHS_MC, T, V, PROCES, LOAD),
      tFCCVDFL => CalcDelay(tFCCVDFL, MHS_MC, T, V, PROCES, LOAD),
      tFCCVDMH => CalcDelay(tFCCVDMH, MHS_MC, T, V, PROCES, LOAD),
      tFCCD    => CalcDelay(tFCCD   , MHS_MC, T, V, PROCES, LOAD),
      tFCCH    => CalcDelay(tFCCH   , MHS_MC, T, V, PROCES, LOAD),
      tFED     => CalcDelay(tFED    , MHS_MC, T, V, PROCES, LOAD),
      tFEH     => CalcDelay(tFEH    , MHS_MC, T, V, PROCES, LOAD),
      tFND     => CalcDelay(tFND    , MHS_MC, T, V, PROCES, LOAD),
      tFNH     => CalcDelay(tFNH    , MHS_MC, T, V, PROCES, LOAD),
      tAPS     => CalcDelay(tAPS    , MHS_MC, T, V, PROCES),
      tAPH     => CalcDelay(tAPH    , MHS_MC, T, V, PROCES),
      tDPIS    => CalcDelay(tDPIS   , MHS_MC, T, V, PROCES),
      tDPIH    => CalcDelay(tDPIH   , MHS_MC, T, V, PROCES),
      tDPOD    => CalcDelay(tDPOD   , MHS_MC, T, V, PROCES, LOAD),
      tDPOH    => CalcDelay(tDPOH   , MHS_MC, T, V, PROCES, LOAD),
      tIFS     => CalcDelay(tIFS    , MHS_MC, T, V, PROCES),
      tIFH     => CalcDelay(tIFH    , MHS_MC, T, V, PROCES),
      tFIPD    => CalcDelay(tFIPD   , MHS_MC, T, V, PROCES, LOAD),
      tFIPH    => CalcDelay(tFIPH   , MHS_MC, T, V, PROCES, LOAD),
      tMCD     => CalcDelay(tMCD    , MHS_MC, T, V, PROCES, LOAD),
      tMCH     => CalcDelay(tMCH    , MHS_MC, T, V, PROCES, LOAD),
      tCMS     => CalcDelay(tCMS    , MHS_MC, T, V, PROCES),
      tHAS     => CalcDelay(tHAS    , MHS_MC, T, V, PROCES),
      tHAH     => CalcDelay(tHAH    , MHS_MC, T, V, PROCES),
      tHAD     => CalcDelay(tHAD    , MHS_MC, T, V, PROCES, LOAD),
      tHAE     => CalcDelay(tHAE    , MHS_MC, T, V, PROCES, LOAD),
      tERD     => CalcDelay(tERD    , MHS_MC, T, V, PROCES, LOAD),
      tERH     => CalcDelay(tERH    , MHS_MC, T, V, PROCES, LOAD),
      
      tTCY     => tTCY,
      tTMS     => CalcDelay(tTMS , MHS_MC, T, V, PROCES),
      tTMH     => CalcDelay(tTMH , MHS_MC, T, V, PROCES),
      tTDIS    => CalcDelay(tTDIS, MHS_MC, T, V, PROCES),
      tTDIH    => CalcDelay(tTDIH, MHS_MC, T, V, PROCES),
      tTRS     => CalcDelay(tTRS , MHS_MC, T, V, PROCES),
      tTRH     => CalcDelay(tTRH , MHS_MC, T, V, PROCES),
      tTDOD    => CalcDelay(tTDOD, MHS_MC, T, V, PROCES, LOAD),
      tTDOH    => CalcDelay(tTDOH, MHS_MC, T, V, PROCES, LOAD)
    )
  
  port map(
    CLK,
    -- Integer Unit Interface Signals
    FP_N,    -- Floating-point (Fp) Present
    FCC,     -- Fp Condition Codes
    FCCV,    -- Fp Condition Codes Valid
    FHOLD_N, -- Fp Hold
    FEXC_N,  -- Fp EXCeption
    FIPAR,   -- Fpu to Iu control PARit
    FXACK,   -- Fp eXception ACKnowledge
    INST,    -- INSTruction fetch
    FINS1,   -- Fp INStruction in buffer 1
    FINS2,   -- Fp INStruction in buffer 2
    FLUSH,   -- Fp instruction fLUSH
    IFPAR,   -- Iu to Fpu control PARity
    
    -- System/Memory Interface Signals
    A,          -- Address bus
    APAR,       -- Address bus PARity
    D,          -- Data bus
    DPAR,       -- Data bus PARity
    DOE_N,      -- Data Output Enable
    COE_N,      -- Control Output Enable
    MHOLDA_N,   -- Memory HOLD
    MHOLDB_N,   -- Memory HOLD
    BHOLD_N,    -- Bus HOLD
    MDS_N,      -- Memory Data Strobe
    FNULL,      -- Fpu NULLify cycle
    RESET_N,    -- Reset signal
    HWERROR_N,  -- Hardware error detected
    CMODE_N,    -- Slave MODE
    MCERR_N,    -- Comparison Error
    N602MODE_N, -- Normal 602MODE Operation
    HALT_N,     -- Halt mode
    
    -- Coprocessor Interface Signals
    CHOLD_N, -- Coprocessor hold.
    CCCV,    -- Coprocessor Condition Code Valid.

    -- Test Access Port (TAP) signals
    TCLK,   -- Test CLocK
    TRST_N, -- Test ReSeT
    TMS,    -- Test Mode Select
    TDI,    -- Test Data In
    TDO     -- Test Data Out
  );

end WithTiming;
