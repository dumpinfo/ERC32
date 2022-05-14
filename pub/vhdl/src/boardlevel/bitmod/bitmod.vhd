--============================================================================--
-- Design units : BitMod(BoardLevel) (Entity and architecture)
--
-- File name    : bitmod.vhd
--
-- Purpose      : The Bit Modulator receives serial input data, modulates it
--                and outputs it synchronously with the system clock. Different
--                modes of modulation can be configured in internal registers.
--                The registers can be read and written via a parallel 
--                interface. The Bit Modulator optionally performs BIST after
--                reset. For further information see the data sheet for the 
--                Bit Modulator dated January 1995.
--
-- Note         : All timing parameter names are compliant with Vital Level 0.
--                Selection of Worst, Typical and Best Case timing is performed
--                by the ESA.Simulation.SimCondition type generic SimCondition.
--
--                This model is modelled after the design (the design was not
--                synthesised from this model). The model has been extensively
--                verified versus the gate-level design on a clock-by-clock
--                basis, and is compliant to the data sheet for the
--                Bit Modulator dated January 1995.
--
--                Test is not allowed to change when Reset_N is de-asserted 
--                (checked in model).
--
--                Unknown values on input signals are handled as follows:
--                  'X' on Reset_N is treated as '1',
--                  'X' on Test    is treated as '0',
--                  'X' on A       is treated as '0',
--                  'X' on D       is treated as '0',
--                  'X' on CS_N    is treated as '1' during write and read
--                  'X' on RW_N    is treated as '1' during write and
--                                            as '0' during read
--                  'X' on SData   is propagated to MData output.
--
--                Unknown values on the Clk input are handled using the 
--                Std_Logic_1164 Rising_Edge() and Falling_Edge() functions in 
--                the functional core. Clk is checked for unknown values when 
--                neither edge is detected, which is also done during reset.
--
--                The SClk input is sampled in the functional core with Clk,
--                and is checked for unknown values in the Board-level 
--                architecture.
--
--                All conversions of logical values are performed in the
--                functional core, except for the signals Reset_N and Test, 
--                which is performed in the Board-level architecture.
--                The conversion of logical values and X-Checking is generally 
--                only performed when the value is needed.
--
--                The timing figures have been taken from the data sheet for the
--                Bit Modulator dated January 1995. The timing figures are 
--                based on 25 pF load on the outputs. The timing parameter
--                names are compliant with Vital Level 0.
--
--                All scheduling of output delays and timing checking are 
--                performed in the Board-level architecture.
--
-- Limitations  : BIST internal function not modelled, only the resulting delay
--                after reset. Manufacturing test is not modelled.
--
--                Do NEVER use this timing modelling instead of worst case
--                timing analysis; the timing modelling is not always accurate
--                and is only intended to be used for simulation.
--
--                The timing checks of parameters TRes and T7 are approximated.
--
--                The Vital_Timing package is analysed into BitMod_Lib, since
--                the package has not yet been formally approved by the IEEE.
--
-- Errors       : None known 
--
-- Library      : BitMod_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                ESA.Simulation, 
--                ESA.Timing, 
--                BitMod_Lib.BitMod_Definition,
--                BitMod_Lib.BitMod_Timing.
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
-- 0.1     SH      1 July 95 New model
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Naming convention:
-- All external signals have been named as in the data sheet.
-- Active low signals are indicated by _N.
-- Internal, strength converted signals are named after their new strength, for
-- example _X01.
-- Internal signals without output delays are indicated by _NoTime.
--------------------------------------------------------------------------------

library IEEE;
use IEEE.Std_Logic_1164.all;

library ESA;
use ESA.Simulation.all;                                -- For simulation cond.
use ESA.Timing.all;                                    -- For time arrays 

library BitMod_Lib;
use BitMod_Lib.BitMod_Timing.all;                      -- Default timing

entity BitMod is
   generic(
      SimCondition:      SimConditionType := WorstCase;
      InstancePath:      String           := "BitMod:";
      TimingChecksOn:    Boolean          := False;

      tperiod_Clk:       TimeArray        := tperiod_Clk;       -- TClk
      tpw_Clk_posedge:   TimeArray        := tpw_Clk_posedge;   -- TCHi
      tpw_Clk_negedge:   TimeArray        := tpw_Clk_negedge;   -- TCLo
      tpw_CSN_negedge:   TimeArray        := tpw_CSN_negedge;   -- T1
      tsetup_D_CSN:      TimeArray        := tsetup_D_CSN;      -- T2
      thold_D_CSN:       TimeArray        := thold_D_CSN;       -- T3
      tpd_CSN_D_negedge: TimeArray        := tpd_CSN_D_negedge; -- T4
      tpd_CSN_D_posedge: TimeArray        := tpd_CSN_D_posedge; -- T5
      tpd_A_D:           TimeArray        := tpd_A_D;           -- T6
      tpd_Clk_MData:     TimeArray01      := tpd_Clk_MData);    -- T9

   port(
      -- System signals (4)
      Test:    in     Std_Logic_Vector(0 to 1);        -- Test mode
      Clk:     in     Std_Logic;                       -- Master Clock
      Reset_N: in     Std_Logic;                       -- Master Reset

      -- Interface to internal registers (12)
      A:       in     Std_Logic_Vector(0 to 1);        -- Address bus
      CS_N:    in     Std_Logic;                       -- Chip select
      RW_N:    in     Std_Logic;                       -- Read/write
      D:       inout  Std_Logic_Vector(0 to 7);        -- Bidir. bus
 
      -- Serial Interface (3)
      SClk:    in     Std_Logic;                       -- Serial clock
      SData:   in     Std_Logic;                       -- Serial input
      MData:   out    Std_Logic);                      -- Serial output

end BitMod; --====================== End of entity ===========================--

--=============================== Architecture ===============================--
 
library IEEE;
use IEEE.Vital_Timing.all;

library BitMod_Lib; 
use BitMod_Lib.BitMod_Definition.all;                -- For custom functions
 
architecture BoardLevel of BitMod is
   -----------------------------------------------------------------------------
   -- Component declarations.
   -----------------------------------------------------------------------------
   component BitMod_Core
      generic(
         InstancePath:  String := "BitMod_Core:");    -- For reports
      port(
         -- System signals
         Test0:   in    Std_ULogic;                    -- Test mode
         Clk:     in    Std_ULogic;                    -- Master Clock
         Reset_N: in    Std_ULogic;                    -- Master Reset
 
         -- Interface to internal registers
         A:       in    Std_Logic_Vector(0 to 1);      -- Address bus
         CS_N:    in    Std_ULogic;                    -- Chip select
         RW_N:    in    Std_ULogic;                    -- Read/write
         D_In:    in    Std_Logic_Vector(0 to 7);      -- Data bus input
         D_Out:   out   Integer range 0 to 255;        -- Data bus output
         DEnable: out   Boolean;                       -- Data bus enable

         -- Serial Interface
         SClk:    in    Std_ULogic;                    -- Serial Clock
         SData:   in    Std_ULogic;                    -- Serial input 
         MData:   out   Std_ULogic);                   -- Serial output
   end component;

   -----------------------------------------------------------------------------
   -- Local signal declarations.
   -----------------------------------------------------------------------------
   -- Signal declarations for un-delayed external signals
   signal MData_NoTime:     Std_ULogic;

   -- Signal declarations for internal registers output data generation
   signal DEn_NoTime:       Boolean;
   signal DEn_Delayed:      Boolean;
   signal D_NoTime:         Integer range 0 to 255;
   signal D_Delayed:        Integer range 0 to 255;

   -- Signal declarations for conversion to type X01
   signal Reset_N_X01:      Std_ULogic;
   signal Test_X01:         Std_ULogic_Vector(0 to 1);

   -----------------------------------------------------------------------------
   -- The following timing parameters are architecture dependent and do not 
   -- change for varying simulation conditions.
   -----------------------------------------------------------------------------
   -- Minimum number of Clk cycles that Reset_N must be asserted
   constant tpw_ResetN_negedge: Integer := 3;          -- TRes
   -- Number of Clk clock periods per SClk period
   constant tperiod_SClk:       Integer := 4;          -- TSCk
   -- Minimum number of Clk cycles that CS_N must be de-asserted
   constant tpw_CSN_posedge:    Integer := 4;          -- T7

begin --========================== Architecture ==============================--

   -----------------------------------------------------------------------------
   -- Strength stripping to X01 using Std_Logic_1164 provided subprograms.
   -----------------------------------------------------------------------------
   Reset_N_X01   <= To_X01(Reset_N);                   -- Semi-static signal
   Test_X01      <= To_StdULogicVector(To_X01(Test));  -- Static signal

   -----------------------------------------------------------------------------
   -- Check for unknown values on the SClk input.
   -- The Clk input is checked in the functional core.
   -----------------------------------------------------------------------------
   CheckDynamicInputs: process(SClk)
   begin
      assert not (Is_X(SClk) and (Now /= 0 ns))        -- No assertions at start
         report InstancePath & " 'X' on SClk input"          
         severity Error;
   end process CheckDynamicInputs;

   -----------------------------------------------------------------------------
   -- Check for unknown values on the static inputs and that they only change
   -- during reset. Check for unknown values on Reset_N.
   -- All other signals are checked for unknown values in the functional core.
   -- Activating production test does not change the state of the model, and has
   -- therefore Severity_Level Note.
   -----------------------------------------------------------------------------
   CheckStaticInputs: process(Reset_N_X01, Test_X01)
   begin
      if (Reset_N_X01='1') and (Now /= 0 ns) then
         -- No assertions at start-up or when Reset_N is asserted
         assert (Test_X01(1)='0')
            report InstancePath & " Prod. test not modelled"     severity Note;
         assert not Is_X(Test_X01)                     -- Note: done on a vector
            report InstancePath & " 'X' on Test input"           severity Error;

         -- Check if the static pin changed after reset
         assert not Test_X01'Event
            report InstancePath & " Test changed after reset"    severity Error;

      elsif Reset_N_X01'Event and (Now /= 0 ns) then   -- Check for X on Reset_N
         assert not Is_X(Reset_N_X01)
            report InstancePath & " 'X' on Reset_N input"        severity Error;
      end if;
   end process CheckStaticInputs;

   -----------------------------------------------------------------------------
   -- Timing checks on inputs (setup, hold, period, pulse width).
   -----------------------------------------------------------------------------
   TimingGenerate: if TimingChecksOn generate
      TimingCheck: process(Clk, SClk, D, CS_N, RW_N, Reset_N_X01)
         -- Variables containing information for period checkers
         variable Period_Clk:       VitalPeriodDataType := VitalPeriodDataInit;
         variable Period_CSN:       VitalPeriodDataType := VitalPeriodDataInit;

         -- Variables containing information for setup and hold checkers
         variable Timing_D:         VitalTimingDataType := VitalTimingDataInit;

         -- Variable indicating whether a timing violation has occurred
         variable Violation:        X01                 := '0';

         -- Variables for enabling timing checkers
         variable DataCheckEnabled: Boolean             := False;

         -- Number of falling Clk edges during reset
         variable Period_Reset:     Integer             := tpw_ResetN_negedge;
         -- Number of Clk periods per SCk periods, w.r.t. falling edges
         variable Period_SClk:      Integer             := tperiod_SClk;
         -- Number of falling Clk edges CS_N must be de-asserted
         variable Period_CSN_Clk:   Integer             := tpw_CSN_posedge;
 
      begin
         -----------------------------------------------------------------------
         -- Enabling of various checkers.
         -----------------------------------------------------------------------
         -- Enables the setup and hold checker for D during write operations.
         -- The checker is enabled when both CS_N and RW_N are asserted, until 
         -- the next read access begins, since the data hold constraint is 
         -- longer than the time either CS_N or RW_N is de-asserted after write.
         if ((Falling_Edge(CS_N) and To_X01(RW_N)='0') or
             (Falling_Edge(RW_N) and To_X01(CS_N)='0')) then
            DataCheckEnabled := True;                  -- Enable checker
         elsif (Falling_Edge(CS_N) and To_X01(RW_N)='1') then
            DataCheckEnabled := False;                 -- Disable checker
         end if;
 
         -----------------------------------------------------------------------
         -- Reset_N low time w.r.t. Clk (TRes) 
         -- Asserts that there are at least tpw_ResetN_negedge number of 
         -- falling Clk edges during the assertion of Reset_N.
         -- This timing checker is approximated w.r.t. the data sheet. Only the
         -- number of Clk edges are counted for tpw_ResetN_negedge.
         -----------------------------------------------------------------------
         if (Falling_Edge(Clk) and Reset_N_X01='0' and (Period_Reset>0)) then
            Period_Reset := Period_Reset - 1;
         end if;
         if Falling_Edge(Reset_N_X01) then             -- Reset begins
            Period_Reset := tpw_ResetN_negedge;
         elsif Rising_Edge(Reset_N_X01) then           -- Reset ends
            assert (Period_Reset = 0)
               report InstancePath & " Signal width too short on Reset_N"
                  severity Error;
         end if;
  
         -----------------------------------------------------------------------
         -- The internal register interface is checked for illegal events. (T8) 
         -----------------------------------------------------------------------
         -- RW_N may not change when CS_N is asserted.
         assert not (RW_N'Event and To_X01(CS_N)='0' and Reset_N_X01='1')
            report InstancePath & " RW_N event while CS_N asserted"
               severity Warning;

         -----------------------------------------------------------------------
         -- Checkers using custom made subprograms.
         -----------------------------------------------------------------------
         -- SClk period (TSCk)
         -- Checks the relation between the two clock inputs SClk and Clk.
         PeriodCheck(
            Violation         => Violation,
            PeriodData        => Period_SClk,
            TestSignal        => SClk,
            TestSignalName    => "SClk",
            RefSignal         => Clk,
            RefSignalName     => "Clk",
            Period            => tperiod_SClk,
            FasterThanRef     => False,
            CheckEnabled      => Reset_N_X01='1',
            HeaderMsg         => InstancePath,
            XOn               => False,
            MsgOn             => True,
            MsgSeverity       => Warning);

         -- CS_N de-assertion width (T7)
         CheckWidth(
            Violation         => Violation,
            PeriodData        => Period_CSN_Clk,
            TestSignal        => CS_N,
            TestSignalName    => "CS_N",
            TestSignalLevel   => '1',
            RefSignal         => Clk,
            RefSignalName     => "Clk",
            CheckEnabled      => Reset_N_X01='1',
            HeaderMsg         => InstancePath,
            XOn               => False,
            MsgOn             => True,
            MsgSeverity       => Warning);

         -----------------------------------------------------------------------
         -- Timing checkers using Vital subprograms. 
         -----------------------------------------------------------------------
         -- Clk period, high and low times (TClk, TCLo, TCHi)
         VitalPeriodPulseCheck(
            Violation         => Violation,
            PeriodData        => Period_Clk,
            TestSignal        => Clk,
            TestSignalName    => "Clk",
            Period            => tperiod_Clk(SimCondition),
            PulseWidthHigh    => tpw_Clk_posedge(SimCondition),
            PulseWidthLow     => tpw_Clk_negedge(SimCondition),
            CheckEnabled      => True,
            HeaderMsg         => InstancePath,
            XOn               => False,
            MsgOn             => True,
            MsgSeverity       => Warning);

         -- CS_N assertion width during write access (T1)
         VitalPeriodPulseCheck(
            Violation         => Violation,
            PeriodData        => Period_Clk,
            TestSignal        => CS_N,
            TestSignalName    => "CS_N",
            PulseWidthLow     => tpw_CSN_negedge(SimCondition),
            CheckEnabled      => To_X01(RW_N)='0',
            HeaderMsg         => InstancePath,
            XOn               => False,
            MsgOn             => True,
            MsgSeverity       => Error);

         -- D setup & hold w.r.t. CS_N (T2, T3)
         VitalSetupHoldCheck(
            Violation         => Violation,
            TimingData        => Timing_D,
            TestSignal        => D,
            TestSignalName    => "D",
            RefSignal         => CS_N,
            RefSignalName     => "CS_N",
            SetupHigh         => tsetup_D_CSN(SimCondition),
            SetupLow          => tsetup_D_CSN(SimCondition),
            HoldHigh          => thold_D_CSN(SimCondition),
            HoldLow           => thold_D_CSN(SimCondition),
            CheckEnabled      => DataCheckEnabled,
            RefTransition     => 'R',
            HeaderMsg         => InstancePath,
            XOn               => False,
            MsgOn             => True,
            MsgSeverity       => Warning);

      end process TimingCheck;
   end generate TimingGenerate;

   -----------------------------------------------------------------------------
   -- Assignment of output delays.
   -----------------------------------------------------------------------------
   -- Generation of tristate or drive for the external data bus. D_NoTime is
   -- delayed w.r.t. the address. DEn_NoTime is delayed, with different timing
   -- for tristate. The D assignment includes an Integer to Std_LogicVector 
   -- conversion.
 
   DEn_Delayed <= transport DEn_NoTime after tpd_CSN_D_negedge(SimCondition)
                     when DEn_NoTime else                                 -- T4
                  DEn_NoTime after tpd_CSN_D_posedge(SimCondition);       -- T5
 
   D_Delayed   <= transport D_NoTime   after tpd_A_D(SimCondition);       -- T6
 
   D           <= To_StdLogicVector(D_Delayed, 8)
                     when DEn_Delayed else
                  (others => 'Z');

   MData       <= MData_NoTime after VitalCalcDelay(                      -- 9
                                        NewVal => MData_NoTime,
                                        OldVal => MData_NoTime'Last_Value,
                                        Delay  => tpd_Clk_MData(SimCondition));

   -----------------------------------------------------------------------------
   -- Instantiation of components.
   -----------------------------------------------------------------------------
   -- The buses A and D are not converted to the unresolved type 
   -- Std_ULogic_Vector since a conversion is done to the type Integer in the 
   -- functional core when the signal values are used.
   --
   -- Only the LSB of the Test bus is used in the functional core. The MSB is
   -- handled in the CheckStaticInputs process.

   FunctionalCore: BitMod_Core
      generic map(
         InstancePath => InstancePath)
      port map(
         Test0        => Test_X01(0),                  -- Only LSB used in core
         Clk          => Clk,                          -- Falling/Rising_Edge
         Reset_N      => Reset_N_X01,
         A            => A,                            -- To_X01 made in core
         CS_N         => CS_N,                         -- To_X01 made in core
         RW_N         => RW_N,                         -- To_X01 made in core
         D_In         => D,                            -- To_X01 made in core
         D_Out        => D_NoTime,                     -- Integer
         DEnable      => DEn_NoTime,
         SClk         => SClk,                         -- Falling/Rising_Edge
         SData        => SData,                        -- To_X01 made in core
         MData        => MData_NoTime);

end BoardLevel; --================ End of architecture =======================--
