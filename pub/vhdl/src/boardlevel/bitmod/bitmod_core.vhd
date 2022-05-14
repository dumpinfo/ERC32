--============================================================================--
-- Design units : BitMod_Core(Behavioural) (Entity and architecture)
--
-- File name    : bitmod_core.vhd
--
-- Purpose      : This is the functional core of the BitMod implementing
--                all the Bit Modulator functionality, except the 
--                multiplexing of the data bus D which is performed in the 
--                Board-level architecture.
--
--                The Bit Modulator receives serial input data, modulates it
--                and outputs it synchronously with the system clock. Different
--                modes of modulation can be configured in internal registers.
--                The registers can be read and written via a parallel
--                interface. The Bit Modulator optionally performs BIST after
--                reset. For further information see the data sheet for the
--                Bit Modulator.
--
-- Note         : This model is modelled after the design (the design was not
--                synthesized from this model). The model has been extensively
--                verified versus the gate-level design on clock-by-clock
--                basis.
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
--                All scheduling of output delays and timing checking are
--                performed in the Board-level architecture.
--
-- Limitations  : BIST internal function not modelled, only the resulting delay
--                after reset. Manufacturing test is not modelled.
--
--                The modelling is not fully correct w.r.t. a typical RAM I/F, 
--                since some relaxations have been introduced.
--
-- Errors       : None known
--
-- Library      : BitMod_Lib
--
-- Dependencies : IEEE.Std_Logic_1164,
--                BitMod_Lib.BitMod_Definition.
--
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
-- Naming convention: Active low signals are indicated by _N.
-- All external signals have been named as in the data sheet.
--------------------------------------------------------------------------------

library IEEE;
use IEEE.Std_Logic_1164.all;                           -- For data types

entity BitMod_Core is
   generic(
      InstancePath: String := "BitMod_Core:"); -- For assertions
   port(
      -- System signals
      Test0:   in   Std_ULogic;                        -- Test mode
      Clk:     in   Std_ULogic;                        -- Master Clock
      Reset_N: in   Std_ULogic;                        -- Master Reset

      -- Interface to internal registers
      A:       in   Std_Logic_Vector(0 to 1);          -- Address bus
      CS_N:    in   Std_ULogic;                        -- Chip select
      RW_N:    in   Std_ULogic;                        -- Read/write
      D_In:    in   Std_Logic_Vector(0 to 7);          -- Data bus input
      D_Out:   out  Integer range 0 to 255;            -- Data bus output
      DEnable: out  Boolean;                           -- Data bus enable

      -- Serial Interface
      SClk:    in   Std_ULogic;                        -- Serial clock
      SData:   in   Std_ULogic;                        -- Serial input 
      MData:   out  Std_ULogic);                       -- Serial output 

end BitMod_Core; --================== End of entity ==========================--

--=============================== Architecture ===============================--
 
library BitMod_Lib; 
use BitMod_Lib.BitMod_Definition.all;                  -- For custom functions

architecture Behavioural of BitMod_Core is
   -----------------------------------------------------------------------------
   -- Definitions for the internal registers
   -----------------------------------------------------------------------------
   constant RegisterSize:    Integer := 4;
   subtype  DataRange     is Integer range 0 to 255;
   subtype  AddressRange  is Integer range 0 to RegisterSize-1;
   type     RegisterType  is array(AddressRange) of DataRange;
   constant RegisterInit:    RegisterType := (0, 1, 2, 255);

   -----------------------------------------------------------------------------
   -- Local signal declarations.
   -----------------------------------------------------------------------------
   signal   DWrite:          DataRange;                -- Data to write
   signal   AWrite:          AddressRange;             -- Address to write

begin --========================== Architecture ==============================--

   -----------------------------------------------------------------------------
   -- Implementation of all functionality driven by Clk.
   -- Generation of synchronisation marker and header octets;
   -- optional generation of trailer; synchronised writing of internal 
   -- registers data; handling of reset and BIST delay; the SClk input is
   -- sampled within the process.
   -----------------------------------------------------------------------------
   ClkRegion: process(Reset_N, Clk, A)
      variable A_Integer: Integer range 0 to 3;
      variable Registers: RegisterType;
   begin
      if Reset_N = '0' then                            
         -- Asynchronous reset of model
         -- To_X01 on Reset_N is done in Board-level
         Registers := RegisterInit;                    -- Initialize reg. file
         D_Out     <= Registers(A_Integer);
      elsif Rising_Edge(Clk) then   
         -- Rising Clk edge region
         Registers(AWrite) := DWrite;
         D_Out <= Registers(A_Integer);
      elsif Falling_Edge(Clk) then                    
         -- Falling Clk edge region
         -- Not implemented in this VHDL code
      else
         -- Check for unknown Clk value, since the model is not being reset,
         -- and neither rising nor falling Clk edge is detected.
         assert not (Is_X(Clk) and (Now /= 0 ns))      -- No assertions at start
           report InstancePath & " 'X' on Clk input" severity Error;
      end if;
      --------------------------------------------------------------------------
      -- Output register data on the internal bus whenever the address changes
      -- Only convert A to integer when it changes (used elsewhere)
      --------------------------------------------------------------------------
      if A'Event then
         -- X is treated as 0
         A_Integer := To_Integer(A, "A", InstancePath, Error);
         D_Out      <= Registers(A_Integer);
      end if;

   end process ClkRegion;

   -----------------------------------------------------------------------------
   -- Implementation of all asynchronous functionality.
   -- Latching of data to be written into the internal registers.
   -- Generation of external data bus enable. Checks for unknown values 
   -- are done for the input signals.
   -- The modelling is not fully correct w.r.t. a typical RAM I/F, since some
   -- relaxations have been introduced.
   -----------------------------------------------------------------------------
   AsynchronousRegion: process(CS_N, RW_N, Reset_N)
   begin
      if Reset_N = '0' then
         -- To_X01 on Reset_N is done in Board-level
         -- Asynchronous reset of model
         DEnable <= False;
      elsif Rising_Edge(CS_N) then 
         -- End of access
         if To_X01(RW_N, "RW_N", InstancePath, Error)='0' then
            -- Write access to internal registers
            -- X on CS_N is treated as no event (no access)
            -- X on RW_N is treated as 1 (no write access)
            -- X on A and D_In are treated as 0
            AWrite  <= To_Integer(A,    "A", InstancePath, Error);
            DWrite  <= To_Integer(D_In, "D", InstancePath, Error);
         end if;
            DEnable <= False;
      elsif Now /= 0 ns then
         -- Asynchronous behaviour
         -- Enabled for read cycles after Reset
         -- X on RW_N is treated as 0, X on CS_N is treated as 1
         DEnable <= (To_X01(RW_N, "RW_N", InstancePath, Error)='1') and
                    (To_X01(CS_N, "CS_N", InstancePath, Error)='0') and
                    (Reset_N='1');
      end if;
   end process AsynchronousRegion;

end Behavioural; --================ End of architecture ======================--
