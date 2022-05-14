-------------------------------------------------------------------------------
-- File name : fact_gen.vhd
-- Title : FACTGeneric
-- project : SPARC 
-- Library : FACTLIB
-- Author(s) : Remi CISSOU
-- Purpose :  modelling of the Fact library 
-- notes :   
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        | RC-MR  |             | first version
--.............................................................................
-------------------------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
-------------------------------------------------------------------------------
---------|---------|---------|---------|---------|---------|---------|--------|

------------------------------------------------------------------------------
-- 54AC245
------------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
library MMS;
use MMS.StdSim.all;
use MMS.StdTiming.all;
use MMS.StdIoImp.all;


entity AC245Generic is
  generic(
     tPLH_245ab : time := 0 ns; -- Propagation Delay An to Bn 
     tPHL_245ab : time := 0 ns; -- Propagation Delay An to Bn 
     tPZH_245ab : time := 0 ns; -- Output Enable Time (to Bn)
     tPZL_245ab : time := 0 ns; -- Output Enable Time (to Bn)
     tPHZ_245ab : time := 0 ns; -- Output Disable Time (to Bn)
     tPLZ_245ab : time := 0 ns; -- Output Disable Time (to Bn)
     tPLH_245ba : time := 0 ns; -- Propagation Delay Bn to An 
     tPHL_245ba : time := 0 ns; -- Propagation Delay Bn to An 
     tPZH_245ba : time := 0 ns; -- Output Enable Time (to An)
     tPZL_245ba : time := 0 ns; -- Output Enable Time (to An)
     tPHZ_245ba : time := 0 ns; -- Output Disable Time (to An)
     tPLZ_245ba : time := 0 ns  -- Output Disable Time (to An)
  );
  
  port(
    OE_N  : in std_logic;                       -- Output Enable
    T_R_N : in std_logic;                       -- Transmit Receive Input
    A     : inout std_logic_vector(7 downto 0); -- Side A TriState Inputs or
                                                -- TriState Outputs
    B     : inout std_logic_vector(7 downto 0)  -- Side B TriState Inputs or
                                                -- TriState Outputs
  );
end AC245Generic;



architecture Behavior of AC245Generic is
  constant tZtoXab : time := MinTime(tPHL_245ab, tPLH_245ab); -- SE suggestion
  constant tZtoXba : time := MinTime(tPHL_245ba, tPLH_245ba); -- SE suggestion
begin

  process (OE_N, T_R_N, A, B)
  begin 

    if OE_N = '1' then 
 
      for i in 0 to 7 loop 
        A(i) <= 'Z' after TpDelay(A(i), 'Z', tPLH_245ba, tPHL_245ba, tPZH_245ba,
                                             tPZL_245ba, tPHZ_245ba, 
                                             tPLZ_245ba);    
        B(i) <= 'Z' after TpDelay(B(i), 'Z', tPLH_245ab, tPHL_245ab, tPZH_245ab,
                                             tPZL_245ab, tPHZ_245ab, 
                                             tPLZ_245ab);
      end loop;

    elsif T_R_N = '0' and OE_N = '0' then 
   
      for i in 0 to 7  loop
        if B(i) = 'Z' then 
          A(i) <= 'X' after tZtoXba; 
        else     
          A(i) <= B(i) after TpDelay(A(i), B(i), tPLH_245ba, tPHL_245ba, 
                                                 tPZH_245ba, 
                                                 tPZL_245ba, tPHZ_245ba, 
                                                 tPLZ_245ba);
        end if;
      end loop;
      
      B <= (others => 'Z');
     
    elsif T_R_N = '1' and OE_N = '0' then 
   
      for i in 0 to 7  loop
        if A(i) = 'Z' then 
          B(i) <= 'X' after tZtoXab;
        else         
          B(i) <= A(i) after TpDelay(B(i), A(i), tPLH_245ab, tPHL_245ab, 
                                                 tPZH_245ab, 
                                                 tPZL_245ab, tPHZ_245ab, 
                                                 tPLZ_245ab);
        end if;
      end loop;
      
      A <= (others => 'Z');
     
    else
 
      A <= (others => 'U');
      B <= (others => 'U');
     
    end if;
     
  end process;
  
end Behavior;




------------------------------------------------------------------------------
-- 54AC377
------------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
library MMS;
use MMS.StdSim.all;
use MMS.StdTiming.all;
use MMS.StdIoImp.all;


entity AC377Generic is
  generic(
     tPLH_377 : time := 0 ns; -- Propagation Delay CP to Qn
     tPHL_377 : time := 0 ns; -- Propagation Delay CP to Qn
     tSD_377  : time := 0 ns; -- Dn Set Up to CP 
     tHD_377  : time := 0 ns; -- Dn Hold to CP
     tSC_377  : time := 0 ns; -- CE_N Set Up to CP
     tHC_377  : time := 0 ns  -- CE_N Hold to CP
  );
  
  port(
    CP    : in std_logic;                      -- Clock Pulse Input
    CE_N  : in std_logic;                      -- Clock Enable
    D     : in std_logic_vector(7 downto 0);   -- Data Inputs
    Q     : out std_logic_vector(7 downto 0)   -- Data Outputs
  );
end AC377Generic;

	
architecture Behavior of AC377Generic is

begin

  SetupHoldCheck (Data => D,
                  Ref  => CP, 
                  Edge => Rising,
                  SetUp => tSD_377, Hold => tHD_377,
                  Path  => "AC377",
                  DelayedData => D);
                  
  SetupHoldCheck (Data => CE_N,
                  Ref  => CP, 
                  Edge => Rising,
                  SetUp => tSC_377, Hold => tHC_377,
                  Path  => "AC377",
                  DelayedData => CE_N);
                  

  process  
  begin 
                  
  wait on CP ;
  
    if rising_edge(CP) and CE_N = '0' then 
 
      for i in 0 to 7 loop 
        if D(i) = '0' then 
          Q(i) <= D(i) after tPHL_377;
        elsif D(i) = '1' then 
          Q(i) <= D(i) after tPLH_377;
        else 
          Q(i) <= D(i);
        end if;
      end loop;
    
    end if;
  
  end process;
  
end Behavior;
-------------------------------------------------------------
-- File containing timing values for the FACT components.
-- 
-- ALL THE TIMING PARAMETERS are given at 125 degrees C,  
-- 4.5 Volts and in worst case process conditions.
-- WARNING: minimal values for output signal propagation 
-- delay in data sheets are usually given in best conditions,  
-- i.e -55 Celsius, 5.5 Volts and best case process conditions.
-- They must be re-calculated for worst case conditions.
-------------------------------------------------------------

package FACTTimPar is

  --------------------------------------------------------
  -- File containing nominal timing values fo AC245.
  --------------------------------------------------------
 
  constant tPLH_245 : time :=  9 ns;
  constant tPHL_245 : time :=  8 ns;
  constant tPZH_245 : time := 11 ns;
  constant tPZL_245 : time := 11 ns;
  constant tPHZ_245 : time := 11 ns;
  constant tPLZ_245 : time := 11 ns;

-------------------------------------------------------------
-- File containing nominal timing values for AC377.
-------------------------------------------------------------
 
  constant tPLH_377 : time := 11 ns;
  constant tPHL_377 : time := 12 ns;
  constant tSD_377  : time :=  6 ns;
  constant tHD_377  : time :=  3 ns;
  constant tSC_377  : time :=  6 ns;
  constant tHC_377  : time :=  2 ns;

end FACTTimPar;
-------------------------------------------------------------------------------
-- File name : Fact_Comp_Pck.vhd
-- Title : FactComponentPackage
-- project : SPARC 
-- Library : FACTLIB
-- Author(s) : Remi CISSOU
-- Purpose : package to declare components of entities
-- notes :  use this package whenever a component is instanciated. 
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        |  MR    | 93-12       | first version
--.............................................................................
-------------------------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
-------------------------------------------------------------------------------
---------|---------|---------|---------|---------|---------|---------|--------|

library IEEE;
use IEEE.Std_Logic_1164.all;
library MMS;
use MMS.StdSim.all;

package FACT_Comp_Pck is

  component AC245Generic
  generic(
     tPLH_245ab : time := 3 ns; -- Propagation Delay An to Bn 
     tPHL_245ab : time := 6 ns; -- Propagation Delay An to Bn 
     tPZH_245ab : time := 9 ns; -- Output Enable Time (to Bn)
     tPZL_245ab : time := 12 ns; -- Output Enable Time (to Bn)
     tPHZ_245ab : time := 15 ns; -- Output Disable Time (to Bn)
     tPLZ_245ab : time := 0 ns;  -- Output Disable Time (to Bn)
     tPLH_245ba : time := 3 ns; -- Propagation Delay Bn to An
     tPHL_245ba : time := 6 ns; -- Propagation Delay Bn to An
     tPZH_245ba : time := 9 ns; -- Output Enable Time  (to An)
     tPZL_245ba : time := 12 ns; -- Output Enable Time (to An)
     tPHZ_245ba : time := 15 ns; -- Output Disable Time (to An)
     tPLZ_245ba : time := 0 ns  -- Output Disable Time (to An)
  );
  
  port(
    OE_N  : in std_logic;                       -- Output Enable
    T_R_N : in std_logic;                       -- Transmit Receive Input
    A     : inout std_logic_vector(7 downto 0); -- Side A TriState Inputs or
                                                -- TriState Outputs
    B     : inout std_logic_vector(7 downto 0)  -- Side B TriState Inputs or
                                                -- TriState Outputs
  );
  end component; -- AC245Generic


  component AC245
  generic(
          T : temperature := T_BOARD;
          V : voltage := V_BOARD;
          PROCES : proces_type := PROCES_BOARD;
          LOADa : capacitance := LOAD_BOARD;
          LOADb : capacitance := LOAD_BOARD
  );
    
  port(
    OE_N  : in std_logic;                       -- Output Enable
    T_R_N : in std_logic;                       -- Transmit Receive Input
    A     : inout std_logic_vector(7 downto 0); -- Side A TriState Inputs or
                                                -- TriState Outputs
    B     : inout std_logic_vector(7 downto 0)  -- Side B TriState Inputs or
                                                -- TriState Outputs
  );
	end component; -- AC245


  component AC377Generic
  generic(
     tPLH_377 : time := 0 ns; -- Propagation Delay CP to Qn
     tPHL_377 : time := 3 ns; -- Propagation Delay CP to Qn
     tSD_377  : time := 6 ns; -- Dn Set Up to CP 
     tHD_377  : time := 9 ns; -- Dn Hold to CP
     tSC_377  : time := 12 ns; -- CE_N Set Up to CP
     tHC_377  : time := 15 ns  -- CE_N Hold to CP
  );
  
  port(
    CP    : in std_logic;                      -- Clock Pulse Input
    CE_N  : in std_logic;                      -- Clock Enable
    D     : in std_logic_vector(7 downto 0);   -- Data Inputs
    Q     : out std_logic_vector(7 downto 0)   -- Data Outputs
  );
	end component;  -- AC377Generic
	

  component AC377
  generic(
          T : temperature := T_BOARD;
          V : voltage := V_BOARD;
          PROCES : proces_type := PROCES_BOARD;
          LOAD : capacitance := LOAD_BOARD
  );
  
  
  port(
    CP    : in std_logic;                      -- Clock Pulse Input
    CE_N  : in std_logic;                      -- Clock Enable
    D     : in std_logic_vector(7 downto 0);   -- Data Inputs
    Q     : out std_logic_vector(7 downto 0)   -- Data Outputs
  );
	end component; -- AC377

  
end FACT_Comp_Pck;
-------------------------------------------------------------------------------
-- File name : fact_gen.vhd
-- Title : FACT_TECHGeneric
-- project : SPARC 
-- Library : FACTLIB
-- Author(s) : Remi CISSOU
-- Purpose :  modelling of the Fact library 
-- notes :   
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        | RC-MR  | 94-12       | first version
--.............................................................................
-------------------------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
-------------------------------------------------------------------------------
---------|---------|---------|---------|---------|---------|---------|--------|

------------------------------------------------------------------------------
-- 54AC245
------------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
library MMS;
use MMS.StdSim.all;
use MMS.StdTiming.all;
use MMS.StdIoImp.all;
library FACTLIB;
use FACTLIB.Fact_Comp_Pck.all;
use FACTLIB.FACTTimPar.all;


entity AC245 is
  generic(
          T : temperature := T_BOARD;
          V : voltage := V_BOARD;
          PROCES : proces_type := PROCES_BOARD;
          LOADa : capacitance := LOAD_BOARD;
          LOADb : capacitance := LOAD_BOARD
  );
    
  port(
    OE_N  : in std_logic;                       -- Output Enable
    T_R_N : in std_logic;                       -- Transmit Receive Input
    A     : inout std_logic_vector(7 downto 0); -- Side A TriState Inputs or
                                                -- TriState Outputs
    B     : inout std_logic_vector(7 downto 0)  -- Side B TriState Inputs or
                                                -- TriState Outputs
  );
end AC245;
	
architecture WithTiming of AC245 is
  constant FACT_TECH : technology := (
     (0.9675, 0.0013, 0.0, 0.0),
     (1.4, -0.08, 0.0, 0.0),
     0.7, 1.3,
     40.0);
   
begin
  MyAC245: AC245Generic
    generic map(
      tPLH_245ab  => CalcDelay(tPLH_245, FACT_TECH, T, V, PROCES, LOADb),
      tPHL_245ab  => CalcDelay(tPHL_245, FACT_TECH, T, V, PROCES, LOADb),
      tPZH_245ab  => CalcDelay(tPZH_245, FACT_TECH, T, V, PROCES, LOADb),
      tPZL_245ab  => CalcDelay(tPZL_245, FACT_TECH, T, V, PROCES, LOADb),
      tPHZ_245ab  => CalcDelay(tPHZ_245, FACT_TECH, T, V, PROCES, LOADb),
      tPLZ_245ab  => CalcDelay(tPLZ_245, FACT_TECH, T, V, PROCES, LOADb),
      tPLH_245ba  => CalcDelay(tPLH_245, FACT_TECH, T, V, PROCES, LOADa),
      tPHL_245ba  => CalcDelay(tPHL_245, FACT_TECH, T, V, PROCES, LOADa),
      tPZH_245ba  => CalcDelay(tPZH_245, FACT_TECH, T, V, PROCES, LOADa),
      tPZL_245ba  => CalcDelay(tPZL_245, FACT_TECH, T, V, PROCES, LOADa),
      tPHZ_245ba  => CalcDelay(tPHZ_245, FACT_TECH, T, V, PROCES, LOADa),
      tPLZ_245ba  => CalcDelay(tPLZ_245, FACT_TECH, T, V, PROCES, LOADa)
    )
  
    port map(
      OE_N , -- in std_logic;                       -- Output Enable
      T_R_N, -- in std_logic;                       -- Transmit Receive Input
      A    , -- inout std_logic_vector(7 downto 0); -- Side A TriState Inputs or
                                                    -- TriState Outputs
      B      -- inout std_logic_vector(7 downto 0)  -- Side B TriState Inputs or
                                                    -- TriState Outputs
    );
    
end WithTiming;

------------------------------------------------------------------------------
-- 54AC377
------------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
library MMS;
use MMS.StdSim.all;
use MMS.StdTiming.all;
use MMS.StdIoImp.all;
library FACTLIB;
use FACTLIB.Fact_Comp_Pck.all;
use FACTLIB.FACTTimPar.all;



entity AC377 is
  generic(
          T : temperature := T_BOARD;
          V : voltage := V_BOARD;
          PROCES : proces_type := PROCES_BOARD;
          LOAD : capacitance := LOAD_BOARD
  );
  
  
  port(
    CP    : in std_logic;                      -- Clock Pulse Input
    CE_N  : in std_logic;                      -- Clock Enable
    D     : in std_logic_vector(7 downto 0);   -- Data Inputs
    Q     : out std_logic_vector(7 downto 0)   -- Data Outputs
  );
	end AC377;

	
architecture WithTiming of AC377 is
  constant FACT_TECH : technology := (
     (0.9675, 0.0013, 0.0, 0.0),
     (1.4, -0.08, 0.0, 0.0),
     0.7, 1.3,
     40.0);
   
begin
  MyAC377: AC377Generic
    generic map(
      tPLH_377 => CalcDelay(tPLH_377, FACT_TECH, T, V, PROCES, LOAD),
      tPHL_377 => CalcDelay(tPHL_377, FACT_TECH, T, V, PROCES, LOAD),
      tSD_377  => CalcDelay(tSD_377,  FACT_TECH, T, V, PROCES),
      tHD_377  => CalcDelay(tHD_377,  FACT_TECH, T, V, PROCES),
      tSC_377  => CalcDelay(tSC_377,  FACT_TECH, T, V, PROCES),
      tHC_377  => CalcDelay(tHC_377,  FACT_TECH, T, V, PROCES)
    )
  
  port map(
    CP    , --: in std_logic;                      -- Clock Pulse Input
    CE_N  , --: in std_logic;                      -- Clock Enable
    D     , --: in std_logic_vector(7 downto 0);   -- Data Inputs
    Q       --: out std_logic_vector(7 downto 0)   -- Data Outputs
  );
    
end WithTiming;

