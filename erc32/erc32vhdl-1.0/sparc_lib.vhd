-------------------------------------------------------------------------------
-- File name : sparc_pck.vhd
-- Title : SparcPck
-- project : SPARC 
-- Library : SPARC_LIB
-- Author(s) : Maxime ROCCA, Jiri Gaisler
-- Purpose : package containing definitions for SPARC environment
-- notes : 
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        |  MR    | 94-03-04    | first version
--.............................................................................
-- v 1.1        |  MR    | 94-05-03    | 2nd version
-- + OddParityOf function has been modified (output inverted).
-- + procedure IOPscheduling: erroneous specification in issue 4 of the IURT
--   device specification about hardware interlocks for JMPL, CALL and LOAD 
--   double.
-- + modif. hardware trap priority.
--.............................................................................
-- v 1.2        |  MR    | 94-05-27    | 3rd version
-- + define specific timing checker for D & DPAR signals
--.............................................................................
-- v 1.3        |  RC    | 95-12-11    | 4rd version
-- + define RegFile and TrapVector as an inout 
--.............................................................................
-- v 1.4        |  JG    | 96-03-06    | 5rd version
-- + bug fix: correct stored PC and nPC after tapped LD and LDD
-------------------------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
-- Copyright ESA/ESTEC

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
use MMS.StdRtl.all;
use MMS.StdSim.all;
use MMS.StdIoImp.all;


package SparcPck is


  -- Size of data bus transfer.
  constant BYTE       : natural := 0;
  constant HALFWORD   : natural := 1;
  constant WORDTYPE   : natural := 2;
  constant DOUBLEWORD : natural := 3;

  -- Address Space Identifiers.
  constant USER_INST       : natural :=  8;
  constant USER_DATA       : natural := 10;
  constant SUPERVISOR_INST : natural :=  9;
  constant SUPERVISOR_DATA : natural := 11;

  -- Number of implemented windows
  constant NWINDOWS : natural := 8;
  
  -- Bit numbers for icc  
  constant N_ICC : natural := 3;
  constant Z_ICC : natural := 2;
  constant V_ICC : natural := 1;
  constant C_ICC : natural := 0;
  
  -- Values for the impl & ver fields of the PSR
  constant PSR_IMPL : std_logic_vector(3 downto 0) := "0001";
  constant PSR_VER  : std_logic_vector(3 downto 0) := "0001";
  constant TBR3_DOWNTO_0  : std_logic_vector(3 downto 0) := "0000";
            
  -- Processor states or modes.
  type ModeType is (RESET_MODE, ERROR_MODE, EXECUTE_MODE);

  -- Mnemonics for instructions.
  type SuperInstMnemonic is (  -- including pseudo-instructions
     -- pseudo-instruction mnemonics for modelling purposes
     XXX, ILLEGAL, IOP, NOTHING, ANNULLED,
     
     -- SPARC instruction set
        --- Load/Store Instruction Mnemonics ---
     LD      , LDA     , LDC      , LDCSR   , LDD     ,
     LDDA    , LDDC    , LDDF     , LDF     , LDFSR   ,
     LDSB    , LDSBA   , LDSH     , LDSHA   , LDSTUB  ,
     LDSTUBA , LDUB    , LDUBA    , LDUH    , LDUHA   ,
     ST      , STA     , STB      , STBA    , STC     ,
     STCSR   , I_STD     , STDA     , STDC    , STDCQ   ,
     STDF    , STDFQ   , STF      , STFSR   , STH     ,
     STHA    , SWAP    , SWAPA    ,
        --- Arithmetic/Logical/Shift Instruction Mnemonics ---
     ADD     , ADDcc   , ADDX     , ADDXcc  , I_AND   ,
     ANDcc   , ANDN    , ANDNcc   , MULScc  , I_OR    ,
     ORcc    , ORN     , ORNcc    , SLL     , SRA     ,
     SRL     , SUB     , SUBcc    , SUBX    , SUBXcc  ,
     TADDcc  , TADDccTV, TSUBcc   , TSUBccTV, XNOR    ,
     XNORcc  , I_XOR   , XORcc    ,
        --- Control Transfer Instruction Mnemonics ---
     JMPL    , RESTORE , RETT     , SAVE    , CALL    ,
     BN      , BE      , BLE      , BL      , BLEU    ,
     BCS     , BNEG    , BVS      , BA      , BNE     ,
     BG      , BGE     , BGU      , BCC     , BPOS    ,
     BVC     , CBN     , CB123    , CB12    , CB13    ,
     CB1     , CB23    , CB2      , CB3     , CBA     ,
     CB0     , CB03    , CB02     , CB023   , CB01    ,
     CB013   , CB012   , FBN      , FBNE    , FBLG    ,
     FBUL    , FBL     , FBUG     , FBG     , FBU     ,
     FBA     , FBE     , FBUE     , FBGE    , FBUGE   ,
     FBLE    , FBULE   , FBO      , TN      , TE      ,
     TLE     , TL      , TLEU     , TCS     , TNEG    ,
     TVS     , TA      , TNE      , TG      , TGE     ,
     TGU     , TCC     , TPOS     , TVC     ,
        --- Read/Write Control Register Instruction Mnemonics ---
     RDPSR   , RDTBR   , RDWIM    , RDY     , WRPSR   ,
     WRTBR   , WRWIM   , WRY      ,
        --- Miscellaneous Instruction Mnemonics ---
     SETHI   , UNIMP   , FLUSH    , -- IFLUSH instead of FLUSH in SPARC v.7
     RDASR   , WRASR   , STBAR    , -- SPARC v.8 only
        --- Integer Multiply/Divide Instructions Mnemonics ---
                   --- SPARC Version 8 only ---
     UMUL    , UMULcc  , SMUL     , SMULcc   , UDIV   ,
     UDIVcc  , SDIV    , SDIVcc   ,
        --- Floating-Point Instruction Mnemonics ---
     FiTOs   , FiTOd   , FiTOq    , FsTOi    , FdTOi  ,
     FqTOi   , FsTOd   , FsTOq    , FdTOs    , FdTOq  ,
     FqTOs   , FqTOd   , FMOVs    , FNEGs    , FABSs  ,
     FSQRTs  , FSQRTd  , FSQRTq   , FADDs    , FADDd  ,
     FADDq   , FSUBs   , FSUBd    , FSUBq    , FMULs  ,
     FMULd   , FMULq   ,
     FsMULd  , FdMULq  , -- SPARC v.8 only for FsMULd & FdMULq
     FDIVs   , FDIVd   , FDIVq    , FCMPs    , FCMPd  ,
     FCMPq   , FCMPEs  , FCMPEd   , FCMPEq   ,
        --- Coprocessor Instruction Mnemonics ---
      CPop1  , CPop2    
  );


  -- Pseudo-functions to tell if an instruction mnemonic is an OPcc (IsOPcc)
  -- or a Bicc or a FBfcc or CBccc.
  -- Constants have deferred values.
  type IsOPccType is array(SuperInstMnemonic) of boolean;
  constant IsOPcc : IsOPccType;
  type IsBranchingInstType is array(SuperInstMnemonic) of boolean;
  constant IsBicc  : IsBranchingInstType;
  constant IsFBfcc : IsBranchingInstType;
  constant IsCBccc : IsBranchingInstType;
  
  -- Other pseudo-functions:
  -- IURs1Rs2AreIn(Mnemonic) returns TRUE if Mnemonic has a source register
  -- rs1 or rs2.
  -- FPURsIsIn(Mnemonic) returns TRUE if Mnemonic has a source register
  -- rs1.
  -- IsFPinst(Mnemonic) returns TRUE if Mnemonic is a FP instruction.
  -- IsFPop(Mnemonic) returns TRUE if Mnemonic is a FP operation.
  -- IsFCMP(Mnemonic) returns TRUE if Mnemonic is a FP compare.
  type MnemoTableType is array(SuperInstMnemonic) of boolean;
  constant IURs1Rs2AreIn : MnemoTableType;
  constant FPURs1IsIn    : MnemoTableType;
  constant IsFPinst      : MnemoTableType;
  constant IsFPop        : MnemoTableType;
  constant IsFPopDouble  : MnemoTableType;
  constant IsFPopQuad    : MnemoTableType;
  constant IsFCMP        : MnemoTableType;
  
  -- Other pseudo-functions for LOAD AND STORE instructions
  -- IsLoadDoubleInst(Mnemonic) returns TRUE if Mnemonic is a LOAD instruction
  -- for double words.
  -- IsLoadInst(Mnemonic) returns TRUE if Mnemonic is a LOAD instruction.
  -- IsLoadSingleInst(Mnemonic) returns TRUE if Mnemonic is a LOAD instruction
  -- for "single" words.
  -- IsLoadFP_CPInst(Mnemonic) returns TRUE if Mnemonic is a Floating-Point
  -- or Coprocessor LOAD.
  -- IsStoreInst(Mnemonic) returns TRUE if Mnemonic is a store instruction.
  -- and so on...
  constant IsLoadDoubleInst   : MnemoTableType;
  constant IsLoadInst         : MnemoTableType;
  constant IsLoadSingleInst   : MnemoTableType;
  constant IsLoadFP_CPInst    : MnemoTableType;
  constant IsLoadByteInst     : MnemoTableType;
  constant IsLoadHalfwordInst : MnemoTableType;
  constant IsLoadInstASI      : MnemoTableType;
  constant IsStoreInst        : MnemoTableType;
  constant IsStoreDoubleInst  : MnemoTableType;
  constant IsStoreFP_CPInst   : MnemoTableType;
  constant IsStoreInstASI     : MnemoTableType;
  constant IsStoreSingleInst  : MnemoTableType;
  
  type Instruction is record
     Mnemo       : SuperInstMnemonic; -- mnemonic of an instruction
     asi         : natural; -- address space identifier
     i           : natural; -- i bit
     a           : natural; -- annul bit for branch instructions
     rd          : natural; -- destination register
     rs1         : natural; -- source register 1
     rs2         : natural; -- source register 2
     simm13      : natural; -- 13-bit immediate value
     disp30      : std_logic_vector(29 downto 0); -- displacement for CALL
     disp22      : natural; -- 22-bit displacement for taken branches
                            -- or 22-bit constant for SETHI & UNIMP inst.
     
     Address     : std_logic_vector(31 downto 0); -- instruction address
     NextAddress : std_logic_vector(31 downto 0); -- addr. of the following
                                                  -- instruction
     Annul       : boolean; -- for branching instruction: TRUE if annulled
     BitInstr    : std_logic_vector(31 downto 0); -- 32-bit value of instr.
  end record; -- Instruction
  
  -- RegisterFile: unbounded array of 32-bit words.
  type RegisterFile is array(natural range <>) of std_logic_vector(31 downto 0);
  
  -- Declarations related to trap handling.
  type TrapMnemonic is (
     DETECTED_TRAP,   -- Used to set flag when a trap is detected (except for
                      -- reset traps).
     RESET_TRAP,      -- reset trap mnemo.
     INST_ACCESS,     -- instruction access exception trap
     ILLEGAL_INST,    -- illegal instruction trap
     PRIVILEGED_INST, -- privileged instruction trap
     FP_DISABLED,     -- Floating-Point disabled trap
     CP_DISABLED,     -- Coprocessor disabled trap
     WINDOW_OVERFLOW, -- window overflow trap
     WINDOW_UNDERFLOW, -- window underflow trap
     MEM_ADDR_NOT_ALIGNED, -- memory-address-not-aligned trap
     FP_EXCEPTION,    -- Floating-Point exception trap
     CP_EXCEPTION,    -- Coprocessor exception trap
     DATA_ACCESS_EXCEPTION, -- data access exception trap
     TAG_OVERFLOW,    -- tag overflow trap for TADDccTV & TSUBccTV
     TRAP_INST,       -- trap instruction Ticc
     INTERRUPT_LEVEL, -- interrupting trap with IRL(3:0)
     PROGRAM_FLOW_ERR, -- Program Flow Error (hardware trap).
     NON_RESTART_IMPRECISE, -- Non-restartable, imprecise error.
     RESTART_IMPRECISE -- Restartable, imprecise error
  );
  
  type TrapVectorType is array(TrapMnemonic) of boolean;
  type TrapType is array(TrapMnemonic) of std_logic_vector(7 downto 0);
  constant TrapTypeTable : TrapType := 
  (
    DETECTED_TRAP         => "00000000", -- no trap type here! Dummy constant!
    RESET_TRAP            => "00000000", -- same thing here (Dummy constant).
    INST_ACCESS           => "00000001", 
    ILLEGAL_INST          => "00000010",
    PRIVILEGED_INST       => "00000011",
    FP_DISABLED           => "00000100",
    CP_DISABLED           => "00100100",
    WINDOW_OVERFLOW       => "00000101",
    WINDOW_UNDERFLOW      => "00000110",
    MEM_ADDR_NOT_ALIGNED  => "00000111",
    FP_EXCEPTION          => "00001000",
    CP_EXCEPTION          => "00101000",
    DATA_ACCESS_EXCEPTION => "00001001",
    TAG_OVERFLOW          => "00001010",
    TRAP_INST             => "00000000", -- Dummy constant: trap type is 
                                         -- computed during execution of Ticc.
    INTERRUPT_LEVEL       => "00000000", -- Dummy constant: trap type is
                                         -- determined by concatenating 0001
                                         -- with IRL value: 0001 & IRL(3:0).
    PROGRAM_FLOW_ERR      => "01100110",
    NON_RESTART_IMPRECISE => "01100100",
    RESTART_IMPRECISE     => "01100011"
  );
  
  type TrapModeType is (
     NOTRAP,       -- no trap condition
     SYNCH_TRAP,   -- SYNCHronous trap
     ASYNCH_TRAP   -- ASYNCHronous trap
  );
  

--============== FUNCTIONS DECLARATIONS  ======================================

  
  -----------------------------------------------------------------------------
  -- GetIndex emulates the windowing scheme of the windowed register file by
  -- returning the right index for this register file.
  -- n: the input index (0<n<31)
  -- CWP: Current Window Pointer
  -----------------------------------------------------------------------------
  function GetIndex(n : natural; CWP : std_logic_vector) return natural;
  
  -----------------------------------------------------------------------------
  -- This function "decodes" a 32-bit vector considered as being a SPARC
  -- instruction by returning a record "Instruction" containing all the  
  -- meaningful information needed to execute this instruction.
  -- A must be a 32-bit vector. The function will crash if A'length is less
  -- than 32, and assert an error if greater than 32. Moreover, A should be
  -- defined as A(31:0), i.e MSB with highest index.
  -- Note: SPARC v.8 instructions RDASR, WRASR & STBAR are not decoded by this
  -- function.
  -----------------------------------------------------------------------------
  function Transcribe(A : std_logic_vector) return Instruction;
  
  -----------------------------------------------------------------------------
  -- Evaluates the icc and returns TRUE if the branch should be taken.
  -----------------------------------------------------------------------------
  function iccEvaluation(Mnemonic : SuperInstMnemonic;
                         icc      : std_logic_vector) return boolean;
                         
  -----------------------------------------------------------------------------
  -- Same as iccEvaluation for Ticc instructions.
  -----------------------------------------------------------------------------
  function TiccEval(Mnemonic : SuperInstMnemonic;
                    icc      : std_logic_vector) return boolean;
                         
  -----------------------------------------------------------------------------
  -- Same as above for FPU with fcc.
  -----------------------------------------------------------------------------
  function fccEvaluation(Mnemonic   : SuperInstMnemonic;
                         signal fcc : std_logic_vector) return boolean;

  -----------------------------------------------------------------------------
  -- Returns TRUE if there is a register dependency between rd and InstB 
  -- (i.e one of the source register of InstB is the destination register rd).
  -- Valid for register of the IU (not the FP registers).
  -----------------------------------------------------------------------------
  function IURegDependency(rd    : natural;
                           InstB : Instruction) return boolean;
                           
  -----------------------------------------------------------------------------
  -- Returns TRUE if there is a register dependency between rd and InstB 
  -- (i.e one of the source register of InstB is the destination register rd).
  -- Valid for registers of the FPU (FP registers).
  -- WARNING: THIS FUNCTION IS OBSOLETE and kept here for information.
  -----------------------------------------------------------------------------
--  function FPURegDependency(rd    : natural;
--                            Mnemo : SuperInstMnemonic;
--                            rs1   : natural;
--                            rs2   : natural) return boolean;

  -----------------------------------------------------------------------------
  -- Computes the value of the address for a load, store or load-store 
  -- instruction.
  -----------------------------------------------------------------------------
  function LoadORStoreORSwapAddrCalc(Inst    : Instruction;
                                     CWP     : std_logic_vector;
                                     RegFile : RegisterFile) 
                                        return std_logic_vector;

  -----------------------------------------------------------------------------
  -- Computes the odd parity over a given std_logic_vector
  -----------------------------------------------------------------------------
  function OddParityOf(Vec : std_logic_vector) return std_logic;

--============== PROCEDURES DECLARATIONS ======================================

  -----------------------------------------------------------------------------
  -- This procedure detects traps (for asynchronous traps) and does the opera-
  -- -tions to be performed when a trap case is present.
  -- All std_logic_vector parameters must be 32 bits long.
  -- Interrupting traps (asynchronous traps) are also taken care of.
  -----------------------------------------------------------------------------
  procedure TrapHandler(signal EX   : Instruction;
                        signal WR   : Instruction;
                               WR1  : Instruction;
                               WR2  : Instruction;
                        pIRLvar     : natural;
                        IRLvar      : natural;
                        TBR         : inout std_logic_vector;
                        PSR         : inout std_logic_vector;
                        TrapVector  : inout TrapVectorType;
                        Mode        : inout ModeType;
                        TrapMode    : out TrapModeType;
                        pPrevAddr   : inout std_logic_vector;
                        PrevAddr    : inout std_logic_vector;
                        CurrentAddr : inout std_logic_vector;
 --                       RegFile     : out RegisterFile); --gd 2208
 -- we dont want to erase the old value
                        RegFile     : inout RegisterFile);
  
  -----------------------------------------------------------------------------
  -- This procedure does the dispatching for instruction execution. Trap flags 
  -- are set when traps are detected for each "family" of instructions.
  -- ResultOpcc, iccTemp, YTemp are parameters for "anticipated" execution of
  -- certain instructions (OPcc instructions).
  -- ResultOpcc, Y, YTemp, PSR, TBR, WIM lengths must be 32 bits long.
  -- iccTemp length must be 4 bits long.
  -----------------------------------------------------------------------------
  procedure ExecutionBody(signal FP_N : std_logic;
                          signal IFT_N: std_logic;
                          signal EX   : Instruction;
                          ResultOpcc  : std_logic_vector;
                          YTemp       : std_logic_vector;
                          iccTemp     : inout std_logic_vector;
                          Y           : inout std_logic_vector;
                          PSR         : inout std_logic_vector;
                          TBR         : inout std_logic_vector;
                          WIM         : inout std_logic_vector;
                          RegFile     : inout RegisterFile;
                          Mode        : inout ModeType;
                          TrapVector  : inout TrapVectorType);
  
  -----------------------------------------------------------------------------
  -- Execution of LOAD instructions. The procedure only detects trap conditions.
  -- The address of the data to load is computed earlier so that the data can
  -- be fetched in time on the bus.
  -----------------------------------------------------------------------------
  procedure LoadInstruction(signal FP_N : std_logic;
                            EX          : Instruction;
                            PSR         : std_logic_vector;
                            RegFile     : RegisterFile;
                            TrapVector  : inout TrapVectorType);
                            
  -----------------------------------------------------------------------------
  -- Execute the store instructions. Detects trap conditions and set the trap
  -- vector accordingly.
  -----------------------------------------------------------------------------
  procedure StoreInstruction(signal FP_N : std_logic;
                             EX          : Instruction;
                             PSR         : std_logic_vector;
                             RegFile     : RegisterFile;
                             TrapVector  : inout TrapVectorType);
                             
  -----------------------------------------------------------------------------
  -- This procedure performs all the logical operations. OPcc being executed by
  -- "anticipation", their results are used (with the parameters ResultOPcc and
  -- iccTemp) to assign the register file and icc of the PSR.
  -----------------------------------------------------------------------------
  procedure LogicalInstruction(EX         : Instruction;
                               CWP        : std_logic_vector;
                               ResultOPcc : std_logic_vector;
                               iccTemp    : std_logic_vector;
                               RegFile    : inout RegisterFile;
                               icc        : out std_logic_vector);
                               
  -----------------------------------------------------------------------------
  -- Additions and substractions are performed here. Same as above for OPcc and
  -- parameters.
  -----------------------------------------------------------------------------
  procedure AddSubInstruction(EX         : Instruction;
                              CWP        : std_logic_vector;
                              ResultOPcc : std_logic_vector;
                              iccTemp    : std_logic_vector;
                              RegFile    : inout RegisterFile;
                              icc        : inout std_logic_vector);
  
                            
  -----------------------------------------------------------------------------
  -- Logical and arithmetic shifts. Integer conditions codes are not affected 
  -- by these instructions.
  -----------------------------------------------------------------------------
  procedure ShiftInstruction(EX         : Instruction;
                             CWP        : std_logic_vector;
                             RegFile    : inout RegisterFile);
                             
  -----------------------------------------------------------------------------
  -- Same as AddSubInstruction but with tags. There can be trap conditions.
  -----------------------------------------------------------------------------
  procedure TaggedAddSubbInst(EX         : Instruction;
                              CWP        : std_logic_vector;
                              ResultOPcc : std_logic_vector;
                              iccTemp    : std_logic_vector;
                              icc        : out std_logic_vector;
                              RegFile    : inout RegisterFile;
--                              RegFile    : out RegisterFile; --gd 2208
                              TrapVector : inout TrapVectorType);
                              
  -----------------------------------------------------------------------------
  -- This is an OPcc instruction (affecting the Integer Condition Codes); so it
  -- is executed by anticipation in ExecuteOPcc. Y, icc and RegFile are assigned
  -- with their right value YTemp, iccTemp, ResultOPcc computed in ExecuteOPcc.
  -----------------------------------------------------------------------------
  procedure MultiplyStepInst(EX         : Instruction;
                             CWP        : std_logic_vector;
                             ResultOPcc : std_logic_vector;
                             iccTemp    : std_logic_vector;
                             YTemp      : std_logic_vector;
                             Y          : out std_logic_vector;
                             icc        : out std_logic_vector;
                             RegFile    : inout RegisterFile); --gd 2208
--                             RegFile    : out RegisterFile);
  
  -----------------------------------------------------------------------------
  -- Execution of SAVE/RESTORE instructions. 
  -- Note that the destination register of these instructions is in the NEW
  -- window whereas the source registers come from the OLD window.
  -----------------------------------------------------------------------------
  procedure SaveRestoreInst(EX         : Instruction;
                            WIM        : std_logic_vector;
                            CWP        : inout std_logic_vector;
                            RegFile    : inout RegisterFile;
                            TrapVector : inout TrapVectorType);
                            
  -----------------------------------------------------------------------------
  -- Execution of the instr. RETT, leaving aside the branching address calcula-
  -- -tion which is performed by "anticipation" in ID stage.
  -----------------------------------------------------------------------------
  procedure RettInstruction(EX         : Instruction;
                            RegFile    : RegisterFile;
                            WIM        : std_logic_vector;
                            PSR        : inout std_logic_vector;
                            Mode       : inout ModeType;
                            tt         : out std_logic_vector;
                            TrapVector : inout TrapVectorType);
                            
  -----------------------------------------------------------------------------
  -- Execution of the instr. JMPL, leaving aside the branching address calcula-
  -- -tion which is performed by "anticipation" in ID stage.
  -----------------------------------------------------------------------------
  procedure JmplInstruction(EX         : Instruction;
                            CWP        : std_logic_vector;
                            RegFile    : inout RegisterFile;
                            TrapVector : inout TrapVectorType);
                            
  -----------------------------------------------------------------------------
  -- Branching address calculation for JMPL/RETT instructions.
  -- Done by "anticipation" in ID stage.
  -----------------------------------------------------------------------------
  procedure JmplRettAddrCalc(ID          : Instruction;
                             CWP         : std_logic_vector;
                             RegFile     : RegisterFile;
                             CurrentAddr : out std_logic_vector);
                            
  -----------------------------------------------------------------------------
  -- Instructions reading the state registers are executed in this procedure.
  -- If some trap conditions are encountered, the trap vector is assigned with 
  -- the correct values to tell the trap handler what to do.
  -----------------------------------------------------------------------------
  procedure ReadStateRegInst(EX         : Instruction;
                             PSR        : std_logic_vector;
                             TBR        : std_logic_vector;
                             WIM        : std_logic_vector;
                             Y          : std_logic_vector;
                             TrapVector : inout TrapVectorType;
                             RegFile    : inout RegisterFile);
 --                            RegFile    : out RegisterFile);
                             
  -----------------------------------------------------------------------------
  -- Instructions writing the state registers are executed in this procedure.
  -- Same as above for trap detection.
  -----------------------------------------------------------------------------
  procedure WriteStateRegInst(EX         : Instruction;
                              RegFile    : RegisterFile;
                              PSR        : inout std_logic_vector; 
                              iccTemp    : out std_logic_vector;
                              TBR        : inout std_logic_vector; 
--                              TBR        : out std_logic_vector;--gd 
                              WIM        : inout std_logic_vector; 
--                              WIM        : out std_logic_vector;--gd 
                              Y          : inout std_logic_vector; 
--                              Y          : out std_logic_vector;--gd 
                              TrapVector : inout TrapVectorType);
                              
  -----------------------------------------------------------------------------
  -- This procedure detects trap conditions for LDSTUBA and SWAPA instructions.
  -- Address calculation is not performed for sequencing reasons.
  -----------------------------------------------------------------------------
  procedure LoadStoreSwapInstruction(EX         : Instruction;
                                     S          : std_logic;
                                     CWP        : std_logic_vector;
                                     RegFile    : RegisterFile;
                                     TrapVector : inout TrapVectorType);
                                 
  -----------------------------------------------------------------------------
  -- Execution of trap instructions. Computation of the trap number when
  -- the trap is taken and assignment of the tt field of the TBR with the com-
  -- -puted value.
  -----------------------------------------------------------------------------
  procedure TiccInstruction(EX         : Instruction;
                            CWP        : std_logic_vector;
                            icc        : std_logic_vector;
                            RegFile    : RegisterFile;
                            tt         : out std_logic_vector;
                            TrapVector : inout TrapVectorType);
                            
  -----------------------------------------------------------------------------
  -- This procedure executes OPcc (operations affecting the Integer Condition
  -- Codes) "in advance" ("anticipated" execution).
  -- Results are stored in temporary data objects ResultOPcc, YTemp and 
  -- iccTemp.
  -- These temp. data objects are used to assign the register file and icc of
  -- the PSR (ex.: for OPcc = ADDcc, execution is performed in the procedure
  -- ExecuteOPcc. The register file and icc are assigned in the procedure
  -- AddSubInstruction).
  -----------------------------------------------------------------------------
  procedure ExecuteOPcc(ID         : Instruction;
                        RegFile    : RegisterFile;
                        CWP        : std_logic_vector;
                        icc        : std_logic_vector;
                        Y          : std_logic_vector;
                        ResultOPcc : out std_logic_vector;
                        iccTemp    : out std_logic_vector;
                        YTemp      : out std_logic_vector);
                        
  -----------------------------------------------------------------------------
  -- "Anticipated" execution of Bicc, to calculate the branching address in 
  -- time. No action is taken in the execution body procedure for these instr.
  ----------------------------------------------------------------------------
  procedure ExecuteBicc(iccTemp     : std_logic_vector;
                        icc         : std_logic_vector;
                        nID         : inout Instruction;
                        CurrentAddr : inout std_logic_vector;
                        TakenBr     : out boolean);
                        
  -----------------------------------------------------------------------------
  -- Same as above for Floating-point branch.
  -----------------------------------------------------------------------------
  procedure ExecuteFBfcc(signal fcc  : std_logic_vector;
                         nID         : inout Instruction;
                         CurrentAddr : inout std_logic_vector;
                         TakenBr     : out boolean);
                        
  -----------------------------------------------------------------------------
  -- Puts nID in the instruction buffer (FIFO)
  -----------------------------------------------------------------------------
  procedure PutInBufferQueue(nID         : Instruction;
                             Buf1IsValid : inout boolean;
                             InstBuffer1 : inout Instruction;
                             Buf2IsValid : inout boolean;
                             InstBuffer2 : inout Instruction);
                             
  -----------------------------------------------------------------------------
  -- Sets flag IOPcase to TRUE if an IOP is to be scheduled.
  -----------------------------------------------------------------------------
  procedure IOPscheduling(InstBuffer1 : Instruction;
                          nID         : Instruction;
                          signal ID   : Instruction;
                          signal EX   : Instruction;
                          signal WR   : Instruction;
                          IOPcase     : out boolean);

  -----------------------------------------------------------------------------
  -- Gets an instruction from the instruction buffer (FIFO) and puts it 
  -- in nIDTemp.
  -----------------------------------------------------------------------------
  procedure GetFromBufferQueue(Buf1IsValid : inout boolean;
                               InstBuffer1 : inout Instruction;
                               Buf2IsValid : inout boolean;
                               InstBuffer2 : inout Instruction;
                               nIDTemp     : out Instruction);
                              
  -----------------------------------------------------------------------------
  -- Fetch data on data bus when WR is load instruction and the register rd of
  -- instruction WR is not 0.
  -----------------------------------------------------------------------------
  procedure DataFetchForLoadAndLdstInst(
                          CurrentAddr     : std_logic_vector;
                          signal D        : std_logic_vector;
                          CWP             : std_logic_vector;
                          signal WR       : Instruction;
                          WR1             : Instruction;
                          RegFile         : inout RegisterFile;
                          SwapData        : out std_logic_vector);

  -----------------------------------------------------------------------------
  -- This procedure fetches the data that appears on the bus when a cache miss
  -- case is encountered and serviced. It is essentially used for load and 
  -- load-store instructions.
  -----------------------------------------------------------------------------
  procedure DataFetchWhenCacheMiss(Addr     : std_logic_vector;
                                   signal D : std_logic_vector;
                                   CWP      : std_logic_vector;
                                   WR1      : Instruction;
                                   WR2      : Instruction;
                                   RegFile  : inout RegisterFile);

  -----------------------------------------------------------------------------
  -- Procedure to perform setup and hold time violation checking for the 
  -- data bus and its parity bit.
  -----------------------------------------------------------------------------
  procedure DbusSetupHoldCheck (signal Data    : std_logic_vector;  
                                signal CLK     : std_ulogic;
                                signal XHOLD_N : std_ulogic;
                                constant SETUP, HOLD : time := 0 ns;
                                constant PATH  : string := "";
                                signal DelayedData : std_logic_vector;
                                signal EN_CHECKING : boolean);

  procedure DbusSetupHoldCheck (signal Data    : std_ulogic;  
                                signal CLK     : std_ulogic;
                                signal XHOLD_N : std_ulogic;
                                constant SETUP, HOLD : time := 0 ns;
                                constant PATH  : string := "";
                                signal DelayedData : std_ulogic;
                                signal EN_CHECKING : boolean);

end SparcPck; -- package


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

package body SparcPck is
  
  -------------------------------------
  constant IsOPcc : IsOPccType :=(
       ADDcc    => TRUE,
       ANDcc    => TRUE,
       ORcc     => TRUE,
       XORcc    => TRUE,
       SUBcc    => TRUE,
       ANDNcc   => TRUE,
       ORNcc    => TRUE,
       XNORcc   => TRUE,
       ADDXcc   => TRUE,
       UMULcc   => TRUE, -- SPARC v.8
       SMULcc   => TRUE, -- SPARC v.8
       SUBXcc   => TRUE,
       UDIVcc   => TRUE, -- SPARC v.8
       SDIVcc   => TRUE, -- SPARC v.8
       TADDcc   => TRUE,
       TSUBcc   => TRUE,
       TADDccTV => TRUE,
       TSUBccTV => TRUE,
       MULScc   => TRUE,
       others => FALSE
  );
  
  -------------------------------------
  constant IsBicc : IsBranchingInstType :=(
       BN   => TRUE,
       BE   => TRUE,
       BLE  => TRUE,
       BL   => TRUE,
       BLEU => TRUE,
       BCS  => TRUE,
       BNEG => TRUE,
       BVS  => TRUE,
       BA   => TRUE,
       BNE  => TRUE,
       BG   => TRUE,
       BGE  => TRUE,
       BGU  => TRUE,
       BCC  => TRUE,
       BPOS => TRUE,
       BVC  => TRUE,
       others => FALSE
  );
  
  -------------------------------------
  constant IsFBfcc : IsBranchingInstType :=(
       FBN   => TRUE,
       FBNE  => TRUE,
       FBLG  => TRUE,
       FBUL  => TRUE,
       FBL   => TRUE,
       FBUG  => TRUE,
       FBG   => TRUE,
       FBU   => TRUE,
       FBA   => TRUE,
       FBE   => TRUE,
       FBUE  => TRUE,
       FBGE  => TRUE,
       FBUGE => TRUE,
       FBLE  => TRUE,
       FBULE => TRUE,
       FBO   => TRUE,
       others => FALSE
  );
  -------------------------------------
  constant IsCBccc : IsBranchingInstType :=(
       CBN   => TRUE,
       CB123 => TRUE,
       CB12  => TRUE,
       CB13  => TRUE,
       CB1   => TRUE,
       CB23  => TRUE,
       CB2   => TRUE,
       CB3   => TRUE,
       CBA   => TRUE,
       CB0   => TRUE,
       CB03  => TRUE,
       CB02  => TRUE,
       CB023 => TRUE,
       CB01  => TRUE,
       CB013 => TRUE,
       CB012 => TRUE,
       others => FALSE
  );
  
  -------------------------------------
  constant IURs1Rs2AreIn : MnemoTableType := (
     LD      => TRUE, LDA   => TRUE, LDC   => TRUE, LDCSR => TRUE,
     LDD     => TRUE,
     LDDA    => TRUE, LDDC  => TRUE, LDDF  => TRUE, LDF   => TRUE,
     LDFSR   => TRUE,
     LDSB    => TRUE, LDSBA => TRUE, LDSH  => TRUE, LDSHA => TRUE,
     LDSTUB  => TRUE,
     LDSTUBA => TRUE, LDUB  => TRUE, LDUBA => TRUE, LDUH  => TRUE,
     LDUHA   => TRUE,
     ST      => TRUE, STA   => TRUE, STB   => TRUE, STBA  => TRUE,
     STC     => TRUE,
     STCSR   => TRUE, I_STD   => TRUE, STDA  => TRUE, STDC  => TRUE,
     STDCQ   => TRUE,
     STDF    => TRUE, STDFQ => TRUE, STF   => TRUE, STFSR => TRUE,
     STH     => TRUE,
     STHA    => TRUE, SWAP  => TRUE, SWAPA => TRUE,
        --- Arithmetic/Logical/Shift Instruction Mnemonics ---
     ADD    => TRUE, ADDcc    => TRUE, ADDX   => TRUE, ADDXcc   => TRUE,
     I_AND  => TRUE,
     ANDcc  => TRUE, ANDN     => TRUE, ANDNcc => TRUE, MULScc   => TRUE,
     I_OR   => TRUE,
     ORcc   => TRUE, ORN      => TRUE, ORNcc  => TRUE, SLL      => TRUE,
     SRA    => TRUE,
     SRL    => TRUE, SUB      => TRUE, SUBcc  => TRUE, SUBX     => TRUE,
     SUBXcc => TRUE,
     TADDcc => TRUE, TADDccTV => TRUE, TSUBcc => TRUE, TSUBccTV => TRUE,
     XNOR   => TRUE,
     XNORcc => TRUE, I_XOR    => TRUE, XORcc  => TRUE,
        --- Control Transfer Instruction Mnemonics ---
     JMPL => TRUE, RESTORE => TRUE, RETT => TRUE, SAVE => TRUE,
     TN   => TRUE, TE      => TRUE,
     TLE  => TRUE, TL      => TRUE, TLEU => TRUE, TCS  => TRUE, TNEG => TRUE,
     TVS  => TRUE, TA      => TRUE, TNE  => TRUE, TG   => TRUE, TGE  => TRUE,
     TGU  => TRUE, TCC     => TRUE, TPOS => TRUE, TVC  => TRUE,
        --- Write Control Register Instruction Mnemonics ---
     WRPSR => TRUE, WRTBR => TRUE, WRWIM => TRUE, WRY => TRUE,
        --- Miscellaneous Instruction Mnemonics ---
     FLUSH => TRUE, -- IFLUSH instead of FLUSH in SPARC v.7
     RDASR => TRUE, WRASR => TRUE, -- SPARC v.8 only
        --- Integer Multiply/Divide Instructions Mnemonics ---
                   --- SPARC Version 8 only ---
     UMUL   => TRUE, UMULcc => TRUE, SMUL   => TRUE, SMULcc => TRUE,
     UDIV   => TRUE,
     UDIVcc => TRUE, SDIV   => TRUE, SDIVcc => TRUE,
       
     others => FALSE
  );

  -------------------------------------
  constant FPURs1IsIn : MnemoTableType := (
    FADDs  => TRUE, FADDd  => TRUE, FADDq  => TRUE, FSUBs  => TRUE,
    FSUBd  => TRUE, FSUBq  => TRUE, FMULs  => TRUE, FMULd  => TRUE, 
    FMULq  => TRUE,
    FsMULd => TRUE, FdMULq => TRUE, -- SPARC v.8 only for FsMULd & FdMULq
    FDIVs  => TRUE, FDIVd  => TRUE, FDIVq  => TRUE, FCMPs  => TRUE,
    FCMPd  => TRUE, FCMPq  => TRUE, FCMPEs => TRUE, FCMPEd => TRUE, 
    FCMPEq => TRUE,
    others => FALSE
  );

  -------------------------------------
  constant IsLoadInst : MnemoTableType := (
     LD    => TRUE, LDD   => TRUE, LDSB  => TRUE, LDUB  => TRUE,
     LDSH  => TRUE, LDUH  => TRUE, LDA   => TRUE, LDDA  => TRUE,
     LDSBA => TRUE, LDUBA => TRUE, LDSHA => TRUE, LDUHA => TRUE,
     LDF   => TRUE, LDDF  => TRUE, LDFSR => TRUE, LDC   => TRUE,
     LDDC  => TRUE, LDCSR => TRUE,
     others  => FALSE
  );
  
  -------------------------------------
  constant IsLoadDoubleInst : MnemoTableType := (
     LDD   => TRUE, LDDA  => TRUE,
     LDDF  => TRUE, LDDC  => TRUE, 
     others  => FALSE
  );
    
  -------------------------------------
  constant IsLoadSingleInst : MnemoTableType := (
     LD    => TRUE, LDSB  => TRUE, LDUB  => TRUE, LDSH  => TRUE,
     LDUH  => TRUE, LDA   => TRUE, LDSBA => TRUE, LDUBA => TRUE,
     LDSHA => TRUE, LDUHA => TRUE, LDF   => TRUE, LDFSR => TRUE, 
     LDC   => TRUE, LDCSR => TRUE,
     others  => FALSE
  );
  
  -------------------------------------
  constant IsLoadFP_CPInst : MnemoTableType := (
     LDF   => TRUE, LDDF  => TRUE, LDFSR => TRUE, LDC   => TRUE,
     LDDC  => TRUE, LDCSR => TRUE,
     others  => FALSE
  );
  
  -------------------------------------
  constant IsLoadByteInst : MnemoTableType := (
     LDSB  => TRUE, LDUB  => TRUE, LDSBA => TRUE, LDUBA => TRUE,
     others  => FALSE
  );
  
  -------------------------------------
  constant IsLoadHalfwordInst : MnemoTableType := (
     LDSH  => TRUE, LDUH  => TRUE, LDSHA => TRUE, LDUHA => TRUE,
     others  => FALSE
  );
  
  -------------------------------------
  constant IsLoadInstASI : MnemoTableType := (
     LDA   => TRUE, LDDA  => TRUE, LDSBA => TRUE, LDUBA => TRUE,
     LDSHA => TRUE, LDUHA => TRUE,
     others  => FALSE
  );

  -------------------------------------
  constant IsStoreInst : MnemoTableType := (
     ST    => TRUE, I_STD   => TRUE, STH  => TRUE, STB  => TRUE, STA   => TRUE,
     STDA  => TRUE, STHA  => TRUE, STBA => TRUE, STF  => TRUE, STDF  => TRUE,
     STFSR => TRUE, STDFQ => TRUE, STC  => TRUE, STDC => TRUE, STCSR => TRUE,
     STDCQ => TRUE,
     others => FALSE
  );
  
  -------------------------------------  
  constant IsStoreFP_CPInst : MnemoTableType := (
    STF  => TRUE, STDF  => TRUE, STFSR => TRUE, STDFQ => TRUE, STC  => TRUE,
    STDC => TRUE, STCSR => TRUE, STDCQ => TRUE,
    others => FALSE
  );
  
  -------------------------------------  
  constant IsStoreDoubleInst : MnemoTableType := (
    I_STD   => TRUE, STDA => TRUE, STDF => TRUE, STDFQ => TRUE, STDC => TRUE,
    STDCQ => TRUE,
    others => FALSE
  );
  
  -------------------------------------  
  constant IsStoreInstASI : MnemoTableType := (
    STA => TRUE, STDA => TRUE, STBA => TRUE, STHA => TRUE,
    others => FALSE
  );
  
  -------------------------------------  
  constant IsStoreSingleInst : MnemoTableType := (
    ST   => TRUE, STH  => TRUE, STB   => TRUE, STA  => TRUE, STBA  => TRUE,
    STHA => TRUE, STF  => TRUE, STFSR => TRUE, STC  => TRUE, STCSR => TRUE,
    others => FALSE
  );
  
  -------------------------------------  
  constant IsFPinst : MnemoTableType := (
    LDDF => TRUE, LDF => TRUE, LDFSR => TRUE, 
    STDF => TRUE, STF => TRUE, STFSR => TRUE,
    STDFQ => TRUE,
    FBN   => TRUE, FBNE => TRUE, FBLG  => TRUE, FBUL => TRUE,
    FBL   => TRUE, FBUG => TRUE, FBG   => TRUE, FBU  => TRUE,
    FBA   => TRUE, FBE  => TRUE, FBUE  => TRUE, FBGE => TRUE,
    FBUGE => TRUE, FBLE => TRUE, FBULE => TRUE, FBO  => TRUE,
    FiTOs  => TRUE, FiTOd  => TRUE, FiTOq  => TRUE, FsTOi  => TRUE,
    FdTOi  => TRUE, FqTOi  => TRUE, FsTOd  => TRUE, FsTOq  => TRUE,
    FdTOs  => TRUE, FdTOq  => TRUE, FqTOs  => TRUE, FqTOd  => TRUE,
    FMOVs  => TRUE, FNEGs  => TRUE, FABSs  => TRUE, FSQRTs => TRUE, 
    FSQRTd => TRUE, FSQRTq => TRUE, FADDs  => TRUE, FADDd  => TRUE,
    FADDq  => TRUE, FSUBs  => TRUE, FSUBd  => TRUE, FSUBq  => TRUE,
    FMULs  => TRUE, FMULd  => TRUE, FMULq  => TRUE,
    FsMULd => TRUE, FdMULq => TRUE, -- SPARC v.8 only for FsMULd & FdMULq
    FDIVs  => TRUE, FDIVd  => TRUE, FDIVq  => TRUE, FCMPs  => TRUE,
    FCMPd  => TRUE, FCMPq  => TRUE, FCMPEs => TRUE, FCMPEd => TRUE, 
    FCMPEq => TRUE,
    others => FALSE
  );
  
  -------------------------------------
  constant IsFPop : MnemoTableType := (
    FiTOs  => TRUE, FiTOd  => TRUE, FiTOq  => TRUE, FsTOi  => TRUE,
    FdTOi  => TRUE, FqTOi  => TRUE, FsTOd  => TRUE, FsTOq  => TRUE,
    FdTOs  => TRUE, FdTOq  => TRUE, FqTOs  => TRUE, FqTOd  => TRUE,
    FMOVs  => TRUE, FNEGs  => TRUE, FABSs  => TRUE, FSQRTs => TRUE, 
    FSQRTd => TRUE, FSQRTq => TRUE, FADDs  => TRUE, FADDd  => TRUE,
    FADDq  => TRUE, FSUBs  => TRUE, FSUBd  => TRUE, FSUBq  => TRUE,
    FMULs  => TRUE, FMULd  => TRUE, FMULq  => TRUE,
    FsMULd => TRUE, FdMULq => TRUE, -- SPARC v.8 only for FsMULd & FdMULq
    FDIVs  => TRUE, FDIVd  => TRUE, FDIVq  => TRUE, FCMPs  => TRUE,
    FCMPd  => TRUE, FCMPq  => TRUE, FCMPEs => TRUE, FCMPEd => TRUE, 
    FCMPEq => TRUE,
    others => FALSE
  );

  -------------------------------------
  constant IsFPopDouble : MnemoTableType := (
    FdTOi  => TRUE, FdTOs  => TRUE, FdTOq  => TRUE, FSQRTd => TRUE,
    FADDd  => TRUE, FSUBd  => TRUE, FMULd  => TRUE, 
    FdMULq => TRUE, -- SPARC v.8 only for FsMULd & FdMULq
    FDIVd  => TRUE, FCMPd  => TRUE, FCMPEd => TRUE, 
    others => FALSE
  );

  -------------------------------------
  constant IsFPopQuad : MnemoTableType := (
    FqTOi  => TRUE, FqTOs  => TRUE, FqTOd  => TRUE, FSQRTq => TRUE,
    FADDq  => TRUE, FSUBq  => TRUE, FMULq  => TRUE, FDIVq  => TRUE,
    FCMPq  => TRUE, FCMPEq => TRUE,
    others => FALSE
  );

  -------------------------------------
  constant IsFCMP : MnemoTableType := (
    FCMPs  => TRUE, FCMPd  => TRUE, FCMPq => TRUE,
    FCMPEs => TRUE, FCMPEd => TRUE, FCMPEq => TRUE,
    others => FALSE
  );  
  
  -------------------------------------
  function GetIndex(n : natural; CWP : std_logic_vector) return natural is
    variable Temp, CWPvar : natural;
  begin
    assert (n >= 0 and n < 32) report "(GetIndex): wrong value for n!"
                               severity error;
                  
    if (n >= 0 and n <= 7) then return n; -- global registers.
    end if;
    
    CWPvar := ToNatural(CWP);
    assert CWPvar < NWINDOWS report "(GetIndex): wrong value for CWP!"
                             severity error;
                  
    Temp := (n - 8 + CWPvar*16) mod (16*NWINDOWS) + 8;
    return Temp;
  end GetIndex; -- function
  
  -------------------------------------
  function Transcribe(A : std_logic_vector) return Instruction is
    constant L : natural := A'length;
    alias op     : std_logic_vector(1 downto 0) is A(31 downto 30);
    alias op2    : std_logic_vector(2 downto 0) is A(24 downto 22);
    alias op3    : std_logic_vector(5 downto 0) is A(24 downto 19);
    alias opf    : std_logic_vector(8 downto 0) is A(13 downto 5);
    alias cond   : std_logic_vector(3 downto 0) is A(28 downto 25);
    alias asi    : std_logic_vector(7 downto 0) is A(12 downto 5);
    alias i      : std_logic is A(13);
    alias a_bit  : std_logic is A(29);
    alias rd     : std_logic_vector(4 downto 0) is A(29 downto 25);
    alias rs1    : std_logic_vector(4 downto 0) is A(18 downto 14);
    alias rs2    : std_logic_vector(4 downto 0) is A( 4 downto 0);
    alias simm13 : std_logic_vector(12 downto 0) is A(12 downto 0);
    alias disp30 : std_logic_vector(29 downto 0) is A(29 downto 0);
    alias disp22 : std_logic_vector(21 downto 0) is A(21 downto 0);
    variable Result : Instruction;
  begin
    assert L = 32 report "(Transcribe): invalid vector length!"
                  severity error;
                  
    if VecUnknown(A) then
      Result.Mnemo := XXX;
      return Result; -- exit function here if unknown bits in A
    end if;
   
    -- For almost all instructions, these 5 fields are relevant: so
    -- they are assigned for all instructions.
    Result.BitInstr := A;
    Result.rd  := ToNatural(rd);
    Result.rs1 := ToNatural(rs1);
    Result.rs2 := ToNatural(rs2);
    Result.simm13 := ToNatural(simm13);
    if i = '0' then
      Result.i := 0; -- Result.i is a natural.
    else
      Result.i := 1;
    end if;
    
    case op is   -- 1st level
    
      when "00" =>  ---------- OP testing ---------------
      
        Result.disp22 := ToNatural(disp22); -- Common statement for op="00".
        Result.Annul := FALSE; -- explicit assignment to FALSE.
        
        case op2 is  -- 2nd level
          when "100" => -------------- OP + OP2 testing --------------
            Result.Mnemo := SETHI;
          when "010" => -------------- OP + OP2 testing --------------
            case cond is  -- 3rd level
              when "0000" => Result.Mnemo := BN;
              when "0001" => Result.Mnemo := BE;
              when "0010" => Result.Mnemo := BLE;
              when "0011" => Result.Mnemo := BL;
              when "0100" => Result.Mnemo := BLEU;
              when "0101" => Result.Mnemo := BCS;
              when "0110" => Result.Mnemo := BNEG;
              when "0111" => Result.Mnemo := BVS;
              when "1000" => Result.Mnemo := BA;
              when "1001" => Result.Mnemo := BNE;
              when "1010" => Result.Mnemo := BG;
              when "1011" => Result.Mnemo := BGE;
              when "1100" => Result.Mnemo := BGU;
              when "1101" => Result.Mnemo := BCC;
              when "1110" => Result.Mnemo := BPOS;
              when "1111" => Result.Mnemo := BVC;
              when others => NULL;
            end case; -- cond
            if a_bit = '0' then 
              Result.a := 0; -- Result.a is a natural.
            else
              Result.a := 1;
            end if;
            
          when "110" => -------------- OP + OP2 testing --------------
            case cond is  -- 3rd level
              when "0000" => Result.Mnemo := FBN;
              when "0001" => Result.Mnemo := FBNE;
              when "0010" => Result.Mnemo := FBLG;
              when "0011" => Result.Mnemo := FBUL;
              when "0100" => Result.Mnemo := FBL;
              when "0101" => Result.Mnemo := FBUG;
              when "0110" => Result.Mnemo := FBG;
              when "0111" => Result.Mnemo := FBU;
              when "1000" => Result.Mnemo := FBA;
              when "1001" => Result.Mnemo := FBE;
              when "1010" => Result.Mnemo := FBUE;
              when "1011" => Result.Mnemo := FBGE;
              when "1100" => Result.Mnemo := FBUGE;
              when "1101" => Result.Mnemo := FBLE;
              when "1110" => Result.Mnemo := FBULE;
              when "1111" => Result.Mnemo := FBO;
              when others => NULL;
            end case; -- cond            
            if a_bit = '0' then
              Result.a := 0; -- Result.a is a natural.
            else
              Result.a := 1;
            end if;
            
          when "111" => -------------- OP + OP2 testing --------------
            case cond is  -- 3rd level
              when "0000" => Result.Mnemo := CBN;
              when "0001" => Result.Mnemo := CB123;
              when "0010" => Result.Mnemo := CB12;
              when "0011" => Result.Mnemo := CB13;
              when "0100" => Result.Mnemo := CB1;
              when "0101" => Result.Mnemo := CB23;
              when "0110" => Result.Mnemo := CB2;
              when "0111" => Result.Mnemo := CB3;
              when "1000" => Result.Mnemo := CBA;
              when "1001" => Result.Mnemo := CB0;
              when "1010" => Result.Mnemo := CB03;
              when "1011" => Result.Mnemo := CB02;
              when "1100" => Result.Mnemo := CB023;
              when "1101" => Result.Mnemo := CB01;
              when "1110" => Result.Mnemo := CB013;
              when "1111" => Result.Mnemo := CB012;
              when others => NULL;
            end case; -- cond
            if a_bit = '0' then
              Result.a := 0; -- Result.a is a natural.
            else
              Result.a := 1;
            end if;
            
          when "000" => -------------- OP + OP2 testing --------------
            Result.Mnemo  := UNIMP;
          when others =>
            Result.Mnemo := ILLEGAL; -- Unknown and illegal instruction
        end case; -- op2
        
      when "11" => -------------- OP testing --------------
      
        Result.asi := ToNatural(asi);
        
        case op3 is  -- 2nd level: OP + OP3 testing -------
          when "000000" => Result.Mnemo := LD;
          when "000001" => Result.Mnemo := LDUB;
          when "000010" => Result.Mnemo := LDUH;
          when "000011" => Result.Mnemo := LDD;
          when "000100" => Result.Mnemo := ST;
          when "000101" => Result.Mnemo := STB;
          when "000110" => Result.Mnemo := STH;
          when "000111" => Result.Mnemo := I_STD;
          when "001001" => Result.Mnemo := LDSB;
          when "001010" => Result.Mnemo := LDSH;
          when "001101" => Result.Mnemo := LDSTUB;
          when "001111" => Result.Mnemo := SWAP;
          when "010000" => Result.Mnemo := LDA;
          when "010001" => Result.Mnemo := LDUBA;
          when "010010" => Result.Mnemo := LDUHA;
          when "010011" => Result.Mnemo := LDDA;
          when "010100" => Result.Mnemo := STA;
          when "010101" => Result.Mnemo := STBA;
          when "010110" => Result.Mnemo := STHA;
          when "010111" => Result.Mnemo := STDA;
          when "011001" => Result.Mnemo := LDSBA;
          when "011010" => Result.Mnemo := LDSHA;
          when "011101" => Result.Mnemo := LDSTUBA;
          when "011111" => Result.Mnemo := SWAPA;
          when "100000" => Result.Mnemo := LDF;
          when "100001" => Result.Mnemo := LDFSR;
          when "100011" => Result.Mnemo := LDDF;
          when "100100" => Result.Mnemo := STF;
          when "100101" => Result.Mnemo := STFSR;
          when "100110" => Result.Mnemo := STDFQ;
          when "100111" => Result.Mnemo := STDF;
          when "110000" => Result.Mnemo := LDC;
          when "110001" => Result.Mnemo := LDCSR;
          when "110011" => Result.Mnemo := LDDC;
          when "110100" => Result.Mnemo := STC;
          when "110101" => Result.Mnemo := STCSR;
          when "110110" => Result.Mnemo := STDCQ;
          when "110111" => Result.Mnemo := STDC;
          when others => 
            Result.Mnemo := ILLEGAL; -- Unknown and illegal instruction
        end case; -- op3
        
      when "10" => -------------- OP testing --------------
      
        case op3 is  -- 2nd level: OP + OP3 testing -------
          when "000000" => Result.Mnemo := ADD;
          when "000001" => Result.Mnemo := I_AND;
          when "000010" => Result.Mnemo := I_OR;
          when "000011" => Result.Mnemo := I_XOR;
          when "000100" => Result.Mnemo := SUB;
          when "000101" => Result.Mnemo := ANDN;
          when "000110" => Result.Mnemo := ORN;
          when "000111" => Result.Mnemo := XNOR;
          when "001000" => Result.Mnemo := ADDX;
          when "001010" => Result.Mnemo := UMUL; -- SPARC v.8
          when "001011" => Result.Mnemo := SMUL; -- SPARC v.8
          when "001100" => Result.Mnemo := SUBX;
          when "001110" => Result.Mnemo := UDIV; -- SPARC v.8
          when "001111" => Result.Mnemo := SDIV; -- SPARC v.8
          when "010000" => Result.Mnemo := ADDcc;
          when "010001" => Result.Mnemo := ANDcc;
          when "010010" => Result.Mnemo := ORcc;
          when "010011" => Result.Mnemo := XORcc;
          when "010100" => Result.Mnemo := SUBcc;
          when "010101" => Result.Mnemo := ANDNcc;
          when "010110" => Result.Mnemo := ORNcc;
          when "010111" => Result.Mnemo := XNORcc;
          when "011000" => Result.Mnemo := ADDXcc;
          when "011010" => Result.Mnemo := UMULcc; -- SPARC v.8
          when "011011" => Result.Mnemo := SMULcc; -- SPARC v.8
          when "011100" => Result.Mnemo := SUBXcc;
          when "011110" => Result.Mnemo := UDIVcc; -- SPARC v.8
          when "011111" => Result.Mnemo := SDIVcc; -- SPARC v.8
          when "100000" => Result.Mnemo := TADDcc;
          when "100001" => Result.Mnemo := TSUBcc;
          when "100010" => Result.Mnemo := TADDccTV;
          when "100011" => Result.Mnemo := TSUBccTV;
          when "100100" => Result.Mnemo := MULScc;
          when "100101" => Result.Mnemo := SLL;
          when "100110" => Result.Mnemo := SRL;
          when "100111" => Result.Mnemo := SRA;
          when "101000" => Result.Mnemo := RDY;
          when "101001" => Result.Mnemo := RDPSR;
          when "101010" => Result.Mnemo := RDWIM;
          when "101011" => Result.Mnemo := RDTBR;
          when "110000" => Result.Mnemo := WRY;
          when "110001" => Result.Mnemo := WRPSR;
          when "110010" => Result.Mnemo := WRWIM;
          when "110011" => Result.Mnemo := WRTBR;
          when "110100" => --- FPop1 instructions ---
            case opf is
              when "000000001" => Result.Mnemo := FMOVs;
              when "000000101" => Result.Mnemo := FNEGs;
              when "000001001" => Result.Mnemo := FABSs;
              when "000101001" => Result.Mnemo := FSQRTs;
              when "000101010" => Result.Mnemo := FSQRTd;
              when "000101011" => Result.Mnemo := FSQRTq;
              when "001000001" => Result.Mnemo := FADDs;
              when "001000010" => Result.Mnemo := FADDd;
              when "001000011" => Result.Mnemo := FADDq;
              when "001000101" => Result.Mnemo := FSUBs;
              when "001000110" => Result.Mnemo := FSUBd;
              when "001000111" => Result.Mnemo := FSUBq;
              when "001001001" => Result.Mnemo := FMULs;
              when "001001010" => Result.Mnemo := FMULd;
              when "001001011" => Result.Mnemo := FMULq;
              when "001001101" => Result.Mnemo := FDIVs;
              when "001001110" => Result.Mnemo := FDIVd;
              when "001001111" => Result.Mnemo := FDIVq;
              when "001101001" => Result.Mnemo := FsMULd; -- SPARC v.8
              when "001101110" => Result.Mnemo := FdMULq; -- SPARC v.8
              when "011000100" => Result.Mnemo := FiTOs;
              when "011000110" => Result.Mnemo := FdTOs;
              when "011000111" => Result.Mnemo := FqTOs;
              when "011001000" => Result.Mnemo := FiTOd;
              when "011001001" => Result.Mnemo := FsTOd;
              when "011001011" => Result.Mnemo := FqTOd;
              when "011001100" => Result.Mnemo := FiTOq;
              when "011001101" => Result.Mnemo := FsTOq;
              when "011001110" => Result.Mnemo := FdTOq;
              when "011010001" => Result.Mnemo := FsTOi;
              when "011010010" => Result.Mnemo := FdTOi;
              when "011010011" => Result.Mnemo := FqTOi;
              when others =>
                Result.Mnemo := ILLEGAL; -- Unknown and illegal instruction
            end case; -- opf
            
          when "110101" => --- FPop2 instructions ---
            case opf is
              when "001010001" => Result.Mnemo := FCMPs;
              when "001010010" => Result.Mnemo := FCMPd;
              when "001010011" => Result.Mnemo := FCMPq;
              when "001010101" => Result.Mnemo := FCMPEs;
              when "001010110" => Result.Mnemo := FCMPEd;
              when "001010111" => Result.Mnemo := FCMPEq;
              when others =>
                Result.Mnemo := ILLEGAL; -- Unknown and illegal instruction
            end case; -- opf
          
          when "110110" => Result.Mnemo := CPop1;
          when "110111" => Result.Mnemo := CPop2;
          when "111000" => Result.Mnemo := JMPL;
          when "111001" => Result.Mnemo := RETT;
          when "111010" => --- Ticc instructions ---
            case cond is  -- 3rd level
              when "0000" => Result.Mnemo := TN;
              when "0001" => Result.Mnemo := TE;
              when "0010" => Result.Mnemo := TLE;
              when "0011" => Result.Mnemo := TL;
              when "0100" => Result.Mnemo := TLEU;
              when "0101" => Result.Mnemo := TCS;
              when "0110" => Result.Mnemo := TNEG;
              when "0111" => Result.Mnemo := TVS;
              when "1000" => Result.Mnemo := TA;
              when "1001" => Result.Mnemo := TNE;
              when "1010" => Result.Mnemo := TG;
              when "1011" => Result.Mnemo := TGE;
              when "1100" => Result.Mnemo := TGU;
              when "1101" => Result.Mnemo := TCC;
              when "1110" => Result.Mnemo := TPOS;
              when "1111" => Result.Mnemo := TVC;
              when others => NULL;
            end case; -- cond
          
          when "111011" => Result.Mnemo := FLUSH;
          when "111100" => Result.Mnemo := SAVE;
          when "111101" => Result.Mnemo := RESTORE;
          when others =>
            Result.Mnemo := ILLEGAL; -- Unknown and illegal instruction
        end case; -- op3
        
      when "01" =>  -------------- OP testing --------------
        Result.Mnemo := CALL;
        Result.disp30 := disp30;
        
      when others => NULL;

    end case; -- op
      
    return Result;
  end Transcribe; -- function
  
  -------------------------------------
  function iccEvaluation(Mnemonic : SuperInstMnemonic;
                         icc      : std_logic_vector) return boolean is
  begin
  
    if ( 
      (Mnemonic = BA) or
      (Mnemonic = BNE  and icc(Z_ICC) = '0') or
      (Mnemonic = BE   and icc(Z_ICC) = '1') or
      (Mnemonic = BG   and (icc(Z_ICC) or (icc(N_ICC) xor icc(V_ICC))) = '0')or
      (Mnemonic = BLE  and (icc(Z_ICC) or (icc(N_ICC) xor icc(V_ICC))) = '1')or
      (Mnemonic = BGE  and (icc(N_ICC) xor icc(V_ICC)) = '0') or
      (Mnemonic = BL   and (icc(N_ICC) xor icc(V_ICC)) = '1')or
      (Mnemonic = BGU  and ((icc(C_ICC) = '0') and (icc(Z_ICC) = '0')) ) or
      (Mnemonic = BLEU and ((icc(C_ICC) = '1') or (icc(Z_ICC) = '1')) ) or
      (Mnemonic = BCC  and icc(C_ICC) = '0') or
      (Mnemonic = BCS  and icc(C_ICC) = '1') or
      (Mnemonic = BPOS and icc(N_ICC) = '0') or
      (Mnemonic = BNEG and icc(N_ICC) = '1') or
      (Mnemonic = BVC  and icc(V_ICC) = '0') or
      (Mnemonic = BVS  and icc(V_ICC) = '1') 
        ) then return TRUE;
    end if;
   
    return FALSE;
   
  end iccEvaluation; -- function
  
  -------------------------------------
  function TiccEval(Mnemonic : SuperInstMnemonic;
                    icc      : std_logic_vector) return boolean is
  begin
    
    if ( 
      (Mnemonic = TA) or
      (Mnemonic = TNE  and icc(Z_ICC) = '0') or
      (Mnemonic = TE   and icc(Z_ICC) = '1') or
      (Mnemonic = TG   and (icc(Z_ICC) or (icc(N_ICC) xor icc(V_ICC))) = '0')or
      (Mnemonic = TLE  and (icc(Z_ICC) or (icc(N_ICC) xor icc(V_ICC))) = '1')or
      (Mnemonic = TGE  and (icc(N_ICC) xor icc(V_ICC)) = '0') or
      (Mnemonic = TL   and (icc(N_ICC) xor icc(V_ICC)) = '1')or
      (Mnemonic = TGU  and ((icc(C_ICC) = '0') and (icc(Z_ICC) = '0')) ) or
      (Mnemonic = TLEU and ((icc(C_ICC) = '1') or (icc(Z_ICC) = '1')) ) or
      (Mnemonic = TCC  and icc(C_ICC) = '0') or
      (Mnemonic = TCS  and icc(C_ICC) = '1') or
      (Mnemonic = TPOS and icc(N_ICC) = '0') or
      (Mnemonic = TNEG and icc(N_ICC) = '1') or
      (Mnemonic = TVC  and icc(V_ICC) = '0') or
      (Mnemonic = TVS  and icc(V_ICC) = '1')
       ) then return TRUE;
    end if;
   
    return FALSE;
   
  end TiccEval; -- function
                         
  -------------------------------------
  function fccEvaluation(Mnemonic   : SuperInstMnemonic;
                         signal fcc : std_logic_vector) return boolean is
    variable E, L, G, U : boolean := FALSE;
    variable Local_fcc : std_logic_vector(1 downto 0) := fcc(1 downto 0);
  begin
    
    case Local_fcc(1 downto 0) is
      when "00" => E := TRUE; -- Equal
      when "01" => L := TRUE; -- Less
      when "10" => G := TRUE; -- Greater
      when "11" => U := TRUE; -- Unordered
      when others => NULL;
    end case; -- Local_fcc
    
    if (
      (Mnemonic = FBA) or
      (Mnemonic = FBU   and U) or
      (Mnemonic = FBG   and G) or
      (Mnemonic = FBUG  and (G or U) ) or
      (Mnemonic = FBL   and L) or
      (Mnemonic = FBUL  and (L or U) ) or
      (Mnemonic = FBLG  and (L or G) ) or
      (Mnemonic = FBNE  and (L or G or U) ) or
      (Mnemonic = FBE   and E ) or
      (Mnemonic = FBUE  and (E or U) ) or
      (Mnemonic = FBGE  and (E or G) ) or
      (Mnemonic = FBUGE and (E or G or U) ) or
      (Mnemonic = FBLE  and (E or L) ) or
      (Mnemonic = FBULE and (E or L or U) ) or
      (Mnemonic = FBO   and (E or L or G) )
       ) then return TRUE;
    end if;
    
    return FALSE;

  end fccEvaluation; -- function

  -------------------------------------
  function IURegDependency(rd    : natural;
                           InstB : Instruction) return boolean is
  begin
    if rd = 0 then return FALSE;
    end if;
    
    if IURs1Rs2AreIn(InstB.Mnemo) then
    
      if InstB.rs1 = rd then return TRUE;
      end if;
      
      if (InstB.i = 0 and InstB.rs2 = rd) then return TRUE;
      end if;
      
    end if;
    
    return FALSE;
  end IURegDependency; -- function
  
  -------------------------------------
--  function FPURegDependency(rd    : natural;
--                            Mnemo : SuperInstMnemonic;
--                            rs1   : natural;
--                            rs2   : natural) return boolean is
--    variable rd_quad, rs1_quad, rs2_quad : natural;
--    variable rd_double, rs1_double, rs2_double : natural;
--  begin
--    if IsFPop(Mnemo) then
--      if IsFPopDouble(Mnemo) then
--        rd_double  := rd/4;
--        rs1_double := rs1/4;
--        rs2_double := rs2/4;
--        if rs2_double = rd_double then return TRUE;
--        end if;
--        if FPURs1IsIn(Mnemo) then
--          if rs1_double = rd_double then return TRUE;
--          end if;
--        end if;
--      elsif IsFPopQuad(Mnemo) then
--        rd_quad  := rd/4;
--        rs1_quad := rs1/4;
--        rs2_quad := rs2/4;
--        if rs2_quad = rd_quad then return TRUE;
--        end if;
--        if FPURs1IsIn(Mnemo) then
--          if rs1_quad = rd_quad then return TRUE;
--          end if;
--        end if;
--      else
--        if rs2 = rd then return TRUE;
--        end if;
--        if FPURs1IsIn(Mnemo) then
--          if rs1 = rd then return TRUE;
--          end if;
--        end if;
--      end if;
--    end if;
    
--    return FALSE;
--  end FPURegDependency; -- function
  
  -------------------------------------
  function LoadORStoreORSwapAddrCalc(Inst    : Instruction;
                                     CWP     : std_logic_vector;
                                     RegFile : RegisterFile) 
                                                return std_logic_vector is
    variable rs1, rs2 : natural;
    variable Address  : std_logic_vector(31 downto 0);
  begin
    rs1 := GetIndex(Inst.rs1, CWP);
    
    case Inst.Mnemo is
      when LD     | LDD   | LDSB  | LDUB  | LDSH  | LDUH  |
           LDF    | LDDF  | LDFSR | LDC   | LDDC  | LDCSR |
           ST     | I_STD | STB   | STH   | STF   | STDF  |
           STFSR  | STDFQ | STC   | STDC  | STCSR | STDCQ | 
           LDSTUB | SWAP =>
         if Inst.i = 0 then
           rs2 := GetIndex(Inst.rs2, CWP);
           Address := RegFile(rs1) + RegFile(rs2);
         else
           Address := Extend(ToStdLogicVector(Inst.simm13, 13), 32);
           Address := Address + RegFile(rs1);
         end if; 
         
      when LDA     | LDDA | LDSBA | LDUBA | LDSHA | LDUHA |
           STA     | STDA | STBA  | STHA  |
           LDSTUBA | SWAPA =>
           rs2 := GetIndex(Inst.rs2, CWP);
           Address := RegFile(rs1) + RegFile(rs2);
         
      when others =>
        assert FALSE report "(LoadORStoreORSwapAddrCalc): error in VHDL model!"
                     severity error;
    end case; -- Inst.Mnemo
    
    return Address;
  end LoadORStoreORSwapAddrCalc; -- function

  
  -------------------------------------
  function OddParityOf(Vec : std_logic_vector) return std_logic is
    constant L      : integer := Vec'length;
    alias VecAlias  : std_logic_vector(L - 1 downto 0) is Vec;
    variable Parity : std_logic := '0';
  begin
    for i in VecAlias'range loop
      Parity := Parity xor VecAlias(i);
    end loop;
    return not(Parity);
  end OddParityOf;

  -------------------------------------
                        
  -------------------------------------
  --********** PROCEDURES *************
  -------------------------------------
  
  -------------------------------------
  procedure TrapHandler(signal EX   : Instruction;
                        signal WR   : Instruction;
                               WR1  : Instruction;
                               WR2  : Instruction;
                        pIRLvar     : natural;
                        IRLvar      : natural;
                        TBR         : inout std_logic_vector;
                        PSR         : inout std_logic_vector;
                        TrapVector  : inout TrapVectorType;
                        Mode        : inout ModeType;
                        TrapMode    : out TrapModeType;
                        pPrevAddr   : inout std_logic_vector;
                        PrevAddr    : inout std_logic_vector;
                        CurrentAddr : inout std_logic_vector;
--                       RegFile     : out RegisterFile) is -- gd 2208
                        RegFile     : inout RegisterFile) is
    alias S   : std_logic is PSR(7);
    alias PS  : std_logic is PSR(6);
    alias ET  : std_logic is PSR(5);
    alias PIL : std_logic_vector(3 downto 0) is PSR(11 downto  8);
    alias CWP : std_logic_vector(4 downto 0) is PSR(4 downto 0);
    alias tt  : std_logic_vector( 7 downto 0) is TBR(11 downto  4);
    variable PILvar, CWPvar : natural;
  begin
      
  --........  Asynchronous interrupt handling: IRL bus ..........
    if VecUnknown(PIL) then
      PILvar := 0;
    else
      PILvar := ToNatural(PIL);
    end if;
        
    if ( (EX.Mnemo /= IOP) and
         (EX.Mnemo /= LDSTUB and EX.Mnemo /= LDSTUBA) and
         (EX.Mnemo /= SWAP and EX.Mnemo /= SWAPA) and
         (EX.Mnemo /= RETT) and  -- on EX=RETT, ET <= 1; so wait 1 cycle.
         ((pIRLvar = IRLvar) and IRLvar /=0) and -- filter transients on IRL.
         (ET = '1' and ((IRLvar = 15) or (IRLvar > PILvar))) 
       ) then
      TrapVector(DETECTED_TRAP) := TRUE;
      TrapVector(INTERRUPT_LEVEL) := TRUE;
    end if;
        
  --................ Servicing detected traps. ..................
    if TrapVector(DETECTED_TRAP) then
  
      TrapMode := SYNCH_TRAP; -- SYNCHronous trap; flag used for pipeline 
                              -- progression.
  
       -- select trap
      if  TrapVector(NON_RESTART_IMPRECISE)   then
        tt := TrapTypeTable(NON_RESTART_IMPRECISE);
        
      elsif  TrapVector(RESTART_IMPRECISE)    then
        tt := TrapTypeTable(RESTART_IMPRECISE);
              
      elsif  TrapVector(PROGRAM_FLOW_ERR)     then
        tt := TrapTypeTable(PROGRAM_FLOW_ERR);
      
      elsif  TrapVector(INST_ACCESS)          then
        tt := TrapTypeTable(INST_ACCESS);
      
      elsif TrapVector(ILLEGAL_INST)          then
        tt := TrapTypeTable(ILLEGAL_INST);
      
      elsif TrapVector(PRIVILEGED_INST)       then
        tt := TrapTypeTable(PRIVILEGED_INST);
      
      elsif TrapVector(FP_DISABLED)           then
        tt := TrapTypeTable(FP_DISABLED);
      
      elsif TrapVector(CP_DISABLED)           then
        tt := TrapTypeTable(CP_DISABLED);
      
      elsif TrapVector(WINDOW_OVERFLOW)       then
        tt := TrapTypeTable(WINDOW_OVERFLOW);
      
      elsif TrapVector(WINDOW_UNDERFLOW)      then
        tt := TrapTypeTable(WINDOW_UNDERFLOW);
      
      elsif TrapVector(MEM_ADDR_NOT_ALIGNED)  then
        tt := TrapTypeTable(MEM_ADDR_NOT_ALIGNED);
      
      elsif TrapVector(FP_EXCEPTION)          then
        tt := TrapTypeTable(FP_EXCEPTION);
      
      elsif TrapVector(CP_EXCEPTION)          then
        tt := TrapTypeTable(CP_EXCEPTION);
      
      elsif TrapVector(DATA_ACCESS_EXCEPTION) then
        tt := TrapTypeTable(DATA_ACCESS_EXCEPTION);
      
      elsif TrapVector(TAG_OVERFLOW)          then
        tt := TrapTypeTable(TAG_OVERFLOW);
      
      elsif TrapVector(TRAP_INST)             then
        NULL; -- tt remains unchanged; value computed by Ticc in ExecutionBody.
            
      elsif TrapVector(INTERRUPT_LEVEL)       then
        tt := "0001" & ToStdLogicVector(IRLvar, 4);
        TrapMode := ASYNCH_TRAP; -- ASYNCHronous trap.
      
      end if;
      
    -- activates a trap window
      CWPvar := ToNatural(CWP);
      CWPvar := (CWPvar - 1) mod NWINDOWS;
      CWP := ToStdLogicVector(CWPvar, CWP'length);
    
    --++++ CAREFUL HERE: cf. Bicc and return from traps. ++++
    --++++ The logical condition might be false if func- ++++
    --++++ -tionality has been misinterpreted.           ++++
      if ( (EX.Mnemo = ANNULLED) and 
           (IsBicc(WR.Mnemo) or IsFBfcc(WR.Mnemo)) ) then 
        RegFile(GetIndex(17, CWP)) := EX.NextAddress;
        RegFile(GetIndex(18, CWP)) := EX.NextAddress + 4;
      elsif IsLoadSingleInst(WR1.Mnemo) or IsLoadDoubleInst(WR2.Mnemo) then
      -- PC and nPC of trapping instruction stored: single load
        RegFile(GetIndex(17, CWP)) := WR.Address;
        RegFile(GetIndex(18, CWP)) := WR.NextAddress;
      else
      -- PC and nPC of trapping instruction stored: regular case.
        RegFile(GetIndex(17, CWP)) := EX.Address;
        RegFile(GetIndex(18, CWP)) := EX.NextAddress;
      end if;
    --++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    -- Address calculation.
      pPrevAddr   := PrevAddr;
      PrevAddr    := CurrentAddr;
      CurrentAddr := TBR; -- branching with TBR.
      
    -- Perform trap actions
      PS := S;
    
    -- enter supervisor mode.
      S  := '1'; 
      
    -- Detect error condition
      if ET = '0' then
        Mode := ERROR_MODE;
        return; -- exit here from procedure
      else
        ET := '0';
      end if;

    end if;
        
  end TrapHandler; -- procedure
  
  -------------------------------------
  procedure ExecutionBody(signal FP_N : std_logic;
                          signal IFT_N: std_logic;
                          signal EX   : Instruction;
                          ResultOpcc  : std_logic_vector;
                          YTemp       : std_logic_vector;
                          iccTemp     : inout std_logic_vector;
                          Y           : inout std_logic_vector;
                          PSR         : inout std_logic_vector;
                          TBR         : inout std_logic_vector;
                          WIM         : inout std_logic_vector;
                          RegFile     : inout RegisterFile;
                          Mode        : inout ModeType;
                          TrapVector  : inout TrapVectorType) is
    alias icc : std_logic_vector(3 downto 0) is PSR(23 downto 20);
    alias EC  : std_logic is PSR(13);
    alias EF  : std_logic is PSR(12);
    alias S   : std_logic is PSR(7);
    alias PS  : std_logic is PSR(6);
    alias ET  : std_logic is PSR(5);
    alias CWP : std_logic_vector(4 downto 0) is PSR(4 downto 0);
    alias tt  : std_logic_vector( 7 downto 0) is TBR(11 downto  4);
      
    variable rd, rs1, rs2 : natural;
  begin
  
    case EX.Mnemo is
      when SETHI =>     
         rd  := GetIndex(EX.rd, CWP);
         if rd /= 0 then
           RegFile(rd)(31 downto 10) := Extend(ToStdLogicVector(EX.disp22), 22);
           RegFile(rd)( 9 downto  0) := (others => '0');
         end if;
         
      when LD   | LDD  | LDSB  | LDUB  | LDSH  | LDUH  |
           LDA  | LDDA | LDSBA | LDUBA | LDSHA | LDUHA |
           LDF  | LDDF | LDFSR | LDC   | LDDC  | LDCSR   =>
         LoadInstruction(FP_N, EX, PSR, RegFile, TrapVector);
         
      when ST   | I_STD | STB   | STH   | STF   | STDF |
           STA  | STDA  | STBA  | STHA  | STFSR | STC  |
           STDC | STCSR | STDFQ | STDCQ  => 
         StoreInstruction(FP_N, EX, PSR, RegFile, TrapVector);
         
      when I_AND | ANDcc | ANDN  | ANDNcc | I_OR | ORcc   |
           ORN   | ORNcc | I_XOR | XORcc  | XNOR | XNORcc  => 
         LogicalInstruction(EX, CWP, ResultOPcc, iccTemp, RegFile, icc);
         
      when ADD | ADDcc | ADDX | ADDXcc |
           SUB | SUBcc | SUBX | SUBXcc  =>
         AddSubInstruction(EX, CWP, ResultOPcc, iccTemp, RegFile, icc);
                  
      when SLL | SRL | SRA =>
         ShiftInstruction(EX, CWP, RegFile);
         
      when TADDcc | TADDccTV |
           TSUBcc | TSUBccTV  =>
         TaggedAddSubbInst(EX, CWP, ResultOPcc, iccTemp, 
                           icc, RegFile,TrapVector);
         
      when MULScc =>
         MultiplyStepInst(EX, CWP, ResultOPcc, iccTemp, YTemp,
                          Y, icc, Regfile);
         
      when SAVE | RESTORE =>
         SaveRestoreInst(EX, WIM, CWP, RegFile, TrapVector);
         
      when BN   | BE  | BLE  | BL  | BLEU | BCS |
           BNEG | BVS | BA   | BNE | BG   | BGE |
           BGU  | BCC | BPOS | BVC  =>
         NULL; -- Nothing to be done here; "anticipated" execution with the 
               -- procedure ExecuteBicc.
         
      when FBN   | FBNE | FBLG  | FBUL | FBL  | FBUG |
           FBG   | FBU  | FBA   | FBE  | FBUE | FBGE |
           FBUGE | FBLE | FBULE | FBO  =>
         if (EF = '0' or FP_N = '1') then
           TrapVector(DETECTED_TRAP) := TRUE;
           TrapVector(FP_DISABLED) := TRUE;
         end if;
         -- Nothing to be done in particular except for trap detection;
         -- "anticipated" execution with the procedure ExecuteFBfcc.
         
      when CALL =>
         RegFile(GetIndex(15, CWP)) := EX.Address; -- Put PC of CALL in r15
         
      when JMPL =>
         JmplInstruction(EX, CWP, RegFile, TrapVector);
          
      when RETT =>
         RettInstruction(EX, RegFile, WIM, PSR, Mode, tt, TrapVector);
         
      when TN   | TE  | TLE  | TL  | TLEU | TCS |
           TNEG | TVS | TA   | TNE | TG   | TGE |
           TGU  | TCC | TPOS | TVC  =>
         TiccInstruction(EX, CWP, icc, RegFile, tt, TrapVector);
         
      when RDPSR | RDTBR | RDWIM | RDY =>
         ReadStateRegInst(EX, PSR, TBR, WIM, Y, TrapVector, RegFile);
         
      when WRPSR | WRTBR | WRWIM | WRY =>
         WriteStateRegInst(EX, RegFile, PSR, iccTemp, TBR, WIM, Y, TrapVector);
         
      when LDSTUB | LDSTUBA | SWAP | SWAPA =>
         LoadStoreSwapInstruction(EX, S, CWP, RegFile, TrapVector);
                  
      when UNIMP =>
         TrapVector(DETECTED_TRAP) := TRUE;
         TrapVector(ILLEGAL_INST) := TRUE;
         
      when FLUSH =>
         if IFT_N = '0' then
           TrapVector(DETECTED_TRAP) := TRUE;
           TrapVector(ILLEGAL_INST) := TRUE;
         end if;
         
      when FMOVs  | FNEGs | FABSs | FSQRTs | FSQRTd | FSQRTq |
           FADDs  | FADDd | FADDq | FSUBs  | FSUBd  | FSUBq  |
           FMULs  | FMULd | FMULq | FDIVs  | FDIVd  | FDIVq  |
           FiTOs  | FdTOs | FqTOs | FiTOd  | FsTOd  | FqTOd  |
           FiTOq  | FsTOq | FdTOq | FsTOi  | FdTOi  | FqTOi  |
           FCMPs  | FCMPd | FCMPq | FCMPEs | FCMPEd | FCMPEq | 
           FsMULd | FdMULq  =>   -- These last 2 instructions: SPARC v.8 only
         if (EF = '0' or FP_N = '1') then
           TrapVector(DETECTED_TRAP) := TRUE;
           TrapVector(FP_DISABLED) := TRUE;
         end if;
         if (EX.Mnemo = FsMULd or EX.Mnemo = FdMULq) then
           TrapVector(DETECTED_TRAP) := TRUE;
           TrapVector(ILLEGAL_INST) := TRUE;
         end if;
         
      when ILLEGAL =>
         TrapVector(DETECTED_TRAP) := TRUE;
         TrapVector(ILLEGAL_INST)  := TRUE;
         
      when CBN   | CB123 | CB12  | CB13 | CB1  | CB23 |
           CB2   | CB3   | CBA   | CB0  | CB03 | CB02 |
           CB023 | CB01  | CB013 | CB012  =>
         TrapVector(DETECTED_TRAP) := TRUE; -- The coprocessor interface is not
         TrapVector(CP_DISABLED)   := TRUE; -- implemented in this version.
         
      when CPop1 | CPop2 =>
         TrapVector(DETECTED_TRAP) := TRUE; -- The coprocessor interface is not
         TrapVector(CP_DISABLED)   := TRUE; -- implemented in this version.
         
      when UMUL | UMULcc | SMUL | SMULcc |
           UDIV | UDIVcc | SDIV | SDIVcc  =>
         TrapVector(DETECTED_TRAP) := TRUE; -- SPARC v.8 instructions
         TrapVector(ILLEGAL_INST)  := TRUE; -- not implemented.
         
      when others =>
         if (EX.Mnemo /= XXX and EX.Mnemo /= IOP and
             EX.Mnemo /= NOTHING and EX.Mnemo /= ANNULLED) then
           assert FALSE report "(ExecutionBody): error in VHDL model!"
                        severity error;
         end if;
         
    end case; -- EX.Mnemo
    
  end ExecutionBody; -- procedure
  
  -------------------------------------
  procedure LoadInstruction(signal FP_N : std_logic;
                            EX          : Instruction;
                            PSR         : std_logic_vector;
                            RegFile     : RegisterFile;
                            TrapVector  : inout TrapVectorType) is    
    alias EF  : std_logic is PSR(12);
    alias S   : std_logic is PSR(7);
    alias CWP : std_logic_vector(4 downto 0) is PSR(4 downto 0);
    variable rs1, rs2 : natural;
    variable Address : std_logic_vector(31 downto 0);
  begin
    rs1 := GetIndex(EX.rs1, CWP);
    
    case EX.Mnemo is
      when LD   | LDD  | LDSB  | LDUB  | LDSH  | LDUH  |
           LDF  | LDDF | LDFSR | LDC   | LDDC  | LDCSR  =>
         if EX.i = 0 then
           rs2 := GetIndex(EX.rs2, CWP);
           Address := RegFile(rs1) + RegFile(rs2);
         else
           Address := Extend(ToStdLogicVector(EX.simm13, 13), 32);
           Address := Address + RegFile(rs1);
         end if; 
         
      when LDA  | LDDA | LDSBA | LDUBA | LDSHA | LDUHA  =>
         -- WARNING: here ILLEGAL_INST trap priority > PRIVILEGED_INST trap
         if EX.i = 1 then
           TrapVector(DETECTED_TRAP) := TRUE;
           TrapVector(ILLEGAL_INST) := TRUE;
           return; -- exit here from procedure
         end if;
         
         if S = '0' then
           TrapVector(DETECTED_TRAP) := TRUE;
           TrapVector(PRIVILEGED_INST) := TRUE;
           return; -- exit here from procedure
         end if;

         rs2 := GetIndex(EX.rs2, CWP);
         Address := RegFile(rs1) + RegFile(rs2);
         
      when others =>
        assert FALSE report "(LoadInstruction): error in VHDL model!"
                     severity error;
    end case; -- EX.Mnemo
    
    
    if ( (EX.Mnemo = LDF or EX.Mnemo = LDDF or EX.Mnemo = LDFSR) and
         (EF = '0' or FP_N = '1') ) then
      TrapVector(DETECTED_TRAP) := TRUE;
      TrapVector(FP_DISABLED) := TRUE;
      return; -- exit here from procedure
      
    elsif (EX.Mnemo = LDC or EX.Mnemo = LDDC or EX.Mnemo = LDCSR) then
      TrapVector(DETECTED_TRAP) := TRUE; -- Coprocessor not implemented.
      TrapVector(CP_DISABLED) := TRUE;
      return; -- exit here from procedure
      
    elsif ( ((EX.Mnemo = LDD or EX.Mnemo = LDDA or EX.Mnemo = LDDF or
             EX.Mnemo = LDDC) and Address(2 downto 0) /= "000") or
            ((EX.Mnemo = LD or EX.Mnemo = LDA or EX.Mnemo = LDF or
             EX.Mnemo = LDFSR or EX.Mnemo = LDC or EX.Mnemo = LDCSR) and
             Address(1 downto 0) /= "00") or
            ((EX.Mnemo = LDSH or EX.Mnemo = LDSHA or EX.Mnemo = LDUH or
             EX.Mnemo = LDUHA) and Address(0) /= '0') ) then
      TrapVector(DETECTED_TRAP) := TRUE;
      TrapVector(MEM_ADDR_NOT_ALIGNED) := TRUE;
      return; -- exit here from procedure
      
    end if;
    
  end LoadInstruction; -- procedure
  
  -------------------------------------
  procedure StoreInstruction(signal FP_N : std_logic;
                             EX          : Instruction;
                             PSR         : std_logic_vector;
                             RegFile     : RegisterFile;
                             TrapVector  : inout TrapVectorType) is
    alias EF  : std_logic is PSR(12);
    alias S   : std_logic is PSR(7);
    alias CWP : std_logic_vector(4 downto 0) is PSR(4 downto 0);
    variable rs1, rs2 : natural;
    variable Address : std_logic_vector(31 downto 0);
  begin
  
    case EX.Mnemo is
      when STA | STDA | STBA | STHA =>
         -- WARNING: here ILLEGAL_INST trap priority > PRIVILEGED_INST trap
         if EX.i = 1 then
           TrapVector(DETECTED_TRAP) := TRUE;
           TrapVector(ILLEGAL_INST) := TRUE;
           return; -- exit here from procedure
         end if;
         
         if S = '0' then
           TrapVector(DETECTED_TRAP) := TRUE;
           TrapVector(PRIVILEGED_INST) := TRUE;
           return; -- exit here from procedure
         end if;
         
      when STDFQ | STDCQ  =>
         if S = '0' then
           TrapVector(DETECTED_TRAP) := TRUE;
           TrapVector(PRIVILEGED_INST) := TRUE;
           return; -- exit here from procedure
         end if;
         
      when others =>
         NULL;
    end case; --EX.Mnemo
    
    if ( (EX.Mnemo = STF or EX.Mnemo = STDF or EX.Mnemo = STFSR or
          EX.Mnemo = STDFQ) and
         (EF = '0' or FP_N = '1') ) then
      TrapVector(DETECTED_TRAP) := TRUE;
      TrapVector(FP_DISABLED) := TRUE;
      return; -- exit here from procedure
    end if;
      
    if (EX.Mnemo = STC or EX.Mnemo = STDC or EX.Mnemo = STCSR or
           EX.Mnemo = STDCQ) then
      TrapVector(DETECTED_TRAP) := TRUE; -- Coprocessor not implemented.
      TrapVector(CP_DISABLED) := TRUE;
      return; -- exit here from procedure
      
    end if;
    
    rs1 := GetIndex(EX.rs1, CWP);
    
    case EX.Mnemo is
      when ST    | I_STD | STB | STH  | STF   | STDF | 
           STFSR | STDFQ | STC | STDC | STCSR | STDCQ  =>
         if EX.i = 0 then
           rs2 := GetIndex(EX.rs2, CWP);
           Address := RegFile(rs1) + RegFile(rs2);
         else
           Address := Extend(ToStdLogicVector(EX.simm13, 13), 32);
           Address := Address + RegFile(rs1);
         end if; 
           
      when STA | STDA | STBA | STHA =>
           rs2 := GetIndex(EX.rs2, CWP);
           Address := RegFile(rs1) + RegFile(rs2);
      
      when others =>
        assert FALSE report "(LoadInstruction): error in VHDL model!"
                     severity error;
    end case; --EX.Mnemo
    
    if ( ( (EX.Mnemo = ST or EX.Mnemo = STA or EX.Mnemo = STF or 
            EX.Mnemo = STFSR or EX.Mnemo = STC or EX.Mnemo = STCSR) and
           (Address(1 downto 0) /= "00") ) or
         ( (EX.Mnemo = STH or EX.Mnemo = STHA) and (Address(0) /= '0') ) or
         ( (EX.Mnemo = I_STD or EX.Mnemo = STDA or EX.Mnemo = STDF or
            EX.Mnemo = STDFQ or EX.Mnemo = STDC or EX.Mnemo = STDCQ) and
           (Address(2 downto 0) /= "000") ) ) then
      TrapVector(DETECTED_TRAP) := TRUE;
      TrapVector(MEM_ADDR_NOT_ALIGNED) := TRUE;
    end if;
    
  end StoreInstruction; -- procedure
  
  -------------------------------------
  procedure LogicalInstruction(EX         : Instruction;
                               CWP        : std_logic_vector;
                               ResultOPcc : std_logic_vector;
                               iccTemp    : std_logic_vector;
                               RegFile    : inout RegisterFile;
                               icc        : out std_logic_vector) is
    variable rd, rs1, rs2 : natural;
    variable Operand2 : std_logic_vector(31 downto 0);
  begin
    rd  := GetIndex(EX.rd, CWP);
    rs1 := GetIndex(EX.rs1, CWP);
    
    if EX.i = 0 then
      rs2 := GetIndex(EX.rs2, CWP);
      Operand2 := RegFile(rs2);
    else
      Operand2 := Extend(ToStdLogicVector(EX.simm13, 13), 32);
    end if;
    
    case EX.Mnemo is
      when I_AND  =>
        if rd /= 0 then RegFile(rd) := RegFile(rs1) and Operand2;
        end if;
        
      when ANDN   =>
        if rd /= 0 then RegFile(rd) := RegFile(rs1) and (not(Operand2));
        end if;

      when I_OR   =>
        if rd /= 0 then RegFile(rd) := RegFile(rs1) or Operand2;
        end if;
        
      when ORN    =>
        if rd /= 0 then RegFile(rd) := RegFile(rs1) or (not(Operand2));
        end if;
        
      when I_XOR  =>
        if rd /= 0 then RegFile(rd) := RegFile(rs1) xor Operand2;
        end if;
        
      when XNOR   =>
        if rd /= 0 then RegFile(rd) := RegFile(rs1) xor (not(Operand2));
        end if;
        
      when ANDcc | ANDNcc | ORcc | ORNcc | XORcc | XNORcc =>
        if rd /= 0 then 
          RegFile(rd) := ResultOPcc;
        end if;
        icc := iccTemp;
        
      when others =>
          assert FALSE report "(LogicalInstruction): error in VHDL model!"
                       severity error;
      
    end case; -- EX.Mnemo
  end LogicalInstruction; -- procedure
  
  -------------------------------------
  procedure AddSubInstruction(EX         : Instruction;
                              CWP        : std_logic_vector;
                              ResultOPcc : std_logic_vector;
                              iccTemp    : std_logic_vector;
                              RegFile    : inout RegisterFile;
                              icc        : inout std_logic_vector) is
    variable rd, rs1, rs2 : natural;
    variable Result, Operand2 : std_logic_vector(31 downto 0);
  begin
    rd  := GetIndex(EX.rd, CWP);
    rs1 := GetIndex(EX.rs1, CWP);
    
    if EX.i = 0 then
      rs2 := GetIndex(EX.rs2, CWP);
      Operand2 := RegFile(rs2);
    else
      Operand2 := Extend(ToStdLogicVector(EX.simm13, 13), 32);
    end if;
    
    case EX.Mnemo is
      when ADD  =>
        Result := RegFile(rs1) + Operand2;
        if rd /= 0 then RegFile(rd) := Result;
        end if;
        
      when ADDX =>
        Result := RegFile(rs1) + Operand2;
        Result := Result + Extend('0' & icc(C_ICC), 32, '0');
        if rd /= 0 then RegFile(rd) := Result;
        end if;
        
      when SUB  =>
        Result := RegFile(rs1) - Operand2;
        if rd /= 0 then RegFile(rd) := Result;
        end if;
        
      when SUBX =>
        Result := RegFile(rs1) - Operand2;
        Result := Result - Extend('0' & icc(C_ICC), 32, '0');
        if rd /= 0 then RegFile(rd) := Result;
        end if;
        
      when ADDcc | ADDXcc | SUBcc | SUBXcc =>
         if rd /= 0 then
           RegFile(rd) := ResultOPcc;  -- Result and icc were calculated by
         end if;                       -- anticipation and stored in ResultOPcc
                                       -- and iccTemp.
         icc := iccTemp;  -- to be assigned even if rd = 0.             
         
      when others  =>
          assert FALSE report "(AddSubInstruction): error in VHDL model!"
                       severity error;
    end case; -- EX.Mnemo 
  end AddSubInstruction; -- procedure
    
  -------------------------------------
  procedure ShiftInstruction(EX         : Instruction;
                             CWP        : std_logic_vector;
                             RegFile    : inout RegisterFile) is
    variable ShiftCount, rs1, rs2, rd : natural;
  begin
    if EX.rd = 0 then return; -- Do nothing if rd is the global reg. %g0.
    end if;
    
    rd := GetIndex(EX.rd, CWP);
    rs1 := GetIndex(EX.rs1, CWP);
    
    if EX.i = 0 then
      rs2 := GetIndex(EX.rs2, CWP);
      ShiftCount := ToNatural(RegFile(rs2)(4 downto 0));
    else
      ShiftCount := EX.rs2; -- If i = 1, shift count = value of rs2.
    end if;
        
    case EX.Mnemo is
      when SLL =>
        RegFile(rd) := ShiftL(RegFile(rs1), ShiftCount, '0'); -- Logical shift.
        
      when SRL =>
        RegFile(rd) := ShiftR(RegFile(rs1), ShiftCount, '0'); -- Logical shift.
        
      when SRA =>
        RegFile(rd) := ShiftR(RegFile(rs1), ShiftCount);-- Arithmetic shft.
        
      when others =>
        assert FALSE report "(ShiftInstruction): error in VHDL model!"
                     severity error;
                     
    end case; -- EX.Mnemo
    
  end ShiftInstruction; -- procedure
  
  -------------------------------------
  procedure TaggedAddSubbInst(EX         : Instruction;
                              CWP        : std_logic_vector;
                              ResultOPcc : std_logic_vector;
                              iccTemp    : std_logic_vector;
                              icc        : out std_logic_vector;
                              RegFile    : inout RegisterFile; --gd
--                              RegFile    : out RegisterFile;
                              TrapVector : inout TrapVectorType) is
    variable rd : natural;
  begin
    if (EX.Mnemo /= TADDcc and EX.Mnemo /= TSUBcc and EX.Mnemo /= TADDccTV
        and EX.Mnemo /= TSUBccTV) then
      assert FALSE report "(TaggedAddSubbInst): error in VHDL model!"
                   severity error;
    end if;
    
    if ( (EX.Mnemo = TADDccTV or EX.Mnemo = TSUBccTV) and 
         iccTemp(V_ICC) = '1' ) then
      TrapVector(DETECTED_TRAP) := TRUE;
      TrapVector(TAG_OVERFLOW) := TRUE;
      return;
    end if;
    
    rd := GetIndex(EX.rd, CWP);
    if rd /= 0 then
      RegFile(rd) := ResultOPcc;
    end if;
    
    icc := iccTemp; -- even if rd = 0.
   
  end TaggedAddSubbInst; -- procedure
  
  -------------------------------------
  procedure MultiplyStepInst(EX         : Instruction;
                             CWP        : std_logic_vector;
                             ResultOPcc : std_logic_vector;
                             iccTemp    : std_logic_vector;
                             YTemp      : std_logic_vector;
                             Y          : out std_logic_vector;
                             icc        : out std_logic_vector;
                             RegFile    : inout RegisterFile) is
--                             RegFile    : out RegisterFile) is
    variable rd : natural;
  begin
    icc := iccTemp;
    Y   := YTemp;
    
    rd := GetIndex(EX.rd, CWP);
    
    if rd /= 0 then
      RegFile(rd) := ResultOPcc;
    end if;
    
  end MultiplyStepInst; -- procedure
  
  -------------------------------------
  procedure SaveRestoreInst(EX         : Instruction;
                            WIM        : std_logic_vector;
                            CWP        : inout std_logic_vector;
                            RegFile    : inout RegisterFile;
                            TrapVector : inout TrapVectorType) is
    variable rs2, rs1, rd, newCWP : natural;
    variable Operand2, Result : std_logic_vector(31 downto 0);
  begin
    case EX.Mnemo is
      when SAVE    =>
         newCWP := (ToNatural(CWP) - 1) mod NWINDOWS;
         if WIM(newCWP) = '1' then
           TrapVector(DETECTED_TRAP) := TRUE;
           TrapVector(WINDOW_OVERFLOW) := TRUE;
           return; -- Exit here from procedure.
         end if;
      when RESTORE =>
         newCWP := (ToNatural(CWP) + 1) mod NWINDOWS;
         if WIM(newCWP) = '1' then
           TrapVector(DETECTED_TRAP) := TRUE;
           TrapVector(WINDOW_UNDERFLOW) := TRUE;
           return; -- Exit here from procedure.
         end if;
      when others  =>
        assert FALSE report "(SaveRestoreInst): error in VHDL model!"
                     severity error;      
    end case; -- EX.Mnemo
    
    rs1 := GetIndex(EX.rs1, CWP);
    
    if EX.i = 0 then
      rs2 := GetIndex(EX.rs2, CWP);
      Operand2 := RegFile(rs2);
    else
      Operand2 := Extend(ToStdLogicVector(EX.simm13, 13), 32);
    end if;
    
    Result := RegFile(rs1) + Operand2;
    CWP := ToStdlogicVector(newCWP, 5);
    
    
    rd := GetIndex(EX.rd, CWP); -- Destination in new window.
    if rd /= 0 then
      RegFile(rd) := Result;
    end if;
  end SaveRestoreInst; -- procedure
  
  -------------------------------------
  procedure RettInstruction(EX         : Instruction;
                            RegFile    : RegisterFile;
                            WIM        : std_logic_vector;
                            PSR        : inout std_logic_vector;
                            Mode       : inout ModeType;
                            tt         : out std_logic_vector;
                            TrapVector : inout TrapVectorType) is
    variable rs2, rs1, newCWP : natural;
    variable Address : std_logic_vector(31 downto 0);
    alias S   : std_logic is PSR(7);
    alias PS  : std_logic is PSR(6);
    alias ET  : std_logic is PSR(5);
    alias CWP : std_logic_vector(4 downto 0) is PSR(4 downto 0);
  begin
    rs1 := GetIndex(EX.rs1, CWP);
    
    newCWP := (ToNatural(CWP) + 1) mod NWINDOWS;
    
    if EX.i = 0 then
      rs2 := GetIndex(EX.rs2, CWP);
      Address := RegFile(rs1) + RegFile(rs2);
    else
      Address := RegFile(rs1) + Extend(ToStdLogicVector(EX.simm13, 13), 32);
    end if;
    
    if ET = '1' then
      TrapVector(DETECTED_TRAP) := TRUE;
      if S = '0' then 
        TrapVector(PRIVILEGED_INST) := TRUE;
      else
        TrapVector(ILLEGAL_INST) := TRUE;
      end if;
      
    elsif S = '0' then
      TrapVector(DETECTED_TRAP) := TRUE;
      Mode := ERROR_MODE;
      tt := TrapTypeTable(PRIVILEGED_INST);
      
    elsif WIM(newCWP) = '1' then
      TrapVector(DETECTED_TRAP) := TRUE;
      Mode := ERROR_MODE;
      tt := TrapTypeTable(WINDOW_UNDERFLOW);
      
    elsif Address(1 downto 0) /= "00" then
      TrapVector(DETECTED_TRAP) := TRUE;
      Mode := ERROR_MODE;
      tt := TrapTypeTable(MEM_ADDR_NOT_ALIGNED);
      
    else
      ET  := '1';
      CWP := ToStdlogicVector(newCWP, 5);
      S   := PS;
    end if;
  end RettInstruction; -- procedure
  
  -------------------------------------
  procedure JmplInstruction(EX         : Instruction;
                            CWP        : std_logic_vector;
                            RegFile    : inout RegisterFile;
                            TrapVector : inout TrapVectorType) is
    variable rs1, rs2, rd : natural;
    variable JumpAddress  : std_logic_vector(31 downto 0);
  begin
    rs1 := GetIndex(EX.rs1, CWP);
    rd  := GetIndex(EX.rd, CWP);
    
    if EX.i = 0 then
      rs2 := GetIndex(EX.rs2, CWP);
      JumpAddress := RegFile(rs1) + RegFile(rs2);
    else
      JumpAddress := RegFile(rs1) + Extend(ToStdLogicVector(EX.simm13, 13), 32);
    end if;
    
    if JumpAddress(1 downto 0) /= "00" then
      TrapVector(DETECTED_TRAP) := TRUE;
      TrapVector(MEM_ADDR_NOT_ALIGNED) := TRUE;
    elsif rd /= 0 then
      RegFile(rd) := EX.Address;
    end if;
  end JmplInstruction; -- procedure
  
  -------------------------------------
  procedure JmplRettAddrCalc(ID          : Instruction;
                             CWP         : std_logic_vector;
                             RegFile     : RegisterFile;
                             CurrentAddr : out std_logic_vector) is
    variable rs1, rs2 : natural;
  begin
    rs1 := GetIndex(ID.rs1, CWP);

    if ID.i = 0 then
      rs2 := GetIndex(ID.rs2, CWP);
      CurrentAddr := RegFile(rs1) + RegFile(rs2);
    else
      CurrentAddr := RegFile(rs1) + Extend(ToStdLogicVector(ID.simm13, 13), 32);
    end if;
    
  end JmplRettAddrCalc; -- procedure
  -------------------------------------
  procedure ReadStateRegInst(EX         : Instruction;
                             PSR        : std_logic_vector;
                             TBR        : std_logic_vector;
                             WIM        : std_logic_vector;
                             Y          : std_logic_vector;
                             TrapVector : inout TrapVectorType;
 --                            RegFile    : out RegisterFile) is
                             RegFile    : inout RegisterFile) is
    alias S   : std_logic is PSR(7);
    alias CWP : std_logic_vector(4 downto 0) is PSR(4 downto 0);
    variable rd : natural;
  begin
    rd := GetIndex(EX.rd, CWP);
    if ((EX.Mnemo = RDPSR or EX.Mnemo = RDWIM or EX.Mnemo = RDTBR) and
         S = '0') then
      TrapVector(DETECTED_TRAP) := TRUE;
      TrapVector(PRIVILEGED_INST) := TRUE;      
    elsif (rd /= 0) then
      case EX.Mnemo is
        when RDPSR  => RegFile(rd) := PSR;
        when RDTBR  => RegFile(rd) := TBR;
        when RDWIM  => RegFile(rd) := WIM;
        when RDY    => RegFile(rd) := Y;
        when others => 
          assert FALSE report "(ReadStateRegInst): error in VHDL model!"
                       severity error;
      end case; -- EX.Mnemo
    end if;
  end ReadStateRegInst; -- procedure
  
  -------------------------------------
  procedure WriteStateRegInst(EX         : Instruction;
                              RegFile    : RegisterFile;
                              PSR        : inout std_logic_vector;
                              iccTemp    : out std_logic_vector;
                              TBR        : inout std_logic_vector;
--                              TBR        : out std_logic_vector;
                              WIM        : inout std_logic_vector;
--                              WIM        : out std_logic_vector;
                              Y          : inout std_logic_vector;
--                              Y          : out std_logic_vector;
                              TrapVector : inout TrapVectorType) is
    alias S    : std_logic is PSR(7);
    alias icc  : std_logic_vector(3 downto 0) is PSR(23 downto 20);    
    alias CWP  : std_logic_vector(4 downto 0) is PSR(4 downto 0);
    alias impl : std_logic_vector(3 downto 0) is PSR(31 downto 28);
    alias ver  : std_logic_vector(3 downto 0) is PSR(27 downto 24);

    variable rs1, rs2, Index : natural;
    variable Result, Operand2 : std_logic_vector(31 downto 0);
  begin
    rs1 := GetIndex(EX.rs1, CWP);
        
    if EX.i = 0 then
      rs2 := GetIndex(EX.rs2, CWP);
      Operand2 := RegFile(rs2);
    else
      Operand2 := Extend(ToStdLogicVector(EX.simm13, 13), 32);
    end if;
    Result := RegFile(rs1) xor Operand2;
    
    case EX.Mnemo is     -- Modelized as non-delayed instructions.
      when WRY    => Y := Result;
      
      when WRPSR  =>
        if S = '0' then
          TrapVector(DETECTED_TRAP) := TRUE;
          TrapVector(PRIVILEGED_INST) := TRUE;
        elsif (ToNatural(Result(4 downto 0)) >= NWINDOWS) then
          TrapVector(DETECTED_TRAP) := TRUE;
          TrapVector(ILLEGAL_INST) := TRUE;
        else
          PSR  := Result;
          iccTemp := icc; -- for Bicc instructions (iccTemp is tested 
                          -- and not icc)
          impl := PSR_IMPL; -- impl & ver fields of the PSR are not affected
          ver  := PSR_VER;  -- by a WRPSR instruction.
        end if;
        
      when WRWIM  =>
        if S = '0' then
          TrapVector(DETECTED_TRAP) := TRUE;
          TrapVector(PRIVILEGED_INST) := TRUE;
        else
          Index := NWINDOWS - 1; -- only bits of implemented windows assigned.
          WIM(Index downto 0) := Result(Index downto 0);
        end if;
        
      when WRTBR  =>
        if S = '0' then
          TrapVector(DETECTED_TRAP) := TRUE;
          TrapVector(PRIVILEGED_INST) := TRUE;
        else
          TBR(31 downto 12) := Result(31 downto 12);
        end if;

      when others =>
        assert FALSE report "(WriteStateRegInst): error in VHDL model!"
                     severity error;
    end case; -- EX.Mnemo
  end WriteStateRegInst; -- procedure
  
  -------------------------------------
  procedure LoadStoreSwapInstruction(EX         : Instruction;
                                     S          : std_logic;
                                     CWP        : std_logic_vector;
                                     RegFile    : RegisterFile;
                                     TrapVector : inout TrapVectorType) is
    variable Address : std_logic_vector(31 downto 0);
    variable rs1, rs2 : natural;
  begin
    if (EX.Mnemo = LDSTUBA or EX.Mnemo = SWAPA) then
      -- WARNING: here ILLEGAL_INST trap priority > PRIVILEGED_INST trap
      if EX.i = 1 then
        TrapVector(DETECTED_TRAP) := TRUE;
        TrapVector(ILLEGAL_INST) := TRUE;
        return; -- here exit from procedure
      end if;
      
      if S = '0' then
        TrapVector(DETECTED_TRAP) := TRUE;
        TrapVector(PRIVILEGED_INST) := TRUE;
        return; -- here exit from procedure
      end if;
    end if;
    
    if (EX.Mnemo = SWAP or EX.Mnemo = SWAPA) then
      rs1 := GetIndex(EX.rs1, CWP);
      if EX.Mnemo = SWAP then
        if EX.i = 0 then
          rs2 := GetIndex(EX.rs2, CWP);
          Address := RegFile(rs1) + RegFile(rs2);
        else
          Address := Extend(ToStdLogicVector(EX.simm13, 13), 32);
          Address := RegFile(rs1) + Address;
        end if;
      elsif EX.Mnemo = SWAPA then
        rs2 := GetIndex(EX.rs2, CWP);
        Address := RegFile(rs1) + RegFile(rs2);
      end if;
      
      if Address(1 downto 0) /= "00" then
        TrapVector(DETECTED_TRAP) := TRUE;
        TrapVector(MEM_ADDR_NOT_ALIGNED) := TRUE;
      end if;
    end if;
    
  end LoadStoreSwapInstruction; -- procedure
  
  -------------------------------------
  procedure TiccInstruction(EX         : Instruction;
                            CWP        : std_logic_vector;
                            icc        : std_logic_vector;
                            RegFile    : RegisterFile;
                            tt         : out std_logic_vector;
                            TrapVector : inout TrapVectorType) is
   variable rs1, rs2 : natural;
   variable Result : std_logic_vector(31 downto 0);
  begin
    if TiccEval(EX.Mnemo, icc) then
      rs1 := GetIndex(EX.rs1, CWP);
      if EX.i = 0 then
        rs2 := GetIndex(EX.rs2, CWP);
        Result := RegFile(rs1) + RegFile(rs2);
      else
        Result := RegFile(rs1) + Extend(ToStdLogicVector(EX.simm13, 13), 32);
      end if;
      
      tt(6 downto 0) := Result(6 downto 0);
      tt(7) := '1';
      TrapVector(DETECTED_TRAP) := TRUE;
      TrapVector(TRAP_INST) := TRUE;
    end if;
  end TiccInstruction; -- procedure
  
  -------------------------------------
  procedure ExecuteOPcc(ID         : Instruction;
                        RegFile    : RegisterFile;
                        CWP        : std_logic_vector;
                        icc        : std_logic_vector;
                        Y          : std_logic_vector;
                        ResultOPcc : out std_logic_vector;
                        iccTemp    : out std_logic_vector;
                        YTemp      : out std_logic_vector) is
    variable rs1, rs2 : natural;
    variable TempRes, Operand1, Operand2 : std_logic_vector(31 downto 0);
    variable YaTmpVar : std_logic; -- Yet Another TeMp VARiable.
  begin
    rs1 := GetIndex(ID.rs1, CWP);
    
    if ID.i = 0 then
      rs2 := GetIndex(ID.rs2, CWP);
      Operand2 := RegFile(rs2);
    else
      Operand2 := Extend(ToStdLogicVector(ID.simm13, 13), 32);
    end if;
    
    case ID.Mnemo is
      when ADDcc | ADDXcc  =>
        TempRes := RegFile(rs1) + Operand2;
        if ID.Mnemo = ADDXcc then
          TempRes := TempRes + Extend('0' & icc(C_ICC), 32, '0');
        end if;
         
        iccTemp(V_ICC) := ( RegFile(rs1)(31) and Operand2(31) and
                            (not(TempRes(31))) ) or ( (not(RegFile(rs1)(31)))
                            and (not(Operand2(31))) and TempRes(31) );
        iccTemp(C_ICC) := ( RegFile(rs1)(31) and Operand2(31) ) or
                          ( (not(TempRes(31))) and ( RegFile(rs1)(31) or 
                            Operand2(31) ) );
        
      when SUBcc | SUBXcc  =>
        TempRes := RegFile(rs1) - Operand2;
        if ID.Mnemo = SUBXcc then
          TempRes := TempRes - Extend('0' & icc(C_ICC), 32, '0');
        end if;
        
        iccTemp(V_ICC) := ( RegFile(rs1)(31) and (not(Operand2(31))) and
                            (not(TempRes(31))) ) or ( (not(RegFile(rs1)(31)))
                            and Operand2(31) and TempRes(31) );
        iccTemp(C_ICC) := ( (not(RegFile(rs1)(31))) and Operand2(31) ) or
                          ( TempRes(31) and ( (not(RegFile(rs1)(31))) or 
                            Operand2(31) ) );

      when ANDcc  =>
        TempRes := RegFile(rs1) and Operand2;
        iccTemp(V_ICC) := '0';
        iccTemp(C_ICC) := '0';
        
      when ORcc   =>
        TempRes := RegFile(rs1) or Operand2;
        iccTemp(V_ICC) := '0';
        iccTemp(C_ICC) := '0';
        
      when XORcc  =>
        TempRes := RegFile(rs1) xor Operand2;
        iccTemp(V_ICC) := '0';
        iccTemp(C_ICC) := '0';
        
      when ANDNcc =>
        TempRes := RegFile(rs1) and (not(Operand2));
        iccTemp(V_ICC) := '0';
        iccTemp(C_ICC) := '0';
        
      when ORNcc  =>
        TempRes := RegFile(rs1) or (not(Operand2));
        iccTemp(V_ICC) := '0';
        iccTemp(C_ICC) := '0';
        
      when XNORcc =>
        TempRes := RegFile(rs1) xor (not(Operand2));
        iccTemp(V_ICC) := '0';
        iccTemp(C_ICC) := '0';
        
      when MULScc =>
        Operand1 := (icc(N_ICC) xor icc(V_ICC)) & RegFile(rs1)(31 downto 1);
        if Y(0) = '0' then Operand2 := (others => '0');
        end if;
        TempRes := Operand1 + Operand2;
        YTemp := RegFile(rs1)(0) & Y(31 downto 1);
        
        iccTemp(V_ICC) := ( Operand1(31) and Operand2(31) and 
                            (not(TempRes(31))) ) or ( (not(Operand1(31))) and
                            (not(Operand2(31))) and TempRes(31) );

        iccTemp(C_ICC) := ( Operand1(31) and Operand2(31) ) or 
                          ( (not(TempRes(31))) and ( Operand1(31) or
                            Operand2(31) ) );
        
      when TADDcc | TADDccTV  =>
        TempRes := RegFile(rs1) + Operand2;
        
        if (RegFile(rs1)(1 downto 0) /= "00" or 
            Operand2(1 downto 0) /= "00") then YaTmpVar := '1';
        else YaTmpVar := '0';
        end if;
        iccTemp(V_ICC) := ( RegFile(rs1)(31) and Operand2(31) and
                            (not(TempRes(31))) ) or ( (not(RegFile(rs1)(31)))
                            and (not(Operand2(31))) and TempRes(31) ) or
                            YaTmpVar;
        iccTemp(C_ICC) := ( RegFile(rs1)(31) and Operand2(31) ) or
                          ( (not(TempRes(31))) and ( RegFile(rs1)(31) or
                            Operand2(31) ) );

      when TSUBcc | TSUBccTV  =>
        TempRes := RegFile(rs1) - Operand2;
        
        if (RegFile(rs1)(1 downto 0) /= "00" or 
            Operand2(1 downto 0) /= "00") then YaTmpVar := '1';
        else YaTmpVar := '0';
        end if;
        iccTemp(V_ICC) := ( RegFile(rs1)(31) and (not(Operand2(31))) and
                            (not(TempRes(31))) ) or ( (not(RegFile(rs1)(31)))
                            and Operand2(31) and TempRes(31) ) or YaTmpVar;
        iccTemp(C_ICC) := ( (not(RegFile(rs1)(31))) and Operand2(31) ) or
                          ( TempRes(31) and ( (not(RegFile(rs1)(31))) or
                            Operand2(31) ) );

      when UMULcc | SMULcc | UDIVcc | SDIVcc =>
        NULL;  -- SPARC v.8
        
      when others  =>
        assert FALSE report "(ExecuteOPcc): error in VHDL model!"
                     severity error;
    end case; -- ID.Mnemo
    
    ResultOPcc := TempRes;
    
    iccTemp(N_ICC) := TempRes(31);
    if TempRes = "00000000000000000000000000000000" then
      iccTemp(Z_ICC) := '1';
    else
      iccTemp(Z_ICC) := '0';
    end if;
    
  end ExecuteOPcc; -- procedure
                        
  -------------------------------------
  procedure ExecuteBicc(iccTemp     : std_logic_vector;
                        icc         : std_logic_vector;
                        nID         : inout Instruction;
                        CurrentAddr : inout std_logic_vector;
                        TakenBr     : out boolean) is
    variable TempVector : std_logic_vector(21 downto 0);
  begin
    nID.NextAddress := nID.Address + 4; -- nominal case.
    
    if iccEvaluation(nID.Mnemo, iccTemp) = TRUE then
      TakenBr := TRUE;
      TempVector := Extend(ToStdLogicVector(nID.disp22), 22);
      CurrentAddr := nID.Address + Extend(TempVector & "00", 32);
      
      if (nID.Mnemo = BA and nID.a = 1) then
        nID.Annul := TRUE;
        nID.NextAddress := CurrentAddr; -- annulled and always taken branch.
      end if;
      
    else
      TakenBr := FALSE;
      if nID.a = 1 then
        nID.Annul := TRUE;
        nID.NextAddress := nID.Address + 8; -- annulled and untaken branch.
      end if;
      
    end if;
  end ExecuteBicc; -- procedure
  
  -------------------------------------
  procedure ExecuteFBfcc(signal fcc  : std_logic_vector;
                         nID         : inout Instruction;
                         CurrentAddr : inout std_logic_vector;
                         TakenBr     : out boolean) is
    variable TempVector : std_logic_vector(21 downto 0);
  begin
    nID.NextAddress := nID.Address + 4; -- nominal case.
    
    if fccEvaluation(nID.Mnemo, fcc) = TRUE then
      TakenBr := TRUE;
      TempVector := Extend(ToStdLogicVector(nID.disp22), 22);
      CurrentAddr := nID.Address + Extend(TempVector & "00", 32);
      
      if (nID.Mnemo = FBA and nID.a = 1) then
        nID.Annul := TRUE;
        nID.NextAddress := CurrentAddr; -- annulled and always taken branch.
      end if;
      
    else
      TakenBr := FALSE;
      if nID.a = 1 then
        nID.Annul := TRUE;
        nID.NextAddress := nID.Address + 8; -- annulled and untaken branch.
      end if;
      
    end if;
  end ExecuteFBfcc; -- procedure
  
  -------------------------------------
  procedure PutInBufferQueue(nID         : Instruction;
                             Buf1IsValid : inout boolean;
                             InstBuffer1 : inout Instruction;
                             Buf2IsValid : inout boolean;
                             InstBuffer2 : inout Instruction) is
  begin
    if Buf1IsValid then
    
      if Buf2IsValid then
        assert FALSE report "(PutInBufferQueue): the FIFO with buffered " &
                            "instructions is FULL. Error in VHDL model!"
                     severity error;
      else
        InstBuffer2 := nID;
        Buf2IsValid := TRUE;
      end if;
      
    else -- case of an empty FIFO
    
      InstBuffer1 := nID;
      Buf1IsValid := TRUE;
      
    end if;
  end PutInBufferQueue; -- procedure
  
  -------------------------------------
  procedure GetFromBufferQueue(Buf1IsValid : inout boolean;
                               InstBuffer1 : inout Instruction;
                               Buf2IsValid : inout boolean;
                               InstBuffer2 : inout Instruction;
                               nIDTemp     : out Instruction) is
  begin
    if Buf1IsValid then

      nIDTemp := InstBuffer1;
      if Buf2IsValid then
        Buf2IsValid := FALSE;
        InstBuffer1 := InstBuffer2;
      else
        Buf1IsValid := FALSE;
      end if;
      
    else -- case of an empty FIFO
    
      assert FALSE report "(GetFromBufferQueue): the FIFO with buffered  " &
                          "instructions is EMPTY. Error in VHDL model!"
                   severity error; 
    end if;
  end GetFromBufferQueue; -- procedure

  -------------------------------------  
  procedure IOPscheduling(InstBuffer1 : Instruction;
                          nID         : Instruction;
                          signal ID   : Instruction;
                          signal EX   : Instruction;
                          signal WR   : Instruction;
                          IOPcase     : out boolean) is
    variable EXvar, WRvar : Instruction;
    variable WRrd_even : natural;
  begin
    EXvar := EX;
    WRvar := WR;
    IOPcase := FALSE;
        
    if
    (
      (ID.Mnemo = JMPL or ID.Mnemo = RETT) or
      
 --     (EX.Mnemo = JMPL and IURegDependency(EXvar.rd, InstBuffer1)) or
       
      IsLoadInst(ID.Mnemo) or 
      
      (IsLoadSingleInst(EX.Mnemo) and not(IsLoadFP_CPInst(EX.Mnemo)) and 
       IURegDependency(EXvar.rd, InstBuffer1)) or
       
      IsLoadDoubleInst(EX.Mnemo) or
      
      (IsLoadDoubleInst(WR.Mnemo) and not(IsLoadFP_CPInst(WR.Mnemo)) and 
       EX.mnemo /= ANNULLED and -- case of no IRL on what is in WR.
 --      (IURegDependency(WRvar.rd, InstBuffer1) or 
       IURegDependency(WRvar.rd + 1, InstBuffer1) ) or
       
 --     (ID.Mnemo = CALL and IURegDependency(15, nID)) or
  
      IsStoreInst(ID.Mnemo) or IsStoreInst(EX.Mnemo) or
      
      (IsStoreDoubleInst(WR.Mnemo) and EX.Mnemo /= ANNULLED) or
      
      (ID.Mnemo = LDSTUB or ID.Mnemo = LDSTUBA or ID.Mnemo = SWAP or
       ID.Mnemo = SWAPA) or
       
      (EX.Mnemo = LDSTUB or EX.Mnemo = LDSTUBA or EX.Mnemo = SWAP or
       EX.Mnemo = SWAPA) or
       
      (WR.Mnemo = LDSTUB or WR.Mnemo = LDSTUBA or WR.Mnemo = SWAP or
       WR.Mnemo = SWAPA)
    ) then
      IOPcase := TRUE;
    end if;
    
  end IOPscheduling; -- procedure
  
  -------------------------------------  
  procedure DataFetchForLoadAndLdstInst(
                          CurrentAddr     : std_logic_vector;
                          signal D        : std_logic_vector;
                          CWP             : std_logic_vector;
                          signal WR       : Instruction;
                          WR1             : Instruction;
                          RegFile         : inout RegisterFile;
                          SwapData        : out std_logic_vector) is
    variable rd : natural;
    variable ByteData : std_logic_vector(7 downto 0);
    variable HalfwordData : std_logic_vector(15 downto 0);
    variable WordData : std_logic_vector(31 downto 0);
    variable LocalCurrentAddr : std_logic_vector(31 downto 0) := 
                                CurrentAddr(31 downto 0);
  begin
    if ((IsLoadInst(WR.Mnemo) and not(IsLoadFP_CPInst(WR.Mnemo))) or 
       (WR.Mnemo = LDSTUB or WR.Mnemo = LDSTUBA) or
       (WR.Mnemo = SWAP or WR.Mnemo = SWAPA)
       ) then
      if (IsLoadByteInst(WR.Mnemo) or
         (WR.Mnemo = LDSTUB or WR.Mnemo = LDSTUBA)
         ) then
        case LocalCurrentAddr(1 downto 0) is
           when "00" => ByteData := D(31 downto 24);
           when "01" => ByteData := D(23 downto 16);
           when "10" => ByteData := D(15 downto 8);
           when "11" => ByteData := D(7 downto 0);
           when others => NULL;
        end case; -- LocalCurrentAddr(1 downto 0)
        
        if (WR.Mnemo = LDSB or WR.Mnemo = LDSBA) then
          WordData := Extend(ByteData, 32); 
        else
          WordData := Extend(ByteData, 32, '0'); -- extend with zeros.
        end if;
            
      elsif IsLoadHalfwordInst(WR.Mnemo) then
        case LocalCurrentAddr(1 downto 0) is
           when "00" => HalfwordData := D(31 downto 16);
           when "10" => HalfwordData := D(15 downto 0);
           when others =>
              assert FALSE report "(DataFetchForLoadAndLdstInst): error in " &
                                  "VHDL model!"
                           severity error;
        end case; -- LocalCurrentAddr(1 downto 0)
        
        if (WR.Mnemo = LDSH or WR.Mnemo = LDSHA) then
          WordData := Extend(HalfwordData, 32); 
        else
          WordData := Extend(HalfwordData, 32, '0'); -- extend with zeros.
        end if;
        
      else
        WordData := D;
        
      end if;

      rd := GetIndex(WR.rd, CWP);
      SwapData := RegFile(rd); -- data object for swap instr.
      if (WR.Mnemo = LDD or WR.Mnemo = LDDA) then
        rd := (rd/2)*2; -- "bit0" of rd is put to 0: register alignment.
        if rd /= 0 then
          RegFile(rd) := WordData;
        end if;
      elsif rd /= 0 then
        RegFile(rd) := WordData;
      end if;
                    
    elsif (WR1.Mnemo = LDD or WR1.Mnemo = LDDA) then -- 2nd data LDD fetched.
      rd := GetIndex(WR1.rd, CWP);
      rd := (rd/2)*2 + 1; -- "bit0" of rd is put to 1: register alignment
      RegFile(rd) := D;
    end if;

  end DataFetchForLoadAndLdstInst;
  

  -------------------------------------  
  procedure DataFetchWhenCacheMiss(Addr     : std_logic_vector;
                                   signal D : std_logic_vector;
                                   CWP      : std_logic_vector;
                                   WR1      : Instruction;
                                   WR2      : Instruction;
                                   RegFile  : inout RegisterFile) is
    variable rd : natural;
    variable ByteData : std_logic_vector(7 downto 0);
    variable HalfwordData : std_logic_vector(15 downto 0);
    variable WordData : std_logic_vector(31 downto 0);
    variable LocalAddr : std_logic_vector(31 downto 0) := Addr(31 downto 0);
  begin
    if ((IsLoadInst(WR1.Mnemo) and not(IsLoadFP_CPInst(WR1.Mnemo))) or 
       (WR1.Mnemo = LDSTUB or WR1.Mnemo = LDSTUBA) or
       (WR1.Mnemo = SWAP or WR1.Mnemo = SWAPA)
       ) then
      if (IsLoadByteInst(WR1.Mnemo) or
         (WR1.Mnemo = LDSTUB or WR1.Mnemo = LDSTUBA)
         ) then
        case LocalAddr(1 downto 0) is
           when "00" => ByteData := D(31 downto 24);
           when "01" => ByteData := D(23 downto 16);
           when "10" => ByteData := D(15 downto 8);
           when "11" => ByteData := D(7 downto 0);
           when others => NULL;
        end case; -- LocalAddr(1 downto 0)
        
        if (WR1.Mnemo = LDSB or WR1.Mnemo = LDSBA) then
          WordData := Extend(ByteData, 32); 
        else
          WordData := Extend(ByteData, 32, '0'); -- extend with zeros.
        end if;
            
      elsif IsLoadHalfwordInst(WR1.Mnemo) then
        case LocalAddr(1 downto 0) is
           when "00" => HalfwordData := D(31 downto 16);
           when "10" => HalfwordData := D(15 downto 0);
           when others =>
              assert FALSE report "(DataFetchWhenCacheMiss): error in " &
                                  "VHDL model!"
                           severity error;
        end case; -- LocalAddr(1 downto 0)
        
        if (WR1.Mnemo = LDSH or WR1.Mnemo = LDSHA) then
          WordData := Extend(HalfwordData, 32); 
        else
          WordData := Extend(HalfwordData, 32, '0'); -- extend with zeros.
        end if;
       
      else 
        WordData := D;
        
      end if;

      rd := GetIndex(WR1.rd, CWP);
      if (WR1.Mnemo = LDD or WR1.Mnemo = LDDA) then
        rd := (rd/2)*2; -- "bit0" of rd is put to 0: register alignment.
        if rd /= 0 then
          RegFile(rd) := WordData;
        end if;
      elsif rd /= 0 then
        RegFile(rd) := WordData;
      end if;
                    
    elsif (WR2.Mnemo = LDD or WR2.Mnemo = LDDA) then -- 2nd data LDD fetched.
      rd := GetIndex(WR2.rd, CWP);
      rd := (rd/2)*2 + 1; -- "bit0" of rd is put to 1: register alignment
      RegFile(rd) := D;
    end if;

  end DataFetchWhenCacheMiss;
  
  -------------------------------------  
  procedure DbusSetupHoldCheck (signal Data    : std_logic_vector;  
                                signal CLK     : std_ulogic;
                                signal XHOLD_N : std_ulogic;
                                constant SETUP, HOLD : time := 0 ns;
                                constant PATH  : string := "";
                                signal DelayedData : std_logic_vector;
                                signal EN_CHECKING : boolean) is
      -- DelayedData must be set to Data'Delayed(abs(HOLD))
    
    variable LastEdge : time := 0 ns;
    variable DeltaT : time;
    variable DeltaTForStupViol : time;
    variable DeltaTForHldViol  : time;
    variable TimeForStupViol : time;
    variable TimeForHldViol  : time;
    variable SetupViol : boolean := FALSE;
    variable HoldViol  : boolean := FALSE;
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
      wait on CLK until rising_edge(CLK);
        -- nothing before first edge of CLK
      LastEdge := now;
      if HOLD >= 0 ns then exit when Data'event or Data'last_event > 0 ns;
      else exit when DelayedData'event or DelayedData'last_event > 0 ns;
      end if;
    end loop SkipInitProblems;

    Infinite : loop
      if CLK'event or Data'event then
        EdgeDetect := rising_edge(CLK);
        
        if EN_CHECKING then
          --------------------
          if SETUP > 0 ns and HOLD >= 0 ns and EdgeDetect then
            DeltaTForStupViol := Data'last_event;
            TimeForStupViol := now;
            if DeltaTForStupViol < SETUP then
              SetupViol := TRUE;
            end if;
          end if;
          if HOLD > 0 ns and SETUP >= 0 ns and Data'event then
            DeltaTForHldViol := now - LastEdge;
            TimeForHldViol := now;
            if DeltaTForHldViol < HOLD then
              HoldViol := TRUE;
            end if;
          end if;
          --------------------
          if SETUP < 0 ns and Data'event then
            DeltaT := now - LastEdge;
            assert not(-SETUP <= DeltaT and DeltaT <= HOLD)
              report "Timing violation on " & PATH & " at time " & ToString(now)
                & ". Observed : " & ToString(DeltaT) & 
                ". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
              severity warning;
          end if;  -- nothing if no data event
          --------------------
          if HOLD < 0 ns and EdgeDetect then
            DeltaT := HOLD - DelayedData'last_event;
            assert not(-SETUP <= DeltaT and DeltaT <= HOLD)
              report "Timing violation on " & PATH & " at time " & ToString(now)
                & ". Observed : " & ToString(DeltaT) & 
                ". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
              severity warning;
          end if;  -- nothing if no edge detected
          --------------------
        end if;
  
        if EdgeDetect then LastEdge := now; end if;
      end if;
    
      if falling_edge(CLK) then
        if XHOLD_N = '1' then -- wait to flag violation
          if SetupViol then
            assert FALSE 
               report "Setup violation on " & PATH & " at time " & 
                      ToString(TimeForStupViol) &
                      ". Observed : " & 
                      ToString(DeltaTForStupViol) & ". Setup : " & 
                      ToString(SETUP)
               severity warning;
          end if;
        
          if HoldViol then
            assert FALSE
               report "Hold violation on " & PATH & " at time " & 
                      ToString(TimeForHldViol) &
                      ". Observed : " & ToString(DeltaTForHldViol) & 
                      ". Hold : " & ToString(HOLD)
               severity warning;
          end if;
        end if;
        SetupViol := FALSE;
        HoldViol := FALSE;
      end if;
      
      wait on CLK, Data;
    end loop Infinite;
  end DbusSetupHoldCheck;  -- procedure

  -------------------------------------  
  procedure DbusSetupHoldCheck (signal Data    : std_ulogic;  
                                signal CLK     : std_ulogic;
                                signal XHOLD_N : std_ulogic;
                                constant SETUP, HOLD : time := 0 ns;
                                constant PATH  : string := "";
                                signal DelayedData : std_ulogic;
                                signal EN_CHECKING : boolean) is
      -- DelayedData must be set to Data'Delayed(abs(HOLD))
    
    variable LastEdge : time := 0 ns;
    variable DeltaT : time;
    variable DeltaTForStupViol : time;
    variable DeltaTForHldViol  : time;
    variable TimeForStupViol : time;
    variable TimeForHldViol  : time;
    variable SetupViol : boolean := FALSE;
    variable HoldViol  : boolean := FALSE;
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
      wait on CLK until rising_edge(CLK);
        -- nothing before first edge of CLK
      LastEdge := now;
      if HOLD >= 0 ns then exit when Data'event or Data'last_event > 0 ns;
      else exit when DelayedData'event or DelayedData'last_event > 0 ns;
      end if;
    end loop SkipInitProblems;

    Infinite : loop
      if CLK'event or Data'event then
        EdgeDetect := rising_edge(CLK);
        
        if EN_CHECKING then
          --------------------
          if SETUP > 0 ns and HOLD >= 0 ns and EdgeDetect then
            DeltaTForStupViol := Data'last_event;
            TimeForStupViol := now;
            if DeltaTForStupViol < SETUP then
              SetupViol := TRUE;
            end if;
          end if;
          if HOLD > 0 ns and SETUP >= 0 ns and Data'event then
            DeltaTForHldViol := now - LastEdge;
            TimeForHldViol := now;
            if DeltaTForHldViol < HOLD then
              HoldViol := TRUE;
            end if;
          end if;
          --------------------
          if SETUP < 0 ns and Data'event then
            DeltaT := now - LastEdge;
            assert not(-SETUP <= DeltaT and DeltaT <= HOLD)
              report "Timing violation on " & PATH & " at time " & ToString(now)
                & ". Observed : " & ToString(DeltaT) & 
                ". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
              severity warning;
          end if;  -- nothing if no data event
          --------------------
          if HOLD < 0 ns and EdgeDetect then
            DeltaT := HOLD - DelayedData'last_event;
            assert not(-SETUP <= DeltaT and DeltaT <= HOLD)
              report "Timing violation on " & PATH & " at time " & ToString(now)
                & ". Observed : " & ToString(DeltaT) & 
                ". Setup : " & ToString(SETUP) & ". Hold : " & ToString(HOLD)
              severity warning;
          end if;  -- nothing if no edge detected
          --------------------
        end if;
  
        if EdgeDetect then LastEdge := now; end if;
      end if;
    
      if falling_edge(CLK) then
        if XHOLD_N = '1' then -- wait to flag violation
          if SetupViol then
            assert FALSE 
               report "Setup violation on " & PATH & " at time " & 
                      ToString(TimeForStupViol) &
                      ". Observed : " & 
                       ToString(DeltaTForStupViol) & ". Setup : " & 
                      ToString(SETUP)
               severity warning;
          end if;
        
          if HoldViol then
            assert FALSE
               report "Hold violation on " & PATH & " at time " & 
                      ToString(TimeForHldViol) &
                      ". Observed : " & ToString(DeltaTForHldViol) & 
                      ". Hold : " & ToString(HOLD)
               severity warning;
          end if;
        end if;
        SetupViol := FALSE;
        HoldViol := FALSE;
      end if;
      
      wait on CLK, Data;
    end loop Infinite;
  end DbusSetupHoldCheck;  -- procedure
  
  -------------------------------------  
  
end SparcPck; -- package body
-------------------------------------------------------------------------------
-- File name : testpck.vhd
-- Title : TestPck
-- project : SPARC 
-- Library : SPARCLIB
-- Author(s) : Maxime ROCCA
-- Purpose : test package for test benches to test and s(t)imulate IURT or 
--           FPURT.
-- notes :   
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        |  MR    | 94-03-04    | first version
-------------------------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
-------------------------------------------------------------------------------
---------|---------|---------|---------|---------|---------|---------|--------|

library IEEE;
use IEEE.Std_Logic_1164.all;
library MMS;
use MMS.StdIoImp.all;
use MMS.StdRtl.all;
use MMS.StdMem.all;
library SPARC_LIB;
use SPARC_LIB.SparcPck.all;


package TestPck is

  ---- Half period of the clock ----
  constant HALF_PERIOD_NB : integer := 25;
  constant HALF_PERIOD : time := HALF_PERIOD_NB*(1 ns);

  ---- constants for modelling purposes ----
  constant STIM_DELAY : time := 30 ns; -- delay for stimuli signals
  
  ---- constants for modelling purposes ----
  constant NB_CAR_MAX : natural := 60;
  type TableOfString is array(natural range <>) of string(1 to NB_CAR_MAX);
  constant MessageTable : TableOfString(0 to 4094); -- Table of messages to 
                                                    -- print on the screen 
                                                    -- during simulation. Coding
                                                    -- on 12 bits => 4096 pos-
                                                    -- -sibilities minus one
                                                    -- which is reserved (all
                                                    -- bits at ONE).
    
  ---- constants for data bus in mode RD (Mem -> Processor)
  constant RD_DAT_OUT_DLY : time := 30 ns; -- data bus output delay
  constant RD_DAT_OUT_VAL : time := 20 ns; -- data bus output valid
  
  ---- Definitions for signal generation.
  type CaseName is (
     VOID, S_CM_A, S_CM_B, S_EXCEP_A, S_EXCEP_B, S_BHOLD_N, S_COE_N,
     S_DOE_N, S_AOE_N, S_IRL, S_RESET_N, S_MHOLDA_N, S_MHOLDB_N, S_HALT_N
  );
  
  type TabParameter is array(CaseName) of integer;
  type TabFlag is array(CaseName) of boolean;
  
  type SignalGenData is record
     Sig    : CaseName;
     Offset : integer;
     Width : integer;
     Delay : integer;
     Value : integer;
     IsIntack : boolean;
  end record; -- SignalGenData

  -----------------------------------------------------------------------------
  -- This function decodes [SETHI value, %g0] instructions to allow the test
  -- bench to generate input signals for the IU.
  -----------------------------------------------------------------------------
  function DecodeSETHI(D : std_logic_vector) return SignalGenData;

  
  -----------------------------------------------------------------------------
  -- Detects a specific range of SETHI to print messages during simulation.
  -- Prints a string message from a table of message whose index is determined 
  -- in Data.
  -----------------------------------------------------------------------------
  procedure PrintMessageWithSETHI(Data : std_logic_vector);
  
                         
  -----------------------------------------------------------------------------
  -- This procedure reads a word from memory. It can also detect memory space
  -- violations (exception case).
  -----------------------------------------------------------------------------
  procedure RDmem(signal D    : out std_logic_vector;
                  signal DPAR : out std_logic;
                  MemAccess   : inout memory_type;
                  Alat        : std_logic_vector;
                  ASIlat      : std_logic_vector;
                  SizeLatched : std_logic_vector;
                  Exception   : out boolean);

  -----------------------------------------------------------------------------
  -- This procedure writes a word to memory. It can also detect memory space
  -- violations (exception case).
  -----------------------------------------------------------------------------
  procedure WRmem(signal D  : std_logic_vector;
                  MemAccess : inout memory_type;
                  Alatched  : std_logic_vector;
                  ASIlat    : std_logic_vector;
                  SizeLat   : std_logic_vector;
                  Exception : out boolean);
                  
  --------------------------------------------------------
  -- Returns a string of characters representing the input
  -- vector Vec in hexadecimal format.
  --------------------------------------------------------
  function Bin2Hexa(Vec : std_logic_vector) return string;

end TestPck; -- package

------------------------------------------------------------------------------

package body TestPck is

  constant MessageTable : TableOfString(0 to 4094) := (
  0    => "@@@@@@@@@@@@@@ END OF TEST PROGRAM REACHED @@@@@@@@@@@@@    ",
  1    => "TEST  1                                                     ",
  2    => "TEST  2                                                     ",
  3    => "TEST  3                                                     ",
  4    => "TEST  4                                                     ",
  5    => "TEST  5                                                     ",
  6    => "TEST  6                                                     ",
  7    => "TEST  7                                                     ",
  8    => "TEST  8                                                     ",
  9    => "TEST  9                                                     ",
  10   => "TEST 10                                                     ",
  11   => "TEST 11                                                     ",
  12   => "TEST 12                                                     ",
  13   => "TEST 13                                                     ",
  14   => "TEST 14                                                     ",
  15   => "TEST 15                                                     ",
  16   => "TEST 16                                                     ",
  17   => "TEST 17                                                     ",
  18   => "TEST 18                                                     ",
  19   => "TEST 19                                                     ",
  20   => "TEST 20                                                     ",
  others => 
  "                                                            "
  );

  -------------------------------------  
  procedure PrintMessageWithSETHI(Data : std_logic_vector) is
    variable Index : natural;
  begin
    if VecUnknown(Data) then return; -- exit here from procedure
    end if;
    
    if ( Data(31 downto 30) = "00" and
         Data(29 downto 25) = "00000" and
         Data(24 downto 22) = "100" and
         Data(21 downto 12) = "1111111111" and
         Data(11 downto 0) /= "111111111111") then
      Index := ToNatural(Data(11 downto 0));
      Print(MessageTable(Index));
    end if;
  end PrintMessageWithSETHI; -- procedure
    
  -------------------------------------  
  procedure RDmem(signal D    : out std_logic_vector;
                  signal DPAR : out std_logic;
                  MemAccess   : inout memory_type;
                  Alat        : std_logic_vector;
                  ASIlat      : std_logic_vector;
                  SizeLatched : std_logic_vector;
                  Exception   : out boolean) is
    
    constant DummyConst : std_logic_vector(31 downto 0) := (others => 'X');
    variable Data       : std_logic_vector(31 downto 0);
    variable ASIvar     : natural := ToNatural(ASIlat);
    variable SizeLat    : std_logic_vector(1 downto 0) 
                          := SizeLatched(1 downto 0);
    variable SizeVar    : natural;
    
  begin
  SizeVar := ToNatural(SizeLat);
  
  -- authorized access checking
    if not( (Alat(31 downto 30) = "00" and ASIvar = SUPERVISOR_INST) or
            (Alat(31 downto 30) = "01" and ASIvar = SUPERVISOR_DATA) or
            (Alat(31 downto 30) = "10" and ASIvar = USER_INST) or
            (Alat(31 downto 30) = "11" and ASIvar = USER_DATA) ) then
            
      Exception := TRUE;
      
    end if;
  
  -- memory access
    MemRead(MemAccess, ToNatural(Extend(Alat, 30)), Data, 4); -- 4-byte access
    
    case Sizevar is
      when WORDTYPE | DOUBLEWORD =>
         D <= DummyConst after RD_DAT_OUT_VAL, Data after RD_DAT_OUT_DLY;
         DPAR <= 'X' after RD_DAT_OUT_VAL,
                 OddParityOf(Data(31 downto 0)) after RD_DAT_OUT_DLY;
      when BYTE     => 
         D(31 downto 24) <= (others => 'X') after RD_DAT_OUT_VAL, 
                            Data(31 downto 24) after RD_DAT_OUT_DLY;
         D(23 downto 16) <= (others => 'X') after RD_DAT_OUT_VAL,
                            Data(31 downto 24) after RD_DAT_OUT_DLY;
         D(15 downto  8) <= (others => 'X') after RD_DAT_OUT_VAL,
                            Data(31 downto 24) after RD_DAT_OUT_DLY;
         D( 7 downto  0) <= (others => 'X') after RD_DAT_OUT_VAL,
                            Data(31 downto 24) after RD_DAT_OUT_DLY;
         DPAR <= 'X' after RD_DAT_OUT_VAL,
                 OddParityOf(Data(31 downto 24) & Data(31 downto 24) & 
                             Data(31 downto 24) & Data(31 downto 24)) 
                             after RD_DAT_OUT_DLY;
      when HALFWORD =>
         D(31 downto 16) <= (others => 'X') after RD_DAT_OUT_VAL,
                            Data(31 downto 16) after RD_DAT_OUT_DLY;
         D(15 downto  0) <= (others => 'X') after RD_DAT_OUT_VAL,
                            Data(31 downto 16) after RD_DAT_OUT_DLY;
         DPAR <= 'X' after RD_DAT_OUT_VAL,
                 OddParityOf(Data(31 downto 24) & Data(31 downto 24) & 
                             Data(31 downto 24) & Data(31 downto 24)) 
                             after RD_DAT_OUT_DLY;
      when others   =>
         assert FALSE report "(RDmem): error in VHDL model!" 
                      severity error;
    end case; -- Sizevar
    
  end RDmem; -- procedure
  
  -------------------------------------  
  procedure WRmem(signal D  : std_logic_vector;
                  MemAccess : inout memory_type;
                  Alatched  : std_logic_vector;
                  ASIlat    : std_logic_vector;
                  SizeLat   : std_logic_vector;
                  Exception : out boolean) is
    variable ASIvar  : natural := ToNatural(ASIlat);
    variable SizeVar : natural := ToNatural(SizeLat);
    variable Alat    : std_logic_vector(31 downto 0) := Alatched(31 downto 0);
  begin

  -- authorized access checking
    if not( (Alat(31 downto 30) = "00" and ASIvar = SUPERVISOR_INST) or
            (Alat(31 downto 30) = "01" and ASIvar = SUPERVISOR_DATA) or
            (Alat(31 downto 30) = "10" and ASIvar = USER_INST) or
            (Alat(31 downto 30) = "11" and ASIvar = USER_DATA) ) then
            
      Exception := TRUE;
      
    end if;
  
  -- memory access
    case SizeVar is
      when WORDTYPE | DOUBLEWORD => 
         MemWrite(MemAccess, ToNatural(Extend(Alat, 30)), D, 4);
         
      when BYTE     => 
         case Alat(1 downto 0) is
           when "00"   =>
              MemWrite(MemAccess, ToNatural(Extend(Alat, 30)), D(31 downto 24));
           when "01"   =>
              MemWrite(MemAccess, ToNatural(Extend(Alat, 30)), D(23 downto 16));
           when "10"   =>
              MemWrite(MemAccess, ToNatural(Extend(Alat, 30)), D(15 downto  8));
           when "11"   =>
              MemWrite(MemAccess, ToNatural(Extend(Alat, 30)), D( 7 downto  0));
           when others => NULL;
         end case; -- Alat(1 downto 0)
         
      when HALFWORD  =>
         case Alat(1 downto 0) is
           when "00"   =>
              MemWrite(MemAccess, ToNatural(Extend(Alat, 30)),
                                  D(31 downto 16), 2);
           when "10"   =>
              MemWrite(MemAccess, ToNatural(Extend(Alat, 30)),
                                  D(15 downto  0), 2);
           when others =>
              assert FALSE 
                     report "(WRmem): wrong value on address bus = " &
                             ToString(Alat) & " !"
                     severity warning;
         end case; -- Alat(1 downto 0)
         
      when others   =>
         assert FALSE report "(WRmem): error in VHDL model!"
                      severity error;
    end case; -- SizeVar
    
  end WRmem; -- procedure

  -------------------------------------  
  function DecodeSETHI(D : std_logic_vector) return SignalGenData is
    variable VarSigData : SignalGenData;
    variable Data : std_logic_vector(31 downto 0) := D(31 downto 0);
  begin
    if (not(VecUnknown(Data)) and 
        Data(31 downto 15) = "00000001001111110") then
      VarSigData.Offset := ToInteger('0' & Data(2 downto 0));
      VarSigData.Width := ToInteger('0' & Data(5 downto 3));
      if Data(14) = '1' then
        VarSigData.Delay := (ToInteger('0' & Data(9 downto 6)) * 
                            2*HALF_PERIOD_NB)/16;
        case Data(13 downto 10) is
          when "0000" =>
            VarSigData.Sig := S_RESET_N;
          when "0001" =>
            VarSigData.Sig := S_BHOLD_N;
          when "0010" =>
            VarSigData.Sig := S_COE_N;
          when "0011" =>
            VarSigData.Sig := S_DOE_N;
          when "0100" =>
            VarSigData.Sig := S_AOE_N;
          when "0101" =>
            VarSigData.Sig := S_HALT_N;
          when "1000" =>
            VarSigData.Sig := S_CM_A;
            VarSigData.Delay := VarSigData.Delay + 4;
            if VarSigData.Width < 3 then
              VarSigData.Width := 3;
            end if;
          when "1001" =>
            VarSigData.Sig := S_EXCEP_A;
            VarSigData.Delay := VarSigData.Delay + 4;
            if VarSigData.Width < 2 then
              VarSigData.Width := 2;
            end if;
          when "1010" =>
            VarSigData.Sig := S_CM_B;
            VarSigData.Delay := VarSigData.Delay + 4;
            if VarSigData.Width < 3 then
              VarSigData.Width := 3;
            end if;
          when "1011" =>
            VarSigData.Sig := S_EXCEP_B;
            VarSigData.Delay := VarSigData.Delay + 4;
            if VarSigData.Width < 2 then
              VarSigData.Width := 2;
            end if;
          when "1100" =>
            VarSigData.Sig := S_MHOLDA_N;
          when "1101" =>
            VarSigData.Sig := S_MHOLDB_N;
          when others => NULL;
        end case; -- Data(13 downto 10)
        return VarSigData;
      else
        VarSigData.Sig := S_IRL;
        VarSigData.Value := ToInteger('0' & Data(13 downto 10));
        VarSigData.Delay := ToInteger('0' & Data(9 downto 7)) * 
                            ((2*HALF_PERIOD_NB)/16);
        if Data(6) = '0' then
          VarSigData.IsIntack := FALSE;
        else
          VarSigData.IsIntack := TRUE;
        end if;
        return VarSigData;
      end if;
    end if;
    
    VarSigData.Sig := VOID;
    return VarSigData;
  end DecodeSETHI;

  --------------------------------
  function Bin2Hexa(Vec : std_logic_vector) return string is
    constant L       : natural := Vec'length;
    alias MyVec      : std_logic_vector(L - 1 downto 0) is Vec;
    constant LVecFul : natural := ((L - 1)/4 + 1)*4;
    variable VecFul  : std_logic_vector(LVecFul - 1 downto 0) 
                                    := (others => '0');
    constant StrLgth : natural := LVecFul/4;
    variable Res     : string(1 to StrLgth) := (others => ' ');
    variable TempVec : std_logic_vector(3 downto 0);
    variable i       : integer := LVecFul - 1;
    variable Index   : natural := 1;
  begin
    assert L > 1 report "(bin2hexa) requires a vector!" severity error;
    
    VecFul(L - 1 downto 0) := MyVec(L -1 downto 0);
    
    while (i - 3 >= 0) loop
      TempVec(3 downto 0) := VecFul(i downto i - 3);
      case TempVec(3 downto 0) is
         when "0000" => Res(Index) := '0';
         when "0001" => Res(Index) := '1';
         when "0010" => Res(Index) := '2';
         when "0011" => Res(Index) := '3';
         when "0100" => Res(Index) := '4';
         when "0101" => Res(Index) := '5';
         when "0110" => Res(Index) := '6';
         when "0111" => Res(Index) := '7';
         when "1000" => Res(Index) := '8';
         when "1001" => Res(Index) := '9';
         when "1010" => Res(Index) := 'a';
         when "1011" => Res(Index) := 'b';
         when "1100" => Res(Index) := 'c';
         when "1101" => Res(Index) := 'd';
         when "1110" => Res(Index) := 'e';
         when "1111" => Res(Index) := 'f';
         when others => Res(Index) := 'x';
      end case; -- TempVec(3 downto 0) 
      Index := Index + 1;
      i := i - 4;
    end loop;
    
    return Res;
    
  end Bin2Hexa;

end TestPck; -- package body
-------------------------------------------------------------------------------
-- File name : tapctrl_iufpu.vhd
-- Title : TAP_iufpu
-- project : SPARC 
-- Library : SPARCLIB
-- Author(s) : Maxime ROCCA
-- Purpose : package containing the TAP controller and related VHDL objects for
--           the iu and the fpu.
--
-- notes : the model of the TAP controller was provided by Saab Ericsson
--         Space. Only minor modifications were performed.
-- 	
-------------------------------------------------------------------------------
-- Modification history :
-------------------------------------------------------------------------------
-- Version No : | Author | Mod. Date : | Changes made :
-------------------------------------------------------------------------------
-- v 1.0        |  MR    | 94-03-04    | first version
--.............................................................................
-- v 1.1        |  MR    | 94-05-03    | 2nd version
-- + change in the entity and file name.
-------------------------------------------------------------------------------
-- Copyright MATRA MARCONI SPACE FRANCE
-------------------------------------------------------------------------------
---------|---------|---------|---------|---------|---------|---------|--------|

library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;


entity TAP_iufpu is
    generic(
      tTDOD : time := 2 ns; -- TDO output delay
      tTDOH : time := 1 ns  -- TDO output valid
    );
    
    port(
      TRst_N : in std_logic;
      TCK    : in std_logic;
      TDI    : in std_logic;
      TMS    : in std_logic;
      TDO    : out std_logic
    );
end TAP_iufpu;

architecture Mini_Spec of TAP_iufpu is

  type TAP_STATE is (RST, IDLE,
    SEL_DR_SCAN, CAPTURE_DR, SHIFT_DR, EXIT1_DR, PAUSE_DR, EXIT2_DR, UPDATE_DR,
    SEL_IR_SCAN, CAPTURE_IR, SHIFT_IR, EXIT1_IR, PAUSE_IR, EXIT2_IR, UPDATE_IR);

  signal State : TAP_STATE;

begin

  TAPControl: process (TRst_N,TCK)

    constant BypassData    : UX01 := '1';
    constant InternalData  : Std_Logic_Vector (0 to 7) := "10101010";

    variable TMSInternal   : UX01;
    variable TDIInternal   : UX01;
    variable IR_Reg        : Std_Logic_Vector (0 to 1);
    variable IR_Shift      : Std_Logic_Vector (0 to 1);
    variable Bypass_Shift  : UX01;
    variable Intern_Shift  : Std_Logic_Vector (0 to 7);
    variable TDOInternal   : UX01;

  begin

    if TRst_N = '0' then   -- Asynchronous reset
      State <= RST;

    elsif Rising_Edge(TCK) then
        TMSInternal := TMS;
        TDIInternal := TDI;

        case State is
----------------------------------------------------------------------------
          when RST =>
            IR_Reg   := "00";   -- Bypass
            IR_Shift := "00";

            if TMSInternal = '1' then
              State <= RST;
            else
              State <= IDLE;
            end if;
----------------------------------------------------------------------------
          when IDLE =>
            if TMSInternal = '1' then
              State <= SEL_DR_SCAN;
            end if;
----------------------------------------------------------------------------
          when SEL_DR_SCAN =>
            if TMSInternal = '1' then
              State <= SEL_IR_SCAN;
            else
              State <= CAPTURE_DR;
            end if;
----------------------------------------------------------------------------
          when CAPTURE_DR =>
            case IR_Reg is
              when "00" => -- Capture bypass register
                Bypass_Shift := BypassData;
                TDOInternal := Bypass_Shift;
              when "01" => -- Capture internal scan
                Intern_Shift := InternalData;
                TDOInternal := Intern_Shift (0);
              when "10" => -- Capture boundary scan is not implemented.
                null;
              when others =>
                null;
            end case;

            if TMSInternal = '1' then
              State <= EXIT1_DR;
            else
              State <= SHIFT_DR;
            end if;
----------------------------------------------------------------------------
          when SHIFT_DR =>
            case IR_Reg is
              when "00" =>                --Bypass
                Bypass_Shift := TDIInternal;
                TDOInternal := Bypass_Shift;
              when "01" =>                --Internal Scan
                Intern_Shift(0 to 1) := Intern_Shift(1 to 2);
                Intern_Shift(2) := TDIInternal;
                TDOInternal := Intern_Shift(0);
              when "10" =>                --Boundary scan is not implemented.
                null;
              when others =>
                null;
            end case;

            if TMSInternal = '1' then
              State <= EXIT1_DR;
            else
              State <= SHIFT_DR;
            end if;
----------------------------------------------------------------------------
          when EXIT1_DR =>
            if TMSInternal = '1' then
              State <= UPDATE_DR;
            else
              State <= PAUSE_DR;
            end if;
----------------------------------------------------------------------------
          when PAUSE_DR => 
            if TMSInternal = '1' then
              State <= EXIT2_DR;
            else
              State <= PAUSE_DR;
            end if;
----------------------------------------------------------------------------
          when EXIT2_DR =>
            if TMSInternal = '1' then
              State <= UPDATE_DR;
            else
              State <= SHIFT_DR;
            end if;
----------------------------------------------------------------------------
          when UPDATE_DR =>
            case IR_Reg is
              when "01" =>   -- Update chip from internal scan register
                null;
              when "10" =>   -- Update chip from boundary scan register is
                null;        -- not implemented.
              when others =>
                null;
            end case;
            if TMSInternal = '1' then
              State <= SEL_DR_SCAN;
            else
              State <= IDLE;
            end if;
----------------------------------------------------------------------------
          when SEL_IR_SCAN =>
            if TMSInternal = '1' then
              State <= RST;
            else
              State <= CAPTURE_IR;
            end if;
----------------------------------------------------------------------------
          when CAPTURE_IR =>
            IR_Shift := IR_Reg;
            if TMSInternal = '1' then
              State <= EXIT1_IR;
            else
              State <= SHIFT_IR;
            end if;
            TDOInternal := IR_Shift (0);
----------------------------------------------------------------------------
          when SHIFT_IR =>
            IR_Shift(0) := IR_Shift(1);
            IR_Shift(1) := TDIInternal;
            if TMSInternal = '1' then
              State <= EXIT1_IR;
            else
              State <= SHIFT_IR;
            end if;
            TDOInternal := IR_Shift (0);
----------------------------------------------------------------------------
          when EXIT1_IR =>
            if TMSInternal = '1' then
              State <= UPDATE_IR;
            else
              State <= PAUSE_IR;
            end if;
----------------------------------------------------------------------------
          when PAUSE_IR =>
            if TMSInternal = '1' then
              State <= EXIT2_IR;
            else
              State <= PAUSE_IR;
            end if;
----------------------------------------------------------------------------
          when EXIT2_IR =>
            if TMSInternal = '1' then
              State <= UPDATE_IR;
            else
              State <= SHIFT_IR;
            end if;
----------------------------------------------------------------------------
          when UPDATE_IR =>
            IR_Reg := IR_Shift;
            if TMSInternal = '1' then
              State <= SEL_DR_SCAN;
            else
              State <= IDLE;
            end if;
        end case;

----------------------------------------------------------------------------
    elsif Falling_Edge(TCK) then
        if State = SHIFT_DR or State = SHIFT_IR then
          TDO <= TDOInternal;
        else
          TDO <= 'X';  --This line is for test purpose only.
        end if;
    end if;
  end process;
end Mini_Spec;

----------------------------------------------------------------------------
-- Package containing the component declaration of TAP
----------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;


package TAPCompPck is 

  component TAP_iufpu
    generic(
      tTDOD : time := 2 ns; -- TDO output delay
      tTDOH : time := 1 ns  -- TDO output valid
    );
    
    port(
      TRst_N : in std_logic;
      TCK    : in std_logic;
      TDI    : in std_logic;
      TMS    : in std_logic;
      TDO    : out std_logic
    );
  end component;

end TAPCompPck;


----------------------------------------------------------------------------
--Test Bench
----------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;

library MMS;
use MMS.StdRTL.all;

entity TAPTest_iufpu is
end TAPTest_iufpu;

architecture Behaviour of TAPTest_iufpu is

    component TAP_iufpu
        port(
             TRst_N : in std_logic;
             TCK    : in std_logic;
             TDI    : in std_logic;
             TMS    : in std_logic;
             TDO    : out std_logic
            );
    end component;

    signal TRst_N : std_logic;
    signal TCK    : std_logic;
    signal TDI    : std_logic;
    signal TMS    : std_logic;
    signal TDO    : std_logic;

begin

    TAP_I: TAP_iufpu
        port map(
                 TRst_N => TRst_N,
                 TCK    => TCK,
                 TDI    => TDI,
                 TMS    => TMS,
                 TDO    => TDO
                );

----------------------------------------------------------------------------
    TCK_Generator: process
    begin
        TCK <= '1', '0' after 50 ns;
        wait for 100 ns;
    end process;

----------------------------------------------------------------------------
    TRst_N_Generator: process
    begin
        TRst_N <= '1', '0' after 10 ns, '1' after 80 ns;
        wait;
    end process;

----------------------------------------------------------------------------
    TestSequence: process                      -- Instruction scan        Data scan               
        constant TMSVector : Std_Logic_Vector := "011000100001000011000000100001000010000110001111111";
        constant TDIVector : Std_Logic_Vector := "XXXXX10XXXXXX0101XXXXXXXXXX101XXXXXX0101XXXXXXXXXXX";
        variable K         : natural;
    begin
        for K in TMSVector'low to TMSVector'high loop
            wait until Falling_Edge(TCK);
            TMS <= TMSVector(K);
            TDI <= TDIVector(K);
        end loop;

        wait;
    end process;

end Behaviour;
