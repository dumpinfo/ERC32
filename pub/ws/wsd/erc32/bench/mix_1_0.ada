 package TYPDEFS is
         type TARGTYPE is (TANK,TRACKED_VEHICLE,MILITARY_VEHICLE,CLUTTER);
         type YESNO is (NO,YES);
         type FLOAT_VECTOR is array (INTEGER range <>) of FLOAT;
 end TYPDEFS;


 package PROGIN is
         PI_CLUSIZ : INTEGER := 8;
         PI_CLREDF : FLOAT := 2000.0;
         PI_CLSIZE : INTEGER := 10;
         PI_CCFOMF : FLOAT := 10.0;
         PI_CODMAX : INTEGER := 10;
         PI_PRLKMX : INTEGER := 15;
         PI_PROTPF : INTEGER := 20;
         PI_PRNTPF : INTEGER := 35;
         PI_PRFOMN : INTEGER := 20;
 end PROGIN;


 package PCCDATA is
         PD_RANGE : FLOAT := 10_000.0;
         PD_RNGRTE : FLOAT := 0.0;
 end PCCDATA;


 with TYPDEFS;
 use TYPDEFS;
 package OTDATA is

         OT_NOTMAX : constant := 100;
         OT_TIME : FLOAT;
         OT_NTARG : INTEGER;
         OT_TREC : FLOAT;

         type OT_SB_ENTRY is record
                 OT_XCENT : INTEGER;
                 OT_YCENT : INTEGER;
                 OT_HEIGHT : INTEGER;
                 OT_WIDTH : INTEGER;
                 OT_FOM : INTEGER;
                 OT_TYPE : TARGTYPE;
                 OT_NLOOKS : INTEGER;
                 OT_COLINF : FLOAT;
                 OT_XCREC : INTEGER;
                 OT_YCREC : INTEGER;
         end record;

         type OT_SB is array (1..OT_NOTMAX) of OT_SB_ENTRY;

         OT_SCRBRD : OT_SB;
         OT_PRLIST : array (1..OT_NOTMAX) of INTEGER;

 end OTDATA;


 with TYPDEFS;
 use TYPDEFS;
 package NTDATA is
         NT_NNTMAX : constant := 100;
         NT_NTARG : INTEGER;
         type NT_SB_ENTRY is record
                 NT_JOBNO : INTEGER;
                 NT_STATUS : INTEGER;
                 NT_XCENT : INTEGER;
                 NT_YCENT : INTEGER;
                 NT_AREA : INTEGER;
                 NT_FOM : INTEGER;
                 NT_TYPE : TARGTYPE;
                 NT_PRTANK : FLOAT;
                 NT_PRTRUK : FLOAT;
                 NT_PRAPC : FLOAT;
                 NT_PRCLTR : FLOAT;
                 NT_MTI : BOOLEAN;
                 NT_COLINF : FLOAT;
                 NT_XCREC : INTEGER;
                 NT_YCREC : INTEGER;
                 NT_FVVALUE : FLOAT_VECTOR(1..20);
         end record;

         type NT_SB is array (1..NT_NNTMAX) of NT_SB_ENTRY;

         NT_SCRBRD : NT_SB;
 end NTDATA;


 package GPM is
         function GPM_MAX(S1 : in INTEGER; S2 :in INTEGER) return INTEGER;
         function GPM_MIN(S1 : in INTEGER; S2 :in INTEGER) return INTEGER;
 end GPM;

 package body GPM is
         function GPM_MAX(S1:in INTEGER; S2:in INTEGER) return INTEGER is
         begin
         if S1 > S2 then
                 return S1;
         else
                 return S2;
         end if;
         end GPM_MAX;

         function GPM_MIN(S1:in INTEGER; S2:in INTEGER) return INTEGER is
         begin
         if S1 < S2 then
                 return S1;
         else
                 return S2;
         end if;
         end GPM_MIN;
 end GPM;


 package EXECDATA is
         EX_PRIME_FREEZE_TIME : FLOAT := 0.0;
         EX_REC_FREEZE_TIME : FLOAT := 0.0;
 end EXECDATA;


 package CONVAR is
         CV_NTPROC : NATURAL;
         CV_TNPROC : array (1..100) of INTEGER;
 end CONVAR;


 with TYPDEFS; use TYPDEFS;
 with NTDATA; use NTDATA;
 with OTDATA; use OTDATA;
 package CAPDTA is
         CA_CLMXNO : constant := 50;
         CA_PRFOMX : constant := 100;
         CA_CLREDD : INTEGER;
         CA_DTPROJ : FLOAT;
         CA_DTPREC : FLOAT;
         CA_CHAIN : array (1..NT_NNTMAX) of INTEGER;
         CA_HASH : array (1..CA_CLMXNO) of INTEGER;
         CA_KNTAOT : array (1..OT_NOTMAX) of INTEGER;
         CA_NOTANT : array (1..NT_NNTMAX) of INTEGER;
         CA_PCHAIN : array (1..OT_NOTMAX) of INTEGER;
         CA_PHASH : array (1..CA_PRFOMX) of INTEGER;
         CA_REDUND : array (1..NT_NNTMAX) of YESNO;
         CA_USEDNT : array (1..NT_NNTMAX) of YESNO;
 end CAPDTA;


 procedure CAP_UPDATE(I1_KOTARG : in NATURAL; I2_KNTARG : in NATURAL);
 with CAPDTA;    use CAPDTA;
 with NTDATA;    use NTDATA;
 with PROGIN;    use PROGIN;
 with OTDATA;    use OTDATA;
 with GPM;       use GPM;

 procedure CAP_UPDATE(I1_KOTARG : in NATURAL; I2_KNTARG : in NATURAL) is
 FOMTOT : NATURAL;
 WOVERH : FLOAT;
 HEIGHT : FLOAT;
 WIDTH : FLOAT;
 function MIN(S1 : in INTEGER; S2 : in INTEGER) return INTEGER
                                                        renames GPM_MIN;

 begin
 -- 1. If the old target number is valid, update its data in the Old
 --      Target Scoreboard.

 if I1_KOTARG > 0 then

         -- 1.1 If the old target has been correlated with a new target,
         --     factor in the new target information.

         if I2_KNTARG > 0 then
                 OT_SCRBRD(I1_KOTARG).OT_XCENT :=
                                        NT_SCRBRD(I2_KNTARG).NT_XCENT;
                 OT_SCRBRD(I1_KOTARG).OT_YCENT :=
                                        NT_SCRBRD(I2_KNTARG).NT_YCENT;
                 HEIGHT := 10.0;
                 OT_SCRBRD(I1_KOTARG).OT_HEIGHT := NATURAL(HEIGHT);
                 WIDTH := 10.0;
                 OT_SCRBRD(I1_KOTARG).OT_WIDTH := NATURAL(WIDTH);
                 FOMTOT := OT_SCRBRD(I1_KOTARG).OT_NLOOKS *
                                 OT_SCRBRD(I1_KOTARG).OT_FOM +
                                 NT_SCRBRD(I2_KNTARG).NT_FOM;
                 OT_SCRBRD(I1_KOTARG).OT_TYPE :=
                                        NT_SCRBRD(I2_KNTARG).NT_TYPE;
                 OT_SCRBRD(I1_KOTARG).OT_NLOOKS :=
                       MIN(OT_SCRBRD(I1_KOTARG).OT_NLOOKS + 1, PI_PRLKMX);
                 OT_SCRBRD(I1_KOTARG).OT_COLINF :=
                       NT_SCRBRD(I2_KNTARG).NT_COLINF;
                 OT_SCRBRD(I1_KOTARG).OT_XCREC :=
                                        NT_SCRBRD(I2_KNTARG).NT_XCREC;
                 OT_SCRBRD(I1_KOTARG).OT_YCREC :=
                                        NT_SCRBRD(I2_KNTARG).NT_YCREC;

 --      1.2 If the old target is uncorrelated, decrease its FOM
 --         accordingly.

         else
                 FOMTOT := OT_SCRBRD(I1_KOTARG).OT_NLOOKS *
                                 OT_SCRBRD(I1_KOTARG).OT_FOM - PI_PROTPF;
         end if;

         OT_SCRBRD(I1_KOTARG).OT_FOM :=
                    MIN(FOMTOT/OT_SCRBRD(I1_KOTARG).OT_NLOOKS, CA_PRFOMX);

 -- 2. If the old target number is not valid, incorporate the new target
 --    data into the Old Target Scoreboard.

 else
         OT_NTARG := OT_NTARG + 1;
         OT_SCRBRD(OT_NTARG).OT_XCENT := NT_SCRBRD(I2_KNTARG).NT_XCENT;
         OT_SCRBRD(OT_NTARG).OT_YCENT := NT_SCRBRD(I2_KNTARG).NT_YCENT;

         -- Feature vector 12 = (height)**2/area;
         -- Feature vector 11 = area;
         -- Therefore, height = (vector 12 * vector 11)**0.5
         HEIGHT := 10.0;
         OT_SCRBRD(OT_NTARG).OT_HEIGHT := NATURAL(HEIGHT);

         -- Feature vector 4 = (width/height)**2;
         -- Therefore, width = ((vector 4)**0.5) * height;

         WIDTH := 10.0;
         OT_SCRBRD(OT_NTARG).OT_WIDTH := NATURAL(WIDTH);

         OT_SCRBRD(OT_NTARG).OT_FOM :=
                  MIN(NT_SCRBRD(I2_KNTARG).NT_FOM - PI_PRNTPF, CA_PRFOMX);
         OT_SCRBRD(OT_NTARG).OT_TYPE := NT_SCRBRD(I2_KNTARG).NT_TYPE;
         OT_SCRBRD(OT_NTARG).OT_NLOOKS := 1;
         OT_SCRBRD(OT_NTARG).OT_COLINF := NT_SCRBRD(I2_KNTARG).NT_COLINF;
         OT_SCRBRD(OT_NTARG).OT_YCREC := NT_SCRBRD(I2_KNTARG).NT_YCREC;
 end if;
 end CAP_UPDATE;


 procedure CAP_SETUP;

 with    NTDATA; use     NTDATA;
 with    OTDATA; use     OTDATA;
 with    TYPDEFS; use    TYPDEFS;
 procedure CAP_SETUP is

 begin
         -- 1. Set up old target scoreboard.

         OT_NTARG := 8;
         OT_SCRBRD(1).OT_XCENT := 200;
         OT_SCRBRD(1).OT_YCENT := 180;
         OT_SCRBRD(1).OT_FOM := 70;
         OT_SCRBRD(1).OT_TYPE := MILITARY_VEHICLE;
         OT_SCRBRD(1).OT_NLOOKS := 3;
         OT_SCRBRD(1).OT_COLINF := 0.0;
         OT_SCRBRD(2).OT_XCENT := 180;
         OT_SCRBRD(2).OT_YCENT := 350;
         OT_SCRBRD(2).OT_FOM := 70;
         OT_SCRBRD(2).OT_TYPE := TANK;
         OT_SCRBRD(2).OT_NLOOKS := 3;
         OT_SCRBRD(2).OT_COLINF := 0.0;
         OT_SCRBRD(3).OT_XCENT := 320;
         OT_SCRBRD(3).OT_YCENT := 300;
         OT_SCRBRD(3).OT_FOM := 60;
         OT_SCRBRD(3).OT_TYPE := TRACKED_VEHICLE;
         OT_SCRBRD(3).OT_NLOOKS := 10;
         OT_SCRBRD(3).OT_COLINF := 1.0;
         OT_SCRBRD(4).OT_XCENT := 160;
         OT_SCRBRD(4).OT_YCENT := 220;
         OT_SCRBRD(4).OT_FOM := 53;
         OT_SCRBRD(4).OT_TYPE := MILITARY_VEHICLE;
         OT_SCRBRD(4).OT_NLOOKS := 1;
         OT_SCRBRD(4).OT_COLINF := 0.0;
         OT_SCRBRD(5).OT_XCENT := 157;
         OT_SCRBRD(5).OT_YCENT := 178;
         OT_SCRBRD(5).OT_FOM := 47;
         OT_SCRBRD(5).OT_TYPE := CLUTTER;
         OT_SCRBRD(5).OT_NLOOKS := 15;
         OT_SCRBRD(5).OT_COLINF := 1.0;
         OT_SCRBRD(6).OT_XCENT := 320;
         OT_SCRBRD(6).OT_YCENT := 33;
         OT_SCRBRD(6).OT_FOM := 33;
         OT_SCRBRD(6).OT_TYPE := MILITARY_VEHICLE;
         OT_SCRBRD(6).OT_NLOOKS := 2;
         OT_SCRBRD(6).OT_COLINF := 0.0;
         OT_SCRBRD(7).OT_XCENT := 155;
         OT_SCRBRD(7).OT_YCENT := 25;
         OT_SCRBRD(7).OT_FOM := 28;
         OT_SCRBRD(7).OT_TYPE := TANK;
         OT_SCRBRD(7).OT_NLOOKS := 2;
         OT_SCRBRD(7).OT_COLINF := 0.0;
         OT_SCRBRD(8).OT_XCENT := 178;
         OT_SCRBRD(8).OT_YCENT := 179;
         OT_SCRBRD(8).OT_FOM := 25;
         OT_SCRBRD(8).OT_TYPE := CLUTTER;
         OT_SCRBRD(8).OT_NLOOKS := 3;
         OT_SCRBRD(8).OT_COLINF := 0.0;

         -- 2. Set up new target scoreboard.

         NT_NTARG := 20;
         NT_SCRBRD(1).NT_XCENT := 185;
         NT_SCRBRD(1).NT_YCENT := 351;
         NT_SCRBRD(1).NT_FOM := 100;
         NT_SCRBRD(1).NT_TYPE := CLUTTER;
         NT_SCRBRD(1).NT_AREA := 20;
         NT_SCRBRD(1).NT_COLINF := 1.0;
         NT_SCRBRD(2).NT_XCENT := 45;
         NT_SCRBRD(2).NT_YCENT := 30;
         NT_SCRBRD(2).NT_FOM := 80;
         NT_SCRBRD(2).NT_TYPE := TANK;
         NT_SCRBRD(2).NT_AREA := 1;
         NT_SCRBRD(2).NT_COLINF := 0.0;
         NT_SCRBRD(3).NT_XCENT := 317;
         NT_SCRBRD(3).NT_YCENT := 27;
         NT_SCRBRD(3).NT_FOM := 45;
         NT_SCRBRD(3).NT_TYPE := TRACKED_VEHICLE;
         NT_SCRBRD(3).NT_AREA := 2;
         NT_SCRBRD(3).NT_COLINF := 1.0;
         NT_SCRBRD(4).NT_XCENT := 153;
         NT_SCRBRD(4).NT_YCENT := 172;
         NT_SCRBRD(4).NT_FOM := 55;
         NT_SCRBRD(4).NT_TYPE := MILITARY_VEHICLE;
         NT_SCRBRD(4).NT_AREA := 3;
         NT_SCRBRD(4).NT_COLINF := 0.0;
         NT_SCRBRD(5).NT_XCENT := 210;
         NT_SCRBRD(5).NT_YCENT := 155;
         NT_SCRBRD(5).NT_FOM := 40;
         NT_SCRBRD(5).NT_TYPE := CLUTTER;
         NT_SCRBRD(5).NT_AREA := 0;
         NT_SCRBRD(5).NT_COLINF := 0.0;
         NT_SCRBRD(6).NT_XCENT := 168;
         NT_SCRBRD(6).NT_YCENT := 178;
         NT_SCRBRD(6).NT_FOM := 0;
         NT_SCRBRD(6).NT_TYPE := TANK;
         NT_SCRBRD(6).NT_AREA := 5;
         NT_SCRBRD(6).NT_COLINF := 0.0;
         NT_SCRBRD(7).NT_XCENT := 20;
         NT_SCRBRD(7).NT_YCENT := 210;
         NT_SCRBRD(7).NT_FOM := 35;
         NT_SCRBRD(7).NT_TYPE := TRACKED_VEHICLE;
         NT_SCRBRD(7).NT_AREA := 0;
         NT_SCRBRD(7).NT_COLINF := 0.0;
         NT_SCRBRD(8).NT_XCENT := 40;
         NT_SCRBRD(8).NT_YCENT := 240;
         NT_SCRBRD(8).NT_FOM := 30;
         NT_SCRBRD(8).NT_TYPE := MILITARY_VEHICLE;
         NT_SCRBRD(8).NT_AREA := 7;
         NT_SCRBRD(8).NT_COLINF := 0.0;
         NT_SCRBRD(9).NT_XCENT := 202;
         NT_SCRBRD(9).NT_YCENT := 177;
         NT_SCRBRD(9).NT_FOM := 85;
         NT_SCRBRD(9).NT_TYPE := CLUTTER;
         NT_SCRBRD(9).NT_AREA := 8;
         NT_SCRBRD(9).NT_COLINF := 0.0;
         NT_SCRBRD(10).NT_XCENT := 315;
         NT_SCRBRD(10).NT_YCENT := 25;
         NT_SCRBRD(10).NT_FOM := 30;
         NT_SCRBRD(10).NT_TYPE := TANK;
         NT_SCRBRD(10).NT_AREA := 9;
         NT_SCRBRD(10).NT_COLINF := 0.0;
         NT_SCRBRD(11).NT_XCENT := 310;
         NT_SCRBRD(11).NT_YCENT := 180;
         NT_SCRBRD(11).NT_FOM := 60;
         NT_SCRBRD(11).NT_TYPE := TRACKED_VEHICLE;
         NT_SCRBRD(11).NT_AREA := 10;
         NT_SCRBRD(11).NT_COLINF := 1.0;
         NT_SCRBRD(12).NT_XCENT := 350;
         NT_SCRBRD(12).NT_YCENT := 300;
         NT_SCRBRD(12).NT_FOM := 25;
         NT_SCRBRD(12).NT_TYPE := MILITARY_VEHICLE;
         NT_SCRBRD(12).NT_AREA := 0;
         NT_SCRBRD(12).NT_COLINF := 0.0;
         NT_SCRBRD(13).NT_XCENT := 100;
         NT_SCRBRD(13).NT_YCENT := 210;
         NT_SCRBRD(13).NT_FOM := 75;
         NT_SCRBRD(13).NT_TYPE := CLUTTER;
         NT_SCRBRD(13).NT_AREA := 12;
         NT_SCRBRD(13).NT_COLINF := 1.0;
         NT_SCRBRD(14).NT_XCENT := 105;
         NT_SCRBRD(14).NT_YCENT := 205;
         NT_SCRBRD(14).NT_FOM := 55;
         NT_SCRBRD(14).NT_TYPE := TANK;
         NT_SCRBRD(14).NT_AREA := 13;
         NT_SCRBRD(14).NT_COLINF := 1.0;
         NT_SCRBRD(15).NT_XCENT := 208;
         NT_SCRBRD(15).NT_YCENT := 176;
         NT_SCRBRD(15).NT_FOM := 70;
         NT_SCRBRD(15).NT_TYPE := TRACKED_VEHICLE;
         NT_SCRBRD(15).NT_AREA := 14;
         NT_SCRBRD(15).NT_COLINF := 0.0;
         NT_SCRBRD(16).NT_XCENT := 220;
         NT_SCRBRD(16).NT_YCENT := 250;
         NT_SCRBRD(16).NT_FOM := 70;
         NT_SCRBRD(16).NT_TYPE := MILITARY_VEHICLE;
         NT_SCRBRD(16).NT_AREA := 15;
         NT_SCRBRD(16).NT_COLINF := 0.0;
         NT_SCRBRD(17).NT_XCENT := 305;
         NT_SCRBRD(17).NT_YCENT := 180;
         NT_SCRBRD(17).NT_FOM := 60;
         NT_SCRBRD(17).NT_TYPE := CLUTTER;
         NT_SCRBRD(17).NT_AREA := 16;
         NT_SCRBRD(17).NT_COLINF := 0.0;
         NT_SCRBRD(18).NT_XCENT := 23;
         NT_SCRBRD(18).NT_YCENT := 335;
         NT_SCRBRD(18).NT_FOM := 10;
         NT_SCRBRD(18).NT_TYPE := TANK;
         NT_SCRBRD(18).NT_AREA := 17;
         NT_SCRBRD(18).NT_COLINF := 0.0;
         NT_SCRBRD(19).NT_XCENT := 20;
         NT_SCRBRD(19).NT_YCENT := 340;
         NT_SCRBRD(19).NT_FOM := 60;
         NT_SCRBRD(19).NT_TYPE := TRACKED_VEHICLE;
         NT_SCRBRD(19).NT_AREA := 18;
         NT_SCRBRD(19).NT_COLINF := 0.0;
         NT_SCRBRD(20).NT_XCENT := 333;
         NT_SCRBRD(20).NT_YCENT := 25;
         NT_SCRBRD(20).NT_FOM := 20;
         NT_SCRBRD(20).NT_TYPE := MILITARY_VEHICLE;
         NT_SCRBRD(20).NT_AREA := 19;
         NT_SCRBRD(20).NT_COLINF := 0.0;

 end CAP_SETUP;


 procedure CAP_PROJCT(I1_KOTARG : in NATURAL);
 with CAPDTA;    use CAPDTA;
 with OTDATA;    use OTDATA;
 with PCCDATA;   use PCCDATA;
 procedure CAP_PROJCT(I1_KOTARG : in NATURAL) is
 FACTOR : FLOAT;
 begin
 FACTOR := PD_RNGRTE * CA_DTPROJ;
 OT_SCRBRD(I1_KOTARG).OT_XCENT := OT_SCRBRD(I1_KOTARG).OT_XCENT;
 OT_SCRBRD(I1_KOTARG).OT_YCENT := OT_SCRBRD(I1_KOTARG).OT_YCENT;
 FACTOR := PD_RNGRTE * CA_DTPREC;
 OT_SCRBRD(I1_KOTARG).OT_XCREC := OT_SCRBRD(I1_KOTARG).OT_XCREC;
 OT_SCRBRD(I1_KOTARG).OT_YCREC := OT_SCRBRD(I1_KOTARG).OT_YCREC;
 end CAP_PROJCT;


 procedure CAP_PRIORITIZE;

 with CAPDTA;    use CAPDTA;
 with PROGIN;    use PROGIN;
 with OTDATA;    use OTDATA;

 procedure CAP_PRIORITIZE is

 FOM : INTEGER;
 IOTARG : NATURAL;
 KOTARG : NATURAL;
 OTRANK : NATURAL;

 begin

 -- 1. Initialize the priority hash table and the priority chain table.

 for FOM in 1..CA_PRFOMX
         loop
         CA_PHASH(FOM) := 0;
         end loop;
 for IOTARG in 1..OT_NOTMAX
         loop
         CA_PCHAIN(IOTARG) := 0;
         end loop;

 -- 2. Squeeze the low Figure of Merit targets out of the Old Target
 --    Scoreboard.  Use the PHASH and PCHAIN tables to keep track of
 --    targets with the same FOM.

 KOTARG := 0;
 for IOTARG in 1..OT_NTARG
         loop
         FOM := OT_SCRBRD(IOTARG).OT_FOM;
         if FOM >= PI_PRFOMN then
             KOTARG := KOTARG + 1;
             if KOTARG < IOTARG then
               OT_SCRBRD(KOTARG).OT_XCENT := OT_SCRBRD(IOTARG).OT_XCENT;
               OT_SCRBRD(KOTARG).OT_YCENT := OT_SCRBRD(IOTARG).OT_YCENT;
               OT_SCRBRD(KOTARG).OT_HEIGHT := OT_SCRBRD(IOTARG).OT_HEIGHT;
               OT_SCRBRD(KOTARG).OT_WIDTH := OT_SCRBRD(IOTARG).OT_WIDTH;
               OT_SCRBRD(KOTARG).OT_TYPE := OT_SCRBRD(IOTARG).OT_TYPE;
               OT_SCRBRD(KOTARG).OT_FOM := OT_SCRBRD(IOTARG).OT_FOM;
               OT_SCRBRD(KOTARG).OT_NLOOKS := OT_SCRBRD(IOTARG).OT_NLOOKS;
               OT_SCRBRD(KOTARG).OT_COLINF := OT_SCRBRD(IOTARG).OT_COLINF;
               OT_SCRBRD(KOTARG).OT_XCREC := OT_SCRBRD(IOTARG).OT_XCREC;
               OT_SCRBRD(KOTARG).OT_YCREC := OT_SCRBRD(IOTARG).OT_YCREC;
             end if;
             CA_PCHAIN(KOTARG) := CA_PHASH(FOM);
             CA_PHASH(FOM) := KOTARG;
         end if;
         end loop;
 OT_NTARG := KOTARG;

 -- 3. Form a prioritized list of targets.

 OTRANK := 1;
 for FOM in reverse 1..CA_PRFOMX
         loop
         KOTARG := CA_PHASH(FOM);
         while KOTARG > 0
                 loop
                 OT_PRLIST(OTRANK) := KOTARG;
                 OTRANK := OTRANK + 1;
                 KOTARG := CA_PCHAIN(KOTARG);
                 end loop;
         end loop;
 end CAP_PRIORITIZE;


 procedure CAP_INIT(I1_RESET : in BOOLEAN);

 with    CAPDTA; use     CAPDTA;
 with    NTDATA; use     NTDATA;
 with    OTDATA; use     OTDATA;
 with    PCCDATA; use    PCCDATA;
 with    PROGIN; use     PROGIN;
 procedure CAP_INIT(I1_RESET : in BOOLEAN) is

 begin
         -- 1. Set clusters empty.

         for ICLUSTR in 1..CA_CLMXNO
                 loop
                 CA_HASH(ICLUSTR) := 0;
                 end loop;

         for ITARG in 1..NT_NNTMAX
                 loop
                 CA_CHAIN(ITARG) := 0;
                 end loop;

         -- 2. Calculate clustering redundancy distance.

         CA_CLREDD := INTEGER(PI_CLREDF/100.0);

         -- 3. If flagged, reset Old Scoreboard.

         if I1_RESET then
                 OT_NTARG := 0;
         end if;

 end CAP_INIT;


 function CAP_CLOSEST(I1_KOTARG : in NATURAL) return NATURAL;

 with TYPDEFS;   use TYPDEFS;
 with GPM;       use GPM;
 with CAPDTA;    use CAPDTA;
 with NTDATA;    use NTDATA;
 with PROGIN;    use PROGIN;
 with OTDATA;    use OTDATA;

 function CAP_CLOSEST(I1_KOTARG : in NATURAL) return NATURAL is

 function MIN(S1 : in INTEGER; S2 : in INTEGER) return INTEGER
 renames GPM_MIN;

 function MAX(S1 : in INTEGER; S2 : in INTEGER) return INTEGER
 renames GPM_MAX;

 DNT2OT : NATURAL;
 DNTOTL : NATURAL;
 ICLUSTR : NATURAL;
 NCLSCH : NATURAL;
 KLUSTR : NATURAL;
 KNTARG : NATURAL;
 LNTARG : NATURAL;
 RESULT : NATURAL;

 begin

 -- 1. Initialize the output to no new tareget within the correlation
 --    distance, set the last new to old target distance, and calculate
 --    the old target cluster number and the number of clusters to search.

 RESULT := 0;
 DNTOTL := PI_CODMAX + 1;
 KLUSTR := OT_SCRBRD(I1_KOTARG).OT_XCENT/PI_CLSIZE + 1;
 NCLSCH := PI_CODMAX/PI_CLSIZE + 1;

 -- 2. Search nearby clusters for the closest new target that is:
 --    (1) within the maximum correlation distance, (2) not redundant,
 --    (3) not already correlated to another old target, and (4) not
 --    already associated with target number I1_KOTARG.

 for ICLUSTR in MAX(1,KLUSTR-NCLSCH)..MIN(KLUSTR+NCLSCH,CA_CLMXNO)
         loop
         KNTARG := CA_HASH(ICLUSTR);
         while KNTARG > 0
                 loop
                 if CA_REDUND(KNTARG) = NO then
                         if CA_USEDNT(KNTARG) = NO then
                                 if KNTARG /= CA_KNTAOT(I1_KOTARG) then

                                    DNT2OT :=
                                        ABS(NT_SCRBRD(KNTARG).NT_XCENT -
                                         OT_SCRBRD(I1_KOTARG).OT_XCENT) +
                                         ABS(NT_SCRBRD(KNTARG).NT_YCENT -
                                         OT_SCRBRD(I1_KOTARG).OT_YCENT);

                                    if DNT2OT < DNTOTL then
                                         RESULT := KNTARG;
                                         DNTOTL := DNT2OT;
                                    end if;
                                 end if;
                         end if;
                 end if;
                 KNTARG := CA_CHAIN(KNTARG);
                 end loop;
         end loop;

         return RESULT;

 end CAP_CLOSEST;


 procedure CAP_CORRELATE;

 with TYPDEFS;   use TYPDEFS;
 with CAPDTA;    use CAPDTA;
 with EXECDATA;  use EXECDATA;
 with NTDATA;    use NTDATA;
 with PROGIN;    use PROGIN;
 with OTDATA;    use OTDATA;
 with CAP_CLOSEST;
 with CAP_PROJCT;
 with CAP_UPDATE;

 procedure CAP_CORRELATE is

 INTARG : NATURAL;
 IOTARG : NATURAL;
 KNTAOT : NATURAL :=1;
 LNTAOT : NATURAL;

 begin
 -- 1. Initialize the association arrays which specify which new target
 --    is associated with each old target (CA_KNTAOT), the number of old
 --    targets associateed with each new target (CA_NOTANT), and which
 --    new targets have been correlated with an old target (CA_USEDNT).

 for IOTARG in 1..OT_NOTMAX
         loop
         CA_KNTAOT(IOTARG) := 0;
         end loop;

 for INTARG in 1..NT_NNTMAX
         loop
         CA_NOTANT(INTARG) := 0;
         CA_USEDNT(INTARG) := NO;
         end loop;

 -- 2. Factor the CCP results into the Figure of Merit.

 for INTARG in 1..NT_NNTMAX
         loop
	 IF ((NT_SCRBRD(INTARG).NT_COLINF*PI_CCFOMF)+
	 FLOAT(NT_SCRBRD(INTARG).NT_FOM))> 32000.0 
	 THEN NT_SCRBRD(INTARG).NT_FOM:=32000;
         ELSE NT_SCRBRD(INTARG).NT_FOM := NT_SCRBRD(INTARG).NT_FOM  +
                     NATURAL(PI_CCFOMF * NT_SCRBRD(INTARG).NT_COLINF);
         END IF;
         end loop;

 -- 3. Process all old targets.

 if OT_NTARG > 0 then

         -- 3.1 For each old target, project position to current time,
         --     find the closest new target and update the association
         --     arrays.

         CA_DTPROJ := EX_PRIME_FREEZE_TIME - OT_TIME;
         CA_DTPREC := EX_REC_FREEZE_TIME - OT_TREC;

         for IOTARG in 1..OT_NTARG
                 loop
                 CAP_PROJCT(IOTARG);
                 KNTAOT := CAP_CLOSEST(IOTARG);
                 if KNTAOT > 0 then
                         CA_NOTANT(KNTAOT) := CA_NOTANT(KNTAOT) + 1;
                 end if;
                 CA_KNTAOT(IOTARG) := KNTAOT;
                 end loop;

         -- 3.2 Correlate the Old Target Scoreboard with the New Target
         --     Scoreboard.

         for IOTARG in 1..OT_NTARG
                 loop
                 -- 3.2.1 Get the closest new target.
                 --       If the closest new target is already correlated,
                 --       look for another close new target;
                 --       Else, if the closest new target is also closest
                 --       to another old target, leave it for the other
                --       old target if another close new target can be
                --       found.

                 KNTAOT := CA_KNTAOT(IOTARG);
                 if KNTAOT > 0 then
                         if CA_USEDNT(KNTAOT) = YES then
                                 CA_NOTANT(KNTAOT) :=
                                        CA_NOTANT(KNTAOT) + 1;
                                 KNTAOT := CAP_CLOSEST(IOTARG);
                         else
                                 if CA_NOTANT(KNTAOT) > 1 then
                                         LNTAOT := CAP_CLOSEST(IOTARG);
                                         if LNTAOT > 0 then
                                            CA_NOTANT(KNTAOT) :=
                                                CA_NOTANT(KNTAOT) - 1;
                                            KNTAOT := LNTAOT;
                                         end if;
                                 end if;
                         end if;
                 end if;

                 -- 3.2.2 Update the old target data and, if a new
                --       target was correlated to the old target, tag
                --       the new target as correlated.

                 CAP_UPDATE(IOTARG, KNTAOT);
                 if KNTAOT > 0 then
                         CA_USEDNT(KNTAOT) := YES;
                 end if;
         end loop;
 end if; -- "if IOTARG > 0"

 -- 4. If there is room, add any worthwhile, uncorrelated new targets to
 --    the Old Target Scoreboard.

 if NT_NTARG > 0 then
         IOTARG := 0;
         for INTARG in 1..NT_NTARG
                 loop
                 if OT_NTARG < OT_NOTMAX then
                         if CA_USEDNT(INTARG) = NO then
                                 if CA_REDUND(INTARG) = NO then
                                         -- Any candidate new targs must
                                        -- have nonzero
                                         -- area.
                                         if NT_SCRBRD(INTARG).NT_FOM > 0
                                                and
                                            NT_SCRBRD(INTARG).NT_AREA > 0
                                                then
                                                CAP_UPDATE(IOTARG,INTARG);
                                         end if;
                                 end if;
                         end if;
                 end if;
                 end loop;
 end if;

 -- 5. Update the time associated with the old target position, and the
 --    time associated with the REC position.

 OT_TIME := EX_PRIME_FREEZE_TIME;
 OT_TREC := EX_REC_FREEZE_TIME;

 end CAP_CORRELATE;


 procedure CAP_CLUSTER;

 with TYPDEFS;   use TYPDEFS;
 with GPM;       use GPM;
 with CAPDTA;    use CAPDTA;
 with CONVAR;    use CONVAR;
 with NTDATA;    use NTDATA;
 with PROGIN;    use PROGIN;

 procedure CAP_CLUSTER is

 function MAX(S1 : in INTEGER; S2 : in INTEGER) return INTEGER
 renames GPM_MAX;

 function MIN(S1 : in INTEGER; S2 : in INTEGER) return INTEGER
 renames GPM_MIN;

 --NT_XCENT : INTEGER renames NT_SCRBRD.NT_XCENT;
 --NT_YCENT : INTEGER renames NT_SCRBRD.NT_YCENT;
 --NT_FOM : INTEGER renames NT_SCRBRD.NT_FOM;

 ITARG : NATURAL;
 ITPROC : NATURAL;
 KLUSTR : NATURAL;
 KTARG : NATURAL;
 XC : NATURAL;
 YC : NATURAL;

 begin
 for ITPROC in 1..CV_NTPROC
         loop
         -- 1. Get target number and position, calculate cluster number
        --    and add the target to the cluster.

         ITARG := CV_TNPROC(ITPROC);
         XC := NT_SCRBRD(ITARG).NT_XCENT;
         YC := NT_SCRBRD(ITARG).NT_YCENT;
         KLUSTR := MIN(XC/PI_CLSIZE + 1, CA_CLMXNO);
         CA_CHAIN(ITARG) := CA_HASH(KLUSTR);
         CA_HASH(KLUSTR) := ITARG;

         -- 2. Check nearby clusters for redundancy.

         CA_REDUND(ITARG) := NO;
         for ICLUSTR in (MAX(1,KLUSTR-1))..(MIN(KLUSTR+1,CA_CLMXNO))
                 loop
                 KTARG := CA_HASH(ICLUSTR);
                 if ICLUSTR = KLUSTR then
                         KTARG := CA_CHAIN(KTARG);
                 end if;
                 while KTARG > 0
                         loop
                         if CA_REDUND(KTARG) = NO then
                                 if abs(NT_SCRBRD(KTARG).NT_XCENT-XC) +
                                    abs(NT_SCRBRD(KTARG).NT_YCENT-YC) <
                                    CA_CLREDD then
                                         if NT_SCRBRD(KTARG).NT_FOM >
                                            NT_SCRBRD(ITARG).NT_FOM then
                                                 CA_REDUND(ITARG) := YES;
                                                 goto NXTITP;
                                         else
                                                 CA_REDUND(KTARG) := YES;
                                         end if;
                                 end if;
                         end if;
                         KTARG := CA_CHAIN(KTARG);
                         end loop;
                 end loop;
 <<NXTITP>> null;
         end loop;
 end CAP_CLUSTER;


 with CONVAR;    use CONVAR;
 with NTDATA;    use NTDATA;
 with CAP_INIT;
 with CAP_PRIORITIZE;
 with CAP_CORRELATE;
 with CAP_SETUP;
 with CAP_CLUSTER;

 procedure CAP_MAIN is

 INX : INTEGER;
 NO_RESET : BOOLEAN := FALSE;

 begin
 CAP_SETUP;
 CAP_INIT(NO_RESET);     --Initialize CAP data structures
 if NT_NTARG > 0 then
         --Set up input to CAP routines
         CV_NTPROC := 0;
         for INX in 1..NT_NTARG
                 loop
                 if NT_SCRBRD(INX).NT_AREA > 0 then
                         CV_NTPROC := CV_NTPROC + 1;
                         CV_TNPROC(CV_NTPROC) := INX;
                 end if;
                 end loop;
         --Do correlation and prioritization
         CAP_CLUSTER;
         CAP_CORRELATE;
         CAP_PRIORITIZE;
 end if;
 end CAP_MAIN;



 with CALENDAR ; use CALENDAR ;

 with Cap_Main;
 with Text_IO;

 procedure Cap is

   package FP_IO is new Text_IO.Float_IO (Float);
   package INT_IO is new Text_IO.Integer_io (integer);

   Start_Time : Time;
   Stop_Time : Time;
   Iterations : Integer := 1000;

 begin

   Start_Time := clock;

   Text_IO.Put ("Running CAP ...");
   Text_IO.New_Line;

   for I in 1..Iterations loop
     Cap_Main;
   end loop;

   Stop_Time := clock;

   INT_IO.Put (Iterations,2);
   Text_IO.Put (" iterations executed in ");
   FP_IO.Put (Item => Float (Stop_Time - Start_Time));
   Text_IO.Put (" seconds (");

   FP_IO.Put (Item => Float (Stop_Time - Start_Time)/Float (Iterations));

   Text_IO.Put_line (" s/iteration)");
   Text_IO.New_Line;

 end Cap;




 with cap_main;

 procedure Size_Cap is

 begin

     Cap_Main;

 end Size_Cap;

package integer_types is

type byte is range 0..255;
	for byte'size use 8;

type int16 is range -2**15..2**15-1;
  subtype nat16 is int16 range 0..int16'last;
  subtype pos16 is int16 range 1..int16'last;

type int32 is range -2**31..2**31-1;
  subtype nat32 is int32 range 0..int32'last;
  subtype pos32 is int32 range 1..int32'last;

end integer_types;

package body integer_types is

begin
  null;
end integer_types;
----------------------------------------------------------------------
--ABSTRACT--BIT_MANIPULATION_CONSTANTS
--	Definition of the constants employed by the bitmanipulation.

--KEYWORDS--BIT_MANIPULATION_CONSTANTS
--	bitmanipulation, Estec-B

--CONTENTS--BIT_MANIPULATION_CONSTANTS
--version:		1.1
--date:			dec 1989	
--author:		Tom Wiley
--machine:		no dependency
--compiler:		no dependency
--changes:		1.2 - Martien de Jong 15-08-90
--					added power_2, a lookup table for the powers of
--					two.
----------------------------------------------------------------------




-- This is the file bitmanip_constants.ada

with INTEGER_TYPES; use INTEGER_TYPES;

package bit_manipulation_constants is

  out_unit_max : int16 := 8;
    -- maximum number of bits we can use in an INTEGER_BYTE.
  dc_coeff_max_bits : int16 := 15;
    -- the number of bits we always use to encode the DC coefficient
  segment_size : int16 := 4;
  number_of_segments : int16 := 16;
    -- we take the coefficients in chunks of 4, and there are 16
    -- of these in an 8*8 block.
  log2_num_seg : int16 := 4;
    -- The number of bits we require to code a `bitmap_address'.
  msb_number_of_segments : int16 := 16;
    -- when encoding the bitmap addresses, we denote addresses of 
    -- non-existent bitmaps by a 0, and addresses of existing bitmaps
    -- by a 1 followed by a number between 0 and number_of_segments-1.
  max_bits_in_bitmap_of_bitmap : constant int16 := 4;
    -- essentially log2 of largest number of bits which you can have
    -- in a coefficient.
  subtype out_unit_index is int16 range 0..out_unit_max;
    -- Records the number of unused bits in an integer_byte.
  type segment is array(int16 range 0..segment_size-1) of int16;
  type segment_block is array(int16 range 0..number_of_segments-1) of segment;
  type bitmap_type is array(int16 range 0..number_of_segments-1) of int16;
  type bitmap_address_type is array(int16 range 0..max_bits_in_bitmap_of_bitmap)
       of int16; 

  power_2: array(int16 range 0..14) of int16 := 
      (1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384);

end bit_manipulation_constants;

package body bit_manipulation_constants is
end bit_manipulation_constants;
----------------------------------------------------------------------
--ABSTRACT--IMAGE_COMPRESSION
--	Specification of the package image_compression

--KEYWORDS--IMAGE_COMPRESSION
--	image compression, Estec-B

--CONTENTS--IMAGE_COMPRESSION
--version:		1.1
--date:			dec 1989
--author:		Tom Wiley
--machine:		no dependency
--compiler:		no dependency
--changes:		1.2 - Martien de Jong 15-08-90
--					Cosmetics
----------------------------------------------------------------------



-- File compress_spec.ada, containing the specification of 
-- Ada procedures `compress' and `decompress', which implement
-- the Estec-B algorithm.  For full details, see the document
-- `Estec-B in Ada'.
--
-- Requires also the package INTEGER_TYPES, which is found in
-- the library /users/wdp/tom/ada/standard/standard.sub
-- For the record, this package contains the declarations
-- 
--   type int32 is -2**31..2**31-1;
--   type int16 is -2**15..2**15-1;
--   type byte is  0..255;
--
-- However we keep these declarations in a separate library
-- in accordance with the principle of recording every piece
-- of information exactly once.
--
-- Written and maintained by Tom Wiley, Estec WDP, Postbus 299,
--                                      2200 AG Noordwijk, Netherlands
--                           Phone :    31 1719 83520

with INTEGER_TYPES; use INTEGER_TYPES;
--with CALENDAR; use CALENDAR;

package image_compression is

n : constant int16 := 8;    -- work with 8*8 blocks
block_size : constant int16 := n * n;
type coeff_row is array(int16 range 0..n-1) of int16;
type coeff_block is array(int16 range 0..n-1) of coeff_row;
type compressed_data is array(int16 range <>) of byte;
  
procedure compress (
		image : in coeff_block; 
		q_step : in int16; 
		number_of_bytes : out int16; 
		compressed_image : out compressed_data);
procedure decompress (
		compressed_image : in compressed_data; 
		q_step : in int16; 
		number_of_bytes : in int16; 
		recovered_image : out coeff_block);

end image_compression;
----------------------------------------------------------------------
--ABSTRACT--REORDER_CONSTANTS
--	Array defining the permutation of the dc-transformed coefficients.

--KEYWORDS--REORDER_CONSTANTS
--	reordering, Estec-B

--CONTENTS--REORDER_CONSTANTS
--version:		1.2
--date:			15-08-90
--author:		Martien de Jong
--machine:		no dependency
--compiler:		no dependency
--changes:
----------------------------------------------------------------------



with integer_types; use integer_types;
with image_compression; 

package reorder_constants is

--
-- This is the constant array employed to reorder the dct coefficients.
-- Note that the same array can be used for both the compression phase as
-- the decompression phase: in stead of 
--
--      out(i) := in(unzigzag(i))
-- 
-- you can use:
--
--      out(zigzag(i)) := in(i)
--
-- where unzigzag(i) is the inverse of zigzag(i). 
--

zigzag : 
array(int16 range 0..image_compression.block_size-1) of int16 := 
(
             0,  2,  3,  9, 10, 20, 21, 35,
             1,  4,  8, 11, 19, 22, 34, 36,
             5,  7, 12, 18, 23, 33, 37, 48,
             6, 13, 17, 24, 32, 38, 47, 49,
            14, 16, 25, 31, 39, 46, 50, 57,
            15, 26, 30, 40, 45, 51, 56, 58,
            27, 29, 41, 44, 52, 55, 59, 62,
            28, 42, 43, 53, 54, 60, 61, 63
);

end reorder_constants;

package body reorder_constants is
end reorder_constants;


----------------------------------------------------------------------
--ABSTRACT--IMAGE_COMPRESSION
--	Alternative body for image_compression, including code to print the
--	timings of the separate stages for benchmarking purposes

--KEYWORDS--IMAGE_COMPRESSION
--	Estec-B, image_compression, benchmarking

--CONTENTS--IMAGE_COMPRESSION
--version:		1.0
--date:			15-08-90
--author:		Martien de Jong
--machine:		no dependency
--compiler:		no dependency
--changes:
----------------------------------------------------------------------



package body image_compression is

  type linear_block is array(int16 range 0..block_size-1) of nat16;

  procedure twod_dct (inblock : in coeff_block; z : out coeff_block)
    is separate;
  procedure twod_dct_inverse (inblock : in coeff_block;
                              outblock : out coeff_block) is separate;
  procedure reorder_quantize (q_step : in int16; inblock : in coeff_block;
                              dc_coeff : out int16;
                              outarray : out linear_block) is separate;
  procedure reorder_quantize_inverse (q_step : in int16;
                                      dc_coeff : in int16;
                                      inline : in linear_block;
                                      outblock : out coeff_block) is separate;
  procedure bitmanip (inblock : in linear_block; dc_coeff : in int16;
                      result : out compressed_data;
                      number_of_bytes : out int16) is separate;
  procedure bitmanip_inverse (in_data : in compressed_data;
                              number_of_bytes : in int16;
                              dc_coeff : out int16;
                              out_block : out linear_block) is separate;


  procedure compress (image : in coeff_block; q_step : in int16;
                      number_of_bytes : out int16;
                      compressed_image : out compressed_data) is


    dct_of_image : coeff_block;
    dc_coeff : int16;
    other_coeffs : linear_block;

  begin

        twod_dct (image, dct_of_image);
        reorder_quantize (q_step, dct_of_image, dc_coeff, other_coeffs);
        bitmanip (other_coeffs, dc_coeff, compressed_image, number_of_bytes);

  end compress;

  procedure decompress (compressed_image : in compressed_data;
                        q_step : in int16; number_of_bytes : in int16;
                        recovered_image : out coeff_block) is

    dc_coeff : int16;
    other_coeffs : linear_block;
    recovered_dct : coeff_block;


  begin
        bitmanip_inverse (  compressed_image, number_of_bytes, 
                            dc_coeff,other_coeffs);
        reorder_quantize_inverse (  q_step, dc_coeff, 
                                    other_coeffs, recovered_dct);
        twod_dct_inverse (recovered_dct, recovered_image);

  end decompress;

begin

  null;
end image_compression;
----------------------------------------------------------------------
--ABSTRACT--DCT
--	This procedure computes the 8*8 discrete cosine transform

--KEYWORDS--DCT
--	compression, discrete cosine transform, Estec-B

--CONTENTS--DCT
--version:		1.1
--date:			dec 1989
--author:		Tom Wiley
--machine:		no dependancy
--compiler:		no dependancy
--changes:		1.2 - Martien de Jong 15-08-90
--					Some storage economy optimizations
----------------------------------------------------------------------



with INTEGER_TYPES;
use INTEGER_TYPES;

separate(image_compression)
procedure twod_dct (inblock : in coeff_block; z : out coeff_block) is

  -- Computes the two-dimensional 8 by 8 discrete cosine transform (DCT)
  -- of inblock.  The output block will contain the dct coefficients
  -- multiplied by 32 when compared to the original 2-d DCT formula.

  type int32_coeff_row is array(coeff_row'range) of int32;
  type int32_coeff_block is array(coeff_block'range) of int32_coeff_row;

  subtype coeff_column_pointer is int16 range coeff_row'range;
  procedure horizontal_fwd_dct (column : in coeff_column_pointer; 
                                inrow : in coeff_row; 
                                outblock : out int32_coeff_block) is
  
  -- first we define sine and cosine values, which are the same
  -- as those used by our occam program
  c2     : int32 := 8035;
  s2     : int32 := 1598;
  c4     : int32 := 7569;
  s4     : int32 := 3135;
  c8     : int32 := 5793;
  s10    : int32 := 6812;
  c10    : int32 := 4551;
  c8s2   : int32 := 1130;
  c8c2   : int32 := 5681;
  c8s10  : int32 := 4817;
  c8c10  : int32 := 3218;
  scaling_factor : constant int32 := 64;
  v0, v1 : int32;
  w0, w1, w2, w3, w4, w5, w6, w7 : int32;

  begin
    w0 := int32(inrow(0) + inrow(7));
    w1 := int32(inrow(1) + inrow(6));
    w2 := int32(inrow(2) + inrow(5));
    w3 := int32(inrow(3) + inrow(4));
    w4 := int32(inrow(3) - inrow(4));
    w5 := int32(inrow(2) - inrow(5));
    w6 := int32(inrow(1) - inrow(6));
    w7 := int32(inrow(0) - inrow(7));

    -- isecond & ithird stage 
    -- MMdJ: did some storage optimisation
    v0 := w0 + w3; 
    w3 := w0 - w3; 
    v1 := w1 + w2; 
    w2 := w1 - w2;

    outblock(0)(column) := ((v0 + v1) * c8) / scaling_factor;
    outblock(4)(column) := ((v0 - v1) * c8) / scaling_factor;
    outblock(2)(column) := (w2*s4 + w3*c4) / scaling_factor;
    outblock(6)(column) := (w3*s4 - w2*c4) / scaling_factor;

    v0 := w6 - w5; 
    w6 := w6 + w5;

    outblock(1)(column) := (w4*s2 + v0*c8s2 + w6*c8c2 + w7*c2) 
                / scaling_factor;
    outblock(5)(column) := (w4*s10 - v0*c8s10 + w7*c10 - w6*c8c10) 
                / scaling_factor;
    outblock(3)(column) := (w7*s10 - w6*c8s10 + v0*c8c10 - w4*c10) 
                / scaling_factor;
    outblock(7)(column) := (w7*s2 + w6*c8s2 - v0*c8c2 - w4*c2) 
                / scaling_factor;

  end horizontal_fwd_dct; 

  procedure vertical_fwd_dct (inrow : in int32_coeff_row;
                  outrow : out coeff_row) is

  -- cX = (cos(X*pi/32)) * (2**8)
  c2    : constant int32 := 251;
  s2    : constant int32 := 50;
  c4    : constant int32 := 237;
  s4    : constant int32 := 98;
  c8    : constant int32 := 181;
  s10   : constant int32 := 213;
  c10   : constant int32 := 142;
  c8s2  : constant int32 := 35;
  c8c2  : constant int32 := 178;
  c8s10 : constant int32 := 151;
  c8c10 : constant int32 := 101;
  v0, v1 : int32;
  w0, w1, w2, w3, w4, w5, w6, w7 : int32;

  begin
    w0 := inrow(0) + inrow(7);
    w1 := inrow(1) + inrow(6);
    w2 := inrow(2) + inrow(5);
    w3 := inrow(3) + inrow(4);
    w4 := inrow(3) - inrow(4);
    w5 := inrow(2) - inrow(5);
    w6 := inrow(1) - inrow(6);
    w7 := inrow(0) - inrow(7);

    -- isecond & ithird stage
    v0 := w0 + w3; 
    w3 := w0 - w3;
    v1 := w1 + w2; 
    w2 := w1 - w2;

    outrow(0) := int16(((v0 + v1) * c8) / 16384);
    outrow(4) := int16(((v0 - v1) * c8) / 16384);
    outrow(2) := int16((w2*s4 + w3*c4) / 16384);
    outrow(6) := int16((w3*s4 - w2*c4) / 16384);

    v0 := w6 - w5; w6 := w6 + w5;
    outrow(1) := int16((w4*s2 + v0*c8s2 + w6*c8c2 + w7*c2) / 16384);
    outrow(5) := int16((w4*s10 - v0*c8s10 + w7*c10 - w6*c8c10) / 16384);
    outrow(3) := int16((w7*s10 - w6*c8s10 + v0*c8c10 - w4*c10) / 16384);
    outrow(7) := int16((w7*s2 + w6*c8s2 - v0*c8c2 - w4*c2) / 16384);

  end vertical_fwd_dct;

begin -- twod_dct

  declare
    intermediate_block : int32_coeff_block;
  begin
    for i in inblock'range loop
      horizontal_fwd_dct(i,inblock(i),intermediate_block);
    end loop;
    for i in intermediate_block'range loop
      vertical_fwd_dct(intermediate_block(i),z(i));
    end loop;
  end;

end twod_dct;
----------------------------------------------------------------------
--ABSTRACT--DCTINV
--	This procedure computes the 8*8 inverse discrete cosine
--	transform. Input and output are blocks of int16's

--KEYWORDS--DCTINV
--	decompression, discrete cosine transform, Estec-B

--CONTENTS--DCTINV
--version:		1.1
--date:			dec 1989
--author:		Tom Wiley
--machine:		no dependency
--compiler:		no dependency
--changes:		1.2 - Martien de Jong 15-08-90 
--					some storage economy optimization
----------------------------------------------------------------------



with INTEGER_TYPES; use INTEGER_TYPES;
--with TEXT_IO;

separate (image_compression)
procedure twod_dct_inverse (inblock : in coeff_block;
                            outblock : out coeff_block) is
-- Computes the inverse of procedure `twod_dct'.

type int32_coeff_row is array(coeff_row'range) of int32;
type int32_coeff_block is array(coeff_block'range) of int32_coeff_row;

procedure horizontal_dct_inverse (inrow : in coeff_row;
                                  outrow : out int32_coeff_row) is

  c2    : constant int32 := 4017;
  s2    : constant int32 := 799;
  c4    : constant int32 := 3784;
  s4    : constant int32 := 1568;
  c8    : constant int32 := 2896;
  s10   : constant int32 := 3406;
  c10   : constant int32 := 2276;
  c8s2  : constant int32 := 565;
  c8c2  : constant int32 := 2841;
  c8s10 : constant int32 := 2408;
  c8c10 : constant int32 := 1609;
  a0,a1,a2,a3,a4,a5,a6,a7 : int32;
  b0 : int32;

  begin
    
    -- ithird stage
    a0 := (int32(inrow(0) + inrow(4)) * c8) / 4096;
    a1 := (int32(inrow(0) - inrow(4)) * c8) / 4096;
    a2 := (int32(inrow(2))*s4 - int32(inrow(6))*c4) / 4096;
    a3 := (int32(inrow(2))*c4 + int32(inrow(6))*s4) / 4096;
    a4 := (int32(inrow(1))*s2 + int32(inrow(5))*s10 
       - int32(inrow(3))*c10 - int32(inrow(7))*c2) / 4096;
    a5 := (int32(inrow(1))*c8s2 - int32(inrow(5))*c8s10
       + int32(inrow(3))*c8c10 - int32(inrow(7))*c8c2)
          / 4096;
    a6 := (int32(inrow(1))*c8c2 - int32(inrow(5))*c8c10 
       + int32(inrow(7))*c8s2 - int32(inrow(3))*c8s10)
          / 4096;
    a7 := (int32(inrow(1))*c2 + int32(inrow(5))*c10
       + int32(inrow(3))*s10 + int32(inrow(7))*s2) / 4096;

    -- isecond & ithird stage
    b0 := a0 + a3; 
    outrow(0) := b0 + a7;
    outrow(7) := b0 - a7;       -- From here on, a7 is available 
                                -- for temp storage
    a3 := a0 - a3;
    outrow(3) := a3 + a4;
    outrow(4) := a3 - a4;
    b0 := a1 + a2; 
    a2 := a1 - a2;
    a7 := a6 - a5; 
    a6 := a6 + a5;
    
    outrow(1) := b0 + a6;
    outrow(6) := b0 - a6;
    outrow(2) := a2 + a7;
    outrow(5) := a2 - a7;

  end horizontal_dct_inverse;

procedure vertical_dct_inverse (incol : in int32_coeff_row; 
                                outcol : out int32_coeff_row) is

  -- cX := (cos(X*pi/32))*(2**8)
  c2    : constant int32 := 251;
  s2    : constant int32 := 50;
  c4    : constant int32 := 237;
  s4    : constant int32 := 98;
  c8    : constant int32 := 181;
  s10   : constant int32 := 213;
  c10   : constant int32 := 142;
  c8s2  : constant int32 := 35;
  c8c2  : constant int32 := 178;
  c8s10 : constant int32 := 151;
  c8c10 : constant int32 := 101;

  a0,a1,a2,a3,a4,a5,a6,a7 : int32;
  b0 : int32;
 
  begin  -- vertical_dct_inverse
    a0 := ((incol(0) + incol(4)) * c8) / 512;
    a1 := ((incol(0) - incol(4)) * c8) / 512;
    a2 := (incol(2)*s4 - incol(6)*c4) / 512;
    a3 := (incol(2)*c4 + incol(6)*s4) / 512;
    a4 := (incol(1)*s2 + incol(5)*s10 - incol(3)*c10 - incol(7)*c2) 
            / 512;
    a5 := (incol(1)*c8s2 - incol(5)*c8s10 + incol(3)*c8c10 - incol(7)*c8c2) 
            / 512;
    a6 := (incol(1)*c8c2 - incol(5)*c8c10 + incol(7)*c8s2 - incol(3)*c8s10) 
            / 512;
    a7 := (incol(1)*c2 + incol(5)*c10 + incol(3)*s10 + incol(7)*s2) 
            / 512;

    b0 := a0 + a3; 
    outcol(0) := b0 + a7;
    outcol(7) := b0 - a7;           -- From here on, a7 is available
                                    -- for temp storage
    a3 := a0 - a3;
    outcol(3) := a3 + a4;
    outcol(4) := a3 - a4;
    b0 := a1 + a2; 
    a2 := a1 - a2;
    a7 := a6 - a5; 
    a6 := a6 + a5;

    outcol(1) := b0 + a6;
    outcol(2) := a2 + a7;
    outcol(5) := a2 - a7;
    outcol(6) := b0 - a6;

  end vertical_dct_inverse; 

begin  -- two_dct_inverse
  declare
    virtual_zero : constant int32 := 8;
    rounding     : constant int32 := 2 * virtual_zero;
    intermediate_block : int32_coeff_block;
    column, row : int32_coeff_row;
  begin
    for i in inblock'range loop
      horizontal_dct_inverse(inblock(i),intermediate_block(i));
------------------------------------------------------------
--	  for j in intermediate_block(i)'range loop
--		TEXT_IO.PUT (int32'image(intermediate_block(i)(j)));
--	  end loop;
--	  TEXT_IO.NEW_LINE;
------------------------------------------------------------
    end loop;
    for i in outblock'range loop
--    declare
--      outrow : coeff_row renames outblock(i);
    begin
      for j in column'range loop
        column(j) := intermediate_block(j)(i);
      end loop;
      vertical_dct_inverse(column,row);
------------------------------------------------------------
--	  for j in row'range loop
--		TEXT_IO.PUT (int32'image(row(j)));
--	  end loop;
--	  TEXT_IO.NEW_LINE;
------------------------------------------------------------
      for j in row'range loop
        if row(j) >= virtual_zero then
          outblock(i)(j) := int16((row(j) + virtual_zero) / rounding);
        elsif row(j) <= (-virtual_zero) then
          outblock(i)(j) := int16((row(j) - virtual_zero) / rounding);
        else
          outblock(i)(j) := int16(0);
        end if;
      end loop;
    end;
    end loop;
  end;
end twod_dct_inverse;
----------------------------------------------------------------------
--ABSTRACT--REORDER
--	Reorder the coefficients from the 8*8 int16 block in approximately
--	decreasing magnitude, using a fixed permutation. Quantize the result
--	with a specified q_step. Except the (0)(0) value, which is treated
--	differently in the rest of the compression algorithm.

--KEYWORDS--REORDER
--	compression, reorder, quantization, Estec-B

--CONTENTS--REORDER
--version:		1.1
--date:			dec 1989
--author:		Tom Wiley
--machine:		no dependency
--compiler:		no dependency
--changes:		1.2 - Martien de Jong 15-08-90
--					Folded the 'flatten' procedure into the reordering
--					using a stepping index and a renaming declaration, 
--					which is fast and portable.
--					Arranged to use the same permutation array as
--					reorderinv, and put this array in a separate
--					package.
--					Changed the mapping of negative numbers to 	
--					(1,3,5,7...) in stead of (3,5,7,9,...). 
----------------------------------------------------------------------



with INTEGER_TYPES; use INTEGER_TYPES;
with reorder_constants; use reorder_constants;

separate(image_compression)
procedure reorder_quantize (q_step : in int16; inblock : in coeff_block;
                            dc_coeff : out int16;
                            outarray : out linear_block) is

-- First we rearrange the elements of `inblock' in approximately
-- decreasing order of size.  Then the DC coefficient is saved in its
-- entirety (because it contains the most information), whereas the
-- others are integer-divided by q_step, to knock of some of their
-- least significant bits.  Non-negative numbers are then mapped onto
-- non-negative even numbers, and negative numbers mapped onto positive
-- odd numbers.

    quantized_value : int16;
    p : int16;
begin
    p := 0;
    for i in inblock'range loop
    declare
        row: coeff_row renames inblock(i);
    begin
        for j in row'range loop
            quantized_value := 2 * (row(j) / q_step);
            if quantized_value < 0 then 
                outarray(zigzag(p)) := -(quantized_value + 1); 
            else
                outarray(zigzag(p)) := quantized_value;
            end if;
            p := p + 1;
        end loop;
    end;
    end loop;
    quantized_value := 2 * inblock(0)(0);
    outarray(outarray'first) := 0;
    if quantized_value < 0 then
        dc_coeff := - (quantized_value + 1);
    else
        dc_coeff := quantized_value;
    end if;
end reorder_quantize;
----------------------------------------------------------------------
--ABSTRACT--REORDERINV
--	Dequantize and re-reorder the values extracted from the compressed
--	data.

--KEYWORDS--REORDERINV
-- decompression, inverse reordering, dequantization, Estec-B

--CONTENTS--REORDERINV
--version:		1.1
--date:			dec 1989
--author:		Tom Wiley
--machine:		no dependency
--compiler:		no dependency
--changes:		1.2 - Martien de Jong 15-08-90
--					Changed this stage to conform to the altered mapping
--					of REORDER.
--					Made a double loop for addressing the outputblock,
--					using a renaming declaration and an index stepping
--					through the input array and the permutation array.
--					This strength-reduces the address calculation.
--					Implemented a rounding mechanism, approximately
--					halving the average quantization error.
----------------------------------------------------------------------


with INTEGER_TYPES; use INTEGER_TYPES;
with reorder_constants; use reorder_constants;

separate(image_compression) 
procedure reorder_quantize_inverse (q_step : in int16; dc_coeff : in int16; 
                                    inline : in linear_block;
                                    outblock : out coeff_block) is
	------------------------------------------------------------	
	-- Does the inverse of procedure `reorder_quantize'
	-- The dc coefficient has been set to zero by the inverse
	-- bitmanipulation. It is dequantized in the main loop, but overwritten
	-- with the correct value in the end.
	------------------------------------------------------------	

    value,p : int16;
	rounding: int16 := q_step/2;
begin
    p := 0;
    for i in outblock'range loop
--        declare
--            row : coeff_row renames outblock(i);
        begin
            for j in outblock(i)'range loop
                value := inline(zigzag(p));
				if value = 0 then
					outblock(i)(j) := 0;
                elsif value rem 2 = 0 then
                    outblock(i)(j) := q_step * (value / 2) + rounding;
                else
                    outblock(i)(j) := q_step * ((value + 1) / (-2)) - rounding;
                end if;
                p := p + 1;
            end loop;
        end;
    end loop;

    if dc_coeff rem 2 = 0 then
        outblock(0)(0) := dc_coeff / 2;
    else 
        outblock(0)(0) := (dc_coeff + 1) / (-2);
    end if;
end reorder_quantize_inverse;
----------------------------------------------------------------------
--ABSTRACT--BITMANIP
--	Accept a block of values and code it according to the Chaturvedi
--	algorithm.

--KEYWORDS--BITMANIP
--	compression, bitmanipulation, Chaturvedi algorithm, Estec-B

--CONTENTS--BITMANIP
--version:		1.1
--date:			dec 1989
--author:		Tom Wiley
--machine:		no dependency
--compiler:		no dependency
--changes:		1.2 - Martien de Jong 15-08-90
--					Implemented a lookup table for the powers of two,
--					and used this to write inline code for the shifting 
--					and masking.
--					Eliminated the conversion to a block of segments.
--					Moved some invariant code out of loops.
--					NOTE: This module could be improved and made more
--					readable, but I did not have the time. The ideas are
--					laid down in the C version of Estec-B.
--				MdJ 28-11-90
--					Took the debugging code out and put it in a separate
--					package.
----------------------------------------------------------------------



with INTEGER_TYPES, BIT_MANIPULATION_CONSTANTS; 
use INTEGER_TYPES, BIT_MANIPULATION_CONSTANTS;
-- with BITMANIP_DEBUG;

separate(image_compression)
procedure bitmanip (inblock : in linear_block; dc_coeff : in int16;
                    result : out compressed_data;
                    number_of_bytes : out int16) is

----------------------------------------------------------------------
-- Takes as input the DC coefficient and an array of other quantized
-- coefficients, arranged approximately in descending order of size.
-- Should return this data coded according to the Chaturvedi algorithm
-- in `result'.  `Number_of_bytes' should contain the number of bytes
-- of `result' which have been assigned information.
--
-- The DC coefficient is coded first, always being assigned 
-- dc_coeff_max_bits bits.  This is because it is usually much bigger
-- than the others (it tends to be big anyway, and it is not quantized
-- like the others).  The other coefficients are divided into
-- `number_of_segments' chunks, each containing `segment_size' integers.
-- All the integers in a chunk should be approximately the same size.
-- We work out the number of significant bits of the largest integer in
-- each chunk.  We call the set of these numbers `bitmap'.  Each 
-- coefficient is then represented in `result' by only the number of bits
-- specified in `bitmap'.  Basically we are throwing away most of the
-- leading zeros.  We are now left with the problem of coding the values
-- in `bitmap', so that we can retrieve the leading zeros of the
-- coefficients when we do the inverse noiseless coding.  This is what
-- happens in the procedure `code_bitmap', and is by far the fiddliest
-- part of the whole compression process.
----------------------------------------------------------------------


function significant_bits (number : in nat16) return nat16 is
-- returns the number of significant bits in 'number'

lookup : array(nat16 range 0..15) of int16 := 
          (0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4);
  -- number of significant bits in a 4-bit number.

begin
  if number <= 15 then 
    return lookup(number);
  else
    return (4 + significant_bits(number / 16));
  end if;
end significant_bits;


procedure find_bitmaps (in_block : in linear_block; 
                        bitmap, bitmap_of_bitmap : out bitmap_type) is
-- Creates the array `bitmap', containing the number of significant bits of
-- the maximum integer in each segment, and `bitmap_of_bitmap', containing
-- the number of significant bits of each element of `bitmap'.
-- I retain the terminology bitmap simply because of its presence in the
-- original, not because I think it is actually meaningful.

    max : int16;
    linear_index: int16;
begin
    -- note that 'reorder' has set the first coefficient to 0, so
    -- that we do not have to make an exception for this.
    linear_index := in_block'first;
    for bitmap_index in reverse bitmap'range loop
    -- note: max(sigbits(segment(i))) = sigbits(max(segment(i)))
        max := in_block(linear_index);
        for i in linear_index+1 .. linear_index+segment_size-1 loop
            if in_block(i) > max then
                max := inblock(i);
            end if;
        end loop;
        max := significant_bits(max);
        bitmap(bitmap_index) := max;
        bitmap_of_bitmap(bitmap_index) := significant_bits(max);
        linear_index := linear_index + segment_size;
    end loop;
end find_bitmaps;


procedure concatenate (data_width, data_value : in int16;
                       empty_bits : in out out_unit_index; 
                       current_byte : in out byte;
                       result_pointer : in out int16;
                       result : out compressed_data) is
-- We wish to catenate the `data_width' least significant bits of `data_value'
-- onto our output string called `result'.  `result_pointer' points to the
-- next element of `result' to be filled up, current_byte is an
-- INTEGER_BYTE whose 8 usable bits will be converted into a BYTE and copied
-- into result, and `empty-bits'
-- tells us how many bits there are still to use in `current_byte'.


    bits_to_spare : int16;
begin
  if data_width > 0 then
    bits_to_spare := empty_bits - data_width;
    if bits_to_spare >= 0 then 
      -- All the bits we are adding to the result will fit in
      -- the current byte.
      current_byte := current_byte + 
              BYTE(data_value * power_2(bits_to_spare));
      empty_bits := bits_to_spare;
    else
      declare
        bits_leftover : int16 := (-bits_to_spare);
        new_dv : int16;
      begin
        current_byte := current_byte + 
                BYTE(data_value / power_2(bits_leftover));
        result(result_pointer) := BYTE(current_byte);
        result_pointer := result_pointer + 1;
        new_dv := data_value rem power_2(bits_leftover);

        -- Now fill in any complete bytes of overflow
        while bits_leftover > out_unit_max loop
            bits_leftover := bits_leftover - out_unit_max;
            result(result_pointer) := 
                    BYTE(new_dv / power_2(bits_leftover));
            result_pointer := result_pointer + 1;
            new_dv := new_dv rem power_2(bits_leftover);
        end loop;

        -- Now the last piece of leftover, guaranteed less than
        -- out_unit_max bits of it.
        empty_bits := out_unit_max - bits_leftover;
        current_byte := BYTE(new_dv * power_2(empty_bits));
      end;
    end if;
  end if; 
end concatenate;

procedure send_coefficients (bitmap : in bitmap_type; 
                             coefficients : in linear_block;
                             empty_bits : in out out_unit_index;
                             current_byte : in out byte;
                             result_pointer : in out int16;
                             result : out compressed_data) is
-- Calls 'concatenate' on all the coefficients (except the first, since
-- the DC coefficient has already been send), getting the appropriate
-- `data_width' from `bitmap'.  It should be remembered that `bitmap'
-- was created in reverse order from `coefficients'.
    index:int16;
begin
    -- the dc coefficient should not be sent; it has been sent already.
    for i in coefficients'first+1 .. 
                coefficients'first+segment_size-1 loop
        concatenate(bitmap(bitmap'last), coefficients(i), empty_bits,
                current_byte, result_pointer, result);
    end loop;

    index := coefficients'first+segment_size;
    for bitmap_index in reverse bitmap'first .. bitmap'last-1 loop
    declare
        current_bitmap: int16 renames bitmap(bitmap_index);
    begin
        if current_bitmap /= 0 then
            for i in index..index+segment_size-1 loop
                concatenate(current_bitmap, coefficients(i), empty_bits,
                      current_byte, result_pointer, result);
            end loop;
        end if;
        index := index + segment_size;
    end;
    end loop;
    -- flush out the leftover bits
    result(result_pointer) := BYTE(current_byte);
end send_coefficients;

procedure code_bitmap (bitmap_of_bitmap, bitmap : in bitmap_type;
                       empty_bits : in out out_unit_index;
                       current_byte : in out byte;
                       result_pointer : in out int16;
                       result : out compressed_data) is

impossible_address : constant int16 := -1;

        
procedure code_bitmap_addresses (bitmap_of_bitmap : in bitmap_type;
                                 addresses : in out bitmap_address_type) is
-- `addresses(x)' will contain `impossible_address' if x does not appear
-- anywhere in `bitmap_of_bitmap'.  Otherwise, if the first occurrence
-- of x is in 'bitmap_of_bitmap(y)', we let `addresses(x) := y'.
-- Remember that `bitmap_of_bitmap' is arranged approximately in
-- ascending order of size.

biggest_so_far : nat16 := 0;
begin
  addresses(addresses'last) := number_of_segments;
    -- others are already initialised to impossible_address
  for i in bitmap_of_bitmap'range loop
    if bitmap_of_bitmap(i) > biggest_so_far then
      biggest_so_far := bitmap_of_bitmap(i);
      addresses(biggest_so_far - 1) := i;
    end if;
  end loop;
  for i in addresses'first..addresses'last-1 loop
    if addresses(i) = impossible_address then
      concatenate(1,0,empty_bits,current_byte,result_pointer,result);
    else
      concatenate(log2_num_seg+1,(addresses(i)+msb_number_of_segments),
                  empty_bits,current_byte,result_pointer,result);
    end if;
  end loop;
end code_bitmap_addresses;

procedure code_bitmap_values (bitmap : in bitmap_type; 
                              addresses : in bitmap_address_type) is
-- `addresses(X)' gives the index of the first element of `bitmap' which
-- needs 2**X bits to represent it.  If `addresses(X)' contains 
-- `impossible_value', then there is no such element in `bitmap'.
-- This first element can be coded with (2**X)-1 bits, because we know 
-- that the first bit must be a `1'.  Subsequent bitmap values up to the
-- indicated by `addresses(X+1)' are encoded with X bits.

pointer : int16 := 0;
begin
  while addresses(pointer) = impossible_address loop
    pointer := pointer + 1;
  end loop;
    -- Skip the ones that aren't there
  declare
    current_width : int16 := pointer + 1;
    last_address : int16 := addresses(pointer);
  begin
    for i in pointer+1..addresses'last loop
      if addresses(i) /= impossible_address then
			concatenate(current_width-1,
						bitmap(last_address) - power_2(current_width-1),
                		empty_bits, current_byte, result_pointer, result);
        for j in last_address+1..addresses(i)-1 loop
			concatenate(current_width, 
						bitmap(j), 
                		empty_bits, current_byte, result_pointer, result);
        end loop;
        current_width := i + 1;
        last_address := addresses(i);
      end if;
    end loop;
  end;
end code_bitmap_values;

begin
  declare
    addresses : bitmap_address_type := (others => impossible_address);
  begin
    code_bitmap_addresses(bitmap_of_bitmap, addresses);
    code_bitmap_values(bitmap, addresses);
  end;
end code_bitmap;


begin
  declare
    b_map : bitmap_type;
    b_b_map : bitmap_type;
  begin
    find_bitmaps(inblock,b_map,b_b_map);
    declare
      empty_bits : out_unit_index := out_unit_max;
      result_pointer : int16 := 0;
      current_byte : byte := 0;
    begin
      concatenate(dc_coeff_max_bits, dc_coeff, empty_bits,
          current_byte, result_pointer, result);
      code_bitmap(b_b_map, b_map, empty_bits, current_byte, 
          result_pointer, result);
      send_coefficients(b_map,inblock,empty_bits,current_byte,
            result_pointer, result);
      number_of_bytes := result_pointer + 1;
    end;
  end;
end bitmanip;
----------------------------------------------------------------------
--ABSTRACT--BITMANIPINV
--	Extract values from a compressed image coded with the Chaturvedi
--	algorithm.

--KEYWORDS--BITMANIPINV
--	decompression, Chaturvedi algorithm, Estec-B

--CONTENTS--BITMANIPINV
--version:		1.1
--date:			dec 1989
--author:		Tom Wiley
--machine:		no dependency
--compiler:		no dependency
--changes:		1.2 - Martien de Jong 15-08-90
--					Used a lookup table for the powers of two to write
--					inline code for the shifting and masking.
--				MdJ 28-11-90
--					Took the debugging code out and put it in a separate
--					package.
----------------------------------------------------------------------



-- This is the file bitmanip_inv.ada

with INTEGER_TYPES,BIT_MANIPULATION_CONSTANTS; 
use INTEGER_TYPES,BIT_MANIPULATION_CONSTANTS; 

-- with BITMANIP_DEBUG;

separate(image_compression)

procedure bitmanip_inverse (in_data : in compressed_data; 
                            number_of_bytes : in int16; 
                            dc_coeff : out int16;
                            out_block : out linear_block) is
-- Precisely the inverse of procedure `bitmanip'.

function make_int16 (byte_number : in byte) return int16 is
-- Converts type `byte' (with 8 bits) to type `integer_byte',
-- which is an integer subtype.
-- Why is this function necessary ? Why, instead of writing
--   current_byte := int_byte(byte_var);     -- assignment 1
-- in the procedure unstring, do we not just write
--   current_byte := INTEGER_BYTE(byte_var); -- assignment 2
-- The baffling thing is that the program runs perfectly when
-- we use assignment 1, but crashes with CONSTRAINT_ERROR when we use
-- assignment 2, when byte_var has its most significant bit set to 1.
-- For some reason, if byte_var = 133 (i.e. 10000101), assignment 1
-- evaluates this as 133, but assignment 2 evaluates it as -123, which
-- is outside the range of the subtype `integer_byte'. There seems
-- to be a compiler bug which means that the type conversion does
-- not work when applied directly to an element of an array.  If
-- you first assign the array element to a scalar variable (which
-- you implicitly do with the function call), it works OK.

begin
  return INT16(byte_number);
end make_int16;

procedure unstring (in_data : in compressed_data; data_width : in int16;
                    bits_remaining : in out out_unit_index; 
                    current_word : in out int16; result : out int16) is
-- Returns the next 'data_width' bits of the compressed_data stream as an
-- integer in 'result'.  Updates bit_pointer and word_pointer.

extra_bits, first_part : int16;
current_byte : int16 := make_int16(in_data(current_word));
-- current_byte : integer_byte := INTEGER_BYTE(in_data(current_word));
begin
  if data_width <= 0 then
    result := 0;
  else
    extra_bits := data_width - bits_remaining;
    if extra_bits <= 0 then
      -- we just need data from the current word
      bits_remaining := (-extra_bits);
      result := (current_byte / power_2(bits_remaining))
                    rem
                power_2(data_width);
    else
      -- we need to take data from the next word as well.
        first_part :=   (current_byte rem power_2(bits_remaining))
                            *
                        power_2(extra_bits);
        current_word := current_word + 1;

      -- Now take any complete words of extra stuff
      while extra_bits > out_unit_max loop
        extra_bits := extra_bits - out_unit_max;
        first_part := first_part + 
                  make_int16(in_data(current_word)) * power_2(extra_bits);
        current_word := current_word + 1;
      end loop;

      -- Now the last few bits
      current_byte := make_int16(in_data(current_word));
      bits_remaining := out_unit_max - extra_bits;
      result := first_part + (current_byte / power_2(bits_remaining));
    end if;
  end if;
end unstring;

procedure get_coefficients (in_data : in compressed_data; 
                            bitmap : in bitmap_type; bits_remaining : in out 
                            int16; current_word : in out int16;
                            out_block : out linear_block) is
-- Uses the information in bitmap and the procedure unstring to retrieve all
-- the coefficients together with their leading zeros.

begin
  for i in out_block'first+1..out_block'last loop
    unstring(in_data, bitmap(number_of_segments-1 - (i/segment_size)),
             bits_remaining, current_word, out_block(i));
  end loop;

  -- this simplifies the reorderinv part
  out_block(out_block'first) := 0;  
end get_coefficients;

procedure decode_bitmap (in_data : in compressed_data; bits_remaining : in out
                         int16; current_word : in out int16; 
                         bitmap : out bitmap_type) is

procedure decode_bitmap_addresses (in_data : in compressed_data; 
                                   bits_remaining : in out int16; 
                                   current_word : in out int16;
                                   bitmap_of_bitmap : out bitmap_type) is
-- If we see a zero, there are no bitmaps with the corresponding number
-- of bits. If we see a one, the next four bits give the bitmap.
-- Assumes bitmap_of_bitmap is initialised to all zeros.

info : int16;
begin
  for i in 0..max_bits_in_bitmap_of_bitmap-1 loop
    unstring(in_data,1,bits_remaining,current_word,info);
    if info = 1 then
      unstring(in_data,log2_num_seg,bits_remaining,current_word,info);
      bitmap_of_bitmap(info) := i+1;
    end if;
  end loop;
end decode_bitmap_addresses;

procedure decode_bitmap_values (in_data : in compressed_data; bitmap_of_bitmap :
                                in bitmap_type; bits_remaining : in out int16;
                                current_word : in out int16;
                                bitmap : out bitmap_type) is
-- bitmap_of_bitmap only contains a positive value for the first occurrence
-- of a value of a particular width.  All other values in bitmap_of_bitmap are
-- zero.  We use width to remember this positive value.  The first encoded
-- bitmap value has its msb knocked off, so we use width-1 to extract this one.
-- It is an important fact that it in general, both bitmap and bitmap_of_bitmap
-- tend to contain increasing series of numbers.

width : int16 := 0;
value_minus_msb : int16;
begin
  for i in bitmap'range loop
    if bitmap_of_bitmap(i) > 0 then
      width := bitmap_of_bitmap(i);
      unstring(in_data,width-1,bits_remaining,current_word,value_minus_msb);
      bitmap(i) := value_minus_msb + power_2(width-1);
    else
      unstring(in_data,width,bits_remaining,current_word,bitmap(i));
    end if; 
  end loop;
end decode_bitmap_values;

begin
  declare
    bitmap_of_bitmap : bitmap_type := (others => 0);
  begin
    decode_bitmap_addresses(in_data, bits_remaining, current_word,
                            bitmap_of_bitmap);
    decode_bitmap_values(in_data, bitmap_of_bitmap, bits_remaining,
                         current_word, bitmap);
  end;
end decode_bitmap;

begin
  declare
    bits_remaining : int16 := out_unit_max;
    current_word : int16 := in_data'first;
    bitmap : bitmap_type;
  begin
    unstring(in_data,dc_coeff_max_bits,bits_remaining,
             current_word,dc_coeff);
    decode_bitmap(in_data,bits_remaining,current_word,bitmap);
    get_coefficients(in_data,bitmap,bits_remaining,current_word,out_block);
  end;
end bitmanip_inverse;
----------------------------------------------------------------------
--ABSTRACT--IC_DEBUGGING
--	Package specification. Contains some supporting routines for 
--	debugging purposes; print the results of various stages.

--KEYWORDS--IC_DEBUGGING
--	Estec-B, debugging routines

--CONTENTS--IC_DEBUGGING
--version:		1.1
--date:			dec 1989
--author:		Tom Wiley
--machine:		no dependency
--compiler:		no dependency
--changes:		
----------------------------------------------------------------------



with image_compression;
with integer_types;
package ic_debugging_tools is

function rms_error (original, reconstructed : in IMAGE_COMPRESSION.coeff_block)
            return float;

end ic_debugging_tools;
----------------------------------------------------------------------
--ABSTRACT--IC_DEBUGGING
--	Package body. Supporting routines for debugging purposes

--KEYWORDS--IC_DEBUGGING
--	Estec-B, debugging

--CONTENTS--IC_DEBUGGING
--version:		1.1
--date:			dec 1989
--author:		Tom Wiley
--machine:		no dependency
--compiler:		no dependency
--changes:		1.2 - Martien de Jong 15-08-90
--					Added a sqrt function for independency.
--					Corrected an error in the rms_error procedure; it
--					used the side of the block in stead of the total
--					number of elements.
----------------------------------------------------------------------



with INTEGER_TYPES; use INTEGER_TYPES;
with image_compression;

package body ic_debugging_tools is

function rms_error (original, reconstructed : in IMAGE_COMPRESSION.coeff_block)
            return float is

sum_of_squares : int16 := 0;
number_of_samples : int16 := IMAGE_COMPRESSION.block_size;

function square (x : in int16) return int16 is

begin
  return x * x;
end square;

function sqrt (x : in FLOAT) return FLOAT is
    lx,y,yred:FLOAT;
begin
    if x<=0.0 then
        return 0.0;
    else
        lx := x;
        yred := 1.0;
        while lx>2.0 loop
            lx := lx * 0.25;
            yred := yred * 2.0;
        end loop;
        while lx<0.5 loop
            lx := lx * 4.0;
            yred := yred * 0.5;
        end loop;
        y := yred * (3.0 - 8.0 / (lx + 3.0));   -- eps = 1.0e-2
        y := (y + x/y) / 2.0;                   -- eps = 5.0e-4
        y := (y + x/y) / 2.0;                   -- eps = 1.3e-9
        return (y + x/y) / 2.0;                 -- eps = 8.8e-19
    end if;
end sqrt;

begin
  for i in original'range loop
    for j in original(i)'range loop
      sum_of_squares := sum_of_squares + square(original(i)(j) - 
                        reconstructed(i)(j));
    end loop;
  end loop;
  return sqrt(FLOAT(sum_of_squares) / FLOAT(number_of_samples));
end rms_error;

end ic_debugging_tools;
----------------------------------------------------------------------
--ABSTRACT--TESTDATA
--	Some real image blocks for testing the compression and decompression
--	routines

--KEYWORDS--TESTDATA
--	image compression, Estec-B

--CONTENTS--TESTDATA
--version:		1.0
--date:			15-08-90
--author:		Martien de Jong
--machine:		no dependency
--compiler:		no dependency
--changes:
----------------------------------------------------------------------



with image_compression; use image_compression;

package testdata is

test_data: array (1..4) of coeff_block := (
(
    (  13,  13,  13,  13,   4,   9,  11,  13),
    (  13,  13,  13,  13,   3,   9,  11,  13),
    (  13,  13,  13,  13,   5,  10,  11,  13),
    (  13,  13,  13,  13,   4,   9,  11,  13),
    (  13,  13,  13,  13,   4,   9,  11,  13),
    (  13,  13,  13,  13,   3,   9,  11,  13),
    (  13,  13,  13,  13,   5,  10,  11,  13),
    (  13,  13,  13,  13,   4,   9,  11,  13)
),(
    (   2,   1,   1,   2,   6,   5,   4,   3),
    (   5,   2,   1,   1,  17,  19,  14,  10),
    (   5,   3,   3,   3,  11,  10,   8,   6),
    (   7,   4,   2,   2,  15,  16,  13,  11),
    (   3,   1,   1,   2,  10,   9,   6,   5),
    (   5,   2,   1,   2,  11,  13,  10,  10),
    (   5,   3,   3,   3,  13,  13,   9,   7),
    (   6,   4,   3,   3,  13,  13,  12,  10)
),(
    (  39,  34,  11,   2,  29,  34,  30,  36),
    (  31,  50,  32,   7,  32,  26,  28,  30),
    (  36,  36,  25,   5,  35,  25,  27,  41),
    (  25,  24,  11,   9,  23,  25,  28,  22),
    (  25,  27,  18,   5,  27,  18,  16,  27),
    (  34,  30,   8,   5,  24,  22,  19,  26),
    (  39,  42,  29,   5,  38,  34,  29,  36),
    (  25,  28,  16,   6,  29,  24,  11,  17)
),(
    (  15,  23,  26,  23,  31,  19,  21,  19),
    (  28,  32,  36,  42,  19,  29,  32,  27),
    (  17,  23,  28,  31,  35,  26,  28,  21),
    (  36,  40,  41,  33,  21,  18,  24,  32),
    (  14,  21,  21,  22,  16,  10,  18,  15),
    (  24,  27,  35,  33,  28,  17,  22,  27),
    (  24,  28,  30,  31,  29,  22,  25,  26),
    (  28,  36,  34,  25,  42,  38,  32,  26)
));

end testdata;

package body testdata is
end testdata;




----------------------------------------------------------------------
--ABSTRACT--TESTPROC
--	Main program for benchmark purposes

--KEYWORDS--TESTPROC
--	test, benchmark, Estec-B

--CONTENTS--TESTPROC
--version:		1.0
--date:			15-08-90
--author:		Martien de Jong
--machine:		no dependency
--compiler:		no dependency
--changes:
----------------------------------------------------------------------



with TEXT_IO; use TEXT_IO;
with INTEGER_TYPES; use INTEGER_TYPES;
with image_compression; use image_compression;
with ic_debugging_tools;use ic_debugging_tools;
with CALENDAR; use CALENDAR;
with testdata; use testdata;

procedure estecb is
        package Flt_Io is new Float_IO (Float); use Flt_Io;
        package Int_Io is new Integer_IO (Integer); use Int_Io;

    input_block, output_block : coeff_block;
    q_step : int16;
    number_of_bytes : int16;
    compressed_image : compressed_data(0..127);
    error,errorbound : float;
    char : character;
    sqrt_3 : constant FLOAT := 1.7320508;
    iterations : constant := 100;
    start_time, stop_time : time;
    tot_size : int16 := 0;
    tot_err : float := 0.0;
begin -- main
    TEXT_IO.PUT_LINE("Running ESTEC-B image compression ...");
    start_time := clock;
    for i in 1..2 loop
        q_step := int16 (4 * i);
        errorbound := FLOAT(q_step) / (2.0 * sqrt_3);
        for block_index in test_data'range loop
            input_block := test_data(block_index);
	    for j in 1..iterations loop
                compress(input_block, q_step, number_of_bytes, compressed_image);
                decompress(compressed_image, q_step, number_of_bytes, output_block);
	    end loop;
	    tot_size := tot_size + number_of_bytes;
            tot_err := tot_err + IC_DEBUGGING_TOOLS.rms_error(input_block,output_block);
        end loop;
    end loop;
    stop_time := clock;
    PUT(ITEM => iterations, WIDTH => 2);
    TEXT_IO.PUT(" iterations executed in ");
    put(Float(stop_time - start_time));
    TEXT_IO.PUT(" seconds ( ");
    put( (Float(stop_time - start_time))/Float(iterations));
    TEXT_IO.PUT_LINE(" s/iteration )");
    if (tot_size /= 324) or (tot_err <= 1.9829) or (tot_err >= 1.9830) then
	put("WARNING: errors during compression detected");
    end if;
    TEXT_IO.NEW_LINE;
end estecb;





----------------------------------------------------------------------
--ABSTRACT--TESTPROC
--	Main program for benchmark purposes

--KEYWORDS--TESTPROC
--	test, benchmark, Estec-B

--CONTENTS--TESTPROC
--version:		1.0
--date:			15-08-90
--author:		Martien de Jong
--machine:		no dependency
--compiler:		no dependency
--changes:
----------------------------------------------------------------------



with INTEGER_TYPES; use INTEGER_TYPES;
with image_compression; use image_compression;
with testdata; use testdata;

procedure size_estecb is

    input_block, output_block : coeff_block;
    q_step : int16;
    number_of_bytes : int16;
    compressed_image : compressed_data(0..127);
    error,errorbound : float;
    char : character;
    sqrt_3 : constant FLOAT := 1.7320508;
    iterations: constant := 100;
begin -- main
    for i in 1..2 loop
        q_step := int16 (4 * i);
        errorbound := FLOAT(q_step) / (2.0 * sqrt_3);
        for block_index in test_data'range loop
            input_block := test_data(block_index);
	    for j in 1..iterations loop
                compress(input_block, q_step, number_of_bytes, compressed_image);
                decompress(compressed_image, q_step, number_of_bytes, output_block);
	    end loop;
        end loop;
    end loop;
end size_estecb;
 procedure kalman_body is


  BLANK : constant CHARACTER := ' ';
  ICC : constant CHARACTER := 'C';

  type MATRIX is array (1..10,1..10) of FLOAT;
  type ARYTYPE is array (1..10) of FLOAT;

  TEMPOUTPUT : INTEGER;
  A,B,C,R,Q,K,D,F,G,E,H,S,SR,SQ,SD : MATRIX;
  X : ARYTYPE;
  EPS,TIMER,PTS,XX,DI,TEMP,ALC,T,SUM : FLOAT;
  I,J,II,N,M,L,NR,NQ,LC,ID,ICH,IT,OPT : INTEGER;
  OPTION : ARRAY (1..3) of CHARACTER;
  T1,T2 : FLOAT;
  NPT,IER,ITER,ITERATIONS : INTEGER;
  JUMPOUT : BOOLEAN;



 procedure SIMEQ(A : in MATRIX; KC : in INTEGER; AINV : in out MATRIX;
                 IERR : in out INTEGER) is

   B : MATRIX;
   N,I,K,COMP,M,J : INTEGER;
   TEMP : FLOAT;

  begin
   N := 1;
   IERR := 1;
   for I in 1..KC loop
    for J in 1..KC loop
      AINV(I,J) := 0.0;
      B(I,J) := A(I,J);
    end loop;
   end loop;
   for I in 1..KC loop
     AINV(I,I) := 1.0;
   end loop;
   for I in 1..KC loop
     COMP := 0;
     K := I;
     << REPEAT_1 >>
     if abs(B(K,I)) <= FLOAT(abs(COMP)) then
      K := K + 1;
     else
       COMP := INTEGER(B(K,I));
       N := K;
       K := K + 1;
     end if;
     if not (K > KC) then
       goto REPEAT_1;
     end if;
     if (B(N,I) /= 0.0) or ((N-I) >= 0) then
       if N-I > 0 then
         for M in 1..KC loop
           TEMP := B(I,M);
           B(I,M) := B(N,M);
           B(N,M) := TEMP;
           TEMP := AINV(I,M);
           AINV(I,M) := AINV(N,M);
           AINV(N,M) := TEMP;
         end loop;
       end if;
       TEMP := B(I,I);
       for M in 1..KC loop
         AINV(I,M) := AINV(I,M) / TEMP;
         B(I,M) := B(I,M) / TEMP;
       end loop;
       for J in 1..KC loop
        if (J /= I) and (B(J,I) /= 0.0) then
          TEMP := B(J,I);
          for N in 1..KC loop
            AINV(J,N) := AINV(J,N) - TEMP * AINV(I,N);
            B(J,N) := B(J,N) - TEMP * B(I,N);
          end loop;
        end if;
       end loop;
     else                               -- print 'matrix singular'
       IERR := 0;
       exit;
     end if;
   end loop;
  end SIMEQ;

 begin                          -- kalman main
  ITERATIONS := 1;
  OPTION(1) := 'C'; OPTION(2) := 'F'; OPTION (3) := ' ';
  N := 2; M := 2; L := 2;
  A(1,1) := -1.0; A(1,2) := 0.0; A(2,1) := 0.0; A(2,2) := -2.0;
  B(1,1) := 1.0; B(1,2) := 0.0; B(2,1) := 0.0; B(2,2) := 1.0;
  C(1,1) := 1.0; C(1,2) := 0.0; C(2,1) := 0.0; C(2,2) := 2.0;
  OPT := 1;
  << REPEAT_2 >>
  if OPTION(OPT) /= BLANK then
    T1 := 0.0; T2 := 0.0; NPT := 0;
    if OPTION(OPT) = ICC then                           -- {control option}
      NR := M;
      NQ := N;
      for I in 1..N loop
        for J in 1..M loop
         E(I,J) := B(I,J);
        end loop;
        for J in 1..L loop
         SD(I,J) := C(J,I);
        end loop;
        for J in 1..N loop
         F(I,J) := A(J,I);
        end loop;
      end loop;
    else                                                -- {filter option}
      NR := L;
      NQ := M;
      for I in 1..n loop
        for J in 1..L loop
         E(I,J) := C(J,I);
        end loop;
        for J in 1..M loop
         SD(I,J) := B(I,J);
        end loop;
        for J in 1..N loop
         F(I,J) := A(I,J);
        end loop;
      end loop;
    end if;
                                -- read R and Q matrices

    SR(1,1) := 1.0; SR(1,2) := 0.0; SR(2,1) := 0.0; SR(2,2) := 2.0;
    SQ(1,1) := 1.0; SQ(1,2) := 1.0; SQ(2,1) := 1.0; SQ(2,2) := 1.0;

                                -- loop to increase computation time

    for ITER in 1..ITERATIONS loop              -- {big loop}
      for I in 1..NR loop
       for J in 1..NR loop
        R(I,J) := SR(I,J);
       end loop;
      end loop;
      for I in 1..NQ loop
       for J in 1..NQ loop
        Q(I,J) := SQ(I,J);
       end loop;
      end loop;
      for I in 1..N loop
       for J in 1..L loop
        D(I,J) := SD(I,J);
       end loop;
      end loop;

      SIMEQ(R,NR,H,IER);                        -- call

      for I in 1..N loop
       for J in 1..N loop
         R(I,J) := 0.0;
         for II in 1..NR loop
          R(I,J) := R(I,J) + H(I,II) * E(J,II);
         end loop;
       end loop;
      end loop;
      if OPTION(OPT) /= ICC then
        for I in 1..N loop
         for J in 1..NQ loop
           G(I,J) := 0.0;
           for II in 1..NQ loop
            G(I,J) := G(I,J) + D(I,II) * Q(II,J);
           end loop;
         end loop;
        end loop;
        for I in 1..N loop
         for J in 1..N loop
           Q(I,J) := 0.0;
           for II in 1..NQ loop
            Q(I,J) := Q(I,J) + G(I,II) * D(J,II);
           end loop;
         end loop;
        end loop;
      end if;
      if NPT <= 0 then
        for I in 1..N loop
          for J in 1..N loop
           G(I,J) := 0.0;
         end loop;
          G(I,I) := 1.0;
        end loop;
        EPS := 0.01;
      else                      -- {initial conditions}

      --  for I in 1..N loop
      --   for J in 1..N loop
      --    readln(G(I,J));
      --   end loop;
      --  end loop;

        TIMER := ABS(T2-T1);
        PTS := 200.0 * TIMER;
        XX := PTS / FLOAT(NPT);
        DI := FLOAT(INTEGER(XX));
        ID := INTEGER(XX);
        if ABS(XX-DI) > 0.05 then
         ID := ID + 1;
        end if;
        IT := INTEGER(PTS);
        EPS := 0.005;
        TIMER := T1;
        if OPTION(OPT) = ICC then
         TIMER := T2;
        end if;
                                        -- {print time here}
        for I in 1..NR loop
         for J in 1..N loop
           K(I,J) := 0.0;
           for II in 1..N loop
            K(I,J) := K(I,J) + R(I,II) * G(II,J);
          end loop;
         end loop;
        end loop;
                                                -- {print K here}
      end if;
      LC := 0;
      ICH := ID;

      << REPEAT_3 >>
      JUMPOUT := FALSE;
      for I in 1..NR loop
       for J in 1..N loop
         K(I,J) := 0.0;
         for II in 1..N loop
          K(I,J) := K(I,J) + R(I,II) * G(II,J);
        end loop;
       end loop;
      end loop;
      for I in 1..N loop
       for J in 1..N loop
         H(I,J) := 0.0;
         for II in 1..NR loop
          H(I,J) := H(I,J) + E(I,II) * K(II,J);
        end loop;
       end loop;
      end loop;
      for I in 1..N loop
       for J in 1..N loop
         D(I,J) := Q(I,J);
         for II in 1..N loop
           TEMP := F(I,II)*G(II,J) + G(I,II)*F(J,II) - G(I,II)*H(II,J);
           D(I,J) := D(I,J) + TEMP;
        end loop;
         S(I,J) := G(I,J) + D(I,J) * EPS;
       end loop;
      end loop;
      if NPT > 0 then
        LC := LC + 1;
        if LC >= ICH then
          ICH := ICH + ID;
          ALC := FLOAT(LC);
          T := ALC * EPS;
          TIMER := T1 + T;
          if OPTION(OPT) = ICC then
           TIMER := T2 - T;
         end if;
                                        -- {print time, K here}
          if LC >= IT then
           JUMPOUT := TRUE;
          end if;
        end if;
        if not JUMPOUT then
         for I in 1..N loop
          for J in 1..N loop
           G(I,J) := S(I,J);
          end loop;
         end loop;
        end if;
      else
        SUM := 0.0;
        for I in 1..N loop
         for J in 1..N loop
           SUM := SUM + abs(D(I,J));
           G(I,J) := S(I,J);
         end loop;
        end loop;
      end if;
 -- slf
 --     TEMPOUTPUT := INTEGER(SUM * 100000.0);
 --     PUT("sum * 100000 = ");
 --     INT_IO.PUT(TEMPOUTPUT);
 --     NEW_LINE;
 -- slf
      if not ((SUM <= 0.0001) or JUMPOUT) then
        goto REPEAT_3;
      end if;
    end loop;                           -- {big loop}

                                        -- {print solution K}
 -- slf
 --   for I in 1..NR loop
 --    for J in 1..N loop
 --     TEMPOUTPUT := INTEGER(K(I,J) * 1000000.0);
 --     PUT("K(");
 --     INT_IO.PUT(I);
 --     PUT(",");
 --     INT_IO.PUT(J);
 --     PUT(") * 1000000 = ");
 --     INT_IO.PUT(TEMPOUTPUT);
 --     NEW_LINE;
 --    end loop;
 --   end loop;
 -- slf

  end if;                               -- {then}
  OPT := OPT + 1;
  if OPTION(OPT) /= BLANK then
   goto REPEAT_2;
  end if;
 end kalman_body;


  with TEXT_IO; use TEXT_IO;
  with calendar; use calendar;
  with kalman_body;

 procedure kalman is

   loop_count : constant integer := 10;
   start_t : time;
   kalman_elapsed : duration;
   dummy_t : time;
   dummy_elapsed_time : duration;

 -- slf
  package INT_IO is new INTEGER_IO(INTEGER); use INT_IO;
  package FP_IO is new Text_IO.Float_IO (Float); use FP_IO;

 -- slf

 begin

   --
   -- first time the dummy loop
   --
   start_t := clock;
   for i in 1..loop_count
   loop
     dummy_t := clock;
   end loop;
   dummy_elapsed_time := clock - start_t;

 --
 -- now time the interesting operation and the dummy operation in the loop.
 --
   text_io.put_line("Running KALMAN ... ");
   start_t := clock;
   for i in 1..loop_count
   loop
     kalman_body;
   end loop;
   kalman_elapsed := clock - start_t;
 --
 -- now subtract off the dummy loop, and report the results
 --
   put(loop_count,2);
   put( " iterations executed in " );
   put( float ( kalman_elapsed - dummy_elapsed_time ) );
   put( " seconds ( " );
   put( float ( kalman_elapsed - dummy_elapsed_time ) / float(loop_count) );
   put_line( " s/iteration )" );
   new_line;
 end kalman;
generic
  type FLOAT_TYPE is digits <>;
package GEF is

  function SQRT (X           : FLOAT_TYPE) return FLOAT_TYPE;
  function LOG  (X           : FLOAT_TYPE) return FLOAT_TYPE;
  function LOG  (X, Base     : FLOAT_TYPE) return FLOAT_TYPE;
  function EXP  (X           : FLOAT_TYPE) return FLOAT_TYPE;
  function "**" (LEFT, RIGHT : FLOAT_TYPE) return FLOAT_TYPE;

  function SIN (X        : FLOAT_TYPE) return FLOAT_TYPE;
  function SIN (X, CYCLE : FLOAT_TYPE) return FLOAT_TYPE;
  function COS (X        : FLOAT_TYPE) return FLOAT_TYPE;
  function COS (X, CYCLE : FLOAT_TYPE) return FLOAT_TYPE;
  function TAN (X        : FLOAT_TYPE) return FLOAT_TYPE;
  function TAN (X, CYCLE : FLOAT_TYPE) return FLOAT_TYPE;
  function COT (X        : FLOAT_TYPE) return FLOAT_TYPE;
  function COT (X, CYCLE : FLOAT_TYPE) return FLOAT_TYPE;

  function ARCSIN (X        : FLOAT_TYPE) return FLOAT_TYPE;
  function ARCSIN (X, CYCLE : FLOAT_TYPE) return FLOAT_TYPE;
  function ARCCOS (X        : FLOAT_TYPE) return FLOAT_TYPE;
  function ARCCOS (X, CYCLE : FLOAT_TYPE) return FLOAT_TYPE;
  function ARCTAN (Y        : FLOAT_TYPE;
                   X        : FLOAT_TYPE := 1.0) return FLOAT_TYPE;
  function ARCTAN (Y        : FLOAT_TYPE;
                   X        : FLOAT_TYPE := 1.0;
                   CYCLE    : FLOAT_TYPE) return FLOAT_TYPE;
  function ARCCOT (Y        : FLOAT_TYPE;
                   X        : FLOAT_TYPE := 1.0) return FLOAT_TYPE;
  function ARCCOT (Y        : FLOAT_TYPE;
                   X        : FLOAT_TYPE := 1.0;
                   CYCLE    : FLOAT_TYPE) return FLOAT_TYPE;

  function SINH    (X : FLOAT_TYPE) return FLOAT_TYPE;
  function COSH    (X : FLOAT_TYPE) return FLOAT_TYPE;
  function TANH    (X : FLOAT_TYPE) return FLOAT_TYPE;
  function COTH    (X : FLOAT_TYPE) return FLOAT_TYPE;
  function ARCSINH (X : FLOAT_TYPE) return FLOAT_TYPE;
  function ARCCOSH (X : FLOAT_TYPE) return FLOAT_TYPE;
  function ARCTANH (X : FLOAT_TYPE) return FLOAT_TYPE;
  function ARCCOTH (X : FLOAT_TYPE) return FLOAT_TYPE;

  ARGUMENT_ERROR : exception;

end GEF;
package body GEF is

  POW2 : constant array (0 .. 127) of FLOAT_TYPE := (
    2.0**0,  2.0**1,  2.0**2,  2.0**3,  2.0**4,  2.0**5,  2.0**6,  2.0**7,
    2.0**8,  2.0**9,  2.0**10, 2.0**11, 2.0**12, 2.0**13, 2.0**14, 2.0**15,
    2.0**16, 2.0**17, 2.0**18, 2.0**19, 2.0**20, 2.0**21, 2.0**22, 2.0**23,
    2.0**24, 2.0**25, 2.0**26, 2.0**27, 2.0**28, 2.0**29, 2.0**30, 2.0**31,
    2.0**32, 2.0**33, 2.0**34, 2.0**35, 2.0**36, 2.0**37, 2.0**38, 2.0**39,
    2.0**40, 2.0**41, 2.0**42, 2.0**43, 2.0**44, 2.0**45, 2.0**46, 2.0**47,
    2.0**48, 2.0**49, 2.0**50, 2.0**51, 2.0**52, 2.0**53, 2.0**54, 2.0**55,
    2.0**56, 2.0**57, 2.0**58, 2.0**59, 2.0**60, 2.0**61, 2.0**62, 2.0**63,
    2.0**64, 2.0**65, 2.0**66, 2.0**67, 2.0**68, 2.0**69, 2.0**70, 2.0**71,
    2.0**72, 2.0**73, 2.0**74, 2.0**75, 2.0**76, 2.0**77, 2.0**78, 2.0**79,
    2.0**80, 2.0**81, 2.0**82, 2.0**83, 2.0**84, 2.0**85, 2.0**86, 2.0**87,
    2.0**88, 2.0**89, 2.0**90, 2.0**91, 2.0**92, 2.0**93, 2.0**94, 2.0**95,
    2.0**96, 2.0**97, 2.0**98, 2.0**99, 2.0**100,2.0**101,2.0**102,2.0**103,
    2.0**104,2.0**105,2.0**106,2.0**107,2.0**108,2.0**109,2.0**110,2.0**111,
    2.0**112,2.0**113,2.0**114,2.0**115,2.0**116,2.0**117,2.0**118,2.0**119,
    2.0**120,2.0**121,2.0**122,1.0E+37,1.0E+37 ,1.0E+37 ,1.0E+37 ,1.0E+37
  );

  PI : constant := 3.14159_26535_89793_23846_26;
  E  : constant := 2.71828_18284_59045_23536_02;

  FRAC_BIN_LEN    : constant := 23;
  ABS_SAFE_EXP    : constant := 125;
  INT_LARGE       : constant := 16#7FFF_FFFF#;
  SAFE_LARGE      : constant := 1.0E+37;
  SAFE_SMALL      : constant := 1.0E-37;
  LN_SAFE_LARGE   : constant := 85.1956484408;
  SQRT_SAFE_LARGE : constant := 3.16227766017E+18;
  SQRT_SAFE_SMALL : constant := 1.0 / SQRT_SAFE_LARGE;
  BIG             : constant := 2.0 ** (FRAC_BIN_LEN / 2);
  EPS_2           : constant := 1.0 / BIG;
  EPS             : constant := 1.0 / (2.0 ** FRAC_BIN_LEN);

  function FLOAT_TYPE_TRUNCATION (X : FLOAT_TYPE) return FLOAT_TYPE is
    Half : constant  := 0.5;  -- 0.4999999
  begin
    if X < 0.0 then
      return FLOAT_TYPE(INTEGER (FLOAT(X) + HALF));
    else
      return FLOAT_TYPE(INTEGER (FLOAT(X) - HALF));
    end if;
  end FLOAT_TYPE_TRUNCATION;

  function FLOAT_TYPE_FRACTION (X : FLOAT_TYPE) return FLOAT_TYPE is
  begin
     return X - FLOAT_TYPE_TRUNCATION(X);
  end;

  function FLOAT_TYPE_EXPONENT(X : FLOAT_TYPE) return integer is
    EXP: INTEGER;
    R: FLOAT_TYPE;

  begin
     --Invariant R = 10^EXP
     EXP := 0;
     R := 1.0;
     if R < abs(X) then
       while R < abs(X) loop
         R := R*10.0;
         EXP := EXP + 1;
       end loop;
       EXP := EXP - 1;
     else
       while R > abs(X) loop
         R := R/10.0;
         EXP := EXP - 1;
       end loop;
     end if;
     return EXP;
  end;

  function SQRT (X : FLOAT_TYPE) return FLOAT_TYPE is
    X_EXP    : INTEGER;
    X_RED, Y : FLOAT_TYPE;
    Diff    : FLOAT_TYPE := X;

  begin
    if abs X = 0.0 then                 -- 1.
      return 0.0;
    elsif X < 0.0 then                  -- 2.
      raise Argument_Error;             -- 3.
    end if;
    Y := X;
    while abs(Diff/Y) > 0.00001 loop
       Diff := (Y*Y - X)/(2.0*Y);
       Y := Y - Diff;
    end loop;
    return Y;
  end SQRT;

  function LOG (X : FLOAT_TYPE) return FLOAT_TYPE is
    SQRT0_5 : constant :=  0.707106781186574;
    A0      : constant := -0.5527074855;
    B0      : constant := -0.6632718214E1;
    C1      : constant :=  0.693359375;  -- Exact: 355/512
    C2      : constant := -2.121944400546905E-4;
    X_RED   : FLOAT_TYPE :=  X;
    X_EXP   : INTEGER :=  0;
    ZNUM, ZDEN, Z, W : FLOAT_TYPE;
  begin
    if X <= 0.0 then                     -- 3.
      raise ARGUMENT_ERROR;
    elsif X = 1.0 then
      return 0.0;
    end if;
    -- Reduce X into the interval [0.5, 1[
    X_RED := FLOAT_TYPE_FRACTION (X) * 0.5;
    X_EXP := FLOAT_TYPE_EXPONENT (X) + 1;
    -- 0.5 <= X < 1.
    if X_RED > SQRT0_5 then             -- 8.
      ZNUM := (X_RED - 0.5) - 0.5;      -- 10.
      ZDEN := X_RED * 0.5 + 0.5;
    else
      ZNUM := X_RED - 0.5;              -- 9.
      ZDEN := ZNUM * 0.5 + 0.5;
      X_EXP := X_EXP - 1;
    end if;
    Z := ZNUM / ZDEN;                   -- 11.
    W := Z * Z;
    return FLOAT_TYPE (X_EXP) * C2 + Z + Z * W * A0 / (B0 + W) +
      FLOAT_TYPE (X_EXP) * C1;
  end LOG;

  function LOG (X, BASE : FLOAT_TYPE) return FLOAT_TYPE is
  begin
    return LOG (X) / LOG (BASE);
  end LOG;

  function EXP (X : FLOAT_TYPE) return FLOAT_TYPE is
    LN2INV : constant :=  1.44269504088896;
    C1     : constant :=  0.693359375;   -- Exact: 355 / 512
    C2     : constant := -2.1219444005469E-4;
    P0     : constant :=  0.2499999995;
    P1     : constant :=  0.41602886268E-2;
    Q0     : constant :=  0.5;
    Q1     : constant :=  0.49987178778E-1;
    X_RED  : FLOAT_TYPE :=  X;  -- Default value.
    X_EXP  : INTEGER;
    EXP, X1, G, Z, GP : FLOAT_TYPE;
  begin
    if X > LN_SAFE_LARGE then                    -- 1.
      return SAFE_LARGE;
    elsif X < - LN_SAFE_LARGE then               -- 3.
      return 0.0;
    elsif abs X < EPS then
      return 1.0;
    end if;
    X_EXP := INTEGER (FLOAT(X_RED * LN2INV));           -- 6.
    EXP := FLOAT_TYPE (X_EXP);
    X1 := FLOAT_TYPE_TRUNCATION (X);
    G := ((X1 - EXP * C1) + X - X1) - EXP * C2;  -- 7.
    Z := G * G;                                  -- 8.
    GP := G * ( P0 + P1 * Z );
    EXP := POW2 (abs (X_EXP + 1));               -- 9.
    if X_EXP < 0 then
      return (0.5 + GP / ( Q0 + Q1 * Z - GP )) / EXP;
    end if;
    return (0.5 + GP / ( Q0 + Q1 * Z - GP )) * EXP;
  end EXP;

  function "**" (LEFT, RIGHT : FLOAT_TYPE) return FLOAT_TYPE is
    -- Table of 2 ** ((1 - I) / 16) rounded to working precision
    A1 : constant array (0 .. 16) of FLOAT_TYPE :=
      (1.0,            8#0.75222_575#, 8#0.72540_306#, 8#0.70146_336#,
       8#0.65642_374#, 8#0.63422_214#, 8#0.61263_452#, 8#0.57204_243#,
       8#0.55202_363#, 8#0.53254_076#, 8#0.51377_326#, 8#0.47572_462#,
       8#0.46033_760#, 8#0.44341_723#, 8#0.42712_701#, 8#0.41325_303#, 0.5);
    -- Table of 2 ** ((1 - 2*J ) / 16) - A1 (2*J)
    A2 : constant array (1 .. 8) of FLOAT_TYPE :=
      (8#0.05_22220#E-8, 8#0.73_02522#E-8, 8#0.05_21760#E-8, 8#0.47_65401#E-8,
       8#0.72_44124#E-8, 8#0.30_11064#E-8, 8#0.34_72542#E-8, 8#0.31_74611#E-8);
    P1     : constant := 0.83357541E-1;
    K      : constant := 0.44269504088896340736;
    Q1     : constant := 0.69314675;
    Q2     : constant := 0.2401851;
    Q3     : constant := 0.54360383E-1;
    BIGX   : constant := 2**27;
    SMALLX : constant := -BIGX;
    X_RED  : FLOAT_TYPE := LEFT;
    SGN    : FLOAT_TYPE := 1.0;
    P      : INTEGER  := 1;
    X_EXP, M_PRIM, P_PRIM, I1 : INTEGER;
    Z, R, U1, U2, Y1, W, W1, W2, EXP : FLOAT_TYPE;
  begin
    if LEFT <= 0.0 then
      if LEFT = 0.0 then
        if RIGHT > 0.0 then -- LEFT = 0.0, RIGHT > 0.0 => LEFT ** RIGHT = 0.0
          return 0.0;
        else -- LEFT = 0.0, RIGHT <= 0.0 => Division by zero
          raise CONSTRAINT_ERROR;
        end if;
      else -- LEFT < 0.0
        if RIGHT = FLOAT_TYPE (INTEGER (FLOAT(RIGHT))) then
          -- LEFT < 0, RIGHT = {INTEGER}
          if RIGHT = 2.0 * FLOAT_TYPE (INTEGER (FLOAT(RIGHT)) / 2) then
            X_RED := - LEFT; -- LEFT ** RIGHT = (-LEFT) ** RIGHT
          else -- LEFT < 0, RIGHT = {odd INTEGER}
            X_RED := - LEFT; -- LEFT ** RIGHT = -(-LEFT) ** RIGHT
            SGN := -1.0;
          end if;
        else -- LEFT < 0, RIGHT = {real but not INTEGER}
          raise ARGUMENT_ERROR;
        end if;
      end if;
    end if;
    -- Reduce X_RED into [0.5, 1[
    X_EXP := FLOAT_TYPE_EXPONENT (X_RED);
    X_RED := FLOAT_TYPE_FRACTION (X_RED);
    if X_RED <= A1 (8) then
      P := 9;
    end if;
    if X_RED <= A1 (P + 3) then
      P := P + 4;
    end if;
    if X_RED <= A1 (P + 1) then
      P := P + 2;
    end if;
    Z  := (X_RED - A1 (P)) - A2 ((P + 1) / 2);   -- 7.
    Z  := Z / (X_RED + A1 (P));
    Z  := Z + Z;
    R  := P1 * FLOAT_TYPE((FLOAT(Z) ** 3));                         -- 8.
    R  := R + K * R;
    U2 := ( R + Z * K ) + Z;
    U1 := FLOAT_TYPE (X_EXP * 16 - P) * 0.0625;       -- 9.
    Y1 := FLOAT_TYPE_TRUNCATION (16.0 * RIGHT) * 0.0625; -- Reduce RIGHT
    W  := U2 * RIGHT + U1 * (RIGHT - Y1);
    W1 := FLOAT_TYPE_TRUNCATION (16.0 * W) * 0.0625;  -- Reduce W
    W2 := W - W1;
    W  := W1 + U1 * Y1;
    W1 := FLOAT_TYPE_TRUNCATION (16.0 * W) * 0.0625;  -- Reduce W
    W2 := W2 + (W - W1);
    W  := FLOAT_TYPE_TRUNCATION (16.0 * W2) * 0.0625; -- Reduce W2
    I1 := INTEGER(FLOAT(FLOAT_TYPE_TRUNCATION (16.0 * (W1 + W))));
    W2 := W2 - W;
    if I1 > BIGX then                            -- 10.
      return SAFE_LARGE;
    elsif I1 < SMALLX then                       -- 11.
      return 0.0;
    end if;
    if W2 > 0.0 then                             -- 13.
      I1 := I1 + 1;
      W2 := W2 - 0.0625;
    end if;
    M_PRIM := I1 / 16 + 1;
    if I1 < 0 then
      M_PRIM := M_PRIM - 1;
    end if;
    P_PRIM := 16 * M_PRIM - I1;
    Z := ((Q3 * W2 + Q2) * W2 + Q1) * W2;        -- 14.
    Z := A1 (P_PRIM) + A1 (P_PRIM) * Z;          -- 15.
    EXP := POW2 (abs M_PRIM);
    if M_PRIM < 0 then
      return SGN * (Z / EXP);
    end if;
    return SGN * (Z * EXP);
  end "**";

  function SIN_COS (Y, SIGN : FLOAT_TYPE) return FLOAT_TYPE is
    PI_2   : constant :=  PI / 2.0;
    PI_INV : constant :=  1.0 / PI;
    C1     : constant :=  3.140625;   -- Exact: 201/64
    C2     : constant :=  9.67653589793E-4;
    R1     : constant := -0.1666665668;
    R2     : constant :=  0.8333025139E-2;
    R3     : constant := -0.1980741872E-3;
    R4     : constant :=  0.2601903036E-5;
    YMAX   : constant :=  PI_INV * INT_LARGE;
    SGN    : FLOAT_TYPE :=  SIGN; -- Sign of the argument.
    N      : INTEGER;
    X_RED, XN, X1, X2, G : FLOAT_TYPE;
  begin
    if Y > YMAX then
      return 0.0;
    end if;
    N := INTEGER (FLOAT(Y * PI_INV));                           -- 6.
    if N Mod 2 = 1 then                              -- 7.
      SGN := - SGN;
    end if;
    XN := FLOAT_TYPE (N);
    X1 := FLOAT_TYPE_TRUNCATION (Y);                     -- 10.
    X2 := Y - X1;
    X_RED := ((X1 - XN * C1) + X2) - XN * C2;
    if abs X_RED < EPS_2 then                            -- 11.
      return SGN * X_RED;
    end if;
    G := X_RED * X_RED;                                  -- 12.
    return SGN * (X_RED +
      X_RED * (((R4 * G + R3) * G + R2) * G + R1) * G);  -- 13, 14, 15.
  end SIN_COS;

  function SIN (X : FLOAT_TYPE) return FLOAT_TYPE is
  begin
    if X < 0.0 then
      return SIN_COS (-X, -1.0);
    end if;
    return SIN_COS (X, 1.0);
  end SIN;

  function SIN (X, CYCLE : FLOAT_TYPE) return FLOAT_TYPE is
    TWO_PI : constant := 2.0 * PI;
  begin
    if CYCLE <= 0.0 then
      raise ARGUMENT_ERROR;
    end if;
    if X < 0.0 then
      return SIN_COS (-(X * TWO_PI / CYCLE), -1.0);
    end if;
    return SIN_COS (X * TWO_PI / CYCLE, 1.0);
  end SIN;

  function COS (X : FLOAT_TYPE) return FLOAT_TYPE is
    PI_2 : constant := PI / 2.0;
  begin
    return SIN_COS (abs X + PI_2, 1.0);
  end COS;

  function COS (X, CYCLE : FLOAT_TYPE) return FLOAT_TYPE is
    PI_2   : constant := PI / 2.0;
    TWO_PI : constant := 2.0 * PI;
  begin
    if CYCLE <= 0.0 then
      raise ARGUMENT_ERROR;
    end if;
    return SIN_COS (abs (X * TWO_PI / CYCLE) + PI_2, 1.0);
  end COS;

  function TAN_COT (X, Y : FLOAT_TYPE; FLAG : BOOLEAN) return FLOAT_TYPE is
    PI_2_INV : constant :=  2.0 / PI;
    PI_2     : constant :=  PI / 2.0;
    C1       : constant :=  1.5703125;  -- Exact: 201 / 128
    C2       : constant :=  4.83826794897E-4;
    P1       : constant := -0.958017723E-1;
    Q1       : constant := -0.429135777;
    Q2       : constant :=  0.971685835E-2;
    YMAX     : constant :=  PI_2 * INT_LARGE;
    N        : INTEGER;
    XN, X1, X_RED, XNUM, XDEN, G : FLOAT_TYPE;
  begin
    if Y > YMAX then
      return 0.0;
    end if;
    N  := INTEGER (FLOAT(X * PI_2_INV));               -- 7.
    XN := FLOAT_TYPE (N);
    X1 := FLOAT_TYPE_TRUNCATION (X);            -- 8.
    X_RED := ((X1 - XN * C1) + X - X1) - XN * C2;
    if abs X_RED < EPS_2 then                   -- 9.
      XDEN := X_RED;                            -- 10.
      XNUM := 1.0;
    else
      G := X_RED * X_RED;                       -- 11.
      XNUM := P1 * G * X_RED + X_RED;           -- 12.
      XDEN := (Q2 * G + Q1) * G + 0.5 + 0.5;
    end if;
    if FLAG then                                -- 13. Calculation of cot
      if N mod 2 = 1 then                   -- 14.
        return -XNUM / XDEN;                    -- 16.
      end if;
      return XDEN / XNUM;                       -- 15.
    end if;                                     -- 13. Calculation of tan
    if N mod 2 = 1 then                     -- 14.
      return - XDEN / XNUM;                     -- 15.
    end if;
    return XNUM / XDEN;                         -- 16.
  end TAN_COT;

  function TAN (X : FLOAT_TYPE) return FLOAT_TYPE is
  begin
    return TAN_COT (X, abs X, FALSE);
  end TAN;

  function TAN (X, CYCLE : FLOAT_TYPE) return FLOAT_TYPE is
    TWO_PI : constant := 2.0 * PI;
  begin
    if CYCLE <= 0.0 then
      raise ARGUMENT_ERROR;
    end if;
    return TAN (X * TWO_PI / CYCLE);
  end TAN;

  function COT (X : FLOAT_TYPE) return FLOAT_TYPE is
    Y : FLOAT_TYPE := abs X;
  begin
    if Y <= SAFE_SMALL then      -- 3.
      if X < 0.0 then
        return -SAFE_LARGE;
      else
        return SAFE_LARGE;
      end if;
    end if;
    return TAN_COT (X, Y, TRUE);
  end COT;

  function COT (X, CYCLE : FLOAT_TYPE) return FLOAT_TYPE is
    TWO_PI : constant := 2.0 * PI;
  begin
    if CYCLE <= 0.0 then
      raise ARGUMENT_ERROR;
    end if;
    return COT (X * TWO_PI / CYCLE);
  end COT;

  function ASIN_ACOS (X : FLOAT_TYPE; FLAG : BOOLEAN) return FLOAT_TYPE is
    A  : constant array (BOOLEAN) of FLOAT_TYPE := (0.0, PI / 4.0);
    B  : constant array (BOOLEAN) of FLOAT_TYPE := (PI / 2.0, PI / 4.0);
    P1 : constant :=  0.933935835;
    P2 : constant := -0.504400557;
    Q0 : constant :=  0.560363004E1;
    Q1 : constant := -0.554846723E1;
    Y     : FLOAT_TYPE := abs X;                            -- 3.
    G, T  : FLOAT_TYPE;
    WHICH : BOOLEAN;
  begin
    if Y > 0.5 then
      if Y > 1.0 then
        raise ARGUMENT_ERROR;
      end if;
      WHICH := not FLAG;                                    -- 4.
      G := (1.0 - Y) * 0.5;                                 -- 8.
      Y := -2.0 * SQRT (G);
      T := Y + Y * (( P2 * G + P1 ) * G / (( G + Q1 ) * G + Q0 ));
    else
      WHICH := FLAG;                                        -- 4.
      if Y < EPS_2 then                                     -- 5.
        T := Y;
      else
        G := Y * Y;                                         -- 7.
        T := Y + Y * ((P2 * G + P1) * G / ((G + Q1) * G + Q0));
      end if;
    end if;
    if FLAG then                                            -- 10.
      if X < 0.0 then
        return 2.0 * B (WHICH) + T;                         -- 12.
      end if;
      return 2.0 * A (WHICH) - T;                           -- 11.
    elsif X < 0.0 then
      return - (2.0 * A (WHICH) + T);                       -- 11.
    end if;
    return 2.0 * A (WHICH) + T;                             -- 11.
  end ASIN_ACOS;

  function ARCSIN (X : FLOAT_TYPE) return FLOAT_TYPE is
  begin
    return ASIN_ACOS (X, FALSE);
  end ARCSIN;

  function ARCSIN (X, CYCLE : FLOAT_TYPE) return FLOAT_TYPE is
    TWO_PI : constant := 2.0 * PI;
  begin
    if CYCLE <= 0.0 then
      raise ARGUMENT_ERROR;
    end if;
    return ASIN_ACOS (X, FALSE) * CYCLE / TWO_PI;
  end ARCSIN;

  function ARCCOS (X : FLOAT_TYPE) return FLOAT_TYPE is
  begin
    return ASIN_ACOS (X, TRUE);
  end ARCCOS;

  function ARCCOS (X, CYCLE : FLOAT_TYPE) return FLOAT_TYPE is
    TWO_PI : constant := 2.0 * PI;
  begin
    if CYCLE <= 0.0 then
      raise ARGUMENT_ERROR;
    end if;
    return ASIN_ACOS (X, TRUE) * CYCLE / TWO_PI;
  end ARCCOS;

  function ATAN2 (Y, X : FLOAT_TYPE) return FLOAT_TYPE is
    A : constant array (0 .. 3) of FLOAT_TYPE := (0.0, PI/6.0, PI/2.0, PI/3.0);
    PI_2      : constant :=  PI / 2.0;
    NEG_PI_2  : constant := -PI / 2.0;
    TWO_SQRT3 : constant :=  0.26794919243112270647;
    SQRT3     : constant :=  1.7320508075688772935274463;
    P0        : constant := -0.4708325141;
    P1        : constant := -0.5090958253E-1;
    Q0        : constant :=  0.1412500740E1;
    MAXEXP    : constant :=  ABS_SAFE_EXP - 3;
    MINEXP    : constant := -ABS_SAFE_EXP + 3;
    ABS_Y_RED : FLOAT_TYPE :=  abs X;
    ABS_X_RED : FLOAT_TYPE :=  abs Y;
    Y_EXP     : INTEGER  :=  0;
    X_EXP     : INTEGER  :=  0;
    N         : INTEGER  :=  0;
    F, T, G   : FLOAT_TYPE;
  begin
    if X = 0.0 then
      if Y = 0.0 then
        raise ARGUMENT_ERROR;                      -- 2.
      elsif Y < 0.0 then                           -- C.
        return NEG_PI_2;
      end if;
      return PI_2;
    elsif Y = 0.0 then
       if X > 0.0 then
          return 0.0;
       else
          return PI;
       end if;
    end if;
    Y_EXP := FLOAT_TYPE_EXPONENT (ABS_Y_RED);
    ABS_Y_RED := FLOAT_TYPE_FRACTION (ABS_Y_RED);
    X_EXP := FLOAT_TYPE_EXPONENT (ABS_X_RED);
    ABS_X_RED := FLOAT_TYPE_FRACTION (ABS_X_RED);
    if Y_EXP - X_EXP > MAXEXP then                 -- 3.
      if Y < 0.0 then                              -- C.
        return NEG_PI_2;
      end if;
      return PI_2;
    elsif Y_EXP - X_EXP < MINEXP then              -- 4.
      T := 0.0;
      if X < 0.0 then                              -- B.
        T := PI;
      end if;
      if Y < 0.0 then
        return - T;
      end if;
      return T;
    end if;
    F := abs (Y / X);                              -- 5.
    if F > 1.0 then                                -- 6.
      F := 1.0 / F;
      N := 2;
    end if;
    if F > TWO_SQRT3 then                          -- 7.
      F := (F * SQRT3 - 1.0) / (SQRT3 + F);        -- 8.
      N := N + 1;
    end if;
    if abs F < EPS_2 then                          -- 10.
      T := F;                                      -- 12.
    else
      G := F * F;                                  -- 11.
      T := F + F * (G * (P1 * G + P0) / (G + Q0)); -- 13, 14.
    end if;
    if N > 1 then
      T := - T;
    end if;
    T := A (N) + T;                                -- 15.
    if X < 0.0 then                                -- 16.
      T := PI - T;
    end if;
    if Y < 0.0 then
      return - T;
    end if;
    return T;
  end ATAN2;

  function ARCTAN (Y : FLOAT_TYPE; X : FLOAT_TYPE := 1.0) return FLOAT_TYPE is
  begin
    return ATAN2 (Y, X);
  end ARCTAN;

  function ARCTAN (Y     : FLOAT_TYPE;
                   X     : FLOAT_TYPE := 1.0;
                   CYCLE : FLOAT_TYPE) return FLOAT_TYPE is
    TWO_PI : constant := 2.0 * PI;
  begin
    if CYCLE <= 0.0 then
      raise ARGUMENT_ERROR;
    end if;
    return ATAN2 (Y, X) * CYCLE / TWO_PI;
  end ARCTAN;

  function ARCCOT (Y : FLOAT_TYPE; X : FLOAT_TYPE := 1.0) return FLOAT_TYPE is
    PI_2 : constant := PI / 2.0;
  begin
    return PI_2 - ATAN2 (Y, X);
  end ARCCOT;

  function ARCCOT (Y     : FLOAT_TYPE;
                   X     : FLOAT_TYPE := 1.0;
                   CYCLE : FLOAT_TYPE) return FLOAT_TYPE is
    PI_2   : constant := PI / 2.0;
    TWO_PI : constant := 2.0 * PI;
  begin
    return (PI_2 - ATAN2 (Y, X)) * CYCLE / TWO_PI;
  end ARCCOT;

  function SINH_COSH (Y    : FLOAT_TYPE;
                      FLAG : BOOLEAN) return FLOAT_TYPE is
    LN_V     : constant := 0.6931610107421875;
    V_HALF_1 : constant := 0.13830277879601902638;
    Z, W     : FLOAT_TYPE;
  begin
    if Y > LN_SAFE_LARGE then                 -- 5.
      W := Y - LN_V;                          -- 8.
      if W > LN_SAFE_LARGE then               -- 9.
        return SAFE_LARGE;                    -- 10.
      end if;
      Z := EXP (W);                           -- 6.
      return Z + V_HALF_1 * Z;
    end if;
    Z := EXP (Y);
    if FLAG then                              -- COSH
      return (Z + 1.0 / Z) * 0.5;
    end if;
    return (Z - 1.0 / Z) * 0.5;               -- SINH
  end SINH_COSH;

  function SINH (X : FLOAT_TYPE) return FLOAT_TYPE is
    P0 : constant := -0.713793159E1;
    P1 : constant := -0.190333399;
    Q0 : constant := -0.428277109E2;
    Y  : FLOAT_TYPE := abs X;                             -- 2.
    X2 : FLOAT_TYPE;
  begin
    if Y > 1.0 then
      if X < 0.0 then
        return - SINH_COSH (Y, FALSE);
      end if;
      return SINH_COSH (Y, FALSE);
    elsif Y < EPS_2 then                              -- 3.
      return X;
    end if;
    X2 := X * X;                                      -- 4.
    return X + X * (X2 * ((P1 * X2 + P0) / (X2 + Q0)));
  end SINH;

  function COSH (X : FLOAT_TYPE) return FLOAT_TYPE is
  begin
    return SINH_COSH (abs X, TRUE);
  end COSH;

  function TANH (X : FLOAT_TYPE) return FLOAT_TYPE is
    LN3_2  : constant :=  0.5493061443340548457;  -- LN (3) / 2
    C      : constant :=  0.6931471805599453094172321;
    XBIG_C : constant :=  13.0 / C; -- (FRAC_BIN_LEN / 2 + 1) / C
    P0     : constant := -0.8237728127;
    P1     : constant := -0.3831010665E-2;
    Q0     : constant :=  0.2471319654E1;
    F      : FLOAT_TYPE := abs X;
    T, G   : FLOAT_TYPE;
  begin
    if F > XBIG_C then                               -- 1.
      T := 1.0;
    elsif F > LN3_2 then                             -- 2.
      T := 2.0 * (0.5 - 1.0/(EXP (2.0 * F) + 1.0));  -- 3.
    elsif F < EPS_2 then                             -- 4.
      T := F;
    else
      G := F * F;                                    -- 5.
      T := F + F * (G * ((P1 * G + P0) / (G + Q0))); -- 6, 7.
    end if;
    if X < 0.0 then
      return - T;
    end if;
    return T;
  end TANH;

  function COTH (X : FLOAT_TYPE) return FLOAT_TYPE is
    TANH_X : FLOAT_TYPE := TANH (X);
  begin
    if abs TANH_X < EPS_2 then
      if TANH_X < 0.0 then
        return - SAFE_LARGE;
      end if;
      return SAFE_LARGE;
    end if;
    return 1.0 / TANH_X;
  end COTH;

  function ARCCOSH (X : FLOAT_TYPE) return FLOAT_TYPE is
    LN2 : constant := 0.6931471805599453094172321;
  begin
    if abs X < 1.0 then
      raise ARGUMENT_ERROR;
    end if;
    if X > BIG then
      return LOG (X) + LN2;
    end if;
    return LOG (X + SQRT (X * X - 1.0));
  end ARCCOSH;

  function ARCTANH (X : FLOAT_TYPE) return FLOAT_TYPE is
  begin
    if abs X >= 1.0 then
      raise ARGUMENT_ERROR;
    end if;
    return 0.5 * LOG ((1.0 + X) / (1.0 - X));
  end ARCTANH;

  function ARCSINH (X : FLOAT_TYPE) return FLOAT_TYPE is
    LN2 : constant := 0.6931471805599453094172321;
    Y   : FLOAT_TYPE := abs X;
    T   : FLOAT_TYPE;
  begin
    if Y < EPS_2 then  -- 1 + X**2 = 1
      return X;
    elsif Y > BIG then  -- SQRT (1 + X**2) = abs X
      if X < 0.0 then
        return - (LOG (Y) + LN2);
      end if;
      return LOG (Y) + LN2;
    end if;
    T := LOG (Y + SQRT (X * X + 1.0));
    if X < 0.0 then
      return - T;
    end if;
    return T;
  end ARCSINH;

  function ARCCOTH (X : FLOAT_TYPE) return FLOAT_TYPE is
  begin
    if abs X <= 1.0 then
      raise ARGUMENT_ERROR;
    end if;
    return 0.5 * LOG ((X + 1.0) / (X - 1.0));
  end ARCCOTH;

end GEF;
--------------------------------------------------------------------
-- Project : European Robotic Arm, Service Layer
-- Unit Id : Pa_Robotic_Math (spec)
-- Version : 1.0
-- Author : Christian Maegaard
-- Date of Creation : Oct 5, 1994
-- Unit Description : Robotic mathematical routines.
--
-- Modifications :
--    Last modified on Tue Nov 22 14:46:54 1994 by peter
--         modified on Wed Nov 09 13:28:51 1994 by cma
--
-- (C) Computer Resources International A/S, 1994.
-- The copyright of this source code is vested in Computer Resources
-- International A/S.
--
-- This source code may only be reproduced in whole or in part,
-- stored in a retrieval system, transmitted in any form, or by
-- any means electronic, mechanical, photocopying, or otherwise,
-- with the prior permission of Computer Resources International A/S.
-----------------------------------------------------------------------

package Pa_Robotic_Math is
   type real is digits 6; -- range -3.40282E+37 .. 3.40282E+37;

   -- Defines a pose in task space
   type TS_Pose_Type is
      record
         X : real;
         Y : real;
         Z : real;
         Roll : real;
         Pitch : real;
         Yaw : real;
      end record;

   -- Defines a pose in task space, using Euler angles
   type Euler_Pose_Type is
      record
         X : real;
         Y : real;
         Z : real;
         Ang1 : real;
         Ang2 : real;
         Ang3 : real;
      end record;

   type Joint_ID_Type is
   (Shoulder_Roll_Joint, Shoulder_Pitch_Joint, Elbow_Joint, Wrist_Pitch_Joint,
    Wrist_Yaw_Joint, Wrist_Roll_Joint);

   -- Defines a position in joint space
   type JS_Pos_Type is array (Joint_ID_Type) of real;
   type HT_Column_Type is (Col_n, Col_o, Col_a, Col_P);
   type HT_Row_Type is (Row_x, Row_y, Row_z, Row_Last);

   -- Defines a homogenous transform using the Denavit-Hartenberg convention
   type HT_Type is array (HT_Column_Type, HT_Row_Type) of real;

   type End_Indicator_Type is (Forward, Backward);
   type Elbow_Indicator_Type is (Above, Below);

   -- Define Pi
   Pi : constant real := 3.14159_26535_89793_23846_26433_83279_50288_41971_69399_37511;

   procedure Inverse_Kinematics (HT : in HT_Type; JS_Pos : out JS_Pos_Type; Valid: out boolean);
   procedure Forward_Kinematics (JS_Pos : in JS_Pos_Type; HT : out HT_Type);
   procedure Convert_to_CV (HT : in HT_Type; CV : out TS_Pose_Type);
   procedure Convert_to_Euler (HT : in HT_Type; E : out Euler_Pose_Type);
   procedure Calculate_Drive_Matrix(E : in Euler_Pose_Type; R : in real; D : out HT_Type);
   procedure Convert_to_HT (CV : in TS_Pose_Type; HT : out HT_Type);
   procedure Invert_HT (HT : in HT_Type; Inv_HT : out HT_Type);
   procedure Multiply_HT (HT1 : in HT_Type; HT2 : in HT_Type; HT_Res : out HT_Type);
   procedure Set_Indicators (S : in End_Indicator_Type;
                             W : in End_Indicator_Type;
                             E : in Elbow_Indicator_Type);
end Pa_Robotic_Math;
--------------------------------------------------------------------
-- Project : European Robotic Arm, Service Layer
-- Unit Id : Pa_Robotic_Math (spec)
-- Version : 1.0
-- Author : Christian Maegaard
-- Date of Creation : Oct 5, 1994
-- Unit Description : Robotic mathematical routines.
--
-- Modifications :
--    Last modified on Tue Nov 22 14:47:49 1994 by peter
--         modified on Wed Nov 16 10:10:13 1994 by cma
--
-- (C) Computer Resources International A/S, 1994.
-- The copyright of this source code is vested in Computer Resources
-- International A/S.
--
-- This source code may only be reproduced in whole or in part,
-- stored in a retrieval system, transmitted in any form, or by
-- any means electronic, mechanical, photocopying, or otherwise,
-- with the prior permission of Computer Resources International A/S.
-----------------------------------------------------------------------

with GEF;

package body Pa_Robotic_Math is

   package Math_Functions is new GEF(real);

   function sin (x: real) return real renames Math_Functions.sin;
   function cos (x: real) return real renames Math_Functions.cos;
   function Atan2 (x: in real;y: in real) return real renames Math_Functions.Arctan;
   function sqrt (x: real) return real renames Math_Functions.sqrt;

   -- Shoulder configuration indicator
   SCI : End_Indicator_Type := Forward;
   -- Wrist configuration indicator
   WCI : End_Indicator_Type := Forward;
   -- Elbow configuration indicator
   ECI : Elbow_Indicator_Type := Below;

   a2 : constant real := 3.971; -- Length of a2
   a3 : constant real := 3.971; -- Length of a3
   a4 : constant real := 0.270; -- Length of a4

   procedure Inverse_Kinematics
     (HT : in HT_Type; JS_Pos : out JS_Pos_Type; Valid : out boolean) is

      --|S1 is short for sin(phi1)
      S1    : real;
      --|C1 is short for cos(phi1)
      C1    : real;
      --|S234 is short for cos(phia4)
      S234  : real;
      --|C234 is short for cos(phia4)
      C234  : real;
      --|S3 is short for sin(phi3)
      S3    : real;
      --|C3 is short for cos(phi3)
      C3    : real;
      --|S5 is short for sin(phi5)
      S5    : real;
      --|C5 is short for cos(phi5)
      C5    : real;
      --|S6 is short for sin(phi6)
      S6    : real;
      --|C6 is short for cos(phi6)
      C6    : real;

      phia4 : real; -- Temporary result (orientation of link a4).
      P_x   : real; -- Position of joint 4 in x1,y1,z1;
      P_y   : real; -- Position of joint 4 in x1,y1,z1;
      Temp_JS_Pos: JS_Pos_Type;

   begin
      --|Calculate shoulder roll angle
      Temp_JS_Pos(Shoulder_Roll_Joint) := Atan2(HT(Col_P,Row_y),HT(Col_P,Row_x));

      --|taking shoulder configuration indicator into account.
      --|if SCI = Backward then
      if SCI = Backward then
         --|if phi1 > 0 then
         if Temp_JS_Pos(Shoulder_Roll_Joint) > 0.0 then
            --|phi1 = phi1 - Pi
            Temp_JS_Pos(Shoulder_Roll_Joint) := Temp_JS_Pos(Shoulder_Roll_Joint) - Pi;
         else
         --|else phi1 = phi1 + Pi
            Temp_JS_Pos(Shoulder_Roll_Joint) := Temp_JS_Pos(Shoulder_Roll_Joint) + Pi;
         end if;
         --|end if
      end if;
      --|end if
      --|
      --|S1 = sin(phi1)
      --|C1 = cos(phi1)
      S1 := sin(Temp_JS_Pos(Shoulder_Roll_Joint));
      C1 := cos(Temp_JS_Pos(Shoulder_Roll_Joint));
      --|
      --|Calculate orientation of link a4
      --|phia4 = Atan2(az, C1*ax+S1*ay)
      phia4 := Atan2(HT(Col_a, Row_z),C1*HT(Col_a, Row_x) + S1*HT(Col_a, Row_y));
      --|taking wrist configuration indicator into account.
      --|if WCI = Backward then
      if WCI = Backward then
         --|if phia4 > 0 then
         if phia4 > 0.0 then
            --|phia4 = phia4 - Pi
            phia4 := phia4 - Pi;
         else
         --|else phia4 = phia4 + Pi
            phia4 := phia4 + Pi;
         end if;
         --|end if
      end if;
      --|end if
      --|
      --|S234 = sin(phia4)
      --|C234 := cos(phia4)
      S234 := sin(phia4);
      C234 := cos(phia4);
      --|
      --|Calculate position of joint 4 in (x1,y1,z1) coordinate system.
      --|P'x = C1*Px + S1*Py - C234*a4
      --|P'y = Pz - S234*a4
      P_x := C1*HT(Col_P, Row_x) + S1*HT(Col_P, Row_y) - C234 * a4;
      P_y := HT(Col_P, Row_z) - S234 * a4;
      --|
      --|Now derive the elbow angle taking elbow configuration indicator into account
      --|C3 = (P'x^2 + P'y^2 - a2^2 - a3^2)/(2*a2*a3)
      C3 := (P_x*P_x + P_y*P_y - a2*a2 - a3*a3)/(2.0*a2*a3);
      --|if abs(C3) > 1 (destination out of reach) then
      if abs(C3) > 1.0 then
         --|valid = False
         valid := False;
      --|else
      else
         --|if ECI = Above then
         if ECI = Above then
           --|S3 = - sqrt(1 - C3*C3)
           S3 := - sqrt(1.0 - C3*C3);
         --|else
         else
            --|S3 = sqrt(1 - C3*C3)
            S3 := sqrt(1.0 - C3*C3);
         end if;
         --|end if
         --|phi3 = Atan2(S3,C3)
         Temp_JS_Pos(Elbow_Joint) := Atan2(S3,C3);
         --|
         --|Calculate shoulder pitch.
         --|phi2 = Atan2((C3*a3 + a2)*P'y - S3*a3*P'x, (C3*a3 + a2)*P'x + S3*a3*P'y)
         Temp_JS_Pos(Shoulder_Pitch_Joint) := Atan2((C3*a3 + a2)*P_y - S3*a3*P_x,
                                                    (C3*a3 + a2)*P_x + S3*a3*P_y);
         --|
         --|The wrist pitch is known modulo 2pi as the orintaion of a4 is known
         --|as well as the previous angles.
         --|phi4 = phia4 - phi2 - phi3
         Temp_JS_Pos(Wrist_Pitch_Joint) := phia4 - Temp_JS_Pos(Shoulder_Pitch_Joint) - Temp_JS_Pos(Elbow_Joint);
         --|if phi4 <= -Pi then
         if Temp_JS_Pos(Wrist_Pitch_Joint) <= - Pi then
            --|phi4 = phi4 + 2Pi
            Temp_JS_Pos(Wrist_Pitch_Joint) := Temp_JS_Pos(Wrist_Pitch_Joint) + 2.0*Pi;
         --|elsif phi4 > Pi then
         elsif Temp_JS_Pos(Wrist_Pitch_Joint) > Pi then
            --|phi4 = phi4 - 2Pi
            Temp_JS_Pos(Wrist_Pitch_Joint) := Temp_JS_Pos(Wrist_Pitch_Joint) - 2.0*Pi;
         end if;
         --|
         --|Calculate the wrist yaw angle taking its special range into account.
         --|S5 = C234*(C1*ax + S1*ay) + S234*az
         --|C5 = S1*ax - C1*ay;
         S5 := C234*(C1*HT(Col_a,Row_x) + S1*HT(Col_a,Row_y)) + S234*HT(Col_a,Row_z);
         C5 := S1*HT(Col_a,Row_x) - C1*HT(Col_a,Row_y);
         --|phi5 = Atan2(S5,C5)
         Temp_JS_Pos(Wrist_Yaw_Joint) := Atan2(S5,C5);
         --|if phi5 < -Pi/2 then
         if Temp_JS_Pos(Wrist_Yaw_Joint) < -Pi/2.0 then
            --|phi5 = phi5 + 2Pi
            Temp_JS_Pos(Wrist_Yaw_Joint) := Temp_JS_Pos(Wrist_Yaw_Joint) + 2.0*Pi;
         end if;
         --|end if
         --|
         --|Finally calculate the wrist roll angle.
         --|S6 = -C5*(C234*(C1*ox + S1*oy) + S234 * oz) + S5 * (S1 * ox - C1 * oy)
         --|C6 = -S234*(C1*ox + S1*oy) + C234*oz
         --|phi6 = Atan2(S6,C6)
         S6 := -C5*(C234*(C1*HT(Col_o,Row_x) + S1*HT(Col_o,Row_y)) + S234 * HT(Col_o, Row_z)) +
           S5*(S1*HT(Col_o,Row_x) - C1*HT(Col_o,Row_y));
         C6 := -S234*(C1*HT(Col_o,Row_x) + S1*HT(Col_o,Row_y)) + C234*HT(Col_o, Row_z);
         Temp_JS_Pos(Wrist_Roll_Joint) := Atan2(S6,C6);
         JS_Pos := Temp_JS_Pos;
         valid := true;
      end if;
   end;

   procedure Forward_Kinematics (JS_Pos : in JS_Pos_Type; HT : out HT_Type) is


      --|S1 is short for sin(phi1)
      S1   : constant real := sin(JS_Pos(Shoulder_Roll_Joint));
      --|S2 is short for sin(phi2)
      S2   : constant real := sin(JS_Pos(Shoulder_Pitch_Joint));
      --|S3 is short for sin(phi3)
      S3   : constant real := sin(JS_Pos(Elbow_Joint));
      --|S4 is short for sin(phi4)
      S4   : constant real := sin(JS_Pos(Wrist_Pitch_Joint));
      --|S5 is short for sin(phi5)
      S5   : constant real := sin(JS_Pos(Wrist_Yaw_Joint));
      --|S6 is short for sin(phi6)
      S6   : constant real := sin(JS_Pos(Wrist_Roll_Joint));
      --|C1 is short for cos(phi1)
      C1   : constant real := cos(JS_Pos(Shoulder_Roll_Joint));
      --|C2 is short for cos(phi2)
      C2   : constant real := cos(JS_Pos(Shoulder_Pitch_Joint));
      --|C3 is short for cos(phi3)
      C3   : constant real := cos(JS_Pos(Elbow_Joint));
      --|C4 is short for cos(phi4)
      C4   : constant real := cos(JS_Pos(Wrist_Pitch_Joint));
      --|C5 is short for cos(phi5)
      C5   : constant real := cos(JS_Pos(Wrist_Yaw_Joint));
      --|C6 is short for cos(phi6)
      C6   : constant real := cos(JS_Pos(Wrist_Roll_Joint));
      --|
      --|First intermediate results from transformation T1 - T5 is defined.
      --|Only results needed to minimize computation effort is computed.
      --|
      --|Calculate intermediate results for T3:
      --|T3(3,1) = C2*S3+S2*C3
      --|T3(3,2) = C2*C3-S2*S3
      --|T3(1,1) = C1*T3(3,2)
      T331 : constant real := C2*S3 + S2*C3;
      T332 : constant real := C2*C3 - S2*S3;
      T311 : constant real := C1*T332;
      --|
      --|Calculate intermediate results for T4.
      --|T4(3,1) = T3(3,2)*S4 + T3(3,1)*C4;
      --|T4(3,3) = T3(3,2)*C4 - T3(3,1)*S4;
      --|T4(1,1) = C1*T4(3,3);
      --|T4(1,3) = -C1*T4(3,1);
      --|T4(2,1) = S1*T4(3,3);
      --|T4(2,3) = -S1*T4(3,1);
      T431 : constant real := T332*S4 + T331*C4;
      T433 : constant real := T332*C4 - T331*S4;
      T411 : constant real := C1*T433;
      T413 : constant real := -C1*T431;
      T421 : constant real := S1*T433;
      T423 : constant real := -S1*T431;
      --|
      --|Calculate intermediate results for T5.
      --|T5(1,1) = T4(1,1)*C5 - S1*S5;
      --|T5(1,2) = T4(1,3);
      --|T5(2,1) = T4(2,1)*C5 + C1*S5;
      --|T5(2,2) = T4(2,3);
      --|T5(3,1) = T4(3,1)*C5;
      --|T5(3,2) = T4(3,3);
      T511 : constant real := T411*C5 - S1*S5;
      T512 : constant real := T413;
      T521 : constant real := T421*C5 + C1*S5;
      T522 : constant real := T423;
      T531 : constant real := T431*C5;
      T532 : constant real := T433;

   begin
      --|
      --|Calculate last transformation (T6) from intermediate results.
      --|T6(1,1) = T511*C6 + T512*S6
      --|T6(1,2) = -T511*S6 + T512*C6
      --|T6(1,3) = T411*S5 + S1*C5
      --|T6(1,4) = T411 * a4 + T311*a3 + C1*C2*a2
      --|T6(2,1) = T521*C6 + T522*S6
      --|T6(2,2) = -T521*S6 + T522*C6
      --|T6(2,3) = T421*S5 - C1*C5
      --|T6(2,4) = T421*a4 + S1*T332*a3 + S1*C2*a2
      --|T6(3,1) = T531*C6 + T532*S6
      --|T6(3,2) = -T531*S6 + T532*C6
      --|T6(3,3) = T431*S5
      --|T6(3,4) = T431*a4 + T331*a3 + S2*a2
      --|T6(4,1) = 0.0
      --|T6(4,2) = 0.0
      --|T6(4,3) = 0.0
      --|T6(4,4) = 1.0
      HT(Col_n,Row_x) := T511*C6 + T512*S6;
      HT(Col_o,Row_x) := -T511*S6 + T512*C6;
      HT(Col_a,Row_x) := T411*S5 + S1*C5;
      HT(Col_P,Row_x) := T411 * a4 + T311*a3 + C1*C2*a2;
      HT(Col_n,Row_y) := T521*C6 + T522*S6;
      HT(Col_o,Row_y) := -T521*S6 + T522*C6;
      HT(Col_a,Row_y) := T421*S5 - C1*C5;
      HT(Col_P,Row_y) := T421*a4 + S1*T332*a3 + S1*C2*a2;
      HT(Col_n,Row_z) := T531*C6 + T532*S6;
      HT(Col_o,Row_z) := -T531*S6 + T532*C6;
      HT(Col_a,Row_z) := T431*S5;
      HT(Col_P,Row_z) := T431*a4 + T331*a3 + S2*a2;
      HT(Col_n,Row_Last) := 0.0;
      HT(Col_o,Row_Last) := 0.0;
      HT(Col_a,Row_Last) := 0.0;
      HT(Col_P,Row_Last) := 1.0;
   end;

   procedure Convert_to_CV (HT : in HT_Type; CV : out TS_Pose_Type) is

      --|SR is short for sin(Roll)
      SR: real;
      --|CR is short for cos(Roll)
      CR: real;
      --|SP is short for sin(Pitch)
      SP: real;
      --|CP is short for cos(Pitch)
      CP: real;
      --|SY is short for sin(Yaw)
      SY: real;
      --|CY is short for cos(Yaw)
      CY: real;
      Temp_CV: TS_Pose_Type; -- Temporary result vector

   begin
      --|if ny = 0 and nx = 0 then
      if HT(Col_n,Row_y) = 0.0 and HT(Col_n,Row_x) = 0.0 then
         --|Roll = 0 by definition
         Temp_CV.Roll := 0.0;
      else
      --|else
         --|Roll = Atan2(ny,nx)
         Temp_CV.Roll := Atan2(HT(Col_n,Row_y),HT(Col_n,Row_x));
      end if;
      --|end if

      --|SR = sin(Roll)
      --|CR = cos(Roll)
      SR := sin(Temp_CV.Roll);
      CR := cos(Temp_CV.Roll);

      --|SP = -nz
      --|CP = CR*nx + SR*ny
      SP := -HT(Col_n,Row_z);
      CP := CR*HT(Col_n,Row_x) + SR*HT(Col_n,Row_y);

      --|Pitch = Atan2(SP,CP)
      Temp_CV.Pitch := Atan2(SP,CP);

      --|SY = SR*ax - CR*ay
      --|CY = -SR*ox + CR*oy
      SY := SR*HT(Col_a,Row_x) - CR*HT(Col_a,Row_y);
      CY := -SR*HT(Col_o,Row_x) + CR*HT(Col_o,Row_y);

      --|Yaw = Atan2(SY,CY)
      Temp_CV.Yaw := Atan2(SY,CY);

      --|x = Px
      --|y = Py
      --|z = Pz
      Temp_CV.x := HT(Col_P,Row_x);
      Temp_CV.y := HT(Col_P,Row_y);
      Temp_CV.z := HT(Col_P,Row_z);
      CV := Temp_CV;
   end;

   procedure Convert_to_Euler (HT : in HT_Type; E : out Euler_Pose_Type) is

      --|S1 is short for sin(Ang1)
      S1: real;
      --|C1 is short for cos(Ang1)
      C1: real;
      --|C2 is short for cos(Ang2)
      C2: real;
      --|S1Ang2 is used for temporary result
      S1Ang2: real;
      --|SVC is used for temporary result
      SCV: real;
      --|CVC is used for temporary result
      CVC: real;
      --|SS is used for temporary result
      SS: real;
      --|S1Ang3 is used for temporary result
      S1Ang3: real;
      --|S2Ang3 is used for temporary result
      S2Ang3: real;
      Temp_E: Euler_Pose_Type; -- Temporary result vector

   begin
      --|if ay = 0 and ax = 0 then
      if HT(Col_a,Row_y) = 0.0 and HT(Col_a,Row_x) = 0.0 then
         --|Roll = 0 by definition
         Temp_E.Ang1 := 0.0;
      else
      --|else
         --|Roll = Atan2(ay,ax)
         Temp_E.Ang1 := Atan2(HT(Col_a,Row_y),HT(Col_a,Row_x));
      end if;
      --|end if

      --|S1Ang2 = sqrt(ax*ax+ay*ay));
      S1Ang2 := sqrt(HT(Col_a,Row_x)*HT(Col_a,Row_x)+HT(Col_a,Row_y)*HT(Col_a,Row_y));
      --|Ang2 = Atan2(SP,az)
      Temp_E.Ang2 := Atan2(S1Ang2,HT(Col_a,Row_z));

      --|S1 = sin(Ang1)
      --|C1 = cos(Ang1)
      --|C2 = cos(Ang2)
      S1 := sin(Temp_E.Ang1);
      C1 := cos(Temp_E.Ang1);
      C2 := cos(Temp_E.Ang2);

      --|SCV = S1*C1*(1-C2);
      --|CVC = C1*C1*(1-C2)+C2;
      --|SS = S1*sin(Ang2);
      --|S1Ang3 = -SCV*nx + CVC*ny - SS*nz;
      --|S2Ang3 = -SCV*ox + CVC*oy - SS*oz;
      SCV := S1*C1*(1.0 - C2);
      CVC := C1*C1*(1.0 - C2) + C2;
      SS  := S1*sin(Temp_E.Ang2);
      S1Ang3 := -SCV*HT(Col_n,Row_x) + CVC*HT(Col_n,Row_y) - SS*HT(Col_n,Row_z);
      S2Ang3 := -SCV*HT(Col_o,Row_x) + CVC*HT(Col_o,Row_y) - SS*HT(Col_o,Row_z);

      --|Ang3 = Atan2(S1Ang3,S2Ang3)
      Temp_E.Ang3 := Atan2(S1Ang3,S2Ang3);

      --|x = Px
      --|y = Py
      --|z = Pz
      Temp_E.x := HT(Col_P,Row_x);
      Temp_E.y := HT(Col_P,Row_y);
      Temp_E.z := HT(Col_P,Row_z);
      E := Temp_E;
   end Convert_to_Euler;

   procedure Calculate_Drive_Matrix(E : in Euler_Pose_Type; R : in real; D : out HT_Type) is
      --|E = Euler representation of total drive
      --|R = Fraction of drive to compute
      --|D = Resulting drive matrix
      --|
      --|S1 = sin(Ang1)
      S1 : constant real := sin(E.Ang1);
      --|SR2 = sin(R*Ang2)
      SR2 : constant real := sin(R*E.Ang2);
      --|SR3 = sin(R*Ang3)
      SR3 : constant real := sin(R*E.Ang3);
      --|C1 = cos(Ang1)
      C1 : constant real := cos(E.Ang1);
      --|CR2 = cos(R*Ang2)
      CR2 : constant real := cos(R*E.Ang2);
      --|CR3 = cos(R*Ang3)
      CR3 : constant real := cos(R*E.Ang3);
      --|
      --|VR2 = 1 - CR2
      VR2 : constant real := 1.0 - CR2;
      --|S1K = S1*S1
      S1K : constant real := S1*S1;
      --|C1K = C1*C1
      C1K : constant real := C1*C1;

      Temp_D: HT_Type; --Temporary result drive matrix

   begin
      --|ox = -SR3*(S1K*VR2 + CR2) + CR3*(-S1*C1*VR2)
      Temp_D(Col_o,Row_x) := -SR3*(S1K*VR2 + CR2) + CR3*(-S1*C1*VR2);
      --|oy = -SR3*(-S1*C1*VR2) + CR3*(C1K*VR2 + CR2)
      Temp_D(Col_o,Row_y) := -SR3*(-S1*C1*VR2) + CR3*(C1K*VR2 + CR2);
      --|oz = -SR3*(-C1*SR2) + CR3*(-S1*SR2);
      Temp_D(Col_o,Row_z) := -SR3*(-C1*SR2) + CR3*(-S1*SR2);
      --|
      --|ax = C1*SR2
      Temp_D(Col_a,Row_x) := C1*SR2;
      --|ay = S1*SR2
      Temp_D(Col_a,Row_y) := S1*SR2;
      --|az = CR2
      Temp_D(Col_a,Row_z) := CR2;
      --|
      --|nx = oy*az - oz*ay
      Temp_D(Col_n,Row_x) :=
        Temp_D(Col_o,Row_y)*Temp_D(Col_a,Row_z) -
        Temp_D(Col_o,Row_z)*Temp_D(Col_a,Row_y);
      --|ny = -ox*az - oz*ax
      Temp_D(Col_n,Row_y) := -
        Temp_D(Col_o,Row_x)*Temp_D(Col_a,Row_z) +
        Temp_D(Col_o,Row_z)*Temp_D(Col_a,Row_x);
      --|nz = ox*ay - oy*ax
      Temp_D(Col_n,Row_z) :=
        Temp_D(Col_o,Row_x)*Temp_D(Col_a,Row_y) -
        Temp_D(Col_o,Row_y)*Temp_D(Col_a,Row_x);
      --|
      --|Px = R*x
      Temp_D(Col_P,Row_x) := R*E.X;
      --|Py = R*y
      Temp_D(Col_P,Row_y) := R*E.Y;
      --|Pz = R*z
      Temp_D(Col_P,Row_z) := R*E.Z;
      --|
      --|Last row = (0,0,0,1)
      Temp_D(Col_n,Row_Last) := 0.0;
      Temp_D(Col_o,Row_Last) := 0.0;
      Temp_D(Col_a,Row_Last) := 0.0;
      Temp_D(Col_P,Row_Last) := 1.0;
      D := Temp_D;
   end Calculate_Drive_Matrix;

   procedure Convert_to_HT (CV : in TS_Pose_Type; HT : out HT_Type) is
      --|SR = sin(Roll)
      --|CR = cos(Roll)
      --|SP = sin(Pitch)
      --|CP = cos(Pitch)
      --|SY = sin(Yaw)
      --|CY = cos(Yaw)
      SR : constant real := sin(CV.Roll);
      CR : constant real := cos(CV.Roll);
      SP : constant real := sin(CV.Pitch);
      CP : constant real := cos(CV.Pitch);
      SY : constant real := sin(CV.Yaw);
      CY : constant real := cos(CV.Yaw);
      Temp_HT: HT_Type; -- Temporary result HT

   begin
      --|
      --|HT =
      --|| CR*CP   -SP*(SR*SP*CY-CR*SY)-SR*CP*CP*CY                CR*SP*CY+SR*SY  x |
      --|| SR*CP   SP*(CR*SP*CY+SR*SY)+SR*CP*CP*CY                 SR*SP*CY-CR*SY  y |
      --|| -SP     SR*CP*(CR*SP*CY+SR*SY)-CR*CP*(SR*SP*CY-CR*SY)   CP*CY           z |
      --|| 0       0                                               0               1 |

      Temp_HT(Col_n,Row_x)    := CR*CP;
      Temp_HT(Col_n,Row_y)    := SR*CP;
      Temp_HT(Col_n,Row_z)    := -SP;
      Temp_HT(Col_n,Row_Last) := 0.0;
      Temp_HT(Col_a,Row_x)    := CR*SP*CY+SR*SY;
      Temp_HT(Col_a,Row_y)    := SR*SP*CY-CR*SY;
      Temp_HT(Col_a,Row_z)    := CP*CY;
      Temp_HT(Col_a,Row_Last) := 0.0;
      Temp_HT(Col_o,Row_x)    := Temp_HT(Col_a,Row_y)*Temp_HT(Col_n,Row_z) - Temp_HT(Col_a,Row_z)*Temp_HT(Col_n,Row_y);
      Temp_HT(Col_o,Row_y)    := -Temp_HT(Col_a,Row_x)*Temp_HT(Col_n,Row_z) + Temp_HT(Col_a,Row_z)*Temp_HT(Col_n,Row_x);
      Temp_HT(Col_o,Row_z)    := Temp_HT(Col_a,Row_x)*Temp_HT(Col_n,Row_y) - Temp_HT(Col_a,Row_y)*Temp_HT(Col_n,Row_x);
      Temp_HT(Col_o,Row_Last) := 0.0;
      Temp_HT(Col_P,Row_x)    := CV.X;
      Temp_HT(Col_P,Row_y)    := CV.Y;
      Temp_HT(Col_P,Row_z)    := CV.Z;
      Temp_HT(Col_P,Row_Last) := 1.0;
      HT := Temp_HT;
   end;

   procedure Invert_HT (HT : in HT_Type; Inv_HT : out HT_Type) is

   begin
      --|Matrix inversion is written explicit as it is very simple
      --|for homogenous transforms.
      --|
      --|Inv_HT =
      --|| nx  ny  nz  -nx*Px-ny*Py-nz*Pz |
      --|| oy  oy  oz  -ox*Px-oy*Py-oz*Pz |
      --|| ax  ay  az  -ax*Px-ay*Py-az*Pz |
      --|| 0   0   0   1                  |
      Inv_HT(Col_n,Row_x) := HT(Col_n,Row_x);
      Inv_HT(Col_n,Row_y) := HT(Col_o,Row_x);
      Inv_HT(Col_n,Row_z) := HT(Col_a,Row_x);
      Inv_HT(Col_n,Row_Last) := 0.0;
      Inv_HT(Col_o,Row_x) := HT(Col_n,Row_y);
      Inv_HT(Col_o,Row_y) := HT(Col_o,Row_y);
      Inv_HT(Col_o,Row_z) := HT(Col_a,Row_y);
      Inv_HT(Col_o,Row_Last) := 0.0;
      Inv_HT(Col_a,Row_x) := HT(Col_n,Row_z);
      Inv_HT(Col_a,Row_y) := HT(Col_o,Row_z);
      Inv_HT(Col_a,Row_z) := HT(Col_a,Row_z);
      Inv_HT(Col_a,Row_Last) := 0.0;
      Inv_HT(Col_P,Row_x) :=
        - HT(Col_n,Row_x)*HT(Col_P,Row_x)
        - HT(Col_n,Row_y)*HT(Col_P,Row_y)
        - HT(Col_n,Row_z)*HT(Col_P,Row_z);
      Inv_HT(Col_P,Row_y) :=
        - HT(Col_o,Row_x)*HT(Col_P,Row_x)
        - HT(Col_o,Row_y)*HT(Col_P,Row_y)
        - HT(Col_o,Row_z)*HT(Col_P,Row_z);
      Inv_HT(Col_P,Row_z) :=
        - HT(Col_a,Row_x)*HT(Col_P,Row_x)
        - HT(Col_a,Row_y)*HT(Col_P,Row_y)
        - HT(Col_a,Row_z)*HT(Col_P,Row_z);
      Inv_HT(Col_P,Row_Last) := 1.0;

   end;

   procedure Multiply_HT (HT1 : in HT_Type; HT2 : in HT_Type; HT_Res : out HT_Type) is

   begin
      --|nx = nx1*nx2 + ox1*ny2 + ax1*nz2
      HT_Res(Col_n, Row_x) :=
        HT1(Col_n,Row_x) * HT2(Col_n,Row_x) +
        HT1(Col_o,Row_x) * HT2(Col_n,Row_y) +
        HT1(Col_a,Row_x) * HT2(Col_n,Row_z);
      --|ny = ny1*nx2 + oy1*ny2 + ay1*nz2
      HT_Res(Col_n, Row_y) :=
        HT1(Col_n,Row_y) * HT2(Col_n,Row_x) +
        HT1(Col_o,Row_y) * HT2(Col_n,Row_y) +
        HT1(Col_a,Row_y) * HT2(Col_n,Row_z);
      --|nz = nz1*nx2 + oz1*ny2 + az1*nz2
      HT_Res(Col_n, Row_z) :=
        HT1(Col_n,Row_z) * HT2(Col_n,Row_x) +
        HT1(Col_o,Row_z) * HT2(Col_n,Row_y) +
        HT1(Col_a,Row_z) * HT2(Col_n,Row_z);
      --|ox = nx1*ox2 + ox1*oy2 + ax1*oz2
      HT_Res(Col_o, Row_x) :=
        HT1(Col_n,Row_x) * HT2(Col_o,Row_x) +
        HT1(Col_o,Row_x) * HT2(Col_o,Row_y) +
        HT1(Col_a,Row_x) * HT2(Col_o,Row_z);
      --|oy = ny1*ox2 + oy1*oy2 + ay1*oz2
      HT_Res(Col_o, Row_y) :=
        HT1(Col_n,Row_y) * HT2(Col_o,Row_x) +
        HT1(Col_o,Row_y) * HT2(Col_o,Row_y) +
        HT1(Col_a,Row_y) * HT2(Col_o,Row_z);
      --|oz = nz1*ox2 + oz1*oy2 + az1*oz2
      HT_Res(Col_o, Row_z) :=
        HT1(Col_n,Row_z) * HT2(Col_o,Row_x) +
        HT1(Col_o,Row_z) * HT2(Col_o,Row_y) +
        HT1(Col_a,Row_z) * HT2(Col_o,Row_z);
      --|ax = nx1*ax2 + ox1*ay2 + ax1*az2
      HT_Res(Col_a, Row_x) :=
        HT1(Col_n,Row_x) * HT2(Col_a,Row_x) +
        HT1(Col_o,Row_x) * HT2(Col_a,Row_y) +
        HT1(Col_a,Row_x) * HT2(Col_a,Row_z);
      --|ay = ny1*ax2 + oy1*ay2 + ay1*az2
      HT_Res(Col_a, Row_y) :=
        HT1(Col_n,Row_y) * HT2(Col_a,Row_x) +
        HT1(Col_o,Row_y) * HT2(Col_a,Row_y) +
        HT1(Col_a,Row_y) * HT2(Col_a,Row_z);
      --|az = nz1*ax2 + oz1*ay2 + az1*az2
      HT_Res(Col_a, Row_z) :=
        HT1(Col_n,Row_z) * HT2(Col_a,Row_x) +
        HT1(Col_o,Row_z) * HT2(Col_a,Row_y) +
        HT1(Col_a,Row_z) * HT2(Col_a,Row_z);
      --|Px = nx1*Px2 + ox1*Py2 + ax1*Pz2 + Px1
      HT_Res(Col_P, Row_x) :=
        HT1(Col_n,Row_x) * HT2(Col_P,Row_x) +
        HT1(Col_o,Row_x) * HT2(Col_P,Row_y) +
        HT1(Col_a,Row_x) * HT2(Col_P,Row_z) +
        HT1(Col_P,Row_x);
      --|Py = ny1*Px2 + oy1*Py2 + ay1*Pz2 + Py1
      HT_Res(Col_P, Row_y) :=
        HT1(Col_n,Row_y) * HT2(Col_P,Row_x) +
        HT1(Col_o,Row_y) * HT2(Col_P,Row_y) +
        HT1(Col_a,Row_y) * HT2(Col_P,Row_z) +
        HT1(Col_P,Row_y);
      --|Pz = nz1*Px2 + oz1*Py2 + az1*Pz2 + Pz1
      HT_Res(Col_P, Row_z) :=
        HT1(Col_n,Row_z) * HT2(Col_P,Row_x) +
        HT1(Col_o,Row_z) * HT2(Col_P,Row_y) +
        HT1(Col_a,Row_z) * HT2(Col_P,Row_z) +
        HT1(Col_P,Row_z);
      --|last row = [0 0 0 1]
      HT_Res(Col_n, Row_Last) := 0.0;
      HT_Res(Col_o, Row_Last) := 0.0;
      HT_Res(Col_a, Row_Last) := 0.0;
      HT_Res(Col_P, Row_Last) := 1.0;
   end Multiply_HT;

   procedure Set_Indicators (S : in End_Indicator_Type;
                             W : in End_Indicator_Type;
                             E : in Elbow_Indicator_Type) is

   begin
      --|Update SCI, WCI, ECI to parameter values.
      SCI := S;
      WCI := W;
      ECI := E;
   end Set_Indicators;
end Pa_Robotic_Math;


with Pa_Robotic_Math; use Pa_Robotic_Math;
with text_io; use text_io;
with calendar; use calendar;

procedure robtest is

	package Flt_Io is new Float_IO (Float); use Flt_Io;
   	package Int_Io is new Integer_IO (Integer); use Int_Io;

      JS_Pos        : Pa_Robotic_Math.JS_Pos_Type;
      Xt_wrt_Base   : Pa_Robotic_Math.HT_Type;
      Link6_wrt_TAC : Pa_Robotic_Math.HT_Type;
      Link6_wrt_Base: Pa_Robotic_Math.HT_Type;
      Valid         : Boolean := true;

      loop_count    : constant := 10000;

	start, finish : time;
	total	:	float;
	check	:	real := 0.0;
begin

	for i in JS_Pos'range loop
		JS_Pos(i) := 0.23;
	end loop;
	for j in Col_n .. Col_P loop
		for k in Row_x .. Row_Last loop
                        Xt_wrt_Base(j,k) := 0.21;
			Link6_wrt_TAC(j,k) := 0.21;
			Link6_wrt_Base(j,k) := 0.31;
		end loop;
	end loop;

	put_line("Running ERA free motion algorithm ...");
	start := clock;
	for i in 1..loop_count loop
         Pa_Robotic_Math.Multiply_HT(Xt_wrt_Base, Link6_wrt_TAC,Link6_wrt_Base);
         Pa_Robotic_Math.Inverse_Kinematics(Link6_wrt_Base, JS_Pos, Valid);
	end loop;
	finish := clock;

        for i in JS_Pos'range loop
                check := check + JS_Pos(i);
        end loop;
        for j in Col_n .. Col_P loop
                for k in Row_x .. Row_Last loop
                        check := check + Xt_wrt_Base(j,k);
                        check := check + Link6_wrt_TAC(j,k);
                        check := check + Link6_wrt_Base(j,k);
                end loop;
        end loop;

		
	total := float(finish - start);

	put(loop_count,2);
	put(" iterations executed in ");
	put(total);
	put(" seconds (");
	put((total)/float(loop_count));
	put_line(" s/iteration)");
	if (check < 12.8) or (check > 13.0) then
		put_line("Errors during calculations detected!");
	end if;
	new_line;

end robtest;


with Pa_Robotic_Math; use Pa_Robotic_Math;

procedure size_robtest is


      JS_Pos        : Pa_Robotic_Math.JS_Pos_Type;
      Xt_wrt_Base   : Pa_Robotic_Math.HT_Type;
      Link6_wrt_TAC : Pa_Robotic_Math.HT_Type;
      Link6_wrt_Base: Pa_Robotic_Math.HT_Type;
      Valid         : Boolean := true;

      loop_count    : constant := 5000;

begin

	for i in JS_Pos'range loop
		JS_Pos(i) := 1.23;
	end loop;
	for j in Col_n .. Col_P loop
		for k in Row_x .. Row_Last loop
                        Xt_wrt_Base(j,k) := 1.01;
			Link6_wrt_TAC(j,k) := 2.01;
			Link6_wrt_Base(j,k) := 3.01;
		end loop;
	end loop;

	for i in 1..loop_count loop
         Pa_Robotic_Math.Multiply_HT(Xt_wrt_Base, Link6_wrt_TAC,Link6_wrt_Base);
         Pa_Robotic_Math.Inverse_Kinematics(Link6_wrt_Base, JS_Pos, Valid);
	end loop;


end size_robtest;

with kalman;
with estecb;
with cap;
with robtest;

procedure mix is
begin
  kalman;
  estecb;
  cap;
  robtest;
end mix;


with kalman_body;
with size_estecb;
with size_cap;
with size_robtest;

procedure size_mix is
begin
  kalman_body;
  size_estecb;
  size_cap;
  size_robtest;
end;
