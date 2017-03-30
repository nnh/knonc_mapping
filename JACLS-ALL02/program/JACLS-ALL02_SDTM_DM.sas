**********************************************************************;
* Project           : JACLS-ALL02
*
* Program name      : JACLS-ALL02_SDTM_DM.sas
*
* Author            : MATSUO YAMAMOTO
*
* Date created      : 20170330
*
* Purpose           : Create DM DataSet
*
* Revision History  : 
*
* Date        Author           Ref    Revision (Date in YYYYMMDD format)
* 
*
**********************************************************************;

/*** Initial setting ***/
%MACRO CURRENT_DIR;

    %LOCAL _FULLPATH _PATH;
    %LET   _FULLPATH = ;
    %LET   _PATH     = ;

    %IF %LENGTH(%SYSFUNC(GETOPTION(SYSIN))) = 0 %THEN
        %LET _FULLPATH = %SYSGET(SAS_EXECFILEPATH);
    %ELSE
        %LET _FULLPATH = %SYSFUNC(GETOPTION(SYSIN));

    %LET _PATH = %SUBSTR(   &_FULLPATH., 1, %LENGTH(&_FULLPATH.)
                          - %LENGTH(%SCAN(&_FULLPATH.,-1,'\')) -1 );

    &_PATH.

%MEND CURRENT_DIR;

%LET _PATH2 = %CURRENT_DIR;
%LET FILE = dm;

%INCLUDE "&_PATH2.\JACLS-ALL02_SDTM_LIBNAME.sas";

/*** CSV read ***/
PROC IMPORT OUT= MAIN
  DATAFILE="&RAW.\ALL-02_OSEFSdata v1.csv"
  DBMS=CSV REPLACE;
  GETNAMES=NO;
  DATAROW=1;
  GUESSINGROWS=5000; 
RUN; 

/*** Extraction of mapping target ***/
DATA  WK01;
  SET  MAIN;
  IF  VAR1 = "0"; ***Eligibility only ;
RUN ;

/*** Mapping ***/
DATA  WK10;
  LENGTH ARMCD ACTARMCD $20. ARM ACTARM $200.;
  SET  WK01;
  STUDYID = "JACLS-ALL02";
  DOMAIN = "DM";
  USUBJID = COMPRESS(STUDYID)||"-"||COMPRESS(PUT(VAR3,Z4.));
  SUBJID = COMPRESS(PUT(VAR3,BEST.));
  RFSTDTC = "";
  BRTHDTC = "";
  AGE = VAR10;
  AGEU = "YEARS";
  IF  VAR12 = 0 THEN SEX = "M";
  ELSE IF VAR12 = 1 THEN SEX = "F";
  ELSE SEX = "U";

  *** Set ARMCD and ARM;
  IF VAR6 = "HR" AND VAR8 = "A" THEN DO; ARMCD = "SR-02A"; ARM = "SR-02A"; END;
  ELSE IF VAR6 = "HR" AND VAR8 = "B" THEN DO; ARMCD = "SR-02B"; ARM = "SR-02B"; END;
  ELSE IF VAR6 = "HR" AND VAR8 = "" THEN DO; ARMCD = "INDFAIL"; ARM = "Induction Failure"; END;
  ELSE IF VAR6 = "SR" AND VAR8 = "A" THEN DO; ARMCD = "HR-02A"; ARM = "HR-02A"; END;
  ELSE IF VAR6 = "SR" AND VAR8 = "B" THEN DO; ARMCD = "HR-02B"; ARM = "HR-02B"; END;
  ELSE IF VAR6 = "SR" AND VAR8 = "" THEN DO; ARMCD = "INDFAIL"; ARM = "Induction Failure"; END;
  ELSE IF VAR6 = "ER" THEN DO; ARMCD = "ER-02"; ARM = "ER-02"; END;
  ELSE IF VAR6 = "F" THEN DO; ARMCD = "F-02"; ARM = "F-02"; END;
  ELSE IF VAR6 = "T" THEN DO; ARMCD = "T-02"; ARM = "T-02"; END;
  ELSE IF VAR6 = ""  THEN DO; ARMCD = "INDFAIL"; ARM = "Induction Failure"; END;
  ELSE DO; ARMCD = "U"; ARM = "Unknown"; END;

  *** Set ACTARMCD and ACTARM;
  IF  ARMCD="SR-02B" AND VAR2 = "Š„•t‚¯B‚¾‚ªA‚ÅŽ¡—Ã" THEN DO; ACTARMCD = "SR-02A"; ACTARM = "SR-02A"; END;
  ELSE IF  ARMCD="HR-02B" AND VAR2 = "Š„•t‚¯B‚¾‚ªA‚ÅŽ¡—Ã" THEN DO; ACTARMCD = "HR-02A"; ACTARM = "HR-02A"; END;
  ELSE DO; ACTARMCD = ARMCD; ACTARM = ARM; END;

  COUNTRY = "JPN";
  RFXSTDTC = PUT(VAR15,IS8601DA.);
  RFICDTC = "";
  DROP VAR:;
RUN ;

PROC SQL ;
   CREATE TABLE LIBSDTM.&FILE AS
   SELECT
    STUDYID  LENGTH=20    LABEL="Study Identifier",
    DOMAIN  LENGTH=2    LABEL="Domain Abbreviation",
    USUBJID  LENGTH=40    LABEL="Unique Subject Identifier",
    SUBJID  LENGTH=8    LABEL="Subject Identifier for the Study",
    RFSTDTC  LENGTH=19    LABEL="Subject Reference Start Date/Time",
    BRTHDTC  LENGTH=19    LABEL="Date/Time of Birth",
    AGE      LABEL="Age",
    AGEU  LENGTH=20    LABEL="Age Units",
    SEX  LENGTH=1    LABEL="Sex",
    ARMCD  LENGTH=20    LABEL="Planned Arm Code",
    ARM  LENGTH=200    LABEL="Description of Planned Arm",
    ACTARMCD   LENGTH=20    LABEL="Actual Arm Code ",
    ACTARM   LENGTH=200    LABEL="Description of Actual Arm ",
    COUNTRY  LENGTH=3    LABEL="Country",
    RFXSTDTC   LENGTH=19    LABEL="Date/Time of First Study Treatment ",
    RFICDTC   LENGTH=19    LABEL="Date/Time of Informed Consent "
   FROM WK10;
QUIT ;

FILENAME OUTF "&OUTPUT.\&FILE..csv" ;

PROC EXPORT DATA=LIBSDTM.&FILE. OUTFILE=OUTF 
  DBMS=CSV REPLACE;
RUN ;

%SDTM_FIN;

/*** END ***/
