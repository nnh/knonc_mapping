**********************************************************************;
* Project           : JACLS-ALL02
*
* Program name      : JACLS-ALL02_SDTM_MH.sas
*
* Author            : MATSUO YAMAMOTO
*
* Date created      : 20170330
*
* Purpose           : Create MH DataSet
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
%LET FILE = mh;

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
DATA  WK02;
  SET  WK01;
  STUDYID = "JACLS-ALL02";
  DOMAIN = "MH";
  USUBJID = COMPRESS(STUDYID)||"-"||COMPRESS(PUT(VAR3,Z4.));
  MHCAT = "PRIMARY DIAGNOSIS";
  MHTERM = "小児急性リンパ性白血病";
  MHDECOD = "小児急性リンパ性白血病";
  MHSTDTC = PUT(VAR9,IS8601DA.);
  MHPTCD = 20064495;
  MHSOC = "急性リンパ芽球性白血病";
  MHSOCCD = "C91.0";
RUN ;

PROC SORT DATA=WK02 ;BY USUBJID MHSTDTC; RUN ;

DATA  WK10;
  SET  WK02;
  RETAIN MHSEQ;
  BY  USUBJID;
  IF FIRST.USUBJID = 1 THEN MHSEQ = 0;
  MHSEQ = MHSEQ + 1;
RUN ;

PROC SQL ;
   CREATE TABLE LIBSDTM.&FILE AS
   SELECT
    STUDYID  LENGTH=20    LABEL="Study Identifier",
    DOMAIN  LENGTH=2    LABEL="Domain Abbreviation",
    USUBJID  LENGTH=40    LABEL="Unique Subject Identifier",
    MHSEQ     LABEL="Sequence Number",
    MHCAT  LENGTH=40    LABEL="Category for Medical History",
    MHTERM  LENGTH=200    LABEL="Reported Term for the Medical History",
    MHDECOD  LENGTH=200    LABEL="Dictionary-Derived Term",
    MHSTDTC  LENGTH=19    LABEL="Start Date/Time of Medical History Event",
    MHPTCD     LABEL="Preferred Term Code",
    MHSOC  LENGTH=60    LABEL="Primary System Organ Class",
    MHSOCCD  LENGTH=20    LABEL="Primary System Organ Class Code"
   FROM WK10;
QUIT ;

FILENAME OUTF "&OUTPUT.\&FILE..csv" ;

PROC EXPORT DATA=LIBSDTM.&FILE. OUTFILE=OUTF 
  DBMS=CSV REPLACE;
RUN ;

%SDTM_FIN;

/*** END ***/
