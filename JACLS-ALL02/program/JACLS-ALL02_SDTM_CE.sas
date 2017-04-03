**********************************************************************;
* Project           : JACLS-ALL02
*
* Program name      : JACLS-ALL02_SDTM_CE.sas
*
* Author            : MATSUO YAMAMOTO(ŽR–{¼—Y)
*
* Date created      : 20170330
*
* Purpose           : Create CE DataSet
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
%LET FILE = ce;

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
  LENGTH CETERM CEDECOD $200.;
  SET  WK01;
  STUDYID = "JACLS-ALL02";
  DOMAIN = "CE";
  USUBJID = COMPRESS(STUDYID)||"-"||COMPRESS(PUT(VAR3,Z4.));
  IF  KINDEX(VAR27,"Ä”­")>0 THEN DO;
    CETERM = "DISEASE RELAPSE";
    CEDECOD = "DISEASE RELAPSE";
    CESTDTC = PUT(VAR34,IS8601DA.);
  END ;
  ELSE DELETE;
RUN ;

DATA  WK03;
  LENGTH CETERM CEDECOD $200.;
  SET  WK01;
  STUDYID = "JACLS-ALL02";
  DOMAIN = "CE";
  USUBJID = COMPRESS(STUDYID)||"-"||COMPRESS(PUT(VAR3,Z4.));
  IF  ^MISSING(VAR36) THEN DO;
    CETERM = "SECONDARY CANCER";
    CEDECOD = "SECONDARY CANCER";
    CESTDTC = PUT(VAR36,IS8601DA.);
  END ;
  ELSE DELETE;
RUN ;

DATA  WK21;
  SET  WK02 WK03;
  DROP VAR:;
RUN ;

PROC SORT DATA=WK21 ;BY USUBJID CESTDTC; RUN ;

DATA  WK20;
  SET  WK21;
  RETAIN CESEQ;
  BY  USUBJID;
  IF FIRST.USUBJID = 1 THEN CESEQ = 0;
  CESEQ = CESEQ + 1;
RUN ;

PROC SQL ;
   CREATE TABLE LIBSDTM.&FILE AS
   SELECT
    STUDYID  LENGTH=20    LABEL="Study Identifier",
    DOMAIN  LENGTH=2    LABEL="Domain Abbreviation",
    USUBJID  LENGTH=40    LABEL="Unique Subject Identifier",
    CESEQ      LABEL="Sequence Number",
    CETERM  LENGTH=200    LABEL="Reported Term for the Clinical Event",
    CEDECOD  LENGTH=200    LABEL="Standardized Clinical Term",
    CESTDTC  LENGTH=19    LABEL="Start Date/Time of Clinical Event"
   FROM WK20;
QUIT ;

FILENAME OUTF "&OUTPUT.\&FILE..csv" ;

PROC EXPORT DATA=LIBSDTM.&FILE. OUTFILE=OUTF 
  DBMS=CSV REPLACE;
RUN ;

%SDTM_FIN;

/*** END ***/
