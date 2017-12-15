**********************************************************************;
* Project           : JACLS-ALL02
*
* Program name      : JACLS-ALL02_SDTM_PR.sas
*
* Author            : MATSUO YAMAMOTO
*
* Date created      : 20171130
*
* Purpose           : Create RS DataSet
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
%LET FILE = pr;

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
  LENGTH PRCAT $40.;
  SET  WK01;
  STUDYID = "JACLS-ALL02";
  DOMAIN = "PR";
  USUBJID = COMPRESS(STUDYID)||"-"||COMPRESS(PUT(VAR3,Z4.));
  PRTRT = "Hematopoietic Stem Cell Transplantation";

  IF ^MISSING(VAR24) THEN DO;
    IF VAR24 in("R-BMT","Çq-ÇaÇlÇs","R-PBSCT")  THEN PRCAT = "Autotransplantation";
    ELSE IF VAR24 in("BMT","çúêëà⁄êA") THEN PRCAT = "Unknown";
    ELSE PRCAT = "Allotransplantation";
    PRSTDTC = PUT(VAR25,IS8601DA.);
  END ;
  ELSE DELETE;

RUN ;

PROC SORT DATA=WK02 ;BY USUBJID ; RUN ;

DATA  WK10;
  SET  WK02;
  RETAIN PRSEQ;
  BY  USUBJID;
  IF FIRST.USUBJID = 1 THEN PRSEQ = 0;
  PRSEQ = PRSEQ + 1;
RUN ;

PROC SQL ;
   CREATE TABLE LIBSDTM.&FILE AS
   SELECT
    STUDYID  LENGTH=20    LABEL="Study Identifier",
    DOMAIN  LENGTH=2    LABEL="Domain Abbreviation",
    USUBJID  LENGTH=40    LABEL="Unique Subject Identifier",
    PRSEQ      LABEL="Sequence Number",
    PRTRT  LENGTH=200    LABEL="Report Name of Procedure",
    PRCAT  LENGTH=40    LABEL="Category for Procedure",
    PRSTDTC  LENGTH=19    LABEL="Start Date/Time of Procedure"
   FROM WK10;
QUIT ;

FILENAME OUTF "&OUTPUT.\&FILE..csv" ;

PROC EXPORT DATA=LIBSDTM.&FILE. OUTFILE=OUTF 
  DBMS=CSV REPLACE;
RUN ;

%SDTM_FIN;

/*** END ***/
