**********************************************************************;
* Project           : JACLS-ALL02
*
* Program name      : JACLS-ALL02_SDTM_RS.sas
*
* Author            : MATSUO YAMAMOTO
*
* Date created      : 20170330
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
%LET FILE = rs;

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
  LENGTH RSORRES RSSTRESC $20.;
  SET  WK01;
  STUDYID = "JACLS-ALL02";
  DOMAIN = "RS";
  USUBJID = COMPRESS(STUDYID)||"-"||COMPRESS(PUT(VAR3,Z4.));
  RSTESTCD = "CLINRESP";
  RSTEST = "Clinical Response";
  RSCAT = "DEFINED BY PROTOCOL";
  IF  VAR18 = "—L" THEN RSORRES = "CR";
  ELSE IF VAR18 = "–³" THEN RSORRES = "NR";
  ELSE RSORRES = "NE";
  RSSTRESC = COMPRESS(RSORRES);
RUN ;

PROC SORT DATA=WK02 ;BY USUBJID ; RUN ;

DATA  WK10;
  SET  WK02;
  RETAIN RSSEQ;
  BY  USUBJID;
  IF FIRST.USUBJID = 1 THEN RSSEQ = 0;
  RSSEQ = RSSEQ + 1;
RUN ;

PROC SQL ;
   CREATE TABLE LIBSDTM.&FILE AS
   SELECT
    STUDYID  LENGTH=20    LABEL="Study Identifier",
    DOMAIN  LENGTH=2    LABEL="Domain Abbreviation",
    USUBJID  LENGTH=40    LABEL="Unique Subject Identifier",
    RSSEQ      LABEL="Sequence Number",
    RSTESTCD  LENGTH=8    LABEL="Response Assessment Short Name",
    RSTEST  LENGTH=40    LABEL="Response Assessment Name",
    RSCAT  LENGTH=40    LABEL="Category for Response Assessment",
    RSORRES  LENGTH=200    LABEL="Response Assessment Original Result",
    RSSTRESC  LENGTH=200    LABEL="Character Result/Finding in Std Format"
   FROM WK10;
QUIT ;

FILENAME OUTF "&OUTPUT.\&FILE..csv" ;

PROC EXPORT DATA=LIBSDTM.&FILE. OUTFILE=OUTF 
  DBMS=CSV REPLACE;
RUN ;

%SDTM_FIN;

/*** END ***/
