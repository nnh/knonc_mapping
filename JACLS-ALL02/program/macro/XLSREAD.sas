**********************************************************************;
* Project           : Macro
*
* Program name      : XLSREAD.sas
*
* Author            : MATSUO YAMAMOTO
*
* Date created      : 20151026
*
* Purpose           : 
*
* Revision History  :
*
* Date        Author           Ref    Revision (Date in YYYYMMDD format) 
* YYYYMMDD    XXXXXX XXXXXXXX  1      XXXXXXXXXXXXXXXXXXXXXXXXXXXX
*
**********************************************************************;

%MACRO XLSREAD(XLSFILE,SHEET,OUTDS,CNUM,RNUM=3,READLIB=&EXT.,CNUMST=1);

  OPTIONS NOXSYNC NOXWAIT;
  X "'&READLIB./&XLSFILE'";

  DATA _NULL_;         
    X=SLEEP(3);
  RUN;

  FILENAME XLS DDE "EXCEL | [&&XLSFILE.]&SHEET.!R&RNUM.C&CNUMST.:R1048576C&CNUM." lrecl=10000;
  DATA &OUTDS ;    
    LENGTH COL&CNUMST.-COL&CNUM. $200;
    INFILE XLS NOTAB MISSOVER DSD DLM="09"X;
    INPUT  COL&CNUMST.-COL&CNUM. $;
  RUN;

  FILENAME SYSTEM DDE 'EXCEL|SYSTEM';
  DATA _NULL_;
    FILE SYSTEM;
    PUT '[QUIT()]';
  RUN;
  
%MEND  XLSREAD;
