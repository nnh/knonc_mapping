**********************************************************************;
* Project           : Macro
*
* Program name      : SDTM_FIN.sas
*
* Author            : MATSUO YAMAMOTO
*
* Date created      : 20151102
*
* Purpose           :
*
* Revision History  :
*
* Date        Author           Ref    Revision (Date in YYYYMMDD format)
* YYYYMMDD    XXXXXX XXXXXXXX  1      XXXXXXXXXXXXXXXXXXXXXXXXXXXX
*
**********************************************************************;

%MACRO SDTM_FIN ;

  DATA _NULL_ ;
       CALL SYMPUT( "_YYMM_" , COMPRESS( PUT( DATE() , YYMMDDN8. ) ) ) ;
       CALL SYMPUT( "_TIME_" , COMPRESS( PUT( TIME() , TIME5. ) , " :" ) ) ;
  RUN ;

  DM LOG "FILE '&LOG.\&FILE._SDTM_LOG_&_YYMM_._&_TIME_..txt' REPLACE" ;
  DM "OUTPUT ; CLEAR ; LOG ; CLEAR ; " ;
%MEND ;
