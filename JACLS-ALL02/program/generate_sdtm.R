# **********************************************************************;
# * Project           : JACLS-ALL02
# *
# * Program name      : generate_sdtm.R
# *
# * Author            : MARIKO OHTSUKA
# *
# * Date created      : 20170318
# *
# * Purpose           : Generate SDTM DataSet
# *
# * Revision History  : 
# *
# * Date        Author           Ref    Revision (Date in YYYYMMDD format)
# * 
# *
# **********************************************************************;
# function
# ############################################## # 
# SetSEQ function                                #
# Set Serial number for each Primary key         #
# Arguments : dst is dataset                     #
#             pkey is Primary key                #
#             columnname is Column name to set   #
#               key                              #
# Return : dst$columnname                        #
# ############################################## # 
SetSEQ <- function(dst, pkey, columnname) {
  #Init
  cntseq = 0
  save.pkey <- ""
  
  # If the keys are changes, add 1
  for (i in 1:nrow(dst)) {
    if (dst[i,pkey] != save.pkey) {
      cntseq <- cntseq + 1
    }  
    dst[i,columnname] <- cntseq
    save.pkey <- dst[i,pkey]
  }
  return(dst[,columnname])
}
# ####################################################### # 
# SetDmARMCD function 　                                  #
# Set DM$ARMCD,ARM  　　　　　　　　　　                  #
# Arguments : dst is dataframe(inputdata$確定リスク,割付) #
# Return : resdst is dataframe(ARMCD,ARM)                 #
# ####################################################### # 
SetDmARMCD <- function(dst) {
  # Initialize
  armcd <- NA
  arm <- NA
  resdst <- data.frame(armcd,arm)
  
  # Set search table
  search1.risk <- c("ER","F","T","HR","SR",NA)
  search1.armcd <- c("ER-02","F-02","T-02","SR-02","HR-02","")
  search1.arm <- c("ER-02","F-02","T-02","SR-02","HR-02","")
  search1.T <- data.frame(risk=search1.risk,armcd=search1.armcd,arm=search1.arm)
  indf.armcd <- "INDFAIL"
  indf.arm <- "Induction Failure"
  indf.T <- data.frame(armcd=indf.armcd,arm=indf.arm)
  other.armcd <- "U"
  other.arm <- "Unknown"
  other.T <- data.frame(armcd=other.armcd,arm=other.arm)
  
  # Main
  for (i in 1:nrow(dst)){
    wk.risk <- dst[i,"risk"]
    wk.alloc <- dst[i,"alloc"]
    # When wk.risk is not exist in search1.T$risk, arm.T is set to NA.
    arm.T <- search1.T[search1.T$risk==wk.risk,]
    arm.T <- arm.T[1,]  # When NA is set, multiple row
    # wk.risk is exist in search1.T$risk
    if(!is.na(arm.T$armcd)){
      if (is.na(wk.risk)){
        # risk is NA : "Induction Failure"
        arm.T <- indf.T
      }
      else if(wk.risk == "HR" || wk.risk == "SR"){
        # wk.alloc is blank : "Induction Failure"
        # wk.alloc is not "A" or "B" : "Unknown"
        if (is.na(wk.alloc)){
          arm.T <- indf.T
        }
        else if(wk.alloc == "A" || wk.alloc == "B"){
          arm.T$armcd <- paste(arm.T$armcd, wk.alloc, sep="")
          arm.T$arm <- paste(arm.T$arm, wk.alloc, sep="")
        }
        else{
          # Unknown
          arm.T <- other.T
        }
      }
    }
    # wk.risk is not exist in search1.T$risk : UnKnown
    else{
      arm.T <- other.T
    }
    resdst[i,"armcd"] <- as.character(arm.T$armcd)
    resdst[i,"arm"] <- as.character(arm.T$arm)
  }
  return(resdst)
}
# ######################################################### # 
# SetCeCEDECOD function 　                                  #
# Set CE$CEDECOD,CEDTC  　　　　　　　　　　                #
# Arguments : dst is dataframe(inputdata$二次がん,中止理由  #
#                              、再発日)                    #
# Return : resdst is dataframe(CEDECOD,CEDTC)               #
# ######################################################### # 
SetCeCEDECOD <- function(dst) {
  # Initialize
  cedecod <- NA
  cedtc <- NA
  resdst <- data.frame(cedecod,cedtc)
  # Main
  for (i in 1:nrow(dst)){
    wk.sec.ca <- dst[i,"sec.ca.date"]
    wk.discontinuing <- dst[i,"discontinuing"]
    
    #二次がん日が空白でなければ「SECONDARY CANCER」二次がん日をセット
    if(!is.na(wk.sec.ca)){
      resdst[i,"cedecod"] <- "SECONDARY CANCER"
      resdst[i,"cedtc"] <- as.character(as.Date(wk.sec.ca))
    }
    
    #中止理由が*再発*なら「DISEASE RELAPSE」再発日をセット
    if(!is.na(wk.discontinuing)){
      if(regexpr("再発",wk.discontinuing)>0){
        resdst[i,"cedecod"] <- "DISEASE RELAPSE"
        resdst[i,"cedtc"] <- as.character(as.Date(dst[i,"relapse.date"]))
      }
    }
  }
  return(resdst)
}
# ######################################################### # 
# SetDsDSDECOD function 　                                  #
# Set DS$DSDECOD,DSDTC  　　　　　　　　　　                #
# Arguments : dst is dataframe(inputdata$二次がん,中止理由  #
#                              、再発日)                    #
# Return : resdst is dataframe(DSDECOD,DSDTC)               #
# ######################################################### # 
SetDsDSDECOD <- function(dst) {
  # Initialize
  dsdecod <- NA
  dsdtc <- NA
  resdst <- data.frame(dsdecod,dsdtc)
  # Set grep table
  grep1.discontinuing <- c("有害事象のため","死亡","プロトコール違反","不適格性","担当医により中止","治療リスクF群")
  grep1.decod.str <- c("ADVERSE EVENT","DEATH","PROTOCOL DEVIATION","PROTOCOL DEVIATION","PHYSICIAN DECISION","PHYSICIAN DECISION")
  grep1.dtc.clm.nm <- c("discontinuing.date","death.date","discontinuing.date","discontinuing.date","discontinuing.date","discontinuing.date")
  grep1.T <- data.frame(discontinuing=grep1.discontinuing,dsdecod=grep1.decod.str,dsdtc=grep1.dtc.clm.nm)

    # Main
  for (i in 1:nrow(dst)){
    wk.discontinuing <- dst[i,"discontinuing"]
    wk.endtype <- dst[i,"endtype"]
    
    # 終了種別が完了ならDSDECOD = ""COMPLETED"、最終確認日
    if(!is.na(wk.endtype)){
      if(wk.endtype == "完了"){
        resdst[i,"dsdecod"] <- "COMPLETED"
        resdst[i,"dsdtc"] <- as.character(as.Date(dst[i,"last.cfm.date"]))
      }
      else{
        if(!is.na(wk.discontinuing)){
          for (j in 1:nrow(grep1.T)){
            wk.grep <- regexpr(as.character(grep1.T[j,"discontinuing"]),as.character(wk.discontinuing))
            if (wk.grep > 0){ 
              resdst[i,"dsdecod"] <- as.character(grep1.T[j,"dsdecod"])
              wk.clm <- as.character(grep1.T[j,"dsdtc"])
              resdst[i,"dsdtc"] <- as.character(as.Date(dst[i,wk.clm]))
              break
            }
          }
        }
      }
    }
    #どれにもあてはまらなければother,中止日
    if (is.na(resdst[i,"dsdecod"])){
      resdst[i,"dsdecod"] <- "OTHER"
      resdst[i,"dsdtc"] <- as.character(as.Date(dst[i,"discontinuing.date"]))
    }
  }
  return(resdst)
}
# #################

# Constant
kStudyId <- "JACLS-ALL02"

# Load data
setwd(kStudyId)
setwd("./input/rawdata")
filenames <- list.files()
rawdata <- read.csv(filenames[1], as.is = T, na.strings = c(""), fileEncoding = "CP932", header = F)
setwd("../..")

# Format data
dataset <- rawdata[!is.na(rawdata[,3]), ]
# rawdataに列名がなかったのでセット
colnames(dataset) <- c("VAR1", "アーム間違い", "ALL02.No", "VAR4", "VAR5", "確定リスク", "寛解判定", "割付", "診断年月日", "診断時年齢", "VAR11", "性別", "VAR13", "VAR14", "induction.therapy開始日", "VAR16", "VAR17", "VAR18", "VAR19", "VAR20", "VAR21", "終了種別", "VAR23", "VAR24", "VAR25", "中止日", "中止理由", "VAR28", "VAR29", "最終確認日", "VAR31", "VAR32", "VAR33", "再発日", "VAR35", "二次がん日", "VAR37", "生死日", "VAR39", "VAR40", "VAR41", "VAR42", "VAR43", "VAR44", "VAR45", "VAR46", "VAR47", "VAR48", "VAR49", "VAR50", "VAR51", "VAR52", "VAR53", "VAR54", "VAR55", "VAR56", "VAR57", "VAR58", "VAR59")
dm <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
dm$DOMAIN <- "DM" 
all02.No <- formatC(dataset$ALL02.No, width = 3, flag = "0")
dm$USUBJID <- paste(kStudyId, all02.No, sep = "-")
dm$SUBJID <- all02.No
dm$RFSTDTC <- NA
dm$BRTHDTC <- NA
dm$AGE <- dataset$診断時年齢
dm$AGEU <- "YEARS"
dm$SEX <- dataset$性別
wk.dm <- data.frame(risk=dataset$確定リスク,alloc=dataset$割付)
wk.arm.T <- SetDmARMCD(wk.dm)
dm$ARMCD <- wk.arm.T$armcd
dm$ARM <- wk.arm.T$arm
dm$ACTARMCD <- dm$ARMCD
dm$ACTARM <- dm$ARM
dm$COUNTRY <- "JPN"
dm$RFXSTDTC <- as.character(as.Date(dataset$induction.therapy開始日))  # ISO8601format
dm$RFICDTC <- NA

# ####### CE #######  
ce <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
ce$DOMAIN <- "CE"
ce$USUBJID <- paste(kStudyId, dataset$検体ID, sep = "-")
ce$CESEQ <- NA
wk.ce <- data.frame(discontinuing=dataset$中止理由,sec.ca.date=dataset$二次がん日,relapse.date=dataset$再発日)
wk.cedecod.T <- SetCeCEDECOD(wk.ce)
ce$CETERM <-wk.cedecod.T$cedecod
ce$CEDECOD <- ce$CETERM
ce$CEDTC <- wk.cedecod.T$cedtc
# Sort(asc) KEY=USUBJID,CETERM,CEDTC
ceSortlist <- order(ce$USUBJID, ce$CETERM, ce$CEDTC)
ce <- ce[ceSortlist,]
# Set CESEQ  Serial number for each USUBJID
ce$CESEQ = SetSEQ(ce, "USUBJID", "CESEQ")

# ####### DS #######  
ds <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
ds$DOMAIN <- "DS"
ds$USUBJID <- paste(kStudyId, dataset$検体ID, sep = "-")
ds$DSSEQ <- NA
wk.ds <- data.frame(discontinuing=dataset$中止理由,endtype=dataset$終了種別,last.cfm.date=dataset$最終確認日,discontinuing.date=dataset$中止日,death.date=dataset$生死日)
wk.dsdecod.T <- SetDsDSDECOD(wk.ds)
ds$DSTERM <- wk.dsdecod.T$dsdecod 
ds$DSDECOD <- ds$DSTERM
ds$DSCAT <- "DISPOSITION EVENT"
ds$DSSTDTC <- wk.dsdecod.T$dsdtc  # ISO8601format
# Sort(asc) KEY=USUBJID,DSTERM,DSSTDTC
dssortlist <- order(ds$USUBJID, ds$DSTERM, ds$DSSTDTC)
ds <- ds[dssortlist,]
# Set DSSEQ  Serial number for each USUBJID
ds$DSSEQ = SetSEQ(ds, "USUBJID", "DSSEQ")

# ####### MH #######  
mh <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
mh$DOMAIN <- "MH"
mh$USUBJID <- paste(kStudyId, dataset$検体ID, sep = "-")
mh$MHSEQ <- NA
mh$MHCAT <- "PRIMARY DIAGNOSIS"
mh$MHTERM <- "急性骨髄性白血病"
mh$MHDECOD <- mh$MHTERM
mh$MHSTDTC <- NA
mh$MHPTCD <- 20058373
mh$MHSOC <- mh$MHTERM
mh$MHSOCCD <- "C92.0"
# Sort(asc) KEY=USUBJID  *MHTERM,MHSTDTC -> constant
mhsortlist <- order(mh$USUBJID)
mh <- mh[mhsortlist,]
# Set MHSEQ  Serial number for each USUBJID
mh$MHSEQ = SetSEQ(mh, "USUBJID", "MHSEQ")

# ####### RS #######  
rs <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
rs$DOMAIN <- "RS"
rs$USUBJID <- paste(kStudyId, dataset$検体ID, sep = "-")
rs$RSSEQ <- NA
rs$RSTESTCD <- "CLINRESP"
rs$RSTEST <- "Clinical Response"
rs$RSCAT <- "DEFINED BY PROTOCOL"
rs$RSORRES <- ifelse(dataset$CR == "CR", "CR",
              ifelse(dataset$CR == "nonCR", "NR", "NE"))
rs$RSSTRESC <- rs$RSORRES
# Sort(asc) KEY=USUBJID  
rssortlist <- order(rs$USUBJID)
rs <- rs[rssortlist,]
# Set RSSEQ  Serial number for each USUBJID
rs$RSSEQ = SetSEQ(rs, "USUBJID", "RSSEQ")

# Save datasets
setwd("./output/SDTM")
write.csv(dm, "dm.csv", row.names = F, na = "")
write.csv(ce, "ce.csv", row.names = F, na = "")
write.csv(ds, "ds.csv", row.names = F, na = "")
write.csv(mh, "mh.csv", row.names = F, na = "")
write.csv(rs, "rs.csv", row.names = F, na = "")
setwd("../../..")
