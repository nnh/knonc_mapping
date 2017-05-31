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
  # Initialize
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
  return (dst[,columnname])
  
  # finalize
  rm(dst)
}
# ############################################## # 
# ISO8601Date function                           #
# YYYY/MM/DD -> YYYY-MM-DD                       #
# Arguments : dte is Date                        #
# Return :    Date(ISO8601format)                #
# ############################################## # 
ISO8601Date <- function(dte) {
  return (as.character(as.Date(dte)))
}
# ####################################################### # 
# SetDmARMCD function 　                                  #
# Set DM$ARMCD,ARM  　　　　　　　　　　                  #
# Arguments : dst is dataframe(inputdata$確定リスク,割付) #
# Return : resdst is dataframe(ARMCD,ARM,ACTARMCD,ACTARM) #
# ####################################################### # 
SetDmARMCD <- function(dst) {
  
  # Initialize
  armcd <- NA
  arm <- NA
  actarmcd <- NA
  actarm <- NA
  resdst <- data.frame(armcd, arm, actarmcd, actarm)
  
  # Main
  for (i in 1:nrow(dst)) {
    # If it does not match any condition, set "Unknown".
    armcd <- "U"
    arm <- "Unknown"
    actarmcd <- NA
    actarm <- NA
    
    wk.risk <- dst[i, "risk"]
    wk.alloc <- dst[i, "alloc"]
    wk.arm.err <- dst[i,"arm.err"]
    
    if (is.na(wk.risk)) {
      # "risk" is NA : "Induction Failure"
      armcd <- "INDFAIL"
      arm <- "Induction Failure"
    }
    else if (wk.risk == "ER" || wk.risk == "F" || wk.risk == "T") {
      armcd <- paste(wk.risk, "-02", sep="")
      arm <- armcd
    }
    else if (wk.risk == "HR" || wk.risk == "SR") {
      if (is.na(wk.alloc)) {
        # "allocations" is blank : "Induction Failure"
        armcd <- "INDFAIL"
        arm <- "Induction Failure"
      }
      else if (wk.alloc == "A" || wk.alloc == "B") {
        # Replace "SR" <-> "HR"
        ifelse(wk.risk == "HR", rep.risk <- "SR", rep.risk <- "HR")
        armcd <- paste(rep.risk, "-02", wk.alloc, sep="")
        arm <- armcd
        if (!is.na(wk.arm.err)) {
          # arm error
          if (wk.arm.err == "割付けBだがAで治療" && wk.alloc == "B") {
            actarmcd <- gsub("B$", "A", armcd)
            actarm <- actarmcd
          }
        }
      }
    }
      
    if (is.na(actarmcd)) {
      # If "arm" has no error, "actarmcd and actarm" is set 
      # same value as "armcd and arm". 
      actarmcd <- armcd
      actarm <- arm
    }
    resdst[i, "armcd"] <- armcd
    resdst[i, "arm"] <- arm
    resdst[i, "actarmcd"] <- actarmcd
    resdst[i, "actarm"] <- actarm
  }
  return (resdst)
  
  # finalize
  rm(dst)
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
  resdst <- data.frame(cedecod, cedtc)
  
  # Main
  for (i in 1:nrow(dst)) {
    # If it does not match any condition, set NA
    cedecod <- NA
    cedtc <- NA
    wk.sec.ca <- dst[i, "sec.ca.date"]
    wk.discontinuing <- dst[i, "discontinuing"]
    
    if (!is.na(wk.sec.ca)) {
      # "date of SECONDARY CANCER" is not blank
      # : set "SECONDARY CANCER", "date of SECONDARY CANCER"
      cedecod <- "SECONDARY CANCER"
      cedtc <- ISO8601Date(wk.sec.ca)
    }
    
    if (!is.na(wk.discontinuing)) {
      if (regexpr("再発", wk.discontinuing) > 0) {
        # "discontinuing" is "RELAPSE" 
        # : set "DISEASE RELAPSE", "date of relapse"
        cedecod <- "DISEASE RELAPSE"
        cedtc <- ISO8601Date(dst[i,"relapse.date"])
      }
    }

    resdst[i,"cedecod"] <- cedecod
    resdst[i,"cedtc"] <- cedtc
  }
  return (resdst)
  
  # finalize
  rm(resdst)
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
  resdst <- data.frame(dsdecod, dsdtc)
  
  # Pattern matching table
  grep1.discontinuing <- c("有害事象のため", "死亡", "プロトコール違反", "不適格性", "担当医により中止", "治療リスクF群")
  grep1.decod.str <- c("ADVERSE EVENT", "DEATH", "PROTOCOL DEVIATION", "PROTOCOL DEVIATION", "PHYSICIAN DECISION", "PHYSICIAN DECISION")
  grep1.dtc.clm.nm <- c("discontinuing.date", "death.date", "discontinuing.date", "discontinuing.date", "discontinuing.date", "discontinuing.date")
  grep1.T <- data.frame(discontinuing=grep1.discontinuing, dsdecod=grep1.decod.str, dsdtc=grep1.dtc.clm.nm)
  # type : factor -> character
  grep1.T$discontinuing <- as.character(grep1.T$discontinuing)
  grep1.T$dsdecod <- as.character(grep1.T$dsdecod)
  grep1.T$dsdtc <- as.character(grep1.T$dsdtc)
  
  # Main
  for (i in 1:nrow(dst)) {
    # If it does not match any condition, set "OTHER"
    dsdecod <- "OTHER"
    dsdtc <- ISO8601Date(dst[i, "discontinuing.date"])
    wk.discontinuing <- as.character(dst[i, "discontinuing"])
    wk.endtype <- dst[i, "endtype"]
    
    if (!is.na(wk.endtype)) {
      if (wk.endtype == "完了") {
        # Endtype is Completion : set "COMPLETED", 
        # date last confirmed
        dsdecod <- "COMPLETED"
        dsdtc <- ISO8601Date(dst[i,"last.cfm.date"])
      }
      else {
        # Endtype is not Completion
        if (!is.na(wk.discontinuing)) {
          # Search Pattern matching table
          for (j in 1:nrow(grep1.T)) {
            wk.grep <- regexpr(grep1.T[j,"discontinuing"], wk.discontinuing)
            if (wk.grep > 0) { 
              dsdecod <- grep1.T[j, "dsdecod"]
              # get columnsname
              wk.clm <- grep1.T[j, "dsdtc"]
              dsdtc <- ISO8601Date(dst[i, wk.clm])
              break
            }
          }
        }
      }
    }

    resdst[i, "dsdecod"] <- dsdecod
    resdst[i, "dsdtc"] <- dsdtc
  }
  return (resdst)
  # finalize
  rm(resdst)
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
dataset <- rawdata[!is.na(rawdata[, 3]), ]
# rawdataに列名がなかったのでセット,不要なら削除
# Set columns name
colnames(dataset) <- c("VAR1", "アーム間違い", "ALL02.No", "VAR4", "VAR5", "確定リスク", "寛解判定", "割付", "診断年月日", "診断時年齢", "VAR11", "性別", "VAR13", "VAR14", "induction.therapy開始日", "VAR16", "VAR17", "VAR18", "VAR19", "VAR20", "VAR21", "終了種別", "VAR23", "VAR24", "VAR25", "中止日", "中止理由", "VAR28", "VAR29", "最終確認日", "VAR31", "VAR32", "VAR33", "再発日", "VAR35", "二次がん日", "VAR37", "生死日", "VAR39", "VAR40", "VAR41", "VAR42", "VAR43", "VAR44", "VAR45", "VAR46", "VAR47", "VAR48", "VAR49", "VAR50", "VAR51", "VAR52", "VAR53", "VAR54", "VAR55", "VAR56", "VAR57", "VAR58", "VAR59")
dm <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
dm$DOMAIN <- "DM" 
# Zero padding (4-digit) ex) 1->0001
all02.No <- formatC(dataset$ALL02.No, width = 4, flag = "0")
dm$USUBJID <- paste(kStudyId, all02.No, sep = "-")
dm$SUBJID <- all02.No
dm$RFSTDTC <- NA
dm$BRTHDTC <- NA
dm$AGE <- dataset$診断時年齢
dm$AGEU <- "YEARS"
dm$SEX <- ifelse(dataset$性別 == 0, "M",
          ifelse(dataset$性別 == 1, "F", "U"))
wk.dm <- data.frame(risk=dataset$確定リスク, alloc=dataset$割付, arm.err=dataset$アーム間違い)
wk.arm.T <- SetDmARMCD(wk.dm)
dm$ARMCD <- wk.arm.T$armcd
dm$ARM <- wk.arm.T$arm
dm$ACTARMCD <- wk.arm.T$actarmcd
dm$ACTARM <- wk.arm.T$actarm
dm$COUNTRY <- "JPN"
dm$RFXSTDTC <- ISO8601Date(dataset$induction.therapy開始日)  # ISO8601format
dm$RFICDTC <- NA

# ####### CE #######  
ce <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
ce$DOMAIN <- "CE"
ce$USUBJID <- paste(kStudyId, all02.No, sep = "-")
ce$CESEQ <- NA
wk.ce <- data.frame(discontinuing=dataset$中止理由, sec.ca.date=dataset$二次がん日, relapse.date=dataset$再発日)
wk.cedecod.T <- SetCeCEDECOD(wk.ce)
ce$CETERM <-wk.cedecod.T$cedecod
ce$CEDECOD <- ce$CETERM
ce$CEDTC <- wk.cedecod.T$cedtc
# Sort(asc) KEY=USUBJID,CETERM,CEDTC
ceSortlist <- order(ce$USUBJID, ce$CETERM, ce$CEDTC)
ce <- ce[ceSortlist, ]
# Set CESEQ  Serial number for each USUBJID
ce$CESEQ = SetSEQ(ce, "USUBJID", "CESEQ")

# ####### DS #######  
ds <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
ds$DOMAIN <- "DS"
ds$USUBJID <- paste(kStudyId, all02.No, sep = "-")
ds$DSSEQ <- NA
wk.ds <- data.frame(discontinuing=dataset$中止理由, endtype=dataset$終了種別, last.cfm.date=dataset$最終確認日, discontinuing.date=dataset$中止日, death.date=dataset$生死日)
wk.dsdecod.T <- SetDsDSDECOD(wk.ds)
ds$DSTERM <- wk.dsdecod.T$dsdecod 
ds$DSDECOD <- ds$DSTERM
ds$DSCAT <- "DISPOSITION EVENT"
ds$DSSTDTC <- wk.dsdecod.T$dsdtc  # ISO8601format
# Sort(asc) KEY=USUBJID,DSTERM,DSSTDTC
dssortlist <- order(ds$USUBJID, ds$DSTERM, ds$DSSTDTC)
ds <- ds[dssortlist, ]
# Set DSSEQ  Serial number for each USUBJID
ds$DSSEQ = SetSEQ(ds, "USUBJID", "DSSEQ")

# ####### MH #######  
mh <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
mh$DOMAIN <- "MH"
mh$USUBJID <- paste(kStudyId, all02.No, sep = "-")
mh$MHSEQ <- NA
mh$MHCAT <- "PRIMARY DIAGNOSIS"
mh$MHTERM <- "小児急性リンパ性白血病"
mh$MHDECOD <- mh$MHTERM
mh$MHSTDTC <- ISO8601Date(dataset$診断年月日)  # ISO8601format
mh$MHPTCD <- 20064495
mh$MHSOC <- "急性リンパ芽球性白血病"
mh$MHSOCCD <- "C91.0"
# Sort(asc) KEY=USUBJID
mhsortlist <- order(mh$USUBJID)
mh <- mh[mhsortlist,]
# Set MHSEQ  Serial number for each USUBJID
mh$MHSEQ = SetSEQ(mh, "USUBJID", "MHSEQ")

# ####### SC #######  
sc <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
sc$DOMAIN <- "SC"
sc$USUBJID <- paste(kStudyId, all02.No, sep = "-")
sc$SCSEQ <- NA
sc$SCTESTCD <- "DEFRISK"
sc$SCTEST <- "Definite Risk"
sc$SCORRES <- dataset$確定リスク
sc$SCSTRESC <- sc$SCORRES
# Sort(asc) KEY=USUBJID  
scsortlist <- order(sc$USUBJID)
sc <- sc[scsortlist,]
# Set RSSEQ  Serial number for each USUBJID
sc$SCSEQ = SetSEQ(sc, "USUBJID", "SCSEQ")

# ####### RS #######  
rs <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
rs$DOMAIN <- "RS"
rs$USUBJID <- paste(kStudyId, all02.No, sep = "-")
rs$RSSEQ <- NA
rs$RSTESTCD <- "CLINRESP"
rs$RSTEST <- "Clinical Response"
rs$RSCAT <- "DEFINED BY PROTOCOL"
rs$RSORRES <- ifelse(dataset$寛解判定 == "有", "CR",
              ifelse(dataset$寛解判定 == "無", "NR", "NE"))
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

# finalize
rm(list=ls())
