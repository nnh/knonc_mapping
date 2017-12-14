YearsConvDays <- function(wk_years){
  # 年数を日数に変換して返す
  if(!is.na(wk_years)){
    res <- floor(as.numeric(wk_years) * 365.25)
  } else {
    res <- NA
  }
  return(res)
}

# readxlのインストールが必要
# install.packages('readxl')
library("readxl")
# common function
source("./common_programs/common_fnk.R")
# TimeZone取得エラー対応
Sys.setenv(TZ="Asia/Tokyo")
kStudyId <- "JDCHCT-TRUMP"
# 今日の日付
kToday <- Sys.Date()

# Load data
setwd(kStudyId)
setwd("./input/rawdata")
filenames <- list.files()
rawdata <- read_excel(filenames[1], sheet=2, col_names=T)
setwd("../..")

# Format dataset
dataset <- rawdata[!is.na(rawdata[ ,"EGA_id"]), ]
# 誕生日情報を追加
for (i in 1:nrow(dm)) {
  if (dataset[i,".DaysOS"] != "NA") {
    wk_RFSTDTC <- kToday - as.numeric(dataset[i,".DaysOS"])
     <- format(wk_RFSTDTC)
    if (dataset[i,".Age"] != "NA") {
      # 小数点以下切り捨て
      #      wk_Age <- floor(as.numeric(dataset[i,".Age"]) * 365.25)
      wk_Age <- YearsConvDays(dataset[i,".Age"])
      # 年齢は登録日時点、登録日から年齢分の日数を除して誕生日を算出
      wk_birth <- wk_RFSTDTC - wk_Age
      dm[i,"BRTHDTC"] <- format(wk_birth)
    }
  }
}
# Common Columns
comdst <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
comdst$DOMAIN <- NA
comdst$DOMAIN <- NA
comdst$USUBJID <- apply(dataset[ ,"EGA_id"], 1, function(x) paste(kStudyId, x, sep="-"))

# ####### DM #######
dm <- comdst
dm$DOMAIN <- "DM"
dm$SUBJID <- dataset[ ,"EGA_id"]
dm$RFSTDTC <- NA
dm$BRTHDTC <- NA
for (i in 1:nrow(dm)) {
  if (dataset[i,".DaysOS"] != "NA") {
    wk_RFSTDTC <- kToday - as.numeric(dataset[i,".DaysOS"])
    dm[i,"RFSTDTC"] <- format(wk_RFSTDTC)
    if (dataset[i,".Age"] != "NA") {
      # 小数点以下切り捨て
#      wk_Age <- floor(as.numeric(dataset[i,".Age"]) * 365.25)
      wk_Age <- YearsConvDays(dataset[i,".Age"])
      # 年齢は登録日時点、登録日から年齢分の日数を除して誕生日を算出
      wk_birth <- wk_RFSTDTC - wk_Age
      dm[i,"BRTHDTC"] <- format(wk_birth)
    }
  }
}
# RFSTDTC - INT(Variables_Yoshizato Blood 2017_send..Age*12*365.25)
dm$AGE <- dataset[ ,".Age"]
dm$AGEU <- "YEARS"
dm$SEX <- dataset[ ,".Sex.R"]
dm$ARMCD <- NA
dm$ARM <- NA
dm$ACTARMCD <- dm$ARMCD
dm$ACTARM <- dm$ARM
dm$COUNTRY <- "JPN"
dm$RFXSTDTC <- NA
dm$RFICDTC <- NA

# ####### PR #######
# .DxToSCTが欠損でないレコードのみ出力対象とする
pr_temp <- subset(dataset, !is.na(.DxToSCT))
# 出力対象が絞られているので個別でセット
pr <- data.frame(STUDYID = rep(kStudyId, nrow(pr_temp)))
pr$DOMAIN <- "PR"
pr$USUBJID <- apply(pr_temp[ ,"EGA_id"], 1, function(x) paste(kStudyId, x, sep="-"))
pr$PRSEQ <- NA
pr$PRTRT <- "Hematopoietic Stem Cell Transplantation"
pr$PRCAT <- "Allotransplantation"
pr$PRSTDTC <- NA

# ####### CE #######
# .Relapse = 1のレコードのみ出力対象とする
ce_temp <- subset(dataset, .Relapse == 1)
# 出力対象が絞られているので個別でセット
ce <- data.frame(STUDYID = rep(kStudyId, nrow(ce_temp)))
ce$DOMAIN <- "CE"
ce$USUBJID <- apply(ce_temp[ ,"EGA_id"], 1, function(x) paste(kStudyId, x, sep="-"))
ce$CESEQ <- NA
ce$CETERM <- "DISEASE RELAPSE"
ce$CEDECOD <- ce$CETERM
ce$CEDTC <- cetemp[ ,".DaysRelapse2"] + RFSTDTC
# Sort(asc) KEY=USUBJID,CETERM,CEDTC
ceSortlist <- order(ce$USUBJID, ce$CETERM, ce$CEDTC)
ce <- ce[ceSortlist, ]
# Set CESEQ  Serial number for each USUBJID
ce$CESEQ <- SetSEQ(ce, "USUBJID", "CESEQ")

# ####### DS #######
ds <- comdst
ds$DOMAIN <- "DS"
ds$DSSEQ <- NA
ds$DSTERM <- ifelse(dataset[ ,".OS"] == "0", "COMPLETED",
                    ifelse(dataset[ ,".OS"] == "1", "DEATH", "OTHER"))
ds$DSDECOD <- ds$DSTERM
ds$DSCAT <- "DISPOSITION EVENT"
ds$DSSTDTC <- kToday
# Sort(asc) KEY=USUBJID,DSTERM,DSSTDTC
dssortlist <- order(ds$USUBJID, ds$DSTERM, ds$DSSTDTC)
ds <- ds[dssortlist, ]
# Set DSSEQ  Serial number for each USUBJID
ds$DSSEQ <- SetSEQ(ds, "USUBJID", "DSSEQ")

# ####### MH #######
mh <- comdst
mh$DOMAIN <- "MH"
mh$MHSEQ <- NA
mh$MHCAT <- "PRIMARY DIAGNOSIS"
mh$MHSCAT <- NA
mh$MHTERM <- NA
mh$MHDECOD <- mh$MHTERM
# 病名開始日 .age_dxは発症時年齢なので誕生日に年齢分の日数を加算して算出
wk_age_dx <- YearsConvDays(dataset[i,".age_dx"])
mh$MHSTDTC <- format(wk_birth + wk_age_dx)
mh$MHPTCD <- NA
# ICD10
mh$MHSCAT <- "ICD10"
mh$MHPTCD <- "D46.9"
mh$MHTERM <- "骨髄異形成症候群"
mh_1 <- mh
# 標準病名マスター
mh$MHSCAT <- "標準病名マスター"
mh$MHPTCD <- "20061922"
mh$MHTERM <- "骨髄異形成症候群"
mh_2 <- mh
# merge
mh <- rbind(mh_1, mh_2)
# Sort(asc) KEY=USUBJID, MHCAT
mhsortlist <- order(mh$USUBJID, mh$MHCAT)
mh <- mh[mhsortlist, ]
# Set MHSEQ  Serial number for each USUBJID
mh$MHSEQ <- SetSEQ(mh, "USUBJID", "MHSEQ")

# ####### SC #######
sc <- comdst
sc$DOMAIN <- "SC"
sc$SCSEQ <- NA
sc$SCTESTCD <- "IPSS"
sc$SCTEST <- sc$SCTESTCD
sc$SCORRES <- dataset$MDS_dx_ipss
sc$SCSTRESC <- sc$SCORRES
sc$SCDTC <- NA
# Sort(asc) KEY=USUBJID
scsortlist <- order(sc$USUBJID)
sc <- sc[scsortlist, ]
# Set SCSEQ  Serial number for each USUBJID
sc$SCSEQ <- SetSEQ(sc, "USUBJID", "SCSEQ")

# Save datasets
WriteCSV_SDTM("CP932")
# WriteCSV_SDTM("UTF-8")
