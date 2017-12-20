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
# TimeZone取得エラー対応
Sys.setenv(TZ="Asia/Tokyo")
kStudyId <- "JDCHCT-TRUMP"
# set path
basepath <- "/Users/admin/Desktop/R/knonc_mapping"
rawdatapath <- paste(basepath, kStudyId, "input/rawdata", sep="/")
outputpath <- paste(basepath, kStudyId, "output/SDTM", sep="/")
# common function
common_function <- paste0(basepath, "/common_programs/common_fnk.R")
source(common_function)
# 基準日
kToday <- as.Date("2017/11/25")

# Load data
filenames <- list.files(rawdatapath, full.names=T)
rawdata <- read_excel(filenames[1], sheet=2, col_names=T)

# Format dataset
dataset <- rawdata[!is.na(rawdata[ ,"EGA_id"]), ]
# datasetに開始基準日、誕生日情報を追加
dataset$RFSTDTC <- NA
dataset$BRTHDTC <- NA
for (i in 1:nrow(dataset)) {
  if (dataset[i,".DaysOS"] != "NA") {
    wk_RFSTDTC <- kToday - as.numeric(dataset[i,".DaysOS"])
    dataset[i,"RFSTDTC"] <- format(wk_RFSTDTC)
    if (dataset[i,".Age"] != "NA") {
      # 小数点以下切り捨て
      wk_Age <- YearsConvDays(dataset[i,".Age"])
      # 年齢は登録日時点、登録日から年齢分の日数を除して誕生日を算出
      wk_birth <- wk_RFSTDTC - wk_Age
      dataset[i,"BRTHDTC"] <- format(wk_birth)
    }
  }
}
# datasetにUSUBJIDをセット
dataset$USUBJID <- apply(dataset[ ,"EGA_id"], 1, function(x) paste(kStudyId, x, sep="-"))

# ####### DM #######
dm <- SetCommon_dataset(dataset)
dm$DOMAIN <- "DM"
dm$SUBJID <- as.vector(t(dataset[ ,"EGA_id"]))
dm$RFSTDTC <- dataset$RFSTDTC
dm$BRTHDTC <- dataset$BRTHDTC
dm$AGE <- as.vector(t(dataset[ ,".Age"]))
dm$AGEU <- "YEARS"
# 1:male,0:female
dm$SEX <- ifelse(dataset$.Sex.R == 1, "M",
                 ifelse(dataset$.Sex.R == 0, "F", "U"))
dm$ARMCD <- NA
dm$ARM <- NA
dm$ACTARMCD <- dm$ARMCD
dm$ACTARM <- dm$ARM
dm$COUNTRY <- "JPN"
dm$RFXSTDTC <- NA
dm$RFICDTC <- NA

# ####### PR #######
# .DxToSCTが欠損でないレコードのみ出力対象とする
pr_temp <- subset(dataset, .DxToSCT != "NA")
pr <- SetCommon_dataset(pr_temp)
pr$DOMAIN <- "PR"
pr$PRSEQ <- NA
pr$PRTRT <- "Hematopoietic Stem Cell Transplantation"
pr$PRCAT <- "Allotransplantation"
pr$PRSTDTC <- NA
# Sort(asc) KEY=USUBJID
pr_sortlist <- order(pr$USUBJID)
pr <- pr[pr_sortlist, ]
# Set PRSEQ  Serial number for each USUBJID
pr$PRSEQ <- SetSEQ(pr, "USUBJID", "PRSEQ")

# ####### CE #######
# .Relapse = 1のレコードのみ出力対象とする
ce_temp <- subset(dataset, .Relapse == 1)
ce <- SetCommon_dataset(ce_temp)
ce$DOMAIN <- "CE"
ce$CESEQ <- NA
ce$CETERM <- "DISEASE RELAPSE"
ce$CEDECOD <- ce$CETERM
ce$CEDTC <- NA
for (i in 1:nrow(ce)) {
  if (ce_temp[i,".DaysRelapse2"] != "NA" && !is.na(ce_temp[i,"RFSTDTC"])) {
    # .DaysRelapse2 小数点以下切り捨て
    ce[i,"CEDTC"] <- format(floor(as.numeric(ce_temp[i,".DaysRelapse2"])) + as.Date(as.character(ce_temp[i,"RFSTDTC"])))
  }
}
# Sort(asc) KEY=USUBJID,CETERM,CEDTC
ce_sortlist <- order(ce$USUBJID, ce$CETERM, ce$CEDTC)
ce <- ce[ce_sortlist, ]
# Set CESEQ  Serial number for each USUBJID
ce$CESEQ <- SetSEQ(ce, "USUBJID", "CESEQ")

# ####### DS #######
ds <- SetCommon_dataset(dataset)
ds$DOMAIN <- "DS"
ds$DSSEQ <- NA
ds$DSTERM <- ifelse(dataset[ ,".OS"] == "0", "COMPLETED",
                    ifelse(dataset[ ,".OS"] == "1", "DEATH", "OTHER"))
ds$DSDECOD <- ds$DSTERM
ds$DSCAT <- "DISPOSITION EVENT"
ds$DSSTDTC <- format(kToday)
# Sort(asc) KEY=USUBJID,DSTERM,DSSTDTC
ds_sortlist <- order(ds$USUBJID, ds$DSTERM, ds$DSSTDTC)
ds <- ds[ds_sortlist, ]
# Set DSSEQ  Serial number for each USUBJID
ds$DSSEQ <- SetSEQ(ds, "USUBJID", "DSSEQ")

# ####### MH #######
mh <- SetCommon_dataset(dataset)
mh$DOMAIN <- "MH"
mh$MHSEQ <- NA
mh$MHCAT <- "PRIMARY DIAGNOSIS"
mh$MHSCAT <- NA
mh$MHTERM <- NA
mh$MHDECOD <- NA
mh$MHSTDTC <- NA
# 病名開始日 .age_dxは発症時年齢なので誕生日に年齢分の日数を加算して算出
for (i in 1:nrow(dataset)){
  if (dataset[i ,"age_dx"] != "NA" && dataset$BRTHDTC != "NA"){
    wk_age_dx <- YearsConvDays(dataset[i ,"age_dx"])
    wk_birth <- as.vector(t(dataset[i ,"BRTHDTC"]))
    mh[i, "MHSTDTC"] <- format(as.Date(wk_birth) + wk_age_dx)
  }
}
mh$MHPTCD <- NA
# ICD10
mh$MHSCAT <- "ICD10"
mh$MHPTCD <- "D46.9"
mh$MHTERM <- "骨髄異形成症候群"
mh$MHDECOD <- mh$MHTERM
mh_1 <- mh
# 標準病名マスター
mh$MHSCAT <- "標準病名マスター"
mh$MHPTCD <- "20061922"
mh$MHTERM <- "骨髄異形成症候群"
mh$MHDECOD <- mh$MHTERM
mh_2 <- mh
# merge
mh <- rbind(mh_1, mh_2)
# Sort(asc) KEY=USUBJID, MHCAT
mh_sortlist <- order(mh$USUBJID, mh$MHCAT)
mh <- mh[mh_sortlist, ]
# Set MHSEQ  Serial number for each USUBJID
mh$MHSEQ <- SetSEQ(mh, "USUBJID", "MHSEQ")

# ####### SC #######
sc <- SetCommon_dataset(dataset)
sc$DOMAIN <- "SC"
sc$SCSEQ <- NA
sc$SCTESTCD <- "IPSS"
sc$SCTEST <- sc$SCTESTCD
# NAが文字列になるので変換
sc$SCORRES <- dataset$MDS_dx_ipss
sc$SCORRES[sc$SCORRES == "NA"] <- NA
sc$SCSTRESC <- sc$SCORRES
sc$SCDTC <- NA
# Sort(asc) KEY=USUBJID
sc_sortlist <- order(sc$USUBJID)
sc <- sc[sc_sortlist, ]
# Set SCSEQ  Serial number for each USUBJID
sc$SCSEQ <- SetSEQ(sc, "USUBJID", "SCSEQ")

# Save datasets
WriteCSV_SDTM("CP932", outputpath)
