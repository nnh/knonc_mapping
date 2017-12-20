DmArmcd <- function(dst_row){
  # DM$ARMCD
  kArm_A <- "3 courses of intensification multi-agent combination chemotherapy."
  kArm_B <- "1 to 3 courses of intensification multi-agent combination chemotherapy followed by allogeneic hematopoietic stem cell transplantation."
  risk_group <- dst_row[kRiskgroup]

  if (risk_group == 1 || risk_group == 2){
    wk_arm <- "A"
    wk_armcd <- kArm_A
  } else if (risk_group== 3){
    wk_arm <- "B"
    wk_armcd <- kArm_B
  } else {
    if (risk_group == 4){
      wk_arm <- "INDFAIL"
      wk_armcd <- "Induction Failure"
    } else {
      wk_arm <- "U"
      wk_armcd <- "Unknown"
    }
  }
  return(c(arm=wk_arm, armcd=wk_armcd))
}

# readxlのインストールが必要
# install.packages('readxl')
library("readxl")
# TimeZone取得エラー対応
Sys.setenv(TZ="Asia/Tokyo")
kStudyId <- "JPLSG-AML05"
# set path
basepath <- "/Users/admin/Desktop/R/knonc_mapping"
rawdatapath <- paste(basepath, kStudyId, "input/rawdata", sep="/")
outputpath <- paste(basepath, kStudyId, "output/SDTM", sep="/")
# common function
common_function <- paste0(basepath, "/common_programs/common_fnk.R")
source(common_function)
# 列コンスタント
kAML05No <- 1  # kAML05No
kSex <- 5  # 性別
kBMA3 <- 72  # "BMA-3判定日"
kRiskgroup <- 76  # リスク分類こば
kRemission <- 74  # 寛解導入療法後の寛解有無
kDonor <- 85  # 移植ドナー
kEvent <- 95  # イベントの種類
kDiagnosisDate <- 90  # 診断日
# Load data
filenames <- list.files(rawdatapath, full.names=T)
rawdata <- read_excel(filenames[1], sheet=1, col_names=F)

# Format dataset
dataset <- rawdata[!is.na(rawdata[  ,kAML05No]), ]
# 列名設定
colnames(dataset) <- dataset[1, ]
dataset <- dataset[-1, ]
# datasetにUSUBJIDをセット
# SUBJIDが数値の場合は4桁で0パディングする
dataset$USUBJID <- apply(dataset[ ,kAML05No], 1, function(x) paste(kStudyId, sprintf("%04s", x), sep="-"))

# ####### DM #######
dm <- SetCommon_dataset(dataset)
dm$DOMAIN <- "DM"
dm$SUBJID <- as.vector(t(dataset[ ,kAML05No]))
dm$RFSTDTC <- NA
dm$BRTHDTC <- NA
dm$AGE <- dataset$年
dm$AGEU <- "YEARS"
dm$SEX <- ifelse(dataset[ ,kSex] == 0, "M",
                 ifelse(dataset[ ,kSex] == 1, "F", "U"))
wk_armcd <- apply(dataset, 1, DmArmcd)
dm$ARMCD <- wk_armcd[1, ]
dm$ARM <- wk_armcd[2, ]
dm$ACTARMCD <- dm$ARMCD
dm$ACTARM <- dm$ARM
dm$COUNTRY <- "JPN"
dm$RFXSTDTC <- NA
dm$RFICDTC <- NA
# Sort(asc) KEY=USUBJID
dm_sortlist <- order(dm$USUBJID)
dm <- dm[dm_sortlist, ]

# ####### PR #######
# 移植日が欠損でないレコードのみ出力対象とする
pr_temp <- subset(dataset, 移植日 != "N/A")
pr <- SetCommon_dataset(pr_temp)
pr$DOMAIN <- "PR"
pr$PRSEQ <- NA
pr$PRTRT <- "Hematopoietic Stem Cell Transplantation"
pr$PRCAT <- apply(pr_temp[ ,kDonor], 1, function(x) ifelse(x == "6" || is.na(x), "Unknown", "Allotransplantation"))
pr$PRSTDTC <- format(as.Date(as.numeric(as.vector(t(pr_temp$移植日))), origin="1899-12-30"))

# Sort(asc) KEY=USUBJID
pr_sortlist <- order(pr$USUBJID)
pr <- pr[pr_sortlist, ]
# Set PRSEQ  Serial number for each USUBJID
pr$PRSEQ <- SetSEQ(pr, "USUBJID", "PRSEQ")

# ####### CE #######
# イベントの種類 = 2 or 4のレコードのみ出力対象とする
ce_temp <- subset(dataset, dataset[ ,kEvent] == 2)
ce_temp <- rbind(ce_temp, subset(dataset, dataset[ ,kEvent] == 4))
ce <- SetCommon_dataset(ce_temp)
ce$DOMAIN <- "CE"
ce$CESEQ <- NA
ce$CETERM <- ifelse(ce_temp[ ,kEvent] == 2,"DISEASE RELAPSE","SECONDARY CANCER")
ce$CEDECOD <- ce$CETERM
ce$CEDTC <- format(as.Date(as.numeric(as.vector(t(ce_temp$イベント日))),origin="1899-12-30"))
# Sort(asc) KEY=USUBJID,CETERM,CEDTC
ce_sortlist <- order(ce$USUBJID, ce$CETERM, ce$CEDTC)
ce <- ce[ce_sortlist, ]
# Set CESEQ  Serial number for each USUBJID
ce$CESEQ <- SetSEQ(ce, "USUBJID", "CESEQ")

# ####### DS #######
ds <- SetCommon_dataset(dataset)
ds$DOMAIN <- "DS"
ds$DSSEQ <- NA
ds$DSTERM <- apply(dataset[ ,"死亡日"], 1, function(x) ifelse(x != "N/A", "DEATH",
                                                         ifelse(x == "N/A", "COMPLETED", "OTHER")))
ds$DSDECOD <- ds$DSTERM
ds$DSCAT <- "DISPOSITION EVENT"
ds$DSSTDTC <- format(as.Date(as.numeric(as.vector(t(dataset$最終確認日))), origin="1899-12-30"))
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
mh$MHSTDTC <- format(as.Date(as.numeric(as.vector(t(dataset[ ,kDiagnosisDate]))), origin="1899-12-30"))
mh$MHPTCD <- NA
# ICD10
mh$MHSCAT <- "ICD10"
mh$MHPTCD <- "C92.0"
mh$MHTERM <- "急性骨髄性白血病"
mh$MHDECOD <- mh$MHTERM
mh_1 <- mh
row.names(mh_1) <- NULL
# 標準病名マスター
mh$MHSCAT <- "標準病名マスター"
mh$MHPTCD <- "20058373"
mh$MHTERM <- "急性骨髄性白血病"
mh$MHDECOD <- mh$MHTERM
mh_2 <- mh
row.names(mh_2) <- NULL
# merge
rm(mh)
mh <- rbind(mh_1, mh_2)
# Sort(asc) KEY=USUBJID, MHCAT
mh_sortlist <- order(mh$USUBJID, mh$MHCAT)
mh <- mh[mh_sortlist, ]
# Set MHSEQ  Serial number for each USUBJID
mh$MHSEQ <- SetSEQ(mh, "USUBJID", "MHSEQ")

# ####### SC #######
# リスク分類こばが1,2,3のレコードのみ出力対象とする
sc_temp <- subset(dataset, dataset[  ,kRiskgroup] == 1)
sc_temp <- rbind(sc_temp, subset(dataset, dataset[  ,kRiskgroup] == 2))
sc_temp <- rbind(sc_temp, subset(dataset, dataset[  ,kRiskgroup] == 3))
# 出力対象が絞られているので個別でセット
sc <-  SetCommon_dataset(sc_temp)
sc$DOMAIN <- "SC"
sc$SCSEQ <- NA
sc$SCTESTCD <- "DEFRISK"
sc$SCTEST <- "Definite Risk"
sc$SCORRES <- apply(sc_temp[ ,kRiskgroup], 1, function(x) ifelse(x == 1, "LR", ifelse(x == 2, "IR", "HR")))
sc$SCSTRESC <- sc$SCORRES
sc$SCDTC <- NA
# Sort(asc) KEY=USUBJID
sc_sortlist <- order(sc$USUBJID)
sc <- sc[sc_sortlist, ]
# Set SCSEQ  Serial number for each USUBJID
sc$SCSEQ <- SetSEQ(sc, "USUBJID", "SCSEQ")

# ####### RS #######
# BMA-3判定日が日付扱いになるもののみ抽出対象とする
rs <- SetCommon_dataset(dataset)
rs$DOMAIN <- "RS"
rs$RSSEQ <- NA
rs$RSTESTCD <- "INDCRESP"
rs$RSTEST <- "Induction Response"
rs$RSCAT <- "DEFINED BY PROTOCOL"
rs$RSORRES <- ifelse(dataset[ ,kRemission] == 1, "CR",
                     ifelse(dataset[ ,kRemission] == 2, "NR", "NE"))

rs$RSSTRESC <- rs$RSORRES
rs$RSDTC <- NA
for (i in 1:nrow(dataset)) {
  # 0-9のみからなる文字列なら日付扱い、それ以外はNAにして行削除
  if (!is.na(dataset[i,kBMA3]) && length(grep("[0-9]", dataset[i,kBMA3])) > 0) {
    rs[i,"RSDTC"] <- format(as.Date(as.numeric(as.vector(t(dataset[i,kBMA3]))), origin="1899-12-30"))
  }
}
rs <- subset(rs, !is.na(rs$RSDTC))
# Sort(asc) KEY=USUBJID
rs_sortlist <- order(rs$USUBJID)
rs <- rs[rs_sortlist, ]
# Set RSSEQ  Serial number for each USUBJID
rs$RSSEQ <- SetSEQ(rs, "USUBJID", "RSSEQ")

# Save datasets
WriteCSV_SDTM("CP932", outputpath)
