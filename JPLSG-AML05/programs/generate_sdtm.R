DmArmcd <- function(dst_row){
  # DM$ARMCD
  kArm_A <- "3 courses of intensification multi-agent combination chemotherapy."
  kArm_B <- "1 to 3 courses of intensification multi-agent combination chemotherapy followed by allogeneic hematopoietic stem cell transplantation."
  risk_group <- dst_row[kRiskgroup]
  # remission <- dst_row[kRemission]

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
# common function
source("./common_programs/common_fnk.R")
# TimeZone取得エラー対応
Sys.setenv(TZ="Asia/Tokyo")
kStudyId <- "JPLSG-AML05"
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
setwd(kStudyId)
setwd("./input/rawdata")
filenames <- list.files()
rawdata <- read_excel(filenames[1], sheet=1, col_names=F)
setwd("../..")

# Format dataset
dataset <- rawdata[!is.na(rawdata[  ,kAML05No]), ]
# 列名設定
colnames(dataset) <- dataset[1, ]
dataset <- dataset[-1, ]
# ソートキーの設定、USUBJIDでソートすると想定した並び順にならないため
dataset$sortkey <- as.numeric(as.vector(t(dataset[ ,kAML05No])))

# Common Columns
comdst <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
comdst$DOMAIN <- NA
comdst$USUBJID <- apply(dataset[ ,kAML05No], 1, function(x) paste(kStudyId, x, sep="-"))

# ####### DM #######
dm <- comdst
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
dm$sortkey <- dataset$sortkey
dmSortlist <- order(dm$sortkey)
dm <- dm[dmSortlist, ]
# sortkey列を削除
dm <- dm[ , colnames(dm) != "sortkey"]

# ####### PR #######
# 移植日が欠損でないレコードのみ出力対象とする
pr_temp <- subset(dataset, 移植日 != "N/A")
# 出力対象が絞られているので個別でセット
pr <- data.frame(STUDYID = rep(kStudyId, nrow(pr_temp)))
pr$DOMAIN <- "PR"
pr$USUBJID <- apply(pr_temp[ ,kAML05No], 1, function(x) paste(kStudyId, x, sep="-"))
pr$PRSEQ <- NA
pr$PRTRT <- "Hematopoietic Stem Cell Transplantation"
pr$PRCAT <- apply(pr_temp[ ,kDonor], 1, function(x) ifelse(x == "6" || is.na(x), "Unknown", "Allotransplantation"))
pr$PRSTDTC <- format(as.Date(as.numeric(as.vector(t(pr_temp$移植日))), origin="1899-12-30"))

# Sort(asc) KEY=USUBJID
pr$sortkey <- pr_temp$sortkey
prSortlist <- order(pr$sortkey)
pr <- pr[prSortlist, ]
# Set PRSEQ  Serial number for each USUBJID
pr$PRSEQ <- SetSEQ(pr, "USUBJID", "PRSEQ")
# sortkey列を削除
pr <- pr[ , colnames(pr) != "sortkey"]

# ####### CE #######
# イベントの種類 = 2 or 4のレコードのみ出力対象とする
ce_temp <- subset(dataset, dataset[ ,kEvent] == 2)
ce_temp <- rbind(ce_temp, subset(dataset, dataset[ ,kEvent] == 4))
# 出力対象が絞られているので個別でセット
ce <- data.frame(STUDYID = rep(kStudyId, nrow(ce_temp)))
ce$DOMAIN <- "CE"
ce$USUBJID <- apply(ce_temp[ ,kAML05No], 1, function(x) paste(kStudyId, x, sep="-"))
ce$CESEQ <- NA
ce$CETERM <- ifelse(ce_temp[ ,kEvent] == 2,"DISEASE RELAPSE","SECONDARY CANCER")
ce$CEDECOD <- ce$CETERM
ce$CEDTC <- format(as.Date(as.numeric(as.vector(t(ce_temp$イベント日))),origin="1899-12-30"))
# Sort(asc) KEY=USUBJID,CETERM,CEDTC
ce$sortkey <- ce_temp$sortkey
ceSortlist <- order(ce$sortkey, ce$CETERM, ce$CEDTC)
ce <- ce[ceSortlist, ]
# Set CESEQ  Serial number for each USUBJID
ce$CESEQ <- SetSEQ(ce, "USUBJID", "CESEQ")
# sortkey列を削除
ce <- ce[ , colnames(ce) != "sortkey"]

# ####### DS #######
ds <- comdst
ds$DOMAIN <- "DS"
ds$DSSEQ <- NA
ds$DSTERM <- apply(dataset[ ,"死亡日"], 1, function(x) ifelse(x != "N/A", "DEATH", ifelse(x == "N/A", "COMPLETED", "OTHER")))
ds$DSDECOD <- ds$DSTERM
ds$DSCAT <- "DISPOSITION EVENT"
ds$DSSTDTC <- format(as.Date(as.numeric(as.vector(t(dataset$最終確認日))), origin="1899-12-30"))
# Sort(asc) KEY=USUBJID,DSTERM,DSSTDTC
ds$sortkey <- dataset$sortkey
dssortlist <- order(ds$sortkey, ds$DSTERM, ds$DSSTDTC)
ds <- ds[dssortlist, ]
# Set DSSEQ  Serial number for each USUBJID
ds$DSSEQ <- SetSEQ(ds, "USUBJID", "DSSEQ")
# sortkey列を削除
ds <- ds[ , colnames(ds) != "sortkey"]

# ####### MH #######
mh <- comdst
mh$DOMAIN <- "MH"
mh$MHSEQ <- NA
mh$MHCAT <- "PRIMARY DIAGNOSIS"
mh$MHSCAT <- NA
mh$MHTERM <- NA
mh$MHDECOD <- NA
mh$MHSTDTC <- format(as.Date(as.numeric(as.vector(t(dataset[ ,kDiagnosisDate]))), origin="1899-12-30"))
mh$MHPTCD <- NA
mh$sortkey <- dataset$sortkey
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
mhsortlist <- order(mh$sortkey, mh$MHCAT)
mh <- mh[mhsortlist, ]
# Set MHSEQ  Serial number for each USUBJID
mh$MHSEQ <- SetSEQ(mh, "USUBJID", "MHSEQ")
# sortkey列を削除
mh <- mh[ , colnames(mh) != "sortkey"]

# ####### SC #######
# リスク分類こばが1,2,3のレコードのみ出力対象とする
sc_temp <- subset(dataset, dataset[  ,kRiskgroup] == 1)
sc_temp <- rbind(sc_temp, subset(dataset, dataset[  ,kRiskgroup] == 2))
sc_temp <- rbind(sc_temp, subset(dataset, dataset[  ,kRiskgroup] == 3))
# 出力対象が絞られているので個別でセット
sc <- data.frame(STUDYID = rep(kStudyId, nrow(sc_temp)))
sc$DOMAIN <- "SC"
sc$USUBJID <- apply(sc_temp[ ,kAML05No], 1, function(x) paste(kStudyId, x, sep="-"))
sc$SCSEQ <- NA
sc$SCTESTCD <- "DEFRISK"
sc$SCTEST <- "Definite Risk"
# sc$SCORRES <- ifelse(as.vector(t(sc_temp[ ,kRiskgroup])) == 1,"LR",
#                     ifelse(as.vector(t(sc_temp[ ,kRiskgroup])) == 2,"IR","HR"))
sc$SCORRES <- apply(sc_temp[ ,kRiskgroup], 1, function(x) ifelse(x == 1, "LR", ifelse(x == 2, "IR", "HR")))

sc$SCSTRESC <- sc$SCORRES
sc$SCDTC <- NA
# Sort(asc) KEY=USUBJID
sc$sortkey <- sc_temp$sortkey
scsortlist <- order(sc$sortkey)
sc <- sc[scsortlist, ]
# Set SCSEQ  Serial number for each USUBJID
sc$SCSEQ <- SetSEQ(sc, "USUBJID", "SCSEQ")
# sortkey列を削除
sc <- sc[ , colnames(sc) != "sortkey"]

# ####### RS #######
rs <- comdst
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
  # 0-9のみからなる文字列なら日付扱い、それ以外はそのまま格納
  if (!is.na(dataset[i,kBMA3]) && length(grep("[0-9]", dataset[i,kBMA3])) > 0) {
    rs[i,"RSDTC"] <- format(as.Date(as.numeric(as.vector(t(dataset[i,kBMA3]))), origin="1899-12-30"))
  } else {
    rs[i,"RSDTC"] <- as.vector(t(dataset[i,kBMA3]))
  }
}
# Sort(asc) KEY=USUBJID
rs$sortkey <- dataset$sortkey
rssortlist <- order(rs$sortkey)
rs <- rs[rssortlist, ]
# Set RSSEQ  Serial number for each USUBJID
rs$RSSEQ <- SetSEQ(rs, "USUBJID", "RSSEQ")
# sortkey列を削除
rs <- rs[ , colnames(rs) != "sortkey"]

# Save datasets
WriteCSV_SDTM("CP932")
# WriteCSV_SDTM("UTF-8")
