DmArmcd <- function(dst_row){
  # DM$ARMCD
  # 寛解有無が寛解以外ならば"INDFAIL"
  # 上記以外でリスク群が"SR" or "IR"であれば"A"、"HR" or "HR/SCT" or "HEX" or "HEX/SCT"であれば"B"
  # どれにも当てはまらなければ"U"
  kArm_A <- "Induction therapy-A (Week 1-6): PSL, VCR, THP-ADR, L-ASP, MTX, Ara-C, HDC. Consolidation therapy-A for standard and intermediate risk patients (Week 8-10): 6MP, Ara-C, CPM, MTX, HDC."
  kArm_B <- "Induction therapy-B (Week 1-6): PSL, VCR, THP-ADR,DNR, CPA, L-ASP, MTX, Ara-C, HDC. Early Consolidation therapy-B (Week 8): DEX, MTX, CPA, Ara-C, L-ASP, HDC."
  risk_group <- dst_row["リスク群"]
  remission <- dst_row["寛解有無"]

  if (remission != "寛解"){
    wk_arm <- "INDFAIL"
    wk_armcd <- "Induction Failure"
  } else {
    if (risk_group=="SR" || risk_group=="IR"){
      wk_arm <- "A"
      wk_armcd <- kArm_A
    } else if (risk_group=="HR" || risk_group=="HR/SCT" || risk_group=="HEX" || risk_group=="HEX/SCT"){
      wk_arm <- "B"
      wk_armcd <- kArm_B
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
kStudyId <- "TCCSG-ALL-L04-16"
# set path
basepath <- "/Users/admin/Desktop/R/knonc_mapping"
rawdatapath <- paste(basepath, kStudyId, "input/rawdata", sep="/")
outputpath <- paste(basepath, kStudyId, "output/SDTM", sep="/")
# common function
common_function <- paste0(basepath, "/common_programs/common_fnk.R")
source(common_function)

# Load data
filenames <- list.files(rawdatapath, full.names=T)
rawdata <- read_excel(filenames[1], sheet=1, col_names=T)

# Format dataset
dataset <- rawdata[!is.na(rawdata[ ,"Sample No"]), ]
# datasetにUSUBJIDをセット
dataset$USUBJID <- apply(dataset[ ,"Sample No"], 1, function(x) paste(kStudyId, x, sep="-"))

# ####### DM #######
dm <- SetCommon_dataset(dataset)
dm$DOMAIN <- "DM"
dm$SUBJID <- as.vector(t(dataset[ ,"Sample No"]))
dm$RFSTDTC <- NA
dm$BRTHDTC <- NA
dm$AGE <- dataset$年齢
dm$AGEU <- "YEARS"
dm$SEX <- ifelse(dataset$性別 == "男", "M",
                 ifelse(dataset$性別 == "女", "F", "U"))
wk_armcd <- apply(dataset, 1, DmArmcd)
dm$ARMCD <- wk_armcd[1, ]
dm$ARM <- wk_armcd[2, ]
dm$ACTARMCD <- dm$ARMCD
dm$ACTARM <- dm$ARM
dm$COUNTRY <- "JPN"
dm$RFXSTDTC <- ISO8601Date(dataset$治療開始日)
dm$RFICDTC <- NA

# ####### CE #######
# イベントが"再発"または"二次がん"のレコードのみ出力対象とする
ce_temp <- subset(dataset, イベント=="再発")
ce_temp <- rbind(ce_temp, subset(dataset, イベント=="二次がん"))
# 出力対象が絞られているので個別でセット
ce <- SetCommon_dataset(ce_temp)
ce$DOMAIN <- "CE"
ce$CESEQ <- NA
wk_CETERM <- ifelse (ce_temp$イベント == "再発", "DISEASE RELAPSE", "SECONDARY CANCER")
ce$CETERM <- wk_CETERM
ce$CEDECOD <- ce$CETERM
ce$CEDTC <- ISO8601Date(ce_temp$イベント日付)
# Sort(asc) KEY=USUBJID,CETERM,CEDTC
ce_sortlist <- order(ce$USUBJID, ce$CETERM, ce$CEDTC)
ce <- ce[ce_sortlist, ]
# Set CESEQ  Serial number for each USUBJID
ce$CESEQ <- SetSEQ(ce, "USUBJID", "CESEQ")

# ####### DS #######
ds <- SetCommon_dataset(dataset)
ds$DOMAIN <- "DS"
ds$DSSEQ <- NA
ds$DSTERM <- ifelse(dataset$生死コード == "死亡", "DEATH",
             ifelse(dataset$生死コード == "生存", "COMPLETED", "OTHER"))
ds$DSDECOD <- ds$DSTERM
ds$DSCAT <- "DISPOSITION EVENT"
ds$DSSTDTC <- ISO8601Date(dataset$最終観察日付)  # ISO8601format
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
mh$MHPTCD <- NA
# ICD10
mh$MHSCAT <- "ICD10"
mh$MHPTCD <- "C91.0"
mh$MHTERM <- "急性リンパ芽球性白血病"
mh$MHDECOD <- mh$MHTERM
mh_1 <- mh
# 標準病名マスター
mh$MHSCAT <- "標準病名マスター"
mh$MHPTCD <- "20064495"
mh$MHTERM <- "小児急性リンパ性白血病"
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
sc$SCTESTCD <-"DEFRISK"
sc$SCTEST <- "Definite Risk"
sc$SCORRES <- dataset$リスク群
sc$SCSTRESC <- sc$SCORRES
sc$SCDTC <- NA
# Sort(asc) KEY=USUBJID
sc_sortlist <- order(sc$USUBJID)
sc <- sc[sc_sortlist, ]
# Set SCSEQ  Serial number for each USUBJID
sc$SCSEQ <- SetSEQ(sc, "USUBJID", "SCSEQ")

# ####### RS #######
rs <- SetCommon_dataset(dataset)
rs$DOMAIN <- "RS"
rs$RSSEQ <- NA
rs$RSTESTCD <- "INDCRESP"
rs$RSTEST <- "Induction Response"
rs$RSCAT <- "DEFINED BY PROTOCOL"
rs$RSORRES <- ifelse(dataset$寛解有無 == "寛解", "CR",
              ifelse(dataset$寛解有無 != "寛解", "NR", "NE"))
rs$RSSTRESC <- rs$RSORRES
rs$RSDTC <- ISO8601Date(dataset$寛解日付)
# Sort(asc) KEY=USUBJID
rs_sortlist <- order(rs$USUBJID)
rs <- rs[rs_sortlist, ]
# Set RSSEQ  Serial number for each USUBJID
rs$RSSEQ <- SetSEQ(rs, "USUBJID", "RSSEQ")

# Save datasets
WriteCSV_SDTM("CP932", outputpath)
