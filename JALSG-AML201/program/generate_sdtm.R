# TimeZone取得エラー対応
Sys.setenv(TZ="Asia/Tokyo")
kStudyId <- "JALSG-AML201"
# set path
basepath <- "/Users/admin/Desktop/R/knonc_mapping"
rawdatapath <- paste(basepath, kStudyId, "input/rawdata", sep="/")
outputpath <- paste(basepath, kStudyId, "output/SDTM", sep="/")
# common function
common_function <- paste0(basepath, "/common_programs/common_fnk.R")
source(common_function)

# Load data
filenames <- list.files(rawdatapath, full.names=T)
rawdata <- read.csv(filenames[1], as.is = T, na.strings = c(""), fileEncoding = "CP932")

# Format data
dataset <- rawdata[!is.na(rawdata$検体ID), ]
# datasetにUSUBJIDをセット
dataset$USUBJID <- sapply(dataset$検体ID, function(x){paste(kStudyId, x, sep="-")})

# ####### DM #######
dm <- SetCommon_dataset(dataset)
dm$DOMAIN <- "DM"
dm$SUBJID <- dataset$検体ID
dm$RFSTDTC <- ISO8601Date(dataset$登録日)
dm$BRTHDTC <- NA
dm$AGE <- dataset$Age
dm$AGEU <- "YEARS"
dm$SEX <- NA
dm$ARMCD <- ifelse(is.na(dataset$寛解導入療法), "SCRNFAIL",
            ifelse(is.na(dataset$地固め療法群), "INDFAIL",
              paste(substr(dataset$寛解導入療法, 1, 1), substr(dataset$地固め療法群, 1, 1), sep = "-")))
dm$ARM <- ifelse(is.na(dataset$寛解導入療法), "Screen Failure",
          ifelse(is.na(dataset$地固め療法群), "Induction Failure",
            chartr("（", ":", paste(gsub("）群", "", dataset$寛解導入療法), gsub("）群", "", dataset$地固め療法群), sep = "-"))))
dm$ACTARMCD <- dm$ARMCD
dm$ACTARM <- dm$ARM
dm$COUNTRY <- "JPN"
dm$RFXSTDTC <- ISO8601Date(dataset$治療開始日)
dm$RFICDTC <- NA

# ####### PR #######
# 移植日が空白でないレコードのみ出力対象とする
pr_temp <- subset(dataset, !is.na(移植日))
pr <- SetCommon_dataset(pr_temp)
pr$DOMAIN <- "PR"
pr$PRSEQ <- NA
pr$PRTRT <- "Hematopoietic Stem Cell Transplantation"
pr$PRCAT <- "Unknown"
pr$PRSTDTC <- ISO8601Date(pr_temp$移植日)
# Sort(asc) KEY=USUBJID
pr_sortlist <- order(pr$USUBJID)
pr <- pr[pr_sortlist, ]
# Set SEQ  Serial number for each USUBJID
pr$PRSEQ <- SetSEQ(pr, "USUBJID", "PRSEQ")

# ####### CE #######
# 再発の有無が"再発"のレコードのみ出力対象とする
ce_temp <- subset(dataset, 再発の有無=="再発")
ce <- SetCommon_dataset(ce_temp)
ce$DOMAIN <- "CE"
ce$CESEQ <- NA
ce$CETERM <- "DISEASE RELAPSE"
ce$CEDECOD <- ce$CETERM
ce$CEDTC <- ISO8601Date(ce_temp$再発日)
# Sort(asc) KEY=USUBJID,CETERM,CEDTC
ce_sortlist <- order(ce$USUBJID, ce$CETERM, ce$CEDTC)
ce <- ce[ce_sortlist, ]
# Set CESEQ  Serial number for each USUBJID
ce$CESEQ <- SetSEQ(ce, "USUBJID", "CESEQ")

# ####### DS #######
ds <- SetCommon_dataset(dataset)
ds$DOMAIN <- "DS"
ds$DSSEQ <- NA
ds$DSTERM <- ifelse(!is.na(dataset$最終生存確認日), "COMPLETED",
             ifelse(!is.na(dataset$死亡日), "DEATH",
               "OTHER"))
ds$DSDECOD <- ds$DSTERM
ds$DSCAT <- "DISPOSITION EVENT"
wk.dsstdtc <- ifelse(!is.na(dataset$最終生存確認日), dataset$最終生存確認日,
              ifelse(!is.na(dataset$死亡日), dataset$死亡日,
                dataset$最終予後日))
ds$DSSTDTC <- ISO8601Date(wk.dsstdtc)  # ISO8601format
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
mh$MHTERM <- "急性骨髄性白血病"
mh$MHDECOD <- mh$MHTERM
mh$MHSTDTC <- NA
mh$MHPTCD <- NA
# ICD10
mh$MHSCAT <- "ICD10"
mh$MHPTCD <- "C92.0"
mh_1 <- mh
# 標準病名マスター
mh$MHSCAT <- "標準病名マスター"
mh$MHPTCD <- "20058373"
mh_2 <- mh
# merge
mh <- rbind(mh_1, mh_2)
# Sort(asc) KEY=USUBJID, MHCAT
mh_sortlist <- order(mh$USUBJID, mh$MHCAT)
mh <- mh[mh_sortlist, ]
# Set MHSEQ  Serial number for each USUBJID
mh$MHSEQ <- SetSEQ(mh, "USUBJID", "MHSEQ")

# ####### RS #######
rs <- SetCommon_dataset(dataset)
rs$DOMAIN <- "RS"
rs$RSSEQ <- NA
rs$RSTESTCD <- "INDCRESP"
rs$RSTEST <- "Induction Response"
rs$RSCAT <- "DEFINED BY PROTOCOL"
rs$RSORRES <- ifelse(dataset$CR == "CR", "CR",
              ifelse(dataset$CR == "nonCR", "NR", "NE"))
rs$RSSTRESC <- rs$RSORRES
rs$RSDTC <- ISO8601Date(dataset$寛解到達日)
# Sort(asc) KEY=USUBJID
rs_sortlist <- order(rs$USUBJID)
rs <- rs[rs_sortlist, ]
# Set RSSEQ  Serial number for each USUBJID
rs$RSSEQ <- SetSEQ(rs, "USUBJID", "RSSEQ")

# Save datasets
WriteCSV_SDTM("CP932", outputpath)
