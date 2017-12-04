#' Set SEQ
#' Set serial number for each primary key
#'
#' @param dst A dataset
#' @param pkey A column name for primary key
#' @param columnname A column name to set serial number
#'
#' @return A vector dest$colummnname
#' @export
SetSEQ <- function(dst, pkey, columnname) {
  #Init
  cntseq <- 0
  save.pkey <- ""
  # If the keys are changes, add 1
  for (i in 1:nrow(dst)) {
    if (dst[i, pkey] != save.pkey) {
      cntseq <- cntseq + 1
    }
    dst[i, columnname] <- cntseq
    save.pkey <- dst[i, pkey]
  }
  return(dst[, columnname])
}

#' ISO8601Date function
#' YYYY/MM/DD -> YYYY-MM-DD
#'
#' @param dte A date
#'
#' @return A vector of character
#' @export
ISO8601Date <- function(dte) {
  return (as.character(as.Date(dte)))
}

kStudyId <- "JALSG-AML201"

# Load data
setwd(kStudyId)
setwd("./input/rawdata")
filenames <- list.files()
rawdata <- read.csv(filenames[1], as.is = T, na.strings = c(""), fileEncoding = "CP932")
setwd("../..")

# Format data
dataset <- rawdata[!is.na(rawdata$検体ID), ]
dm <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
dm$DOMAIN <- "DM"
dm$USUBJID <- paste(kStudyId, dataset$検体ID, sep = "-")
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
pr <- data.frame(STUDYID = rep(kStudyId, nrow(pr_temp)))
pr$DOMAIN <- "PR"
pr$USUBJID <- paste(kStudyId, pr_temp$検体ID, sep = "-")
pr$PRSEQ <- NA
pr$PRTRT <- "Hematopoietic Stem Cell Transplantation"
pr$PRCAT <- "Unknown"
pr$PRSTDTC <- pr_temp$移植日
# Sort(asc) KEY=USUBJID
prSortlist <- order(pr$USUBJID)
pr <- pr[prSortlist, ]
# Set SEQ  Serial number for each USUBJID
pr$PRSEQ <- SetSEQ(pr, "USUBJID", "PRSEQ")

# ####### CE #######
# 再発の有無が"再発"のレコードのみ出力対象とする
ce_temp <- subset(dataset, 再発の有無=="再発")
ce <- data.frame(STUDYID = rep(kStudyId, nrow(ce_temp)))
ce$DOMAIN <- "CE"
ce$USUBJID <- paste(kStudyId, ce_temp$検体ID, sep = "-")
ce$CESEQ <- NA
ce$CETERM <- "DISEASE RELAPSE"
ce$CEDECOD <- ce$CETERM
ce$CEDTC <- ISO8601Date(ce_temp$再発日)
# Sort(asc) KEY=USUBJID,CETERM,CEDTC
ceSortlist <- order(ce$USUBJID, ce$CETERM, ce$CEDTC)
ce <- ce[ceSortlist, ]
# Set CESEQ  Serial number for each USUBJID
ce$CESEQ <- SetSEQ(ce, "USUBJID", "CESEQ")

# ####### DS #######
ds <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
ds$DOMAIN <- "DS"
ds$USUBJID <- paste(kStudyId, dataset$検体ID, sep = "-")
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
dssortlist <- order(ds$USUBJID, ds$DSTERM, ds$DSSTDTC)
ds <- ds[dssortlist, ]
# Set DSSEQ  Serial number for each USUBJID
ds$DSSEQ <- SetSEQ(ds, "USUBJID", "DSSEQ")

# ####### MH #######
mh <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
mh$DOMAIN <- "MH"
mh$USUBJID <- paste(kStudyId, dataset$検体ID, sep = "-")
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
mhsortlist <- order(mh$USUBJID, mh$MHCAT)
mh <- mh[mhsortlist, ]
# Set MHSEQ  Serial number for each USUBJID
mh$MHSEQ <- SetSEQ(mh, "USUBJID", "MHSEQ")

# ####### RS #######
rs <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
rs$DOMAIN <- "RS"
rs$USUBJID <- paste(kStudyId, dataset$検体ID, sep = "-")
rs$RSSEQ <- NA
rs$RSTESTCD <- "INDCRESP"
rs$RSTEST <- "Induction Response"
rs$RSCAT <- "DEFINED BY PROTOCOL"
rs$RSORRES <- ifelse(dataset$CR == "CR", "CR",
              ifelse(dataset$CR == "nonCR", "NR", "NE"))
rs$RSSTRESC <- rs$RSORRES
rs$RSDTC <- ISO8601Date(dataset$寛解到達日)
# Sort(asc) KEY=USUBJID
rssortlist <- order(rs$USUBJID)
rs <- rs[rssortlist, ]
# Set RSSEQ  Serial number for each USUBJID
rs$RSSEQ <- SetSEQ(rs, "USUBJID", "RSSEQ")

# Save datasets
setwd("./output/SDTM")
write.csv(dm, "dm.csv", row.names = F, na = "")
write.csv(pr, "pr.csv", row.names = F, na = "")
write.csv(ce, "ce.csv", row.names = F, na = "")
write.csv(ds, "ds.csv", row.names = F, na = "")
write.csv(mh, "mh.csv", row.names = F, na = "")
write.csv(rs, "rs.csv", row.names = F, na = "")
setwd("../../..")
