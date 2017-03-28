# Constant
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
dm$RFSTDTC <- NA
# TODO(ohtsuka): 変数の順番に処理して加えていって下さい。GoogleSpreadsheetで下記作りました。
dm$BRTHDTC <- NULL
dm$AGE <- dataset$Age
dm$AGEU <- "YEARS"
dm$SEX <- NULL
dm$ARMCD <- ifelse(is.na(dataset$寛解導入療法), "SCRNFAIL",
            ifelse(is.na(dataset$地固め療法群), "INDFAIL",
              paste(substr(dataset$寛解導入療法, 1, 1), substr(dataset$地固め療法群, 1, 1), sep = "-")))
dm$ARM <- ifelse(is.na(dataset$寛解導入療法), "Screen Failure",
                 ifelse(is.na(dataset$地固め療法群), "Induction Failure",
                        paste(gsub("群", "", dataset$寛解導入療法), gsub("群", "", dataset$地固め療法群), sep = "-")))
# dm$ACTARMCD <-
# dm$ACTARM <-
# dm$COUNTRY <-
# dm$RFXSTDTC <-
# dm$RFICDTC <-
ce <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
# ce$DOMAIN <-
# ce$USUBJID <-
# ce$CESEQ <-
# ce$CETERM <-
ce$CEDECOD <- ifelse(dataset$再発の有無 == "再発", "RELAPSE", NA)
# ce$CESTDTC <-
# ds$STUDYID <-
# ds$DOMAIN <-
# ds$USUBJID <-
# ds$DSSEQ <-
# ds$DSTERM <-
# ds$DSDECOD <-
# ds$DSCAT <-
# ds$DSSTDTC <-
# mh$STUDYID <-
# mh$DOMAIN <-
# mh$USUBJID <-
# mh$MHSEQ <-
# mh$MHCAT <-
# mh$MHTERM <-
# mh$MHDECOD <-
# mh$MHSTDTC <-
# mh$MHPTCD <-
# mh$MHSOC <-
# mh$MHSOCCD <-
# sc$STUDYID <-
# sc$DOMAIN <-
# sc$USUBJID <-
# sc$SCSEQ <-
# sc$SCTESTCD <-
# sc$SCTEST <-
# sc$SCORRES <-
# sc$SCSTRESC <-
# rs$STUDYID <-
# rs$DOMAIN <-
# rs$USUBJID <-
# rs$RSSEQ <-
# rs$RSTESTCD <-
# rs$RSTEST <-
# rs$RSCAT <-
# rs$RSORRES  <-
# rs$RSSTRESC <-

# Save datasets
setwd("./output/SDTM")
write.csv(dm, "DM.csv", row.names = F, na = "")
write.csv(ce, "CE.csv", row.names = F, na = "")
# write.csv(ds, "DS.csv", row.names = F, na = "")
# write.csv(mh, "MH.csv", row.names = F, na = "")
# write.csv(rs, "RS.csv", row.names = F, na = "")
# write.csv(sc, "SC.csv", row.names = F, na = "")
setwd("../../..")
