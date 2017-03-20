# Constant
kStudyId <- "JALSG-AML201"

# Load data
setwd(kStudyId)
setwd("./input/rawdata")
filenames <- list.files()
rawdata <- read.csv(filenames[1], as.is = T, fileEncoding = "CP932")
setwd("../..")

# Format data
dm <- data.frame(STUDYID = rep(kStudyId, nrow(rawdata)))
dm$DOMAIN <- "DM"
dm$USUBJID <- paste(kStudyId, rawdata$検体ID, sep = "-")
dm$SUBJID <- rawdata$検体ID
dm$RFSTDTC <- NA
# TODO(ohtsuka): 変数の順番に処理して加えていって下さい。GoogleSpreadsheetで下記作りました。
# dm$BRTHDTC <-
# dm$AGE <-
# dm$AGEU <-
# dm$SEX <-

for (i in nrow(rawdata)) {
  # if (rawdata$寛解導入療法[i] == "") {
  #   rawdata$ARMCD[i] <- "SCRNFAIL"
  # } else if (rawdata$地固め療法群[i] == "") {
  #   rawdata$ARMCD[i] <- "INDFAIL"
  # } else {
    rawdata$ARMCD[i] <- paste(substr(rawdata$寛解導入療法[i], 1, 1), substr(rawdata$地固め療法群[i], 1, 1), sep = "-")
  # }
}
dm$ARMCD <- rawdata$ARMCD

# dm$ARM <-
# dm$ACTARMCD <-
# dm$ACTARM <-
# dm$COUNTRY <-
# dm$RFXSTDTC <-
# dm$RFICDTC <-
# ce$STUDYID <-
# ce$DOMAIN <-
# ce$USUBJID <-
# ce$CESEQ <-
# ce$CETERM <-
# ce$CEDECOD <-
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
# TODO(ohtsuka): 他のドメインについても同様に処理をして下さい

# Save datasets
setwd("./output/SDTM")
write.csv(dm, "DM.csv", row.names = F, na = "")
# write.csv(ds, "DS.csv", row.names = F, na = "")
# write.csv(mh, "MH.csv", row.names = F, na = "")
# write.csv(rs, "RS.csv", row.names = F, na = "")
# write.csv(sc, "SC.csv", row.names = F, na = "")
setwd("../../..")
