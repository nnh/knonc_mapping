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
dm$BRTHDTC <- NULL  # TODO(ohtsuka): NULLだと列ごと削除されると思います。空値の列を作るならNAかと。
dm$AGE <- dataset$Age
dm$AGEU <- "YEARS"
dm$SEX <- NULL  #TODO(ohtsuka): Required変数なので削除はまずいかと。
dm$ARMCD <- ifelse(is.na(dataset$寛解導入療法), "SCRNFAIL",
            ifelse(is.na(dataset$地固め療法群), "INDFAIL",
              paste(substr(dataset$寛解導入療法, 1, 1), substr(dataset$地固め療法群, 1, 1), sep = "-")))
dm$ARM <- ifelse(is.na(dataset$寛解導入療法), "Screen Failure",
          ifelse(is.na(dataset$地固め療法群), "Induction Failure",
            chartr("（", ":", paste(gsub("）群", "", dataset$寛解導入療法), gsub("）群", "", dataset$地固め療法群), sep = "-"))))
dm$ACTARMCD <- dataset$ARMCD
dm$ACTARM <- dataset$ARM
dm$COUNTRY <- "JPN"
dm$RFXSTDTC <- dataset$治療開始日
dm$RFICDTC <- NULL
ce <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
ce$DOMAIN <- "CE"
# TODO(ohtsuka): CE.SUBJIDが不明、要確認
ce$USUBJID <-  paste(kStudyId, dataset$検体ID, sep = "-")
ce$CESEQ <- NA  # まずカラムを作成しておかないとエラーになる
for (i in unique(ce$USUBJID)) {
  ce[ce$USUBJID == i, ]$CESEQ <- c(1:nrow(ce[ce$USUBJID == i, ]))
}
ce$CETERM <- ifelse(is.na(dataset$再発の有無), "",  # NAはCESEQ確認の論理式でおかしな動きをするので取り除きました。
             ifelse(dataset$再発の有無 == "再発", "RELAPSE", ""))
ce$CEDECOD <- ce$CETERM
# for (i in unique(ce$CEDECOD)) {  # USUBJIDだとCESEQが全て1になってしまいますので、CEDECODで行ってみました。
#   ce[ce$CEDECOD == i, ]$CESEQ <- c(1:nrow(ce[ce$CEDECOD == i, ]))
# }
ce$CESTDTC <- ifelse(dataset$再発の有無 == "再発", dataset$再発日, NA)
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
