# function
# ################################## # 
# SetSEQ function                    #
# Serial number for each Primary key #
# Arguments : dst as dataset         #
#             pkey as Primary key    #
#             setclmnm as ColumnName #
# Return : dst$setclmnm              #
# ################################## # 
SetSEQ <- function(dst, pkey, setclmnm) {
  #Init
  cntseq = 0
  svpkey <- ""
  
  # If the keys are changes, add 1 to the key
  for (i in 1:nrow(dst)) {
    if (dst[i,pkey] != svpkey) {
      cntseq <- cntseq + 1
    }  
    dst[i,setclmnm] <- cntseq
    svpkey <- dst[i,pkey]
  }
  return(dst[,setclmnm])
}
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
dm$RFSTDTC <- dataset$登録日
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
dm$RFXSTDTC <- dataset$治療開始日
dm$RFICDTC <- NA
# ####### CE #######  
ce <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
ce$DOMAIN <- "CE"
# TODO(ohtsuka): CE.SUBJIDが不明、要確認
ce$USUBJID <- paste(kStudyId, dataset$検体ID, sep = "-")
ce$CESEQ <- NA
ce$CETERM <- ifelse(is.na(dataset$再発の有無), "",  # NAはCESEQ確認の論理式でおかしな動きをするので取り除きました。
             ifelse(dataset$再発の有無 == "再発", "RELAPSE", ""))
ce$CEDECOD <- ce$CETERM
# for (i in unique(ce$CEDECOD)) {  # USUBJIDだとCESEQが全て1になってしまいますので、CEDECODで行ってみました。
#   ce[ce$CEDECOD == i, ]$CESEQ <- c(1:nrow(ce[ce$CEDECOD == i, ]))
# }
ce$CEDTC <- ifelse(dataset$再発の有無 == "再発", dataset$再発日, NA)
# Sort(asc) KEY=USUBJID,CETERM,CEDTC
ceSortlist <- order(ce$USUBJID, ce$CETERM, ce$CEDTC)
ce <- ce[ceSortlist,]
# Set CESEQ  Serial number for each USUBJID
ce$CESEQ = SetSEQ(ce, "USUBJID", "CESEQ")

# ####### DS #######  
ds <- data.frame(STUDYID = rep(kStudyId, nrow(dataset)))
ds$DOMAIN <- "DS"
ds$USUBJID <- paste(kStudyId, dataset$検体ID, sep = "-")
ds$DSSEQ <- NA
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
write.csv(dm, "dm.csv", row.names = F, na = "")
write.csv(ce, "ce.csv", row.names = F, na = "")
write.csv(ds, "ds.csv", row.names = F, na = "")
# write.csv(mh, "MH.csv", row.names = F, na = "")
# write.csv(rs, "RS.csv", row.names = F, na = "")
# write.csv(sc, "SC.csv", row.names = F, na = "")
setwd("../../..")
