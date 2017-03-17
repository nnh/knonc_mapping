# Constant
kStudyId <- "JALSG-AML201"

# Load data
setwd(kStudyId)
setwd("./input/rawdata")
filenames <- list.files()
rawdata <- read.csv(filenames[1], as.is = T, fileEncoding = 'CP932')
setwd("../..")

# Format data
dm <- data.frame(STUDYID = rep(kStudyId, nrow(rawdata)))
dm$DOMAIN <- "DM"
dm$USUBJID <- paste(kStudyId, rawdata$検体ID, sep = "-")
dm$SUBJID <- rawdata$検体ID
dm$RFSTDTC <- ""
# TODO(ohtsuka): 変数の順番に処理して加えていって下さい。
# TODO(ohtsuka): JACLS-ALL02のSDTMのcsvを上手く使って"dm$XXX <- "の部分はExcelでコード生成して下さい

# TODO(ohtsuka): 他のドメインについても同様に処理をして下さい

# Save datasets
setwd("./output")
write.csv(dm, "DM.csv", row.names = F)
# write.csv(ds, "DS.csv", row.names = F)
# write.csv(mh, "MH.csv", row.names = F)
# write.csv(rs, "RS.csv", row.names = F)
# write.csv(sc, "SC.csv", row.names = F)
setwd("../..")
