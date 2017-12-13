# readxlのインストールが必要
# install.packages('readxl')
library("readxl")
# common function
source("./common_programs/common_fnk.R")
# TimeZone取得エラー対応
Sys.setenv(TZ="Asia/Tokyo")
kStudyId <- "TCCSG-ALL-L04-16"

# Load data
setwd(kStudyId)
setwd("./input/rawdata")
filenames <- list.files()
rawdata <- read_excel(filenames[1], sheet=1, col_names=T)
setwd("../..")

# Format dataset
dataset <- rawdata[!is.na(rawdata[ ,"Sample No"]), ]
# 全ての"Sample No"
wk0_dataset <- dataset
wk0_dataset <- wk0_dataset[ ,1:3]
colnames(wk0_dataset) <- c("ID","del","Sample No")
wk0_dataset$ID <- as.vector(t(wk0_dataset[ ,3]))
# 重複1
wk1_dataset <- subset(dataset, !is.na(dataset[ ,1]))
wk1_dataset <- wk1_dataset[ ,1:3]
colnames(wk1_dataset) <- c("ID","del","Sample No")
# 重複2
wk2_dataset <- subset(dataset, !is.na(dataset[ ,2]))
wk2_dataset <- wk2_dataset[ ,1:3]
colnames(wk2_dataset) <- c("del","ID","Sample No")
# マージ
wk3_dataset <- rbind(wk0_dataset[ ,c(1,3)],wk1_dataset[ ,c(1,3)],wk2_dataset[ ,2:3])
# "Sample No",＂ID"昇順でソート
wk3_dataset[ ,"Sample No"] <- as.vector(t(wk3_dataset[ ,"Sample No"]))
wk3_dataset[ ,"ID"] <- as.vector(t(wk3_dataset[ ,"ID"]))
sortlist <- order(as.vector(t(wk3_dataset[ ,"Sample No"])),as.vector(t(wk3_dataset[ ,"ID"])))
wk4_dataset <- wk3_dataset[sortlist, ]

# output csv
output_dataset <- data.frame()
output_dataset <- data.frame(matrix(rep(NA), ncol=2, nrow=nrow(wk4_dataset)))
colnames(output_dataset) <- c("USUBJID", "ID")
# USUBJIDを生成
output_dataset$USUBJID <- apply(wk4_dataset[ ,"Sample No"], 1, function(x) paste(kStudyId, x, sep="-"))
output_dataset$ID <- wk4_dataset$ID
# Save datasets
setwd("./output/SDTM")
write.csv(output_dataset, "linktable.csv", row.names = F, na = "", fileEncoding = "CP932")
setwd("../../..")
