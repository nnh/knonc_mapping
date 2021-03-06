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
  # 同一のキーで複数レコードがあれば連番を振る
  for (i in 1:nrow(dst)) {
    if (dst[i, pkey] != save.pkey) {
      cntseq <- 1
    } else {
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

#' WriteCSV_SDTM function
#' Output Datasets
#'
#' @param encode File Encoding
#' "CP932", "UTF-8"...
#'        outputpath 省略時は"./output/SDTM"をセット
#' @return
#' @export CSV File
WriteCSV_SDTM <- function(encode, outputpath=NA){
  # 出力するdomainを指定、存在しないデータセットはスキップする
  output_domain_T <- c("dm",
                       "pr",
                       "ce",
                       "ds",
                       "mh",
                       "sc",
                       "rs")
  if (is.na(outputpath)){
    setwd("./output/SDTM")
    outputpath <- getwd()
  }
  for (i in 1:length(output_domain_T)){
    if(exists(output_domain_T[i])){
      dst <- get(output_domain_T[i])
      write.csv(dst, paste0(outputpath, "/", output_domain_T[i], ".csv"), row.names = F, na = "", fileEncoding = encode)
    }
  }
  if (is.na(outputpath)){
    setwd("../../..")
  }
}

#' SetCommon_dataset function
#' ドメイン共通列の生成
#'
#' @param input_dataset
#' @return Common_dataset
#' @export
SetCommon_dataset <- function(input_dataset){
  # Common Columns
  common_dataset <- data.frame(STUDYID = rep(kStudyId, nrow(input_dataset)))
  common_dataset$DOMAIN <- NA
  common_dataset$USUBJID <- input_dataset$USUBJID
  return(common_dataset)
}
