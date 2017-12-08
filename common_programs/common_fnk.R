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
