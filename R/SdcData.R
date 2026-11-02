#' syssel100 data
#' 
#' @name syssel100
#' @docType data
#' @format A data frame 
#' @keywords datasets
#' @usage data(syssel100)
NULL


#' sn2007_sn2025 data
#' 
#' @details
#' 
#' Koden som genererte dataene 
#' 
#' \preformatted{
#' url1 <- paste0(
#'   "https://www.ssb.no/virksomheter-foretak-og-regnskap/",
#'   "metoder-og-dokumentasjon/ny-standard-for-naeringsgruppering-",
#'   "innfores-1.september-2025/_/attachment/inline/",
#'   "59422d4c-e20e-46b9-a8c7-5afaf468daa5:",
#'   "354c259375a8c7b284c0703a986b00cc2f57b740/",
#'   "Korrespondansetabell\%20SN2007-SN2025_29.04.2026.xlsx"
#' )
#' read_excel_url <- function(path, sheet = 1, ...) {
#'   if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
#'   if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
#'   library(httr)
#'   library(readxl)
#' 
#'   # Hvis path er en http/https-URL, last ned midlertidig
#'   if (grepl("^https?://", path, ignore.case = TRUE)) {
#'     tmpfile <- tempfile(fileext = ".xlsx")
#'     message("📥 Leser Excel direkte fra URL (laster ned midlertidig)...")
#'     GET(path, write_disk(tmpfile, overwrite = TRUE))
#'     path <- tmpfile
#'   }
#' 
#'   # Bruk vanlig read_excel
#'   as.data.frame(read_excel(path, sheet = sheet, ...))
#' }
#' 
#' 
#' df1 <- read_excel_url(url1)[c(1,3)]
#' df <- df1[nchar(df1[[1]]) == 6 & nchar(df1[[2]]) == 6 & !is.na(df1[[1]])  & !is.na(df1[[2]]),  ]
#' 
#' 
#' nace_siffer <- function(df, siffer = 5) {
#'   if (siffer < 5) {
#'     siffer_ <- siffer
#'     if (siffer > 2) {
#'       siffer_ <- siffer + 1
#'     }
#'     df[[1]] <- substr(df[[1]], 1, siffer_)
#'     df[[2]] <- substr(df[[2]], 1, siffer_)
#'   }
#'   names(df) <- paste(names(df), siffer, sep = "_")
#'   df
#' }
#' 
#' sn2007_sn2025 <- cbind(nace_siffer(df, 1), 
#'                        nace_siffer(df, 2), 
#'                        nace_siffer(df, 3), 
#'                        nace_siffer(df, 4), 
#'                        nace_siffer(df, 5))
#' }
#' 
#' 
#' @name sn2007_sn2025
#' @docType data
#' @format A data frame 
#' @keywords datasets
#' @usage data(sn2007_sn2025)
NULL



# stackoverflow questions 30357330
pkgEnvSdcForetakPersonData <- new.env(parent=emptyenv())
#' Function that returns a dataset
#'
#' 
#' @param dataset Name of data set within the SdcForetakPerson package
#'
#' @return data frame
#' 
#' @export
#' @importFrom utils data
#' @author Øyvind Langsrud
#'
#' @examples
#' SdcData("syssel7")
#' z <- SdcData("syssel100")
#' a10 <- SdcData("sdc_sample_10")
#' a1000 <- SdcData("sdc_sample_1000")
SdcData <- function(dataset) {
  if(dataset == "syssel7"){
    z <- SdcData("syssel100")[c(25, 29, 49, 73, 74, 86, 99) ,c(3,4,5,9,10)]
    rownames(z) <- NULL
    return(z)
  }
  
  if(dataset == "syssel27"){
    z <- SdcData("syssel100")[c(4, 5, 12, 14, 16, 21, 25, 27, 33, 38, 44, 45, 54, 55, 56, 66, 
                                68, 72, 82, 83, 84, 85, 89, 94, 95, 96, 97), ]
    rownames(z) <- NULL
    return(z)
  }
  
  if (substr(dataset, 1, 11) == "sdc_sample_") {  
    n <- as.integer(substr(dataset, 12, 22))
    z <-  SdcData("syssel100")
    z <- z[-(3:4)]
    df <- SdcData("sn2007_sn2025")
    a <- sdc_sample(n, z, df)
    a$sektor <- "Privat"  # stemmer ikke nedefor, men genererer noe 
    a$sektor[a$SN2007_2 == "85"] <- "Fylke"
    a$sektor[a$SN2007_2 == "86"] <- "Kommual"
    a$sektor[a$SN2007_2 == "87"] <- "Statlig"
    a$sektor2 <- "Off"
    a$sektor2[a$sektor == "Privat"] <- "Privat"
    return(a)
  }
  
  if (!exists(dataset, pkgEnvSdcForetakPersonData))
    data(list = dataset, package = "SdcForetakPerson", envir = pkgEnvSdcForetakPersonData)
  return(pkgEnvSdcForetakPersonData[[dataset]])
  return(NULL)
}



sdc_sample <- function(n, z, df, rndSeed = 123) {
  if (!is.null(rndSeed)) {       
    if (!exists(".Random.seed")) 
      if (runif(1) < 0) 
        stop("Now seed exists")
    exitSeed <- .Random.seed
    on.exit(.Random.seed <<- exitSeed)
    set.seed(rndSeed)
  }
  z <- z[rep_len(seq_len(nrow(z)), n), ]
  df <- df[sample.int(nrow(df), n, replace = TRUE), ]
  a <- cbind(z, df)
  rownames(a) <- NULL
  a
}
