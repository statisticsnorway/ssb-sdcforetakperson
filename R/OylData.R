#' syssel100 data
#' 
#' @name syssel100
#' @docType data
#' @format A data frame 
#' @keywords datasets
#' @usage data(syssel100)
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
  
  if (!exists(dataset, pkgEnvSdcForetakPersonData))
    data(list = dataset, package = "SdcForetakPerson", envir = pkgEnvSdcForetakPersonData)
  return(pkgEnvSdcForetakPersonData[[dataset]])
  return(NULL)
}
