
#' Test av SdcForetakPerson med decimal 
#' 
#' Om det blir samme svar via `decimal = TRUE`
#' 
#' @param data vanlige data 
#' @param data2 data brukt når decimal er input (noen variabler kan fjernes)
#' @param ... SdcForetakPerson-parametere 
#' @param stopWhenFALSE Feiler ved FALSE når TRUE   
#'
#' @return logical
#' @export
#' 
#' @keywords internal
#'
#' @examples
#' prikkeVarA <- c("arb_fylke", "ARB_ARBKOMM", "nar8", "sektor")
#' z <- SdcData("syssel27")
#' SdcForetakPersonDecimalTest(z, between = prikkeVarA)
SdcForetakPersonDecimalTest <- function(data, data2, ..., stopWhenFALSE = TRUE) {
  a <- SdcForetakPerson(data, ...)
  d <- SdcForetakPerson(data, ..., decimal = TRUE)
  b <- SdcForetakPerson(data2, ..., decimal = d)
  isIdentical <- identical(b, a[names(b)])
  if(stopWhenFALSE & !isIdentical)
    stop("Not identical results")
  isIdentical
}