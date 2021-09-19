
#' Test av SdcForetakPerson med decimal 
#' 
#' Om det blir samme svar via `decimal = TRUE`
#' 
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
SdcForetakPersonDecimalTest <- function(..., stopWhenFALSE = TRUE) {
  a <- SdcForetakPerson(...)
  d <- SdcForetakPerson(..., decimal = TRUE)
  b <- SdcForetakPerson(..., decimal = d)
  isIdentical <- identical(b, a[names(b)])
  if(stopWhenFALSE & !isIdentical)
    stop("Not identical results")
  isIdentical
}