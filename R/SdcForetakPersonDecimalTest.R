
#' Test av SdcForetakPerson med decimal 
#' 
#' Om det blir samme svar via `decimal = TRUE`
#' 
#' @param ... SdcForetakPerson-parametere 
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
SdcForetakPersonDecimalTest <- function(...) {
  a <- SdcForetakPerson(...)
  d <- SdcForetakPerson(..., decimal = TRUE)
  b <- SdcForetakPerson(..., decimal = d)
  identical(b, a[names(b)])
}