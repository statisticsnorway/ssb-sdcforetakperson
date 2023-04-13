#' Navn i data som er matchende etter \code{\link{tolower}}
#'
#' @param x Data eller bare vektor med navn 
#' @param code Kode det skal matches mot
#'
#' @return Som input-`code`, eventuelt endret  p.g.a. første match som ble funnet. 
#' @export
#'
#' @examples
#' z <- SdcData("syssel27")
#' lower_match(z, "frtk_id_ssb")
#' lower_match(z, "frtk_ID_ssb")
#' lower_match(z, "ALDER")  # ingen match funnet 
#' lower_match(z, "ALDER6")
#' lower_match(z, "Sektor")
lower_match <- function(x, code) {
  if (!is.null(dim(x))) {
    x <- names(x)
  }
  ma <- match(tolower(code), tolower(x))
  if (is.na(ma)) {
    return(code)
  }
  x[ma]
}