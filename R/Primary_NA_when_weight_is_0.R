
#
# I SdcForetakPerson settes weightVar = "narWeight" til 0 for nace-kode nace00.
# Når nace00 aldri skal primærprikkes kan derfor dette baseres på weight
# Denne funksjonen kan brukes i kombinasjon med "vanlig" primærprikkefunksjon. 
#
Primary_NA_when_weight_is_0 <- function(weight, crossTable, ...) {
  primary <- rep(FALSE, nrow(crossTable))
  if (!is.null(weight)) {
    primary[weight == 0] <- NA
  }
  primary
}


