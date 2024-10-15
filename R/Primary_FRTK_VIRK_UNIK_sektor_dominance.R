Primary_FRTK_VIRK_UNIK_sektor_dominance <- function(data, 
                                                      freq, 
                                                      x, 
                                                      maxN, 
                                                      protectZeros, 
                                                      num, 
                                                      charVar, 
                                                      sector="sektor", 
                                                      private = "Privat", 
                                                      between = NULL, 
                                                      crossTable, 
                                                      allowTotal = FALSE,
                                                      k1 = NULL,
                                                      k2 = NULL,
                                                      pPercent = NULL,
                                                      freqVar = freqVar,
                                                      ...) {
  
  magnitude_rule <- length(c(k1, k2, pPercent)) > 0
  
  if (magnitude_rule) {
    if (!is.null(between)) {
      stop("Bruk av k1, k2 eller pPercent ikke implementert i for dette tilfellet")
    }
  }
  
  primary <- freq <= maxN
  
  if (anyNA(match(c(sector, "FRTK_VIRK_UNIK"), charVar))) 
    stop("sektor and/or FRTK_VIRK_UNIK missing in charVar")
  
  if (anyNA(match(c(sector, "FRTK_VIRK_UNIK"), names(data)))) 
    stop("sektor and/or FRTK_VIRK_UNIK missing in data")
  
  y <- data[["FRTK_VIRK_UNIK"]]
  
  data_sektor_privat <- data[[sector]] == private
  
  if (!any(data_sektor_privat)) {
    warning(paste0('Ingen tilfeller av "', private, '" funnet i ',  sector))
  }
  
  if(!is.null(between)){
    holdingInd = RowGroups(crossTable[ , names(crossTable) %in% between, drop=FALSE])
  } else {
    holdingInd=NULL
  }
  
  y[data_sektor_privat] <- NA
  nOff <- NcontributorsHolding(x, y, holdingInd)
  y <- data[["FRTK_VIRK_UNIK"]]
  y[data[[sector]] != private] <- NA
  nPrivat <- NcontributorsHolding(x, y, holdingInd)
  
  
  primaryB <- rep(FALSE, length(freq)) 
  
  primaryB[nPrivat == 1 & nOff == 0] <- TRUE
  primaryB[nPrivat == 1 & nOff == 1] <- TRUE
  primaryB[nPrivat == 2 & nOff == 0] <- TRUE
  
  if (magnitude_rule) {
    mc <- MaxContribution(x = x, y = data[[freqVar]], n = 2, groups = y)
    mc[is.na(mc)] <- 0
    p12 <- 100 * mc/freq
    colnames(p12) <- c("dominance1", "dominance2")
    if (!is.null(pPercent)) {
      primaryB[((100 - p12[, 1] - p12[, 2])/p12[, 1]) < pPercent/100] <- TRUE
    } else {
      primaryB[p12[, 1] > k1] <- TRUE
      primaryB[(p12[, 1] + p12[, 2]) > k2] <- TRUE
    }
  } else {
    p12 <- matrix(0, length(freq), 0)
  }
  
  if(allowTotal){
    if(!length(between)){
      allowTotal <- FALSE
      warning("allowTotal ignored when no between variables")
    }
  }
  
  if(allowTotal){
    crossTable_B <- crossTable[ primaryB, !(names(crossTable) %in% between), drop=FALSE]
    if(ncol(crossTable_B) & nrow(crossTable_B)){
      primaryB[primaryB][rowSums(crossTable_B != "Total") == 0] <- FALSE
    }
    rm(crossTable_B)
  } 
  primary[primaryB] <- TRUE 
  
  if (!protectZeros) 
    primary[freq == 0] <- FALSE   
  
  list(primary = primary, numExtra = cbind(data.frame(nPrivat=nPrivat, nOff=nOff), p12))
}