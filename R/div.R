
#' Genererer variabel som er foretak-ID dersom den finnes, ellers virksomhet-ID, eller en unik ID. 
#'
#' @param data data frame 
#' @param frtk foretak variabel 
#' @param virk virksomhet variabel
#' @param unik unik variabel  
#' @param makeunik Unik variabel genereres ved TRUE eller antas det at den finnes 
#' @param varnames Vektor av 7 variabelnavn. Bruk NA dersom variabel ikke ønskes i output. 
#' @param miss Kode for missing 
#' @param dupvar Variabler som definerer grupper 
#'
#' @return input data med nye variabler 
#' @export
#' @importFrom SSBtools Number
#' @examples 
#' z <- SdcData("syssel7")
#' Make_FRTK_VIRK_UNIK_AggVar(z)
#' Make_FRTK_VIRK_UNIK_AggVar(z, 
#'     varnames = c("FRTK_VIRK_UNIK", "sysselsatte", NA, NA, NA, NA, "ant_FRTK_VIRK_UNIK"))
#' z$id <- 1:7
#' Make_FRTK_VIRK_UNIK_AggVar(z, unik = "id", makeunik = FALSE)
Make_FRTK_VIRK_UNIK_AggVar =  function(data, frtk="FRTK_ID_SSB", virk="VIRK_ID_SSB", unik = "UNIK_ID", 
                                       makeunik = TRUE,
                                       varnames = c("FRTK_VIRK_UNIK", "sysselsatte", "sysselsatteVIRK", "sysselsatteFRTK", "antVIRK", "antFRTK", "ant_FRTK_VIRK_UNIK"),
                                       miss="", dupvar=NULL){
  if(makeunik)
    data[[unik]] =  paste0("UNIK",Number(seq_len(nrow(data)),7))
  if(!is.na(varnames[1])) data[[varnames[1]]] = MissingReplaceFirst( data[, c(frtk, virk, unik)],missing = miss) 
  if(!is.na(varnames[2])) data[[varnames[2]]] = Ones2agg(data, varnames[1]) 
  if(!is.na(varnames[3])) data[[varnames[3]]] = Ones2agg(data, virk, miss) 
  if(!is.na(varnames[4])) data[[varnames[4]]] = Ones2agg(data, frtk, miss) 
  if(!is.na(varnames[5])) data[[varnames[5]]] = Ones2agg(data, virk, miss, dupvar) 
  if(!is.na(varnames[6])) data[[varnames[6]]] = Ones2agg(data, frtk, miss, dupvar) 
  if(!is.na(varnames[7])) data[[varnames[7]]] = Ones2agg(data, varnames[1], miss=NULL, dupvar) 
  data
}




#' Ones to be aggregated to counts 
#' 
#' Based on a variable in the data, missing and duplicates within groups are set to zero.
#' 
#' To treat duplicates without grouping set `dupvar` to the same value as `var`.  
#' Also `character(0)` or `integer(0)` will work when `var` is specified 
#' by name or number, respectively.     
#' 
#'
#' @param data A data frame
#' @param var A variable in data 
#' @param miss Values treated as missing
#' @param dupvar variables defining groups (see details)  
#' @param int Integers returned when TRUE 
#'
#' @return Vector of ones and zeros 
#' @export
#'
#' @examples
#' a <- data.frame(x = rep(c("A", "B", "C", ""), each = 2), 
#'                 y = c(5, 5, 5, 6, 6, 6, 7, 7), 
#'                 z = c("r", rep("s", 7)))
#' cbind(a, ones = Ones2agg(a, "x"))
#' cbind(a, ones = Ones2agg(a, "x", ""))
#' cbind(a, ones = Ones2agg(a, "x", "", "y"))
#' cbind(a, ones = Ones2agg(a, "x", "", "z"))
#' cbind(a, ones = Ones2agg(a, "x", "", c("y", "z")))
#' cbind(a, ones = Ones2agg(a, "x", dupvar = "x"))
#' cbind(a, ones = Ones2agg(a, "x", dupvar = character(0)))
#' a[1, 1] <- NA
#' cbind(a, ones = Ones2agg(a, "x"))
Ones2agg <- function(data, var, miss = NULL, dupvar = NULL, int = TRUE) {
  
  x <- !is.na(data[[var]])
  
  if (!is.null(miss)) 
    x[data[[var]] %in% miss] <- FALSE
  
  if (!is.null(dupvar)) 
    x[duplicated(data[, unique(c(dupvar, var))])] <- FALSE
  
  if (!int) 
    return(as.numeric(x))
  
  as.integer(x)
}




#' First column with missing values replaced 
#'
#' The first column is returned with missing values sequentially replaced with values from the other columns  
#' 
#' @param x data.frame 
#' @param missing Values treated as missing
#'
#' @return First column with missing values replaced 
#' @export
#'
#' @examples
#' a <- data.frame(x = c("yes", "", "no", ""), y = c("", "ok", "ok", ""), z = "z")
#' print(a)
#' MissingReplaceFirst(a)
#' MissingReplaceFirst(a, "")
#' MissingReplaceFirst(a, c("", "yes"))
#' a[3, 1] <- NA
#' MissingReplaceFirst(a)
MissingReplaceFirst <- function(x, missing = NULL) {
  z <- x[[1]]
  z[] <- NA
  for (i in seq_along(x)) {
    miss <- is.na(z)
    if (!any(miss)) 
      return(z)
    a <- x[[i]][miss]
    if (!is.null(missing)) 
      a[a %in% missing] <- NA
    z[miss] <- a
  }
  z
}



Primary_FRTK_VIRK_UNIK_sektor_ZerosCanBeTRUE <- function(data, freq, x, maxN, protectZeros, num, charVar, ...) {
  primary <- freq <= maxN
  
  if (!protectZeros) 
    primary[freq == 0] <- FALSE   
  
  if (anyNA(match(c("sektor", "FRTK_VIRK_UNIK"), charVar))) 
    stop("sektor and/or FRTK_VIRK_UNIK missing in charVar")
  
  if (anyNA(match(c("sektor", "FRTK_VIRK_UNIK"), names(data)))) 
    stop("sektor and/or FRTK_VIRK_UNIK missing in data")
  
  y <- data[["FRTK_VIRK_UNIK"]]
  
  data_sektor_privat <- data[["sektor"]] == "Privat"
  
  if (!any(data_sektor_privat)) {
    warning("Ingen tilfeller av 'Privat' sektor funnet")
  } else {
    
    y[data_sektor_privat] <- NA
    nOff <- Ncontributors(x, y)
    y <- data[["FRTK_VIRK_UNIK"]]
    y[data[["sektor"]] != "Privat"] <- NA
    nPrivat <- Ncontributors(x, y)
    primary[nPrivat == 1 & nOff == 0] <- TRUE
    primary[nPrivat == 1 & nOff == 1] <- TRUE
    primary[nPrivat == 2 & nOff == 0] <- TRUE
  }
  
  list(primary = primary, numExtra = data.frame(nPrivat=nPrivat, nOff=nOff))
}




Primary_FRTK_VIRK_UNIK_sektor <- function(data, freq, x, maxN, protectZeros, num, charVar, sector="sektor", private = "Privat", between = NULL, crossTable,...) {
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
  primary[nPrivat == 1 & nOff == 0] <- TRUE
  primary[nPrivat == 1 & nOff == 1] <- TRUE
  primary[nPrivat == 2 & nOff == 0] <- TRUE
  
  if (!protectZeros) 
    primary[freq == 0] <- FALSE   
  
  list(primary = primary, numExtra = data.frame(nPrivat=nPrivat, nOff=nOff))
}



Primary_FRTK_VIRK_UNIK_sektor_smallFALSE <- function(freq, maxN, ...) {
  primary <- rep(FALSE, length(freq))
  primary[Primary_FRTK_VIRK_UNIK_sektor(freq = freq, maxN = -1, ...)] <- TRUE
  primary[freq <= maxN] <- FALSE
  primary
}


Make_NarWeight_00 <- function(x , narVar, nar00="00"){
  as.integer(x[[narVar]]!= nar00)
}


GaussSuppressed <- function(x, dimVar){
  x = SortRows(x[x$suppressed, names(x) %in% dimVar])
  rownames(x) = NULL
  x
}


Hidden_Lonnstaker0<- function(crossTable, ...) {
  if (!("Lonnstaker" %in% names(crossTable))) 
    stop("Lonnstaker missing")
  crossTable[["Lonnstaker"]] == "0"
}


Primary_FRTK_VIRK_UNIK_sektor_Lonnstaker0 <- function(data, freq, x, maxN, protectZeros, num, charVar, crossTable, ...) {
  if (!("Lonnstaker" %in% names(crossTable))) 
    stop("Lonnstaker missing")
  
  p2 <- Primary_FRTK_VIRK_UNIK_sektor(data = data, freq = freq, x = x, maxN = maxN, protectZeros=protectZeros, num=num, charVar = charVar, crossTable=crossTable, ...)
  
  p2$primary[crossTable[["Lonnstaker"]] == "0"] <- FALSE
  
  p2                   
}




Primary_FRTK_VIRK_UNIK_sektor_Lonnstaker0B <- function(data, freq, x, maxN, protectZeros, num, charVar, crossTable, ...) {
  if (!("Lonnstaker" %in% names(crossTable))) 
    stop("Lonnstaker missing")
  
  primary <- freq <= maxN
  
  r = which(crossTable[["Lonnstaker"]] == "Total")
  
  p2 <- Primary_FRTK_VIRK_UNIK_sektor(data = data, freq = freq[r], x = x[, r], maxN = -1, protectZeros=TRUE, num=num[r, ], charVar = charVar, crossTable=crossTable[r, ], ...)
  
  #primary <- rep(FALSE, length(freq))
  
  
  k = match("Lonnstaker", names(crossTable))

  maPrimary = Match(crossTable[, -k ,drop=FALSE], crossTable[ r[p2$primary], -k ,drop=FALSE])
  
  maAll = Match(crossTable[, -k ,drop=FALSE], crossTable[ r, -k ,drop=FALSE])
  
  primary[!is.na(maPrimary)] <- TRUE
  
  if (!protectZeros) 
    primary[freq == 0] <- FALSE   
  
  primary[crossTable[["Lonnstaker"]] == "0"] <- FALSE
  p2$primary = primary
  p2[[2]] = p2[[2]][maAll, ]              
  p2
}




