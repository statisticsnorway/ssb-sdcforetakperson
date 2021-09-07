

#' Prikking av foretak og avrunding eller prikking av personer
#'
#' @param data Datasett som data frame 
#' @param between  Variabler som grupperer foretak for prikking  
#' @param within Ytterligere variabler innen foretak som brukes til avrunding eller prikking av personer
#' @param by Tid eller andre variabler som deler datasettet. Metoden kjøres på hver del og resultatet settes sammen. 
#' @param roundBase Base for avrunding
#' @param maxN Max-verdi for primærprikking av personer.  Ikke-NULL verdi betyr prikking istedenfor avrunding. 
#' @param protectZeros Suppression parameter. Empty cells (count=0) are set as primary suppressed When TRUE.  
#' @param secondaryZeros Suppression parameter.
#' @param freqVar A single variable holding counts (name or number) or NULL in the case of micro data  
#' @param sector Sektor-variabel som inneholder Privat-koden (se parameter \code{"private"}). 
#' @param private Privat-koden
#' @param nace between-variabel med nace-kode eller koding (starter med) som leter etter slik variabel (`nace` kan settes til `NULL`) 
#' @param nace00 nace-koden som foretrekkes til sekundærprikking  (`nace00` kan settes til `NULL`)
#' @param frtk foretak variabel 
#' @param virk virksomhet variabel
#' @param unik unik variabel  
#' @param makeunik Unik variabel genereres ved TRUE ellers antas det at den finnes
#' @param removeZeros When TRUE, rows with zero count will be removed from the data within the algorithm. 
#'                    Default er at parameteren er motsatt av `protectZeros`. 
#'                    Altså 0-er i data fjernes når 0-er ikke skal prikkes. 
#'                    Parameteren har betydning for telling av antall foretak bak et tall. 
#' @param preAggregate Input til \code{\link{GaussSuppressionFromData}}. Parameteren er med her for testing og sammenlikning av resultater. 
#' @param output Ved avrunding kan ulike type output velges. Enten "rounded" (samme som NULL) eller "suppressed" (liste med begge hvis noe annet). 
#'               Her kan det bli endring. 
#' @param decimal Ved TRUE og når maxN er ikke-NULL kjøres \code{\link{GaussSuppressDec}}. Ekstra kolloner i output er
#' * **`freqDec`:** Heltall og syntetiske desimaltall istedenfor prikker.
#' * **`isPublish`:** Om dette er en vanlig output-celle. 
#' * **`isInner`:** Om dette er en indre celle som kan benyttes til aggregering av andre celler.                   
#'
#' @return data frame 
#' @export
#' @importFrom GaussSuppression GaussSuppressionFromData NcontributorsHolding Ncontributors GaussSuppressDec SuppressionFromDecimals
#' @importFrom SSBtools WildcardGlobbingVector SortRows RowGroups Match
#' @importFrom methods hasArg
#' @importFrom stats aggregate
#' @examples
#' 
#' prikkeVarA <- c("arb_fylke", "ARB_ARBKOMM", "nar8", "sektor")
#' prikkeVarB <- c("arb_fylke", "ARB_ARBKOMM", "nar17")
#' 
#' z <- SdcData("syssel27")
#' 
#' SdcForetakPerson(z, between = prikkeVarA)
#' SdcForetakPerson(z, between = prikkeVarA, output = "suppressed")
#' 
#' SdcForetakPerson(z, between = prikkeVarB, within = "PERS_KJOENN")
#' 
#' SdcForetakPerson(z, between = prikkeVarA, maxN = 2)
#' SdcForetakPerson(z, between = prikkeVarA, maxN = 2, decimal = TRUE)
#' SdcForetakPerson(z, between = prikkeVarB, within = "PERS_KJOENN", maxN = 2, decimal = TRUE)
#' 
#' z100 <- SdcData("syssel100")
#' out <- SdcForetakPerson(z100, between = prikkeVarB, within = c("PERS_KJOENN", "alder6"))
#' head(out)
#' tail(out)
#' 
#' # Lager data med to stataar
#' z100$stataar <- "2019"
#' z$stataar <- "2020"
#' z127 <- rbind(z100, z)
#' out127 <- SdcForetakPerson(z127, between = prikkeVarB, by = "stataar")
#' head(out127)
#' tail(out127)
SdcForetakPerson = function(data, between  = NULL, within = NULL, by = NULL, 
                            roundBase = 3, maxN = NULL,
                            protectZeros = FALSE, 
                            secondaryZeros = FALSE,
                            freqVar = NULL,
                            sector = "sektor",
                            private = "Privat",
                            nace = c("nar*", "NACE*", "nace*"), nace00="00",
                            frtk="FRTK_ID_SSB", virk="VIRK_ID_SSB", unik = "UNIK_ID", 
                            makeunik = TRUE, removeZeros = !protectZeros, preAggregate = TRUE,
                            output = NULL,
                            decimal = FALSE, 
                            freqDec = "freqDec"){
  
  if (is.data.frame(decimal)){
    dataDec <- decimal
    decimal <- FALSE
  } else {
    dataDec <- NULL
  }
  
  
  if (is.null(output)) 
    output = "rounded"
  
  if (hasArg("freqvar"))
    stop('Misspelled parameter "freqvar" found. Use "freqVar".')
  if (hasArg("roundbase"))
    stop('Misspelled parameter "roundbase" found. Use "roundBase".')
  if (hasArg("maxn"))
    stop('Misspelled parameter "maxn" found. Use "maxN".')
  
  #if(!is.null(maxN))
  #  stop("Bruk av maxN er ikke implementert")
  
  #if(sector != "sektor")
  #  stop("Bruk av sector er ikke implementert")
  
  if (length(class(data)) > 1 | class(data)[1] != "data.frame") 
    data <- as.data.frame(data)
  
  
  CheckInput(by,  type = "varNrName", data = data, okNULL = TRUE, okSeveral = TRUE)
  
  
  if(!is.null(by)){
    if(!is.null(dataDec)){
     stop("Desimal-input kombinert med by er ikke implementert")
    }
    
    if(!(output %in% c("rounded", "suppressed")))
      stop('Output must be "rounded" or "suppressed" when non-NULL "by"')
    return(KostraApply( data=data, by=by, Fun=SdcForetakPerson, 
                        between  = between , within = within, roundBase = roundBase, maxN = maxN, 
                        protectZeros = protectZeros, secondaryZeros = secondaryZeros, freqVar = freqVar,  
                        sector=sector, private = private,
                        nace = nace, nace00=nace00, frtk=frtk, virk=virk, unik =unik, makeunik =makeunik, 
                        removeZeros = removeZeros, preAggregate = preAggregate, output = output, decimal = decimal)) 
  }
  
  CheckInput(between, type = "varNrName", data = data, okNULL = TRUE, okSeveral = TRUE)
  CheckInput(within,  type = "varNrName", data = data, okNULL = TRUE, okSeveral = TRUE)
  CheckInput(roundBase,   type = "integer", min = 2, okNULL = TRUE)
  CheckInput(maxN,   type = "integer", okNULL = TRUE)
  CheckInput(freqVar,  type = "varNrName", data = data, okNULL = TRUE)
  
  if(!is.null(between)) between <- names(data[1, between, drop = FALSE])
  if(!is.null(within))  within  <- names(data[1, within, drop = FALSE])
  if(!is.null(freqVar)) freqVar <- names(data[1, freqVar, drop = FALSE])
  
  if (removeZeros & !is.null(freqVar)) 
    data <- data[data[[freqVar]] != 0, , drop = FALSE]
  
  # nace brukes bare sammen med nace00
  if (length(nace00) == 0) nace <- NULL
  
  
  if(is.null(freqVar)){
    freqVar <- "sySseLsaTte" # Kun variabelnavn som brukes internt
    data$sySseLsaTte <- 1L
  }
  
  
  alleVar <- c(between , within)
  
  supData <- NULL
  prikkData <- NULL
  
  if(length(between )>0){
    
    CheckInput(sector,  type = "varNrName", data = data, okNULL = TRUE)
    CheckInput(frtk,  type = "varNrName", data = data, okNULL = TRUE)
    CheckInput(virk,  type = "varNrName", data = data, okNULL = TRUE)
    if(!makeunik) CheckInput(unik,  type = "varNrName", data = data, okNULL = TRUE)
    
    
    data <- Make_FRTK_VIRK_UNIK_AggVar(data, frtk=frtk, virk=virk, unik =unik, varnames = c("FRTK_VIRK_UNIK", NA, NA, NA, NA, NA, NA))
    
    if (!is.null(nace)) {
      if(length(nace)){
        nace <- WildcardGlobbingVector(names(data[1, between, drop=FALSE]), nace)
        if(length(nace)==0){
          warning("Ingen nace-variabel funnet")
        }
        if(length(nace)>1){
          stop("nace-variabel ikke unikt spesifisert")
        }
      }
    }
    
    if(length(nace)==0){
      nace = NULL
    }
    
    
    if (!is.null(nace)) {
      data$narWeight <- Make_NarWeight_00(data, nace, nace00)
    } else {
      data$narWeight <- 1L
    }
    
    if(is.null(maxN)){
      if(is.null(dataDec)){
        if(decimal){
          a <-         GaussSuppressDec(data, dimVar = between , freqVar = freqVar, 
                                                charVar = c(sector, "FRTK_VIRK_UNIK"), 
                                                weightVar = "narWeight", protectZeros = protectZeros, maxN = -1, 
                                                secondaryZeros = secondaryZeros,
                                                primary = Primary_FRTK_VIRK_UNIK_sektor, 
                                                singleton = NULL, singletonMethod = "none", preAggregate = preAggregate,
                                                sector = sector, private = private, output = "both")
          dimVarOut <- between[between %in% names(a$publish)]
          ma <- Match(a$publish[dimVarOut], a$inner[dimVarOut])
          prikkData <- cbind(a$inner[ma[!is.na(ma)], between, drop = FALSE], 
                             a$publish[!is.na(ma), !(names(a$publish) %in% c(between, "weight", "primary")), drop = FALSE])
          names(prikkData)[names(prikkData) == "suppressed"] <- "prikk"
          prikkData$prikk <- as.integer(prikkData$prikk)
          rownames(prikkData) <- NULL
          return(prikkData)
        } else {
          prikkData <- GaussSuppressionFromData(data, dimVar = between , freqVar = freqVar, 
                                                charVar = c(sector, "FRTK_VIRK_UNIK"), 
                                                weightVar = "narWeight", protectZeros = protectZeros, maxN = -1, 
                                                secondaryZeros = secondaryZeros,
                                                primary = Primary_FRTK_VIRK_UNIK_sektor, 
                                                singleton = NULL, singletonMethod = "none", preAggregate = preAggregate,
                                                sector = sector, private = private)
        }
        
        if(output == "suppressed"){
          prikkData$prikk <- as.integer(prikkData$suppressed)
          return(prikkData)
        }
        
        # Lager prikkede kombinasjoner
        supData <- GaussSuppressed(prikkData, between )
      } else {
        notInDataDec <- between[!(between %in% names(dataDec))]
        if(length(notInDataDec)){
          stop(paste("Mangler i decimal: ",paste(notInDataDec, collapse = ", ")))
        }
        
        if(length(freqVar)){
          uniqueBetween <-  RowGroups(data[data[[freqVar]]>0, between, drop=FALSE], TRUE)$groups 
        } else {
          uniqueBetween <-  RowGroups(data[between], TRUE)$groups 
        }
        ma<- Match(uniqueBetween, dataDec[between])
        if(anyNA(ma)){
          print(uniqueBetween[head(which(is.na(ma))), ,drop=FALSE])
          stop("Finner ikke matchende rader i decimal")
        }
        prikkData <- SuppressionFromDecimals(dataDec, dimVar = between , freqVar = "freq", 
                                              decVar = "freqDec")
        if(output == "suppressed"){
          prikkData$prikk <- as.integer(prikkData$suppressed)
          return(prikkData)
        }
        
        # Lager prikkede kombinasjoner
        supData <- GaussSuppressed(prikkData, between )
      }
    }  
  } # if(length(between )>0){
  
  if(output == "suppressed") return(prikkData)
  
  
  if(!is.null(maxN)){
    if(decimal){
      if(length(between )>0){
        prikkData <- GaussSuppressDec(data, dimVar = alleVar, freqVar = freqVar, 
                                              charVar = c(sector, "FRTK_VIRK_UNIK"), 
                                              weightVar = "narWeight", protectZeros = protectZeros, maxN = maxN,
                                              secondaryZeros = secondaryZeros,
                                              primary = Primary_FRTK_VIRK_UNIK_sektor, # singleton = NULL, singletonMethod = "none", 
                                              preAggregate = preAggregate,
                                              sector = sector, private = private, between = between)
      } else {
        prikkData <- GaussSuppressDec(data, dimVar = alleVar, freqVar = freqVar, 
                                              protectZeros = protectZeros, maxN = maxN, 
                                              secondaryZeros = secondaryZeros,
                                              preAggregate = preAggregate)
      }
      
      
      ############################################
      # Endring foreløpig output til å være lik tidligere ArbForhold/Lonnstaker 
      ############################################
      # Endrer fra TRUE/FALSE til 0/1
      prikkData$primary <- as.integer(prikkData$primary)
      prikkData$suppressed <- as.integer(prikkData$suppressed)
      # Tar bort weight og inn med prikket på samme plass
      names(prikkData)[names(prikkData) == "weight"] <- "prikket"
      prikkData$prikket <- prikkData$freq
      prikkData$prikket[prikkData$suppressed==1] <- NA
      
      # Endrer mer fra TRUE/FALSE til 0/1 for å være konsekvent 
      prikkData$isPublish <- as.integer(prikkData$isPublish)
      prikkData$isInner <- as.integer(prikkData$isInner)
      
      rownames(prikkData) <- NULL
      return(prikkData)
    }
    if(length(between )>0){
      prikkData <- GaussSuppressionFromData(data, dimVar = alleVar, freqVar = freqVar, 
                                            charVar = c(sector, "FRTK_VIRK_UNIK"), 
                                            weightVar = "narWeight", protectZeros = protectZeros, maxN = maxN,
                                            secondaryZeros = secondaryZeros,
                                            primary = Primary_FRTK_VIRK_UNIK_sektor, # singleton = NULL, singletonMethod = "none", 
                                            preAggregate = preAggregate,
                                            sector = sector, private = private, between = between)
    } else {
      prikkData <- GaussSuppressionFromData(data, dimVar = alleVar, freqVar = freqVar, 
                                            protectZeros = protectZeros, maxN = maxN, 
                                            secondaryZeros = secondaryZeros,
                                            preAggregate = preAggregate)
    }

    ############################################
    # Endring foreløpig output til å være lik tidligere ArbForhold/Lonnstaker 
    ############################################
    # Endrer fra TRUE/FALSE til 0/1
    prikkData$primary <- as.integer(prikkData$primary)
    prikkData$suppressed <- as.integer(prikkData$suppressed)
    # Tar bort weight og inn med prikket på samme plass
    names(prikkData)[names(prikkData) == "weight"] <- "prikket"
    prikkData$prikket <- prikkData$freq
    prikkData$prikket[prikkData$suppressed==1] <- NA
    
    
    return(prikkData)
  }
  
  
  aggData <- aggregate(data[, freqVar, drop = FALSE], data[, alleVar], sum)
  
  
  prsData <- PLSroundingSuppressed(aggData, freqVar, dataSuppressed = supData, roundBase = roundBase)
  
  prsData <- prsData[, !(names(prsData) %in% "nCells")]
  
  if(output == "rounded")
    return(prsData)
  
  list( rounded = prsData, suppressed = prikkData)
}
  