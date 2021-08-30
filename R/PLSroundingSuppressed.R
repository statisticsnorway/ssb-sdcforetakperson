

#' Rounding after suppression
#' 
#' Within \code{\link{PLSrounding}} within the current function, the suppressed combinations are not treated as publishable and 
#' are not in output from that function, but they are in output from the current function as suppressed cells.
#'
#' @param data 	   Input data as a data frame (inner cells)
#' @param freqVar  Variable holding counts (inner cells frequencies)
#' @param dataSuppressed Data frame specifying combinations to be suppressed
#' @param hierarchies List of hierarchies
#' @param formula A model formula
#' @param numVar  Other numerical variables to be aggregated 
#' @param output If not "data.frame" (default), more content in output
#' @param printPLSrounded When TRUE, output from print \code{\link{PLSrounding}} within the function is printed
#' @param rndSeed If non-NULL, a random generator seed to be used locally within the function without affecting the random value stream in R. 
#' @param ... 	 Further parameters sent to \code{\link{PLSrounding}} within the function is printed 
#'
#' @return By default, a data frame with aggregates and rounded frequencies with suppression information 
#' @export   
#' @importFrom SSBtools ModelMatrix Match
#' @importFrom SmallCountRounding PLSrounding 
#' @importFrom Matrix colSums
#' @importFrom stats runif
#'
#' @examples
#' 
#' z1 <- SmallCountRounding::SmallCountData("z1")
#' sup <- GaussSuppressionFromData(z1, 1:2, 3)
#' sup <- sup[sup$suppressed, !(names(sup) %in% c("freq", "primary", "suppressed"))]
#' a <- PLSroundingSuppressed(z1, "ant", sup, roundBase = 20)
#' aNoSuppression <- PLSroundingSuppressed(z1, "ant", roundBase = 20)
#' 
#' z3 <- SmallCountRounding::SmallCountData("z3")
#' sup <- GaussSuppressionFromData(z3, c(1, 2, 4), 7)
#' sup <- sup[sup$suppressed, !(names(sup) %in% c("freq", "primary", "suppressed"))]
#' b <- PLSroundingSuppressed(z3[, -c(3, 6)], "ant", sup, roundBase = 3)
#' 
#' if (require(easySdcTable)) {
#'   sup <- ProtectTable(EasyData("z1"), 1:2, 3, method = "Gauss")$data
#'   sup <- sup[is.na(sup$suppressed), !(names(sup) %in% c("freq", "sdcStatus", "suppressed"))]
#'   a_ <- PLSroundingSuppressed(z1, "ant", sup, roundBase = 20)
#'   print(identical(a, a_))
#'   
#'   sup <- ProtectTable(EasyData("z3"), c(1, 2, 4), 7, method = "Gauss")$data
#'   sup <- sup[is.na(sup$suppressed), !(names(sup) %in% c("freq", "sdcStatus", "suppressed"))]
#'   b_ <- PLSroundingSuppressed(z3[, -c(3, 6)], "ant", sup, roundBase = 3)
#'   print(identical(b, b_))
#' }
#' 
PLSroundingSuppressed = function(data, freqVar, dataSuppressed = NULL, hierarchies = NULL, formula = NULL, numVar = NULL,  
                                 output = "data.frame", printPLSrounded = FALSE, rndSeed = 123, ...){ #  
  

  if (!is.null(rndSeed)) {       # Move to PLSrounding
    if (!exists(".Random.seed")) 
      if (runif(1) < 0) 
        stop("Now seed exists")
    exitSeed <- .Random.seed
    on.exit(.Random.seed <<- exitSeed)
    set.seed(rndSeed)
  }
      
      
  freqVar  = names(data[1, freqVar, drop=FALSE])
  numVar   = names(data[1, numVar, drop=FALSE])
  
  a = SSBtools::ModelMatrix(data[, !(names(data) %in% c(freqVar, numVar)), drop=FALSE], 
                            hierarchies = hierarchies, formula = formula, crossTable = TRUE) 
  
  
  ma0 = Match(dataSuppressed, a$crossTable[, names(dataSuppressed), drop=FALSE])
  
  if(anyNA(ma0)){
    warning("NA when matching dataSuppressed") 
  }
  
  if(!is.null(dataSuppressed)){
    ok = is.na(Match(a$crossTable[, names(dataSuppressed), drop=FALSE], dataSuppressed))
  } else {
    ok = rep(TRUE, nrow(a$crossTable))
  }
  
  b = PLSrounding(data, freqVar= freqVar, hierarchies = NULL,formula = NULL,
                  x = a$modelMatrix[, ok], crossTable=FALSE, ...)
  
  if (printPLSrounded){
    print(b)
  }
  
  out <- NULL
  
  cNames <- colnames(data)[colnames(data) %in% colnames(a$crossTable)]
  
  if (output != "data.frame")
  out$inner <- cbind( data[, cNames, drop = FALSE], b$inner)
  
  out$publish <- cbind(as.data.frame(a$crossTable[, cNames, drop = FALSE], stringsAsFactors = FALSE), 
                       as.matrix(crossprod(a$modelMatrix, as.matrix(b$inner))))
  out$publish$roundedSuppressed <- out$publish$rounded
  out$publish$roundedSuppressed[!ok] = NA
  
  out$publish$nCells=colSums(a$modelMatrix)
  
  if (!is.null(numVar)) {
    out$publish <- cbind(out$publish, as.data.frame(as.matrix(crossprod(a$modelMatrix, as.matrix(data[, numVar, drop = FALSE])))))
  }
  
  
  rownames(out$publish) <- NULL
  
  if (output == "data.frame")
    return(out$publish)
  
  out$pLSrounding <- b
  
  out
}
