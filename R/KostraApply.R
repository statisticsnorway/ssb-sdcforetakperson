
#' Apply a function with data frame output over data subsets
#' 
#' Funksjonen er kopiert fra Kostra-pakken
#'
#' @param data Data frame
#' @param by Vector of variable names (or variable numbers) defining the subsets or alternatively a named list or data frame (see examples).
#' @param Fun Function to be applied on data subsets
#' @param ... Arguments passed to Fun
#'
#' @return A data.frame
#' @export
#' @importFrom SSBtools RowGroups RbindAll
#'
#' @note The output ordering of the example depends on default.stringsAsFactors()
#'
#' @examples
#' 1+1
#'
KostraApply <- function( data, by, Fun, ...){
  if(is.list(by)){
    byList <- by
    by <- names(by)
  }
  else
    byList <- NULL
  rg <- RowGroups(data[,by,drop=FALSE],TRUE)
  n  <- max(rg$idx)
  okGroup <- rep(TRUE,n)
  if(!is.null(byList)){
    if(is.data.frame(byList))
      okGroup = !is.na(Match(rg$groups,byList))
    else
      for(i in seq_len(length(by))){
        if(!is.null(byList[[i]])) okGroup[!(rg$groups[,by[i]] %in% byList[[i]])] = FALSE
      }
  }
  n  <- sum(okGroup)
  wok <- which(okGroup)
  a <- vector("list",n)
  for(i in seq_len(n))
    a[[i]] = cbind(rg$groups[wok[i],,drop=FALSE],Fun(data=data[rg$idx==wok[i], ,drop=FALSE],...),row.names = NULL)
  RbindAll(a)
}

