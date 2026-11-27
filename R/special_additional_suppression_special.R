

# Modifisert versjon av GaussSuppression:::special_additional_suppression

special_additional_suppression_special <- function(data, ..., 
                                           suppressedData) {
  
  if (!is.data.frame(suppressedData)) {  # empty list as NULL
    if (is.list(suppressedData))
      if (!length(suppressedData)) {
        suppressedData <- NULL
      }
  }
  
  
  GaussSuppressDec(data = data, ..., 
                   fun = GaussSuppressionFromData_special,    
                   hidden = GaussSuppression::PrimaryFromSuppressedData, 
                   forced = GaussSuppression::ForcedFromSuppressedData, 
                   suppressedData = suppressedData)
  

}


GaussSuppressionFromData_special <- function(...) {
  out <- GaussSuppressionFromData(...)
  out$publish$suppressed[is.na(out$publish$suppressed)] <- TRUE
  
  out
}