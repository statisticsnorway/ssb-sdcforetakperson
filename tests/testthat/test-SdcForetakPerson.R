

SF <- function(... , Fun = SdcForetakPerson) {  # check sum
  co <- capture.output({z <- Fun(...)})
  z <- strtoi(as.character(as.matrix(z)),36L)  %% 1000000
  z <- as.numeric(c(z, order(z)))
  sum(z * (seq_len(length(z)) %%99), na.rm=TRUE) 
}

prikkeVarA <- c("arb_fylke", "ARB_ARBKOMM", "nar8", "sektor")
prikkeVarB <- c("arb_fylke", "ARB_ARBKOMM", "nar17")

z100 <- SdcData("syssel100")


test_that("SdcForetakPerson works", {
  expect_identical(SF(z100, between = prikkeVarB, within = c("PERS_KJOENN", "alder6"), nace00="85", allowTotal = FALSE),  41193589017) 
  expect_identical(SF(z100, between = prikkeVarB, within = c("PERS_KJOENN", "alder6"), nace00="85"),  40943387364) 
  expect_identical(SF(z100, between = prikkeVarB, within = c("PERS_KJOENN", "alder6"), allowTotal = FALSE), 40428997551) 
  expect_identical(SF(z100, between = prikkeVarB, within = c("PERS_KJOENN", "alder6")), 40194581165)
  expect_identical(SF(z100, between = prikkeVarA, output = "suppressed"), 13515336550) 
  expect_identical(SF(z100, between = prikkeVarA), 5895185552) 
})

z <- Make_FRTK_VIRK_UNIK_AggVar(z100, varnames = c("FRTK_VIRK_UNIK", NA, NA, NA, NA, NA, NA))
z$narWeight = SdcForetakPerson:::Make_NarWeight_00(z, "nar8", "85")

z$ArbForhold = 1
z$Lonnstaker <- c(1,1,0,1,1)


test_that("KOde for ArbForhold og Lonnstaker", {
  GF = function(...) SF(..., Fun = Suppression578)
  # Funksjon som på mail, men med flere input-parametere 
  Suppression578 <- function(data, dimVar, freqVar,  weightVar = "narWeight", protectZeros = FALSE, maxN = 2) {
    prikk <- GaussSuppression::GaussSuppressionFromData(data = data, dimVar = dimVar, freqVar = freqVar, 
                                      charVar = c("sektor", "FRTK_VIRK_UNIK"), weightVar = weightVar, protectZeros = protectZeros, 
                                      maxN = maxN, primary = SdcForetakPerson:::Primary_FRTK_VIRK_UNIK_sektor, preAggregate = TRUE)
    
    # Endrer fra TRUE/FALSE til 0/1
    prikk$primary <- as.integer(prikk$primary)
    prikk$suppressed <- as.integer(prikk$suppressed)
    
    # Tar bort weight og inn med prikket på samme plass
    names(prikk)[names(prikk) == "weight"] <- "prikket"
    prikk$prikket <- prikk$freq
    prikk$prikket[prikk$suppressed==1] <- NA
    prikk
  }
  
  expect_identical(GF(z, prikkeVarB , "ArbForhold", maxN = -1), 343700515)
  expect_identical(GF(z, prikkeVarB , "ArbForhold", maxN = 5), 343710393)
  expect_identical(GF(z, prikkeVarB , "Lonnstaker", maxN = -1), 343527078)
  expect_identical(GF(z, prikkeVarB , "Lonnstaker", maxN = 5), 343533630)
  expect_identical(GF(z, prikkeVarB , "ArbForhold", maxN = -1, weightVar =NULL), 343693162)
  expect_identical(suppressWarnings(GF(z, prikkeVarB , "ArbForhold", maxN = 5, protectZeros =TRUE)), 343701795)
})



test_that("Med ren GaussSuppressionFromData", {
  GD = function(...) SF(..., Fun = GaussSuppression::GaussSuppressionFromData)
  
  expect_identical(GD(z, prikkeVarA, "Lonnstaker",   weightVar = "narWeight", protectZeros = FALSE, maxN = 2), 13042550120)
  expect_identical(suppressWarnings(GD(z, prikkeVarA, "Lonnstaker",   weightVar = "narWeight", protectZeros = TRUE, maxN = 2)), 12106514632)
  expect_identical(suppressWarnings(GD(z, prikkeVarA, "Lonnstaker", protectZeros = TRUE, maxN = 5)), 11744811938)
  expect_identical(GD(z, prikkeVarA, "Lonnstaker",   weightVar = "narWeight", protectZeros = FALSE, maxN = 2, singleton = NULL), 13418947051)
  expect_identical(GD(z, prikkeVarA, "Lonnstaker",   weightVar = "narWeight", protectZeros = FALSE, maxN = 2, singletonMethod = "none"), 13418947051)
  
})


test_that("SdcForetakPerson med decimal", {

  GD <- SdcForetakPersonDecimalTest
  expect_true(GD(z, prikkeVarA, freqVar = "Lonnstaker", nace00 = "85", protectZeros = FALSE, maxN = 2))
  expect_true(GD(z, prikkeVarA, freqVar = "Lonnstaker", nace00 = "85", protectZeros = TRUE, maxN = 2))
  expect_true(GD(z, prikkeVarA, freqVar = "Lonnstaker", protectZeros = FALSE, maxN = 5))
  
  expect_true(GD(z, prikkeVarA, freqVar = "Lonnstaker", nace00 = "85", protectZeros = FALSE))
  expect_true(GD(z, prikkeVarA, freqVar = "Lonnstaker", nace00 = "85", protectZeros = TRUE))
  expect_true(GD(z, prikkeVarA, freqVar = "Lonnstaker", protectZeros = FALSE))
  
  expect_true(GD(z100, between = prikkeVarB, within = c("PERS_KJOENN", "alder6"), nace00="85")) 
  expect_true(GD(z100, between = prikkeVarB, within = c("PERS_KJOENN", "alder6"))) 

})


test_that("SdcForetakPerson med decimal og allowTotal", {
  # secondaryZeros is trick to avoid a singleton-method 
  z100 <- SdcData("syssel100")
  GD <- SdcForetakPersonDecimalTest
  expect_true(GD(z, between = prikkeVarB, maxN = -1, within = c("PERS_KJOENN", "alder6"), allowTotal = TRUE, secondaryZeros = TRUE))
  expect_true(GD(z, between = prikkeVarB, maxN = -1, within = c("PERS_KJOENN", "alder6"), allowTotal = FALSE, secondaryZeros = TRUE))
  aT <- SdcForetakPerson(z, between = prikkeVarB, maxN = -1, within = c("PERS_KJOENN", "alder6"), allowTotal = TRUE, secondaryZeros = TRUE)
  aF <- SdcForetakPerson(z, between = prikkeVarB, maxN = -1, within = c("PERS_KJOENN", "alder6"), allowTotal = FALSE, secondaryZeros = TRUE)
  expect_true(all.equal(as.vector(unlist(aF[aF$primary != aT$primary, c("PERS_KJOENN", "alder6")])), rep("Total", 6)))
  expect_true(sum(aF$suppressed != aT$suppressed) > 0)
})  





