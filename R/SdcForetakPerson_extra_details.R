#' Ekstra SdcForetakPerson-detaljer
#' 
#' Ekstra `SdcForetakPerson`-detaljer. 
#'
#' @details
#' 
#' Nedenfor er eksempel som viser kode for doble nace-koder med og uten bruk 
#' av parameteren `linkedGauss` (se parameterbeskrivelse: [SdcForetakPerson()]).
#' 
#' Når linkedGauss brukes (annet enn `FALSE`), blir det produsert dobbelt sett med desimaltall. 
#' Det betyr at når man bruker desimaltall som input, 
#' må det spesifiseres hvilket sett med desimaltall som kan brukes. 
#' Det enkleste er å bruke parameteren `linked_table` som da settes til `1` eller `2`. 
#' Ved ikke-standard navn på desimaltallvariablene må man isteden bruke parameteren `freqDec`. 
#' 
#' Merk at det er mulig å gjøre feil ved at man f.eks- spesifiserer `linked_table` til feil verdi. 
#' Prikkingen blir da fullstendig feil. 
#' En mulig fremtidig løsning som kan hindre dette, er å lagre to desimaltallfiler med litt 
#' færre variabler i hver fil. 
#' 
#' Merk at det også går an å sette linkedGauss til annet enn TRUE:
#' * **`"super-consistent"`:** Samme som `TRUE`.
#' * **`"consistent"`:** En litt annen metode.  
#' * **`"global"`:** Skal gi samme svar som vanlig metode (`linkedGauss = FALSE`), 
#' men dobbelt sett av desimaltall produseres. 
#' 
#' @examples
#' a <- SdcData("sdc_sample_25000")
#' 
#' prikkeVarA_1 <- c("arb_fylke", "ARB_ARBKOMM", "SN2007_2", "sektor")
#' prikkeVarB_1 <- c("arb_fylke", "ARB_ARBKOMM", "SN2007_3")
#' prikkeVarA_2 <- c("arb_fylke", "ARB_ARBKOMM", "SN2025_2", "sektor")
#' prikkeVarB_2 <- c("arb_fylke", "ARB_ARBKOMM", "SN2025_3")
#' 
#' 
#' 
#' #### vanlig metode ####
#' decimal <- SdcForetakPerson(a, 
#'   between = ~(arb_fylke + ARB_ARBKOMM) * nar2_begge * sektor + (arb_fylke + ARB_ARBKOMM) * nar3_begge, 
#'   decimal = TRUE, 
#'   double_vars = list(nar2_begge = c("SN2007_2", "SN2025_2"), 
#'   nar3_begge = c("SN2007_3", "SN2025_3")), 
#'   use_diff_groups = TRUE, 
#'   double_priority = 1, 
#'   nace = "SN2007_2")
#' 
#' outA_1 <- SdcForetakPerson(a, between = prikkeVarA_1, decimal = decimal)
#' outB_1 <- SdcForetakPerson(a, between = prikkeVarB_1, within = "PERS_KJOENN", decimal = decimal)
#' outA_2 <- SdcForetakPerson(a, between = prikkeVarA_2, decimal = decimal)
#' outB_2 <- SdcForetakPerson(a, between = prikkeVarB_2, within = "PERS_KJOENN", decimal = decimal)
#' 
#' 
#' 
#' #### linkedGauss = TRUE ####
#' decimal_ <- SdcForetakPerson(a, 
#'   between = ~(arb_fylke + ARB_ARBKOMM) * nar2_begge * sektor + (arb_fylke + ARB_ARBKOMM) * nar3_begge, 
#'   decimal = TRUE, 
#'   double_vars = list(nar2_begge = c("SN2007_2", "SN2025_2"), 
#'   nar3_begge = c("SN2007_3", "SN2025_3")), 
#'   use_diff_groups = TRUE, 
#'   double_priority = 1, 
#'   nace = "SN2007_2", 
#'   linkedGauss = TRUE)
#' 
#' outA_1_ <- SdcForetakPerson(a, between = prikkeVarA_1, decimal = decimal_, linked_table = 1)
#' outB_1_ <- SdcForetakPerson(a, between = prikkeVarB_1, within = "PERS_KJOENN", decimal = decimal_, linked_table = 1)
#' outA_2_ <- SdcForetakPerson(a, between = prikkeVarA_2, decimal = decimal_, linked_table = 2)
#' outB_2_ <- SdcForetakPerson(a, between = prikkeVarB_2, within = "PERS_KJOENN", decimal = decimal_, linked_table = 2)
#' 
#' 
#' 
#' #### antall prikker #### 
#' 
#' # ved vanlig metode
#' sum(is.na(outA_1$roundedSuppressed)) + sum(is.na(outB_1$roundedSuppressed))
#' sum(is.na(outA_2$roundedSuppressed)) + sum(is.na(outB_2$roundedSuppressed))
#' 
#' # ved linkedGauss = TRUE
#' sum(is.na(outA_1_$roundedSuppressed)) + sum(is.na(outB_1_$roundedSuppressed))
#' sum(is.na(outA_2_$roundedSuppressed)) + sum(is.na(outB_2_$roundedSuppressed))
#'
#' @name SdcForetakPerson_extra_details
NULL
