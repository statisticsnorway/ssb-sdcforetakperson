# Genererer variabel som er foretak-ID dersom den finnes, ellers virksomhet-ID, eller en unik ID.

Genererer variabel som er foretak-ID dersom den finnes, ellers
virksomhet-ID, eller en unik ID.

## Usage

``` r
Make_FRTK_VIRK_UNIK_AggVar(
  data,
  frtk = "FRTK_ID_SSB",
  virk = "VIRK_ID_SSB",
  unik = "UNIK_ID",
  makeunik = TRUE,
  varnames = c("FRTK_VIRK_UNIK", "sysselsatte", "sysselsatteVIRK", "sysselsatteFRTK",
    "antVIRK", "antFRTK", "ant_FRTK_VIRK_UNIK"),
  miss = "",
  dupvar = NULL
)
```

## Arguments

- data:

  data frame

- frtk:

  foretak variabel

- virk:

  virksomhet variabel

- unik:

  unik variabel

- makeunik:

  Unik variabel genereres ved TRUE eller antas det at den finnes

- varnames:

  Vektor av 7 variabelnavn. Bruk NA dersom variabel ikke ønskes i
  output.

- miss:

  Kode for missing

- dupvar:

  Variabler som definerer grupper

## Value

input data med nye variabler

## Examples

``` r
z <- SdcData("syssel7")
Make_FRTK_VIRK_UNIK_AggVar(z)
#>    nar8 nar17 alder6 VIRK_ID_SSB FRTK_ID_SSB     UNIK_ID FRTK_VIRK_UNIK
#> 1 45-82 58-63  25-39   990000054   880000035 UNIK0000001      880000035
#> 2 45-82 49-53  55-66   990000021   880000043 UNIK0000002      880000043
#> 3 45-82 49-53  40-54                         UNIK0000003    UNIK0000003
#> 4 45-82 49-53  20-24   990000021   880000043 UNIK0000004      880000043
#> 5    85    85  55-66   990000022   880000035 UNIK0000005      880000035
#> 6    85    85  25-39   990000014             UNIK0000006      990000014
#> 7    85    85  55-66   990000024   880000014 UNIK0000007      880000014
#>   sysselsatte sysselsatteVIRK sysselsatteFRTK antVIRK antFRTK
#> 1           1               1               1       1       1
#> 2           1               1               1       1       1
#> 3           1               0               0       0       0
#> 4           1               1               1       1       1
#> 5           1               1               1       1       1
#> 6           1               1               0       1       0
#> 7           1               1               1       1       1
#>   ant_FRTK_VIRK_UNIK
#> 1                  1
#> 2                  1
#> 3                  1
#> 4                  1
#> 5                  1
#> 6                  1
#> 7                  1
Make_FRTK_VIRK_UNIK_AggVar(z, 
    varnames = c("FRTK_VIRK_UNIK", "sysselsatte", NA, NA, NA, NA, "ant_FRTK_VIRK_UNIK"))
#>    nar8 nar17 alder6 VIRK_ID_SSB FRTK_ID_SSB     UNIK_ID FRTK_VIRK_UNIK
#> 1 45-82 58-63  25-39   990000054   880000035 UNIK0000001      880000035
#> 2 45-82 49-53  55-66   990000021   880000043 UNIK0000002      880000043
#> 3 45-82 49-53  40-54                         UNIK0000003    UNIK0000003
#> 4 45-82 49-53  20-24   990000021   880000043 UNIK0000004      880000043
#> 5    85    85  55-66   990000022   880000035 UNIK0000005      880000035
#> 6    85    85  25-39   990000014             UNIK0000006      990000014
#> 7    85    85  55-66   990000024   880000014 UNIK0000007      880000014
#>   sysselsatte ant_FRTK_VIRK_UNIK
#> 1           1                  1
#> 2           1                  1
#> 3           1                  1
#> 4           1                  1
#> 5           1                  1
#> 6           1                  1
#> 7           1                  1
z$id <- 1:7
Make_FRTK_VIRK_UNIK_AggVar(z, unik = "id", makeunik = FALSE)
#>    nar8 nar17 alder6 VIRK_ID_SSB FRTK_ID_SSB id FRTK_VIRK_UNIK sysselsatte
#> 1 45-82 58-63  25-39   990000054   880000035  1      880000035           1
#> 2 45-82 49-53  55-66   990000021   880000043  2      880000043           1
#> 3 45-82 49-53  40-54                          3              3           1
#> 4 45-82 49-53  20-24   990000021   880000043  4      880000043           1
#> 5    85    85  55-66   990000022   880000035  5      880000035           1
#> 6    85    85  25-39   990000014              6      990000014           1
#> 7    85    85  55-66   990000024   880000014  7      880000014           1
#>   sysselsatteVIRK sysselsatteFRTK antVIRK antFRTK ant_FRTK_VIRK_UNIK
#> 1               1               1       1       1                  1
#> 2               1               1       1       1                  1
#> 3               0               0       0       0                  1
#> 4               1               1       1       1                  1
#> 5               1               1       1       1                  1
#> 6               1               0       1       0                  1
#> 7               1               1       1       1                  1
```
