# Test av SdcForetakPerson med decimal

Om det blir samme svar via `decimal = TRUE`

## Usage

``` r
SdcForetakPersonDecimalTest(data, data2, ..., stopWhenFALSE = TRUE)
```

## Arguments

- data:

  vanlige data

- data2:

  data brukt når decimal er input (noen variabler kan fjernes)

- ...:

  SdcForetakPerson-parametere

- stopWhenFALSE:

  Feiler ved FALSE når TRUE

## Value

logical

## Examples

``` r
prikkeVarA <- c("arb_fylke", "ARB_ARBKOMM", "nar8", "sektor")
z <- SdcData("syssel27")
SdcForetakPersonDecimalTest(z, z[1:7], between = prikkeVarA)
#> [preAggregate 27*14->21*7]
#> [extraAggregate 21*7->10*7] Checking .....
#> GaussSuppression_none: ..............................
#> [aggregate for PLSroundingSuppressed 27*14->10*5]
#> [preAggregate 27*14->21*7]
#> [extraAggregate 21*7->10*7] Checking .....
#> GaussSuppression_none: ..............................
#> [preAggregate 10*11->10*8]
#> GaussSuppression_none: 
#> [aggregate for PLSroundingSuppressed 27*8->10*5]
#> [1] TRUE
```
