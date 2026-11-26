# Navn i data som er matchende etter [`tolower`](https://rdrr.io/r/base/chartr.html)

Navn i data som er matchende etter
[`tolower`](https://rdrr.io/r/base/chartr.html)

## Usage

``` r
lower_match(x, code)
```

## Arguments

- x:

  Data eller bare vektor med navn

- code:

  Kode det skal matches mot

## Value

Som input-`code`, eventuelt endret p.g.a. første match som ble funnet.

## Examples

``` r
z <- SdcData("syssel27")
lower_match(z, "frtk_id_ssb")
#> [1] "FRTK_ID_SSB"
lower_match(z, "frtk_ID_ssb")
#> [1] "FRTK_ID_SSB"
lower_match(z, "ALDER")  # ingen match funnet 
#> [1] "ALDER"
lower_match(z, "ALDER6")
#> [1] "alder6"
lower_match(z, "Sektor")
#> [1] "sektor"
```
