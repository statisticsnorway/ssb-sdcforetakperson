# Function that returns a dataset

Function that returns a dataset

## Usage

``` r
SdcData(dataset)
```

## Arguments

- dataset:

  Name of data set within the SdcForetakPerson package

## Value

data frame

## Author

Øyvind Langsrud

## Examples

``` r
SdcData("syssel7")
#>    nar8 nar17 alder6 VIRK_ID_SSB FRTK_ID_SSB
#> 1 45-82 58-63  25-39   990000054   880000035
#> 2 45-82 49-53  55-66   990000021   880000043
#> 3 45-82 49-53  40-54                        
#> 4 45-82 49-53  20-24   990000021   880000043
#> 5    85    85  55-66   990000022   880000035
#> 6    85    85  25-39   990000014            
#> 7    85    85  55-66   990000024   880000014
z <- SdcData("syssel100")
a10 <- SdcData("sdc_sample_10")
a1000 <- SdcData("sdc_sample_1000")
```
