# First column with missing values replaced

The first column is returned with missing values sequentially replaced
with values from the other columns

## Usage

``` r
MissingReplaceFirst(x, missing = NULL)
```

## Arguments

- x:

  data.frame

- missing:

  Values treated as missing

## Value

First column with missing values replaced

## Examples

``` r
a <- data.frame(x = c("yes", "", "no", ""), y = c("", "ok", "ok", ""), z = "z")
print(a)
#>     x  y z
#> 1 yes    z
#> 2     ok z
#> 3  no ok z
#> 4        z
MissingReplaceFirst(a)
#> [1] "yes" ""    "no"  ""   
MissingReplaceFirst(a, "")
#> [1] "yes" "ok"  "no"  "z"  
MissingReplaceFirst(a, c("", "yes"))
#> [1] "z"  "ok" "no" "z" 
a[3, 1] <- NA
MissingReplaceFirst(a)
#> [1] "yes" ""    "ok"  ""   
```
