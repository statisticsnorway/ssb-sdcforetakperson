# Ones to be aggregated to counts

Based on a variable in the data, missing and duplicates within groups
are set to zero.

## Usage

``` r
Ones2agg(data, var, miss = NULL, dupvar = NULL, int = TRUE)
```

## Arguments

- data:

  A data frame

- var:

  A variable in data

- miss:

  Values treated as missing

- dupvar:

  variables defining groups (see details)

- int:

  Integers returned when TRUE

## Value

Vector of ones and zeros

## Details

To treat duplicates without grouping set `dupvar` to the same value as
`var`. Also `character(0)` or `integer(0)` will work when `var` is
specified by name or number, respectively.

## Examples

``` r
a <- data.frame(x = rep(c("A", "B", "C", ""), each = 2), 
                y = c(5, 5, 5, 6, 6, 6, 7, 7), 
                z = c("r", rep("s", 7)))
cbind(a, ones = Ones2agg(a, "x"))
#>   x y z ones
#> 1 A 5 r    1
#> 2 A 5 s    1
#> 3 B 5 s    1
#> 4 B 6 s    1
#> 5 C 6 s    1
#> 6 C 6 s    1
#> 7   7 s    1
#> 8   7 s    1
cbind(a, ones = Ones2agg(a, "x", ""))
#>   x y z ones
#> 1 A 5 r    1
#> 2 A 5 s    1
#> 3 B 5 s    1
#> 4 B 6 s    1
#> 5 C 6 s    1
#> 6 C 6 s    1
#> 7   7 s    0
#> 8   7 s    0
cbind(a, ones = Ones2agg(a, "x", "", "y"))
#>   x y z ones
#> 1 A 5 r    1
#> 2 A 5 s    0
#> 3 B 5 s    1
#> 4 B 6 s    1
#> 5 C 6 s    1
#> 6 C 6 s    0
#> 7   7 s    0
#> 8   7 s    0
cbind(a, ones = Ones2agg(a, "x", "", "z"))
#>   x y z ones
#> 1 A 5 r    1
#> 2 A 5 s    1
#> 3 B 5 s    1
#> 4 B 6 s    0
#> 5 C 6 s    1
#> 6 C 6 s    0
#> 7   7 s    0
#> 8   7 s    0
cbind(a, ones = Ones2agg(a, "x", "", c("y", "z")))
#>   x y z ones
#> 1 A 5 r    1
#> 2 A 5 s    1
#> 3 B 5 s    1
#> 4 B 6 s    1
#> 5 C 6 s    1
#> 6 C 6 s    0
#> 7   7 s    0
#> 8   7 s    0
cbind(a, ones = Ones2agg(a, "x", dupvar = "x"))
#>   x y z ones
#> 1 A 5 r    1
#> 2 A 5 s    0
#> 3 B 5 s    1
#> 4 B 6 s    0
#> 5 C 6 s    1
#> 6 C 6 s    0
#> 7   7 s    1
#> 8   7 s    0
cbind(a, ones = Ones2agg(a, "x", dupvar = character(0)))
#>   x y z ones
#> 1 A 5 r    1
#> 2 A 5 s    0
#> 3 B 5 s    1
#> 4 B 6 s    0
#> 5 C 6 s    1
#> 6 C 6 s    0
#> 7   7 s    1
#> 8   7 s    0
a[1, 1] <- NA
cbind(a, ones = Ones2agg(a, "x"))
#>      x y z ones
#> 1 <NA> 5 r    0
#> 2    A 5 s    1
#> 3    B 5 s    1
#> 4    B 6 s    1
#> 5    C 6 s    1
#> 6    C 6 s    1
#> 7      7 s    1
#> 8      7 s    1
```
