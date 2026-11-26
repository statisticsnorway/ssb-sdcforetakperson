# Apply a function with data frame output over data subsets

Funksjonen er kopiert fra Kostra-pakken og det er lagt in ekstra for å
håndtere desimaltallsinput.

## Usage

``` r
KostraApply(data, by, Fun, dataDec, decimal, ...)
```

## Arguments

- data:

  Data frame

- by:

  Vector of variable names (or variable numbers) defining the subsets or
  alternatively a named list or data frame (see examples).

- Fun:

  Function to be applied on data subsets

- dataDec:

  ekstra for SdcForetakPerson

- decimal:

  ekstra for SdcForetakPerson

- ...:

  Arguments passed to Fun

## Value

A data.frame

## Note

The output ordering of the example depends on default.stringsAsFactors()

## Examples

``` r
1+1
#> [1] 2
```
