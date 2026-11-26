# Rounding after suppression

Within `PLSrounding` within the current function, the suppressed
combinations are not treated as publishable and are not in output from
that function, but they are in output from the current function as
suppressed cells.

## Usage

``` r
PLSroundingSuppressed(
  data,
  freqVar,
  dataSuppressed = NULL,
  hierarchies = NULL,
  formula = NULL,
  numVar = NULL,
  output = "data.frame",
  printPLSrounded = FALSE,
  rndSeed = 123,
  allowTotal = FALSE,
  ...
)
```

## Arguments

- data:

  Input data as a data frame (inner cells)

- freqVar:

  Variable holding counts (inner cells frequencies)

- dataSuppressed:

  Data frame specifying combinations to be suppressed

- hierarchies:

  List of hierarchies

- formula:

  A model formula

- numVar:

  Other numerical variables to be aggregated

- output:

  If not "data.frame" (default), more content in output

- printPLSrounded:

  When TRUE, output from print `PLSrounding` within the function is
  printed

- rndSeed:

  If non-NULL, a random generator seed to be used locally within the
  function without affecting the random value stream in R.

- allowTotal:

  When TRUE, no suppression when other variables are `"Total"`.

- ...:

  Further parameters sent to `PLSrounding` within the function is
  printed

## Value

By default, a data frame with aggregates and rounded frequencies with
suppression information

## Examples

``` r

z1 <- SmallCountRounding::SmallCountData("z1")
sup <- GaussSuppression::GaussSuppressionFromData(z1, 1:2, 3)
#> GaussSuppression_anySum: ..........................................
sup <- sup[sup$suppressed, !(names(sup) %in% c("ant", "primary", "suppressed"))]
a <- PLSroundingSuppressed(z1, "ant", sup, roundBase = 20)
aNoSuppression <- PLSroundingSuppressed(z1, "ant", roundBase = 20)

z3 <- SmallCountRounding::SmallCountData("z3")
sup <- GaussSuppression::GaussSuppressionFromData(z3, c(1, 2, 4), 7)
#> GaussSuppression_anySum: ...........................
sup <- sup[sup$suppressed, !(names(sup) %in% c("ant", "primary", "suppressed"))]
b <- PLSroundingSuppressed(z3[, -c(3, 6)], "ant", sup, roundBase = 3)

if (FALSE) { # \dontrun{
if (require(easySdcTable)) {
  sup <- ProtectTable(EasyData("z1"), 1:2, 3, method = "Gauss")$data
  sup <- sup[is.na(sup$suppressed), !(names(sup) %in% c("freq", "sdcStatus", "suppressed"))]
  a_ <- PLSroundingSuppressed(z1, "ant", sup, roundBase = 20)
  print(identical(a, a_))
  
  sup <- ProtectTable(EasyData("z3"), c(1, 2, 4), 7, method = "Gauss")$data
  sup <- sup[is.na(sup$suppressed), !(names(sup) %in% c("freq", "sdcStatus", "suppressed"))]
  b_ <- PLSroundingSuppressed(z3[, -c(3, 6)], "ant", sup, roundBase = 3)
  print(identical(b, b_))
}
} # }
```
