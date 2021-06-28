
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Writing R Functions

<!-- badges: start -->
<!-- badges: end -->

This tutorial provides resources and exercises for learning and
improving skills in writing R functions.

## Learning objectives

By the end of this tutorial, participants would be able to:

1.  identify the structure of an R function;

2.  demonstrate appropriate syntax and recommended style for writing an
    R function; and,

3.  write performant and robust R functions given specific purposes and
    intended functionalities.

## Learning resources

-   **Programming with R: Creating Functions** from *Software Carpentry*
    found at
    <https://swcarpentry.github.io/r-novice-inflammation/02-func-R/>

-   **Functions** from *R for Data Science* found at
    <https://r4ds.had.co.nz/functions.html>

-   **R code** from *R Packages* found at <https://r-pkgs.org/r.html>

## Skills application and demonstration

### Exercise 1

Using the **Nutriverse** `nutricheckr` pockage, review the structure of
the `flag_who` function and the type of output/s it provides (see
<https://github.com/nutriverse/nutricheckr/blob/master/R/flag_who.R>).

``` r
################################################################################
#
#'
#' Apply World Health Organization (WHO) anthropometric z-score indices flagging
#' criteria
#'
#' @param df A data.frame containing anthropometric z-score indices for
#'   `height-for-age`, `weight-for-age` and/or `weight-for-height`
#' @param haz A character value indicating the variable name in `df` for the
#'   `height-for-age z-score`
#' @param waz A character value indicating the variable name in `df` for the
#'   `weight-for-age z-score`
#' @param whz A character value indicating the variable name in `df` for the
#'   `weight-for-height z-score`
#' @param add Logical. Should flag values be added to `df`. Default is TRUE.
#'
#' @return If `add` FALSE, returns a vector of `flag` coded values indicating
#'   problematic measurements. if `add` TRUE, returns `df` with additional
#'   column named `flag` containing coded values indicating problematic
#'   measurements
#'
#' @examples
#' flag_who(df = zscorer::anthro1, haz = "haz", waz = "waz", whz = "whz")
#'
#' @export
#'
#
################################################################################

flag_who <- function(df, haz = NULL, waz = NULL, whz = NULL, add = TRUE) {
  ##
  flag <- vector(mode = "numeric", length = nrow(df))
  ##
  if(!is.null(haz)) {
    flag <- ifelse(
      !is.na(df[[haz]]) & (df[[haz]] < -6 | df[[haz]] > 6), 
      flag + 1, flag
    )
  }
  ##
  if(!is.null(whz)) {
    flag <- ifelse(
      !is.na(df[[whz]]) & (df[[whz]] < -5 | df[[whz]] > 5), 
      flag + 2, flag
    )
  }
  ##
  if(!is.null(waz)) {
    flag <- ifelse(
      !is.na(df[[waz]]) & (df[[waz]] < -6 | df[[waz]] > 5), 
      flag + 4, flag
    )
  }
  ##
  if(add) {
    df$flag <- flag
    flag <- df
  }
  return(flag)
}
```

Then, create an alternative function using a different structure and/or
approach to getting at flagged z-score values.

### Exercise 2

The following table of haemoglobin values at sea level used to diagnose
anaemia.

| Population                  | Mild      | Moderate | Severe  |
|:----------------------------|:----------|:---------|:--------|
| Children 6-59 months of age | 100 - 109 | 70 - 99  | &lt; 70 |
| Children 5-11 years of age  | 110 - 114 | 80 - 109 | &lt; 80 |
| Children 12-14 years of age | 110 - 119 | 80 - 109 | &lt; 80 |
| Non-pregnant women          |           |          |         |
| (15 years and above)        | 110 - 119 | 80 - 109 | &lt; 80 |
| Pregnant women              | 100 - 109 | 70 - 99  | &lt; 70 |
| Men                         |           |          |         |
| (15 years and above)        | 110 - 129 | 80 - 109 | &lt; 80 |

Hb levels to diagnose anaemia at sea level in grams per litre (g/L)

Using this information, create a function that would accept data
containing haemoglobin values, age/age grouping and altitude and would
then classify each row of the data to whether the specific
sample/participant has anaemia.
