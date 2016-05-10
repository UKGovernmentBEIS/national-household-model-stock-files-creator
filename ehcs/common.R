#' Utility to ensure that the levels of a given column are as required.
check.levels <- function(col, check) {
    if (!is.factor(col)) {
        stop("column is not a factor")
    }
    l <- levels(col)
    if (!setequal(l, check)) {
        stop(cat(sep="\n",
            cat("Extra levels: ", setdiff(l, check), sep=","),
            cat("Missing levels: ", setdiff(check, l), sep=",")
        ))
    }
}

#' Wraps plyr::revalue to check that the levels in the revaluing
#' rule are exhaustive and complete.
checked.revalue <- function(a, b) {
    check.levels(a, names(b))
    plyr::revalue(a, b)
}


#' Replaces values in y$a with those in y$b for numeric values
#' used for replacing fields coded with numbers representing unobtainable or not applicable with NA
checked.renum <- function(x,y) {
  if (!is.numeric(x)) {
    stop("column is not a numeric")
  }
  x <- mapvalues(x, from = y$a, to = y$b)
  return(x)
}


#' Convert the confusing SHCS dual column for building type into a single bit of data
#' @param C1 - house type
#' @param C2 - flat type
building.type <- function(C1, C2) {
    ## TODO add check that no part of C1 and C2 are stupid
    ## and say not house and not flat
    as.factor(
        ifelse(
            C1 == "Not house",
            as.character(C2),
            as.character(C1)))
}

is.flat <- function(buildingtype) {
    buildingtype %in% c("Tenement", "4-in-a-block", "Tower or slab", "Flat from conversion")
}

is.house <- function(buildingtype) {
    !is.flat(buildingtype)
}

#' Stub method just returns string of TODO
#'  
#' @param length - The number of rows to return
to.do <- function(length){
  rep("TODO", length(length))
}

is.a.house <-function(dwellingType){
  as.factor(checked.revalue(
    dwellingType,c(
      "converted flat" = FALSE,
      "purpose built flat, high rise" = FALSE,
      "semi-detached house" = TRUE,
      "purpose built flat, low rise" = FALSE,
      "medium/large terraced house" = TRUE,
      "detached house" = TRUE,
      "small terraced house" = TRUE,
      "bungalow" = TRUE)))}
