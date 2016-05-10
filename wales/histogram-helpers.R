source("wales/constants.R");

## Produces the distribution of a particular variable within the English survey after it has been reweighted to fit Wales.
actual.distribution <- function(column, englishData, idsAndWeights)
    merge(
        englishData[,c(column, ehs.idCol)],
        idsAndWeights,
        by.x = ehs.idCol,
        by.y = ehs.idCol
    )[,c(column, ehs.weightCol)];
