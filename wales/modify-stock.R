source("constants.R");

library(tools);

## Special Behaviour for cases table
## Set the region to Wales.
## Set the appropriate dwelling weight.
## Set the household weight to 0, since it should not be used.
wales.casesTable <- function(casesTable, idsToWeights) {
    casesTable[[ehs.regionCol]] <- 'Wales';

    ## Remove existing weight column.
    casesTable[[ehs.weightCol]] <- NULL;

    ## Bring in new weights instead.
    casesTable <- merge(
        casesTable,
        idsToWeights,
        by.x = ehs.idCol,
        by.y = ehs.idCol
    );

    ## Household weights are meaningless with the rebalancing we've done.
    casesTable[[ehs.householdWeightCol]] <- 0;
    
    casesTable;
};

## Convert an English id into a Welsh one.
## We know that the English Housing Survey has no AACodes beginning with 'w', so this is safe.
wales.makeWelshId <- function(englishId) {
    paste("w", englishId, sep="")
};

## Create a Welsh version of an EHS table.
## Given a table from the stock, and a table of English ids -> Welsh ids, return a version of that table with Welsh ids instead.
wales.withWelshIds <- function(table) {
    table[[ehs.idCol]] <- wales.makeWelshId(
        table[[ehs.idCol]]
    );

    table;
};

wales.transformTable <- function(table, idsToWeights, isCasesTable) {
    rowsMatchingWales <- table[[ehs.idCol]] %in% idsToWeights[[ehs.idCol]];

    welshTable <- wales.withWelshIds(
        table[rowsMatchingWales, ]
    );

    if (isCasesTable) {
        idsToWeights <- wales.withWelshIds(idsToWeights);

        welshTable <- wales.casesTable(welshTable, idsToWeights);
    }

    welshTable;
};
