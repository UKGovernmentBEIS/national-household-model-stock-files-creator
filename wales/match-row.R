source("constants.R");

## A recursive function which works on the English table plus a single row of the Welsh table with its weight.

## Looks at each column in the Welsh table and filters down the English table accordingly.

## Columns which have no match in the English table will be ignored.
wales.matchRow <- function(englishSubset, welshId, welshRow, usedColumns = list()) {
    if (ncol(welshRow) == 0) {
        ## We've tried to match all the columns, so return whatever we have left.
        return(
            wales.matchedRows(englishSubset, welshId, usedColumns)
        );
    }

    headColumn <- welshRow[1];
    column <- names(headColumn);
    value <- unlist(headColumn[1]);

    restColumns <- welshRow[-1];

    if (is.na(value)) {
        return(
            wales.matchRow(
                englishSubset,
                welshId,
                restColumns,
                usedColumns
            )
        );
    }

    ## Remove rows which don't have the right value for the column we're looking at.
    matchingEnglish <- englishSubset[
        englishSubset[[column]] == value,
    ];

    matchingEnglish <- na.omit(matchingEnglish);

    if (nrow(matchingEnglish) == 0) {
        ## We can't match this column, so we'll ignore it and carry on.
        return(
            wales.matchRow(
                englishSubset,
                welshId,
                restColumns,
                usedColumns
            )
        );

    } else if (nrow(matchingEnglish) == 1) {
        usedColumns[column] <- value;
        
        ## We've only got one match left. The remaining columns can't help us get a better match.
        return(
            wales.matchedRows(
                matchingEnglish,
                welshId,
                usedColumns
            )
        );

    } else {
        ## We've successfully filtered our English cases, but we still have some options to choose from. Use the remaining columns to restrict them further.
        usedColumns[column] <- value;

        return(
            wales.matchRow(
                matchingEnglish,
                welshId,
                restColumns,
                usedColumns
            )
        );
    }
}

## Shapes the output of our matchRow function: return one row for every aacode matched in the English stock.
wales.matchedRows <- function(matchingRows, welshId, usedColumns) {
    len <- nrow(matchingRows);

    columnsStr <- (
        if (length(usedColumns) == 0) {
            "";
        } else {
            ## Write out the names and values of the columns we matched in order.
            paste(
                mapply(
                    FUN = function(name, value) {
                        return(
                            paste(name, value, collapse = " ")
                        );
                    },
                    names(usedColumns),
                    usedColumns
                ),
                collapse = "; "
            );
        }
    );

    return(
        data.frame(
            aacode = matchingRows[[ehs.idCol]],
            welshId = rep(welshId, len),
            usedColumns = rep(columnsStr, len),
            stringsAsFactors = FALSE
        )
    );
};
