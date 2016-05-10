## Provides the classIntervals function.
library(classInt);

## Takes in two dataframes with the same columns.
## Finds columns which are numeric.
## Bins the data for those columns.

## Returns a list containing two properties:
### englishData: the dataframe containing the binned English data
## welshData: the dataframe containing the binned Welsh data

wales.binning <- function(englishData, welshData) {
    if (!is.data.frame(englishData)) {
        stop("englishData was not a data frame");
    }

    if (!is.data.frame(welshData)) {
        stop("welshData was not a data frame");
    }

    ## Find which columns in the Welsh data set are numeric.
    numericCols <-sapply(welshData, is.numeric);

    if (!any(numericCols)) {
        warning("Didn't find any numeric columns");
        ## Nothing to bin, so return what we got.
        return(
            list(
                englishData = englishData,
                welshData = welshData
            )
        );
    }


    ## Get the Welsh numeric columns and their names.
    welshNumericData <- welshData[numericCols];
    numericCols <- names(
        welshNumericData
    )

    ## Check that all numeric columns in the Welsh dataset are present in the English one.
    missing <- setdiff(numericCols, names(englishData));
    if (length(missing) > 0) {
        stop("The following columns were missing from the English dataset: ", missing);
    }

    ## Get the columns from the English data which *should* be numeric.
    englishNumericData <- englishData[numericCols];

    ## Checks that the columns are also numeric in the English data set.
    wales.binning.ensureNumeric(englishNumericData);

    ## Calculate our bins based on the combined data.
    bins <- lapply(
        rbind(englishNumericData, welshNumericData),
        FUN=classIntervals
    );

    ## Extract just the intervals.
    bins <- lapply(
        bins,
        FUN="[",
        "brks"
    );

    ## Convert our intervals into a numeric vector.
    bins <- lapply(
        bins,
        FUN=unlist
    );

    ## Bin the variables and set that data back.
    ## We can use mapply here because the bins and the numeric data have the same number of columsn in the same order.
    ## We are effectively doing cut (columnOfData, columnOfIntervals) for each column.
    englishData[,numericCols] <- mapply(
        cut,
        englishNumericData,
        bins,
        include.lowest = TRUE
    );

    welshData[,numericCols] <- mapply(
        cut,
        welshNumericData,
        bins,
        include.lowest = TRUE
    );

    return(
        list(
            englishData = englishData,
            welshData = welshData
        )
    );
};

wales.binning.ensureNumeric <- function(data) {
    ## Get the columns in my data which weren't numeric.
    failingCols <- !sapply(
        data,
        is.numeric
    );

    ## Fail if we found any non-numeric columns.
    if (any(failingCols)) {
        failingColNames <- names(
            data[failingCols]
        )
        
        stop("The following columns were not numeric in the English data set: ", failingColNames);
    }
};
