## Load the Welsh and English stocks.
## 

source("constants.R");
source("external-construction-type.R");
source("binning.R");
source("match-row.R");
source("normalize-weights.R");
source("modify-stock.R");

library(plyr);

include.english.table <- function(dataSoFar, columns, tableName) {
    ## Load a table, extract the columns we want, then join it onto the existing data using the id column.
    table <- england.load(tableName);
    
    table.cols <- intersect(columns, names(table));
    table.cols <- append(table.cols, ehs.idCol);
    
    merge(
        dataSoFar,
        table[table.cols],
        by.x = ehs.idCol,
        by.y = ehs.idCol
    );
}

wales.load <- function(wales.inputs) {
    wales.data <- read.csv(
        wales.inputs,
        stringsAsFactors = FALSE
    );

    cols <- names(wales.data);

    ## Exclude the weight column.
    cols <- cols[
        -(which(
             cols == "weight"
         ))
    ];

    ## Data from cases.
    england.data.cases <- england.load("cases.csv");
    cols.cases <- intersect(cols, names(england.data.cases));

    cols.cases <- append(cols.cases, ehs.idCol);
    cols.cases <- append(cols.cases, ehs.weightCol);
    
    england.data <- england.data.cases[,cols.cases];

    england.data <- include.english.table(england.data, cols, "occupants.csv");
    england.data <- include.english.table(england.data, cols, "roofs.csv");
    england.data <- include.english.table(england.data, cols, "space-heating.csv");

    ## Add the externalwallconstructiontype column.
    england.data <- merge(
        england.data,
        england.externalwallconstructiontype(),
        by.x = ehs.idCol,
        by.y = ehs.idCol
    );

    ## England has some "NULL" values for children, and so gets read in as a string column.
    england.data[["children"]] <- as.numeric(england.data[["children"]]);

    ## Missing columns.
    missing <- setdiff(cols, names(england.data));

    if (length(missing) > 0) {
        stop("Missing columns in English data ", paste(missing, collision=" "));
    }

    ## Everything lower case.
    dfToLower <- colwise(function(col) {
        return (
            if (is.character(col)) {
                tolower(col);
            } else {
                col
            }
        );
    });
    england.data <- dfToLower(england.data);
    wales.data <- dfToLower(wales.data);

    ## Extract the weights for later.
    wales.weights <- wales.data[["weight"]];
    wales.weights.sum <- sum(wales.data[["weight"]]);
    wales.data[["weight"]] <- NULL;

    ## Bin numeric columns
    binned <- wales.binning(england.data, wales.data);
    england.data.unbinned <- england.data;
    england.data <- binned$englishData;
    wales.data <- binned$welshData;

    wales.data[["weight"]] <- wales.weights;

    matched <- ldply(
        ## Get the row ids so that we can identify what comes out.
        rownames(wales.data),
        function(rowId) {
            return(
                wales.matchRow(
                    england.data,
                    rowId,
                    wales.data[rowId,]
                )
            );
        }
    );

    ## Join the English weights onto our results.
    matched <- merge(
        matched,
        dfToLower(
            england.data.cases[,c(ehs.idCol, ehs.weightCol)]
        ),
        by.x = ehs.idCol,
        by.y = ehs.idCol
    );

    ## Normalize weights over our matches
    weighted.aacodes <- ddply(
        matched,
        c("welshId"),
        function(englishSubset) {
            welshId <- as.integer(
                englishSubset[1, "welshId"]
            );

            return(
                wales.distributeWeights(
                    englishSubset[,c(ehs.idCol, ehs.weightCol)],
                    ## Get the welsh weight for this subset.
                    wales.weights[[welshId]]
                )
            );
        }
    );

    ## Merge weights for the same aacode
    weighted.aacodes <- ddply(
        weighted.aacodes,
        c(ehs.idCol),
        function(englishSubset) {
            return(
                c(
                    dwellingcaseweight = sum(
                        englishSubset[[ehs.weightCol]]
                    )
                )
            );
        }
    );

    ## Ensure we have the right number of dwellings in wales.
    if (wales.weights.sum != sum(weighted.aacodes[[ehs.weightCol]])) {
        stop(
            paste(
                "Wrong number of dwellings in Wales",
                "should be",
                wales.weights.sum,
                "was",
                sum(weighted.aacodes[[ehs.weightCol]]),
                sep = " "
            )
        );
    }

    list(
        idsAndWeights = weighted.aacodes,
        englishData = england.data.unbinned,
        columns = cols
    );
}
