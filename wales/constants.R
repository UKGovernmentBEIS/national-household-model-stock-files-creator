ehs.weightCol <- "dwellingcaseweight";
ehs.householdWeightCol <- "householdcaseweight";
ehs.regionCol <- "regiontype";
ehs.idCol <- "aacode";
ehs.tables <- c(
    "cases"
);

england.dir <- "outputs/england";
wales.inputs <- "data/LiW-2008/wales-stock-input-data.csv";

england.load <- function(file) {
    table <- read.csv(
        paste(england.dir, file, sep = "/"),
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    table[[ehs.idCol]] <- tolower(table[[ehs.idCol]]);

    table;
};
