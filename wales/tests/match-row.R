library('testthat')

source("wales/match-row.R");

test.welshId <- "W";

test.matchRow <- function(welshRow) {
    return(
        wales.matchRow(
            test.ehs,
            test.welshId,
            welshRow
        )
    );
};

test.matchRowExpecting <- function(welshRow, rows, ids, columns) {
    result <- test.matchRow(welshRow);

    expect_equal(nrow(result), rows);
    expect_equal(result[[ehs.idCol]], ids);

    ## Ensure these came out as they went in.
    expect_true(all(
        result[["welshId"]] == test.welshId
    ));

    columnsStr = paste(
        mapply(
            FUN = function(name, value) {
                return(
                    paste(
                        name,
                        value,
                        collapse = " "  
                    )
                );
            },
            names(columns),
            columns
        ),
        collapse = "; "
    );

    expect_true(
        all(
            result[["usedColumns"]] == columnsStr
        )
    );
};

test.ehs <- data.frame(
    "aacode" = as.character(1:5),
    "foo" = as.character(101:105),
    "bar" = c("a", "b", "c", "c", "b"),
    "dwellingcaseweight" = c(10, 10, 20, 20, 30),
    stringsAsFactors = FALSE
);

test_that("Match nothing", {
    ## When we match nothing, we actually match everything.
    test.matchRowExpecting(
        data.frame(
            foo = "0"
        ),
        rows = nrow(test.ehs),
        ids = test.ehs[[ehs.idCol]],
        columns = list()
    );
});

test_that("Match one row", {
    result <- test.matchRowExpecting(
        data.frame(
            foo = "103",
            stringsAsFactors = FALSE
        ),
        rows = 1,
        ids = "3",
        columns = list("foo" = "103")
    );
});

test_that("Match many rows", {
    result <- test.matchRowExpecting(
        data.frame(
            bar = "b",
            stringsAsFactors = FALSE            
        ),
        rows = 2,
        ids = c("2", "5"),
        columns = list("bar" = "b")
    );
});

test_that("Match one row with multiple properties", {
    result <- test.matchRowExpecting(
        data.frame(
            bar = "b",
            foo = "105",
            stringsAsFactors = FALSE
        ),
        rows = 1,
        ids = "5",
        columns = list("bar" = "b", "foo" = "105")
    );
});

test_that("Match one row with multiple properties, short-circuiting the second property.", {
    result <- test.matchRowExpecting(
        data.frame(
            foo = "105",
            bar = "b",
            stringsAsFactors = FALSE
        ),
        rows = 1,
        ids = "5",
        columns = list("foo" = "105")
    );
});

test_that("Match some rows, with the first property failing.", {
    result <- test.matchRowExpecting(
        data.frame(
            foo = NA_character_,
            bar = "b",
            stringsAsFactors = FALSE
        ),
        rows = 2,
        ids = c("2", "5"),
        columns = list("bar" = "b")
    );
});
