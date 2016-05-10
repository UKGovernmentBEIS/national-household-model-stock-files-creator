library('testthat');
source("wales/binning.R");

test.textData <-list(data = c("a", "b", "c"));

test_that("Does nothing if no numeric columns", {
    welshData <- data.frame(data = c("a", "b", "c"));

    expect_warning(
        results <- wales.binning(
            data.frame(),
            welshData
        ),
        "Didn't find any numeric columns"
    )

    expect_equal(results$englishData, data.frame());
    expect_equal(results$welshData, welshData);
});

test_that("Should fail if English numeric column missing", {
    welshData <- data.frame(data = 1:10);

    expect_error(
        wales.binning(
            data.frame(),
            welshData
        ),
        "The following columns were missing from the English dataset: data"
    );
});

test_that("Should fail if English column is non-numeric", {
    englishData <- data.frame(data = c("a", "b", "c"));
    welshData <- data.frame(data = 1:10);

    expect_error(
        wales.binning(
            englishData,
            welshData
        ),
        "The following columns were not numeric in the English data set: data"
    );
});

test_that("Fails if arguments are not data frames", {
    expect_error(
        wales.binning(
            list(),
            data.frame()
        ),
        "englishData was not a data frame"
    );

    expect_error(
        wales.binning(
            data.frame(),
            list()
        ),
        "welshData was not a data frame"
    );
});

test_that("Should put data into bins", {
    englishData <- data.frame(data = 1:20);
    welshData <- data.frame(data = 11:40);

    results <- wales.binning(
        englishData,
        welshData
    );

    expect_equal(
        results$englishData$data[[1]],
        "[1,8]"
    );

    expect_equal(
        results$englishData$data[[20]],
        "(16,20]"
    );

    expect_equal(
        results$welshData$data[[1]],
        "(8,13]"
    );

    expect_equal(
        results$welshData$data[[30]],
        "(33,40]"
    );
});
