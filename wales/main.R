## Takes Toby's recoded variables for Wales (eventually we probably want to run the script that produces these instead here).

source("constants.R");
source("match-and-normalize.R");
source("functions-for-raw-wales-data-production.R")
source("functions-for-wales-income-uprating-2017versions.R");

make.wales <- function(path.to.wales, output.dir) {
    print("Making Wales...")
    

    #produce lookup data from liw.data (including uprating incomes to 2012-13)
    weighted.wales.data <- produce.wales.data(path.to.wales)

    ## transform the weighted data in accordance with kieran's request
    ## to scale up by a factor of 1.0251. This is to reflect new
    ## builds, so rather than scaling everything up by that amount, we
    ## will scale up newer builds by the amount needed to have that
    ## effect.

    total.current.weight <- sum(weighted.wales.data$weight)
    target.weight <- total.current.weight * 1.0332
    delta.weight <- target.weight - total.current.weight
    subpopulation <- !is.na(weighted.wales.data$buildyear) &
        (weighted.wales.data$buildyear >= 2000) # this 2000 lets you
                                               # decide which subset
                                               # to up-weight
    sub.current.weight <- sum(weighted.wales.data[subpopulation, ]$weight)
    sub.target.weight <- sub.current.weight + delta.weight
    scale.factor <- sub.target.weight / sub.current.weight

    print(paste("Adjusting weights of", sum(subpopulation), "cases by", scale.factor,
                "to produce", delta.weight, "more weight"))

    weighted.wales.data$weight[subpopulation] <-
        weighted.wales.data$weight[subpopulation] * scale.factor

    print(paste("Weight was", total.current.weight, "target was", target.weight, "new weight is",
                sum(weighted.wales.data$weight)))

    ## now the weights are justed, we store the results in a file
    ## for inspection, and then load that file again and
    ## run the re-weighter on it.

    wales.input.path <- file.path(path.to.wales, "wales-stock-input-data.csv")

    # store the input data
    write.csv(weighted.wales.data,
              wales.input.path, row.names = FALSE)
    
    #create wales ids and weights
    idsAndWeights <- wales.load(wales.input.path)$idsAndWeights;

    ## Make sure that the output dir exists.
    dir.create(output.dir,
               showWarnings = FALSE);
    
    england.files <- list.files(england.dir, full.names = FALSE);
    
    excludedFiles <-
      c("IImportLogDTO.csv", "IStockImportMetadataDTO.csv", "metadata.csv");
    
    ## Output modified stock.
    lapply(england.files,
           function(file) {
             if (file_ext(file) == "csv" && !(file %in% excludedFiles)) {
               table <- england.load(file);
               
               outputTable <- wales.transformTable(table,
                                                   idsAndWeights,
                                                   file == "cases.csv");
               
               write.csv(outputTable,
                         paste(output.dir, file, sep = "/"),
                         row.names = FALSE,
                         na="");
               
             } else {
               ## Unknown file extension - just copy it.
               file.copy(paste(england.dir, file, sep = "/"),
                         output.dir,
                         overwrite = TRUE);
             }
           });

}
