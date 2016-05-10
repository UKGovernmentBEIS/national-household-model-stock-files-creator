#' ---
#' title: "DTO import files functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#'The additional files required for the stock import are created.
#'
#'##Make and save IStockImportMetadata
#'
#'Create a .csv file that contains the correct data which is created in the function
#' make.IStockImportMetadataDTO
#'
#'@param shcs - the scottish survey data
#' 
#'@param output - the path to the output file including the required file name and
#' the .csv
save.IStockImportMetadataDTO <- function(shcs, output) {
  write.csv(make.IStockImportMetadataDTO(shcs), file=output, row.names=FALSE)
}

#'Make a IStockImportMetadataDTO from the SHCS
#' 
#' @param shcs - the scottish survey data
make.IStockImportMetadataDTO <- function(shcs) {
  data.frame(
              Date	= "[new_date]",
              DescriptionByUser	= "[description_update]",
              SourceName	= "[sourceName_update]",
              SourceVersion	= "2012",
              StockImporterVersion	= "[stockImportVersion_update]",
              UserName =	"[ImporterUserName]")
}

#'
#'\pagebreak
#'
#'##Make and save IImportLog
#'
#'Create a .csv file that contains the correct data which is created in the function
#' make.IImportLogDTO
#'
#'@param shcs - the scottish survey data
#' 
#'@param output - the path to the output file including the required file name and
#' the .csv
save.IImportLogDTO <- function(shcs, output) {
  write.csv(make.IImportLogDTO(shcs), file=output, row.names=FALSE)
}

#'Make a IImportLog from the SHCS
#' 
#' @param shcs - the scottish survey data
make.IImportLogDTO <- function(shcs) {
subset(data.frame(
              Aacode	= "",
              Messages	= ""), Aacode == "", DROP = TRUE)
}

#'
#'\pagebreak
#'
#'##Make and save metadata
#'
#'Create a .csv file that contains the correct data which is created in the function
#' make.metadata
#'
#'@param shcs - the scottish survey data
#' 
#'@param output - the path to the output file including the required file name and
#' the .csv
save.metadata <- function(shcs, output) {
  write.csv(make.metadata(shcs), file=output, row.names=FALSE)
}

#'Make a metadata from the SHCS
#' 
#' @param shcs - the scottish survey data
make.metadata <- function(shcs) {
subset(data.frame(
              type	= "",
              DTO	= ""), type == "", DROP = TRUE)
}
