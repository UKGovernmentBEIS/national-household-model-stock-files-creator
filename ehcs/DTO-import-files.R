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
#'@param allEntries - the combined EHS spss files
#'
#'@param output - the path to the output file including the required file name and
#' the .csv
save.eng_IStockImportMetadataDTO <- function(allEntries, output) {
  write.csv(make.eng_IStockImportMetadataDTO(allEntries), file=output, row.names=FALSE)
}

#'Make a IStockImportMetadataDTO
#' 
#' @param shcs - the scottish survey data
make.eng_IStockImportMetadataDTO <- function(allEntries) {
  data.frame(
              Date = Sys.Date(),
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
#'@param allEntries - the combined EHS spss files
#'#' 
#'@param output - the path to the output file including the required file name and
#' the .csv
save.eng_IImportLogDTO <- function(allEntries, output) {
  write.csv(make.eng_IImportLogDTO(allEntries), file=output, row.names=FALSE)
}

#'Make a IImportLog
#' 
#' @param shcs - the scottish survey data
make.eng_IImportLogDTO <- function(allEntries) {
subset(data.frame(
              Aacode	= "",
              Messages	= ""), Aacode == "", drop = TRUE)
}

#'
#'\pagebreak
#'
#'##Make and save metadata
#'
#'Create a .csv file that contains the correct data which is created in the function
#' make.metadata
#'
#'@param allEntries - the combined EHS spss files
#' 
#'@param output - the path to the output file including the required file name and
#' the .csv
save.eng_metadata <- function(allEntries, output) {
  write.csv(make.eng_metadata(allEntries), file=output, row.names=FALSE)
}

#'Make a metadata
#' 
#' @param allEntries - the combined EHS spss files
make.eng_metadata <- function(allEntries) {
subset(data.frame(
              type	= "",
              DTO	= ""), type == "", drop = TRUE)
}
