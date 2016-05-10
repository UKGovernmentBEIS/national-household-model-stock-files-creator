
dtofile <- "cases.csv"
country <- "england"


test.files ("cases.csv","england")

test.files <- function(dtofile,country="england"){
  old.folder <-ifelse(country == "england","EHCS2012_Orginal_for_DECC","TODO")# Don't have origional stock for Scotland with same files
  
  old.stock <- read.csv(file.path(getwd(),"data",old.folder,dtofile),header=TRUE)
  new.stock <- read.csv(file.path(getwd(),"outputs",country,dtofile),header=TRUE)
  overall.messages <- test.comparefile.overall(old.stock,new.stock)
  strcture.messages <- test.comparefile.structure(old.stock,new.stock)
}

test.comparefile.overall <- function(old.stock,new.stock){
  #Do csv files match exactly? (case sensitive, the nhm is not)
  incorrect.file<- paste("CSV files identical:",isTRUE(all.equal(new.stock,old.stock)))
  
  dims.col <- ifelse(length(new.stock)==length(old.stock),
                     paste("Number of columns: correct - ",length(new.stock)),
                     paste("Number of columns: incorrect - have ",length(new.stock),",should have",length(old.stock)))
  dims.rows <- ifelse(length(new.stock[[1]])==length(old.stock[[1]]),
                      paste("Number of records: correct - ",length(new.stock[[1]])),
                      paste("Number of records: incorrect - have ",length(new.stock[[1]]),",should have",length(old.stock[[1]])))
  
  #do column names match?
  #incorrect.names is a list of column names that appear in the new stock but not in the old stock (not case sensitive)
  incorrect.names <- paste("Column name incorrect:",grep(paste(colnames(old.stock),collapse="|"),colnames(new.stock),ignore.case=TRUE,value=TRUE,invert=TRUE))
  
  #incorrect.names is a list of column names that appear in the old stock but not in the new stock (not case sensitive)
  missing.names <- paste("Column name missing:",grep(paste(colnames(new.stock),collapse="|"),colnames(old.stock),ignore.case=TRUE,value=TRUE,invert=TRUE))
  
  #incorrect.position is a list of column names that are in the wrong position
  
  if(length(new.stock)==length(old.stock)){
    incorrect.position <- paste("Column postion incorrect:",grep(paste(grep(paste(colnames(old.stock),collapse="|"),
                                                                            colnames(new.stock),ignore.case=TRUE,value=TRUE,invert=TRUE),
                                                                       collapse="|"),colnames(new.stock[ifelse(colnames(new.stock)==colnames(old.stock),FALSE,TRUE)]),value=TRUE,invert = TRUE))
  } else {incorrect.position <-("Column positions may be incorrect, recheck when all columns exist")}
  
  column.messages <- c(incorrect.file,dims.col,incorrect.names,missing.names,incorrect.position,dims.rows)
  return(column.messages)
}


test.comparefile.structure <- function(old.stock,new.stock){
  str(old.stock)
  str(new.stock)
}






















#Test that returns a string indicating if column names do or don't match
ifelse(isTRUE(ifelse(colnames(old.stock)==colnames(new.stock),1,0)),"Column names match","Column names don't match")

b <- colnames(new.stock[ifelse(colnames(new.stock)==colnames(old.stock),FALSE,TRUE)])
a <- grep(paste(colnames(old.stock),collapse="|"), colnames(new.stock),ignore.case=FALSE,value=TRUE,invert=TRUE)

#list of the differences between files
difference.list <- all.equal(new.stock,old.stock)
