library(openxlsx)
library(dplyr)


#' External data required:
#'  1. Annual Survey of Hours and Earnings (ASHEs) from 2008 onwards (to year of required uprating)
#'       - Gross weekly pay, all deciles and median
#'       - Male, Female
#'       - Full Time/Part Time
#'       - England, Wales, Scotland
#' Source: https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=30

#' 2. RPI index covering at least 2008 onwards (to year of required uprating)
#' Source: https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/chaw/mm23


#'EHS variables needed:

#'   aacode                - totes obvs    
#'   Sex                   - of hrp?
#'   agehrpx_hh            - derived interview dataset
#'   emphrpx_hh            - derived interview dataset
#'   hasOccupantOnBenefits - derived from people?
#'   fpfullinc             - fuelpov dataset
#'   fpbasinc              - fuelpov dataset
#'   twentile_fpfullinc    - fuelpov dataset
#'   twentile_fpbasicinc   - fuelpov dataset
#'   rentwkx               ?
#'   mortwkx               ?   

#' Access and process ASHE data


produce.wales.income.uprating.lup <- function() {
  #get raw data from NOMIS query
  ashe.raw <- read.xlsx("data/non-HCS-data/NOMIS_ASHE_FullPartTime_MaleFemale_2008-2016_GrossWeeklyPay_Alldeciles.xlsx")
  
  #create a series of numbers that refer to the annual columns
  n <- ncol(ashe.raw)
  gross.weekly.pay.by.year.cols <- c(1,seq(2,n,2))
  
  wales.mft <- ashe.raw[234:248,gross.weekly.pay.by.year.cols]
  wales.mpt <- ashe.raw[262:276,gross.weekly.pay.by.year.cols]
  wales.fft <- ashe.raw[291:305,gross.weekly.pay.by.year.cols]
  wales.fpt <- ashe.raw[319:333,gross.weekly.pay.by.year.cols]
  
  tidy.tables <- function(data,data.name,hrpwk) {
    ntile.number <- c(NA,5,NA,NA,NA,1,2,NA,3,4,6,7,NA,8,9)
    data <- cbind(data.name,hrpwk,ntile.number,data)
    n <- 2008 + ncol(data) - 5
    years <- as.character(seq(2008,n))
    column.headings <- c("Employee type","hrpwk","ASHE_dcode","ashe.item",years)
    names(data) <- column.headings
    tidied.data <- data %>% filter(ASHE_dcode < 10)
    
    #convert annual gross weekly income to numeric
    tidied.data[years] <- sapply(tidied.data[years],as.numeric)
    return(tidied.data)
  }
  
  wales.mft <- tidy.tables(wales.mft,"Wales - Male, full-time","1")
  wales.mpt <- tidy.tables(wales.mpt,"Wales - Male, part-time","2")
  wales.fft <- tidy.tables(wales.fft,"Wales - Female, full-time","3")
  wales.fpt <- tidy.tables(wales.fpt,"Wales - Female, part-time","4")
  
  ashe.lup <- rbind(wales.mft,wales.mpt,wales.fft,wales.fpt)
  
  #RPI
  RPI <- read.xlsx("data/non-HCS-data/CHAW-260617.xlsx")
  n <- nrow(RPI)
  RPI <- RPI[6:n,]
  RPI[1:2] <- sapply(RPI[1:2],as.numeric)
  names(RPI) <- c("Year", "RPI")
  
  RPI <- RPI %>% filter(Year >= 2008 & Year <= 2016)
  RPI <- spread(RPI,key=Year,value="RPI")
  
  RPI.ashe.format <- data.table(`Employee type` = "all other",
                                hrpwk = 0,
                                ASHE_dcode = seq(1,9),
                                ashe.item = NA,
                                RPI)
  
  ashe.lup <- rbind(ashe.lup,RPI.ashe.format)
  
  ashe.lup <- ashe.lup %>% 
    mutate(change.2008.to.2014 = `2014` / `2008`,
           change.2008.to.2015 = `2015` / `2008`) %>% 
    mutate(change.2008.to.2014 = ifelse(is.na(change.2008.to.2014),1,change.2008.to.2014),
           change.2008.to.2015 = ifelse(is.na(change.2008.to.2015),1,change.2008.to.2015))
  
  return(ashe.lup)
  
}
