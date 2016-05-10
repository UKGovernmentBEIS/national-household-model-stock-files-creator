library(RUnit)
source("~/software-projects/r-nhm-stock-creator/ehcs/cases.R", chdir=T);

test.floor.construction <- function(){
  checkEquals("SOLID",  groundfloor.construction.type(NA,NA))
  checkEquals("SOLID",  groundfloor.construction.type("Yes",NA))
  checkEquals("SOLID",  groundfloor.construction.type("Yes","Yes"))
  checkEquals("SOLID",  groundfloor.construction.type(NA,"Yes"))
  
  checkEquals("SUSPENDED",  groundfloor.construction.type("No",NA))
  checkEquals("SUSPENDED",  groundfloor.construction.type(NA,"No"))
  checkEquals("SUSPENDED",  groundfloor.construction.type("Yes","No"))
  checkEquals("SUSPENDED",  groundfloor.construction.type("No","Yes"))
}

test.occupant.counts <- function(){
  checkEquals(0, occupant.counts(NA,NA)$adults)
  checkEquals(0, occupant.counts(NA,NA)$children)
  
  checkEquals(1, occupant.counts(1,0)$adults)
  checkEquals(0, occupant.counts(0,1)$adults)
  checkEquals(0, occupant.counts(1,0)$children)
  checkEquals(1, occupant.counts(0,1)$children)
}

test.livingroom.data <- function(){
  checkEquals(0, cal.livingarea.data(1,4,7,0)$livingAreaFaction)
  checkEquals(0, cal.livingarea.data(1,88.8,7,100)$livingAreaFaction)
  checkEquals(0, cal.livingarea.data(1,1,88.8,100)$livingAreaFaction)
  checkEquals(0.28, cal.livingarea.data(1,4,7,100)$livingAreaFaction)
  
  # Test some null values - would expect 0's
  checkEquals(0, cal.livingarea.data(1,NA,NA,100)$livingAreaFaction)
}


test.plot.dimensions.backplot <- function() {
  #widthOfPlot, doesFrontPlotExist, doesBackPlotExist, depthOfBackPlot
  checkEquals(10, calc.plot.dimensions(10, NA, "Yes", 10, 0)$backplotDepth)
  checkEquals(20, calc.plot.dimensions(20, NA, "Yes", 10, 0)$backplotWidth)
  checkEquals(100, calc.plot.dimensions(10, NA, "Yes", 10, 0)$backplotArea)

  checkEquals(0, calc.plot.dimensions(10, NA, "No", 10, 0)$backplotDepth)
  checkEquals(0, calc.plot.dimensions(10, NA, "No", 10, 0)$backplotArea)
}

test.plot.dimensions.frontplot <- function() {
  #widthOfPlot, doesFrontPlotExist, doesBackPlotExist, depthOfBackPlot
  checkEquals(10, calc.plot.dimensions(10, "Yes", NA, 0, 10)$frontplotDepth)
  checkEquals(20, calc.plot.dimensions(20, "Yes", NA, 0, 10)$frontplotWidth)
  checkEquals(100, calc.plot.dimensions(10, "Yes", NA, 0, 10)$frontplotArea)
  
  checkEquals(0, calc.plot.dimensions(10, "No", NA, 0, 10)$frontplotDepth)
  checkEquals(0, calc.plot.dimensions(10, "No", NA, 0, 10)$frontplotArea)
}
