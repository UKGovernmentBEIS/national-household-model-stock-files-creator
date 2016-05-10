library(RUnit)
source("~/software-projects/r-nhm-stock-creator/ehcs/stories.R", chdir=T);

test.has.basment <- function(){
  checkEquals(FALSE, has.basement(NA))
  
  checkEquals(TRUE, has.basement("BB"))
  checkEquals(TRUE, has.basement("bb"))
  
  checkEquals(TRUE, has.basement("-1"))
}

test.num.of.floors <- function(){
  checkEquals(2, actual.num.floors(1, TRUE))
  checkEquals(1, actual.num.floors(1, FALSE))
  checkEquals(0, actual.num.floors(NA, FALSE))
  checkEquals(0, actual.num.floors(NA, NA))
}