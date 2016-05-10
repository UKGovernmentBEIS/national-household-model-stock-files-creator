library(RUnit)

setwd("/home/rich/software-projects/r-nhm-stock-creator/ehcs/");
test.suite <- defineTestSuite("example",
                              dirs = file.path("unitTests"),
                              testFileRegexp = 'Tests.R')

test.result <- runTestSuite(test.suite)
printTextProtocol(test.result)

