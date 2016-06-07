install.packages("devtools")
library(devtools)

dependencies = list(
    foreign = "0.8-61",
    data.table = "1.9.6",
    dplyr = "0.4.1",
    Hmisc = "3.14-5",
    memisc = "0.97",
    splancs = "2.01-38",
    knitr = "1.11",
    plyr = "1.8.1",
    reshape = "0.8.5",
    tidyr = "0.2.0",
    rmarkdown = "0.8.1",
    classInt = "0.1-23",
    RUnit = "0.4.31",
    ggplot2 = "1.0.0",
    knitr = "1.12.3"
)

lapply(
    names(dependencies),
    FUN = function(name) {
        install_version(
            name,
            dependencies[name],
            type = "source"
        )
        dependencies[name]
    }
)
