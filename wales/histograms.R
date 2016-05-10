## Produces histograms comparing English and Welsh stocks across the properties we care about.

library(ggplot2);
library(plyr);

source("wales/constants.R");
source("wales/match-and-normalize.R", chdir = TRUE);
source("wales/histogram-helpers.R");

wales.compare <- function(column, welshIntentions, englishData, idsAndWeights) {
    ## Desired Welsh stock distribution.
    expected.data <- welshIntentions[,c(column, "weight")];

    ## Created Welsh stock distribution
    actual.data <- actual.distribution(column, englishData, idsAndWeights);
    actual.data[["weight"]] <- actual.data[[ehs.weightCol]];
    actual.data[[ehs.weightCol]] <- NULL;

    expected.data[["set"]] <- "expected";
    actual.data[["set"]] <- "actual";

    plots <- rbind(expected.data, actual.data);

    eval(
        substitute(
            qplot(category, weight = weight, fill = set, data = plots, position = position_dodge()),
            list(category = as.name(column))
        )
    );
};

wales.stock <- wales.load();

exportGraph <- function(column, welshIntentions, englishData, idsAndWeights) {
    target.path <- file.path(
        output.dir,
        "charts",
        paste(column, "png", sep = ".")
    );

    wales.compare(column, welshIntentions, englishData, idsAndWeights);
    ggsave(target.path);
};

dir.create(
    paste(output.dir, "charts", sep = "/"),
    showWarnings = FALSE
);

wales.data <- read.csv(
    wales.inputs,
    stringsAsFactors = FALSE
);

lapply(
    wales.stock$columns,
    exportGraph,
    wales.data,
    wales.stock$englishData,
    wales.stock$idsAndWeights
);

##report on data behind histograms
wales.exportTable <- function(englishData, idsAndWeights) {
  
  merge(
    idsAndWeights,
    englishData,
    by.x = ehs.idCol,
    by.y = ehs.idCol
  )
  
}


test.xtab <- function(actual.wales, expected.wales, actual.value, expected.value) {
  
  actual <- ddply(actual.wales, .(value = actual.value), summarise, actual.number.households = sum(dwellingcaseweight.x))
  expected <- ddply(expected.wales, .(value = expected.value), summarise, expected.number.households = sum(weight))

  combined <- join(expected, actual, by = "value")
  
  totals <- data.frame(value = "all.households",
                       expected.number.households = sum(expected$expected.number.households),
                       actual.number.households = sum(actual$actual.number.households))
  
  rbind(combined, totals)
  
}


test.builtform <- function(actual.wales, expected.wales, output.dir) {
  builtform <- test.xtab(actual.wales, expected.wales, actual.wales$builtformtype, expected.wales$builtformtype)
  write.csv(builtform, file.path(output.dir, "charts", "builtform.csv"), row.names = FALSE)
}

test.rurality <- function(actual.wales, expected.wales, output.dir) {
  rurality <- test.xtab(actual.wales, expected.wales, actual.wales$morphologytype, expected.wales$morphologytype)
  write.csv(rurality, file.path(output.dir, "charts", "morphology.csv"), row.names = FALSE)
}

test.tenuretype <- function(actual.wales, expected.wales, output.dir) {
  tenuretype <- test.xtab(actual.wales, expected.wales, actual.wales$tenuretype, expected.wales$tenuretype)
  write.csv(tenuretype, file.path(output.dir, "charts", "tenuretype.csv"), row.names = FALSE)
}

test.numofbedrooms <- function(actual.wales, expected.wales, output.dir) {
  numofbedrooms <- test.xtab(actual.wales, expected.wales, actual.wales$numofbedrooms, expected.wales$numofbedrooms)
  write.csv(numofbedrooms, file.path(output.dir, "charts", "numofbedrooms.csv"), row.names = FALSE)
}

test.walls <- function(actual.wales, expected.wales, output.dir) {
  walls <- test.xtab(actual.wales, expected.wales, actual.wales$externalwallconstructiontype, expected.wales$externalwallconstructiontype)
  write.csv(walls, file.path(output.dir, "charts", "externalwallconstructiontype.csv"), row.names = FALSE)
}

test.loftins <- function(actual.wales, expected.wales, output.dir) {
  loftins <- test.xtab(actual.wales, expected.wales, actual.wales$insulationthickness, expected.wales$insulationthickness)
  write.csv(loftins, file.path(output.dir, "charts", "loftinsulationthickness.csv"), row.names = FALSE)
}

test.children <- function(actual.wales, expected.wales, output.dir) {
  children <- test.xtab(actual.wales, expected.wales, actual.wales$children, expected.wales$children)
  write.csv(children, file.path(output.dir, "charts", "children.csv"), row.names = FALSE)
}

test.hasdisabledorsickoccupant <- function(actual.wales, expected.wales, output.dir) {
  disabled <- test.xtab(actual.wales, expected.wales, actual.wales$hasdisabledorsickoccupant, expected.wales$hasdisabledorsickoccupant)
  write.csv(disabled, file.path(output.dir, "charts", "hasdisabledorsickoccupant.csv"), row.names = FALSE)
}

test.buildyear <- function(actual.wales, expected.wales, output.dir) {
  
  buildyear <- test.xtab(actual.wales, 
                   expected.wales, 
                   gsub("\\(([0-9]*)\\,([0-9]*)\\]"
                        ,cut(actual.wales$buildyear, seq(1625,2025,by = 25), dig.lab = 4)
                        ,replacement="\\1 - \\2"),
                   gsub("\\(([0-9]*)\\,([0-9]*)\\]"
                        ,cut(expected.wales$buildyear, seq(1625,2025,by = 25), dig.lab = 4)
                        ,replacement="\\1 - \\2"))
  
  write.csv(buildyear, file.path(output.dir, "charts", "buildyear.csv"), row.names = FALSE)
}

test.income <- function(actual.wales, expected.wales, output.dir) {
  
  income <- test.xtab(actual.wales, 
                   expected.wales, 
                   cut(actual.wales$householdincomebeforetax, seq(-10000,1000000,by = 10000), dig.lab = 6),
                   cut(expected.wales$householdincomebeforetax, seq(-10000,1000000,by = 10000), dig.lab = 6))
  
  write.csv(income, file.path(output.dir, "charts", "householdincomebeforetax.csv"), row.names = FALSE)
}

test.hrpage <- function(actual.wales, expected.wales, output.dir) {
  
  hrpage <- test.xtab(actual.wales, 
                   expected.wales, 
                   gsub("\\(([0-9]*)\\,([0-9]*)\\]"
                        ,cut(actual.wales$chiefincomeearnersage, seq(16,100,by = 12), dig.lab = 4)
                        ,replacement="\\1 - \\2"),
                   gsub("\\(([0-9]*)\\,([0-9]*)\\]"
                        ,cut(expected.wales$chiefincomeearnersage, seq(16,100,by = 12), dig.lab = 4)
                        ,replacement="\\1 - \\2"))
  
  write.csv(hrpage, file.path(output.dir, "charts", "chiefincomeearnersage.csv"), row.names = FALSE)
}


produce.frequency.tables <- function(wales.data, wales.stock, output.dir) {

new.wales.stock.data <- wales.exportTable(wales.stock$englishData,wales.stock$idsAndWeights)

test.walls(new.wales.stock.data, wales.data, output.dir)
test.hrpage(new.wales.stock.data, wales.data, output.dir)
test.income(new.wales.stock.data, wales.data, output.dir)
test.buildyear(new.wales.stock.data, wales.data, output.dir)
test.hasdisabledorsickoccupant(new.wales.stock.data, wales.data, output.dir)
test.children(new.wales.stock.data, wales.data, output.dir)
test.loftins(new.wales.stock.data, wales.data, output.dir)
test.numofbedrooms(new.wales.stock.data, wales.data, output.dir)
test.rurality(new.wales.stock.data, wales.data, output.dir)
test.builtform(new.wales.stock.data, wales.data, output.dir)
test.tenuretype(new.wales.stock.data, wales.data, output.dir)

}

produce.frequency.tables(wales.data, wales.stock, output.dir)