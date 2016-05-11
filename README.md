# Synopsis
A series of R scripts that can be used to create structured zip file known as the 'Default Import Package' that can be imported by the [National Household Model](https://github.com/cse-bristol/national-household-model-standalone) as a housing stock. For more information on the content and structure of files within the 'Default Import Package' please see the Stock section within the [NHM documentation](https://github.com/cse-bristol/national-household-model-documentation/releases).

TODO - Some blurb about the source files

# Installation
The scripts for creating the stock files have been written in [R](https://cran.r-project.org/), which will need to be downloaded and installed.

[Installation for Windows](windows_installation.md)

# Running the script
1. Ensure you have downloaded the required source survey files as previously mentioned.
2. Start the R command line application or R studio
3. Switch your workspace to the location you have downloaded this repository too
4. Execute the stock creation scripts by entering 'source("main.R")'.
5. Check the console for the status of the import, if succesful and output dirtory should be created your workspace directory, this should include sub directories for scotland, england and wales and uk. Each of these sub-directories should include the structured csv files and a zip file which can be imported into the NHM standalone as a stock.

# External Project Dependencies
We cannot distributre the source data files for the Housing Condition survey files the R scripts build the stock files, you will need to obtain these from the the following locations.

1. English Housing Condition Survey - TODO
2. Scottish Housing Condition Survey - TODO

# License
[Open Government License] (http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)
