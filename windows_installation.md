# Installing R and running the creator script
Currently the majority of those using the stock creator scripts have been doing so via R Studio on Windows, the following outlines how to install R Studio and run the scripts.

# Download and install the following software to your PC
1. [R](https://www.r-project.org/) - The core R application
2. [Mitex](http://www.miktex.org/) - Provides libraries to display pdf documents detailing code and testing results
3. [Java](https://www.java.com/en/download/) - You will need a version of Java installed, the location of the installed java location 'bin' directory should be added to your PATH variable [Windows PATH](https://support.microsoft.com/en-us/kb/310519)
4. [RTools](https://cran.r-project.org/bin/windows/Rtools/) - Again the location of the RTools 'bin' directory should be added to your windows path
5. [R Studio](https://www.rstudio.com/) - A simple user interface for editing and running R scripts

# Installing R-Packages
The current stock creator R scripts require a number of packages that do not come bundled with the standard R installation these need to be installed.

1. Start up R studio
2. Set your working directory to the location you have downloaded this repository e.g. setwd("C:/national-household-model-stock-files-creator")
3. Run the install dependencies script by typing source("install-dependencies.R"), you should see details of installation in the console window, this may take a while to complete, typically 5-10mins depending on your internet connection
4. If no errors occured above you can proceed to the next section




