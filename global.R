# Global file for storing functions and objects which will be accessable to both
# the server.R and ui.R functions accross all user sessions. If an object or
# function needs to be separate between user sessions (the rundata data frame
# maybe) then it should be called from within the shinyServer() function in the
# server.R file. The point of this global file is so that all the utility
# functions and global objects are loaded once for the Shiny app and do not need
# to be loaded for each session or call.

library(SDMTools)
library(tidyverse)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(DT)


# Call utility functions
    utilPath <- "helperFunctions"  # path to utility function directory
    source(file.path(utilPath, "BioMICADAStoRunData.R"))
    source(file.path(utilPath, "KIRAMStoRunData.R"))
    source(file.path(utilPath, "NECtoRunData.R"))
    source(file.path(utilPath, "numInputToIntegers.R"))
    source(file.path(utilPath, "NearestStdRuns.R"))
    source(file.path(utilPath, "smallUtilityFuncs.R"))
    
# Call Shiny modules
    modulePath <- "modules"  # path to shiny module files
    source(file.path(modulePath, "runlogFileInput.R"))
    source(file.path(modulePath, "runlogFile.R"))
    source(file.path(modulePath, "runPlotUI.R"))
    source(file.path(modulePath, "runPlot.R"))
    source(file.path(modulePath, "standardsInput.R"))
    source(file.path(modulePath, "standards.R"))

# Define global objects
    # rundata <- NA, this is not instatiated here because I don't want this to
    # be a shared object across user sessions. It needs to be separate for each
    # user session. That way two or more people can work on an analysis at the
    # same time without clashing with each other. Then at the end of an analysis
    # the rundata will be written to a file or, better, to a database.
    
    # define constants to be used in analysis
    kElemQ <- 1.6022E-19  # elementary charge
    kVPDB <- 0.0112372  # Vienna PeeDee Belemnite standard 13C/12C ratio
    kOX2ModernFactor <- 0.7459  # Modern = this * normalized 14C/12C of standards
    kOX1ModernFactor <- 0.95  # OX1 modern, convert d13C (-19)
    
    bioMICADASDeadTime <- 5.2E-6  # dead time of detector
    bioMICADASPileUp <- 3.5E-6  # pile up time constant for detector
    
    KIRAMSdeadtime <- 13  # dead time of detector in microseconds
    
    # data frame of nominal values for primary and secondary standards
    nomVals <- data.frame(
        "name" = c("OX1","OXI","OX2","OXII","ANU","C6","C1","C2","C3","C4","C5",
                   "C7","C8","C9"),
        "pMC" = c(103.98,103.98,134.07,134.07,150.61,150.61,0,41.14,129.41,0.32,
                  23.05,49.53,15.03,0.165),
        "d13C" = c(-19.0,-19.0,-17.8,-17.8,-10.8,-10.8,2.42,-8.25,-24.91,-23.96,
                   -25.49,-14.48,-18.31,-23.9)
    )
