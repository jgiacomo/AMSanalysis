# Global file for storing functions and objects which will be accessable to both
# the server.R and ui.R functions accross all user sessions. If an object or
# function needs to be separate between user sessions (the rundata data frame
# maybe) then it should be called from within the shinyServer() function in the
# server.R file. The point of this global file is so that all the utility
# functions and global objects are loaded once for the Shiny app and do not need
# to be loaded for each session or call.

# Call utility functions
    utilPath <- "helperFunctions"  # path to utility function directory
    source(file.path(utilPath, "BioMICADAStoRunData.R"))
    source(file.path(utilPath, "NECtoRunData.R"))
    source(file.path(utilPath, "numInputToIntegers.R"))
    source(file.path(utilPath, "NearestStdRuns.R"))
    source(file.path(utilPath, "smallUtilityFuncs.R"))

# Define global objects
    # rundata <- NA, this is not instatiated here because I don't want this to
    # be a shared object across user sessions. It needs to be separate for each
    # user session. That way two or more people can work on an analysis at the
    # same time without clashing with each other. Then at the end of an analysis
    # the rundata will be written to a file or, better, to a database.
    
    # define constants to be used in analysis
    kElemQ <- 1.6022E-19  # elementary charge
    kVPDB <- 0.0112372  # Vienna PeeDee Belemnite standard 13C/12C ratio
    kOX2ModernFactor <- 0.7459 # Modern = this * normalized 14C/12C of standards
    

# Call shiny modules
    
    