# This script duplicates the old Vitalea radiocarbon dating routine only using
# NEC data from a runlog file.

library(dplyr)
library(SDMTools)

#-----Define Constants---------------------------------------------------------

kElemQ <- 1.6022e-19  # elementrary charge in Coulombs
kVPDB <- 0.0112372  # Vienna PeeDee Belemnite standard 13C/12C ratio
kOX2ModernFactor <- 0.7459  # Modern = this * normalized 14C/12C of standards


#-----Source external functions------------------------------------------------

setwd("AMSanalysis/")
source("helperFunctions/NEC_runlog_to_dataframe.R")
source("helperFunctions/NearestStdRuns.R")


#-----define internal functions------------------------------------------------

rundataFromRunlog <- function(){
    # Get runlog from user
    if(.Platform$OS.type=="windows"){
        runFile <- choose.files(caption="Select runlog", multi=FALSE)
    } else {
        runFile <- file.choose()
    }
    
    # Get laboratory name from user
    labName <- tolower(readline("Enter the laboratory name: "))
    if(!(labName %in% c("accium","georgia","georgia250"))){
        stop("That laboratory is not in the system.")
    }
    
    # Get run data
    run.data <- NECrunlogToDF(file=runFile, lab=labName)
    
    # Add 'active' field and set all TRUE
    run.data$active <- TRUE
    
    # Convert dates to "YYYY-MM-DD HH:MM:SS TZ" notation (character class)
    run.data$Run.Completion.Time <- as.character(
        strptime(run.data$Run.Completion.Time, format="%c")
    )
    
    # Set 13C/12C to ratio not percentage
    run.data$he13.12 <- run.data$he13.12 / 100
    
    return(run.data)
    
    # NOTE: if the lab is not "accium" then sample types and labels have to be
    # added to run.data before proceeding further.
    
}

rundataFromDataFrame <- function(){
    # Get data frame with run data
    dfName <- readline("Enter the name of the data frame:")
    
    rundata <- NULL
    try(rundata <- get(dfName), silent=TRUE)
    if(is.null(rundata)){
        errorStr = paste("Data frame, ", dfName, ", does not exist.",sep="")
        stop(errorStr)
    }
    
    # Check if the active field exists and set it if not.
    if(is.null(rundata$active)){
        rundata$active <- TRUE
    }
    
    return(rundata)
}

#-----Main---------------------------------------------------------------------

# Prompt user for the source of the run data
cat("Please choose a source for the run data:\n")
cat(" 1 ---- runlog file\n")
cat(" 2 ---- existing data frame\n")
datasource <- as.numeric(readline(
    "Enter your choice:"
))

if(datasource==1){
    run.data <- rundataFromRunlog()
} else if(datasource==2){
    run.data <- rundataFromDataFrame()
} else {
    stop("Your answer was not understood. Now exiting the program.")
}

# Assign unique run identifyers if they don't exist
if(is.null(run.data$run)){
    run.data <- run.data %>% mutate(run=paste(Pos, Meas, sep="_"))
}

# obtain the machine blank value (he14.13)
mb.answer <- readline(
    "Enter 'a' to choose samples for the machine background or 'b' to enter
    a value:")
if(tolower(mb.answer) == "a"){
    mb.samples <- readline("Enter a comma separated list of all the machine
                           background ITNs (do not use any spaces): ")
} else if(tolower(mb.answer) == "b"){
    machine.blank <- as.numeric(readline(
        "Enter the corrC14toC12 value for the machine blank: "))
} else {
    stop("Your answer was not understood. Now exiting program.")
}

# obtain the process blank value (d25 corrected ratio) and error
pb.answer <- readline(
    "To choose samples enter 'a' or to enter a value for the process blank
  enter 'b': ")
if(tolower(pb.answer) == "a"){
    pb.samples <- readline("Enter a comma separated list of all the process
    blank ITNs (do not use any spaces): ")
} else if(tolower(pb.answer) == "b"){
    process.blank <- as.numeric(readline(
        "Enter the d25C14toC12 value for the process blank: "))
    process.blank.error <- as.numeric(readline(
        "Enter the errd25C14toC12 value for the process blank: "))
} else {
    stop("Your answer was not understood. Now exiting program.")
}

# Find C13 normalization and calculate d13C for each run.
# This is done by linear fit.
run.data$C13norm <- NA
active.stds <- run.data %>% filter(SmType=="OX2", active==TRUE)
std.regression <- lm(active.stds$he13.12 ~ active.stds$he13C)
std13C.intercept <- coef(std.regression)[[1]]
std13C.coef1 <- coef(std.regression)[[2]]

for(i in 1:nrow(run.data)){
    run.data[i,]$C13norm <- 
        (-17.8 / 1000 + 1) * kVPDB / 
        (run.data[i,]$he13C * std13C.coef1 + std13C.intercept)
}

run.data$norm13.12 <- run.data$he13.12 * run.data$C13norm
run.data$d13C <- (run.data$norm13.12/kVPDB - 1)*1000


# Subtract machine background
