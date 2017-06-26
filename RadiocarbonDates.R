# Temporary. For use with rundata data frames that are produced from the Shiny
# app. Also, only useful with NEC runlog data files.

# -------------------------include libraries-----------------------------------

library(doBy)  # used for summary statistics on data.frames
library(SDMTools)  # weighted means and variance calculations
library(dplyr)
library(readr)

# -------------------------source files----------------------------------------

source("helperFunctions/numInputToIntegers.R")

# -------------------------define constants------------------------------------

kDeadTime <- 5.2E-6  # dead time of detector
kPileUp <- 3.5E-6  # pile up time constant for detector
kElemQ <- 1.6022E-19  # elementary charge
kVPDB <- 0.0112372  # Vienna PeeDee Belemnite standard 13C/12C ratio
kOX2ModernFactor <- 0.7459  # Modern = this * normalized 14C/12C of standards
kOX1ModernFactor <- 0.95  # OX1 modern, convert d13C (-19)

# -------------------------define Functions------------------------------------

NearestStdRuns <- function(run, data.set, sampletype) {
    # This function takes as input the run from which you wish to find the 
    # nearest standard runs and the data.frame you wish to work with. The 
    # function returns a vector containing the run numbers for the nearest 
    # standard measurements. It will find the nearest 6 standard runs, both 
    # forwards and back, or less if there are less than 6 standard runs. It 
    # will also exclude the run which is input in case the run you are 
    # searching from is a standard measurement itself. The output can be used 
    # to determine the correct normalization for any unknown sample or even a 
    # standard as the run being normalized will not be included in the 
    # calculation of the normalization thereby preventing self-normalized 
    # results.
    
    # get the run numbers for the standards assuming the data.frame has a RUN
    # column.
    std.runs <- data.set[data.set$smType == sampletype,]
    
    # use only active standard runs
    std.runs <- std.runs[std.runs$active,]
    
    # remove the run specified from the standard runs if it is a standard
    std.runs <- std.runs[std.runs$run != run, ]
    
    # find the date and time of the run specified
    run.time <- data.set[data.set$run == run, ]$dateTime
    
    # find the difference between the time of the run and all standard runs
    std.runs$TimeDiff <- abs(as.numeric(difftime(std.runs$dateTime, run.time)))
    
    std.runlist <- c()  # initialize a list to hold the runs
    
    # set loop count to number of rows in std.runs or 6 whichever is smaller
    if(nrow(std.runs) < 6) {
        count <- nrow(std.runs)
    }
    else {
        count = 6
    }
    
    # find nearest standard run then remove that run and repeat to count
    for(i in 1:count) {
        min.run <- std.runs[which.min(std.runs$TimeDiff), ]$run
        std.runlist <- c(std.runlist, as.character(min.run))
        std.runs <- std.runs[std.runs$run != min.run, ]
    }
    return(std.runlist)
}

Convertd13C <- function(c13.sample, c13.vpdb, bool = TRUE) {
    # This function takes the sample 13C/12C ratio or the sample d13C and the 
    # VPDB standard 13C/12C ratio along with a boolean operator. The function 
    # will either convert the sample's 13C/12C ratio to a d13C value if the 
    # boolean parameter is TRUE or it will convert the sample's d13C value to 
    # a 13C/12C ratio if the boolean parameter is FALSE. The default case is 
    # bool = TRUE to convert from a ratio to the d13C value. 
    
    if(bool) {
        return((c13.sample / c13.vpdb - 1) * 1000)
    }
    else {
        return((c13.sample / 1000 + 1) * c13.vpdb)
    }
    
}

C13Normalization <- function(run, data.set, d13C, slope, intercept) {
    # This function takes a run as input and finds all the standard runs as 
    # per the NearestStdRuns function then uses these to generate the 13C/12C 
    # normalization factor. The function returns a data.frame 
    # with the run which was input and the normalization factor contained. 
    
    std.C13 <- data.set$he13C[data.set$run==run] * slope + intercept
    C13norm <- Convertd13C(d13C, kVPDB, FALSE) / std.C13
    
    return(data.frame("run" = run, "C13norm" = C13norm))
    
}

wt.summary <- function(x, error){
    mn <- wt.mean(x, 1/error^2)
    stdev <- wt.sd(x, 1/error^2)
    return(list(mn,stdev))
}


# -------------------------main------------------------------------------------

# Remove all inactive runs from the data
run.data <- rundata %>% filter(active==TRUE)

# Which standard to use.
cat("Choose standard\n")
cat("Enter '1' for OX1\n")
cat("Enter '2' for OX2\n")
std.answer <- readline("Choice:")
if(std.answer=="1"){
    std.type <- "OX1"
} else if(std.answer=="2"){
    std.type <- "OX2"
} else {
    stop("Your answer was not recognised.")
}


# Get info about the blanks.
cat("Machine Blank\n")
cat("Enter '1' to choose positions for the machine blank.\n")
cat("Enter '2' to enter a known blank value.\n")
cat("Enter '3' for no machine blank correction.\n\n")
mb.answer <- readline("Choice:")
if(mb.answer=="1"){
    cat("Enter the positions for the machine blanks\n")
    mb.pos <- readline("Comma separated positions:")
    mb.pos <- numInputToIntegers(mb.pos)
    mb.ratio <- wt.summary(run.data[run.data$pos %in% mb.pos,]$he14.13,
                         run.data[run.data$pos %in% mb.pos,]$he14.13.error)[[1]]
} else if(mb.answer=="2"){
    cat("Enter the 14C/12C or 14C/13C ratio for the machine blank\n")
    mb.ratio <- as.numeric(readline("Ratio:"))
    mb.pos <- 1000  # Unrealistic position since there is presumably no MB.
} else if(mb.answer=="3"){
    mb.ratio <- 0
} else {
    stop("Your answer was not recognised.")
}

cat("\nChemical Blank\n")
cat("Enter '1' to choose positions for the chemical blank.\n")
cat("Enter '2' to enter a known chem blank value and error.\n")
cat("Enter '3' for no chemical blank correction.\n\n")
cb.answer <- readline("Choice:")
if(cb.answer=="1"){
    cb.pos <- readline("Comma separated positions:")
    cb.pos <- numInputToIntegers(cb.pos)
} else if(cb.answer=="2"){
    cb.ratio <- readline("Ratio:")
    cb.error <- readline("Error:")
} else if(cb.answer=="3"){
    cb.ratio <- 0
    cb.error <- 0
    cb.pos <- 1000  # Unrealistic position since there is presumably no CB.
} else {
    stop("Your answer was not recognised.")
}

# Setup new columns
run.data$std.runs <- NA
run.data$d13C <- NA
run.data$he14.13.mb <- NA
run.data$he14.13.mb.error <- NA
run.data$he14.13.d13C <- NA
run.data$he14.13.d13C.error <- NA
run.data$pMC <- NA
run.data$pMC.error <- NA

# Subtract the machine blank, but not from the machine blank.
run.data[!(run.data$pos %in% mb.pos),]$he14.13.mb <-
    run.data[!(run.data$pos %in% mb.pos),]$he14.13 - mb.ratio
run.data[run.data$pos %in% mb.pos,]$he14.13.mb <-
    run.data[run.data$pos %in% mb.pos,]$he14.13
run.data$he14.13.error.mb <- run.data$he14.13.error

# For each run, find the nearest standard runs and d13C values
for(i in 1:nrow(run.data)){
    # Find the nearest standards
    run <- run.data[i,]$run
    std.runlist <- NearestStdRuns(run, run.data, std.type)
    run.data[i,]$std.runs <- list(std.runlist)
    std.data <- run.data[run.data$run %in% std.runlist,]

    # Calculate normalized d13C results
    std.regression <- lm(std.data$he13.12 ~ std.data$he13C)
    reg.13.12 <- run.data[i,]$he13C * std.regression$coefficients[2] +
        std.regression$coefficients[1]
    if(std.type=="OX1"){std.d13C <- -19}
    if(std.type=="OX2"){std.d13C <- -17.8}
    norm.13.12 <- Convertd13C(std.d13C,kVPDB,FALSE) / reg.13.12
    smpl.13.12 <- run.data[i,]$he13.12 * norm.13.12
    run.data[i,]$d13C <- Convertd13C(smpl.13.12,kVPDB,TRUE)
    
    # Calculate d13C normalized 14C/13C ratios
    std.regression
}

# Calculate the d13C normalized 14C/13C ratios
run.data$he14.13.d13C <- run.data$he14.13.mb *
                         (-25/1000 + 1)/(run.data$d13C/1000 + 1)
run.data$he14.13.d13C.error <- run.data$he14.13.error.mb *
                         (-25/1000 + 1)/(run.data$d13C/1000 + 1)
if(std.type=="OX1"){
    run.data[run.data$smType=="OX1",]$he14.13.d13C <-
        run.data[run.data$smType=="OX1",]$he14.13.mb *
        (-19/1000 + 1)/(run.data[run.data$smType=="OX1",]$d13C/1000 + 1)
    run.data[run.data$smType=="OX1",]$he14.13.d13C.error <-
        run.data[run.data$smType=="OX1",]$he14.13.error.mb *
        (-19/1000 + 1)/(run.data[run.data$smType=="OX1",]$d13C/1000 + 1)
}

# Get the chemical blank value
if(cb.answer=="1"){
    cb.result <- wt.summary(run.data[run.data$pos %in% cb.pos,]$he14.13.d13C,
                         run.data[run.data$pos %in% cb.pos,]$he14.13.d13C.error)
    cb.ratio <- cb.result[[1]]
    cb.error <- cb.result[[2]]
}

# Subtract chemical blank and calculate the standard normalized pMC
if(std.type=="OX1"){ModernFactor <- kOX1ModernFactor}
if(std.type=="OX2"){ModernFactor <- kOX2ModernFactor}

for(i in 1:nrow(run.data)){
    std.runs <- run.data[i,]$std.runs[[1]]
    std.data <- run.data[run.data$run %in% std.runs,]
    std.results <- wt.summary(std.data$he14.13.d13C,
                              std.data$he14.13.d13C.error)
    
    if(!(run.data[i,]$pos %in% c(mb.pos, cb.pos))){
        run.data[i,]$pMC <- (run.data[i,]$he14.13.d13C - cb.ratio) /
            (std.results[[1]] - cb.ratio) / ModernFactor * 100
        
        run.data[i,]$pMC.error <- sqrt(
            (run.data[i,]$he14.13.d13C.error/(std.results[[1]]-cb.ratio) /
                 ModernFactor)^2 +
                (std.results[[2]]*(run.data[i,]$he14.13.d13C-cb.ratio) /
                     ModernFactor / (std.results[[1]]-cb.ratio)^2)^2 +
                (cb.error/cb.ratio)^2
        )
    } else{
        run.data[i,]$pMC <- (run.data[i,]$he14.13.d13C) /
            (std.results[[1]]) / ModernFactor * 100
        
        run.data[i,]$pMC.error <- sqrt(
            (run.data[i,]$he14.13.d13C.error/(std.results[[1]]) /
                 ModernFactor)^2 +
                (std.results[[2]]*(run.data[i,]$he14.13.d13C) /
                     ModernFactor / (std.results[[1]])^2)^2
        )
    }
}

# Compute final results
finalResults <- run.data %>% group_by(pos,analysis,label,smType) %>%
    summarize(he13C=mean(he13C),he13.12=mean(he13.12),trans12C=mean(trans12C),
              mn.d13C=mean(d13C),d13C.error=sd(d13C)/sqrt(n()-1),
              mn.pMC=mean(pMC),pMC.error=sd(pMC)/sqrt(n()-1)) %>%
    mutate(Age=round(-8033*log(mn.pMC/100),0),
           Age.error=round(8033*pMC.error/mn.pMC,0))


filename <- paste0(finalResults[1,]$analysis,"_finalResults.csv")
write_csv(finalResults,filename)
