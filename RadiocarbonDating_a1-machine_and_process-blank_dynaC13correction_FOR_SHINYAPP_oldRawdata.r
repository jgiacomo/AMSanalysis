# R-script
#
# Copyright © 2013 Eckert & Ziegler Vitalea Science, Inc.
#
# Author: Jason Giacomo, Ph.D.
#
# Description:
# This program is designed to take BioMICADAS data from the rawdata view of the
# database of a magazine measured for radiocarbon dating results. The program
# will query the database for the data and perform calculations to determine
# the conventional radiocarbon date for each unknown sample. Results of the
# program are written to an excel spreadsheet where additional analyses can be
# performed if needed. A full discussion of the calculations is too long for
# this comment and will be provided as a separate document.
#
# Version: a1 (first alpha)
#


# -------------------------include libraries-----------------------------------

library(doBy)  # used for summary statistics on data.frames
library(RODBC)  # used for database access
library(SDMTools)  # weighted means and variance calculations
library(dplyr)

# -------------------------define constants------------------------------------

kDeadTime <- 5.2E-6  # dead time of detector
kPileUp <- 3.5E-6  # pile up time constant for detector
kElemQ <- 1.6022E-19  # elementary charge
kVPDB <- 0.0112372  # Vienna PeeDee Belemnite standard 13C/12C ratio
kOX2ModernFactor <- 0.7459  # Modern = this * normalized 14C/12C of standards

# -------------------------define Functions------------------------------------

DataFromCSV <- function() {
  # This function will pull in measurement data from a csv file.
  
  magazine <- readline("Enter the magazine name:")
  data.run <- read.csv(file = file.choose(new = FALSE))
  # check to see if `active` is already in the csv data, create it if not
  if(is.null(data.run$active)){
      data.run$active <- TRUE
  }
  return(data.run)
}

GetData <- function() {
  # This function opens a connection via DSN on Windows to the database using
  # the ODBC drivers for MySQL. Then it runs the given SELECT query to pull the
  # needed information from the database. The SELECT query takes the Magazine
  # name as input from the user. The recordset is then imported into a
  # data.frame and the connection is closed.
  
  dsn <- "BioMICADAS"  # specify the data source name (DSN) to use
  
  # check if the DSN appears in the list of data sources on the system
  if(length(grep(dsn, names(odbcDataSources()))) < 1) {
    stop("The data source name, ", dsn, ", is not found on your system.\n",
         "  Please contact the maintainer of this program for assistance.\n")
  }
  
  tryCatch({
    BioMICADAS <- odbcConnect(dsn) # opens the database connection
  }, warning = function(war) {
    cat("A warning was generated when connecting to the database.\n")
    cat(as.character(war), "\n")
    stop("The program was stopped because a warning was generated when trying
          to connect to the database.", call. = FALSE)
  }, error = function(ex) {
    cat("An error ocurred when connecting to the database.\n")
    cat(as.character(ex), "\n")
    stop("The program has stopped because an error was generated when trying
          to connect to the database.", call. = FALSE)
  }, finally = {
    
  })
  
  magazine <- readline("Enter the magazine name:")
  query <- paste("SELECT Label, Run, samptype, MAX(timedat) AS DateTime,
                  SUM(SCA1) AS SCA1, SUM(SCA2) AS SCA2, 
                  SUM(SCA3) AS SCA3, SUM(SCA5) AS SCA5, SUM(SCA6) AS SCA6, 
                  SUM(SCA5) / SUM(SCA6) * 1E6 AS rawC14rate, 
                  400 * SUM(SCA2) / SUM(SCA6) AS	C12, 
                  4 * SUM(SCA3) / SUM(SCA6) AS C13, 
                  1000 * SUM(SCA1) / SUM(SCA6) AS C12L
                  FROM `old_rawdata` 
                  WHERE `MAGAZINE` = '", magazine, "' AND `cycltrue` IS NULL 
                  GROUP BY `Run` 
                  ORDER BY `Label`, `Run`;", sep="")
                  
  tryCatch({
    run.data <- sqlQuery(BioMICADAS, query)
  }, error = function(ex) {
    cat("An error ocurred while executing the database query.\n")
    print(as.character(ex))
    stop("An error caused the program to stop.")
  }, finally = {
    # release resources if they exist
    if(tryCatch({odbcGetInfo(BioMICADAS);TRUE},error=function(...)FALSE)) {
      odbcClose(BioMICADAS)  # closes the datbase connection
    }
    if(exists("magazine")) {
      rm(magazine)
    }
    if(exists("query")) {
      rm(query)
    }
	}
  )
  
  if(exists("run.data") & nrow(run.data) > 0) {
    run.data$active <- TRUE
    return(run.data)
  }
  else {
    stop("\nThe program was stopped because no data could be retreived from
          the database. Check the magazine name.\n")
  }

}

DataFromDataFrame <- function(df){
    run.data <- df[ , c("LABEL", "RUN", "samptype", "DateTime", "SCA1", "SCA2",
                        "SCA3", "SCA5", "SCA6", "rawC14rate", "C12", "C13",
                        "C12L")]
    if(is.null(df$active)){
        run.data$active <- TRUE
    } else {
        run.data$active <- df$active
    }
    
    return(run.data)
}

RateCorrection <- function(c14.counts, count.time, kDeadTime, kPileUp) {
  # This function takes as input the number of 14C counts from an AMS 
  # measurement and the counting time (as long integers or doubles) and the 
  # dead time and pile up time for the 14C detector (double). The function 
  # then attempts to calculate the corrected detection rate based on the 
  # provided dead time and pile up time. This is specific to the BioMICADAS 
  # and could change if the detector operation or mechanics are modified. 

  raw.rate <- c14.counts / count.time * 1E6  # the uncorrected rate
  dead.time <- raw.rate / (1 - raw.rate * kDeadTime)  # dead time correction
  pile.time <- dead.time * exp(2 * kPileUp * dead.time)  # pile up correction

  return(pile.time)

}


Transmission <- function(SCA1, SCA2) {
  # This function takes the scalar values for the C12 and C12L integrators 
  # and calculates the transmission as a percentage of the ratio C12/C12L. 

  return(100 * 400 * SCA2 / (1000 * SCA1))
}


RawR <- function(corr.C14rate, C12, kElemQ) {
  # This function takes the corrected count rate and 12C current results along 
  # with the elementary charge in coloumbs and outputs the raw (but dead 
  # time corrected) 14C/12C ratio. 
  
  return(corr.C14rate / (C12 * 1E-6 / kElemQ))
  
}


RawS <- function(C13, C12) {
  # This function takes in the 13C and 12C results and returns the 
  # un-normalized 13C/12C ratio. 
  
  return(C13 / C12)
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
  std.runs <- data.set[data.set$samptype == sampletype,]
  
  # use only active standard runs
  std.runs <- std.runs[std.runs$active,]
  
  # remove the run specified from the standard runs if it is a standard
  std.runs <- std.runs[std.runs$RUN != run, ]
  
  # find the date and time of the run specified
  run.time <- data.set[data.set$RUN == run, ]$DateTime
  
  # find the difference between the time of the run and all standard runs
  std.runs$TimeDiff <- abs(as.numeric(difftime(std.runs$DateTime, run.time)))
  
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
    min.run <- std.runs[which.min(std.runs$TimeDiff), ]$RUN
    std.runlist <- c(std.runlist, as.character(min.run))
    std.runs <- std.runs[std.runs$RUN != min.run, ]
  }
  return(std.runlist)
}


C13Normalization <- function(run, data.set, d13C, slope, intercept) {
  # This function takes a run as input and finds all the standard runs as 
  # per the NearestStdRuns function then uses these to generate the 13C/12C 
  # normalization factor. The function returns a data.frame 
  # with the run which was input and the normalization factor contained. 
  
  std.C13 <- data.set$C12[data.set$RUN==run] * slope + intercept
  C13norm <- Convertd13C(d13C, kVPDB, FALSE) / std.C13
  
  return(data.frame("RUN" = run, "C13norm" = C13norm))
  
}

ModernNormalization <- function(run, data.set, kOX2ModernFactor) {
  # This function takes a run as input and finds all the standard runs as 
  # per the NearestStdRuns function then uses these to generate the modern 
  # normalization factor. The function also calculates the internal and
  # external error of the modern normalization. The function returns a
  # data.frame with the run which was input, the normalization factor, and
  # the larger of the external or internal error. 
  
  # find mean modern ratio from standards
  std.runlist <- NearestStdRuns(run, data.set, "S")
  std.C14mean <- mean(data.set[data.set$RUN %in% std.runlist, ]$d25C14toC12)
  modern.norm <- kOX2ModernFactor * std.C14mean
  
  # find standard deviation of modern ratio from standards
  std.C14stddev <- sd(data.set[data.set$RUN %in% std.runlist, ]$d25C14toC12)
  modern.stddev <- kOX2ModernFactor * std.C14stddev
  
  # find number of standards used in normalization
  number.standards <- nrow(data.set[data.set$RUN %in% std.runlist, ])
  
  # calculate internal error
  interr.standards <- kOX2ModernFactor *
    sqrt(sum(data.set[data.set$RUN %in% std.runlist, ]$errd25C14toC12^2)) /
    number.standards
  
  # calculate external error
  exterr.standards <- modern.stddev / sqrt(number.standards)
  
  # set error in standard normalization to larger of external or internal
  err.standards <- max(interr.standards, exterr.standards)
  
  return(data.frame("RUN" = run, "Modern" = modern.norm,
                    "ErrorInModern" = err.standards))
  
}


C14d13Ccorr <- function(C14toC12, d13C) {
  # This function applies the d13C normalization to the 14C/12C ratio. The 
  # inputs are the dead time corrected 14C/12C ratio and the standard 
  # normalized d13C values. The function returns the normalized 14C/12C 
  # ratio. 
  
  return(C14toC12 * ((-25 / 1000 + 1) / (d13C / 1000 + 1))^2)
  
}


error.bar <- function(x, y, upper, lower=upper, length=0.1, ...) {
  # This function takes the x, y coordinates of your data points and a upper 
  # and lower error and uses the arrow function to draw y error bars in your 
  # plot. The default for the lower error bar is to equal the upper error 
  # bar so that if you have symmetric error you only need specify the error 
  # vector once. The length parameter provides the length of the 
  # perpendicular line at the end of the error bar (the arrow head) and its 
  # default is 0.1. The output of this function is to plot y error bars on 
  # an existing plot of your data. 

  # check if the vectors are the same length and exit if not
  if(length(x) != length(y) | length(y) != length(lower) | length(lower) !=
    length(upper)) {
      stop("Vectors in error.bar must be of the same length")
  }
  
  arrows(x, y + upper, x, y - lower, angle=90, code=3, length=length, ...)
  
}


run.plot <- function(data.set) {
  # This function will plot all the run fMC data for the data.set specified. 
  # The assumption is that the data.set is a data.frame with a RUN and fMC 
  # column. In typical usage the data.set corresponds to the runs for a 
  # single sample. The plot generated will display the error bars of each 
  # data point and the standard deviation (sd), 2sd, and 3sd lines. This is 
  # for performing a quick outlier test. If any data point is outside of the 
  # 3sd line (including its error bars) then it may be considered an outlier 
  # and may be removed from the calculations. The output of this function is 
  # a plot. 

  data.mean <- mean(data.set$fMC)
  data.sd <- sd(data.set$fMC)
  # begin plot, set y axis range to +/- 4*sd, do not plot x axis
  plot(c(1:nrow(data.set)), data.set$fMC, xlim = c(0, nrow(data.set) + 1),
    ylim = c(data.mean - 4 * data.sd, data.mean + 4 * data.sd), pch=16,
    xaxt="n", main=as.character(data.set$LABEL[1]), xlab=" ", ylab="fMC")
  # add sd lines
  abline(h=data.mean)
  abline(h=c(data.mean+data.sd, data.mean-data.sd), col="blue")
#  abline(h=data.mean-data.sd, col="blue")
  abline(h=c(data.mean+2*data.sd, data.mean-2*data.sd), col="green")
#  abline(h=data.mean-2*data.sd, col="green")
  abline(h=c(data.mean+3*data.sd, data.mean-3*data.sd), col="red")
#  abline(h=data.mean-3*data.sd, col="red")
  
  # add error bars
  error.bar(c(1:nrow(data.set)), data.set$fMC, data.set$fMCerror, length=0.05)
  
  # add run labels to x axis
  axis(1, at=c(1:nrow(data.set)), labels=data.set$RUN, las=2)

}

outliers <- function(data.set, label, sigma = 3) {
  # This function will take the run data and a sample label as input and find
  # the modern values for the run and check if they are outside the given
  # sigma value. If they are then it will return the run which is an outlier.
  
  runs <- data.set[data.set$LABEL==label,]
  runs.mean <- wt.mean(runs$fMC, 1/runs$fMCerror^2)
  runs.sd <- wt.sd(runs$fMC, 1/runs$fMCerror^2)
  
  for (i in 1:nrow(runs)) {
    test = 0
    if((runs$fMC[i] > runs.mean + sigma*runs.sd) | 
      (runs$fMC[i] < (runs.mean - sigma*runs.sd))) {
      print(paste("Sample: ", label, ", Run: ", runs$RUN[i],
        " is an outlier.", sep=""))
      test = test + 1
    }
  }
  if(test == 0){
  print(paste("No outliers were found for sample ", label, sep=""))
  }
}


#-----------------------------Begin Program------------------------------------
# retrieve data from database or csv file
datasource <- as.numeric(readline(
  "Enter 1 to retrieve data from the database,2 from a CSV file, or 3 from a data frame: "))

if(datasource==1){
  run.data <- GetData()  # retrieve basic run data from the database.
} else if(datasource==2){
    run.data <- DataFromCSV()
} else if(datasource==3){
    dfname <- readline("Enter the name of the data frame: ")
    run.data <- get(dfname)
    run.data <- DataFromDataFrame(run.data)
} else {
    stop("Your answer was not understood. Now exiting program.")
}

# obtain the machine blank value (corrC14toC12)
mb.answer <- readline(
  "Enter 'a' to choose samples for the machine background or 'b' to enter
  a value:")
if(tolower(mb.answer) == "a"){
  mb.samples <- readline("Enter a comma separated list of all the machine
    background ITNs (do not use any spaces): ")
} else if(tolower(mb.answer) == "b"){
    machine.blank <- as.numeric(readline(
      "Enter the corrC14toC12 value for the process blank: "))
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

# calculate the non-normalized run results
run.data$transmission <- Transmission(run.data$SCA1, run.data$SCA2)
run.data$corrC14rate <- RateCorrection(run.data$SCA5, run.data$SCA6,
  kDeadTime, kPileUp)
run.data$corrC14toC12 <- RawR(run.data$corrC14rate, run.data$C12, kElemQ)
run.data$rawC13toC12 <- RawS(run.data$C13, run.data$C12)

# find the C13 normalization and calculate normalized d13C
run.data["C13norm"] <- NA  # initialize column for C13 normalization factor
# use only active standard runs
active.stds <- run.data[run.data$active,]
std.regression <- lm(active.stds$rawC13toC12[active.stds$samptype=='S'] ~
                     active.stds$C12[active.stds$samptype=='S'])
std.intercept <- std.regression$coefficients[1]
std.slope <- std.regression$coefficients[2]

for(i in 1:nrow(run.data)) {
  new.data <- C13Normalization(run.data[i, ]$RUN, run.data, -17.8, std.slope,
    std.intercept)
  run.data[i, ]$C13norm <- new.data$C13norm
  rm(new.data)
}
rm(i)  # clean up

run.data$normC13toC12 <- run.data$rawC13toC12 * run.data$C13norm
run.data$d13C <- Convertd13C(run.data$normC13toC12, kVPDB, TRUE)

# subtract machine background
if(exists("mb.samples")){
  if(mb.samples != ""){
    mb.samples <- strsplit(mb.samples,',')[[1]]
    machine.blank <- mean(
      run.data[run.data$LABEL %in% mb.samples,]$corrC14toC12)
  } else {
      machine.blank <- 0
  }
}

run.data$mb.C14toC12 <- run.data$corrC14toC12 - machine.blank

# calculate d13C = -25 per mil 14C/12C correction
run.data$d25C14toC12 <- C14d13Ccorr(run.data$mb.C14toC12, run.data$d13C)

# calculate error in d25C14toC12
run.data$errd25C14toC12 <- run.data$d25C14toC12 / sqrt(run.data$corrC14rate *
  run.data$SCA6 * 1E-6)

# calculate the process blank d25C14toC12 and error
if(exists("pb.samples")){
  if (pb.samples != ""){
    pb.samples <- strsplit(pb.samples,',')[[1]]
    process.blank <- wt.mean(run.data[run.data$LABEL %in% pb.samples,]$d25C14toC12,
      1/run.data[run.data$LABEL %in% pb.samples,]$errd25C14toC12^2)
    process.blank.error <- wt.sd(
      run.data[run.data$LABEL %in% pb.samples,]$d25C14toC12,
      1/run.data[run.data$LABEL %in% pb.samples,]$errd25C14toC12^2) /
      sqrt(nrow(run.data[run.data$LABEL %in% pb.samples,]))
  } else {
      process.blank <- 0
      process.blank.error <- 0
  }
}

# find the Modern value by standard normalization
run.data["Modern"] <- NA # initialize column for modern factor
run.data["ErrInModern"] <- NA  # initialize column for modern factor error

for(i in  1:nrow(run.data)) {
  new.data <- ModernNormalization(run.data[i, ]$RUN, run.data,
                                  kOX2ModernFactor)
  run.data[i, ]$Modern <- new.data$Modern
  run.data[i, ]$ErrInModern <- new.data$ErrorInModern
  rm(new.data)
}
rm(i)  # clean up

# find process blank corrected fMC using Modern factor just obtained
run.data$fMC <- (run.data$d25C14toC12 - process.blank) /
                (run.data$Modern/kOX2ModernFactor - process.blank) /
                kOX2ModernFactor

# find error in process blank corrected fMC value
run.data$fMCerror <- sqrt(
    (run.data$errd25C14toC12 / (run.data$Modern / kOX2ModernFactor -
    process.blank))^2 +
    ((process.blank - run.data$d25C14toC12) / (run.data$Modern /
    kOX2ModernFactor - process.blank)^2 * run.data$ErrInModern /
    kOX2ModernFactor)^2 +
    ((run.data$d25C14toC12 - run.data$Modern / kOX2ModernFactor) /
    (run.data$Modern / kOX2ModernFactor - process.blank)^2 *
    process.blank.error)^2)
    
# find fMC without process blank correction (fMC_noPB)
run.data$fMC_noPB <- run.data$d25C14toC12 / run.data$Modern

run.data$fMC_noPB.error <- run.data$fMC_noPB * sqrt(
  (run.data$errd25C14toC12/run.data$d25C14toC12)^2 +
  (run.data$ErrInModern/run.data$Modern)^2)

# run.data$fMCerror <- run.data$fMC * sqrt((run.data$errd25C14toC12 /
  # run.data$d25C14toC12)^2 + (run.data$ErrInModern / run.data$Modern)^2)

# generate mean sample data from only active runs
final.data <- summaryBy(C12 + transmission + d13C ~ LABEL,
                        data=run.data[run.data$active,],
                        FUN=function(x){ c(mean=mean(x)) }
                        )

# calculate error in d13C and merge with final.data
final.data <- merge(final.data, summaryBy(d13C ~ LABEL,
                        data=run.data[run.data$active,],
                        FUN=function(x){ c(error=sd(x)/sqrt(length(x))) }),
                        by="LABEL"
                    )

# change LABEL from factor to character class
final.data$LABEL <- as.character(final.data$LABEL)

# find error weighted average of fMC, internal error, and external error
# of the weighted mean result
final.data$fMC.WeightedMean <- NA  # initialize column
final.data$fMC.int.error <- NA  # initialize column
final.data$fMC.ext.error <- NA  # initialize column
final.data$fMC.error <- NA  # initialize column
final.data$fMC_noPB <- NA  # initiate column
final.data$fMC_noPB.error <- NA  # initiate column

# split the data by labels, only active runs
split.run.data <- splitBy(formula=~LABEL, data=run.data[run.data$active,])

# calculate fMC, int error and ext error by iterating over each label
for(i in 1:nrow(final.data)) {
  final.data[i, ]$fMC.WeightedMean <- 
    wt.mean(split.run.data[[final.data[i, ]$LABEL]]$fMC,
      1 / (split.run.data[[final.data[i, ]$LABEL]]$fMCerror)^2)

  final.data[i, ]$fMC.int.error <-  # internal error calculation
    sqrt(1 / (sum(1 / (split.run.data[[final.data[i, ]$LABEL]]$fMCerror)^2)))

  final.data[i, ]$fMC.ext.error <-  # external error calculation
    wt.sd(split.run.data[[final.data[i, ]$LABEL]]$fMC,
      1 / (split.run.data[[final.data[i, ]$LABEL]]$fMCerror)^2) / 
        sqrt(nrow(split.run.data[[final.data[i, ]$LABEL]]))

  # find larger of the internal or external error
  final.data[i, ]$fMC.error <- max(final.data[i, ]$fMC.int.error,
    final.data[i, ]$fMC.ext.error)
  
  final.data[i, ]$fMC_noPB <- 
    wt.mean(split.run.data[[final.data[i, ]$LABEL]]$fMC_noPB,
      1/(split.run.data[[final.data[i, ]$LABEL]]$fMC_noPB.error))
  
  final.data[i, ]$fMC_noPB.error <-
    final.data[i, ]$fMC.error /
    final.data[i, ]$fMC.WeightedMean *
    final.data[i, ]$fMC_noPB
}
rm(i)  # clean up
rm(split.run.data)  # clean up

# calculate age and error
final.data$Age <- -8033 * log(final.data$fMC.WeightedMean)
final.data$Age.error <- abs(-8033 * 1 / final.data$fMC.WeightedMean) * 
  final.data$fMC.error
final.data$Age_noPB <- -8033 * log(final.data$fMC_noPB)
final.data$Age_noPB.error <- abs(-8033 * 1 / final.data$fMC_noPB) *
  final.data$fMC_noPB.error

# cleanup
rm(machine.blank, mb.answer, mb.samples, pb.answer, process.blank,
   process.blank.error, pb.samples)

#-----------------------------End of Program-----------------------------------