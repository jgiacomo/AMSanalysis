NECtoRunData <- function(runlog){
    # This function takes an NEC runlog and produces a data frame in the
    # universal RunData format (as explained in readme).
    
    # Inputs: runlog - the runlog file created by the NEC software containing
    #                  the AMS measurement data.
    
    # Value: a data frame of AMS results in a universal format.
    
    library(readr)
    library(stringr)
    
    # create column widths, types, and read fixed-width-file to data frame.
    colWidths <- fwf_empty(runlog, skip=5)
    colTypes <- "ccciicciddiiiddd"
    df <- read_fwf(runlog, colWidths, skip=6, col_types=colTypes)
    
    colNames <- c("E","Item","runTime","pos","meas","smType",
                  "sampleName","numCycles","he12C","he13C","cntTotH",
                  "cntTotS","cntTotGT","he13.12","he14.12","he14.13")
    names(df) <- colNames

    # coerce to the proper time format (POSIXct required by dplyr)
    df$runTime <- as.POSIXct(strptime(df$runTime, format='%c'))
    
    # create missing columns, batch inferred from runlog file
    batch <- str_extract(readLines(runlog, n=3)[3], '[bB]atch[^\\s]*')
    df$analysis <- batch  # This is the batch number or magazine number.
    df$trans12C <- NA  # Doesn't exist yet for the NEC instrument.
    df$active <- TRUE  # Initially we make all runs active.
    df$run <- paste(df$analysis, df$pos, df$meas, sep="-")
    df$count14C <- df$cntTotGT
    
    # Now to adjust columns to those for the general rundata data frame
    rundata <- df %>% select(analysis, sampleName, smType, pos, run, runTime,
                             numCycles, he12C, he13C, count14C, he13.12,
                             he14.12, he14.13, trans12C, active)
    
}