NECtoRunData <- function(runlogFile, resultFile = NULL){
    # This function takes an NEC runlog and, if available, an NEC "result.xls"
    # file and produces a data frame in the universal RunData format (as
    # explained in the project readme).
    
    # Inputs: runlogFile - the path and filename of the NEC runlog (runlog.cat).
    #         resultFile - the path and filename of the NEC result file
    #                      (result.xls).
    
    # Value: a data frame of AMS results in a universal format.
    
    library(dplyr)
    library(readr)
    library(stringr)
    
    ############################################################################
    ############################# FUNCTIONS ####################################
    ############################################################################
    
    # Dealing with runlogs
    DFfromRunlogFile <- function(inFile){
        # Create column widths, types, and read fixed-width-file to data frame.
        # In the runlog line 5 contains column headers and line 6 contains the
        # column width markers.
        colWidths <- fwf_empty(inFile, skip=5)
        colTypes <- "ccciicciddiiiddd"
        df <- read_fwf(inFile, colWidths, skip=6, col_types=colTypes)
        
        colNames <- c("E","item","dateTime","pos","meas","smType",
                      "label","numCycles","he12C","he13C","cntTotH",
                      "cntTotS","cntTotGT","he13.12","he14.12","he14.13")
        names(df) <- colNames
        
        # Coerce to the proper time format (POSIXct required by dplyr).
        df$dateTime <- as.POSIXct(strptime(df$dateTime, format='%c'))
        
        # Transform 13C/12C to absolute ratio rather than %.
        df$he13.12 <- df$he13.12/100
        
        # Create missing columns, batch inferred from runlog file.
        # Line 3 in the runlog contains the batch directory.
        batch <- str_extract(readLines(inFile, n=3)[3], '[bB]atch[^\\s]*')
        df$analysis <- batch  # This is the batch number or magazine number.
        df$le12C    <- NA     # Only available in results.xls.
        df$trans12C <- NA     # Only available in results.xls.
        df$active   <- TRUE   # Initially we make all runs active.
        df$run      <- sprintf("%s-%s-%02d",df$analysis,df$pos,df$meas)
        df$count14C <- df$cntTotGT
        
        # Now to adjust columns to those for the general rundata data frame.
        rundata <- df %>% select(analysis, label, smType, pos, run, dateTime,
                                 numCycles, le12C, he12C, he13C, count14C,
                                 he13.12, he14.12, he14.13, trans12C, active)
        
        return(rundata)
    }
    
    # Dealing with result.xls
    DFfromResultFile <- function(inFile){
        # results.xls is a comma separated version of the runlog but with more
        # measurement information (e.g. transmission, magnetic fields, terminal
        # voltage). Measuremnet data begins on line 7. Each line, unfortunately,
        # ends with a comma. This must be removed before the file can be
        # imported as a csv.
        
        # Remove trailing comma from every line
        tmpResult <- readLines(inFile)
        tmpResult <- str_replace(tmpResult, ',$', '')
        
        # Create temporary csv file to create data frame from
        tmpFile <- tempfile(fileext = ".csv")
        writeLines(tmpResult, con=tmpFile)
        colTypes <- "cccciiiiddddiiiiidddddddddd"
        df <- read_csv(tmpFile, col_names=FALSE, col_types=colTypes, skip=6)
        file.remove(tmpFile)  # clean up the temporary file
        colNames <- c("E", "index", "item", "dateTime", "group", "pos", "meas",
                      "numCycles", "le12C", "le13C", "he12C", "he13C",
                      "cntTotH", "cntTotS", "cntTotGT", "cnt4", "cnt5",
                      "trans12C", "le13.12", "he13.12", "he14.12", "he14.13",
                      "terminal.kV", "stripPR", "igc03-1", "leMag.G", "heMag.G")
        names(df) <- colNames
        
        # Coerce to the proper time format (POSIXct required by dplyr).
        df$dateTime <- as.POSIXct(strptime(df$dateTime, format='%c'))
        
        # Transform 13C/12C to absolute ratio rather than %.
        df$he13.12 <- df$he13.12/100
        
        # Create missing columns, batch inferred from runlog file.
        # Line 3 in the runlog contains the batch directory.
        batch <- str_extract(readLines(inFile, n=3)[3], '[bB]atch[^\\s]*')
        df$analysis <- batch  # This is the batch number or magazine number.
        df$active   <- TRUE   # Initially we make all runs active.
        df$run      <- paste(df$analysis, df$pos, df$meas, sep="-")
        df$count14C <- df$cntTotGT
        
        # Now to adjust columns to those for the general rundata data frame.
        rundata <- df %>% select(analysis, pos, run, dateTime,
                                 numCycles, le12C, he12C, he13C, count14C,
                                 he13.12, he14.12, he14.13, trans12C, active)
        
        return(rundata)
    }
    
    ############################################################################
    ############################# MAIN #########################################
    ############################################################################
    
    # Create a data frame from the runlog.
    df <- DFfromRunlogFile(runlogFile)
    
    # Check if a result file has been defined and create data frame if it has.
    # Then join the extra data contained in the result file which the runlog
    # did not contain.
    if(!is.null(resultFile)){
        results <- DFfromResultFile(resultFile)
        results <- results %>% select(run, le12C, trans12C)  # only needed cols.
        df <- df %>% select(-le12C, -trans12C)  # to prevent duplication.
        df <- left_join(df, results, by="run")
    }
    
    return(df)
}