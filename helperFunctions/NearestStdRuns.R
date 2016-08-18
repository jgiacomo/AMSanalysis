NearestStdRuns <- function(df, run, standardPos, n=6){
    # Finds standard runs (from the list of standards) which are nearest in time
    # to the unknown's run. The number of standard runs found is determined by
    # n.
    
    # Inputs
    #     df = data frame containing AMS run data
    #     run = the run number of the unknown
    #     standardPos = a list of the positions of standards
    #     n = number of nearest standard runs to find
    #
    # Outputs
    #     nearestStdRuns = a data frame of the nearest standard runs and the
    #                      difference in time to the input run.
    
    library(dplyr)
    
    # Check that the run completion times are formatted as date times (POSIXlt)
    if(!is(df$runTime, "POSIXlt")){
        tryCatch({
            df$runTime <- strptime(df$runTime, format='%c')
        }, error = function(c) {
            "Error: unable to coerce to POSIXlt in function NearestStdRuns()."
        })
    }
    # Should move this to the NECtoRunData.R script. Also, this may not work
    # as the strptime function may just return NA instead of throwing an error.
    
    # Check that run times are formatted as date times (POSIXlt) and exit if not
    if(!is(df$runTime, "POSIXlt")){
        stop("Error: runTime not of class POSIXlt in function NearestStdRuns.")
    }
    
    # Parse out the standards data keeping only active runs
    stdRuns <- df %>% filter(Pos in standardPos, active==TRUE)
    # In case the run in question is a standard, remove it from the list
    stdRuns <- stdRuns[stdRuns$Run != run,]
    
    # Find the run time for the run in question
    smplRT <- df %>% filter(Run == run) %>% select(runTime)
    
    # Find the time differences (in seconds)
    stdRuns$timeDiff <- abs(difftime(stdRuns$runTime, smplRT, units="secs"))
    
    # Find the nearest n standard runs to the run in question
    nearestRunsIndex <- sort(stdRuns$timeDiff,index.return=TRUE)[[2]]
    nearestRuns <- stdRuns[nearestRunsIndex,] %>% select(Run,timeDiff)
    
}