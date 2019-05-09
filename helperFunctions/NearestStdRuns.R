NearestStdRuns <- function(df, Run, standardPos, n=6){
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
    
    # Check that run times are formatted as date times (POSIXlt) and exit if not
    if(!is(df$dateTime, "POSIXct")){
        stop("Error: runTime not of class POSIXct in function NearestStdRuns.")
    }
    
    # Parse out the standards data keeping only active runs
    stdRuns <- df %>% filter(pos %in% standardPos, active==TRUE)
    # In case the run in question is a standard, remove it from the list
    stdRuns <- stdRuns[stdRuns$run != Run,]
    
    # Find the run time for the run in question
    smplRT <- df %>% filter(run == Run) %>% select(runTime) %>% pull()
    
    # Find the time differences (in seconds)
    stdRuns$timeDiff <- abs(difftime(stdRuns$dateTime, smplRT, units="secs"))
    
    # Find the nearest n standard runs to the run in question
    nearestRunsIndex <- sort.int(stdRuns$timeDiff,index.return=TRUE)[[2]]
    nearestRuns <- stdRuns[nearestRunsIndex,] %>% select(run,timeDiff) %>%
        head(n)
    
    return(nearestRuns)
    
}