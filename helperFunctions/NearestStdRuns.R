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
    #     nearestStdRuns = a list of the nearest standard runs
    
    # Check that the run completion times are formatted as date times (POSIXlt)
    try
    
    
}