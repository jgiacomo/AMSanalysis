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
    if(!is(df$runTime, "POSIXlt")){
        tryCatch({
            df$runTime <- strptime(df$runTime, format='%c')
        }, error = function(c) {
            "Error: unable to coerce to POSIXlt in function NearestStdRuns()."
        })
    }
    # Should move this to the NECtoRunData.R script. Also, this may not work
    # as the strptime function may just return NA instead of throwing an error.
    
    
}