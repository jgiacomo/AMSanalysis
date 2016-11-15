# This file contains small utility functions used for the AMS analysis app

Convertd13C <- function(c13.sample, c13.vpdb, ratioToDelta = TRUE) {
    # This function takes the sample 13C/12C ratio or the sample d13C and the 
    # VPDB standard 13C/12C ratio along with a boolean operator. The function 
    # will either convert the sample's 13C/12C ratio to a d13C value if the 
    # boolean parameter is TRUE or it will convert the sample's d13C value to 
    # a 13C/12C ratio if the boolean parameter is FALSE. The default case is 
    # ratioToDelta = TRUE to convert from a ratio to the d13C value. 
    
    if(ratioToDelta) {
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
    # This function takes a vector of data and an equal length vector of errors
    # and returns a list of the error weighted mean value of x and the error
    # weighted standard deviation of x.
    mn <- wt.mean(x, 1/error^2)
    stdev <- wt.sd(x, 1/error^2)
    return(list("mean"=mn,"sd"=stdev))
}
