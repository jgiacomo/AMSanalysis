NECtoRunData <- function(runlog){
    # This function takes an NEC runlog and produces a data frame in the
    # universal RunData format (as explained in readme).
    
    # Inputs: runlog - the runlog file created by the NEC software containing
    #                  the AMS measurement data.
    
    # Value: a data frame of AMS results in a universal format.
    
    source("NEC_runlog_to_dataframe.R")  # convert runlog to data frame.
    
    df <- NECrunlogToDF(runlog, lab="accium")
    
    df$Run.Completion.Time <- strptime(df$Run.Completion.Time, format='%c')
    
}