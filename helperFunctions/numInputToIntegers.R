numInputToIntegers <- function(numText){
    
    library(stringr)
    
    if(is.null(numText)) return(NULL)
    
    splitText <- str_split(numText, ',')[[1]]
    trimText <- str_trim(splitText, side="both")
    
    # Extract the single positions
    positions <- trimText[!grepl('-',trimText)]
    
    # Extract the ranges
    ranges <- trimText[grep('-',trimText)]
    
    # Convert ranges to sequences
    rangescomma <- gsub('(^\\d+)( *- *)(\\d+$)','\\1,\\3',ranges)
    sequences <- str_split(rangescomma,',')
    rangematrix <- sapply(sequences, function(x) seq(as.numeric(x[1]),
                                                     as.numeric(x[2])))
    rangePos <- as.character(rangematrix)
    
    finalPos <- c(positions, rangePos)
    
    finalPos <- as.character(sort(as.numeric(finalPos)))
    
    return(finalPos)
}