numInputToIntegers <- function(numText){
    
    library(stringr)
    
    # Catch any errors or warnings and return an empty string.
    tryCatch({
        
        if(is.null(numText)|numText=="") return("")
        
        # Check if user wants all data.
        if(tolower(numText)=="all"){
            return("all")
        }
        
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
        rangePos <- as.character(unlist(rangematrix))
        
        finalPos <- c(positions, rangePos)
        
        finalPos <- sort(as.integer(finalPos))
        
        return(finalPos)
        
    },
    warning=function(c) return(""),
    error=function(c) return("")
    )
}