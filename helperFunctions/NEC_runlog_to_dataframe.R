# Takes a NEC runlog and converts it to an R data frame.

NECrunlogToDF <- function(file, lab=""){
  library(readr)
  
  # if(!file.exists(file)){
  #   stop("That file doesn't appear to exist")
  # }
  
  # create column widths
  colWidths <- fwf_empty(file, skip=4)
  
  # create appropriate column names based on lab
  if(tolower(lab)=="georgia"){
    colNames <- c("E","J","Item","Run.Completion.Time","Grp","Pos",
                   "Meas","Cycles","le12C","le13C","he12C","he13C","CntTotH",
                   "CntTotS","CntTotGT","le13.12","he13.12","he14.12",
                   "he14.13","GvmVR","stripPR","S1ovnTR","LEmagFld",
                   "HEmagFld","LEmagCC","HEmagCC")
    colTypes <- "cccciiiidddddddddddddddddd"
    
    df <- read_fwf(file, colWidths, skip=4, col_types=colTypes)
    names(df) <- colNames
    
  } else if(tolower(lab)=="georgia250"){
    colNames <- c("E","J","Item","Run.Completion.Time","Grp","Pos",
                  "Meas","Cycles","le12C","le13C","he12C","he13C",
                  "CntTotS","CntTotGT","le13.12","he13.12","he14.12",
                  "he14.13","bias","stripPR","S1ovnTR","S2ovnTR","LEmagFld",
                  "HEmagFld","LEmagCC","HEmagCC")
    colTypes <- "ccccciiiddddiidddddddddddd"
    
    df <- read_fwf(file, colWidths, skip=4, col_types=colTypes)
    names(df) <- colNames
    
    # Add CntTotH column and set values to CntTotS
    df <- df %>% mutate(CntTotH = CntTotS)
    
  }else if(tolower(lab)=="accium"){
    colNames <- c("E","Item","Run.Completion.Time","Pos","Meas","SmType",
                    "Sample.Name","Cycles","he12C","he13C","CntTotH",
                    "CntTotS","CntTotGT","he13.12","he14.12","he14.13")
    colTypes <- "ccciiccidddddddd"
    
    df <- read_fwf(file, colWidths, skip=4, col_types=colTypes)
    names(df) <- colNames
    
  } else{
    # read the file as a fixed width text format
    df <- read_fwf(file, colWidths, skip=4)
    
    # create column names from the first row of data and format nice
    names(df) <- make.names(df[1,])
    
  }
  
  # remove the first two rows which are not data
  df <- df[-c(1,2),]
  
  return(df)
}