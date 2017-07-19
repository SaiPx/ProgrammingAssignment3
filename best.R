best <- function(state, outcome) {
        ## Read outcome data from file
        outcomeData<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        
        ## Check that state and outcome are valid
        ## Allowable outcome
        allowOutcome<-data.frame(dis=c("heart attack","heart failure","pneumonia"),
                                 datacol=c(11,            17,             23))
        
        if(!(state %in% outcomeData[["State"]])){
                stop("invalid state")
        }
        else if(!(outcome %in% allowOutcome[,1])){
                stop("invalid outcome")
        }
        ## Return hospital name in that state with lowest 30-day death
        ## Determine the right column to extract information
        datcolname <- allowOutcome[which(allowOutcome[,1]==outcome),2]
        
        ## Split the outcome table by State
        statewiseoutcome <- split(outcomeData,list(outcomeData[,7]))
        
        ## Extract the Minimum Value in the Outcome column
        ## Requires a Transpose to find minimum value
        outcomeforstate<-t(statewiseoutcome[[state]][datcolname])
        minval=min(as.numeric(outcomeforstate),na.rm = TRUE)
        
        ## Determine the index of the minimum value
        index<-which(as.numeric(outcomeforstate) == minval)
        
        ## rate and Return the Name of the Hospital
        statewiseoutcome[[state]][index,2]
}