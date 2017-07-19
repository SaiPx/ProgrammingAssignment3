rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
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
        
        ## Check if num is a string of "worst" or "best" or int
        isdec <- FALSE
        if(num=="best"){
                isdec = FALSE
                num <- 1
        }
        else if(num=="worst"){
                isdec = TRUE
                num <- 1
        }
        else {
                ## Check if num is larger than dataset available
                tablelen <- as.integer(summary(statewiseoutcome[state])[,1])
                if(as.integer(num) > tablelen){
                        stop(NA)
                }
        }
        
        ## Create a Data Frame for the required output
        totalset <-data.frame(statewiseoutcome[[state]][2],
                              statewiseoutcome[[state]][datcolname])
        
        ## Rename the Mortality Column to Rate
        colnames(totalset)[2]<-"Rate"

        ## Return hospital name in that state with the given rank
        rankorder <- order(as.numeric(t(totalset[2])), 
                           as.character(t(totalset[1])), 
                           na.last=NA, decreasing = isdec)

        ## 30-day death rate
        data.frame(totalset[t(rankorder[1:num]),], Rank=c(1:num))
}