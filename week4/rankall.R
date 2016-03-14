rankall <- function(outcome, num = "best") {
    outcomeData <- read.csv('outcome-of-care-measures.csv',colClasses="character")
    if(outcome == 'heart attack') {
        col <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
    }else if(outcome == 'heart failure') {
        col <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
    }else if(outcome == 'pneumonia') {
        col <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
    } else {
        stop('invalid outcome')
    }
    
    outcomeData[,col] <- as.numeric(outcomeData[,col])
    outcomeData <- outcomeData[!is.na(outcomeData[col]),]
    outcomeData <- outcomeData[,c('Hospital.Name','State',col)]
    
    print(dim(outcomeData))
    states <- sort(unique(outcomeData$State))
    retorno <- data.frame(hospital=NA,states=states)
    rownames(retorno) <- states
    for(i in 1:length(states)) {
        state <- states[i]
        outcomeStates <- outcomeData[outcomeData$State==state,]
        rates <- outcomeStates[,col]
        outcomeStates <- outcomeStates[sort(rates,index.return=T)$ix,]
        
        if(is.numeric(num)){
            retorno[state,'hospital'] <- outcomeStates$Hospital.Name[num]
        }else if(num == 'best') {
            retorno[state,'hospital'] <- outcomeStates$Hospital.Name[1]
        } else if(num == 'worst') {
            retorno[state,'hospital'] <- outcomeStates$Hospital.Name[nrow(outcomeStates)]
        }
    }
    retorno
}