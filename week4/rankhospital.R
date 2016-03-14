rankhospital <- function(state, outcome, num = "best") {
    outcomeData <- read.csv('outcome-of-care-measures.csv',colClasses="character")
    outcomeData <- outcomeData[outcomeData$State==state,]
    if(nrow(outcomeData) == 0) {
        stop('invalid state')
    }
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
    rates <- outcomeData[,col]
    #print (sort(rates,index.return=T)$ix)
    outcomeData <- outcomeData[sort(rates,index.return=T)$ix,]
    #print(head(outcomeData$Hospital.Name))
    if(is.numeric(num)){
        outcomeData$Hospital.Name[num]
    }else if(num == 'best') {
        outcomeData$Hospital.Name[1]
    } else if(num == 'worst') {
        outcomeData$Hospital.Name[nrow(outcomeData)]
    }
}