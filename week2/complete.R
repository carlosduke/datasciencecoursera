complete <- function(directory, id=1:332) {
    l <- length(id)
    nobs <- numeric(l)
    
    for(i in 1:l){
        csvData <- read.csv(paste(directory,'/',sprintf('%03d',id[i]),'.csv',sep=''))
        nobs[i] <- nrow(csvData[complete.cases(csvData),])
        
    }
    data.frame(id,nobs)
}