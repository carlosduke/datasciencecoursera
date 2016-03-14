corr <- function(directory, threshold = 0) {
    completes <- complete(directory,1:332)
    cases <- completes[,2] > threshold
    completes <- completes[cases,]
    r <- numeric(nrow(completes))
    if(nrow(completes) == 0){
        return(r)
    }
    for(i in 1:nrow(completes)) {
        complete <- completes[i,]
        csvData <- read.csv(paste(directory,'/',sprintf('%03d',complete$id),'.csv',sep=''))
        r[i] <- cor(csvData$sulfate,csvData$nitrate,use='complete')
    }
    r
}
cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)