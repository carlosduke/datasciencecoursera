pollutantmean <- function(directory, pollutant, id=1:332) {
    l <- length(id)
    sumT <- 0
    qtdT <- 0
    
    for(i in 1:l){
        csvData <- read.csv(paste(directory,'/',sprintf('%03d',id[i]),'.csv',sep=''))
        pColumn <- csvData[,pollutant]
        
        sumT <- sumT + sum(pColumn,na.rm=T)
        qtdT <- qtdT + length(pColumn[!is.na(pColumn)])
        
    }
    sumT/qtdT
}