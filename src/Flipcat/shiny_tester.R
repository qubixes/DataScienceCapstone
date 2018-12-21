SplitLine <- function(curLine){
    #Remove leading whitespace:
    curLine <- gsub("^\\s+", "", curLine)
    #Remove extra whitespace:
    curLine <- gsub("\\s+", " ", curLine)
    #Split the line:
    strsplit(curLine, " ")[[1]]
}


TextTester <- function(text, predictor){
    lineIn <- SplitLine(text)
    lineInClean <- tolower(sapply(lineIn, removePunctuation))
    
    curPrediction <- tolower(predictor$Start())[1:3]
    
    nCorrect <- integer(3)
    nTested <- numeric(1)
    if(length(lineIn) < 1) return(nCorrect)
    for(i in seq(length(lineIn))){
        if(lineInClean[i]=="") next();
        
        for(j in 1:3){
            if(lineInClean[i] == curPrediction[j])
                nCorrect[j] <- nCorrect[j]+1
        }
        nTested <- nTested+1
        curPrediction <- tolower(predictor$Next(lineIn[i]))
    }
    if(nTested > 0)
        return(nCorrect/nTested)
    return(nCorrect)
}