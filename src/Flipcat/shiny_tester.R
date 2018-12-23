SplitLine <- function(curLine){
    #Remove leading whitespace:
    curLine <- gsub("^\\s+", "", curLine)
    #Remove extra whitespace:
    curLine <- gsub("\\s+", " ", curLine)
    #Split the line:
    strsplit(curLine, " ")[[1]]
}

# Find the fraction of correctly predicted words from a body of text.
TextTester <- function(text, predictor){
    #First tokenize the thing.
    lineIn <- SplitLine(text)
    lineInClean <- tolower(sapply(lineIn, removePunctuation))
    
    #Start with the first prediction.
    curPrediction <- tolower(predictor$Start())[1:3]
    nCorrect <- integer(3)
    nTested <- numeric(1)
    if(length(lineIn) < 1) return(nCorrect)
    for(i in seq(length(lineIn))){
        #Don't count empty solutions.
        if(lineInClean[i]==""){
            curPrediction <- tolower(predictor$Next(lineIn[i]))
            next();
        } 
        
        #Keep track of which prediction was correct.
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