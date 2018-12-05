srcDir <- dirname(sys.frame(1)$ofile)
source(file.path(srcDir, "init_data.R"))
source(file.path(srcDir, "corpus.R"))
source(file.path(srcDir, "constant_predictor.R"))

SplitLine <- function(curLine){
    #Remove leading whitespace:
    curLine <- gsub("^\\s+", "", curLine)
    #Remove extra whitespace:
    curLine <- gsub("\\s+", " ", curLine)
    #Split the line:
    strsplit(curLine, " ")[[1]]
}

Tester <- function(testDir, predictor){
    files = dir(testDir)
    
    nTested = numeric(length(files))
    nCorrect = numeric(length(files))
    
    names(nTested) <- files
    names(nCorrect) <- files
    tStart <- Sys.time();
    
    for (curFile in files){
        con <- file(file.path(testDir, curFile), "r")
        
        # curIt=0
        while(length(curLine <- readLines(con, 1, skipNul = T))>0){
            lineIn <- SplitLine(curLine)
            lineInClean <- tolower(sapply(lineIn, removePunctuation))
            
            curPrediction <- tolower(predictor$Start())
            # print(length(lineIn))
            for(i in seq(length(lineIn))){
                if(lineInClean[i] == "" || lineInClean[i] %in% curPrediction){
                    nCorrect[curFile] <- nCorrect[curFile]+1
                }
                nTested[curFile] <- nTested[curFile]+1
                curPrediction <- tolower(predictor$Next(lineIn[i]))
            }
            # curIt <- curIt+1
        }
        # print(curIt)
        close(con)
    }
    fracCorrect = nCorrect/nTested
    score = mean(fracCorrect)
    tEnd <- Sys.time()
    list(score, fracCorrect, 1000*as.numeric(tEnd-tStart)/sum(nTested), tEnd-tStart)
}

language <- "de_DE"
# curDir <- file.path(devDir, language)
# cPredictor <- CreateConstantPredictor(curDir,  language)
# sTime <- Sys.time()
results <- Tester(file.path(testDir, language), cPredictor)
# print(Sys.time()-sTime)
print(results)