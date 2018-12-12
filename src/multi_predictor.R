library(quanteda)
library(tm)
library(plyr)
library(dplyr)
library(Rcpp)
srcDir <- dirname(sys.frame(1)$ofile)
source(file.path(srcDir, "init_data.R"))



# language <- "en_US"
# myDir <- file.path(testDir, language)
# myCorpus <- Corpus(DirSource(myDir), readerControl = list(reader = readPlain, language = language, load = TRUE))
# myTokens <- corpus(myCorpus) %>% tokens(what="word", remove_punct=T, remove_symbols=T, remove_hyphens=T) %>% tolower
# tokenList <- myTokens


PredictorListFromCPL <- function(countPhraseList, ngram){
    count = countPhraseList[["count"]]
    phrase = countPhraseList[["phrase"]]
    if(ngram == 1){
        oPhrase  = phrase[order(count, decreasing = T)]
        return(oPhrase[1:3])
    }
    
    splitPhrase <- strsplit(phrase, ' (?=[^ ]+$)', perl=TRUE) %>% unlist %>% matrix(ncol=2, byrow=T)
    btmDf <- data.frame(count=count, firstTerm  = splitPhrase[,1], secondTerm = splitPhrase[,2])
    btmDfBest <- btmDf %>% group_by(firstTerm) %>% top_n(3,count) 
    allFirst <- unique(as.character(btmDf$firstTerm))
    btmDfBest <- btmDfBest[order(as.character(btmDfBest$firstTerm)),]
    bestList <- vector("list", length = length(allFirst))
    
    bestList <- mapply(function(x){ x=character(3)}, bestList, SIMPLIFY = F)
    nSame=0
    iList=0
    prev="xxx"
    for(i in seq(dim(btmDfBest)[1])){
        curFirst <- as.character(btmDfBest$firstTerm[i])
        curSecond <- as.character(btmDfBest$secondTerm[i])
        if(curFirst == prev){
            nSame <- nSame+1
        }
        else{
            nSame=1; prev=curFirst; iList=iList+1
            names(bestList)[iList] = curFirst
        }
        
        bestList[[iList]][nSame] = curSecond
    }
    list2env(bestList, hash = TRUE)
}

TrainMultigramPredictor <- function(trainDir, language, parameters){
    threshold <- parameters[["threshold"]]
    maxNGram <- parameters[["maxNGram"]]
    
    if(is.null(threshold) || is.null(maxNGram)){
        print("Error creating predictor: parameters not supplied")
        return (NULL)
    }
    
    myCorpus <- Corpus(DirSource(trainDir), readerControl = list(reader = readPlain, language = language, load = TRUE))
    myTokens <- corpus(myCorpus) %>% tokens(what="word", remove_punct=T, remove_symbols=T, remove_hyphens=T) %>% tolower
    myList <- GetMultigram(tokenList, threshold, maxNGram)
    
    rm("myTokens"); rm("myCorpus")
    
    predictorList = list()
    for(i in 1:maxNGram)
        predictorList[[i]] = PredictorListFromCPL(myList[[i]], i)
    
    rm("myList")
    lastValues = character(maxNGram-1)
    
    Start <- function(){predictorList[[1]]}
    Next <- function(word){
        cleanWord <- word %>% tokens(what="word", remove_punct=T, remove_symbols=T, remove_hyphens=T) %>% tolower
        if(sum(cleanWord != "") != 0){
            nWord <- length(cleanWord)
            # print(paste("move  = [", nWord+1,",",maxNGram-1,"]"))
            # print(paste("to = ", 1, (maxNGram-1-nWord)))
            # print(lastValues)
            lastValues[1:(maxNGram-1-nWord)] <<- lastValues[(nWord+1):(maxNGram-1)]
            lastValues[(maxNGram-nWord):(maxNGram-1)] <<- cleanWord
        }
        nPredict=0; iGram=maxNGram;
        prediction = character(3)
        while(nPredict<3 && iGram >=1){
            if(iGram == 1)
                newPrediction = predictorList[[1]]
            else{
                myStr <- paste(lastValues[(maxNGram-iGram+1):(maxNGram-1)], collapse = " ")
                # print(myStr)
                # print(lastValues)
                # print(paste((maxNGram-iGram+1),(maxNGram-1)))
                newPrediction = predictorList[[iGram]][[myStr]]
            }
            if(!is.null(newPrediction)){
                nEmpty = sum(newPrediction == "")
                for(i in 1:(3-nEmpty)){
                    if(!(newPrediction[i] %in% prediction)){
                        prediction[nPredict+1] = newPrediction[i]
                        nPredict = nPredict+1
                    }
                }
                # prediction[(nPredict+1):(nPredict+3-nEmpty)] <- newPrediction[1:(3-nEmpty)]
                # nPredict <- nPredict + 3-nEmpty;
            }
            iGram <- iGram-1
        }
        prediction[1:3]
    }
    list(Start = Start, Next = Next)
}


if(!exists("ngramPredictors")) ngramPredictors <- list()
myPred <- TrainMultigramPredictor(myDir, "en_US", parameters)
myPred$Start() %>% print
myPred$Next("We") %>% print
myPred$Next("are") %>% print
myPred$Next("going") %>% print