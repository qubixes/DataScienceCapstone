library(quanteda)
library(tm)
library(plyr)
library(dplyr)
library(Rcpp)
# srcDir <- dirname(sys.frame(1)$ofile)
# source(file.path(srcDir, "init_data.R"))
# sourceCpp(file.path(srcDir, "vargram.cpp"))
# source("init_data.R")
sourceCpp("vargram.cpp")


dotRepresentation <- "xxxdotxxx"

PredictorListFromCPL <- function(countPhraseList, ngram){
    count = countPhraseList[["count"]]
    phrase = countPhraseList[["phrase"]]
    if(ngram == 1){
        oPhrase  = phrase[order(count, decreasing = T)]
        oPhrase <- oPhrase[oPhrase != dotRepresentation]
        return(oPhrase[1:3])
    }
    
    if(length(phrase) == 0){
        return(emptyenv())
    }
    
    splitPhrase <- strsplit(phrase, ' (?=[^ ]+$)', perl=TRUE)
    goodSplits <- (sapply(splitPhrase, length) == 2)
    count <- count[ goodSplits ] 
    splitPhrase <- splitPhrase[ goodSplits ] %>% unlist
    
    firstTerm <- splitPhrase[seq(1, length(splitPhrase), 2)]; secondTerm <- splitPhrase[seq(2, length(splitPhrase), 2)]
    firstTerm <- firstTerm[secondTerm!=dotRepresentation]; count <- count[secondTerm!=dotRepresentation]; secondTerm <- secondTerm[secondTerm!=dotRepresentation]; 
    btmDf <- data.frame(count=count, firstTerm  = firstTerm, secondTerm = secondTerm)
    btmDfBest <- btmDf %>% group_by(firstTerm) %>% top_n(3,count) 
    allFirst <- unique(as.character(btmDf$firstTerm))
    btmDfBest <- btmDfBest[order(as.character(btmDfBest$firstTerm)),]
    bestList <- vector("list", length = length(allFirst)*1.1)
    
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

GetTokens <- function(trainDir, language){
    myCorpus <- Corpus(DirSource(trainDir), readerControl = list(reader = readPlain, language = language, load = TRUE))
    myTokens <- corpus(myCorpus) %>% tokens(what="word", remove_punct=T, remove_symbols=T, remove_hyphens=T) %>% tolower
    rm("myCorpus")
    myTokens
}

GetTokens2 <- function(trainDir, language){
    myCorpus <- Corpus(DirSource(trainDir), readerControl = list(reader = readPlain, language = language, load = TRUE))
    myTokens <- corpus(myCorpus) %>% tokens(what="word", remove_punct=F, remove_symbols=T, remove_hyphens=T) %>% tokens_replace(pattern= ".", replacement = dotRepresentation) %>% tokens(what="fastestword", remove_punct=T) %>% tolower
    # myTokens <- gsub("\.+", "xspecialx_dot", myTokens) %>% 
    rm("myCorpus")
    myTokens
}



TrainNGramPredictorList <- function(trainDir, parameters, myTokens=NULL){
    threshold <- parameters[["threshold"]]
    maxNGram <- parameters[["maxNGram"]]
    maxTokens <- parameters[["maxTokens"]]
    minOccurence <- parameters[["minOccurence"]]
    language <- parameters[["language"]]
    
    if(is.null(threshold) || is.null(maxNGram)){
        print("Error creating predictor: parameters not supplied")
        return (NULL)
    }
    
    if(is.null(myTokens)) myTokens <- GetTokens2(trainDir, language)
    if(!is.null(maxTokens) && maxTokens < length(myTokens)) myTokens = myTokens[1:maxTokens]
    
    if(!is.null(minOccurence)){
        if(threshold*length(myTokens) < minOccurence)
            threshold <- (minOccurence+0.1)/length(myTokens)
    }
    myList <- GetMultigram(myTokens, threshold, maxNGram)
    
    rm("myTokens"); 
    
    predictorList = list()
    for(i in 1:maxNGram)
        predictorList[[i]] = PredictorListFromCPL(myList[[i]], i)
    print(paste("Finished training language:", language))
    predictorList
}


TokenizeFaster <- function(words){
    gsub('[.]', paste0(" ", dotRepresentation, " "), words) %>% tokens(what="fastestword", remove_punct=T, remove_symbols=T, remove_hyphens=T) %>% tolower
}

TokenizeFastest <- function(words){
    words %>% tokens(what="fastestword", remove_punct=T, remove_symbols=T, remove_hyphens=T) %>% tolower
}

TokenizeSlower <- function(words){
    words %>% tokens(what="fastestword", remove_punct=F, remove_symbols=T, remove_hyphens=T) %>% tokens_replace(pattern= ".", replacement = dotRepresentation) %>% tokens(what="fastestword", remove_punct=T) %>% tolower
}


NewNGramPredictor <- function(newPredList = NULL, tokenizer = "slow"){
    
    TokenizeSlow <- function(words){
        gsub('[.]', paste0(" ", dotRepresentation, " "), words) %>% tokens(what="word", remove_punct=T, remove_symbols=T, remove_hyphens=T) %>% tolower
    }

    TokenizeFast <- function(words){
        gsub('[.]', paste0(" ", dotRepresentation, " "), words) %>% tokens(what="fastestword", remove_punct=T, remove_symbols=T, remove_hyphens=T) %>% tolower %>% removePunctuation
    }    
    
    predictorList <- NULL
    maxNGram <- integer(1)
    
    InitPredList <- function(newList){
        if(!is.null(newList)){
            predictorList <<- newList
            maxNGram <<- length(predictorList)
        } else {
            predictorList <<- NULL
            maxNGram <<- 1
        }
        
    }
    
    SetTokenizer <- function(newTokenizer="slow"){
        if(newTokenizer == "fast"){ 
            myTokenizer <<- TokenizeFast
         } else {
            myTokenizer <<- TokenizeSlow
        }
    }
    
    Start <- function(){
        if(maxNGram <= 0){
            print("Error: didn't load prediction data.")
        }
        lastValues <<- character(maxNGram-1)
        Next(".")
    }
    
    Next <- function(word){
        cleanWord <- myTokenizer(word)
        if(sum(cleanWord != "") != 0){
            cleanWord <- cleanWord[cleanWord != ""]
            nWord <- length(cleanWord)
            i=1
            while(i<=maxNGram-1-nWord){
                lastValues[i] <<- lastValues[i+nWord]
                i = i+1;
            }
            i=max(c(maxNGram-nWord,1))
            while(i<=maxNGram-1){
                lastValues[i] <<- cleanWord[nWord+(i-maxNGram+1)]
                i=i+1;
            }
        }
        nPredict=0; iGram=maxNGram;
        prediction = character(3)
        while(nPredict<3 && iGram >=1){
            if(iGram == 1)
                newPrediction = predictorList[[1]]
            else{
                myStr <- paste(lastValues[(maxNGram-iGram+1):(maxNGram-1)], collapse = " ")
                newPrediction = predictorList[[iGram]][[myStr]]
            }
            if(!is.null(newPrediction)){
                nEmpty = sum(newPrediction == "")
                for(i in 1:(3-nEmpty)){
                    if(!(any(newPrediction[i] == prediction))){
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
    
    SetTokenizer(tokenizer)
    InitPredList(newPredList)
    
    lastValues <- character(maxNGram-1)
    
    list(Start = Start, Next = Next, SetTokenizer = SetTokenizer, InitPredList = InitPredList)
}

GetStatsPredictor <- function(predictor){
        predList <- get("predictorList", environment(predictor$Start))
        nElem <- integer(length(predList))
        for( i in seq(length(predList))){
            nElem[i] <- length(predList[[i]]) 
        }
        nElem
}


