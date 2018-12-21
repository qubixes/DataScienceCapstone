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
        oPhrase <- oPhrase[oPhrase != "-dot-"]
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
    firstTerm <- firstTerm[secondTerm!="-dot-"]; count <- count[secondTerm!="-dot-"]; secondTerm <- secondTerm[secondTerm!="-dot-"]; 
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
    myTokens <- corpus(myCorpus) %>% tokens(what="word", remove_punct=F, remove_symbols=T, remove_hyphens=T) %>% tokens_replace(pattern= ".", replacement = "-dot-") %>% tokens(what="fastestword", remove_punct=T) %>% tolower
    # myTokens <- gsub("\.+", "xspecialx_dot", myTokens) %>% 
    rm("myCorpus")
    myTokens
}

TokenTransformer2 <- function(words){
    words %>% tokens(what="fastestword", remove_punct=F, remove_symbols=T, remove_hyphens=T) %>% tokens_replace(pattern= ".", replacement = "-dot-") %>% tokens(what="fastestword", remove_punct=T) %>% tolower
}

TokenTransformer <- function(words){
    words %>% tokens(what="fastestword", remove_punct=T, remove_symbols=T, remove_hyphens=T) %>% tolower
}

TrainMultigramPredictor <- function(trainDir, parameters, myTokens=NULL){
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
    
    rm("myList")
    lastValues = character(maxNGram-1)
    
    Next <- function(word){
        cleanWord <- TokenTransformer2(word)
        if(sum(cleanWord != "") != 0){
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
    
    Start <- function(){
        lastValues <<- character(maxNGram-1)
        Next(".")
        # predictorList[[1]]
    }
    print(paste("Finished training language:", language))
    list(Start = Start, Next = Next)
}

GetStatsPredictor <- function(predictor){
        predList <- get("predictorList", environment(predictor$Start))
        nElem <- integer(length(predList))
        for( i in seq(length(predList))){
            nElem[i] <- length(predList[[i]]) 
        }
        nElem
}


