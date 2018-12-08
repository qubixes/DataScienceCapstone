srcDir <- dirname(sys.frame(1)$ofile)
source(file.path(srcDir, "init_data.R"))
source(file.path(srcDir, "corpus.R"))
library(tm)
library(dplyr)
library(pryr)

CreateNGramTokenizer <- function(n){
    NGramTokenizer <- function(x){ 
        unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = TRUE) 
    }
    NGramTokenizer
}

WordCleaner <- function(x){
    x <- removePunctuation(x) %>% gsub(pattern='"', replacement="") %>% gsub(pattern="_", replacement = "") 
    x <- x %>% gsub(pattern="-", replacement = "") %>% gsub(pattern="—", replacement = "") %>% gsub(pattern="“", replacement = "") 
    x <- x %>% gsub(pattern="”", replacement = "") %>% gsub(pattern="\u2013", replacement = "") %>% gsub(pattern="…", replacement = "")
    x <- x %>% gsub(pattern="·", replacement = "") %>% gsub(pattern="\u0096", replacement = "") %>% gsub(pattern="\u0097", replacement = "")
    x <- x %>% gsub(pattern="„", replacement = "") %>% gsub(pattern="«", replacement = "") %>% gsub(pattern="»", replacement = "")
    x
}

RemoveTDMGarbage <- function(tmData){
    tmData <- tm_map(tmData, content_transformer(WordCleaner))
    tmData
}

PredictorListFromTM <- function(mytm){
    btmDf <- as.data.frame(mytm)
    totCounts <- colSums(btmDf)
    btmDf <- btmDf/totCounts
    btmDf <- mutate(btmDf, terms = rownames(mytm))
    splitTerms <- strsplit(btmDf$terms, ' (?=[^ ]+$)', perl=TRUE) %>% unlist %>% matrix(ncol=2, byrow=T)
    btmDf <- cbind(btmDf, firstTerm  = splitTerms[,1], secondTerm = splitTerms[,2])
    btmDf <- cbind(btmDf, rate = rowMeans(btmDf[,1:(length(colnames(btmDf))-3)]))
    btmDfBest <- btmDf %>% group_by(firstTerm) %>% top_n(3,rate) 
    allFirst <- unique(as.character(btmDf$firstTerm))
    btmDfBest <- btmDfBest[order(as.character(btmDfBest$firstTerm)),]
    bestList <- vector("list", length = length(allFirst)*1.5)
    
    bestList <- mapply(function(x){ x=character(3)}, bestList, SIMPLIFY = F)
    nSame=0
    iList=0
    prev="xxx"
    # View(btmDf)
    # View(btmDfBest)
    newAllFirst <- character(length=length(allFirst)+2)
    for(i in seq(dim(btmDfBest)[1])){
        curFirst <- as.character(btmDfBest$firstTerm[i])
        curSecond <- as.character(btmDfBest$secondTerm[i])
        if(curFirst == prev){
            nSame <- nSame+1
        }
        else{
            nSame=1; prev=curFirst; iList=iList+1
            # print(paste(iList, length(allFirst), curFirst))
            names(bestList)[iList] = curFirst
            newAllFirst[iList] = curFirst
        }
        
        bestList[[iList]][nSame] = curSecond
    }
    # allFirst <- c(allFirst, "", "")
    # View(data.frame(uniq=allFirst, notUniq=newAllFirst))
    
    list2env(bestList, hash = TRUE)
}

TrainMonogramPredictor <- function(trainDir, language, cleanCorpus = NULL){
    if(!is.null(ngramPredictors[[trainDir]][[language]][["mono"]])) 
        return(ngramPredictors[[trainDir]][[language]][["mono"]])
    if(is.null(cleanCorpus)){
        cleanCorpus <- VCorpus(DirSource(trainDir), readerControl = list(reader = readPlain, language = language, load = TRUE))
        cleanCorpus <- RemoveTDMGarbage(cleanCorpus)
    }
    tdm <- TermDocumentMatrix(cleanCorpus); rm(cleanCorpus);
    freqTable <- TDM2FrequencyTable(tdm); rm(tdm);
    predictionList <- freqTable[1:3, "phrase"]; rm(freqTable);

    Start <- function(){predictionList}
    Next <- function(word){predictionList}

    ngramPredictors[[trainDir]][[language]][["mono"]] <<- list(Start = Start, Next = Next)
    list(Start = Start, Next = Next)
}

TrainBigramPredictor <- function(trainDir, language, cleanCorpus = NULL){
    if(!is.null(ngramPredictors[[trainDir]][[language]][["bi"]])) 
        return(ngramPredictors[[trainDir]][[language]][["bi"]])
    
    if(is.null(cleanCorpus)){
        cleanCorpus <- VCorpus(DirSource(trainDir), readerControl = list(reader = readPlain, language = language, load = TRUE))
        cleanCorpus <- RemoveTDMGarbage(cleanCorpus)
    }
    cPredictor <- TrainMonogramPredictor(trainDir, language, cleanCorpus);
    bigram_tdm <- TermDocumentMatrix(cleanCorpus, control = list(tokenize = CreateNGramTokenizer(2))); rm(cleanCorpus)
    bigramTextMatrix <- as.matrix(bigram_tdm) ; rm(bigram_tdm)
    bigramTextMatrix <- bigramTextMatrix[rowSums(bigramTextMatrix)>2,]

    predictList <- PredictorListFromTM(bigramTextMatrix); rm(bigramTextMatrix);
    
    
    lastValues=character(1)
    Start <- function(){
        cPredictor$Start()
    }
    Next <- function(word){
        cleanWord <- WordCleaner(word)
        
        if(cleanWord == "") cleanWord <- lastValues
        else lastValues <<- cleanWord
        
        if(cleanWord == "" || is.null(predictList[[cleanWord]])){
            return(cPredictor$Next(cleanWord))
        }
        biPredict <- predictList[[cleanWord]]
        nEmpty <- sum(biPredict == "")
        biPredict[(4-nEmpty):3] <- cPredictor$Start()[1:nEmpty]
        return(biPredict)
    }
    print(paste("Finished training bigram predictor: ", language))
    ngramPredictors[[trainDir]][[language]][["bi"]] <<- list(Start = Start, Next = Next)
    list(Start = Start, Next = Next)
}

TrainTrigramPredictor <- function(trainDir, language){
    # if(!is.null(ngramPredictors[[trainDir]][[language]][["tri"]])) 
        # return(ngramPredictors[[trainDir]][[language]][["tri"]])
    cleanCorpus <- VCorpus(DirSource(trainDir), readerControl = list(reader = readPlain, language = language, load = TRUE))
    cleanCorpus <- RemoveTDMGarbage(cleanCorpus); print(mem_used())
    # biPredictor <- TrainBigramPredictor(trainDir, language, cleanCorpus); 
    print(mem_used())
    trigram_tdm <- TermDocumentMatrix(cleanCorpus, control = list(tokenize = CreateNGramTokenizer(3))); rm(cleanCorpus)
    print(mem_used())
    return(trigram_tdm)
    # print(inspect(trigram_tdm))
    trigramTextMatrix <- as.matrix(trigram_tdm); rm(trigram_tdm);
    trigramTextMatrix <- trigramTextMatrix[rowSums(trigramTextMatrix)>2,]
    print(mem_used())
    
    predictList <- PredictorListFromTM(trigramTextMatrix); rm(trigramTextMatrix);
    print(mem_used())
    lastValues=character(2)
    Start <- function(){
        biPredictor$Start()
    }
    Next <- function(word){
        cleanWord <- WordCleaner(word)
        biPredict <- biPredictor$Next(cleanWord)
        
        if(cleanWord != ""){
            lastValues[1] <<- lastValues[2]
            lastValues[2] <<- cleanWord
        }
        newStr <- paste(lastValues, collapse=" ")
        
        triPredict <- predictList[[newStr]]
        nEmpty <- sum(triPredict == "")
        if(!nEmpty) return(triPredict)
        triPredict[(4-nEmpty):3] <- biPredict[1:nEmpty]
        return(triPredict)
    }
    print(mem_used())
    print(paste("Finished training trigram predictor: ", language))
    ngramPredictors[[trainDir]][[language]][["tri"]] <<- list(Start = Start, Next = Next)
    list(Start = Start, Next = Next)
}

if(!exists("ngramPredictors")) ngramPredictors <- list()
