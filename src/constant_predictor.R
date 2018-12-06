srcDir <- dirname(sys.frame(1)$ofile)
source(file.path(srcDir, "init_data.R"))
source(file.path(srcDir, "corpus.R"))


TrainMonogramPredictor <- function(trainDir, language){
    tmData <- VCorpus(DirSource(trainDir), readerControl = list(reader = readPlain, language = language, load = TRUE))
    tdm <- TermDocumentMatrix(tmData)
    freqTable <- TDM2FrequencyTable(tdm)
    
    predictionList <- freqTable[1:3, "phrase"]
    Start <- function(){predictionList}
    Next <- function(word){predictionList}
    list(Start = Start, Next = Next)
}

CreateNGramTokenizer <- function(n){
    NGramTokenizer <- function(x){ 
        unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE) 
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

TrainBigramPredictor <- function(trainDir, language){
    tmData <- VCorpus(DirSource(trainDir), readerControl = list(reader = readPlain, language = language, load = TRUE))
    tmData <- RemoveTDMGarbage(tmData)
    bigram_tdm <- TermDocumentMatrix(tmData, control = list(tokenize = CreateNGramTokenizer(2)))
    bigramTextMatrix <- as.matrix(bigram_tdm)
    bigramTextMatrix <- bigramTextMatrix[rowSums(bigramTextMatrix)>2,]

    predictList <- PredictorListFromTM(bigramTextMatrix)
    cPredictor <- TrainMonogramPredictor(trainDir, language)
    
    
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
    list(Start = Start, Next = Next)
}

TrainTrigramPredictor <- function(trainDir, language){
    tmData <- VCorpus(DirSource(trainDir), readerControl = list(reader = readPlain, language = language, load = TRUE))
    tmData <- RemoveTDMGarbage(tmData)
    
    trigram_tdm <- TermDocumentMatrix(tmData, control = list(tokenize = CreateNGramTokenizer(3)))
    
    trigramTextMatrix <- as.matrix(trigram_tdm)
    trigramTextMatrix <- trigramTextMatrix[rowSums(trigramTextMatrix)>2,]
    
    
    biPredictor <- TrainBigramPredictor(trainDir, language)
    predictList <- PredictorListFromTM(trigramTextMatrix)
    
    lastValues=character(2)
    Start <- function(){
        biPredictor$Start()
    }
    Next <- function(word){
        cleanWord <- WordCleaner(word)
        biPredict <- biPredictor$Next(cleanWord)
        
        if(cleanWord != ""){
            lastValues[2] <<- lastValues[1]
            lastValues[1] <<- cleanWord
        }
        newStr <- paste(lastValues, collapse=" ")
        
        triPredict <- predictList[[newStr]]
        nEmpty <- sum(triPredict == "")
        if(!nEmpty) return(triPredict)
        triPredict[(4-nEmpty):3] <- biPredict[1:nEmpty]
        return(triPredict)
    }
    print(paste("Finished training trigram predictor: ", language))
    list(Start = Start, Next = Next)
}


