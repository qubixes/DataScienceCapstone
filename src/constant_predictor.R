srcDir <- dirname(sys.frame(1)$ofile)
source(file.path(srcDir, "init_data.R"))
source(file.path(srcDir, "corpus.R"))


CreateConstantPredictor <- function(trainDir, language){
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
    # PatternRemover <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x))})
    tmData <- tm_map(tmData, content_transformer(WordCleaner))
    # tmData <- tm_map(tmData, removePunctuation)
    # tmData <- tm_map(tmData, PatternRemover, '"')
    # tmData <- tm_map(tmData, PatternRemover, "_")
    # tmData <- tm_map(tmData, PatternRemover, "-")
    # tmData <- tm_map(tmData, PatternRemover, "—")
    # tmData <- tm_map(tmData, PatternRemover, "“")
    # tmData <- tm_map(tmData, PatternRemover, "”")
    # tmData <- tm_map(tmData, PatternRemover, "\u2013")
    # tmData <- tm_map(tmData, PatternRemover, "…")
    # tmData <- tm_map(tmData, PatternRemover, "·")
    # tmData <- tm_map(tmData, PatternRemover, '["|_|“|”|\u2013|…|·|-]')
    # 
    # tmData <- tm_map(tmData, content_transformer(function(x){gsub()}))
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
    # tm_map(tmData, removePunctuation)
    
    
    onegram_tdm <- TermDocumentMatrix(tmData)
    bigram_tdm <- TermDocumentMatrix(tmData, control = list(tokenize = CreateNGramTokenizer(2)))
    # trigram_tdm <- TermDocumentMatrix(tmData, control = list(tokenize = CreateNGramTokenizer(3)))
    
    bigramTextMatrix <- as.matrix(bigram_tdm)
    bigramTextMatrix <- bigramTextMatrix[rowSums(bigramTextMatrix)>2,]
    # View(bigramTextMatrix)
    predictList <- PredictorListFromTM(bigramTextMatrix)
    cPredictor <- CreateConstantPredictor(trainDir, language)
    
    
    lastValues=character(1)
    Start <- function(){
        cPredictor$Start()
    }
    Next <- function(word){
        cleanWord <- WordCleaner(word)
        
        if(cleanWord == "") cleanWord <- lastValues
        else lastValues <<- cleanWord
        
        if(is.null(predictList[[cleanWord]])){
            return(cPredictor$Next(cleanWord))
        }
        return(predictList[[cleanWord]])
    }
    list(Start = Start, Next = Next)
}


# btmDf <- mutate(btmDf, rate = mean(1:3))
# btmDf <- mutate(btmDf, firstTerm = strsplit(terms, " ")[[1]][1], secondTerm = strsplit(terms, " ")[[1]][2])


