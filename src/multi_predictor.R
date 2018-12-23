library(quanteda)
library(tm)
library(plyr)
library(dplyr)
library(Rcpp)

#Load custom ngram computation.
sourceCpp("vargram.cpp")

#Define a presentation for the '.' character.
dotRepresentation <- "xxxdotxxx"

#We get a n-gram list with counts, and need to make it into something useful.
#This "useful" thing is a hashed environment where we can look for the (n-1)-gram and get the top three choices.

PredictorListFromCPL <- function(countPhraseList, ngram){
    count = countPhraseList[["count"]]
    phrase = countPhraseList[["phrase"]]
    
    #In case of a 1-gram, just return a list of the three most common words (excepting the "."). 
    if(ngram == 1){
        oPhrase  = phrase[order(count, decreasing = T)]
        oPhrase <- oPhrase[oPhrase != dotRepresentation]
        return(oPhrase[1:3])
    }
    
    #If there's no phrases/counts, return an empty environment.
    if(length(phrase) == 0){
        return(emptyenv())
    }
    
    #Split the phrases into two parts: (n-1),(1)-grams. For some reason, the split sometimes fails, so we throw those away.
    splitPhrase <- strsplit(phrase, ' (?=[^ ]+$)', perl=TRUE)
    goodSplits <- (sapply(splitPhrase, length) == 2)
    count <- count[ goodSplits ] 
    splitPhrase <- splitPhrase[ goodSplits ] %>% unlist
    firstTerm <- splitPhrase[seq(1, length(splitPhrase), 2)]; secondTerm <- splitPhrase[seq(2, length(splitPhrase), 2)]
    
    #Remove all the "." predictions, since these are unwanted.
    firstTerm <- firstTerm[secondTerm!=dotRepresentation]; count <- count[secondTerm!=dotRepresentation]; secondTerm <- secondTerm[secondTerm!=dotRepresentation]; 
    
    #Create a dataframe and use grouping to find the top-3 choices among the predictions.
    btmDf <- data.frame(count=count, firstTerm  = firstTerm, secondTerm = secondTerm)
    btmDfBest <- btmDf %>% group_by(firstTerm) %>% top_n(3,count) 
    allFirst <- unique(as.character(btmDf$firstTerm))
    btmDfBest <- btmDfBest[order(as.character(btmDfBest$firstTerm)),]
    #For some bizarre reason the ordering doesn't quite do it's job properly, so the length of the vector can be 
    #slightly bigger than expected. Create a list of predictions.
    bestList <- vector("list", length = length(allFirst)*1.1) 
    bestList <- mapply(function(x){ x=character(3)}, bestList, SIMPLIFY = F)
    
    nSame=0
    iList=0
    prev="xxx"
    #Now go through the predictions data frame and put them in the allocated list. 
    #If subsequent (n-1)-grams are the same, stop them in the same list, otherwise move to the next item.
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
    #Hash the environment.
    list2env(bestList, hash = TRUE)
}

#Create tokens from files in a training directory, and a given language.
GetTokens <- function(trainDir, language){
    myCorpus <- Corpus(DirSource(trainDir), readerControl = list(reader = readPlain, language = language, load = TRUE))
    myTokens <- corpus(myCorpus) %>% tokens(what="word", remove_punct=T, remove_symbols=T, remove_hyphens=T) %>% tolower
    rm("myCorpus")
    myTokens
}

#Same as above, but transform all "."'s by special tokens.
GetTokens2 <- function(trainDir, language){
    myCorpus <- Corpus(DirSource(trainDir), readerControl = list(reader = readPlain, language = language, load = TRUE))
    myTokens <- corpus(myCorpus) %>% tokens(what="word", remove_punct=F, remove_symbols=T, remove_hyphens=T) %>% tokens_replace(pattern= ".", replacement = dotRepresentation) %>% tokens(what="fastestword", remove_punct=T) %>% tolower
    rm("myCorpus")
    myTokens
}


#This trains the predictor on data. The result is a variable n-gram predictor with a given maximum and an occurence threshold.
#The parameters are given in the "parameters" list. myTokens is an optional argument in case
#the tokens are already loaded in memory somewhere. 

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
    
    #Load the tokens into memory.
    if(is.null(myTokens)) myTokens <- GetTokens2(trainDir, language)
    if(!is.null(maxTokens) && maxTokens < length(myTokens)) myTokens = myTokens[1:maxTokens]
    
    #The parameter minOccurence puts a lower limit on minimum number of occurences of a certain x-gram to be counted.
    #The problem is that for small corpii, we might count single occurences, which might not be desirable 
    #(using a lot of memory and overfitting the data).
    if(!is.null(minOccurence)){
        if(threshold*length(myTokens) < minOccurence)
            threshold <- (minOccurence+0.1)/length(myTokens)
    }
    
    #The meat of the whole algorithm. This is a C++ function defined in vargram.cpp.
    myList <- GetMultigram(myTokens, threshold, maxNGram)
    
    rm("myTokens"); 
    
    #Get the results from the C++ function into a format suitable for R.
    predictorList = list()
    for(i in 1:maxNGram)
        predictorList[[i]] = PredictorListFromCPL(myList[[i]], i)
    print(paste("Finished training language:", language))
    predictorList
}

#Legacy tokenizer
TokenizeFaster <- function(words){
    gsub('[.]', paste0(" ", dotRepresentation, " "), words) %>% tokens(what="fastestword", remove_punct=T, remove_symbols=T, remove_hyphens=T) %>% tolower
}

#Legacy tokenizer
TokenizeFastest <- function(words){
    words %>% tokens(what="fastestword", remove_punct=T, remove_symbols=T, remove_hyphens=T) %>% tolower
}

#Legacy tokenizer
TokenizeSlower <- function(words){
    words %>% tokens(what="fastestword", remove_punct=F, remove_symbols=T, remove_hyphens=T) %>% tokens_replace(pattern= ".", replacement = dotRepresentation) %>% tokens(what="fastestword", remove_punct=T) %>% tolower
}


NewNGramPredictor <- function(newPredList = NULL, tokenizer = "slow"){
    #Slow tokenizer using quanteda to transform text into tokens, taking care of "."'s differently.
    TokenizeSlow <- function(words){
        gsub('[.]', paste0(" ", dotRepresentation, " "), words) %>% tokens(what="word", remove_punct=T, remove_symbols=T, remove_hyphens=T) %>% tolower
    }
    
    #Same idea, but use the "fastestword" quanteda algorithm. 
    #Unfortunately contains a bug (23/12/2018) that fails to remove some puncutation.
    TokenizeFast <- function(words){
        gsub('[.]', paste0(" ", dotRepresentation, " "), words) %>% tokens(what="fastestword", remove_punct=T, remove_symbols=T, remove_hyphens=T) %>% tolower %>% removePunctuation
    }    
    
    predictorList <- NULL
    myTokenizer <- NULL
    maxNGram <- integer(1)
    lastValues <- character(1)
    
    #Function to switch prediction data structures.
    InitPredList <- function(newList){
        if(!is.null(newList)){
            predictorList <<- newList
            maxNGram <<- length(predictorList)
            lastValues <<- character(maxNGram-1)
        } else {
            predictorList <<- NULL
            maxNGram <<- 1
            lastValues <<- character(0)
        }
        
    }
    
    #Function to switch the tokenizer.
    SetTokenizer <- function(newTokenizer="slow"){
        if(newTokenizer == "fast"){ 
            myTokenizer <<- TokenizeFast
         } else {
            myTokenizer <<- TokenizeSlow
        }
    }
    
    #Start up the prediction with only an assumptive end of sentence.
    Start <- function(){
        if(maxNGram <= 0){
            print("Error: didn't load prediction data.")
        }
        lastValues <<- character(maxNGram-1)
        Next(".")
    }
    
    #Make the next prediction based on previously entered words. "word" here can be any number of words.
    Next <- function(word){
        #Tokenize the text
        cleanWord <- myTokenizer(word)
        
        # If not empty, put the new words at the end of lastValues cache, while shifting the cache to the front 
        # (basically a ring buffer).
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
        
        # Start with the biggest n(-gram), and try to fill it. Until it is filled, lower n. A simple back-off algorithm.
        nPredict=0; iGram=maxNGram; prediction = character(3)
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
            }
            iGram <- iGram-1
        }
        prediction[1:3]
    }
    
    SetTokenizer(tokenizer)
    InitPredList(newPredList)
    
    list(Start = Start, Next = Next, SetTokenizer = SetTokenizer, InitPredList = InitPredList)
}

#Legacy (doesn't work anymore).
GetStatsPredictor <- function(predictor){
        predList <- get("predictorList", environment(predictor$Start))
        nElem <- integer(length(predList))
        for( i in seq(length(predList))){
            nElem[i] <- length(predList[[i]]) 
        }
        nElem
}


