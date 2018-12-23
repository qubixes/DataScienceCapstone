library(quanteda)
library(tm)

#Define a presentation for the '.' character.
dotRepresentation <- "xxxdotxxx"

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