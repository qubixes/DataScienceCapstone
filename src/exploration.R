srcDir <- dirname(sys.frame(1)$ofile)

source(file.path(srcDir, "init_data.R"))

library(tm)
library(dplyr)
library(tidyr)
library(ggplot2)



ExploreTDM <- function(tdm, gram){
    #Get frequency matrix
    textMatrix <- as.matrix(tdm)
    
    #Get all of the occurance frequencies, and sort them.
    freqT <- findFreqTerms(tdm, 30)
    frequencies <- rowSums(textMatrix, na.rm=TRUE)
    totCounts <- colSums(textMatrix)
    freqOrder <- frequencies[order(frequencies, decreasing=TRUE)]/sum(totCounts)
    cumFreq <- cumsum(freqOrder)
    
    #Put the whole thing in a data phrame with several columns (including cumulative occurances), and thin them out to save memory
    dfCumFreq <- data.frame(fraction=(1:length(cumFreq))/length(cumFreq), occurance=freqOrder, cumFreq=cumFreq, 
                            gram=gram, rank=1:length(cumFreq))
    dfSelected <- round(exp(seq(0,log(length(cumFreq)), length=500))) %>% unique
    dfCumFreq <- dfCumFreq[dfSelected,]
    rownames(dfCumFreq) <- NULL
    
    #Get top 20 most frequent phrases
    allOrder <- textMatrix[order(frequencies, decreasing=TRUE),]
    allDf <- data.frame(allOrder)
    allDf <- mutate(allDf, word=rownames(allDf))
    rownames(allDf) <- NULL
    allDf$word <- factor(allDf$word, levels=allDf$word)
    allDf <- head(allDf, 20)
    allDf <- gather(allDf, key="file", value="count", -word)
    allDf <- mutate(allDf, freq = count/totCounts[file])
    
    #Make the bar plot of these 20
    plot_tdm <- ggplot(allDf, aes(x=word, y=freq, fill=file))+geom_bar(stat="identity", position=position_dodge()) + 
        theme(axis.text.x=element_text(angle=90, hjust=1))
    
    list(plot_tdm, dfCumFreq)
}

BigramTokenizer <- function(x){ unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE) }
TrigramTokenizer <- function(x){ unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE) }


curDir <- file.path(devDir, "en_US")

tmData <- VCorpus(DirSource(curDir), readerControl = list(reader = readPlain, language = "en_US", load = TRUE))
tdm <- TermDocumentMatrix(tmData, list(stemming = FALSE, stopwords = FALSE))

onegram <- ExploreTDM(tdm, 1); rm("tdm")


bigram_tdm <- TermDocumentMatrix(tmData, control = list(tokenize = BigramTokenizer))
bigram <- ExploreTDM(bigram_tdm, 2); rm("bigram_tdm")

trigram_tdm <- TermDocumentMatrix(tmData, control = list(tokenize = TrigramTokenizer))
trigram <- ExploreTDM(trigram_tdm, 3); rm("trigram_tdm")

allgram_cumFreq <- rbind(onegram[[2]], bigram[[2]], trigram[[2]])

cumFreqPlot <- ggplot(allgram_cumFreq, aes(x=fraction, y=cumFreq, colour=factor(gram)))+geom_line()+scale_x_continuous(trans='log10')
freqDecreasePlot <- ggplot(allgram_cumFreq, aes(x=rank, y=occurance, colour=factor(gram)))+geom_line()+scale_x_continuous(trans = "log10")+scale_y_continuous(trans="log10")

xes <- round(exp(seq(0,log(1e5), length=500))) %>% unique

regress <- data.frame(rank=xes, x1=0.06*xes^-0.9, x2=0.0045*xes^-0.6, x3=3.4e-4*xes^-0.43)

freqDecreaseWLine <- freqDecreasePlot + geom_line(data=regress, mapping= aes(x=rank, y=x1), colour="red")+ geom_line(data=regress, mapping= aes(x=rank, y=x2), colour="green")+ geom_line(data=regress, mapping= aes(x=rank, y=x3), colour="blue")
