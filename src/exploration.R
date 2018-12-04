srcDir <- dirname(sys.frame(1)$ofile)

source(file.path(srcDir, "init_data.R"))

library(tm)
library(dplyr)
library(tidyr)
library(ggplot2)

ExploreTDM <- function(tdm, gram){
    textMatrix <- as.matrix(tdm)
    
    freqT <- findFreqTerms(tdm, 30)
    frequencies <- rowSums(textMatrix, na.rm=TRUE)
    totCounts <- colSums(textMatrix)
    freqOrder <- frequencies[order(frequencies, decreasing=TRUE)]/sum(totCounts)
    cumFreq <- cumsum(freqOrder)
    allOrder <- textMatrix[order(frequencies, decreasing=TRUE),]
    allDf <- data.frame(allOrder)
    
    allDf <- mutate(allDf, word=rownames(allDf))
    rownames(allDf) <- NULL
    allDf$word <- factor(allDf$word, levels=allDf$word)
    allDf <- head(allDf, 20)
    allDf <- gather(allDf, key="file", value="count", -word)
    allDf <- mutate(allDf, freq = count/totCounts[file])
    
    plot_tdm <- ggplot(allDf, aes(x=word, y=freq, fill=file))+geom_bar(stat="identity", position=position_dodge()) + 
        theme(axis.text.x=element_text(angle=90, hjust=1))
    dfCumFreq <- data.frame(fraction=(1:length(cumFreq))/length(cumFreq), cumFreq=cumFreq, gram=gram)
    rownames(dfCumFreq) <- NULL
    list(plot_tdm, dfCumFreq)
}

BigramTokenizer <- function(x){ unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE) }
TrigramTokenizer <- function(x){ unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE) }


curDir <- file.path(devDir, "en_US")

tmData <- VCorpus(DirSource(curDir), readerControl = list(reader = readPlain, language = "en_US", load = TRUE))
tdm <- TermDocumentMatrix(tmData, list(stemming = FALSE, stopwords = FALSE))

onegram <- ExploreTDM(tdm, 1)


bigram_tdm <- TermDocumentMatrix(tmData, control = list(tokenize = BigramTokenizer))
bigram <- ExploreTDM(bigram_tdm, 2)

trigram_tdm <- TermDocumentMatrix(tmData, control = list(tokenize = TrigramTokenizer))
trigram <- ExploreTDM(trigram_tdm, 3)

allgram_cumFreq <- rbind(onegram[[2]], bigram[[2]], trigram[[2]])

cumFreqPlot <- ggplot(allgram_cumFreq, aes(x=fraction, y=cumFreq, colour=factor(gram)))+geom_line()+scale_x_continuous(trans='log10')
