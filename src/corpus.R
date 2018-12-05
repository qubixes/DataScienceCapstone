TDM2FrequencyTable <- function(tdm){
    textMatrix <- as.matrix(tdm)
    totCounts <- colSums(textMatrix)
    
    textMatrix <- apply(textMatrix, 2, function(x) x/sum(x))
    textMatrix <- cbind(textMatrix, meanFreq = apply(textMatrix, 1, mean))
    textMatrix <- textMatrix[order(textMatrix[,"meanFreq"], decreasing = T),]
    
    textDf <- as.data.frame(textMatrix)
    textDf <- mutate(textDf, phrase=rownames(textDf))
    rownames(textDf) <- NULL
    
    
    
    textDf
    
}