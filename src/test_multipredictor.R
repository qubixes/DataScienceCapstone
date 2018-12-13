source("multi_predictor.R")
source("tester.R")

parameters <- list(threshold=1e-6, maxNGram=5)
language <- "fi_FI"
deTestDir <- file.path(testDir, language)
deDevDir <- file.path(devDir, language)
# if(!exists("myTestTokens")){
    myTestTokens <- GetTokens(deTestDir, language)
    myDevTokens <- GetTokens(deDevDir, language)
    myTrainTokens <- GetTokens(file.path(trainDir, language), language)
# }

# myPred <- TrainMultigramPredictor(deDir, language, parameters)
# myPred$Start() %>% print
# myPred$Next("We") %>% print
# myPred$Next("are") %>% print
# myPred$Next("going") %>% print
# GetStatsPredictor(myPred) %>% print


# set.seed(12938714)
nTry=5
myDf <- data.frame(threshold=numeric(0), ngram=numeric(0), source=character(0), score=numeric(0), stringsAsFactors = F)
gramNames = paste("gram", 1:7)
for(i in (seq(nTry)+nrow(myDf))){
    parameters[["threshold"]] = 1e-7*10^(runif(1)/59)
    parameters[["maxNGram"]] = sample(2:7, 1, replace=T)
    
    if(rbinom(1,1,0.5) != 0.5){
        myTokens=myTrainTokens
        curSource = "train"
        curTestDir = file.path(testDir, language)
    }
    else{
        myTokens=myDevTokens
        curSource = "dev"
        curTestDir = file.path(testDir, language)
    }
    
    curPredictor <- TrainMultigramPredictor(NULL, NULL, parameters, myTokens)
    curCounts <- GetStatsPredictor(curPredictor); length(curCounts) <- 7
    results <- Tester(curTestDir, curPredictor)
    score <- results[[1]]["score"]
    myDf[i,"threshold"] <- parameters[["threshold"]]
    myDf[i,"ngram"] <- parameters[["maxNGram"]]
    myDf[i,"source"] <- curSource
    myDf[i, "score"] <- score
    for(k in seq(7))
        myDf[i, gramNames[k]] <- curCounts[k]
}
orderedDf <- myDf[order(myDf$score, decreasing = T),]
newDf <- cbind(orderedDf, mem=rowSums(orderedDf[,5:11], na.rm=T))
print(newDf)