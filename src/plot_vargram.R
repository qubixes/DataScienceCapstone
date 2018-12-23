source("Flipcat/shiny_load.R")
source("tester.R")
source("multi_predictor.R")

myMemUsage <- "highest"
myTokenizer <- "slow"
myPred <- NewNGramPredictor(tokenizer=myTokenizer)

allResults <- list()
for(lang in allLang){
	myPred$InitPredList(predictors[[lang]][[myMemUsage]])
    allResults[[lang]] <- Tester(file.path(testDir, lang), myPred)
}

newRes <- cbind(data.frame(StraightenResults(allResults, 4)), language = allLang)
resPlot <- PlotResults(newRes)
print(resPlot)
