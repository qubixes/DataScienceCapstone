srcDir <- dirname(sys.frame(1)$ofile)
source(file.path(srcDir, "init_data.R"))
source(file.path(srcDir, "tester.R"))
source(file.path(srcDir, "multi_predictor.R"))

ParametersToFilename <- function(parameters){
    filename <- "pred"
    if(!is.null(parameters[["threshold"]])) filename <- paste0(filename, "_thres", parameters[["threshold"]])
    if(!is.null(parameters[["maxNGram"]])) filename <- paste0(filename, "_ngram", parameters[["maxNGram"]])
    if(!is.null(parameters[["minOccurence"]])) filename <- paste0(filename, "_occ", parameters[["minOccurence"]])
    if(!is.null(parameters[["language"]])) filename <- paste0(filename, "_", parameters[["language"]])
    if(!is.null(parameters[["trainDir"]])) filename <- paste0(filename, "_", parameters[["trainDir"]])
    filename
}

parameters <- list(threshold=1e-6, maxNGram=5, minOccurence=3, language="en_US", trainDir="train")

if(parameters[["trainDir"]] == "dev"){
    baseTrainDir <- devDir
    baseTestDir <- testDir
} else if(parameters[["trainDir"]] == "train"){
    baseTrainDir <- trainDir
    baseTestDir <- devDir
} else{
    baseTrainDir <- testDir
    baseTestDir <- devDir
}


allLang <- dir(trainDir)
predictDir <- file.path(srcDir, "predict")
if(!dir.exists(predictDir))
    dir.create(predictDir)

allResults <- list()
predictors <- list()
for(lang in allLang){
    parameters[["language"]] = lang
    fileBase <- ParametersToFilename(parameters = parameters)
    myTrainDir <- file.path(baseTrainDir, lang)
    myTestDir <- file.path(baseTestDir, lang)
    predFile <- file.path(predictDir, paste0(fileBase, ".rds"))
    if(file.exists(predFile)){
        predictors[[lang]] <- readRDS(predFile)
    } else{
        predictors[[lang]] <- TrainMultigramPredictor(trainDir = myTrainDir, parameters = parameters)
        saveRDS(predictors[[lang]], file = predFile)
    }
    # predictors[[lang]] <- myPred
    resFile <- file.path(predictDir, paste0(fileBase, ".dat"))
    if(file.exists(resFile)){
        myRes <- readRDS(resFile)
    } else {
        myRes <- Tester(myTestDir, predictors[[lang]])
        saveRDS(myRes, file = resFile)
    }
    allResults[[lang]] = myRes
}

newRes <- cbind(data.frame(StraightenResults(allResults, 4)), language = allLang)
resPlot <- PlotResults(newRes)
print(resPlot)
