
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


allLang <- c(German = "de_DE", English = "en_US", Finnish = "fi_FI", Russian = "ru_RU")
allThresholds <- c(lowest=3e-6, low=1e-6, high=3e-7, highest=1e-7)
predictDir <- "predict"

if(!exists("predictors")){
    memUsed <- list()
    predictors <- list()
    for(lang in allLang){
        parameters[["language"]] = lang
        predictors[[lang]] <- list()
        for(thres in names(allThresholds)){
            parameters[["threshold"]] = allThresholds[thres]
            fileBase <- ParametersToFilename(parameters = parameters)
            predFile <- file.path(predictDir, paste0(fileBase, ".rds"))
            print(paste("Getting file: ", predFile))
            if(file.exists(predFile)){
                predictors[[lang]][[thres]] <- readRDS(predFile)
                if(lang == allLang[1]){
                    memUsed[[thres]] <- object.size(serialize(predictors[[lang]][[thres]], connection=NULL))
                } else {
                    memUsed[[thres]] <- memUsed[[thres]] + object.size(serialize(predictors[[lang]][[thres]], connection=NULL))
                }
            } else{
                print(paste("Error loading prediction file:", predFile))
            }
        }
    }
}

memFormat <- sapply(memUsed, format, "MiB", digits=0)