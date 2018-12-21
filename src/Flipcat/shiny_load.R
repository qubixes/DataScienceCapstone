
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


allLang <- c("de_DE", "en_US", "fi_FI", "ru_RU")
predictDir <- "predict"

if(!exists("predictors")){
    predictors <- list()
    for(lang in allLang){
        parameters[["language"]] = lang
        fileBase <- ParametersToFilename(parameters = parameters)
        predFile <- file.path(predictDir, paste0(fileBase, ".rds"))
        if(file.exists(predFile)){
            predictors[[lang]] <- readRDS(predFile)
        } else{
            print(paste("Error loading prediction file:", predFile))
        }
    }
}