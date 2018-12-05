srcDir <- dirname(sys.frame(1)$ofile)
source(file.path(srcDir, "init_data.R"))
source(file.path(srcDir, "corpus.R"))


CreateConstantPredictor <- function(trainDir, language){
    tmData <- VCorpus(DirSource(curDir), readerControl = list(reader = readPlain, language = language, load = TRUE))
    tdm <- TermDocumentMatrix(tmData)
    freqTable <- TDM2FrequencyTable(tdm)
    
    predictionList <- freqTable[1:3, "phrase"]
    Start <- function(word){predictionList}
    Next <- function(word){predictionList}
    list(Start = Start, Next = Next)
}

