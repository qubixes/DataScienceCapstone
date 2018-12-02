srcDir <- dirname(sys.frame(1)$ofile)
dataDir <- file.path(srcDir, "..", "data")
swiftFile <- file.path(dataDir, "swiftkey.zip")

FinalToSmallFile <- function (fileIn, fileOut, nLinesOut){
    nLinesIn <- system(paste("wc -l", fileIn), intern = T)
    nLinesIn <- as.integer(strsplit(nLinesIn, "[ ]{1,}")[[1]][2])
    
    chooseLines <- sample(nLinesIn, nLinesOut)
    chooseLines <- chooseLines[order(chooseLines)]
    
    con <- file(fileIn, "r", encoding="UTF-8")
    conOut <- file(fileOut, "w")
    nOcc=0
    curChoice=1
    for(i in seq(nLinesIn)){
        curLine <- readLines(con, 1, skipNul = T)
        if(curChoice <= nLinesOut && i == chooseLines[curChoice]){
            writeLines(curLine, conOut)
            curChoice <- curChoice+1
        }
    }
    close(con)
    close(conOut)
}


# Download the file if it doesn't exist yet.
if(!file.exists(swiftFile)){
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", swiftFile)
    unzip(swiftFile)
}

smallDir <- file.path(dataDir, "small")
if(!dir.exists(smallDir)){
    dir.create(smallDir)
    set.seed(128371)
    languages <- dir(file.path(dataDir, "final"))
    for (language in languages){
        curFinalDir <- file.path(dataDir, "final", language)
        curSmallDir <- file.path(smallDir, language)
        dir.create(curSmallDir, showWarnings = F)
        curFiles <- dir(curFinalDir)
        for (file in curFiles){
            FinalToSmallFile(file.path(curFinalDir, file), file.path(curSmallDir, file), 10000)
        }
    }
}
