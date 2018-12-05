srcDir <- dirname(sys.frame(1)$ofile)
dataDir <- file.path(srcDir, "..", "data")
swiftFile <- file.path(dataDir, "swiftkey.zip")

# This function takes a file name and writes a number of randomly selected lines to another file.
CreateTrainDevTest <- function (srcFile, dstFiles, nLinesDev, nLinesTest){
    trainFile <- dstFiles[1]
    devFile   <- dstFiles[2]
    testFile  <- dstFiles[3]
    
    nLinesIn <- system(paste("wc -l", srcFile), intern = T)
    nLinesIn <- as.integer(strsplit(nLinesIn, "[ ]{1,}")[[1]][2])
    
    devTestLines <- sample(nLinesIn, nLinesDev+nLinesTest)
    devLines <- devTestLines[1:nLinesDev]
    testLines <- devTestLines[-(1:nLinesDev)]
    devLines <- devLines[order(devLines)]
    testLines <- testLines[order(testLines)]

    conSrc   <- file(srcFile, "r", encoding="UTF-8")
    conTrain <- file(trainFile, "w")
    conDev   <- file(devFile, "w")
    conTest  <- file(testFile, "w")

    curDevChoice=1
    curTestChoice=1
    for(i in seq(nLinesIn)){
        curLine <- readLines(conSrc, 1, skipNul = T)
        if(curDevChoice <= nLinesDev && i == devLines[curDevChoice]){
            writeLines(curLine, conDev)
            curDevChoice <- curDevChoice+1
        }
        else if(curTestChoice <= nLinesTest && i == testLines[curTestChoice]){
            writeLines(curLine, conTest)
            curTestChoice <- curTestChoice+1
        }
        else{
            writeLines(curLine, conTrain)
        }
    }
    close(conSrc); close(conTrain)
    close(conDev); close(conTest)
}


# Download the file if it doesn't exist yet.
if(!file.exists(swiftFile)){
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", swiftFile)
    unzip(swiftFile)
}

allNewDirs = file.path(dataDir, c("train", "dev", "test"))
trainDir <- allNewDirs[1]
devDir   <- allNewDirs[2]
testDir  <- allNewDirs[3]

if(sum(!dir.exists(allNewDirs))){
    for(dir in allNewDirs) dir.create(dir, showWarnings = F)
    
    set.seed(128371)
    languages <- dir(file.path(dataDir, "final"))
    
    for (language in languages){
        curSrcDir <- file.path(dataDir, "final", language)
        curDstDirs <- file.path(allNewDirs, language)
        for(dir in curDstDirs) dir.create(dir, showWarnings = F)
        curFiles <- dir(curSrcDir)
        for (file in curFiles){
            CreateTrainDevTest(file.path(curSrcDir, file), file.path(curDstDirs, file), 10000, 10000)
        }
    }
}
