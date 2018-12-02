library(tm)

swiftFile <- "swiftkey.zip"
if(!file.exists(swiftFile)){
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", swiftFile)
    unzip(swiftFile)
}

EN_dir <- file.path("final", "en_US")

textFiles <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
textFiles <- file.path(EN_dir, textFiles)

FindLargestLine <- function(file){
    con <- file(file, "r", encoding="UTF-8")
    maxLine=0
    
    while( length(curLine <- readLines(con, 1, skipNul = T)) > 0){
        maxLine <- max(maxLine, nchar(curLine))
    }
    close(con)
    maxLine
}

FindNPhrase <- function(file, phrase){
    con <- file(file, "r", encoding="UTF-8")
    nOcc=0
    
    while( length(curLine <- readLines(con, 1, skipNul = T)) > 0){
        nOcc <- nOcc+grepl(phrase, curLine)
    }
    close(con)
    nOcc    
}

ShowMatches <- function(file, phrase){
    con <- file(file, "r", encoding="UTF-8")
    nOcc=0
    
    while( length(curLines <- readLines(con, 10, skipNul = T)) > 0){
        if(length(goodLines <- grep(phrase, curLines, value=T)))
            print(goodLines)
    }
    close(con)
}

# maxLines <- sapply(textFiles, FUN=FindLargestLine)
# nLove <- FindNPhrase(textFiles[3], "love")
# nHate <- FindNPhrase(textFiles[3], "hate")
ShowMatches(textFiles[3], "A computer once beat me at chess, but it was no match for me at kickboxing")