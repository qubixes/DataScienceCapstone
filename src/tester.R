srcDir <- dirname(sys.frame(1)$ofile)
source(file.path(srcDir, "init_data.R"))
source(file.path(srcDir, "corpus.R"))
source(file.path(srcDir, "constant_predictor.R"))

SplitLine <- function(curLine){
    #Remove leading whitespace:
    curLine <- gsub("^\\s+", "", curLine)
    #Remove extra whitespace:
    curLine <- gsub("\\s+", " ", curLine)
    #Split the line:
    strsplit(curLine, " ")[[1]]
}

Tester <- function(testDir, predictor){
    files = dir(testDir)
    
    nTested = numeric(length(files))
    nCorrect = numeric(length(files))
    
    names(nTested) <- files
    names(nCorrect) <- files
    tStart <- Sys.time();
    
    for (curFile in files){
        con <- file(file.path(testDir, curFile), "r")
        
        curIt=0
        while(length(curLine <- readLines(con, 1, skipNul = T))>0 && curIt<1000){
            lineIn <- SplitLine(curLine)
            lineInClean <- tolower(sapply(lineIn, removePunctuation))
            
            curPrediction <- tolower(predictor$Start())
            # print(length(lineIn))
            for(i in seq(length(lineIn))){
                if(lineInClean[i] == "" || lineInClean[i] %in% curPrediction){
                    nCorrect[curFile] <- nCorrect[curFile]+1
                }
                nTested[curFile] <- nTested[curFile]+1
                curPrediction <- tolower(predictor$Next(lineIn[i]))
            }
            curIt <- curIt+1
        }
        # print(curIt)
        close(con)
    }
    fracCorrect = nCorrect/nTested
    score = mean(fracCorrect)
    tEnd <- Sys.time()
    list(c(score=100*score, ms_per_predict=1000*as.numeric(tEnd-tStart)/sum(nTested)), perc_correct=100*fracCorrect)
}

StraightenResults <- function(results, n){
    score <- numeric(0)
    ms_per_predict <- numeric(0)
    perc_correct <- matrix(nrow=n, ncol = length(results[[1]][[2]]))
    for(i in 1:n){
        score <- c(score, results[[i]][[1]]["score"])
        ms_per_predict <- c(ms_per_predict, results[[i]][[1]]["ms_per_predict"])
        perc_correct[i,] <- results[[i]][[2]]
    }
    matr <- cbind(score, ms_per_predict, perc_correct)
    rownames(matr) <- NULL
    colnames(matr) <- c(colnames(matr)[1:2], names(results[[1]][[2]]))
    matr
}

GetPredictorResults <- function(datadir, makePredictor){
    languages <- dir(datadir)
    allDirs <- file.path(datadir, languages)
    predictors <- mapply(makePredictor, allDirs, languages, SIMPLIFY = F)
    results <- mapply(Tester, allDirs, predictors, SIMPLIFY = F)
    results <- StraightenResults(results, length(languages))
    colnames(results) <- gsub(".txt$", "", colnames(results))
    colnames(results) <- gsub("^de_DE.", "", colnames(results))
    cbind(as.data.frame(results), language=languages)
}

PlotResults <- function(results){
    small_results <- gather(results, key=source, val=indiv_score, blogs, news, twitter, score)
    plot_tdm <- ggplot(small_results, aes(x=language, y=indiv_score, fill=source))+
        geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x=element_text(angle=90, hjust=1))
    plot_tdm
}

# results <- GetPredictorResults(testDir, CreateConstantPredictor)
# language <- "de_DE"
# curDir <- file.path(devDir, language)
# cPredictor <- CreateConstantPredictor(curDir,  language)
# results <- Tester(file.path(testDir, language), cPredictor)
# print(results)