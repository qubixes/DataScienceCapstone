srcDir <- dirname(sys.frame(1)$ofile)
source(file.path(srcDir, "tester.R"))

if(!exists("monogram_dev_results"))
    monogram_dev_results <- GetPredictorResults(devDir, testDir, TrainMonogramPredictor)

if(!exists("bigram_dev_results"))
    bigram_dev_results <- GetPredictorResults(devDir, testDir, TrainBigramPredictor)

# if(!exists("trigram_dev_results"))
    # trigram_dev_results <- GetPredictorResults(devDir, testDir, TrainTrigramPredictor)


# if(!exists("monogram_train_results"))
    # monogram_train_results <- GetPredictorResults(trainDir, testDir, TrainMonogramPredictor)
# 
# if(!exists("bigram_train_results"))
    # bigram_train_results <- GetPredictorResults(trainDir, testDir, TrainBigramPredictor)
# 
# if(!exists("trigram_train_results"))
    # trigram_train_results <- GetPredictorResults(trainDir, testDir, TrainTrigramPredictor)
