Rprof("out.rprof")
res <- Tester(myDir, myPred)
Rprof(NULL)
summaryRprof("out.rprof")