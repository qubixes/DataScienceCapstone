#server.R: Server side code for the CatFlip application. See shiny_load.R, shiny_tester.R and multipredictor.R for more details
# on the actual algorithm and loading process. 


library(shiny)
library(ggplot2)
library(data.table)

source("shiny_multi_predictor.R")
source("shiny_load.R")
source("shiny_tester.R")


shinyServer(function(input, output, session) {
    memSizeChoices <- paste0(names(allThresholds), " (", memFormat[1:length(allThresholds)], ")")
    updateSelectInput(session, "mem_usage", label="memory usage", choices = memSizeChoices, selected = memSizeChoices[length(memSizeChoices)])
    
    #This is the predictor we'll be using most of the time. 
    myPred <- NewNGramPredictor()
    
    #Sometimes we want to manually refresh the buttons/plot.
    rv <- reactiveValues()
    rv$refreshPlot <- 0
    rv$refreshButtons <- 0
    
    #A copy of the current text most of the time. Probably better use isolate(), but it #works.
    curText <- ""
    
    #Current predictions of the algorithm.
    newChoices <- character(3)
    
    #If the text changes (i.e. something is typed/removed) or in case of a manual refresh, we update our preditions.
    observeEvent({input$curText;rv$refreshButtons}, {
        if(input$curText != ""){
             myPred$Start()
             newChoices <<- myPred$Next(input$curText)
        } else {
            newChoices <<- myPred$Start()
        }
        updateActionButton(session, "choice1", label=newChoices[1])
        updateActionButton(session, "choice2", label=newChoices[2])
        updateActionButton(session, "choice3", label=newChoices[3])
     })
    
    #If one of the buttons is pressed, put that prediction in the text box.
    observeEvent(input$choice1, {
        updateTextAreaInput(session, "curText", value = paste0(trimws(input$curText, "right"), " ", newChoices[1], " "));
    })
    observeEvent(input$choice2, {
        updateTextAreaInput(session, "curText", value = paste0(trimws(input$curText, "right"), " ", newChoices[2], " "));
    })
    observeEvent(input$choice3, {
        updateTextAreaInput(session, "curText", value = paste0(trimws(input$curText, "right"), " ", newChoices[3], " "));
    })
    
    #If the language or memory usage is changed, change the predictor accordingly.
    observeEvent({input$language; input$mem_usage}, {
        newLang <- allLang[[input$language]]
        newMem <- which(input$mem_usage == memSizeChoices)
        if(length(newMem)<1) newMem<-1
        myPred$InitPredList(predictors[[newLang]][[newMem]])
        rv$refreshButtons <- rv$refreshButtons+1
    })
    
    #Change from fast to slow tokenization (x10 speed difference) or vice versa.
    observeEvent(input$tokenize, {
        if(input$tokenize == T){
            myPred$SetTokenizer("fast");
        } else {
            myPred$SetTokenizer("slow");
        }
    })
    
    # output$curLanguage <- renderText(paste("Current language: ", input$language))
    
    #This is the event for the refresh button. Update the plot if it is pressed.
    observeEvent(input$recalcButton, {rv$refreshPlot <- rv$refreshPlot+1; curText <<- input$curText})
    
    #Plot the prediction performance of the current text.
    pChoice <- c("all",as.character(1:3))
    output$testText <- renderPlot({
        if(rv$refreshPlot >= 0){
            if(input$test_all_models == F){
                #Here we only plot and test the performance of the currently selected model. 
                fracCorrect <- TextTester(curText, myPred)
                myDf <- data.frame(choice=factor(pChoice, levels=pChoice), correct=100*c(sum(fracCorrect), fracCorrect))
                myDf$labels <- as.character(round(myDf$correct, digits=2))
                #Plot with a bar histogram
                g <- ggplot(data=myDf, aes(x=choice, y=correct, fill=choice)) + geom_bar(stat="identity") + geom_text(data=myDf, aes(x=choice, y=correct, label=labels), colour="black", vjust=2)
            } else {
                #Otherwise plot with all model sizes, but the same language. Thus, we create a new predictor.
                newPred <- NewNGramPredictor()
                if(input$tokenize == T){
                    newPred$SetTokenizer("fast")
                } else {
                    newPred$SetTokenizer("slow")
                }
                curLang <- allLang[input$language]
                
                choiceV <- character(0)
                correctV <- numeric(0)
                nameV <- character(0)
                
                #Create a long (tidy) vector from the test results of the different models.
                for(iPred in 1:length(allThresholds)){
                    newPred$InitPredList(predictors[[curLang]][[iPred]])
                    fracCorrect <- TextTester(curText, newPred)
                    choiceV <- c(choiceV, pChoice);
                    correctV <- c(correctV, 100*c(sum(fracCorrect), fracCorrect))
                    nameV <- c(nameV, rep(memSizeChoices[iPred], 4))
                }
                
                myDf <- data.frame(choice=factor(choiceV, levels=pChoice), correct = correctV, 
                                   models= factor(nameV, levels<- nameV[seq(length(nameV), 1, -4)]), labels = as.character(round(correctV,1)))
                g <- ggplot(data=myDf, aes(x=choice, y=correct, fill=models)) + geom_bar(stat="identity", position=position_dodge()) + geom_text(data=myDf, aes(x=choice, y=correct, label=labels), colour="black", vjust=2, position=position_dodge(width=0.9))
            }
            g + ylab("% correctly predicted")
        }
    })
})
