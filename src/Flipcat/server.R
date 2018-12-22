#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(data.table)
library(shinyjs)
source("multi_predictor.R")
source("shiny_load.R")
source("shiny_tester.R")

choices <- character(3)

langCodes = c(German="de_DE", English="en_US", Finnish="fi_FI", Russian="ru_RU")

shinyServer(function(input, output, session) {
    i<-1
    
    memSizeChoices <- paste0(names(allThresholds), " (", memFormat, ")")
    updateSelectInput(session, "mem_usage", label="memory usage", choices = memSizeChoices, selected = memSizeChoices[length(memSizeChoices)])
    
    myPred <- NewNGramPredictor()
    rv <- reactiveValues()
    rv$refreshPlot <- 0
    
    curText <- ""
    
    newChoices <- character(3)
    
    # observe({runjs(jsx)})
    
    observeEvent(input$curText, {
        if(input$curText != ""){
             newChoices <<- myPred$Next(input$curText)
             
        } else {
            newChoices <<- myPred$Start()
        }
        updateActionButton(session, "choice1", label=newChoices[1])
        updateActionButton(session, "choice2", label=newChoices[2])
        updateActionButton(session, "choice3", label=newChoices[3])
     })
    
    observeEvent(input$choice1, {
        updateTextAreaInput(session, "curText", value = paste0(input$curText, newChoices[1], " "));         print(newChoices[1])}
    )
    observeEvent(input$choice2, {
        updateTextAreaInput(session, "curText", value = paste0(input$curText, newChoices[2], " "));         print(newChoices[2])}
    )
    observeEvent(input$choice3, {
        updateTextAreaInput(session, "curText", value = paste0(input$curText, newChoices[3], " "));         print(newChoices[3])}
    )
    
    observeEvent({input$language; input$mem_usage},
    {
        print(paste("Change in language/mem_usage", langCodes[[input$language]]))
        newLang <- allLang[[input$language]]
        newMem <- which(input$mem_usage == memSizeChoices)
        if(length(newMem)<1) newMem<-1
        myPred$InitPredList(predictors[[newLang]][[newMem]])
        newChoices <<- myPred$Start()
        updateActionButton(session, "choice1", label=newChoices[1])
        updateActionButton(session, "choice2", label=newChoices[2])
        updateActionButton(session, "choice3", label=newChoices[3])
    })
    
    observeEvent(input$tokenize, {
        print(paste("Change tokenization method:", input$tokenize));
        if(input$tokenize == T){
            myPred$SetTokenizer("fast");
        } else {
            myPred$SetTokenizer("slow");
        }
    })
    
    # observeEvent(input$mem_usage, {
        # myPred$InitPredList(predictors[[allLanguages[[input$language]]]])
    # })
    
    output$curLanguage <- renderText(paste("Current language: ", input$language))
    
    observeEvent(input$recalcButton, {rv$refreshPlot <- rv$refreshPlot+1; curText <<- input$curText})
    
    pChoice <- c("all",as.character(1:3))
    output$testText <- renderPlot({
        if(rv$refreshPlot >= 0){
            if(input$test_all_models == F){
                print(address(myPred))
                fracCorrect <- TextTester(curText, myPred)
                myDf <- data.frame(choice=factor(pChoice, levels=pChoice), correct=100*c(sum(fracCorrect), fracCorrect))
                myDf$labels <- as.character(round(myDf$correct, digits=2))
                g <- ggplot(data=myDf, aes(x=choice, y=correct, fill=choice)) + geom_bar(stat="identity") + geom_text(data=myDf, aes(x=choice, y=correct, label=labels), colour="black", vjust=2)
            } else {
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
                
                for(iPred in 1:length(predictors[[curLang]])){
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
            g
        }
    })
})
