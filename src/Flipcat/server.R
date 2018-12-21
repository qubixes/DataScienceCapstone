#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("multi_predictor.R")
source("shiny_load.R")
source("shiny_tester.R")

choices <- character(3)

langCodes = c(German="de_DE", English="en_US", Finnish="fi_FI", Russian="ru_RU")
myPred <- predictors[["en_US"]]

# UpdateButtons <- function(input, output, session, newChoices){
# }



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    i<-1
    
    rv <- reactiveValues()
    rv$myText <- ""
    # newChoices <- myPred$Start()
    # # newChoices %>% print
    # updateActionButton(session, "choice1", label=newChoices[1])
    # updateActionButton(session, "choice2", label=newChoices[2])
    # updateActionButton(session, "choice3", label=newChoices[3])
    # 
    # 
    # observeEvent(input$keyEvent,
    # {
    #     newChoices <- myPred$Next(input$curText)
    #     
    #     updateActionButton(session, "choice1", label=newChoices[1])
    #     updateActionButton(session, "choice2", label=newChoices[2])
    #     updateActionButton(session, "choice3", label=newChoices[3])
    #     
    #     i <<- i+1
    #     print(paste(input$curText))
    # })
    
    newChoices <- character(3)
    
    observe({
        if(input$curText != ""){
         newChoices <<- myPred$Next(input$curText)
         
         updateActionButton(session, "choice1", label=newChoices[1])
         updateActionButton(session, "choice2", label=newChoices[2])
         updateActionButton(session, "choice3", label=newChoices[3])
         
         i <<- i+1
         print(paste(input$curText))
    }
     })
    
    observeEvent(input$choice1, {
        updateTextAreaInput(session, "curText", value = paste0(input$curText,newChoices[1], " "));         print(newChoices[1])}
    )
    observeEvent(input$choice2, {
        updateTextAreaInput(session, "curText", value = paste0(input$curText,newChoices[2], " "));         print(newChoices[2])}
    )
    observeEvent(input$choice3, {
        updateTextAreaInput(session, "curText", value = paste0(input$curText,newChoices[3], " "));         print(newChoices[3])}
    )
    
    observeEvent(input$language,
    {
        print(paste("Change in language", langCodes[[input$language]]))
        myPred <<- predictors[[langCodes[[input$language]]]]
        # str(myPred)
        # print(myPred$Start())
        newChoices <<- myPred$Start()
        # newChoices %>% print
        updateActionButton(session, "choice1", label=newChoices[1])
        updateActionButton(session, "choice2", label=newChoices[2])
        updateActionButton(session, "choice3", label=newChoices[3])
        
        # UpdateButtons(input, output, session, myPred$Start())
        # callModule(?)
    })
    output$curLanguage <- renderText(paste("Current language: ", input$language))
    
    observeEvent(input$recalcButton, {rv$myText <- input$curText})
    
    output$testText <- renderPlot({
        fracCorrect <- TextTester(rv$myText, myPred)
        myDf <- data.frame(choice=factor(c("all",as.character(1:3))), correct=100*c(sum(fracCorrect), fracCorrect))
        print(myDf)
        myDf$labels <- as.character(round(myDf$correct, digits=2))
        ggplot(data=myDf, aes(x=choice, y=correct, fill=choice)) + geom_bar(stat="identity") + geom_text(data=myDf, aes(x=choice, y=correct, label=labels), colour="black", vjust=2)
    })
  # output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2] 
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  # })
  
})
