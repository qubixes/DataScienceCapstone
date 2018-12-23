#ui.R: Definition of the user interface for the FlipCat (demo) application.

library(shiny)
library(shinyjs)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("CatFlip"),
  p("This application is a demo to show the natural language processing capabilities of the underlying algorithm. This algorithm is based on an n-gram model (shown here with n=5), to predict the following word: given the n-1 words, find the n-gram with the highest occurance rate. If this does not exist, lower n by one until a prediction is made. "),
  sidebarLayout(
    sidebarPanel(
       textAreaInput("curText", label="", cols=80, rows=10, placeholder = "Start typing text here"),
       actionButton("choice1", label="-", width="32%"),
       actionButton("choice2", label="-", width="32%"),
       actionButton("choice3", label="-", width="32%"), 
       selectInput("language", label="Language", choices=c("German", "English", "Finnish", "Russian"), selected="English"),
       selectInput("mem_usage", label="Memory usage", choices=c("lowest ()", "low ()", "high ()", "highest ()"), selected="smaller"),
       checkboxInput("tokenize", label="Fast tokenizer", value=T)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(
            tabPanel("stats",
                plotOutput("testText"),
                flowLayout(
                checkboxInput("test_all_models", label="All models", value=F),
                actionButton("recalcButton", label="Refresh", width="80%", style="color: #fff; background-color: #0000ff")
                )
            ),
            tabPanel("help",
                     p("The base idea of this application is that you can type in the text field and there are three buttons below it that show the best three predictions for the next word. You can click them to put them into the text field (random clicking for random garbage!). In the main panel you can view how good the algorithm predicts the words currently typed in the text field. Be sure to hit the <refresh> button to update the calculation."),
                     p("There are a few different options in this application. There are 4 choices of language. There are 4 different ngram decision trees loaded as well. These decision trees have vastly different memory footprints, with the ones using more memory being generally better than the low memory ones. Using the <All models> checkbox one can compare all the models for the input text. The <Fast tokenizer> checkbox toggles whether to use a fast tokenization algorithm. The fast tokenization algorithm takes about 2 ms instead of 20 ms for the slow version. It is slightly less accurate, on the order of about 0.3% top three predictions. ")
                     
                     )
       )
    )
  )
))
