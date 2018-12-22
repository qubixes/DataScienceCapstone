#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# js <- '
# $(document).on("keydown": function(e) {
#     if(e.keyCode == 9) {$(document).keyup(e); e.preventDefault;}
# },
# "keyup":function(e){
#     // if(e.keyCode == 32 || e.keyCode == 13 || e.keyCode == 9){
#         Shiny.onInputChange("keyPressed", e.keyCode);
#         Shiny.onInputChange("keyEvent", Math.random())
#     // }
# }
# );
# '

# js1 <- '
# $(document).on("keydown", function(e) {
#     if(e.keyCode == 9) {
#         Shiny.onInputChange("keyPressed", e.keyCode);
#         Shiny.onInputChange("keyEvent", Math.random());
#         e.stopPropagation();
#         e.preventDefault();
#     
#     }
# }'

js2 <- '
$(document).on("keypress", function(e){
      if(e.keyCode == 32 || e.keyCode == 13){
        Shiny.onInputChange("keyPressed", e.keyCode);
        Shiny.onInputChange("keyEvent", Math.random());
    //  e.stopPropagation();
    //    e.preventDefault();
     }
}
);
'


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # tags$script(js1),
  # tags$script(js2),
  # Application title
  useShinyjs(),
  titlePanel("FlipCat"),
  p("A demo for fast n-gram prediction."),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textAreaInput("curText",
                   label="",
                   cols=80,
                   rows=10,
                   placeholder = "Start typing text here"),
       actionButton("choice1", label="-", width="32%"),
       actionButton("choice2", label="-", width="32%"),
       actionButton("choice3", label="-", width="32%"), 
       selectInput("language", label="Language", choices=c("German", "English", "Finnish", "Russian"), selected="English"),
       selectInput("mem_usage", label="Memory usage", choices=c("lowest ()", "low ()", "high ()", "highest ()"), selected="smaller"),
       checkboxInput("tokenize", label="Fast tokenizer", value=T)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("testText"),
       flowLayout(
       checkboxInput("test_all_models", label="All models", value=F),
       actionButton("recalcButton", label="Refresh", width="80%", style="color: #fff; background-color: #0000ff")
       )
    )
  )
))
