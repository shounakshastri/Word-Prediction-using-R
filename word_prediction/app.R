#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
suppressPackageStartupMessages({
    #library(tidyverse)
    #library(stringr)
})

#source("C:\\Users\\Intel\\Documents\\Coursera DS Capstone Project\\prediction_code.R")
source("prediction_code.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            h2("Word Prediction App"),
            h5("1. Delete the original text in the text box and start writing in the text box. The predictions would appear below."),
            h5("2. The code displays \"...thinking...\" when it is trying to predict a word"),
            h5("3. You can try copying a longer sentence to see what is the prediction."),
            h5("4. The app can process pentagrams i.e. it can predict the 5th word based on the previous 4."),
            h5("5. But if the user writes directly in the text box, then it predicts only bigrams.")
        ),
        # Show a plot of the generated distribution
        mainPanel(
            
                tabPanel("predict",
                         textInput("user_input", h3("Your Input:"), 
                                   value = "Enter a word or a sentence"),
                         h3("Prediction:"),
                         h4(em(span(textOutput("predicted_word"), style="color:blue"))))
            )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$predicted_word <- renderText({
        prediction(input$user_input)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
