#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(tm)

bigram_model <- read.csv("bigram.csv", header = TRUE)
trigram_model <- read.csv("trigram.csv", header = TRUE)


########################################################################
# Define UI for application that predicts the next word
ui <- fluidPage(
   
  titlePanel("Data Science Capstone - using NLP to predict next words"),
  sidebarLayout(
    sidebarPanel(
      helpText("Enter a word, text or sentence to get prediction for the next word."),
      hr(),
      textInput("inputText", "Enter your input here ",value = ""),
      hr(),
      helpText("After the input text has been entered the next word prediction will be shown on the right as a suggestion."),
      hr(),
      hr()
    ),
    mainPanel(
      h3("Words suggested on basis of the input text: "),
      verbatimTextOutput("prediction"),
      strong("The text input:"),
      strong(code(textOutput('sentence1')))
    )
  )
)

########################################################################

predictWord <- function(word) {
  
  word_processed <- stripWhitespace(removeNumbers(removePunctuation(tolower(word),preserve_intra_word_dashes = TRUE)))
  
  word_processed <- strsplit(word_processed, " ")[[1]]

  n <- length(word_processed)

  # Bigram
  if (n == 1) {
    word_processed <- as.character(tail(word_processed,1))
    functionBigram(word_processed)
  }
  
  # Trigram
  else {
    word_processed <- as.character(tail(word_processed,2))
    functionTrigram(word_processed)
  }
 
}

########################################################################
functionBigram <- function(word) {
  
  if (identical(character(0),as.character(head(bigram_model[bigram_model$word1 == word[1], 2], 1)))) {
    as.character(head("it",1))
  }
  else {
    as.character(head(bigram_model[bigram_model$word1 == word[1],2], 3))
  }
}

########################################################################
functionTrigram <- function(word) {
  
  if (identical(character(0),as.character(head(trigram_model[trigram_model$word1 == word[1]
                                                  & trigram_model$word2 == word[2], 3], 1)))) {
    as.character(predictWord(word[2]))

  }
  else {
    as.character(head(trigram_model[trigram_model$word1 == word[1]
                         & trigram_model$word2 == word[2], 3], 1))
  }

}

########################################################################
# Define server logic required to predict the word

server <- function(input, output) {
   
  output$prediction <- renderPrint({
    result <- predictWord(input$inputText)
    result
  });
  
  output$sentence1 <- renderText({
    input$inputText});
  
}

########################################################################
# Run the application 
shinyApp(ui = ui, server = server)

