library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(twitteR)
library(tidytext)
library(tidyr)
library(purrr)
library(scales)
library(rtweet)
trump_tweets <- load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))

ui <- fluidPage(
  titlePanel("Twitt3rR"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("Text", h3("Twitter Handle"), 
                value = "Enter Twitter Handle...")
    ),
    mainPanel(
      textOutput("value")
    )
  )
  
)

server <- function(input, output) {
  output$value <- renderText({
    paste("Tweet analysis of", input$Text)
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)



