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
      thing<- textInput("Text", h3("Twitter Handle"), 
                value = "Enter Twitter Handle...")
    ),
    mainPanel(
    )
  )
  
)

server <- function(input, output) {
  print(thing)
  
  
  
  
}

shinyApp(ui = ui, server = server)



