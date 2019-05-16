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
      selectInput("User", label=("Choose a twitter user to display"), 
                choices = c("POTUS",
                            "BarackObama",
                            "GeorgeWBush"),
                selected = "POTUS")
    ),
    mainPanel(
      textOutput("value")
    )
  )
  
)

server <- function(input, output) {
  output$value <- renderText({
    paste("Tweet analysis of", input$User)
    userName <- input$User
    get_timeline(userName, n=10)
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)



