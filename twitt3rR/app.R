library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(twitteR)
library(tidytext)
library(tidyr)
library(purrr)
library(scales)
trump_tweets <- load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))

ui <- fluidPage(
  titlePanel("Twitt3rR"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("Text", h3("Twitter Handle"), 
                value = "Enter Twitter Handle...")
    ),
    mainPanel(
      plotOutput("mobile")
    )
  )
  
)

server <- function(input, output) {
  tweets <- trump_tweets_df %>%
    select(id, statusSource, text, created) %>%
    extract(statusSource, "source", "Twitter for (.*?)<") %>%
    filter(source %in% c("iPhone", "Android"))
  
  output$mobile = renderPlot({
    tweets %>%
      count(source, hour = hour(with_tz(created, "EST"))) %>%
      mutate(percent = n / sum(n)) %>%
      ggplot(aes(hour, percent, color = source)) +
      geom_line() +
      scale_y_continuous(labels = percent_format()) +
      labs(x = "Hour of day (EST)",
           y = "% of tweets",
           color = "")
    
    
    ## ggplot here
  })
  
  
  
  
  
}

shinyApp(ui = ui, server = server)