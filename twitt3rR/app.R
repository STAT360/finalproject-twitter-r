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
      textOutput("value"),
      dataTableOutput("presidents"),
      plotOutput("wordPlot")
    )
  )
  
)

server <- function(input, output) {
  output$value <- renderText({
    paste("Tweet analysis of:", input$User)
    
  })
  
  output$presidents <- renderDataTable({
    curTweets <- get_timeline(input$User, n=10) %>% 
      select(text)
  })
  
  output$wordPlot <- renderPlot({
    curTweets <- get_timeline(input$User, n=10) %>% 
      select(text)
    
    data<- unnest_tokens(curTweets)
    cleaned_data<- data %>% 
      anti_join(get_stopwords())
    cleaned_data %>% 
      count(word, sort = TRUE)
    
    nrc <- get_sentiments("nrc")
    # 
    # newTweets <- cleaned_data %>% 
    #   inner_join(nrc) %>% 
    #   count(word, index = line %/% 80, sentiment) %>% 
    #   spread(sentiment, n, fill=0) %>% 
    #   mutate(sentiment = positive-negative)
    # 
    # ggplot(newTweets, aes(index, sentiment, fill=word))+
    #   geom_bar(stat="identity", show.legend = FALSE)
    # 
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)



