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
library(wordcloud)
library(reshape2)
trump_tweets <- load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))

ui <- fluidPage(
  titlePanel("Twitt3rR"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("User", label=("Choose a twitter user to display"), 
                choices = c("POTUS",
                            "BarackObama",
                            "JebBush"),
                selected = "POTUS")
    ),
    mainPanel(
      dataTableOutput("presidents"),
      plotOutput("wordPlot"),
      #plotOutput("wordCloud"),
      plotOutput("goodBadWords")
    )
  )
  
)

server <- function(input, output) {
  output$value <- renderText({
    paste("Tweet analysis of:", input$User)
    
  })
  
  # output$presidents <- renderDataTable({
  #   curTweets <- get_timeline(input$User, n=10) %>% 
  #     select(text)
  # })
  
  output$wordPlot <- renderPlot({
    curTweets <- get_timeline(input$User, n=10) %>% 
      select(text)
    
    data<- unnest_tokens(curTweets, word, text)
    cleaned_data<- data %>% 
      anti_join(get_stopwords())
    cleaned_data %>% 
      count(word, sort = TRUE)
    
    nrc <- get_sentiments("nrc")

    newTweets <- cleaned_data %>%
      inner_join(nrc) %>%
      count(word, sentiment) %>%
      spread(sentiment, n, fill=0) %>%
      mutate(sentiment = positive-negative)

    ggplot(newTweets, aes(word, sentiment, fill=word))+
      geom_bar(stat="identity", show.legend = FALSE)+
      coord_flip()

    
  })
  # Do we want this???
  # output$wordCloud<- renderPlot({
  #   curTweets <- get_timeline(input$User, n=10) %>% 
  #     select(text)
  #   
  #   data<- unnest_tokens(curTweets, word, text)
  #   cleaned_data<- data %>% 
  #     anti_join(get_stopwords())
  #   cleaned_data %>% 
  #     count(word) %>% 
  #     with(wordcloud(word, n, max.words = 100))
  # })
  
  output$goodBadWords<- renderPlot({
    curTweets<- get_timeline(input$User, n=10) %>% 
      select(text)
    data<- unnest_tokens(curTweets, word, text)
    bing<- get_sentiments("bing")
    data %>% 
      inner_join(bing) %>% 
      count(word, sentiment, sort = TRUE) %>% 
      acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 50)
  })
  
  
}

shinyApp(ui = ui, server = server)


## set up rsconnect, but will not load online
