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
library(ggthemes)

ui <- fluidPage(
  titlePanel("Advanced Twitt3rR Sentiment Scrutiny"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("User", label=("Choose a twitter user to display"), 
                choices = c("ameliamn",
                            "RealDonaldTrump",
                            "BarackObama",
                            "JebBush",
                            "fritzsfoodtruck",
                            "twitt3r_R",
                            "drob",
                            "HadleyWickham",
                            "JennyBryan",
                            "rstudio"),
                selected = "POTUS"),
      p("By selecting an input from the dropdown menu, the application will then pull the
        selected Public Twitter User's last 100 tweets from their timeline using the rtweet
        package's function 'get_timeline()'. Here is the
        profile picture of the currently selected user:"),
      htmlOutput("userImage", height = 1000, width = 1000)
      
    ),
    mainPanel(
      plotOutput("wordPlot"),
      p("In the above plot, the 20 most commonly used words from the selected User's 
        Twitter account are displayed according to their sentiment. Sentiment is calculated
         using bing's lexicon and the tidytext package. A positive number reflects a positive
         word sentiment, and vice versa."),
      plotOutput("goodBadWords"),
      p("Above is a word cloud representing word sentiment. Larger words have a more significant
        sentiment score. A blue coloring represents a positive sentiment,
         and red represents negative."),
      plotOutput("timePlot"),
      p("The above plot shows the frequency of the selected User's tweets by time of day."),
      plotOutput("wordCount"),
      p("The above plot shows the 20 most frequently used words by the User, and the frequency 
        at which they occur in their last 100 tweets."),
      p("Below is the selected User's last 100 tweets in their raw form received from Twitter:"),
      dataTableOutput("presidents")
    )
  )
  
)

server <- function(input, output) {
  
  output$userImage<- renderText({
    curTweets<- get_timeline(input$User, n=1) %>%
      pull("profile_image_url")
    display<- c('<img src = "',
                  curTweets,
                  '" height = 100 width = 100>')
    return(display)
  })
  
  output$presidents <- renderDataTable({
    curTweets <- get_timeline(input$User, n=10) %>% 
      filter(is_retweet == FALSE)
  })
  
  
  output$wordPlot <- renderPlot({
    curTweets <- get_timeline(input$User, n=10) %>% 
      select(text)
    
    data<- unnest_tokens(curTweets, word, text)
    cleaned_data<- data %>% 
      anti_join(get_stopwords())
    cleaned_data %>% 
      count(word, sort = TRUE)
    
    bing <- get_sentiments("bing")

    newTweets <- cleaned_data %>%
      inner_join(bing) %>%
      count(word, sentiment) %>%
      spread(sentiment, n, fill=0) %>%
      mutate(sentiment = positive-negative) %>% 
      arrange(desc(sentiment)) %>% 
      mutate(word = fct_reorder(word,sentiment))

    ggplot(newTweets, aes(word, sentiment, fill=sentiment))+
      geom_bar(stat="identity", show.legend = FALSE)+
      coord_flip()+
      labs(title = "Sentiment Analysis", x="Word Used", y="Sentiment Score")+
      theme(plot.title = element_text(hjust = .5, size = 30))
  })
  output$timePlot<- renderPlot({
    curTweets <- get_timeline(input$User, n=100) %>% 
      select(text,created_at) %>% 
      count(hour = hour(with_tz(created_at, "America/Chicago"))) %>% 
      mutate(countn = n)
    ggplot(curTweets)+
      geom_line(aes(x=hour, y=countn), color = "DodgerBlue4", size = 2)+
      labs(x = "Hour of Day (CT)", y= "Frequency")+
      ggtitle("Tweet Frequency by Time of Day")+
      scale_x_continuous(breaks = c(0,5,10,15,20),labels = c("12AM", "5AM", "10AM", "3PM", "8PM"))+
      theme(plot.title = element_text(hjust = .5, size = 30))
  })
  output$wordCount<- renderPlot({
    curTweets<- get_timeline(input$User, n=100) %>% 
      filter(is_retweet == FALSE) %>% 
      select(text)
      
    data<- unnest_tokens(curTweets, word, text)
    cleaned_data<- data %>% 
      anti_join(get_stopwords()) %>% 
      filter(!(word %in% c("https", "t.co")))
      
    cleaned_data %>% 
      count(word, sort = TRUE) %>% 
      #summarise(total = n()) %>% 
      #arrange(desc(total)) %>%
      head(20) %>% 
      mutate(word = reorder(word, n)) %>% 
      ggplot(aes(word,n, fill=n))+
        geom_bar(stat = "identity")+
        scale_fill_distiller(palette = "Blues")+
        coord_flip()+
        labs(x = "Word Used", y = "Frequency", title = "Word Frequency")+
        theme(plot.title = element_text(hjust = .5, size = 30))
  })
  
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
