#!/usr/bin/env Rscript
## AirTweet_Client.R
#
# This file should contain student's handler that processes incoming tweets
# from the server in real time. 
#title: "Airline Tweet Sentiment Analysis"
#author: "M MANIVASSAKAM"
#date: "November 2, 2017"
#output: html_document
####

---
  
source('AirTweet_Connection.R')

load("slda.model.Rdata")

# global vars
tweets_counter <- 0                                               # tweet event counter
BUF_SIZE <- 1000                                                  # we create buffers in advance:
received_data <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),     # dataframe for received tweets
                            text=character(BUF_SIZE),
                            negative_prob=as.numeric(rep(NaN, BUF_SIZE)),
                            neutral_prob=as.numeric(rep(NaN, BUF_SIZE)),
                            positive_prob=as.numeric(rep(NaN, BUF_SIZE)),
                            predic=as.numeric(rep(NaN, BUF_SIZE)),
                            wordy=character(BUF_SIZE),
                            stringsAsFactors=FALSE )

library(lda)

# user defined handler
## arguments:
#### text - character vector of unit length
## returns:
#### sentiment probability vector of length 3
new_tweet_handler <- function(text) {
    now <- Sys.time()
    # log event
    message(now, ' : ', text)
    
    # update dataframe (store probability after classification)
    tweets_counter <<- tweets_counter + 1
    received_data[tweets_counter,] <<- list(now, text, NaN, NaN, NaN, NaN,NaN)
    
    # calculate probabilities (some sophisticated model usage is expected to be here)
    text <- gsub("http[^[:space:]]+", " ", text)  # remove http links
    text <- gsub("[^a-zA-Z']+", " ", text)        # change non-letter sequences to whitespaces
    text <- gsub("(^ | $)", "", text)             # remove whitespace at beginning and end of documents
    text <- tolower(text)                         # force to lowercase
    # split documents to words:
    words <- unlist(strsplit(text, " ")) # Splitting into words
    words <- words[words %in% vocab] # Keeping only the vocab words
    
    if(length(words)<1){  # If there are no words left after all the cleaning, and checking in vocab
      sentiment.prob.vector <- c(0.25, 0.5, 0.25) # Send a neutral sentiment
      prediction <- NaN
      words <- NaN
    }
    else{
      docs <- list(words)
      docs2 <- lapply(docs,get.terms)
      prediction <- slda.predict(docs2, fit$topics, fit$model, alpha=alpha, eta=eta)
      print(words)
      prediction <- as.vector(prediction)
      print(prediction)
    
      if(prediction < -1){
        sentiment.prob.vector <- c(1, 0, 0)
      }
      else if(prediction >1){
        sentiment.prob.vector <- c(0, 0, 1)
      }
      else if(prediction >0.9){
        sentiment.prob.vector <- c(0, 1-prediction, prediction)
      }
      else{
        ix <- which(bins>prediction)[1]
        sentiment.prob.vector <- probVeclist[[ix-1]]
      }
    }
    # log result
    message('Predicted probabilities: ', paste0(sentiment.prob.vector, ' '))
    
    # store probabilities in global dataframe
    received_data$negative_prob[tweets_counter] <<- sentiment.prob.vector[1]
    received_data$neutral_prob[tweets_counter] <<- sentiment.prob.vector[2]
    received_data$positive_prob[tweets_counter] <<- sentiment.prob.vector[3]
    received_data$predic[tweets_counter] <<- prediction
    received_data$wordy[tweets_counter] <<- paste0(words,collapse = " ")
    
    return(sentiment.prob.vector)
}


# server options
host <- "datastream.ilykei.com"
port <- 30009
login <- "manivassakam@uchicago.edu"
password <- "hRztiPgU"
stream_name <- "AirTweet"
catch_handler_errors <- TRUE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handler
result <- Connect(host, port, login, password, stream_name, new_tweet_handler, catch_handler_errors)

# remove empty values from buffers
received_data <- received_data[!is.na(received_data$time),]

# after all you can dump your data/result and analyze it later
dump(c("received_data", "result"), file = "results.txt")

write.csv(received_data,"data.csv")
