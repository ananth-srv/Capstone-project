library(shiny)
library(data.table)
library(stringr)
library(ANLP)
library(quanteda)

print('loading models')
gram2 <- read.csv("./data/gram2.csv",header = TRUE,colClasses = c("integer", "character", "character"))
gram3 <- read.csv("./data/gram3.csv",header = TRUE,colClasses = c("integer", "character", "character"))
gram4 <- read.csv("./data/gram4.csv",header = TRUE,colClasses = c("integer", "character", "character"))

getNextWord = function(x = " ") {
  print(x)
  clean_text <- tokens(x, what = c("word"), remove_numbers = TRUE, remove_symbols = TRUE, remove_punct = TRUE,
                       remove_url = TRUE, remove_separators = TRUE, verbose = TRUE)
  clean_text <- tokens_tolower(clean_text)
  
  words_all <- clean_text[[1]]
  last_char <- substr(x,nchar(x),nchar(x))
  print(words_all)
  pred_data <- head(gram4,1)
  pred_data <- pred_data[-1, ]
  if(!(last_char == " ")){
    words_all <- words_all[1:(length(words_all) - 1)]
  }
  
  if(length(words_all) > 2){
    input_words <- paste(tail(words_all, 3), collapse=' ')
    pred_data <- gram4[gram4$word_input==input_words, ]
    pred_data <- pred_data[!duplicated(pred_data$word_pred),]
  }
  
  if((nrow(pred_data)< 3)& length(words_all) > 1){
    if (length(words_all) > 2){
      input_words <- paste(tail(words_all,2), collapse = ' ')
    }
    else{
      input_words <- paste(words_all, collapse = ' ')
    }
    pred_data1 <- gram3[gram3$word_input==input_words, ]
    pred_data <- rbind(pred_data, pred_data1)
    pred_data <- pred_data[!duplicated(pred_data$word_pred),]
  }
  if((nrow(pred_data) < 3)& length(words_all) > 0){
    if (length(words_all) > 1){
      input_words <- tail(words_all,1)
    }
    else{
      input_words <- words_all
    }
    pred_data1 <- gram2[gram2$word_input==input_words, ]
    pred_data <- rbind(pred_data, pred_data1)
    pred_data <- pred_data[!duplicated(pred_data$word_pred),]
  }
  
  if (nrow(pred_data) > 2){
     output1 <- pred_data$word_pred[1]
     output2 <- pred_data$word_pred[2]
     output3 <- pred_data$word_pred[3]
  }
  else if(nrow(pred_data) == 2){
    output1 <- pred_data$word_pred[1]
    output2 <- pred_data$word_pred[2]
    output3 <- "the"
  }
  else if(nrow(pred_data) == 1){
    output1 <- pred_data$word_pred[1]
    output2 <- "the"
    output3 <- "a"
  }
  else{
    output1 <- "the"
    output2 <- "a"
    output3 <- "of"
  }
  paste(c(output1, output2, output3), collapse = ', ')
}

print('models loaded. go ahead with the prediction')

shinyServer(function(input, output, session) {
  
  observe({

    # get the next word
    output$text2 <- renderText({
      validate(
        need(input$Phrase != "", "Please enter a phrase")
      )
      getNextWord(input$Phrase)  
    })
  })
})
