library(shiny)
library(shinythemes)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  theme = shinytheme("darkly"),
  # Application title.
  titlePanel("Coursera Capstone Project : Next Word Predicton "),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText(" To get started : Start typing in the text box available to the right ",
               "to test the application. Once you type a word and press space the ",
               "app will automatically predict the next word. Try it out and have fun. To have a look at the source code
               please check the link below."),

      
      tags$span(style="color:blue", 
                            tags$a(
                              href="https://github.com/ananth-srv/Capstone-project",
                              target="_blank",
                              "Source Code"))
      
      # submitButton("Predict Next Word")
    ),
    
    mainPanel(
      textInput("Phrase","Start typing below :", "Here ", width = "400px"),

      h5("Top 3 predicted words:"),
      textOutput("text2")

    )
  )
))
