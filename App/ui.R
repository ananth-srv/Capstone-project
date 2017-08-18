library(shiny)
library(shinythemes)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  theme = shinytheme("darkly"),
  # Application title.
  titlePanel("Coursera Capstone Project : Next Word Predicton "),
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view. The helpText function is
  # also used to include clarifying text. Most notably, the
  # inclusion of a submitButton defers the rendering of output
  # until the user explicitly clicks the button (rather than
  # doing it immediately when inputs change). This is useful if
  # the computations required to render output are inordinately
  # time-consuming.
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
    
    # Show a summary of the dataset and an HTML table with the
    # requested number of observations. Note the use of the h4
    # function to provide an additional header above each output
    # section.
    mainPanel(
      textInput("Phrase","Start typing below :", "Here ", width = "400px"),

      h5("Top 3 predicted words:"),
      textOutput("text2")

    )
  )
))