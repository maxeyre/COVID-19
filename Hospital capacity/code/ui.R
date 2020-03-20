library(shiny)
library(shinycssloaders)
library(DT)

# Define UI for Python 3 example app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel('COVID-19 Hospital capacity'),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      h4('Inputs'),
      
      #br(),
      
      #hr(),
      
      # Input: Select the number of initial infections (passed to Python) ----
      numericInput('infected',
                   'Number of initial infections:',
                   value = 1),
      # Input: Select the R0 (passed to Python) ----
      numericInput('R0',
                   'R0:',
                   value = 2.2)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel('Graph predictions', 
              br(),
              h3('Predicted hospital demand'),
              br(),
              withSpinner(plotOutput('plot')),
              br(),
              verbatimTextOutput('which_python'),
              verbatimTextOutput('python_version'),
              verbatimTextOutput('ret_env_var')
    )
      
  )
)