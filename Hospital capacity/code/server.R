library(shiny)
library(DT)
library(RColorBrewer)
library(tidyverse)


plot_cols <- brewer.pal(11, 'Spectral')

shinyServer(function(input, output) {
  
  if (!Sys.info()[['sysname']] == 'Max'){
    # When running on shinyapps.io, create a virtualenv 
    reticulate::virtualenv_create(envname = 'python35_env', 
                                  python = '/usr/bin/python3')
    reticulate::virtualenv_install('python35_env', 
                                   packages = c('numpy','scipy','pandas'))  # <- Add other packages here, if needed
  }
  reticulate::use_virtualenv('python35_env', required = T)
  reticulate::source_python('Simple_Max_edit.py')
  

  # Generate a plot of the data
  output$plot <- renderPlot({
    InitCaseNo<- input$infected
    R0 <- input$R0
    N <- 150000
    NoWeeksPred <- 20
    
    z <- model_full(N=as.integer(N),InitCaseNo=as.integer(InitCaseNo),R0 =as.numeric(R0),NoWeeksPred=as.integer(NoWeeksPred))
    
    return(ggplot(z) + geom_line(aes(x=seq(1:140),y=z[,1])))
  })
  
  # Testing that numpy function can be used
  output$xy <- renderText({
    z = test_numpy_function(input$x, input$y)
    return(paste0('x + y = ', z))
  })
  
  
  # Display system path to python
  output$which_python <- renderText({
    paste0('which python: ', Sys.which('python'))
  })
  
  # Python version
  output$python_version <- renderText({
    rr = reticulate::py_discover_config(use_environment = 'python35_env')
    paste0('Python version: ', rr$version)
  })
  
  # Display RETICULATE_PYTHON
  output$ret_env_var <- renderText({
    paste0('RETICULATE_PYTHON: ', Sys.getenv('RETICULATE_PYTHON'))
  })
  
})