library(shiny)
runApp("shinyapp/")

rsconnect::setAccountInfo(name='maxeyre',
                          token='SECRET',
                          secret='SECRET')


rsconnect::deployApp('shinyapp/')

# Playing with python
reticulate::virtualenv_create(envname = 'python35_env',python= NULL)
reticulate::virtualenv_install('python35_env', 
                               packages = c('numpy'))


reticulate::virtualenv_install(envname = "python35_env", packages = c('scipy','pandas'))

library(reticulate)


reticulate::source_python('COVID_hospital/Simple_Max_edit.py')

z <- model_full()
