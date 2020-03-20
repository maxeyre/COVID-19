library(shiny)
runApp("shinyapp/")

rsconnect::setAccountInfo(name='maxeyre',
                          token='4F867E67A24716CACF3E0D31D9395046',
                          secret='kJmQRMRN/HyEx/tkwlgULIsrLi4mmng8wCRCem5v')


rsconnect::deployApp('shinyapp/')

# Playing with python
reticulate::virtualenv_create(envname = 'python35_env',python= NULL)
reticulate::virtualenv_install('python35_env', 
                               packages = c('numpy'))


reticulate::virtualenv_install(envname = "python35_env", packages = c('scipy','pandas'))

library(reticulate)


reticulate::source_python('COVID_hospital/Simple_Max_edit.py')

z <- model_full()
