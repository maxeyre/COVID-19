library(shiny)
runApp("shinyapp/")

rsconnect::setAccountInfo(name='maxeyre',
                          token='SECRET',
                          secret='SECRET')


rsconnect::deployApp('shinyapp/', account='maxeyre')

