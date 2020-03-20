library(shiny)
runApp("shinyapp/")

rsconnect::setAccountInfo(name='maxeyre',
                          token='4F867E67A24716CACF3E0D31D9395046',
                          secret='kJmQRMRN/HyEx/tkwlgULIsrLi4mmng8wCRCem5v')


rsconnect::deployApp('shinyapp/')

