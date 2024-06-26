## ----echo = TRUE, results = 'hide'--------------------------------------------
library(shiny)
library(DT)
library(editbl)

## ----screenshot.opts = list(vwidth = 700), screenshot.alt = 'screenshots/howto_switch_from_DT_1.png'----
ui <- fluidPage(DTOutput("DT"))
server <- function(input, output, session){
  output$DT <- renderDataTable({
    datatable(mtcars)  %>%
     formatRound('disp', 1)
  })  
  observe({
      print(input[["DT_cell_clicked"]])
   })
}
shinyApp(ui, server)

## ----screenshot.opts = list(vwidth = 700), screenshot.alt = 'screenshots/howto_switch_from_DT_2.png'----
ui <- fluidPage(eDTOutput("DT"))
server <- function(input, output, session){
  editbl::eDT(
    id = "DT",
    data = mtcars,
    format = function(x){formatRound(x,'disp', 1)})
   
   observe({
      print(input[["DT-DT_cell_clicked"]])
   })
}
shinyApp(ui, server)

