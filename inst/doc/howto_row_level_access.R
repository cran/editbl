## ----screenshot.opts = list(vwidth = 700, vheight = 500), , screenshot.alt = 'screenshots/howto_row_level_access_1.png'----
library(editbl)
library(shiny)
conn <- DBI::dbConnect(RSQLite::SQLite(), "")
df <- data.frame(
    user = c("Albert","Donald","Mickey"),
    email = c('albert@einstein.com', 'donald@duck.com', 'mickey@mouse.com')
)
DBI::dbWriteTable(conn, "characters", df)
tibble <- dplyr::tbl(conn, 'characters')

CURRENT_USER = 'Mickey'

shiny::shinyApp( 
    ui = editbl::eDTOutput('id'),
    server =  function(input, output,session){
      result <- eDT(id='id',
        data = tibble %>% filter(user == CURRENT_USER),
        in_place = TRUE
      )
    })

print(tibble)

DBI::dbDisconnect(conn)

## ----screenshot.opts = list(vwidth = 700, vheight = 500), , screenshot.alt = 'screenshots/howto_row_level_access_2.png'----
library(editbl)
df <- tibble::tibble(
    user = c("Albert","Donald","Mickey"),
    email = c('albert@einstein.com', 'donald@duck.com', 'mickey@mouse.com')
)

CURRENT_USER = 'Mickey'

rowModificationLogic <- function(row){
        if (row[,'user'] == CURRENT_USER){
          TRUE
        } else {
          FALSE  
        }
    }

shiny::shinyApp( 
    ui = editbl::eDTOutput('id'),
    server =  function(input, output,session){
      eDT(id='id',
        data = df,
        canEditRow = rowModificationLogic,
        canDeleteRow = rowModificationLogic
        )
    })


