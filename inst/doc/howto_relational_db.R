## ----echo = TRUE, results = 'hide'--------------------------------------------
library(dplyr)
library(shiny)
library(editbl)

## ----echo = TRUE, results = 'hide'--------------------------------------------
tmpFile <- tempfile(fileext = ".sqlite")
file.copy(system.file("extdata", "chinook.sqlite", package = 'editbl'), tmpFile) 
conn <-   DBI::dbConnect(
    dbname = tmpFile,
    drv = RSQLite::SQLite()
)

## -----------------------------------------------------------------------------
DBI::dbListTables(conn)

## -----------------------------------------------------------------------------
dplyr::tbl(conn, 'Album')

## -----------------------------------------------------------------------------
dbUI <- function(id) {
  ns <- NS(id)
  fluidPage(
      eDTOutput(id = ns('Album'))
  )
}

dbServer <- function(id, conn) {
  moduleServer(
      id,
      function(input, output, session) {        
        Album <- eDT(
            id = "Album",
            key = "AlbumId",
            data = dplyr::tbl(conn, "Album"),
            in_place = TRUE 
        )
          
        invisible()
      }
  )
}

## ----screenshot.opts = list(vwidth = 700, vheight = 500), screenshot.alt = 'screenshots/howto_relational_db_1.png'----
shiny::shinyApp( 
    ui = dbUI('id'),
    server =  function(input, output,session){
      dbServer('id', conn)
    })

## -----------------------------------------------------------------------------
dbServer_hidden_keys <- function(id, conn) {
  moduleServer(
      id,
      function(input, output, session) {
        db_album <- dplyr::tbl(conn, "Album")
        db_artist <- dplyr::tbl(conn, "Artist")
        
        Album <- eDT(
            id = "Album",
            data = db_album,
            in_place = TRUE,
            foreignTbls = list(
              foreignTbl(
                x = db_album,
                y = db_artist,
                by = 'ArtistId',
                naturalKey = 'Name'
              )
             ),
             options = list(
               columnDefs = list(
                 list(visible=FALSE, targets=c("AlbumId","ArtistId"))
               )
             )
        )
        invisible()
      }
  )
}

## ----screenshot.opts = list(vwidth = 700, vheight = 500), screenshot.alt = 'screenshots/howto_relational_db_2.png'----
shiny::shinyApp( 
    ui = dbUI('id'),
    server =  function(input, output,session){
      dbServer_hidden_keys('id', conn)
    })

## -----------------------------------------------------------------------------

dbUI_advanced <- function(id) {
  ns <- NS(id)
  fluidPage(
      shiny::uiOutput(ns("artistSelector_UI")),
      eDTOutput(id = ns('Album'))
  )
}

dbServer_advanced <- function(id, conn) {
  moduleServer(
      id,
      function(input, output, session) {
        ns <- session$ns
                
                
        db_album <- dplyr::tbl(conn, "Album")
        db_artist <- dplyr::tbl(conn, "Artist")
        
        output$artistSelector_UI <- shiny::renderUI(
          shiny::selectInput(ns('artist'),
           label = 'artist',
           choices = db_artist %>% select(Name) %>% collect())
        )

        Album <- eDT(
            id = "Album",
            data = db_album,
            in_place = TRUE,
            foreignTbls = reactive(
            {
             req(input$artist)
             selected <- input$artist     
             list(
              foreignTbl(
                x = db_album,
                y = db_artist %>% filter(Name == selected),
                by = 'ArtistId',
                naturalKey = 'Name'
              )
             )}),
             options = list(
               columnDefs = list(
                 list(visible=FALSE, targets=c("AlbumId","ArtistId"))
               )
             )

        )
        invisible()
      }
  )
}

## ----screenshot.opts = list(vwidth = 700, vheight = 500), screenshot.alt = 'screenshots/howto_relational_db_3.png'----
shiny::shinyApp( 
    ui = dbUI_advanced('id'),
    server =  function(input, output,session){
      dbServer_advanced('id', conn)
    })

