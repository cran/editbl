## ----echo = TRUE, results = 'hide'--------------------------------------------
library(dplyr)
library(shiny)
library(editbl)
library(dm)

## ----echo = TRUE, results = 'hide'--------------------------------------------
tmpFile <- tempfile(fileext = ".sqlite")
file.copy(system.file("extdata", "chinook.sqlite", package = 'editbl'), tmpFile) 
conn <-   DBI::dbConnect(
    dbname = tmpFile,
    drv = RSQLite::SQLite()
)

## ----echo = TRUE, results = 'hide'--------------------------------------------
dm <- dm::dm_from_con(conn, learn_keys = FALSE)

## ----echo = TRUE, results = 'hide'--------------------------------------------
dm <- dm %>%
    dm_add_pk(Artist, ArtistId) %>%
    dm_add_pk(Album, AlbumId) %>%
    dm_add_pk(Customer, CustomerId) %>%
    dm_add_pk(Employee, EmployeeId) %>%
    dm_add_pk(Genre, GenreId) %>%
    dm_add_pk(Invoice, InvoiceId) %>%
    dm_add_pk(InvoiceLine, InvoiceLineId) %>%
    dm_add_pk(MediaType, MediaTypeId) %>%
    dm_add_pk(Playlist, PlaylistId) %>%
    dm_add_pk(PlaylistTrack, c(PlaylistId, TrackId)) %>%
    dm_add_pk(Track, TrackId)
    

## ----echo = TRUE, results = 'hide'--------------------------------------------
dm <- dm %>%
    dm_add_fk(
      table = Album,
      columns = ArtistId,
      ref_table = Artist) %>%
    dm_add_fk(
      table = Invoice,
      columns = CustomerId,
      ref_table = Customer) %>%
    dm_add_fk(
      table = InvoiceLine,
      columns = InvoiceId,
      ref_table = Invoice) %>%
    dm_add_fk(
      table = InvoiceLine,
      columns = TrackId,
      ref_table = Track) %>%
    dm_add_fk(
      table = PlaylistTrack,
      columns = TrackId,
      ref_table = Track) %>%
    dm_add_fk(
      table = PlaylistTrack,
      columns = PlaylistId,
      ref_table = Playlist) %>%
    dm_add_fk(
      table = Track,
      columns = AlbumId,
      ref_table = Album) %>%
    dm_add_fk(
      table = Track,
      columns = MediaTypeId,
      ref_table = MediaType) %>%
    dm_add_fk(
      table = Track,
      columns = GenreId,
      ref_table = Genre)

## -----------------------------------------------------------------------------
dm::dm_flatten_to_tbl(dm, "Album", .recursive = TRUE)

## -----------------------------------------------------------------------------
getForeignTbls <- function(dm, table){
  dm_fks <- dm::dm_get_all_fks(dm)
  dm_fks <- dm_fks[dm_fks$child_table == table,]
  tbl_list <- dm::dm_get_tables(dm)
  
  foreignTbls <- lapply(seq_len(nrow(dm_fks)), function(i){
      r <- dm_fks[i,]   
      x <- tbl_list[r$child_table][[1]]
      y <- dm::dm_flatten_to_tbl(dm, !!(r$parent_table), .recursive = TRUE)
      
      child_fks <- unlist(r$child_fk_cols)
      parent_fks <- unlist(r$parent_key_cols)
      
      # Renaming of parent colums to avoid naming conflicts
      # Done a bit heuristically here for convenience.
      lookup <- parent_fks
      names(lookup) <- child_fks
      other_parent_cols <- setdiff(colnames(y), parent_fks)
      names(other_parent_cols) <- paste(r$parent_table, other_parent_cols, sep = '.')
      lookup <- c(lookup, other_parent_cols)
      y <- y %>% dplyr::rename(all_of(lookup))
      
      editbl::foreignTbl(
        x = x,
        y = y,
        by = child_fks,
        naturalKey = colnames(y)
    )
    
  })
  foreignTbls
  }
  

## -----------------------------------------------------------------------------
dbUI <- function(id) {
  ns <- NS(id)
  fluidPage(
      uiOutput(outputId = ns('selectUI')),
      eDTOutput(id = ns('DT'))
  )
}

dbServer <- function(id, dm) {
  moduleServer(
      id,
      function(input, output, session) {
        ns <- session$ns
        
        tables <- dm::dm_get_tables(dm)
        
        output$selectUI <- renderUI({
          selectInput(ns('table'), label = 'table', choices = names(tables))  
        })
        
        data <- reactive({
              req(input$table)
              tables[input$table][[1]]
              })
        
        foreignTbls <-  reactive({
              req(input$table)
              getForeignTbls(dm, input$table)
              })
        
        eDT(
            id = "DT",
            data = data,
            foreignTbls = foreignTbls,
            in_place = TRUE
        )
          
        invisible()
      }
  )
}


## ----screenshot.opts = list(vwidth = 700, vheight = 500, delay = 1), screenshot.alt = 'screenshots/howto_relational_db_dm_1.png'----
shiny::shinyApp( 
    ui = dbUI('id'),
    server =  function(input, output,session){
      dbServer('id', dm)
    })

