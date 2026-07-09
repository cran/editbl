## ----echo = TRUE, results = 'hide'--------------------------------------------
library(shiny)
library(editbl)

## -----------------------------------------------------------------------------
ui <- fluidPage(eDTOutput("DT"))

# Define a custom update function
e_rows_update.mtcars <- function(
    x,
    y,
    by = NULL,
    ...,
    match = match,
    unmatched = c("error", "ignore"),
    copy = FALSE,
    in_place = FALSE){		
		
		# Extra checks
		if(any(y[,"mpg"] < 0)){
			stop("mpg should be a positive value!")	
		}
		
		# Logging
		print("Updated rows:")
		print(y)
		
		# Drop extra class and execute default code
		class(x) <- class(x)[class(x) != 'mtcars']
		e_rows_update(
	      x = x,
	      y = y,
	      by = by,
	      ... ,
	      match = match,
	      unmatched = unmatched,
	      copy = copy,
	      in_place = in_place
	  )
	}

server <- function(input, output, session){
	
	# Add a new class to your data object
	df = tibble::tibble(mtcars)
	class(df) <- c("mtcars", class(df))

	  result = eDT(
	    id = "DT",
	    data = df
	)
	   
}
shinyApp(ui, server)

## -----------------------------------------------------------------------------
ui <- fluidPage(eDTOutput("DT"))

server <- function(input, output, session){
	  result = eDT(
	    id = "DT",
	    data = mtcars
	  )
	  
	  # Some logging
	  observe({
		print('These rows have been permanently deleted:')
		print(result$deleted())
	  })
	   
}
shinyApp(ui, server)

