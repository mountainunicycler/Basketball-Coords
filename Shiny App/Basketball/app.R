
library(shiny)
library(shinyjs)
library(shinycustomloader)
library(plotly)
library(tidyverse)

options(shiny.port = 8080)


# Define UI for application that draws a histogram
ui = fluidPage(
	useShinyjs(),
	includeCSS("basketball.css"),
	# div(
	# 		id = 'load-anim',
	# 		h1("LOADING")
	# 	),
	div(
		id = 'main-content',
		fluidRow(column(12, h1(''))),
		fluidRow(
			column(12, 
				withLoader(plotlyOutput('mainPlot', height="75vh"), 
					type='html', loader='loader1')
			)
		)
	),
	# includeScript("jquery.js"),
	includeScript("basketball.js")
)

# Define server logic required to draw a histogram
server = function(input, output) {
	
	output$mainPlot <- renderPlotly({

		# input$reload
		# show('load-anim')

		bears = read_csv("Bears.csv")

		
		hexplot = ggplot(bears, aes(event_coord_x, event_coord_y)) +
			geom_hex(binwidth=15) + 
			coord_fixed() + 
			theme_void() + 
			scale_fill_distiller(palette = 'Spectral') +
			theme(legend.position = "none",
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )


		# hide('load-anim')
		print(hexplot)
	})
}

# Run the application 
shinyApp(ui = ui, server = server)

