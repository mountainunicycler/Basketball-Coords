
library(shiny)
library(shinyjs)
library(shinycustomloader)
library(plotly)
library(tidyverse)
library(fmsb)
library(ggradar)
library(scales)

options(shiny.port = 8080)

#bigger dataset

# csv_path <- './Shiny App/Basketball/Data'
# teams_list <- list.files(csv_path)
# teams_list

# empty_list <- list()
# 
# for(i in 1:length(teams_list)){
#   print(paste(csv_path,teams_list[i],sep=''))
#   empty_list[[i]] <- read_csv(paste(csv_path,'/',teams_list[i],sep=''))
#   empty_list[[i]]$team <-teams_list[i]
# }

# Big_Teams <- bind_rows(empty_list)

NEW <- read_csv("Data/big.csv")



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
		fluidRow(column(12, h1('Force Of Habit: NCAA Basketball'))),
		fluidRow(
			column(3,
				selectInput('hexteam', "Select Hexplot Team", 
					c('Blue Devils',
					'Cavaliers',
					'Demon Deacons',
					'Fighting Irish',
					'Hokies',
					'Hurricanes',
					'Orange',
					'Seminoles',
					'Tar Heels',
					'Wolfpack'
					)
				)
			) 
		),
		fluidRow(
			column(12, 
				withLoader(plotlyOutput('mainPlot', height="75vh"), 
					type='html', loader='loader1')
			)
		),
		fluidRow(
			column(3,
				selectInput('team1', "Team 1", c('Orange',
					'Cavaliers',
					'Demon Deacons',
					'Fighting Irish',
					'Hokies',
					'Hurricanes',
					'Blue Devils',
					'Seminoles',
					'Tar Heels',
					'Wolfpack')
					)
			), 
			column(3,
				selectInput('team2', "Team 1", c('Cavaliers',
					'Blue Devils',
					'Demon Deacons',
					'Fighting Irish',
					'Hokies',
					'Hurricanes',
					'Orange',
					'Seminoles',
					'Tar Heels',
					'Wolfpack')
					)
			),
			column(3,
				selectInput('team3', "Team 1", c('Seminoles',
					'Cavaliers',
					'Demon Deacons',
					'Fighting Irish',
					'Hokies',
					'Hurricanes',
					'Orange',
					'Blue Devils',
					'Tar Heels',
					'Wolfpack')
					)
			),
			column(3,
				selectInput('team4', "Team 1", c('Hokies',
					'Cavaliers',
					'Demon Deacons',
					'Fighting Irish',
					'Blue Devils',
					'Hurricanes',
					'Orange',
					'Seminoles',
					'Tar Heels',
					'Wolfpack')
					)
			)
		),
		fluidRow(
			column(12, 
				plotlyOutput('radarplot', height="75vh")
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

		# bears = read_csv("Bears.csv")

		hexplot = NEW %>%
		  filter(team_name == input$hexteam) %>%
		  ggplot() +
  			geom_hex(aes(event_coord_x, event_coord_y), binwidth=15) + 
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

	output$radarplot = renderPlotly({
	  
	  Radar_teams <- NEW %>%
	    group_by(team_name) %>%
	    filter(team_name %in% c(input$team1, input$team2, input$team3, input$team4)) %>%
	    select(three_point_shot, shot_made, points_scored) %>%
	    mutate(three_point = ifelse(three_point_shot == TRUE, 1, 0)) %>%
	    mutate(success_point = ifelse(shot_made == TRUE, 1, 0)) %>%
	    mutate(stuff = success_point + three_point) %>%
	    mutate(ifstuff = ifelse(stuff == 2, 1, 0))
	  
	  Radar_teams2 <- Radar_teams %>%
	    summarize(three_point_attempt = sum(three_point),
	              three_point_made = sum(ifstuff),
	              three_point_percent = (three_point_made)/(three_point_attempt),
	              total_shots = n(),
	              total_made = sum(success_point),
	              total_percent = (total_made)/(total_shots)) %>%
	    mutate_each(funs(rescale), -team_name)
	  Radar_teams2 %>%
	    ggradar() + scale_color_brewer(palette = 'Spectral') + 
	    theme(
	      text = element_text(colour = "white"),
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )

	  })
}

# Run the application 
shinyApp(ui = ui, server = server)

