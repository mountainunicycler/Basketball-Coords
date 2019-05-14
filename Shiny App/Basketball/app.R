
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
shots3000 <- read_csv("Data/shots3000.csv")



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
		fluidRow(column(10, offset = 1, h1('Force Of Habit: NCAA Basketball'))),
		fluidRow(column(10, offset = 1, h3('Spatial map of all shots taken up to the 2018 NCAA season by the team selected below.'))),
		fluidRow(
			column(3, offset = 1,
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
		fluidRow(column(10, offset = 1, p('The plot below is a hexplot that shows the frequency of shots at each location on the basketball court. Each hexagon is interactive and will show the total number of shots from the location. This is a heatmap that will be red where a bunch of shots were taken and blue where only a few were taken. We filtered out free throws since the freethrow position is by far the most common single point, hiding the variation in the rest of the data.'))),
		
		fluidRow(
			column(12, 
				withLoader(plotlyOutput('mainPlot', height="75vh"), 
					type='html', loader='loader1')
			)
		),

		# fluidRow(
		# 	column(2, offset = 1,
		# 		selectInput('team1', "Team 1", c('Orange',
		# 			'Cavaliers',
		# 			'Demon Deacons',
		# 			'Fighting Irish',
		# 			'Hokies',
		# 			'Hurricanes',
		# 			'Blue Devils',
		# 			'Seminoles',
		# 			'Tar Heels',
		# 			'Wolfpack')
		# 		),
		# 		selectInput('team2', "Team 2", c('Cavaliers',
		# 			'Blue Devils',
		# 			'Demon Deacons',
		# 			'Fighting Irish',
		# 			'Hokies',
		# 			'Hurricanes',
		# 			'Orange',
		# 			'Seminoles',
		# 			'Tar Heels',
		# 			'Wolfpack')
		# 		),
		# 		selectInput('team3', "Team 3", c('Seminoles',
		# 			'Cavaliers',
		# 			'Demon Deacons',
		# 			'Fighting Irish',
		# 			'Hokies',
		# 			'Hurricanes',
		# 			'Orange',
		# 			'Blue Devils',
		# 			'Tar Heels',
		# 			'Wolfpack')
		# 		),
		# 		selectInput('team4', "Team 4", c('Hokies',
		# 			'Cavaliers',
		# 			'Demon Deacons',
		# 			'Fighting Irish',
		# 			'Blue Devils',
		# 			'Hurricanes',
		# 			'Orange',
		# 			'Seminoles',
		# 			'Tar Heels',
		# 			'Wolfpack')
		# 		),
		# 		selectInput('team4', "Team 4", c('Hokies',
		# 			'Cavaliers',
		# 			'Demon Deacons',
		# 			'Fighting Irish',
		# 			'Blue Devils',
		# 			'Hurricanes',
		# 			'Orange',
		# 			'Seminoles',
		# 			'Tar Heels',
		# 			'Wolfpack')
		# 		)
		# 	),
		# 	column(8, 
		# 		plotlyOutput('radarplot', height="75vh")
		# 	)
		# ),
		fluidRow(
			column(2, offset = 1,
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
			column(2,
				selectInput('team2', "Team 2", c('Cavaliers',
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
			column(2,
				selectInput('team3', "Team 3", c('Seminoles',
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
			column(2,
				selectInput('team4', "Team 4", c('Hokies',
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
	fluidRow(column(10, offset = 1, p('The plot above is a radar chart that will take 4 teams or less if the same team is selected more than once, and compare their shots to the other teams that were picked. If one wants to compare teams that are not Duke, then one should not pick Duke on the dropdown menu since they are so dominant. '))),
	# includeScript("jquery.js"),
	includeScript("basketball.js")
)



# Define server logic required to draw a histogram
server = function(input, output) {
	
	output$mainPlot <- renderPlotly({

		# input$reload
		# show('load-anim')

		# bears = read_csv("Bears.csv")
	  # input$hexteam = 'Blue Devils'

		dir = 'Data/'
		type = '.csv'
	  
		hexplot = read_csv(paste(dir,input$hexteam,type,sep = ''),
         cols(
           team_name = col_character(),
           three_point_shot = col_logical(),
           shot_made = col_logical(),
           type = col_character(),
           shot_type = col_character(),
           shot_subtype = col_character(),
           points_scored = col_double(),
           event_coord_x = col_double(),
           event_coord_y = col_double(),
           team_basket = col_character(),
           elapsed_time_sec = col_double(),
           game_clock = col_time(format = ""),
           round = col_character()
         ), col_names = T) %>%
		  filter(type != 'freethrow')%>%
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
	    summarize(`Three Point Shots` = sum(three_point),
	              `Three Point Made` = sum(ifstuff),
	              `Three Point Shot Percent` = (`Three Point Made`)/(`Three Point Shots`),
	              `Total Shots` = n(),
	              `Total Shots Made` = sum(success_point),
	              `Total Percent Made` = (`Total Shots Made`)/(`Total Shots`)) %>%
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

