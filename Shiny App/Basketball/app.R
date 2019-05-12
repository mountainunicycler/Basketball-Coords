
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


radarchart2 <- function(data,varlabs=NULL,grplabs=NULL,colors=(1:nrow(data)),axislim=NULL,fill=TRUE,title="")
{
  
  makeTransparent<-function(someColor, alpha=50)
  {
    newColor<-col2rgb(someColor)
    apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                                blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
  }
  if(!is.null(varlabs)) { 
    if(length(varlabs) != ncol(data)) {stop("varlabs must have same length as ncol(data)")} 
    names(data) <- varlabs }
  if(!is.null(grplabs)) { 
    if(length(grplabs) != nrow(data)) {stop("grplabs must have same length as ncol(data)")} 
    rownames(data) <- grplabs }
  
  varlabs <- colnames(data); grplabs <- rownames(data)
  
  maxnum <- max(data); minnum <- min(data)
  if(!is.null(axislim)) {  
    maxnum <- axislim[2]; minnum <- axislim[1]
    if( axislim[2] <= axislim[1] ) { stop("Max value must be greater than min value") }
    if(!is.numeric(axislim)) { stop("Axis limits must be numeric")}
  }
  maxnum <- round(maxnum,1);minnum <- round(minnum,1)
  
  temp <- rbind(rep(maxnum,ncol(data)),rep(minnum,ncol(data)),data)
  
  colors_border <- colors
  colors_fill <- NA
  if(fill) { colors_fill <- makeTransparent(colors_border) }
  caxislabels <- c(minnum,rep("",3),maxnum)
  radarchart( temp, maxmin=TRUE, pcol=colors_border, pfcol=colors_fill,plwd=2 , plty=1,
              cglcol="grey",cglty=1,cglwd=0.8,vlcex=0.8,
              axistype=1,axislabcol="black",caxislabels=caxislabels,pch=20,title=title)
  
  legend("topright", legend = grplabs, bty = "n", pch=20 , col=colors_border , text.col = "black", cex=0.8, pt.cex=1,lty=1,
         lwd=2)
}

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
			column(12, 
				withLoader(plotlyOutput('mainPlot', height="75vh"), 
					type='html', loader='loader1')
			)
		),
		fluidRow(
			column(12, 
				plotlyOutput('radarplot')
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

	output$radarplot = renderPlotly({
	  NEW <- read_csv("Data/big.csv")
	  
	  Radar_teams <- NEW %>%
	    group_by(team_name) %>%
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
	    ggradar()
	  })
}

# Run the application 
shinyApp(ui = ui, server = server)

