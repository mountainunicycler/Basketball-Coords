library(tidyverse)

teams = c('Cavaliers',
'Fighting Irish',
'Hokies',
'Hurricanes',
'Blue Devils',
'Seminoles',
'Tar Heels',
'Wolfpack')

dir = 'Data/'
type = '.csv'

# team1 = 'Demon Deacons'
# raw = read_csv('Data/Demon Deacons.csv', 
#        cols(
#          team_name = col_character(),
#          three_point_shot = col_logical(),
#          shot_made = col_logical(),
#          type = col_character(),
#          shot_type = col_character(),
#          shot_subtype = col_character(),
#          points_scored = col_double(),
#          event_coord_x = col_double(),
#          event_coord_y = col_double(),
#          team_basket = col_character(),
#          elapsed_time_sec = col_double(),
#          game_clock = col_time(format = ""),
#          round = col_character()
#        ), col_names = T)
# subsampled = raw %>% sample_n(3000)
# shots3000 = subsampled

for(team in teams){
	raw = read_csv(paste(dir,team,type,sep = ''),
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
         ), col_names = T)
	subsampled = raw  %>%
    filter(type != 'freethrow')%>%
    sample_n(5000)

	# shots3000 = rbind(subsampled, shots3000)
  subsampled %>% write_csv(paste(dir,team,'_5000',type,sep = ''))

}

# shots3000 %>% write_csv('Data/shots3000.csv')
