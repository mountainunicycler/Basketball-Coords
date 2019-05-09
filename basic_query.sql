SELECT home_name, away_name, event_coord_x, event_coord_y, shot_type, shot_subtype, three_point_shot, points_scored
FROM `bigquery-public-data.ncaa_basketball.mbb_pbp_sr`
WHERE home_name = 'Bears' 
AND shot_type IS NOT NULL
AND points_scored is NOT NULL
AND event_coord_x IS NOT NULL
AND event_coord_y IS NOT NULL


-------------


SELECT team_name, three_point_shot, shot_made, type, shot_subtype, points_scored
FROM `bigquery-public-data.ncaa_basketball.mbb_pbp_sr`
WHERE team_name = 'Cavaliers' 
AND type IN ('freethrow', 'fieldgoal')