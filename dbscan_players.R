set.seed(123)

# scale all of our variables to add into k-means model 
all_scaled <- as.data.frame(scale(players[,5:48], 
                                  center = TRUE, 
                                  scale = TRUE))

# raw data - only variables to include in model
all_numeric <- dplyr::select_if(players[,5:48], is.numeric)

colnames(all_scaled)

all_scaled_m <- all_scaled %>%  
  select(
          kicks
          ,handballs                    
          ,disposals                      
          ,disposal_efficiency_percentage
          ,goals                          
          ,behinds                       
          ,hitouts                        
          ,tackles                       
          ,rebounds                      
          ,inside_fifties                
          ,clearances                     
          # ,clangers                      
          # ,free_kicks_for                
          # ,free_kicks_against            
          ,contested_possessions          
          ,uncontested_possessions       
          ,brownlow_votes                 
          ,contested_marks               
          ,marks_inside_fifty             
          ,one_percenters                
          ,bounces                       
          ,goal_assists                  
          # ,time_on_ground_percentage      
          ,centre_clearances             
          ,stoppage_clearances            
          ,score_involvements            
          ,metres_gained                  
          ,turnovers                     
          ,intercepts                     
          ,tackles_inside_fifty          
          ,contest_def_losses             
          ,contest_def_one_on_ones       
          ,contest_off_one_on_ones        
          ,contest_off_wins              
          ,def_half_pressure_acts         
          ,effective_kicks               
          ,f50_ground_ball_gets           
          ,ground_ball_gets              
          ,hitouts_to_advantage           
          ,intercept_marks               
          ,pressure_acts                  
          ,ruck_contests                 
          ,score_launches                 
          ,spoils 
)



# Compute DBSCAN using fpc package
library("fpc")
db <- fpc::dbscan(all_scaled_m, eps = 0.15, MinPts = 3)

# Plot DBSCAN results
library("factoextra")
fviz_cluster(db, data = all_scaled_m, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",
             palette = "jco", 
             ggtheme = theme_classic())

print(db)




