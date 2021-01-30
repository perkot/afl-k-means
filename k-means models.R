# MODELS


# Model [1] 
    # these ruck-related stats distinguish ruckmen entirely from the group
    # 90.6% variance explained with just these stats
    hitouts,
    hitouts_to_advantage,
    ruck_contests,
    score_launches,
    # this midfield related fields produces some separation of midfielders, reducing overall variance
    clearances,
    contested_possessions,
    uncontested_possessions,
    centre_clearances,
    stoppage_clearances,
    # forwards
    goals,
    tackles_inside_fifty,
    f50_ground_ball_gets,
    # defenders
    rebounds,
    # one_percenters, keep this out, makes too similar to rucks
    # intercepts, # really improves variance, but muddies model
    contest_def_one_on_ones,
    disposal_efficiency_percentage
    
# Model 2 - 77.5% 
    
    rebounds,
    contested_possessions, # USEFUL
    centre_clearances,
    clearances,
    stoppage_clearances,
    contest_def_one_on_ones
    
# Model 3 - 84.1%
    
    contested_possessions, 
    centre_clearances,
    clearances,
    stoppage_clearances,
    contest_def_one_on_ones
    
# Model 4 - 88.4%
    hitouts,
    hitouts_to_advantage,
    ruck_contests,
    
    contest_def_losses,
    contest_def_one_on_ones,
    spoils

    
    
    
    
    