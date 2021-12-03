# Generate Snowflakes

# function to build snowflake data
snowflake <- function(snowflake_length,snowflake_scale,number_of_branches,branch_scale,branch_angle){
  
  # set snowflake_id
  snowflake_id <- paste0(snowflake_length,'-',snowflake_scale,'-',number_of_branches,'-',branch_scale,'-',branch_angle)
  
  # build single length of snowflake
  base_y <- 0:snowflake_length
  base_x <- rep(0,length(base_y))
  base_path <- base_y
  base_id <- rep("Base",length(base_y))
  
  # build branches of snowflake
  # initiate centre point of branch
  branch_centre_y <- 1:snowflake_length
  branch_centre_x <- rep(0,snowflake_length)
  branch_centre_path <- rep(2,snowflake_length)
  branch_id <- paste("Branch ",branch_centre_y)
  
  # set top and bottom of snowflake branch
  # use tan function to determine length of branch
  branch_top_x <- c(rep(tanpi(1/6),snowflake_length))*(1:snowflake_length)
  branch_top_y <- branch_centre_y
  branch_top_path <- rep(1,snowflake_length)
  
  branch_bottom_x <- c(rep(tanpi(1/6),snowflake_length))*(1:snowflake_length)*(-1)
  branch_bottom_y <- branch_centre_y
  branch_bottom_path <- rep(3,snowflake_length)
  
  # build base snowflake data frame
  base_df <- data.frame(type=base_id,path=base_path,x=base_x,y=base_y,stringsAsFactors = F)
  
  # build snowflake branches data frame
  branch_top <- data.frame(type=branch_id,path=branch_top_path,x=branch_top_x,y=branch_top_y,stringsAsFactors = F)
  branch_centre <- data.frame(type=branch_id,path=branch_centre_path,x=branch_centre_x,y=branch_centre_y,stringsAsFactors = F)
  branch_bottom <- data.frame(type=branch_id,path=branch_bottom_path,x=branch_bottom_x,y=branch_bottom_y,stringsAsFactors = F)
  branch_df <- rbind(branch_top,branch_centre,branch_bottom)
  
  # filter for chosen number of branches
  branch_filter <- sample(branch_id,number_of_branches)
  branch_df <- subset(branch_df, type %in% branch_filter)
  
  # scale branches
  branch_df$branch_length <- branch_df$x * branch_scale
  branch_df$x <- branch_df$branch_length
  
  # angle branches
  branch_df$x <- cos(branch_angle) * branch_df$branch_length 
  branch_df$y <- (ifelse(branch_df$path == 3,-1,1) * sin(branch_angle)) * branch_df$branch_length + branch_df$y
  
  # drop branch length branch_df
  branch_df <- branch_df[ , (names(branch_df)!='branch_length')]
  
  # build snowflake
  snowflake_arm <- rbind(base_df,branch_df)
  snowflake_arm$snowflake_id <- snowflake_id
  
  # scale entire snowflake
  snowflake_arm$x <- snowflake_arm$x * snowflake_scale
  snowflake_arm$y <- snowflake_arm$y * snowflake_scale
  
  # set initial arm as arm 0
  snowflake_df <- snowflake_arm
  snowflake_df$arm <- 0
  
  # set list 1,2,3,4,5 to loop and rotate, building a new snowflake arm each time
  rotations <- 1:5
  
  for(i in rotations){
    rotated_arm <- snowflake_arm
    rotated_arm$arm <- i
    px <- rotated_arm$x
    py <- rotated_arm$y
    rotated_arm$x <- (px * cospi(i/3)) - (py * sinpi(i/3))
    rotated_arm$y <- (px * sinpi(i/3)) + (py * cospi(i/3))
    
    snowflake_df <- rbind(snowflake_df,rotated_arm)
  }
  
  # output snowflake data frame
  return(snowflake_df)
}

# Randomly generate values for each snowflake parameter
sn_length <- rep(6,100)
sn_scales <- round(runif(100, min=0.6, max=1),2)
n_branches <- round(runif(100, min=3, max=5),0)
bn_scales <- round(runif(100, min=0.5, max=0.8),2)
bn_angle <- round(runif(100, min=-6, max=6),0)*pi*(1/24)

# Check all snowflakes unique
all_snowflake_ids <- paste0(sn_length,"-",sn_scales,"-",n_branches,"-",bn_scales,"-",bn_angle)
length(unique(all_snowflake_ids))==length(all_snowflake_ids)

# set columns and rows for Tableau grid
cols <- rep(1:10,10)
rows <- c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(7,10),rep(8,10),rep(9,10),rep(10,10))

# Loop through and build each snowflake
for(a in 1:100){
  df <- snowflake(sn_length[a],sn_scales[a],n_branches[a],bn_scales[a],bn_angle[a])
  df$row <- rows[a]
  df$col <- cols[a]
  
  if(a == 1){
    snowflake_df <- df
  }
  if(a > 1){
    snowflake_df <- rbind(snowflake_df,df)
  }
}

# output all snowflakes to csv file
write.csv(snowflake_df,"test.snowflake.csv",row.names = F)
