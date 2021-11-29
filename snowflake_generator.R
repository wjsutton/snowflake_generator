# Generate Snowflakes

# set snowflake parameters


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
  
  # build data frame
  
  base_df <- data.frame(type=base_id,path=base_path,x=base_x,y=base_y,stringsAsFactors = F)
  
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
  #p'y = sin(theta) * (px-ox) + cos(theta) * (py-oy) + oy
  #branch_df$x <- branch_df$branch_length*cos(branch_angle)
  branch_df$y <- (ifelse(branch_df$path == 3,-1,1) * sin(branch_angle)) * branch_df$branch_length + branch_df$y
  
  branch_df <- branch_df[ , (names(branch_df)!='branch_length')]
  
  # build snowflake
  snowflake_arm <- rbind(base_df,branch_df)
  snowflake_arm$snowflake_id <- snowflake_id
  
  snowflake_df <- snowflake_arm
  snowflake_df$arm <- 0
  
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
  
  #snowflake_df$x <- ifelse(snowflake_df$type!='Base' && snowflake_df$path==3,-snowflake_df$x,snowflake_df$x)
  #snowflake_df$y <- ifelse(snowflake_df$type!='Base' && snowflake_df$path==3,-snowflake_df$y,snowflake_df$y)
  
  return(snowflake_df)
}

sn_length <- rep(6,100)
sn_scales <- round(runif(100, min=0.5, max=1),2)
n_branches <- round(runif(100, min=3, max=5),0)
bn_scales <- round(runif(100, min=0.5, max=0.8),2)
bn_angle <- round(runif(100, min=-4, max=4),0)*pi*(1/12)


cols <- rep(1:10,10)
rows <- c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(7,10),rep(8,10),rep(9,10),rep(10,10))

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

#snowflake_length <- 5
#snowflake_scale <- 1
#number_of_branches <- 3
#branch_scale <- 1
#branch_angle <- 0

write.csv(snowflake_df,"test.snowflake.csv",row.names = F)
