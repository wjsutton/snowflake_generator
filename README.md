<h1 style="font-weight:normal">
  Snowflake Generator :snowflake::snowflake::snowflake:
</h1>


[![Status](https://img.shields.io/badge/status-active-success.svg)]() [![GitHub Issues](https://img.shields.io/github/issues/wjsutton/snowflake_generator.svg)](https://github.com/wjsutton/snowflake_generator/issues) [![GitHub Pull Requests](https://img.shields.io/github/issues-pr/wjsutton/snowflake_generator.svg)](https://github.com/wjsutton/snowflake_generator/pulls) [![License](https://img.shields.io/badge/license-MIT-blue.svg)](/LICENSE)

A project to generate geometric co-ordinates and paths of snowflakes to create a viz in Tableau

[Twitter][Twitter] :speech_balloon:&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;[LinkedIn][LinkedIn] :necktie:&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;[GitHub :octocat:][GitHub]&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;[Website][Website] :link:


<!--
Quick Link 
-->

[Twitter]:https://twitter.com/WJSutton12
[LinkedIn]:https://www.linkedin.com/in/will-sutton-14711627/
[GitHub]:https://github.com/wjsutton
[Website]:https://wjsutton.github.io/

### :a: About

Winter is coming and with that comes snowflakes, and from seeing [snowflake icons](https://www.google.com/search?q=snowflake+icons&tbm=isch) and computer-generated art like [CJ Mayes' Geometric Pattern viz](https://public.tableau.com/app/profile/cj.mayes/viz/GeometricPattern/Geometric_001) I figured I could create the co-ordinates for simple geometric snowflakes using R and then visualise them in Tableau.

The script [snowflake_generator.R](https://github.com/wjsutton/snowflake_generator/blob/master/snowflake_generator.R) contains a function to generate simple geometric snowflake given a few parameters, a loop that randomly creates snowflakes using random numbers, and then writes the data to csv.

### :clipboard: Method

For our snowflakes they have two features:
- each snowflake is unique
- each snowflake has 6-fold symmetry, meaning a rotation of 60 degrees is the same shape as before 

<img  align="left" src='https://github.com/wjsutton/snowflake_generator/blob/master/images/Method%201%20-%20rotate.png?raw=true'>
<img  align="left" src='https://github.com/wjsutton/snowflake_generator/blob/master/images/Method%202%20-%20defintions.png?raw=true'>
<img  align="left" src='https://github.com/wjsutton/snowflake_generator/blob/master/images/Method%203%20-%20Tableau.png?raw=true'>

1. Knowing a snowflake has 6-fold symmetry we can craft 1 "arm" of the snowflake then rotate the arm 5 times to make the snowflakes symmetrical.
2. To make our snowflakes more interesting each snowflake "arm" has a varying number of branches, of differing length and angle
3. From the points generated we can use Tableau's Line chart mark "Line" and the path option to draw snowflakes on the screen

### :hammer_and_wrench: Code and Build

Walking through the script [snowflake_generator.R](https://github.com/wjsutton/snowflake_generator/blob/master/snowflake_generator.R) 

The function `snowflake` takes the parameters:
- snowflake length - the length of the snowflake's arms, and determines the number of branches available
- snowflake scale - allows you to resize the entire snowflake after it is generated
- number_of_branches - determines how many branches each arm has, branch positions are randomly assigned
- branch_scale - sizes the length of the branch where 1 will mean a branch will touch a branch on another arm
- branch angle - gives the slant of the branches, 0 will be perpendicular to the arm
 
```
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
```

After building the function we need to generate some random data that will become our snowflakes, in the example below I want each snowflake to be of length 6 but randomise all other options.

```
sn_length <- rep(6,100)
sn_scales <- round(runif(100, min=0.6, max=1),2)
n_branches <- round(runif(100, min=3, max=5),0)
bn_scales <- round(runif(100, min=0.5, max=0.8),2)
bn_angle <- round(runif(100, min=-6, max=6),0)*pi*(1/24)
```

Lastly, we check our snowflakes are all unique, in a loop generate each snowflake and then write the data to csv.

```
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
```

In Tableau to build our grid of snowflakes we should set up the following:
- Columns: col (as discrete), X (as dimension)
- Rows: row (as discrete), Y (as dimension)
- Marks Card: Type: Line, | Detail: Arm, Snowflake Id, Type | Path: Path

Then recolour your dashboard to give you snowflakes a snowy feel, I chose a dark blue background `#223459` to contrast against `#f5f5f5` snowflakes at 90% opacity.

If you run into any issues you can download my dashboard from [Tableau Public.](https://public.tableau.com/app/profile/wjsutton/viz/Snowflakes/Snowflakes)

---

<div style="overflow: hidden;margin: 0 10px 0 0">
<a href="https://public.tableau.com/app/profile/wjsutton/viz/Snowflakes/Snowflakes">
<img src='https://github.com/wjsutton/snowflake_generator/blob/master/Snowflakes.png?raw=true' width="100%">
</a>
</div>

<br>

Will Sutton, December 2021<br>
[Twitter][Twitter] :speech_balloon:&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;[LinkedIn][LinkedIn] :necktie:&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;[GitHub :octocat:][GitHub]&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;[Website][Website] :link:
