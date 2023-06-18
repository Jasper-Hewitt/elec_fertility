#creating master csv
library(tidyverse)
library(tidytext)
library(stringr)
library(readtext)
library(lubridate)
library(purrr)
library(readxl)
setwd("/Users/jasperhewitt/Desktop/fertnews")
master_df <- read.csv("/Users/jasperhewitt/Desktop/fertnews/FB_All_Candidates.csv")
candidate_info <- read_xlsx("/Users/jasperhewitt/Desktop/fertnews/candidates_info.xlsx")


#only keep relevant columns 
master_df <- master_df %>%
  select('Page.Name', 'Post.Created.Date', 'Message', 'Image.Text', 'Description')

#merge the two dfs on Page.Name
master_df<-master_df%>%
  left_join(candidate_info, by='Page.Name')

#format time 
master_df$Post.Created.Date <- as.Date(master_df$Post.Created.Date, "%Y-%m-%d")
master_df$Date_Started_Running <- as.Date(master_df$Date_Started_Running, "%Y-%m-%d")
#manually fix黃偉哲!!!!! they get turned around
#then also check 林智堅's posts, they also got some problem! idk why!


#filter out the posts before each candidate formally started running
master_df <- master_df%>%
  filter(Post.Created.Date >= Date_Started_Running)

library(dplyr)

# some differ significantly from their other dates
#like 黃偉哲, find out what went wrong
#i downloaded his csv files and it says 495. but after
#running this he suddenly has 540?
#it somehow changes the date from 03-02 to 02-03???? 
#why does it do this this is super weird. 
count_df <- master_df %>%
  group_by(Candidate) %>%
  summarise(Count = n(), .groups = "drop")

# Print the result
print(count_df)






#delete all the posts before they started running
#IDEA
#then select date (later than) -> started running date (this way we should only 
#be left with the posts from their running period! 













#merge on Page.Name (they both have it). 
#this way we automatically assign the city, party and english name
#then select date (later than) -> started running date (this way we should only 
#be left with the posts from their running period! 

#verify with the other master_df (same number of rows).

#then put the datacleaning part here already? or not.?

