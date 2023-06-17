#creating master csv
library(tidyverse)
library(tidytext)
library(stringr)
library(readtext)
library(lubridate)
library(purrr)
library(readxlsx)
setwd("/Users/jasperhewitt/Desktop/fertnews")
master_df <- read.csv("/Users/jasperhewitt/Desktop/fertnews/FB_All_Candidates.csv")
candidate_info <- read_xlsx("/Users/jasperhewitt/Desktop/fertnews/candidates_info.xlsx")


#only keep relevant columns 
master_df <- master_df %>%
  select('Page.Name', 'Post.Created.Date', 'Message', 'Image.Text', 'Description')

#merge on Page.Name (they both have it). 
#this way we automatically assign the city, party and english name
#then select date (later than) -> started running date (this way we should only 
#be left with the posts from their running period! 

#verify with the other master_df (same number of rows).

#then put the datacleaning part here already? or not.?

