library(readxl)
library(tidyverse)
library(stringr)
library(openxlsx)
library(readr)
library(ggplot2)


#get data
huang_shanshan <- read.csv('https://raw.githubusercontent.com/Jasper-Hewitt/elec_fertility/main/data/taipei_huang.csv')
chiang_wanan <- read.csv('https://raw.githubusercontent.com/Jasper-Hewitt/elec_fertility/main/data/taipei_chiang.csv')
chen_shihchung<-read.csv('https://raw.githubusercontent.com/Jasper-Hewitt/elec_fertility/main/data/taipei_chen.csv')
hou_youyi<-read.csv('https://raw.githubusercontent.com/Jasper-Hewitt/elec_fertility/main/data/newtaipei_hou.csv')
lin_chialung<-read.csv('https://raw.githubusercontent.com/Jasper-Hewitt/elec_fertility/main/data/newtaipei_lin.csv')

#____________________________________________________________________________________________________________________
#get rows for manual labelling
#put into one df and select the proper columns 
ntp_tp_df <- rbind(huang_shanshan, chiang_wanan, chen_shihchung, hou_youyi, lin_chialung)
#get colnames
print(colnames(ntp_tp_df))
#select relevant columns
ntp_tp_df <- select(ntp_tp_df, "Page.Name", "Post.Created", "URL", "Link", "Message", "Image.Text", "Link.Text", "Description")

#randomize rows
ntp_tp_df <- ntp_tp_df[sample(nrow(ntp_tp_df)), ]

#save as xlsx
write.xlsx(ntp_tp_df, "ntp_tp_df.xlsx")

#____________________________________________________________________________________________________________________









#fill na in Message column (to prevent errors when we use detect)
#still check this because there are more columns that might contain the data we need
#it will have to be an or search I guess
huang_shanshan$Message[is.na(huang_shanshan$Message)] <- 0
chiang_wanan$Message[is.na(chiang_wanan$Message)] <- 0
chen_shihchung$Message[is.na(chen_shihchung$Message)] <- 0
hou_youyi$Message[is.na(hou_youyi$Message)] <- 0
lin_chialung$Message[is.na(lin_chialung$Message)] <- 0

#find posts related to 生育率. we can adjust the search terms later.
#define keywords
search_pattern<-"生育|生孩子|孕育|懷孕|育兒|育嬰|新生兒|托嬰|公托|臨托|產檢|孕"

#search
huang_shanshan$fert_mention <- ifelse(grepl(search_pattern, huang_shanshan$Message),1,0)
chiang_wanan$fert_mention <- ifelse(grepl(search_pattern,chiang_wanan$Message),1,0)
chen_shihchung$fert_mention <- ifelse(grepl(search_pattern,chen_shihchung$Message),1,0)
hou_youyi$fert_mention <- ifelse(grepl(search_pattern,hou_youyi$Message),1,0)
lin_chialung$fert_mention <- ifelse(grepl(search_pattern,lin_chialung$Message),1,0)

#count in how many tweets our keywords appeared fc = fertility count
huang_shanshan_fc <- sum(huang_shanshan$fert_mention == 1)
chiang_wanan_fc <- sum(chiang_wanan$fert_mention == 1)
chen_shihchung_fc <- sum(chen_shihchung$fert_mention == 1)
hou_youyi_fc <- sum(hou_youyi$fert_mention == 1)
lin_chialung_fc <- sum(lin_chialung$fert_mention == 1)

print(huang_shanshan_fc) #16 posts about our search terms
print(chiang_wanan_fc) #35 posts about our search terms
print(chen_shihchung_fc) #15 posts about our search terms
print(hou_youyi_fc) #4
print (lin_chialung_fc) #11


#bind these all together
fert_df<-rbind(huang_shanshan, chiang_wanan, chen_shihchung, hou_youyi, lin_chialung)

#keep only the rows where fert_mention=1
fert_df <- fert_df[fert_df$fert_mention == 1, ]

#select relevant columns
fert_df <- select(fert_df, "Page.Name", "Post.Created", "URL", "Link", "Message", "Image.Text", "Link.Text", "Description", "fert_mention")

#save as xlsx
write.xlsx(fert_df, "fertTPNTP_df.xlsx")





#__________________________________________________________________________________________________
#plot results in simple bar plot

# in the following lines I create a dataframe to plot the results.
total_mentions_taipei <- data.frame(
  candidate=c("Huang Shan-shan","Chiang Wan-an","Chen Shih-chung", 'Hou you-yi', 'Lin Chia-lung') ,
  total_mentions=c(huang_shanshan_fc, chiang_wanan_fc, chen_shihchung_fc, hou_youyi_fc, lin_chialung_fc)
)

#this one is somehow not working
# create a barplot with with ggplot.
# In order to display the discrepancy between the bars more clearly, I zoomed in on on
#the Y range 0 to 20 with ylim.
#ggplot(total_mentions_taipei, aes(x = candidate, y = total_mentions, fill = candidate)) +
#  geom_bar(stat = "identity") +
#  coord_cartesian(ylim = c(0, 50)) +


# Create a bar plot with ggplot
bar_plot <- ggplot(total_mentions_taipei, aes(x = candidate, y = total_mentions, fill = candidate)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0, 50)) +
  geom_text(aes(label = total_mentions), vjust = -0.5) +  # Add the exact numbers above the bars
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Candidate", y = "Total Mentions", title = "Total Mentions for Taipei Candidates")

# Display the plot
print(bar_plot)


#______________________________________________________________________________________________________________________________
#analyse some of the posts. do they really mention 生育率？ or are they using some of the keywords to talk about something else




