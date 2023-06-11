library(httr)
library(widyr)
library(writexl)
library(readr)
library(readxl)
library(writexl)
library(dplyr)
library(tidytext)
library(httr)
library(stringr)

#the code below was provided by Torrent

options(warn = -1)


setwd("/Users/jasperhewitt/Desktop")
fertsolutions<-read.xlsx("updated_fertility_check_manual.xlsx")


library(openxlsx)
fertsolutions<- read.xlsx("updated_fertility_check_manual.xlsx")



#____________________________________________________________________________________________________________________________________________________________________________________

#now let's do the same for our project. start with fert_check

library(openxlsx)

fertcheck <- read.xlsx("https://github.com/Jasper-Hewitt/elec_fertility/raw/main/data/GPT_feasibility_study/fertcheck_0.466.xlsx")
#there is only one row with a post that might be too long. find and delete it. 
which(nchar(fertcheck$Message) > 4000)
fertcheck <- fertcheck[nchar(fertcheck$Message) <= 4000, ]



fertcheck_chatgpt <- data.frame()

for (i in 1:105) {
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", "")),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      max_tokens = 1,
      temperature = 0,
      n = 1,
      messages = list(list(role = "system", content = "you are a researcher that judges whether a bunch of posts mention a certain topic. You only respond with 1 and 0"),
                      list(role = "user", content = paste0("judge wether the following posts are related to the eageing population, childbirth, policies related to small children
                                                            or any other policies aimed at creating a better environment to raise children, like subsidies or improved daycare.
                                                            if yes, give me 1. if it is completely unrelated. give me 0. do not give me anything else.
                                                        
                                                            this is the post:" , fertcheck$Message[i])))
    )
  )
  
  answer <- strsplit(httr::content(response)$choices[[1]]$message$content, "\n")[[1]]
  
  print(c(i, answer))
  
  temp <- data.frame(order = i,
                     chatgpt = answer[1],
                     stringsAsFactors = FALSE)
  
  fertcheck_chatgpt <- bind_rows(fertcheck_chatgpt, temp)
  
}

fertcheck_chatgpt <- fertcheck_chatgpt[!duplicated(fertcheck_chatgpt$order), ]


#now calculate the accuracy. change column name first
#fertcheck <- rename(fertcheck, manual_label = "fertility_check.(1.if.yes,.0.if.no)")

#put the two together
fertcheck$chatgpt <- fertcheck_chatgpt$chatgpt

#this is incase you have to clean something. but for now turn it off bc it messes up
#the code
#imdb_manual_done$chatgpt <- tolower(imdb_manual_done$chatgpt)
#imdb_manual_done$chatgpt <- gsub(".", "", imdb_manual_done$chatgpt)

fertcheck <- fertcheck %>%
  mutate(check = case_when(manual_label == chatgpt ~ 1,
                           TRUE ~ 0))



sum(fertcheck$check) / nrow(fertcheck) #accuracy 0.514



# Install and load the package
install.packages("writexl")
library(writexl)

# Write the dataframe to an Excel file
write_csv(fertcheck, "fertcheck_0.514.csv")

#___________________________________________________________________________________________________________________________________________________________________________________________________
#now for fert_solutions
fertsolutions<- read.csv("https://github.com/Jasper-Hewitt/elec_fertility/raw/main/data/GPT_feasibility_study/fertsolution_manual_labelled%202.csv")


#there is only one row with a post that might be too long. find and delete it. no instances of too long posts
which(nchar(fertsolutions$Message) > 4000)
fertsolutions <- fertsolutions[nchar(fertsolutions$Message) <= 4000, ]



fertsolutions_chatgpt <- data.frame()

for (i in 1:90) {
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", "")),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      max_tokens = 5,
      temperature = 0,
      n = 1,
      messages = list(list(role = "system", content = "you are a researcher that does text classification in facebook posts related to 生育率. you only answer with numbers"),
                      list(role = "user", content = paste0("what kind of policy direction does the following post offer to solve the problem. 
                                                            label 1 友善職場: examples include: 安胎假、產檢假, 產假、陪產假, 育嬰留職停薪, 家庭照顧假, 企業托育設施或措施.
                                                            
                                                            label 2 經濟支持: examples include: 妊娠生產醫療給付,各類保險生育給付, 地方政府生育津貼, 地方政府低收入戶生, 育補助+
                                                            育嬰留職停薪津貼, 就業者家庭部分托育, 費用補助, 特殊境遇家庭兒童托育津貼補助, 未就業家庭育兒津貼, 幼兒學前特別扣除額,中低收入戶幼兒就學補助+
                                                            5 歲幼兒免, 學費就學補助, 兒童與少年未來教育及發展帳戶(0-未滿 18 歲), 特殊境遇家庭子女生活津貼補助(0-15 歲以下), 弱勢家庭兒童及少年緊急生活扶助(0-未滿 18 歲)+
                                                            弱勢兒童及少年生活扶助, 3 歲以下兒童醫療補助, 發展遲緩兒童療育及交通費補助發展, 中低收入戶兒童及少年健保費補助(0-未滿 18 歲).
                                                            
                                                            label 3 教保環境:examples include : 孕產婦關懷中心,產前健康檢查,公立幼兒園、非營利幼兒園,公私協力托嬰中心,托育資源中心,居家式托育服務,兒童預防保健服務(0-未滿 7 歲) ,收出養媒合及寄養安置,兒童三級預防措施，防治兒虐事件.
                                                            
                                                            label 4: other: anything else
                                                            
                                                            label 5 no specific policy mentioned: only mentions the problem of 少子化 but does not offer a specific solution
                                                            only answer the right numbers, like this: 1. if there are two labels give me something like: 2, 3. do not write anything else. this is the post:" , fertsolutions$Message[i])))
    )
  )
  
  answer <- strsplit(httr::content(response)$choices[[1]]$message$content, "\n")[[1]]
  
  print(c(i, answer))
  
  temp <- data.frame(order = i,
                     chatgpt = answer[1],
                     stringsAsFactors = FALSE)
  
  fertsolutions_chatgpt <- bind_rows(fertsolutions_chatgpt, temp)
  
}

fertsolutions_chatgpt <- fertsolutions_chatgpt[!duplicated(fertsolutions_chatgpt$order), ]


#now calculate the accuracy. change column name first

#take out the row where we had an error
#fertsolutions <- fertsolutions[-c(30), ]


#put the two together
fertsolutions$chatgpt <- fertsolutions_chatgpt$chatgpt

#this is incase you have to clean something. but for now turn it off bc it messes up
#the code
#imdb_manual_done$chatgpt <- tolower(imdb_manual_done$chatgpt)
#imdb_manual_done$chatgpt <- gsub(".", "", imdb_manual_done$chatgpt)

# Remove commas and spaces because sometimes gpt adds a comma after the last label sometimes
fertsolutions <- fertsolutions %>% 
  mutate(manual_label = str_replace_all(manual_label, "[ ,]", ""),
         chatgpt = str_replace_all(chatgpt, "[ ,]", ""))

fertsolutions <- fertsolutions %>%
  mutate(check = case_when(manual_label == chatgpt ~ 1,
                           TRUE ~ 0))

sum(fertsolutions$check) / nrow(fertsolutions) #accuracy 0.2333


# save as csv 
write_csv(fertsolutions, "fertsolutions_GPT0.233.csv")

#_______________________________________________________________________________________

#now to get the general topics
topics_df<- read.csv("https://github.com/Jasper-Hewitt/elec_fertility/raw/main/data/GPT_feasibility_study/general_topics_manual_label.csv")


#see if there are posts that are too long, in this case there are none
which(nchar(topics_df$Message) > 4000)
topics_df <- topics_df[nchar(topics_df$Message) <= 4000, ]



topics_df_chatgpt <- data.frame()

for (i in 1:106) {
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", "")),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      max_tokens = 8,
      temperature = 0,
      n = 1,
      messages = list(list(role = "system", content = "you are a researcher that does text classification in facebook posts related to the elections, you only respond with numbers"),
                      list(role = "user", content = paste0("categorize the following posts based on the following labels only give me the numbers. if there are multiple labels, note it like this 1, 3, 5          
                                                           '社會住宅': '1' anything related to public housing or making housing more affordable
                                                            '幼托政策': '2' anything related to creating a better environment for little childeren and ideas that aim to boost the birth rate by encouraging people to have kids (subsidies, medical treatments, etc.)
                                                            '長照政策': '3', anything related to care for the elderly 
                                                            '運輸': '4', anything related to transportation (public transport, but also traffic)
                                                            '經濟發展': '5', anything related to companies that are trying 
                                                            'other': '6' literally anything else. this is the post:" , topics_df$Message[i])))
    )
  )
  
  answer <- strsplit(httr::content(response)$choices[[1]]$message$content, "\n")[[1]]
  
  print(c(i, answer))
  
  temp <- data.frame(order = i,
                     chatgpt = answer[1],
                     stringsAsFactors = FALSE)
  
  topics_df_chatgpt <- bind_rows(topics_df_chatgpt, temp)
  
}

topics_df_chatgpt <- topics_df_chatgpt[!duplicated(topics_df_chatgpt$order), ]


#now calculate the accuracy. change column name first


#put the two together
topics_df$chatgpt <- topics_df_chatgpt$chatgpt

#this is incase you have to clean something. but for now turn it off bc it messes up
#the code
#imdb_manual_done$chatgpt <- tolower(imdb_manual_done$chatgpt)
#imdb_manual_done$chatgpt <- gsub(".", "", imdb_manual_done$chatgpt)

# Remove commas and spaces because sometimes gpt adds a comma after the last label 
topics_df <- topics_df %>% 
  mutate(manual_label = str_replace_all(manual_label, "[ ,]", ""),
         chatgpt = str_replace_all(chatgpt, "[ ,]", ""))

topics_df <- topics_df %>%
  mutate(check = case_when(manual_label == chatgpt ~ 1,
                           TRUE ~ 0))

sum(topics_df$check) / nrow(topics_df) #accuracy 0.481


# save as csv 
write_csv(topics_df, "general_topics_GPT0.481.csv")







