setwd("/Users/jasperhewitt/Desktop/fertnews")
library(tidyverse)
library(tidytext)
library(quanteda)
library(stringr)
library(jiebaR)
library(readtext)
library(pdftools)
library(lubridate)
library(purrr)
library(wordcloud2)

master_df <- read.csv("https://github.com/Jasper-Hewitt/elec_fertility/raw/main/data/master.csv")

#get relevant columns 
names(master_df)
#put all columns that may contain content (message, image text and description together in a new column called 'Content')

#first, replace the NA in the columns that we want to put together, otherwise there will be NA's all over the place

master_df <- master_df %>%
  #put content from possible rows containing content together
  mutate(Content = paste(Message, Image.Text, Description, sep = " "))%>%
  #the last step results in some 'NA' because if we try to paste an empty row to another one it will show NA at the end.
  #remove these instances of "NA", this is not the the same as na.omit. 
  mutate(Content = str_replace_all(Content, "NA", "")) %>%
  rename("Candidate" = "Page.Name")%>%
  #only keep the relevant columns
  select('Candidate', 'City', 'Party', 'Content', 'Post.Created')%>%
  #delete empty rows, these rows are empty because they only contained videos
  #so we delete all the rows less than 3 characters, before (5998 obs), after (5976)
  #we can't do na.omit because we deleted all the instances of "NA" before
  filter(nchar(as.character(Content))>=3) 

#create important sentence column. 

#___________________________________________________________________________________________________________________
#TIMELINES

#per city

#per party


#___________________________________________________________________________________________________________________
#text classification through keyword search...
#according to the 5 categories that we found 經濟發展，運輸，拖育等等




#___________________________________________________________________________________________________________________
#wordclouds. let's try to do it first on the full posts 

#text processing
#keywords
#we can adjust these based on policies that we find. I took out 孕， maybe we should consider taking out 孕育 too
search_pattern<-"少子化|生育率|生育|生孩子|孕育|懷孕|育兒|育嬰|新生兒|托嬰|公托|臨托|產檢|祝你好孕|國家一起養"


#add a column called fert_mention, if this post mentions one of our keywords we give it 1, if there is no mention we give it 0
master_df <- master_df %>%
  mutate(fert_mention = ifelse(grepl(search_pattern, Content), 1, 0))

#create a new  df with only the posts about fertility
master_fert_df <- master_df %>% 
  filter(fert_mention == 1)


#__________________________________________________________________________________________________________________
#PLOT WORDCLOUD BASED ON FULL POSTS
## for word segmentation only

#add doc_id
master_fert_df <- master_fert_df %>% 
  mutate(doc_id = row_number())

stopwords <- readLines("stopwords_zh_trad.txt",
                           encoding = "UTF-8")



my_seg <- worker(bylines = T,
                 user = "customdict.txt",
                 symbol = T)

##Add customized terms if necessary
#temp_new_words <-c("九合一選舉", "蔣萬安", "黃珊珊", "陳時中")
#new_user_word(my_seg, temp_new_words)


master_fert_word <- master_fert_df %>%
  ## word tokenization
  unnest_tokens(
    output = word,
    input = Content,  # the name of the column we are plotting
    token = function(x)
      segment(x, jiebar = my_seg)
  ) %>%
  group_by(doc_id) %>%
  mutate(word_id = row_number()) %>% # create word index within each document
  ungroup



#if you want to add some custom keywords, we can use this.We want to take out some basic words like 
#少子化 and 生育率, because they are not very informative 
#custom_stopwords <- c("經濟", "科技", "報導", "可能", "指出", "認為", "新聞網", "國際", 
#                      "應該", "可能", "提出", "過去", "現在", "進行","今天", "相關", "社會",
#                      "議題", "很多", "undo", "需要", "需求", "已經", "目前", "今年", "透過",
#                      "地方", "沒有", "記者", "成為", "持續", "市場", "表示", "台灣", "造成",
#                      "不少", "原因", "影響", 
#                      "少子化", "台北", "生育率", "問題", "育兒", "生育")  # specific words about 生育率
custom_stopwords<-""
stopwords <- c(stopwords, custom_stopwords)



## create word freq list
master_word_freq <- master_fert_word %>%
  filter(!word %in% stopwords) %>% # remove stopwords
  filter(word %>% str_detect(pattern = "\\D+")) %>% # remove words consisting of digits
  count(word) %>%
  arrange(desc(n))


#we could also consider to just keep in certain words. and go from there!
#like we just select a bunch of keywords that we think are worth discussing and then we only run a word cloud with that?
master_word_freq %>%
  filter(n > 10) %>%
  filter(nchar(word) >= 2) %>% ## remove monosyllabic tokens
  wordcloud2(shape = "circle", size = 0.4)







#plot city vs city

#preprocessing (tokenization, customdict, stopwords, etc)





#plot party vs party
#preprocessing (tokenization, customdict, stopwords, etc)


#consider this https://rpubs.com/brandonkopp/creating-word-clouds-in-r



















