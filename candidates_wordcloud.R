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

#___________#__________### PREPROCESSING AND CLEANING ####___________#__________# 

#get relevant columns 
names(master_df)

master_df <- master_df %>%
  #put content from the relevant columns together into a new column
  mutate(Content = paste(Message, Image.Text, Description, sep = " "))%>%
  #the previous step creates many NAs (because not every column contains data), delete these "NA"s. This is not the same as na.omit
  mutate(Content = str_replace_all(Content, "NA", "")) %>%
  rename("Candidate" = "Page.Name")%>%
  #only keep the relevant columns
  select('Candidate', 'City', 'Party', 'Content', 'Post.Created')%>%
  #delete empty rows, these rows are empty because they only contained videos or photos without text. 
  #so we delete all the rows less than 3 characters, before (5998 obs), after (5976)
  filter(nchar(as.character(Content))>=3) 

#find posts related to fertility based on all the keywords we have identified in our research
search_pattern<-"少子化|生育率|生育|生孩子|懷孕|育兒|育嬰|新生兒|托嬰|公托|臨托
                |產檢安胎假|陪產假|產檢假|育嬰留職停薪|生理假|企業托育|聯合托育|企業哺乳室|
                生育獎勵|嬰兒補助|育兒津貼|育兒補助|生養津貼|生養補助|生育津貼|生育補助
                |育嬰留職停薪津貼|就學補助|就寫津貼｜幼兒學前特別扣除額|免學費|育嬰留職停薪津貼
                |未來教育及發展帳戶|公托|公幼|托育|公共托育|托育資源|居家式托育服務|機構式托育服務|收托|送托
                |托嬰|夜托|臨托|教保服務|教保人員|社區保母|在校安親班|準公共機制|平價就學場域
                |平價名額|兒童預防保健服務0-未滿 7 歲|收出養媒合|養安置|兒童三級預防措施|防治兒虐事件|
                懷孕以及孕婦相關|不孕症|試管嬰兒|不孕夫|凍卵|坐月子|產檢|孕產婦關懷中心|產前健康檢查" 


#add a column called fert_mention, if this post mentions one of our keywords we give it 1, if there is no mention we give it 0
master_df <- master_df %>%
  mutate(fert_mention = ifelse(grepl(search_pattern, Content), 1, 0))

#create a new  df with only the posts about fertility
master_fert_df <- master_df %>% 
  filter(fert_mention == 1) #284


#___________#__________### TOKENIZATION, STOPWORDS, CUSTOM DICTIONARY ####___________#__________# 

#this part is based on this tutorial by NTNU: https://alvinntnu.github.io/NTNU_ENC2036_LECTURES/chinese-text-processing.html

#custom dict and stopwords: https://github.com/Jasper-Hewitt/elec_fertility/tree/main/data/dict_and_stopwords

#add doc_id
master_fert_df <- master_fert_df %>% 
  mutate(doc_id = row_number())

#load stopwords 
stopwords <- readLines("stopwords_zh_trad.txt",
                       encoding = "UTF-8")


#load custom dictionary 
my_seg <- worker(bylines = T,
                 user = "customdict.txt",
                 symbol = T)


## word tokenization
master_fert_word <- master_fert_df %>%
  unnest_tokens(
    output = word,
    input = Content,  # the name of the column we are plotting
    token = function(x)
      segment(x, jiebar = my_seg)
  ) %>%
  group_by(doc_id) %>%
  mutate(word_id = row_number()) %>% # create word index within each document
  ungroup



#same custom stopwords as we use for the wisenews wordcloud
custom_stopwords <- c("經濟", "科技", "報導", "可能", "指出", "認為", "新聞網", "國際", 
                      "應該", "可能", "提出", "過去", "現在", "進行","今天", "相關", "社會",
                      "議題", "很多", "undo", "需要", "需求", "已經", "目前", "今年", "透過",
                      "地方", "沒有", "記者", "成為", "持續", "市場", "表示", "台灣", "造成",
                      "不少", "原因", "影響", "人口","台北", "生育率", "問題", "育兒", "生育", 
                      "少子化", "/", "10", "20", "30", "一起", "桃園", "台中", "市長", "市民",
                      "城市", "12", "11", "高雄", "https", "台北市", "台中市", "台南市", "高雄市", "台南")  # specific words about 生育率# specific words about 生育率
stopwords <- c(stopwords, custom_stopwords)

#___________#__________### WORDCLOUD AND WORD FREQUENCY LIST ####___________#__________# 

## create word freq list
master_word_freq <- master_fert_word %>%
  mutate(word = str_trim(word)) %>%  # remove whitespaces
  filter(str_length(word) > 1, !word %in% stopwords) %>% # remove single character words and stopwords 
  count(word) %>%
  arrange(desc(n))


#plot wordcloud
master_word_freq %>%
  filter(n > 50) %>%
  filter(nchar(word) >= 2) %>% ## remove one character words because they are usually not really relevant
  wordcloud2(shape = "circle", size = 0.4)


# Select 30 most frequent words
top_30_words <- master_word_freq %>%
  top_n(30, n)

# plot sideways word frequency list
ggplot(top_30_words, aes(x = reorder(word, n), y = n)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Words", y = "Count", title = "30 Most Frequent Words") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC"))

