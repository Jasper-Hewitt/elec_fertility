#creating master csv
library(tidyverse)
library(dplyr)
library(tidytext)
library(stringr)
library(readtext)
library(lubridate)
library(purrr)
library(readxl)
library(ggplot2)
library(quanteda)
library(jiebaR)
library(readtext)
library(pdftools)
library(lubridate)
library(purrr)
library(wordcloud2)
setwd("/Users/jasperhewitt/Desktop/fertnews")



#___________#__________### 1: CREATING MASTER FILE ####___________#__________# 

#import full crowdtangle csv 8073 rows
master_df <- read.csv("/Users/jasperhewitt/Desktop/fertnews/FB_All_Candidates.csv")

#import xlsx with additional data for each candidate (party, city, english name, etc.)
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

#filter out the posts before each candidate formally started running 8073 -> 5939 
master_df <- master_df%>%
  filter(Post.Created.Date >= Date_Started_Running)

#add campaign duration date with difftime. We use floor so that it does not give you fractions of a date
master_df$Campaign_duration <- floor(as.numeric(difftime(as.Date("2022-11-27"), master_df$Date_Started_Running, units = "days")))

#___________#  林智堅 SPECIAL TREATMENT #__________# 

#林智堅 was replaced so we have to delete his posts after 2022-08-17     5939 -> 5825 
#create a new DataFrame with only the posts from '林智堅' 
temp_df <- master_df %>%
  filter(Candidate == '林智堅') %>%
  #delete posts after 2022-08-17
  filter(Post.Created.Date < as.Date("2022-08-17"))%>%
  #get campaign duration for 林智堅 directly. 
  mutate(Campaign_duration = floor(as.numeric(difftime(as.Date("2022-08-17"), Date_Started_Running, units = "days"))))

#Merge back together

#remove all 林智堅 posts from the master_df
master_df <- master_df %>%
  filter(Candidate != '林智堅')
  
# Now merge (bind rows) the candidate_df back to master_df 
master_df <- rbind(master_df, temp_df)

#___________#  林智堅 SPECIAL TREATMENT (END) #__________# 

#check number of posts per candidate
count_df <- master_df %>%
  group_by(Candidate) %>%
  summarise(Count = n())

class(master_df$Date_Started_Running)


#write to a csv file
#write.csv(master_df, "master_candidates_posts.csv", row.names = FALSE)

#___________#__________### 2: PREPROCESSING AND CLEANING ####___________#__________# 

master_df <- master_df %>%
  #put content from the relevant columns together into a new column
  mutate(Content = paste(Message, Image.Text, Description, sep = " "))%>%
  #the previous step creates many 'NAs' (because not every column contains data), delete these "NA"s. This is not the same as na.omit
  mutate(Content = str_replace_all(Content, "NA", "")) %>%
  #delete empty rows, these rows are empty because they only contained videos or photos without text. 
  #so we delete all the rows less than 3 characters, 5825 -> 5802
  filter(nchar(as.character(Content))>=3) 



#___________#__________### 3: number of fertility posts average daily fertility posts ####___________#__________# 

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

#create a new  df with only the posts about fertility 5802 -> 282
master_fert_df <- master_df %>% 
  filter(fert_mention == 1) #282

#plot number of posts per candidate
#add chinese name and englihs name together for in the plot
master_fert_df$Candidate <- paste(master_fert_df$Candidate, master_fert_df$Candidate_eng, sep = " ")

#group by candidate 
by_candidate <- master_fert_df%>%
  group_by(Candidate)%>%
  summarise(fert_posts=n())

#plot in a flipped barplot 
ggplot(by_candidate, aes(x = reorder(Candidate, fert_posts), y = fert_posts)) +  # arrange from highest total to lowest 
  geom_bar(stat = "identity", fill = 'lightblue') +
  coord_flip() +
  theme_minimal() +
  labs(title="total number of posts about fertility related issues per candidate", x = "Candidate", y = "Posts") +
  theme(text = element_text(family = "Songti SC", size=16))


#average daily posts

#bind total number of fertility posts per candidate to master_fert_df
master_fert_df<-master_fert_df%>%
  left_join(by_candidate, by='Candidate')


#average number of posts about fertility related issues relative to the length of the campaign
#if a candidate ran for 91 days (黃珊珊) and has 19 posts that mention fertility, that means they had 91/19= 0.21 
by_candidate <- master_fert_df %>%
  group_by(Candidate) %>%
  mutate(avg_daily_fert_post=fert_posts / Campaign_duration)%>%
  summarise(avg_daily_fert_post = mean(avg_daily_fert_post, na.rm = TRUE))

# plot in a flipped barplot 
ggplot(by_candidate, aes(x = reorder(Candidate, avg_daily_fert_post), y = avg_daily_fert_post)) +  
  geom_bar(stat = "identity", fill = 'lightblue') +
  coord_flip() +
  theme_minimal() +
  labs(title="average daily number of posts about fertility \n (relative to campaign length)", x = "Candidate", y = "Average daily posts about fertility") +
  theme(text = element_text(family = "Songti SC", size=16))


#___________#__________### 4: NUMBER OF POSTS OVERTIME (OF TOP 5 CANDIDATES)? ####___________#__________#


#get counts per date
master_fert_df_timeplot <- master_fert_df %>%
  group_by(Post.Created.Date) %>%
  summarise(count = n())

#plot with lineplot
ggplot(master_fert_df_timeplot, aes(x=Post.Created.Date, y=count)) + 
  geom_line(colour = "orange") +
  labs(title = "Daily Facebook posts by all candidates about population ageing", x = "Date", y = "Posts") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + #set date per month instead of day
  scale_y_continuous(breaks = seq(2, 8, by = 2)) + #set y axis breaks manually
  theme_minimal() + 
  theme(plot.title = element_text(size = 20, face = "bold", color = "darkblue"))





#___________#__________### 5: FERTILITY RELATIVE TO OTHER TOPICS (ELDERCARE, PUBLIC TRANSPORT, ECONOMY, TRANSPORT )####___________#__________# 

#get other categories, we already have the number of posts about fertility, so we only need to get the other 4 categories
keywords_Eldercare <- c("銀髮族", "長照", "在地安佬", "在地老化", "不老城", "重陽敬老金", "老人健保", "無障礙公車", "日照", "銀髮樂活", "敬老卡", "高齡運動")
keywords_Public_Housing <- c("居住正義", "社會住宅", "社宅", "租屋", "青銀共居", "捷運社宅", "台中好宅", "老屋翻新", "包租代管", "囤房特別稅", "社宅")
keywords_Transport  <-c("鐵路", "鐵路地下化", "Youbike", "YouBike", "捷運", "腳踏車", "自行車", "公車", "車禍", "幸福里程", "通勤", "大眾運輸", "步行", "國道", "客運轉運站", "公車", "交通", "運輸套票", "智慧交通", "機車路權", "軌道捷運", "iBike", "幹道", "輕軌", "台中大環", "閘道", "不塞車", "Tbike", "人行道", "電動公車", "自行車質量", "聯外道路", "人行安全", "行人安全", "人性化路牌", "智慧街道", "四橫三路", "道路壅塞", "外環道路", "鐵路立體化", "行人地獄")
keywords_Economy<- c("智慧城市", "製造業", "產業", "物流", "金融", "科技", "引擎", "醫材", "工業4.0", "數位升級", "智慧升級", "創業", "數位新科技", "在地工作", "產業帶", "轉型", "新創轉型", "都心發展", "機能區", "藍色經濟圈", "海洋經濟", "海空雙港", "低碳產業", "產業高值化", "觀光", "旅客", "旅行社", "商圈", "招商引資", "拼經濟", "產業園區", "中科2.0", "新苗計劃", "創業孵化器", "航空城", "產學訓用", "青創基地", "高科技產業", "優質就業", "國際廠商", "高階製造中心")


#Tag as 1 if any of the words appaer in the messages, otherwise 0.
master_df$Eldercare <- ifelse(grepl(paste(keywords_Eldercare, collapse = "|"), master_df$Content), 1, 0)
master_df$Public_Housing <- ifelse(grepl(paste(keywords_Public_Housing, collapse = "|"), master_df$Content), 1, 0)
master_df$Transport <- ifelse(grepl(paste(keywords_Transport, collapse = "|"), master_df$Content), 1, 0)
master_df$Economy <- ifelse(grepl(paste(keywords_Economy, collapse = "|"), master_df$Content), 1, 0)

#get the sum for all of the mentions
relative_importance <- master_df%>%
  select('Eldercare', 'Public_Housing', 'Transport', 'Economy', 'fert_mention')%>%
  rename(Fertility=fert_mention)%>%
  summarise(
    Eldercare = sum(Eldercare, na.rm = TRUE),
    Public_Housing = sum(Public_Housing, na.rm = TRUE),
    Transport = sum(Transport, na.rm = TRUE),
    Economy = sum(Economy, na.rm = TRUE),
    Fertility = sum(Fertility, na.rm = TRUE)
  )

#put in long format to plot with ggplot
l_relative_importance <- relative_importance%>%
  gather('topic', 'mentions', 1:5)


# plot in a flipped barplot 
ggplot(l_relative_importance, aes(x = reorder(topic, mentions), y = mentions)) +  
  geom_bar(stat = "identity", fill = 'lightblue') +
  coord_flip() +
  theme_minimal() +
  labs(title="all candidates: number of mentions per topic", x = "Topic", y = "number of times they mentioned a specific topic") +
  theme(text = element_text(family = "Songti SC", size=16))


#___________#__________### 6:  MENTIONS OF SPECIFIC POLICY DIRECTIONS ####___________#__________# 
#in this section we will look at what kind of policy directions the candidates focussed most on.
#the charts below represent the number of mentions, not the number of posts. If a candidate mentions three different policy
#directions within a single post, we will count the three specific policy directions that were mentioned
#the main part of this section is to figure out how certain cities/parties mostly attempt to solve this problem.


#___________# KEYWORD SEARCH #__________# 

#define keywords for each category (based on our research into government websites, candidates' leaflets, and other sources)
search_pattern_workplace <- "安胎假|陪產假|產檢假|育嬰留職停薪|生理假|企業托育|聯合托育|企業哺乳室"

search_pattern_financial_aid <-"生育獎勵|嬰兒補助|育兒津貼|育兒補助|生養津貼|生養補助|生育津貼|生育補助|育嬰留職停薪津貼
                                |就學補助|就寫津貼｜幼兒學前特別扣除額|免學費|育嬰留職停薪津貼|未來教育及發展帳戶"

search_pattern_cat_childcare <- "公托|公幼|托育|收托|送托|托嬰|夜托|臨托|教保服務
                                 |教保人員|社區保母|在校安親班|準公共機制|平價就學場域|平價名額|兒童預防保健服務0-未滿 7 歲|收出養媒合|養安置
                                 |兒童三級預防措施|防治兒虐事件"

search_pattern_cat_infertility <- "懷孕以及孕婦相關|不孕症|試管嬰兒|不孕夫|凍卵|坐月子|產檢|孕產婦關懷中心|產前健康檢查"


#label posts according to the results of the keyword search    
master_fert_df <- master_fert_df %>%
  mutate(Workplace = ifelse(grepl(search_pattern_workplace, Content), 1, 0))%>%
  mutate(Financial_aid = ifelse(grepl(search_pattern_financial_aid, Content), 1, 0))%>%
  mutate(Childcare = ifelse(grepl(search_pattern_cat_childcare, Content), 1, 0))%>%
  mutate(Infertility=ifelse(grepl(search_pattern_cat_infertility, Content), 1, 0))


 
#___________# PLOT ALL CANDIDATES COMBINED #__________# 

# Calculate the sum of the variables
sum_df <- master_fert_df %>%
  summarise(Workplace = sum(Workplace),
            Financial_aid = sum(Financial_aid),
            Childcare = sum(Childcare),
            Infertility = sum(Infertility))

# Reshape the data into a long format
l_df <- sum_df %>%
  gather('label', 'sum', 1:4)

# Plot
ggplot(l_df, aes(x = label, y = sum, fill = label)) + 
  geom_bar(stat = "identity", position = "dodge", fill="lightblue") +
#  scale_fill_manual(values = c("Workplace" = "purple", "Financial_aid" = "orange", "Childcare" = "skyblue", "Infertility" = "green")) +
  theme_minimal() +
  labs(title = "specific fertility policy directions mentioned \n by all candidates", x = "policy directions", y = "mentions", fill = "Label") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC", size = 20))


#___________# PLOT PARTY VS PARTY#__________# 

#group by party 
by_party <- master_fert_df%>%
  group_by(Party)%>%
  summarise(Workplace = sum(Workplace),
            Financial_aid = sum(Financial_aid),
            Childcare = sum(Childcare),
            Infertility=sum(Infertility)
  )
by_party$Total <- rowSums(by_party[2:5])

l_by_party<-by_party%>%
  gather('label', 'sum', 2:5)

# Plot
ggplot(l_by_party, aes(x = reorder(Party, Total), y = sum, fill = label)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Workplace" = "darkgreen", "Financial_aid" = "orange", "Childcare" = "blue", "Infertility" = "lightblue")) +
  scale_x_discrete(labels = c("民眾黨" = "TPP", "無黨籍" = "IND", "國民黨" = "KMT", "民進黨" = "DPP")) +
  theme_minimal() +
  labs(title = 'specific fertility policy directions mentioned \n per party', x = "policy directions", y = "mentions", fill = "Label") +
  theme(text = element_text(family = "Songti SC", size=20))



#___________#PLOT CITY VS CITY #__________# 

#put Chinese and english name for each city together
master_fert_df <- master_fert_df %>%
  mutate(City = paste(City, City_eng, sep = "\n"))

#group by city
by_city <- master_fert_df%>%
  group_by(City)%>%
  summarise(Workplace = sum(Workplace),
            Financial_aid = sum(Financial_aid),
            Childcare = sum(Childcare),
            Infertility=sum(Infertility)
  )

# Create a column with the total mentions for each candidate
by_city$Total <- rowSums(by_city[2:5])


l_by_city<-by_city%>%
  gather('label', 'sum', 2:5)

# plot, city vs city, from least total mentions (left), to most total mentions (right)
#ggplot(l_by_city, aes(x = City, y = sum, fill = label)) +
ggplot(l_by_city, aes(x = reorder(City, Total), y = sum, fill = label)) + #reorder based on total count 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Workplace" = "darkgreen", "Financial_aid" = "orange", "Childcare" = "blue", "Infertility" = "lightblue")) +
  theme_minimal() +
  labs(title='specific fertility policy directions mentioned \n per city', x = "policy directions", y = "mentions", fill = "Label") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC", size=20))


#ALTERNATIVE (STACKPLOT) CITY BY CITY
# plot, city vs city in ordered stack plot
#ggplot(l_by_city, aes(x = reorder(City, Total), y = sum, fill = label)) +  # sort from high to low
#  geom_bar(stat = "identity", position = "stack") +
#  scale_fill_manual(values = c("Workplace" = "darkgreen", "Financial_aid" = "orange", "Childcare" = "blue", "Infertility" = "lightblue")) +
#  coord_flip() +
#  theme_minimal() +
#  labs(title='City vs City', x = "City", y = "Sum", fill = "Label") +
#  theme(text = element_text(family = "Songti SC", size=20))


#___________# (OPTIONAL) PLOT CANDIDATE VS CANDIDATE #__________# 

#candidate vs candidate old version
#group by candidate
by_candidate <- master_fert_df%>%
  group_by(Candidate)%>%
  summarise(Workplace = sum(Workplace),
            Financial_aid = sum(Financial_aid),
            Childcare = sum(Childcare),
            Infertility=sum(Infertility)
  )

# Create a column with the total mentions for each candidate
by_candidate$Total <- rowSums(by_candidate[2:5])

l_by_candidate<-by_candidate%>%
  gather('label', 'sum', 2:5)

# plot, all candidates
ggplot(l_by_candidate, aes(x = reorder(Candidate, Total), y = sum, fill = label)) +  #arrange from highest total to lowest toatl
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Workplace" = "darkgreen", "Financial_aid" = "orange", "Childcare" = "blue", "Infertility" = "lightblue")) +
  coord_flip() +
  theme_minimal() +
  labs(title="specific fertility policy directions mentioned \n per candidate", x = "candidate", y = "mentions", fill = "Label") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC", size=20))

# (OPTIONAL) STACKPLOT
#ggplot(l_by_candidate, aes(x = reorder(Candidate, Total), y = sum, fill = label)) + 
#  geom_bar(stat = "identity", position = "stack") + 
#  scale_fill_manual(values = c("Workplace" = "darkgreen", "Financial_aid" = "orange", "Childcare" = "blue", "Infertility" = "lightblue")) +
#  coord_flip() + 
#  theme_minimal() +
#  labs(title="Candidate vs Candidate", x = "Candidate", y = "Sum", fill = "Label") +
#  theme(text = element_text(family = "Songti SC", size=20))


#___________#__________### 7:  WORDCLOUD FOR CANDIDATES FERTILITY POSTS ####___________#__________# 


#___________# TOKENIZATION, STOPWORDS, CUSTOM DICTIONARY #__________# 

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
                      "少子化", "/", "10", "20", "30", "一起", "桃園", "台中", "市長", "市民", "朋友",
                      "城市", "12", "11", "高雄", "https", "台北市", "台中市", "台南市", "高雄市", "台南")  # specific words about 生育率# specific words about 生育率
stopwords <- c(stopwords, custom_stopwords)

#___________# WORDCLOUD AND WORD FREQUENCY LIST #__________# 

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


# Select 20 most frequent words
top_20_words <- master_word_freq %>%
  top_n(20, n)

# plot sideways word frequency list
ggplot(top_20_words, aes(x = reorder(word, n), y = n)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Words", y = "Count", title = "20 most frequent words in candidates' \n posts about fertility") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC", size = 20))
