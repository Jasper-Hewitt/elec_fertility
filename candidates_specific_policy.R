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




#___________#__________### PREPROCESSING ####___________#__________# 

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

#we can adjust these based on policies that we find. I took out 孕， maybe we should consider taking out 孕育 too
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
  filter(fert_mention == 1)

#___________#__________### keyword search ####___________#__________# 

master_fert_df

#define keywords for each category (based on our research into government websites)
search_pattern_workplace <- "安胎假|陪產假|產檢假|育嬰留職停薪|生理假|企業托育|聯合托育|企業哺乳室"

search_pattern_financial_aid <-"生育獎勵|嬰兒補助|育兒津貼|育兒補助|生養津貼|生養補助|生育津貼|生育補助|育嬰留職停薪津貼|就學補助|就寫津貼｜幼兒學前特別扣除額|免學費|育嬰留職停薪津貼|未來教育及發展帳戶"

search_pattern_cat_childcare <- "公托|公幼|托育|公共托育|托育資源|居家式托育服務|機構式托育服務|收托|送托|托嬰|夜托|臨托|教保服務|教保人員|社區保母|在校安親班|準公共機制|平價就學場域
                                |平價名額|兒童預防保健服務0-未滿 7 歲|收出養媒合|養安置|兒童三級預防措施|防治兒虐事件"

search_pattern_cat_infertility <- "懷孕以及孕婦相關|不孕症|試管嬰兒|不孕夫|凍卵|坐月子|產檢|孕產婦關懷中心|產前健康檢查"


#label posts according to keyword search    
master_fert_df <- master_fert_df %>%
  mutate(Workplace = ifelse(grepl(search_pattern_workplace, Content), 1, 0))%>%
  mutate(Financial_aid = ifelse(grepl(search_pattern_financial_aid, Content), 1, 0))%>%
  mutate(Childcare = ifelse(grepl(search_pattern_cat_childcare, Content), 1, 0))%>%
  mutate(Infertility=ifelse(grepl(search_pattern_cat_infertility, Content), 1, 0))

print(names(master_fert_df))

#___________#__________### plot all candidates ####___________#__________# 

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
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Workplace" = "purple", "Financial_aid" = "orange", "Childcare" = "skyblue", "Infertility" = "green")) +
  theme_minimal() +
  labs(title = "All candidates", x = "Label", y = "Sum", fill = "Label") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC", size = 20))


#___________#__________### plot party vs party ####___________#__________# 
  

#group by party 
by_party <- master_fert_df%>%
  group_by(Party)%>%
  summarise(Workplace = sum(Workplace),
            Financial_aid = sum(Financial_aid),
            Childcare = sum(Childcare),
            Infertility=sum(Infertility)
  )

l_by_party<-by_party%>%
  gather('label', 'sum', 2:5)




# plot
#ggplot(l_by_party, aes(x = label, y = sum, fill = Party)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  scale_fill_manual(values = color) +
#  theme_minimal() +
#  labs(x = "Party", y = "Sum", fill = "Party") +
#  theme_minimal() +
#  theme(text = element_text(family = "Songti SC"))

# plot, all parties grouped together
ggplot(l_by_party, aes(x = Party, y = sum, fill = label)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Workplace" = "purple", "Financial_aid" = "orange", "Childcare" = "skyblue", "Infertility" = "green")) +
  theme_minimal() +
  labs(title = 'Party vs Party', x = "Party", y = "Sum", fill = "Label") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC", size=20))


#now let's look at it from a relative perspective. 
#by_party <- master_fert_df %>%
#  group_by(Party) %>%
#  summarise(教保公共普及化 = sum(教保公共普及化),
#            友善職場 = sum(友善職場),
#            經濟支持 = sum(經濟支持),
#            total_posts = n()  
#  )

# Calculate the percentages. 
#by_party <- by_party %>%
#  mutate(教保公共普及化 = 教保公共普及化 / total_posts * 100,
#         友善職場 = 友善職場 / total_posts * 100,
#         經濟支持 = 經濟支持 / total_posts * 100)

#l_by_party<-by_party%>%
#  gather('label', 'percentage', 2:4)



# plot
#ggplot(l_by_party, aes(x = label, y = percentage, fill = Party)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  scale_fill_manual(values = color) +
#  theme_minimal() +
#  labs(x = "Party", y = "percentage", fill = "Party") +
#  theme_minimal() +
#  theme(text = element_text(family = "Songti SC"))

# plot, all parties grouped together
#ggplot(l_by_party, aes(x = Party, y = percentage, fill = label)) + 
#  geom_bar(stat = "identity", position = "dodge") +
#  scale_fill_manual(values = c("教保公共普及化" = "purple", "友善職場" = "orange", "經濟支持" = "skyblue")) +
#  theme_minimal() +
#  labs(x = "Party", y = "Percentage", fill = "Label") +
#  theme_minimal() +
#  theme(text = element_text(family = "Songti SC"))

#___________#__________### plot city vs city ####___________#__________# 


#group by city
by_city <- master_fert_df%>%
  group_by(City)%>%
  summarise(Workplace = sum(Workplace),
            Financial_aid = sum(Financial_aid),
            Childcare = sum(Childcare),
            Infertility=sum(Infertility)
  )

l_by_city<-by_city%>%
  gather('label', 'sum', 2:5)


# plot
#ggplot(l_by_party, aes(x = label, y = sum, fill = Party)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  scale_fill_manual(values = color) +
#  theme_minimal() +
#  labs(x = "Party", y = "Sum", fill = "Party") +
#  theme_minimal() +
#  theme(text = element_text(family = "Songti SC"))

# plot, all cities grouped together
ggplot(l_by_city, aes(x = City, y = sum, fill = label)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Workplace" = "purple", "Financial_aid" = "orange", "Childcare" = "skyblue", "Infertility" = "green")) +
  theme_minimal() +
  labs(title='City vs City', x = "City", y = "Sum", fill = "Label") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC", size=20))


#___________#__________### plot candidate vs candidate ###___________#__________# 


#group by party 
by_candidate <- master_fert_df%>%
  group_by(Candidate)%>%
  summarise(Workplace = sum(Workplace),
            Financial_aid = sum(Financial_aid),
            Childcare = sum(Childcare),
            Infertility=sum(Infertility)
  )

l_by_candidate<-by_candidate%>%
  gather('label', 'sum', 2:5)

# plot, all candidates grouped together
ggplot(l_by_candidate, aes(x = Candidate, y = sum, fill = label)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Workplace" = "purple", "Financial_aid" = "orange", "Childcare" = "skyblue", "Infertility" = "green")) +
  coord_flip() +
  theme_minimal() +
  labs(title="Candidate vs Candidate", x = "Candidate", y = "Sum", fill = "Label") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC", size=20))










