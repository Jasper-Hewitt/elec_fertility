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
search_pattern<-"少子化|生育率|生育|生孩子|孕育|懷孕|育兒|育嬰|新生兒|托嬰|公托|臨托|產檢" 


#add a column called fert_mention, if this post mentions one of our keywords we give it 1, if there is no mention we give it 0
master_df <- master_df %>%
  mutate(fert_mention = ifelse(grepl(search_pattern, Content), 1, 0))

#create a new  df with only the posts about fertility
master_fert_df <- master_df %>% 
  filter(fert_mention == 1)

#___________#__________### keyword search ####___________#__________# 

master_fert_df

#define keywords for each category (based on our research into government websites)
search_pattern_cat_1 <- "幼兒園|教保服務|托育資源|教保人員|居家式托育服務|機構式托育服務|家長親職|育兒知能|親職教育活動|托育|孕產婦關懷中心|
                         托育資源中心|居家式托育服務|兒童預防保健服務0-未滿 7 歲|收出養媒合及寄養安置|兒童三級預防措施|防治兒虐事件|公立幼兒園|
                         非營利幼兒園|孕產婦關懷中心|產前健康檢查|非營利幼兒園實施辦法|社區保母|托育人員|收托|送托|托嬰中心|公私協力托嬰中心|
                         托育安親管道|公托|補助托嬰兒|公幼|在校安親班|夜托|臨托|收托時間|托育人員|私幼|托育|親子中心|托育補助|托育家園|公共化托育
                         |公立托育中心|2-6歲（未滿）幼兒|公立幼兒園|非營利幼兒園|幼兒教育及照顧法|準公共機制|平價就學場域|公共化幼兒園|幼兒園入園率|
                         0-2歲（未滿）幼兒|公共托育|準公共機制|居家式托育|私立托嬰中心|平價名額"

search_pattern_cat_2 <-"安胎假|產檢|陪產假|產檢假|育嬰留職停薪|企業托育設施或措施|企業托育資訊|有薪產檢假|有薪陪產假|生理假|聯合托育|哺乳室"

search_pattern_cat_3 <- "妊娠生產醫療給付|各類保險生育給付|地方政府生育津貼|地方政府低收入戶生育補助|育嬰留職停薪津貼|就業者家庭部分托育費用補助|
                         特殊境遇家庭兒童托育津貼補助|未就業家庭育兒津貼特殊境遇家庭兒童托育津貼補助|幼兒學前特別扣除額|中低收入戶幼兒就學補助|5歲幼兒學費就學補助|
                         兒童與少年未來教育及發展帳戶|父母未就業家庭|托育費用補助|免學費|就學補助|社會住宅|凍卵|坐月子|產檢|0-2歲（未滿）幼兒托育補助|2-6歲（未滿）幼兒就學補助
                         |0-6歲育兒津貼|公共化機構|準公共化機構|公設民營托嬰中心|社區公共托育家園|每月托育補助|第一胎|第二胎|第三胎|不孕症|試管嬰兒|不孕夫妻"

#label posts according to keyword search    
master_fert_df <- master_fert_df %>%
  mutate(教保公共普及化 = ifelse(grepl(search_pattern_cat_1, Content), 1, 0))%>%
  mutate(友善職場 = ifelse(grepl(search_pattern_cat_2, Content), 1, 0))%>%
  mutate(經濟支持 = ifelse(grepl(search_pattern_cat_3, Content), 1, 0))

print(names(master_fert_df))

#___________#__________### plot all candidates ####___________#__________# 

# Calculate the sum for the relevant columns 
sums_per_cat <-colSums(master_fert_df[,c("教保公共普及化", "友善職場", "經濟支持")])

#create a long df for plotting in a stackplot
l_plot_df <- data.frame(name = names(sums_per_cat), value = sums_per_cat)

# Create the plot
ggplot(l_plot_df, aes(x = reorder(name, value), y = value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Categories", y = "Sum", title = "Sum for Each Category") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC"))


#___________#__________### plot party vs party ####___________#__________# 
  

#group by party 
by_party <- master_fert_df%>%
  group_by(Party)%>%
  summarise(教保公共普及化 = sum(教保公共普及化),
            友善職場 = sum(友善職場),
            經濟支持 = sum(經濟支持)
  )

l_by_party<-by_party%>%
  gather('label', 'sum', 2:4)



# set colors for party
color <- c("DPP" = "green", "KMT" = "blue", "TPP" = "red", "IND" = "gray")

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
  scale_fill_manual(values = c("教保公共普及化" = "purple", "友善職場" = "orange", "經濟支持" = "skyblue")) +
  theme_minimal() +
  labs(x = "Party", y = "Sum", fill = "Label") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC"))


#now let's look at it from a relative perspective. 
by_party <- master_fert_df %>%
  group_by(Party) %>%
  summarise(教保公共普及化 = sum(教保公共普及化),
            友善職場 = sum(友善職場),
            經濟支持 = sum(經濟支持),
            total_posts = n()  
  )

# Calculate the percentages. 
by_party <- by_party %>%
  mutate(教保公共普及化 = 教保公共普及化 / total_posts * 100,
         友善職場 = 友善職場 / total_posts * 100,
         經濟支持 = 經濟支持 / total_posts * 100)

l_by_party<-by_party%>%
  gather('label', 'percentage', 2:4)



# plot
#ggplot(l_by_party, aes(x = label, y = percentage, fill = Party)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  scale_fill_manual(values = color) +
#  theme_minimal() +
#  labs(x = "Party", y = "percentage", fill = "Party") +
#  theme_minimal() +
#  theme(text = element_text(family = "Songti SC"))

# plot, all parties grouped together
ggplot(l_by_party, aes(x = Party, y = percentage, fill = label)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("教保公共普及化" = "purple", "友善職場" = "orange", "經濟支持" = "skyblue")) +
  theme_minimal() +
  labs(x = "Party", y = "Percentage", fill = "Label") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC"))

#___________#__________### plot city vs city ####___________#__________# 


#group by party 
by_city <- master_fert_df%>%
  group_by(City)%>%
  summarise(教保公共普及化 = sum(教保公共普及化),
            友善職場 = sum(友善職場),
            經濟支持 = sum(經濟支持)
  )

l_by_city<-by_city%>%
  gather('label', 'sum', 2:4)


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
  scale_fill_manual(values = c("教保公共普及化" = "purple", "友善職場" = "orange", "經濟支持" = "skyblue")) +
  theme_minimal() +
  labs(x = "City", y = "Sum", fill = "Label") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC"))


















