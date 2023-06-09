setwd("/Users/jasperhewitt/Desktop/wisenews")
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

master_df <- read.csv("https://github.com/Jasper-Hewitt/elec_wiseility/raw/main/data/master.csv")




#___________#__________### PREPROCESSING ####___________#__________# 

#PART 1: DATA CLEANING 

#convert to txt with pdftools package
text <- pdf_text("wisenews_wiseility.pdf")

# put everything into one single string. 
text_combined <- paste(text, collapse = "\n")

# Split text on '文章編號:' because this indicates the end of an article
split_text <- strsplit(text_combined, split = "文章編號:")


# Convert the list to a data frame
wise_df <- data.frame(dirty_content = split_text[[1]], stringsAsFactors = FALSE)

# for now, put the same content into the dirty_date column.
#we will start writing separate regexes for both columns later. 
wise_df$dirty_date <- wise_df$dirty_content


#drop the last row, bc this only contains unimportant information that came after the last article
wise_df <- wise_df[-nrow(wise_df), ]

#regex for date column: our goal is to only get the publishing date of the article. Nothing else
# Define the pattern. everything from '|' until '網站' (because that's where the date is in between)
pattern <- "\\|.*\\n網站"
wise_df$date <- str_extract(wise_df$dirty_date, pattern)

# delete additional characters from the date column 
wise_df$date <- str_replace_all(wise_df$date, "\\| ", "")
wise_df$date <- str_replace_all(wise_df$date, "\n網站", "")
# Convert the date column to a format that R can read with lubridate 
wise_df$date <- ymd_hm(wise_df$date)

# clean content column 
#delete everything before '文字快照'. 
wise_df$content <- sub(".*文字快照", "", wise_df$dirty_content)

#Since all of the rows now start with a link, and are then followed by a news article in Chinese
#the following code uses the stringi package to delete all the non-han characters at the start of each row. This effectively gets
#rid of all of the links
wise_df$content <- stringi::stri_replace_all_regex(wise_df$content, '^[^\\p{Han}]*', "")

# delete the compressed posts. these are the posts that end with . . . AND have fewer than 200 characters.
#some posts have . . . but are not actually compressed, so we have to leave those in. 
wise_df <- wise_df[!(str_detect(wise_df$content, "\\.\\.\\.$") & nchar(wise_df$content) < 200), ]

#delete newline symbols
wise_df$content <- gsub("\n", "", wise_df$content)



#delete the column we no longer need
wise_df$dirty_content <- NULL
wise_df$dirty_date <- NULL


print(wise_df$content[1])

#OPTIONAL, get important_sentences to plot just those

#search_pattern <- "少子化|生育率|生育|生孩子|孕育|懷孕|育兒|育嬰|新生兒|托嬰|公托|臨托|產檢|孕"

#wise_df <- wise_df %>%
#  mutate(important_sentences = str_split(content, "[。？！]")) %>% #split it into sentences
#find the sentences that contain some of our keywords, and paste that sentences into the important_sentences column
#  mutate(important_sentences = map(important_sentences, ~ .[str_detect(., regex(search_pattern, ignore_case = TRUE))])) %>%
#unnest the columns, so each important sentence will get their own row. just like 'explode in pandas' 
#  unnest(important_sentences)


#OPTIONAL prepare dataframe for later plots
wise_df <- wise_df %>% 
  filter(str_length(content) >= 3)%>% #remove empty docs, docs with less than 3 characters
  mutate(doc_id = row_number())

master_wise_df<-wise_df

master_wise_df<-master_wise_df%>%
  rename(Content=content)


#___________#__________### keyword search ####___________#__________# 

master_wise_df

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
master_wise_df <- master_wise_df %>%
  mutate(教保公共普及化 = ifelse(grepl(search_pattern_cat_1, Content), 1, 0))%>%
  mutate(友善職場 = ifelse(grepl(search_pattern_cat_2, Content), 1, 0))%>%
  mutate(經濟支持 = ifelse(grepl(search_pattern_cat_3, Content), 1, 0))

print(names(master_wise_df))

# Calculate the sum for the relevant columns 
sums_per_cat <-colSums(master_wise_df[,c("教保公共普及化", "友善職場", "經濟支持")])

#create a long df for plotting in a stackplot
l_plot_df <- data.frame(name = names(sums_per_cat), value = sums_per_cat)

# Create the plot
ggplot(l_plot_df, aes(x = reorder(name, value), y = value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Categories", y = "Sum", title = "policy directions mentioned in the media") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC"))


