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


#___________#__________### PREPROCESSING AND CLEANING ####___________#__________# 

#PART 1: DATA CLEANING 

#convert to txt with pdftools package
text <- pdf_text("wisenews_fertility.pdf")

# put everything into one single string
text_combined <- paste(text, collapse = "\n")

# Split text on '文章編號:' because this indicates the end of an article
split_text <- strsplit(text_combined, split = "文章編號:")

# Convert the list to a data frame
wise_df <- data.frame(dirty_content = split_text[[1]], stringsAsFactors = FALSE)

# create date column and fill it with the same content as the content column 
wise_df$dirty_date <- wise_df$dirty_content

#drop the last row, bc this only contains unimportant information that came after the last article
wise_df <- wise_df[-nrow(wise_df), ]

#regex for date column: our goal is to only get the publishing date of the article, nothing else
# Define the pattern. everything from '|' until '網站' (because that's where the date is in between)
pattern <- "\\|.*\\n網站"
wise_df$date <- str_extract(wise_df$dirty_date, pattern)

# delete additional characters from the date column (the characters surrounding the date)
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

#delete the column we no longer need
wise_df$dirty_content <- NULL
wise_df$dirty_date <- NULL

# delete the compressed posts. these are the posts that end with . . . AND have fewer than 200 characters.
#some posts have . . . but are not actually compressed, so we have to leave those in. 
#first we trim the data
wise_df$content <- trimws(wise_df$content)
wise_df <- wise_df[!(str_detect(wise_df$content, "\\.\\.\\.$") & nchar(wise_df$content) < 200), ] #4197 -> 4154

#also delete the empty or almost empty rows (some pages in the pdf only provided links to news articles)
wise_df <- subset(wise_df, nchar(content) >= 10) #4154 -> 3917

#delete newline symbols
wise_df$content <- gsub("\n", "", wise_df$content)

#change column name for later code
master_wise_df<-master_wise_df%>%
  rename(Content=content)


#___________#__________### keyword search ####___________#__________# 

master_wise_df

#define keywords for each category (based on our research into government websites)
search_pattern_workplace <- "安胎假|陪產假|產檢假|育嬰留職停薪|生理假|企業托育|聯合托育|企業哺乳室"

search_pattern_financial_aid <-"生育獎勵|嬰兒補助|育兒津貼|育兒補助|生養津貼|生養補助|生育津貼|生育補助|育嬰留職停薪津貼|就學補助|就寫津貼｜幼兒學前特別扣除額|免學費|育嬰留職停薪津貼|未來教育及發展帳戶"

search_pattern_cat_childcare <- "公托|公幼|托育|公共托育|托育資源|居家式托育服務|機構式托育服務|收托|送托|托嬰|夜托|臨托|教保服務|教保人員|社區保母|在校安親班|準公共機制|平價就學場域
                                |平價名額|兒童預防保健服務0-未滿 7 歲|收出養媒合|養安置|兒童三級預防措施|防治兒虐事件"

search_pattern_cat_infertility <- "懷孕以及孕婦相關|不孕症|試管嬰兒|不孕夫|凍卵|坐月子|產檢|孕產婦關懷中心|產前健康檢查"


#label posts according to keyword search    
master_wise_df <- master_wise_df %>%
  mutate(Workplace = ifelse(grepl(search_pattern_workplace, Content), 1, 0))%>%
  mutate(Financial_aid = ifelse(grepl(search_pattern_financial_aid, Content), 1, 0))%>%
  mutate(Childcare = ifelse(grepl(search_pattern_cat_childcare, Content), 1, 0))%>%
  mutate(Infertility=ifelse(grepl(search_pattern_cat_infertility, Content), 1, 0))

print(names(master_wise_df))

# Calculate the sum of the labels
sum_df <- master_wise_df %>%
  summarise(Workplace = sum(Workplace),
            Financial_aid = sum(Financial_aid),
            Childcare = sum(Childcare),
            Infertility = sum(Infertility))

# Reshape the data into a long format for plot
l_df <- sum_df %>%
  gather('label', 'sum', 1:4)

# Plot
ggplot(l_df, aes(x = label, y = sum, fill = label)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Workplace" = "green", "Financial_aid" = "orange", "Childcare" = "blue", "Infertility" = "lightblue")) +
  theme_minimal() +
  labs(title = "Media", x = "Label", y = "Sum", fill = "Label") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC", size = 20))
