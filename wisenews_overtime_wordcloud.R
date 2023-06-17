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
library(lubridate)

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


#___________#__________### Mentions over time ####___________#__________# 

#plot newsarticles over time 
#create new column with only the dates. remove everything after the first space 
wise_df$dateplot <- as.Date(str_replace(wise_df$date, " .*", ""), format = "%Y-%m-%d")

#get counts per date
wise_df_timeplot <- wise_df %>%
  group_by(dateplot) %>%
  summarise(count = n())

#beautiful plot with ggplot. This one suddenly has some problems 
ggplot(wise_df_timeplot, aes(x=dateplot, y=count)) + 
  geom_line(colour = "purple") +
  labs(title = "Daily articles about population ageing", x = "Date", y = "Count") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + #set date per month instead of day
  theme_minimal() + 
  theme(plot.title = element_text(size = 20, face = "bold", color = "darkblue"))

#___________#__________### TOKENIZATION, STOPWORDS, CUSTOM DICTIONARY ####___________#__________# 

#the following code is based on https://alvinntnu.github.io/NTNU_ENC2036_LECTURES/chinese-text-processing.html .
#we have made several adjustments

#add doc_id
wise_df <- wise_df %>% 
  mutate(doc_id = row_number())

## for word segmentation only
my_seg <- worker(bylines = T,
                 user = "customdict.txt",
                 symbol = T)



wise_word <- wise_df %>%
  ## word tokenization
  unnest_tokens(
    output = word,
    input = content,  # the name of the column we are plotting
    token = function(x)
      segment(x, jiebar = my_seg)
  ) %>%
  group_by(doc_id) %>%
  mutate(word_id = row_number()) %>% # create word index within each document
  ungroup

#stop word lists
## load chinese stopwords
stopwords <- readLines("stopwords_zh_trad.txt",
                           encoding = "UTF-8")

#We want to take out some basic words like 
#少子化 and 生育率, because they are not very informative 
custom_stopwords <- c("經濟", "科技", "報導", "可能", "指出", "認為", "新聞網", "國際", 
                      "應該", "可能", "提出", "過去", "現在", "進行","今天", "相關", "社會",
                      "議題", "很多", "undo", "需要", "需求", "已經", "目前", "今年", "透過",
                      "地方", "沒有", "記者", "成為", "持續", "市場", "表示", "台灣", "造成",
                      "不少", "原因", "影響", "人口","台北", "生育率", "問題", "育兒", "生育", 
                      "少子化", "/", "10", "20", "30", "一起", "桃園", "台中", "市長", "市民", "朋友",
                      "城市", "12", "11", "高雄", "https", "台北市", "台中市", "台南市", "高雄市", "台南")  # specific words about 生育率# specific words about 生育率
stopwords <- c(stopwords, custom_stopwords)

#___________#__________### WORDCLOUD & WORD FREQUENCY CHART ####___________#__________# 

## create word freq list
wise_word_freq <- wise_word %>%
  mutate(word = str_trim(word)) %>%  # remove leading and trailing whitespace
  filter(str_length(word) > 1, !word %in% stopwords) %>% # remove single character words and stopwords
  count(word) %>%
  arrange(desc(n))


#we could also consider to just keep in certain words. and go from there!
#like we just select a bunch of keywords that we think are worth discussing and then we only run a word cloud with that?
wise_word_freq %>%
  filter(n > 1000) %>%
  filter(nchar(word) >= 2) %>% ## remove monosyllabic tokens
  wordcloud2(shape = "circle", size = 0.4)

#word frequency

# Selecting top 30 most frequent words
top_30_words <- wise_word_freq %>%
  top_n(30, n)


# Creating the plot
ggplot(top_30_words, aes(x = reorder(word, n), y = n)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Words", y = "Count", title = "30 Most Frequent Words") +
  theme_minimal() +
  theme(text = element_text(family = "Songti SC"))



print(top_30_words[1])













