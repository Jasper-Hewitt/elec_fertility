#_____________________________________________________________________________________________________________
setwd("/Users/jasperhewitt/Desktop/fertnews")

#pdffiles 
install.packages("pdftools")
library(pdftools)
library(tidyverse)
library(stringr)
library(lubridate)

#convert to txt with pdftools package
text <- pdf_text("fertnews1MERGED.pdf")

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

# delete the parts that we don't need
wise_df$date <- str_replace_all(wise_df$date, "\\| ", "")
wise_df$date <- str_replace_all(wise_df$date, "\n網站", "")

# delete everything before '文字快照'. 
wise_df$content <- sub(".*文字快照", "", wise_df$dirty_content)

#delete the column we no longer need
wise_df$dirty_content <- NULL
wise_df$dirty_date <- NULL

#this works: using a regular regex here does not really work because the links have several different formats
#Since all of the rows now start with a link, and are then followed by a news article in Chinese
#the following code uses the stringi package to delete all the non-han characters at the start of each row. This effectively gets
#rid of all of the links
wise_df$content <- stringi::stri_replace_all_regex(wise_df$content, '^[^\\p{Han}]*', "")

print(wise_df$content[1])

#to do!

#still select the part after 更多內容 in some of the posts and delete it bc it might contain some titles to other
#news articles. if those news articles have our key words than the whole article is not about
#our topic!

#delete rows with empty content column! because these are maybe not that important. 



