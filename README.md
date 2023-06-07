# elec_fertility


### TO-DO
1) Wisenews Analysis
- Download all articles with keyword from 2022/02/11 - 2022/11/27 
- Wordcloud analysis to identify key topics


2) Candidate Post
- Combine all CSVs (DONE - see master)
- Preliminary keyword search of different categories of policies to look at frequency mentioned by candidates (in lieu of GPT) (take reference from 選舉公報)
- Wordcloud analysis (update STOP WORDS and KEYWORDS)
- do the final GPT task just for the sake of completeness (?)
- look into topic modelling? or BERTtopic in python (?)




3) Poster 
- Complete first cut draft by Fri 6/9
- Send for printing on Mon 6/12


4) Written Report 
- By 6/22 (?)








### snippets

         search_pattern<-"少子化｜生育率｜生育|生孩子|孕育|懷孕|育兒|育嬰|新生兒|托嬰|公托|臨托|產檢|孕"
 

instruction for loading data from github directly into your code:
The code below downloads the csv file for hou. If you want someone else's data you have to get the raw link. 
You can get the raw link by going to data -> click the file you need -> right-click raw in the right top and save the link. 

         library(readr)

         url <- "https://raw.githubusercontent.com/Jasper-Hewitt/elec_fertility/main/data/newtaipei_hou.csv"
         data <- read_csv(url)

### for reference
useful for plotting our own data (code from datacamp):

https://github.com/Georgits/datacamp/blob/master/01_R/Text%20Mining%20Bag%20of%20Words/Text%20Mining%20Bag%20of%20Words.R

NTNU intro to tokenization and wordclouds:

https://github.com/Jasper-Hewitt/elec_fertility/blob/main/wisenews_textmining.R





