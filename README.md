# elec_fertility


##TO-DO
- clear up the GPT files (some of them were empty rows, so we have to recalculate the accuracy, also strip the commas and spaces)
- look into topic modelling? or BERTtopic in python





         search_pattern<-"少子化｜生育率｜生育|生孩子|孕育|懷孕|育兒|育嬰|新生兒|托嬰|公托|臨托|產檢|孕"
 
wassup

instruction for loading data from github directly into your code:
The code below downloads the csv file for hou. If you want someone else's data you have to get the raw link. 
You can get the raw link by going to data -> click the file you need -> click raw in the right top. 

         library(readr)

         url <- "https://raw.githubusercontent.com/Jasper-Hewitt/elec_fertility/main/data/newtaipei_hou.csv"
         data <- read_csv(url)

useful for plotting our own data (code from datacamp) https://github.com/Georgits/datacamp/blob/master/01_R/Text%20Mining%20Bag%20of%20Words/Text%20Mining%20Bag%20of%20Words.R





