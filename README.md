# elec_fertility

wassup

instruction for loading data from github directly into your code:
The code below downloads the csv file for hou. If you want someone else's data you have to get the raw link. 
You can get the raw link by going to data -> click the file you need -> click raw in the right top. 

         library(readr)

         url <- "https://raw.githubusercontent.com/Jasper-Hewitt/elec_fertility/main/data/newtaipei_hou.csv"
         data <- read_csv(url)

useful for plotting our own data (code from datacamp) https://github.com/Georgits/datacamp/blob/master/01_R/Text%20Mining%20Bag%20of%20Words/Text%20Mining%20Bag%20of%20Words.R





