library(readxl)
library(dplyr)
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
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(extrafont)
install.packages("extrafont")

library(extrafont)
font_import()

library(rvest)
library(xml2)
library(tidyverse)
library(tidytext)
library(knitr)

install.packages("showtext")
library(showtext)

#read the files 
TP_KMT <- read.csv("TP_KMT.csv")
TP_DPP <- read.csv("TP_DPP.csv")
TP_HUANG <- read.csv("TP_HUANG.csv")
NT_KMT <- read.csv("NT_KMT.csv")
NT_DPP <- read.csv("NT_DPP.csv")
TY_KMT <- read.csv("TY_KMT.csv")
TY_DPP <- read.csv("TY_DPP.csv")
TY_TPP <- read.csv("TY_TPP.csv")
TY_LZJ <- read.csv("TY_LZJ.csv")
TC_KMT <- read.csv("TC_KMT.csv")
TC_DPP <- read.csv("TC_DPP.csv")
TN_KMT <- read.csv("TN_KMT.csv")
TN_DPP <- read.csv("TN_DPP.csv")
KH_KMT <- read.csv("KH_KMT.csv")
KH_DPP <- read.csv("KH_DPP.csv")



#create a new column with the City name

TP_KMT$City <- c("TP")
TP_DPP$City  <- c("TP")
TP_HUANG$City  <- c("TP")
NT_KMT$City  <- c("NT")
NT_DPP$City  <- c("NT")
TY_KMT$City  <- c("TY")
TY_DPP$City  <- c("TY")
TY_TPP$City  <- c("TY")
TY_LZJ$City  <- c("TY")
TC_KMT$City  <- c("TC")
TC_DPP$City  <- c("TC")
TN_KMT$City <- c("TN")
TN_DPP$City  <- c("TN")
KH_KMT$City  <- c("KH")
KH_DPP$City  <- c("KH")

#create a new column with the Party

TP_KMT$Party <- c("KMT")
TP_DPP$Party  <- c("DPP")
TP_HUANG$Party  <- c("IND")
NT_KMT$Party  <- c("KMT")
NT_DPP$Party  <- c("DPP")
TY_KMT$Party  <- c("KMT")
TY_DPP$Party  <- c("DPP")
TY_TPP$Party  <- c("TPP")
TY_LZJ$Party  <- c("DPP")
TC_KMT$Party  <- c("KMT")
TC_DPP$Party  <- c("DPP")
TN_KMT$Party <- c("KMT")
TN_DPP$Party  <- c("DPP")
KH_KMT$Party  <- c("KMT")
KH_DPP$Party  <- c("DPP")

#All of the CSV files have 42 columns except TY_DPP, TY_KMT, TY_TPP, TN_DPP, KH_DPP, KH_DPP  which have 43. The difference is the additional columns 40 "[40] "Total.Interactions..weighted.....Likes.1x.Shares.1x.Comments.1x.Love.1x.Wow.1x.Haha.1x.Sad.1x.Angry.1x.Care.1x.." and 41 "[41] "Overperforming.Score" which is combined into one column in the other CSVs.                                                                                            
#To align the data,we can remove these column 40 in the CSV with 42 columns; and columns 40 and 41 in the CSV with 43 columns

#First delete column 40 for the CSVs with 42 columns
TP_KMT <- TP_KMT[,-40]
TP_DPP <- TP_DPP[,-40]
TP_HUANG <- TP_HUANG[,-40]
NT_KMT <- NT_KMT[,-40]
NT_DPP <- NT_DPP[,-40]
TY_LZJ <- TY_LZJ[,-40]
TC_KMT <- TC_KMT[,-40]
TC_DPP <- TC_DPP[,-40]
TN_KMT <- TN_KMT[,-40]

#Then delete column 40 and 41 for the CSVs with 43 columns
TY_DPP <- TY_DPP[,c(-40,-41)]
TY_KMT <- TY_KMT[,c(-40,-41)]
TY_TPP <- TY_TPP[,c(-40,-41)]
TN_DPP <- TN_DPP[,c(-40,-41)]
KH_KMT <- KH_KMT[,c(-40,-41)]
KH_DPP <- KH_DPP[,c(-40,-41)]

#merge all the 15 CSVs into one single dataframe

master <- rbind(TP_KMT, TP_DPP,TP_HUANG,NT_KMT,NT_DPP,TY_KMT,TY_DPP,TY_TPP,TY_LZJ,TC_KMT,TC_DPP,TN_KMT,TN_DPP,KH_KMT,KH_DPP)

#you can change the folder name accordingly
write.csv(master,"/Users/ks/Desktop/RP/master.csv")

#tag 1 0 for the posts that mention any of the keywords in the 5 categories - keywords are referenced from the 選舉公報

##Keywords

keywords_Eldercare <- c("銀髮族", "長照", "在地安佬", "在地老化", "不老城", "重陽敬老金", "老人健保", "無障礙公車", "日照", "銀髮樂活", "敬老卡", "高齡運動")
keywords_Public_Housing <- c("居住正義", "社會住宅", "社宅", "租屋", "青銀共居", "捷運社宅", "台中好宅", "老屋翻新", "包租代管", "囤房特別稅", "社宅")
keywords_Fertility <- c("公托", "補助托嬰兒", "學前教育", "凍卵", "公幼", "親子中心", "公融式公園", "共融公園", "共融式特色公園", "產檢", "新生兒", "私幼", "第二胎", "年輕爸媽", "生養小孩", "托育", "生養津貼", "收托時間", "托育人員", "坐月子", "產後憂鬱", "在校安親班", "夜托", "臨托", "少子化", "生育率","生育","生孩子","懷孕","育兒","育嬰","新生兒","托嬰", "祝你好孕", "0-6歲", "0-2歲", "0-3歲") 
keywords_Transport  <-c("鐵路", "鐵路地下化", "Youbike", "YouBike", "捷運", "腳踏車", "自行車", "公車", "車禍", "幸福里程", "通勤", "大眾運輸", "步行", "國道", "客運轉運站", "公車", "交通", "運輸套票", "智慧交通", "機車路權", "軌道捷運", "iBike", "幹道", "輕軌", "台中大環", "閘道", "不塞車", "Tbike", "人行道", "電動公車", "自行車質量", "聯外道路", "人行安全", "行人安全", "人性化路牌", "智慧街道", "四橫三路", "道路壅塞", "外環道路", "鐵路立體化", "行人地獄")
keywords_Economy<- c("智慧城市", "製造業", "產業", "物流", "金融", "科技", "引擎", "醫材", "工業4.0", "數位升級", "智慧升級", "創業", "數位新科技", "在地工作", "產業帶", "轉型", "新創轉型", "都心發展", "機能區", "藍色經濟圈", "海洋經濟", "海空雙港", "低碳產業", "產業高值化", "觀光", "旅客", "旅行社", "商圈", "招商引資", "拼經濟", "產業園區", "中科2.0", "新苗計劃", "創業孵化器", "航空城", "產學訓用", "青創基地", "高科技產業", "優質就業", "國際廠商", "高階製造中心")

#Tag as 1 if any of the words appaer in the messages, otherwise 0.
master$Eldercare <- ifelse(grepl(paste(keywords_Eldercare, collapse = "|"), master$Message), 1, 0)
master$Public_Housing <- ifelse(grepl(paste(keywords_Public_Housing, collapse = "|"), master$Message), 1, 0)
master$Fertility <- ifelse(grepl(paste(keywords_Fertility, collapse = "|"), master$Message), 1, 0)
master$Transport <- ifelse(grepl(paste(keywords_Transport, collapse = "|"), master$Message), 1, 0)
master$Economy <- ifelse(grepl(paste(keywords_Economy, collapse = "|"), master$Message), 1, 0)

print(sum(master$Eldercare==1))
print(sum(master$Public_Housing==1))
print(sum(master$Fertility==1))
print(sum(master$Transport==1))
print(sum(master$Economy==1))

#Create new dataframe with the 5 columns
# Create a new data frame with column sums
Overall_post <- data.frame(
  Eldercare = sum(master$Eldercare),
  Public_Housing = sum(master$Public_Housing),
  Fertility = sum(master$Fertility),
  Transport = sum(master$Transport),
  Economy = sum(master$Economy)
)

Overall_post<- Overall_post %>%
  gather(key = "Category", value = "Number_of_Posts") 

Overall_post <- Overall_post %>%
  arrange(desc(Number_of_Posts))

Overall_post <- Overall_post %>%
  mutate(Percentage=Overall_post$Number_of_Posts/5998)


Overall_post <- Overall_post[order(-Overall_post$Number_of_Posts), ]

ggplot(Overall_post, aes(x = Number_of_Posts, y = reorder(Category, Number_of_Posts), fill = Category)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of Posts", y = "Category", title = "Number of Posts by Category") +
  theme_minimal() +
  guides(fill = "none")  




##FILTER BY PARTY (KMT VS DPP VS OTHERS)

master_byparty_KMT <- master %>%
  filter(Party=="KMT") 

master_byparty_DPP <- master %>%
  filter(Party=="DPP")

Overall_post_KMT <- data.frame(
  Eldercare = sum(master_byparty_KMT$Eldercare),
  Public_Housing = sum(master_byparty_KMT$Public_Housing),
  Fertility = sum(master_byparty_KMT$Fertility),
  Transport = sum(master_byparty_KMT$Transport),
  Economy = sum(master_byparty_KMT$Economy)
)
  
Overall_post_DPP <- data.frame(
  Eldercare = sum(master_byparty_DPP$Eldercare),
  Public_Housing = sum(master_byparty_DPP$Public_Housing),
  Fertility = sum(master_byparty_DPP$Fertility),
  Transport = sum(master_byparty_DPP$Transport),
  Economy = sum(master_byparty_DPP$Economy)
)


Overall_post_KMT_table<- Overall_post_KMT %>%
  gather(key = "Category", value = "Number_of_Posts") 

Overall_post_DPP_table<- Overall_post_DPP %>%
  gather(key = "Category", value = "Number_of_Posts") 

Overall_post_Party_Table <- left_join(Overall_post_KMT_table,Overall_post_DPP_table,by="Category")

colnames(Overall_post_Party_Table)[2:3] <- c("KMT","DPP")

Overall_post_Party_Table <- Overall_post_Party_Table %>%
  gather("Party","Number_of_Post",2:3)




ggplot(Overall_post_Party_Table, aes(x = Category, y = Number_of_Post, fill = Party)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Number of Posts", y = "Category", title = "Number of Posts by Party") +
  theme_minimal()




##FILTER BY CITY


master_bycity_TP <- master %>%
  filter(City=="TP") 

master_bycity_NT <- master %>%
  filter(City=="NT")

master_bycity_TY <- master %>%
  filter(City=="TY") 

master_bycity_TC <- master %>%
  filter(City=="TC")

master_bycity_TN <- master %>%
  filter(City=="TN")

master_bycity_KH <- master %>%
  filter(City=="KH")

#individual city table
Overall_post_TP<- data.frame(
  Eldercare = sum(master_bycity_TP$Eldercare),
  Public_Housing = sum(master_bycity_TP$Public_Housing),
  Fertility = sum(master_bycity_TP$Fertility),
  Transport = sum(master_bycity_TP$Transport),
  Economy = sum(master_bycity_TP$Economy)
)
#cTP <- c("TP")
#Overall_post_TP<- cbind(cTP,Overall_post_TP)

Overall_post_NT<- data.frame(
  Eldercare = sum(master_bycity_NT$Eldercare),
  Public_Housing = sum(master_bycity_NT$Public_Housing),
  Fertility = sum(master_bycity_NT$Fertility),
  Transport = sum(master_bycity_NT$Transport),
  Economy = sum(master_bycity_NT$Economy)
)
#cNT <- c("NT")
#Overall_post_NT<- cbind(cNT,Overall_post_NT)

Overall_post_TY<- data.frame(
  Eldercare = sum(master_bycity_TY$Eldercare),
  Public_Housing = sum(master_bycity_TY$Public_Housing),
  Fertility = sum(master_bycity_TY$Fertility),
  Transport = sum(master_bycity_TY$Transport),
  Economy = sum(master_bycity_TY$Economy)
)
#cTY <- c("TY")
#Overall_post_TY<- cbind(cTY,Overall_post_TY)

Overall_post_TC<- data.frame(
  Eldercare = sum(master_bycity_TC$Eldercare),
  Public_Housing = sum(master_bycity_TC$Public_Housing),
  Fertility = sum(master_bycity_TC$Fertility),
  Transport = sum(master_bycity_TC$Transport),
  Economy = sum(master_bycity_TC$Economy)
)
#cTC <- c("TC")
#Overall_post_TC<- cbind(cTC,Overall_post_TC)

Overall_post_TN<- data.frame(
  Eldercare = sum(master_bycity_TN$Eldercare),
  Public_Housing = sum(master_bycity_TN$Public_Housing),
  Fertility = sum(master_bycity_TN$Fertility),
  Transport = sum(master_bycity_TN$Transport),
  Economy = sum(master_bycity_TN$Economy)
)
#cTN <- c("TN")
#Overall_post_TN<- cbind(cTN,Overall_post_TN)

Overall_post_KH<- data.frame(
  Eldercare = sum(master_bycity_KH$Eldercare),
  Public_Housing = sum(master_bycity_KH$Public_Housing),
  Fertility = sum(master_bycity_KH$Fertility),
  Transport = sum(master_bycity_KH$Transport),
  Economy = sum(master_bycity_KH$Economy)
)
#cKH <- c("KH")
#Overall_post_KH<- cbind(cKH,Overall_post_KH)

#rbind
Overall_post_by_city <-  rbind(Overall_post_TP,Overall_post_NT,Overall_post_TY,Overall_post_TC,Overall_post_TN,Overall_post_KH)


Overall_post_by_city <- t(Overall_post_by_city)
colnames(Overall_post_by_city)<- c("TP","NT","TY","TC","TN","KH")



Category <- data.frame(Column=c("Eldercare","Public_Housing","Fertility","Transport","Economy"))

Overall_post_by_city<- cbind(Category,Overall_post_by_city)

Overall_post_by_city<- Overall_post_by_city %>%
  gather("Category","Post_by_City",2:7)
         
colnames(Overall_post_by_city)[1:3]<- c("Category","City","Post_by_city")

#ggplot(Overall_post_by_city,aes(x="City",y="Post_by_city",color=Category,group=Category)) +geom_bar()


ggplot(Overall_post_by_city, aes(x = Category, y = Post_by_city, fill = City)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Category", y = "Number of Posts", title = "Number of Posts by City") +
  theme_minimal()

ggplot(Overall_post_by_city, aes(x = City, y = Post_by_city, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "City", y = "Number of Posts", title = "Number of Posts by City") +
  theme_minimal()




##PLOT BY PERIOD

#note 盧秀燕 and 蔡其昌 dates in the Post.created.date column is in '/'. So we instead cut from the Post.created column instead.


master$Date_cleaned <- substr(master$Post.Created,1,10)
master$Date_cleaned <- as.Date(master$Date_cleaned)

fertility_posts <- master %>%
  filter(Fertility==1)

fertility_posts_bydate <- fertility_posts %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())


ggplot(fertility_posts_bydate, aes(x=Date_cleaned, y=post)) + geom_line()


#double check what are the posts on 2022-11-05 and 2022-11-12which showed a peak. 
a <- fertility_posts%>%
  filter(Date_cleaned=="2022-11-05") 

aa <- fertility_posts%>%
  filter(Date_cleaned=="2022-11-12") 


##Calculate the average policy post per candidate

master$policy_mention <- as.integer(rowSums(master[,c("Eldercare","Public_Housing","Fertility","Transport","Economy")])>0) 

master_policy_mention <- master %>%
  filter(policy_mention==1) %>%
  group_by(Page.Name) %>%
  summarise(number_of_post=n())

city<- c(column="NT","TY","NT","TY","KH","TC","TC","TP","TN","TY","TY","TP","TN","TP")

colnames(master_policy_mention)[1] <- "Candidate"

election_period <- read.csv("Electiondate.csv")

master_policy_mention_combined<- merge(master_policy_mention,election_period, by="Candidate")

master_policy_mention_combined <- master_policy_mention_combined %>%
  mutate(Averagedailypost = number_of_post/Number.of.Dats)

master_policy_mention_combined <- master_policy_mention_combined %>%
  arrange(desc(master_policy_mention_combined$Averagedailypost))

#you need this to show the chinese text
showtext_auto()

#randomise colours
num_colors <- length(unique(master_policy_mention_combined$Candidate))
colors <- sample(colors(), num_colors)

#Bar plot showing the average daily post with policy mention per candate - arranged in descending order
ggplot(master_policy_mention_combined, aes(x = reorder(Candidate, Averagedailypost), y = Averagedailypost, fill = Candidate)) +
  geom_bar(stat = "identity", color = "black", size = 0.2) +
  labs(x = "Candidate", y = "Average Daily Posts", title = "Average Daily Post with Policy Mention") +
  coord_flip() +
  theme(axis.text.y = element_text(hjust = 1)) +
  scale_fill_manual(values = colors)


##focus on policy post trends --- no obvious trends in most candidates except for Huang Shanshan and Chen Shih Chung and Hou which peaked during the policy debate.


#master$Date_cleaned <- substr(master$Post.Created,1,10)
#master$Date_cleaned <- as.Date(master$Date_cleaned)


#TPE [Huang; Chen spiked during the 11/5 debate; Chiang no]
TP_HUANG_period <- master %>%
  filter(Page.Name=="黃珊珊") %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TP_HUANG_period, aes(x=Date_cleaned, y=post)) + geom_line()

TP_DPP_period <- master %>%
  filter(Page.Name=="陳時中") %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TP_DPP_period, aes(x=Date_cleaned, y=post)) + geom_line()

TP_KMT_period <- master %>%
  filter(Page.Name=="蔣萬安") %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TP_KMT_period, aes(x=Date_cleaned, y=post)) + geom_line()

#NT [Hou peaked during the 11/14 debate; Lin didn't)]

NT_DPP_period <- master %>%
  filter(Page.Name=="林佳龍") %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(NT_DPP_period, aes(x=Date_cleaned, y=post)) + geom_line()

NT_KMT_period <- master %>%
  filter(Page.Name=="侯友宜") %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(NT_KMT_period, aes(x=Date_cleaned, y=post)) + geom_line()



#TY 

TY_DPP_period <- master %>%
  filter(Page.Name=="鄭運鵬") %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TY_DPP_period, aes(x=Date_cleaned, y=post)) + geom_line()

TY_KMT_period <- master %>%
  filter(Page.Name=="張善政") %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TY_KMT_period, aes(x=Date_cleaned, y=post)) + geom_line()

TY_TPP_period <- master %>%
  filter(Page.Name=="賴香伶") %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TY_TPP_period, aes(x=Date_cleaned, y=post)) + geom_line()


#TC 

TC_DPP_period <- master %>%
  filter(Page.Name=="蔡其昌") %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TC_DPP_period, aes(x=Date_cleaned, y=post)) + geom_line()

TC_KMT_period <- master %>%
  filter(Page.Name=="盧秀燕") %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TC_KMT_period, aes(x=Date_cleaned, y=post)) + geom_line()


#TN [Hou peaked during the 11/14 debate; Lin didn't)]

TN_DPP_period <- master %>%
  filter(Page.Name=="黃偉哲") %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TN_DPP_period, aes(x=Date_cleaned, y=post)) + geom_line()

TN_KMT_period <- master %>%
  filter(Page.Name=="謝龍介") %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TN_KMT_period, aes(x=Date_cleaned, y=post)) + geom_line()


#KH 
#CHEN QI MAI code does not run if i choose the chinese name column so i used user.name column instead

KH_DPP_period <- master %>%
  filter(User.Name=="chenchimai") %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

#KH_DPP_period <- master %>%
  #filter(Page.Name=="陳其邁") %>%
  #filter(policy_mention==1) %>%
  #group_by(Date_cleaned) %>%
  #summarise(post=n())

ggplot(KH_DPP_period, aes(x=Date_cleaned, y=post)) + geom_line()

KH_KMT_period <- master %>%
  filter(Page.Name=="柯志恩") %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(KH_KMT_period, aes(x=Date_cleaned, y=post)) + geom_line()

#overall daily post

overall_daily_policy_count <- master %>%
  filter(policy_mention==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(overall_daily_policy_count,aes(x=Date_cleaned, y=post)) +geom_line()


#I try to create a plot with ALL + TP_HUANG + TP_DPP + NT_KMT on the daily post

overall_daily_master_candidate <- left_join(overall_daily_policy_count, TP_HUANG_period, by = "Date_cleaned")
overall_daily_master_candidate <- left_join(overall_daily_master_candidate, TP_DPP_period, by = "Date_cleaned")
overall_daily_master_candidate <- left_join(overall_daily_master_candidate, NT_KMT_period, by = "Date_cleaned")

colnames(overall_daily_master_candidate)[2:5] <- c("Overall","TP_HUANG","TP_DPP","NT_KMT")

overall_daily_master_candidate <- overall_daily_master_candidate  %>%
  gather("Date","Number_of_post",2:5)

ggplot(overall_daily_master_candidate, aes(x=Date_cleaned, y=Number_of_post, group=Date)) + geom_line(aes(color=Date))


##Do the plot for just fertility posts by each candidate

ggplot(house_gender, aes(term, number, group = gender)) + geom_line(aes(color = gender))


TP_HUANG_period <- master %>%
  filter(Page.Name=="黃珊珊") %>%
  filter(Fertility==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TP_HUANG_period, aes(x=Date_cleaned, y=post)) + geom_line()

TP_DPP_period <- master %>%
  filter(Page.Name=="陳時中") %>%
  filter(Fertility==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TP_DPP_period, aes(x=Date_cleaned, y=post)) + geom_line()

TP_KMT_period <- master %>%
  filter(Page.Name=="蔣萬安") %>%
  filter(Fertility==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TP_KMT_period, aes(x=Date_cleaned, y=post)) + geom_line()

#NT 

NT_DPP_period <- master %>%
  filter(Page.Name=="林佳龍") %>%
  filter(Fertility==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(NT_DPP_period, aes(x=Date_cleaned, y=post)) + geom_line()

NT_KMT_period <- master %>%
  filter(Page.Name=="侯友宜") %>%
  filter(Fertility==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(NT_KMT_period, aes(x=Date_cleaned, y=post)) + geom_line()



#TY 

TY_DPP_period <- master %>%
  filter(Page.Name=="鄭運鵬") %>%
  filter(Fertility==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TY_DPP_period, aes(x=Date_cleaned, y=post)) + geom_line()

TY_KMT_period <- master %>%
  filter(Page.Name=="張善政") %>%
  filter(Fertility==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TY_KMT_period, aes(x=Date_cleaned, y=post)) + geom_line()

TY_TPP_period <- master %>%
  filter(Page.Name=="賴香伶") %>%
  filter(Fertility==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TY_TPP_period, aes(x=Date_cleaned, y=post)) + geom_line()


#TC 

TC_DPP_period <- master %>%
  filter(Page.Name=="蔡其昌") %>%
  filter(Fertility==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TC_DPP_period, aes(x=Date_cleaned, y=post)) + geom_line()

TC_KMT_period <- master %>%
  filter(Page.Name=="盧秀燕") %>%
  filter(Fertility==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TC_KMT_period, aes(x=Date_cleaned, y=post)) + geom_line()


#TN 

TN_DPP_period <- master %>%
  filter(Page.Name=="黃偉哲") %>%
  filter(Fertility==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TN_DPP_period, aes(x=Date_cleaned, y=post)) + geom_line()

TN_KMT_period <- master %>%
  filter(Page.Name=="謝龍介") %>%
  filter(Fertility==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(TN_KMT_period, aes(x=Date_cleaned, y=post)) + geom_line()


#KH 
#CHEN QI MAI code does not run if i choose the chinese name column so i used user.name column instead

KH_DPP_period <- master %>%
  filter(User.Name=="chenchimai") %>%
  filter(Fertility==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())

ggplot(KH_DPP_period, aes(x=Date_cleaned, y=post)) + geom_line()

KH_KMT_period <- master %>%
  filter(Page.Name=="柯志恩") %>%
  filter(Fertility==1) %>%
  group_by(Date_cleaned) %>%
  summarise(post=n())



ggplot(KH_KMT_period, aes(x=Date_cleaned, y=post,fill=Date)) + geom_line()


#### Classify POST by CATEGORIES

master_fertility <- master %>%
  filter(Fertility==1)

##Keywords

keywords_Childcare <- c("公托", "補助托嬰兒","公幼","在校安親班", "夜托", "臨托","收托時間", "托育人員","私幼","托育","親子中心","托育補助","托育家園","公共化托育","公立托育中心")
keywords_Subsidies <- c("生養津貼","第二胎","第2胎", "第三胎", "第3胎","第一胎","第1胎" ,"三胎以上","3胎以上","育兒津貼")
keywords_Pregnancy <- c("凍卵","產後憂鬱","坐月子","產檢")
keywords_Infrastructure <- c("公融式公園", "共融公園", "共融式特色公園")

#Tag as 1 if any of the words appear in the messages, otherwise 0.
master_fertility$Childcare <- ifelse(grepl(paste(keywords_Childcare, collapse = "|"), master_fertility$Message), 1, 0)
master_fertility$Subsidies <- ifelse(grepl(paste(keywords_Subsidies, collapse = "|"), master_fertility$Message), 1, 0)
master_fertility$Pregnancy <- ifelse(grepl(paste(keywords_Pregnancy, collapse = "|"), master_fertility$Message), 1, 0)
master_fertility$Infrastructure <- ifelse(grepl(paste(keywords_Infrastructure, collapse = "|"), master_fertility$Message), 1, 0)


print(sum(master_fertility$Childcare))
print(sum(master_fertility$Subsidies))
print(sum(master_fertility$Pregnancy))
print(sum(master_fertility$Infrastructure))


#Compare by city
#Compare by candidate
#Compare by party




