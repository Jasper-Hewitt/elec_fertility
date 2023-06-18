# elec_fertility

## to do


- Do not plot candidate vs candidate specific policy mentions. Only city vs city and perhaps party vs party!
for first sections (how often did candidates mention this problem)
- add in the x axis how many mentions each city has. but keep the emphasis on the proportions.
- put specific policy mention thing in the main file??
- rerun wordcloud based on the new posts!
- do we still want to make a timeline for the few candidates that mentioned this the most?
- clean up the repo

Part 1.1. This can be in same code as the aboveSEE NEW_master_preprocessing clean Kai's parts,in the end we are gonna put daily average as well as the total volume (per candidate)
- I created a search with ALL candidates for period 11 Feb 2022 to 27 Nov 2022
- https://apps.crowdtangle.com/search/1778778?customStartDate=2022-02-10T16:00:00&customEndDate=2022-11-27T15:59:00&platform=facebook&postTypes=&includedProducerIds=8749595,392096,415690,375324,143122,1883191,399441,2047893,467834,2222312,486515,19963244,448410,4654371,3146293&producerTypes=3,1,2&q=&sortBy=date&sortOrder=desc&timeframe=custom
- Will upload file "FB_All_Candidates.csv"






## keywords 
check this for making a cool readme https://www.makeareadme.com/
<div align="justify">

This is how we write our own thing. and this is what we do lol.  asdf asdf asd fasdf asdf asdf adsf asdf asdf asdf asd fasdf asdf asdf sad fasd fasdf asd fasd fasd fasd fasdf asd fasdf asd fsad fasdf asdf asd fsda fasdf asd fasdf asdf asdf asdf asd fasdf asdf asdf asd fasd fasd fasdf asdf asd fasdf asdfadsf asdf a
</div>

![Alt Text](https://github.com/Jasper-Hewitt/elec_fertility/blob/main/data/%E6%9E%97%E4%BD%B3%E9%BE%8D.png?raw=true)
**1. Discovering specific directions of fertility posts**
          
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
                         
                         
                         
                         
**2. Finding posts related to fertility**

          search_pattern<-"少子化｜生育率｜生育|生孩子|孕育|懷孕|育兒|育嬰|新生兒|托嬰|公托|臨托|產檢|孕"
         
**3. Relative importance related to other topics**
         
         keywords_Eldercare <- c("銀髮族", "長照", "在地安佬", "在地老化", "不老城", "重陽敬老金", "老人健保", "無障礙公車", "日照", "銀髮樂活", "敬老卡", "高齡運動")
         keywords_Public_Housing <- c("居住正義", "社會住宅", "社宅", "租屋", "青銀共居", "捷運社宅", "台中好宅", "老屋翻新", "包租代管", "囤房特別稅", "社宅")
         keywords_Fertility <- c("公托", "補助托嬰兒", "學前教育", "凍卵", "公幼", "親子中心", "公融式公園", "共融公園", "共融式特色公園", "產檢", "新生兒", "私幼", 
         "第二胎", "年輕爸媽", "生養小孩", "托育", "生養津貼", "收托時間", "托育人員", "坐月子", "產後憂鬱", "在校安親班", "夜托", "臨托", "少子化", "生育率","生育",
         "生孩子","懷孕","育兒","育嬰","新生兒","托嬰", "祝你好孕", "0-6歲", "0-2歲", "0-3歲") 
         
         keywords_Transport  <-c("鐵路", "鐵路地下化", "Youbike", "YouBike", "捷運", "腳踏車", "自行車", "公車", "車禍", "幸福里程", "通勤", "大眾運輸", 
         "步行", "國道", "客運轉運站", "公車", "交通", "運輸套票", "智慧交通", "機車路權", "軌道捷運", "iBike", "幹道", "輕軌", "台中大環", "閘道", "不塞車", 
         "Tbike", "人行道", "電動公車", "自行車質量", "聯外道路", "人行安全", "行人安全", "人性化路牌", "智慧街道", "四橫三路", "道路
         壅塞", "外環道路", "鐵路立體化", "行人地獄")
         
                        
                     






main doc: https://docs.google.com/document/d/18kQVusbmW53OZeb1G9uWIwLzbALJsUUTlVkBPKkpV6o/edit

### TO-DO
1) Wisenews Analysis
- Download all articles with keyword from 2022/02/11 - 2022/11/27 
- Wordcloud analysis to identify key topics


2) Candidate Post
- Combine all CSVs (DONE - see master)
- Preliminary keyword search of different categories of policies to look at frequency mentioned by candidates (in lieu of GPT) (take reference from 選舉公報)
- Wordcloud analysis (update STOP WORDS and KEYWORDS)
- Summary of the policies relating to fertility 
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

https://alvinntnu.github.io/NTNU_ENC2036_LECTURES/chinese-text-processing.html




