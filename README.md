# An Investigation into How the Declining Fertility Problem Featured in Taiwan's 2022 Mayoral Election

## Project Description

<div align="justify">
  
In 2022, Taiwan recorded its lowest fertility rate of 0.87. This is expected to drop even further and Taiwan’s fertility rate is set to become the world’s lowest by 2035 (Kao and Liu, 2022). The issue has also become one of the hot topics during the mayoral election last year. The 2022 mayoral candidates have put forward various strategies and policy proposals to tackle the low fertility rate problem. 
This project seeks to ascertain how the issue of low fertility rates factored into the 2022 local elections. Moreover, we aim to uncover the underlying factors that shaped the candidates' outlooks on low fertility rates. To this end, we ask the following three research questions.

- How frequently did the candidates mention the issue on Facebook?
- What policy ideas did they propose to resolve the issue?
- How does this compare to discussions and perceptions in the media?
  
We focus on Taiwan’s six special municipalities: Taipei, New Taipei, Taoyuan, Taichung, Tainan, and Kaohsiung. 

Initially, we planned to employ ChatGPT (3.5) for our analysis. However, after manually labelling 300 posts we found that ChatGPT’s accuracy is not high enough. Therefore, we adjusted our methodology to informed keyword searches in addition to a word cloud. 


</div>

## Getting Started

<div align="justify">
[Provide instructions on how to get started with your project, including any necessary software or data. Include installation instructions and any prerequisites or dependencies that are required.]
</div>

## File Structure

<div align="justify">
  
- GPT_test contains the code and the results for our test with chat_GPT. The csv files contain the post, manual labels, and GPT's output.
- data contains all of our data from both Wisenews and Crowdtangle
  - Wisenews: we split the pdf file in two because it was too large to upload.
  - FB_All_candidates.csv contains all the Facebook posts from all the candidates in the period 2022/02/11 to 2022/11/27. master_candidates_posts.csv is the preprocessed version
    that is adjusted for the exact campaign period for each candidate. candidates_info.xlsx contains additional information about each candidate (e.g. city, party, english name,
    etc.). Lastly, election_bulletin.xlsx contains all of the candidates' election bullitins. These played an important role in compiling our keyword list.
- dict_and_stopwords contains our custom dictionary and stop word list, and additional_custom stop word list
- birth_rate_plot contains linear plots based on Taiwan's fertility data. We use this in our introduction
- final_paper_group06.pdf (final report)
- main_code.R contains all of the preprocessing, data cleaning, text mining, and plots for the candidates' facebook posts.
- wisenews.R contains all of the preprocessing, data cleaning, text mining, and plots for the articles collected from wisenews.
  
</div>
  <strong> file structure overview </strong>
  
          ├── GPT_test
          │   ├── GPT_test.R
          │   ├── fertcheck_0.514.csv
          │   ├── fertsolutions_GPT0.233.csv
          │   └── general_topics_GPT0.481.csv
          ├── data
          │   ├── Wisenews_data 
          │   │   ├── wisenews_fertility_part1.pdf
          │   │   └── wisenews_fertility_part2.pdf
          │   └── candidate_posts
          │       ├── FB_All_Candidates.csv
          │       ├── Election_bulletin.xlsx
          │       ├── candidates_info.xlsx
          │       └── master_candidates_posts.csv
          ├── dict_and_stopwords
          │   ├── additional_custom_stopwords.txt
          │   ├── customdict.txt
          │   └── stopwords_zh_trad.txt
          ├── README.md
          ├── birth_rate_plots.R
          ├── final_paper_group06.pdf
          ├── main_code.R
          └── wisenews.R


## Analysis

<div align="justify">
[Describe your analysis methods and include any visualizations or graphics that you used to present your findings. Explain the insights that you gained from your analysis and how they relate to your research question or problem statement.]
</div>

## Results

<div align="justify">
[Provide a summary of your findings and conclusions, including any recommendations or implications for future research. Be sure to explain how your results address your research question or problem statement.]
</div>

## Contributors

[List the contributors to your project and describe their roles and responsibilities.]

## Acknowledgments

[Thank any individuals or organizations who provided support or assistance during your project, including funding sources or data providers.]

## References

The references can be found in Section 7 of the final_paper_group06.pdf.


# elec_fertility

## code
all wisenews stuff is in wisenews.r

all candidates stuff is in main_code.r


## to do
- clean up repository
- make candidates posts plot over time
- put plots into report 



**1. Discovering specific directions of fertility posts**
          
          search_pattern_workplace <- "安胎假|陪產假|產檢假|育嬰留職停薪|生理假|企業托育|聯合托育|企業哺乳室"
          
          search_pattern_financial_aid <-"生育獎勵|嬰兒補助|育兒津貼|育兒補助|生養津貼|生養補助|生育津貼|生育補助|育嬰留職停薪津貼
                                          |就學補助|就寫津貼｜幼兒學前特別扣除額|免學費|育嬰留職停薪津貼|未來教育及發展帳戶"
          
          search_pattern_cat_childcare <- "公托|公幼|托育|收托|送托|托嬰|夜托|臨托|教保服務
                                           |教保人員|社區保母|在校安親班|準公共機制|平價就學場域|平價名額|兒童預防保健服務0-未滿 7 歲|收出養媒合|養安置
                                           |兒童三級預防措施|防治兒虐事件"
          
          search_pattern_cat_infertility <- "懷孕以及孕婦相關|不孕症|試管嬰兒|不孕夫|凍卵|坐月子|產檢|孕產婦關懷中心|產前健康檢查"
                         
                         
                         
                         
**2. Finding posts related to fertility**

          keywords_Fertility<-"少子化|生育率|生育|生孩子|懷孕|育兒|育嬰|新生兒|托嬰|公托|臨托
                              |產檢安胎假|陪產假|產檢假|育嬰留職停薪|生理假|企業托育|聯合托育|企業哺乳室|
                              生育獎勵|嬰兒補助|育兒津貼|育兒補助|生養津貼|生養補助|生育津貼|生育補助
                              |育嬰留職停薪津貼|就學補助|就寫津貼｜幼兒學前特別扣除額|免學費|育嬰留職停薪津貼
                              |未來教育及發展帳戶|公托|公幼|托育|收托|送托
                              |托嬰|夜托|臨托|教保服務|教保人員|社區保母|在校安親班|準公共機制|平價就學場域
                              |平價名額|兒童預防保健服務0-未滿 7 歲|收出養媒合|養安置|兒童三級預防措施|防治兒虐事件
                              |懷孕以及孕婦相關|不孕症|試管嬰兒|不孕夫|凍卵|坐月子|產檢|孕產婦關懷中心|產前健康檢查" 
         
**3. Relative importance related to other topics**
        
        keywords_Fertility <-          see above 
        
        keywords_Eldercare <-       c("銀髮族", "長照", "在地安佬", "在地老化", "不老城", "重陽敬老金", "老人健保", "無障礙公車", "日照", "銀髮樂活", "敬老卡", "高齡運動")
        
        keywords_Public_Housing <-  c("居住正義", "社會住宅", "社宅", "租屋", "青銀共居", "捷運社宅", "台中好宅", "老屋翻新", "包租代管", "囤房特別稅", "社宅")
        
        keywords_Transport  <-      c("鐵路", "鐵路地下化", "Youbike", "YouBike", "捷運", "腳踏車", "自行車", "公車", "車禍", "幸福里程", "通勤", "大眾運輸", "步行", "國道", "客運轉運站", "公車", 
                                      "交通", "運輸 套票", "智慧交通", "機車路權", "軌道捷運", "iBike", "幹道", "輕軌", "台中大環", "閘道", "不塞車", "Tbike", "人行道", "電動公車", "自行車質量", 
                                      "聯外道路",  "人行安全", "行人安全", "人性化路牌", "智慧街道", "四橫三路", "道路壅塞", "外環道路", "鐵路立體化", "行人地獄")
                                      
        keywords_Economy<-          c("智慧城市", "製造業", "產業", "物流", "金融", "科技", "引擎", "醫材", "工業4.0", "數位升級", "智慧升級", "創業", "數位新科技", "在地工作", "產業帶", "轉型", "新創轉
                                       型", "都心發展", "機能區", "藍色經濟圈", "海洋經濟", "海空雙港", "低碳產業", "產業高值化", "觀光", "旅客", "旅行社", "商圈", "招商引資", "拼經濟", "產業園區", 
                                       "中科2.0", "新苗計劃", "創業孵化器", "航空城", "產學訓用", "青創基地", "高科技產業", "優質就業", "國際廠商", "高階製造中心")
         
                        
                     






