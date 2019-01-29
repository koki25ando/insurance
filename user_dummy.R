# 擬似個人情報データ生成サービース: https://hogehoge.tk/personal/
  
personal_info <- read.csv("personal_infomation.csv")  
dummy_users <- personal_info %>% 
  select(氏名, 性別, 住所1, 住所2, 年齢)

library(rvest)
url = "http://grading.jpn.org/SRB02101.html"
page <- read_html(url)
table <- page %>% 
  html_table() %>% 
  extract2(2)
names(table) <- table[1,]
merged_dummy <- merge(table[-c(1,2),], dummy_users, by.x = "都道府県", by.y = "住所1")
dummy_df <- merged_dummy[,c(1,3, 7:10)]
dummy_df$年平均気温 <- dummy_df$年平均気温 %>% 
  str_remove("度摂氏") %>% 
  as.numeric()

dummy_df %>% 
  ggplot(aes(x = 年齢, y = 年平均気温)) +
  geom_point()
  head()
