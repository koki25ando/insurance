# import data
transactions_df <- read.csv("BreadBasket_DMS.csv")
probability <- read.csv("Probability.csv")

# data of item
items <- probability$Var1
transactions <- transactions_df %>% 
  filter(Item %in% items) #購入回数が5回以上の商品のみを選択する

# 取引ごとの購買商品の組み合わせを作成する
itemset <- transactions %>% 
  group_by(Transaction) %>%
  summarize(ItemSet = list(Item))

set_list = list()
for (i in 1:length(itemset$ItemSet)){
  len <- length(itemset$ItemSet[[i]])
  
  if (len == 1) {
    msg = itemset$ItemSet[[i]][1]
  } else if (len == 2){
    msg = paste(itemset$ItemSet[[i]][1], itemset$ItemSet[[i]][2], sep = ", ")
  } else if (len == 3){
    msg = paste(itemset$ItemSet[[i]][1], itemset$ItemSet[[i]][2], itemset$ItemSet[[i]][3], sep = ", ")
  } else if (len == 4){
    msg = paste(itemset$ItemSet[[i]][1], itemset$ItemSet[[i]][2], itemset$ItemSet[[i]][3], itemset$ItemSet[[i]][4], 
                sep = ", ")
  } else if (len == 5){
    msg = paste(itemset$ItemSet[[i]][1], itemset$ItemSet[[i]][2], itemset$ItemSet[[i]][3], itemset$ItemSet[[i]][4],
                itemset$ItemSet[[i]][5],
                sep = ", ")
  } else if (len == 6){
    msg = paste(itemset$ItemSet[[i]][1], itemset$ItemSet[[i]][2], itemset$ItemSet[[i]][3], itemset$ItemSet[[i]][4], 
                itemset$ItemSet[[i]][5], itemset$ItemSet[[i]][6],
                sep = ", ")
  } else if (len == 7){
    msg = paste(itemset$ItemSet[[i]][1], itemset$ItemSet[[i]][2], itemset$ItemSet[[i]][3], itemset$ItemSet[[i]][4],
                itemset$ItemSet[[i]][5], itemset$ItemSet[[i]][6], itemset$ItemSet[[i]][7],
                sep = ", ")
  } else if (len == 8){
    msg = paste(itemset$ItemSet[[i]][1], itemset$ItemSet[[i]][2], itemset$ItemSet[[i]][3], itemset$ItemSet[[i]][4],
                itemset$ItemSet[[i]][5], itemset$ItemSet[[i]][6], itemset$ItemSet[[i]][7], itemset$ItemSet[[i]][8],
                sep = ", ")
  } else if (len == 9){
    msg = paste(itemset$ItemSet[[i]][1], itemset$ItemSet[[i]][2], itemset$ItemSet[[i]][3], itemset$ItemSet[[i]][4],
                itemset$ItemSet[[i]][5], itemset$ItemSet[[i]][6], itemset$ItemSet[[i]][7], itemset$ItemSet[[i]][8],
                itemset$ItemSet[[i]][9],
                sep = ", ")
  } else if (len == 10){
    msg = paste(itemset$ItemSet[[i]][1], itemset$ItemSet[[i]][2], itemset$ItemSet[[i]][3], itemset$ItemSet[[i]][4],
                itemset$ItemSet[[i]][5], itemset$ItemSet[[i]][6], itemset$ItemSet[[i]][7], itemset$ItemSet[[i]][8],
                itemset$ItemSet[[i]][9], itemset$ItemSet[[i]][10],
                sep = ", ")
  } else if (len == 11){
    msg = paste(itemset$ItemSet[[i]][1], itemset$ItemSet[[i]][2], itemset$ItemSet[[i]][3], itemset$ItemSet[[i]][4],
                itemset$ItemSet[[i]][5], itemset$ItemSet[[i]][6], itemset$ItemSet[[i]][7], itemset$ItemSet[[i]][8],
                itemset$ItemSet[[i]][9], itemset$ItemSet[[i]][10], itemset$ItemSet[[i]][11],
                sep = ", ")
  } else if (len == 12){
    msg = paste(itemset$ItemSet[[i]][1], itemset$ItemSet[[i]][2], itemset$ItemSet[[i]][3], itemset$ItemSet[[i]][4],
                itemset$ItemSet[[i]][5], itemset$ItemSet[[i]][6], itemset$ItemSet[[i]][7], itemset$ItemSet[[i]][8],
                itemset$ItemSet[[i]][9], itemset$ItemSet[[i]][10], itemset$ItemSet[[i]][11], itemset$ItemSet[[i]][12],
                sep = ", ")
  } else {
    msg = itemset$ItemSet[[i]]
  }
  
  set_list[[i]] = msg
}

transactions_data = do.call(rbind, set_list)
transaction_set <- data.frame(transactions_data)
names(transaction_set) <- "Set"

transaction_set$ID = 1:length(itemset$ItemSet)
merged_df <- merge(transactions, transaction_set, by.x = "Transaction", by.y = "ID")
merged_df$Set <- merged_df$Set %>% 
  str_remove(", NONE") %>% 
  str_remove_all("NONE, ")
merged_df$Item <- as.character(merged_df$Item)
clean_df <- merged_df %>% 
  filter(Item != "NONE") %>% 
  mutate(tf = str_detect(Set, Item)) %>% 
  mutate(Tset = case_when(
    tf == TRUE ~ Set,
    tf == FALSE ~ Item
  )) %>% 
  select(Transaction, Date, Time, Item, Tset)

transaction_set <- clean_df[!duplicated(clean_df$Transaction),] %>% 
  select(Transaction, Date, Tset)

support <- data.frame(table(transaction_set$Tset)) %>% 
  arrange(desc(Freq)) %>% 
  mutate(Support = Freq/sum(Freq))
# write.csv(support, "Support.csv")
