library(ggplot2)

orders = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_orders_dataset.csv",na.strings = c("","NA"))
customers = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_customers_dataset.csv")
order_items = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_order_items_dataset.csv")

merge(orders,order_items, by="order_id") -> df
df<- merge(df, customers, by="customer_id")

#Split into quarters

#split
df$order_purchase_timestamp %>%
  str_split_fixed(pattern = " ", n=2) -> df.time

length(df.time)

#append on
df %>%
  mutate(df.time[,1]) -> df

df %>%
  mutate(df.time[,2]) -> df

#convert to date
df$`df.time[, 1]` <- as.Date(df$`df.time[, 1]`, format= "%Y-%m-%d")

#sort out only 2016
subset(df, df$`df.time[, 1]`  > "2016-01-01" & df$`df.time[, 1]` < "2016-03-31") -> df_q1_2016

subset(df, df$`df.time[, 1]` > "2016-04-01" & df$`df.time[, 1]` < "2016-6-30") -> df_q2_2016

subset(df, df$`df.time[, 1]` > "2016-07-01" & df$`df.time[, 1]` < "2016-09-30") -> df_q3_2016

subset(df, df$`df.time[, 1]` > "2016-10-01" & df$`df.time[, 1]` < "2016-12-31") -> df_q4_2016

#2017
subset(df, df$`df.time[, 1]`  > "2017-01-01" & df$`df.time[, 1]` < "2017-03-31") -> df_q1_2017

subset(df, df$`df.time[, 1]` > "2017-04-01" & df$`df.time[, 1]` < "2017-6-30") -> df_q2_2017

subset(df, df$`df.time[, 1]` > "2017-07-01" & df$`df.time[, 1]` < "2017-09-30") -> df_q3_2017

subset(df, df$`df.time[, 1]` > "2017-10-01" & df$`df.time[, 1]` < "2017-12-31") -> df_q4_2017

#2018
subset(df, df$`df.time[, 1]`  > "2018-01-01" & df$`df.time[, 1]` < "2018-03-31") -> df_q1_2018

subset(df, df$`df.time[, 1]` > "2018-04-01" & df$`df.time[, 1]` < "2018-6-30") -> df_q2_2018

subset(df, df$`df.time[, 1]` > "2018-07-01" & df$`df.time[, 1]` < "2018-09-30") -> df_q3_2018

subset(df, df$`df.time[, 1]` > "2018-10-01" & df$`df.time[, 1]` < "2018-12-31") -> df_q4_2018


#plots
ggplot(data = df_q1_2017,aes(x = df_q1_2017$order_purchase_timestamp, y = df_q1_2017$price)) + geom_point(color='blue') + geom_smooth(method = "lm", se = FALSE)
lm(data = df_q1_2017, price ~ order_purchase_timestamp)
