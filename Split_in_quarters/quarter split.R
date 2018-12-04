library(ggplot2)

orders = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_orders_dataset.csv",na.strings = c("","NA"))
customers = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_customers_dataset.csv")
order_items = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_order_items_dataset.csv")
product_category = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_products_dataset.csv")
product_trans = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/product_category_name_translation.csv")

merge(orders,order_items, by="order_id") -> df
df<- merge(df, customers, by="customer_id")

merge(df, product_category, by="product_id") -> df
merge(df, product_trans, by="product_category_name") -> df
View(df)

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
df_q1_2017$group = "q1_2017"

subset(df, df$`df.time[, 1]` > "2017-04-01" & df$`df.time[, 1]` < "2017-6-30") -> df_q2_2017
df_q2_2017$group = "q2_2017"

subset(df, df$`df.time[, 1]` > "2017-07-01" & df$`df.time[, 1]` < "2017-09-30") -> df_q3_2017
df_q3_2017$group = "q3_2017"

subset(df, df$`df.time[, 1]` > "2017-10-01" & df$`df.time[, 1]` < "2017-12-31") -> df_q4_2017
df_q4_2017$group = "q4_2017"

rbind(df_q1_2017,df_q2_2017) ->main_df
rbind(main_df,df_q3_2017) -> main_df
rbind(main_df,df_q4_2017) -> main_df

View(main_df)

main_df %>%
  filter(main_df$product_category_name_english %in% "health_beauty") -> main_df_h_and_b

ggplot(data = main_df,
       aes(x = as.numeric(main_df$order_purchase_timestamp),
           y = main_df$price)) +
  geom_point() +
  geom_smooth(method = 'glm', aes(color=group)) +
  ggtitle("Regression of 2017 Health and Beauty") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Purchased",y="Price")


#2018
subset(df, df$`df.time[, 1]`  > "2018-01-01" & df$`df.time[, 1]` < "2018-03-31") -> df_q1_2018

subset(df, df$`df.time[, 1]` > "2018-04-01" & df$`df.time[, 1]` < "2018-6-30") -> df_q2_2018

subset(df, df$`df.time[, 1]` > "2018-07-01" & df$`df.time[, 1]` < "2018-09-30") -> df_q3_2018

subset(df, df$`df.time[, 1]` > "2018-10-01" & df$`df.time[, 1]` < "2018-12-31") -> df_q4_2018

#%s/df_q1_2017/df_q1_2017_h_and_b/gc
#plots
df_q1_2017 %>%
  filter(df_q1_2017$product_category_name_english %in% "health_beauty") -> df_q1_2017_h_and_b

qplot(data=df_q1_2017_h_and_b,x=as.numeric(df_q1_2017_h_and_b$order_purchase_timestamp),y=df_q1_2017_h_and_b$price, geom = c("point","smooth"))

qplot(data = df_q1_2017_h_and_b,
       aes(x = as.numeric(df_q1_2017_h_and_b$order_purchase_timestamp),
           y = df_q1_2017_h_and_b$price), geom = c("point","smooth")) +
  ggtitle("Regression of Q1-2017 Health and Beauty") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Purchased",y="Price")

reggresor <- lm(data = df_q1_2017_h_and_b, (price) ~ as.numeric(order_purchase_timestamp))
summary(reggresor)
#

#df_q2_2017_h_and_b
df_q2_2017 %>%
  filter(df_q2_2017$product_category_name_english %in% "health_beauty") -> df_q2_2017_h_and_b

ggplot(data = df_q2_2017_h_and_b,
       aes(x = as.numeric(df_q2_2017_h_and_b$order_purchase_timestamp),
           y = df_q2_2017_h_and_b$price)) +
  geom_point(color='red') +
  geom_smooth(method = 'glm') +
  ggtitle("Regression of Q2-2017 Health and Beauty") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Purchased",y="Price")

reggresor <- lm(data = df_q2_2017_h_and_b, (price) ~ as.numeric(order_purchase_timestamp))
summary(reggresor)
#

#df_q3_2017_h_and_b
df_q3_2017 %>%
  filter(df_q3_2017$product_category_name_english %in% "health_beauty") -> df_q3_2017_h_and_b

ggplot(data = df_q3_2017_h_and_b,
       aes(x = as.numeric(df_q3_2017_h_and_b$order_purchase_timestamp),
           y = df_q3_2017_h_and_b$price)) +
  geom_point(color='red') +
  geom_smooth(method = 'glm') +
  ggtitle("Regression of Q3-2017 Health and Beauty") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Purchased",y="Price")

reggresor <- lm(data = df_q3_2017_h_and_b, (price) ~ as.numeric(order_purchase_timestamp))
summary(reggresor)
#

#df_q4_2017_h_and_b
df_q4_2017 %>%
  filter(df_q4_2017$product_category_name_english %in% "health_beauty") -> df_q4_2017_h_and_b

ggplot(data = df_q4_2017_h_and_b,
       aes(x = as.numeric(df_q4_2017_h_and_b$order_purchase_timestamp),
           y = df_q4_2017_h_and_b$price)) +
  geom_point(color='red') +
  geom_smooth(method = 'glm') +
  ggtitle("Regression of Q4-2017 Health and Beauty") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Purchased",y="Price")

reggresor <- lm(data = df_q4_2017_h_and_b, (price) ~ as.numeric(order_purchase_timestamp))
summary(reggresor)
#

