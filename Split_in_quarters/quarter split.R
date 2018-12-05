library(ggplot2)
library(data.table)

orders = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_orders_dataset.csv",na.strings = c("","NA"))
customers = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_customers_dataset.csv")
order_items = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_order_items_dataset.csv")
product_category = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_products_dataset.csv")
product_trans = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/product_category_name_translation.csv")

#merge
merge(orders,order_items, by="order_id") -> df
df<- merge(df, customers, by="customer_id")

merge(df, product_category, by="product_id") -> df
merge(df, product_trans, by="product_category_name") -> df
#

#Split into quarters

#split purchase by date and time
df$order_purchase_timestamp %>%
  str_split_fixed(pattern = " ", n=2) -> df.time

#append table back onto df
df %>%
  mutate(df.time[,1]) -> df

df %>%
  mutate(df.time[,2]) -> df

#create frequency table of num of items bought in a day
table(df$`df.time[, 1]`) %>%
  as.data.frame() -> new_new

#filter top by cat
table(df$product_category_name_english) %>%
  as.data.frame() -> new_cat
new_cat %>%
  arrange(-Freq) -> new_cat
new_cat = new_cat[1:5,]
#
#rename col
names(new_cat) = c("product_category_name_english","Freq_Prod")
#

#merge back
left_join(x=df,y=new_cat,by="product_category_name_english") -> df
#

#filter out na
df %>%
  filter(!is.na(Freq_Prod)) -> df
#

#filter out categories == 0
df_temp <- subset(x = df, df$Freq_Prod != 0)


#convert to date type
df$`df.time[, 1]` <- as.Date(df$`df.time[, 1]`, format= "%Y-%m-%d")
new_new$Day_Pur <- as.Date(new_new$Day_Pur, format = "%Y-%m-%d")

#rename new_new
#
setnames(df, old=c("df.time[, 1]"),new=c("Day_Pur"))
names(new_new) = c("Day_Pur","Freq")
#
View(new_new)
View(df)
#

#keep only top 5
new_new %>%
  arrange(-Freq) -> new_new
new_new[1:5,] -> new_new
#

##join
left_join(x=df,y=new_new,by="Day_Pur") -> df
View(df)
##

#remove rows with NA
df %>% 
  filter(!is.na(Freq)) -> df
#

#add quarter column

##

##filter only top 5 categories

#


#plotting new_new for a freq table instead of regression to price
qplot(data=df_temp,x=as.numeric(Day_Pur),y=Freq,color=df_temp$product_category_name_english) + 
  geom_point() +
  geom_smooth(method = "glm",color="red")+
  ggtitle("")+
  labs(x="Purchased Date",y="Number of Products Bought")

reg <- lm(data=new_new, Freq ~  as.numeric(Var1))
summary(reg)
plot(reg)
##

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

#2018
subset(df, df$`df.time[, 1]`  > "2018-01-01" & df$`df.time[, 1]` < "2018-03-31") -> df_q1_2018

subset(df, df$`df.time[, 1]` > "2018-04-01" & df$`df.time[, 1]` < "2018-6-30") -> df_q2_2018

subset(df, df$`df.time[, 1]` > "2018-07-01" & df$`df.time[, 1]` < "2018-09-30") -> df_q3_2018

subset(df, df$`df.time[, 1]` > "2018-10-01" & df$`df.time[, 1]` < "2018-12-31") -> df_q4_2018

#end sub set

#main_df plot of 2017
rbind(df_q1_2017,df_q2_2017) -> main_df
rbind(main_df,df_q3_2017) -> main_df
rbind(main_df,df_q4_2017) -> main_df
##

#fitler by category
main_df %>%
  filter(main_df$product_category_name_english %in% "health_beauty") -> main_df_h_and_b

main_df %>%
  filter(main_df$product_category_name_english %in% "computers_accessories") -> main_df_comp_ass

main_df %>%
  filter(main_df$product_category_name_english %in% "sports_leisure") -> main_df_sport_lei
##

#plot by category

#test
x = as.numeric(main_df_h_and_b$order_purchase_timestamp)
y=log(main_df_h_and_b$price)

plot(x,y)

ggplot(data = main_df_h_and_b,
       aes(x = as.numeric(order_purchase_timestamp),
           y = price) +
         geom_point(aes(color=group)) +
         geom_smooth(method = 'glm', aes(color=group),fill="black") +
         ggtitle("Regression of 2017 Health and Beauty") +
         theme(plot.title = element_text(hjust = 0.5)) +
         labs(x="Purchased",y="Price")+
         coord_cartesian(ylim=c(0, 250))
#test

#health and beauty
ggplot(data = main_df_h_and_b,
       aes(x = as.numeric(order_purchase_timestamp),
           y = price) +
  geom_point(aes(color=group)) +
  geom_smooth(method = 'glm', aes(color=group),fill="black") +
  ggtitle("Regression of 2017 Health and Beauty") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Purchased",y="Price")+
  coord_cartesian(ylim=c(0, 250))

#reg
reggresor <- lm(data = main_df_h_and_b, (price) ~ as.numeric(order_purchase_timestamp))
summary(reggresor)
plot(reggresor)
##

#

#computer accessories
ggplot(data = main_df_comp_ass,
       aes(x = as.numeric(order_purchase_timestamp),
           y = log(price))) +
  geom_point(aes(color=group)) +
  geom_smooth(method = 'glm', aes(color=group),fill="black") +
  ggtitle("Regression of 2017 Computer Accessories") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Purchased",y="Price")
#reg
reggresor <- lm(data = main_df_comp_ass, log(price) ~ as.numeric(order_purchase_timestamp))
summary(reggresor)
plot(reggresor)
##

##

#sports_leisure
ggplot(data = main_df_sport_lei,
       aes(x = as.numeric(order_purchase_timestamp),
           y = log(price))) +
  geom_point(aes(color=group)) +
  geom_smooth(method = 'glm', aes(color=group),fill="black") +
  ggtitle("Regression of 2017 Sports Leisure") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Purchased",y="Price")
#reg
reggresor <- lm(data = main_df_sport_lei, log(price) ~ as.numeric(order_purchase_timestamp))
summary(reggresor)
plot(reggresor)
##

##

##end plots

#%s/df_q1_2017/df_q1_2017_h_and_b/gc
#plots
ggplot(data = df_q1_2017,aes(x = as.numeric(df_q1_2017$order_purchase_timestamp), y = df_q1_2017$price)) + geom_point(color='blue') + geom_smooth(method = "lm", se = FALSE)
lm(data = df_q1_2017, price ~ order_purchase_timestamp)

df_q1_2017 %>%
  filter(df_q1_2017$product_category_name_english %in% "health_beauty") -> df_q1_2017_h_and_b

#try below func as qplot instead
qplot(data=df_q1_2017_h_and_b,x=as.numeric(df_q1_2017_h_and_b$order_purchase_timestamp),y=df_q1_2017_h_and_b$price, geom = c("point","smooth"))
#

ggplot(data = df_q1_2017_h_and_b,
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