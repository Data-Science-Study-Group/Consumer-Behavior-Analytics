library(tidyverse)
library(dplyr)

#load dataset
data = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_customers_dataset.csv")
orders = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_order_items_dataset.csv")

#find out most freq customers by state
state = table(data$customer_state)
state = as.data.frame(state)
freq_state = state[with(state,order(-Freq)),]

freq_state$ID <- seq.int(nrow(freq_state)) #create index column
rownames(freq_state) = freq_state[,3]#replace column 0 with index column
freq_state$ID <- NULL #erase our index column

#only plot the top 5
freq_state[1:5,] -> plot_state_freq
names(plot_state_freq) = c("State", "Frequency")
ggplot(data=plot_state_freq,aes(x=State,y=Frequency)) + geom_bar(stat="identity")
plot_state_freq %>% arrange(Frequency) -> plot_state_freq
ggplot(data=plot_state_freq,aes(x=reorder(State,-Frequency),y=Frequency)) + geom_col() + ggtitle("Most Frequent Purchases by State")
  
#find out freq customers by city
cities = table(data$customer_city)
cities = as.data.frame(cities)
freq_cities = cities[with(cities,order(-Freq)),]

freq_cities$ID <- seq.int(nrow(freq_cities)) #create index column
rownames(freq_cities) = freq_cities[,3]#replace column 0 with index column
freq_cities$ID <- NULL #erase our index column



#bar graph of top 5 cities
freq_cities[1:5,] -> plot_city
names(plot_city) = c("City","Frequency")
ggplot(plot_city,aes(x=reorder(City,-Frequency),y=Frequency)) + geom_col() + ggtitle("Most Frequent Purchases by City")

#Regression of state sales
df_state_sales <- data.frame(data$customer_state, orders$price)
df_state_sales
