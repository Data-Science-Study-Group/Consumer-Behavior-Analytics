library(lubridate)
library(zoo)
library(tidyverse)
library(stringr)

#load data
data = read.csv("~/GitHub/datasci_proj/delivery_speed/olist_orders_dataset.csv")

#get the two columns we need
dates = data[,c(4,6)]

#take on the date and time and put each as a separate new column
dates$parsed_purchased_date = 
  lapply(dates[,1], function(x) (str_split_fixed(x," ",2)[1]))

dates$parsed_purchased_time = 
  lapply(dates[,1], function(x) (str_split_fixed(x," ",2)[2]))

#same as above
dates$pasred_deliver_carrier_date = 
  lapply(dates[,2], function(x) (str_split_fixed(x," ",2)[1]))

dates$pasred_deliver_carrier_time = 
  lapply(dates[,2], function(x) (str_split_fixed(x," ",2)[2]))

#calculate the difference in days
d1 = strptime(dates[,3],format = "%m/%d/%Y")
d2 = strptime(dates[,5],format = "%m/%d/%Y")
dates$day_difference = round(difftime(d1,d2, units = "days") * -1,digit = 0)

#difference in time
t1 = strptime(dates[,4], format = "%H:%M")
t2 = strptime(dates[,6], format = "%H:%M")
dates$time_difference_min = difftime(t2,t1, units = "min") #as.numeric
dates$time_difference_min = lapply(dates[,8], function(x)(if(is.na(x) != TRUE && x < 0){x * -1} else{x})) #turn all values positive


#get total difference in of days and minutes combined
temp = data[,c(1,2)] #get the first two columns
temp$customer_id = NULL #erase the second column
temp$days = dates$day_difference
temp$min = dates$time_difference_min
temp$days_min = temp$days * 1440 #24 * 60 = 1440
temp$total_min = temp$days_min + as.numeric(temp$min) #add all the minutes together

total_diff_min = temp[c(1,5)]


#seq payments
#order_items = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_order_items_dataset.csv")
pay = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_order_payments_dataset.csv")
orders = read.csv("~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_orders_dataset.csv",na.strings = c("","NA"))

library(dplyr)
merge(orders,pay,by="order_id") -> df

temp = df[,c(4,6,9,10)]
temp$parsed_purchased_date = 
  lapply(temp[,1], function(x) (str_split_fixed(x," ",2)[1]))

temp$parsed_purchased_time = 
  lapply(temp[,1], function(x) (str_split_fixed(x," ",2)[2]))

#same as above
temp$pasred_deliver_carrier_date = 
  lapply(temp[,2], function(x) (str_split_fixed(x," ",2)[1]))

temp$pasred_deliver_carrier_time = 
  lapply(temp[,2], function(x) (str_split_fixed(x," ",2)[2]))

#calculate the difference in days
d1 = strptime(temp[,5],format = "%Y-%m-%d")
d2 = strptime(temp[,7],format = "%Y-%m-%d")
temp$day_difference = round(difftime(d1,d2, units = "days") * -1,digit = 0)

#difference in time
t1 = strptime(temp[,6], format = "%H:%M")
t2 = strptime(temp[,8], format = "%H:%M")
temp$time_difference_min = difftime(t2,t1, units = "min") #as.numeric
temp$time_difference_min = lapply(temp[,10], function(x)(if(is.na(x) != TRUE && x < 0){x * -1} else{x})) #turn all values positive

#get total minutes
temp$total_min = as.numeric(temp$time_difference_min) + (temp$day_difference * 1440) #24 * 60 = 1440 minutes in a day

#temp$is_seq = temp[temp$payment_sequential == 1,]
#temp$is_seq = temp[temp$payment_sequential > 1,]

temp$is_seq = lapply(temp$payment_sequential, function(x){if(x == 1){1} else{0}})

<<<<<<< HEAD
"""
=======

>>>>>>> a6a5986991cdaa3361e2a2718be60d6f8b2e1989
#get rid of na values
non_seq$total_min = lapply(non_seq$total_min, function(x){if(is.na(x) == TRUE){0} else{x}})
seq$total_min = lapply(seq$total_min, function(x){if(is.na(x) == TRUE){0} else{x}})
mean(as.numeric(non_seq$total_min))
mean(as.numeric(seq$total_min)) """

temp %>% 
  filter(is_seq == 0) -> new
qplot(data = new ,x=payment_sequential) + 
  geom_bar()+
  coord_cartesian(ylim = c(0,20))

reg_pay <- lm(data=temp, total_min ~ payment_sequential)
plot(reg_pay)
