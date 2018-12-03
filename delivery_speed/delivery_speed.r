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

