library(lubridate)
library(zoo)
library(tidyverse)
library(stringr)

#load data
data = read.csv("C:/Users/Carlos/Desktop/Project/Seq payments/olist_orders_dataset.csv")

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

#example on how to take the difference in days off two dates.
#date_strings = c("1/2/2013", "1/3/2013")
#datetimes = strptime(date_strings, format = "%m/%d/%Y")
#difftime(datetimes[1], datetimes[2], units = "days") # days

#calculate the difference in days
d1 = strptime(dates[,3],format = "%m/%d/%Y")
d2 = strptime(dates[,5],format = "%m/%d/%Y")
dates$day_difference = round(difftime(d1,d2, units = "days") * -1,digit = 0)

#difference in time
t1 = strptime(dates[,4], format = "%H:%M")
t2 = strptime(dates[,6], format = "%H:%M")
dates$time_difference_min = difftime(t2,t1, units = "min") #as.numeric
dates$time_difference_min = lapply(dates[,8], function(x)(if(is.na(x) != TRUE && x < 0){x * -1} else{x})) #turn all values positive


#ORDER_ID AND TOTAL MINUTES
dummy = data[c(1)] #get order_id column
dummy$days = dates$day_difference #get diff days column
dummy$time = dates$time_difference_min #get diff min column
dummy$days_minutes = dummy$days * 1440 #24 * 60 = 1440 minutes in a day
dummy$total_minutes = dummy$days_minutes + as.numeric(dummy$time)

#take only the order id the total minutes
final = dummy[c(1,5)]
plot(final)
