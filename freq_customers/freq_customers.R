#load dataset
data = read.csv("C:/Users/Carlos/Desktop/Project/ver 6/olist_customers_dataset.csv")

#find out most freq customers by state
state = table(data$customer_state)
state = as.data.frame(state)
freq_state = state[with(state,order(-Freq)),]

freq_state$ID <- seq.int(nrow(freq_state)) #create index column
rownames(freq_state) = freq_state[,3]#replace column 0 with index column
freq_state$ID <- NULL #erase our index column



#find out freq customers by city
cities = table(data$customer_city)
cities = as.data.frame(cities)
freq_cities = cities[with(cities,order(-Freq)),]

freq_cities$ID <- seq.int(nrow(freq_cities)) #create index column
rownames(freq_cities) = freq_cities[,3]#replace column 0 with index column
freq_cities$ID <- NULL #erase our index column


