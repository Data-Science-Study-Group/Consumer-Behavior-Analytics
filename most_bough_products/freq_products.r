#loading the data
dataset = read.csv("~/GitHub/datasci_proj/most_bough_products/dataset.csv")

#FINDING THE MOST BOUGHT PRODUCT
products = table(dataset$product_category)
products = as.data.frame(products)
freq_product = products[with(products,order(-Freq)),]

freq_product$ID = seq.int(nrow(freq_product))#set an additional index column
rownames(freq_product) = freq_product[,3]#change column 0 to index column
freq_product$ID = NULL#erase index column



#FINDING THE HOW MUCH CONSUMERS HAVE SPENT ON EACH CATEGORY
#copy over the category and prices variables.
values = dataset[,c(4,7)]
colnames(values)[2] = "total" 
totals = aggregate(total ~ product_category + total, data = values, sum)

total_by_category = totals[with(totals,order(-total)),]

total_by_category$ID <- seq.int(nrow(total_by_category)) #create index column
rownames(total_by_category) = total_by_category[,3]#replace column 0 with index column
total_by_category$ID <- NULL #erase our index column

#PIE CHART of top 5
total_by_category$total = round(total_by_category$total)
numbers = as.numeric(total_by_category[1:5,2])
labels = total_by_category[1:5,1] #get the labels of the top 5
piepercent =  round(100*numbers/sum(numbers), 1)#to get the percentage on the pie chart
pie(as.numeric(total_by_category[1:5,2]),labels = piepercent,main = "Top 5 bought categories")

#Bar chart top 5
library(ggplot2)
ggplot(data = total_by_category[1:5,], 
       aes(x = product_category, y = total,fill = product_category)) + 
        geom_bar(stat="identity") + 
          theme(axis.title.x = element_text(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
       