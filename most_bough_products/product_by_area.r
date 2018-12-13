library(dplyr)

#loading the data
dataset = read.csv("~/GitHub/datasci_proj/most_bough_products/product_by_area.csv")

#select only rows if it has one of the top 5 states
top_states = c("SP","RJ","MG","RS","PR")
dummy = dataset[dataset$state == top_states,]

table(dummy$product_category)

#GET MOST BOUGHT CATEGORY FOR EACH STATE

#SP
sp = dummy[dummy$state == "SP",]
sp = table(sp$product_category) %>% as.data.frame() %>% arrange(desc(Freq))
sp$state = "SP" #add an extra state column

#RJ
rj = dummy[dummy$state == "RJ",]
rj = table(rj$product_category) %>% as.data.frame() %>% arrange(desc(Freq))
rj$state = "RJ"

#MG
mg = dummy[dummy$state == "MG",]
mg = table(mg$product_category) %>% as.data.frame() %>% arrange(desc(Freq))
mg$state = "MG"

#RS
rs = dummy[dummy$state == "RS",]
rs = table(rs$product_category) %>% as.data.frame() %>% arrange(desc(Freq))
rs$state = "RS"

#PR
pr = dummy[dummy$state == "PR",]
pr = table(pr$product_category) %>% as.data.frame() %>% arrange(desc(Freq))
pr$state = "PR"

#combine the top category for each state
top_category_state = sp[1,]
top_category_state = rbind(top_category_state,rj[1,])
top_category_state = rbind(top_category_state,mg[1,])
top_category_state = rbind(top_category_state,rs[1,])
top_category_state = rbind(top_category_state,pr[1,])


#GET MOST BOUGHT CATEGORY FOR EACH CITY
top_cities = c("belo horizonte","brasilia","curitiba","rio de janeiro","sao paulo")
dummy = dataset[dataset$city == top_cities,]

#belo horizonte
belo_horizonte = dummy[dummy$city == "belo horizonte",]
belo_horizonte = table(belo_horizonte$product_category) %>% as.data.frame() %>% arrange(desc(Freq))
belo_horizonte$city = "belo horizonte" #add an extra city column

#brasilia
brasilia = dummy[dummy$city == "brasilia",]
brasilia = table(brasilia$product_category) %>% as.data.frame() %>% arrange(desc(Freq))
brasilia$city = "brasilia"

#curitiba
curitiba = dummy[dummy$city == "curitiba",]
curitiba = table(curitiba$product_category) %>% as.data.frame() %>% arrange(desc(Freq))
curitiba$city = "curitiba" 

#rio de janeiro
rio_de_janeiro = dummy[dummy$city == "rio de janeiro",]
rio_de_janeiro = table(rio_de_janeiro$product_category) %>% as.data.frame() %>% arrange(desc(Freq))
rio_de_janeiro$city = "rio_de janeiro"

#sao paulo
sao_paulo = dummy[dummy$city == "sao paulo",]
sao_paulo = table(sao_paulo$product_category) %>% as.data.frame() %>% arrange(desc(Freq))
sao_paulo$city = "sao_paulo"

#combine the top from each city
top_category_city = belo_horizonte[1,]
top_category_city = rbind(top_category_city,brasilia[1,])
top_category_city = rbind(top_category_city,curitiba[1,])
top_category_city = rbind(top_category_city,rio_de_janeiro[1,])
top_category_city = rbind(top_category_city,sao_paulo[1,])





#FIND OUT HOW MUCH EACH STATE/CITY HAS SPENT

dummy = dataset[dataset$state == top_states,]

table(dummy$product_category)

#GET SPENDING FOR EACH STATE

#SP
sp = dummy[dummy$state == "SP",]
state_spending = data.frame(matrix(ncol = 2,nrow = 0))#make a new dataframe and save all the total spendings
colnames(state_spending) = c("total_spending","state")
state_spending[nrow(state_spending)+1,] = c(sum(sp$price),"SP")

#RJ
rj = dummy[dummy$state == "RJ",]
state_spending[nrow(state_spending)+1,] = c(sum(rj$price),"RJ")


#MG
mg = dummy[dummy$state == "MG",]
state_spending[nrow(state_spending)+1,] = c(sum(mg$price),"MG")

#RS
rs = dummy[dummy$state == "RS",]
state_spending[nrow(state_spending)+1,] = c(sum(rs$price),"RS")
sum(rs$price)

#PR
pr = dummy[dummy$state == "PR",]
state_spending[nrow(state_spending)+1,] = c(sum(pr$price),"PR")


#SPENDING FOR EACH CITY
top_cities = c("belo horizonte","brasilia","curitiba","rio de janeiro","sao paulo")
dummy = dataset[dataset$city == top_cities,]

#belo horizonte
belo_horizonte = dummy[dummy$city == "belo horizonte",]
city_spending = data.frame(matrix(ncol = 2,nrow = 0))#make a new dataframe and save all the total spendings
colnames(city_spending) = c("total_spending","city") #name our columns
city_spending[nrow(city_spending)+1,] = c(sum(belo_horizonte$price),"belo horizonte")

#brasilia
brasilia = dummy[dummy$city == "brasilia",]
city_spending[nrow(city_spending)+1,] = c(sum(brasilia$price),"brasilia")

#curitiba
curitiba = dummy[dummy$city == "curitiba",]
city_spending[nrow(city_spending)+1,] = c(sum(curitiba$price),"curitiba")

#rio de janeiro
rio_de_janeiro = dummy[dummy$city == "rio de janeiro",]
city_spending[nrow(city_spending)+1,] = c(sum(rio_de_janeiro$price),"rio de janeiro")

#sao paulo
sao_paulo = dummy[dummy$city == "sao paulo",]
city_spending[nrow(city_spending)+1,] = c(sum(sao_paulo$price),"sao paulo")

library(ggplot2)
ggplot(data = city_spending,
       aes(x = city, y = total_spending,fill = city)) + 
  geom_bar(stat="identity") + 
  theme(axis.title.x = element_text(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

