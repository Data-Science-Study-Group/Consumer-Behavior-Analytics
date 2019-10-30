#using dplyr for data manipulation
library("dplyr")

#importing data from project github for public accessibility
df_geo_loc = read.csv("https://raw.githubusercontent.com/troykirin/datasci_proj/master/brazilian-ecommerce/geolocation_olist_public_dataset.csv")
df_olist_classified = read.csv("https://raw.githubusercontent.com/troykirin/datasci_proj/master/brazilian-ecommerce/olist_classified_public_dataset.csv")
df_olist_v2 = read.csv("https://raw.githubusercontent.com/troykirin/datasci_proj/master/brazilian-ecommerce/olist_public_dataset_v2.csv")
df_olist_v2_customers = read.csv("https://raw.githubusercontent.com/troykirin/datasci_proj/master/brazilian-ecommerce/olist_public_dataset_v2_customers.csv")
df_olist_v2_payments = read.csv("https://raw.githubusercontent.com/troykirin/datasci_proj/master/brazilian-ecommerce/olist_public_dataset_v2_payments.csv")
df_product_category_name_translation = read.csv("https://raw.githubusercontent.com/troykirin/datasci_proj/master/brazilian-ecommerce/product_category_name_translation.csv")

names(df_geo_loc)

#these two df seem to be correlated although utilizes different column names, will have to clean
names(df_olist_classified)
names(df_olist_v2)

names(df_olist_v2_customers)
names(df_olist_v2_payments)
names(df_product_category_name_translation)

sum(is.na(df_geo_loc))
sum(is.na(df))