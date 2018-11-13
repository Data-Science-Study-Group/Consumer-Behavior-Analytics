customers = readxl::read_xls(path = "~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_customers_dataset.xls")
View(customers)
summary(customers)
plot(customers)

geolocation = readxl::read_xls(path = "~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_geolocation_dataset.xls")
View(geolocation)
summary(geolocation)
plot(geolocation)

order_items = readxl::read_xls(path = "~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_order_items_dataset.xls")
View(order_items)
summary(order_items)
plot(order_items)

order_payments = readxl::read_xls(path = "~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_order_payments_dataset.xls")
View(order_payments)
summary(order_payments)
plot(order_payments)

order_reviews = readxl::read_xls(path = "~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_order_reviews_dataset.xls")
View(order_reviews)
summary(order_reviews)
plot(order_reviews)

orders = readxl::read_xls(path = "~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_orders_dataset.xls")
View(orders)
summary(orders)
plot(orders)

#products
products = readxl::read_xls(path = "~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_products_dataset.xls")
View(products)
summary(products)
plot(products)

#sellers
sellers = readxl::read_xls(path = "~/GitHub/datasci_proj/brazilian-ecommerce_ver6/olist_sellers_dataset.xls")
View(sellers)
summary(sellers)
plot(sellers)

#product_category
product_category = readxl::read_xls(path = "~/GitHub/datasci_proj/brazilian-ecommerce_ver6/product_category_name_translation.xls")
View(product_category)
summary(product_category)
plot(product_category)



