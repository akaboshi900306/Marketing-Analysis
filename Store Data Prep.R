library(data.table)
library(ggplot2)
setwd('D:/Emory/Marketing/Segmentation/Pernalonga')

transac = fread("transaction_table.csv",header=TRUE)
prod = fread("product_table.csv",header=TRUE)

data = merge(transac, prod, by='prod_id', all=TRUE)
data$tran_dt = as.Date(data$tran_dt)
data[,weekdays := weekdays(data$tran_dt)]

# discount rate by stores
data[,discount_rate := sum(abs(tran_prod_discount_amt))/sum(tran_prod_sale_amt),by=store_id]
# discount freq
data[,discount_count := sum(tran_prod_discount_amt!= 0), by=store_id]
data[,discount_freq := discount_count/.N, by=store_id]
# total sales
data[,total_sales := sum(tran_prod_paid_amt),by=store_id]
# product types
data[,product_types := uniqueN(prod_id), by=store_id]
# weekend frequency
data[,weekend_visits := sum(weekdays =="Saturday" | weekdays == "Sunday"), by=.(cust_id,store_id)]
data[,total_visits := sum(weekdays =="Monday" | weekdays == "Tuesday"|
                            weekdays =="Wednesday" | weekdays == "Thursday"| weekdays =="Friday"|
                            weekdays =="Saturday" | weekdays == "Sunday"), by=.(cust_id,store_id)]
data[,weekend_freq := weekend_visits/total_visits]

# store segmentation data preparation
stores = as.data.table(unique(data$store_id))
names(stores) = "store_id"
stores[data[stores, discount_rate, on=.(store_id=store_id),by= .EACHI],
          discount_rate := discount_rate , on=.(store_id=store_id)]
stores[data[stores, discount_freq, on=.(store_id=store_id),by= .EACHI],
       discount_freq := discount_freq , on=.(store_id=store_id)]
stores[data[stores, total_sales, on=.(store_id=store_id),by= .EACHI],
       total_sales := total_sales , on=.(store_id=store_id)]
stores[data[stores, product_types, on=.(store_id=store_id),by= .EACHI],
       product_types := product_types , on=.(store_id=store_id)]
stores[data[stores, weekend_freq, on=.(store_id=store_id),by= .EACHI],
       weekend_freq := weekend_freq , on=.(store_id=store_id)]

write.csv(stores, file = "stores_data.csv",row.names=FALSE)

