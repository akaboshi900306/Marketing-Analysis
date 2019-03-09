library(data.table)
setwd('D:/Emory/Marketing/Segmentation/Pernalonga')

transac = fread("transaction_table.csv",header=TRUE)
prod = fread("product_table.csv",header=TRUE)

data = merge(transac, prod, by='prod_id', all=TRUE)

# calculated columns
data[,discount_rate:= abs(tran_prod_discount_amt)/tran_prod_sale_amt]
data$tran_dt = as.Date(data$tran_dt)
data[,weekdays := weekdays(data$tran_dt)]
data[,months := month(data$tran_dt)]

# calcualted columns by grouping customers
data[,month_spending := sum(tran_prod_paid_amt), by=.(months,cust_id)]
data[,month_freq := uniqueN(tran_dt), by=.(months,cust_id)]
data[,weekday_spending := sum(tran_prod_paid_amt), by=.(weekdays,cust_id)]
data[,discount_rate := sum(abs(tran_prod_discount_amt))/sum(tran_prod_sale_amt),by=cust_id]
data[,discount_count := sum(tran_prod_discount_amt!= 0), by=cust_id]
data[,discount_freq := discount_count/.N, by=cust_id]
data[,product_types := uniqueN(prod_id), by=cust_id]
data[,total_spending := sum(tran_prod_paid_amt),by=cust_id]
data[,total_freq:= uniqueN(tran_dt)/(365*2),by=cust_id]

# customer segmentation data preparation
customers = as.data.table(unique(data$cust_id))
names(customers) = "cust_id"
# monthly spending & visiting frequency
customers[,months:=1]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
  jan_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
  jan_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=2]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          feb_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          feb_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=3]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          mar_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          mar_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=4]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          apr_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          apr_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=5]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          may_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          may_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=6]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          jun_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          jun_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=7]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          jul_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          jul_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=8]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          aug_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          aug_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=9]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          sep_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          sep_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=10]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          oct_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          oct_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=11]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          nov_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          nov_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

customers[,months:=12]
customers[data[customers, month_spending, on=.(cust_id=cust_id, months=months),by= .EACHI],
          dec_month_spending := month_spending , on=.(cust_id=cust_id, months=months)]
customers[data[customers, month_freq, on=.(cust_id=cust_id, months=months),by= .EACHI],
          dec_month_freq := month_freq , on=.(cust_id=cust_id, months=months)]

# weekday spending
customers[,weekdays:="Monday"]
customers[data[customers, weekday_spending, on=.(cust_id=cust_id, weekdays=weekdays),by= .EACHI],
          Monday_spending := weekday_spending , on=.(cust_id=cust_id, weekdays=weekdays)]

customers[,weekdays:="Tuesday"]
customers[data[customers, weekday_spending, on=.(cust_id=cust_id, weekdays=weekdays),by= .EACHI],
          Tuesday_spending := weekday_spending , on=.(cust_id=cust_id, weekdays=weekdays)]

customers[,weekdays:="Wednesday"]
customers[data[customers, weekday_spending, on=.(cust_id=cust_id, weekdays=weekdays),by= .EACHI],
          Wednesday_spending := weekday_spending , on=.(cust_id=cust_id, weekdays=weekdays)]

customers[,weekdays:="Thursday"]
customers[data[customers, weekday_spending, on=.(cust_id=cust_id, weekdays=weekdays),by= .EACHI],
          Thursday_spending := weekday_spending , on=.(cust_id=cust_id, weekdays=weekdays)]

customers[,weekdays:="Friday"]
customers[data[customers, weekday_spending, on=.(cust_id=cust_id, weekdays=weekdays),by= .EACHI],
          Friday_spending := weekday_spending , on=.(cust_id=cust_id, weekdays=weekdays)]

customers[,weekdays:="Saturday"]
customers[data[customers, weekday_spending, on=.(cust_id=cust_id, weekdays=weekdays),by= .EACHI],
          Saturday_spending := weekday_spending , on=.(cust_id=cust_id, weekdays=weekdays)]

customers[,weekdays:="Sunday"]
customers[data[customers, weekday_spending, on=.(cust_id=cust_id, weekdays=weekdays),by= .EACHI],
          Sunday_spending := weekday_spending , on=.(cust_id=cust_id, weekdays=weekdays)]

# overall discount rate
customers[data[customers, discount_rate, on=.(cust_id=cust_id),by= .EACHI],
          discount_rate := discount_rate , on=.(cust_id=cust_id)]

# overall discount frequency
customers[data[customers, discount_freq, on=.(cust_id=cust_id),by= .EACHI],
          discount_freq := discount_freq , on=.(cust_id=cust_id)]

# kinds of products bought by customer
customers[data[customers, product_types, on=.(cust_id=cust_id),by= .EACHI],
          product_types := product_types , on=.(cust_id=cust_id)]

# total spending
customers[data[customers, total_spending, on=.(cust_id=cust_id),by= .EACHI],
          total_spending := total_spending , on=.(cust_id=cust_id)]

# total freq
customers[data[customers, total_freq, on=.(cust_id=cust_id),by= .EACHI],
         total_freq := total_freq , on=.(cust_id=cust_id)]

write.csv(customers, file = "customers.csv",row.names=FALSE)

#customers = fread("customers.csv",header=TRUE)
