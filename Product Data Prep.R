if(!require("data.table")) { install.packages("data.table"); require("data.table") }
if(!require("plyr")) { install.packages("plyr"); require("plyr") }
if(!require("tidyverse")) { install.packages("tidyverse"); require("tidyverse") }
if(!require("factoextra")) { install.packages("factoextra"); require("factoextra") }


product <- fread("C:/Users/Guest User/Desktop/product_table.csv")
transaction <- fread("C:/Users/Guest User/Desktop/transaction_table.csv")
transaction$weekday<- weekdays(as.Date(transaction$tran_dt))
transaction$month<- month(as.Date(transaction$tran_dt))


#Calculate the customers by sale_amount, sale_quantity and find the max customer_id
transaction_customer <- transaction[, .(sum(tran_prod_sale_amt),sum(tran_prod_sale_qty)),by=.(cust_id,prod_unit)][order(cust_id)]
colnames(transaction_customer)[3:4]<-c("sale","num")
 which.max(transaction_customer$sale) #12109
 
 
#transaction_product<- transaction[,.(.N,length(unique(cust_id)),sum(tran_prod_sale_amt),sum(tran_prod_paid_amt)),by=.(prod_id,prod_unit)][order(prod_id)]
#colnames(transaction_product)[3:6]<-c("volume","customers","revenue","profit")
#define each row of transacion(>1) as one transaction
transaction1<- copy(transaction)
transaction1[transaction1$tran_prod_offer_cts>1,10]="1"

#caculate the "mean_price","sum_sale_quantity","freq_of _discount_number","discount_ration" using data.table
transaction_product1<- transaction1[,.(sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty),sum(tran_prod_sale_qty),
                                       (sum(tran_prod_offer_cts)/.N),(sum(tran_prod_discount_amt)/sum(tran_prod_sale_amt))),
                                    by=.(prod_id,prod_unit)][order(prod_id)]
colnames(transaction_product1)[3:6]<-c("mean_price","sum_sale_quantity","freq_of _discount_number","discount_ration")


#Find the weekend transaction ratio and merge the data into transaction_final
transaction2 <- transaction1[weekday=="Sunday"|weekday=="Saturday",.N,by=.(prod_id,prod_unit)]
transaction3 <- transaction1[,.N,by=.(prod_id,prod_unit)]
transaction_final <- merge(transaction2,transaction3,by=c("prod_id","prod_unit"))
transaction_final$Weekend <- transaction_final$N.x/transaction_final$N.y
transaction_final <- transaction_final[,c(1,2,5)]
transaction_final <- merge(transaction_final,transaction_product1,by=c("prod_id","prod_unit"))
transaction_final <- merge(transaction_final,product[,c(1,6,7)],by="prod_id")

#write out the csv and put into python
write.csv(transaction_final, file = "transaction_final.csv",row.names=FALSE, na="")
