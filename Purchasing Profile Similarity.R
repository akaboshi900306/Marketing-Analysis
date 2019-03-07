library(data.table)
library(recommenderlab)
setwd("C:/Users/Guest User/Desktop")

transac = fread("transaction_table.csv",header=TRUE)
prod = fread("product_table.csv",header=TRUE)

data = merge(transac, prod, by='prod_id')
data[brand_desc=="HEINEKEN",sum(tran_prod_offer_cts)/(.N),by= brand_desc]
customer_cluster = fread("customer_cluster.csv", header=TRUE)
bigtable = merge(data, customer_cluster, by="cust_id",all.x=TRUE)
data[brand_desc=="HEINEKEN",sum(tran_prod_offer_cts)/(.N),by= brand_desc]



# looking at customers in three segments based on previous project
segment1 = bigtable[cluster==1,]
segment2 = bigtable[cluster==2,]
segment3 = bigtable[cluster==3,]

# looking at segment 1 first: loyal customers----------------

# get the purchasing profile for each customer (customer vs. brand)
customer_brand_mat1 = dcast(segment1, cust_id ~ brand_desc, fun.aggregate = function(x) 1, fill =0)

# customers who bought Heineken
H1 = customer_brand_mat1[HEINEKEN==1,]
H1 = subset(H1, select = -c(HEINEKEN,`SUPER BOCK`) )
# only bought superbock
SB1 = customer_brand_mat1[`SUPER BOCK`==1 & HEINEKEN==0,]
SB1 = subset(SB1, select = -c(HEINEKEN,`SUPER BOCK`) )

# user by user similarity using recommenderlab similarity
# convert to matrix format first
mat_H1 = as(as.matrix(H1),"realRatingMatrix")
mat_SB1 = as(as.matrix(SB1),"realRatingMatrix")

# similarity bewteen SB-only buyers and Heineken buyers
similarity1 = similarity(mat_SB1,mat_H1,method = "jaccard")
similarity1 = as.data.frame(as(similarity1,"matrix"))
# average similarity for each SB only buyer comparing to all Heineiken only buyer
similarity1$avg = rowMeans(similarity1[,1:nrow(mat_H1)]) 
mean(similarity1$avg)
# create a dataframe show all SB only buyers and their similariy scores
similarity_segment1 = data.frame(SB1$cust_id,similarity1$avg)
names(similarity_segment1) = c("cust_id","similarity score")
summary(similarity_segment1$`similarity score`)
hist(similarity_segment1$`similarity score`)

# looking at segment 2: cherry-pickers -----------------------

# get the purchasing profile for each customer (customer vs. brand)
customer_brand_mat2 = dcast(segment2, cust_id ~ brand_desc, fun.aggregate = function(x) 1, fill =0)

# customers who bought Heineken
H2 = customer_brand_mat2[HEINEKEN==1,]
H2 = subset(H2, select = -c(HEINEKEN,`SUPER BOCK`) )
# only bought superbock
SB2 = customer_brand_mat2[`SUPER BOCK`==1 & HEINEKEN==0,]
SB2 = subset(SB2, select = -c(HEINEKEN,`SUPER BOCK`) )

# user by user similarity using recommenderlab similarity
# convert to matrix format first
mat_H2 = as(as.matrix(H2),"realRatingMatrix")
mat_SB2 = as(as.matrix(SB2),"realRatingMatrix")

# similarity bewteen SB-only buyers and Heineken buyers
similarity2 = similarity(mat_SB2,mat_H2,method = "jaccard")
similarity2 = as.data.frame(as(similarity2,"matrix"))
# average similarity for each SB only buyer comparing to all Heineiken only buyer
similarity2$avg = rowMeans(similarity2[,1:nrow(mat_H2)]) 

# create a dataframe show all SB only buyers and their similariy scores
similarity_segment2 = data.frame(SB2$cust_id,similarity2$avg)
names(similarity_segment2) = c("cust_id","similarity score")
summary(similarity_segment2$`similarity score`)
hist(similarity_segment2$`similarity score`)

# looking at segment 3: average customers --------------------

# get the purchasing profile for each customer (customer vs. brand)
customer_brand_mat3 = dcast(segment3, cust_id ~ brand_desc, fun.aggregate = function(x) 1, fill =0)

# customers who bought Heineken
H3 = customer_brand_mat3[HEINEKEN==1,]
H3 = subset(H3, select = -c(HEINEKEN,`SUPER BOCK`) )
# only bought superbock
SB3 = customer_brand_mat3[`SUPER BOCK`==1 & HEINEKEN==0,]
SB3 = subset(SB3, select = -c(HEINEKEN,`SUPER BOCK`) )

# user by user similarity using recommenderlab similarity
# convert to matrix format first
mat_H3 = as(as.matrix(H3),"realRatingMatrix")
mat_SB3 = as(as.matrix(SB3),"realRatingMatrix")

# similarity bewteen SB-only buyers and Heineken buyers
similarity3 = similarity(mat_SB3,mat_H3,method = "jaccard")
similarity3 = as.data.frame(as(similarity3,"matrix"))
# average similarity for each SB only buyer comparing to all Heineiken only buyer
similarity3$avg = rowMeans(similarity3[,1:nrow(mat_H3)]) 

# create a dataframe show all SB only buyers and their similariy scores
similarity_segment3 = data.frame(SB3$cust_id,similarity3$avg)
names(similarity_segment3) = c("cust_id","similarity score")
summary(similarity_segment3$`similarity score`)
hist(similarity_segment3$`similarity score`)





