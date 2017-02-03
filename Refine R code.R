#Homework Assignment: 3.1 Data Wrangling Ex 1

library(readxl)
library(tidyr)
library(dummies)
library(car)

refine = read_excel("refine_original.xlsx")

#Standardize names of Company

refine$company <- recode(refine$company, "c('fillips', 'Phillips', 'phillips', 'phllips', 'phillps', 'phillipS') 
       = 'philips'")
refine$company <- recode(refine$company, "c('Akzo', 'AKZO', 'akz0', 'ak zo') = 'akzo'")
refine$company <- recode(refine$company, "c('Van Houten', 'van Houten') = 'van houten'")
refine$company <- recode(refine$company, "c('Unilever', 'unilver') = 'unilever'")

#Split column for product code
colnames(refine)[2] = paste("product_code_number")
refine <- separate(refine, product_code_number, into = c("product_code", "product_number"))

#Add product categories columns

refine$product_category <- refine$product_code
refine$product_category <- Recode(refine$product_category, "'p' = 'Smartphone';
                                'v' = 'TV'; 'x' = 'Laptop'; 'q' = 'Tablet'")

#Concatenate address field

refine$full_address <- paste(refine$address, refine$city, refine$country, sep = ", ")

#Create dummy variables for Product and Company

refine <- cbind(refine, dummy(refine$product_category, sep = "_"))
refine <- cbind(refine, dummy(refine$company, sep = "_"))
df1 <- cbind(df1, dummy(df1$year, sep = "_"))

#rename and save code

refine_clean <- refine

