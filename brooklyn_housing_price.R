### Created by Sakshi Shende

#install.packages("zoo")
library(dplyr)
library(tidyverse)
library(stringr)
library(base)
library(corrplot)
library(Hmisc)
library(zoo)
library(lmtest)
library(ggplot2)
library(olsrr)

################################################################################
#
# Step 1: Import and prepare the data for analysis
#
################################################################################

################# 1.1 Bring the data into R ####################################

#import data and renaming the columns
b2016 = read.csv("2016_brooklyn.csv", header = TRUE, skip = 4)
names(b2016) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

#removing 'comma', 'hypen', errorenous data from each column and changing the data type
b2016$aptnum = gsub('3-Feb','',b2016$aptnum)
b2016$aptnum = gsub('5:00 AM','',b2016$aptnum)
b2016$resunits = as.integer(gsub('-','',b2016$resunits))
b2016$comunits = as.integer(gsub('-','0',b2016$comunits))
b2016$totunits = as.integer(gsub('-','',b2016$totunits))
b2016$landsqft = gsub(',','',b2016$landsqft)
b2016$landsqft = as.integer(gsub('-','',b2016$landsqft))
b2016$grosssqft = gsub(',','',b2016$grosssqft)
b2016$grosssqft = as.integer(gsub('-','',b2016$grosssqft))
b2016$price = gsub(',','',b2016$price)
b2016$price = as.integer(gsub('-','',b2016$price))
b2016$date = as.Date(b2016$date, "%m/%d/%Y")

#Strip leading and trailing Space 
b2016$neighborhood = trimws(b2016$neighborhood, which = c("both"))
b2016$bldclasscat = trimws(b2016$bldclasscat, which = c("both"))
b2016$taxclasscurr = trimws(b2016$taxclasscurr, which = c("both"))
b2016$bldclasscurr = trimws(b2016$bldclasscurr, which = c("both"))
b2016$address = trimws(b2016$address, which = c("both"))
b2016$aptnum = trimws(b2016$aptnum, which = c("both"))
b2016$bldclasssale = trimws(b2016$bldclasssale, which = c("both"))

b2017 = read.csv("2017_brooklyn.csv", header = TRUE, skip = 4)
names(b2017) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

#removing 'comma', 'hypen', errorenous data from each column and changing the data type
b2017$aptnum = gsub('0.25','',b2017$aptnum)
b2017$landsqft = gsub(',','',b2017$landsqft)
b2017$landsqft = as.integer(gsub('-','',b2017$landsqft))
b2017$grosssqft = gsub(',','',b2017$grosssqft)
b2017$grosssqft = as.integer(gsub('-','',b2017$grosssqft))
b2017$price = as.integer(gsub(',','',b2017$price))
b2017$date = strptime(as.character(b2017$date), "%m/%d/%y")
b2017$date = as.Date(b2017$date)

#Strip leading and trailing Space 
b2017$neighborhood = trimws(b2017$neighborhood, which = c("both"))
b2017$bldclasscat = trimws(b2017$bldclasscat, which = c("both"))
b2017$taxclasscurr = trimws(b2017$taxclasscurr, which = c("both"))
b2017$bldclasscurr = trimws(b2017$bldclasscurr, which = c("both"))
b2017$address = trimws(b2017$address, which = c("both"))
b2017$aptnum = trimws(b2017$aptnum, which = c("both"))
b2017$bldclasssale = trimws(b2017$bldclasssale, which = c("both"))

b2018 = read.csv("2018_brooklyn.csv", header = TRUE, skip = 4)
names(b2018) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

#removing 'comma', 'hypen', errorenous data, dollar sign from each column and changing the data type
b2018$aptnum = gsub('5-Jan','',b2018$aptnum)
b2018$aptnum = gsub('1-Apr','',b2018$aptnum)
b2018$aptnum = gsub('2-Jun','',b2018$aptnum)
b2018$aptnum = gsub('3:00 AM','',b2018$aptnum)
b2018$aptnum = gsub('12:00 AM','',b2018$aptnum)
b2018$aptnum = gsub('3-Feb','',b2018$aptnum)
b2018$resunits = as.integer(b2018$resunits)
b2018$totunits = as.integer(b2018$totunits)
b2018$landsqft = as.integer(gsub(',','',b2018$landsqft))
b2018$grosssqft = as.integer(gsub(',','',b2018$grosssqft))
b2018$taxclasssale = as.integer(b2018$taxclasssale)
b2018$price = substring(b2018$price, 2)
b2018$price = as.integer(gsub(',','',b2018$price))
b2018$date = strptime(as.character(b2018$date), "%m/%d/%y")
b2018$date = as.Date(b2018$date)

#Strip leading and trailing Space 
b2018$neighborhood = trimws(b2018$neighborhood, which = c("both"))
b2018$bldclasscat = trimws(b2018$bldclasscat, which = c("both"))
b2018$taxclasscurr = trimws(b2018$taxclasscurr, which = c("both"))
b2018$bldclasscurr = trimws(b2018$bldclasscurr, which = c("both"))
b2018$address = trimws(b2018$address, which = c("both"))
b2018$aptnum = trimws(b2018$aptnum, which = c("both"))
b2018$bldclasssale = trimws(b2018$bldclasssale, which = c("both"))

b2019 = read.csv("2019_brooklyn.csv", header = TRUE, skip = 4)
names(b2019) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

#removing 'comma', 'hypen', errorenous data, dollar sign from each column and changing the data type
b2019$aptnum = gsub('1-Mar','',b2019$aptnum)
b2019$aptnum = gsub('2-Mar','',b2019$aptnum)
b2019$aptnum = gsub('1-Apr','',b2019$aptnum)
b2019$aptnum = gsub('3-May','',b2019$aptnum)
b2019$aptnum = gsub('3-Jun','',b2019$aptnum)
b2019$aptnum = gsub('1-Aug','',b2019$aptnum)
b2019$aptnum = gsub('3-Aug','',b2019$aptnum)
b2019$landsqft = as.integer(gsub(',','',b2019$landsqft))
b2019$grosssqft = as.integer(gsub(',','',b2019$grosssqft))
b2019$taxclasssale = as.integer(b2019$taxclasssale)
b2019$price = as.integer(gsub(',','',b2019$price))
b2019$date = strptime(as.character(b2019$date), "%m/%d/%y")
b2019$date = as.Date(b2019$date)

#Strip leading and trailing Space 
b2019$neighborhood = trimws(b2019$neighborhood, which = c("both"))
b2019$bldclasscat = trimws(b2019$bldclasscat, which = c("both"))
b2019$taxclasscurr = trimws(b2019$taxclasscurr, which = c("both"))
b2019$bldclasscurr = trimws(b2019$bldclasscurr, which = c("both"))
b2019$address = trimws(b2019$address, which = c("both"))
b2019$aptnum = trimws(b2019$aptnum, which = c("both"))
b2019$bldclasssale = trimws(b2019$bldclasssale, which = c("both"))

b2020 = read.csv("2020_brooklyn.csv", header = TRUE, skip = 6)
b2020 = b2020[-1,]
names(b2020) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

#removing 'comma', 'hypen', errorenous data, dollar sign from each column and changing the data type
b2020$landsqft = as.integer(gsub(',','',b2020$landsqft))
b2020$grosssqft = as.integer(gsub(',','',b2020$grosssqft))
b2020$taxclasssale = as.integer(b2020$taxclasssale)
b2020$price = as.integer(gsub(',','',b2020$price))
b2020$date = strptime(as.character(b2020$date), "%m/%d/%y")
b2020$date = as.Date(b2020$date)

#Strip leading and trailing Space 
b2020$neighborhood = trimws(b2020$neighborhood, which = c("both"))
b2020$bldclasscat = trimws(b2020$bldclasscat, which = c("both"))
b2020$taxclasscurr = trimws(b2020$taxclasscurr, which = c("both"))
b2020$bldclasscurr = trimws(b2020$bldclasscurr, which = c("both"))
b2020$address = trimws(b2020$address, which = c("both"))
b2020$aptnum = trimws(b2020$aptnum, which = c("both"))
b2020$bldclasssale = trimws(b2020$bldclasssale, which = c("both"))
b2020$date = trimws(b2020$date, which = c("both"))

#################### 1.2 Join the data and make it usable for analysis ####################

data = rbind(b2016, b2017, b2018, b2019, b2020)
dim(data)

#################### 1.3 Filter the data and make transformations specific to this analysis ####################

data = filter(data, (startsWith(data$bldclasssale, "A") | startsWith(data$bldclasssale, "R")))

data = filter(data, data$totunits == 1 & data$resunits == 1)

data = filter(data, data$grosssqft > 0 & !is.na(data$price))
nrow(data) #19640

################################################################################
#
# Step 2: EDA and feature engineering 
#
################################################################################

######################### 2.1 Exploratory data analysis ########################

#creating new df with only variables that be used for linear regression
bdata = data[-c(1,5,6,7,9,10,12,13,14)]
str(bdata)

bdata$neighborhood = as.factor(bdata$neighborhood)
bdata$bldclasscat = str_squish(bdata$bldclasscat)
bdata$bldclasscat = as.factor(bdata$bldclasscat)
bdata$taxclasscurr = as.factor(bdata$taxclasscurr)
bdata$bldclasscurr = as.factor(bdata$bldclasscurr)
bdata$zip = as.factor(bdata$zip)
bdata$landsqft = as.numeric(bdata$landsqft)
bdata$grosssqft = as.numeric(bdata$grosssqft)
bdata$yrbuilt = as.numeric(bdata$yrbuilt)
bdata$taxclasssale = as.factor(bdata$taxclasssale)
bdata$bldclasssale = as.factor(bdata$bldclasssale)
bdata$price = as.numeric(bdata$price)

dim(bdata) #19640


#identifying the outliears and removing them
price_max_out1 = (bdata %>% group_by(zip) %>% slice(which.max(price)))$price
bdata = bdata[-which(bdata$price %in% price_max_out1),]

price_min_out1 = (bdata %>% group_by(zip) %>% slice(which.min(price)))$price
bdata = bdata[-which(bdata$price %in% price_min_out1),]

price_max_out2 = (bdata  %>% group_by(zip) %>% slice(which.max(price)))$price[1:5]
bdata = bdata [-which(bdata $price %in% price_max_out2),]

price_min_out2 = (bdata  %>% group_by(zip) %>% slice(which.min(price)))$price
bdata  = bdata [-which(bdata $price %in% price_min_out2),]

dim(bdata) #13001

###################### 2.2 Pre-modeling and feature engineering ##############################

hist(bdata$price) #right skewed, transformation would help

m1 = lm(sqrt(price) ~., bdata)
summary(m1) #r2 0.6777    DF 123 
sqrt(mean(m1$residuals^2)) #RMSE 177.73

m2 = lm(sqrt(price)~ neighborhood  + zip+ landsqft + grosssqft + yrbuilt + date, bdata)
summary(m2) #0.667  DF 100
sqrt(mean(m2$residuals^2)) #RMSE 180.8261

m3 = lm(sqrt(price)~ zip + landsqft + grosssqft + yrbuilt + date, bdata)
summary(m3) #0.6251  DF 41 
sqrt(mean(m3$residuals^2)) #192.2967

#creating new columne called age
bdata$age = as.numeric(format(bdata$date,"%Y")) - bdata$yrbuilt

m4 = lm(sqrt(price) ~ zip +grosssqft + landsqft + age + date, bdata)
summary(m4) #0.6595 64
sqrt(mean(m4$residuals^2))

#grouping zip with similar mean prices
mean_by_zip <- bdata %>% group_by(zip) %>% summarise(mean_price = mean(price))

plot(mean_by_zip$zip, sort(mean_by_zip$mean_price))

bdata$zip[which(bdata$zip == 11231)] <- 11201
bdata$zip[which(bdata$zip == 11217)] <- 11201
bdata$zip[which(bdata$zip == 11215)] <- 11201
bdata$zip[which(bdata$zip == 11238)] <- 11201
bdata$zip[which(bdata$zip == 11239)] <- 11203
bdata$zip[which(bdata$zip == 11209)] <- 11205
bdata$zip[which(bdata$zip == 11223)] <- 11205
bdata$zip[which(bdata$zip == 11236)] <- 11207
bdata$zip[which(bdata$zip == 11213)] <- 11210
bdata$zip[which(bdata$zip == 11208)] <- 11212
bdata$zip[which(bdata$zip == 11220)] <- 11213
bdata$zip[which(bdata$zip == 11230)] <- 11218
bdata$zip[which(bdata$zip == 11211)] <- 11218
bdata$zip[which(bdata$zip == 11235)] <- 11219
bdata$zip[which(bdata$zip == 11221)] <- 11219
bdata$zip[which(bdata$zip == 11204)] <- 11228
bdata$zip[which(bdata$zip == 11233)] <- 11229
bdata$zip[which(bdata$zip == 11216)] <- 11232
bdata$zip[which(bdata$zip == 11226)] <- 11232
bdata$zip[which(bdata$zip == 11222)] <- 11249

###################### final regression ####################
model <- lm(sqrt(price) ~ date + grosssqft*zip  + age  + landsqft , bdata)
summary(model) #DF 38 r2 0.6506
RMSE = sqrt(mean(model$residuals^2))
RMSE #185.6574

############# IID ###########

#RMSE
sqrt(mean(model$residuals^2))
hist(model$residuals, breaks = 100)
ks.test(model$residuals/summary(model)$sigma, pnorm)
bptest(model)
dwtest(model)

#############################################################################
#               Housing Price changes in 2020 Q3 and Q4                     #
#############################################################################
df_2020 = bdata

df_2020$quarter = as.yearqtr(bdata$date, format = "%Y-%m-%d")

#filtering data for 2020 Q3 and 2020 Q4
df_2020 = filter(df_2020, quarter == '2020 Q3' | quarter == '2020 Q4')

ggplot(df_2020, aes(quarter, price, col = zip)) + geom_point() + geom_line()
