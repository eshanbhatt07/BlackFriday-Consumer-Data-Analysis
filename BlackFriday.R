library(data.table)
library(Matrix)
library(xgboost)
library(caret)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(rpart)
library(randomForest)
library(plotly)
library(ggExtra)
library(plumber)
# install.packages("C50")
# library(C50)
# packages = c('ggplot2', 'gridExtra', 'corrplot', 'rpart', 'randomForest', 'stepPlr', 'C50', 'plyr'
#              , 'MASS', 'caret', 'caretEnsemble', 'dplyr', 'xgboost')


#* @get /createmodel
#* @serializer unboxedJSON
createDataModel <- function(){
data <- read.csv('train.csv')


data$Product_Category_2[data$Product_Category_1 == 1 &
                          is.na(data$Product_Category_2)] <- 2
data$Product_Category_2[data$Product_Category_1 == 2 &
                          is.na(data$Product_Category_2)] <- 4
data$Product_Category_2[data$Product_Category_1 == 3 &
                          is.na(data$Product_Category_2)] <- 4
data$Product_Category_2[data$Product_Category_1 == 4 &
                          is.na(data$Product_Category_2)] <- 5
data$Product_Category_2[data$Product_Category_1 == 5 &
                          is.na(data$Product_Category_2)] <- 8
data$Product_Category_2[data$Product_Category_1 == 6 &
                          is.na(data$Product_Category_2)] <- 8
data$Product_Category_2[data$Product_Category_1 == 7 &
                          is.na(data$Product_Category_2)] <- 12
data$Product_Category_2[data$Product_Category_1 == 8 &
                          is.na(data$Product_Category_2)] <- 14
data$Product_Category_2[data$Product_Category_1 == 9 &
                          is.na(data$Product_Category_2)] <- 15
data$Product_Category_2[data$Product_Category_1 == 10 &
                          is.na(data$Product_Category_2)] <- 13
data$Product_Category_2[data$Product_Category_1 == 11 &
                          is.na(data$Product_Category_2)] <- 15
data$Product_Category_2[data$Product_Category_1 == 12 &
                          is.na(data$Product_Category_2)] <- 14
data$Product_Category_2[data$Product_Category_1 == 13 &
                          is.na(data$Product_Category_2)] <- 16
data$Product_Category_2[data$Product_Category_1 == 14 &
                          is.na(data$Product_Category_2)] <- 16
data$Product_Category_2[data$Product_Category_1 == 15 &
                          is.na(data$Product_Category_2)] <- 16




data$Product_Category_3[data$Product_Category_1 == 1 &
                          is.na(data$Product_Category_3)] <-  15
data$Product_Category_3[data$Product_Category_1 == 2 &
                          is.na(data$Product_Category_3)] <-  15
data$Product_Category_3[data$Product_Category_1 == 3 &
                          is.na(data$Product_Category_3)] <- 5
data$Product_Category_3[data$Product_Category_1 == 4 &
                          is.na(data$Product_Category_3)] <- 9
data$Product_Category_3[data$Product_Category_1 == 5 &
                          is.na(data$Product_Category_3)] <- 14
data$Product_Category_3[data$Product_Category_1 == 6 &
                          is.na(data$Product_Category_3)] <- 16
data$Product_Category_3[data$Product_Category_1 ==  7 &
                          is.na(data$Product_Category_3)] <- 2
data$Product_Category_3[data$Product_Category_1 ==  8 &
                          is.na(data$Product_Category_3)] <- 17
data$Product_Category_3[data$Product_Category_1 ==  9 &
                          is.na(data$Product_Category_3)] <- 2
data$Product_Category_3[data$Product_Category_1 ==  10 &
                          is.na(data$Product_Category_3)] <- 16
data$Product_Category_3[data$Product_Category_1 ==  11 &
                          is.na(data$Product_Category_3)] <- 16
data$Product_Category_3[data$Product_Category_1 ==  12 &
                          is.na(data$Product_Category_3)] <- 17
data$Product_Category_3[data$Product_Category_1 ==  13 &
                          is.na(data$Product_Category_3)] <- 16
data$Product_Category_3[data$Product_Category_1 ==  14 &
                          is.na(data$Product_Category_3)] <- 2
data$Product_Category_3[data$Product_Category_1 ==  15 &
                          is.na(data$Product_Category_3)] <- 17

data$Product_Category_2 = impute(data$Product_Category_2, 8)
data$Product_Category_3 = impute(data$Product_Category_3, 14)

data$Gender <- as.factor(data$Gender)
data$Occupation <- as.factor(data$Occupation)
data$City_Category <- as.factor(data$City_Category)
data$Marital_Status <- as.factor(data$Marital_Status)
data$Age <- as.factor(data$Age)
data$Stay_In_Current_City_Years = as.factor(data$Stay_In_Current_City_Years)
data$User_ID = as.factor(data$User_ID)
data$Product_ID = as.factor(data$Product_ID)

# data$Product_Category_1 = as.factor(data$Product_Category_1)
# data$Product_Category_2 = as.factor(data$Product_Category_2)
# data$Product_Category_3 = as.factor(data$Product_Category_3)

EDA_distinct = distinct(
  data,
  User_ID,
  Age,
  Gender,
  Marital_Status,
  Occupation,
  City_Category,
  Stay_In_Current_City_Years
)
head(EDA_distinct)
head(data$User_ID, 40)


#including total number of products purchased in the dataset
userIDCount = as.data.frame(table(data$User_ID))
print(userIDCount)
uid = colnames(data)
print(uid[1])
names(userIDCount) = c("User_ID", "Purchase_Count")
head(userIDCount)

data = merge(x = data,
             y = userIDCount,
             by = uid[1],
             all.x = T)
str(data)

totspent = aggregate(data$Purchase,
                     by =  list(Category = data$User_ID),
                     FUN = sum)
head(totspent)
summary(totspent$x) }


#* @get /showviz
#* @serializer unboxedJSON 
showViz <- function()
install.packages("sparkr")
p1 = ggplot(totspent, aes(x = totspent$x)) + geom_density(fill = "red", col =
                                                            "black", alpha = 0.8)

p2 = ggplot(data, aes(x = data$Purchase_Count)) + geom_histogram(fill =
                                                                   'red', col = 'black', alpha = 0.8)

p3 <-
  ggplot(data, aes(x = Age, y = Purchase_Count, fill = Age)) + geom_boxplot() + facet_grid(Gender ~
                                                                                             Marital_Status) + labs(x = "Age", y = "Customer Purchase Count")

p4 <-
  ggplot(data, aes(x = Age, y = Purchase_Count, fill = Age)) + geom_boxplot() + facet_grid(Gender ~
                                                                                             Marital_Status) + labs(x = "Age", y = "Customer Purchase Count")

p5 = ggplot(data, aes(x = Occupation, y = Purchase_Count, fill = Occupation)) + geom_boxplot() + facet_grid(Gender ~
                                                                                                              Marital_Status) + labs(x = "Occupation", y = "Purchase Count")

productidCount = as.data.frame(table(data$Product_ID))
names(productidCount) = c('Product_ID', "Product_Count")

productmean = aggregate(data$Purchase,
                        by = list(Category = data$Product_ID),
                        FUN = mean)
names(productmean) = c('Product_ID', "Product_Mean")
productsd = aggregate(data$Purchase,
                      by = list(Category = data$Product_ID),
                      FUN = sd)
names(productsd) = c('Product_ID', 'Product_SD')
names(ProductData) = c("Product_ID")
ProductData = as.data.frame(data$Product_ID)
ProductData = merge(x = ProductData,
                    y = productidCount,
                    by = "Product_ID",
                    all.x = T)
ProductData = merge(x = ProductData,
                    y = productmean,
                    by = "Product_ID",
                    all.x = T)
ProductData = merge(x = ProductData,
                    y = productsd,
                    by = "Product_ID",
                    all.x = T)


data = merge(x = data,
             y = ProductData,
             by = "Product_ID",
             all.x = T)
ProductData$Product_Count = as.integer(ProductData$Product_Count)
ProductData$Product_Mean = as.integer(ProductData$Product_Mean)
ProductData$Product_SD = as.integer(ProductData$Product_SD)


head(ProductData[-order(ProductData$Product_Count), ])
tail(ProductData[-order(ProductData$Product_Count), ])


str(ProductData)


data = merge(x = data,
             y = productidCount,
             by = "Product_ID",
             all.x = T)

p7 = ggplot(Product)


head(ProductData[order(-ProductData$Product_Count), ])
tail(ProductData[order(-ProductData$Product_Count), ])


d3 = table(EDA_distinct$Gender, EDA_distinct$Marital_Status)
d3
p8 = ggplot(EDA_distinct, aes(x = Gender, fill = Marital_Status)) + geom_bar(position = "dodge") +
  ggtitle("") + labs("Gender", "No of distinct sales")



ggplotly(p8)

p14 = ggplot(EDA_distinct, aes(x = Occupation, fill = Age)) + geom_bar() + facet_grid(Gender ~
                                                                                        Marital_Status)
ggplotly(p14)


p17 = ggplot(EDA_distinct, aes(x = Age, fill = Stay_In_Current_City_Years)) + geom_bar() + facet_grid(City_Category ~
                                                                                                        Stay_In_Current_City_Years)
ggplotly(p17)

summary(data)


data$Product_Category_1 = as.factor(data$Product_Category_1)
data$Product_Category_2 = as.factor(data$Product_Category_2)
data$Product_Category_3 = as.factor(data$Product_Category_3)


data$cat1 = as.factor(ifelse((
  data$Product_Category_1 == '1' | data$Product_Category_2 == '1' |
    data$Product_Category_3 == '1'
),
1,
0
))

for (i in 2:20) {
  assign(paste("cat_", as.character(i), sep = ""), as.factor(ifelse((
    data$Product_Category_1 == i |
      data$Product_Category_2 == i |
      data$Product_Category_3 == i
  ),
  1,
  0
  )))
}

data = cbind(
  data,
  cat_2,
  cat_3,
  cat_4,
  cat_5,
  cat_6,
  cat_7,
  cat_8,
  cat_9,
  cat_10,
  cat_11,
  cat_12,
  cat_13,
  cat_14,
  cat_15,
  cat_16,
  cat_17,
  cat_18,
  cat_19,
  cat_20
)
to_drop = c("Product_Category_1",
            "Product_Category_3",
            "Product_Category_2")
data = data[, !names(data) %in% to_drop]

print(dim(data))
summary(data)
print(as.matrix(sapply(data, function(x)
  class(x))))
sapply(data, function(x)
  sum(is.na(x)))
XGB_data = data
XGB_data$User_ID = as.numeric(XGB_data$User_ID)
XGB_data$User_ID = as.numeric(XGB_data$User_ID)
XGB_data$Product_ID = as.numeric(XGB_data$Product_ID)
XGB_data$Purchase_Count = as.numeric(XGB_data$Purchase_Count)
XGB_data$Product_Count = as.numeric(XGB_data$Product_Count)
XGB_data$Gender = as.numeric(ifelse(XGB_data$Gender == 'Male', 1, 0))
XGB_data$Marital_Status = as.numeric(ifelse(XGB_data$Marital_Status == "Married", 1, 0))
XGB_data$City_Category = as.numeric(ifelse(
  XGB_data$City_Category == 'A',
  1,
  ifelse(
    XGB_data$City_Category == 'B',
    2,
    ifelse(XGB_data$City_Category ==
             'C', 3)
  )
))
XGB_data$City_Category = as.numeric(ifelse(
  XGB_data$City_Category == 'A',
  1,
  ifelse(XGB_data$City_Category ==
           'B', 2, 3)
))
XGB_data$Age = as.numeric(ifelse(
  XGB_data$Age == '0-17',
  17 ,
  ifelse(
    XGB_data$Age == '18-25',
    25,
    ifelse(
      XGB_data$Age == '26-35',
      35,
      ifelse(
        XGB_data$Age == '36-45' ,
        45,
        ifelse(
          XGB_data$Age == '46-50',
          50,
          ifelse(XGB_data$Age ==
                   '51-55', 55, 65)
        )
      )
    )
  )
))
print(summary(XGB_data$Stay_In_Current_City_Years))
XGB_data$Stay_In_Current_City_Years = as.numeric(
  ifelse(
    XGB_data$Stay_In_Current_City_Years == '4+',
    6,
    XGB_data$Stay_In_Current_City_Years
  )
)
print(summary(XGB_data$Stay_In_Current_City_Years))
XGB_data[, c(12:31)] = as.numeric(unlist(XGB_data[, c(12:31)])) - 1
y = as.numeric(XGB_data$Purchase)
n = nrow(XGB_data)
shuffled_data = XGB_data[sample(n), ]
train_ind = 1:round(0.6 * n)
model_train = shuffled_data[train_ind, ]
test_ind = (round(0.6 * n) + 1):n
model_test = shuffled_data[test_ind, ]
model_test$Purchase = NULL
print(dim(model_test))
model_train = data.matrix(model_train)
model_test = data.matrix(model_test)
str(model_train)
summary(model_train)
print(class(model_train))
n = nrow(XGB_data)
shuffled_data = XGB_data[sample(n), ]
train_ind = 1:round(0.6 * n)
model_train = shuffled_data[train_ind, ]
test_ind = (round(0.6 * n) + 1):n
model_test = shuffled_data[test_ind, ]
model_test$Purchase = NULL
print(dim(model_test))
Y = model_train$Purchase
model_train$Purchase = NULL
model_train = data.matrix(model_train)
model_test = data.matrix(model_test)
xgb_model_1 = xgboost(
  model_train,
  label = Y,
  model_test,
  cv = 5,
  objective = "reg:linear",
  nrounds = 500,
  max.depth = 10,
  eta = 0.1,
  colsample_bytree = 0.5,
  seed = 100,
  metric = "rmse",
  importance = 1
)
pred = predict(xgb_model_1, model_test, ouputmargin = T)
write.csv(pred, "xgboost prediction.csv")
install.packages("DataExplorer")
library(devtools)
library(Rserve)

devtools::install_github('brodieg/fansi@development')

