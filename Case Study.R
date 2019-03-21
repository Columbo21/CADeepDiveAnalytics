library(tidyverse)
library(caret)
library(GGally)
library(Metrics)
library(rpart.plot)
library(fpp2)
library(prophet)
library(lubridate)

activity_info <- read_csv("10_case_study/data/activity_info.csv")
#activity_info$ProductCategory %>% table()
#activity_info %>% 
 # group_by(ProductCategory,Location) %>% count()
user_info <- read_csv("10_case_study/data/user_info.csv")
#user_info %>% 
 # group_by(Age) %>% count()
#user_info %>% 
 # group_by(Address) %>% count()

# Data cleanup
activity = activity_info
user = user_info

activity$Timestamp <- as.Date(activity_info$Timestamp, "%m/%d/%Y %H:%M") + years(18)
activity$TransactionId <- as.factor(activity_info$TransactionId)
options(scipen=999)
activity$ItemId <- as.factor(activity_info$ItemId)
activity$UserId <- as.factor(activity_info$UserId)
options(scipen=0)
activity$Location <- NULL
activity$ProductCategory <- NULL
activity$X1 <- NULL

user$UserId <- as.factor(user$UserId)
user$Gender <- NULL
user$UserType <- NULL

# Data Engineering
# Create the Churn Flag

CHURN_PERIOD = 21
CHURN_TRESHOLD = 0
last_date <- activity$Timestamp %>% max()


'diffdays <- activity %>% 
  select(UserId, TransactionId, Timestamp) %>% 
  unique() %>%  # weil in einer Transaktion können mehrere Produkte sein
  group_by(UserId) %>% 
  arrange(Timestamp) %>% 
  summarise(diffdays = mean(diff(Timestamp)))

last_date <- activity$Timestamp %>% max()
  
activity %>% 
  group_by(UserId) %>% 
  summarise(lastdate = max(Timestamp)) %>% 
  left_join(diffdays, by = c("UserId" = "UserId")) %>% 
  mutate(churn = (last_date - lastdate) > diffdays)'

churnlabel <- activity %>% 
  mutate(in_period = Timestamp > last_date-Churn_Period) %>% 
  select(UserId, TransactionId, in_period) %>%
  distinct() %>% 
  group_by(UserId) %>% 
  summarise(churn = sum(in_period) <= Churn_Treshold) %>% 
  mutate_at(vars(churn), as.factor)

churnlabel$churn %>% table()

'
getFlag <- function(uId){
  return(activity %>% 
           select(Timestamp, UserId) %>% 
           filter(UserId == uId,Timestamp>today()-Churn_Period) %>% 
           count()>Churn_Treshold)
}
userw <- activity %>% 
  mutate(ChurnFlag = sapply(user$UserId, getFlag))

For (u in Churns){
  activity %>% 
    select(Timestamp, UserId) %>% 
    filter(UserId == userid,Timestamp>today()-Churn_Period) %>% 
    count()>Churn_Treshold
}'

'
- Total_Quantity
- Total_Value

- StDev_Quantity
- StDev_Value

- AvgTimeDelta
- Recency

- Count_Unique_TransactionId
- Count_Unique_ItemId

- Mean_Quantity_per_Unique_TransactionId
- Mean_Quantity_per_Unique_ItemId

- Mean_Value_per_Unique_TransactionId
- Mean_Value_per_Unique_ItemId

'
activity_before <- activity %>% filter(Timestamp < last_date - Churn_Period)

test <-activity_before %>% 
  select(UserId, Quantity, Value, TransactionId)

kpis1 <- activity_before %>% 
  group_by(UserId) %>%
  arrange(UserId, Timestamp) %>% 
  summarise(Total_Quantity = sum(Quantity),
          Total_Value = sum(Value),
          StDev_Quantity = sd(Quantity),
          StDev_Value = sd(Value),
          Recency = last_date - CHURN_PERIOD - max(Timestamp)) 


kpis2 =  activity_before %>% 
        group_by(UserId, ItemId) %>% 
        summarise(Quantity_sum = sum(Quantity),
                  Value_sum = sum(Value)) %>% 
        summarise(Mean_Quantity_per_Unique_ItemId = mean(Quantity_sum),
                  Mean_Value_per_Unique_ItemId = mean(Value_sum),
                  Count_Unique_ItemId = n_distinct(ItemId))

kpis3 = activity_before %>% 
        group_by(UserId, TransactionId) %>% 
        summarise(Quantity_sum = sum(Quantity),
                  Value_sum = sum(Value),
                  Timestamp=max(Timestamp)) %>% 
        summarise(Mean_Quantity_per_Unique_TransactionId = mean(Quantity_sum),
                  Mean_Value_per_Unique_TransactionId = mean(Value_sum),
                  Count_Unique_TransactionId = n_distinct(TransactionId),
                  AvgTimeDelta = mean(diff(Timestamp)))
  

activity_measures = kpis1 %>% full_join(kpis2 %>% full_join(kpis3))
modeldat <- user %>%
  inner_join(churnlabel) %>% 
  inner_join(activity_measures) %>% 
  drop_na()

#check for nas
modeldat %>% 
  summarise_all(function(x) is.na(x) %>% sum()) %>% 
  gather(var, na) %>% filter(na>0)

# Modelling
# Splitting

intrain <-createDataPartition(modeldat$churn, p=0.8, list=F) #erste Variable price sagt ihm, welche Variable möglichst representativ geteilt werden soll (=Zielvariable), p=0.8 heißt 80% in Trainingsdatensatz, 20% Testdatensatz
training <- modeldat[intrain,]
testing <- modeldat[-intrain,]

#Training
fit_glm <- train(churn ~ . -UserId, 
                trControl = trainControl(method="cv"),
                method="glm", family = "binomial",
                preProcess = c("center", "scale"),
                data=training)
fit_glm

fit_rf <- train(churn ~ . -UserId, 
                 trControl = trainControl(method="cv"),
                 method="LogitBoost",
                 preProcess = c("center", "scale"),
                 data=training)
fit_rf

fit_xgbtree <- train(churn ~ . -UserId, 
                 trControl = trainControl(method="cv"),
                 method="xgbTree",
                 preProcess = c("center", "scale"),
                 data=training)
fit_xgbtree

# Evaluation
resamples(list(glm = fit_glm, rf = fit_rf, xgbtree = fit_xgbtree)) %>% 
  bwplot(scales="free")
    # Kappa misst die Vorhersagegüte im Vergleich zur Verteilung der Grundgesamtheit (wenn in Grundmenge 90% richtig, 10% falsch, dann hätte ich von Haus aus eine Accuracy von 90%, wenn ich immer richtig schätze => dies wird bei Kappa berücksichtigt, ist erst ab o,4 gut)

fit_glm %>% predict(testing) %>% confusionMatrix(testing$churn)
fit_rf %>% predict(testing) %>% confusionMatrix(testing$churn)
fit_xgbtree %>% predict(testing) %>% confusionMatrix(testing$churn)

library(rpart.plot)
fit_rf$finalModel %>% prp()

varImp(fit_rf)
