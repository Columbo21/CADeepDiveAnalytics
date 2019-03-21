library(tidyverse)
library(caret)
library(GGally)
library(Metrics)
library(rpart.plot)

set.seed(1337) #setzt den Startpunkt der Zufallszahlen (Einteilung Test/Training), damit das Ergebnis immer reproduzierbar ist

data("Sacramento")
Sacramento

#PReisvorhersage für Häuser in Sacramento 
#Reprocessing
Sacramento %>% 
  select(price,sqft,baths) %>% 
  ggpairs()

#Splitting
intrain <-createDataPartition(Sacramento$price, p=0.8, list=F) #erste Variable price sagt ihm, welche Variable möglichst representativ geteilt werden soll (=Zielvariable), p=0.8 heißt 80% in Trainingsdatensatz, 20% Testdatensatz
training <- Sacramento[intrain,]
testing <- Sacramento[-intrain,]
dim(training)
dim(testing)

#Training
fit_lm <- train(price ~ sqft, 
                trControl = trainControl(method="cv"),
                method="lm",
                data=training)

#Prediction
tibble(pred = predict(fit_lm, training),
       obs = training$price) %>% 
  ggplot(aes(x = obs, y = pred)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +#perfekte Referenzlinie
  ylim(c(0,750000))


#Training2
fit_dtree <- train(price ~ sqft, 
                trControl = trainControl(method="cv"),
                method="rpart",
                tuneLength = 10, #wie viele Hyperparameter rechnet er durch (beim Decision Tree: probiert hier 10 unterschiedliche Baumgrößen und rechnet die Fehlerwerte aus)
                data=training)

#Prediction2
tibble(pred = predict(fit_dtree, training),
       obs = training$price) %>% 
  ggplot(aes(x = obs, y = pred)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +#perfekte Referenzlinie
  ylim(c(0,750000))

#evaluation linear / decision tree model
mape(training$price, predict(fit_lm, training))
mape(testing$price, predict(fit_lm, testing))

mape(training$price, predict(fit_dtree, training))
mape(testing$price, predict(fit_dtree, testing))

fit_dtree$finalModel %>% prp() # zeigt Decision Tree an

resamples(list(lm = fit_lm, dtree = fit_dtree)) %>% #zeigt grafisch die Boxplots der Fehlerwerte beider Modelle an => Entscheidung bei gleichem Mittelwert für kleinere Streuung
  bwplot(scales="free")



ans <- tibble(obs = testing$price,
              pred_lm = predict(fit_lm, testing),
              pred_dtree = predict(fit_dtree, testing))

'
ans %>% 
  arrange(obs) %>% 
  mutate(index = row_number(obs)) %>% 
  gather(type, value, -index) %>% 
  ggplot(aes(x = index, y = value, color = type)) + geom_point(alpha=.8) +
  scale_y_continuous(labels=scales::comma)
'

#Training3 mit 2 Variablen
fit_lm2 <- train(price ~ sqft + zip, 
                trControl = trainControl(method="cv"),
                method="lm",
                data=training)

#Prediction3
tibble(pred = predict(fit_lm2, training),
       obs = training$price) %>% 
  ggplot(aes(x = obs, y = pred)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +#perfekte Referenzlinie
  ylim(c(0,750000))

#evaluation linear2 model
mape(training$price, predict(fit_lm2, training))
mape(testing$price, predict(fit_lm2, testing))

resamples(list(lm = fit_lm, dtree = fit_dtree, lm2 = fit_lm2)) %>% #zeigt grafisch die Boxplots der Fehlerwerte beider Modelle an => Entscheidung bei gleichem Mittelwert für kleinere Streuung
  bwplot(scales="free")



ans<-ans %>% 
  mutate(pred_lm2=predict(fit_lm2, testing))

ans %>% 
  arrange(obs) %>% 
  mutate(index = row_number(obs)) %>% 
  gather(type, value, -index) %>% 
  ggplot(aes(x = index, y = value, color = type)) + geom_point(alpha=.8) +
  scale_y_continuous(labels=scales::comma)

#Training4 mit boosting (Emsemble)
fit_boo <- train(price ~ sqft, 
                 trControl = trainControl(method="cv"),
                 method="xgbTree",
                 data=training)

#Prediction4
tibble(pred = predict(fit_boo, training),
       obs = training$price) %>% 
  ggplot(aes(x = obs, y = pred)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +#perfekte Referenzlinie
  ylim(c(0,750000))

#evaluation xgbTree model
mape(training$price, predict(fit_boo, training))
mape(testing$price, predict(fit_boo, testing))

resamples(list(lm = fit_lm, dtree = fit_dtree, lm2 = fit_lm2, boost = fit_boo)) %>% #zeigt grafisch die Boxplots der Fehlerwerte beider Modelle an => Entscheidung bei gleichem Mittelwert für kleinere Streuung
  bwplot(scales="free")



ans<-ans %>% 
  mutate(pred_boo=predict(fit_boo, testing))

ans %>% 
  arrange(obs) %>% 
  mutate(index = row_number(obs)) %>% 
  gather(type, value, -index) %>% 
  ggplot(aes(x = index, y = value, color = type)) + geom_point(alpha=.8) +
  scale_y_continuous(labels=scales::comma)


#Variablenkorrelation ausrechnen
varImp(fit_lm2) # Sortiert die Variablen nach ihrer Korrelation

getwd()

