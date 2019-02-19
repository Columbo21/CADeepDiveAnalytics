library(tidyverse)
library(caret)
library(Metrics)
library(GGally)
data(Sacramento)

set.seed(955)
options(scipen = 100)


Sacramento <- Sacramento %>% as_tibble()
    

Sacramento %>% select(price, sqft, baths) %>% ggpairs()

Sacramento %>% ggplot(aes(sqft, price)) + geom_point() + scale_x_log10() + scale_y_log10()

in_train <- createDataPartition(log10(Sacramento$price), p = .8, list = FALSE)

training <- Sacramento[ in_train,]
testing  <- Sacramento[-in_train,]

fit <- train(log(price) ~ beds + baths + log(sqft) + type,
             trControl = trainControl(method='cv'),
             preProcess = c('center', 'scale', 'zv'),
             method = 'lm',
             data=training)

ans <- tibble(pred =predict(fit, training) %>% exp(),
              obs = training$price)

ans %>% ggplot(aes(pred, obs)) + geom_point() + geom_abline(intercept=0,slope=1)

fit_xgb <- train(price ~ beds + baths + sqft + type,
             trControl = trainControl(method='cv'),
             preProcess = c('center', 'scale', 'zv'),
             method = 'xgbTree',
             data=training)

fit_city_xgb <- train(price ~ beds + baths + sqft + type + city,
                 trControl = trainControl(method='cv'),
                 preProcess = c('center', 'scale', 'zv'),
                 method = 'xgbTree',
                 data=training)

fit_city_zip_xgb <- train(price ~ beds + baths + sqft + type + city + zip,
                      trControl = trainControl(method='cv'),
                      preProcess = c('center', 'scale', 'zv'),
                      method = 'xgbTree',
                      data=training)

fit_city <- train(price ~ beds + baths + sqft + type + city,
             trControl = trainControl(method='cv'),
             preProcess = c('center', 'scale', 'zv'),
             method = 'gbm',
             data=training)

fit_city <- train(price ~ beds + baths + sqft + type + city + zip,
                  trControl = trainControl(method='cv'),
                  preProcess = c('center', 'scale', 'zv'),
                  method = 'lm',
                  data=training)

fit_coord <- train(price ~ beds + baths + sqft + type + latitude + longitude,
                  trControl = trainControl(method='cv'),
                  preProcess = c('center', 'scale', 'zv'),
                  method = 'lm',
                  data=training)




res <- resamples(list(fit=fit, 
                      xgb = fit_xgb, 
                      xgb_city = fit_city_xgb,
                      xgb_city_zip = fit_city_zip_xgb,
                      city = fit_city,
                      coord = fit_coord))
res %>% summary()
res %>% bwplot()

varImp(fit_coord)

ans <- data.frame(pred = predict(fit_city, testing),
        obs = testing$price) 

ans %>% 
    ggplot(aes(pred, obs)) + geom_point() + 
        scale_y_continuous(labels=scales::comma) +
        scale_x_continuous(labels=scales::comma) +
        geom_abline(intercept=0, slope=1, color='red')

defaultSummary(ans)

ans %>%
    summarise(mape = mape(pred, obs),
              rmse = rmse(pred, obs),
              mae = mae(pred, obs),
              rsquared = cor(pred, obs)^2)


# only makes sense for time series 
# ts(ans$pred - ans$obs) %>% forecast::checkresiduals()


