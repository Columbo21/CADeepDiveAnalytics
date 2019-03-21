library(prophet)
library(tidyverse)
library(lubridate)
library(xts)
library(Metrics)

data(wineind, package="forecast")
wineind
df <- as.numeric(wineind) %>% as_tibble() %>% 
  mutate(ds=time(wineind) %>% as_date()) %>% 
  rename(y=value)

df %>% 
  ggplot(aes(x = ds, y = y)) + 
  geom_line() +
  geom_smooth(method="lm")

m <- prophet(df)
future <- make_future_dataframe(m, periods = 365, freq="month")
head(future)

forecast <- predict(m, future)
head(forecast)
forecast %>% write.csv2("forecast.csv") #macht eine csv aus einem Data Frame
plot(m, forecast)
dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)
getwd() #zeigt Arbeitsverzeichnis an

mape(df$y, forecast$yhat[1:length(df$y)]) #Mean absolute percentage error (Gütemaß), hier 6,4% Abweichung im SChnitt  => gut < 10%
mase(df$y, forecast$yhat[1:length(df$y)])


#Cross Validation (bei Zeitreihen kann man keine Zufallsteile als Test- und Trainings-set bestimmen, sondern kann immer nur hinten das Testset abschneiden)
cv <- cross_validation(m,horizon=365,
                       initial=365*8,
                       period=365/12,
                       units="days")
cv %>% plot_cross_validation_metric(metric="mape", rolling_window=.2)

cv %>% performance_metrics()
cv %>% performance_metrics() %>% 
  summarise(mape = mean(mape))

                                      