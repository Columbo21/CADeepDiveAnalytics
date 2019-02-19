library(tidyverse)
library(prophet)
library(xts)
library(lubridate)
library(dygraphs)

data(package='forecast')

data(wineind, package='forecast')



df <- as_tibble(wineind) %>% 
    mutate(ds = time(wineind) %>% as_date()) %>% 
    rename(y = x)

df %>% column_to_rownames('ds') %>% 
    dygraph() %>% dygraphs::dyRangeSelector()

m <- prophet(df)

p <- make_future_dataframe(m, 24, freq = 'month') %>% 
    predict(m, .)

dyplot.prophet(m, p)

plot(m, p)

prophet::prophet_plot_components(m, p)
cv <- prophet::cross_validation(m, 365, units = 'days')

cv %>% prophet::plot_cross_validation_metric(metric='mape')
