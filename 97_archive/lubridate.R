

library(tidyverse)
library(lubridate)

# arbitrary string
d <- c('31. Juli 2017 15:00')

# convert to datetime
dttm <- dmy_hm(d)

# convert to date
date <- dmy_hm(d) %>% as_date()

# show numbers
dttm %>% as.numeric()
date %>% as.numeric()


# also with quarter format
q <- yq('2017-Q3')

# Just the time
hms('10:00:00')

# create sequence of dates / datetimes
seq(ymd_h('2018-01-01 00'), ymd_h('2019-01-01 00'), by = '1 hours')

# weekdays
wday(d, label=T)

# create a character like 2017-01
glue::glue('{year(d)}-{str_pad(month(d),2, pad=0)}')

d %>% format('%Y-%m')



# Aufgabe: Erzeuge ein tibble mit folgendem Layout (20 Min + 10 Min show)

season <- function(x){
    if(!is.Date(x)) return(NA)
    
    m <- month(x)
    if(m %in% c(12,1,2)) return('Winter')
    if(m %in% 3:5) return('Frühling')
    if(m %in% 6:8) return('Sommer')
    if(m %in% 9:11) return('Herbst')
}


tibble(Date = seq(ymd('2019-01-01'), ymd('2019-12-01'), by = '1 month')) %>% 
    mutate(Quarter = paste0('Q',quarter(Date))) %>% 
    mutate(Month = Date %>% format('%Y-%m')) %>% 
    mutate(Month_label = Date %>% month(label=T)) %>% 
    mutate(Season = sapply(Date, season)) 



