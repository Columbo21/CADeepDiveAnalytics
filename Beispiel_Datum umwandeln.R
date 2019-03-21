library(lubridate)
library(tidyverse)
d <- c("31. Juli 2017 15:00")
date1<-dmy_hm(d)

d2 <- c("2018/05/01")
date2<-ymd(d2)

d3 <- c("2017-Q1")
date3 <- yq(d3)

date4<-hms("10:00:00")

date5 <- seq(ymd("2017-01-01"),ymd("2017-02-01"),2)

seq(ymd("2017-01-01"),ymd("2018-02-01"),"1 week")

wday(date3,label=T)

df = data.frame(Date=seq(ymd("2019-01-01"),ymd("2020-01-01"),"1 month")) %>% 
  mutate(QUarter = paste("Q",quarter(Date),sep="")) %>% 
  mutate(Month = substr(as.character.Date(Date),1,7)) %>% #oder: mutate(Month = format(dates,"%Y-%m"))
  mutate(Month_label = month(Date, label = T)) %>% 
  mutate(Season = seasons(Date)) %>% #oder alternativ:
  mutate(Season = sapply(Date, season)) # muss so geschrieben werden, weil die Funktion nur jeweils 1 Datum verarbeiten kann, Alternative: Funktion so ändern, dass Datumsvektoren verarbeitet werden können (mit Schleife)
  
df

dmy_hm("31. Juli 2017 15:00", locale = "DE_de")

season <- function(date){
  m <- month(date)
  if(m %in% c(12, 1, 2)) return("Winter")
  if(m %in% c(3, 4, 5)) return("Frühling")
  if(m %in% c(6, 7, 8)) return("Sommer")
  if(m %in% c(9, 10, 11)) return("Herbst")
}

#Alternative season function, die einen Vector zurpückgibt:

seasons <- function(dates){
  ans<-c()  
  for (m in month(dates)){
    if(m %in% c(12, 1, 2)) ans <- c(ans, "Winter")
    if(m %in% c(3, 4, 5)) ans <- c(ans, "Frühling")
    if(m %in% c(6, 7, 8)) ans <- c(ans, "Sommer")
    if(m %in% c(9, 10, 11)) ans <- c(ans, "Herbst")
  }
return(ans)
}

