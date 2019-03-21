library (tidyverse)
data("table4a")
table4a
gather(table4a, year, revenue, -country)

#STRG + shift + M -> Pipe (%>%)
table4a %>% 
  gather(year, revenue,-country)
table4a

#Beispiel für Pipe
c(5,3,6) %>% 
  sum()

data("table5")
table5
table5 %>% 
  unite(century, year, col="year", sep="") %>% 
  separate(rate, into=c("cases","pop"))

