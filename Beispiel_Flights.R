library(nycflights13)
data(flights)
flights

#TOP 5 Reiseziele 
flights %>% 
  select(dest, carrier) %>% 
  group_by(dest) %>% 
  count() %>% 
  arrange (desc(n))

#Unpünktlichste Fluggesellschaften (+SVERWEIS auf Webseite als Schmankerl)
ans <- flights %>% 
  select(arr_delay, carrier, distance) %>% 
  drop_na(arr_delay) %>% # oder später bei summarise als zusatzoption: summarise(n=mean(..., na.rm=TRUE))
  group_by(carrier) %>% 
  summarise(n=mean(arr_delay/distance), m = n()) %>% 
  arrange (desc(n))

#
library(rvest)
url <- "https://aspmhelp.faa.gov/index.php/ASQP_:_Carrier_Codes_And_Names"
carrier_codes <- read_html(url) %>% 
  html_table() %>% 
  .[[1]]
ans %>% 
  inner_join(carrier_codes, by=c('carrier'='IATA Carrier Code'))