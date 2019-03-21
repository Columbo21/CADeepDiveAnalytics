library(readr)
who_messy <- read_delim("03_tidyr/who_messy.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
View(who_messy)
who_messy %>% 
  gather(detail, cases, -(1:4)) %>% 
  separate("detail",into=c("type","sex","age")) %>% 
  drop_na(cases)
