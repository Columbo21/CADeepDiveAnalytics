library(tidyverse)

# Beispiele:
data("table4a")
data("table5")

table4a %>% gather(year, count, -country)
table5 %>% separate(rate, into = c("cases", "population")) %>% 
    unite(year, century, year, sep="")




#################################################################

data(who, package='tidyr')

who_tidy <- who %>% gather(class, count, -(country:year)) %>% 
    mutate(class = str_replace(class, 'new(_)?', '')) %>%
    separate(class, into = c('method', 'group')) %>% 
    mutate(group = str_replace(group, '(m|f)(.+)', '\\1-\\2')) %>% 
    #pull(group) %>% unique()
    separate(group, into = c('sex', 'group')) %>%
    drop_na()

write.csv2(who_tidy, 'who_tidy.csv', row.names = F)

who_messy <- who_tidy  %>% 
    mutate(string = str_c(method, sex, group, sep='_')) %>% 
    select(-(method:group)) %>% 
    spread(string, count)

write.csv2(who_messy, 'who_messy.csv', row.names = F)    


data(table5, package='tidyr')

# Aufgabe:
# Transformiere das Messy WHO Format in das Tidy Format
# gather, separate, drop_na
           