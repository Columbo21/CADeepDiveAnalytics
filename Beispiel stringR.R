library(tidyverse)
data(fruit)
data(sentences)

fruit
sentences

words <- str_split(fruit," ") %>% 
  unlist() %>% 
  unique() #entfernt Duplikate

ans <- c()
for (w in words){
  ans <- c(ans, str_count(sentences, w) %>% sum())
}

tibble(words, count = ans) %>% 
  arrange(desc(count))


#oder mit Dataframes:
fruit %>% 
  str_split(" ") %>% 
  unlist() %>% 
  unique() %>% 
  sapply(function(x) str_detect(sentences, x) %>% sum(), simplify = F) %>% 
  as_tibble() %>% 
  gather(fruit, count) %>% 
  arrange(desc(count))

#ALternative:
tibble(words) %>% 
         mutate(ct = sapply(words, function(x) str_count(sentences, x) %>% sum()))
