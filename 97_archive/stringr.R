library(tidyverse)

data(fruit)
data(sentences)

words <- fruit %>% str_split(" ") %>% unlist()

ans <- list()

for(w in words){
    ans[[w]] <- str_detect(tolower(sentences), tolower(w)) %>% sum()
}

ans %>% as_tibble() %>% 
    gather(fruit, ct) %>% 
    filter(ct > 0) %>% 
    arrange(desc(ct)) %>% 
    ggplot(aes(fruit %>% fct_reorder(ct), ct)) + geom_col(fill='#d15200') + coord_flip() +
    theme_light()



### without for loop

fruit %>% 
    str_split(" ") %>% 
    unlist() %>% 
    unique() %>%
    sapply(function(x) str_detect(tolower(sentences), x) %>% sum(), simplify = F) %>% 
    as_tibble() %>% 
    gather(fruit, ct) %>% 
    filter(ct > 0) %>% 
    ggplot(aes(fruit, ct)) + geom_col() + coord_flip()
