library(tidyverse)
data(starwars)

# sortieren danach, wie häufig ein Charakter in den Fiolmen vorkommt
starwars %>% 
  select(name,films) %>% 
  unnest() %>% 
  group_by(name) %>% 
  count() %>% 
  arrange(desc(n))

#Alternative:
starwars %>% 
  select(name,films) %>% 
  mutate(n=map_dbl(films, length)) %>%  #map_dbl wendet length auf jede einzelne Zeile an
  arrange(desc(n))

#Frauenquote pro FIlm
starwars %>% 
  select(name,gender,films) %>% 
  unnest() %>% 
  group_by(films,gender) %>% 
  count() %>% # oder: summarise(n=n())
  filter(gender %in% c("female", "male")) %>%  #oder: filter(gender =="female" | gender == "male")
  spread(gender, n) %>% 
  mutate(female_perc = female / (female + male)) %>% 
  arrange (desc(female_perc))
  