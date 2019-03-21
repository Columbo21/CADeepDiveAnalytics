library(tidyverse)

data(midwest)
midwest <- midwest %>% 
  mutate_at(vars(county, state, inmetro), as.factor)

midwest %>% 
  ggplot(aes(x=percasian/100, fill= as.factor(inmetro))) + 
  geom_histogram(bins=100, color = 1) +
  labs (x="Prozent Asiaten", y = "Anzahl", title = "Histogram") +
  scale_x_continuous(labels = scales::percent) #weil wir library scales nicht importiert haben, können wir auch über [library]::[function] darauf zugreifen, also hier scales::percent
  #ggplot(aes(x=percasian)) + geom_histogram(bins=100, color = 1, fill = "blue")

midwest %>% 
  ggplot(aes(x=percasian/100, fill= as.factor(inmetro))) + 
  geom_density(color = 1, alpha = 0.5) +
  labs (x="Prozent Asiaten", y = "Anzahl", title = "Density plot") +
  scale_x_continuous(labels = scales::percent) 

midwest %>% 
  ggplot(aes(x=state, y = percollege, fill = state)) + 
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = .6) +
  geom_boxplot(alpha = .3) +
  labs (x="States", y = "Percent College Students") +
  ggtitle("Boxplot Titel in die Mitte") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") #versteckt die Legende

# boxplot
geom_boxplot(alpha = .3) +
  labs (x="States", y = "Percent College Students") +
  ggtitle("Boxplot Titel in die Mitte") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") #versteckt die Legende

# poptotal area
midwest %>% 
  ggplot(aes(x=area, y = poptotal)) + 
  geom_point(alpha = .2) +
  geom_smooth(method="lm") +
  scale_y_log10(label = scales::comma)
  
# poptotal area state (mit facets)
midwest %>% 
  ggplot(aes(x=area, y = poptotal)) + 
  geom_point(alpha = .2) +
  geom_smooth(method="lm") +
  scale_y_log10(label = scales::comma) +
  facet_wrap(vars(state))

# poptotal area state inmetro (mit facets)
midwest %>% 
  ggplot(aes(x=area, y = poptotal)) + 
  geom_point(alpha = .2) +
  geom_smooth(method="lm") +
  scale_y_log10(label = scales::comma) +
  facet_grid(cols=vars(state), rows=vars(inmetro))


#Matrixdarstellung, wenn man mal viele Variablenkorrelationen anschauen will (und noch keine These hat)
library(GGally)
midwest %>% 
  select(percollege, percbelowpoverty,percblack, percasian, inmetro) %>% 
  ggpairs(aes(color=inmetro))
