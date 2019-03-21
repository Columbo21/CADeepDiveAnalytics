library(fpp2)

data(uschange, package="fpp2")
df <- uschange %>% as_tibble() %>% 
  mutate(ds=time(uschange) %>% as_date()) %>% 
  rename(y=Consumption)

#"Vorhersage" der IST-Daten als Vergleichsbasis für spätere "Vorhersage" mit zusätzlichen Regressoren
m <- prophet(df)
forecast <- df %>% predict(m, .)
dyplot.prophet(m, forecast)

#"Vorhersage" der IST-Daten mit zusätzlichen Regressoren
m <- prophet() %>% 
  add_regressor("Income") %>% 
  add_regressor("Production") %>% 
  add_regressor("Savings") %>% 
  add_regressor("Unemployment") %>% 
  fit.prophet(df)
forecast <- df %>% predict(m, .)
dyplot.prophet(m, forecast)

#für wirkliche Vorhersagen mit zusätzlichen Regressoren müssen alle Regressoren für den gewünschten Vorhersagezeitraum geschätzt / prognostiziert werden und in der Variable future (siehe "Beispiel_propjet") abgespeichert werden