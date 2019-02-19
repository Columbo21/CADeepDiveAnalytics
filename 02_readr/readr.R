library(readr)
library(readxl)
library(tidyverse)

ContractData <- read_csv("02_readr/ContractData.csv")
CallsData <- read_excel("02_readr/CallsData.xls")

ContractData %>% 
    left_join(CallsData)


