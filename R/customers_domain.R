customers <- read.csv("/home/tpin3694/Documents/university/MSc/fundamental/THG-Jarvis/MAIN_customer_data.csv")
customers$EDomain <- as.character(customers$EDomain)

library(dplyr)
uni <- customers %>% 
  mutate(length.dom = nchar(EDomain)) %>% 
  filter(length.dom < 6)
