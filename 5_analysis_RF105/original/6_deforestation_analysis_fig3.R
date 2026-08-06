# Deforestation using Hansen Data
## The numbers presented here are generated in google earth engine at the following link https://code.earthengine.google.com/a0ed5ca155aee1d07c46f36fbde4b4d9 
# Author: Christopher LeBoa 
# Version: 2023-06-08

# Libraries
library(tidyverse)

# Parameters
file_in_hansen <- here::here("2_data_raw/deforestation/hansen_deforestation_21.csv")

defor <- read_csv(file_in)
#===============================================================================


#Load data 
data_hansen <- read_csv(file_in_hansen)


data_hansen %>%
  mutate(forest_loss_km = round(forest_loss_m / 10^6, 1)) %>% 
  ggplot(aes(x = year, y = forest_loss_km)) +
  geom_hline(aes(yintercept = 4.76), color = "darkgreen", linetype = "dashed") + 
  geom_hline(aes(yintercept = 8.83), color = "red", linetype = "solid") + 
  geom_hline(aes(yintercept = 5.4), color = "orange", linetype = "dotdash") + 
  geom_bar(stat='identity', fill = "darkred") +
  geom_text(aes(label = forest_loss_km), vjust = - 0.5) + 
  annotate("text", x = 2002, y = 5, label = "pre-refugee") + 
  annotate("text", x = 2002, y = 9.2, label = "before LPG") + 
  annotate("text", x = 2002, y = 5.7, label = "after LPG") + 
  theme_bw() + 
  labs(x = "Year", y = "Forest Loss in Cox's Bazar Penninsula \n (in sq. km)")
