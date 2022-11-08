# Clean Niño region 3.4

pacman::p_load("tidyverse", "here", "tsModel")

nino <- read.delim(here("data_raw", "enso", "nino34_anom.csv"), sep=",") %>% 
  select(1,2,9,10) %>% 
  magrittr::set_colnames(c("year", "month", "sst", "nino34.0")) %>%
  filter(year>2007, year<2021)
  
# Create lags
lags <- data.frame(tsModel::Lag(nino$nino34.0, k = 1:12)) %>% 
    magrittr::set_colnames(c("nino34.1","nino34.2", "nino34.3", "nino34.4", 
                             "nino34.5", "nino34.6", "nino34.7", "nino34.8", 
                             "nino34.9", "nino34.10", "nino34.11", "nino34.12"))

nino <- nino %>% bind_cols(lags) %>% 
  filter(year>2008)

saveRDS(nino, here("city", "data_use", "nino.rds"))
saveRDS(nino, here("province", "data_use", "nino.rds"))

# END