# Clean precipitation data

pacman::p_load("tidyverse", "lubridate", "here", "readxl", "zoo", "lubridate")

##################  CLEAN DATASETS  ################################
prcp.cru <- readRDS(here("data_raw", "prcp", "cru_prcp_station_ts.rds"))
prcp.raf <- read.csv(here("data_raw", "prcp", "PP Rafaela 2008-2020.csv"))
prcp.sauce <- read.csv(here("data_raw", "prcp", "PP Sauce Viejo 2008-2020.csv"))
prcp.2020 <- read.csv(here("data_raw", "prcp", "PP 2019-2020.csv"))

# Clean CRU station data
# Function for filling with averages
f <- function(x) ifelse(is.na(x), # if this observation in the column is NA
                        zoo::rollapply(x, # apply in this column
                                  width = 5,# up to 5 rows above
                                  FUN = function(x) mean(x, na.rm=TRUE), # this function 
                                  partial = TRUE, # allow for running the function evenif there're fewer than 5 values
                                  fill = NA, 
                                  align = "right"),
                        x) # otherwise just keep the og value

prcp.cru <- prcp.cru %>% 
  magrittr::set_colnames(c("date", "ceres", "concord", "guale", "par", "recon", "ros")) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  arrange(month) %>% # Arrange and group by month (because we want means of the same month in prev years)
  group_by(month) %>% 
  mutate(across(c(2:7), ~f(.))) %>%  # apply the function to columns 2 to 7
  ungroup() %>% 
  filter(date>as.Date("2007-12-01", "%Y-%m-%d")) %>% 
  arrange(date) %>%
  select(date, year, month, ceres, concord, guale, par, recon, ros)
  
# Clean 2019 and 2020 data
prcp.2020 <- prcp.2020 %>% 
  magrittr::set_colnames(c("date", "ros", "par", "sauce", "ceres", "recon", "concord", "guale")) %>% 
  mutate(date=as.Date(date, "%m/%d/%Y"),
         month=month(date),
         year=year(date)) %>% 
  arrange(date) %>% 
  mutate(across(.cols=c(2:8), ~as.numeric(.x))) %>% 
  group_by(year, month) %>% 
  summarise(across(.cols=c(ceres, concord, guale, par, recon, ros), ~sum(.x, na.rm=TRUE)), 
            .groups="drop") %>%
  mutate(date=as.Date(lubridate::make_date(year, month))) %>% 
  select(date, year, month, ceres, concord, guale, par, recon, ros)

# Clean Rafaela and Sauce Viejo data
prcp.raf <- prcp.raf %>% 
  magrittr::set_colnames(c("date", "raf")) %>% 
  mutate(date=str_replace(date, " 00:00:00.0", ""),
         date=ifelse(grepl("\\-", date), 
                     format(as.Date(date, "%Y-%m-%d"), "%d/%m/%Y"), 
                     date),
         date=as.Date(date, "%d/%m/%Y"),
         month=month(date),
         year=year(date)) %>% 
  group_by(year, month) %>% 
  summarise(raf=sum(as.numeric(raf), na.rm=TRUE), .groups="drop") %>% 
  mutate(date=as.Date(lubridate::make_date(year, month))) %>% 
  arrange(date)

prcp.sauce <- prcp.sauce %>% 
  magrittr::set_colnames(c("date", "sauce")) %>% 
  mutate(date=as.Date(date, "%m/%d/%Y"),
         month=month(date),
         year=year(date)) %>% 
  group_by(year, month) %>% 
  summarise(sauce=sum(as.numeric(sauce), na.rm=TRUE)) %>% 
  mutate(date=as.Date(lubridate::make_date(year, month))) %>% 
  arrange(date)

prcp <- bind_rows(prcp.cru, prcp.2020) %>% 
  arrange(date) %>% 
  bind_cols(list(raf=prcp.raf$raf,
                 sauce=prcp.sauce$sauce))

##################  CALCULATE WEIGHTED AVERAGES  #########################
pop.city <- data.frame(prov = c("Santa Fe", "Entre Ríos", "Entre Ríos", "Entre Ríos", 
                                    "Santa Fe", "Santa Fe", "Santa Fe", "Santa Fe"),
                       stn = c("ceres", "concord", "guale", "par", "recon", "ros", 
                                "raf", "sauce"),
                       pop = c(68878, 170033, 109461, 339930, 176410, 
                               1193605, 178092, 525093)) %>% 
  group_by(prov) %>% 
  mutate(wt = pop/sum(pop)) %>% 
  ungroup() %>% 
  select(prov, stn, wt)

# Rearrange dataset and calculate weighted means
prcp <- prcp %>% 
  pivot_longer(cols=c(4:11), names_to="stn", values_to="vals") %>% 
  left_join(pop.city) %>% 
  group_by(date, year, month, prov) %>% 
  summarise(prcp=weighted.mean(vals, wt),
            .groups="drop") %>% 
  arrange(prov, date, year, month)

# Create lags
lags <- as.data.frame(tsModel::Lag(prcp$prcp, k=0:6)) %>% 
  magrittr::set_colnames(c(paste0("prcp.", c(0:6))))

# Add lags to og data, filter years
prcp <- prcp %>% 
  bind_cols(lags) %>% 
  filter(year >= 2009 & year <2021)

saveRDS(prcp, here("province", "data_use", "prcp.rds"))

# END