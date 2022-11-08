# Hydrometric level of rivers Paraná and Uruguay

# Call packages
pacman::p_load("tidyverse", "lubridate", "here", "readxl")

##################  CLEAN DATASETS  ################################
# RIVER HEIGHT 2005-2018
file.l <- list.files(here("data_raw", "river_height"), pattern = "*.csv", full.names = T)[-7]
rh <- lapply(file.l, read.delim, sep = ",")
obj <- c("par.bell", "par.lp", "par.par", "par.ros", "uru.conc", "uru.conco")

# Function for cleaning different river stations
wrangle.hyd <- function(my.dat) {
  my.dat %>% 
    filter(!(is.na(valor))) %>% 
    rename(date_time = timestart,
           rh = valor) %>% 
    separate(date_time, c("date", "time"), " ") %>%
    select(date, rh) %>% 
    mutate(date = as.Date(date, "%m/%d/%y")) %>% 
    mutate(year = year(date),
           month = month(date)) %>%
    group_by(year, month) %>% 
    summarise(rh = max(rh, na.rm = TRUE)) %>% 
    data.frame()
}

# Clean datasets
one.df <- NULL
for(i in 1:6){
  if(i == 1){
    add <- wrangle.hyd(rh[[i]]) %>% 
      magrittr::set_colnames(c("year", "month", obj[i]))
  }else{
    add <- wrangle.hyd(rh[[i]]) %>% 
      magrittr::set_colnames(c("year", "month", obj[i])) %>% 
      select(3)
  }
  one.df <- one.df %>% bind_cols(add)
}

one.df <- one.df %>% 
  select(-starts_with("uru")) %>%
  filter(year>2007)

# RIVER HEIGHT 2019-2020
par.bell <- read_excel(here("data_raw", "river_height", "niv_hid_2019_2020.xlsx"), 
                       sheet="Bellavista", col_names=FALSE)
par.lp <- read_excel(here("data_raw", "river_height", "niv_hid_2019_2020.xlsx"), 
                     sheet="La Paz", col_names=FALSE)
par.par <- read_excel(here("data_raw", "river_height", "niv_hid_2019_2020.xlsx"), 
                      sheet="Paraná", col_names=FALSE)
par.ros <- read_excel(here("data_raw", "river_height", "niv_hid_2019_2020.xlsx"),
                      sheet="Rosario", col_names=FALSE)
pars <- list(par.bell, par.lp, par.par, par.ros)
nms <- c("par.bell", "par.lp", "par.par", "par.ros")

for(i in 1:4){
  pars[[i]] <- pars[[i]] %>% 
    magrittr::set_colnames(c("date", "rh")) %>% 
    separate(date, into=c("date", "extra"), sep="T") %>% 
    mutate(date=as.Date(date, "%Y-%m-%d"),
           year=lubridate::year(date),
           month=lubridate::month(date)) %>% 
    group_by(year, month) %>% 
    summarise(rh=max(rh, na.rm=T), .groups="drop") %>%
    magrittr::set_colnames(c("year", "month", nms[i])) %>% 
    arrange(year, month) %>% 
    filter(year<2021) %>% 
    select(3)
}

new.one <- bind_cols(pars) %>% 
  mutate(year=rep(c(2019, 2020), each=12),
         month=rep(c(1:12), 2)) %>% 
  select(year, month, par.bell, par.lp, par.par, par.ros)
  
one.df <- one.df %>% 
  bind_rows(new.one)
  
# RIVER HEIGHT PARANA - SANTA FE
sfe.rh1 <- read_excel(here("data_raw", "river_height", "sfe_riv_2009_2018.xlsx"))
sfe.rh2 <- read_excel(here("data_raw", "river_height", "niv_hid_2019_2020.xlsx"), 
                      sheet="Santa Fe", col_names=FALSE)

sfe.rh1 <- sfe.rh1 %>% 
  magrittr::set_colnames(c("year", "month", "par.sf")) %>% 
  slice(-c(1:2)) %>% 
  mutate(across(.cols=everything(), ~as.numeric(.x)))

sfe.rh2 <- sfe.rh2 %>%
  magrittr::set_colnames(c("date", "par.sf")) %>% 
  separate(date, into=c("date", "extra"), sep="T") %>% 
  mutate(date=as.Date(date, "%Y-%m-%d"),
         year=lubridate::year(date),
         month=lubridate::month(date)) %>% 
  group_by(year, month) %>% 
  summarise(par.sf=max(par.sf, na.rm=T), .groups="drop") %>% 
  arrange(year, month) %>% 
  filter(year<2021)

# ADD 2008's SANTA FE DATA
sfe.2008 <- read_excel(here("data_raw", "river_height", "niveles faltantes.xlsx"), 
                       sheet="id30 Santa Fe 2008", col_names=FALSE)

sfe.2008 <- sfe.2008 %>% 
  magrittr::set_colnames(c("date", "rh")) %>% 
  separate(date, into=c("date", "time"), sep="T") %>% 
  mutate(date=as.Date(date, "%Y-%m-%d"),
         month=month(date),
         year=year(date)) %>% 
  group_by(year, month) %>% 
  summarise(par.sf=max(rh), .groups="drop")

sfe.rh <- bind_rows(sfe.rh1, sfe.rh2, sfe.2008) %>% 
  arrange(year, month)

# JOIN RIVER HEIGHTS
par.rh <- bind_cols(list(one.df,
                         par.sf=sfe.rh$par.sf))

# SALADO RIVER
salado <- read_excel(here("data_raw", "river_height", "niveles faltantes.xlsx"), 
                     sheet="id123 Recreo RS 2008-2020")

sal <- salado %>% 
  magrittr::set_colnames(c("date", "rh")) %>% 
  separate(date, into=c("date", "time"), sep="T") %>% 
  mutate(date=as.Date(date, "%Y-%m-%d"),
         month=month(date),
         year=year(date)) %>% 
  group_by(year, month) %>% 
  summarise(sal=max(rh, na.rm=TRUE), .groups="drop")

sal <- sal %>% 
  bind_rows(data.frame(year=2020, month=8, sal=mean(c(sal[[151,3]], sal[[152,3]])))) %>% 
  arrange(year, month)

lags <- as.data.frame(tsModel::Lag(sal$sal, k=0:6)) %>% 
  magrittr::set_colnames(c(paste0("sal.", c(0:6))))

sal <- bind_cols(sal, lags) %>% dplyr::select(-sal) %>% filter(year>2008)

saveRDS(sal, here("province", "data_use", "salado.rds"))

##################  CALCULATE WEIGHTED AVERAGES  #####################

pop.city <- data.frame(prov = c("north", "north", "er", "sf", "sf"),
                       stn = c("par.bell", "par.lp", "par.par", "par.ros", "par.sf"),
                       pop = c(37181, 66903, 339930, 1193605, 525093)) %>% 
  group_by(prov) %>% 
  mutate(wt = pop/sum(pop)) %>% 
  ungroup() %>% 
  select(prov, stn, wt)

# Rearrange dataset and calculate weighted means
par.rh <- par.rh %>% 
  pivot_longer(cols=c(3:7), names_to="stn", values_to="vals") %>% 
  left_join(pop.city) %>% 
  group_by(year, month, prov) %>% 
  summarise(rh=weighted.mean(vals, wt),
            .groups="drop") %>% 
  pivot_wider(names_from=prov, values_from=rh) %>% 
  arrange(year, month) %>% 
  dplyr::select(year, month, er, sf) %>% 
  as.data.frame()

# Create lags
lags <- NULL
nms <- c("er.", "sf.")

for(i in 1:2){
  lags[[i]] <- as.data.frame(tsModel::Lag(par.rh[,i+2], k=0:6)) %>% 
    magrittr::set_colnames(c(paste0("par.", c(0:6)))) %>% 
    mutate(prov=nms[i],
           across(.cols=c(1:7), ~round(.x, 2)),
           year=par.rh$year,
           month=par.rh$month)
}

bind_rows(lags) %>% 
  mutate(prov=str_replace_all(prov, c("er\\."="Entre Ríos", "sf\\."="Santa Fe"))) %>% 
  filter(year >= 2009) %>% 
  saveRDS(here("data_use", "river_height.rds"))

# END