# Cleaaning of cases and merge with other variables

# Call packages
pacman::p_load("tidyverse", "readxl", "here")

# Cases per city
lepto <- read_excel(here("data_raw", "cases", "city", "Lepto Santa Fe y Paraná (ciudades).xlsx"))

lepto %>%
  magrittr::set_colnames(c("year", "month", "SF", "P")) %>%
  pivot_longer(cols=c(SF, P), names_to="city", values_to="cases") %>%
  mutate(date=as.Date(lubridate::make_date(year, month))) %>%
  saveRDS(here("city", "data_use", "city_cases.rds"))

# Cases per province
lepto.all <- read_excel(here("data_raw", "cases", "prov", "casos_2009_2018.xlsx"), sheet = "CASOS X PCIA X MES")
lepto.er <- read_excel(here("data_raw", "cases", "prov", "Pcia Entre Ríos LEPTO 2019 y 2020.xlsx"))
lepto.sf <- read_excel(here("data_raw", "cases", "prov", "Santa Fe LEPTO 2019 y 2020.xlsx"))
leptos <- list(lepto.er, lepto.sf)

lepto.all <- lepto.all %>% 
  select(Año, Mes, Provincia, `N°casos`) %>% 
  rename(year = Año, month = Mes, prov = Provincia, cases = `N°casos`) %>%
  mutate(date = as.Date(lubridate::make_date(year, month))) %>% 
  select(date, year, month, prov, cases)

provs <- c("Entre Ríos", "Santa Fe")

for(i in 1:2){
  leptos[[i]] <- leptos[[i]] %>%
    select(1,2,6) %>% 
    slice(-c(13:15)) %>% 
    magrittr::set_colnames(c("month", "cases.2019", "cases.2020")) %>% 
    pivot_longer(cols=c(cases.2019, cases.2020), names_to="year", values_to="cases") %>% 
    mutate(year=as.numeric(str_replace(year, "cases\\.", "")),
           month=rep(c(1:12), each=2),
           date=as.Date(lubridate::make_date(year, month)),
           prov=provs[i]) %>% 
    select(date, year, month, prov, cases)
}

bind_rows(leptos, lepto.all) %>% 
  arrange(prov, date, year, month) %>% 
  saveRDS(here("province", "data_use", "prov_cases.rds"))
  
# END