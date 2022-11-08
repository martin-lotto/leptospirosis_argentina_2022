################################################################################
## 4- RUN PREDICTIONS (Lotto-Rees et al., 2022)
##
## Author: Martin Lotto Batista
## Year: 2022
################################################################################
source("1_functions.R")

# Prepare data
df.lepto <- readRDS(here("data_use", "prov_cases.rds"))
pop <- readRDS(here("data_use", "pop.rds"))
prcp <- readRDS(here("data_use", "prcp.rds"))
riv <- readRDS(here("data_use", "river_height.rds"))
sal <- readRDS(here("data_use", "salado.rds"))
nino <- readRDS(here("data_use", "nino.rds"))

data.lepto <- df.lepto %>% 
  left_join(pop) %>% 
  left_join(prcp) %>% 
  left_join(riv) %>% 
  left_join(nino) %>% 
  arrange(prov, year, month) %>% 
  mutate(ID.year=rep(c(1:12), each = 12, times = 2),
         ID.month=rep(seq(1:144), 2))

df.er <- data.lepto %>% filter(prov == "Entre Ríos") %>% 
  mutate(across(.cols=c(7:35), ~(.x-mean(.x))/sd(.x)))
df.sf <- data.lepto %>% filter(prov == "Santa Fe") %>% 
  mutate(across(.cols=c(7:35), ~(.x-mean(.x))/sd(.x)))

################################################################################
# ENTRE RIOS
################################################################################
pred.models(data=df.er,
            forms=c("","+nino34.2","+nino34.3","+nino34.4","+par.1+prcp.1"), 
            lags=c(0,2,3,4,1), 
            fileName="er_preds")

################################################################################
# SANTA FE
################################################################################
pred.models(data=df.sf,
            forms=c("","+nino34.2","+nino34.3","+par.1+prcp.1"), 
            lags=c(0,2,3,1), 
            fileName="sf_preds")

# END