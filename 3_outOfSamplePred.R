## ############################################################################
##
## Towards a Leptospirosis Early Warning System in North-Eastern Argentina
## Lotto Batista, M, Rees E., et al., 2022
##
## SCRIPT 03: Compute out of sample predictions
##
## ############################################################################
##
## Script author: 
## - Martín Lotto Batista, ORCID: 0000-0002-9437-5270
##
## Contact: martin.lotto@bsc.es
## 
## License: 
##
## ############################################################################
source("1_functions.R")

# Prepare data
df.lepto <- readRDS("data_use/prov_cases.rds")
pop <- readRDS("data_use/pop.rds")
prcp <- readRDS("data_use/prcp.rds")
riv <- readRDS("data_use/river_height.rds")
nino <- readRDS("data_use/nino.rds")

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

## ############################################################################
## END
## ############################################################################