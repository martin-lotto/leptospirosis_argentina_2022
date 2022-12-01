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
## - Eleanor Rees, ORCID: 0000-0002-4993-2795
## - Rachel Lowe, ORCID: 0000-0003-3939-7343
##
## Contact: martin.lotto@bsc.es
## 
## License: 
##
## ############################################################################
source("1_sourceFunctions.R")

# Prepare data
df.lepto <- read.csv("data_use/prov_cases.csv")
pop <- read.csv("data_use/pop.csv")
prcp <- read.csv("data_use/prcp.csv")
riv <- read.csv("data_use/river_height.csv")
nino <- read.csv("data_use/nino.csv")

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