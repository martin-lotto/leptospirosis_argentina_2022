## ############################################################################
##
## Towards a Leptospirosis Early Warning System in North-Eastern Argentina
## Lotto Batista, M, Rees E., et al., 2022
##
## SCRIPT 02: Model fitting
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

dfs <- list(
  df.er <- data.lepto %>% filter(prov == "Entre Ríos") %>% 
    mutate(across(.cols=c(7:35), ~(.x-mean(.x))/sd(.x))),
  df.sf <- data.lepto %>% filter(prov == "Santa Fe") %>% 
    mutate(across(.cols=c(7:35), ~(.x-mean(.x))/sd(.x)))
)

# ENSO
enso <- c("","+nino34.0","+nino34.1","+nino34.2","+nino34.3","+nino34.4",
          "+nino34.5","+nino34.6","+nino34.7","+nino34.8","+nino34.9",
          "+nino34.10","+nino34.11","+nino34.12")

# Precipitation
prcp <- c("","+prcp.0","+prcp.1","+prcp.2","+prcp.3","+prcp.4","+prcp.5")

# Parana river
par <- c("","+par.0","+par.1","+par.2","+par.3","+par.4","+par.5")

################################################################################
# MODELS
################################################################################
nms <- c("er", "sf")
for(i in 1:2){
  # ENSO
  fit.models(dfs[[i]], var1=enso, fileName=paste0(nms[i],"_enso_fit"))
  # Local climate
  fit.models(dfs[[i]], var1=par, var2=prcp, fileName=paste0(nms[i],"_clim_fit"))
}

## ############################################################################
## END
## ############################################################################