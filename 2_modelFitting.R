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

dfs <- list(
  df.er=data.lepto %>% filter(prov == "Entre Ríos") %>% 
    mutate(across(.cols=c(7:35), ~(.x-mean(.x))/sd(.x))),
  df.sf=data.lepto %>% filter(prov == "Santa Fe") %>% 
    mutate(across(.cols=c(7:35), ~(.x-mean(.x))/sd(.x)))
)

## ############################################################################
## Distribution check: Poisson vs Negative Binomial
res <- "cases~1+f(month, model='rw2', cyclic=TRUE)+f(ID.year, model='iid')"

## POISSON
check <- NULL
for(df in names(dfs)){
  check[["mod"]][[df]] <- run.mod(res, "poisson", dfs[[df]])
  check[["distribution"]][[df]] <- fast_distribution_check(check$mod[[df]])
  check[["dispersion"]][[df]] <- dispersion_check(check$mod[[df]])
}

# Check DIC
check$mod$df.er$dic$dic
check$mod$df.sf$dic$dic

# Check distribution
plot(check$distribution$df.er) # Fits distribution
plot(check$distribution$df.sf) # Fits distribution

# Check dispersion
plot(check$dispersion$df.er) # High over-dispersion
plot(check$dispersion$df.sf) # High over-dispersion

## NEGATIVE BINOMIAL
check <- NULL
for(df in names(dfs)){
  check[["mod"]][[df]] <- run.mod(res, "nbinomial", dfs[[df]])
  check[["distribution"]][[df]] <- fast_distribution_check(check$mod[[df]])
  check[["dispersion"]][[df]] <- dispersion_check(check$mod[[df]])
}

# Check DIC
check$mod$df.er$dic$dic
check$mod$df.sf$dic$dic

# Check distribution
plot(check$distribution$df.er) # Fits distribution
plot(check$distribution$df.sf) # Fits distribution

# Check dispersion
plot(check$dispersion$df.er) # A bit of underdispersion
plot(check$dispersion$df.sf) # No over- underdispersion

# Check overdispersion parameter
check$mod$df.er$summary.hyperpar[1,c(1:5)] # >0 and CIs don't include the null
check$mod$df.sf$summary.hyperpar[1,c(1:5)] # >0 and CIs don't include the null

## ############################################################################
## Model fitting

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