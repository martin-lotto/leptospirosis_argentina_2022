## ############################################################################
##
## Towards a Leptospirosis Early Warning System in North-Eastern Argentina
## Lotto Batista, M, Rees E., et al., 2022
##
## SCRIPT 01: Create functions for running models
##
## ############################################################################
##
## Script author: 
## - Martín Lotto Batista, ORCID: 0000-0002-9437-5270
## - Eleanor Rees, ORCID: 0000-0002-4993-2795
##
## Contact: martin.lotto@bsc.es
## 
## License: 
##
## ############################################################################

### PACKAGES
suppressPackageStartupMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load("dplyr", "tibble", "INLA", "hydroGOF", 
                 "svMisc", "scoringutils", "pROC")
})

################################################################################
# FITTING MODELS
################################################################################
fit.models <- function(data,
                       var1="", var2="", var3="", var4="",
                       fileName=""){
  
  ### CREATE FORMULA ####
  # Note: if the arguments were left empty, then there will only be a random
  # effects model
  forms <- expand.grid(var1, var2, var3, var4) %>%
    mutate(form=paste0(Var1, Var2, Var3, Var4))
  
  ### AUXILIARY FUNCTIONS
  # 1. Run INLA models
  # config=TRUE to keep the GMRF structure for calculating the CRPS later
  run.mod <- function(form, conf=TRUE){
    mod <- INLA::inla(as.formula(form), 
                      family="nbinomial", 
                      offset=log(pop/100000),
                      control.inla=list(strategy='adaptive'),
                      control.compute=list(dic=TRUE, cpo=TRUE, config=conf),
                      control.predictor=list(link=1, compute=TRUE),
                      verbose=FALSE,
                      data=data)
    return(mod)
  }
  
  # 2. Calculate CRPS
  crps <- function(mod){
    
    # Sample from the posterior
    s <- 1000
    xx <- inla.posterior.sample(s, mod)
    
    # Extract values of interest from the posterior sample
    xx.s <- inla.posterior.sample.eval(function(...) c(theta[1], 
                                                       Predictor[1:144]),
                                       xx)
    
    # Create posterior predictive sample
    y.pred <- matrix(NA, 144, s)
    for(s.idx in 1:s) {
      xx.sample <- xx.s[,s.idx]
      y.pred[, s.idx] <- rnbinom(144,
                                 mu=exp(xx.sample[-1]), # Predicted means
                                 size=xx.sample[1]) # Overdispersion parameter
    }
    
    # Calculate CRPS
    crps <- median(scoringutils::crps(data$cases, y.pred))
    
    return(crps)
  }
  
  # 3. Clean fixed effects
  w.params <- function(x, n){
    x %>% rownames_to_column(var="var") %>% mutate(mod=n)
  }
  
  # 4. Calculate MAE
  mod.mae <- function(mod, type=NA){
    # Type argument specifies mean or median ('0.5quant')
    hydroGOF::mae(mod[["summary.fitted.values"]][[type]], 
                  data[["cases"]], 
                  na.rm=TRUE)
  }
  
  # 5. Calculate R2
  r.sqr <- function(mod, ref=NA){
    # Ref argument specifies the reference model (null or random effects only)
    dev <- mod[["dic"]][["deviance.mean"]]
    n <- nrow(mod[["summary.fitted.values"]])
    
    if(ref=="null"){
      # In reference to null model
      nulldev <- null.mod$dic$deviance.mean
      x <- round(1 - exp((-2/n)*((dev/-2) - (nulldev/-2))), 3)
      
    } else if(ref=="re"){
      # In reference to random effects model
      redev <- re.mod$dic$deviance.mean
      x <- round(1 - exp((-2/n)*((dev/-2) - (redev/-2))), 3)
    }
    return(x)
  }
  
  # 6. Run models and extract information
  covs <- function(form){
    
    # Double arrow to update 'i' outside of the function
    i <<- i+1
    mod <- run.mod(paste0("cases ~ 1", res, form))
    
    # Extract elements of interest
    temp[["fits"]][[i]] <<- mod$summary.fitted.values %>% mutate(mod=form)
    temp[["params"]][[i]] <<- w.params(mod$summary.fixed, i)
    temp[["random"]][[i]] <<- lapply(mod$summary.random, mutate, mod=i)
    
    # MAE
    mean.fit <- mod.mae(mod, type="mean")
    median.fit <- mod.mae(mod, type="0.5quant")
    
    # Add GoF to table
    add <- data.frame(mod=i,
                      form=form,
                      dic=mod$dic$dic,
                      cpo=-mean(log(mod$cpo$cpo)),
                      rsq.null=r.sqr(mod, ref="null"),
                      rsq.re=r.sqr(mod, ref="re"),
                      mean.mae=mean.fit,
                      median.mae=median.fit,
                      mean.dif.mae.null=mean.null.fit-mean.fit,
                      median.dif.mae.null=median.null.fit-median.fit,
                      mean.dif.mae.re=mean.re.fit-mean.fit,
                      median.dif.mae.re=median.re.fit-median.fit,
                      crps=crps(mod),
                      crpss.null=1-crps(mod)/crps(null.mod),
                      crpss.re=1-crps(mod)/crps(re.mod))
    
    temp[["gof"]] <<- bind_rows(temp[["gof"]], add)
    
    return(i)
  }
  
  # Templates
  temp <- list(gof=data.frame(),
               fits=vector(mode="list", length=length(forms$form)),
               params=vector(mode="list", length=length(forms$form)),
               random=vector(mode="list", length=length(forms$form)))

  ### RUN NULL MODEL AND EXTRACT INFORMATION ####
  # Note: this model is outside the loop because it's used as reference later
  null.mod <- run.mod("cases~1")
  
  # Fitted values
  temp$fits[[1]] <- null.mod$summary.fitted.values %>% mutate(mod=1) 
  # Fixed effects
  temp$params[[1]] <- w.params(null.mod$summary.fixed, 1) 
  # Random effects
  temp$random[[1]] <- lapply(null.mod$summary.random, mutate, mod=1) 
  
  # Calculate MAE
  mean.null.fit <- mod.mae(null.mod, type="mean")
  median.null.fit <- mod.mae(null.mod, type="0.5quant")
  
  # Extract goodness of fit statistics
  add <- data.frame(mod=1, # Model ID
                    form="Null", # Formula
                    dic=null.mod$dic$dic, # DIC
                    cpo=-mean(log(null.mod$cpo$cpo)), # CPO
                    rsq.null=0, # R2 reference to null
                    rsq.re=0, # R2 reference to REs only model
                    mean.mae=mean.null.fit, # Mean of the MAE
                    median.mae=median.null.fit, # Median of the MAE
                    mean.dif.mae.null=0, # Mean dif in MAE ref to null
                    median.dif.mae.null=0, # Median dif in MAE ref to null
                    mean.dif.mae.re=NA, # Mean dif in MAE ref to REs only
                    median.dif.mae.re=NA, # Median dif in MAE ref to REs only
                    crps=crps(null.mod), # CRPS
                    crpss.null=NA, # CRPSS ref to null
                    crpss.re=NA) # CRPSS ref to REs only
  
  temp[["gof"]] <- bind_rows(temp[["gof"]], add)
  
  ### RUN RANDOM EFFECTS ONLY MODEL AND EXTRACT INFORMATION ####
  # Note: this model is outside the loop to make it easier to use as a reference
  # later but it will be calculated inside of the loop again, that is why it is
  # indexed in the templates
  res <- "+f(month, model='rw2', cyclic=TRUE)+f(ID.year, model='iid')"
  re.mod <- run.mod(paste0("cases~1", res))
  
  # Calculate MAE
  mean.re.fit <- mod.mae(re.mod, type="mean")
  median.re.fit <- mod.mae(re.mod, type="0.5quant")
  
  ### COVARIATE MODELS ####
  i <- 1
  pbapply::pblapply(forms$form, covs)
  
  # Options with functions that are able to parallelize the process 
  #parallel::mclapply(forms$form, covs, mc.cores=1)
  #parallel::parLapply(cl, forms$form, covs)
  
  print("Finished fitting models")
  
  # Results of the fitting process
  #   i) a dataframe with the goodness of fit statistics,
  #   ii) a list with the fitted values, where indexes match model IDs in i),
  #   iii) a list with the fixed effects, where indexes match model IDs in i),
  #   iv) and a list with the random effects,where indexes match model IDs in i)
  saveRDS(temp, paste0("model_out/", paste0(fileName, ".rds")))
  print(paste0("Saved prediction outputs as a R serialized object in model_out/", 
              paste0(fileName, ".rds")))
  
  ### END OF FUNCTION
}

################################################################################
# PREDICTIONS
################################################################################
pred.models <- function(data,
                        forms=NULL,
                        lags=NULL,
                        fileName=""){
  
  ### AUXILIARY FUNCTIONS
  # 1. Run INLA models
  run.mod <- function(form, conf=TRUE){
    mod <- INLA::inla(as.formula(form), 
                      family="nbinomial", 
                      offset=log(pop/100000),
                      control.inla=list(strategy='adaptive'),
                      control.compute=list(dic=TRUE, cpo=TRUE, config=conf),
                      control.predictor=list(link=1, compute=TRUE),
                      verbose=FALSE,
                      data=data)
    return(mod)
  }
  
  # 2. Calculate outbreak probability
  outb.prob <- function(data, pred.samples, lag=l) {
    # data = dataset with observed counts
    # pred.sample = sample from the posterior predictive distribution
    # lag = the lag for that specific model
    
    floor <- 5 # minimum number of cases in the threshold
    s <- 1000 # number of samples
    t <- 0.75 # quantile
    
    # Calculate the outbreak threshold by removing one year at a time and
    # computing the 75th percentile of the remaining time series
    out <- NULL
    add <- NULL
    for(j in 1:12){ # For each year
      for(m in 1:12){ # For each month
        add.month <- data %>%
          mutate(thresh=quantile(data$cases[(data$ID.year != j) & (data$month==m)], 
                                 t),
                 thresh=ifelse(thresh<floor, floor, thresh)) %>%
          filter(ID.year==j, month==m)
        
        add <- add %>% bind_rows(add.month)
      }
    }
    out <- out %>% bind_rows(add)
    
    out$prob.out <- NA
    for(i in 1:144){
      # Calculate probability of exceeding outbreak threshold 
      out$prob.out[i]<-length(pred.samples[i,][(pred.samples[i,] > out$thresh[i])])/s 
    }
    
    out <- out %>%
      # Outbreak=1, No outbreak=0
      mutate(outbreak=ifelse(cases > thresh, 1, 0)) %>% 
      # Create variable with "x" to use in plots
      mutate(outb=ifelse(outbreak==1, "x", NA), 
             br.score=(prob.out-outbreak)^2) %>% # Compute Brier score
      dplyr::select(1:6, contains(c("thresh","prob.out","outbreak",
                                    "outb","br.score")))
    
    return(out)
  }

  ### LEAVE-ONE-MONTH-OUT CROSS VALIDATION
  # Templates
  temp <- list(post.samples=NULL,
               outb.prob=NULL,
               crps=NULL,
               trigger=NULL,
               roc=NULL,
               auc=NULL)
  
  nmonths <- 144 # Total number of months
  s <- 1000 # Number of posterior samples
  
  for(j in 1:length(forms)) {
    # The random effects only model has only seasonal effects to recreate 
    # current practice but the remaining models have yearly effects as in 
    # the fitting procedure
    if(forms[[j]]==""){
      res <- "+f(month, model='rw2', cyclic=TRUE)"
    } else{
      res <- "+f(month, model='rw2', cyclic=TRUE)+f(ID.year, model='iid')"
    }
    
    form <- forms[[j]]
    l <- lags[[j]]
    nyear <- 12 # Add this as an argument (update above)
    y.pred.full <- matrix(NA, 12*nyear, s)
    
    # nmonths can be a bit confusing, think of using ntimes
    
    for(i in 1:nmonths){
      # Prepare data
      
      # Think of replacing the NA in ID.year with values from the previous year
      
      dt.pred <- mutate(data, 
                        # convert cases and ID.year to NA
                        # month 1: target month minus de lag
                        # month 12: target month minus the lag, plus 12
                        cases=ifelse(ID.month>=i-l & ID.month<=i-l+11, 
                                     NA, 
                                     cases), 
                        ID.year=ifelse(ID.month>=i-l & ID.month<=i-l+11, 
                                       NA, 
                                       ID.year))
      # Extract identifiers for NA values
      idx.pred <- which(data$ID.month>=i-l & data$ID.month<=i-l+11)
      # Number of rows to predict
      mpred <- length(idx.pred)
      
      # Run model
      mod <- run.mod(paste0("cases ~ 1", res, form))
      
      # Extract samples
      # extract overdispersion parameter
      xx <- inla.posterior.sample(s, mod, selection=list(Predictor=i))
      # extract predicted values of interest
      xx.s <- inla.posterior.sample.eval(function(...) c(theta[1], 
                                                         Predictor[1]),
                                         xx) # from this sample of the posterior
      
      # Compute the posterior predictive distribution for each of the 
      # months for which a prediction was made using the posterior mean
      # of the target month
      y.pred <- matrix(NA, mpred, s)
      for(s.idx in 1:s) {
        xx.sample <- xx.s[, s.idx]
        y.pred[, s.idx] <- rnbinom(mpred,
                                   mu=exp(xx.sample[-1]),
                                   size=xx.sample[1]) 
      }
      
      # Because l can be 0, R has to extract the target month correctly as 1
      target <- ifelse(l==0, 1, l)
      
      # Extract the target month from the set of posterior predictive distributions
      # and include them in the time series
      if(i<=l){
        y.pred.full[idx.pred[[i]], ] <- y.pred[i,]
      } else if(l==0){
        y.pred.full[idx.pred[idx.pred==i], ] <- y.pred[target,]
      } else{
        y.pred.full[idx.pred[idx.pred==i], ] <- y.pred[target+1,]
      }
      
      # Show progress
      svMisc::progress(i, max.value=nmonths)
    }
    
    ### EXTRACT POSTERIOR SAMPLES AND PREDICTIVE PERFORMANCE STATISTICS
    temp[["post.samples"]][[j]] <- y.pred.full
    temp[["outb.prob"]][[j]] <- outb.prob(data, y.pred.full)
    temp[["crps"]][[j]] <- median(scoringutils::crps(data$cases, y.pred.full))
    temp[["roc"]][[j]] <- pROC::roc(temp[["outb.prob"]][[j]]$outbreak, 
                                    temp[["outb.prob"]][[j]]$prob.out, 
                                    direction="<", ci=TRUE, quiet=TRUE)
    temp[["trigger"]][[j]] <- pROC::coords(temp[["roc"]][[j]], 
                                           x="best", 
                                           best.method="closest.topleft")
    temp[["auc"]][[j]] <- data.frame(mod=j, 
                                     auc=temp[["roc"]][[j]]$auc[[1]], 
                                     lower=temp[["roc"]][[j]]$ci[[1]], 
                                     upper=temp[["roc"]][[j]]$ci[[3]])
    
    # Calculate outbreak detection indicators
    add2 <- temp[["outb.prob"]][[j]] %>% 
      # Create a binary variable with 1=outbreak triggered
      mutate(trigger=ifelse(prob.out>temp[["trigger"]][[j]][[1]], 1, 0),
             mod=j)
    
    saveRDS(temp, paste0("model_out/", paste0(fileName, "_", j, ".rds")))
    
    print(paste0("Model ", j, " of ", length(forms)))
    
  }
  
  # Results from the prediction process
  #   i) a list with the posterior samples, where indexes match model IDs in 1.i)
  #   ii) a list with the outbreak probabilities, where indexes match model IDs in 1.i)
  #   iii) a list with the CRPS values, where indexes match model IDs in 1.i)
  #   iv) a list with the outbreak trigger values, where indexes match model IDs in 1.i)

  saveRDS(temp, paste0("model_out/", paste0(fileName, ".rds")))
    
  print(paste0("Saved prediction outputs as a R serialized object in model_out/", 
              paste0(fileName, ".rds")))
  
  ### END OF FUNCTION
}

## ############################################################################
## END
## ############################################################################