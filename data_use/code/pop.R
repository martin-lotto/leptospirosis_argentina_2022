# Estimation of population size for Entre Rios and Santa Fe
# Oficial population projections span between 2010 and 2020. In this script there's an estimation for 2008 and 2009

pacman::p_load("tidyverse", "readxl", "here")

# Call data
orig.pop.er <- read_excel(here("data_raw", "demog", "pop_proj_er.xls"))
orig.pop.sf <- read_excel(here("data_raw", "demog", "pop_proj_sf.xls"))

# Function for adding 2008 and 2009
run.proj <- function(my.data) {
  
  new.years <- data.frame(year = c(2008:2009)) # Prediction years
  
  my.data <- my.data %>%
    gather("year", "pop", -Departamento) %>% 
    group_by(year) %>% 
    summarise(pop = sum(pop)) %>% 
    mutate_at(.vars = "year", .funs = as.numeric) # Convert year to numeric
  
  df_lm <- my.data %>%
    lm(pop ~ year, data=.) %>%  # Perform the linear regression
    predict(newdata=new.years) %>% # Predict for 2008 and 2009
    data.frame() %>% 
    mutate(year = c("2008", "2009")) %>% 
    rename(pop = ".") %>% 
    dplyr::select(year, pop)
  
  my.data <- rbind(my.data, df_lm) %>% # Join predictions and save it to the same df
    filter(year > 2008 & year < 2021)
}

# Create the new years
bind_rows(list(run.proj(orig.pop.er) %>% mutate(prov="Entre Ríos"),
               run.proj(orig.pop.sf) %>% mutate(prov="Santa Fe"))) %>% 
  mutate(year=as.numeric(year)) %>% 
  arrange(prov, year) %>%
  saveRDS(here("province", "data_use", "pop.rds"))

# END