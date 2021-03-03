# Survival analysis
# end of follow up (based on latest data freeze date in UKB)
# https://biobank.ndph.ox.ac.uk/ukb/exinfo.cgi?src=Data_providers_and_dates
# may need to create separate end date for different providers as they have
# different data freeze date
library(survival)
library(lubridate)
source("scripts/define_hf.R")

date_censored <- ymd("2016-02-28")

make_df_surv <- function(df_diag, phenotype, col_pheno = pheno){
  col_pheno <- enquo(col_pheno)
  df_diag %>%
    filter(!!col_pheno == phenotype) %>% 
    left_join(df_death_all, by = "eid") %>% 
    # exclude prevalent cases + incident cases within 1st year after recruitment
    filter(date_diag > date_attending + dyears(1)) %>% 
    # define censored (death == 0) or death (death == 1) status based on presence in death registry
    mutate(death = ifelse(is.na(date_of_death, 0, 1)),
           time = ifelse(death == 1,
                         date_of_death - date_diag,
                         date_censored - date_diag) %>% as.numeric) 
}
  
  
  df_surv <- df_diag %>% 
  filter(date_diag == min(date_diag)) %>% 
  inner_join(df_basedate, by = "eid") %>% 
  left_join(df_death_all, by = "eid")

df_surv %>% 
  # exclude prevalent cases + incident cases within 1st year after recruitment
  filter(date_diag > date_attending + dyears(1)) %>% 
  # define censored (death == 0) or death (death == 1) status based on presence in death registry
  mutate(death = ifelse(is.na(date_of_death, 0, 1)),
         time = ifelse(death == 1,
                       date_of_death - date_attending,
                       date_censored - date_attending) %>% as.numeric) 