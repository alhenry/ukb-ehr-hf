# exploratory data analysis to preprocess data

pacman::p_load(tidyverse, vroom, magrittr, lubridate)

# list of df

list_file <- list(
  baseline = "resources/data/baseline_subset.txt",
  pheno_308 = "resources/data/comorbidities_308_defined.txt",
  death = "resources/data/death_subset.txt",
  hes = "resources/data/hes_subset.txt",
  gp = "resources/data/gp_clinical_subset.txt",
  oper = "resources/data/operation_subset.txt"
)

list_df <- map(list_file, vroom, col_select=-1)

list_df_code <- readxl::read_excel("resources/code_list/2021-01-19_clean_pheno.xlsx",
                              sheet = "ALL") %>%
  select(-code) %>% 
  group_by(dict) %>% 
  nest %>% 
  deframe

df_basedate <- list_df$baseline %>% 
  filter(field == 53, i == 0, n == 0) %>% 
  mutate(date_attending = ymd(value)) %>% 
  select(eid, date_attending) %>% 
  distinct(eid, .keep_all = T)

# function to join and summarise based on earliest occurence
join_sum <- function(df, fn_date, col_date, col_code, dict){
  col_code <- enquo(col_code)
  col_date <- enquo(col_date)
  df %>% 
    inner_join(df_basedate, by = "eid") %>% 
    mutate(date_diag = fn_date(!!col_date)) %>% 
    filter(!is.na(!!col_date),
           # remove 'special dates' in gp data
           date_diag > ymd("1903-03-03")) %>%
    select(eid, date_diag, code = !!col_code) %>% 
    inner_join(list_df_code[[dict]],
               by = c(code = "code_clean")) %>%
    mutate(dict = dict) %>% 
    group_by(eid, pheno)
}

df_diag <- bind_rows(
  list_df$hes %>% join_sum(ymd, admidate, icd10, "icd10"),
  list_df$gp %>% join_sum(ymd, event_dt, read_code, "read"),
  list_df$oper %>% join_sum(ymd, opdate, oper4, "opcs")
) %>% 
  filter(date_diag == min(date_diag, na.rm = T))



df_death_all <- vroom("resources/data/death.txt.gz") %>% 
  mutate(date_of_death = dmy(date_of_death)) %>% 
  select(eid, date_of_death)


  








