  library(dplyr)
  library(tidyr)
  library(stringr)
  library(data.table)
  
  # Set to your working directory
  setwd("/Users/bapu/Projects/watershed/action/public-safety/yolo/analysis/west-sac")
  
  # Read in raw data
  west_sac_raw <- read.csv("./west_sac_arrests_raw.csv",
                       na.strings="")
  
  # Add identifier field for charges vs cases
  west_sac_raw$type<- ifelse(is.na(west_sac_raw$transac_num),
                                       "charge", "case")
  
  # Pivot to wide format
  west_sac_wide <- west_sac_raw %>%
    fill(transac_num) %>%
    group_by(transac_num) %>% 
    mutate(case_num_orig = first(case_num)) %>%
    slice(-1) %>%
    ungroup %>% 
    select(case_num_new = case_num, case_num = case_num_orig, transac_num) %>%
    mutate(new_col = str_c('charge', rowid(transac_num))) %>% 
    pivot_wider(names_from = new_col, values_from = case_num_new)
  
  # Bind all other columns from original file
  west_sac_wide <- left_join(west_sac_wide, west_sac_raw, by = c("case_num"))
  
  # Delete duplicate transac_num, rename original; delete charge/case column
  west_sac_wide$transac_num.y <- NULL
  west_sac_wide <- west_sac_wide %>% rename(transac_num = transac_num.x)
  west_sac_wide$type <- NULL
  
  # Export csv
  write.csv(west_sac_wide, "./west_sac_arrests_wide.csv",
            na = "", row.names = F)
