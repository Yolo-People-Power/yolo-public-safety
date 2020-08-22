library(tidyverse)
library(googlesheets4)
library(data.table)
library(lubridate)

setwd("/Users/bapu/Projects/watershed/action/public-safety/yolo/analysis/davis/")  

# SERVICE CALLS----
    # For Shiny
  # Create aggregate datasets
  calls_sum <- calls_desc %>% count(broad_cat, date, disp_desc)
  # Order factor levels
  calls_sum$broad_cat <- factor(calls_sum$broad_cat, 
                                levels = c(
                                  "Miscellaneous general",
                                  "Public services",
                                  "Bikes",
                                  "Checkup",
                                  "Patrol",
                                  "Vehicular violations",
                                  "Miscellaneous crime/citation",
                                  "Disturbance",
                                  "Alcohol/drugs",
                                  "Accidents/injuries",
                                  "Burglary/theft",
                                  "Person search",
                                  "Suspicious activity/threats",
                                  "Violent crime, physical & sexual abuse, death"
                                ))
    # Group by date etc.
  calls_sum <- calls_sum %>%
    group_by(date, broad_cat, disp_desc) %>%
    summarize(n = sum(n))
  calls_sum$date <- as.Date(calls_sum$date)
  
  write.csv(calls_sum,"./data/generated/calls_sum.csv", row.names = F)
  write.csv(calls_sum,"./shiny/calls_sum.csv", row.names = F)

  # ARRESTS BY RACE, SEVERITY, DATE----
    # for Shiny
  arrests <- davis_log_long %>% pivot_wider(
    id_cols = c("indiv_id", "date", "sex", "race", "age"), 
    names_from = c("charge_num"),
    values_from = c("severity_1", "sec_code", "description_1"))
    # Recode and order severity levels
  arrests$severity <- paste(arrests$severity_1_charge1,
                            arrests$severity_1_charge2, 
                            arrests$severity_1_charge3,
                            sep = ", ")
  arrests$sec_codes <- paste(arrests$sec_code_charge1,
                             arrests$sec_code_charge2,
                             arrests$sec_code_charge3,
                             sep = ", ")
  arrests$charges <- paste(arrests$description_1_charge1,
                           arrests$description_1_charge2,
                           arrests$description_1_charge3,
                           sep = ", ")
  arrests$severity <- recode_factor(
    arrests$severity,
    "Felony, Felony, Felony" = "Felony [3]",
    "Felony, Felony, Misdemeanor"  = "Felony [2], Misdemeanor [1]",
    "Felony, Felony, NA" = "Felony [2]",   
    "Felony, Misdemeanor, Felony" = "Felony [2], Misdemeanor [1]",
    "Felony, Misdemeanor, Misdemeanor" = "Felony [1], Misdemeanor [2]",
    "Felony, Misdemeanor, NA" = "Felony [1], Misdemeanor [1]", 
    "Felony, NA, Felony" = "Felony [2]",     
    "Felony, NA, Misdemeanor" = "Felony [1], Misdemeanor [1]",     
    "Felony, NA, NA" = "Felony [1]",            
    "Misdemeanor, Felony, Felony" = "Felony [2], Misdemeanor [1]",
    "Misdemeanor, Felony, Misdemeanor" = "Felony [1], Misdemeanor [2]",
    "Misdemeanor, Felony, NA" = "Felony [1], Misdemeanor [1]",  
    "Misdemeanor, Misdemeanor, Felony" = "Felony [1], Misdemeanor [2]",  
    "Misdemeanor, Misdemeanor, Misdemeanor" = "Misdemeanor [3]",
    "Misdemeanor, Misdemeanor, NA" = "Misdemeanor [2]",
    "Misdemeanor, NA, Felony" = "Felony [1], Misdemeanor [1]",            
    "Misdemeanor, NA, Misdemeanor" = "Misdemeanor [2]",       
    "Misdemeanor, NA, NA" = "Misdemeanor [1]",                
    "NA, Felony, Felony" = "Felony [2]",                  
    "NA, Felony, Misdemeanor" = "Felony [1], Misdemeanor [1]",             
    "NA, Felony, NA" = "Felony [1]",                   
    "NA, Misdemeanor, Felony" = "Felony [1], Misdemeanor [1]",             
    "NA, Misdemeanor, Misdemeanor" = "Misdemeanor [2]",          
    "NA, Misdemeanor, NA" = "Misdemeanor [1]",           
    "NA, NA, Felony" = "Felony [1]",                    
    "NA, NA, Misdemeanor" = "Misdemeanor [1]",            
    "NA, NA, NA" = "Unclear/Unknown" )
  arrests <- arrests %>% arrange(severity) %>%
    mutate(severity = factor(severity, levels = 
                           c("Felony [3]", "Felony [2], Misdemeanor [1]",
                             "Felony [2]", "Felony [1], Misdemeanor [2]",
                             "Felony [1], Misdemeanor [1]", "Felony [1]",            
                             "Misdemeanor [3]", "Misdemeanor [2]",
                             "Misdemeanor [1]", "Unclear/Unknown")))
  arrests$severity_1_charge1 <- NULL
  arrests$severity_1_charge2 <- NULL
  arrests$severity_1_charge3 <- NULL
  arrests$sec_code_charge1 <- NULL
  arrests$sec_code_charge2 <- NULL
  arrests$sec_code_charge3 <- NULL
  arrests$description_1_charge1 <- NULL
  arrests$description_1_charge2 <- NULL
  arrests$description_1_charge3 <- NULL
    # Recode races
  arrests$race <- recode_factor(arrests$race, 
                                    "Asian Indian" = "Asian",
                                    "Chinese" = "Asian",
                                    "Filipino" = "Asian",
                                    "Japanese" = "Asian",
                                    "Korean" = "Asian",
                                    "Other Asian" = "Asian",
                                    "Vietnamese" = "Asian",
                                    "Laotian" = "Asian",
                                    "Hawaiian" = "Other/Unknown",
                                    "Indian/Nativ" = "Other/Unknown",
                                    "Missing" = "Other/Unknown",
                                    "Other" = "Other/Unknown",
                                    "Pacific" = "Other/Unknown",
                                    "Unknown" = "Other/Unknown")
  arrests$indiv_id <- NULL
  arrests$date <- as.Date(arrests$date)
      # remove NAs
  arrests[c("severity", "sec_codes", "charges")] <- 
    as.data.frame(sapply(arrests[c("severity", "sec_codes", "charges")], 
    function(x) x <- gsub(", NA", "", x)))
     # Export
  write.csv(arrests, "./shiny/arrests.csv", row.names = F)
  
  # CHARGES-----
    # For Shiny
  charges <- davis_log_long %>% 
    subset(select  = c("indiv_id", "date", "sex", "race", "age",
                       "sec_code", "description_1", "severity_1", "category"))
  charges$race <- recode_factor(charges$race, 
                                "Asian Indian" = "Asian",
                                "Chinese" = "Asian",
                                "Filipino" = "Asian",
                                "Japanese" = "Asian",
                                "Korean" = "Asian",
                                "Other Asian" = "Asian",
                                "Vietnamese" = "Asian",
                                "Laotian" = "Asian",
                                "Hawaiian" = "Other/Unknown",
                                "Indian/Nativ" = "Other/Unknown",
                                "Missing" = "Other/Unknown",
                                "Other" = "Other/Unknown",
                                "Pacific" = "Other/Unknown",
                                "Unknown" = "Other/Unknown")
  write.csv(charges, "./shiny/charges.csv", row.names = F)
  
  
  # Charges by category
  
    # Create aggregates
  davis_charge_cat <- davis_log_long %>%
    dplyr::count(date, indiv_id, race, category, name = "daily_count")
  totals <- davis_charge_cat 
    # Change NA to "Missing"
  totals$category <- `levels<-`(addNA(totals$category), 
                                c(levels(totals$category), "(Missing data)"))
    # Choose dates, collapse all races etc. into category types
  totals <- totals %>% count(category, name = "total_count")
    # Add percent column, two decimals
  totals$pct <- as.numeric(format(
    round((totals$total_count/sum(totals$total_count))*100, 1), nsmall = 1))
    # Reorder factor levels by rough category
  totals$category <- factor(totals$category, 
                            levels = c(
                              "Impeding public justice",
                              "Probation and prison violations",
                              "Sentence enhancements",
                              "Alcohol-related incidents",
                              "Drug offenses",
                              "Disturbing the peace",
                              "Miscellaneous",
                              "Vehicular violations",
                              "Weapons",
                              "Vandalism",
                              "Trespassing",
                              "Theft",
                              "Burglary",
                              "Arson",
                              "Robbery",
                              "Threats to the person/child endangerment",
                              "Assault and battery",
                              "Sexual assault/offenses",
                              "Kidnapping/false imprisonment",
                              "Homicide",
                              "(Missing data)"
                            ))
  
    # Major arrest categories, for visualizations
  freq_arrests <- c("All arrests", "Alcohol-related incidents", 
                    "Assault and battery",
                    "Drug offenses", "Theft",
                    "")
    # Export
  write.csv(davis_charge_cat, "./data/generated/davis_charge_cat.csv")
  
  # CHARGES BY RACE & CATEGORY----
  
  race_totals <- davis_charge_cat 
    # Change NA to "Missing"
  race_totals$category <- `levels<-`(addNA(race_totals$category), 
                                     c(levels(race_totals$category), 
                                       "(Missing data)"))
    # Collapse race category
  race_totals$race <- recode_factor(race_totals$race, 
                                    "Asian Indian" = "Asian",
                                    "Chinese" = "Asian",
                                    "Filipino" = "Asian",
                                    "Japanese" = "Asian",
                                    "Korean" = "Asian",
                                    "Other Asian" = "Asian",
                                    "Vietnamese" = "Asian",
                                    "Laotian" = "Asian",
                                    "Hawaiian" = "Other/Unknown",
                                    "Indian/Nativ" = "Other/Unknown",
                                    "Missing" = "Other/Unknown",
                                    "Other" = "Other/Unknown",
                                    "Pacific" = "Other/Unknown",
                                    "Unknown" = "Other/Unknown")
    # Choose filters
  race_totals <- race_totals %>% count(category, race, name = "total_count")
    # Create and bind totals rows, add percent by race
  race_totals_sum <- race_totals %>% group_by(race) %>%
    summarize(total_count = sum(total_count))
  race_totals_sum$category <- "*All charges"
  race_totals <- bind_rows(race_totals, race_totals_sum)
    # Pull "all arrests" to top of sort order
  race_totals <- race_totals %>% arrange(category)
  write.csv(race_totals, "./data/generated/race_totals.csv")
  
    # Major arrest categories, for later visualizations
  freq_charges<- c("Alcohol-related incidents", 
                   "Assault and battery",
                   "Drug offenses", "Theft")
  freq_charges_all <- c("*All charges", "Alcohol-related incidents", 
                        "Assault and battery",
                        "Drug offenses", "Theft")
  
  # RACE/CHARGE RATIOS BY CATEGORY----
  # Reshape to create race columns
  race_cat <- race_totals %>% pivot_wider(names_from = race,
                                          values_from = total_count,
                                          values_fill = 0)
  # Remove missing, pull "all" to top of sort order
  race_cat <- subset(race_cat, category != "(Missing data)")
  race_cat$category[race_cat$category=="All"] <- "*All arrests"
  race_cat <- race_cat %>% arrange(category)
  
    # Add Davis population 2015-2019
  davis_pop <- as.vector(
    c((15410 + 14974 + 14751 + 14729 + 14429),  # Asian  
      (4095 + 3939 + 3914 + 3385 + 3892), # Other
      (1597 + 1502 + 1592 + 1784 + 1383), # Black
      (9648 + 9340 + 9580 + 9570 + 9406), # Hispanic
      (38663 + 37871 + 37380 + 37195 + 37071))) # White
    # Normalized per 100k
  race_cat_norm <- t(t(race_cat[,2:6]) / davis_pop)
  race_cat_norm <- race_cat_norm * 1000
  race_cat_norm <- cbind(race_cat[,1], race_cat_norm)
    # Long for visualizations
  race_cat_norm <- race_cat_norm %>% pivot_longer(
    cols = c("Asian", "Other/Unknown", "Black", "White", "Hispanic"),
    names_to = "race",
    values_to = "rate_k"
  )
  write.csv(race_cat_norm, "./data/generated/race_cat_norm.csv")
  
    # For stacked bar plot
  race_cat_pop <- rbind(race_cat, c("Davis population (2019)", davis_pop))
  race_cat_pop <- race_cat_pop %>% pivot_longer(
    cols = c("Asian", "Other/Unknown", "Black", "White", "Hispanic"),
    names_to = "race",
    values_to = "total_arrests")
  race_cat_pop <- race_cat_pop %>% mutate_at(vars(category, race), as.factor)
  race_cat_pop <- race_cat_pop %>% mutate_at(vars(total_arrests), as.numeric)
  race_cat_pop$category <- relevel(race_cat_pop$category, 
                                   "Davis population (2019)")
  write.csv(race_cat_pop, "./data/generated/race_cat_pop.csv")
  
  # DRUG CHARGES----
  drug_charges <- subset(davis_log_long, category ==
                           "Drug offenses")
  drug_charges <- drug_charges[,c("date", "description_1")]
  drug_charges$date <- year(drug_charges$date)
  drug_charges$race <- recode_factor(drug_charges$race, 
                                    "Asian Indian" = "Asian",
                                    "Chinese" = "Asian",
                                    "Filipino" = "Asian",
                                    "Japanese" = "Asian",
                                    "Korean" = "Asian",
                                    "Other Asian" = "Asian",
                                    "Vietnamese" = "Asian",
                                    "Laotian" = "Asian",
                                    "Hawaiian" = "Other/Unknown",
                                    "Indian/Nativ" = "Other/Unknown",
                                    "Missing" = "Other/Unknown",
                                    "Other" = "Other/Unknown",
                                    "Pacific" = "Other/Unknown",
                                    "Unknown" = "Other/Unknown")
  drug_charges <- drug_charges %>% count(date, description_1)
  drug_wide <- drug_charges %>% 
    pivot_wider(names_from = date,
                values_from = n,
                values_fill = 0)
    # By broad type
  drug_ch_race <- subset(davis_log_long, category ==
                           "Drug offenses")
  drug_ch_race <- drug_ch_race[,c("race", "description_1")]
  drug_ch_race$race <- recode_factor(drug_ch_race$race, 
                                     "Asian Indian" = "Asian",
                                     "Chinese" = "Asian",
                                     "Filipino" = "Asian",
                                     "Japanese" = "Asian",
                                     "Korean" = "Asian",
                                     "Other Asian" = "Asian",
                                     "Vietnamese" = "Asian",
                                     "Laotian" = "Asian",
                                     "Hawaiian" = "Other/Unknown",
                                     "Indian/Nativ" = "Other/Unknown",
                                     "Missing" = "Other/Unknown",
                                     "Other" = "Other/Unknown",
                                     "Pacific" = "Other/Unknown",
                                     "Unknown" = "Other/Unknown")
    # import categorization
  drug_ch_type <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1skKAGQRg6SqeUU_M3zhLPHwOsgHdjN70USpoKXXzcP4/edit?usp=sharing")
    # Join charges and types
  drug_ch_type <- left_join(drug_ch_race, drug_ch_type, by = "description_1")
  drug_ch_type <- drug_ch_type %>% count(race, type)
  drug_ch_type <- drug_ch_type %>% drop_na
  
  # Delete duplicates
  # drug_charges <- drug_charges %>% mutate(
  #   description_2 = ifelse(
  #     description_1 == description_2 | 
  #       description_3 == description_2 | 
  #       description_4== description_2,
  #     NA,
  #     as.character(.$description_2)))
  # 
  # drug_charges <- drug_charges %>% mutate(
  #   description_3 = ifelse(
  #     description_1 == description_3 | 
  #       description_4== description_3,
  #     NA,
  #     as.character(.$description_3)))
  # 
  # drug_charges <- drug_charges %>% mutate(
  #   description_4 = ifelse(
  #     description_1 == description_4,
  #     NA,
  #     as.character(.$description_4)))
  # wide format for race analysis
  

  # ALCOHOL CHARGES-----
  alco_charges <- subset(davis_log_long, category ==
                           "Alcohol-related incidents")
  alco_charges <- alco_charges[,c(4,24)]
   # Duplicates irrelevant, only description_1 needed
  
  # ARRESTS BY AGE/RACE----
    # Download davis_pop_tables
  age_race_pop <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1BzpwqzWVDZeNHHlTFHNgH-C4RKosAoks7HN5EusBZ2o/edit?usp=sharing"
  , sheet = "org_census_groups")
  age_race_pop$age_group <- as.factor(age_race_pop$age_group)

  
    # Reshape for later join
  age_race_pop <- age_race_pop %>% pivot_longer(
    cols = c("Black", "Asian", "White", "Hispanic", "Other/Unknown"),
    names_to = "race",
    values_to = "pop"
  )
    # Subset Davis arrest log
  age_race_arrests <- davis_log
  age_race_arrests$date <- substring(age_race_arrests$date, 7, 10)
  age_race_arrests <- subset(age_race_arrests, date == "2018")
  age_race_arrests <- age_race_arrests[c("race", "age")]
    # Create age groups that conform to census groups
  age_race_arrests$age_group <- cut(
    age_race_arrests$age,
    c(0,4,9,14,17,19,24,29,34,44,54,64,74,84,120), labels = c("age_0_u5",
                                               "age_5_9",
                                               "age_10_14",
                                               "age_15_17",
                                               "age_18_19",
                                               "age_20_24",
                                               "age_25_29",
                                               "age_30_34",
                                               "age_35_44",
                                               "age_45_54",
                                               "age_55_64",
                                               "age_65_74",
                                               "age_75_84",
                                               "age_85over"))
  age_race_arrests$race <- recode_factor(age_race_arrests$race, 
                                    "Asian Indian" = "Asian",
                                    "Chinese" = "Asian",
                                    "Filipino" = "Asian",
                                    "Japanese" = "Asian",
                                    "Korean" = "Asian",
                                    "Other Asian" = "Asian",
                                    "Vietnamese" = "Asian",
                                    "Laotian" = "Asian",
                                    "Hawaiian" = "Other/Unknown",
                                    "Indian/Nativ" = "Other/Unknown",
                                    "Missing" = "Other/Unknown",
                                    "Other" = "Other/Unknown",
                                    "Pacific" = "Other/Unknown",
                                    "Unknown" = "Other/Unknown")
    # Create counts by age group and race
  age_race_arrests <- age_race_arrests %>% count(race, age_group)
  colnames(age_race_arrests) <- c("race", "age_group", "arrests")
  age_race <- left_join(age_race_arrests, age_race_pop, by = c(
    "race", "age_group"
  ))
    # Add rate per 1000k column
  age_race$rate <- (age_race$arrests/age_race$pop)*1000
                                    
  
  
  arrest_race <- davis_log
 
  arrest_race <- as.data.frame(table(arrest_race$race))
  colnames(arrest_race) <- c("race", "arrests")
  arrest_race$pct <- (arrest_race$arrests/sum(arrest_race$arrests))*100
  arrest_race$pop <-  as.vector(c(
    (15410 + 14974 + 14751 + 14729 + 14429), 
    (4095 + 3939 + 3914 + 3385 + 3892), 
    (38663 + 37871 + 37380 + 37195 + 37071),
    (1597 + 1502 + 1592 + 1784 + 1383), 
    (9648 + 9340 + 9580 + 9570 + 9406)))
  arrest_race$rate <- (arrest_race$arrests/arrest_race$pop)*1000
  
    
    
  
