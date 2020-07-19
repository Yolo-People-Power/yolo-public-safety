  
  # SERVICE CALLS BY CHARGE CATEGORY----
  # Create aggregate datasets
  calls_sum <- calls_desc %>% count(broad_cat)
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
  calls_sum$pct <- (calls_sum$n/(sum(calls_sum$n)))*100
  calls_sum$pct <- round(calls_sum$pct, digits = 2)
  
  # ARRESTS BY RACE----
  arrest_race <- davis_log
  arrest_race$race <- recode_factor(arrest_race$race, 
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
  
  
  # CHARGES BY CATEGORY----
  
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
      (38663 + 37871 + 37380 + 37195 + 37071))) # Other
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
  
  # ARREST RATIOS----
    # Calculate raw fractions of arrests by race
  race_ratio <- race_cat
  race_ratio$asian_frac <- race_ratio$Asian/
    (race_ratio$Asian + race_ratio$Black + race_ratio$Hispanic +
       race_ratio$`Other/Unknown` + race_ratio$White)
  race_ratio$black_frac <- race_ratio$Black/
    (race_ratio$Asian + race_ratio$Black + race_ratio$Hispanic +
       race_ratio$`Other/Unknown` + race_ratio$White)
  race_ratio$hispanic_frac <- race_ratio$Hispanic/
    (race_ratio$Asian + race_ratio$Black + race_ratio$Hispanic +
       race_ratio$`Other/Unknown` + race_ratio$White)
  race_ratio$other_frac <- race_ratio$`Other/Unknown`/
    (race_ratio$Asian + race_ratio$Black + race_ratio$Hispanic +
       race_ratio$`Other/Unknown` + race_ratio$White)
  race_ratio$white_frac <- race_ratio$White/
    (race_ratio$Asian + race_ratio$Black + race_ratio$Hispanic +
       race_ratio$`Other/Unknown` + race_ratio$White)
  
    # Compare with population information from 2010 census, 2019 projections. 
    # >=2 races included in "other."
    # Source: https://www.census.gov/quickfacts/fact/table/daviscitycalifornia,US/PST045219
  
    # Arrest rate: arrest fraction relative to population fraction
  race_ratio$asian_ratio<- race_ratio$asian_frac/.222
  race_ratio$black_ratio <- race_ratio$black_frac/.023
  race_ratio$hisp_ratio <- race_ratio$hispanic_frac/.139
  race_ratio$other_ratio <- race_ratio$other_frac/.072
  race_ratio$white_ratio <- race_ratio$white_frac/.557
  
    # Ratio of arrest rates, by race and category
  race_ratio$asian_white <- race_ratio$asian_ratio/race_ratio$white_ratio
  race_ratio$black_white <- race_ratio$black_ratio/race_ratio$white_ratio
  race_ratio$hisp_white <- race_ratio$hisp_ratio/race_ratio$white_ratio
  race_ratio$other_white <- race_ratio$other_ratio/race_ratio$white_ratio
  
  race_ratio$asian_white <- round(race_ratio$asian_white, 2)
  race_ratio$black_white <- round(race_ratio$black_white, 2)
  race_ratio$hisp_white <- round(race_ratio$hisp_white, 2)
  race_ratio$other_white <- round(race_ratio$other_white, 2)
  
    # Reshape for plots
  race_ratio_long <- melt(race_ratio[, c(1, 17:20)], id.vars = "category")
  
  # DRUG CHARGES----
  drug_charges <- subset(davis_log_long, category ==
                           "Drug offenses")
  drug_charges <- drug_charges[,c(24:27)]
  # Delete duplicates
  drug_charges <- drug_charges %>% mutate(
    description_2 = ifelse(
      description_1 == description_2 | 
        description_3 == description_2 | 
        description_4== description_2,
      NA,
      as.character(.$description_2)))
  
  drug_charges <- drug_charges %>% mutate(
    description_3 = ifelse(
      description_1 == description_3 | 
        description_4== description_3,
      NA,
      as.character(.$description_3)))
  
  drug_charges <- drug_charges %>% mutate(
    description_4 = ifelse(
      description_1 == description_4,
      NA,
      as.character(.$description_4)))
  
  # ALCOHOL CHARGES-----
  alco_charges <- subset(davis_log_long, category ==
                           "Alcohol-related incidents")
  alco_charges <- alco_charges[,c(24:27)]
   # Duplicates irrelevant, only description_1 needed
  
  # Arrests by age/race----
    # Download davis_pop_tables
  age_race <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1BzpwqzWVDZeNHHlTFHNgH-C4RKosAoks7HN5EusBZ2o/edit?usp=sharing"
  )
    
    
  
