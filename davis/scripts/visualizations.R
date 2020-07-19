library(tidyverse)
library(ggplot2)
library(ggtext)


race_colors <- 

  # SERVICE CALLS-----
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

    # Visualizations
  ggplot(calls_sum) + 
    geom_col(aes(x=broad_cat, y = n, fill = broad_cat),
             position = "dodge") + 
    geom_col(aes(x=broad_cat, y = pct * 68262/29.95, fill = broad_cat)) +
    theme(axis.text.x=element_text(angle = 55, hjust = 1, 
                                   size = 14, vjust = 1),
          axis.ticks.x=element_blank(),
          axis.text.y=element_text(size = 12),
          axis.title.y=element_text(size =14)
    ) +
    scale_y_continuous(name = expression("Total # of calls"),
                       sec.axis = sec_axis(~ . * 29.95/68262,
                                           name = "",
                                           breaks = seq(0,30, by = 5)),
                       limits = c(0, 75000)) +
    geom_text(aes(x = broad_cat, y = pct * 68262/29.95, 
                  label = paste(pct,"%", sep ="")), 
              position=position_dodge(width=0.9), 
              vjust=-0.25, size = 5) +
    xlab("") + labs(fill = "Call category") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white")) +
    guides(fill = F) 
    
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
  
    # visualization
  ggplot(arrest_race,
         aes(x = race, fill = race, y = rate)
         ) + 
    geom_bar(stat = "identity") + 
    xlab("") + 
    ylab("Annual arrests/1000 people") + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 16),
          axis.ticks.x = element_blank(), 
          axis.title.y = element_text(size = 16)) +
    scale_y_continuous(breaks = seq(0,50, by = 5),
                       limits = c(0,50)) + 
    geom_text(aes(x = race, y = rate, 
                  label = round(rate,2)),
                  size = 6,
                  vjust = -0.25) +
  guides(fill = F)
 
  
  geom_text(aes(x = category, y = pct * 39.1, 
                label = paste(pct,"%", sep ="")), 
            position=position_dodge(width=0.9), 
            vjust=-0.25, size = 6)
  

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

    # Generate plot
  charge_colors <- c("steelblue1", "steelblue3", "steelblue4",
                     "seagreen2", "seagreen4",
                     "plum1", "plum2", "plum3", "plum4", 
                     "orange1", "orange2", "orange3", "orange3", "orange4", "sienna4",
                     "tomato1", "tomato3" , "red2", "red4", "tomato4",
                     "grey40")
  
  ggplot(totals) + 
    geom_col(aes(x=category, 
                 y = total_count, 
                 fill = category), 
             position = "dodge") +
    geom_col(aes(x = category, 
                 y = pct * 39.1, 
                 fill = category)) +
    theme(axis.text.x = 
        element_text(angle = 55, 
                     hjust = 1, 
                     size = 16, 
                     vjust = 1),
          axis.ticks.x = element_blank(),
          legend.text = element_text(size =16),
          panel.background = element_rect(fill = "white"),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=14)) +
    scale_y_continuous(
      name = expression("# of charges"),
      sec.axis = sec_axis(~ . * 0.025572519,
                          name = "",
                          breaks = seq(0,35, by = 5)),
      limits = c(0,2000), breaks = seq(0,1500, by = 250)) +
    geom_text(aes(x = category, y = pct * 39.1, 
                  label = paste(pct,"%", sep ="")), 
              position=position_dodge(width=0.9), 
              vjust=-0.25, size = 6) +
    xlab("") + labs(fill = "Arrest category") + 
    guides(fill = F) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) + 
    scale_fill_manual(values = charge_colors) +
    annotate("text", label = "Public Justice", 
    x =2, y =1900, size=6, color = "steelblue4") +
    annotate("text", label = "(5.0%)", 
             x =2, y =1750, size=6, color = "steelblue4") +
    annotate("text", label = "Substance Abuse", 
             x =4.5, y =1900, size=6, color = "seagreen4") +
    annotate("text", label = "(45.9%)", 
             x =4.5, y =1750, size=6, color = "seagreen4") +
    annotate("text", label = "Miscellaneous", 
             x =7.5, y =1900, size=6, color = "plum4") +
    annotate("text", label = "(11.8%)", 
             x =7.5, y =1750, size=6, color = "plum4") +
    annotate("text", label = "Property Crimes", 
             x =12.5, y =1900, size=6, color = "sienna") +
    annotate("text", label = "(19.0%)", 
             x =12.5, y =1750, size=6, color = "sienna") +
    annotate("text", label = "Crimes Against Persons", 
             x =18, y =1900, size=6, color = "tomato4") +
    annotate("text", label = "(14.4%)", 
             x =18, y =1750, size=6, color = "tomato4") 
    
  
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
  
    # Visualizations, totals by race
  ggplot(subset(race_totals, category %in% freq_charges)) +
    geom_col(aes(x = category, y = total_count, 
                 group = race, fill = race),
             position = "dodge") +
    theme(axis.text.x=element_text(angle = 55, hjust = 1, 
                                   size = 14, vjust = 1),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          panel.background = element_rect(fill = "white")) +
    xlab("") + ylab("Total charges") +
    scale_fill_discrete(name = "Race") +
    scale_y_continuous(breaks=seq(0,1000, by = 100)) 
  
  # RACE/ARREST RATIOS BY CATEGORY---
    # Reshape to create race columns
  race_cat <- race_totals %>% pivot_wider(names_from = race,
                                      values_from = total_count,
                                      values_fill = 0)
    # Remove missing, pull "all" to top of sort order
  race_cat <- subset(race_cat, category != "(Missing data)")
  race_cat$category[race_cat$category=="All"] <- "*All arrests"
  race_cat <- race_cat %>% arrange(category)
  
    # Add Davis population
  davis_pop <- as.vector(
    c((15410 + 14974 + 14751 + 14729 + 14429), 
       (4095 + 3939 + 3914 + 3385 + 3892), 
        (1597 + 1502 + 1592 + 1784 + 1383), 
       (9648 + 9340 + 9580 + 9570 + 9406), 
       (38663 + 37871 + 37380 + 37195 + 37071)))
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
  
    # Visualize
      # Categories at bottom
  ggplot(subset(race_cat_norm, category %in% freq_charges)) +
    geom_col(aes(x = category, y = rate_k, 
                 group = race, fill = race),
             position = "dodge") +
    theme(axis.text.x=element_text(angle = 55, hjust = 1, 
                                   size = 14, vjust = 1),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(size = 14),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 18),
          panel.background = element_rect(fill = "white")) +
    xlab("") + ylab("Annual charges/thousand people") +
    scale_fill_discrete(labels = c("Asian",
                                   "Black",
                                   "Hispanic",
                                   "Other/Unknown",
                                   "White"), 
                        name = "Race") +
    scale_y_continuous(breaks=seq(0,20, by = 2),
                       limits = c(0,20)) 
  
          # Categories at bottom, disagg only
  
  ggplot(subset(race_cat_norm, category %in% freq_arrests)) +
    geom_col(aes(x = category, y = rate_k, 
                 group = race, fill = race),
             position = "dodge") +
    theme(axis.text.x=element_text(angle = 55, hjust = 1, 
                                   size = 14, vjust = 1),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(size = 14),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 18)) +
    xlab("") + ylab("Arrest rate/thousand people") +
    scale_fill_discrete(labels = c("Asian",
                                   "Black",
                                   "Hispanic",
                                   "Other/Unknown",
                                   "White"), 
                        name = "Race") +
    scale_y_continuous(breaks=seq(0,20, by = 2))
  
      # Races at bottom
    ggplot(subset(race_cat_norm, category %in% freq_arrests)) +
      geom_col(aes(x = race, y = rate_k, 
                   group = category, fill = category),
               position = "dodge") +
      theme(axis.text.x=element_text(angle = 55, hjust = 1, 
                                     size = 14, vjust = 1),
            axis.ticks.x=element_blank(),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16)) +
      xlab("") + ylab("Arrest rate/thousand people") +
      scale_fill_discrete(labels = c("All",
                                     "Alcohol-related incidents",
                                     "Assault and battery",
                                     "Drug offenses",
                                     "Theft"), 
                          name = "Arrest category") +
      scale_y_continuous(breaks=seq(0, 70, by = 10))
    
    # Alternative, stacked bar
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


      # All and pop only visualization
  ggplot(subset(race_cat_pop, 
                category %in% c("*All arrests", "Davis population (2019)")),
         aes(fill = race, y = total_arrests*100, x = category)) +
    geom_bar(position = "fill", stat = "identity") +
    ylab("Proportion of population") + xlab("") +
    theme(axis.text.x=element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16)) + 
    scale_fill_discrete(name = "Race")
  
  
    
  
    # Each and pop
  ggplot(subset(race_cat_pop, 
                category %in% c("Davis population (2019)", 
                                freq_arrests)),
         aes(fill = race, y = total_arrests, x = category)) +
    geom_bar(position = "fill", stat = "identity") +
    ylab("Proportion of population") + xlab("")  
  
    
    # Arrest rates
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
  
    # Visualizations
  ggplot(subset(race_ratio_long, category %in% freq_arrests)) +
    geom_col(aes(x = category, y = value, 
                 group = variable,fill = variable),
             position = "dodge") +
    theme(axis.text.x=element_text(angle = 55, hjust = 1, 
                                   size = 14, vjust = 1),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16)) +
    xlab("") + ylab("Arrest ratio") +
    scale_fill_discrete(labels = c("Asian-White",
                                 "Black-White",
                                 "Hispanic-White",
                                 "Other-White"), 
                        name = "Racial arrest ratio") +
    geom_hline(yintercept = 1, color = "grey10", linetype = "dashed") +
    scale_y_continuous(breaks=seq(0,10, by =1)) 
  