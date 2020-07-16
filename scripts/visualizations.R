library(tidyverse)
library(ggplot2)
library(reshape2)
library(ineq)
library(gglorenz)


  # SERVICE CALLS-----
    # Create aggregate datasets
  calls_sum <- calls_desc %>% count(broad_cat)
  calls_sum <- subset(calls_sum, broad_cat != "Unknown")
  calls_sum$pct <- (calls_sum$n/(sum(calls_sum$n)))*100
  calls_sum$pct <- round(calls_sum$pct, digits = 2)
  
    # Visualizations
  ggplot(calls_sum) + 
    geom_col(aes(x=broad_cat, y = n, fill = broad_cat),
             position = "dodge") + 
    geom_col(aes(x=broad_cat, y = pct * 68262/29.95, fill = broad_cat)) +
    theme(axis.text.x=element_text(angle = 55, hjust = 1, 
                                   size = 10, vjust = 1),
          axis.ticks.x=element_blank(),
    ) +
    scale_y_continuous(name = expression("Total # of calls"),
                       sec.axis = sec_axis(~ . * 29.95/68262,
                                           name = "% of calls",
                                           breaks = seq(0,30, by = 5)),
                       limits = c(0, 75000)) +
    geom_text(aes(x = broad_cat, y = pct * 68262/29.95, 
                  label = paste(pct,"%", sep ="")), 
              position=position_dodge(width=0.9), 
              vjust=-0.25, size = 4) +
    xlab("") + labs(fill = "Call category") +
    theme(legend.text=element_text(size=14),
          legend.title=element_text(size=16))
  
  # ARRESTS BY CATEGORY----
    # Create aggregates
  davis_arrest_cat <- davis_log_long %>%
    dplyr::count(date, indiv_id, race, category, name = "daily_count")
  totals <- davis_arrest_cat 
    # Change NA to "Missing"
  totals$category <- `levels<-`(addNA(totals$category), 
                                c(levels(totals$category), "(Missing data)"))
    # Choose dates, collapse all races etc. into category types
  totals <- totals %>%     
    filter(date > "2015-01-01" & date < "2021-01-01") %>%
    dplyr::count(category, name = "total_count")
    # Add percent column, two decimals
  totals$pct <- as.numeric(format(
    round((totals$total_count/sum(totals$total_count))*100, 1), nsmall = 1))
  # Major arrest categories, for visualizations
  freq_arrests <- c("All arrests", "Alcohol-related incidents", 
                    "Assault and battery",
                    "Drug offenses", "Theft")
  
    # Generate plot
  ggplot(totals) + 
    geom_col(aes(x=category, y = total_count, fill = category), 
             position = "dodge") +
    geom_col(aes(x=category, y = pct * 1700/32, fill = category)) +
    theme(axis.text.x=element_text(angle = 55, hjust = 1, 
                                   size = 10, vjust = 1),
          axis.ticks.x=element_blank(),
          ) +
    scale_y_continuous(name = expression("# of charges"),
                       sec.axis = sec_axis(~ . * 32/1700,
                                           name = "% of charges",
                                           breaks = seq(0,35, by = 5)),
                       limits = c(0,1800)) +
    geom_text(aes(x = category, y = pct * 1700/32, 
                  label = paste(pct,"%", sep ="")), 
              position=position_dodge(width=0.9), 
              vjust=-0.25, size = 4) +
    xlab("") + labs(fill = "Arrest category")
  
  # CRIMES BY RACE & CATEGORY----
  totals_race <- davis_arrest_cat 
    # Change NA to "Missing"
  totals_race$category <- `levels<-`(addNA(totals_race$category), 
                                c(levels(totals_race$category), 
                                  "(Missing data)"))
    # Collapse Asian race category
  totals_race$race <- recode_factor(totals_race$race, 
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
  totals_race <- totals_race %>%     
    filter(date > "2015-01-01" & date < "2021-01-01") %>%
    filter(race %in% 
             c("White", "Black", "Asian", "Hispanic", "Other/Unknown")) %>%
    dplyr::count(category, race, name = "total_count")
    # Create and bind totals rows, add percent by race
  totals_race_sum <- totals_race %>% group_by(race) %>%
    dplyr::summarize(total_count = sum(total_count))
  totals_race_sum$category <- "*All arrests"
  totals_race <- bind_rows(totals_race, totals_race_sum)
    # Pull "all arrests" to top of sort order
  totals_race <- totals_race %>% arrange(category)
    # Major arrest categories, for later visualizations
  freq_arrests <- c("*All arrests", "Alcohol-related incidents", 
                    "Assault and battery",
                    "Drug offenses", "Theft")
  
    # Visualizations, totals by race
  ggplot(subset(totals_race, category %in% freq_arrests)) +
    geom_col(aes(x = category, y = total_count, 
                 group = race, fill = race),
             position = "dodge") +
    theme(axis.text.x=element_text(angle = 55, hjust = 1, 
                                   size = 14, vjust = 1),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14)) +
    xlab("") + ylab("Total arrests") +
    scale_fill_discrete(name = "Race") +
    scale_y_continuous(breaks=seq(0,3000, by = 500))
  
  # RACE/ARREST RATIOS BY CATEGORY---
    # Reshape to create race columns
  race_cat <- totals_race %>% pivot_wider(names_from = race,
                                      values_from = total_count,
                                      values_fill = 0)
    # Remove missing, pull "all" to top of sort order
  race_cat <- subset(race_cat, category != "(Missing data)")
  race_cat$category[race_cat$category=="All"] <- "*All arrests"
  race_cat <- race_cat %>% arrange(category)
  
    # Add Davis population
  davis_pop <- as.vector(
    c(15410, 4095, 1597, 9648, 38663))
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
  
    # Visualize
      # Categories at bottom
  ggplot(subset(race_cat_norm, category %in% freq_arrests)) +
    geom_col(aes(x = category, y = rate_k, 
                 group = race, fill = race),
             position = "dodge") +
    theme(axis.text.x=element_text(angle = 55, hjust = 1, 
                                   size = 14, vjust = 1),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16)) +
    xlab("") + ylab("Arrest rate/thousand people") +
    scale_fill_discrete(labels = c("Asian",
                                   "Black",
                                   "Hispanic",
                                   "Other/Unknown",
                                   "White"), 
                        name = "Race") +
    scale_y_continuous(breaks=seq(0,400, by = 50))
  
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
      scale_y_continuous(breaks=seq(0, 400, by = 50))
    
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


      # All and pop only visualization
  ggplot(subset(race_cat_pop, 
                category %in% c("*All arrests", "Davis population (2019)")),
         aes(fill = race, y = total_arrests*100, x = category)) +
    geom_bar(position = "fill", stat = "identity") +
    ylab("Proportion of population") + xlab("") 
  
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
  
  
  # UNUSED OLD CODE----
  # Distribution of charges
  # charge_gini <- davis_arrest_cat  %>% count(indiv_id)
  # 
  # ggplot(charge_gini, aes(n)) + 
  #   stat_lorenz(desc = T) +
  #   xlab("% of people arrested") + 
  #   ylab("% of charges filed") +
  #   hrbrthemes::scale_x_percent() +
  #   hrbrthemes::scale_y_percent() +
  #   theme(axis.title.x = element_text(size=16),
  #         axis.title.y = element_text(size=16),
  #         axis.text.x = element_text(size=16),
  #         axis.text.y = element_text(size=16),
  #         plot.margin = unit(c(1,1,1,1), "cm")) +
  #   theme(panel.grid.major = element_line(colour = "grey"))
  
  # # Race/arrest ratios by severity
  # race_cat_sev <- dcast(totals_race_sev, severity ~ race, value.var = "total_count",
  #                      fun.aggregate = sum)
  # 
  # # Remove missing, pull "all" to top of sort order
  # race_cat_sev <- subset(race_cat_sev, severity != "(Missing data)")
  # race_cat_sev$severity[race_cat_sev$severity=="All"] <- "[All]"
  # race_cat_sev <- race_cat_sev %>% arrange(severity)
  # 
  # # Calculate raw fractions of arrests by race
  # race_cat_sev$asian_frac <- race_cat_sev$Asian/
  #   (race_cat_sev$Asian + race_cat_sev$Black + race_cat_sev$Hispanic +
  #      race_cat_sev$`Other/Unknown` + race_cat_sev$White)
  # race_cat_sev$black_frac <- race_cat_sev$Black/
  #   (race_cat_sev$Asian + race_cat_sev$Black + race_cat_sev$Hispanic +
  #      race_cat_sev$`Other/Unknown` + race_cat_sev$White)
  # race_cat_sev$hispanic_frac <- race_cat_sev$Hispanic/
  #   (race_cat_sev$Asian + race_cat_sev$Black + race_cat_sev$Hispanic +
  #      race_cat_sev$`Other/Unknown` + race_cat_sev$White)
  # race_cat_sev$other_frac <- race_cat_sev$`Other/Unknown`/
  #   (race_cat_sev$Asian + race_cat_sev$Black + race_cat_sev$Hispanic +
  #      race_cat_sev$`Other/Unknown` + race_cat_sev$White)
  # race_cat_sev$white_frac <- race_cat_sev$White/
  #   (race_cat_sev$Asian + race_cat_sev$Black + race_cat_sev$Hispanic +
  #      race_cat_sev$`Other/Unknown` + race_cat_sev$White)
  # 
  # # Compare with population information from 2010 census, 2019 projections. 
  # # >=2 races included in "other."
  # # Source: https://www.census.gov/quickfacts/fact/table/daviscitycalifornia,US/PST045219
  # 
  # # Arrest rate: arrest fraction relative to population fraction
  # race_cat_sev$asian_ratio<- race_cat_sev$asian_frac/.222
  # race_cat_sev$black_ratio <- race_cat_sev$black_frac/.023
  # race_cat_sev$hisp_ratio <- race_cat_sev$hispanic_frac/.139
  # race_cat_sev$other_ratio <- race_cat_sev$other_frac/.072
  # race_cat_sev$white_ratio <- race_cat_sev$white_frac/.557
  # 
  # # Ratio of arrest rates, by race and severity
  # race_cat_sev$asian_white <- race_cat_sev$asian_ratio/
  #   race_cat_sev$white_ratio
  # race_cat_sev$black_white <- race_cat_sev$black_ratio/
  #   race_cat_sev$white_ratio
  # race_cat_sev$hisp_white <- race_cat_sev$hisp_ratio/
  #   race_cat_sev$white_ratio
  # race_cat_sev$other_white <- race_cat_sev$other_ratio/
  #   race_cat_sev$white_ratio
  # 
  # race_cat_sev$asian_white <- round(race_cat_sev$asian_white, 2)
  # race_cat_sev$black_white <- round(race_cat_sev$black_white, 2)
  # race_cat_sev$hisp_white <- round(race_cat_sev$hisp_white, 2)
  # race_cat_sev$other_white <- round(race_cat_sev$other_white, 2)
  # 
  # # Reshape for plots
  # race_cat_sev_long <- melt(race_cat_sev[, c(1, 17:20)], 
  #                              id.vars = "severity")
  # 
  # # Visualizations
  # # Restricting to major arrest categories
  # 
  # ggplot(race_cat_sev_long) +
  #   geom_col(aes(x = severity, y = value, 
  #                group = variable,fill = variable),
  #            position = "dodge") +
  #   theme(axis.text.x=element_text(angle = 55, hjust = 1, 
  #                                  size = 14, vjust = 1),
  #         axis.ticks.x=element_blank(),
  #         axis.text.y = element_text(size = 12),
  #         legend.text = element_text(size = 14),
  #         legend.title = element_text(size = 16)) +
  #   xlab("") + ylab("Arrest ratio") +
  #   scale_fill_discrete(labels = c("Asian-White",
  #                                  "Black-White",
  #                                  "Other-White",
  #                                  "Hispanic-White"), 
  #                       name = "Racial arrest ratio") +
  #   geom_hline(yintercept = 1, color = "grey10", linetype = "dashed") +
  #   scale_y_continuous(breaks=seq(0,10, by =1))
  # 
  # 
  # # # Crimes by race & severity [NEED TO REWORK SEVERITY FIELD IS USING BELOW]
  # totals_race_sev <- davis_arrest_cat 
  # 
  # # Change NA to "Missing"
  # totals_race_sev$severity <- `levels<-`(addNA(totals_race_sev$severity), 
  #                                    c(levels(totals_race_sev$severity), 
  #                                      "(Missing data)"))
  # 
  # # Collapse Asian race severity
  # totals_race_sev$race <- recode_factor(totals_race_sev$race, 
  #                                   "Asian Indian" = "Asian",
  #                                   "Chinese" = "Asian",
  #                                   "Filipino" = "Asian",
  #                                   "Japanese" = "Asian",
  #                                   "Korean" = "Asian",
  #                                   "Other Asian" = "Asian",
  #                                   "Vietnamese" = "Asian",
  #                                   "Laotian" = "Asian",
  #                                   "Hawaiian" = "Other/Unknown",
  #                                   "Indian/Nativ" = "Other/Unknown",
  #                                   "Missing" = "Other/Unknown",
  #                                   "Other" = "Other/Unknown",
  #                                   "Pacific" = "Other/Unknown",
  #                                   "Unknown" = "Other/Unknown")
  # 
  # # Choose filters
  # totals_race_sev <- totals_race_sev %>%     
  #   filter(date > "2015-01-01" & date < "2021-01-01") %>%
  #   filter(race %in% 
  #            c("White", "Black", "Asian", "Hispanic", "Other/Unknown")) %>%
  #   count(severity, race, name = "total_count")
  # 
  # # Create and bind totals rows, add percent by race
  # totals_race_sev_sum <- totals_race_sev %>% group_by(race) %>%
  #   summarize(total_count = sum(total_count))
  # totals_race_sev_sum$severity <- "All"
  # totals_race_sev <- bind_rows(totals_race_sev, totals_race_sev_sum)
  # 
  # # Delete missing, pull "all" to top of sort order
  # totals_race_sev <- subset(totals_race_sev, 
  #                                    severity != "(Missing data)")
  # totals_race_sev$severity[totals_race_sev$severity=="All"] <- "[All]"
  # totals_race_sev <- totals_race_sev %>% arrange(severity)
  # 
  # # Visualizations, totals by race
  # ggplot(subset(totals_race_sev, severity %in% freq_arrests)) +
  #   geom_col(aes(x = severity, y = total_count, 
  #                group = race, fill = race),
  #            position = "dodge") +
  #   theme(axis.text.x=element_text(angle = 55, hjust = 1, 
  #                                  size = 14, vjust = 1),
  #         axis.ticks.x=element_blank(),
  #         axis.text.y = element_text(size = 12),
  #         legend.text = element_text(size = 12),
  #         legend.title = element_text(size = 14)) +
  #   xlab("") + ylab("Total arrests") +
  #   scale_fill_discrete(name = "Race") +
  #   scale_y_continuous(breaks=seq(0,3000, by = 500))
  
  