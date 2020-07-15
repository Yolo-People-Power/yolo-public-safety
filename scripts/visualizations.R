library(tidyverse)
library(ggplot2)
library(reshape2)
library(ineq)
library(gglorenz)


  # Service calls-----
  calls_sum <- calls_desc %>% count(broad_cat)
  calls_sum <- subset(calls_sum, broad_cat != "Unknown")
  calls_sum$pct <- (calls_sum$n/(sum(calls_sum$n)))*100
  calls_sum$pct <- round(calls_sum$pct, digits = 2)
  
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
  

  # Crimes by category----
  # What crimes are taking place in Davis over given time period?
  totals <- davis_daily_log
  
    # Change NA to "Missing"
  totals$category <- `levels<-`(addNA(totals$category), 
                                c(levels(totals$category), "(Missing data)"))
  
    # Choose dates, collapse all races etc. into category types
  totals <- totals %>%     
    filter(date > "2015-01-01" & date < "2021-01-01") %>%
    count(category, name = "total_count")
    
    # Add percent column, two decimals
  totals$pct <- as.numeric(format(
    round((totals$total_count/sum(totals$total_count))*100, 1), nsmall = 1))
  
  # Major arrest categories, for visualizations
  freq_arrests <- c("[All]", "Alcohol-related incidents", 
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
  
  # Distribution of charges----
  # charge_gini <- davis_daily_log %>% count(indiv_id)
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
    
  
  
  
  # Crimes by race & category----
  totals_race <- davis_daily_log
  
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
    count(category, race, name = "total_count")
  
    # Create and bind totals rows, add percent by race
  totals_race_sum <- totals_race %>% group_by(race) %>%
    summarize(total_count = sum(total_count))
  totals_race_sum$category <- "All"
  totals_race <- bind_rows(totals_race, totals_race_sum)

    # Pull "all" to top of sort order
  totals_race$category[totals_race$category=="All"] <- "[All]"
  totals_race <- totals_race %>% arrange(category)
  
    # Major arrest categories, for later visualizations
  freq_arrests <- c("[All]", "Alcohol-related incidents", 
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
  
  # # Crimes by race & severity [NEED TO REWORK SEVERITY FIELD IS USING BELOW]
  # totals_race_sev <- davis_daily_log
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
  
    # The above can be used to break crime categories by race, or 
    # races by crime category. The below provides population-adjusted ratios
    # of arrests. 
  
    # Race/arrest ratios by category----
  race_ratios <- dcast(totals_race, category ~ race, value.var = "total_count",
                       fun.aggregate = sum)
  
    # Remove missing, pull "all" to top of sort order
  race_ratios <- subset(race_ratios, category != "(Missing data)")
  race_ratios$category[race_ratios$category=="All"] <- "[All]"
  race_ratios <- race_ratios %>% arrange(category)
  
    # Calculate raw fractions of arrests by race
  race_ratios$asian_frac <- race_ratios$Asian/
    (race_ratios$Asian + race_ratios$Black + race_ratios$Hispanic +
       race_ratios$`Other/Unknown` + race_ratios$White)
  race_ratios$black_frac <- race_ratios$Black/
    (race_ratios$Asian + race_ratios$Black + race_ratios$Hispanic +
       race_ratios$`Other/Unknown` + race_ratios$White)
  race_ratios$hispanic_frac <- race_ratios$Hispanic/
    (race_ratios$Asian + race_ratios$Black + race_ratios$Hispanic +
       race_ratios$`Other/Unknown` + race_ratios$White)
  race_ratios$other_frac <- race_ratios$`Other/Unknown`/
    (race_ratios$Asian + race_ratios$Black + race_ratios$Hispanic +
       race_ratios$`Other/Unknown` + race_ratios$White)
  race_ratios$white_frac <- race_ratios$White/
    (race_ratios$Asian + race_ratios$Black + race_ratios$Hispanic +
       race_ratios$`Other/Unknown` + race_ratios$White)
  
    # Compare with population information from 2010 census, 2019 projections. 
    # >=2 races included in "other."
    # Source: https://www.census.gov/quickfacts/fact/table/daviscitycalifornia,US/PST045219
  
    # Arrest rate: arrest fraction relative to population fraction
  race_ratios$asian_ratio<- race_ratios$asian_frac/.222
  race_ratios$black_ratio <- race_ratios$black_frac/.023
  race_ratios$hisp_ratio <- race_ratios$hispanic_frac/.139
  race_ratios$other_ratio <- race_ratios$other_frac/.072
  race_ratios$white_ratio <- race_ratios$white_frac/.557
  
    # Ratio of arrest rates, by race and category
  race_ratios$asian_white <- race_ratios$asian_ratio/race_ratios$white_ratio
  race_ratios$black_white <- race_ratios$black_ratio/race_ratios$white_ratio
  race_ratios$hisp_white <- race_ratios$hisp_ratio/race_ratios$white_ratio
  race_ratios$other_white <- race_ratios$other_ratio/race_ratios$white_ratio
  
  race_ratios$asian_white <- round(race_ratios$asian_white, 2)
  race_ratios$black_white <- round(race_ratios$black_white, 2)
  race_ratios$hisp_white <- round(race_ratios$hisp_white, 2)
  race_ratios$other_white <- round(race_ratios$other_white, 2)
  
    # Reshape for plots
  race_ratios_long <- melt(race_ratios[, c(1, 17:20)], id.vars = "category")
  
    # Visualizations
      # Restricting to major arrest categories
  
  ggplot(subset(race_ratios_long, category %in% freq_arrests)) +
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
                                 "Other-White",
                                 "Hispanic-White"), 
                        name = "Racial arrest ratio") +
    geom_hline(yintercept = 1, color = "grey10", linetype = "dashed") +
    scale_y_continuous(breaks=seq(0,10, by =1))
  
  # # Race/arrest ratios by severity----
  # race_ratios_sev <- dcast(totals_race_sev, severity ~ race, value.var = "total_count",
  #                      fun.aggregate = sum)
  # 
  # # Remove missing, pull "all" to top of sort order
  # race_ratios_sev <- subset(race_ratios_sev, severity != "(Missing data)")
  # race_ratios_sev$severity[race_ratios_sev$severity=="All"] <- "[All]"
  # race_ratios_sev <- race_ratios_sev %>% arrange(severity)
  # 
  # # Calculate raw fractions of arrests by race
  # race_ratios_sev$asian_frac <- race_ratios_sev$Asian/
  #   (race_ratios_sev$Asian + race_ratios_sev$Black + race_ratios_sev$Hispanic +
  #      race_ratios_sev$`Other/Unknown` + race_ratios_sev$White)
  # race_ratios_sev$black_frac <- race_ratios_sev$Black/
  #   (race_ratios_sev$Asian + race_ratios_sev$Black + race_ratios_sev$Hispanic +
  #      race_ratios_sev$`Other/Unknown` + race_ratios_sev$White)
  # race_ratios_sev$hispanic_frac <- race_ratios_sev$Hispanic/
  #   (race_ratios_sev$Asian + race_ratios_sev$Black + race_ratios_sev$Hispanic +
  #      race_ratios_sev$`Other/Unknown` + race_ratios_sev$White)
  # race_ratios_sev$other_frac <- race_ratios_sev$`Other/Unknown`/
  #   (race_ratios_sev$Asian + race_ratios_sev$Black + race_ratios_sev$Hispanic +
  #      race_ratios_sev$`Other/Unknown` + race_ratios_sev$White)
  # race_ratios_sev$white_frac <- race_ratios_sev$White/
  #   (race_ratios_sev$Asian + race_ratios_sev$Black + race_ratios_sev$Hispanic +
  #      race_ratios_sev$`Other/Unknown` + race_ratios_sev$White)
  # 
  # # Compare with population information from 2010 census, 2019 projections. 
  # # >=2 races included in "other."
  # # Source: https://www.census.gov/quickfacts/fact/table/daviscitycalifornia,US/PST045219
  # 
  # # Arrest rate: arrest fraction relative to population fraction
  # race_ratios_sev$asian_ratio<- race_ratios_sev$asian_frac/.222
  # race_ratios_sev$black_ratio <- race_ratios_sev$black_frac/.023
  # race_ratios_sev$hisp_ratio <- race_ratios_sev$hispanic_frac/.139
  # race_ratios_sev$other_ratio <- race_ratios_sev$other_frac/.072
  # race_ratios_sev$white_ratio <- race_ratios_sev$white_frac/.557
  # 
  # # Ratio of arrest rates, by race and severity
  # race_ratios_sev$asian_white <- race_ratios_sev$asian_ratio/
  #   race_ratios_sev$white_ratio
  # race_ratios_sev$black_white <- race_ratios_sev$black_ratio/
  #   race_ratios_sev$white_ratio
  # race_ratios_sev$hisp_white <- race_ratios_sev$hisp_ratio/
  #   race_ratios_sev$white_ratio
  # race_ratios_sev$other_white <- race_ratios_sev$other_ratio/
  #   race_ratios_sev$white_ratio
  # 
  # race_ratios_sev$asian_white <- round(race_ratios_sev$asian_white, 2)
  # race_ratios_sev$black_white <- round(race_ratios_sev$black_white, 2)
  # race_ratios_sev$hisp_white <- round(race_ratios_sev$hisp_white, 2)
  # race_ratios_sev$other_white <- round(race_ratios_sev$other_white, 2)
  # 
  # # Reshape for plots
  # race_ratios_sev_long <- melt(race_ratios_sev[, c(1, 17:20)], 
  #                              id.vars = "severity")
  # 
  # # Visualizations
  # # Restricting to major arrest categories
  # 
  # ggplot(race_ratios_sev_long) +
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
  # 
  # 
  # 
  # 
  # 
  # 
