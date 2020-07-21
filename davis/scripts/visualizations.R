library(tidyverse)
library(ggplot2)
library(ggtext)

  # SERVICE CALLS-----
    # Calls by charge category
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
    
  # ARRESTS----
  
    # By race
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
  guides(fill = F) +
  geom_text(aes(x = category, y = pct * 39.1, 
                label = paste(pct,"%", sep ="")), 
            position=position_dodge(width=0.9), 
            vjust=-0.25, size = 6)
  
  # CHARGES BY CATEGORY----
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
  
    # Totals by race
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

      # All and pop only
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
  
    # ARREST RATIOS----
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
  
    # DRUG CHARGES----
      # year trends
  ggplot(drug_charges) +
    geom_line(aes(x = date, 
             y = n, 
             group = description_1, 
             color = description_1)) +
    facet_wrap(~race) +
    guides(fill=guide_legend(ncol=2)) + 
    theme(legend.text = element_text(size = 6))
  
      # Race/drug charge type
  ggplot(drug_ch_type) +
    geom_col(aes(x = race,
                 y = n,
                 fill = type),
             position = "stack")
  
    # ARRESTS BY AGE & RACE-----
  ggplot(subset(age_race, age_group != "age_30_34"),
         aes(x = age_group, y = rate, 
             fill = race)) +
    geom_bar(stat = "identity", position = "dodge") +
    ylab("Arrests per 1000 people, 2018")
    
  