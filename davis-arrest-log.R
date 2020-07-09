  library(tabulizer)
  library(plyr)
  library(tidyverse)
  library(ggplot2)
  library(rJava)
  
  setwd("/Users/bapu/Projects/watershed/action/public-safety")
  
  # Scrape arrest log
  davis_arrest_scrape <- extract_tables(
    file = "./davis/davis-arrest-log.pdf",
    method = "decide",
    output = "data.frame", 
    header = F)
  
  # Clean up
  colnames <- c("date", "charge1", "charge2", "charge3", "sex", "race", "age")
  davis_log <- lapply(davis_arrest_scrape, setNames, colnames)
  davis_log <- rbind.fill(davis_log)
  davis_log <- davis_log[c(-1, -2852),]
  rownames(davis_log) <- 1:nrow(davis_log)
  
  # Reshape long [add event IDs, make charges long]
  
  
  # Corrections [spacing on codes]
  davis_log[24,2] <- "23224(B) VC"
  davis_log[1285,2] <- "23247(e) VC"
  davis_log[2629,2] <- "273a(b) PC"
  davis_log[593,2] <- "26.04.020(a) CC"
  davis_log[2130,2] <- "459.5(a) PC	"
  davis_log[2135,2] <- "459.5(a) PC"
  davis_log[2153,2] <- "459.5(a) PC	"
  davis_log[2162,2] <- "459.5(a) PC	"
  davis_log[2194,2] <- "459.5(a) PC"
  davis_log[2231,2] <- ""
  davis_log[2269,2] <- ""
  davis_log[2309,2] <- ""
  davis_log[2310,2] <- ""
  davis_log[2364,2] <- ""
  davis_log[2397,2] <- ""
  davis_log[2461,2] <- ""
  davis_log[2514,2] <- ""
  davis_log[2768,2] <- ""
  davis_log[2770,2] <- ""
  davis_log[2803,2] <- ""
  davis_log[2835,2] <- ""
  
  davis_log2 <- davis_log %>% separate(charge1, c("charge1", "charge1_code"), 
                                      sep = " ")
  
  # Import CA code library
  
  
  # Merge code library with case log
  
  
  # Visualizations
  
  
  
  
