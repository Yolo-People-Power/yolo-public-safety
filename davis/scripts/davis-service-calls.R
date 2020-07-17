  library(tidyverse)
  library(readxl)
  library(tabulizer)
  library(rJava)
  library(chron)
  library(googlesheets4)
  
 setwd("/Users/bapu/Projects/watershed/action/public-safety/yolo/analysis/davis/")
  
  # Import datasets. All datasets from Davis PD, via PRR----
  
    # Service calls 2015-2020. This is from 
  calls_raw <- lapply(
    excel_sheets("./data/raw/service-calls-raw.xlsx"), 
    read_excel, path = "./data/raw/service-calls-raw.xlsx")

    # Disposition and incident codes
    # You will be asked to confirm email for Google sheets access
  disp_code <- read_sheet(
    ("https://docs.google.com/spreadsheets/d/1SrhDb-rxyqIBRAQBG6anMMDuILskZcbDtiO6iP7hgcw/edit?usp=sharing"))
  disp_code <- disp_code[,1:2] 
      # Rename value field for later joining
  colnames(disp_code) <- c("d1", "disp_desc")
  
    # Incident types
  incident_type <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1svbcwdO2EqMoTQHGl5g37CjHaaQNzfceL1A6AjB2fCU/edit?usp=sharing")
  incident_type$type <- unlist(incident_type$type)
  incident_type <- incident_type %>% mutate_all(as.factor)
  
  # Prep datasets----
  
    # Bind service call list objects into one data frame
  calls <- bind_rows(calls_raw)
  colnames(calls) = c("inc_num", "date", "time", "type", "beat", "street",
                      "cross_str", "d1")
  calls_desc <- left_join(calls, disp_code, by = "d1")
  calls_desc <- left_join(calls_desc, incident_type, by = "type")
  
    # Delete empty rows
  calls_desc <- calls_desc %>% filter_all(any_vars(!is.na(.)))
  
    # Correct col classes
  calls_desc <- calls_desc %>% mutate_at(vars(
    inc_num, type, beat, d1, disp_desc), as.factor)
  calls_desc$time <- times(calls_desc$time)
  
  # Export datasets----
  write.csv(calls_desc, "./data/generated/service-calls.csv")

  
 


    