  library(tabulizer)
  library(rJava)
  library(plyr)
  library(tidyverse)
  library(reshape2)
  library(googlesheets4)
  
  setwd("/Users/bapu/Projects/watershed/action/public-safety/ssc-analysis")
  
  # Scrape arrest log----
  davis_log_raw <- extract_tables(
    file = "./data/davis-arrest-log.pdf",
    method = "decide",
    output = "data.frame", 
    header = F)
  
  # Clean up arrest log----
    # Rename columns
  colnames <- c("date", "charge1", "charge2", "charge3", "sex", "race", "age")
  davis_log <- lapply(davis_log_raw, setNames, colnames)
    # Bind list into one data frame
  davis_log <- rbind.fill(davis_log)
    # Remove old column name row, blank final row
  davis_log <- davis_log[c(-1, -2852),]
    # Renumber rows & add incident ID column with row numbers
  rownames(davis_log) <- 1:nrow(davis_log)
  davis_log <- cbind("incident_id" = rownames(davis_log), data.frame(davis_log))
    # Fix errors in race field
  davis_log$race <- gsub("Be lAacmkerican|eB lAacmkerican", 
                         "Black", davis_log$race)
  davis_log$race <- gsub("eH iAspmaenriiccan|HIsilsapnadneirc", 
                         "Hispanic", davis_log$race)
  davis_log$race <- gsub("eO tAhmererican", 
                         "Other", davis_log$race)
  davis_log$race <- gsub("eW Ahmiteerican|We Ahmiteerican", 
                         "White", davis_log$race)
  davis_log$race <- gsub("OIsltahnedr eArsian", 
                         "Other Asian", davis_log$race)
  
  # Reshape and prep dataset----
  davis_log <- melt(davis_log, id.vars = 
                 c("incident_id", "date", "sex", "race", "age"))
  colnames(davis_log) <- c("incident_id", "date", "sex", "race", "age",
                           "charge_num", "sec_code")
    # Fill in rows with missing race, remove rows with blank charges
  davis_log$race <- sub("^$", "Missing", davis_log$race)
  davis_log <- davis_log %>% na_if("") %>% na.omit
    # Standardize charge codes (capitalize, delete spaces)
  davis_log$sec_code <- toupper(davis_log$sec_code)
  davis_log$sec_code <- gsub(" ", "", davis_log$sec_code, fixed = TRUE)
    # Correct typos in charge codes
  davis_log$sec_code[davis_log$sec_code == "508"] <- "508PC"
  davis_log$sec_code[davis_log$sec_code == "11364"] <- "11364(A)HS"
    # Correct col classes
  davis_log <- davis_log %>% mutate_at(vars(incident_id, sex, race, charge_num,
                                            sec_code), as.factor)
  davis_log$age <- as.numeric(davis_log$age)
  davis_log$date <- mdy(davis_log$date)
  
  # Add & clean up CA law enforcement code tables----
  le_code_raw <- read.csv(url(
    "https://oag.ca.gov/sites/all/files/agweb/law-enforcement/code-tables/macrcode.csv?070920201056"),
    header = FALSE)
  
    # Retain only code, section, description and sentence (don't know what other columns are!)
  le_code <- le_code_raw[c("V6", "V5", "V7", "V8")]
  colnames(le_code) <- c("ca_code", "section_full", 
                         "description", "sentence")
  
    # Add missing sections, codes, & code descriptions
  le_code <- le_code %>% add_row(section_full = "11357(A)", ca_code ="HS", 
                                 description = "POSSESSION MARIJUANA")
  le_code <- le_code %>% add_row(section_full = "11357(D)", ca_code ="HS", 
                                 description = "POSSESSION MARIJUANA")
  le_code <- le_code %>% add_row(section_full = "11357(E)", ca_code ="HS", 
                                 description = "POSSESSION MARIJUANA")
  le_code <- le_code %>% add_row(section_full = "11359", ca_code ="HS", 
                                 description = "POSSESSION MARIJUANA SALE")
  le_code <- le_code %>% add_row(section_full = "11364.1(A)", ca_code ="HS", 
                                 description = "POSSESSION DRUG PARAPHENALIA")
  le_code <- le_code %>% add_row(section_full = "11365", ca_code ="HS", 
                                 description = "VISIT WHERE CONTROLLED SUBSTANCE USED")
  le_code <- le_code %>% add_row(section_full = "11375(B)", ca_code ="HS", 
                                 description = "POSS FOR SALE DESIGNATED CONTROLLED SUBSTANCE")
  le_code <- le_code %>% add_row(section_full = "11379", ca_code ="HS", 
                                 description = "TRANSPORT CONTROLLED SUBSTANCE")
  le_code <- le_code %>% add_row(section_full = "12020(A)", ca_code ="PC", 
                                 description = "WEAPONS POSSESSION")
  le_code <- le_code %>% add_row(section_full = "12021(A)", ca_code ="PC", 
                                 description = "CONVICTED PERSON WEAPONS POSSESSION")
  le_code <- le_code %>% add_row(section_full = "12022.1(B)", ca_code ="PC", 
                                 description = "FELONY WEAPONS POSSESSION")
  le_code <- le_code %>% add_row(section_full = "12022.53(B)", ca_code ="PC", 
                                 description = "FELONY WEAPONS POSSESSION")
  le_code <- le_code %>% add_row(section_full = "12022.53(D)", ca_code ="PC", 
                                 description = "FELONY WEAPONS POSSESSION")
  le_code <- le_code %>% add_row(section_full = "12022.7(A)", ca_code ="PC", 
                                 description = "FELONY BODILY INJURY")
  le_code <- le_code %>% add_row(section_full = "12022(A)(1)", ca_code ="PC", 
                                 description = "FELONY WEAPONS POSSESSION")
  le_code <- le_code %>% add_row(section_full = "12022(C)", ca_code ="PC", 
                                 description = "FELONY WEAPONS POSSESSION")
  le_code <- le_code %>% add_row(section_full = "1203.2(A)(1)", ca_code ="PC", 
                                 description = "PROBATION VIOLATION")
  le_code <- le_code %>% add_row(section_full = "1203.2(A)(4)", ca_code ="PC", 
                                 description = "PROBATION VIOLATION")
  le_code <- le_code %>% add_row(section_full = "1203.2(A)", ca_code ="PC", 
                                 description = "PROBATION VIOLATION")
  le_code <- le_code %>% add_row(section_full = "12090", ca_code ="PC", 
                                 description = "UNMARKED FIREARMS")
  le_code <- le_code %>% add_row(section_full = "13700", ca_code ="PC", 
                                 description = "DOMESTIC VIOLENCE")
  le_code <- le_code %>% add_row(section_full = "16028(A)", ca_code ="VC", 
                                 description = "UNINSURED VEHICLE")
  le_code <- le_code %>% add_row(section_full = "21453(A)", ca_code ="VC", 
                                 description = "RED LIGHT VIOLATION")
  le_code <- le_code %>% add_row(section_full = "21806(A)(1)", ca_code ="VC", 
                                 description = "NO PULLOVER EMERGENCY VEHICLE")
  le_code <- le_code %>% add_row(section_full = "22350", ca_code ="VC", 
                                 description = "UNSAFE SPEED")
  le_code <- le_code %>% add_row(section_full = "22450(A)", ca_code ="VC", 
                                 description = "RUN STOP SIGN")
  le_code <- le_code %>% add_row(section_full = "22810", ca_code ="PC", 
                                 description = "UNLAWFUL POSSESSION/USE TEAR GAS OR TEAR GAS WEAPON")
  le_code <- le_code %>% add_row(section_full = "23136(A)", ca_code ="VC", 
                                 description = "UNDERAGE DRUNK DRIVING 0.01")
  le_code <- le_code %>% add_row(section_full = "23140(A)", ca_code ="VC", 
                                 description = "UNDERAGE DRUNK DRIVING 0.05")
  le_code <- le_code %>% add_row(section_full = "23153(E)", ca_code ="VC", 
                                 description = "DUI ALCOHOL:CAUSING BODILY INJURY")
  le_code <- le_code %>% add_row(section_full = "23154(A)", ca_code ="VC", 
                                 description = "PROBATION DRUNK DRIVING 0.01")
  le_code <- le_code %>% add_row(section_full = "23222(B)", ca_code ="VC", 
                                 description = "DRIVING OPEN MARIJUANA")
  le_code <- le_code %>% add_row(section_full = "23223(A)", ca_code ="VC", 
                                 description = "DRIVING OPEN BOTTLE")
  le_code <- le_code %>% add_row(section_full = "23223(B)", ca_code ="VC", 
                                 description = "DRIVING OPEN BOTTLE")
  le_code <- le_code %>% add_row(section_full = "23226(B)", ca_code ="VC", 
                                 description = "DRIVING OPEN BOTTLE COMPARTMENT")
  le_code <- le_code %>% add_row(section_full = "237", ca_code ="PC", 
                                 description = "FALSE IMPRISONMENT")
  le_code <- le_code %>% add_row(section_full = "246.3", ca_code ="PC", 
                                 description = "WILLFUL DISCHARGE OF FIREARM IN A GROSSLY NEGLIGENT MANNER")
  le_code <- le_code %>% add_row(section_full = "273A(1)", ca_code ="PC",
                                 description = "WILLFUL CRUELTY TO CHILD:POSSIBLE INJURY/DEATH")
  le_code <- le_code %>% add_row(section_full = "273A(A)(1)", ca_code ="PC", 
                                 description = "WILLFUL CRUELTY TO CHILD:POSSIBLE INJURY/DEATH")
  le_code <- le_code %>% add_row(section_full = "273A(B)", ca_code = "PC",
                                 description = "WILLFUL CRUELTY TO CHILD:POSSIBLE INJURY/DEATH")
  le_code <- le_code %>% add_row(section_full = "2800.1", ca_code ="VC", 
                                 description = "EVADING PEACE OFFICER")
  le_code <- le_code %>% add_row(section_full = "2800.2", ca_code ="VC", 
                                 description = "EVADE PEACE OFFICER WITH WANTON DISREGARD FOR SAFETY")
  le_code <- le_code %>% add_row(section_full = "288.4", ca_code ="PC", 
                                 description = "ARRANGE A MEETING WITH MINOR")
  le_code <- le_code %>% add_row(section_full = "288A(G)", ca_code ="PC", 
                                 description = "ARRANGE A MEETING WITH MINOR")
  le_code <- le_code %>% add_row(section_full = "3455(A)", ca_code ="PC", 
                                 description = "POSTRELEASE COMMUNITY SUPERVISION VIOLATION")
  le_code <- le_code %>% add_row(section_full = "368(A)", ca_code ="PC", 
                                 description = "CAUSE HARM/DEATH OF ELDER /DEPENDENT ADULT")
  le_code <- le_code %>% add_row(section_full = "368(B)", ca_code ="PC", 
                                 description = "CAUSE HARM/DEATH OF ELDER /DEPENDENT ADULT")
  le_code <- le_code %>% add_row(section_full = "4000(A)", ca_code ="VC", 
                                 description = "UNREGISTERED VEHICLE")
  le_code <- le_code %>% add_row(section_full = "40302(B)", ca_code ="VC", 
                                 description = "FAILURE TO PRESENT ID")
  le_code <- le_code %>% add_row(section_full = "404.6", ca_code ="PC", 
                                 description = "RIOT IN PRISON/JAIL/ETC RESULTS IN SERIOUS BODILY INJURY")
  le_code <- le_code %>% add_row(section_full = "4140BP", ca_code = "PC",
                                 description = "POSSESSION HYPODERMIC NEEDLE")
  le_code <- le_code %>% add_row(section_full = "417.2(A)", ca_code ="PC", 
                                 description = "THREATEN WITH LASER SCOPE WITH INTENT TO CAUSE FEAR")
  le_code <- le_code %>% add_row(section_full = "422", ca_code ="PC", 
                                 description = "THREATEN CRIME WITH INTENT TO TERRORIZE")
  le_code <- le_code %>% add_row(section_full = "4462(B)", ca_code ="VC", 
                                 description = "UNLAWFUL POSSESSION OF VEHICLE REGISTRATION/ETC")
  le_code <- le_code %>% add_row(section_full = "4573(A)", ca_code ="PC", 
                                 description = "BRING CONTROLLED SUBSTANCE/ETC INTO PRISON/JAIL/ETC")
  le_code <- le_code %>% add_row(section_full = "459.5(A)", ca_code ="PC", 
                                 description = "SHOPLIFTING")
  le_code <- le_code %>% add_row(section_full = "475", ca_code ="PC", 
                                 description = "POSSESS/ETC BAD/ETC CHECK/ETC")
  le_code <- le_code %>% add_row(section_full = "484E(2)", ca_code ="PC", 
                                 description = "SELL/ETC LOST/ETC ACCESS CARD")
  le_code <- le_code %>% add_row(section_full = "484F(1)", ca_code ="PC", 
                                 description = "FORGE ACCESS CARD TO DEFRAUD")
  le_code <- le_code %>% add_row(section_full = "530.5", ca_code ="PC", 
                                 description = "OBTAIN CREDIT/ETC:USE OTHERS ID")
  le_code <- le_code %>% add_row(section_full = "594(A)", ca_code ="PC", 
                                 description = "VANDALISM:DEFACE PROPERTY")
  le_code <- le_code %>% add_row(section_full = "594(B)(3)", ca_code ="PC", 
                                 description = "VANDALISM ($400 OR MORE)")
  le_code <- le_code %>% add_row(section_full = "626.10(A)", ca_code ="PC", 
                                 description = "POSSESS WEAPON/ETC AT SCHOOL")
  le_code <- le_code %>% add_row(section_full = "647(J)", ca_code ="PC", 
                                 description = "DISORDERLY CONDUCT:INVADE PRIVACY W/CAMERA/ETC IN BATHRM/ETC")
  le_code <- le_code %>% add_row(section_full = "664/10851(A)", ca_code ="VC", 
                                 description = "VEHICLE WITHOUT CONSENT")
  le_code <- le_code %>% add_row(section_full = "664/207", ca_code ="PC", 
                                 description = "KIDNAPPING")
  le_code <- le_code %>% add_row(section_full = "664/211", ca_code ="PC",
                                 description = "ROBBERY")
  le_code <- le_code %>% add_row(section_full = "664/459", ca_code ="PC", 
                                 description = "BURGLARY")
  le_code <- le_code %>% add_row(section_full = "664/487", ca_code ="PC", 
                                 description = "GRAND THEFT:MONEY/LABOR/PROPERTY")
  le_code <- le_code %>% add_row(section_full = "664/488", ca_code ="PC", 
                                 description = "PETTY THEFT")
  le_code <- le_code %>% add_row(section_full = "664/490.5(A)", ca_code ="PC", 
                                 description = "PETTY THEFT:RETAIL MERCHANDISE/ETC")
  le_code <- le_code %>% add_row(section_full = "666", ca_code ="PC", 
                                description = "PETTY THEFT:PRIOR SPECIAL CONVICTION SEX REG REQUIRED")
  
    # Code corrections
  le_code$section_full <- gsub("266I \\(", "266 I\\(", le_code$section_full)
  le_code$section_full <- gsub("653F \\(", "653 F\\(", le_code$section_full)
  le_code$section_full <- gsub("0\\(", "0 \\(", le_code$section_full)
  le_code$section_full <- gsub("1\\(", "1 \\(", le_code$section_full)
  le_code$section_full <- gsub("2\\(", "2 \\(", le_code$section_full)
  le_code$section_full <- gsub("3\\(", "3 \\(", le_code$section_full)
  le_code$section_full <- gsub("4\\(", "4 \\(", le_code$section_full)
  le_code$section_full <- gsub("5\\(", "5 \\(", le_code$section_full)
  le_code$section_full <- gsub("6\\(", "6 \\(", le_code$section_full)
  le_code$section_full <- gsub("7\\(", "7 \\(", le_code$section_full)
  le_code$section_full <- gsub("8\\(", "8 \\(", le_code$section_full)
  le_code$section_full <- gsub("9\\(", "9 \\(", le_code$section_full)
  le_code$section_full <- gsub("3A", "3 A", le_code$section_full)
  le_code$section_full <- gsub("288A", "288 A", le_code$section_full)
  le_code$section_full[le_code$section_full=="148.2.1"] <- "148.2 (1)"
  le_code$section_full[le_code$section_full=="330B (A)"] <- "330 B(A)"
  le_code$section_full[le_code$section_full=="374.5(A) (1)"] <- "374.5 (A) (1)"
  le_code$section_full[le_code$section_full=="664/187 (A)"] <- "187 (A) [664]"
  le_code$section_full[le_code$section_full=="664/189"] <- "189 [664]"
  le_code$section_full[le_code$section_full=="1798.90.1 (A) (1)"] <- 
    "1798.90 (1) (A) (1)"
  le_code$section_full[le_code$section_full=="3A"] <- "3 A"
  le_code$section_full[le_code$section_full=="4140BP"] <- "4140 BP"
  le_code$section_full[le_code$section_full=="484E(2)"] <- "484 E(2)"
  le_code$section_full[le_code$section_full=="484F(1)"] <- "484 F(1)"
  le_code$section_full[le_code$section_full=="664/10851 (A)"] <- 
    "10851 (A) [664]"
  le_code$section_full[le_code$section_full=="664/207"] <- "207 [664]"
  le_code$section_full[le_code$section_full=="664/211"] <- "211 [664]"
  le_code$section_full[le_code$section_full=="664/459"] <- "459 [664]"
  le_code$section_full[le_code$section_full=="664/487"] <- "487 [664]"
  le_code$section_full[le_code$section_full=="664/488"] <- "488 [488"
  le_code$section_full[le_code$section_full=="664/490.5 (A)"] <- 
    "490.5 (A) [664]"
    # Delete non-code lines
  le_code <- subset(le_code, !section_full=="AGRICULTURE")
  le_code <- subset(le_code, !section_full=="DESERTION")
  le_code <- subset(le_code, !section_full=="EDUCATION")
  le_code <- subset(le_code, !section_full=="ELECTION LAWS")
  le_code <- subset(le_code, !section_full=="FEDERAL OFFENSE")
  le_code <- subset(le_code, !section_full=="FIRE")
  le_code <- subset(le_code, !section_full=="FISH AND GAME")
  le_code <- subset(le_code, !section_full=="IMMIGRATION")
  le_code <- subset(le_code, !section_full=="LOCAL")
  le_code <- subset(le_code, !section_full=="LABOR")
  le_code <- subset(le_code, !section_full=="MILITARY")
  le_code <- subset(le_code, !section_full=="OUTSIDE WARR/F")
  le_code <- subset(le_code, !section_full=="OUTSIDE WARR/M")
  le_code <- subset(le_code, !section_full=="REGULATIONS")
  le_code <- subset(le_code, !section_full=="TRAFFIC OFN")
  le_code <- subset(le_code, !section_full=="VIOL PROB/FEL")
  le_code <- subset(le_code, !section_full=="VIOL PROB/MISD")
  le_code <- subset(le_code, !section_full=="WATERCRAFT")
  
    # Unite charge columns for rapid correction purposes
  le_code$sec_code <- paste(le_code$section_full, le_code$ca_code, sep = "")
  le_code$sec_code<- gsub(" ", "", le_code$sec_code, fixed = TRUE)
  
    # Separate section and subsection columns
  le_code$section_full <- gsub("\\)\\(", "\\) \\(", le_code$section_full)
  
  le_code <- le_code %>% separate(section_full, 
                                  into = c("section", "subsection1", 
                                           "subsection2", "subsection3",
                                           "subsection4", "subsection5"),
                                  sep = " ",
                                  remove = FALSE)
  
    # Change class of section to allow for numeric merge with CA Codes
  le_code$section <- as.numeric(le_code$section)
  
    # Add CA Codes headings
  le_code[, c("part", "part_head", "title", "title_head", 
              "chapter", "chapter_head")] <- NA
  
    # Add Codes values----
      # Penal Code Parts
  le_code <- le_code %>% 
    mutate(part = case_when(section < 681 & ca_code == "PC" ~ 1,
                           section >=681 & section <=1620 & ca_code == "PC" ~ 2 ,
                           section >=2000 & section <=10007 & ca_code == "PC" ~ 3,
                           section >=11006 & section <=14315 & ca_code == "PC" ~ 4,
                           section >=15001 & section <=15003 & ca_code == "PC" ~ 5,
                           section >=16000 & section <=34370 & ca_code == "PC" ~ 6))
  le_code <- le_code %>% 
    mutate(part_head = case_when(section < 681 & ca_code == "PC" 
                                 ~ "crimes and punishments",
                            section >=681 & section <=1620 & ca_code == "PC" ~ 
                              "criminal procedure",
                            section >=2000 & section <=10007 & ca_code == "PC" ~ 
                              "imprisonment and the death penalty",
                            section >=11006 & section <=14315 & ca_code == "PC" ~ 
                              "prevention of crimes and apprehension of criminals",
                            section >=15001 & section <=15003 & ca_code == "PC" ~ 
                              "peace officers' memorial",
                            section >=16000 & section <=34370 & ca_code == "PC" ~ 
                              "control of deadly weapons"))
      # Penal Code Titles
  le_code <- le_code %>%
    mutate(title = case_when(
                             section >=25 & section <=29.8 & ca_code == "PC" ~ 1,
                             section >=30 & section <=33 & ca_code == "PC" ~ 2 ,
                             section >=37  & section <=38 & ca_code == "PC" ~ 3 ,
                             section >=67  & section <=77 & ca_code == "PC" ~ 5 ,
                             section >=85  & section <=88 & ca_code == "PC" ~ 6,
                             section >=92  & section <=186.36 & ca_code == "PC" ~ 7,
                             section >=187  & section <=248 & ca_code == "PC" ~ 8,
                             section >=261  & section <=368.7 & ca_code == "PC" ~ 9,
                             section >=369  & section <=402 & ca_code == "PC" ~ 10,
                             section >=403  & section <=420.1 & ca_code == "PC" ~ 11,
                             section >=422  & section <=422.4  & ca_code == "PC" ~ 11.5,
                             section >=422.55  & section <=422.57 & ca_code == "PC" ~ 11.6,
                             section >=423 & section <=423.6 & ca_code == "PC" ~ 11.7,
                             section >=424  & section <=440 & ca_code == "PC" ~ 12,
                             section >=450  & section <=593 & ca_code == "PC" ~ 13,
                             section >=594  & section <=625 & ca_code == "PC" ~ 14,
                             section >=626  & section <=653.75 & ca_code == "PC" ~ 15,
                             section >=654  & section <=678 & ca_code == "PC" ~ 16 ,
                             section >=679  & section <=680.4 & ca_code == "PC" ~ 17))
  
  le_code <- le_code %>%
    mutate(title_head = case_when(
      section >=25 & section <=29.8 & ca_code == "PC" ~ 
        "persons liable to punishment for crime",
      section >=30 & section <=33 & ca_code == "PC" ~ 
        "parties to crime" ,
      section >=37  & section <=38 & ca_code == "PC" ~ 
        "offenses against the sovereignty of the state",
      section >=67  & section <=77 & ca_code == "PC" ~ 
        "crimes by and against the executive power of the state",
      section >=85  & section <=88 & ca_code == "PC" ~ 
        "crimes against the legislative power",
      section >=92  & section <=186.36 & ca_code == "PC" ~ 
        "crimes against public justice",
      section >=187  & section <=248 & ca_code == "PC" ~ 
        "crimes against the person",
      section >=261  & section <=368.7 & ca_code == "PC" ~ 
        "crimes against the person involving sexual assault, and crimes against public decency and good morals",
      section >=369  & section <=402 & ca_code == "PC" ~ 
        "crimes against the public health and safety",
      section >=403  & section <=420.1 & ca_code == "PC" ~ 
        "crimes against the public peace",
      section >=422  & section <=422.4  & ca_code == "PC" ~ 
        "criminal threats",
      section >=422.55  & section <=422.57 & ca_code == "PC" ~ 
        "civil rights",
      section >=423 & section <=423.6 & ca_code == "PC" ~ 
        "california freedom of access to clinic and church entrances act",
      section >=424  & section <=440 & ca_code == "PC" ~ 
        "crimes against the revenue and property of this state",
      section >=450  & section <=593 & ca_code == "PC" ~ 
        "crimes against property",
      section >=594  & section <=625 & ca_code == "PC" ~ 
        "malicious mischief",
      section >=626  & section <=653.75 & ca_code == "PC" ~ 
        "miscellaneous crimes",
      section >=654  & section <=678 & ca_code == "PC" ~ 
        "general provisions",
      section >=679  & section <=680.4 & ca_code == "PC" ~ 
        "rights of victims and witnesses of crime"))
  
    # Penal Code Chapters
  le_code <- le_code %>% 
    mutate(chapter = case_when(
      section >=92 & section <=100 & ca_code == "PC" ~ 1,
      section ==102 & ca_code == "PC" ~ 2,
      section >=107 & section <=110 & ca_code == "PC" ~ 3,
      section >=112 & section <=117 & ca_code == "PC" ~ 4,
      section >=118 & section <=131 & ca_code == "PC" ~ 5 ,
      section >=132 & section <=141 & ca_code == "PC" ~ 6,
      section >=142 & section <=181 & ca_code == "PC" ~ 7,
      section >=182 & section <=185 & ca_code == "PC" ~ 8,
      section >=186 & section <=186.8 & ca_code == "PC" ~ 9,
      section >=186.9 & section <=186.10 & ca_code == "PC" ~ 10 ,
      section >=186.11 & section <=186.12 & ca_code == "PC" ~ 10.5,
      section >=186.20 & section <=186.36 & ca_code == "PC" ~ 11,
      section >=187 & section <=199 & ca_code == "PC" ~ 1 ,
      section >=203 & section <=206.1 & ca_code == "PC" ~ 2,
      section >=207 & section <=210 & ca_code == "PC" ~ 3,
      section ==210.5 & ca_code == "PC" ~ 3.5,
      section >=211 & section <=215 & ca_code == "PC" ~ 4,
      section >=217.1 & section <=219.3 & ca_code == "PC" ~ 5 ,
      section >=220 & section <=222 & ca_code == "PC" ~ 6,
      section >=236 & section <=237 & ca_code == "PC" ~ 8,
      section >=240 & section <=248 & ca_code == "PC" ~ 9,
      section >=261 & section <=269 & ca_code == "PC" ~ 1,
      section >=270 & section <273.75 & ca_code == "PC" ~ 2,
      section >=273.8 & section <=273.88 & ca_code == "PC" ~ 2.5,
      section >=277 & section <=280 & ca_code == "PC" ~ 4,
      section >=281 & section <=289.6 & ca_code == "PC" ~ 5,
      section >=290 & section <=294 & ca_code == "PC" ~ 5.5,
      section >=295 & section <=300.4 & ca_code == "PC" ~ 6,
      section >=302 & section <=310.5 & ca_code == "PC" ~ 7,
      section >=311 & section <=312.7 & ca_code == "PC" ~ 7.5,
      section >=313 & section <=313.5 & ca_code == "PC" ~ 7.6,
      section >=314 & section <=318.6 & ca_code == "PC" ~ 8,
      section >=319 & section <=329 & ca_code == "PC" ~ 9,
      section >=330 & section <=337 & ca_code == "PC" ~ 10,
      section >=337.1 & section <=337.9 & ca_code == "PC" ~ 10.5,
      section ==343 & ca_code == "PC" ~ 11,
      section >=346 & section <=367 & ca_code == "PC" ~ 12,
      section >=368 & section <=368.7 & ca_code == "PC" ~ 13,
      section >=422.55 & section <=422.57 & ca_code == "PC" ~ 1,
      section >=422.6 & section <=422.865 & ca_code == "PC" ~ 2,
      section ==422.87 & ca_code == "PC" ~ 2.5,
      section >=422.88 & section <=422.93 & ca_code == "PC" ~ 3,
      section >=450 & section <=457.1 & ca_code == "PC" ~ 1,
      section >=458 & section <=464 & ca_code == "PC" ~ 2,
      section >=466 & section <=469 & ca_code == "PC" ~ 3,
      section >=470 & section <=483.5 & ca_code == "PC" ~ 4,
      section >=484 & section <=502.9 & ca_code == "PC" ~ 5,
      section >=503 & section <=515 & ca_code == "PC" ~ 6,
      section >=518 & section <=527 & ca_code == "PC" ~ 7,
      section >=528 & section <=539 & ca_code == "PC" ~ 8,
      section >=548 & section <=551 & ca_code == "PC" ~ 10,
      section >=552 & section <=558.1 & ca_code == "PC" ~ 12,
      section >=560 & section <=560.6 & ca_code == "PC" ~ 12.5,
      section >=565 & section <=566 & ca_code == "PC" ~ 12.6,
      section >=570 & section <=574 & ca_code == "PC" ~ 12.7,
      section >=577 & section <=583 & ca_code == "PC" ~ 14,
      section >=587 & section <=593 & ca_code == "PC" ~ 15,
      section >=626 & section <=626.11 & ca_code == "PC" ~ 1,
      section >=627 & section <=627.10 & ca_code == "PC" ~ 1.1,
      section >=628 & section <=628.5 & ca_code == "PC" ~ 1.3,
      section >=629.50 & section <=629.98 & ca_code == "PC" ~ 1.4,
      section >=630 & section <=638.55 & ca_code == "PC" ~ 1.5,
      section >=639 & section <=653.2 & ca_code == "PC" ~ 1,
      section >=653.20 & section <=653.28 & ca_code == "PC" ~ 1.5,
      section >=653.55 & section <=653.61 & ca_code == "PC" ~ 3,
      section ==653.75 & ca_code == "PC" ~ 4))
  
  # Chapters
  le_code <- le_code %>% 
    mutate(chapter_head = case_when(
      section >=92 & section <=100 & ca_code == "PC" ~ 
        "bribery and corruption",
      section ==102 & ca_code == "PC" ~ 
        "rescues",
      section >=107 & section <=110 & ca_code == "PC" ~ 
        "escapes and aiding therein",
      section >=112 & section <=117 & ca_code == "PC" ~ 
        "forging, stealing, mutilating, and falsifying judicial and public records and documents",
      section >=118 & section <=131 & ca_code == "PC" ~ 
        "perjury and subornation of perjury",
      section >=132 & section <=141 & ca_code == "PC" ~ 
        "falsifying evidence, and bribing, influencing, intimidating or threatening witnesses",
      section >=142 & section <=181 & ca_code == "PC" ~ 
        "other offenses against public justice",
      section >=182 & section <=185 & ca_code == "PC" ~ 
        "conspiracy",
      section >=186 & section <=186.8 & ca_code == "PC" ~ 
        "criminal profiteering",
      section >=186.9 & section <=186.10 & ca_code == "PC" ~ 
        "money laundering",
      section >=186.11 & section <=186.12 & ca_code == "PC" ~ 
        "fraud and embezzlement: victim restitution",
      section >=186.20 & section <=186.36 & ca_code == "PC" ~ 
        "street terrorism enforcement and prevention act",
      section >=187 & section <=199 & ca_code == "PC" ~ 
        "homicide",
      section >=203 & section <=206.1 & ca_code == "PC" ~ 
        "mayhem",
      section >=207 & section <=210 & ca_code == "PC" ~ 
        "kidnapping",
      section ==210.5 & ca_code == "PC" ~ 
        "hostages",
      section >=211 & section <=215 & ca_code == "PC" ~ 
        "robbery",
      section >=217.1 & section <=219.3 & ca_code == "PC" ~ 
        "attempts to kill",
      section >=220 & section <=222 & ca_code == "PC" ~ 
        "assaults with intent to commit felony, other than assaults with intent to commit murder",
      section >=236 & section <=237 & ca_code == "PC" ~ 
        "false imprisonment and human trafficking",
      section >=240 & section <=248 & ca_code == "PC" ~ 
        "assault and battery",
      section >=261 & section <=269 & ca_code == "PC" ~ 
        "rape, abduction, carnal abuse of children, and seduction",
      section >=270 & section <273.75 & ca_code == "PC" ~ 
        "abandonment of children",
      section >=273.8 & section <=273.88 & ca_code == "PC" ~ 
        "spousal abusers",
      section >=277 & section <=280 & ca_code == "PC" ~ 
        "child abduction",
      section >=281 & section <=289.6 & ca_code == "PC" ~ 
        "bigamy, incest, and the crime against nature",
      section >=290 & section <=294 & ca_code == "PC" ~ 
        "sex offenders",
      section >=295 & section <=300.4 & ca_code == "PC" ~ 
        "dna and forensic identification data base and data bank act of 1998",
      section >=302 & section <=310.5 & ca_code == "PC" ~ 
        "crimes against religion and conscience, and other offenses against good morals",
      section >=311 & section <=312.7 & ca_code == "PC" ~ 
        "obscene matter",
      section >=313 & section <=313.5 & ca_code == "PC" ~ 
        "harmful matter",
      section >=314 & section <=318.6 & ca_code == "PC" ~ 
        "indecent exposure, obscene exhibitions, and bawdy and other disorderly houses",
      section >=319 & section <=329 & ca_code == "PC" ~ 
        "lotteries",
      section >=330 & section <=337 & ca_code == "PC" ~ 
        "gaming",
      section >=337.1 & section <=337.9 & ca_code == "PC" ~ 
        "horse racing",
      section ==343 & ca_code == "PC" ~ 
        "pawnbrokers",
      section >=346 & section <=367 & ca_code == "PC" ~ 
        "other injuries to persons",
      section >=368 & section <=368.7 & ca_code == "PC" ~
        "crimes against elders, dependent adults, and persons with disabilities",
      section >=422.55 & section <=422.57 & ca_code == "PC" ~ 
        "definitions",
      section >=422.6 & section <=422.865 & ca_code == "PC" ~ 
        "crimes and penalties",
      section ==422.87 & ca_code == "PC" ~ 
        "law enforcement agency policies",
      section >=422.88 & section <=422.93 & ca_code == "PC" ~ 
        "general provisions",
      section >=450 & section <=457.1 & ca_code == "PC" ~ 
        "arson",
      section >=458 & section <=464 & ca_code == "PC" ~ 
        "burglary",
      section >=466 & section <=469 & ca_code == "PC" ~ 
        "burglarious and larcenous instruments and deadly weapons",
      section >=470 & section <=483.5 & ca_code == "PC" ~ 
        "forgery and counterfeiting",
      section >=484 & section <=502.9 & ca_code == "PC" ~ 
        "larceny",
      section >=503 & section <=515 & ca_code == "PC" ~ 
        "embezzlement",
      section >=518 & section <=527 & ca_code == "PC" ~ 
        "extortion",
      section >=528 & section <=539 & ca_code == "PC" ~ 
        "false personation and cheats",
      section >=548 & section <=551 & ca_code == "PC" ~ 
        "crimes against insured property and insurers",
      section >=552 & section <=558.1 & ca_code == "PC" ~ 
        "unlawful interference with property",
      section >=560 & section <=560.6 & ca_code == "PC" ~ 
        "crimes involving bailments",
      section >=565 & section <=566 & ca_code == "PC" ~ 
        "crimes involving branded containers, cabinets, or other dairy equipment",
      section >=570 & section <=574 & ca_code == "PC" ~ 
        "unlawful subleasing of motor vehicles",
      section >=577 & section <=583 & ca_code == "PC" ~ 
        "fraudulent issue of documents to title to merchandise",
      section >=587 & section <=593 & ca_code == "PC" ~ 
        "malicious injuries ot railroad bridges, highways, bridges, and telegraphs",
      section >=626 & section <=626.11 & ca_code == "PC" ~ 
        "schools",
      section >=627 & section <=627.10 & ca_code == "PC" ~ 
        "access to school premises",
      section >=628 & section <=628.5 & ca_code == "PC" ~ 
        "massage therapy",
      section >=629.50 & section <=629.98 & ca_code == "PC" ~ 
        "interception of wire, electronic digital pager, or electronic cellular telephone communications",
      section >=630 & section <=638.55 & ca_code == "PC" ~ 
        "invasion of privacy",
      section >=639 & section <=653.2 & ca_code == "PC" ~ 
        "of other and miscellaneous offenses",
      section >=653.20 & section <=653.28 & ca_code == "PC" ~ 
        "loitering for the purpose of engaging in a prostitution offense",
      section >=653.55 & section <=653.61 & ca_code == "PC" ~ 
        "immigration matters",
      section ==653.75 & ca_code == "PC" ~ 
        "crimes committed while in custody in correctional facilities"))
    
    # Vehicle Code
      # Divisions
  le_code[c("division", "division_head")]<- NA
  le_code <- le_code %>% 
    mutate(division = case_when(
      section >=100 & section <=681 & ca_code == "VC" ~ 1,
      section >=1500 & section <=3097 & ca_code == "VC" ~ 2,
      section >=4000 & section <=9808 & ca_code == "VC" ~ 3,
      section >=9840 & section <=9928 & ca_code == "VC" ~ 3.5,
      section >=9950 & section <=9993 & ca_code == "VC" ~ 3.6,
      section >=10500 & section <=10904 & ca_code == "VC" ~ 4,
      section >=11100 & section <=12217 & ca_code == "VC" ~ 5,
      section >=12500 & section <=15326 & ca_code == "VC" ~ 6,
      section >=15500 & section <=15501 & ca_code == "VC" ~ 6.5,
      section >=15600 & section <=15632 & ca_code == "VC" ~ 6.7,
      section >=16000 & section <=16560 & ca_code == "VC" ~ 7,
      section >=17000 & section <=17714 & ca_code == "VC" ~ 9,
      section >=20000 & section <=20018 & ca_code == "VC" ~ 10,
      section >=21000 & section <=23336 & ca_code == "VC" ~ 11,
      section >=23500 & section <=23675 & ca_code == "VC" ~ 11.5,
      section >=24000 & section <=28160 & ca_code == "VC" ~ 12,
      section >=29000 & section <=31560 & ca_code == "VC" ~ 13,
      section >=31600 & section <=31620 & ca_code == "VC" ~ 14,
      section >=32000 & section <=32053 & ca_code == "VC" ~ 14.1,
      section >=32100 & section <=32109 & ca_code == "VC" ~ 14.3,
      section >=33000 & section <=33002 & ca_code == "VC" ~ 14.5,
      section >=34000 & section <=34100 & ca_code == "VC" ~ 14.7,
      section >=34500 & section <=34520.5 & ca_code == "VC" ~ 14.8,
      section >=34600 & section <=34672 & ca_code == "VC" ~ 14.85,
      section >=34680 & section <=34693 & ca_code == "VC" ~ 14.86,
      section >=34700 & section <=34725 & ca_code == "VC" ~ 14.9,
      section >=35000 & section <=35796 & ca_code == "VC" ~ 15,
      section >=36000 & section <=36800 & ca_code == "VC" ~ 16,
      section >=38000 & section <=38604 & ca_code == "VC" ~ 16.5,
      section >=38750 & section <=38755 & ca_code == "VC" ~ 16.6,
      section >=39000 & section <=39011 & ca_code == "VC" ~ 16.7,
      section >=40000.1 & section <=41610 & ca_code == "VC" ~ 17,
      section >=42000 & section <=42277 & ca_code == "VC" ~ 18,
    ))
  
  le_code <- le_code %>% 
    mutate(division_head = case_when(
      section >=100 & section <=681 & ca_code == "VC" ~ 
        "words and phrases defined",
      section >=1500 & section <=3097 & ca_code == "VC" ~ 
        "administration",
      section >=4000 & section <=9808 & ca_code == "VC" ~ 
        "registration of vehicles and certificates of title",
      section >=9840 & section <=9928 & ca_code == "VC" ~ 
        "registration and transfer of vessels",
      section >=9950 & section <=9993 & ca_code == "VC" ~ 
        "vehicle sales",
      section >=10500 & section <=10904 & ca_code == "VC" ~ 
        "special antitheft laws",
      section >=11100 & section <=12217 & ca_code == "VC" ~
        "occupational licensing and business regulations",
      section >=12500 & section <=15326 & ca_code == "VC" ~ 
        "drivers' licenses",
      section >=15500 & section <=15501 & ca_code == "VC" ~ 
        "motor vehicle transactions with minors",
      section >=15600 & section <=15632 & ca_code == "VC" ~ 
        "unattended child in motor vehicle safety act",
      section >=16000 & section <=16560 & ca_code == "VC" ~ 
        "financial responsibility laws",
      section >=17000 & section <=17714 & ca_code == "VC" ~ 
        "civil liability",
      section >=20000 & section <=20018 & ca_code == "VC" ~ 
        "accident and accident reports",
      section >=21000 & section <=23336 & ca_code == "VC" ~ 
        "rules of the road",
      section >=23500 & section <=23675 & ca_code == "VC" ~ 
        "sentencing for driving while under the influence",
      section >=24000 & section <=28160 & ca_code == "VC" ~ 
        "equipment of vehicles",
      section >=29000 & section <=31560 & ca_code == "VC" ~ 
        "towing and loading equipment",
      section >=31600 & section <=31620 & ca_code == "VC" ~ 
        "transportation of explosives",
      section >=32000 & section <=32053 & ca_code == "VC" ~ 
        "transportation of hazardous material",
      section >=32100 & section <=32109 & ca_code == "VC" ~
        "transportation of inhalation hazards",
      section >=33000 & section <=33002 & ca_code == "VC" ~ 
        "transportation of radioactive materials",
      section >=34000 & section <=34100 & ca_code == "VC" ~ 
        "flammable and combustible liquids",
      section >=34500 & section <=34520.5 & ca_code == "VC" ~ 
        "safety regulations",
      section >=34600 & section <=34672 & ca_code == "VC" ~ 
        "motor carriers of property permit act",
      section >=34680 & section <=34693 & ca_code == "VC" ~ 
        "private carriers of passengers registration act",
      section >=34700 & section <=34725 & ca_code == "VC" ~ 
        "motor vehicle damage control",
      section >=35000 & section <=35796 & ca_code == "VC" ~ 
        "size, weight, and load",
      section >=36000 & section <=36800 & ca_code == "VC" ~ 
        "implements of husbandry",
      section >=38000 & section <=38604 & ca_code == "VC" ~ 
        "off-highway vehicles",
      section >=38750 & section <=38755 & ca_code == "VC" ~ 
        "autonomous vehicles",
      section >=39000 & section <=39011 & ca_code == "VC" ~ 
        "registration and licensing of bicycles",
      section >=40000.1 & section <=41610 & ca_code == "VC" ~ 
        "offenses and prosecution",
      section >=42000 & section <=42277 & ca_code == "VC" ~ 
        "penalties and disposition of fees, fines, and forfeitures",
    ))
  
  # Business and Professions Code
  le_code <- le_code %>% 
    mutate(division = case_when(
      section >= 100 & section <= 472.5 & ca_code == "BP" ~ 1,
      section >= 475 & section <= 499 & ca_code == "BP"  ~ 1.5,
      section >= 500 & section <= 4999.129 & ca_code == "BP"  ~ 2,
      section >= 5000 & section <= 9998.11 & ca_code == "BP"  ~ 3,
      section >= 10000 & section <= 11506 & ca_code == "BP"  ~ 4,
      section >= 12001 & section <= 13800 & ca_code == "BP"  ~ 5,
      section >= 14000 & section <= 14704 & ca_code == "BP"  ~ 6,
      section >= 16000 & section <= 18001 & ca_code == "BP"  ~ 7,
      section >= 18400 & section <= 22949.51 & ca_code == "BP"  ~ 8,
      section >= 22950 & section <= 22964 & ca_code == "BP"  ~ 8.5,
      section >= 22970 & section <= 22991 & ca_code == "BP"  ~ 8.6,
      section >= 23000 & section <= 25762 & ca_code == "BP"  ~ 9,
      section >= 26000 & section <= 26250 & ca_code == "BP"  ~ 10
    ))
  
  le_code <- le_code %>% 
    mutate(division_head = case_when(
      section >= 100 & section <= 472.5 & ca_code == "BP" ~ 
        "department of consumer affairs",
      section >= 475 & section <= 499 & ca_code == "BP"  ~ 
        "denial, suspension, and revocation of licenses",
      section >= 500 & section <= 4999.129 & ca_code == "BP"  ~ 
        "healing arts",
      section >= 5000 & section <= 9998.11 & ca_code == "BP"  ~ 
        "professions and vocations generally",
      section >= 10000 & section <= 11506 & ca_code == "BP"  ~ 
        "real estate",
      section >= 12001 & section <= 13800 & ca_code == "BP"  ~ 
        "weights and measures",
      section >= 14000 & section <= 14704 & ca_code == "BP"  ~ 
        "business rights",
      section >= 16000 & section <= 18001 & ca_code == "BP"  ~ 
        "general business regulations",
      section >= 18400 & section <= 22949.51 & ca_code == "BP"  ~ 
        "special business regulations",
      section >= 22950 & section <= 22964 & ca_code == "BP"  ~ 
        "stop tobacco access to kids enforcement act",
      section >= 22970 & section <= 22991 & ca_code == "BP"  ~ 
        "cigarette and tobacco products licensing act of 2003",
      section >= 23000 & section <= 25762 & ca_code == "BP"  ~ 
        "alcoholic beverages",
      section >= 26000 & section <= 26250 & ca_code == "BP"  ~ 
        "cannabis"
    ))
  
  # Health and Safety Code
    # Divisions
  le_code <- le_code %>% 
    mutate(division = case_when(
  section >= 135 & section <= 1179.102 & ca_code == "HS" ~ 1 ,
  section >= 1180 & section <= 1180.6 & ca_code == "HS" ~1.5 ,
  section >= 1200 & section <= 1797.8 & ca_code == "HS" ~2 ,
  section >= 1797 & section <= 1799.207 & ca_code == "HS" ~2.5 ,
  section >= 2000 & section <= 2910 & ca_code == "HS" ~3 ,
  section >= 4600 & section <= 6127 & ca_code == "HS" ~5 ,
  section >= 6400 & section <= 6982 & ca_code == "HS" ~6 ,
  section >= 7000 & section <= 8030 & ca_code == "HS" ~7 ,
  section >= 8100 & section <= 9703 & ca_code == "HS" ~8 ,
  section >= 11000 & section <= 11651 & ca_code == "HS" ~10 ,
  section >= 11700 & section <= 11717 & ca_code == "HS" ~10.2 ,
  section >= 11750 & section <= 11975 & ca_code == "HS" ~10.5 ,
  section >= 11998 & section <= 11998.4 & ca_code == "HS" ~10.6 ,
  section >= 11999 & section <= 11999.3 & ca_code == "HS" ~10.7 ,
  section >= 11999.4 & section <= 11999.13 & ca_code == "HS" ~10.8 ,
  section >= 11999.2 & section <= 11999.25 & ca_code == "HS" ~10.9 ,
  section >= 12000 & section <= 12761 & ca_code == "HS" ~11 ,
  section >= 13000 & section <= 14959 & ca_code == "HS" ~12 ,
  section >= 16000 & section <= 16604 & ca_code == "HS" ~12.5 ,
  section >= 17000 & section <= 19997 & ca_code == "HS" ~13 ,
  section >= 20000 & section <= 20115 & ca_code == "HS" ~14 ,
  section >= 24000 & section <= 26250 & ca_code == "HS" ~20 ,
  section >= 32000 & section <= 32499.4 & ca_code == "HS" ~23 ,
  section >= 32500 & section <= 32508 & ca_code == "HS" ~23.5 ,
  section >= 33000 & section <= 37964 & ca_code == "HS" ~24 ,
  section >= 38000 & section <= 38041 & ca_code == "HS" ~25 ,
  section >= 38050 & section <= 38065 & ca_code == "HS" ~25.1 ,
  section >= 38070 & section <= 38081.1 & ca_code == "HS" ~25.2 ,
  section >= 38500 & section <= 38599 & ca_code == "HS" ~25.5 ,
  section >= 39000 & section <= 44474 & ca_code == "HS" ~26 ,
  section >= 44500 & section <= 44563 & ca_code == "HS" ~27 ,
  section >= 46000 & section <= 46080 & ca_code == "HS" ~28 ,
  section >= 50000 & section <= 54034 & ca_code == "HS" ~31 ,
  section >= 55000 & section <= 55117 & ca_code == "HS" ~32 ,
  section >= 57000 & section <= 57020 & ca_code == "HS" ~37 ,
  section >= 57050 & section <= 57053.9 & ca_code == "HS" ~37.5 ,
  section >= 100100 & section <= 101997 & ca_code == "HS" ~101 ,
  section >= 102100 & section <= 103925 & ca_code == "HS" ~102 ,
  section >= 104100 & section <= 106036 & ca_code == "HS" ~103 ,
  section >= 106500 & section <= 119406 & ca_code == "HS" ~104 ,
  section >= 120100 & section <= 122477 & ca_code == "HS" ~105 ,
  section >= 123100 & section <= 125850 & ca_code == "HS" ~106 ,
  section >= 127000 & section <= 130070 & ca_code == "HS" ~107 ,
  section >= 130100 & section <= 130158 & ca_code == "HS" ~108 ,
  section == 130200  & ca_code == "HS" ~109 ,
  section >= 130250 & section <= 130255 & ca_code == "HS" ~109.5 ,
  section >= 130275 & section <= 130282 & ca_code == "HS" ~109.6 ,
  section >= 130300 & section <= 130315 & ca_code == "HS" ~110 ,
  section >= 130400 & section <= 130410 & ca_code == "HS" ~111 ,
  section >= 130500 & section <= 130544 & ca_code == "HS" ~112 ,
  section >= 131000 & section <= 131231 & ca_code == "HS" ~112 ,
  section >= 131500 & section <= 131550 & ca_code == "HS" ~113 ,
  section >= 132000 & section <= 132008 & ca_code == "HS" ~114 ,
  section >= 134000 & section <= 134002 & ca_code == "HS" ~114.01 ,
  section >= 136000 & section <= 136030 & ca_code == "HS" ~115 ,
  section >= 150200 & section <= 150208 & ca_code == "HS" ~116 ,
  section >= 151000 & section <= 151003 & ca_code == "HS" ~120))
  
  le_code <- le_code %>% 
    mutate(division_head = case_when(
      section >= 135 & section <= 1179.102 & ca_code == "HS" ~ 
        "administration of public health" ,
      section >= 1180 & section <= 1180.6 & ca_code == "HS" ~ 
        "use of seclusion and behavioral restraints in facilities" ,
      section >= 1200 & section <= 1797.8 & ca_code == "HS" ~ 
        "licensing provisions" ,
      section >= 1797 & section <= 1799.207 & ca_code == "HS" ~ 
        "emergency medical services" ,
      section >= 2000 & section <= 2910 & ca_code == "HS" ~ 
        "pest abatement" ,
      section >= 4600 & section <= 6127 & ca_code == "HS" ~ 
        "sanitation" ,
      section >= 6400 & section <= 6982 & ca_code == "HS" ~ 
        "sanitary districts" ,
      section >= 7000 & section <= 8030 & ca_code == "HS" ~ 
        "dead bodies" ,
      section >= 8100 & section <= 9703 & ca_code == "HS" ~ 
        "cemeteries" ,
      section >= 11000 & section <= 11651 & ca_code == "HS" ~ 
        "uniform controlled substances act" ,
      section >= 11700 & section <= 11717 & ca_code == "HS" ~ 
        "drug dealer liability act" ,
      section >= 11750 & section <= 11975 & ca_code == "HS" ~ 
        "alcohol and drug programs" ,
      section >= 11998 & section <= 11998.4 & ca_code == "HS" ~ 
        "drug and alcohol abuse master plans" ,
      section >= 11999 & section <= 11999.3 & ca_code == "HS" ~ 
        "illegal use of drugs and alcohol" ,
      section >= 11999.4 & section <= 11999.13 & ca_code == "HS" ~ 
        "substance abuse treatment funding" ,
      section >= 11999.2 & section <= 11999.25 & ca_code == "HS" ~ 
        "substance abuse testing and treatment accountability program" ,
      section >= 12000 & section <= 12761 & ca_code == "HS" ~ 
        "explosives" ,
      section >= 13000 & section <= 14959 & ca_code == "HS" ~ 
        "fires and fire protection" ,
      section >= 16000 & section <= 16604 & ca_code == "HS" ~ 
        "buildings used by the public" ,
      section >= 17000 & section <= 19997 & ca_code == "HS" ~ 
        "housing" ,
      section >= 20000 & section <= 20115 & ca_code == "HS" ~ 
        "police protection" ,
      section >= 24000 & section <= 26250 & ca_code == "HS" ~ 
        "miscellaneous health and safety provisions" ,
      section >= 32000 & section <= 32499.4 & ca_code == "HS" ~ 
        "hospital districts" ,
      section >= 32500 & section <= 32508 & ca_code == "HS" ~ 
        "endowment hospitals" ,
      section >= 33000 & section <= 37964 & ca_code == "HS" ~ 
        "community development and housing" ,
      section >= 38000 & section <= 38041 & ca_code == "HS" ~ 
        "health and welfare agency—direct service contracts reform act" ,
      section >= 38050 & section <= 38065 & ca_code == "HS" ~ 
        "health and welfare agency—administrative appeals process for nonprofit human services agencies" ,
      section >= 38070 & section <= 38081.1 & ca_code == "HS" ~ 
        "state department of health services cooperative agreement act" ,
      section >= 38500 & section <= 38599 & ca_code == "HS" ~ 
        "california global warming solutions act of 2006" ,
      section >= 39000 & section <= 44474 & ca_code == "HS" ~ 
        "air resources" ,
      section >= 44500 & section <= 44563 & ca_code == "HS" ~ 
        "california pollution control financing authority act" ,
      section >= 46000 & section <= 46080 & ca_code == "HS" ~ 
        "noise control act" ,
      section >= 50000 & section <= 54034 & ca_code == "HS" ~ 
        "housing and home finance" ,
      section >= 55000 & section <= 55117 & ca_code == "HS" ~ 
        "seismic safety building rehabilitation loans" ,
      section >= 57000 & section <= 57020 & ca_code == "HS" ~ 
        "regulation of environmental protection" ,
      section >= 57050 & section <= 57053.9 & ca_code == "HS" ~ 
        "repair or maintenance projects" ,
      section >= 100100 & section <= 101997 & ca_code == "HS" ~ 
        "administration of public health" ,
      section >= 102100 & section <= 103925 & ca_code == "HS" ~ 
        "vital records and health statistics" ,
      section >= 104100 & section <= 106036 & ca_code == "HS" ~ 
        "disease prevention and health promotion" ,
      section >= 106500 & section <= 119406 & ca_code == "HS" ~ 
        "environmental health" ,
      section >= 120100 & section <= 122477 & ca_code == "HS" ~ 
        "communicable disease prevention and control" ,
      section >= 123100 & section <= 125850 & ca_code == "HS" ~ 
        "personal health care (including maternal, child, and adolescent)" ,
      section >= 127000 & section <= 130070 & ca_code == "HS" ~ 
        "statewide health planning and development" ,
      section >= 130100 & section <= 130158 & ca_code == "HS" ~ 
        "california children and families program" ,
      section == 130200 & ca_code == "HS" ~ 
        "office of health information integrity" ,
      section >= 130250 & section <= 130255 & ca_code == "HS" ~ 
        "california health information technology and exchange act" ,
      section >= 130275 & section <= 130282 & ca_code == "HS" ~ 
        "health information exchange privacy and security demonstration projects" ,
      section >= 130300 & section <= 130315 & ca_code == "HS" ~
        "the health insurance portability and accountability implementation act of 2001" ,
      section >= 130400 & section <= 130410 & ca_code == "HS" ~ 
        "golden bear state pharmacy assistance program" ,
      section >= 130500 & section <= 130544 & ca_code == "HS" ~ 
        "california discount prescription drug program" ,
      section >= 131000 & section <= 131231 & ca_code == "HS" ~ 
        "public health" ,
      section >= 131500 & section <= 131550 & ca_code == "HS" ~ 
        "the adult health coverage expansion program" ,
      section >= 132000 & section <= 132008 & ca_code == "HS" ~ 
        "prescription drug discount prohibition" ,
      section >= 134000 & section <= 134002 & ca_code == "HS" ~ 
        "preserving access to affordable drugs" ,
      section >= 136000 & section <= 136030 & ca_code == "HS" ~ 
        "office of patient advocate" ,
      section >= 150200 & section <= 150208 & ca_code == "HS" ~ 
        "surplus medication collection and distribution" ,
      section >= 151000 & section <= 151003 & ca_code == "HS" ~ 
        "sexual health education accountability act"))
  
  # Add felony/misdemeanor, offense type categories. Note 'severity' file is 
  # manually constructed from aentence durations.
  le_code <- left_join(le_code, severity, by = "sentence")
  le_code$severity <- as.factor(le_code$severity)
  
    # Correct column classes
  le_code <- le_code %>% 
    mutate_at(vars(ca_code, section_full, section, subsection1, subsection2,
             subsection3, subsection4, subsection5, description, sentence, 
             sec_code, part, title, chapter, division, severity), as.factor)
  
  # Join arrest log & CA code, export in various aggregates-----
  davis_log <- left_join(davis_log, le_code, by = "sec_code")
    # Verify column classes
  davis_log <- davis_log %>% 
    mutate_at(vars(incident_id, sex, race, charge_num, sec_code, ca_code,
                   section_full, section, section, subsection1, subsection2,
                   subsection3, subsection4, subsection5, description,
                   part, part_head, division, division_head,
                   title, title_head, chapter,
                   chapter_head), as.factor)
  davis_log$age <- as.numeric(davis_log$age)
  davis_log$date <- mdy(davis_log$date)
    # Add new rough category for Davis-included charges
  supp_codes <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1jExCh-Kv4o3vFSrZaWBJ4OchHXhehPFryMuufxiDqMI/edit#gid=1099502257"
  )
      # Retain only section and category columns
  supp_codes <- supp_codes[,4:5]
      # Merge with Davis arrest log
  davis_log <- left_join(davis_log, supp_codes, by = "sec_code")
      # Export full
  write.csv(davis_log, "./data/davis_log.csv")
      
    # Monthly aggregates
  davis_log_month <- davis_log %>% 
    count(date, race, severity, category)
    
  davis_log_month <- davis_log_month %>% 
    group_by(month = floor_date(date, "month")) %>% 
    count(race, severity, category)
  
  write.csv(davis_log_month, "./data/davis_log_month.csv")
  

  
  
  
  
