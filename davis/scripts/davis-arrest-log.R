  library(tabulizer)
  library(rJava)
  library(tidyverse)
  library(googlesheets4)
  library(data.table)
  library(lubridate)

  
  # Change working directory to your local R project folder
  setwd(
    "/Users/bapu/Projects/watershed/action/public-safety/yolo/analysis/davis/")
  
  # SCRAPE/DOWNLOAD DATA TABLES----
    # Arrest log 6/15 to 6/20, from Davis PD via a Public Records Request
  davis_log_raw <- extract_tables(
    file = "./data/raw/davis-arrest-log-raw.pdf",
    method = "decide",
    output = "data.frame", 
    header = F)
    # California law enforcement code tables, from CA Attorney General
  le_code_raw <- read.csv(url(
    "https://oag.ca.gov/sites/all/files/agweb/law-enforcement/code-tables/macrcode.csv?070920201056"),
    header = FALSE)
    # Crime sentencing (felony/misdemeanor) severity 
    # File is manually constructed from sentence field in le_code_raw
    # You will be asked to confirm email for Google sheets access
  severity <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1arqMRPvlznsOTjmrRYr2EEucXNydZrLpqz7L4eD5iyY/edit?usp=sharing")
    # Arrest categories for charges included in Davis arrest log only 
    # Manually constructed from https://oag.ca.gov/sites/all/files/agweb/pdfs/cjsc/prof10/codes.pdf
  supp_codes <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1jExCh-Kv4o3vFSrZaWBJ4OchHXhehPFryMuufxiDqMI/edit#gid=1099502257")
      # Retain only section code and category columns
  supp_codes <- supp_codes[,4:5]
  
  # CLEAN ARREST LOG----
    # Rename columns
  colnames <- c("date", "charge1", "charge2", "charge3", "sex", "race", "age")
  davis_log <- lapply(davis_log_raw, setNames, colnames)
    # Remove redundant and blank rows
  davis_log[[1]] <- davis_log[[1]][-1,]
  davis_log[[67]] <- davis_log[[67]][-15,]
  davis_log <- lapply(davis_log, transform, age = as.numeric(age))
    # Bind list into one data frame
  davis_log <- bind_rows(davis_log)
    # Renumber rows & add individual ID column with row numbers
  rownames(davis_log) <- 1:nrow(davis_log)
  davis_log <- cbind("indiv_id" = rownames(davis_log), data.frame(davis_log))
    # Fix scraping errors in race field
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
  
  # RESHAPE/PREP ARREST LOG----
    # Melt into single charge column
  davis_log_long <- davis_log %>% 
    pivot_longer(-c("indiv_id", "date", "sex", "race", "age"),
                 names_to = "charge_num")
    # Rename sec_code column
  colnames(davis_log_long)[7] <- "sec_code"
    # Fill in rows with missing race, remove rows with blank charges
  davis_log_long$race <- sub("^$", "Missing", 
                             davis_log_long$race)
  davis_log_long <- davis_log_long %>% na_if("") %>% na.omit
    # Standardize charge codes (capitalize, delete spaces)
  davis_log_long$sec_code <- toupper(davis_log_long$sec_code)
  davis_log_long$sec_code <- gsub(" ", "", davis_log_long$sec_code, 
                                  fixed = TRUE)
    # Correct typos in charge codes
  davis_log_long$sec_code[davis_log_long$sec_code == "508"] <- "508PC"
  davis_log_long$sec_code[davis_log_long$sec_code == "11364"] <- "11364(A)HS"
    # Correct column classes
  davis_log_long <- davis_log_long %>% 
    mutate_at(vars(indiv_id, sex, race, charge_num, sec_code), as.factor)
  davis_log_long$age <- as.numeric(davis_log_long$age)
  davis_log_long$date <- mdy(davis_log_long$date)
    
  # CLEAN UP LAW ENFORCEMENT CODE TABLES----
    # Retain important fields (I don't know what the other fields are!)
  le_code <- le_code_raw[c("V6", "V5", "V7", "V8")]
  colnames(le_code) <- c("ca_code", "section_full", "description", "sentence")
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
                                 description = 
                                   "VISIT WHERE CONTROLLED SUBSTANCE USED")
  le_code <- le_code %>% add_row(section_full = "11375(B)", ca_code ="HS", 
                                 description = 
                                   "POSS FOR SALE DESIGNATED CONTROLLED SUBSTANCE")
  le_code <- le_code %>% add_row(section_full = "11379", ca_code ="HS", 
                                 description = "TRANSPORT CONTROLLED SUBSTANCE")
  le_code <- le_code %>% add_row(section_full = "12020(A)", ca_code ="PC", 
                                 description = "WEAPONS POSSESSION")
  le_code <- le_code %>% add_row(section_full = "12021(A)", ca_code ="PC", 
                                 description = 
                                   "CONVICTED PERSON WEAPONS POSSESSION")
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
                                 description = 
                                   "UNLAWFUL POSSESSION/USE TEAR GAS OR TEAR GAS WEAPON")
  le_code <- le_code %>% add_row(section_full = "23136(A)", ca_code ="VC", 
                                 description = "UNDERAGE DRUNK DRIVING 0.01")
  le_code <- le_code %>% add_row(section_full = "23140(A)", ca_code ="VC", 
                                 description = "UNDERAGE DRUNK DRIVING 0.05")
  le_code <- le_code %>% add_row(section_full = "23153(E)", ca_code ="VC", 
                                 description = 
                                   "DUI ALCOHOL:CAUSING BODILY INJURY")
  le_code <- le_code %>% add_row(section_full = "23154(A)", ca_code ="VC", 
                                 description = "PROBATION DRUNK DRIVING 0.01")
  le_code <- le_code %>% add_row(section_full = "23222(B)", ca_code ="VC", 
                                 description = "DRIVING OPEN MARIJUANA")
  le_code <- le_code %>% add_row(section_full = "23223(A)", ca_code ="VC", 
                                 description = "DRIVING OPEN BOTTLE")
  le_code <- le_code %>% add_row(section_full = "23223(B)", ca_code ="VC", 
                                 description = "DRIVING OPEN BOTTLE")
  le_code <- le_code %>% add_row(section_full = "23226(B)", ca_code ="VC", 
                                 description = 
                                   "DRIVING OPEN BOTTLE COMPARTMENT")
  le_code <- le_code %>% add_row(section_full = "237", ca_code ="PC", 
                                 description = "FALSE IMPRISONMENT")
  le_code <- le_code %>% add_row(section_full = "246.3", ca_code ="PC", 
                                 description = 
                                   "WILLFUL DISCHARGE OF FIREARM IN A GROSSLY NEGLIGENT MANNER")
  le_code <- le_code %>% add_row(section_full = "273A(1)", ca_code ="PC",
                                 description = 
                                   "WILLFUL CRUELTY TO CHILD:POSSIBLE INJURY/DEATH")
  le_code <- le_code %>% add_row(section_full = "273A(A)(1)", ca_code ="PC", 
                                 description = 
                                   "WILLFUL CRUELTY TO CHILD:POSSIBLE INJURY/DEATH")
  le_code <- le_code %>% add_row(section_full = "273A(B)", ca_code = "PC",
                                 description = 
                                   "WILLFUL CRUELTY TO CHILD:POSSIBLE INJURY/DEATH")
  le_code <- le_code %>% add_row(section_full = "2800.1", ca_code ="VC", 
                                 description = "EVADING PEACE OFFICER")
  le_code <- le_code %>% add_row(section_full = "2800.2", ca_code ="VC", 
                                 description = 
                                   "EVADE PEACE OFFICER WITH WANTON DISREGARD FOR SAFETY")
  le_code <- le_code %>% add_row(section_full = "288.4", ca_code ="PC", 
                                 description = "ARRANGE A MEETING WITH MINOR")
  le_code <- le_code %>% add_row(section_full = "288A(G)", ca_code ="PC", 
                                 description = "ARRANGE A MEETING WITH MINOR")
  le_code <- le_code %>% add_row(section_full = "3455(A)", ca_code ="PC", 
                                 description = 
                                   "POSTRELEASE COMMUNITY SUPERVISION VIOLATION")
  le_code <- le_code %>% add_row(section_full = "368(A)", ca_code ="PC", 
                                 description = 
                                   "CAUSE HARM/DEATH OF ELDER /DEPENDENT ADULT")
  le_code <- le_code %>% add_row(section_full = "368(B)", ca_code ="PC", 
                                 description = 
                                   "CAUSE HARM/DEATH OF ELDER /DEPENDENT ADULT")
  le_code <- le_code %>% add_row(section_full = "4000(A)", ca_code ="VC", 
                                 description = "UNREGISTERED VEHICLE")
  le_code <- le_code %>% add_row(section_full = "40302(B)", ca_code ="VC", 
                                 description = "FAILURE TO PRESENT ID")
  le_code <- le_code %>% add_row(section_full = "404.6", ca_code ="PC", 
                                 description = 
                                   "RIOT IN PRISON/JAIL/ETC RESULTS IN SERIOUS BODILY INJURY")
  le_code <- le_code %>% add_row(section_full = "4140BP", ca_code = "PC",
                                 description = "POSSESSION HYPODERMIC NEEDLE")
  le_code <- le_code %>% add_row(section_full = "417.2(A)", ca_code ="PC", 
                                 description = 
                                   "THREATEN WITH LASER SCOPE WITH INTENT TO CAUSE FEAR")
  le_code <- le_code %>% add_row(section_full = "422", ca_code ="PC", 
                                 description = "THREATEN CRIME WITH INTENT TO TERRORIZE")
  le_code <- le_code %>% add_row(section_full = "4462(B)", ca_code ="VC", 
                                 description = 
                                   "UNLAWFUL POSSESSION OF VEHICLE REGISTRATION/ETC")
  le_code <- le_code %>% add_row(section_full = "4573(A)", ca_code ="PC", 
                                 description = 
                                   "BRING CONTROLLED SUBSTANCE/ETC INTO PRISON/JAIL/ETC")
  le_code <- le_code %>% add_row(section_full = "459.5(A)", ca_code ="PC", 
                                 description = "SHOPLIFTING")
  le_code <- le_code %>% add_row(section_full = "475", ca_code ="PC", 
                                 description = "POSSESS/ETC BAD/ETC CHECK/ETC")
  le_code <- le_code %>% add_row(section_full = "484E(2)", ca_code ="PC", 
                                 description = "SELL/ETC LOST/ETC ACCESS CARD")
  le_code <- le_code %>% add_row(section_full = "484F(1)", ca_code ="PC", 
                                 description = "FORGE ACCESS CARD TO DEFRAUD")
  le_code <- le_code %>% add_row(section_full = "530.5", ca_code ="PC", 
                                 description = 
                                   "OBTAIN CREDIT/ETC:USE OTHERS ID")
  le_code <- le_code %>% add_row(section_full = "594(A)", ca_code ="PC", 
                                 description = "VANDALISM:DEFACE PROPERTY")
  le_code <- le_code %>% add_row(section_full = "594(B)(3)", ca_code ="PC", 
                                 description = "VANDALISM ($400 OR MORE)")
  le_code <- le_code %>% add_row(section_full = "626.10(A)", ca_code ="PC", 
                                 description = "POSSESS WEAPON/ETC AT SCHOOL")
  le_code <- le_code %>% add_row(section_full = "647(J)", ca_code ="PC", 
                                 description = 
                                   "DISORDERLY CONDUCT:INVADE PRIVACY W/CAMERA/ETC IN BATHRM/ETC")
  le_code <- le_code %>% add_row(section_full = "664/10851(A)", ca_code ="VC", 
                                 description = "VEHICLE WITHOUT CONSENT")
  le_code <- le_code %>% add_row(section_full = "664/207", ca_code ="PC", 
                                 description = "KIDNAPPING")
  le_code <- le_code %>% add_row(section_full = "664/211", ca_code ="PC",
                                 description = "ROBBERY")
  le_code <- le_code %>% add_row(section_full = "664/459", ca_code ="PC", 
                                 description = "BURGLARY")
  le_code <- le_code %>% add_row(section_full = "664/487", ca_code ="PC", 
                                 description = 
                                   "GRAND THEFT:MONEY/LABOR/PROPERTY")
  le_code <- le_code %>% add_row(section_full = "664/488", ca_code ="PC", 
                                 description = "PETTY THEFT")
  le_code <- le_code %>% add_row(section_full = "664/490.5(A)", ca_code ="PC", 
                                 description = 
                                   "PETTY THEFT:RETAIL MERCHANDISE/ETC")
  le_code <- le_code %>% add_row(section_full = "666", ca_code ="PC", 
                                description = 
                                  "PETTY THEFT:PRIOR SPECIAL CONVICTION SEX REG REQUIRED")
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
    # Delete lines without specific CA code reference
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
                                  sep = " ", remove = FALSE)
    # Change class of section to allow for numeric merge with CA Codes
  le_code$section <- as.numeric(le_code$section)
    # Add CA Codes headings
  le_code[, c("part", "part_head", "title", "title_head", 
              "chapter", "chapter_head")] <- NA
  
  # ADD CALIFORNIA CODES INFORMATION----
    # Penal Code Parts
        # Part numbers 
  le_code <- le_code %>% 
    mutate(part = case_when(section < 681 & ca_code == "PC" ~ 1,
                           section >=681 & section <=1620 & 
                             ca_code == "PC" ~ 2 ,
                           section >=2000 & section <=10007 & 
                             ca_code == "PC" ~ 3,
                           section >=11006 & section <=14315 & 
                             ca_code == "PC" ~ 4,
                           section >=15001 & section <=15003 & 
                             ca_code == "PC" ~ 5,
                           section >=16000 & section <=34370 & 
                             ca_code == "PC" ~ 6))
        # Part headings
  le_code <- le_code %>% 
    mutate(part_head = case_when(section < 681 & ca_code == "PC" 
                                 ~ "Crimes and punishments",
                            section >=681 & section <=1620 & ca_code == "PC" ~ 
                              "Criminal procedure",
                            section >=2000 & section <=10007 & ca_code == "PC" ~ 
                              "Imprisonment and the death penalty",
                            section >=11006 & section <=14315 & 
                              ca_code == "PC" ~ 
                              "Prevention of crimes and apprehension of criminals",
                            section >=15001 & section <=15003 & 
                              ca_code == "PC" ~ 
                              "Peace officers' memorial",
                            section >=16000 & section <=34370 & 
                              ca_code == "PC" ~ 
                              "Control of deadly weapons"))
      # Penal Code Titles
        # Title numbers
  le_code <- le_code %>%
    mutate(title = 
             case_when(section >=25 & section <=29.8 & ca_code == "PC" ~ 1,
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
      # Title headings
  le_code <- le_code %>%
    mutate(title_head = case_when(
      section >=25 & section <=29.8 & ca_code == "PC" ~ 
        "Persons liable to punishment for crime",
      section >=30 & section <=33 & ca_code == "PC" ~ 
        "Parties to crime" ,
      section >=37  & section <=38 & ca_code == "PC" ~ 
        "Offenses against the sovereignty of the state",
      section >=67  & section <=77 & ca_code == "PC" ~ 
        "Crimes by and against the executive power of the state",
      section >=85  & section <=88 & ca_code == "PC" ~ 
        "Crimes against the legislative power",
      section >=92  & section <=186.36 & ca_code == "PC" ~ 
        "Crimes against public justice",
      section >=187  & section <=248 & ca_code == "PC" ~ 
        "Crimes against the person",
      section >=261  & section <=368.7 & ca_code == "PC" ~ 
        "Crimes against the person involving sexual assault, and crimes against public decency and good morals",
      section >=369  & section <=402 & ca_code == "PC" ~ 
        "Crimes against the public health and safety",
      section >=403  & section <=420.1 & ca_code == "PC" ~ 
        "Crimes against the public peace",
      section >=422  & section <=422.4  & ca_code == "PC" ~ 
        "Criminal threats",
      section >=422.55  & section <=422.57 & ca_code == "PC" ~ 
        "Civil rights",
      section >=423 & section <=423.6 & ca_code == "PC" ~ 
        "California freedom of access to clinic and church entrances act",
      section >=424  & section <=440 & ca_code == "PC" ~ 
        "Crimes against the revenue and property of this state",
      section >=450  & section <=593 & ca_code == "PC" ~ 
        "Crimes against property",
      section >=594  & section <=625 & ca_code == "PC" ~ 
        "Malicious mischief",
      section >=626  & section <=653.75 & ca_code == "PC" ~ 
        "Miscellaneous crimes",
      section >=654  & section <=678 & ca_code == "PC" ~ 
        "General provisions",
      section >=679  & section <=680.4 & ca_code == "PC" ~ 
        "Rights of victims and witnesses of crime"))
    # Penal Code Chapters
      # Chapter numbers
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
        # Chapter headings
  le_code <- le_code %>% 
    mutate(chapter_head = case_when(
      section >=92 & section <=100 & ca_code == "PC" ~ 
        "Bribery and corruption",
      section ==102 & ca_code == "PC" ~ 
        "Rescues",
      section >=107 & section <=110 & ca_code == "PC" ~ 
        "Escapes and aiding therein",
      section >=112 & section <=117 & ca_code == "PC" ~ 
        "Forging, stealing, mutilating, and falsifying judicial and public records and documents",
      section >=118 & section <=131 & ca_code == "PC" ~ 
        "Perjury and subornation of perjury",
      section >=132 & section <=141 & ca_code == "PC" ~ 
        "Falsifying evidence, and bribing, influencing, intimidating or threatening witnesses",
      section >=142 & section <=181 & ca_code == "PC" ~ 
        "Other offenses against public justice",
      section >=182 & section <=185 & ca_code == "PC" ~ 
        "Conspiracy",
      section >=186 & section <=186.8 & ca_code == "PC" ~ 
        "Criminal profiteering",
      section >=186.9 & section <=186.10 & ca_code == "PC" ~ 
        "Money laundering",
      section >=186.11 & section <=186.12 & ca_code == "PC" ~ 
        "Fraud and embezzlement: victim restitution",
      section >=186.20 & section <=186.36 & ca_code == "PC" ~ 
        "Street terrorism enforcement and prevention act",
      section >=187 & section <=199 & ca_code == "PC" ~ 
        "Homicide",
      section >=203 & section <=206.1 & ca_code == "PC" ~ 
        "Mayhem",
      section >=207 & section <=210 & ca_code == "PC" ~ 
        "Kidnapping",
      section ==210.5 & ca_code == "PC" ~ 
        "Hostages",
      section >=211 & section <=215 & ca_code == "PC" ~ 
        "Robbery",
      section >=217.1 & section <=219.3 & ca_code == "PC" ~ 
        "Attempts to kill",
      section >=220 & section <=222 & ca_code == "PC" ~ 
        "Assaults with intent to commit felony, other than assaults with intent to commit murder",
      section >=236 & section <=237 & ca_code == "PC" ~ 
        "False imprisonment and human trafficking",
      section >=240 & section <=248 & ca_code == "PC" ~ 
        "Assault and battery",
      section >=261 & section <=269 & ca_code == "PC" ~ 
        "Rape, abduction, carnal abuse of children, and seduction",
      section >=270 & section <273.75 & ca_code == "PC" ~ 
        "Abandonment of children",
      section >=273.8 & section <=273.88 & ca_code == "PC" ~ 
        "Spousal abusers",
      section >=277 & section <=280 & ca_code == "PC" ~ 
        "Child abduction",
      section >=281 & section <=289.6 & ca_code == "PC" ~ 
        "Bigamy, incest, and the crime against nature",
      section >=290 & section <=294 & ca_code == "PC" ~ 
        "Sex offenders",
      section >=295 & section <=300.4 & ca_code == "PC" ~ 
        "DNA and forensic identification data base and data bank act of 1998",
      section >=302 & section <=310.5 & ca_code == "PC" ~ 
        "Crimes against religion and conscience, and other offenses against good morals",
      section >=311 & section <=312.7 & ca_code == "PC" ~ 
        "Obscene matter",
      section >=313 & section <=313.5 & ca_code == "PC" ~ 
        "Harmful matter",
      section >=314 & section <=318.6 & ca_code == "PC" ~ 
        "Indecent exposure, obscene exhibitions, and bawdy and other disorderly houses",
      section >=319 & section <=329 & ca_code == "PC" ~ 
        "Lotteries",
      section >=330 & section <=337 & ca_code == "PC" ~ 
        "Gaming",
      section >=337.1 & section <=337.9 & ca_code == "PC" ~ 
        "Horse racing",
      section ==343 & ca_code == "PC" ~ 
        "Pawnbrokers",
      section >=346 & section <=367 & ca_code == "PC" ~ 
        "Other injuries to persons",
      section >=368 & section <=368.7 & ca_code == "PC" ~
        "Crimes against elders, dependent adults, and persons with disabilities",
      section >=422.55 & section <=422.57 & ca_code == "PC" ~ 
        "Definitions",
      section >=422.6 & section <=422.865 & ca_code == "PC" ~ 
        "Crimes and penalties",
      section ==422.87 & ca_code == "PC" ~ 
        "Law enforcement agency policies",
      section >=422.88 & section <=422.93 & ca_code == "PC" ~ 
        "General provisions",
      section >=450 & section <=457.1 & ca_code == "PC" ~ 
        "Arson",
      section >=458 & section <=464 & ca_code == "PC" ~ 
        "Burglary",
      section >=466 & section <=469 & ca_code == "PC" ~ 
        "Burglarious and larcenous instruments and deadly weapons",
      section >=470 & section <=483.5 & ca_code == "PC" ~ 
        "Forgery and counterfeiting",
      section >=484 & section <=502.9 & ca_code == "PC" ~ 
        "Larceny",
      section >=503 & section <=515 & ca_code == "PC" ~ 
        "Embezzlement",
      section >=518 & section <=527 & ca_code == "PC" ~ 
        "Extortion",
      section >=528 & section <=539 & ca_code == "PC" ~ 
        "False personation and cheats",
      section >=548 & section <=551 & ca_code == "PC" ~ 
        "Crimes against insured property and insurers",
      section >=552 & section <=558.1 & ca_code == "PC" ~ 
        "Unlawful interference with property",
      section >=560 & section <=560.6 & ca_code == "PC" ~ 
        "Crimes involving bailments",
      section >=565 & section <=566 & ca_code == "PC" ~ 
        "Crimes involving branded containers, cabinets, or other dairy equipment",
      section >=570 & section <=574 & ca_code == "PC" ~ 
        "Unlawful subleasing of motor vehicles",
      section >=577 & section <=583 & ca_code == "PC" ~ 
        "Fraudulent issue of documents to title to merchandise",
      section >=587 & section <=593 & ca_code == "PC" ~ 
        "Malicious injuries ot railroad bridges, highways, bridges, and telegraphs",
      section >=626 & section <=626.11 & ca_code == "PC" ~ 
        "schools",
      section >=627 & section <=627.10 & ca_code == "PC" ~ 
        "Access to school premises",
      section >=628 & section <=628.5 & ca_code == "PC" ~ 
        "Massage therapy",
      section >=629.50 & section <=629.98 & ca_code == "PC" ~ 
        "Interception of wire, electronic digital pager, or electronic cellular telephone communications",
      section >=630 & section <=638.55 & ca_code == "PC" ~ 
        "Invasion of privacy",
      section >=639 & section <=653.2 & ca_code == "PC" ~ 
        "Of other and miscellaneous offenses",
      section >=653.20 & section <=653.28 & ca_code == "PC" ~ 
        "Loitering for the purpose of engaging in a prostitution offense",
      section >=653.55 & section <=653.61 & ca_code == "PC" ~ 
        "Immigration matters",
      section ==653.75 & ca_code == "PC" ~ 
        "Crimes committed while in custody in correctional facilities"))
    # Vehicle Code
      # Divisions
        # Division numbers
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
        # Division headings
  le_code <- le_code %>% 
    mutate(division_head = case_when(
      section >=100 & section <=681 & ca_code == "VC" ~ 
        "Words and phrases defined",
      section >=1500 & section <=3097 & ca_code == "VC" ~ 
        "Administration",
      section >=4000 & section <=9808 & ca_code == "VC" ~ 
        "Registration of vehicles and certificates of title",
      section >=9840 & section <=9928 & ca_code == "VC" ~ 
        "Registration and transfer of vessels",
      section >=9950 & section <=9993 & ca_code == "VC" ~ 
        "Vehicle sales",
      section >=10500 & section <=10904 & ca_code == "VC" ~ 
        "Special antitheft laws",
      section >=11100 & section <=12217 & ca_code == "VC" ~
        "Occupational licensing and business regulations",
      section >=12500 & section <=15326 & ca_code == "VC" ~ 
        "Drivers' licenses",
      section >=15500 & section <=15501 & ca_code == "VC" ~ 
        "Motor vehicle transactions with minors",
      section >=15600 & section <=15632 & ca_code == "VC" ~ 
        "Unattended child in motor vehicle safety act",
      section >=16000 & section <=16560 & ca_code == "VC" ~ 
        "Financial responsibility laws",
      section >=17000 & section <=17714 & ca_code == "VC" ~ 
        "Civil liability",
      section >=20000 & section <=20018 & ca_code == "VC" ~ 
        "Accident and accident reports",
      section >=21000 & section <=23336 & ca_code == "VC" ~ 
        "Rules of the road",
      section >=23500 & section <=23675 & ca_code == "VC" ~ 
        "Sentencing for driving while under the influence",
      section >=24000 & section <=28160 & ca_code == "VC" ~ 
        "Equipment of vehicles",
      section >=29000 & section <=31560 & ca_code == "VC" ~ 
        "Towing and loading equipment",
      section >=31600 & section <=31620 & ca_code == "VC" ~ 
        "Transportation of explosives",
      section >=32000 & section <=32053 & ca_code == "VC" ~ 
        "Transportation of hazardous material",
      section >=32100 & section <=32109 & ca_code == "VC" ~
        "Transportation of inhalation hazards",
      section >=33000 & section <=33002 & ca_code == "VC" ~ 
        "Transportation of radioactive materials",
      section >=34000 & section <=34100 & ca_code == "VC" ~ 
        "Flammable and combustible liquids",
      section >=34500 & section <=34520.5 & ca_code == "VC" ~ 
        "Safety regulations",
      section >=34600 & section <=34672 & ca_code == "VC" ~ 
        "Motor carriers of property permit act",
      section >=34680 & section <=34693 & ca_code == "VC" ~ 
        "Private carriers of passengers registration act",
      section >=34700 & section <=34725 & ca_code == "VC" ~ 
        "Motor vehicle damage control",
      section >=35000 & section <=35796 & ca_code == "VC" ~ 
        "Size, weight, and load",
      section >=36000 & section <=36800 & ca_code == "VC" ~ 
        "Implements of husbandry",
      section >=38000 & section <=38604 & ca_code == "VC" ~ 
        "Off-highway vehicles",
      section >=38750 & section <=38755 & ca_code == "VC" ~ 
        "Autonomous vehicles",
      section >=39000 & section <=39011 & ca_code == "VC" ~ 
        "Registration and licensing of bicycles",
      section >=40000.1 & section <=41610 & ca_code == "VC" ~ 
        "Offenses and prosecution",
      section >=42000 & section <=42277 & ca_code == "VC" ~ 
        "Penalties and disposition of fees, fines, and forfeitures",
    ))
  # Business and Professions Code
    # Divisions
      # Division numbers
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
     # Division headings
  le_code <- le_code %>% 
    mutate(division_head = case_when(
      section >= 100 & section <= 472.5 & ca_code == "BP" ~ 
        "Department of consumer affairs",
      section >= 475 & section <= 499 & ca_code == "BP"  ~ 
        "Denial, suspension, and revocation of licenses",
      section >= 500 & section <= 4999.129 & ca_code == "BP"  ~ 
        "Healing arts",
      section >= 5000 & section <= 9998.11 & ca_code == "BP"  ~ 
        "Professions and vocations generally",
      section >= 10000 & section <= 11506 & ca_code == "BP"  ~ 
        "Real estate",
      section >= 12001 & section <= 13800 & ca_code == "BP"  ~ 
        "Weights and measures",
      section >= 14000 & section <= 14704 & ca_code == "BP"  ~ 
        "Business rights",
      section >= 16000 & section <= 18001 & ca_code == "BP"  ~ 
        "General business regulations",
      section >= 18400 & section <= 22949.51 & ca_code == "BP"  ~ 
        "Special business regulations",
      section >= 22950 & section <= 22964 & ca_code == "BP"  ~ 
        "Stop tobacco access to kids enforcement act",
      section >= 22970 & section <= 22991 & ca_code == "BP"  ~ 
        "Cigarette and tobacco products licensing act of 2003",
      section >= 23000 & section <= 25762 & ca_code == "BP"  ~ 
        "Alcoholic beverages",
      section >= 26000 & section <= 26250 & ca_code == "BP"  ~ 
        "Cannabis"
    ))
  # Health and Safety Code
    # Divisions
      # Division numbers
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
        # Division headings
  le_code <- le_code %>% 
    mutate(division_head = case_when(
      section >= 135 & section <= 1179.102 & ca_code == "HS" ~ 
        "Administration of public health" ,
      section >= 1180 & section <= 1180.6 & ca_code == "HS" ~ 
        "Use of seclusion and behavioral restraints in facilities" ,
      section >= 1200 & section <= 1797.8 & ca_code == "HS" ~ 
        "Licensing provisions" ,
      section >= 1797 & section <= 1799.207 & ca_code == "HS" ~ 
        "Emergency medical services" ,
      section >= 2000 & section <= 2910 & ca_code == "HS" ~ 
        "Pest abatement" ,
      section >= 4600 & section <= 6127 & ca_code == "HS" ~ 
        "Sanitation" ,
      section >= 6400 & section <= 6982 & ca_code == "HS" ~ 
        "Sanitary districts" ,
      section >= 7000 & section <= 8030 & ca_code == "HS" ~ 
        "Dead bodies" ,
      section >= 8100 & section <= 9703 & ca_code == "HS" ~ 
        "Cemeteries" ,
      section >= 11000 & section <= 11651 & ca_code == "HS" ~ 
        "Uniform controlled substances act" ,
      section >= 11700 & section <= 11717 & ca_code == "HS" ~ 
        "Drug dealer liability act" ,
      section >= 11750 & section <= 11975 & ca_code == "HS" ~ 
        "Alcohol and drug programs" ,
      section >= 11998 & section <= 11998.4 & ca_code == "HS" ~ 
        "Drug and alcohol abuse master plans" ,
      section >= 11999 & section <= 11999.3 & ca_code == "HS" ~ 
        "Illegal use of drugs and alcohol" ,
      section >= 11999.4 & section <= 11999.13 & ca_code == "HS" ~ 
        "Substance abuse treatment funding" ,
      section >= 11999.2 & section <= 11999.25 & ca_code == "HS" ~ 
        "Substance abuse testing and treatment accountability program" ,
      section >= 12000 & section <= 12761 & ca_code == "HS" ~ 
        "Explosives" ,
      section >= 13000 & section <= 14959 & ca_code == "HS" ~ 
        "Fires and fire protection" ,
      section >= 16000 & section <= 16604 & ca_code == "HS" ~ 
        "Buildings used by the public" ,
      section >= 17000 & section <= 19997 & ca_code == "HS" ~ 
        "Housing" ,
      section >= 20000 & section <= 20115 & ca_code == "HS" ~ 
        "Police protection" ,
      section >= 24000 & section <= 26250 & ca_code == "HS" ~ 
        "Miscellaneous health and safety provisions" ,
      section >= 32000 & section <= 32499.4 & ca_code == "HS" ~ 
        "Hospital districts" ,
      section >= 32500 & section <= 32508 & ca_code == "HS" ~ 
        "Endowment hospitals" ,
      section >= 33000 & section <= 37964 & ca_code == "HS" ~ 
        "Community development and housing" ,
      section >= 38000 & section <= 38041 & ca_code == "HS" ~ 
        "Health and welfare agency—direct service contracts reform act" ,
      section >= 38050 & section <= 38065 & ca_code == "HS" ~ 
        "Health and welfare agency—administrative appeals process for nonprofit human services agencies" ,
      section >= 38070 & section <= 38081.1 & ca_code == "HS" ~ 
        "State department of health services cooperative agreement act" ,
      section >= 38500 & section <= 38599 & ca_code == "HS" ~ 
        "California global warming solutions act of 2006" ,
      section >= 39000 & section <= 44474 & ca_code == "HS" ~ 
        "Air resources" ,
      section >= 44500 & section <= 44563 & ca_code == "HS" ~ 
        "California pollution control financing authority act" ,
      section >= 46000 & section <= 46080 & ca_code == "HS" ~ 
        "Noise control act" ,
      section >= 50000 & section <= 54034 & ca_code == "HS" ~ 
        "Housing and home finance" ,
      section >= 55000 & section <= 55117 & ca_code == "HS" ~ 
        "Seismic safety building rehabilitation loans" ,
      section >= 57000 & section <= 57020 & ca_code == "HS" ~ 
        "Regulation of environmental protection" ,
      section >= 57050 & section <= 57053.9 & ca_code == "HS" ~ 
        "Repair or maintenance projects" ,
      section >= 100100 & section <= 101997 & ca_code == "HS" ~ 
        "Administration of public health" ,
      section >= 102100 & section <= 103925 & ca_code == "HS" ~ 
        "Vital records and health statistics" ,
      section >= 104100 & section <= 106036 & ca_code == "HS" ~ 
        "Disease prevention and health promotion" ,
      section >= 106500 & section <= 119406 & ca_code == "HS" ~ 
        "Environmental health" ,
      section >= 120100 & section <= 122477 & ca_code == "HS" ~ 
        "Communicable disease prevention and control" ,
      section >= 123100 & section <= 125850 & ca_code == "HS" ~ 
        "Personal health care (including maternal, child, and adolescent)" ,
      section >= 127000 & section <= 130070 & ca_code == "HS" ~ 
        "Statewide health planning and development" ,
      section >= 130100 & section <= 130158 & ca_code == "HS" ~ 
        "California children and families program" ,
      section == 130200 & ca_code == "HS" ~ 
        "Office of health information integrity" ,
      section >= 130250 & section <= 130255 & ca_code == "HS" ~ 
        "California health information technology and exchange act" ,
      section >= 130275 & section <= 130282 & ca_code == "HS" ~ 
        "Health information exchange privacy and security demonstration projects" ,
      section >= 130300 & section <= 130315 & ca_code == "HS" ~
        "The health insurance portability and accountability implementation act of 2001" ,
      section >= 130400 & section <= 130410 & ca_code == "HS" ~ 
        "Golden bear state pharmacy assistance program" ,
      section >= 130500 & section <= 130544 & ca_code == "HS" ~ 
        "California discount prescription drug program" ,
      section >= 131000 & section <= 131231 & ca_code == "HS" ~ 
        "Public health" ,
      section >= 131500 & section <= 131550 & ca_code == "HS" ~ 
        "The adult health coverage expansion program" ,
      section >= 132000 & section <= 132008 & ca_code == "HS" ~ 
        "Prescription drug discount prohibition" ,
      section >= 134000 & section <= 134002 & ca_code == "HS" ~ 
        "Preserving access to affordable drugs" ,
      section >= 136000 & section <= 136030 & ca_code == "HS" ~ 
        "Office of patient advocate" ,
      section >= 150200 & section <= 150208 & ca_code == "HS" ~ 
        "Surplus medication collection and distribution" ,
      section >= 151000 & section <= 151003 & ca_code == "HS" ~ 
        "Sexual health education accountability act"))
    # Add felony/misdemeanor categories
  le_code <- left_join(le_code, severity, by = "sentence")
    # Correct column classes
  le_code <- le_code %>% 
   mutate_at(vars(ca_code, section_full, section, subsection1, subsection2,
             subsection3, subsection4, subsection5, description, sentence, 
             sec_code, part, part_head, title, title_head, chapter, chapter_head,
             division, division_head, severity), as.factor)
    # Reshape multiple sec_code rows into single row 
  le_code <- le_code %>% group_by(sec_code) %>% mutate(id = row_number())
  le_code <- le_code %>%
    pivot_wider(names_from = id, 
                values_from = c(description, sentence, severity))
  
  # JOIN DATASETS-----
  davis_log_long <- left_join(davis_log_long, le_code, by = "sec_code")
  davis_log_long <- left_join(davis_log_long, supp_codes, by = "sec_code")
    # Verify column classes
  davis_log_long <- davis_log_long %>% 
    mutate_at(vars(indiv_id, sex, race, charge_num, sec_code, ca_code,
                   section_full, section, section, subsection1, subsection2,
                   subsection3, subsection4, subsection5, description_1, 
                   description_2, description_3, description_4, description_5,
                   description_6, description_7, description_8, sentence_1, 
                   sentence_2, sentence_3, sentence_4, sentence_5, sentence_6, 
                   sentence_7, sentence_8, severity_1, severity_2, severity_3, 
                   severity_4, severity_5, severity_6, severity_7, severity_8, 
                   part, part_head, division, division_head,
                   title, title_head, chapter,
                   chapter_head, category), as.factor)
  
  # EXPORT INTO REPO----
  write.csv(davis_log_long, "./data/generated/davis_log_long.csv")


  