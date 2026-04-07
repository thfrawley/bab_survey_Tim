rm(list = ls(all = TRUE))

###Aggregate In-Person Honorarium Versions

library(dplyr)

###Choose the directory on your machine where the data is stored
setwd("C:/Users/tfrawley/Desktop/Merging_Surveys/")

##Load first file
SD<-read.csv("Input_Data/bab_survey_2025_in_person_honorarium_SD.csv")
####Add in ID info to track versions
SD$Version<-"P2_SD_Honor."
SD$Mechanism<-"In_Person"

##Load second file
Sonoma<-read.csv("Input_Data/bab_survey_2025_in_person_honorarium_sonoma.csv")
####Add in ID info to track versions
Sonoma$Version<-"P2_Sonoma_Honor."
Sonoma$Mechanism<-"In_Person"
##Remove duplicate column identifier info
Sonoma <- Sonoma  [-(1:2), ]

##Load third file
LA<-read.csv("Input_Data/bab_survey_2025_in_person_honorarium_la.csv")
####Add in ID info to track versions
LA$Mechanism<-"In_Person"
LA$Version<-"P2_LA_Honor."
##Remove duplicate column identifier info
LA <- LA [-(1:2), ]

###Load 4th file
HDN<-read.csv("Input_Data/bab_survey_2025_in_person_honorarium_hdn.csv")
####Add in ID info to track versions
HDN$Version<-"P2_HDN_Honor."
HDN$Mechanism<-"In_Person"
##Remove duplicate column identifier info
HDN <- HDN [-(1:2), ]

###Load 5th file
SB<-read.csv("Input_Data/bab_survey_2025_in_person_honorarium_hdn.csv")
####Add in ID info to track versions
SB$Version<-"P2_SB_Honor."
SB$Mechanism<-"In_Person"
##Remove duplicate column identifier info
SB <- SB [-(1:2), ]

###Load 6th file
Template<-read.csv("Input_Data/Survey_UCSB_Final_2025_template.csv")
Template<- rename(Template, 
                  Q50_online= Q50.1)
Template$Version<-"P2_Template"
Template$Mechanism<-"In_Person"
##Remove duplicate column identifier info
Template <- Template [-(1:2), ]


##Aggregate all Honorarium Versions
P2_Honor<-rbind(SD, Sonoma, LA, HDN, SB, Template)

###Add in Blank Prize Columns Missing from Honorarium Versions
P2_Honor$Prize<-NA
P2_Honor$Prize_Region<-NA
P2_Honor$Prize_Ferry<-NA


####
#####
##Aggregate Prize Surveys

##Load in first prize file
Online_Prize<-read.csv("Input_Data/bab_survey_2025_online_prize.csv")
####Add in ID info to track versions
Online_Prize$Version<-"P2_Online_Prize"
Online_Prize$Mechanism<-"Online"

##Load in second prize file
SoCal_Prize<-read.csv("Input_Data/bab_survey_2025_in_person_prize_socal.csv")
####Add in ID info to track versions
SoCal_Prize$Version<-"P2_SoCal_Prize"
SoCal_Prize$Mechanism<-"In_Person"
##Remove duplicate column identifier info
SoCal_Prize <- SoCal_Prize [-(1:2), ]

##Load in third prize file
NorCal_Prize<-read.csv("Input_Data/bab_survey_2025_in_person_prize_norcal.csv")
####Add in ID info to track versions
NorCal_Prize$Version<-"P2_NorCal_Prize"
NorCal_Prize$Mechanism<-"In_Person"
##Remove duplicate column identifier info
NorCal_Prize <- NorCal_Prize [-(1:2), ]

###Aggregate all Prize Versions
P2_Prize<-rbind(Online_Prize, SoCal_Prize, NorCal_Prize)

##Rename Prize Column Names
P2_Prize<- rename(P2_Prize, 
                  Prize = Q51,
                  Prize_Region = Q52,
                  Prize_Ferry = Q53) 

##Remove duplicate column identifier info
P2_Prize <- P2_Prize [-(1:2), ]


###Create Master Data Set Combining Honorarium Data and Prize Data
P2_Survey<-rbind(P2_Honor, P2_Prize)

##write.csv(P2_Survey, "P2_Survey.csv", row.names=FALSE)



###OPTIONAL
###Adds IN Data From The Tribal pilot
####If it was me I would omit this data because the survey structure is so different from everything else there
### is a high likelihood of data corruption and the potential upside is only 8-10 response


Tribal<-read.csv("Input_Data/Survey_Final_V3_Tribal_pilot.csv")

##Matches based on column identifiers
ids_P2_Survey <- as.character(P2_Survey[2, ])
ids_Tribal <- as.character(Tribal[2, ])

# Step 2: Create the look-up dictionary from the Master dataset
lookup_table <- setNames(colnames(P2_Survey), ids_P2_Survey)

# Step 3: Translate column names (keeping the NAs for the audit table)
matched_names <- unname(lookup_table[ids_Tribal])

# Step 4: Create the intermediate audit data.frame
match_review <- data.frame(
  Original_Tribal_Name = colnames(Tribal),
  Qualtrics_ID = ids_Tribal,
  Matched_P2_Name = matched_names,
  Match_Found = !is.na(matched_names), # Creates a TRUE/FALSE column
  stringsAsFactors = FALSE)

###Creates a copy of the original data frame
Tribal_Base<-Tribal

final_names <- matched_names
missing_matches <- is.na(final_names)
final_names[missing_matches] <- colnames(Tribal)[missing_matches]
colnames(Tribal) <- final_names


# Step 6: Identify which P2_Survey columns are completely missing from Tribal
missing_columns <- setdiff(colnames(P2_Survey), colnames(Tribal))

# Step 7: Create these missing columns in Tribal and fill them with NA (blanks)
Tribal[missing_columns] <- NA

# Step 8: THE FIX - Strictly subset and reorder to match P2_Survey exactly
# This automatically drops any leftover Tribal columns that didn't match
Tribal <- Tribal[, colnames(P2_Survey)]

###Adds in columns from the original data frame that the matching missed for whatever reason
Tribal$Q14_5<-Tribal_Base$Q16_6
Tribal$Q21b_1<-Tribal_Base$Q23_6
Tribal$Q21b_2<-Tribal_Base$Q23_7
Tribal$Q21b_3<-Tribal_Base$Q23_8
Tribal$Q21b_4<-Tribal_Base$Q23_9
Tribal$Q21b_5<-Tribal_Base$Q23_10

Tribal$Version<-"P2_Tribal_Pilot"
Tribal$Mechanism<-"Online"

Tribal <- Tribal [-(1:2), ]


P2_Survey<-rbind(P2_Survey, Tribal)

write.csv(P2_Survey, "Output_Data/P2_Survey.csv", row.names=FALSE)







