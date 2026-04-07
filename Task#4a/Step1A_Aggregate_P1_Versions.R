
rm(list = ls(all = TRUE))

library(dplyr)


###Choose the directory on your machine where data is stored
setwd("C:/Users/tfrawley/Desktop/Merging_Surveys/")



###Load in and Aggregate P1 In-Person Surveys

###Load first file
Person_UCSC<-read.csv("Input_Data/P1/UCSC_Survey_Final_V2_In_Person.csv")
##Add-in version & mechanism identifiers
Person_UCSC$Version<-"P1_UCSC_V2"
Person_UCSC$Mechanism<-"In_Person"

##Load second file
Person_Lucas<-read.csv("Input_Data/P1/Lucas_Survey_Final_V2_In-Person.csv")
###Remove in Write in Gender Column, requested by Lucas
Person_Lucas<-Person_Lucas %>% 
  select(-Q25._5_TEXT)
Person_Lucas$Version<-"P1_Lucas_V2"
Person_Lucas$Mechanism<-"In_Person"
##Remove redundant header info
Person_Lucas <- Person_Lucas [-(1:2), ]

###Load in third file
Person_UCSB<-read.csv("Input_Data/P1/UCSB_Survey_Final_V2_In-Person.csv")
###Remove in Columns appended at the end of this version
Person_UCSB<-Person_UCSB %>% 
  select(-Q78_1_1, -Q78_2_1)
Person_UCSB$Version<-"P1_UCSB_V2"
Person_UCSB$Mechanism<-"In_Person"
Person_UCSB <- Person_UCSB [-(1:2), ]

###Aggregate all in-person files
P1_Person<-rbind(Person_UCSC, Person_Lucas, Person_UCSB)

##Rename final coulmns
P1_Person<- rename(P1_Person, 
                  Email = Q33._1_TEXT,
                  Update = Q33.)


###Add in Tribal Demographic Questions that were subsequently added to version 3
P1_Person$Q24b <- NA
P1_Person$Q24b.1 <- NA
P1_Person$Q24b.1_1_TEXT <- NA
new_cols <- c("Q24b", "Q24b.1", "Q24b.1_1_TEXT")
P1_Person <- P1_Person %>%
  relocate(all_of(new_cols), .after = Q24a)

###Add in a prize column to be consistent with subsequent versions                  
P1_Person$Prize<-NA
new_col <- c("Prize")
P1_Person <- P1_Person %>%
  relocate(all_of(new_col), .after = Update)
                 

###Load in and Aggregate P1 Online Surveys

###Load in second online version
V3_Online<-read.csv("Input_Data/P1/Survey_Final_V3_Online.csv")

##Relocate demographic Q's from front to back
V3_Demographic_Columns <- c("Q22", "Q23", "Q23_1_TEXT", "Q24", "Q24a.", "Q24b", "Q24b.1.", "Q24b.1._1_TEXT", "Q25", "Q26", "Q27", "Q28", "Q29.", "Q30", "Q31.")
V3_Online <- V3_Online %>%
  relocate(all_of(V3_Demographic_Columns), .after = Q21_4)

V3_Online$Version<-"P1_V3_Online"
V3_Online$Mechanism<-"Online"

## Lod in first online version
V2_Online<-read.csv("Input_Data/P1/Survey_Final_V2_Online.csv")

###Add in Tribal Demographic Questions that were subsequently added
V2_Online$Q24b <- NA
V2_Online$Q24b.1. <- NA
V2_Online$Q24b.1._1_TEXT <- NA
new_cols <- c("Q24b", "Q24b.1.", "Q24b.1._1_TEXT")
V2_Online <- V2_Online %>%
  relocate(all_of(new_cols), .after = Q24a.)

###Add in survey identifiers and remove redundant column info
V2_Online$Version<-"P1_V2_Online"
V2_Online$Mechanism<-"Online"
V2_Online <- V2_Online[-(1:2), ]


##Aggregate online versions
P1_Online<-rbind(V3_Online, V2_Online)


###Standardize naming and order of final colums
P1_Online<- rename(P1_Online, 
                   Q18=Q19.,
                   Prize = Q34,
                   Update = Q36,
                   Email = Q33a.)

P1_Online <- P1_Online %>%
  relocate(any_of(c("Update", "Prize", "Email")), .after = Q32)

###

##Remove Redundant Header Info
P1_Person <- P1_Person [-(1:2), ]
###Remove Extra Spaces and Periods from column names, sometimes these get added when files are exported/imported
P1_Person <- P1_Person %>% rename_with(~ gsub("\\.", "", .x))
P1_Online <- P1_Online %>% rename_with(~ gsub("\\.", "", .x))

##Bind all P1 Data
P1_Final<-rbind(P1_Online, P1_Person)

###Output Aggregated P1 Data
write.csv(P1_Final, "Output_Data/P1_Survey.csv", row.names=FALSE)










                