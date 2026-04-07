
rm(list = ls(all = TRUE))



library(dplyr)

###Choose the directory on your machine where the data is stored
setwd("C:/Users/tfrawley/Desktop/Merging_Surveys/")

##Load in P2 Survey Data
P2_Survey<-read.csv("Output_Data/P2_Survey.csv")

####Rename this initial columns where additional periods got added in
colnames(P2_Survey)[colnames(P2_Survey) == "Duration..in.seconds."] <- "Durationinseconds"
colnames(P2_Survey)[colnames(P2_Survey) == "Q1."] <- "Q1"
colnames(P2_Survey)[colnames(P2_Survey) == "Q2."] <- "Q2"

##Insert Blank Column to accomodate P1's "name the most important area" question
insert_after <- which(colnames(P2_Survey) == "Q3")
P2_Survey$Q3_4_TEXT_P1 <- NA
new_cols <- c("Q3_4_TEXT_P1")
remaining <- setdiff(colnames(P2_Survey), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
P2_Survey<- P2_Survey[, new_order]

##Rename Mapping County Columns
P2_Survey<- rename(P2_Survey, 
                   QMapping_North_County = Q3a1,
                   QMapping_Central_County = Q3a2,
                   QMapping_South_County = Q3a3) 


##Change naming convention of "list marine species commonly targeted while fishing or harvesting" to be consistent and continuous
colnames(P2_Survey)[colnames(P2_Survey) == "Q12_4"] <- "QFishing_Species_A"
colnames(P2_Survey)[colnames(P2_Survey) == "Q12_6"] <- "QFishing_Species_B"

###Add In additional slot for marine species listing to match the three used in Phase I data
insert_after <- which(colnames(P2_Survey) == "QFishing_Species_B")
P2_Survey$QFishing_Species_C<- NA
new_cols <- c("QFishing_Species_C")
remaining <- setdiff(colnames(P2_Survey), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
P2_Survey<- P2_Survey[, new_order]


##Change naming convention of "list marine species valued for observing or interacting" to be consistent and continuous
colnames(P2_Survey)[colnames(P2_Survey) == "Q14_4"] <- "QOI_Species_A"
colnames(P2_Survey)[colnames(P2_Survey) == "Q14_5"] <- "QOI_Species_B"

###Add In additional slot for marine species listing to match the three used in Phase I data
insert_after <- which(colnames(P2_Survey) == "QOI_Species_B")
P2_Survey$QOI_Species_C<- NA
new_cols <- c("QOI_Species_C")
remaining <- setdiff(colnames(P2_Survey), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
P2_Survey<- P2_Survey[, new_order]


###Rename additional columns to faciliate tracking and merging across versions and phases
P2_Survey<- rename(P2_Survey, 
                   QImportant_Activities = Q4,
                   QImportant_Activities_TEXT = Q4_18_TEXT,
                   QImportant_Activities_Most = Q5,
                   QImportant_Activities_Most_TEXT = Q5_18_TEXT,
                   QTransport_Mode = Q6,
                   QTransport_Time = Q7,
                   QActivity_Mentor = Q8,
                   QActivity_Mentor_TEXT = Q8_6_TEXT,
                   QActivity_Companion = Q9,
                   QFishing = Q10,
                   QFishing_Type = Q11,
                   QCatch_Type = Q13)


###Rename Barriers Columns to establish consistent and continuous naming system
old_names <- c("Q21a_1", "Q21a_2", "Q21a_3", "Q21a_4", "Q21a_5", 
               "Q21b_1", "Q21b_2", "Q21b_3", "Q21b_4", "Q21b_5")

P2_Survey <- P2_Survey %>%
  rename_with(~ paste0("QBarriers_", 1:10), all_of(old_names))

###Rename Microagression Columns to establish consistent and continuous naming system
P2_Survey <- rename(P2_Survey , 
                    QMicroA_1 = Q22_1, 
                    QMicroA_2 = Q22_2, 
                    QMicroA_3 = Q22_3, 
                    QMicroA_4 = Q22_4) 


###Rename Media Engagement Questions to establish consistent and continuous naming system

P2_Survey <- rename(P2_Survey , 
                  QMedia1 = Q18_1, 
                  QMedia2 = Q18_2)

###Rename Climate Observations & Concerns Questions to establish consistent and continuous naming system

P2_Survey <- rename(P2_Survey , 
                    QClimate_Obs_S_1 = Q23_1, 
                    QClimate_Obs_S_2 = Q23_2, 
                    QClimate_Obs_S_3 = Q23_3, 
                    QClimate_Obs_S_4 = Q23_4) 

P2_Survey <- rename(P2_Survey , 
                    QClimate_Conc_S_1 = Q24_1, 
                    QClimate_Conc_S_2 = Q24_2, 
                    QClimate_Conc_S_3 = Q24_3, 
                    QClimate_Conc_S_4 = Q24_4) 

P2_Survey <- rename(P2_Survey , 
                    QClimate_Obs_E_1 = Q25_1, 
                    QClimate_Obs_E_2 = Q25_2, 
                    QClimate_Obs_E_3 = Q25_3, 
                    QClimate_Obs_E_4 = Q25_4,
                    QClimate_Obs_E_5 = Q25_5,
                    QClimate_Obs_E_6 = Q25_6,
                    QClimate_Obs_E_7 = Q25_7)

P2_Survey <- rename(P2_Survey , 
                    QClimate_Conc_E_1 = Q26_1, 
                    QClimate_Conc_E_2 = Q26_2, 
                    QClimate_Conc_E_3 = Q26_3, 
                    QClimate_Conc_E_4 = Q26_4,
                    QClimate_Conc_E_5 = Q26_5,
                    QClimate_Conc_E_6 = Q26_6,
                    QClimate_Conc_E_7 = Q26_7) 

P2_Survey <- rename(P2_Survey, 
                    QMPA_Aware = Q27, 
                    QSanct_Aware = Q28)


P2_Survey <- rename(P2_Survey, 
                    QMPA_Purpose = Q29, 
                    QMPA_Science = Q30)

##Remove this placeholder columns which refer to the MPA Maps that are only part of Phase 2
P2_Survey<-P2_Survey %>% 
  select(-Info1, -Info1_repeat)


P2_Survey <- rename(P2_Survey, 
                    QMPA_Visit = Q31, 
                    QMPA_Eval1 = Q32_1,
                    QMPA_Eval2 = Q32_2,
                    QMPA_Eval3 = Q32_3,
                    QMPA_Eval4 = Q32_4)

##Rename Priority Ranking Question
P2_Survey<- rename(P2_Survey, 
                   QPriority_1 = Q33_1, 
                   QPriority_2 = Q33_2, 
                   QPriority_3 = Q33_3, 
                   QPriority_4 = Q33_4, 
                   QPriority_5 = Q33_5, 
                   QPriority_6 = Q33_6, 
                   QPriority_7 = Q33_7, 
                   QPriority_8 = Q33_8,
                   QPriority_9 = Q33_9,
                   QPriority_10 = Q33_10)


##Rename Climate Belief and Attitude Questions
P2_Survey<- rename(P2_Survey, 
                   QClimate_Belief = Q34, 
                   QClimate_Attitude1 = Q35_1,
                   QClimate_Attitude2 = Q35_2, 
                   QClimate_Attitude3 = Q35_3, 
                   QClimate_Attitude4 = Q35_4, 
                   QClimate_Attitude5 = Q35_5)

###Rename Futures  Questions
P2_Survey<- rename(P2_Survey, 
                   QFutures1 = Q36_1,
                   QFutures2 = Q36_2, 
                   QFutures3 = Q36_3, 
                   QFutures4 = Q36_4)

###Rename Demographic Questions
P2_Survey<- rename(P2_Survey, 
                   QDemographic_Home = Q37,
                   QDemographic_PrimaryZip = Q37b, 
                   QDemographic_Country_State = Q37c, 
                   QDemographic_CA_Years = Q38,
                   QDemographic_Race= Q39,
                   QDemographic_Race_TEXT= Q39_10_TEXT,
                   QDemographic_Asian= Q40,
                   QDemographic_Asian_TEXT=Q40_4_TEXT,
                   QDemographic_Tribal=Q41,
                   QDemographic_Tribal_TEXT=Q42,
                   QDemographic_Gender=Q43,
                   QDemographic_Birth=Q44,
                   QDemographic_Income=Q45,
                   QDemographic_Education=Q46,
                   QDemographic_Family=Q47,
                   QDemographic_FamilySize=Q48,
                   QDemographic_Swimming=Q49)


##Rename Final Columns
P2_Survey<- rename(P2_Survey, 
                   Comments = Q50,
                   Update = Q50_online,
                   Email = email)


write.csv(P2_Survey, "Output_Data/P2_Restructured.csv", row.names=FALSE)






