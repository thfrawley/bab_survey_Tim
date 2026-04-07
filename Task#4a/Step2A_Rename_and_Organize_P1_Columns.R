


###START HERE



rm(list = ls(all = TRUE))

library(dplyr)


###Choose the directory on your machine where data is stored
setwd("C:/Users/tfrawley/Desktop/Merging_Surveys/")



###Load in Aggregated Phase 1 Data Generated in Step 1A
Combined<-read.csv("Output_Data/P1_Survey.csv")


###Insert Blank Column for "Have you taken the survey before" question used in Phase 2
insert_after <- which(colnames(Combined) == "Consent")
Combined$Q0 <- NA
new_cols <- c("Q0")
remaining <- setdiff(colnames(Combined), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Combined<- Combined[, new_order]


###Rename Initial Location Question
colnames(Combined)[colnames(Combined) == "Q3_4_TEXT"] <- "Q3_4_TEXT_P1"

###ADD IN Blank Mapping Columns
New_Cols<-c("QMapping_North_County","QMapping_Central_County","QMapping_South_County",
            "Q3b1_1_x","Q3b1_1_y","Q3b2_1_x","Q3b2_1_y","Q3b3_1_x","Q3b3_1_y",
            "Q3b4_1_x","Q3b4_1_y","Q3b5_1_x","Q3b5_1_y","Q3b6_1_x","Q3b6_1_y",
            "Q3b7_1_x","Q3b7_1_y","Q3b8_1_x","Q3b8_1_y","Q3b9_1_x","Q3b9_1_y",
            "Q3b10_1_x","Q3b10_1_y","Q3b11_1_x","Q3b11_1_y","Q3b12_1_x","Q3b12_1_y",
            "Q3b13_1_x","Q3b13_1_y","Q3b14_1_x","Q3b14_1_y","Q3b15_1_x","Q3b15_1_y",
            "Q3b16_1_x","Q3b16_1_y","Q3b18_1_x","Q3b18_1_y")

for(col_name in New_Cols){
  Combined[[col_name]]<-NA
}


insert_after<-which(colnames(Combined) == "Q3_4_TEXT_P1")
remaining<-setdiff(colnames(Combined), New_Cols)
new_order<-c(remaining[1:insert_after], New_Cols, remaining[(insert_after+1):length(remaining)])
Combined<-Combined[, new_order]


###Add In Blank Column For Text Box Used To Specify Important Activities

insert_after <- which(colnames(Combined) == "Q4")
Combined$Q4_18_TEXT <- NA
new_cols <- c("Q4_18_TEXT")
remaining <- setdiff(colnames(Combined), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Combined<- Combined[, new_order]


###Add In Blank Column For Text Box Used To Specify Most Important Activity
insert_after <- which(colnames(Combined) == "Q5")
Combined$Q5_18_TEXT <- NA
new_cols <- c("Q5_18_TEXT")
remaining <- setdiff(colnames(Combined), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Combined<- Combined[, new_order]


###Remove text box used to specify usual mode of transportation used in Phase 2=1 survey
Combined<- select(Combined, -Q6_8_TEXT)

###Remove text box used to who most important activity is usually done with in Phase 1 survey
Combined<- select(Combined, -Q9_6_TEXT)

###Rename Fishing Activities Participation question 
colnames(Combined)[colnames(Combined) == "Q5a"] <- "Q10"

###Add In Blank Column For "Type of Fisherman" Question
insert_after <- which(colnames(Combined) == "Q10")
Combined$Q11 <- NA
new_cols <- c("Q11")
remaining <- setdiff(colnames(Combined), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Combined<- Combined[, new_order]

###Rename list marine species commonly targeted columns to match conventions used in Phase II
colnames(Combined)[colnames(Combined) == "Q5b1_4"] <- "QFishing_Species_A"
colnames(Combined)[colnames(Combined) == "Q5b1_5"] <- "QFishing_Species_B"
colnames(Combined)[colnames(Combined) == "Q5b1_6"] <- "QFishing_Species_C"

###Rename "What usually happens to the marine species listed above" question to match Phase II format
colnames(Combined)[colnames(Combined) == "Q5c"] <- "QCatch_Type"


####Collapse the 3 different "list 3 species questions" asked to non-fishers in Phase I to a single question series 
     ###In order to match the format of Phase II
Combined <- Combined %>%
  mutate(across(starts_with("Q5b"), ~ na_if(trimws(.), ""))) %>%
  mutate(
    Q14_A = coalesce(Q5b2_4, Q5b3_4, Q5b4_4),
    Q14_B = coalesce(Q5b2_5, Q5b3_5, Q5b4_5),
    Q14_C = coalesce(Q5b2_6, Q5b3_6, Q5b4_6) ) %>%
  select(-starts_with("Q5b2"), -starts_with("Q5b3"), -starts_with("Q5b4"))

insert_after <- which(colnames(Combined) == "QCatch_Type")
new_cols <- c("Q14_A", "Q14_B", "Q14_C")
remaining <- setdiff(colnames(Combined), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Combined<- Combined[, new_order]

####Rename Columns to facilite tracking and aggregation across survey phases
Combined<- rename(Combined, 
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
                  QOI_Species_A= Q14_A,
                  QOI_Species_B = Q14_B,
                  QOI_Species_C = Q14_C)


###Add in Blank Rows to Correspond with the Economics Questions asked in Phase II
New_Cols <- c("Q15", "Q15_2_TEXT", "Q16", 
              "Q17a_1_1", "Q17a_2_1", "Q17a_3_1", "Q17a_4_1", 
              "Q17a_5_1", "Q17a_6_1", "Q17a_7_1", "Q17a_8_1", 
              "Q17a_9_1", "Q17a_10_1", "Q17a_11_1", 
              "Q17b_1_1", "Q17b_2_1", "Q17b_3_1", "Q17b_4_1", 
              "Q17b_5_1", "Q17b_6_1", "Q17b_7_1", "Q17b_8_1", 
              "Q17b_9_1", "Q17b_10_1", "Q17b_11_1", "Q17b_12_1", 
              "Q17b_13_1", "Q17b_14_1", 
              "Time.Tracker_First.Click", "Time.Tracker_Last.Click", 
              "Time.Tracker_Page.Submit", "Time.Tracker_Click.Count")

for(col_name in New_Cols){
  Combined[[col_name]] <- NA
}

insert_after <- which(colnames(Combined) == "QOI_Species_C")
remaining <- setdiff(colnames(Combined), New_Cols)
new_order <- c(remaining[1:insert_after], New_Cols, remaining[(insert_after+1):length(remaining)])
Combined <- Combined[, new_order]


###Rename Media Engagement Question to Match Phase II format
Combined<- rename(Combined, 
             QMedia1 = Q10_1, 
             QMedia2 = Q10_2)

### Remove and Save Demographic Data, to append on the end, matching Phase II format
demographic_cols <- c("Q22", "Q23", "Q23_1_TEXT", "Q24", "Q24a", 
                 "Q24b", "Q24b1", "Q24b1_1_TEXT", "Q25", 
                 "Q26", "Q27", "Q28", "Q29", "Q30", "Q31")

Phase1_Demographic_Data<- select(Combined, all_of(demographic_cols))
Combined <- select(Combined, -any_of(demographic_cols))

###Rename Priority Ranking Questions to Be Consistent with Phase 2

Combined<- rename(Combined, 
                  QPriority_1 = Q19_1, 
                  QPriority_2 = Q19_2, 
                  QPriority_3 = Q19_3, 
                  QPriority_4 = Q19_4, 
                  QPriority_5 = Q19_5, 
                  QPriority_6 = Q19_6, 
                  QPriority_7 = Q19_7, 
                  QPriority_8 = Q19_8,
                  QPriority_9 = Q19_9,
                  QPriority_10 = Q19_10)

###Rename Wellbeing Questions to bo be Consistent With Phase 2
Combined<- rename(Combined, 
                  Q19_1 = Q11_1, 
                  Q19_2 = Q11_2, 
                  Q19_3 = Q11_3, 
                  Q19_4 = Q11_4, 
                  Q19_5 = Q11_5, 
                  Q19_6 = Q11_6, 
                  Q19_7 = Q11_7, 
                  Q19_8 = Q11_8, 
                  Q19_9 = Q11_9)

###Rename Ecosystem Services Questions to be Consistent With Phase 2

Combined<- rename(Combined, 
                  Q20_1 = Q12_1, 
                  Q20_2 = Q12_2, 
                  Q20_3 = Q12_3, 
                  Q20_4 = Q12_4, 
                  Q20_5 = Q12_5, 
                  Q20_6 = Q12_6, 
                  Q20_7 = Q12_7, 
                  Q20_8 = Q12_8, 
                  Q20_9 = Q12_9,
                  Q20_10 = Q12_10)


###Insert a new column for the "I have enough time" barriers prompt used in Phase II 
insert_after <- which(colnames(Combined) == "Q13_1")
Combined$QBarriers_2 <- NA
new_cols <- c("QBarriers_2")
remaining <- setdiff(colnames(Combined), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Combined<- Combined[, new_order]

###Rename Barriers Questions
Combined<- rename(Combined, 
                  QBarriers_1 = Q13_1, 
                  QBarriers_3 = Q13_2, 
                  QBarriers_4 = Q13_3, 
                  QBarriers_5 = Q13_4, 
                  QBarriers_6 = Q13_5, 
                  QBarriers_7 = Q13_6, 
                  QBarriers_8 = Q13_7, 
                  QBarriers_9 = Q13_8, 
                  QBarriers_10 = Q13_9)


###Rename Microagression Questions
Combined<- rename(Combined, 
                  QMicroA_1 = Q14_1, 
                  QMicroA_2 = Q14_2, 
                  QMicroA_3 = Q14_3, 
                  QMicroA_4 = Q14_4) 


###Rename Climate Observation Questions and Establish Social and Environmental Identifiers, Consistent with Phase II
Combined<- rename(Combined, 
                  QClimate_Obs_S_1 = Q15_1, 
                  QClimate_Obs_S_2 = Q15_2,
                  QClimate_Obs_S_3 = Q15_3, 
                  QClimate_Obs_S_4 = Q15_4, 
                  QClimate_Obs_E_1 = Q15_5, 
                  QClimate_Obs_E_2 = Q15_6, 
                  QClimate_Obs_E_3 = Q15_7, 
                  QClimate_Obs_E_4 = Q15_8, 
                  QClimate_Obs_E_5 = Q15_9, 
                  QClimate_Obs_E_6 = Q15_10,
                  QClimate_Obs_E_7 = Q15_11)

###Rename Climate Concern Questions and Establish Social and Environmental Identifiers, Consistent with Phase II

Combined<- rename(Combined, 
                  QClimate_Conc_S_1 = Q16_1, 
                  QClimate_Conc_S_2 = Q16_2,
                  QClimate_Conc_S_3 = Q16_3, 
                  QClimate_Conc_S_4 = Q16_4, 
                  QClimate_Conc_E_1 = Q16_5, 
                  QClimate_Conc_E_2 = Q16_6, 
                  QClimate_Conc_E_3 = Q16_7, 
                  QClimate_Conc_E_4 = Q16_8, 
                  QClimate_Conc_E_5 = Q16_9, 
                  QClimate_Conc_E_6 = Q16_10,
                  QClimate_Conc_E_7 = Q16_11)

###Relumps Social & Environmental Questions, Consistent with Phase II Structure
Combined <- Combined %>%
  relocate(
    starts_with("QClimate_Obs_S"),
    starts_with("QClimate_Conc_S"),
    starts_with("QClimate_Obs_E"),
    starts_with("QClimate_Conc_E"),
   .after = QMicroA_4) 

###Rename MPA & Sanctuary Awareness Questions
Combined <- rename(Combined, 
                    QMPA_Aware = Q17, 
                    QSanct_Aware = Q18)


##Add in BlanK Columns for MPA Science Awareness and Purpose Awareness Questions added to Phase 2
insert_after <- which(colnames(Combined) == "QSanct_Aware")
Combined$QMPA_Purpose <- NA
Combined$QMPA_Science <- NA
new_cols <- c("QMPA_Purpose", "QMPA_Science")
remaining <- setdiff(colnames(Combined), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Combined<- Combined[, new_order]


###Add in Blank Columns for MPA Visitation and Evaluation Questions
insert_after <- which(colnames(Combined) == "QMPA_Science")
Combined$QMPA_Visit <- NA
Combined$QMPA_Eval1 <- NA
Combined$QMPA_Eval2 <- NA
Combined$QMPA_Eval3 <- NA
Combined$QMPA_Eval4 <- NA
new_cols <- c("QMPA_Visit", "QMPA_Eval1", "QMPA_Eval2", "QMPA_Eval3", "QMPA_Eval4")
remaining <- setdiff(colnames(Combined), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Combined<- Combined[, new_order]

###ADD in Blanks for New Time Tracker Data
insert_after <- which(colnames(Combined) == "QMPA_Eval4")
Combined$Time.Tracker_First.Click.1 <- NA
Combined$Time.Tracker_Last.Click.1 <- NA
Combined$Time.Tracker_Page.Submit.1 <- NA
Combined$Time.Tracker_Click.Count.1 <- NA
new_cols <- c("Time.Tracker_First.Click.1", "Time.Tracker_Last.Click.1", "Time.Tracker_Page.Submit.1", "Time.Tracker_Click.Count.1")
remaining <- setdiff(colnames(Combined), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Combined<- Combined[, new_order]


###ADD in Blanks for New Priority Time Tracker Data
insert_after <- which(colnames(Combined) == "QPriority_10")
Combined$Ranking_Timer_First.Click <- NA
Combined$Ranking_Timer_Last.Click <- NA
Combined$Ranking_Timer_Page.Submit <- NA
Combined$Ranking_Timer_Click.Count <- NA
new_cols <- c("Ranking_Timer_First.Click", "Ranking_Timer_Last.Click", "Ranking_Timer_Page.Submit", "Ranking_Timer_Click.Count")
remaining <- setdiff(colnames(Combined), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Combined<- Combined[, new_order]

##Rename Climate Attitudes & Beliefs Questions
Combined<- rename(Combined, 
                  QClimate_Belief = Q20, 
                  QClimate_Attitude1 = Q20a_1,
                  QClimate_Attitude2 = Q20a_2, 
                  QClimate_Attitude3 = Q20a_3, 
                  QClimate_Attitude4 = Q20a_4, 
                  QClimate_Attitude5 = Q20a_5)

###Rename Futures Questions
Combined <- rename(Combined,
                   QFutures1 = Q21_1,
                   QFutures2 = Q21_2, 
                   QFutures3 = Q21_3, 
                   QFutures4 = Q21_4)

###ADD in Blanks For Phase 2 Demographic Home Question
insert_after <- which(colnames(Combined) == "QFutures4")
Combined$QDemographic_Home<-NA
new_cols <- c("QDemographic_Home")
remaining <- setdiff(colnames(Combined), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Combined<- Combined[, new_order]


###Reformat & Name Demographic Data so it is Consistent with Phase II Structure

Phase1_Demographic_Data<- rename(Phase1_Demographic_Data, 
                   QDemographic_PrimaryZip = Q22, 
                   QDemographic_Country_State = Q23, 
                   QDemographic_CA_Years = Q23_1_TEXT,
                   QDemographic_Race= Q24,
                   QDemographic_Asian= Q24a,
                   QDemographic_Tribal=Q24b,
                   QDemographic_Tribal_TEXT=Q24b1_1_TEXT,
                   QDemographic_Gender=Q25,
                   QDemographic_Birth=Q26,
                   QDemographic_Income=Q27,
                   QDemographic_Education=Q28,
                   QDemographic_Family=Q29,
                   QDemographic_FamilySize=Q30,
                   QDemographic_Swimming=Q31)

Phase1_Demographic_Data<-Phase1_Demographic_Data %>% 
  select(-Q24b1)

insert_after <- which(colnames(Phase1_Demographic_Data) == "QDemographic_Race")
Phase1_Demographic_Data$QDemographic_Race_TEXT<-NA
new_cols <- c("QDemographic_Race_TEXT")
remaining <- setdiff(colnames(Phase1_Demographic_Data), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Phase1_Demographic_Data<- Phase1_Demographic_Data[, new_order]

insert_after <- which(colnames(Phase1_Demographic_Data) == "QDemographic_Asian")
Phase1_Demographic_Data$QDemographic_Asian_TEXT<-NA
new_cols <- c("QDemographic_Asian_TEXT")
remaining <- setdiff(colnames(Phase1_Demographic_Data), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Phase1_Demographic_Data<- Phase1_Demographic_Data[, new_order]

###ADD DEMOGraphic DATA BACK IN
Combined <- bind_cols(Combined, Phase1_Demographic_Data)
Combined <- Combined %>%
  relocate(any_of(names(Phase1_Demographic_Data)), .after = "QDemographic_Home")

Combined<- rename(Combined, 
                  Comments = Q32) 

Combined$Prize_Region<-NA
Combined$Prize_Ferry<-NA

Combined<- Combined %>%
  relocate(Email, Version, Mechanism, Prize, .before = Prize)

###Output Phase 1 Data with column names cleaned and reordered

write.csv(Combined, "Output_Data/P1_Restructured.csv", row.names=FALSE)



