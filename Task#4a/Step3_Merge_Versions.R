

rm(list = ls(all = TRUE))



library(dplyr)

###Choose the directory on your machine where the data is stored
setwd("C:/Users/tfrawley/Desktop/Merging_Surveys/")


###Load in and aggregate output files from Step 2
###P1, Column names cleaned and restructured
P1<-read.csv("Output_Data/P1_Restructured.csv")
###Remove redundant column header info
P1 <- P1[-(1:2), ]
###P2, Column names cleaned and restructured
P2<-read.csv("Output_Data/P2_Restructured.csv")

##Combine P1 and P2
Complete<-rbind(P2, P1)


###Do some additional renaming to make column names more intuitive
Complete<- rename(Complete, 
                  QRepeat = Q0, 
                  QDesired_Time = Q1, 
                  QActual_Time = Q2, 
                  QUse_Area = Q3, 
                  QUse_Area_TEXT = Q3_4_TEXT_P1)

Complete<- rename(Complete, 
                  QE_TimeSpent = Q15, 
                  QE_TimeSpent_TEXT = Q15_2_TEXT, 
                  QE_GroupSize = Q16)

Complete<- rename(Complete, 
                   QWB_1 = Q19_1, 
                   QWB_2 = Q19_2, 
                   QWB_3 = Q19_3, 
                   QWB_4 = Q19_4, 
                   QWB_5 = Q19_5, 
                   QWB_6 = Q19_6, 
                   QWB_7 = Q19_7, 
                   QWB_8 = Q19_8,
                   QWB_9 = Q19_9)

Complete<- rename(Complete, 
                  QES_1 = Q20_1, 
                  QES_2 = Q20_2, 
                  QES_3 = Q20_3, 
                  QES_4 = Q20_4, 
                  QES_5 = Q20_5, 
                  QES_6 = Q20_6, 
                  QES_7 = Q20_7, 
                  QES_8 = Q20_8,
                  QES_9 = Q20_9,
                  QES_10 = Q20_10)


###OPTIONAL IF YOU WANT TO CONDENSE & RENAME MAPPING COLUMNS

Complete<- Complete %>%
  # 1. Clean whitespace and convert empty strings to NA so coalesce works correctly
  mutate(across(matches("^Q3b\\d+_1_[xy]$"), ~ na_if(trimws(.), ""))) %>%
  # 2. Coalesce all X columns into one, and all Y columns into one
  mutate(
    Final_X = coalesce(
      Q3b1_1_x, Q3b2_1_x, Q3b3_1_x, Q3b4_1_x, Q3b5_1_x, Q3b6_1_x, 
      Q3b7_1_x, Q3b8_1_x, Q3b9_1_x, Q3b10_1_x, Q3b11_1_x, Q3b12_1_x, 
      Q3b13_1_x, Q3b14_1_x, Q3b15_1_x, Q3b16_1_x, Q3b18_1_x),
    Final_Y = coalesce(
      Q3b1_1_y, Q3b2_1_y, Q3b3_1_y, Q3b4_1_y, Q3b5_1_y, Q3b6_1_y, 
      Q3b7_1_y, Q3b8_1_y, Q3b9_1_y, Q3b10_1_y, Q3b11_1_y, Q3b12_1_y, 
      Q3b13_1_y, Q3b14_1_y, Q3b15_1_y, Q3b16_1_y, Q3b18_1_y) ) %>% 
  select(-matches("^Q3b\\d+_1_[xy]$"))    

insert_after <- which(colnames(Complete) == "QMapping_South_County")
new_cols <- c("Final_X", "Final_Y")
remaining <- setdiff(colnames(Complete), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Complete<- Complete[, new_order]



###OPTIONAL IF YOU WANT TO CONDENSE & RENAME ECONOMICS COLUMNS

###Start with renaming the questions asked only of overnight vistors
Complete<- rename(Complete, 
                  QE_Overnight_Lodging = Q17b_12_1,
                  QE_Overnight_Camping = Q17b_13_1,
                  QE_Overnight_Transportation =Q17b_14_1) 


##Collapse Questions asked to Both Overnight & Day Vistors
Complete <- Complete %>%
  # 1. Clean whitespace and convert empty strings to NA so coalesce works correctly
  mutate(across(matches("^Q17[ab]_\\d+_1$"), ~ na_if(trimws(.), ""))) %>%
  # 2. Coalesce the 'a' and 'b' versions into a single unified column
  mutate(
    Q17_1_1 = coalesce(Q17a_1_1, Q17b_1_1),
    Q17_2_1 = coalesce(Q17a_2_1, Q17b_2_1),
    Q17_3_1 = coalesce(Q17a_3_1, Q17b_3_1),
    Q17_4_1 = coalesce(Q17a_4_1, Q17b_4_1),
    Q17_5_1 = coalesce(Q17a_5_1, Q17b_5_1),
    Q17_6_1 = coalesce(Q17a_6_1, Q17b_6_1),
    Q17_7_1 = coalesce(Q17a_7_1, Q17b_7_1),
    Q17_8_1 = coalesce(Q17a_8_1, Q17b_8_1),
    Q17_9_1 = coalesce(Q17a_9_1, Q17b_9_1),
    Q17_10_1 = coalesce(Q17a_10_1, Q17b_10_1),
    Q17_11_1 = coalesce(Q17a_11_1, Q17b_11_1)
  ) %>%
  
  # 3. Drop all original 'a' and 'b' columns
  select(-matches("^Q17[ab]_\\d+_1$"))


insert_after <- which(colnames(Complete) == "QE_GroupSize")
new_cols <- c("Q17_1_1", "Q17_2_1", "Q17_3_1", "Q17_4_1", "Q17_5_1", 
              "Q17_6_1", "Q17_7_1", "Q17_8_1", "Q17_9_1", "Q17_10_1", "Q17_11_1")
remaining <- setdiff(colnames(Complete), new_cols)
new_order <- c(remaining[1:insert_after], new_cols, remaining[(insert_after+1):length(remaining)])
Complete <- Complete[, new_order]

###Rename Questions Asked to Overnight & Day Visitors
Complete<- rename(Complete, 
                 QE_Activities = Q17_1_1,
                 QE_Entry = Q17_2_1,
                 QE_Parking =Q17_3_1, 
                 QE_PublicT= Q17_4_1,
                 QE_CarRental=  Q17_5_1, 
                 QE_Gas =Q17_6_1,
                 QE_Restuarant= Q17_7_1, 
                 QE_Groccery  =Q17_8_1, 
                 QE_Rental =Q17_9_1, 
                 QE_Gear =Q17_10_1, 
                 QE_Other= Q17_11_1) 


write.csv(Complete, "Output_Data/Merged_Versions.csv", row.names=FALSE)
