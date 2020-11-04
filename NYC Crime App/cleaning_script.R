
# clear global environment
rm(list = ls(all.names = TRUE))


# libraries
library(naivebayes)
library(tidyverse)
library(lubridate)
library(rgdal)


# import data
nb_data <- read_csv("~/Documents/R/RProjects-Public/Collaborative-Projects-Data/NYPD_Complaint_Data_Historic.csv",
                    col_names = TRUE)
ytd_data <- read_csv("~/Documents/R/RProjects-Public/Collaborative-Projects-Data/NYPD_Complaint_Data_YTD.csv",
                     col_names = TRUE) %>% select(c(1:35))

# combine data into one large set
nb_data <- as.data.frame(rbind(nb_data,ytd_data))

my_spdf <- readOGR("~/Documents/R/RProjects-Public/Collaborative-Projects-Data/Police Precincts.geojson")

# take key columns for naive_bayes analysis: date (month, year), time, law_ct_code, borough,
nb_data <- nb_data %>% select(c(2,3,6,9,11:14,))

# quick string fix for 2015 - 2020
nb_data$CMPLNT_FR_DT <- str_replace(nb_data$CMPLNT_FR_DT,"/1015","/2015")
nb_data$CMPLNT_FR_DT <- str_replace(nb_data$CMPLNT_FR_DT,"/1016","/2016")
nb_data$CMPLNT_FR_DT <- str_replace(nb_data$CMPLNT_FR_DT,"/1017","/2017")
nb_data$CMPLNT_FR_DT <- str_replace(nb_data$CMPLNT_FR_DT,"/1018","/2018")
nb_data$CMPLNT_FR_DT <- str_replace(nb_data$CMPLNT_FR_DT,"/1019","/2019")
nb_data$CMPLNT_FR_DT <- str_replace(nb_data$CMPLNT_FR_DT,"/1020","/2020")


# cleaning & pre-processing
nb_data$CMPLNT_FR_DT <- as.Date(nb_data$CMPLNT_FR_DT, "%m/%d/%Y")
nb_data$CRM_ATPT_CPTD_CD <- as.factor(nb_data$CRM_ATPT_CPTD_CD)
nb_data$OFNS_DESC <- as.factor(nb_data$OFNS_DESC)
nb_data$PD_DESC <- as.factor(nb_data$PD_DESC)
nb_data$LAW_CAT_CD <- as.factor(nb_data$LAW_CAT_CD)
nb_data$BORO_NM <- as.factor(nb_data$BORO_NM)


# clean up strings
nb_data$OFNS_DESC <- str_replace(nb_data$OFNS_DESC, 
                                 "INTOXICATED/IMPAIRED DRIVING", "INTOXICATED & IMPAIRED DRIVING")

nb_data <- nb_data %>% mutate(Year = as.factor(year(nb_data$CMPLNT_FR_DT))) %>% 
  mutate(Month = as.factor(month(nb_data$CMPLNT_FR_DT, label = TRUE)))

timebin <- as.POSIXct(strptime(c("050000","105959","110000","165959","170000",
                        "235959", "240000", "045959"),"%H%M%S"),"UTC")

timetoday <- as.POSIXct(strptime(nb_data$CMPLNT_FR_TM,"%H:%M:%S"),"UTC")

nb_data$timenominal <- case_when(
  between(timetoday,timebin[1],timebin[2]) ~"Morning",
  between(timetoday,timebin[3],timebin[4]) ~"Afternoon",
  between(timetoday,timebin[5],timebin[6]) ~"Evening",
  is.na(timetoday) ~ "remove row",
  TRUE ~"Late Night")


colnames(nb_data) <- c("Date", "Time Numeric", "Precinct", "Offense Description", "PD Description", 
                       "Crime Status","Crime Category", "Borough", "Year", "Month", "Time")

# last minute conversions & cleaning
nb_data$Precinct <- as.factor(nb_data$Precinct)
nb_data$Time <- as.factor(nb_data$Time)

nb_data <- nb_data %>% filter(Time != "remove row") %>% # time not present in 48 records that are not valid
  filter(Precinct != "-99") %>% # Department of Corrections record = no valid precinct
  filter(!(is.na(Month))) # excludes 15 valid records that do not have a complnt fr date


# replace approx. 18,000 missing values in the offense description set by values in outside vector
# step 1: arrange values by pd description
nb_data <- nb_data %>% arrange(`PD Description`)

# step 2: create the hash map via two vectors
sample2 <- nb_data %>%
  group_by(`Offense Description`, `PD Description`) %>% 
  summarise(count = n()) %>% 
  arrange(`Offense Description`)

hash_map <- sample2[c(1:295,297:422),1:2] # used to recode the offense description NA values

# hash map recoding into new vector to double check values
vec <- nb_data$`PD Description`
vec_levels <- hash_map$`PD Description`
vec_labels <- hash_map$`Offense Description`

offdes2 <- vec_labels[match(vec,vec_levels)] # new vector to check against offense description vector

nb_data <- nb_data %>% 
  mutate(offdesc2 = offdes2) # adds new Offense Description vector to check against original

# replace the offense description vector with the new vector
nb_data <- nb_data %>% mutate(`Offense Description` = offdesc2) %>% select(-c("offdesc2")) %>% 
  droplevels()

# filter to appropriate time frame
nb_data <- nb_data %>% filter(Year %in% c("2016","2017","2018","2019","2020")) %>% droplevels()


################################# DATA CHECK AFTER PRE-PROCESSING ######################################

# quick view of data
view(head(nb_data, n = 20))
str(nb_data)

# saved data files for shiny app
saveRDS(nb_data[,3:11], file = "nbdata.rds") # main Data for app
saveRDS(my_spdf, file = "spdf.rds") # spatial data for app

# export the dataset
write_csv(nb_data,"~/Documents/R/RProjects-Public/Machine-Learning-Data/cleaned_nb_data.csv")

















