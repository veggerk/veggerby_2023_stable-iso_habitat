
# packages needed
library(here)
library(tidyverse)

# bring in all data sheets

# file name 
set1 <- "set1.csv"
set2 <- "set2.csv"
set3 <- "set3.csv"
set4 <- "set4.csv"
set5 <- "set5.csv"
set6 <- "set6.csv"
set7 <- "set7.csv"
set8 <- "set8.csv"
set9 <- "set9.csv"
set10 <- "set10.csv"
set11 <- "set11.csv"
set12 <- "set12.csv"
set13 <- "set13.csv"
set14 <- "set14.csv"
set15 <- "set15.csv"
set16 <- "set16.csv"
set17 <- "set17.csv"
set18 <- "set18.csv"
set19 <- "set19.csv"
set20 <- "set20.csv"
set21 <- "set21.csv"
set22 <- "set22.csv"
set23 <- "set23.csv"
set24 <- "set24.csv"
set25 <- "set25.csv"
set26 <- "set26.csv"
set27 <- "set27.csv"
set28 <- "set28.csv"
set29 <- "set29.csv"
set30 <- "set30.csv"
set31 <- "set31.csv"
set32 <- "set32.csv"
set33 <- "set33.csv"
set34 <- "set34.csv"
set35 <- "set35.csv"
set36 <- "set36.csv"
set37 <- "set37.csv"
set38 <- "set38.csv"


sample_data<- "sample_collection.csv"
consortium_metadata<- "consortium_metadata.csv"

# file locations
file_loc_sets1<- here("data/03_raw data", set1)
file_loc_sets2 <- here("data/03_raw data", set2)
file_loc_sets3 <- here("data/03_raw data", set3)
file_loc_sets4 <- here("data/03_raw data", set4)
file_loc_sets5 <- here("data/03_raw data", set5)
file_loc_sets6 <- here("data/03_raw data", set6)
file_loc_sets7 <- here("data/03_raw data", set7)
file_loc_sets8 <- here("data/03_raw data", set8)
file_loc_sets9 <- here("data/03_raw data", set9)
file_loc_sets10 <- here("data/03_raw data", set10)
file_loc_sets11 <- here("data/03_raw data", set11)
file_loc_sets12 <- here("data/03_raw data", set12)
file_loc_sets13 <- here("data/03_raw data", set13)
file_loc_sets14 <- here("data/03_raw data", set14)
file_loc_sets15 <- here("data/03_raw data", set15)
file_loc_sets16 <- here("data/03_raw data", set16)
file_loc_sets17 <- here("data/03_raw data", set17)
file_loc_sets18 <- here("data/03_raw data", set18)
file_loc_sets19 <- here("data/03_raw data", set19)
file_loc_sets20 <- here("data/03_raw data", set20)
file_loc_sets21 <- here("data/03_raw data", set21)
file_loc_sets22 <- here("data/03_raw data", set22)
file_loc_sets23 <- here("data/03_raw data", set23)
file_loc_sets24 <- here("data/03_raw data", set24)
file_loc_sets25 <- here("data/03_raw data", set25)
file_loc_sets26 <- here("data/03_raw data", set26)
file_loc_sets27 <- here("data/03_raw data", set27)
file_loc_sets28 <- here("data/03_raw data", set28)
file_loc_sets29 <- here("data/03_raw data", set29)
file_loc_sets30 <- here("data/03_raw data", set30)
file_loc_sets31 <- here("data/03_raw data", set31)
file_loc_sets32 <- here("data/03_raw data", set32)
file_loc_sets33 <- here("data/03_raw data", set33)
file_loc_sets34 <- here("data/03_raw data", set34)
file_loc_sets35 <- here("data/03_raw data", set35)
file_loc_sets36 <- here("data/03_raw data", set36)
file_loc_sets37 <- here("data/03_raw data", set37)
file_loc_sets38 <- here("data/03_raw data", set38)


file_loc_samp_dat <- here("data/03_raw data", sample_data)
file_loc_con_data <- here("data/03_raw data", consortium_metadata)

# import files
set1 <- read_csv(file_loc_sets1,
                             na = c("", "NA"))

set2 <- read_csv(file_loc_sets2,
                 na = c("", "NA"))

set3 <- read_csv(file_loc_sets3,
                 na = c("", "NA"))

set4 <- read_csv(file_loc_sets4,
                 na = c("", "NA"))

set5 <- read_csv(file_loc_sets5,
                 na = c("", "NA"))

set6 <- read_csv(file_loc_sets6,
                 na = c("", "NA"))

set7 <- read_csv(file_loc_sets7,
                 na = c("", "NA"))

set8 <- read_csv(file_loc_sets8,
                 na = c("", "NA"))

set9 <- read_csv(file_loc_sets9,
                 na = c("", "NA"))

set10 <- read_csv(file_loc_sets10,
                 na = c("", "NA"))

set11 <- read_csv(file_loc_sets11,
                 na = c("", "NA"))

set12 <- read_csv(file_loc_sets12,
                 na = c("", "NA"))

set13 <- read_csv(file_loc_sets13,
                 na = c("", "NA"))

set14 <- read_csv(file_loc_sets14,
                  na = c("", "NA"))

set15 <- read_csv(file_loc_sets15,
                  na = c("", "NA"))

set16 <- read_csv(file_loc_sets16,
                  na = c("", "NA"))

set17 <- read_csv(file_loc_sets17,
                  na = c("", "NA"))

set18 <- read_csv(file_loc_sets18,
                  na = c("", "NA"))

set19 <- read_csv(file_loc_sets19,
                  na = c("", "NA"))

set20 <- read_csv(file_loc_sets20,
                  na = c("", "NA"))

set21 <- read_csv(file_loc_sets21,
                  na = c("", "NA"))

set22 <- read_csv(file_loc_sets22,
                  na = c("", "NA"))

set23 <- read_csv(file_loc_sets23,
                  na = c("", "NA"))

set24 <- read_csv(file_loc_sets24,
                  na = c("", "NA"))

set25 <- read_csv(file_loc_sets25,
                  na = c("", "NA"))

set26 <- read_csv(file_loc_sets26,
                  na = c("", "NA"))

set27 <- read_csv(file_loc_sets27,
                  na = c("", "NA"))

set28 <- read_csv(file_loc_sets28,
                  na = c("", "NA"))

set29 <- read_csv(file_loc_sets29,
                  na = c("", "NA"))

set30 <- read_csv(file_loc_sets30,
                  na = c("", "NA"))

set31 <- read_csv(file_loc_sets31,
                na = c("", "NA"))

set32 <- read_csv(file_loc_sets32,
                  na = c("", "NA"))

set33 <- read_csv(file_loc_sets33,
                  na = c("", "NA"))

set34 <- read_csv(file_loc_sets34,
                  na = c("", "NA"))

set35 <- read_csv(file_loc_sets35,
                  na = c("", "NA"))

set36 <- read_csv(file_loc_sets36,
                  na = c("", "NA"))

set37 <- read_csv(file_loc_sets37,
                  na = c("", "NA"))

set38 <- read_csv(file_loc_sets38,
                  na = c("", "NA"))


# bring in metadata to join to SI data

sample_data <- read_csv(file_loc_samp_dat,
                  na = c("", "NA"))

consortium_metadata <- read_csv(file_loc_con_data,
                        na = c("", "NA"))

# join all the result files together
master_data<-rbind(set1,set2,set3,set4,set5,set6,set7,set8,set9,set10,
                   set11,set12,set13,set14,set15,set16,set17,set18,set19,
                   set20,set21,set22,set23,set24,set25,set26,set27,set28,
                   set29,set30,set31,set32,set33,set34,set35,set36,set37,
                   set38)



# remove unneeded  columns
master_data<-master_data[ ,c(-1,-3,-6,-7,-8,-9)]



# join ggroups together
cleaned_up_data<-inner_join(master_data,sample_data,by = "sampleID")

consortium_data<-inner_join(consortium_metadata,cleaned_up_data,by = "sampleID")


# remove empty columns, order by species
cleaned_up_data <- cleaned_up_data %>%
  arrange(sample_category) %>%
  select(sampleID,d15N,d13C,sampling_id,location,year,habitat_type,sample_category,length_mm,notes)



# fix names
cleaned_up_data$sample_category[cleaned_up_data$sample_category=="amphipod"]<-"amphipods"

cleaned_up_data$sample_category[cleaned_up_data$sample_category=="dungeness claw"]<-"dungeness crab"

cleaned_up_data$sample_category[cleaned_up_data$sample_category=="graceful crab claw"]<-"graceful crab"

cleaned_up_data$sample_category[cleaned_up_data$sample_category=="mud snail"]<-"Japanese mud snail"

cleaned_up_data$sample_category[cleaned_up_data$sample_category=="marina"]<-"marina eelgrass"

cleaned_up_data$sample_category[cleaned_up_data$sample_category=="Japonica"]<-"japonica eelgrass"


# make dataframes for each consumer
dungeness_crab<-filter(cleaned_up_data,sample_category == "dungeness crab")
dungeness_crab<-filter(dungeness_crab, location != "Padilla Bay Reserve")
dungeness_crab<-filter(dungeness_crab,length_mm>=31)

dungeness_crab_padilla<-filter(cleaned_up_data,sample_category == "dungeness crab")
dungeness_crab_padilla<-filter(dungeness_crab_padilla, location == "Padilla Bay Reserve")
dungeness_crab_padilla<-filter(dungeness_crab_padilla,length_mm>=31)

shore_crab<-filter(cleaned_up_data,sample_category == "shore crab")
shore_crab<-filter(shore_crab, location != "Padilla Bay Reserve")

stickleback<-filter(cleaned_up_data,sample_category == "stickleback")
stickleback<-filter(stickleback, location != "Padilla Bay Reserve")
stickleback<-filter(stickleback,length_mm>=31)

stickleback_padilla<-filter(cleaned_up_data,sample_category == "stickleback")
stickleback_padilla<-filter(stickleback_padilla, location == "Padilla Bay Reserve")
stickleback_padilla<-filter(stickleback_padilla,length_mm>=31)

shiner_perch<-filter(cleaned_up_data,sample_category == "shiner perch")
shiner_perch<-filter(shiner_perch, location != "Padilla Bay Reserve")

shiner_perch_padilla<-filter(cleaned_up_data,sample_category == "shiner perch")
shiner_perch_padilla<-filter(shiner_perch_padilla, location == "Padilla Bay Reserve")

staghorn_sculpin<-filter(cleaned_up_data,sample_category == "staghorn sculpin")
staghorn_sculpin<-filter(staghorn_sculpin, location != "Padilla Bay Reserve")

flatfish<-filter(cleaned_up_data,sample_category == "English sole"|
                   sample_category == "starry flounder")
flatfish<-filter(flatfish, location != "Padilla Bay Reserve")

# save all the organized data files to the clean data folder.
# These will then be put into my main clean data folder for analysis
clean_file_name_data <- "SI_data_for_analysis.csv"
clean_data_loc <- here("data/04_clean data", clean_file_name_data)
cleaned_up_data %>% write_csv(clean_data_loc)

clean_file_name_data <- "dungeness_crab.csv"
clean_data_loc <- here("data/04_clean data", clean_file_name_data)
dungeness_crab %>% write_csv(clean_data_loc)

clean_file_name_data <- "dungeness_crab_padilla.csv"
clean_data_loc <- here("data/04_clean data", clean_file_name_data)
dungeness_crab_padilla %>% write_csv(clean_data_loc)

clean_file_name_data <- "shore_crab.csv"
clean_data_loc <- here("data/04_clean data", clean_file_name_data)
shore_crab %>% write_csv(clean_data_loc)

clean_file_name_data <- "stickleback.csv"
clean_data_loc <- here("data/04_clean data", clean_file_name_data)
stickleback %>% write_csv(clean_data_loc)

clean_file_name_data <- "stickleback_padilla.csv"
clean_data_loc <- here("data/04_clean data", clean_file_name_data)
stickleback_padilla %>% write_csv(clean_data_loc)

clean_file_name_data <- "shiner_perch.csv"
clean_data_loc <- here("data/04_clean data", clean_file_name_data)
shiner_perch %>% write_csv(clean_data_loc)

clean_file_name_data <- "shiner_perch_padilla.csv"
clean_data_loc <- here("data/04_clean data", clean_file_name_data)
shiner_perch_padilla %>% write_csv(clean_data_loc)

clean_file_name_data <- "staghorn_sculpin.csv"
clean_data_loc <- here("data/04_clean data", clean_file_name_data)
staghorn_sculpin %>% write_csv(clean_data_loc)

clean_file_name_data <- "flatfish.csv"
clean_data_loc <- here("data/04_clean data", clean_file_name_data)
flatfish %>% write_csv(clean_data_loc)


clean_file_name_data <- "just_consortium_data.csv"
clean_data_loc <- here("data/04_clean data", clean_file_name_data)
consortium_data_with_no_iso %>% write_csv(clean_data_loc)

