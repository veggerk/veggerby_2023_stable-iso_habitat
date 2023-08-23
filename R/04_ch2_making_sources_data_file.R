
# required packages and data load
library(tidyverse)
library(here)

isodat<-"SI_data_for_analysis.csv"
isodat <- here("data/03_clean data", isodat)
isodat <- read_csv(isodat,
                   na = c("", "NA"))



# filter just the sources we want to use
main_sources<-filter(isodat,sample_category=="bubble snail"|
                       sample_category=="Japanese mud snail"|
                       sample_category=="oyster gut")

sources<-filter(main_sources,habitat_type=="flipbags"|
                       habitat_type=="eelgrass")


# select location, save a separate file just for padilla sources
padilla_sources<-filter(sources,location == "Padilla Bay Reserve")

sources<-filter(sources,location != "Padilla Bay Reserve")



# add new column for the three source categories
sources<-sources %>% 
mutate(Sources = case_when(habitat_type=="flipbags" & sample_category=="bubble snail" ~ "farm habitat",
                 habitat_type=="flipbags" & sample_category=="Japanese mud snail" ~ "farm habitat",
                 habitat_type=="flipbags" & sample_category=="oyster gut" ~ "pelagic",
                 habitat_type=="eelgrass" & sample_category=="bubble snail" ~ "eelgrass habitat",
                 habitat_type=="eelgrass" & sample_category=="Japanese mud snail" ~ "eelgrass habitat",
                 habitat_type=="eelgrass" & sample_category=="oyster gut" ~ "pelagic"))%>%
  arrange(Sources)%>%
  select(Sources, d15N, d13C)



# save source files in clean folder
clean_file_name_data <- "sources.csv"
clean_data_loc <- here("data/03_clean data", clean_file_name_data)
sources %>% write_csv(clean_data_loc)



# add new column for the three source categories in padilla
padilla_sources<-padilla_sources %>% 
  mutate(padilla_sources = case_when(sample_category=="bubble snail" ~ "eelgrass habitat",
                             sample_category=="Japanese mud snail" ~ "eelgrass habitat",
                             sample_category=="oyster gut" ~ "pelagic"))%>%
  arrange(padilla_sources)%>%
  select(padilla_sources, d15N, d13C)



# save padilla source files in clean folder
clean_file_name_data <- "sources_padilla.csv"
clean_data_loc <- here("data/03_clean data", clean_file_name_data)
padilla_sources %>% write_csv(clean_data_loc)
