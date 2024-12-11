# Loading packages
library(readxl)
library(tidyverse)
library(data.table)
library(DT)
library(hrbrthemes)
library(ggthemes)
library(ggpubr)
pacman::p_load(
  rio,           # import/export
  tidyverse,     # data mgmt and viz
  naniar,        # assess and visualize missingness
  mice           # missing data imputation
)
library(arrow)

# Importing dataset----
dt_raw <- fread("C:/Users/loren/OneDrive/TNC/Activity_8_survey_app/TNC_fellowship/data/raw/wetland_survey_data_2.csv", header=T, na.strings=c("","NA"))
str(dt_raw) # Checking the structure
setDT(dt_raw) # convert to data.table

# Cleaning the dataset----
## Questions that TNC is interested: A2, B1, B4, B6, B8, C1
## Additional questions based on my interest: A7, A8, C2

names(dt_raw)

dt_long<-dt_raw %>%
  rename(Question_B1=contacted)%>%
  rename(Question_B4=prgm_interest)%>%
  select(-fam_land)%>%
  pivot_longer(
    cols = starts_with("cnt"),
    names_to = "Question_A2",
    values_to = "Answer_A2",
    values_drop_na = TRUE)%>%
  pivot_longer(
    cols = starts_with("fam"),
    names_to = "Question_B6",
    values_to = "Answer_B6",
    values_drop_na = TRUE)%>%
  pivot_longer(
    cols = starts_with("motiv"),
    names_to = "Question_B8",
    values_to = "Answer_B8",
    values_drop_na = TRUE)%>%
  pivot_longer(
    cols = starts_with("wtlnd"),
    names_to = "Question_C1",
    values_to = "Answer_C1",
    values_drop_na = TRUE)%>%
  dplyr::select(sid,
                Question_A2, Answer_A2,
                Question_B1, 
                Question_B4, 
                Question_B6, Answer_B6, 
                Question_B8, Answer_B8,
                Question_C1, Answer_C1)%>%
  dplyr::mutate(across(c(sid, Question_A2), as.factor))

# Recode questions and answers
dt_long<-dt_long%>%
  mutate(Question_B1 = recode(as.character(Question_B1),
                              '1' = "Never contacted",
                              '2' = "Once or twice",
                              '3' = "Three to four times",
                              '4' = "Four to five times",
                              '5' = "Five times or more",
                              '9999' = "Not answered")) %>%
  mutate(Question_B1 = factor(Question_B1, 
                              levels = c("Never contacted", 
                                         "Once or twice",
                                         "Three to four times",
                                         "Four to five times", 
                                         'Five times or more',
                                         "Not answered")))%>%
  mutate(Question_B4 = recode(as.character(Question_B4), 
                              '1' = "Not interested", 
                              '2' = "Potentially interested", 
                              '3' = "Very interested", 
                              '4' = "Already participating", 
                              '8888' = 'More than one response',
                              '9999' = "Not answered")) %>%
  mutate(Question_B4 = factor(Question_B4, 
                              levels = c("Not interested", 
                                         "Potentially interested",
                                         "Very interested",
                                         "Already participating", 
                                         'More than one response',
                                         "Not answered"))) %>%
  mutate(Question_B6 = recode(as.character(Question_B6),
                              'fam_eqip' = "EQIP",
                              'fam_csp' = "CSP",
                              'fam_crp' = "CRP",
                              'fam_wre' = "WRE",
                              'fam_crep' = "CREP",
                              'fam_macs' = "MACS",
                              'fam_nawca' = "NAWCA",
                              'fam_repi' = "REPI")) %>%
  mutate(Answer_B6 = recode(as.character(Answer_B6),
                              '1' = "Never heard",
                              '2' = "Heard of it, but not that familiar",
                              '3' = "Somewhat familiar",
                              '4' = "Very familiar",
                              '8888' = "More than one response",
                              '9999' = "Not answered")) %>%
  mutate(Answer_B6 = factor(Answer_B6, 
                              levels = c("Never heard", 
                                         "Heard of it, but not that familiar",
                                         "Somewhat familiar",
                                         "Very familiar", 
                                         'More than one response',
                                         "Not answered")))%>%
  mutate(Question_B8 = recode(as.character(Question_B8),
                              'motiv_attract' = "Attractiveness",
                              'motiv_hunt' = "Improved hunting",
                              'motiv_money' = "Money for practice",
                              'motiv_remove' = "Removing marginal cropland",
                              'motiv_wildlife' = "Wildlife",
                              'motiv_wquality' = "Improved water quality")) %>%
  mutate(Answer_B8 = recode(as.character(Answer_B8), 
                            '1' = "Not at all", 
                            '2' = "Somewhat motivating", 
                            '3' = "Very motivating",
                            '9998' = "Don't know",
                            '8888' = "More than one response",
                            '9999' = "Not answered")) %>%
  mutate(Answer_B8 = factor(Answer_B8, 
                            levels = c("Not at all", 
                                       "Somewhat motivating",
                                       "Very motivating",
                                       "Don't know",
                                       "More than one response",
                                       "Not answered")))%>%
  mutate(Question_C1 = recode(as.character(Question_C1),
                              'wtlnd_beau' = "are beautiful",
                              'wtlnd_pest' = "attract pests/mosquitos",
                              'wtlnd_wild' = "help protect wildlife",
                              'wtlnd_qual' = "help protect water quality",
                              'wtlnd_flood' = "help prevent/reduce flooding",
                              'wtlnd_prop' = "hurt property values",
                              'wtlnd_hunt' = "are good for hunting",
                              'wtlnd_loss' = "prevent land loss to erosion/subsidence")) %>%
  mutate(Answer_C1 = recode(as.character(Answer_C1), 
                            '1' = "Strongly disagree", 
                            '2' = "Somewhat disagree", 
                            '3' = "Somewhat agree",
                            '4' = "Strongly agree",
                            '9998' = "Don't know",
                            '8888' = "More than one response",
                            '9999' = "Not answered")) %>%
  mutate(Answer_C1 = factor(Answer_C1, 
                            levels = c("Strongly disagree", 
                                       "Somewhat disagree",
                                       "Somewhat agree",
                                       "Strongly agree",
                                       "Don't know",
                                       "More than one response",
                                       "Not answered")))%>%
  mutate(across(c(Answer_A2, Question_A2), as.factor))
  

names(dt_long)
str(dt_long)

# creating a county category
dt_long <- dt_long %>%
  dplyr::mutate(
    County = case_when(
      Answer_A2 == 1 & Question_A2 == "cnt_car" ~ "Caroline",
      Answer_A2 == 1 & Question_A2 == "cnt_dor" ~ "Dorchester",
      Answer_A2 == 1 & Question_A2 == "cnt_kentmd" ~ "Kent",
      Answer_A2 == 1 & Question_A2 == "cnt_que" ~ "Queen Anne’s",
      Answer_A2 == 1 & Question_A2 == "cnt_som" ~ "Somerset",
      Answer_A2 == 1 & Question_A2 == "cnt_talb" ~ "Talbot",
      Answer_A2 == 1 & Question_A2 == "cnt_wic" ~ "Wicomico",
      Answer_A2 == 1 & Question_A2 == "cnt_wor" ~ "Worcester",
      Answer_A2 == 1 & Question_A2 == "cnt_new" ~ "New Castle",
      Answer_A2 == 1 & Question_A2 == "cnt_kentde" ~ "Kent (DE)",
      Answer_A2 == 1 & Question_A2 == "cnt_suss" ~ "Sussex counties (DE)",
      Answer_A2 == 1 & Question_A2 == "cnt_north" ~ "Northampton (VA)",
      Answer_A2 == 1 & Question_A2 == "cnt_acc" ~ "Accomack (VA)",
      TRUE ~ "Not answered"  # Catch all other counties
    ))

# Creating a state category
dt_long <- dt_long %>%
  dplyr::mutate(
    State = case_when(
      County == "Caroline" ~ "MD",
      County == "Dorchester" ~ "MD",
      County == "Kent" ~ "MD",
      County == "Queen Anne’s" ~ "MD",
      County == "Somerset" ~ "MD",
      County == "Talbot" ~ "MD",
      County == "Wicomico" ~ "MD",
      County == "Worcester" ~ "MD",
      County == "New Castle" ~ "MD",
      County == "Kent (DE)" ~ "DE",
      County == "Sussex counties (DE)" ~ "DE",
      County == "Northampton (VA)" ~ "VA",
      County == "Accomack (VA)" ~ "VA",
      TRUE ~ "Not answered"  # Catch all other counties
    ))

levels(as.factor(dt_long$County))
levels(as.factor(dt_long$State))

dt_long <- dt_long %>%
  select(sid, County, State, Question_B1, Question_B4, 
         Question_B6, Answer_B6, Question_B8, Answer_B8, 
         Question_C1, Answer_C1)%>%
  mutate(across(c(sid, 
                  Question_B1, 
                  Question_B4, 
                  Question_B6, Answer_B6, 
                  Question_B8, Answer_B8, 
                  Question_C1, Answer_C1, 
                  County, State), as.factor))

# Saving file----
write.csv(x = dt_long, 
          file = paste("./data/derived_products", "dt_long.csv", sep = "/"),
          row.names = FALSE)

# Save to Feather format
write_feather(dt_long, "./data/derived_products/dt_long.feather")

# Read the Feather file
# dt_long <- read_feather("dt_long.feather")


