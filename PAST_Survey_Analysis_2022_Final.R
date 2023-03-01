# 2021 AAA/ANCATL Student Survey
# This is the R code for analysis of survey responses from the 2021 AAA/ANCATL 
# survey into Australian archaeology undergraduate student motivations.

#To run this analysis, the following packages and datafiles are required:
library(readxl)
library(plyr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(wesanderson)

#- an xls file called "ANCATL_Survey_Data_2022" that includes responses to the survey questions
#survey_data <- read_excel("ANCATL_Student_Survey_Data_2021.xlsx")
View(survey_data)

##Demographic composition of respondents
demo.data <- as_tibble(survey_data) %>% 
  dplyr::select("Age", "Gender", "Ethnicity") %>%
  mutate(Age = replace(Age, Age == 999, NA),
         Ethnicity = replace(Ethnicity, Ethnicity == 999, NA),
         age_group = case_when(Age < 20 ~ "19 and under",
                               Age >= 20 & Age < 30 ~ "20 to 29",
                               Age >= 30 & Age < 40 ~ "30 to 39",
                               Age >= 40 & Age < 50 ~ "40 to 49",
                               Age >= 50 & Age < 60 ~ "50 to 59",
                               Age >= 60 & Age < 70 ~ "60 to 69",
                               Age >= 70 & Age < 79 ~ "70 to 79",
                               Age >= 80 ~ "80 and over")) %>%  
  drop_na(Age)

#summary data for age and gender data
a.g.table <-  desc_statby(demo.data, measure.var = "Age",
                      grps = "Gender") %>%
  dplyr::rename(n = length,
                "median age (years)" = median,
                "mean age (years)" = mean) %>%
  mutate(across(where(is.numeric), round, 2))

gender.pc <- tabyl(demo.data, "Gender") %>%
  adorn_pct_formatting(digits = 2)

a.g.tab <- full_join(a.g.table, gender.pc)
a.g.tab <- a.g.tab[, c("Gender", "n", "percent", "median age (years)", "mean age (years)", "sd")]

a.g.tab.p <- ggtexttable(a.g.tab, rows = NULL, theme = ttheme("light")) %>%
  tab_add_footnote(text = "n = 106", size = 10, face = "italic")
a.g.tab.p

ggsave("age_tab.png", width = 10, height = 10)
ggsave("age_tab.tiff", width = 10, height = 10)

#Summary and visualisation of age and gender data
#Overall age distribution by gender - crosstabs
age <- tabyl(demo.data, "age_group") %>%
  adorn_pct_formatting(digits = 2) %>%
  dplyr::rename("Age group" = age_group) %>%
  select(-percent)
age.table <- ggtexttable(age, rows = NULL, theme = ttheme("light")) %>%
  tab_add_footnote(text = "n = 106", size = 10, face = "italic")
age.table

age.gender <- tabyl(demo.data, Gender, age_group)
age.gen.table <- ggtexttable(age.gender, rows = NULL, theme = ttheme("light")) %>%
  tab_add_footnote(text = "n = 103", size = 10, face = "italic")
age.gen.table

#Age by gender - summary statistics
age.resp <- demo.data %>%
  drop_na(Gender, Age) %>%
  group_by(Gender) %>%
  dplyr::summarise(grp.mean = mean(Age, na.rm = FALSE), grp.med = median(Age, na.rm = FALSE)) %>%
  ungroup()
age.resp$grp.mean <- round(age.resp$grp.mean, digits = 2)

#Reorder age_group classes to run in natural order along axis and plot age distribution by gender
age.plot <- demo.data %>% 
  drop_na(Gender, age_group) %>%
  filter(!Gender %in% c("Prefer not to say")) %>%
  ggplot(aes(x = age_group, y = ((..count..)/sum(..count..)), fill = Gender, colour = Gender)) +
  geom_bar(colour = "black", stat = "count", position = position_dodge2(width = 0.9, preserve = "single")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(name = "Age of student (years)", limits = c("19 and under",
                                                               "20 to 29",
                                                               "30 to 39",
                                                               "40 to 49",
                                                               "50 to 59",
                                                               "60 to 69",
                                                               "70 to 79")) +
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1")) +
  labs(title = "Age and gender profile of Level 1 Archaeology students") +
  theme_minimal(base_size = 20) +
  theme(axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(1,0.8),
        legend.justification = c(1,1),
        legend.background = element_blank(),
        legend.direction = "vertical",
        legend.title = element_blank(),
        plot.title = element_blank())
age.plot 

ggsave("age_groups.png", width = 10, height = 8)
ggsave("age_groups.tiff", width = 10, height = 8)

#Respondent ethnicity - summary and bar chart
eth.data <- demo.data %>%
  drop_na(Ethnicity) %>%
  separate_rows(Ethnicity, sep = ", ") %>%
  group_by(Ethnicity) %>%
  dplyr::summarize(n_students = n()) %>%
  mutate(percent = n_students / sum(n_students)*100) %>%
  ungroup()

eth.data$percent <- round(eth.data$percent, digits = 2)

eth.data$Ethnicity <- factor(eth.data$Ethnicity,                 # Relevel group factor
                             levels = c("European", 
                                        "Asian",
                                        "Aboriginal and/or Torres Strait Islander",
                                        "Maori or other Pacific Islander", 
                                        "Middle Eastern or North African"))

#Respondent ethnicity - summary table
eth.tab <- eth.data %>%
  dplyr::summarize(n_students = n()) %>%
  mutate(percent = n_students / sum(n_students)*100) %>%   
  ungroup()
eth.data$percent <- round(eth.data$percent, digits = 2)

#Respondent ethnicity - summary table
eth.tab <- eth.data %>%
  dplyr::rename("Broad Ethnic or Cultural Group" = Ethnicity,
                "n" = n_students,
                "% of total" = percent)
eth.table <- ggtexttable(eth.tab, rows = NULL, theme = ttheme("light")) %>%
  tab_add_footnote(text = "n = 106", size = 10, face = "italic")
eth.table

ggsave("eth_tab.png", width = 10, height = 10)
ggsave("eth_tab.tiff", width = 10, height = 10)

#Educational and family background of respondents
dem.groups <- demo.data %>%
  dplyr::select("age_group", "Ethnicity", "Gender")

edu.data <- as_tibble(survey_data) %>% 
  dplyr::select("Parent_Ed", "Secondary_Ed_Type", "Incomp_Prev_Ed", "Curr_Enrol", "Student_Status") %>%
  mutate(Parent_Ed = replace(Parent_Ed, Parent_Ed == 999 | Parent_Ed == 900, NA),
         Secondary_Ed_Type = replace(Secondary_Ed_Type, Secondary_Ed_Type == 999, NA),
         parent_group = case_when(Parent_Ed == 200~"Completed some school below Year 10 or equivalent",
                               Parent_Ed == 210 | Parent_Ed == 220 ~ "Completed at least Year 10 or equivalent",
                               Parent_Ed == 300 ~ "Completed Secondary School",
                               Parent_Ed == 400 | Parent_Ed == 500 ~ "Trade Certificate or Diploma",
                               Parent_Ed == 600 ~ "Bachelor Degree",
                               Parent_Ed == 700 ~ "Honours and/or Graduate Degree",
                               Parent_Ed == 800 ~ "Masters Degree",
                               Parent_Ed == 820 ~ "Doctoral or Professional Degree"),
         secondary_group = case_when(Secondary_Ed_Type == 100 ~ "Australian public school",
                                     Secondary_Ed_Type == 200 ~ "Australian independent/selective school",
                                     Secondary_Ed_Type == 300 ~ "Australian private school",
                                     Secondary_Ed_Type == 400 ~ "Homeschooled or non-traditional high school",
                                     Secondary_Ed_Type == 500 ~ "Completed schooling outside of Australia",
                                     Secondary_Ed_Type == 600 ~ "Did not complete high school"),
         prev_qual_group = case_when(Incomp_Prev_Ed == 110 ~ "TAFE - started but did not complete",
                                   Incomp_Prev_Ed == 120 ~ "TAFE - completed",
                                   Incomp_Prev_Ed == 210 ~ "University - started but did not complete",
                                   Incomp_Prev_Ed == 220 ~ "University - completed",
                                   Incomp_Prev_Ed == 900 | Incomp_Prev_Ed == 999 ~ "No previous qualifications"))

#summary data for educational data - plots
#Parent educational background
parent.data <- edu.data %>%
  drop_na(parent_group) %>%
  group_by(parent_group) %>%
  dplyr::summarize(n_parent = n()) %>%
  mutate(percent = n_parent / sum(n_parent)*100) %>%
  ungroup()

parent.data$percent <- round(parent.data$percent, digits = 2)

parent.data$parent_group <- factor(parent.data$parent_group,                 # Relevel group factor
                             levels = c("Doctoral or Professional Degree",
                                        "Masters Degree",
                                        "Honours and/or Graduate Degree",
                                        "Bachelor Degree",
                                        "Trade Certificate or Diploma",
                                        "Completed Secondary School",
                                        "Completed at least Year 10 or equivalent",
                                        "Completed some school below Year 10 or equivalent"))

par.table <- ggtexttable(parent.data, rows = NULL, theme = ttheme("light")) %>%
  tab_add_footnote(text = "n = 100", size = 10, face = "italic")
par.table


parent.plot <- ggplot(parent.data, 
                   aes(x = parent_group,
                       y = percent)) +
  geom_bar(stat = "identity", colour = "black", fill = c("orange2")) +
  scale_x_discrete(labels = function(parent_group) str_wrap(parent_group, width = 15)) +
  labs(title = "Highest parent educational background",
       caption = "n = 100") +
  labs(fill = "") +
  xlab("") +
  ylab("%") +
  theme_classic(base_size = 16)
parent.plot

#Create figure with bar plot and summary data tables
parent.plot.p <- ggarrange(parent.plot, par.table, 
                        ncol = 1, nrow = 2,
                        heights = c(1, 0.5))
parent.plot.p

ggsave("parent_groups_bar.png", width = 14, height = 10)
ggsave("parent_groups_bar.tiff", width = 14, height = 10)

#Student schooling background
edhist.data <- edu.data %>%
  drop_na(secondary_group) %>%
  group_by(secondary_group) %>%
  dplyr::summarize(n_edhist = n()) %>%
  mutate(percent = n_edhist / sum(n_edhist)*100) %>%
  ungroup()
edhist.data$percent <- round(edhist.data$percent, digits = 2)
edhist.data <- edhist.data

edhist.table <- ggtexttable(edhist.data, rows = NULL, theme = ttheme("light")) %>%
  tab_add_footnote(text = "n = 105", size = 10, face = "italic")
edhist.table

edhist.plot <- ggplot(edhist.data, 
                   aes(x = reorder(secondary_group, desc(percent)),
                       y = percent)) +
  geom_bar(stat = "identity", colour = "black", fill = "orange2") +
  scale_x_discrete(labels = function(secondary_group) str_wrap(secondary_group, width = 15)) +
  labs(title = "Secondary education background of Level 1 Archaeology students",
       caption = "n = 105") +
  labs(fill = "") +
  xlab("") +
  ylab("%") +
  geom_text(aes(label = percent),
            vjust = -0.5, size = 3) +
  theme_classic(base_size = 16)
edhist.plot

#Create figure with bar plot and summary data tables
edhist.plot.p <- ggarrange(edhist.plot, edhist.table, 
                        ncol = 1, nrow = 2,
                        heights = c(1, 0.5))
edhist.plot.p

ggsave("secondary_school.png", width = 10, height = 8)
ggsave("secondary_school.tiff", width = 10, height = 8)

#Previous attempts at qualifications
qualhist.data <- edu.data %>%
  drop_na(prev_qual_group) %>%
  group_by(prev_qual_group) %>%
  dplyr::summarize(n_qualhist = n()) %>%
  mutate(percent = n_qualhist / sum(n_qualhist)*100) %>%
  ungroup()
qualhist.data$percent <- round(qualhist.data$percent, digits = 2)

qualhist.table <- ggtexttable(qualhist.data, rows = NULL, theme = ttheme("light")) %>%
  tab_add_footnote(text = "n = 107", size = 10, face = "italic")
qualhist.table

qualhist.plot <- ggplot(qualhist.data, 
                      aes(x = prev_qual_group,
                          y = percent)) +
  geom_bar(stat = "identity", colour = "black", fill = c("lightgrey", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C")) +
  scale_x_discrete(labels = function(prev_qual_group) str_wrap(prev_qual_group, width = 15)) +
  labs(title = "Previous completed or attempted qualifications",
       caption = "n = 107") +
  labs(fill = "") +
  xlab("") +
  ylab("%") +
  geom_text(aes(label = percent),
            vjust = -0.5, size = 3) +
  theme_classic(base_size = 16)
qualhist.plot

#Create figure with bar plot and summary data tables
qualhist.plot.p <- ggarrange(qualhist.plot, qualhist.table, 
                           ncol = 1, nrow = 2,
                           heights = c(1, 0.5))
qualhist.plot.p

ggsave("prev_qual_bar.png", width = 10, height = 10)
ggsave("prev_qual_bar.tiff", width = 10, height = 10)

#Enrolment type
enrol.data <- edu.data %>%
  drop_na(Curr_Enrol) %>%
  group_by(Curr_Enrol) %>%
  dplyr::summarize(n_enrol = n()) %>%
  mutate(percent = n_enrol / sum(n_enrol)*100) %>%
  ungroup()
enrol.data$percent <- round(enrol.data$percent, digits = 2)

enrol.plot <- edu.data %>% 
  ggplot(aes(x = Curr_Enrol, 
             y = ((..count..)/sum(..count..)))) +
  geom_bar(stat = "count", colour = "black", fill = "lightgrey") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(limits = c("Full Time", "Part Time", "Non Award")) +
  coord_flip() +
  labs(title = "Enrolment status of Level 1 Archaeology students") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(),
        legend.position = "none",
        plot.title = element_text(size = 20, margin = margin(b = 10)))
enrol.plot

#ggsave("enrol_bar.png", width = 10, height = 8)
#ggsave("enrol_bar.tiff", width = 10, height = 8)

#Student status
status.data <- edu.data %>%
  drop_na(Student_Status) %>%
  group_by(Student_Status) %>%
  dplyr::summarize(n_status = n()) %>%
  mutate(p_status = n_status / sum(n_status)*100) %>%
  ungroup()
status.data$p_status <- round(status.data$p_status, digits = 2)

status.plot <- edu.data %>% 
  ggplot(aes(x = Student_Status, 
             y = ((..count..)/sum(..count..)))) +
  geom_bar(stat = "count", colour = "black", fill = "lightgrey") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(limits = c("Domestic", "International")) +
  coord_flip() +
  labs(title = "Student status of Level 1 Archaeology students") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(),
        legend.position = "none",
        plot.title = element_text(size = 20, margin = margin(b = 10)))
status.plot

ggsave("status_bar.png", width = 10, height = 8)
ggsave("status_bar.tiff", width = 10, height = 8)

#Caring responsibilities amongst Archaeology students
care.data <- as_tibble(survey_data) %>% 
  dplyr::select("Caring_Resp") %>%
  mutate(Caring_Resp = replace(Caring_Resp, Caring_Resp == "NA", NA))
care.sum.data <- care.data %>%
  group_by(Caring_Resp) %>%
  drop_na(Caring_Resp) %>%
  dplyr::summarize(n_care = n()) %>%
  mutate(percent = n_care / sum(n_care)*100) %>%
  ungroup()
care.sum.data$percent <- round(care.sum.data$percent, digits = 2)

care.plot <- ggplot(care.sum.data, 
                      aes(x = reorder(Caring_Resp, desc(percent)),
                          y = percent)) +
  geom_bar(stat = "identity", width = 0.7, colour = "black", fill = "orange2") +
  scale_x_discrete(labels = function(Caring_Resp) str_wrap(Caring_Resp, width = 15)) +
  labs(title = "Students with caring responsibilities",
       caption = "n = 106") +
  labs(fill = "") +
  xlab("") +
  ylab("%") +
  geom_text(aes(label = percent),
            vjust = -0.5, size = 3) +
  theme_classic(base_size = 16)
care.plot

ggsave("caring.png", width = 10, height = 8)
ggsave("caring.tiff", width = 10, height = 8)

#Disability and chronic issues amongst Archaeology students
#- a sheet from the ANCATL_Student_Survey_Data_2021.xlsx titled "Health_Cond_Type" that includes counts of relevant sources of information as selected by respondents
health_data <- read_excel("ANCATL_Student_Survey_Data_2021.xlsx", 
                                               sheet = "Health_Cond_Type")
View(health_data)

#Convert counts to % of responses
health.data <- health_data %>%
  mutate(percent = Count / 105 * 100) %>%
  mutate(Cond_Type = case_when(Cond_Type == "Neurological" ~ "A neurological impairment", 
                               Cond_Type == "Mobility" ~ "A mobility impairment",
                               Cond_Type == "Autoimmune" ~ "An autoimmune and/or connective tissue disorder", 
                               Cond_Type == "Sensory" ~ "A sensory impairment (vision or hearing)",
                               Cond_Type == "Other" ~ "A disability or condition not otherwise listed", 
                               Cond_Type == "Learning" ~ "A learning disability or form of neurodiversity",
                               Cond_Type == "Mental" ~ "A mental health condition"))

health.data$percent <- round(health.data$percent, digits = 2)
View(health.data)

#summary data for health and disability data - plots
health.plot <- ggplot(health.data, 
                      aes(x = reorder(Cond_Type, desc(percent)),
                          y = percent)) +
  geom_bar(stat = "identity", width = 0.7, colour = "black", fill = "orange2") +
  scale_x_discrete(labels = function(Cond_Type) str_wrap(Cond_Type, width = 15)) +
  labs(title = "Reported chronic health conditions and disabilities amongst Level 1 Archaeology students",
       caption = "n = 105") +
  labs(fill = "") +
  xlab("") +
  ylab("%") +
  geom_text(aes(label = percent),
            vjust = -0.5, size = 4) +
  theme_classic(base_size = 16)
health.plot

ggsave("health_type.png", width = 15, height = 10)
ggsave("health_type.tiff", width = 15, height = 10)

#Previous exposure to archaeological information
#- a sheet from the ANCATL_Student_Survey_Data_2021.xlsx titled "Arch_Info_Type" that includes counts of relevant sources of information as selected by respondents
arch_data <- read_excel("Data and Scripts/ANCATL_Student_Survey_Data_2022.xlsx", 
                        sheet = "Arch_Info_Type")
View(arch_data)

#Convert counts to % of responses
arch.summ <- arch_data %>%
  mutate(percent = Count / 107 * 100) %>%
  mutate(Arch_Info_Type = fct_relevel(Arch_Info_Type, 
                                      "Arch_Collab", "Arch_Comm_Fam", "Arch_Podcasts", 
                                      "Arch_Soc_Med", "Arch_Fic", "Arch_Media", 
                                      "Arch_Online", "Arch_Film_TV", "Arch_Non_Fic", 
                                      "Arch_Travel", "Arch_Doc")) %>%
  mutate(Arch_Info_Type = case_when(Arch_Info_Type == "Arch_Doc" ~ "Film and Television Documentaries", 
                                    Arch_Info_Type == "Arch_Travel" ~ "In-person visits to sites, museums, etc.",
                                    Arch_Info_Type == "Arch_Non_Fic" ~ "Non-fiction books", 
                                    Arch_Info_Type == "Arch_Film_TV" ~ "Fictionalised/dramatised film and television",
                                    Arch_Info_Type == "Arch_Online" ~ "Online sources (i.e. Wikipedia, blogs, or other websites)", 
                                    Arch_Info_Type == "Arch_Media" ~ "News media (including print, television, radio, and online media sources)",
                                    Arch_Info_Type == "Arch_Fic" ~ "Fiction books",
                                    Arch_Info_Type == "Arch_Soc_Med" ~ "Social media (including YouTube, Instagram, TikTok, Twitter, Facebook, etc.)", 
                                    Arch_Info_Type == "Arch_Podcasts" ~ "Podcasts", 
                                    Arch_Info_Type == "Arch_Comm_Fam" ~ "Community and family members", 
                                    Arch_Info_Type == "Arch_Collab" ~ "Community collaboration with archaeologists/researchers"))
View(arch.summ)

#summary data for archaeological information sources data - plots
arch.info.plot <- arch.summ %>%
  ggplot(aes(x = reorder(Arch_Info_Type, desc(percent)), y = percent)) +
  geom_segment(aes(xend = Arch_Info_Type, yend = 0)) +
  geom_point(size = 8, colour = "cyan4") +
  scale_x_discrete(labels = function(Arch_Info_Type) str_wrap(Arch_Info_Type, width = 40)) +
  coord_flip() +
  labs(title = "Pre-enrolment sources of archaeological information") +
  xlab("") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.position = "none",
        plot.title = element_blank())
arch.info.plot

ggsave("arch_type.png", width = 15, height = 8)
ggsave("arch_type.tiff", width = 15, height = 8)

#Previous exposure to information about Aboriginal and Torres Strait Islander people and cultures
#- a sheet from the ANCATL_Student_Survey_Data_2021.xlsx titled "ATSI_Info_Type" that includes counts of relevant sources of information as selected by respondents
atsi_data <- read_excel("Data and Scripts/ANCATL_Student_Survey_Data_2022.xlsx", 
                        sheet = "ATSI_Info_Type")
View(atsi_data)

#Convert counts to % of responses
atsi.summ <- atsi_data %>%
  mutate(percent = Count / 107 * 100) %>%
  mutate(ATSI_Info_Type = case_when(ATSI_Info_Type == "ATSI_Doc" ~ "Film and Television Documentaries", 
                                    ATSI_Info_Type == "ATSI_Tourism" ~ "Travel guides and/or tourist information centres, brochures, and signs",
                                    ATSI_Info_Type == "ATSI_Non_Fic" ~ "Non-fiction books", 
                                    ATSI_Info_Type == "ATSI_Film_TV" ~ "Fictionalised/dramatised film and television",
                                    ATSI_Info_Type == "ATSI_Online" ~ "Online sources (i.e. Wikipedia, blogs, or other websites)", 
                                    ATSI_Info_Type == "ATSI_Media" ~ "News media (including print, television, radio, and online media sources)",
                                    ATSI_Info_Type == "ATSI_Fic" ~ "Fiction books",
                                    ATSI_Info_Type == "ATSI_Soc_Med" ~ "Social media (including YouTube, Instagram, TikTok, Twitter, Facebook, etc.", 
                                    ATSI_Info_Type == "ATSI_Podcasts" ~ "Podcasts", 
                                    ATSI_Info_Type == "ATSI_Comm_Fam" ~ "Community and family members", 
                                    ATSI_Info_Type == "ATSI_None" ~ "I do not have any knowledge or understanding of Aboriginal & Torres Strait Islander culture and heritage",
                                    ATSI_Info_Type == "ATSI_Indig_Teach" ~ "School lesson(s) taught by an Aboriginal/Torres Strait Islander teacher",
                                    ATSI_Info_Type == "ATSI_Visit" ~ "School visits (incursions or excursions) by Aboriginal & Torres Strait Islander people",
                                    ATSI_Info_Type == "ATSI_nonIndig_Teach" ~ "School lessons taught by a non-Aboriginal/Torres Strait Islander teacher"))
View(atsi.summ)

#summary data for archaeological information sources data - plots
atsi.info.plot <- atsi.summ %>%
  ggplot(aes(x = reorder(ATSI_Info_Type, desc(percent)), y = percent)) +
  geom_segment(aes(xend = ATSI_Info_Type, yend = 0)) +
  geom_point(size = 10, colour = "cyan4") +
  scale_x_discrete(labels = function(ATSI_Info_Type) str_wrap(ATSI_Info_Type, width = 40)) +
  coord_flip() +
  labs(title = "Sources of information about Aboriginal & Torres Strait Islander culture & heritage") +
  xlab("") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.position = "none",
        plot.title = element_blank())
atsi.info.plot

ggsave("ATSI_type.png", width = 20, height = 14)
ggsave("ATSI_type.tiff", width = 20, height = 14)

#Previous and planned archaeological experiences
plan.data <- as_tibble(survey_data) %>% 
  dplyr::select("Prev_Arch_Exp", "Plan_Study", "Work_Arch", "Arch_Empl", "Consider_Empl", "Skill_Pass")

#Planned to study archaeology ahead of enrolment
plan.study.plot <- plan.data %>% 
  ggplot(aes(x = reorder(Plan_Study, Plan_Study,
                         function(x)-length(x)), 
             y = ((..count..)/sum(..count..)))) +
  geom_bar(stat = "count", colour = "black", fill = "lightgrey") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Planned to study archaeology before enrolling in current degree") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(),
        legend.position = "none",
        plot.title = element_text(size = 20, margin = margin(b = 10)))
plan.study.plot

ggsave("plan_study.png", width = 20, height = 10)
ggsave("plan_study.tiff", width = 20, height = 10)

#Previous exposure to archaeology (Q14)
prev.arch.plot <- plan.data %>% 
  ggplot(aes(x = reorder(Prev_Arch_Exp, Prev_Arch_Exp,
                         function(x)-length(x)), 
             y = ((..count..)/sum(..count..)))) +
  geom_bar(stat = "count", colour = "black", fill = "lightgrey") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Previous hands-on experience or observation of archaeology or heritage activities") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(),
        legend.position = "none",
        plot.title = element_text(size = 20, margin = margin(b = 10)))
prev.arch.plot

ggsave("prev_arch.png", width = 20, height = 10)
ggsave("prev_arch.tiff", width = 20, height = 10)

#- a sheet from the ANCATL_Student_Survey_Data_2021.xlsx titled "Q14" that includes counts of relevant sources of information as selected by respondents
Q14 <- read_excel("ANCATL_Student_Survey_Data_2021.xlsx", 
                          sheet = "Q14")
View(Q14)

Q14 <- as_tibble(Q14) %>% 
  mutate(percent = n / sum(n)*100)
Q14$percent <- round(Q14$percent, digits = 2)

Q14.plot <- ggplot(Q14, 
                  aes(x = reorder(Activity, desc(percent)),
                      y = percent)) +
  geom_bar(stat = "identity", width = 0.7, colour = "black", fill = "cyan4") +
  scale_x_discrete(labels = function(Activity) str_wrap(Activity, width = 15)) +
  labs(fill = "") +
  xlab("") +
  theme_classic(base_size = 20)
Q14.plot

ggsave("outreach.png", width = 16, height = 10)
ggsave("outreach.tiff", width = 16, height = 10)

#Plan to work as an archaeologist (Q18)
plan.work.plot <- plan.data %>% 
  ggplot(aes(x = reorder(Work_Arch, Work_Arch,
                         function(x)-length(x)), 
             y = ((..count..)/sum(..count..)))) +
  geom_bar(stat = "count", colour = "black", fill = "orange2") +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "") +
  xlab("") +
  ylab("") +
  theme_classic(base_size = 16) +
  theme(axis.text = element_text(size = 14))
plan.work.plot

ggsave("plan_work.png", width = 10, height = 10)
ggsave("plan_work.tiff", width = 10, height = 10)

#Are there enough employment opportunities for archaeologists (Q19)
arch.empl.plot <- plan.data %>% 
  ggplot(aes(x = reorder(Arch_Empl, Arch_Empl,
                         function(x)-length(x)), 
             y = ((..count..)/sum(..count..)))) +
  geom_bar(stat = "count", colour = "black", fill = "lightgrey") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Are there enough employment opportunities for archaeologists?") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(),
        legend.position = "none",
        plot.title = element_text(size = 20, margin = margin(b = 10)))
arch.empl.plot

ggsave("arch_empl.png", width = 20, height = 10)
ggsave("arch_empl.tiff", width = 20, height = 10)

#Did employability play a role in your decision to study archaeology? (Q20)
employability.plot <- plan.data %>% 
  ggplot(aes(x = reorder(Consider_Empl, Consider_Empl,
                         function(x)-length(x)), 
             y = ((..count..)/sum(..count..)))) +
  geom_bar(stat = "count", colour = "black", fill = "orange2") +
  scale_x_discrete(labels = function(Consider_Empl) str_wrap(Consider_Empl, width = 15)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Did you consider employability when enrolling in this degree?",
       caption = "n = 107") +
  labs(fill = "") +
  xlab("") +
  ylab("") +
  theme_classic(base_size = 16) +
  theme(axis.text = element_text(size = 14))
employability.plot

ggsave("employability.png", width = 15, height = 10)
ggsave("employability.tiff", width = 15, height = 10)

#Perceived value of Skills Passport in preparation for future employment
passport.plot <- plan.data %>%
  drop_na(Skill_Pass) %>%
  ggplot(aes(x = reorder(Skill_Pass, Skill_Pass,
                         function(x)-length(x)), 
             y = ((..count..)/sum(..count..)))) +
  geom_bar(stat = "count", colour = "black", fill = "cyan4") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = function(Skill_Pass) str_wrap(Skill_Pass, width = 40)) +
  coord_flip() +
  labs(fill = "") +
  xlab("") +
  ylab("") +
  theme_classic(base_size = 20)
passport.plot

ggsave("Skill_Pass.png", width = 15, height = 10)
ggsave("Skill_Pass.tiff", width = 15, height = 10)

#- a sheet from the ANCATL_Student_Survey_Data_2021.xlsx titled "Q13_f" that includes counts of relevant sources of information as selected by respondents
Q13_f <- read_excel("ANCATL_Student_Survey_Data_2021.xlsx", 
                          sheet = "Q13_f")
View(Q13_f)

#Top five factors influencing student enrollment in this unit
Q13_f$rank_text <- factor(Q13_f$rank_text,                 # Relevel group factor
                         levels = c("Fifth (least)", "Fourth", "Third", "Second", "First (most important)"))

decision.plot <- ggplot(data = Q13_f) +
  geom_col(aes(x = factor, y = n, fill = rank_text), colour="black") +
  scale_x_discrete(labels = function(factor) str_wrap(factor, width = 40)) +
  coord_flip() +
  scale_fill_manual(
    values = colorRampPalette(
      RColorBrewer::brewer.pal(n = 9, name = "PuBuGn"))(5),
    guide = guide_legend(reverse = TRUE)) +
  labs(fill = "") +
  xlab("") +
  ylab("Number of responses") +
  labs(title = "Five most important factors for deciding to enrol in this unit") +
  theme_classic(base_size = 26) +
  theme(panel.background = element_rect(fill = "grey95"),
        axis.text = element_text(size = 26),
        plot.title = element_blank(),
        legend.position = "top")
decision.plot

ggsave("Decision_Factors.png", width = 20, height = 20)
ggsave("Decision_Factors.tiff", width = 20, height = 20)


#- an xls file called "ANCATL_Student_Survey_2021" that includes responses to the survey questions
Q7 <- read_excel("ANCATL_Student_Survey_Data_2021.xlsx", 
                          sheet = "Q7")
View(Q7)

Q7 <- as_tibble(Q7) %>% 
  mutate(percent = n / sum(n)*100)
Q7$percent <- round(Q7$percent, digits = 2)

Q7.plot <- ggplot(Q7, 
                    aes(x = reorder(Degree, desc(percent)),
                        y = percent)) +
  geom_bar(stat = "identity", width = 0.7, colour = "black", fill = "orange2") +
  scale_x_discrete(labels = function(Degree) str_wrap(Degree, width = 15)) +
  labs(title = "Degree type",
       caption = "n = 88") +
  labs(fill = "") +
  xlab("") +
  ylab("%") +
  geom_text(aes(label = percent),
            vjust = -0.5, size = 3) +
  theme_classic(base_size = 16)
Q7.plot

ggsave("degree.png", width = 10, height = 10)
ggsave("degree.tiff", width = 10, height = 10)