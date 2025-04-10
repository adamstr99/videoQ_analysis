# Analytic Script Supplement for:

# Strojil, A., Cígler, H. (n.d.). Video-administered Questionnaire: Psychometric properties and comparison with a text-based format
# Preprint available at: 
# Raw data available at: https://osf.io/fznx9/

# INPSY - Psychology Research Institute
# Masaryk University, Faculty of Social Studies

# Original R version used: 4.2.3




# NOTE:
#   For easier navigation, please refer to the code sections in R Studio
#   Section headers are IN CAPITALS. Subsection headers begin with "."
#   Section headers marked "*" are prerequisites for subsequent sections of the code
#   All subsections require the whole parent section to be initialized
#   Sections not marked with "*" should work as standalone
#     = No need to run the previous code except for the sections marked with "*"
#     - Section "ALL MODELS FITMEASURES" is an exception (see NOTE under its header)




#*ENVIRONMENT SETUP----
#Used packages
library(tidyverse)
library(tidyselect)
library(car)
library(jtools)
library(psych)
library(lavaan)
library(semTools)
library(effectsize)
library(modelsummary) #summary of lin. reg. tables only
library(ggpubr) #distribution graphs only

#ggplot custom theme
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line())
theme_set(apatheme)







#*RAW DATA IMPORT----
#Qualtrics raw data import (doctype: csv)
raw_rawdata <- read.csv("VideoQ_RawData.csv", na.strings = c(""," ",NA))
rawdata <- slice(raw_rawdata, -1, -2)
rawdata <- type.convert(rawdata, as.is = TRUE)






#*COALESCE THE VARIABLES FROM DIFFERENT VERSIONS----
#Height inventory scale (x.1...)
data_coal <- rawdata %>%
  mutate(x1.1 = coalesce(!!! syms(vars_select(names(.), starts_with("X1.1."))))) %>%
  mutate(x1.2 = coalesce(!!! syms(vars_select(names(.), starts_with("X1.2"))))) %>%
  mutate(x1.3 = coalesce(!!! syms(vars_select(names(.), starts_with("X1.3"))))) %>%
  mutate(x1.4 = coalesce(!!! syms(vars_select(names(.), starts_with("X1.4"))))) %>%
  mutate(x1.5 = coalesce(!!! syms(vars_select(names(.), starts_with("X1.5"))))) %>%
  mutate(x1.6 = coalesce(!!! syms(vars_select(names(.), starts_with("X1.6"))))) %>%
  mutate(x1.7 = coalesce(!!! syms(vars_select(names(.), starts_with("X1.7"))))) %>%
  mutate(x1.8 = coalesce(!!! syms(vars_select(names(.), starts_with("X1.8"))))) %>%
  mutate(x1.9 = coalesce(!!! syms(vars_select(names(.), starts_with("X1.9"))))) %>%
  mutate(x1.10 = coalesce(!!! syms(vars_select(names(.), starts_with("X1.10"))))) %>%
  mutate(x1.11 = coalesce(!!! syms(vars_select(names(.), starts_with("X1.11")))))

#RSES scale (x.2...)
data_coal <- data_coal %>%
  mutate(x2.1 = coalesce(!!! syms(vars_select(names(.), starts_with("X2.1."))))) %>%
  mutate(x2.2 = coalesce(!!! syms(vars_select(names(.), starts_with("X2.2"))))) %>%
  mutate(x2.3 = coalesce(!!! syms(vars_select(names(.), starts_with("X2.3"))))) %>%
  mutate(x2.4 = coalesce(!!! syms(vars_select(names(.), starts_with("X2.4"))))) %>%
  mutate(x2.5 = coalesce(!!! syms(vars_select(names(.), starts_with("X2.5"))))) %>%
  mutate(x2.6 = coalesce(!!! syms(vars_select(names(.), starts_with("X2.6"))))) %>%
  mutate(x2.7 = coalesce(!!! syms(vars_select(names(.), starts_with("X2.7"))))) %>%
  mutate(x2.8 = coalesce(!!! syms(vars_select(names(.), starts_with("X2.8"))))) %>%
  mutate(x2.9 = coalesce(!!! syms(vars_select(names(.), starts_with("X2.9"))))) %>%
  mutate(x2.10 = coalesce(!!! syms(vars_select(names(.), starts_with("X2.10")))))

#Body image item
data_coal <- data_coal %>%
  mutate(vaha = coalesce(!!! syms(vars_select(names(.), starts_with("vaha")))))

#Item Timers
#Height Questionnaire scale
data_coal <- data_coal %>%
  mutate(timer1.1_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer1.1.") & ends_with("Page.Submit"))))) %>%
  mutate(timer1.2_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer1.2") & ends_with("Page.Submit"))))) %>%
  mutate(timer1.3_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer1.3") & ends_with("Page.Submit"))))) %>%
  mutate(timer1.4_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer1.4") & ends_with("Page.Submit"))))) %>%
  mutate(timer1.5_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer1.5") & ends_with("Page.Submit"))))) %>%
  mutate(timer1.6_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer1.6") & ends_with("Page.Submit"))))) %>%
  mutate(timer1.7_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer1.7") & ends_with("Page.Submit"))))) %>%
  mutate(timer1.8_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer1.8") & ends_with("Page.Submit"))))) %>%
  mutate(timer1.9_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer1.9") & ends_with("Page.Submit"))))) %>%
  mutate(timer1.10_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer1.10") & ends_with("Page.Submit"))))) %>%
  mutate(timer1.11_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer1.11") & ends_with("Page.Submit")))))

#RSES scale
data_coal <- data_coal %>%
  mutate(timer2.1_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer2.1.") & ends_with("Page.Submit"))))) %>%
  mutate(timer2.2_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer2.2") & ends_with("Page.Submit"))))) %>%
  mutate(timer2.3_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer2.3") & ends_with("Page.Submit"))))) %>%
  mutate(timer2.4_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer2.4") & ends_with("Page.Submit"))))) %>%
  mutate(timer2.5_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer2.5") & ends_with("Page.Submit"))))) %>%
  mutate(timer2.6_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer2.6") & ends_with("Page.Submit"))))) %>%
  mutate(timer2.7_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer2.7") & ends_with("Page.Submit"))))) %>%
  mutate(timer2.8_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer2.8") & ends_with("Page.Submit"))))) %>%
  mutate(timer2.9_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer2.9") & ends_with("Page.Submit"))))) %>%
  mutate(timer2.10_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer2.10") & ends_with("Page.Submit")))))

#Body image item
data_coal <- data_coal %>%
  mutate(timer_vaha_S = coalesce(!!! syms(vars_select(names(.), starts_with("timer_vaha") & ends_with("Page.Submit")))))


#Extracting variables for the analysis (datamarix_full)
datamatrix_full <- data_coal %>% 
  select(vek, pohlavi, vyska, group, 
         
         x1.1, x1.2, x1.3, x1.4, x1.5, x1.6, 
         x1.7, x1.8, x1.9, x1.10, x1.11,
         
         x2.1, x2.2, x2.3, x2.4, x2.5, 
         x2.6, x2.7, x2.8, x2.9, x2.10,
         
         vaha,
         
         eng1, eng2, tech1, cteni,
         
         vars_select(names(data_coal), matches("^end[0-9]+$")),
         vars_select(names(data_coal), matches("^end2.[0-9]+$")),
         
         vars_select(names(data_coal), matches("^start[0-9]+$")),
         vars_select(names(data_coal), matches("^start2.[0-9]+$")),
         
         vars_select(names(data_coal), matches("^pause[0-9]+$")),
         vars_select(names(data_coal), matches("^pause2.[0-9]+$")),
         
         windowCount, windowCount2,
         
         vars_select(names(data_coal), matches("^timer.*_S$")),
         Duration..in.seconds., Finished, MetaData_Operating.System
         
  )

#Renaming and variable changes
datamatrix_full <- datamatrix_full %>%
  rename(duration = Duration..in.seconds.)

datamatrix_full <- datamatrix_full %>%
  rename(op_sys = MetaData_Operating.System)

datamatrix_full <- mutate(datamatrix_full, os_type = if_else(grepl("^(Windows|Macintosh|Linux)", op_sys), "PC", "mobile"))


datamatrix_full$pohlavi <- factor(datamatrix_full$pohlavi, levels = c(1, 2, 3), labels = c("Žena", "Muž", "Jiné"))

datamatrix_full$verze <- ifelse(datamatrix_full$group %in% c("video_z", "video_m"), "video", 
                                ifelse(datamatrix_full$group == "text", "text", NA))

datamatrix_full$verze <- factor(datamatrix_full$verze, levels = c("video", "text"), labels = c("video", "text"))
datamatrix_full$group <- factor(datamatrix_full$group, levels = c("video_z", "video_m","text"), labels = c("video_z", "video_m","text"))







#*DATA CLEANING ----
##. Straight-lining ----
#Flag if responses in a scale are the same or all NA
datamatrix_full <- datamatrix_full %>%
  mutate(
    sl_vyska = apply(select(., starts_with("x1")), 1, function(row) {
      all(!is.na(row) & row == row[1]) | all(is.na(row))
    }),
    sl_rses = apply(select(., starts_with("x2")), 1, function(row) {
      all(!is.na(row) & row == row[1]) | all(is.na(row))
    }),
    straight_full = sl_vyska & sl_rses #both scales
  )

##. Missing ----
#Flag cases with more than 12 NAs in the scales
datamatrix_full$missing_values <- rowSums(select(datamatrix_full, starts_with("x1"), starts_with("x2")) %>% is.na())
datamatrix_full$miss <- ifelse(datamatrix_full$missing_values > 12, TRUE, FALSE)

##. Speeding ----
#Flag completion times under 30 seconds
datamatrix_full$toofast_t <- ifelse(datamatrix_full$duration < 30 & datamatrix_full$group == "text", TRUE, FALSE)
datamatrix_full$toofast_vm <- ifelse(datamatrix_full$duration < 30 & datamatrix_full$group == "video_m", TRUE, FALSE)
datamatrix_full$toofast_vz <- ifelse(datamatrix_full$duration < 30 & datamatrix_full$group == "video_z", TRUE, FALSE)

datamatrix_full$toofast <- datamatrix_full$toofast_t | datamatrix_full$toofast_vm | datamatrix_full$toofast_vz

##. Flag invalid ----
#straight-lining, missing, speeding
datamatrix_full$fraud <- 
  datamatrix_full$straight_full | 
  datamatrix_full$miss | 
  datamatrix_full$toofast |
  is.na(datamatrix_full$group)









#*REVERSE ITEMS, 0-3 range ####
datamatrix_full_rec <- datamatrix_full %>% mutate(across(starts_with("x1"), ~recode(., "1=0; 2=1; 3=2; 4=3")))
datamatrix_full_rec <- datamatrix_full_rec %>% mutate(across(starts_with("x2"), ~recode(., "1=0; 2=1; 3=2; 4=3")))

vars_reverse <- c("x1.2", "x1.4", "x1.6", "x1.7", "x1.9", "x1.11", "x2.2", "x2.5", "x2.7", "x2.9", "x2.10")
datamatrix_full_rec <- datamatrix_full_rec %>%
  mutate(across(starts_with(vars_reverse), ~recode(., "0=3; 1=2; 2=1; 3=0")))









#DESCRIPTIVES AND MISSING VALUES####
##. Sample descriptives----

nrow(datamatrix_full_rec) #including invalid

mat_descript <- datamatrix_full_rec %>% filter(!fraud)

nrow(mat_descript) #invalid filtered out = N
table_descript <- mat_descript %>%
  mutate(pohlavi = factor(pohlavi, labels = c("Female","Male","Other"))) %>%
  mutate(group = factor(group, labels = c("VQf","VQm","Text"))) %>% 
  group_by(group, pohlavi) %>%
  summarise(
    n = n()
  )
table_descript

#Graph of sample distribution (age/gender)
mat_descript <- mutate(mat_descript, age_group = cut(vek, breaks = seq(0, 100, 5), include.lowest = TRUE))
ggplot(mat_descript, aes(x = age_group)) +
  geom_bar(aes(fill = pohlavi)) +
  scale_fill_manual(values = c("grey20", "grey50", "grey80"), 
                    labels = c("Female", "Male", "Other")) +
  labs(x = "Age", y = "Frequency", fill = "Gender") + 
  apatheme +
  scale_y_continuous(expand = c(0, 0))



table_vek <- mat_descript %>% 
  group_by(age_group) %>% 
  summarize(N = n(), Freq = n() / nrow(mat_descript))
table_vek #table of age group distribution

mat_descript %>% group_by(os_type) %>% summarize(n = n()) #mobile x PC
mat_descript %>% group_by(verze) %>% summarize(n = n()) #VQ x TQ
mat_descript %>%
  mutate(group = factor(group, labels = c("VQf","VQm","Text"))) %>% 
  group_by(group) %>% summarize(n = n()) #VQm x VQf x TQ


mat_descript %>% summarize(mean_age = mean(vek), sd_age = sd(vek), Med_age = median(vek), n = n())
#males
mat_descript %>% filter(mat_descript$pohlavi == "Muž") %>% summarize(mean_age = mean(vek), sd_age = sd(vek), Med_age = median(vek), n = n())
#females
mat_descript %>% filter(mat_descript$pohlavi == "Žena") %>% summarize(mean_age = mean(vek), sd_age = sd(vek), Med_age = median(vek), n = n())

#gender by methods
table_pohl <- mat_descript %>% 
  mutate(pohlavi = factor(pohlavi, labels = c("Female","Male","Other"))) %>% 
  group_by(pohlavi) %>% 
  summarize(N = n(), Freq = n() / nrow(mat_descript))
table_pohl

table_pohl2 <- mat_descript %>%
  mutate(pohlavi = factor(pohlavi, labels = c("Female","Male","Other"))) %>%
  mutate(group = factor(group, labels = c("VQf","VQm","Text"))) %>% 
  group_by(pohlavi, group) %>% 
  summarize(N = n(), Freq = n() / nrow(mat_descript))
table_pohl2

#age by methods
table_vek2 <- mat_descript %>%
  mutate(group = factor(group, labels = c("VQf","VQm","Text"))) %>% 
  group_by(group) %>% 
  summarize(N = n(), 
            Freq = n() / nrow(mat_descript), 
            mean = mean(vek),
            sd = sd(vek),
            med = median(vek),
            var = var(vek))
table_vek2

table_vek3 <- mat_descript %>% 
  group_by(verze) %>% 
  summarize(N = n(), 
            Freq = n() / nrow(mat_descript), 
            mean = mean(vek),
            sd = sd(vek),
            med = median(vek),
            var = var(vek))
table_vek3


##. Invalid cases analysis----
table_fraud <- datamatrix_full_rec %>% filter(fraud) %>%
  group_by(group) %>%
  summarise(N = n(),
            Freq = n() / nrow(datamatrix_full_rec))
table_fraud

data_coal %>% filter(is.na(group))
datamatrix_full_rec %>% filter(fraud, !is.na(group))%>%select(starts_with("x1"), starts_with("x2"), group, start0, op_sys)

fraud_os <- datamatrix_full_rec %>% filter(fraud, !is.na(group))%>%select(starts_with("x1"), starts_with("x2"), group, verze, start0, pause0, end0, timer1.1_S, op_sys, os_type , pohlavi) %>% 
  mutate(played = start0+pause0+end0)


odpovedi <- fraud_os %>% select(starts_with("x1"),starts_with("x2")) %>% mutate(odpovedi = rowSums(!is.na(.)))
odpovedi$odpovedi

fraud_os %>% group_by(verze, os_type) %>% summarize(
  N=n()
)

fraud_os %>% 
  mutate(pohlavi = factor(pohlavi, labels = c("Female","Male","Other"))) %>%
  group_by(pohlavi, verze) %>% 
  summarize(
  N=n()
)

#NA ratio for dropped cases
fraud_desc <- datamatrix_full_rec %>% filter(fraud) %>% 
  select(vek, vyska ,starts_with("x1"), starts_with("x2"), vaha, eng1, eng2, tech1, cteni)
fraud_desc$na_ratio <- apply(fraud_desc, 1, function(x) mean(is.na(x)))

fraud_desc %>% summarize(
  M = mean(na_ratio),
  n = n(),
  min = min(na_ratio),
  max = max(na_ratio),
  med = median(na_ratio)
)
ncol(fraud_desc)-1

#graph of NA ratio for dropped cases
ggplot(fraud_desc, aes(na_ratio)) + 
  geom_histogram(fill = "grey", color = "black", binwidth = 0.05) +
  labs(y = "Frequency", x = "NA prevalence") +
  scale_x_continuous(breaks = seq(0, 1, 0.05))



#NA cases, break-off
break_off <- datamatrix_full_rec %>% filter(!fraud) %>% select(starts_with("x1"), starts_with("x2"), pohlavi, group, duration, op_sys, os_type)
break_off <- break_off %>% filter(rowSums(is.na(.)) > 5)


table_na2 <- break_off %>% 
  mutate(group = factor(group, labels = c("VQf","VQm","Text"))) %>% 
  group_by(group) %>%
  summarise(N = n(),
            Freq = n() / nrow(break_off),
            Freq_Full = n() / nrow(datamatrix_full_rec))
table_na2



##. break-offs TOTAL----
break_off_all <- datamatrix_full_rec %>% filter(!is.na(group))
break_off_all$b_o <- ifelse(break_off_all$Finished == 0 & is.na(break_off_all$vaha), TRUE, FALSE)
(t <- table(break_off_all$b_o, break_off_all$verze))

#test for statistical difference
prop.test(x = c(32,27), n = c(183,185), alternative="g", conf.level=0.95, correct=FALSE)

fisher.test(t, alternative = "l")




#age for break-off
break_off_all %>% filter(b_o == T) %>% group_by(verze) %>% 
  summarise(
    M = mean(vek, na.rm = T),
    SD = sd(vek, na.rm = T)
  )

datamatrix_full_rec %>% group_by(verze) %>% 
  summarise(
    M = mean(vek, na.rm = T),
    SD = sd(vek, na.rm = T)
  )








#COMPLETION TIME----
time_analysis_item <- select(datamatrix_full, group, verze, pohlavi, duration, ends_with("_S"), fraud)

#filter:not invalid, no NAs, gender male/female
time_analysis_item_filter <- time_analysis_item %>%
  filter(!fraud) %>%
  drop_na() %>% 
  filter(pohlavi!="Jiné")

cols_S <- names(time_analysis_item_filter)[grepl("_S$", names(time_analysis_item_filter))]

#data frame times only
time_analysis_item_filter$total_time <- rowSums(time_analysis_item_filter[, cols_S])

#Mean Time
time_analysis_item_filter$mean_time <- time_analysis_item_filter$total_time/length(cols_S)


time_analysis_item_filter_cut <- time_analysis_item_filter %>%
  mutate(group = factor(group, labels = c("VQf","VQm","Text"))) %>% 
  group_by(group) %>%
  filter(total_time >= quantile(total_time, 0.1) & total_time <= quantile(total_time, 0.90))

box_mean_time <- ggplot(time_analysis_item_filter_cut, aes(group, total_time)) + 
  geom_boxplot()
box_mean_time


#cont.table - mean:total_time: group
table_total_time <- time_analysis_item_filter_cut %>%
  summarise(N = n(), 
            M = mean(total_time),
            Med = median(total_time),
            SD = sd(total_time),
            var = var(total_time)
  )
table_total_time


##. difference in time between VQm and TQ----
srovn <- filter(time_analysis_item_filter_cut, group != "VQf")
t.test(srovn$total_time ~ srovn$group)
effectsize::cohens_d(srovn$total_time ~ srovn$group)

box_total_time_sex <- ggplot(time_analysis_item_filter_cut, aes(x=group, y=total_time, fill=pohlavi)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("gray40", "gray70"),
                    labels = c("Females","Males")) +
  labs(fill = "Gender")
box_total_time_sex


dodge <- position_dodge(width = 0.1)
ggplot(data = time_analysis_item_filter_cut, mapping = aes(x = group, y = total_time, color = pohlavi),position = dodge) +
  geom_point(stat = "summary", fun = mean, position = dodge) +
  geom_line(stat = "summary", fun = mean, aes(group = pohlavi), position = dodge) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_boot, position = dodge) + 
  labs(y = "Mean duration", x = "Group", color = "Gender") +
  scale_x_discrete(labels=c('VQf', 'VQm', 'TQ')) + 
  scale_color_manual(values = c("#c9344d", "#0000cd"),
                     labels = c("Females", "Males"))








#CRITERION VALIDITY - HEIGHT INVENTORY----
mat_correl <- datamatrix_full_rec %>% filter(!fraud) #only valid responses

#mean score of Height inventory if no less than 2 are NAs
mat_correl$mean_vyska <- ifelse(rowSums(is.na(mat_correl[, c("x1.1", "x1.2", "x1.3", "x1.4", "x1.5", "x1.6", "x1.7", "x1.8", "x1.9", "x1.10", "x1.11")])) > 2 | mat_correl$sl_vyska, NA, 
                                rowMeans(mat_correl[, c("x1.1", "x1.2", "x1.3", "x1.4", "x1.5", "x1.6", "x1.7", "x1.8", "x1.9", "x1.10", "x1.11")], na.rm = T))

#mean score of RSES if no less than 1 is NA
mat_correl$mean_rses <- ifelse(rowSums(is.na(mat_correl[, c("x2.1", "x2.2", "x2.3", "x2.4", "x2.5", "x2.6", "x2.7", "x2.8", "x2.9", "x2.10")])) > 1 | mat_correl$sl_rses, NA, 
                               rowMeans(mat_correl[, c("x2.1", "x2.2", "x2.3", "x2.4", "x2.5", "x2.6", "x2.7", "x2.8", "x2.9", "x2.10")], na.rm = TRUE))

#Only male/female respondents for Height Inventory
mat_correl_filter <- mat_correl %>% filter(pohlavi != "Jiné")
mat_correl_filter <- mat_correl_filter %>%
  mutate(vyska = ifelse(vyska == 130, NA, vyska)) #unlikely small height (130 cm) - set to NA

(nrow(mat_correl_filter))
(nrow(mat_correl))

#preparing regression
mat_regr <- select(mat_correl_filter, pohlavi, mean_vyska, verze, vyska, group)
mat_regr <- na.omit(mat_regr)
mat_regr$vyska_c <- mat_regr$vyska - mean(mat_regr$vyska)
mat_regr$group <- relevel(mat_regr$group, ref = "text")
mat_regr$pohlavi <- relevel(mat_regr$pohlavi, ref = "Žena")
mat_regr$verze <- relevel(mat_regr$verze, ref = "text")


##. Hierarchical regression ATTEMPT 1 ----
model_list2 <- list(
  "Height prediction" = lm(data = mat_regr, mean_vyska ~  pohlavi + vyska_c*pohlavi),
  "Effect of method" = lm(data = mat_regr, mean_vyska ~  pohlavi + vyska_c*pohlavi + verze),
  "Method interactions" = lm(data = mat_regr, mean_vyska ~ pohlavi + vyska_c*pohlavi + verze + verze*vyska_c+verze*pohlavi)
)

# TABLE
modelsummary(model_list2, shape = term ~ model + statistic,
             estimate = "{estimate}{stars}",
             statistic = c("std.error",
                           "statistic",
                           "p.value"),
             coef_rename = c('pohlaviMuž' = 'Gender(M)', 
                             "vyska_c" = "Height",
                             "verzevideo" = "Method(VQ)"),
             fmt = fmt_decimal(digits = 2, pdigits = 3))

#F tests for increments in R2
anova(model_list2$`Height prediction`,model_list2$`Effect of method`, model_list2$`Method interactions`)

##. Assumtion checks, outliers ----
vif(model_list2$`Method interactions`)

plot(model_list2$`Method interactions`)

avPlots(model_list2$`Method interactions`)

ggplot(mat_regr, aes(x = vyska_c, y = mean_vyska, color = verze)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Reported height (centered)", y = "Mean score of height inventory", color = "Method") +
  scale_color_manual(values = c("black", "#50C878"), label = c("TQ","VQ"))


##. Hierarchical regression FINAL ----

#Dropped cases with Cook's distance greater than 0.04
mat_regr2 <- mat_regr
mat_regr2$coo <- cooks.distance(model_list2$`Method interactions`)
mat_regr2$outlier <- ifelse(mat_regr2$coo > 0.04, "YES", "NO")
mat_regr2_filter <- filter(mat_regr2, coo <= 0.04)

#visualization
ggplot(mat_regr2, aes(coo)) + geom_boxplot() +
  labs(x = "Cook's distance") +   
  scale_y_continuous(breaks = c())

#flagging outliers for graph output
mat_regr2$verze2 <- ifelse(mat_regr2$coo > 0.04, "YES", "NO")
mat_regr2$verze2[mat_regr2$verze2 == "NO"] <- as.character(mat_regr2$verze[mat_regr2$verze2 == "NO"])
mat_regr2$verze2 <- factor(mat_regr2$verze2)

#graph output
ggplot(mat_regr2, aes(x = vyska_c, y = mean_vyska, color = verze, shape = verze2)) +
  geom_point() +
  geom_smooth(data = subset(mat_regr2, verze2 == "video"), aes(group = verze), method = "lm", se = FALSE) +
  geom_smooth(data = subset(mat_regr2, verze2 == "text"), aes(group = verze), method = "lm", se = FALSE) +
  labs(x = "Reported height (centered)", y = "Mean score of the height inventory", shape = "Version") +
  scale_color_manual(values = c("black","#0000cd"), label = c("TQ","VQ")) +
  scale_shape_manual(values = c(1, 16, 4),label = c("TQ","VQ","Outlier")) +
  guides(color = "none")


nrow(mat_regr)
nrow(mat_regr2_filter)


model_list2.outliers <- list(
  "Height prediction" = lm(data = mat_regr2_filter, mean_vyska ~  pohlavi + vyska_c*pohlavi),
  "Effect of method" = lm(data = mat_regr2_filter, mean_vyska ~  pohlavi + vyska_c*pohlavi + verze),
  "Method interactions" = lm(data = mat_regr2_filter, mean_vyska ~ pohlavi + vyska_c*pohlavi + verze + verze*vyska_c+verze*pohlavi)
)

#TABLE
modelsummary(model_list2.outliers, shape = term ~ model + statistic,
             estimate = "{estimate}{stars}",
             statistic = c("std.error",
                           "statistic",
                           "p.value"),
             coef_rename = c('pohlaviMuž' = 'Gender(M)', 
                             "vyska_c" = "Height",
                             "verzevideo" = "Method(VQ)"),
             fmt = fmt_decimal(digits = 2, pdigits = 3))

#F tests for increments in R2
anova(model_list2.outliers$`Height prediction`,model_list2.outliers$`Effect of method`, model_list2.outliers$`Method interactions`)









#SCALE ITEM ANALYSIS----
mat_pol <- datamatrix_full_rec %>% filter(!fraud)


mat_pol_vyska <- mat_pol %>% filter(!sl_vyska)
mat_pol_rses <- mat_pol %>% filter(!sl_rses)

##. Scales descriptives----
describe(select(mat_pol_vyska, starts_with("x1")))
describe(select(mat_pol_rses, starts_with("x2")))

describeBy(select(mat_pol_vyska, starts_with("x1"), verze), group = mat_pol_vyska$verze)
describeBy(select(mat_pol_rses, starts_with("x2"), verze), group = mat_pol_rses$verze)




##. Height inventory Item comparison ####
table_descr_vyska_video <- mat_pol_vyska %>% filter(verze == "video") %>% select(starts_with("x1")) %>% describe()
table_descr_vyska_text <- mat_pol_vyska %>% filter(verze == "text") %>% select(starts_with("x1")) %>% describe()


combined_descr_video <- rbind(table_descr_vyska_video, table_descr_vyska_text)
combined_descr_video$verze <- rep(c("video", "text"), each = 11)

ggplot(combined_descr_video, aes(x = vars, y = mean, fill = verze)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "Item", y = "Mean score", fill = "Method") +
  scale_fill_manual(values = c("grey40", "#50C878")) + 
  coord_cartesian(ylim = c(0,3)) +
  scale_x_continuous(breaks = 1:11) +
  scale_y_continuous(expand = c(0, 0))


##. Social Desirability - RSES item comparison####
table_descr_rses_video <- mat_pol_rses %>% filter(verze == "video") %>% select(starts_with("x2")) %>% describe()
table_descr_rses_text <- mat_pol_rses %>% filter(verze == "text") %>% select(starts_with("x2")) %>% describe()

combined_descr_rses <- rbind(table_descr_rses_video, table_descr_rses_text)
combined_descr_rses$verze <- rep(c("video", "text"), each = 10)

#visual comparison
ggplot(combined_descr_rses, aes(x = vars, y = mean, fill = verze)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "Item", y = "Mean score", fill = "Method") +
  scale_fill_manual(values = c("grey40", "#50C878")) + 
  coord_cartesian(ylim = c(0,3)) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(expand = c(0, 0))


#statistical comparison
rses_table_wilcox <- data.frame(Item = character(), W = numeric(), p_value = numeric())

for (i in 1:10) {
  item_name <- paste0("x2.", i)
  test_result <- wilcox.test(as.formula(paste(item_name, "~ verze")), data = mat_pol_rses)
  rses_table_wilcox[i, ] <- c(item_name, test_result$statistic, test_result$p.value)
}

rses_table_wilcox #table of Wilcox R-S tests for each item

wilcox.test(mat_pol_rses$x2.1 ~ mat_pol_rses$verze) #most significant difference
rank_biserial(mat_pol_rses$x2.1 ~ mat_pol_rses$verze) #effect size



##. Scale means distribution----
mat_correl <- datamatrix_full_rec %>% filter(!fraud)

#prumer kdyz !straightlining nebo pocet NA v radku neni vyssi nez 2/1
mat_correl$mean_vyska <- ifelse(rowSums(is.na(mat_correl[, c("x1.1", "x1.2", "x1.3", "x1.4", "x1.5", "x1.6", "x1.7", "x1.8", "x1.9", "x1.10", "x1.11")])) > 2 | mat_correl$sl_vyska, NA, 
                                rowMeans(mat_correl[, c("x1.1", "x1.2", "x1.3", "x1.4", "x1.5", "x1.6", "x1.7", "x1.8", "x1.9", "x1.10", "x1.11")], na.rm = T))

mat_correl$mean_rses <- ifelse(rowSums(is.na(mat_correl[, c("x2.1", "x2.2", "x2.3", "x2.4", "x2.5", "x2.6", "x2.7", "x2.8", "x2.9", "x2.10")])) > 1 | mat_correl$sl_rses, NA, 
                               rowMeans(mat_correl[, c("x2.1", "x2.2", "x2.3", "x2.4", "x2.5", "x2.6", "x2.7", "x2.8", "x2.9", "x2.10")], na.rm = TRUE))


mat_correl_filter <- mat_correl %>% filter(pohlavi != "Jiné")
mat_correl_filter <- mat_correl_filter %>%
  mutate(vyska = ifelse(vyska == 130, NA, vyska))


descr_mean_v <- describe(mat_correl_filter$mean_vyska)
descr_mean_rses <- describe(mat_correl$mean_rses)

descr_vyska <- describeBy(mat_correl_filter$vyska, mat_correl_filter$pohlavi, mat=T)
descr_mean_vyska_sex <- describeBy(mat_correl_filter$mean_vyska, mat_correl_filter$pohlavi, mat=T)

nrow(mat_correl_filter)
nrow(mat_correl)


#RSES mean scores distribution
ggplot(data = mat_correl, aes(x = mean_rses)) +
  geom_histogram(aes(y= after_stat(density)), binwidth = 0.2, color = "black", fill = "grey") +
  labs(x = "Mean RSES", y="Density") +
  stat_function(fun = dnorm, args = list(mean = mean(mat_correl$mean_rses, na.rm = TRUE), 
                                         sd = sd(mat_correl$mean_rses, na.rm = TRUE)), 
                color = "blue", linewidth = 1)



ggplot(mat_correl_filter, aes(mean_vyska, fill = pohlavi)) + 
  geom_histogram(alpha = 0.5, aes(y = after_stat(density)), 
                 position = 'identity', binwidth = 0.1) +
  labs(x = "Mean Height", y = "Density", fill = "Gender") +
  scale_fill_manual(values = c("#C41E3A", "#008AD8"), labels = c("Females", "Males"))



library(ggpubr)
mat_correl_filter <- mutate(mat_correl_filter, 
                            gender = case_when(
                              pohlavi == "Muž" ~ "Male",
                              pohlavi == "Žena" ~ "Female"
                            ))

#distribution for height inventory mean scores by gender
gghistogram(mat_correl_filter, x = "mean_vyska", y = "density",
            add = "mean", rug = F,
            color = "gender", fill = "gender",
            palette = c("#C41E3A", "#008AD8"), add_density = T,
            ylab = "Density", xlab = "Mean Height inventory score", 
            legend.title = "Gender", bins = 18)


#distribution for reported height by gender
gghistogram(mat_correl_filter, x = "vyska", y = "density",
            add = "mean", rug = F,
            color = "gender", fill = "gender",
            palette = c("#C41E3A", "#008AD8"), add_density = T, 
            ylab = "Density", xlab = "Height/cm", legend.title = "Gender",
            bins = 18)



##. Alpha reliability----

#NOTE: For omega reliability coefficients computation, see: 
# STRUCTURAL MODELING RSES and STRUCTURAL MODELING HEIGHT chapters

#female respondents HEIGHT
table_omega_vyska <- mat_pol_vyska %>% filter(pohlavi == "Žena") %>%
  select(starts_with("x1")) %>%
  omega()
table_omega_vyska

table_omega_vyska_text <- mat_pol_vyska %>% filter(pohlavi == "Žena") %>%
  filter(verze == "text") %>%
  select(starts_with("x1")) %>%
  omega()
table_omega_vyska_text

table_omega_vyska_video <- mat_pol_vyska %>% filter(pohlavi == "Žena") %>%
  filter(verze == "video") %>%
  select(starts_with("x1")) %>%
  omega()
table_omega_vyska_video




#male respondents HEIGHT
table_omega_vyska_m <- mat_pol_vyska %>% filter(pohlavi == "Muž") %>%
  select(starts_with("x1")) %>%
  omega()
table_omega_vyska_m

table_omega_vyska_text_m <- mat_pol_vyska %>% filter(pohlavi == "Muž") %>%
  filter(verze == "text") %>%
  select(starts_with("x1")) %>%
  omega()
table_omega_vyska_text_m

table_omega_vyska_video_m <- mat_pol_vyska %>% filter(pohlavi == "Muž") %>%
  filter(verze == "video") %>%
  select(starts_with("x1")) %>%
  omega()
table_omega_vyska_video_m





#RSES
table_omega_rses <- mat_pol_rses %>%
  select(starts_with("x2")) %>%
  omega()
table_omega_rses

table_omega_rses_text <- mat_pol_rses %>%
  filter(verze == "text") %>%
  select(starts_with("x2")) %>%
  omega()
table_omega_rses_text

table_omega_rses_video <- mat_pol_rses %>%
  filter(verze == "video") %>%
  select(starts_with("x2")) %>%
  omega()
table_omega_vyska_video



#COMPARISON TABLES

#females height
table_alpha_vyska <- data.frame(
  Group = c("Combined","Text","Video"),
  Alfa = c(table_omega_vyska$alpha, table_omega_vyska_text$alpha, table_omega_vyska_video$alpha))
table_alpha_vyska

#males height
table_alpha_vyska_m <- data.frame(
  Skupina = c("Combined","Text","Video"),
  Alfa = c(table_omega_vyska_m$alpha, table_omega_vyska_text_m$alpha, table_omega_vyska_video_m$alpha), 
  Omega = c(table_omega_vyska_m$omega.tot, table_omega_vyska_text_m$omega.tot, table_omega_vyska_video_m$omega.tot))
table_alpha_vyska_m

#RSES
table_alpha_rses <- data.frame(
  Skupina = c("Combined","Text","Video"),
  Alfa = c(table_omega_rses$alpha, table_omega_rses_text$alpha, table_omega_rses_video$alpha))
table_alpha_rses









#SOCIAL DESIRABILITY - BODY IMAGE----

mat_vaha <- filter(datamatrix_full_rec, pohlavi !="Jiné", !fraud)

mat_vaha <- mutate(mat_vaha, 
                        gender = case_when(
                        pohlavi == "Muž" ~ "Male",
                        pohlavi == "Žena" ~ "Female"
                        ))

table_vaha <- mat_vaha %>%
  mutate(group = factor(group, labels = c("VQf","VQm","TQ"))) %>%
  group_by(gender, group) %>%
  summarize(
    n = n(),
    m = mean(vaha, na.rm = T)
  )
table_vaha

(desc_vaha <- describeBy(mat_vaha$vaha, mat_vaha$gender, mat = T))


mat_vaha_zeny <- filter(mat_vaha, pohlavi == "Žena")
mat_vaha_muzi <- filter(mat_vaha, pohlavi == "Muž")

ggplot(mat_vaha, aes(x = vaha)) + 
  geom_density(aes(color = group), bw = 1) +
  facet_wrap(~pohlavi, nrow = 2, labeller = labeller(pohlavi = c("Muž" = "Males", "Žena" = "Females"))) +
  scale_color_manual(values = c("#C41E3A", "#008AD8", "grey20"), 
                     name = "Condition",
                     labels = c("VQf", "VQm", "TQ")) +
  labs(y = "Density", x = "Response option", pohlavi = "Gender") + 
  scale_x_continuous(limits = c(1, 10), breaks = seq(1, 10, 1))


histogra_vaha <- gghistogram(mat_vaha_zeny, x = "vaha", y = "density",
                             rug = F,
                             color = "verze", fill = "verze",
                             palette = c("#50C878", "grey20"), add_density = F,
                             ylab = "Density", xlab = "Response option", 
                             legend.title = "Version", bins = 10, binwidth = 1)

ggpar(histogra_vaha, xticks.by = 1, xlim = c(0.5,10))

histogra_vaha +
  scale_x_continuous(
    breaks = 1:10,
    labels = LETTERS[1:10]
  )

#Fishers exact tests
mat_vaha_muzi_vid <- filter(mat_vaha_muzi, verze == "video")
mat_vaha_zeny_vid <- filter(mat_vaha_zeny, verze == "video")

#females - VQm x VQf
fisher.test(
  mat_vaha_zeny_vid$vaha, 
  mat_vaha_zeny_vid$group)

#females - video x text
fisher.test(
  mat_vaha_zeny$vaha, 
  mat_vaha_zeny$verze)


#males - VQm x VQf
fisher.test(
  mat_vaha_muzi_vid$vaha, 
  mat_vaha_muzi_vid$group)

#males - text x video
fisher.test(
  mat_vaha_muzi$vaha, 
  mat_vaha_muzi$verze)










#ENJOYMENT AND FOCUS----
mat_eng <- filter(datamatrix_full_rec, !fraud)

mat_eng <- mat_eng %>% mutate(eng1 = recode(eng1, "1=0; 2=1; 3=2; 4=3"))
mat_eng <- mat_eng %>% mutate(eng2 = recode(eng2, "1=0; 2=1; 3=2; 4=3"))




descr_eng <- describeBy(mat_eng$eng1, group = mat_eng$verze, mat = T)
descr_eng


wilcox.test(eng1~ verze,
            data = mat_eng,
            exact = FALSE, alternative = "g")

effectsize::rank_biserial(mat_eng$eng1, mat_eng$verze)


eng_table <- table(mat_eng$verze, mat_eng$eng1)
table_fisher_eng1 <- fisher.test(mat_eng$eng1, mat_eng$verze)
table_fisher_eng1

ggplot(mat_eng, aes(x = eng1)) + 
  geom_bar(aes(y = after_stat(prop), group = 1)) + 
  facet_wrap(~group)

ggplot(mat_eng, aes(x = eng1)) + 
  geom_bar(aes(y = after_stat(prop), group = 1)) + 
  facet_wrap(~verze) +
  scale_y_continuous(labels = scales::percent_format())

freq_df <- as.data.frame(table(mat_eng$verze, mat_eng$eng1))
names(freq_df) <- c("group", "response_option", "frequency")
freq_df <- freq_df %>% mutate(response_option = recode(response_option, "0=3; 1=2; 2=1; 3=0"))
freq_df$response_option <- factor(freq_df$response_option)
ggplot(freq_df, aes(x = group, y = frequency, fill = response_option)) +
  geom_col(position = "fill", width = 0.9) +
  scale_fill_manual(values = c("#388c54","#84d8a0","#c9344d","#891528"),
                    labels = c("Strongly agree",
                               "Agree",
                               "Disagree",
                               "Strongly disagree")) +
  labs(x = "Method", y = "Frequency", fill = "Response") + coord_flip() +
  scale_x_discrete(labels = c("VQ", "TQ"))

##. Enjoyment-Aversion to Reading----
mat_eng$cteni2 <- ifelse(mat_eng$cteni == 1 | mat_eng$cteni == 2, FALSE, TRUE)

mat_eng_spok_cteni <- filter(mat_eng, verze == "video")
describeBy(mat_eng_spok_cteni$eng1, group = mat_eng_spok_cteni$cteni2)
table(mat_eng_spok_cteni$eng1, mat_eng_spok_cteni$cteni2)

#kendalls tau
corr.test(mat_eng_spok_cteni$eng1, mat_eng_spok_cteni$cteni, method = "kendall")
cor.test(mat_eng_spok_cteni$eng1, mat_eng_spok_cteni$cteni, method = "kendall")




##. Focus----

#Subjective
table_eng2 <- mat_eng %>%
  group_by(verze) %>%
  summarize(
    n = n(),
    m = mean(eng2, na.rm = T)
  )
table_eng2

describeBy(mat_eng$eng2, group = mat_eng$verze)
describeBy(mat_eng$eng2, group = mat_eng$group)


table(mat_eng$eng2, mat_eng$verze)
table_fisher_eng2 <- fisher.test(mat_eng$eng2, mat_eng$verze)
table_fisher_eng2

table(mat_eng$eng2, mat_eng$group)


ggplot(mat_eng, aes(x = eng2)) + 
  geom_bar(aes(y = after_stat(prop), group = 1, fill = group)) + 
  facet_wrap(~group, 
             labeller = labeller(group = c("video_z" = "VQf", "video_m" = "VQm", "text" = "TQ"))) + 
  labs(y = "Frequency", x = "Response") +
  scale_fill_manual(values = c("#96deae","#388c54","grey40")) + guides(fill = F)


ggplot(mat_eng, aes(x = eng2)) + 
  geom_bar(aes(y = after_stat(prop), group = 1, fill = verze)) + 
  facet_wrap(~verze, 
             labeller = labeller(verze = c("video" = "VQ", "text" = "TQ"))) +
  labs(y = "Frequency", x = "Response") +
  scale_fill_manual(values = c("#50C878","grey40")) + guides(fill = F)



wilcox.test(eng2~ verze,
            data = mat_eng,
            exact = FALSE, alternative = "g")
effectsize::rank_biserial(mat_eng$eng2, mat_eng$verze, alternative = "g")



#Objective
mat_window <- select(mat_eng, windowCount, windowCount2, verze)
mat_window$win_exp <- mat_window$windowCount - mat_window$windowCount2

table_window <- mat_window %>%
  group_by(verze) %>%
  summarize(
    n = n(),
    M_okno1 = mean(win_exp, na.rm = T),
  )
table_window

table(mat_window$verze, mat_window$win_exp)
table_fisher_window <- fisher.test(mat_window$verze, mat_window$win_exp)
table_fisher_window













#STRUCTURAL MODELING RSES----
##. Rses factor analysis----

faktory_rses <- 
  
  '
f1 =~ x2.2 + x2.5 + x2.7 + x2.9 + x2.10
f2 =~ x2.6 + x2.8 + x2.4 + x2.1 + x2.3
              
' 


#filter out invalid
cfa_rses <- datamatrix_full_rec %>% filter(!fraud, !sl_rses) %>% select(verze, starts_with("x2"))


cfa_rses_model <- sem(model = faktory_rses, 
                      data = cfa_rses, 
                      orthogonal = FALSE, ordered = T,
                      missing = "pairwise", std.lv = T, estimator = "WLSMV")

#only text
cfa_rses_t_model <- sem(model = faktory_rses, data = cfa_rses %>% filter(verze == "text"), 
                        orthogonal = FALSE, ordered = T,
                        missing = "pairwise", std.lv = T, estimator = "WLSMV")
#only video
cfa_rses_v_model <- sem(model = faktory_rses, data = cfa_rses %>% filter(verze == "video"), 
                        orthogonal = FALSE, ordered = T,
                        missing = "pairwise", std.lv = T, estimator = "WLSMV")


summary(cfa_rses_model, fit.measures = T)
summary(cfa_rses_t_model, fit.measures = T)
summary(cfa_rses_v_model, fit.measures = T)


fitMeasures(cfa_rses_model, fit.measures =  
              c("RMSEA", "TLI", "SRMR"))

fitMeasures(cfa_rses_v_model, fit.measures =  
              c("RMSEA", "TLI", "SRMR"))

fitMeasures(cfa_rses_t_model, fit.measures =  
              c("RMSEA", "TLI", "SRMR"))


compRelSEM(cfa_rses_model, tau.eq = F, return.total = T)
compRelSEM(cfa_rses_t_model, tau.eq = F, return.total = T)
compRelSEM(cfa_rses_v_model, tau.eq = F, return.total = T)


##. Rses invariance----


rses_conf <- measEq.syntax(faktory_rses, cfa_rses, ordered = T, missing = "pairwise", std.lv = T, 
                           estimator = "WLSMV", parameterization = "theta", group = "verze", 
                           ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T)

rses_th <- measEq.syntax(faktory_rses, cfa_rses, ordered = T, missing = "pairwise", std.lv = T, 
                         estimator = "WLSMV", parameterization = "theta", group = "verze", 
                         ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T, 
                         group.equal = c("thresholds"))

rses_metr <- measEq.syntax(faktory_rses, cfa_rses, ordered = T, missing = "pairwise", std.lv = T, 
                           estimator = "WLSMV", parameterization = "theta", group = "verze", 
                           ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T, 
                           group.equal = c("thresholds", "loadings"))

rses_scal <- measEq.syntax(faktory_rses, cfa_rses, ordered = T, missing = "pairwise", std.lv = T, 
                           estimator = "WLSMV", parameterization = "theta", group = "verze", 
                           ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T, 
                           group.equal = c("thresholds", "loadings", "intercepts"))

rses_resid <- measEq.syntax(faktory_rses, cfa_rses, ordered = T, missing = "pairwise", std.lv = T, 
                            estimator = "WLSMV", parameterization = "theta", group = "verze", 
                            ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T, 
                            group.equal = c("thresholds", "loadings", "intercepts", "residuals"))

rses_means <- measEq.syntax(faktory_rses, cfa_rses, ordered = T, missing = "pairwise", std.lv = T, 
                            estimator = "WLSMV", parameterization = "theta", group = "verze", 
                            ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T, 
                            group.equal = c("thresholds", "loadings", "intercepts", "residuals", 
                                            "means"))

rses_var <- measEq.syntax(faktory_rses, cfa_rses, ordered = T, missing = "pairwise", std.lv = T, 
                          estimator = "WLSMV", parameterization = "theta", group = "verze", 
                          ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T, 
                          group.equal = c("thresholds", "loadings", "intercepts", "residuals",
                                          "means","lv.variances"))

rses_covar <- measEq.syntax(faktory_rses, cfa_rses, ordered = T, missing = "pairwise", std.lv = T, 
                            estimator = "WLSMV", parameterization = "theta", group = "verze", 
                            ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T, 
                            group.equal = c("thresholds", "loadings", "intercepts", "residuals", 
                                            "means","lv.variances","lv.covariances"))

rses_fit1 <- compareFit(rses_conf, rses_th, rses_metr, rses_scal, rses_resid, rses_means, rses_var, rses_covar)

summary(rses_fit1, fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                                    "tli.scaled", 
                                    "rmsea.scaled", "rmsea.ci.lower.scaled","rmsea.ci.upper.scaled", 
                                    "srmr"))











#STRUCTURAL MODELING HEIGHT: FEMALES----
##. Height:females factor analysis----

faktory_vyska <- 'f1 =~ x1.1 + x1.3 + x1.5 + x1.8 + x1.9 + x1.10
                  f2 =~ x1.2 + x1.4 + x1.6 + x1.7 + x1.11'


cfa_vyska <- datamatrix_full_rec %>% filter(!fraud, !sl_vyska) %>% select(verze, pohlavi, starts_with("x1"))



cfa_vyska_zeny <- filter(cfa_vyska, pohlavi == "Žena")


cfa_vyska_model <- sem(model = faktory_vyska, 
                       data = cfa_vyska, 
                       orthogonal = FALSE, ordered = T,
                       missing = "pairwise", std.lv = T, estimator = "WLSMV")

cfa_vyska_zeny_model <- sem(model = faktory_vyska, 
                            data = cfa_vyska_zeny, 
                            orthogonal = FALSE, ordered = T,
                            missing = "pairwise", std.lv = T, estimator = "WLSMV")

cfa_vyska_t_z_model <- sem(model = faktory_vyska, data = cfa_vyska_zeny %>% filter(verze == "text"), 
                           orthogonal = FALSE, ordered = T,
                           missing = "pairwise", std.lv = T, estimator = "WLSMV")

cfa_vyska_v_z_model <- sem(model = faktory_vyska, data = cfa_vyska_zeny %>% filter(verze == "video"), 
                           orthogonal = FALSE, ordered = T,
                           missing = "pairwise", std.lv = T, estimator = "WLSMV")


summary(cfa_vyska_model, fit.measures = T)

summary(cfa_vyska_zeny_model, fit.measures = T)
summary(cfa_vyska_t_z_model, fit.measures = T)
summary(cfa_vyska_v_z_model, fit.measures = T)



fitMeasures(cfa_vyska_zeny_model, fit.measures =  
              c("RMSEA", "TLI", "SRMR"))

fitMeasures(cfa_vyska_t_z_model, fit.measures =  
              c("RMSEA", "TLI", "SRMR"))

fitMeasures(cfa_vyska_v_z_model, fit.measures =  
              c("RMSEA", "TLI", "SRMR"))


##. Omega rel. height:females----
compRelSEM(cfa_vyska_model, tau.eq = F, return.total = T)

compRelSEM(cfa_vyska_zeny_model, tau.eq = F, return.total = T)
compRelSEM(cfa_vyska_t_z_model, tau.eq = F, return.total = T)
compRelSEM(cfa_vyska_v_z_model, tau.eq = F, return.total = T)


##. Height:females invariance----

vyska_conf <- measEq.syntax(faktory_vyska, cfa_vyska_zeny, ordered = T, missing = "pairwise", std.lv = T, 
                            estimator = "WLSMV", parameterization = "theta", group = "verze", 
                            ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T)

vyska_th <- measEq.syntax(faktory_vyska, cfa_vyska_zeny, ordered = T, missing = "pairwise", std.lv = T, 
                          estimator = "WLSMV", parameterization = "theta", group = "verze", 
                          ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T, 
                          group.equal = c("thresholds"))

vyska_metr <- measEq.syntax(faktory_vyska, cfa_vyska_zeny, ordered = T, missing = "pairwise", std.lv = T, 
                            estimator = "WLSMV", parameterization = "theta", group = "verze", 
                            ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T, 
                            group.equal = c("thresholds", "loadings"))

vyska_scal <- measEq.syntax(faktory_vyska, cfa_vyska_zeny, ordered = T, missing = "pairwise", std.lv = T, 
                            estimator = "WLSMV", parameterization = "theta", group = "verze", 
                            ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T, 
                            group.equal = c("thresholds", "loadings", "intercepts"))

vyska_resid <- measEq.syntax(faktory_vyska, cfa_vyska_zeny, ordered = T, missing = "pairwise", std.lv = T, 
                             estimator = "WLSMV", parameterization = "theta", group = "verze", 
                             ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T, 
                             group.equal = c("thresholds", "loadings", "intercepts", "residuals"))

vyska_means <- measEq.syntax(faktory_vyska, cfa_vyska_zeny, ordered = T, missing = "pairwise", std.lv = T, 
                             estimator = "WLSMV", parameterization = "theta", group = "verze", 
                             ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T, 
                             group.equal = c("thresholds", "loadings", "intercepts", "residuals", 
                                             "means"))

vyska_var <- measEq.syntax(faktory_vyska, cfa_vyska_zeny, ordered = T, missing = "pairwise", std.lv = T, 
                           estimator = "WLSMV", parameterization = "theta", group = "verze", 
                           ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T, 
                           group.equal = c("thresholds", "loadings", "intercepts", "residuals", 
                                           "means","lv.variances"))

vyska_covar <- measEq.syntax(faktory_vyska, cfa_vyska_zeny, ordered = T, missing = "pairwise", std.lv = T, 
                             estimator = "WLSMV", parameterization = "theta", group = "verze", 
                             ID.fac = "UV", ID.cat = "Wu2016", return.fit = T, verbose = T, 
                             group.equal = c("thresholds", "loadings", "intercepts", "residuals", 
                                             "means","lv.variances","lv.covariances"))

summary(compareFit(vyska_conf, vyska_th, vyska_metr, vyska_scal, vyska_resid))


rses_fit2 <- compareFit(vyska_conf, vyska_th, vyska_metr, vyska_scal, vyska_resid, vyska_means, vyska_var, vyska_covar)

summary(rses_fit2, fit.measures = c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                                    "tli.scaled", 
                                    "rmsea.scaled", "rmsea.ci.lower.scaled","rmsea.ci.upper.scaled", 
                                    "srmr"))


#STRUCTURAL MODELING HEIGHT: MALES----
##. Height:males factor analysis----

faktory_vyska <- 'f1 =~ x1.1 + x1.3 + x1.5 + x1.8 + x1.9 + x1.10
                  f2 =~ x1.2 + x1.4 + x1.6 + x1.7 + x1.11'


cfa_vyska <- datamatrix_full_rec %>% filter(!fraud, !sl_vyska) %>% select(verze, pohlavi, starts_with("x1"))

cfa_vyska_muzi<- filter(cfa_vyska, pohlavi == "Muž")

cfa_vyska_muzi_model <- sem(model = faktory_vyska, 
                            data = cfa_vyska_muzi, 
                            orthogonal = FALSE, ordered = T,
                            missing = "pairwise", std.lv = T, estimator = "WLSMV")

cfa_vyska_t_m_model <- sem(model = faktory_vyska, data = cfa_vyska_muzi %>% filter(verze == "text"), 
                           orthogonal = FALSE, ordered = T,
                           missing = "pairwise", std.lv = T, estimator = "WLSMV")

cfa_vyska_v_m_model <- sem(model = faktory_vyska, data = cfa_vyska_muzi %>% filter(verze == "video"), 
                           orthogonal = FALSE, ordered = T,
                           missing = "pairwise", std.lv = T, estimator = "WLSMV")



fitMeasures(cfa_vyska_muzi_model, fit.measures =  
              c("RMSEA", "TLI", "SRMR"))

fitMeasures(cfa_vyska_t_m_model, fit.measures =  
              c("RMSEA", "TLI", "SRMR"))

fitMeasures(cfa_vyska_v_m_model, fit.measures =  
              c("RMSEA", "TLI", "SRMR"))



##. Omega rel. height:females----

compRelSEM(cfa_vyska_muzi_model, tau.eq = F, return.total = T)
compRelSEM(cfa_vyska_t_m_model, tau.eq = F, return.total = T)
compRelSEM(cfa_vyska_v_m_model, tau.eq = F, return.total = T)







#ALL MODELS FITMEASURES----
#NOTE: Requires initiation of previous sections: STRUCTURAL MODELING RSES/HEIGHT

models <- list(cfa_rses_t_model, cfa_rses_v_model, cfa_vyska_t_z_model, cfa_vyska_v_z_model, cfa_vyska_t_m_model, cfa_vyska_v_m_model)

fit_measures <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "tli.scaled", "srmr")

fit_measures_list <- lapply(models, function(model) fitMeasures(model, fit.measures = fit_measures, output = "matrix"))

combined_fit_measures <- do.call(cbind, fit_measures_list)

row.names(combined_fit_measures) <- fit_measures

combined_fit_measures









#OMEGA COMPARISON Z-TEST----
##. Bootstrapping standard error----

#list of previous models
set.seed(494112)
n_bootstrap = 2000 #N OF ATTEMTED ITERATIONS
model_list <- list(cfa_rses_t_model, 
                   cfa_rses_v_model, 
                   cfa_vyska_t_z_model, 
                   cfa_vyska_v_z_model, 
                   cfa_vyska_t_m_model, 
                   cfa_vyska_v_m_model)

boot_results <- list()


#BOOTSTRAP - bootstrapLavaan OVER compRelSEM function
for (i in seq_along(model_list)) {
  boot_results[[i]] <- bootstrapLavaan(model_list[[i]], R = n_bootstrap, verbose = TRUE,
                                       type = "nonparametric",
                                       FUN = function(x) {
                                         compRelSEM(x, tau.eq = F, return.total = T)
                                       })
}

#as DF
df_bootresults <- as.data.frame(boot_results)
summary(df_bootresults)


##. Comparison of omega coefficients----

#for all models in one loop
for (j in 1:3) {
  boot_table <- matrix(nrow = length(model_list), ncol = 4)
  colnames(boot_table) <- c("M", "SE", "CImin", "CImax")
  rownames(boot_table) <- c("RSES TQ", "RSES VQ", "Height F VQ", "Height F VQ", "Height M TQ", "Height M VQ")
  
  table_diff_omega <- matrix(nrow = length(model_list)/2, ncol = 4)
  colnames(table_diff_omega) <- c("Delta OMEGA", "SE", "Z", "p")
  rownames(table_diff_omega) <- c("RSES T-V", "HEIGHT(F) T-V", "HEIGHT(M) T-V")
  
  
  #count M, SE, CI
  for (i in seq_along(model_list)) {
    boot_table[i,] <- c(mean(boot_results[[i]][,j], na.rm = T), 
                        sd(boot_results[[i]][,j], na.rm = T), 
                        (mean(boot_results[[i]][,j], na.rm = T) - 1.96*sd(boot_results[[i]][,j], na.rm = T)), 
                        (mean(boot_results[[i]][,j], na.rm = T) + 1.96*sd(boot_results[[i]][,j], na.rm = T)))
    
  }
  
  #delta omega, SE, Z, p
  for(i in 1:3) {
    
    omega1 <- compRelSEM(model_list[[2*i-1]], tau.eq = F, return.total = T)[j]
    omega2 <- compRelSEM(model_list[[2*i]], tau.eq = F, return.total = T)[j]
    
    mean_diff <- abs(omega1 - omega2)
    
    se_diff <- sqrt((boot_table[2*i-1, 2]^2 + boot_table[2*i, 2]^2))
    
    z_stat <- mean_diff / se_diff
    
    p_value <- 2*(1-pnorm(abs(z_stat)))
    
    table_diff_omega[i, ] <- c(mean_diff, se_diff, z_stat, p_value)
  }
  
  assign(paste0("boot_table_", j), boot_table)
  assign(paste0("table_diff_omega_", j), table_diff_omega)
  
}

#bootstrap total/f1/f2
boot_table_3
boot_table_1
boot_table_2

#ztest total/f1/f2
table_diff_omega_3
table_diff_omega_1
table_diff_omega_2








#POWER ANALYSIS FOR INVARIANCE TESTS----
#NOTE: SUPPLEMENTAL ANALYSIS - not included in the article
#NOTE: Requires STRUCTURAL MODELING sections to run



cov.pop.g1 <- fitted(sem(cfa_vyska_v_z_model))$cov
cov.pop.g2 <- fitted(sem(cfa_vyska_t_z_model))$cov

model.h0 <- faktory_vyska

fit.h0 <- sem(model.h0, 
              sample.cov = list(cov.pop.g1, cov.pop.g2),
              sample.nobs = list(1000, 1000), 
              sample.cov.rescale = F,
              group.equal = 'loadings', likelihood='wishart')
# get model implied covariance matrices
cov.h0.g1 <- fitted(fit.h0)$`Group 1`$cov
cov.h0.g2 <- fitted(fit.h0)$`Group 2`$cov
# df of metric invariance model
df <- fit.h0@test[[1]]$df
# obtain baseline df (no invariance constraints)
fit.bl <- sem(model.h0, sample.cov = list(cov.pop.g1, cov.pop.g2),
              sample.nobs = list(1000, 1000), sample.cov.rescale=F,
              likelihood='wishart')
df.bl <- fit.bl@test[[1]]$df

# difference in df
df.diff <- df - df.bl

# a priori power analysis
ap1 <- semPower.aPriori(SigmaHat = list(cov.h0.g1, cov.h0.g2),
                        Sigma = list(cov.pop.g1, cov.pop.g2),
                        alpha = .1, beta = .25, N = list(1, 1), df = df.diff)

# post hoc power analysis
ph1 <- semPower.postHoc(SigmaHat = list(cov.h0.g1, cov.h0.g2),
                        Sigma = list(cov.pop.g1,cov.pop.g2),
                        alpha = .1, N= list(
                          nrow(cfa_vyska_zeny %>% filter(verze == "video")),
                          nrow(cfa_vyska_zeny %>% filter(verze == "text"))), df = df.diff)

summary(ap1)

summary(ph1)


#POWERANAL pro rses
cov.pop.g1 <- fitted(sem(cfa_rses_v_model))$cov
cov.pop.g2 <- fitted(sem(cfa_rses_t_model))$cov

model.h0 <- faktory_rses

fit.h0 <- sem(model.h0, 
              sample.cov = list(cov.pop.g1, cov.pop.g2),
              sample.nobs = list(1000, 1000), 
              sample.cov.rescale = F,
              group.equal = 'loadings', likelihood='wishart')
# get model implied covariance matrices
cov.h0.g1 <- fitted(fit.h0)$`Group 1`$cov
cov.h0.g2 <- fitted(fit.h0)$`Group 2`$cov
# df of metric invariance model
df <- fit.h0@test[[1]]$df
# obtain baseline df (no invariance constraints)
fit.bl <- sem(model.h0, sample.cov = list(cov.pop.g1, cov.pop.g2),
              sample.nobs = list(1000, 1000), sample.cov.rescale=F,
              likelihood='wishart')
df.bl <- fit.bl@test[[1]]$df

# difference in df
df.diff <- df - df.bl

#power analysis a priori
ap2 <- semPower.aPriori(SigmaHat = list(cov.h0.g1, cov.h0.g2),
                        Sigma = list(cov.pop.g1, cov.pop.g2),
                        alpha = .01, beta = .25, N = list(1, 1), df = df.diff)

#power analysis post-hoc
ph2 <- semPower.postHoc(SigmaHat = list(cov.h0.g1, cov.h0.g2),
                        Sigma = list(cov.pop.g1,cov.pop.g2),
                        alpha = .01, N= list(
                          nrow(cfa_rses %>% filter(verze == "video")),
                          nrow(cfa_rses %>% filter(verze == "text"))), df = df.diff)

summary(ap2)

summary(ph2)




#NUMBER OF REPLAYS----
#NOTE: SUPPLEMENTAL ANALYSIS - not included in the article

mat_ended <- datamatrix_full_rec %>%
  filter(verze == "video", !fraud)

stacked1 <- stack(mat_ended[c(paste0("end", 0:11))])

table_counts_ended1 <- table(stacked1)
table_counts_ended1

stacked2 <- stack(mat_ended[c(paste0("end2.", 1:10))])

table_counts_ended2 <- table(stacked2)
table_counts_ended2


mat_ended_compl_1 <- mat_ended %>%
  filter(if_all(starts_with("x1"), ~ !is.na(.)))

mat_ended_compl_2 <- mat_ended %>%
  filter(if_all(starts_with("x2"), ~ !is.na(.)))


stacked_no_na1 <- stack(mat_ended_compl_1[c(paste0("end", 0:11))])

#number of replays in VQ for Height inventory items (JavaScript counter)
table_counts_ended4 <- table(stacked_no_na1)
table_counts_ended4


stacked_no_na2 <- stack(mat_ended_compl_2[c(paste0("end2.", 1:10))])

#number of replays in VQ for RSES items (JavaScript counter)
table_counts_ended5 <- table(stacked_no_na2)
table_counts_ended5




#DURATION PER ITEM PER CONDITION CHARTS----
#NOTE: SUPPLEMENTAL ANALYSIS - not included in the article

time_analysis_item <- select(datamatrix_full, group, verze, pohlavi, duration, ends_with("_S"), fraud)

#filter:not invalid, no NAs, gender male/female
time_analysis_item_filter <- time_analysis_item %>%
  filter(!fraud) %>%
  drop_na() %>% 
  filter(pohlavi!="Jiné")

##. height inventory----
time_analysis_item_filter_long_vyska <- tidyr::pivot_longer(time_analysis_item_filter, starts_with("timer1."), names_to = "polozka", values_to = "time")
time_analysis_item_filter_long_vyska$polozka_num <- as.numeric(gsub("timer1\\.(\\d+)_S", "\\1", time_analysis_item_filter_long_vyska$polozka))

time_analysis_item_filter_long_vyska_cut <- time_analysis_item_filter_long_vyska %>% 
  group_by(polozka_num, group, pohlavi) %>%
  filter(time >= quantile(time, 0.1) & time <= quantile(time, 0.9))

hist_time_comp_vyska <- ggplot(time_analysis_item_filter_long_vyska_cut, aes(x = pohlavi, y = time, fill = group)) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge()) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_boot, position = position_dodge(0.9), width = 0.2) +
  facet_wrap(~polozka_num, ncol = 2, scales = "free_x") +
  labs(x = "Gender", y = "Submission time/s") +
  scale_fill_manual(values = c("#96deae", "#388c54", "grey60"), name = "Group", labels = c("VQf","VQm","TQ")) +
  scale_x_discrete(labels = c("Females", "Males"))
hist_time_comp_vyska + theme_bw()



##. rses----
time_analysis_item_filter_long_rses <- tidyr::pivot_longer(time_analysis_item_filter, starts_with("timer2."), names_to = "polozka", values_to = "time")
time_analysis_item_filter_long_rses$polozka_num <- as.numeric(gsub("timer2\\.(\\d+)_S", "\\1", time_analysis_item_filter_long_rses$polozka))


time_analysis_item_filter_long_rses_cut <- time_analysis_item_filter_long_rses %>% 
  group_by(polozka_num, group, pohlavi) %>%
  filter(time >= quantile(time, 0.1) & time <= quantile(time, 0.9))


hist_time_comp_rses <- ggplot(time_analysis_item_filter_long_rses_cut, aes(x = pohlavi, y = time, fill = group)) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge()) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_boot, position = position_dodge(0.9), width = 0.2) +
  facet_wrap(~polozka_num, ncol = 2, scales = "free_x") +
  labs(x = "Gender", y = "Submission time/s") +
  scale_fill_manual(values = c("#96deae", "#388c54", "grey60"), name = "Group", labels = c("VQf","VQm","TQ")) +
  scale_x_discrete(labels = c("Females", "Males"))
hist_time_comp_rses + theme_bw()

