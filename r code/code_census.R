library(pacman)
p_load(tidyverse, haven, data.table, purrr, readxl, rgeoapi)

setwd("/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4. sciences po/work/semestre 2/serranito - advanced econometrics/serranito - paper/data/")


###############################################################################################################################

immig_90 <- read.csv("census_data/rp90_tableauxstandards.csv/Csv/LR/COMLR14/nat01sc_ano.csv", sep = ";")





###############################################################################################################################

immig_99 <- read.csv("census_data/rp99_tableauxanalyses.csv/Csv/Quart/nat9st.csv", sep = ";") %>% 
  select((1:12), C9)

hlm_99 <- read.csv("census_data/rp99_tableauxanalyses.csv/Csv/Exhaustif/prin3ss.csv", sep = ";")%>%
  select(C45, COM) %>%
  group_by(COM) %>% 
  summarize(hlm_pop = sum(C45)) %>% 
  rename("depcom" = "COM")

emp_99 <- read.csv("census_data/rp99_tableauxanalyses.csv/Csv/Quart/nav2ss.csv", sep = ";") %>% 
  select(C9, C25, C41, C49, COM) %>% 
  group_by(COM) %>% 
  summarize(pop_agriculteurs = sum(C9),
            pop_cadres = sum(C25),
            pop_employes = sum(C41),
            pop_ouvriers = sum(C49)) %>% 
  rename("depcom" = "COM")

retraites_chomeurs_99 <- read.csv("census_data/rp99_tableauxanalyses.csv/Csv/Exhaustif/pop4ess.csv", sep = ";") %>% 
  select(C1, C3, COM) %>% 
  group_by(COM) %>% 
  summarize(pop_retraites = sum(C1),
            pop_chomeurs = sum(C3)) %>% 
  rename("depcom" = "COM")
  
dropouts_99 <- read.csv("census_data/rp99_tableauxanalyses.csv/Csv/Exhaustif/for2ss.csv", sep = ";") %>% 
  select(C4, COM) %>% 
  group_by(COM) %>% 
  summarize(pop_dropouts = sum(C4)) %>% 
  rename("depcom" = "COM")

rp99 = mig1_99 %>% 
  left_join(hlm_99, by = "depcom") %>% 
  left_join(emp_99, by = "depcom") %>% 
  left_join(retraites_chomeurs_99, by = "depcom") %>% 
  left_join(dropouts_99, by = "depcom") %>% 
  ungroup() %>% 
  mutate(immi_prop = commune_pop_immi / commune_pop_total,
         chomeurs_prop = pop_chomeurs / commune_pop_total, 
         hlm_prop = hlm_pop / commune_pop_total,
         ouvriers_prop = pop_ouvriers / commune_pop_total,
         employes_prop = pop_employes / commune_pop_total, 
         retraites_prop = pop_retraites / commune_pop_total, 
         cadres_prop = pop_cadres / commune_pop_total,
         agriculteurs_prop = pop_agriculteurs / commune_pop_total) %>% 
  select(-c(commune_pop_immi, hlm_pop, pop_agriculteurs, pop_cadres, pop_employes,
            pop_ouvriers, pop_retraites, pop_chomeurs, pop_dropouts))

#################################################################################################################################

rp2007 = lapply(list.files("census_data/rp0617/rp2007.csv/Csv/", full.names = TRUE), fread) %>% 
  rbindlist() %>% 
  select(CANTVILLE, ARM, IRIS, IPONDI, IMMI, HLML, TACT, CS1, DIPL) %>%
  mutate(depcom = substr(IRIS, start = 1, stop = 5)) %>% 
  group_by(CANTVILLE, ARM) %>%
  mutate(
    commune_pop_total = n(),
    commune_pop_ponderation = sum(IPONDI)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(individu_immi_ponderated = ifelse(IMMI == 1, IPONDI, 0),
         individu_chomeurs_ponderated = ifelse(TACT == 12, IPONDI, 0),
         individu_hlm_ponderated = ifelse(HLML == 1, IPONDI, 0),
         individu_ouvriers_ponderated = ifelse(CS1 == 6, IPONDI, 0),
         individu_employes_ponderated = ifelse(CS1 == 5, IPONDI, 0),
         individu_retraites_ponderated = ifelse(CS1 == 7, IPONDI, 0),
         individu_cadres_ponderated = ifelse(CS1 == 3, IPONDI, 0),
         individu_agriculteurs_ponderated = ifelse(CS1 == 1, IPONDI, 0),
         individu_dropout_ponderated = ifelse((DIPL < 13), IPONDI, 0)) %>%
  ungroup() %>%
  group_by(CANTVILLE, ARM) %>%
  mutate(commune_immi_ponderated = sum(individu_immi_ponderated), #these are not proportions of the population !
         commune_chomeurs_ponderated = sum(individu_chomeurs_ponderated),
         commune_hlm_ponderated = sum(individu_hlm_ponderated),
         commune_ouvriers_ponderated = sum(individu_ouvriers_ponderated),
         commune_employes_ponderated = sum(individu_employes_ponderated),
         commune_retraites_ponderated = sum(individu_retraites_ponderated),
         commune_cadres_ponderated = sum(individu_cadres_ponderated),
         commune_agriculteurs_ponderated = sum(individu_agriculteurs_ponderated),
         commune_dropout_ponderated = sum(individu_dropout_ponderated)) %>% 
  saveRDS("census_data/rp0617/rp2007_clean.RDS")

#################################################################################################################################

rp2012 = 
  lapply(list.files("census_data/rp0617/rp2012.dta/Stata/", full.names = TRUE), read_dta) %>% 
  lapply(., function(df){
    df %>% 
      select(cantville, arm, iris, immi, ipondi, hlml, tact, cs1, dipl) %>%
      as.data.table()
  }) %>% 
  rbindlist() %>% 
  mutate(depcom = substr(iris, start = 1, stop = 5)) %>% 
  group_by(cantville, arm) %>%
  mutate(commune_pop_total = n(),
         commune_pop_ponderation = sum(ipondi)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(individu_immi_ponderated = ifelse(immi == 1, ipondi, 0),
         individu_chomeurs_ponderated = ifelse(tact == 12, ipondi, 0),
         individu_hlm_ponderated = ifelse(hlml == 1, ipondi, 0),
         individu_ouvriers_ponderated = ifelse(cs1 == 6, ipondi, 0),
         individu_employes_ponderated = ifelse(cs1 == 5, ipondi, 0),
         individu_retraites_ponderated = ifelse(cs1 == 7, ipondi, 0),
         individu_cadres_ponderated = ifelse(cs1 == 3, ipondi, 0),
         individu_agriculteurs_ponderated = ifelse(cs1 == 1, ipondi, 0),
         individu_dropout_ponderated = ifelse((dipl < 13), ipondi, 0)) %>%
  ungroup() %>%
  group_by(cantville, arm) %>%
  mutate(commune_immi_ponderated = sum(individu_immi_ponderated), #these are not proportions of the population !
         commune_chomeurs_ponderated = sum(individu_chomeurs_ponderated),
         commune_hlm_ponderated = sum(individu_hlm_ponderated),
         commune_ouvriers_ponderated = sum(individu_ouvriers_ponderated),
         commune_employes_ponderated = sum(individu_employes_ponderated),
         commune_retraites_ponderated = sum(individu_retraites_ponderated),
         commune_cadres_ponderated = sum(individu_cadres_ponderated),
         commune_agriculteurs_ponderated = sum(individu_agriculteurs_ponderated),
         commune_dropout_ponderated = sum(individu_dropout_ponderated)) %>% 
  ungroup() %>% 
  saveRDS("census_data/rp0617/rp2012_clean.RDS")


#################################################################################################################################

tmp2017 = lapply(list.files("census_data/rp0617/rp2017.csv/Csv/", full.names = TRUE), fread)

tmp2017_1 = tmp2017 %>% 
  rbindlist() %>%
  saveRDS("census_data/rp0617/tmp_2017_1.RDS")

tmp2017_1 = readRDS("census_data/rp0617/tmp_2017_1.RDS") %>% 
  select(CANTVILLE, ARM, IRIS, IPONDI, IMMI, HLML, TACT, CS1, DIPL) 

rp2017 = tmp2017_1 %>%
  as.data.table() %>% 
  group_by(CANTVILLE, ARM) %>%
  mutate(
    commune_pop_total = n(),
    commune_pop_ponderation = sum(IPONDI),
    depcom = substr(IRIS, start = 1, stop = 5)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(individu_immi_ponderated = ifelse(IMMI == 1, IPONDI, 0),
         individu_chomeurs_ponderated = ifelse(TACT == 12, IPONDI, 0),
         individu_hlm_ponderated = ifelse(HLML == 1, IPONDI, 0),
         individu_ouvriers_ponderated = ifelse(CS1 == 6, IPONDI, 0),
         individu_employes_ponderated = ifelse(CS1 == 5, IPONDI, 0),
         individu_retraites_ponderated = ifelse(CS1 == 7, IPONDI, 0),
         individu_cadres_ponderated = ifelse(CS1 == 3, IPONDI, 0),
         individu_agriculteurs_ponderated = ifelse(CS1 == 1, IPONDI, 0),
         individu_dropout_ponderated = ifelse(DIPL %in% c("01", "02", "03", "11", "12"), IPONDI, 0)) %>%
  ungroup() %>%
  group_by(CANTVILLE, ARM) %>%
  mutate(commune_immi_ponderated = sum(individu_immi_ponderated), #these are not proportions of the population !
         commune_chomeurs_ponderated = sum(individu_chomeurs_ponderated),
         commune_hlm_ponderated = sum(individu_hlm_ponderated),
         commune_ouvriers_ponderated = sum(individu_ouvriers_ponderated),
         commune_employes_ponderated = sum(individu_employes_ponderated),
         commune_retraites_ponderated = sum(individu_retraites_ponderated),
         commune_cadres_ponderated = sum(individu_cadres_ponderated),
         commune_agriculteurs_ponderated = sum(individu_agriculteurs_ponderated),
         commune_dropout_ponderated = sum(individu_dropout_ponderated))

rp2017 %>% saveRDS("census_data/rp0617/rp2017_clean.RDS")

#################################################################################################################################

rp2007 = readRDS("census_data/rp0617/rp2007_clean.RDS")
rp2012 = readRDS("census_data/rp0617/rp2012_clean.RDS")
rp2017 = readRDS("census_data/rp0617/rp2017_clean.RDS")

#################################################################################################################################

rp2007 = readRDS("census_data/rp0617/rp2007_clean.RDS") %>% 
  mutate(CANTVILLE = if_else(str_length(CANTVILLE) == 3,
                             str_pad(CANTVILLE, width = 4, pad = "0"),
                             CANTVILLE)) %>% 
  select(-starts_with("commune_")) %>% 
  group_by(CANTVILLE) %>% #compute STOCKS at the cantville level first
  #compute STOCKS at the cantville level first
  mutate(cantville_pop_total = n(),
         cantville_pop_ponderation = sum(IPONDI),
         cantville_immi_ponderated = sum(individu_immi_ponderated), 
         cantville_chomeurs_ponderated = sum(individu_chomeurs_ponderated),
         cantville_hlm_ponderated = sum(individu_hlm_ponderated),
         cantville_ouvriers_ponderated = sum(individu_ouvriers_ponderated),
         cantville_employes_ponderated = sum(individu_employes_ponderated),
         cantville_retraites_ponderated = sum(individu_retraites_ponderated),
         cantville_cadres_ponderated = sum(individu_cadres_ponderated),
         cantville_agriculteurs_ponderated = sum(individu_agriculteurs_ponderated),
         cantville_dropout_ponderated = sum(individu_dropout_ponderated)) %>% 
  #compute proportions at the cantville level
  mutate(cantville_immi_prop = cantville_immi_ponderated / cantville_pop_ponderation, 
         cantville_chomeurs_prop = cantville_chomeurs_ponderated / cantville_pop_ponderation,
         cantville_hlm_prop = cantville_hlm_ponderated / cantville_pop_ponderation,
         cantville_ouvriers_prop = cantville_ouvriers_ponderated / cantville_pop_ponderation,
         cantville_employes_prop = cantville_employes_ponderated / cantville_pop_ponderation,
         cantville_retraites_prop = cantville_retraites_ponderated / cantville_pop_ponderation,
         cantville_cadres_prop = cantville_cadres_ponderated / cantville_pop_ponderation,
         cantville_agriculteurs_prop = cantville_agriculteurs_ponderated / cantville_pop_ponderation,
         cantville_dropout_prop = cantville_dropout_ponderated / cantville_pop_ponderation) %>% 
  ungroup %>% 
  group_by(depcom) %>%
  #compute STOCKS at the depcom level
  mutate(depcom_pop_total = n(),
         depcom_pop_ponderation = sum(IPONDI),
         depcom_immi_ponderated = sum(individu_immi_ponderated),
         depcom_chomeurs_ponderated = sum(individu_chomeurs_ponderated),
         depcom_hlm_ponderated = sum(individu_hlm_ponderated),
         depcom_ouvriers_ponderated = sum(individu_ouvriers_ponderated),
         depcom_employes_ponderated = sum(individu_employes_ponderated),
         depcom_retraites_ponderated = sum(individu_retraites_ponderated),
         depcom_cadres_ponderated = sum(individu_cadres_ponderated),
         depcom_agriculteurs_ponderated = sum(individu_agriculteurs_ponderated),
         depcom_dropout_ponderated = sum(individu_dropout_ponderated)) %>% 
  #compute proportions at the depcom level
  mutate(depcom_immi_prop = depcom_immi_ponderated / depcom_pop_ponderation, 
         depcom_chomeurs_prop = depcom_chomeurs_ponderated / depcom_pop_ponderation,
         depcom_hlm_prop = depcom_hlm_ponderated / depcom_pop_ponderation,
         depcom_ouvriers_prop = depcom_ouvriers_ponderated / depcom_pop_ponderation,
         depcom_employes_prop = depcom_employes_ponderated / depcom_pop_ponderation,
         depcom_retraites_prop = depcom_retraites_ponderated / depcom_pop_ponderation,
         depcom_cadres_prop = depcom_cadres_ponderated /  depcom_pop_ponderation,
         depcom_agriculteurs_prop = depcom_agriculteurs_ponderated / depcom_pop_ponderation,
         depcom_dropout_prop = depcom_dropout_ponderated /  depcom_pop_ponderation)

rp2012 = readRDS("census_data/rp0617/rp2012_clean.RDS") %>% 
  mutate(CANTVILLE = if_else(str_length(cantville) == 3,
                             str_pad(cantville, width = 4, pad = "0"),
                             cantville)) %>% 
  select(-starts_with("commune_")) %>% 
  group_by(cantville) %>% 
  #compute STOCKS at the cantville level first
  mutate(cantville_pop_total = n(),
         cantville_pop_ponderation = sum(ipondi),
         cantville_immi_ponderated = sum(individu_immi_ponderated), 
         cantville_chomeurs_ponderated = sum(individu_chomeurs_ponderated),
         cantville_hlm_ponderated = sum(individu_hlm_ponderated),
         cantville_ouvriers_ponderated = sum(individu_ouvriers_ponderated),
         cantville_employes_ponderated = sum(individu_employes_ponderated),
         cantville_retraites_ponderated = sum(individu_retraites_ponderated),
         cantville_cadres_ponderated = sum(individu_cadres_ponderated),
         cantville_agriculteurs_ponderated = sum(individu_agriculteurs_ponderated),
         cantville_dropout_ponderated = sum(individu_dropout_ponderated)) %>% 
  #compute proportions at the cantville level
  mutate(cantville_immi_prop = cantville_immi_ponderated / cantville_pop_ponderation, 
         cantville_chomeurs_prop = cantville_chomeurs_ponderated / cantville_pop_ponderation,
         cantville_hlm_prop = cantville_hlm_ponderated / cantville_pop_ponderation,
         cantville_ouvriers_prop = cantville_ouvriers_ponderated / cantville_pop_ponderation,
         cantville_employes_prop = cantville_employes_ponderated / cantville_pop_ponderation,
         cantville_retraites_prop = cantville_retraites_ponderated / cantville_pop_ponderation,
         cantville_cadres_prop = cantville_cadres_ponderated / cantville_pop_ponderation,
         cantville_agriculteurs_prop = cantville_agriculteurs_ponderated / cantville_pop_ponderation,
         cantville_dropout_prop = cantville_dropout_ponderated / cantville_pop_ponderation) %>% 
  ungroup %>% 
  group_by(depcom) %>%
  #compute STOCKS at the depcom level
  mutate(depcom_pop_total = n(),
         depcom_pop_ponderation = sum(ipondi),
         depcom_immi_ponderated = sum(individu_immi_ponderated),
         depcom_chomeurs_ponderated = sum(individu_chomeurs_ponderated),
         depcom_hlm_ponderated = sum(individu_hlm_ponderated),
         depcom_ouvriers_ponderated = sum(individu_ouvriers_ponderated),
         depcom_employes_ponderated = sum(individu_employes_ponderated),
         depcom_retraites_ponderated = sum(individu_retraites_ponderated),
         depcom_cadres_ponderated = sum(individu_cadres_ponderated),
         depcom_agriculteurs_ponderated = sum(individu_agriculteurs_ponderated),
         depcom_dropout_ponderated = sum(individu_dropout_ponderated)) %>% 
  #compute proportions at the depcom level
  mutate(depcom_immi_prop = depcom_immi_ponderated /  depcom_pop_ponderation, 
         depcom_chomeurs_prop = depcom_chomeurs_ponderated /  depcom_pop_ponderation,
         depcom_hlm_prop = depcom_hlm_ponderated /  depcom_pop_ponderation,
         depcom_ouvriers_prop = depcom_ouvriers_ponderated /  depcom_pop_ponderation,
         depcom_employes_prop = depcom_employes_ponderated /  depcom_pop_ponderation,
         depcom_retraites_prop = depcom_retraites_ponderated /  depcom_pop_ponderation,
         depcom_cadres_prop = depcom_cadres_ponderated /  depcom_pop_ponderation,
         depcom_agriculteurs_prop = depcom_agriculteurs_ponderated /  depcom_pop_ponderation,
         depcom_dropout_prop = depcom_dropout_ponderated /  depcom_pop_ponderation)

rp2017 = readRDS("census_data/rp0617/rp2017_clean.RDS") %>%
  mutate(CANTVILLE = if_else(str_length(CANTVILLE) == 3,
                             str_pad(CANTVILLE, width = 4, pad = "0"),
                             CANTVILLE)) %>% 
  select(-starts_with("commune_")) %>% 
  group_by(CANTVILLE) %>% 
  #compute STOCKS at the cantville level first
  mutate(cantville_pop_total = n(),
         cantville_pop_ponderation = sum(IPONDI),
         cantville_immi_ponderated = sum(individu_immi_ponderated), 
         cantville_chomeurs_ponderated = sum(individu_chomeurs_ponderated),
         cantville_hlm_ponderated = sum(individu_hlm_ponderated),
         cantville_ouvriers_ponderated = sum(individu_ouvriers_ponderated),
         cantville_employes_ponderated = sum(individu_employes_ponderated),
         cantville_retraites_ponderated = sum(individu_retraites_ponderated),
         cantville_cadres_ponderated = sum(individu_cadres_ponderated),
         cantville_agriculteurs_ponderated = sum(individu_agriculteurs_ponderated),
         cantville_dropout_ponderated = sum(individu_dropout_ponderated)) %>% 
  #compute proportions at the cantville level
  mutate(cantville_immi_prop = cantville_immi_ponderated / cantville_pop_ponderation, 
         cantville_chomeurs_prop = cantville_chomeurs_ponderated / cantville_pop_ponderation,
         cantville_hlm_prop = cantville_hlm_ponderated / cantville_pop_ponderation,
         cantville_ouvriers_prop = cantville_ouvriers_ponderated / cantville_pop_ponderation,
         cantville_employes_prop = cantville_employes_ponderated / cantville_pop_ponderation,
         cantville_retraites_prop = cantville_retraites_ponderated / cantville_pop_ponderation,
         cantville_cadres_prop = cantville_cadres_ponderated / cantville_pop_ponderation,
         cantville_agriculteurs_prop = cantville_agriculteurs_ponderated / cantville_pop_ponderation,
         cantville_dropout_prop = cantville_dropout_ponderated / cantville_pop_ponderation) %>% 
  ungroup %>% 
  group_by(depcom) %>%
  #compute STOCKS at the depcom level
  mutate(depcom_pop_total = n(),
         depcom_pop_ponderation = sum(IPONDI),
         depcom_immi_ponderated = sum(individu_immi_ponderated),
         depcom_chomeurs_ponderated = sum(individu_chomeurs_ponderated),
         depcom_hlm_ponderated = sum(individu_hlm_ponderated),
         depcom_ouvriers_ponderated = sum(individu_ouvriers_ponderated),
         depcom_employes_ponderated = sum(individu_employes_ponderated),
         depcom_retraites_ponderated = sum(individu_retraites_ponderated),
         depcom_cadres_ponderated = sum(individu_cadres_ponderated),
         depcom_agriculteurs_ponderated = sum(individu_agriculteurs_ponderated),
         depcom_dropout_ponderated = sum(individu_dropout_ponderated)) %>% 
  #compute proportions at the depcom level
  mutate(depcom_immi_prop = depcom_immi_ponderated /  depcom_pop_ponderation, 
         depcom_chomeurs_prop = depcom_chomeurs_ponderated /  depcom_pop_ponderation,
         depcom_hlm_prop = depcom_hlm_ponderated /  depcom_pop_ponderation,
         depcom_ouvriers_prop = depcom_ouvriers_ponderated /  depcom_pop_ponderation,
         depcom_employes_prop = depcom_employes_ponderated /  depcom_pop_ponderation,
         depcom_retraites_prop = depcom_retraites_ponderated /  depcom_pop_ponderation,
         depcom_cadres_prop = depcom_cadres_ponderated /  depcom_pop_ponderation,
         depcom_agriculteurs_prop = depcom_agriculteurs_ponderated /  depcom_pop_ponderation,
         depcom_dropout_prop = depcom_dropout_ponderated /  depcom_pop_ponderation)

#

rp2007 %>% saveRDS("census_data/rp0617/rp2007_clean2.RDS")
rp2012 %>% saveRDS("census_data/rp0617/rp2012_clean2.RDS")
rp2017 %>% saveRDS("census_data/rp0617/rp2017_clean2.RDS")

########

summarize_cantville = function(data){
  if (any(data$unique_depcom == 0)){
    return(data)
  } else {
    summarized <- data %>% 
      summarize(across(5:23), mean, na.rm = TRUE)
    return(summarized)
  }
}
summarize_depcom = function(data){
  if (any(data$unique_depcom == 1)){
    return(data)
  } else {
    summarized <- data %>% 
      summarize(across(5:23), mean, na.rm = TRUE)
    return(summarized)
  }
}

rp2007 = readRDS("census_data/rp0617/rp2007_clean2.RDS") %>% 
  select(-contains("individu"), -contains("ponderated")) %>% 
  select(-c(ARM, IRIS, IPONDI, IMMI, HLML, TACT, CS1, DIPL)) %>% 
  mutate(year = 2007) %>% 
  distinct() %>% 
  group_by(CANTVILLE) %>% 
  mutate(depcom = case_when(depcom %in% (75000:75999) ~ "ZZZZZ",
                            .default = depcom)) %>% 
  mutate(unique_depcom = ifelse(any(depcom == "ZZZZZ"),
                                0, #if this cantville contains a ZZZZZ depcom, then it is not unique
                                1)) %>% 
  relocate(year, .before = CANTVILLE) %>% 
  relocate(unique_depcom, .after = depcom)

rp2007_cantville = rp2007 %>% 
  filter(unique_depcom == 0) %>% 
  group_by(CANTVILLE) %>% 
  nest() %>% 
  mutate(data = lapply(data, summarize_cantville)) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  # select(-c(depcom)) %>% 
  select(-contains("depcom_")) %>% 
  distinct() %>% 
  rename_with(~gsub("^cantville_", "", .x), starts_with("cantville_")) %>% 
  rename("cantville" = "CANTVILLE")

rp2007_depcom = rp2007 %>% 
  filter(unique_depcom == 1) %>% 
  group_by(depcom) %>% 
  nest() %>% 
  mutate(data = lapply(data, summarize_depcom)) %>% 
  unnest(data) %>% 
  ungroup %>%
  # select(-CANTVILLE) %>% 
  select(-contains("cantville_")) %>% 
  distinct() %>% 
  rename_with(~gsub("^depcom_", "", .x), starts_with("depcom_")) %>% 
  rename("cantville" = "CANTVILLE")

rp2007_merged = bind_rows(rp2007_cantville, rp2007_depcom) %>% 
  filter(cantville < 97001)

##

rp2012 = readRDS("census_data/rp0617/rp2012_clean2.RDS") %>% 
  select(-contains("individu"), -contains("ponderated")) %>% 
  select(-c(arm, iris, ipondi, immi, hlml, tact, cs1, dipl)) %>% 
  mutate(year = 2012) %>% 
  distinct() %>% 
  group_by(cantville) %>% 
  mutate(depcom = case_when(depcom %in% (75000:75999) ~ "ZZZZZ",
                            .default = depcom)) %>% 
  mutate(unique_depcom = ifelse(any(depcom == "ZZZZZ"),
                                0, #if this cantville contains a ZZZZZ depcom, then it is not unique
                                1)) %>% 
  relocate(year, .before = cantville) %>% 
  relocate(unique_depcom, .after = depcom)

rp2012_cantville = rp2012 %>% 
  filter(unique_depcom == 0) %>% 
  group_by(cantville) %>% 
  nest() %>% 
  mutate(data = lapply(data, summarize_cantville)) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  # select(-c(depcom)) %>% 
  select(-contains("depcom_")) %>% 
  distinct() %>% 
  rename_with(~gsub("^cantville_", "", .x), starts_with("cantville_")) 
# %>% 
#   rename("cantville" = "CANTVILLE")

rp2012_depcom = rp2012 %>% 
  filter(unique_depcom == 1) %>% 
  group_by(depcom) %>% 
  nest() %>% 
  mutate(data = lapply(data, summarize_depcom)) %>% 
  unnest(data) %>% 
  ungroup %>%
  # select(-CANTVILLE) %>% 
  select(-contains("cantville_")) %>% 
  distinct() %>% 
  rename_with(~gsub("^depcom_", "", .x), starts_with("depcom_")) 
# %>% 
#   rename("cantville" = "CANTVILLE")

rp2012_merged = bind_rows(rp2012_cantville, rp2012_depcom) %>% 
  filter(cantville < 97001)

#

rp2017 = readRDS("census_data/rp0617/rp2017_clean2.RDS") %>%
  select(-contains("individu"), -contains("ponderated")) %>% 
  select(-c(ARM, IRIS, IPONDI, IMMI, HLML, TACT, CS1, DIPL)) %>% 
  mutate(year = 2017) %>% 
  distinct() %>% 
  group_by(CANTVILLE) %>% 
  mutate(depcom = case_when(depcom %in% (75000:75999) ~ "ZZZZZ",
                            .default = depcom)) %>% 
  mutate(unique_depcom = ifelse(any(depcom == "ZZZZZ"),
                                0, #if this cantville contains a ZZZZZ depcom, then it is not unique
                                1)) %>% 
  relocate(year, .before = CANTVILLE) %>% 
  relocate(unique_depcom, .after = depcom)
  
rp2017_cantville = rp2017 %>% 
  filter(unique_depcom == 0) %>% 
  group_by(CANTVILLE) %>% 
  nest() %>% 
  mutate(data = lapply(data, summarize_cantville)) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  # select(-c(depcom)) %>% 
  select(-contains("depcom_")) %>% 
  distinct() %>% 
  rename_with(~gsub("^cantville_", "", .x), starts_with("cantville_")) %>% 
  rename("cantville" = "CANTVILLE")

rp2017_depcom = rp2017 %>% 
  filter(unique_depcom == 1) %>% 
  group_by(depcom) %>% 
  nest() %>% 
  mutate(data = lapply(data, summarize_depcom)) %>% 
  unnest(data) %>% 
  ungroup %>%
  # select(-CANTVILLE) %>% 
  select(-contains("cantville_")) %>% 
  distinct() %>% 
  rename_with(~gsub("^depcom_", "", .x), starts_with("depcom_")) %>% 
  rename("cantville" = "CANTVILLE")

rp2017_merged = bind_rows(rp2017_cantville, rp2017_depcom) %>%
  filter(cantville < 97001)

##################

# rp99 %>% saveRDS("census_data/rp99_tableauxanalyses.csv/rp99_clean3.RDS")
rp2007_merged %>% saveRDS("census_data/rp0617/rp2007_clean3.RDS")
rp2012_merged %>% saveRDS("census_data/rp0617/rp2012_clean3.RDS")
rp2017_merged %>% saveRDS("census_data/rp0617/rp2017_clean3.RDS")

################################################################################