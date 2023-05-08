library(pacman)
p_load(haven, data.table, purrr, foreign, tidyverse, readxl, haven, shp)

setwd("/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4. sciences po/work/semestre 2/serranito - advanced econometrics/serranito - paper/data/")

###########################################################################################################

leg2017 = read_excel("elections_data/legislative/Leg_2017_Resultats_communes_T1_c.xlsx", skip = 3, guess_max = 350000) %>% 
  select(-(7:20)) %>% 
  # select(-starts_with("Sexe")) %>% 
  mutate_all(as.character) %>%
  pivot_longer(
    cols = -c("Code du département", "Libellé du département", "Code de la circonscription",
              "Libellé de la circonscription", "Code de la commune", "Libellé de la commune"),
    names_to = "variable",
    values_to = "value") %>% 
  separate(variable, into = c("variable_name", "id"), sep = "\\.\\.\\.", remove = TRUE, convert = TRUE) %>%
  mutate(variable_name = gsub("\\d+$", "", variable_name)) %>% 
  pivot_wider(
    id_cols = c("Code du département", "Libellé du département", "Code de la circonscription",
                "Libellé de la circonscription", "Code de la commune", "Libellé de la commune", "id"),
    names_from = "variable_name",
    values_from = "value") %>% 
  mutate(id_candidate = (row_number() - 1) %/% 8 + 1) %>% 
  relocate(id_candidate, .after = "Libellé de la commune") %>% 
  as.data.table() %>% 
  .[, lapply(.SD, na.omit), by = id_candidate] %>% 
  select(-id) %>% 
  distinct() %>% 
  na.omit() %>% 
  filter(Nuance %in% c("FN", "EXD")) %>% 
  rename("code_dept" = "Code du département",
         "lib_dept" = "Libellé du département",
         "code_circ" = "Code de la circonscription",
         "lib_circ" = "Libellé de la circonscription",
         "code_commune" = "Code de la commune",
         "lib_commune" = "Libellé de la commune",
         "prop_candidate" = "% Voix/Exp") %>% 
  group_by(code_dept, code_circ, code_commune) %>% 
  mutate(prop_extdr = sum(as.numeric(prop_candidate))) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c("N°Panneau", "Sexe", "Nom", "Prénom", "Nuance", "Voix", "% Voix/Ins", "prop_candidate"))

##

leg2012 = read_excel("elections_data/legislative/Leg_2012_Resultats_communes_T1.T2_c.xls", guess_max = 400000) %>% 
  select(-(7:17)) %>%
  mutate_all(as.character) %>% 
  pivot_longer(
    cols = -c("Code du département", "Libellé du département", "Code de la circonscription",
              "Libellé de la circonscription", "Code de la commune", "Libellé de la commune"),
    names_to = "variable",
    values_to = "value") %>%
  separate(variable, into = c("variable_name", "id"), sep = "\\.\\.\\.", remove = TRUE, convert = TRUE) %>%
  mutate(variable_name = gsub("\\d+$", "", variable_name)) %>% 
  filter(!is.na(value)) %>% 
  pivot_wider(
    id_cols = c("Code du département", "Libellé du département", "Code de la circonscription",
                "Libellé de la circonscription", "Code de la commune", "Libellé de la commune", "id"),
    names_from = "variable_name",
    values_from = "value") %>% 
  # filter(!(if_any((8:14), ~ .%in% c("FALSE", "TRUE")))) %>% 
  filter(`Libellé du département` != "FRANCAIS DE L'ETRANGER") %>% 
  group_by(group = (row_number() - 1) %/% 7) %>% 
  mutate(id_candidate = group + 1) %>% 
  ungroup() %>% 
  select(-group) %>% 
  as.data.table() %>% 
  .[, lapply(.SD, na.omit), by = id_candidate] %>% 
  select(-id) %>% 
  distinct() %>%
  na.omit() %>% 
  filter(Nuance %in% c("FN", "EXD")) %>%
  rename("code_dept" = "Code du département",
         "lib_dept" = "Libellé du département",
         "code_circ" = "Code de la circonscription",
         "lib_circ" = "Libellé de la circonscription",
         "code_commune" = "Code de la commune",
         "lib_commune" = "Libellé de la commune",
         "prop_candidate" = "% Voix/Exp") %>% 
  group_by(code_dept, code_circ, code_commune) %>% 
  mutate(prop_extdr = sum(as.numeric(prop_candidate))) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c("Sexe", "Nom", "Prénom", "Nuance", "Voix", "% Voix/Ins", "prop_candidate"))

##
  
leg2007 <- read_excel("elections_data/legislative/Leg_2007_Resultats_communes_T1_c.xls", guess_max = 350000) %>% 
  select(-(7:17)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(
    cols = -c("Code du département", "Libellé du département", "Code de la circonscription",
              "Libellé de la circonscription", "Code de la commune", "Libellé de la commune"),
    names_to = "variable",
    values_to = "value") %>%
  separate(variable, into = c("variable_name", "id"), sep = "\\.\\.\\.", remove = TRUE, convert = TRUE) %>%
  mutate(variable_name = gsub("\\d+$", "", variable_name)) %>% 
  mutate(id_candidate = (row_number() - 1) %/% 7 + 1) %>% 
  relocate(id_candidate, .after = "Libellé de la commune") %>% 
  pivot_wider(
    id_cols = c("Code du département", "Libellé du département", "Code de la circonscription",
                "Libellé de la circonscription", "Code de la commune", "Libellé de la commune", "id", "id_candidate"),
    names_from = "variable_name",
    values_from = "value") %>%
  as.data.table() %>%
  .[, lapply(.SD, na.omit), by = id_candidate] %>% 
  select(-id) %>% 
  distinct() %>% 
  na.omit() %>% 
  filter(Nuance %in% c("FN", "EXD")) %>% 
  rename("code_dept" = "Code du département",
         "lib_dept" = "Libellé du département",
         "code_circ" = "Code de la circonscription",
         "lib_circ" = "Libellé de la circonscription",
         "code_commune" = "Code de la commune",
         "lib_commune" = "Libellé de la commune",
         "prop_candidate" = "% Voix/Exp") %>% 
  group_by(code_dept, code_circ, code_commune) %>% 
  mutate(prop_extdr = sum(as.numeric(prop_candidate))) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c("Sexe", "Nom", "Prénom", "Nuance", "Voix", "% Voix/Ins", "prop_candidate"))

##

leg2002 <- read_excel("elections_data/legislative/Leg_2002_Resultats_communes_T1_c.xls", guess_max = 400000) %>% 
  select(-(7:17)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(
    cols = -c("Code du département", "Libellé du département", "Code de la circonscription",
              "Libellé de la circonscription", "Code de la commune", "Libellé de la commune"),
    names_to = "variable",
    values_to = "value") %>%
  separate(variable, into = c("variable_name", "id"), sep = "\\.\\.\\.", remove = TRUE, convert = TRUE) %>%
  mutate(variable_name = gsub("\\d+$", "", variable_name)) %>% 
  filter(!is.na(value)) %>% 
  filter(!(value %in% c("TRUE", "FALSE"))) %>% 
  mutate(id_candidate = (row_number() - 1) %/% 7 + 1) %>% 
  pivot_wider(
    id_cols = c("Code du département", "Libellé du département", "Code de la circonscription",
                "Libellé de la circonscription", "Code de la commune", "Libellé de la commune", "id", "id_candidate"),
    names_from = "variable_name",
    values_from = "value") %>% 
  relocate(id_candidate, .after = "Libellé de la commune") %>% 
  as.data.table() %>%
  .[, lapply(.SD, na.omit), by = id_candidate] %>% 
  select(-id) %>% 
  distinct() %>% 
  na.omit() %>% 
  filter(Nuance %in% c("FN", "EXD")) %>% 
  rename("code_dept" = "Code du département",
         "lib_dept" = "Libellé du département",
         "code_circ" = "Code de la circonscription",
         "lib_circ" = "Libellé de la circonscription",
         "code_commune" = "Code de la commune",
         "lib_commune" = "Libellé de la commune",
         "prop_candidate" = "% Voix/Exp") %>% 
  group_by(code_dept, code_circ, code_commune) %>% 
  mutate(prop_extdr = sum(as.numeric(prop_candidate))) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c("Sexe", "Nom", "Prénom", "Nuance", "Voix", "% Voix/Ins", "prop_candidate"))
  
##

leg1997 = read_excel("elections_data/legislative/Leg_1997_Resultats_communes_T1.T2_c.xls", guess_max = 36000) %>% 
  select(-(7:17)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(
    cols = -c("Code du département", "Libellé du département", "Code de la circonscription",
              "Libellé de la circonscription", "Code de la commune", "Libellé de la commune"),
    names_to = "variable",
    values_to = "value") %>% 
  separate(variable, into = c("variable_name", "id"), sep = "\\.\\.\\.", remove = TRUE, convert = TRUE) %>% 
  mutate(variable_name = gsub("\\d+$", "", variable_name)) %>% 
  filter(!is.na(value)) %>%
  mutate(id_candidate = (row_number() - 1) %/% 7 + 1) %>% 
  relocate(id_candidate, .after = "Libellé de la commune") %>%
  pivot_wider(
    id_cols = c("Code du département", "Libellé du département", "Code de la circonscription",
                "Libellé de la circonscription", "Code de la commune", "Libellé de la commune", "id", "id_candidate"),
    names_from = "variable_name",
    values_from = "value") %>% 
# %>%
  as.data.table() %>%
  .[, lapply(.SD, na.omit), by = id_candidate] %>% 
  select(-id) %>% 
  distinct() %>% 
  na.omit() %>% 
  filter(Nuance %in% c("FRN", "EXD")) %>% 
  rename("code_dept" = "Code du département",
         "lib_dept" = "Libellé du département",
         "code_circ" = "Code de la circonscription",
         "lib_circ" = "Libellé de la circonscription",
         "code_commune" = "Code de la commune",
         "lib_commune" = "Libellé de la commune",
         "prop_candidate" = "% Voix/Exp") %>% 
  group_by(code_dept, code_circ, code_commune) %>% 
  mutate(prop_extdr = sum(as.numeric(prop_candidate))) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-c("Sexe", "Nom", "Prénom", "Nuance", "Voix", "% Voix/Ins", "prop_candidate"))

leg1988 = read.csv("elections_data/legislative/Leg_1988_Resultats_communes_T1_c.csv")





#save cleaned version before binding 
saveRDS(leg1997, "elections_data/legislative/leg1997_clean.RDS")
saveRDS(leg2002, "elections_data/legislative/leg2002_clean.RDS")
saveRDS(leg2007, "elections_data/legislative/leg2007_clean.RDS")
saveRDS(leg2012, "elections_data/legislative/leg2012_clean.RDS")
saveRDS(leg2017, "elections_data/legislative/leg2017_clean.RDS")

###########################################################################################################

codgeo1999 = read_dbf("code_geographique/codegeographique1999.dbf") %>% 
  mutate(depcom = paste0(DEP, COM),
         cantonville = paste0(DEP, CT)) %>% 
  select(DEP, COM, CT, NCC, depcom, cantonville)


codgeo2002 = read_dbf("code_geographique/codegeographique2002.dbf") %>% 
  mutate(depcom = paste0(DEP, COM),
         cantonville = paste0(DEP, CT)) %>% 
  select(DEP, COM, CT, NCC, depcom, cantonville)

# arr2002 = read_dbf("code_geographique/arr_geographique2002.dbf")

codgeo2007 = read_dbf("code_geographique/codegeographique2007.dbf") %>% 
  mutate(depcom = paste0(DEP, COM),
         cantonville = paste0(DEP, CT)) %>% 
  select(DEP, COM, CT, NCC, depcom, cantonville)

codgeo2012 = read_dbf("code_geographique/codegeographique2012.dbf") %>% 
  mutate(depcom = paste0(DEP, COM),
         cantonville = paste0(DEP, CT)) %>% 
  select(DEP, COM, CT, NCC, depcom, cantonville)

codgeo2017 = read_dbf("code_geographique/codegeographique2017.dbf") %>% 
  mutate(depcom = paste0(DEP, COM),
         cantonville = paste0(DEP, CT)) %>% 
  select(DEP, COM, CT, NCC, depcom, cantonville)



################################################################################

leg1997 <- readRDS("elections_data/legislative/leg1997_clean.RDS") %>% 
  mutate_at(vars(code_circ, code_commune, prop_extdr),
            as.numeric) %>% 
  rename("extdr_leg" = "prop_extdr") %>%
  filter(!str_detect(code_dept, "Z")) %>% 
  mutate(year = 1997) %>% 
  mutate(code_dept = ifelse(nchar(code_dept) == 1, paste0("0", code_dept), code_dept),
         code_commune = as.character(code_commune),
         code_commune = case_when(nchar(code_commune) == 1 ~ paste0("00", code_commune),
                                  nchar(code_commune) == 2 ~ paste0("0", code_commune),
                                  .default = code_commune),
         depcom = paste0(code_dept, code_commune)) %>% 
  select(-c(id_candidate, lib_circ)) %>% 
  left_join(codgeo2002, by = "depcom") %>%
  select(-c(DEP, COM, CT)) %>%
  group_by(depcom) %>% 
  mutate(extdr_leg1 = mean(extdr_leg)) %>% 
  select(-extdr_leg) %>% 
  distinct() %>% 
  rename("extdr_leg" = "extdr_leg1")

leg2002 <- readRDS("elections_data/legislative/leg2002_clean.RDS") %>% 
  mutate_at(vars(code_circ, code_commune, prop_extdr),
            as.numeric) %>% 
  rename("extdr_leg" = "prop_extdr") %>%
  filter(!str_detect(code_dept, "Z")) %>% 
  mutate(year = 2002) %>% 
  mutate(code_dept = ifelse(nchar(code_dept) == 1, paste0("0", code_dept), code_dept),
         code_commune = as.character(code_commune),
         code_commune = case_when(nchar(code_commune) == 1 ~ paste0("00", code_commune),
                                  nchar(code_commune) == 2 ~ paste0("0", code_commune),
                                  .default = code_commune),
         depcom = paste0(code_dept, code_commune)) %>% 
  select(-c(id_candidate, lib_circ)) %>% 
  left_join(codgeo2002, by = "depcom") %>% 
  select(-c(DEP, COM, CT)) %>% 
  group_by(depcom) %>% 
  mutate(extdr_leg1 = mean(extdr_leg)) %>% 
  select(-extdr_leg) %>% 
  distinct() %>% 
  rename("extdr_leg" = "extdr_leg1")

leg2007 <- readRDS("elections_data/legislative/leg2007_clean.RDS") %>% 
  mutate_at(vars(code_circ, code_commune, prop_extdr),
            as.numeric) %>% 
  rename("extdr_leg" = "prop_extdr") %>%
  filter(!str_detect(code_dept, "Z")) %>% 
  mutate(year = 2007) %>% 
  mutate(code_dept = ifelse(nchar(code_dept) == 1, paste0("0", code_dept), code_dept),
         code_commune = as.character(code_commune),
         code_commune = case_when(nchar(code_commune) == 1 ~ paste0("00", code_commune),
                                  nchar(code_commune) == 2 ~ paste0("0", code_commune),
                                  .default = code_commune),
         depcom = paste0(code_dept, code_commune)) %>% 
  left_join(codgeo2007, by = "depcom") %>% 
  select(-c(DEP, COM, CT, code_circ, lib_circ)) %>% 
  group_by(depcom) %>% 
  mutate(extdr_leg1 = mean(extdr_leg)) %>% 
  select(-extdr_leg) %>% 
  distinct() %>% 
  rename("extdr_leg" = "extdr_leg1")

leg2012 <- readRDS("elections_data/legislative/leg2012_clean.RDS") %>% 
  mutate_at(vars(code_circ, code_commune, prop_extdr),
            as.numeric) %>% 
  rename("extdr_leg" = "prop_extdr") %>%
  select(-c(id_candidate)) %>% 
  filter(!str_detect(code_dept, "Z")) %>% 
  mutate(year = 2012) %>% 
  mutate(code_dept = ifelse(nchar(code_dept) == 1, paste0("0", code_dept), code_dept),
         code_commune = as.character(code_commune),
         code_commune = case_when(nchar(code_commune) == 1 ~ paste0("00", code_commune),
                                  nchar(code_commune) == 2 ~ paste0("0", code_commune),
                                  .default = code_commune),
         depcom = paste0(code_dept, code_commune)) %>% 
  left_join(codgeo2012, by = "depcom") %>% 
  select(-c(DEP, COM, CT, code_circ, lib_circ)) %>% 
  group_by(depcom) %>% 
  mutate(extdr_leg1 = mean(extdr_leg)) %>% 
  select(-extdr_leg) %>% 
  distinct() %>% 
  rename("extdr_leg" = "extdr_leg1")

leg2017 <- readRDS("elections_data/legislative/leg2017_clean.RDS") %>% 
  mutate_at(vars(code_circ, code_commune, prop_extdr),
            as.numeric) %>% 
  rename("extdr_leg" = "prop_extdr") %>%
  select(-c(id_candidate)) %>% 
  mutate(year = 2017) %>% 
  mutate(code_dept = case_when(lib_dept == "Corse-du-Sud" ~ "2A",
                               lib_dept == "Haute-Corse" ~ "2B",
                               .default = code_dept)) %>% 
  filter(!str_detect(code_dept, "Z")) %>% 
  mutate(code_dept = ifelse(nchar(code_dept) == 1, paste0("0", code_dept), code_dept),
         code_commune = as.character(code_commune),
         code_commune = case_when(nchar(code_commune) == 1 ~ paste0("00", code_commune),
                                  nchar(code_commune) == 2 ~ paste0("0", code_commune),
                                  .default = code_commune),
         depcom = paste0(code_dept, code_commune)) %>% 
  left_join(codgeo2017, by = "depcom") %>% 
  select(-c(DEP, COM, CT, code_circ, lib_circ)) %>% 
  group_by(depcom) %>% 
  mutate(extdr_leg1 = mean(extdr_leg)) %>% 
  select(-extdr_leg) %>% 
  distinct() %>% 
  rename("extdr_leg" = "extdr_leg1")

#################################################################################################

leg1988 %>% saveRDS("elections_data/legislative/leg1988_clean2.RDS")
leg1997 %>% saveRDS("elections_data/legislative/leg1997_clean2.RDS")
leg2002 %>% saveRDS("elections_data/legislative/leg2002_clean2.RDS")
leg2007 %>% saveRDS("elections_data/legislative/leg2007_clean2.RDS")
leg2012 %>% saveRDS("elections_data/legislative/leg2012_clean2.RDS")
leg2017 %>% saveRDS("elections_data/legislative/leg2017_clean2.RDS")


  
