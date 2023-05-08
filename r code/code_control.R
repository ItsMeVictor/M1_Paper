library(pacman)
p_load(tidyverse, readxl, rgeoapi, scraEP, tidyverse, zoo)

setwd("/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4. sciences po/work/semestre 2/serranito - advanced econometrics/serranito - paper/data/")


###########################################################################################################################################

#just a function used later to use the Insee API 
#to get insee codes from names or inversely
dep_by_name <- function(x){
  result <- tryCatch(
    DepByName(x)[1, 2],
    error = function(e){
      "TBC"
    }
  )
  return(result)
}

#chomage dept
chomage_dept = read_excel("control_data/chomage_loc.xlsx") %>% 
  rename("lib_dept" = "Libellé") %>% 
  mutate(lib_dept = gsub("Taux de chômage localisés (moyenne annuelle) - Ensemble - ", "", lib_dept, fixed = TRUE)) %>% 
  select(!("Période")) %>% 
  select(1, "2007", "2012", "2017") %>% 
  filter(!is.na(lib_dept)) %>% 
  filter(!grepl("Taux de chômage", lib_dept, fixed = TRUE)) %>% 
  mutate(lib_dept = unaccent(gsub(" ", "-", lib_dept))) %>% 
  mutate(lib_dept = case_when(lib_dept == "Cote-d Or" ~ "Cote-d'Or",
                              lib_dept == "Cotes-d Armor" ~ "Cotes-d'Armor",
                              lib_dept == "Ville-de-Paris" ~ "Paris",
                              lib_dept == "Val-d Oise" ~ "Val-d'Oise",
                              lib_dept == "Provence-Alpes-Cote-d Azur" ~ "Provence-Alpes-Cote-d'Azur",
                              .default = lib_dept)) %>% 
  rowwise() %>% 
  mutate(code_dept = dep_by_name(lib_dept)) %>% 
  ungroup() %>% 
  filter(!is.na(code_dept)) %>% 
  filter(lib_dept != "Corse") %>% 
  relocate(code_dept, .before = lib_dept) %>% 
  mutate_at((3:5), .funs = as.numeric) %>% 
  mutate(code_dept = ifelse(lib_dept == "Loire",
                            "42",
                            code_dept)) %>% 
  rename("chomage_dept2007" = "2007",
         "chomage_dept2012" = "2012",
         "chomage_dept2017" = "2017")


######################################################################################################################################

#pop_commune
tmp_commune = read_excel("control_data/pop_communes.xlsx", skip = 5) %>% 
  select(1:24) %>% 
  rename("code_insee" = "CODGEO") %>% 
  select(c(code_insee, PMUN17, PMUN12, PMUN07)) %>% 
  rename("pop2017" = "PMUN17",
         "pop2012" = "PMUN12",
         "pop2007" = "PMUN07") %>% 
  filter(!(code_insee %in% (75000:75999)))

paris_commune = read_excel("control_data/pop_communes.xlsx", skip = 5) %>% 
  select(1:24) %>% 
  rename("code_insee" = "CODGEO") %>% 
  select(c(code_insee, PMUN17, PMUN12, PMUN07)) %>% 
  rename("pop2017" = "PMUN17",
         "pop2012" = "PMUN12",
         "pop2007" = "PMUN07") %>% 
  filter(code_insee %in% (75000:75999)) %>% 
  summarize(code_insee = 75056,
            pop2017 = sum(pop2017),
            pop2012 = sum(pop2012),
            pop2007 = sum(pop2007)) %>% 
  mutate(code_insee = as.character(code_insee))

pop_commune = bind_rows(tmp_commune, paris_commune)


######################################################################################################################################

#etab_scolaires
etab_scolaires = read.csv("control_data/etab_scolaires.csv", sep = ";") %>% 
  select(numero_uai, libelle_commune, date_ouverture, code_departement, libelle_departement, code_commune, libelle_commune) %>% 
  mutate(school_status = "open")

etab_fermes <- read.csv("control_data/etab_fermes.csv", sep = ";") %>% 
  select(numero_uai, libelle_commune, date_ouverture, date_fermeture, code_departement, libelle_departement, code_commune, libelle_commune) %>% 
  mutate(school_status = "closed")

school_status <- function(open_year, close_year, status, target_year) {
  if (status == "open" && open_year <= target_year) {
    return(1)
  } else if (status == "closed" && open_year <= target_year && (is.na(close_year) || close_year > target_year)) {
    return(1)
  }
  return(0)
}

school_data = bind_rows(etab_scolaires, etab_fermes) %>% 
  mutate(status_2007 = mapply(school_status, date_ouverture, date_fermeture, school_status, 2007),
         status_2012 = mapply(school_status, date_ouverture, date_fermeture, school_status, 2012),
         status_2017 = mapply(school_status, date_ouverture, date_fermeture, school_status, 2017)) %>% 
  group_by(code_departement, code_commune, libelle_commune) %>% 
  summarize(open2007 = sum(status_2007),
            open2012 = sum(status_2012),
            open2017 = sum(status_2017)) %>% 
  mutate(depcom = code_commune) %>% 
  filter(code_departement != "")

########################################################################################################################################
#dwellings in construction

logements_dept <- read_excel("control_data/famille_CONSTRUCTION-LOGEMENTS_29032023.xlsx") %>% 
  rename("lib" = "Libellé") %>% 
  filter(!grepl("Surface de plancher", lib, fixed = TRUE)) %>% 
  filter(grepl("Cumul sur douze mois - Total", lib, fixed = TRUE)) %>% 
  filter(!str_detect(lib, "logements commencés")) %>% 
  select(-(2:160)) %>% 
  select(-(134:195)) %>% 
  select(lib, starts_with("2007") | starts_with("2012") | starts_with("2017")) %>% 
  select(lib, ends_with("12")) %>% 
  rename("logements2007" = "2007-12",
         "logements2012" = "2012-12",
         "logements2017" = "2017-12") %>% 
  mutate(lib = gsub("Nombre de logements autorisés - Cumul sur douze mois - Total - ", "", lib),
         lib = gsub(" - Estimations en date réelle", "", lib)) %>% 
  rename("lib_dept" = "lib") %>% 
  mutate(lib_dept = unaccent(gsub(" ", "-", lib_dept))) %>% 
  mutate(lib_dept = case_when(lib_dept == "Cote-d Or" ~ "Cote-d'Or",
                              lib_dept == "Cotes-d Armor" ~ "Cotes-d'Armor",
                              lib_dept == "Ville-de-Paris" ~ "Paris",
                              lib_dept == "Val-d Oise" ~ "Val-d'Oise",
                              lib_dept == "Provence-Alpes-Cote-d Azur" ~ "Provence-Alpes-Cote-d'Azur",
                              .default = lib_dept)) %>% 
  rowwise() %>% 
  mutate(code_dept = dep_by_name(lib_dept)) %>% 
  ungroup() %>% 
  filter(!is.na(code_dept) & code_dept < 97) %>% 
  filter(lib_dept != "Corse") %>% 
  mutate(code_dept = ifelse(lib_dept == "Loire",
                            "42",
                            code_dept)) %>% 
  relocate(code_dept, .before = lib_dept)

##################################################################################################################################
#births per departement

naissance_dept = read_excel("control_data/naissances_domicilies.xlsx") %>% 
  rename("lib_dept" = "Libellé") %>% 
  select(1, (106:121)) %>% 
  filter(str_detect(lib_dept, "Naissances domiciliées par département - ")) %>% 
  mutate(lib_dept = gsub("Naissances domiciliées par département - ", "", lib_dept)) %>% 
  mutate(lib_dept = unaccent(gsub(" ", "-", lib_dept))) %>% 
  mutate(lib_dept = case_when(lib_dept == "Cote-d Or" ~ "Cote-d'Or",
                              lib_dept == "Cotes-d Armor" ~ "Cotes-d'Armor",
                              lib_dept == "Ville-de-Paris" ~ "Paris",
                              lib_dept == "Val-d Oise" ~ "Val-d'Oise",
                              lib_dept == "Provence-Alpes-Cote-d Azur" ~ "Provence-Alpes-Cote-d'Azur",
                              .default = lib_dept)) %>% 
  rowwise() %>% 
  mutate(code_dept = dep_by_name(lib_dept)) %>% 
  ungroup() %>% 
  filter(!is.na(code_dept) & code_dept < 97) %>% 
  filter(lib_dept != "Corse") %>% 
  mutate(code_dept = ifelse(lib_dept == "Loire",
                            "42",
                            code_dept)) %>% 
  relocate(code_dept, .before = lib_dept) %>% 
  mutate(across(3:18, ~ as.numeric(.))) %>%
  pivot_longer(cols = -(1:2),
               names_to = "year", 
               values_to = "naissances_dept") %>% 
  group_by(code_dept) %>% 
  arrange(code_dept, year) %>% 
  mutate(sum5_naissances_dept = 
           lag(naissances_dept,1) + lag(naissances_dept, 2) + 
           lag(naissances_dept, 3) + lag(naissances_dept, 4) + 
           naissances_dept,
         year = as.numeric(year)) %>% 
  filter(year %in% c("2007", "2012", "2017")) %>% 
  ungroup() %>% 
  select(-lib_dept)

##################################################################################################################################
#crime per departement

crimedata_path = "control_data/tableaux-4001-ts.xlsx"
sheet_names = excel_sheets(crimedata_path)

crime_dept <- lapply(sheet_names, function(sheet) {
  sheet_data <- read_excel(crimedata_path, sheet = sheet)
  sheet_data$code_dept <- sheet  
  return(sheet_data)}) %>%
  bind_rows() %>% 
  relocate(code_dept, .before = Index) %>%
  select((1:3), starts_with("_2017"), starts_with("_2012"), starts_with("_2007")) %>% 
  filter(!(code_dept %in% c("France_Entière", "France_Métro") | code_dept > 97))

crime_dept2017 = crime_dept %>% 
  select(1:15) %>% 
  rowwise() %>% 
  mutate(crime2017 = sum(across(4:15))) %>% 
  group_by(code_dept) %>% 
  mutate(crime2017 = sum(crime2017)) %>% 
  select(1, 16) %>% 
  distinct()

crime_dept2012 = crime_dept %>% 
  select((1:3), (16:27)) %>% 
  rowwise() %>% 
  mutate(crime2012 = sum(across(4:15))) %>% 
  group_by(code_dept) %>% 
  mutate(crime2012 = sum(crime2012)) %>% 
  select(1, 16) %>% 
  distinct()

crime_dept2007 = crime_dept %>% 
  select((1:3), (28:39)) %>% 
  rowwise() %>% 
  mutate(crime2007 = sum(across(4:15))) %>% 
  group_by(code_dept) %>% 
  mutate(crime2007 = sum(crime2007)) %>% 
  select(1, 16) %>% 
  distinct()

crime_dept = crime_dept2017 %>% 
  left_join(crime_dept2012, by = "code_dept") %>% 
  left_join(crime_dept2007, by = "code_dept")

############################################################################################################
#population per departement

popdept_path = "control_data/estim-pop-dep-sexe-gca-1975-2023.xls"
sheet_names1 = excel_sheets(popdept_path)

pop_dept <- lapply(sheet_names1, function(sheet) {
  sheet_data1 <- read_excel(popdept_path, sheet = sheet, skip = 3, col_names = TRUE)
  sheet_data1$year <- sheet  
  return(sheet_data1)}) %>%
  bind_rows() %>% 
  slice(-(1:13)) %>%
  select(-1) %>% 
  rename("lib_dept" = "...2",
         "code_dept" = "Départements",
         "pop_dept" = "Ensemble") %>% 
  select(1:4) %>% 
  filter(!is.na(lib_dept)) %>% 
  filter(!is.na(code_dept)) %>% 
  filter(!(code_dept %in% c("971", "972", "973", "974", "976"))) %>% 
  mutate(pop_dept = as.numeric(pop_dept)) %>% 
  arrange(code_dept, year) %>% 
  group_by(code_dept) %>% 
  mutate(pop_growth_rate = (pop_dept / lag(pop_dept) - 1) * 100 + 100) %>% 
  ungroup() %>% 
  arrange(year) %>% 
  group_by(code_dept) %>%
  mutate(avg5_pop_growth_rate = rollmean(pop_growth_rate, k = 5, fill = NA, align = "right"),
         avg5_pop_dept = rollmean(pop_dept, k = 5, fill = NA, align = "right")) %>% 
  select(-lib_dept) %>% 
  filter(year %in% c("2007", "2012", "2017")) %>% 
  mutate(year = as.numeric(year)) %>% 
  ungroup()

         