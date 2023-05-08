library(pacman)
p_load(haven, data.table, purrr, readxl, rgeoapi, tidyverse, sf, mapsf, haven, stargazer)

setwd("/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4.sciences_po/work/semestre_2/serranito_advancedeconometrics/serranito_paper/data/")

#shapefile communes
communes_shp <- st_read("commune_shapefile/communes-20220101-shp/communes-20220101.shp") %>% 
  mutate(depcom = insee) %>% 
  rename("code_insee" = "insee") %>% 
  mutate(depcom = code_insee) %>% 
  select(code_insee, depcom, geometry)

#####################################################################################################################################################

#merge 2007
leg2007 = readRDS("elections_data/legislative/leg2007_clean2.RDS") %>% 
  rename("cantville" = "cantonville") %>% 
  select(-id_candidate) %>% 
  ungroup() %>% 
  distinct()

rp2007 = readRDS("census_data/rp0617/rp2007_clean3.RDS") 

merged2007 <- leg2007 %>%
  left_join(rp2007, by = "depcom") %>%
  mutate(missing_depcom = is.na(unique_depcom)) %>%
  rename("cantville" = "cantville.x") %>% 
  left_join(rp2007 %>% filter(depcom == "ZZZZZ"), by = "cantville", suffix = c(".x", ".y")) %>%
  mutate(across(starts_with("value"), ~if_else(missing_depcom, .y, .x), .names = "value{col}")) %>%
  ungroup() %>% 
  rename("cantville.x" = "cantville") %>% 
  select(-year) %>% 
  mutate(
    year = coalesce(year.x, year.y),
    depcom = coalesce(depcom.x, depcom.y),
    cantville = coalesce(cantville.x, cantville.y),
    unique_depcom = coalesce(unique_depcom.x, unique_depcom.y),
    pop_total = coalesce(pop_total.x, pop_total.y),
    pop_ponderation = coalesce(pop_ponderation.x, pop_ponderation.y),
    immi_prop = coalesce(immi_prop.x, immi_prop.y),
    chomeurs_prop = coalesce(chomeurs_prop.x, chomeurs_prop.y),
    hlm_prop = coalesce(hlm_prop.x, hlm_prop.y),
    ouvriers_prop = coalesce(ouvriers_prop.x, ouvriers_prop.y),
    employes_prop = coalesce(employes_prop.x, employes_prop.y),
    retraites_prop = coalesce(retraites_prop.x, retraites_prop.y),
    cadres_prop = coalesce(cadres_prop.x, cadres_prop.y),
    agriculteurs_prop = coalesce(agriculteurs_prop.x, agriculteurs_prop.y),
    dropouts_prop = coalesce(dropout_prop.x, dropout_prop.y)) %>% 
  select(-ends_with(".x"), -ends_with(".y"))

#merge 2012
leg2012 = readRDS("elections_data/legislative/leg2012_clean2.RDS") %>% 
  rename("cantville" = "cantonville") 

rp2012 = readRDS("census_data/rp0617/rp2012_clean3.RDS") 

merged2012 <- leg2012 %>%
  left_join(rp2012, by = "depcom") %>%
  mutate(missing_depcom = is.na(unique_depcom)) %>%
  rename("cantville" = "cantville.x") %>% 
  left_join(rp2012 %>% filter(depcom == "ZZZZZ"), by = "cantville", suffix = c(".x", ".y")) %>%
  mutate(across(starts_with("value"), ~if_else(missing_depcom, .y, .x), .names = "value{col}")) %>%
  ungroup() %>%
  rename("cantville.x" = "cantville") %>% 
  select(-year) %>% 
  mutate(
    year = coalesce(year.x, year.y),
    depcom = coalesce(depcom.x, depcom.y),
    cantville = coalesce(cantville.x, cantville.y),
    unique_depcom = coalesce(unique_depcom.x, unique_depcom.y),
    pop_total = coalesce(pop_total.x, pop_total.y),
    pop_ponderation = coalesce(pop_ponderation.x, pop_ponderation.y),
    immi_prop = coalesce(immi_prop.x, immi_prop.y),
    chomeurs_prop = coalesce(chomeurs_prop.x, chomeurs_prop.y),
    hlm_prop = coalesce(hlm_prop.x, hlm_prop.y),
    ouvriers_prop = coalesce(ouvriers_prop.x, ouvriers_prop.y),
    employes_prop = coalesce(employes_prop.x, employes_prop.y),
    retraites_prop = coalesce(retraites_prop.x, retraites_prop.y),
    cadres_prop = coalesce(cadres_prop.x, cadres_prop.y),
    agriculteurs_prop = coalesce(agriculteurs_prop.x, agriculteurs_prop.y),
    dropouts_prop = coalesce(dropout_prop.x, dropout_prop.y)) %>% 
  select(-ends_with(".x"), -ends_with(".y"))

#merge 2017 
leg2017 = readRDS("elections_data/legislative/leg2017_clean2.RDS") %>% 
  rename("cantville" = "cantonville") %>% 
  # select(-depcom) %>% 
  ungroup() 

rp2017 = readRDS("census_data/rp0617/rp2017_clean3.RDS") %>% 
  mutate(cantville = case_when(cantville == "75ZZ" ~ "75NA",
                               .default = cantville))

merged2017 <- leg2017 %>%
  left_join(rp2017, by = "depcom") %>%
  mutate(missing_depcom = is.na(unique_depcom)) %>%
  rename("cantville" = "cantville.x") %>% 
  left_join(rp2017 %>% filter(depcom == "ZZZZZ"), by = "cantville", suffix = c(".x", ".y")) %>%
  mutate(across(starts_with("value"), ~if_else(missing_depcom, .y, .x), .names = "value{col}")) %>%
  ungroup() %>% 
  rename("cantville.x" = "cantville") %>% 
  select(-year) %>% 
  mutate(
    year = coalesce(year.x, year.y),
    depcom = coalesce(depcom.x, depcom.y),
    cantville = coalesce(cantville.x, cantville.y),
    unique_depcom = coalesce(unique_depcom.x, unique_depcom.y),
    pop_total = coalesce(pop_total.x, pop_total.y),
    pop_ponderation = coalesce(pop_ponderation.x, pop_ponderation.y),
    immi_prop = coalesce(immi_prop.x, immi_prop.y),
    chomeurs_prop = coalesce(chomeurs_prop.x, chomeurs_prop.y),
    hlm_prop = coalesce(hlm_prop.x, hlm_prop.y),
    ouvriers_prop = coalesce(ouvriers_prop.x, ouvriers_prop.y),
    employes_prop = coalesce(employes_prop.x, employes_prop.y),
    retraites_prop = coalesce(retraites_prop.x, retraites_prop.y),
    cadres_prop = coalesce(cadres_prop.x, cadres_prop.y),
    agriculteurs_prop = coalesce(agriculteurs_prop.x, agriculteurs_prop.y),
    dropouts_prop = coalesce(dropout_prop.x, dropout_prop.y)) %>% 
  select(-ends_with(".x"), -ends_with(".y"))

###################################################################################################################################

final_merge = bind_rows(merged2007, merged2012, merged2017) %>%
  ungroup() %>%
  # rename("depcom" = "depcom.x") %>%
  # select(-id_candidate) %>%
  left_join(school_data, by = "depcom") %>% 
  select(-c("code_commune.x", "unique_depcom", "code_departement", "NCC")) %>% 
  rename(
    "lib_commune_orig" = "lib_commune",
    "lib_commune2023" = "libelle_commune") %>% 
  relocate(code_commune.y, .after = depcom) %>% 
  relocate(lib_commune2023, .after = lib_commune_orig) %>% 
  relocate(year, .before = code_dept) %>% 
  relocate(extdr_leg, .after = code_commune.y) %>% 
  # select(-depcom.y) %>%
  mutate(open2007 = ifelse(is.na(lib_commune2023) & is.na(code_commune.y) & !is.na(depcom), 0, open2007),
         open2012 = ifelse(is.na(lib_commune2023) & is.na(code_commune.y) & !is.na(depcom), 0, open2012),
         open2017 = ifelse(is.na(lib_commune2023) & is.na(code_commune.y) & !is.na(depcom), 0, open2017)) %>% 
  filter(!is.na(pop_total)) %>%
  rowwise() %>%
  mutate(lib_commune2023_2 = list(ComByCode(depcom))) %>%
  ungroup() %>%
  saveRDS("final_data/final_merge.RDS")

##################################################################################################################################

final_merge = readRDS("final_data/final_merge.RDS") %>% 
  mutate(select_libcommune2023 = map(lib_commune2023_2, ~ select(.x, c(name, codeInsee, codeDepartement, codeRegion)))) %>% 
  unnest(select_libcommune2023) %>% 
  select(-c(lib_commune2023, code_commune.y, lib_commune2023_2, codeDepartement, missing_depcom)) %>% 
  rename("code_region" = "codeRegion",
         "code_insee" = "codeInsee",
         "lib_commune2023" = "name") %>% 
  relocate(code_region, .before = code_dept) %>%
  relocate(code_insee, .before = extdr_leg) %>% 
  relocate(lib_commune2023, .before = depcom) %>% 
  rename("schools2007" = "open2007",
         "schools2012" = "open2012",
         "schools2017" = "open2017") %>%
  left_join(pop_commune, by = "code_insee") %>% 
  left_join(chomage_dept, by = "code_dept") %>% 
  select(-lib_dept.x) %>% 
  rename("lib_dept" = "lib_dept.y") %>% 
  relocate(lib_dept, .after = code_dept) %>%
  left_join((logements_dept %>% select(-lib_dept)), by = "code_dept") %>% 
  # left_join((naissance_dept %>% select(-lib_dept)), by = "code_dept") %>% 
  left_join(crime_dept, by = "code_dept") %>%
  left_join(communes_shp, by = "code_insee") %>% 
  # select(-lib_commune2023_2) %>% 
  pivot_longer(
    cols = c("crime2007", "crime2012", "crime2017"),
    names_to = "year_temp",
    values_to = "crime_dept") %>%
  mutate(year_temp = as.integer(str_extract(year_temp, "\\d{4}"))) %>%
  filter(year == year_temp) %>%
  select(-year_temp) %>% 
  rename_with(~sub("^chomage_dept", "chomage", .), starts_with("chomage_dept")) %>% 
  pivot_longer(
    cols = c("chomage2007", "chomage2012", "chomage2017"),
    names_to = "year_temp",
    values_to = "chomage_dept") %>% 
  mutate(year_temp = as.integer(str_extract(year_temp, "\\d{4}"))) %>% 
  filter(year == year_temp) %>% 
  select(-year_temp) %>% 
  pivot_longer(
    cols = c("logements2007", "logements2012", "logements2017"),
    names_to = "year_temp",
    values_to = "logements_dept") %>% 
  mutate(year_temp = as.integer(str_extract(year_temp, "\\d{4}"))) %>% 
  filter(year == year_temp) %>% 
  select(-year_temp) %>% 
  pivot_longer(
    cols = c("pop2007", "pop2012", "pop2017"),
    names_to = "year_temp",
    values_to = "pop_commune") %>% 
  mutate(year_temp = as.integer(str_extract(year_temp, "\\d{4}"))) %>% 
  filter(year == year_temp) %>% 
  select(-year_temp) %>% 
  pivot_longer(
    cols = c("schools2007", "schools2012", "schools2017"),
    names_to = "year_temp",
    values_to = "schools") %>% 
  mutate(year_temp = as.integer(str_extract(year_temp, "\\d{4}"))) %>% 
  filter(year == year_temp) %>% 
  select(-year_temp) %>% #yes I know I should have defined a function
  relocate(geometry, .after = schools) %>% 
  select(-depcom.y) %>% 
  rename("depcom" = "depcom.x") %>% 
  saveRDS("final_data/final_merge1.RDS")

final_merge = readRDS("final_data/final_merge1.RDS") %>%
  relocate(pop_commune, .after = pop_ponderation) %>% 
  # select(-c(lib_commune2023_2, pop_total)) %>% 
  left_join(naissance_dept, by = c("code_dept", "year")) %>% 
  relocate(naissances_dept, .before = crime_dept) %>% 
  left_join(pop_dept, by = c("code_dept", "year")) %>% 
  relocate(pop_dept, .before = crime_dept) %>% 
  rename("pop_dept_growth" = "pop_growth_rate", 
         "avg5_popdept_growth" = "avg5_pop_growth_rate") %>% 
  select(-c(depcom, pop_total, pop_ponderation)) %>% 
  mutate(naissances_dept = as.numeric(naissances_dept),
         crime_dept_prop = crime_dept / pop_dept * 100,
         naissance_dept_prop = naissances_dept / pop_dept * 100,
         logements_dept_prop = logements_dept / pop_dept * 100,
         avg5_naissances_dept = sum5_naissances_dept / avg5_pop_dept * 100) %>% 
  relocate(crime_dept_prop, .after = crime_dept) %>% 
  relocate(naissance_dept_prop, .after = naissances_dept) %>% 
  relocate(logements_dept_prop, .after = logements_dept) %>% 
  relocate(geometry, .after = avg5_naissances_dept) %>% 
  mutate(immi_prop = immi_prop*100,
         chomeurs_prop = chomeurs_prop*100,
         hlm_prop = hlm_prop*100,
         ouvriers_prop = ouvriers_prop*100,
         employes_prop = employes_prop*100,
         retraites_prop = retraites_prop*100,
         cadres_prop = cadres_prop*100,
         agriculteurs_prop = agriculteurs_prop*100,
         dropouts_prop = dropouts_prop*100) %>% 
  relocate(cantville, .after = code_insee) %>% 
  saveRDS("final_data/final_merge2.RDS")

final_merge = readRDS("final_data/final_merge2.RDS") %>% 
  filter(!str_starts(code_insee, "2A") & !str_starts(code_insee, "2B")) 

# readRDS("final_data/final_merge2.RDS") 
# # %>%
#   filter(!str_starts(code_insee, "2A") & !str_starts(code_insee, "2B")) %>% 
#   select(-geometry) %>% 
#   write_dta("final_data/final_merge.dta")

#################################################################################################################################

leg2002 = readRDS("elections_data/legislative/leg2002_clean2.RDS") %>% 
  select(year, code_dept, lib_commune, depcom, cantonville, extdr_leg) %>% 
  rowwise() %>% 
  mutate(lib_commune2023_2 = list(ComByCode(depcom))) %>% 
  ungroup() %>% 
  saveRDS("elections_data/legislative/leg2002_clean3.RDS")
  
leg2002 = readRDS("elections_data/legislative/leg2002_clean3.RDS") %>% 
  mutate(select_libcommune2023 = map(lib_commune2023_2, ~ select(.x, c(name, codeInsee, codeDepartement, codeRegion)))) %>% 
  unnest(select_libcommune2023) %>% 
  rename("code_region" = "codeRegion",
         "code_insee" = "codeInsee",
         "lib_commune2023" = "name") %>% 
  relocate(c(code_region, code_dept), .after = year) %>% 
  relocate(lib_commune2023, .after = lib_commune) %>% 
  relocate(code_insee, .after = lib_commune2023) %>% 
  select(-c(codeDepartement, lib_commune2023_2)) %>% 
  filter(!str_starts(code_insee, "2A") & !str_starts(code_insee, "2B")) 

leg1997 = readRDS("elections_data/legislative/leg1997_clean2.RDS") %>% 
  select(year, code_dept, lib_commune, depcom, cantonville, extdr_leg) %>% 
  rowwise() %>% 
  mutate(lib_commune2023_2 = list(ComByCode(depcom))) %>% 
  ungroup() %>% 
  saveRDS("elections_data/legislative/leg1997_clean3.RDS")
  
leg1997 = readRDS("elections_data/legislative/leg1997_clean3.RDS") %>% 
  mutate(select_libcommune2023 = map(lib_commune2023_2, ~ select(.x, c(name, codeInsee, codeDepartement, codeRegion)))) %>% 
  unnest(select_libcommune2023) %>% 
  rename("code_region" = "codeRegion",
         "code_insee" = "codeInsee",
         "lib_commune2023" = "name") %>% 
  relocate(c(code_region, code_dept), .after = year) %>% 
  relocate(lib_commune2023, .after = lib_commune) %>% 
  relocate(code_insee, .after = lib_commune2023) %>% 
  select(-c(codeDepartement, lib_commune2023_2)) %>% 
  filter(!str_starts(code_insee, "2A") & !str_starts(code_insee, "2B")) 

##############################################################################################################################

final_merge = readRDS("final_data/final_merge2.RDS") %>% 
  filter(!str_starts(code_insee, "2A") & !str_starts(code_insee, "2B")) 

leg1997 = readRDS("elections_data/legislative/leg1997_clean3.RDS") %>% 
  mutate(select_libcommune2023 = map(lib_commune2023_2, ~ select(.x, c(name, codeInsee, codeRegion)))) %>% 
  unnest(select_libcommune2023) %>% 
  rename("code_insee" = "codeInsee",
         "cantville" = "cantonville",
         "lib_commune_orig" = "lib_commune",
         "lib_commune2023" = "name",
         "code_region" = "codeRegion") %>% 
  select(-c(lib_commune2023_2, depcom))
  
leg2002 = readRDS("elections_data/legislative/leg2002_clean3.RDS") %>% 
  mutate(select_libcommune2023 = map(lib_commune2023_2, ~ select(.x, c(name, codeInsee, codeRegion)))) %>% 
  unnest(select_libcommune2023) %>% 
  rename("code_insee" = "codeInsee",
         "cantville" = "cantonville",
         "lib_commune_orig" = "lib_commune",
         "lib_commune2023" = "name",
         "code_region" = "codeRegion") %>% 
  select(-c(lib_commune2023_2, depcom))

extend_merge = bind_rows(final_merge, leg2002, leg1997) %>% 
  distinct() %>% 
  group_by(code_insee) %>% 
  mutate(nrow = n()) %>% 
  ungroup()
  
# extend_merge %>%
#   filter(nrow == 5) %>% 
#   select(-c(geometry)) %>%
#   write_dta("final_data/extend_merge.dta")
