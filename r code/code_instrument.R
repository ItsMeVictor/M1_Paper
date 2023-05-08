library(pacman)
p_load(haven, data.table, purrr, foreign, tidyverse, readxl, haven, shp, stringr, scraEP, rgeoapi)

setwd("/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4.sciences_po/work/semestre_2/serranito_advancedeconometrics/serranito_paper/data")

####################################################################################################################################
#leg 1986

#this is a table for 1986 electoral results at the commune level
communes_leg1986 <- read_csv("elections_data/legislative/Leg_1986_Resultats_communes_T1_c.csv") %>%
  slice(-1) %>%
  rename("code_dept" = "Code département",
         "lib_dept" = "département",
         "code_commune" = "numéro de commune",
         "lib_commune" = "Commune",
         "extdr_voix" = "Extrême droite",
         "total_vote" = "Exprimés") %>%
  mutate(code_dept = str_pad(code_dept, 2, "left", 0),
         code_commune = ifelse(code_dept == "75" & lib_dept == "Paris" & code_commune == 100,
                               056,
                               code_commune),
         code_commune = str_pad(code_commune, 3, "left", 0),
         depcom = paste0(code_dept, code_commune),
         extdr_voix = as.numeric(extdr_voix),
         total_vote = as.numeric(total_vote),
         extdr1986_communes = extdr_voix / total_vote * 100) %>% 
  rowwise() %>% 
  mutate(lib_commune_2023 = list(ComByCode(depcom)[1])) %>% 
  unnest(lib_commune_2023) %>% 
  rename("lib_commune2023" = "name") %>% 
  relocate(lib_commune2023, .after = lib_commune) %>% 
  relocate(extdr1986_communes, .after = lib_commune2023) %>% 
  relocate(depcom, .after = code_commune) %>% 
  select(1:7)


#this is a table that matches communes to 1986 circonscriptions
#beware, this is a little barbaric
tmp_circo1986 <- read_excel("instrument_data/table_circo2012-1986.xls", guess_max = 25000) %>% 
  rename("code_dept" = "Code département",
         "lib_dept" = "Nom département",
         "code_commune" = "Code commune",
         "lib_commune" = "Nom commune",
         "code_circ1986" = "Circ. législative 1986",
         "code_circ2012" = "Circ. législative 2012") %>% 
  select(code_dept, code_commune, lib_commune, code_circ1986) %>% 
  mutate(code_dept = str_pad(code_dept, 2, "left", 0),
         code_commune = str_pad(code_commune, 3, "left", 0),
         depcom = paste0(code_dept, code_commune),
         hascommune = depcom %in% communes_leg1986$depcom,
         code_circ1986 = ifelse(grepl("et", code_circ1986) & hascommune == TRUE,
                                NA,
                                code_circ1986),
         code_circ1986 = ifelse(code_dept == 69 & grepl("123", code_commune) & hascommune == FALSE,
                                NA,
                                code_circ1986)) %>% 
  distinct() %>% 
  na.omit()
  
extract = tmp_circo1986 %>% 
  filter(grepl("et", code_circ1986)) %>% 
  separate(code_circ1986, into = c("code_circ1986_1", "code_circ1986_2"), sep = "et", remove = FALSE) 

random_choice <- as.character(sample(3:4, 1))
extract[99, 5] <- random_choice

extract = extract %>% 
  mutate(code_circ1986_1 = as.numeric(code_circ1986_1), 
         code_circ1986_2 = as.numeric(code_circ1986_2),
         code_circ1986 = map2_dbl(code_circ1986_1, code_circ1986_2, ~sample(c(.x, .y), 1))) %>% 
  select(-code_circ1986_1, -code_circ1986_2)

map_circo1986 = tmp_circo1986 %>% 
  filter(!grepl("et", code_circ1986)) %>% 
  mutate(code_circ1986 = as.numeric(code_circ1986)) %>% 
  bind_rows(extract) %>% 
  mutate(depcom = substr(start = 1, stop = 5, depcom)) %>% 
  group_by(depcom) %>% 
  slice(1)

res1_merge = map_circo1986 %>% 
  full_join(communes_leg1986, by = c("code_dept", "depcom")) %>% 
  mutate(code_commune = coalesce(code_commune.x, code_commune.y),
         lib_commune = coalesce(lib_commune.x, lib_commune.y)) %>% 
  select(-c(code_commune.x, code_commune.y, lib_commune.x, lib_commune.y)) %>% 
  relocate(depcom, .after = code_commune) %>% 
  relocate(c(lib_commune, lib_commune2023), .after = code_dept) %>% 
  relocate(c(code_commune, depcom), .after = lib_commune2023) %>% 
  relocate(lib_dept, .after = code_dept)
  

#this is a table for 1986 electoral results at the circonscription level
circo_leg1986 <- read_csv("elections_data/legislative/cdsp_legi1986_circ.csv") %>%
  rename("code_dept" = "code département",
         "lib_dept" = "département",
         "code_circ1986" = "circonscription",
         "total_vote" = "exprimés",
         "extdr_voix" = "FRN") %>% 
  select(code_dept, lib_dept, code_circ1986, total_vote, extdr_voix) %>% 
  mutate(extdr_leg = extdr_voix / total_vote * 100) %>% 
  select(-c(extdr_voix, total_vote, lib_dept)) %>% 
  mutate(code_dept = str_pad(code_dept, 2, "left", 0))

res2_merge = res1_merge %>% 
  left_join(circo_leg1986, by = c("code_dept", "code_circ1986")) %>% 
  rename("extdr1986_circo" = "extdr_leg") %>% 
  mutate(extdr1986 = ifelse(!is.na(extdr1986_communes),
                            extdr1986_communes,
                            extdr1986_circo))

legislative86 = res2_merge

#####################################################################################################################################
#changement taux chomage
chomage_loc = read_excel("instrument_data/chomage_loc.xls", sheet = 2, skip = 3) %>%
  rename("code_dept" = "Code",
         "lib_dept" = "Libellé") %>% 
  pivot_longer(cols = starts_with("T"),
               names_to = c("trimester", "year"),
               names_pattern = "T(\\d)_(\\d{4})",
               values_to = "chomage_trim") %>% 
  mutate(year = as.numeric(year),
         trimester = as.numeric(trimester)) %>% 
  group_by(code_dept) %>% 
  mutate(chomage_growth = (chomage_trim - lag(chomage_trim, 1))/lag(chomage_trim, 1)*100) %>%
  na.omit() %>%
  filter(trimester %in% c(1, 2)) %>%
  pivot_wider(names_from = trimester,
              values_from = chomage_growth, 
              names_prefix = "trim_chomage_growth",
              names_sep = "") %>% 
  filter(year %in% c(2007, 2012, 2017)) %>% 
  group_by(code_dept, lib_dept, year) %>% 
  summarize(trim_chomage_growth1 = max(trim_chomage_growth1, na.rm = TRUE),
            trim_chomage_growth2 = max(trim_chomage_growth2, na.rm = TRUE)) %>% 
  filter(!(code_dept %in% c("971", "972", "973", "974"))) %>% 
  ungroup()
  
###################################################################################################################################
#defaillances entreprises

popdept_path = "control_data/estim-pop-dep-sexe-gca-1975-2023.xls"
sheet_names1 = excel_sheets(popdept_path)

tmp_popdept <- lapply(sheet_names1, function(sheet) {
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
  select(-c(lib_dept, pop_growth_rate)) %>% 
  mutate(year = as.numeric(year))

def_entreprises <- read_excel("instrument_data/defaillances_entreprises.xlsx", sheet = 2) %>%
  select(1, starts_with("1997"), starts_with("2002"), starts_with("2007"), starts_with("2012"), starts_with("2017")) %>% 
  rename("lib_dept" = "Libellé") %>% 
  pivot_longer(cols = -1,
               names_to = c("year", "trimester"),
               names_pattern = "^(\\d{4})-T(\\d)$",
               values_to = "entreprises_down") %>%
  mutate(lib_dept = gsub("Nombre de défaillances d'entreprises par date de jugement - Données brutes - ", "", lib_dept),
         year = as.numeric(year),
         trimester = as.numeric(trimester)) %>% 
  separate(col = lib_dept, into = c("lib_dept", "sector"), sep = " - ") %>% 
  filter(sector == "Tous secteurs d'activité")

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
  
tmp_entreprises = def_entreprises %>% 
  select(lib_dept) %>% 
  distinct() %>% 
  mutate(lib_dept_clean = unaccent(gsub(" ", "-", lib_dept))) %>% 
  mutate(lib_dept_clean = case_when(lib_dept_clean == "Cote-d Or" ~ "Cote-d'Or",
                              lib_dept_clean == "Cotes-d Armor" ~ "Cotes-d'Armor",
                              lib_dept_clean == "Ville-de-Paris" ~ "Paris",
                              lib_dept_clean == "Val-d Oise" ~ "Val-d'Oise",
                              lib_dept_clean == "Provence-Alpes-Cote-d Azur" ~ "Provence-Alpes-Cote-d'Azur",
                              .default = lib_dept_clean)) %>% 
  rowwise() %>% 
  mutate(code_dept = dep_by_name(lib_dept_clean)) %>% 
  mutate(code_dept = ifelse(lib_dept_clean == "Loire", "42", code_dept)) %>% 
  filter(lib_dept_clean != "Corse") %>% 
  filter(!is.na(code_dept)) %>% 
  filter(!(code_dept %in% c("971", "972", "973", "974", "TBC"))) %>% 
  select(1, 3)
  
def_entreprises2 = def_entreprises %>% 
  left_join(tmp_entreprises, by = "lib_dept") %>% 
  relocate(code_dept, .before = lib_dept) %>%
  left_join(tmp_popdept, by = c("code_dept", "year")) %>% 
  mutate(trim_prop_entreprise = entreprises_down / pop_dept * 10000) %>% 
  filter(trimester %in% c(1, 2)) %>% 
  select(-c(lib_dept, sector, pop_dept)) %>% 
  na.omit() %>% 
  select(-entreprises_down) %>% 
  pivot_wider(names_from = trimester,
              values_from = trim_prop_entreprise, 
              names_prefix = "trim_entreprise_prop_",
              names_sep = "")


###################################################################################################################################
# here we merge data to construct the dataset that includes all the instruments

instrument_merge = legislative86 %>% 
  left_join(chomage_loc, by = "code_dept", relationship = "many-to-many") %>% 
  relocate(depcom, .before = lib_commune) %>% 
  relocate(extdr1986, .after = year) %>% 
  left_join(def_entreprises2, by = c("code_dept", "year")) %>% 
  rename("code_insee" = "depcom") %>% 
  select(-lib_commune, -code_commune) %>% 
  ungroup() %>% 
  rename("lib_dept" = "lib_dept.y") %>% 
  relocate(lib_dept, .after = code_dept) %>% 
  select(-lib_dept.x) %>% 
  select(-(4:8)) 

####################################################################################################################################
# here we merge data for the instruments with our main dataset computed in code_merge

instrument_extend_merge = extend_merge %>% 
  select(-nrow, -lib_dept) %>%
  left_join(instrument_merge, by = c("year", "code_dept", "code_insee")) %>% 
  relocate(cantville, .after = lib_dept) %>% 
  relocate(code_insee, .after = cantville) %>% 
  ungroup()

instrument_extend_merge %>% 
  select(-geometry) %>% 
  write_dta("final_data/instrument_merge.dta")
  