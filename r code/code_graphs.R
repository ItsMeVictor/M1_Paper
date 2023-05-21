library(pacman)
p_load(haven, data.table, purrr, readxl, rgeoapi, tidyverse, sf, mapsf, viridis, patchwork)

setwd("/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4.sciences_po/work/semestre_2/serranito_advancedeconometrics/serranito_paper")

# setwd("/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4. sciences po/work/semestre 2/serranito - advanced econometrics/serranito - paper/data/")

communes_shp <- st_read("commune_shapefile/communes-20220101-shp/communes-20220101.shp") %>%
  rename("code_insee" = "insee") %>%
  mutate(depcom = code_insee) %>%
  select(code_insee, depcom, geometry)

############################################################################################################  
#map 2017 concentration of immigrants (based on df from code_merge.R)

immi2017 = final_merge %>% 
  filter(year == 2017) %>% 
  select(immi_prop, geometry, cantville, code_dept) %>% 
  filter(!str_starts(code_dept, "2A")) %>% 
  filter(!str_starts(code_dept, "2B")) %>%
  st_as_sf %>% 
  st_transform(., crs = 2154)

pdf("/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4.sciences_po/work/semestre_2/serranito_advancedeconometrics/serranito_paper/serranito_paperitself/immigrants2017.pdf", 
    width = 8, 
    height = 12, 
    compress = TRUE)
mf_map(immi2017,
       var = "immi_prop",
       type = "choro",
       breaks = mf_get_breaks(immi2017$immi_prop,
                              breaks = "quantile"),
       nbreaks = length(mf_get_breaks(immi2017$immi_prop,
                                      breaks = "quantile")),
       pal = viridis(n = length(mf_get_breaks(immi2017$immi_prop,
                                              breaks = "quantile")),
                     option = "F",
                     begin = 0.3,
                     end = 0.9,
                     direction = -1),
       lwd = 0.5,
       leg_title = "Proportion of immigrants",
       leg_val_cex = 0.60,
       leg_frame = TRUE,
       leg_pos = "topleft")
dev.off()

############################################################################################################
#map 2017 concentration of immigrants (based on df from code_merge.R)

extdr2017 = leg2017 %>% 
  left_join(communes_shp, by = "depcom") %>% 
  filter(!str_starts(code_dept, "2A")) %>% 
  filter(!str_starts(code_dept, "2B")) %>% 
  select(extdr_leg, geometry) %>% 
  st_as_sf %>% 
  st_transform(., crs = 2154)

pdf("/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4.sciences_po/work/semestre_2/serranito_advancedeconometrics/serranito_paper/serranito_paperitself/extdrvote2017.pdf", 
    width = 8, 
    height = 12, 
    compress = TRUE)
mf_map(extdr2017, 
       var = "extdr_leg",
       type = "choro",
       breaks = mf_get_breaks(extdr2017$extdr_leg,
                              breaks = "quantile"),
       nbreaks = length(mf_get_breaks(extdr2017$extdr_leg,
                                      breaks = "quantile"),),
       pal = viridis(n = length(mf_get_breaks(extdr2017$extdr_leg, breaks = "quantile")),
                     option = "G",
                     begin = 0.3,
                     end = 0.9,
                     direction = -1),
       lwd = 0.5,
       leg_title = "Far-right vote shares",
       leg_val_cex = 0.60,
       leg_frame = TRUE,
       leg_pos = "topleft")
dev.off()

###########################################################################################################
# far-right voting histograms
plot_vote <- function(data, time){
  plot <- data %>%
    filter(year == time) %>%
    ggplot(aes(x = extdr_leg)) +
    geom_histogram(bins = 100, fill = 'dodgerblue2', color = "grey", size = 0.01) +
    theme_minimal() +
    labs(x = "",
         y = "Frequency") +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 20))

  return(plot)
}

vote2007 <- plot_vote(final_merge, "2007")
vote2012 <- plot_vote(final_merge, "2012")
vote2017 <- plot_vote(final_merge, "2017")

ggsave("vote2007.pdf", vote2007, device = "pdf", dpi = 500, units = "cm", path = "serranito_paperitself_new/")
ggsave("vote2012.pdf", vote2012, device = "pdf", dpi = 500, units = "cm", path = "serranito_paperitself_new/")
ggsave("vote2017.pdf", vote2017, device = "pdf", dpi = 500, units = "cm", path = "serranito_paperitself_new/")


###########################################################################################################
#proportion of immigrants histograms

plot_immi <- function(data, time){
  plot <- data %>%
    filter(year == time) %>%
    ggplot(aes(x = immi_prop)) +
    geom_histogram(bins = 80, fill = 'salmon2', color = "grey", size = 0.01) +
    theme_minimal() +
    labs(x = "",
         y = "Frequency") +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 20))
  
  return(plot)
}

immi2007 <- plot_immi(final_merge, "2007")
immi2012 <- plot_immi(final_merge, "2012")
immi2017 <- plot_immi(final_merge, "2017")

ggsave("immi2007.pdf", immi2007, device = "pdf", dpi = 500, units = "cm", path = "serranito_paperitself_new/")
ggsave("immi2012.pdf", immi2012, device = "pdf", dpi = 500, units = "cm", path = "serranito_paperitself_new/")
ggsave("immi2017.pdf", immi2017, device = "pdf", dpi = 500, units = "cm", path = "serranito_paperitself_new/")