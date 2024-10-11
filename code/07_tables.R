packages <- c("tidyverse", "here", "openxlsx", "janitor")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)
select <- dplyr::select
rm(packages)

## Load the database

list_database <- read_rds(here("output", "data", "database_list_main.rds"))
nimet <- list_database %>% names()

groups <- c(
  "rttd_m_c",
  "rttd_m",
  "rttd_f",
  "rttw_m",
  "rttw_f",
  "rtf_m",
  "rtf_f",
  "rth_m",
  "rth_f",
  "rtk_m",
  "rtk_f",
  "unknown")

cv <- function(vektori){
  v1 <- sd(vektori, na.rm =T)/mean(vektori, na.rm = T)
  v1 <- round(v1, digits = 2)
  return(v1)
}

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


## Tables by element

### Femur

fig1_fem <- list_database$femur %>% 
  #drop_na() %>%
  filter(!subspecies %in% c("hybrid", "no")) %>% 
  mutate(group = droplevels(as.factor(group))) %>% 
  mutate(group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  select(group, fem_gl:fem_pl) %>% 
  pivot_longer(cols = fem_gl:fem_pl,
               names_to = "variable",
               values_to = "value") %>% 
  group_by(group, variable) %>% 
  summarise(N = n(), Mean = mean(value), CV = cv(value), Median = median(value), Min = min(value), Max = max(value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "group",
              values_from = c(N, Mean, CV, Median, Min, Max)) %>% 
  mutate(variable = str_replace_all(variable,
                                    "fem_",
                                    "")) %>% 
  mutate(element = "femur") %>% 
  relocate(element)

### Humerus

fig1_hum <- list_database$humerus %>% 
  #drop_na() %>%
  filter(!subspecies %in% c("hybrid", "no")) %>% 
  mutate(group = as.factor(group)) %>% 
  mutate(group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  select(group, hum_gl:hum_pl) %>% 
  pivot_longer(cols = hum_gl:hum_pl,
               names_to = "variable",
               values_to = "value") %>% 
  group_by(group, variable) %>% 
  summarise(N = n(), Mean = mean(value), CV = cv(value), Median = median(value), Min = min(value), Max = max(value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "group",
              values_from = c(N, Mean, CV, Median, Min, Max)) %>% 
  mutate(variable = str_replace_all(variable,
                                    "hum_",
                                    "")) %>% 
  mutate(element = "humerus") %>% 
  relocate(element)

### metacarpus

fig1_mec <- list_database$metacarpus %>% 
  #drop_na() %>%
  filter(group %in% c(1:7)) %>% 
  mutate(group = as.factor(group)) %>% 
  mutate(group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  select(group, mec_gl:mec_dfp) %>% 
  pivot_longer(cols = mec_gl:mec_dfp,
               names_to = "variable",
               values_to = "value") %>% 
  group_by(group, variable) %>% 
  summarise(N = n(), Mean = mean(value, na.rm = T), CV = cv(value), Median = median(value, na.rm = T), Min = min(value, na.rm = T), Max = max(value, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "group",
              values_from = c(N, Mean, CV, Median, Min, Max)) %>% 
  mutate(variable = str_replace_all(variable,
                                    "mec_",
                                    "")) %>% 
  mutate(element = "metacarpus") %>% 
  relocate(element)

### metatarsus

fig1_met <- list_database$metatarsus %>% 
  #drop_na() %>%
  filter(group %in% c(1:7)) %>% 
  mutate(group = as.factor(group)) %>% 
  mutate(group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  select(group, met_gl:met_dfp) %>% 
  pivot_longer(cols = met_gl:met_dfp,
               names_to = "variable",
               values_to = "value") %>% 
  group_by(group, variable) %>% 
  summarise(N = n(), Mean = mean(value, na.rm = T), CV = cv(value), Median = median(value, na.rm = T), Min = min(value, na.rm = T), Max = max(value, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "group",
              values_from = c(N, Mean, CV, Median, Min, Max)) %>% 
  mutate(variable = str_replace_all(variable,
                                    "met_",
                                    "")) %>% 
  mutate(element = "metatarsus") %>% 
  relocate(element)

### pelvis

fig1_pel <- list_database$pelvis %>% 
  #drop_na() %>%
  filter(group %in% c(1:7)) %>% 
  mutate(group = as.factor(group)) %>% 
  mutate(group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7"),
         subspecies = droplevels(subspecies),
         castrated = droplevels(castrated)) %>% 
  filter(group != "rttd_m_c") %>% 
  mutate(group = droplevels(group)) %>% 
  select(group, pel_la, pel_lar, pel_sh, pel_sb, pel_sc, pel_lfo, pel_dpmin, pel_dam, pel_dps) %>% 
  pivot_longer(cols = c(pel_la, pel_lar, pel_sh, pel_sb, pel_sc, pel_lfo, pel_dpmin, pel_dam, pel_dps),
               names_to = "variable",
               values_to = "value") %>% 
  group_by(group, variable) %>% 
  summarise(N = n(), Mean = mean(value, na.rm = T), CV = cv(value), Median = median(value, na.rm = T), Min = min(value, na.rm = T), Max = max(value, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "group",
              values_from = c(N, Mean, CV, Median, Min, Max)) %>% 
  mutate(variable = str_replace_all(variable,
                                    "pel_",
                                    "")) %>% 
  mutate(element = "pelvis") %>% 
  relocate(element)

### radioulna

fig1_rad <- list_database$radioulna %>% 
 # drop_na() %>%
  filter(group %in% c(1:7)) %>% 
  mutate(group = as.factor(group)) %>% 
  mutate(group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  select(group, raduln_gl:uln_sdo) %>% 
  pivot_longer(cols = raduln_gl:uln_sdo,
               names_to = "variable",
               values_to = "value") %>% 
  group_by(group, variable) %>% 
  summarise(N = n(), Mean = mean(value), CV = cv(value), Median = median(value), Mode = mode(value), Min = min(value), Max = max(value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "group",
              values_from = c(N, Mean, CV, Median, Mode, Min, Max)) %>% 
  mutate(variable = str_replace_all(variable,
                                    "rad_",
                                    ""),
         variable = str_replace_all(variable,
                                    "raduln_",
                                    ""),
         variable = str_replace_all(variable,
                                    "uln_",
                                    ""),) %>% 
  mutate(element = "radioulna") %>% 
  relocate(element)

### tibia

fig1_tib <- list_database$tibia %>% 
  #drop_na() %>%
  filter(group %in% c(1:7)) %>% 
  mutate(group = as.factor(group)) %>% 
  mutate(group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  select(group, tib_gl:tib_bfd) %>% 
  pivot_longer(cols = tib_gl:tib_bfd,
               names_to = "variable",
               values_to = "value") %>% 
  group_by(group, variable) %>% 
  summarise(N = n(), Mean = mean(value), CV = cv(value), Median = median(value), Mode = mode(value), Min = min(value), Max = max(value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "group",
              values_from = c(N, Mean, CV, Median, Mode, Min, Max)) %>% 
  mutate(variable = str_replace_all(variable,
                                    "tib_",
                                    "")) %>% 
  mutate(element = "tibia") %>% 
  relocate(element)


### Full table

fig1_full <- bind_rows(
  fig1_fem,
  fig1_hum,
  fig1_mec,
  fig1_met,
  fig1_pel,
  fig1_rad,
  fig1_tib
) %>% arrange(
  element, variable)

wb1 <- createWorkbook(creator = "Henri Wallen")
addWorksheet(wb1, "Femur") 
addWorksheet(wb1, "Humerus")
addWorksheet(wb1, "Metacarpus") 
addWorksheet(wb1, "Metatarsus")
addWorksheet(wb1, "Pelvis")
addWorksheet(wb1, "Radioulna")
addWorksheet(wb1, "Tibia")
addWorksheet(wb1, "Total chaos") 

writeData(wb1,
          "Femur",
          fig1_fem,
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Humerus",
          fig1_hum,
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Metacarpus",
          fig1_mec,
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Metatarsus",
          fig1_met,
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Pelvis",
          fig1_pel,
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Radioulna",
          fig1_rad,
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Tibia",
          fig1_tib,
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Total chaos",
          fig1_full,
          startRow = 1,
          startCol = 1)


saveWorkbook(wb = wb1,
             file = paste0(here("output", "tables"), "/",today(), "_descriptivestats.xlsx"),
             overwrite = T)


# Percent differences -----------------------------------------------------

df_per <- fig1_full %>% 
  select(1:2, contains("mean")) 
names(df_per) <- str_replace_all(names(df_per), "Mean_", "")

perdiff <- function(x,y){
  abs((x - y)/((x+y)/2))*100
}


df_per %>%
  mutate(rttd_m_cVVVrttd_m = perdiff(rttd_m_c, rttd_m),
         rttd_m_cVVVrttd_f = perdiff(rttd_m_c, rttd_f),
         rttd_m_cVVVrttw_m = perdiff(rttd_m_c, rttw_m),
         rttd_m_cVVVrttw_f = perdiff(rttd_m_c, rttw_f),
         rttd_m_cVVVrtf_m = perdiff(rttd_m_c, rtf_m),
         rttd_m_cVVVrtf_f = perdiff(rttd_m_c, rtf_f),
         rttd_mVVVrttd_f = perdiff(rttd_m, rttd_f),
         rttd_mVVVrttw_m = perdiff(rttd_m, rttw_m),
         rttd_mVVVrttw_f = perdiff(rttd_m, rttw_f),
         rttd_mVVVrtf_m = perdiff(rttd_m, rtf_m),
         rttd_mVVVrtf_f = perdiff(rttd_m, rtf_f),
         rttd_fVVVrttw_m = perdiff(rttd_f, rttw_m),
         rttd_fVVVrttw_f = perdiff(rttd_f, rttw_f),
         rttd_fVVVrtf_m = perdiff(rttd_f, rtf_m),
         rttd_fVVVrtf_f = perdiff(rttd_f, rtf_f),
         rttw_mVVVrttw_f = perdiff(rttw_m, rttw_f),
         rttw_mVVVrtf_m = perdiff(rttw_m, rtf_m),
         rttw_mVVVrtf_f = perdiff(rttw_m, rtf_f),
         rttw_fVVVrtf_m = perdiff(rttw_f, rtf_m),
         rttw_fVVVrtf_f = perdiff(rttw_f, rtf_f),
         rtf_mVVVrtf_f = perdiff(rtf_m, rtf_f)) %>% 
  write.xlsx(here("output","tables", "percent_difference.xlsx"))

# Descriptive, observation per element ------------------------------------

fig2_fem <- list_database$femur %>% 
  filter(!subspecies %in% c("hybrid", "no")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  group_by(collection, subspecies, sex, castrated) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(element = "Femur") %>% 
  relocate(collection, element)

fig2_hum <- list_database$humerus %>% 
  filter(!subspecies %in% c("hybrid", "no")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  group_by(collection, subspecies, sex, castrated) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(element = "Humerus") %>% 
  relocate(collection, element)

fig2_mec <- list_database$metacarpus %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  group_by(collection, subspecies, sex, castrated) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(element = "Metacarpus") %>% 
  relocate(collection, element)

fig2_met <- list_database$metatarsus %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  filter(sex %in% c("m", "f")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  group_by(collection, subspecies, sex, castrated) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(element = "Metatarsus") %>% 
  relocate(collection, element)

fig2_pel <- list_database$pelvis %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  filter(sex %in% c("m", "f")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            #'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  group_by(collection, subspecies, sex, castrated) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(element = "Pelvis") %>% 
  relocate(collection, element)


fig2_rad <- list_database$radioulna %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  filter(sex %in% c("m", "f")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  group_by(collection, subspecies, sex, castrated) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(element = "Radioulna") %>% 
  relocate(collection, element)

fig2_tib <- list_database$tibia %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  filter(sex %in% c("m", "f")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  group_by(collection, subspecies, sex, castrated) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(element = "Tibia") %>% 
  relocate(collection, element)

desc_full <- bind_rows(
  fig2_fem,
  fig2_hum,
  fig2_mec,
  fig2_met,
  fig2_pel,
  fig2_rad,
  fig2_tib) %>% 
  mutate(sex = fct_recode(sex,
                          "Female" = "f",
                          "Male" = "m"),
         castrated = fct_recode(castrated,
                                "No" = "no",
                                "Yes" = "yes")) %>% 
  rename("Collection" = "collection",
         "Element" = "element",
         "Subspecies" = "subspecies",
         "Sex" = "sex",
         "Castrated" = "castrated",
         "N" = "n") %>% 
  arrange(Element, Subspecies, Sex)


# Simple table ------------------------------------------------------------


fig2_fem <- list_database$femur %>% 
  filter(!subspecies %in% c("hybrid", "no")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  group_by(subspecies, sex, castrated) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(element = "Femur") %>% 
  relocate(element)

fig2_hum <- list_database$humerus %>% 
  filter(!subspecies %in% c("hybrid", "no")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  group_by(subspecies, sex, castrated) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(element = "Humerus") %>% 
  relocate(element)

fig2_mec <- list_database$metacarpus %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  group_by(subspecies, sex, castrated) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(element = "Metacarpus") %>% 
  relocate(element)

fig2_met <- list_database$metatarsus %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  filter(sex %in% c("m", "f")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  group_by(subspecies, sex, castrated) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(element = "Metatarsus") %>% 
  relocate(element)

fig2_pel <- list_database$pelvis %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  filter(sex %in% c("m", "f")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            #'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  group_by(subspecies, sex, castrated) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(element = "Pelvis") %>% 
  relocate(element)

fig2_rad <- list_database$radioulna %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  filter(sex %in% c("m", "f")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  group_by(subspecies, sex, castrated) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(element = "Radioulna") %>% 
  relocate(element)

fig2_tib <- list_database$tibia %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  filter(sex %in% c("m", "f")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  group_by(subspecies, sex, castrated) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(element = "Tibia") %>% 
  relocate(element)

desc_simple <- bind_rows(
  fig2_fem,
  fig2_hum,
  fig2_mec,
  fig2_met,
  fig2_rad,
  fig2_tib) %>% 
  mutate(sex = fct_recode(sex,
                          "Female" = "f",
                          "Male" = "m"),
         castrated = fct_recode(castrated,
                                "No" = "no",
                                "Yes" = "yes")) %>% 
  rename("Element" = "element",
         "Subspecies" = "subspecies",
         "Sex" = "sex",
         "Castrated" = "castrated",
         "N" = "n") %>% 
  arrange(Element, Subspecies, Sex)

desc_collection <- desc_full %>% 
  group_by(Collection, Element) %>% 
  summarise(N = sum(N))

# Specimen table ----------------------------------------------------------


fig2_fem <- list_database$femur %>% 
  filter(!subspecies %in% c("hybrid", "no")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  select(specimen,collection, subspecies, sex, castrated) %>% 
  mutate(element = "Femur") %>% 
  relocate(specimen, collection, element)

fig2_hum <- list_database$humerus %>% 
  filter(!subspecies %in% c("hybrid", "no")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  select(specimen,collection, subspecies, sex, castrated) %>% 
  mutate(element = "Humerus") %>% 
  relocate(specimen, collection, element)

fig2_mec <- list_database$metacarpus %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  select(specimen,collection, subspecies, sex, castrated) %>% 
  mutate(element = "Metacarpus") %>% 
  relocate(specimen, collection, element)

fig2_met <- list_database$metatarsus %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  filter(sex %in% c("m", "f")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  select(specimen,collection, subspecies, sex, castrated) %>% 
  mutate(element = "Metatarsus") %>% 
  relocate(specimen,collection, element)

fig2_pel <- list_database$pelvis %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  filter(sex %in% c("m", "f")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            #'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  select(specimen,collection, subspecies, sex, castrated) %>% 
  mutate(element = "Pelvis") %>% 
  relocate(specimen,collection, element)


fig2_rad <- list_database$radioulna %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  filter(sex %in% c("m", "f")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  select(specimen,collection, subspecies, sex, castrated) %>% 
  mutate(element = "Radioulna") %>% 
  relocate(specimen,collection, element)

fig2_tib <- list_database$tibia %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>% 
  filter(sex %in% c("m", "f")) %>% 
  mutate_if(is.factor, droplevels) %>% 
  mutate(group = factor(group),
         group = fct_recode(group, 
                            'rttd_m_c' = "1",
                            'rttd_m' = "2",
                            'rttd_f' = "3",
                            'rttw_m' = "4",
                            'rttw_f' = "5",
                            'rtf_m' = "6",
                            'rtf_f' = "7")) %>% 
  select(specimen,collection, subspecies, sex, castrated) %>% 
  mutate(element = "Tibia") %>% 
  relocate(specimen,collection, element)

desc_specimen <- bind_rows(
  fig2_fem,
  fig2_hum,
  fig2_mec,
  fig2_met,
  fig2_pel,
  fig2_rad,
  fig2_tib) %>% 
  mutate(sex = fct_recode(sex,
                          "Female" = "f",
                          "Male" = "m"),
         castrated = fct_recode(castrated,
                                "No" = "no",
                                "Yes" = "yes")) %>% 
  rename("Collection" = "collection",
         "Element" = "element",
         "Subspecies" = "subspecies",
         "Sex" = "sex",
         "Castrated" = "castrated") %>% 
  mutate(test = 1) %>% 
  pivot_wider(names_from = "Element",
              values_from = "test") %>% 
  mutate(Femur = case_when(Femur == 1 ~ "Yes",
                           .default = "No"),
         Humerus = case_when(Humerus == 1 ~ "Yes",
                             .default = "No"),
         Metacarpus = case_when(Metacarpus == 1 ~ "Yes",
                           .default = "No"),
         Metatarsus = case_when(Metatarsus == 1 ~ "Yes",
                           .default = "No"),
         Pelvis = case_when(Pelvis == 1 ~ "Yes",
                           .default = "No"),
         Radioulna = case_when(Radioulna == 1 ~ "Yes",
                           .default = "No"),
         Tibia = case_when(Tibia == 1 ~ "Yes",
                           .default = "No"))


# All tables --------------------------------------------------------------


wb1 <- createWorkbook(creator = "Henri Wallen")
addWorksheet(wb1, "Complete Long") 
addWorksheet(wb1, "Simple Long")
addWorksheet(wb1, "Collection Long") 

addWorksheet(wb1, "Complete Wide") 
addWorksheet(wb1, "Simple Wide")
addWorksheet(wb1, "Collection Wide") 

addWorksheet(wb1, "Specimen")

writeData(wb1,
          "Complete Long",
          desc_full,
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Complete Wide",
          desc_full %>% 
            pivot_wider(names_from = "Element", values_from = "N") %>% 
            replace(is.na(.), 0),
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Simple Long",
          desc_simple,
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Simple Wide",
          desc_simple %>% 
            pivot_wider(names_from = "Element", values_from = "N") %>% 
            replace(is.na(.),0),
          startRow = 1,
          startCol = 1)


writeData(wb1,
          "Collection Long",
          desc_collection,
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Collection Wide",
          desc_collection %>% 
            pivot_wider(names_from = "Element", values_from = "N") %>% 
            replace(is.na(.),0),
          startRow = 1,
          startCol = 1)


writeData(wb1,
          "Specimen",
          desc_specimen,
          startRow = 1,
          startCol = 1)

saveWorkbook(wb = wb1,
             file = paste0(here("output", "tables"), "/",today(), "_descriptive_database.xlsx"),
             overwrite = T)


  

