## Load packages

packages <- c("tidyverse","here", "readxl", "openxlsx", "tm", "janitor")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)
select <- dplyr::select
rm(packages)


# Excluded specimen -------------------------------------------------------

exclspecimen <- c("25006", ## Too young
                  "13674", ## Too young
                  "32327", ## Too young
                  "18869", ## Too young
                  "28453", ## Sex unknown
                  "B.M. 444", ## fusion lines visible
                  "B. 1866",  ## fusion lines visible
                  "B.M. 4602", ## fusion lines visible
                  "B. 2077") 


# Load data ---------------------------------------------------------------

df_fem <- read.xlsx(here("data", "db_RenCastration_November2022.xlsx"), 
                     sheet = "Femur") %>% 
  as_tibble() %>% 
  dplyr::select(Specimen, fem_GL:fem_PL, Group, Subspecies, Castrated, Sex, Collection) %>% 
  filter(!(Specimen %in% exclspecimen)) %>% 
  mutate_if(is.character, as.factor) %>% 
  rename_with(tolower)
  
df_hum <- read.xlsx(here("data", "db_RenCastration_November2022.xlsx"), 
                    sheet = "Humerus") %>% 
  tibble() %>% 
  select(Specimen, hum_GL:hum_PL, Group, Subspecies, Castrated, Sex, Collection) %>% 
  filter(!(Specimen %in% exclspecimen)) %>%
  mutate_if(is.character, stripWhitespace) %>% 
  mutate_if(is.character, as.factor) %>% 
  rename_with(tolower)

df_metc <- read.xlsx(here("data", "db_RenCastration_November2022.xlsx"), 
                    sheet = "Metacarpus") %>% 
  tibble() %>% 
  select(Specimen, mec_GL:mec_DFp, Group, Subspecies, Castrated, Sex, Collection) %>% 
  filter(!(Specimen %in% exclspecimen)) %>%
  mutate_if(is.character, as.factor) %>% 
  rename_with(tolower)

df_mett <- read.xlsx(here("data", "db_RenCastration_November2022.xlsx"), 
                     sheet = "Metatarsus") %>% 
  tibble() %>% 
  select(Specimen, met_GL:met_DFp, Group, Subspecies, Castrated, Sex, Collection) %>% 
  filter(!(Specimen %in% exclspecimen)) %>%
  mutate_if(is.character, as.factor) %>% 
  rename_with(tolower)

df_pel <- read.xlsx(here("data", "db_RenCastration_November2022.xlsx"), 
                    sheet = "Pelvis") %>% 
  tibble() %>% 
  select(Specimen, pel_GL:pel_DPS, Group, Subspecies, Castrated, Sex, Collection) %>% 
  filter(!(Specimen %in% exclspecimen)) %>%
  mutate_if(is.character, as.factor) %>% 
  rename_with(tolower)

df_rad <- read.xlsx(here("data", "db_RenCastration_November2022.xlsx"), 
                    sheet = "Radioulna") %>% 
  tibble() %>% 
  select(Specimen, raduln_GL:uln_SDO, Group, Subspecies, Castrated, Sex, Collection) %>% 
  filter(!(Specimen %in% exclspecimen)) %>%
  mutate_if(is.character, as.factor) %>% 
  rename_with(tolower)

df_tib <- read.xlsx(here("data", "db_RenCastration_November2022.xlsx"), 
                    sheet = "Tibia") %>% 
  tibble() %>% 
  select(Specimen, tib_GL:tib_BFd, Group, Subspecies, Castrated, Sex, Collection) %>% 
  filter(!(Specimen %in% exclspecimen)) %>%
  mutate_if(is.character, as.factor) %>% 
  rename_with(tolower)

## Calculate shape and size indeces for each element

### Femur


df_fem <- df_fem %>% 
  left_join(df_fem %>% 
              select(specimen, contains("fem")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_full = sum(c_across(fem_gl:fem_pl))/11) %>% 
              ungroup() %>% 
              mutate(across(fem_gl:fem_pl, ~ .x - iSize_full)) %>% 
              rename_with(~str_c("iShape_full_", .), .cols = contains("fem")),
            by = "specimen") %>% 
  left_join(df_fem %>% 
              select(specimen, contains("fem")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_proximal = sum(c(fem_bp,fem_dc))/2) %>% 
              ungroup() %>% 
              mutate(across(c(fem_bp,fem_dc), ~ .x - iSize_proximal)) %>% 
              rename_with(~str_c("iShape_proximal_", .), .cols = contains("fem")),
            by = "specimen") %>% 
  left_join(df_fem %>% 
              select(specimen, contains("fem")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_distal = sum(c(fem_bd, fem_dd, fem_bt))/3) %>% 
              ungroup() %>% 
              mutate(across(c(fem_bd,fem_dd, fem_bt), ~ .x - iSize_distal)) %>% 
              rename_with(~str_c("iShape_distal_", .), .cols = contains("fem")),
            by = "specimen") %>% 
  left_join(df_fem %>% 
              select(specimen, contains("fem")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_shaft = sum(c(fem_sd, fem_sdd, fem_cd))/3) %>% 
              ungroup() %>% 
              mutate(across(c(fem_cd, fem_sd, fem_sdd), ~ .x - iSize_shaft)) %>% 
              rename_with(~str_c("iShape_shaft_", .), .cols = contains("fem")),
            by = "specimen")  %>% 
  left_join(df_fem %>% 
              select(specimen, contains("fem")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_complete = sum(c(fem_glc, fem_bp, fem_dc, fem_sd, fem_bd, fem_dd, fem_bt))/7) %>% 
              ungroup() %>% 
              mutate(across(c(fem_glc, fem_bp, fem_dc, fem_sd, fem_bd, fem_dd, fem_bt), ~ .x - iSize_complete)) %>% 
              rename_with(~str_c("iShape_complete_", .), .cols = contains("fem")),
            by = "specimen")  %>%  
  pivot_longer(cols = c(contains("fem_"), contains("iSize")),
               names_to = "variable",
               values_to = "value") %>% 
  mutate(variable = str_replace_all(variable,
                                    "fem_", ""),
         element = "femur")  %>% 
  relocate(element, collection, specimen, subspecies, group, sex, castrated)

## Humerus

df_hum <- df_hum %>% 
  left_join(df_hum %>% 
              select(specimen, contains("hum")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_full = sum(c_across(hum_gl:hum_pl))/14) %>% 
              ungroup() %>% 
              mutate(across(hum_gl:hum_pl, ~ .x - iSize_full)) %>% 
              rename_with(~str_c("iShape_full_", .), .cols = contains("hum")),
            by = "specimen") %>% 
  left_join(df_hum %>% 
              select(specimen, contains("hum")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_proximal = sum(c(hum_bp,hum_dc, hum_dp))/3) %>% 
              ungroup() %>% 
              mutate(across(c(hum_bp,hum_dc, hum_dp), ~ .x - iSize_proximal)) %>% 
              rename_with(~str_c("iShape_proximal_", .), .cols = contains("hum")),
            by = "specimen") %>% 
  left_join(df_hum %>% 
              select(specimen, contains("hum")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_distal = sum(c(hum_bd, hum_bt, hum_ht, hum_htc, hum_dd))/5) %>% 
              ungroup() %>% 
              mutate(across(c(hum_bd, hum_bt, hum_ht, hum_htc, hum_dd), ~ .x - iSize_distal)) %>% 
              rename_with(~str_c("iShape_distal_", .), .cols = contains("hum")),
            by = "specimen") %>% 
  left_join(df_hum %>% 
              select(specimen, contains("hum")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_shaft = sum(c(hum_sd, hum_cd))/2) %>% 
              ungroup() %>% 
              mutate(across(c(hum_sd, hum_cd), ~ .x - iSize_shaft)) %>% 
              rename_with(~str_c("iShape_shaft_", .), .cols = contains("hum")),
            by = "specimen") %>% 
  left_join(df_hum %>% 
              select(specimen, contains("hum")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_complete = sum(c(hum_glc, hum_bp, hum_sd, hum_bt, hum_ht, hum_dc, hum_dp, hum_dd))/8) %>% 
              ungroup() %>% 
              mutate(across(c(hum_glc, hum_bp, hum_sd, hum_bt, hum_ht, hum_dc, hum_dp, hum_dd), ~ .x - iSize_complete)) %>% 
              rename_with(~str_c("iShape_complete_", .), .cols = contains("hum")),
            by = "specimen") %>%
  pivot_longer(cols = c(contains("hum_"), contains("iSize")),
               names_to = "variable",
               values_to = "value")  %>% 
  mutate(variable = str_replace_all(variable,
                                    "hum_", ""),
         element = "humerus") %>% 
  relocate(element, collection, specimen, subspecies, group, sex, castrated)

df_metc <- df_metc %>% 
  slice(-29) %>% 
  left_join(df_metc %>% 
              select(specimen, contains("mec")) %>% 
              slice(-29) %>%  
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_full = sum(c_across(mec_gl:mec_dfp), na.rm = F)/18) %>% 
              ungroup() %>% 
              mutate(across(mec_gl:mec_dfp, ~ .x - iSize_full)) %>% 
              rename_with(~str_c("iShape_full_", .), .cols = contains("mec")), 
              by = "specimen") %>% 
  left_join(df_metc %>% 
              select(specimen, contains("mec")) %>% 
              slice(-29) %>%  
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_proximal = sum(c(mec_bp,mec_bap, mec_dp, mec_dfp))/4) %>% 
              ungroup() %>% 
              mutate(across(c(mec_bp,mec_bap, mec_dp, mec_dfp), ~ .x - iSize_proximal)) %>% 
              rename_with(~str_c("iShape_proximal_", .), .cols = contains("mec")), 
            by = "specimen") %>% 
  left_join(df_metc %>% 
              select(specimen, contains("mec")) %>% 
              slice(-29) %>%  
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_distal = sum(c(mec_bd, mec_btm, mec_btl, mec_dvm, mec_dvl, mec_bdf, mec_ba, mec_sdd))/8) %>% 
              ungroup() %>% 
              mutate(across(c(mec_bd, mec_btm, mec_btl, mec_dvm, mec_dvl, mec_bdf, mec_ba, mec_sdd), ~ .x - iSize_distal)) %>%               rename_with(~str_c("iShape_distal_", .), .cols = contains("mec")), 
            by = "specimen") %>% 
  left_join(df_metc %>% 
              select(specimen, contains("mec")) %>% 
              slice(-29) %>%  
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_shaft = sum(c(mec_sd, mec_cd))/2) %>% 
              ungroup() %>% 
              mutate(across(c(mec_sd, mec_cd), ~ .x - iSize_shaft)) %>% 
              rename_with(~str_c("iShape_shaft_", .), .cols = contains("mec")), 
            by = "specimen") %>% 
  left_join(df_metc %>% 
              select(specimen, contains("mec")) %>% 
              slice(-29) %>%  
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_complete = sum(c(mec_gl, mec_bp, mec_dp, mec_sd, mec_bd, mec_bdf, mec_ba))/7) %>% 
              ungroup() %>% 
              mutate(across(c(mec_gl, mec_bp, mec_dp, mec_sd, mec_bd, mec_bdf, mec_ba), ~ .x - iSize_complete)) %>% 
              rename_with(~str_c("iShape_complete_", .), .cols = contains("mec")), 
            by = "specimen") %>% 
  pivot_longer(cols = c(contains("mec_"), contains("iSize")),
               names_to = "variable",
               values_to = "value")  %>% 
  mutate(variable = str_replace_all(variable,
                                    "mec_", ""),
         element = "metacarpus") %>% 
  relocate(element, collection, specimen, subspecies, group, sex, castrated)

df_mett <- df_mett %>% 
  left_join(df_mett %>% 
              select(specimen, contains("met")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_full = sum(c_across(met_gl:met_dfp), na.rm = F)/18) %>% 
              ungroup() %>% 
              mutate(across(met_gl:met_dfp, ~ .x - iSize_full)) %>% 
              rename_with(~str_c("iShape_full_", .), .cols = contains("met")),
            by = "specimen") %>% 
  left_join(df_mett %>% 
              select(specimen, contains("met")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_proximal = sum(c(met_bp,met_bap, met_dp, met_dfp))/4) %>% 
              ungroup() %>% 
              mutate(across(c(met_bp,met_bap, met_dp, met_dfp), ~ .x - iSize_proximal)) %>% 
              rename_with(~str_c("iShape_proximal_", .), .cols = contains("met")),
            by = "specimen") %>% 
  left_join(df_mett %>% 
              select(specimen, contains("met")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_distal = sum(c(met_ba, met_bd, met_bdf, met_btl, met_btm, met_dvl, met_dvm, met_sdd))/8) %>% 
              ungroup() %>% 
              mutate(across(c(met_ba, met_bd, met_bdf, met_btl, met_btm, met_dvl, met_dvm, met_sdd), ~ .x - iSize_distal)) %>% 
              rename_with(~str_c("iShape_distal_", .), .cols = contains("met")),
            by = "specimen") %>% 
  left_join(df_mett %>% 
              select(specimen, contains("met")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_shaft = sum(c(met_cd, met_sd))/2) %>% 
              ungroup() %>% 
              mutate(across(c(met_cd, met_sd), ~ .x - iSize_shaft)) %>% 
              rename_with(~str_c("iShape_shaft_", .), .cols = contains("met")),
            by = "specimen") %>% 
  left_join(df_mett %>% 
              select(specimen, contains("met")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_complete = sum(c(met_ba, met_bd, met_bdf, met_bp, met_dp, met_gl, met_sd))/7) %>% 
              ungroup() %>% 
              mutate(across(c(met_ba, met_bd, met_bdf, met_bp, met_dp, met_gl, met_sd), ~ .x - iSize_complete)) %>% 
              rename_with(~str_c("iShape_complete_", .), .cols = contains("met")),
            by = "specimen") %>% 
  pivot_longer(cols = c(contains("met_"), contains("iSize")),
               names_to = "variable",
               values_to = "value")  %>% 
  mutate(variable = str_replace_all(variable,
                                    "met_", ""),
         element = "metatarsus") %>% 
  relocate(element, collection, specimen, subspecies, group, sex, castrated)

df_pel <- df_pel %>% 
  left_join(df_pel %>% 
              select(specimen, contains("pel")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_full = sum(c_across(pel_gl:pel_dps), na.rm = F)/15) %>% 
              ungroup() %>% 
              mutate(across(pel_gl:pel_dps, ~ .x - iSize_full)) %>% 
              rename_with(~str_c("iShape_full_", .), .cols = contains("pel")),
            by = "specimen") %>% 
  left_join(df_pel %>% 
              select(specimen, contains("pel")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_complete = sum(c(pel_dam, pel_dpmin, pel_dps, pel_gba, pel_gl, pel_la, pel_lfo, pel_sh))/8) %>% 
              ungroup() %>% 
              mutate(across(c(pel_dam, pel_dpmin, pel_dps, pel_gba, pel_gl, pel_la, pel_lfo, pel_sh), ~ .x - iSize_complete)) %>% 
              rename_with(~str_c("iShape_complete_", .), .cols = contains("pel")),
            by = "specimen") %>%
    pivot_longer(cols = c(contains("pel_"), contains("iSize")),
               names_to = "variable",
               values_to = "value")  %>% 
  mutate(variable = str_replace_all(variable,
                                    "pel_", ""),
         element = "pelvis") %>% 
  relocate(element, collection, specimen, subspecies, group, sex, castrated)


df_rad <- df_rad %>% 
  left_join(df_rad %>% 
              select(specimen, 2:13) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_full = sum(c_across(raduln_gl:uln_sdo), na.rm = F)/12) %>% 
              ungroup() %>% 
              mutate(across(raduln_gl:uln_sdo, ~ .x - iSize_full)) %>% 
              rename_with(~str_c("iShape_full_", .), .cols = raduln_gl:uln_sdo),
            by = "specimen")  %>% 
  left_join(df_rad %>% 
              select(specimen, 2:13) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_proximal = sum(c(rad_bfp,rad_bp, rad_dp, uln_lo, uln_sdo))/5) %>% 
              ungroup() %>% 
              mutate(across(c(rad_bfp,rad_bp, rad_dp, uln_lo, uln_sdo), ~ .x - iSize_proximal)) %>% 
              rename_with(~str_c("iShape_proximal_", .), .cols = c(rad_bfp,rad_bp, rad_dp, uln_lo, uln_sdo)) %>% 
              select(specimen, contains("iSize"), contains("iShape")),
            by = "specimen")  %>% 
  left_join(df_rad %>% 
              select(specimen, 2:13) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_distal = sum(c(raduln_bd, rad_dd))/2) %>% 
              ungroup() %>% 
              mutate(across(c(raduln_bd, rad_dd), ~ .x - iSize_distal)) %>% 
              rename_with(~str_c("iShape_distal_", .), .cols = c(raduln_bd, rad_dd)) %>% 
              select(specimen, contains("iSize"), contains("iShape")),
            by = "specimen")  %>% 
  left_join(df_rad %>% 
              select(specimen, 2:13) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_shaft = sum(c(rad_sd, rad_sdd))/2) %>% 
              ungroup() %>% 
              mutate(across(c(rad_sd, rad_sdd), ~ .x - iSize_shaft)) %>% 
              rename_with(~str_c("iShape_shaft_", .), .cols = c(rad_sd, rad_sdd)) %>% 
              select(specimen, contains("iSize"), contains("iShape")),
            by = "specimen")  %>% 
  left_join(df_rad %>% 
              select(specimen, 2:13) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_complete = sum(c(raduln_bd, rad_bp, rad_dd, rad_dp, uln_lo, rad_pl, rad_sd, uln_sdo))/8) %>% 
              ungroup() %>% 
              mutate(across(c(raduln_bd, rad_bp, rad_dd, rad_dp, uln_lo, rad_pl, rad_sd, uln_sdo), ~ .x - iSize_complete)) %>% 
              rename_with(~str_c("iShape_complete_", .), .cols = c(raduln_bd, rad_bp, rad_dd, rad_dp, uln_lo, rad_pl, rad_sd, uln_sdo)) %>% 
              select(specimen, contains("iSize"), contains("iShape")),
            by = "specimen") %>%
  pivot_longer(cols = c(contains("rad_"), contains("uln_"), contains("iSize")),
               names_to = "variable",
               values_to = "value")  %>% 
  mutate(variable = str_replace_all(variable,
                                    "raduln_", ""),
         variable = str_replace_all(variable,
                                    "rad_", ""),
         variable = str_replace_all(variable,
                                    "uln_", ""),
         element = "radioulna") %>% 
  relocate(element, collection, specimen, subspecies, group, sex, castrated)

df_tib <- df_tib %>% 
  left_join(df_tib %>% 
              select(specimen, contains("tib")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_full = sum(c_across(tib_gl:tib_bfd), na.rm = F)/12) %>% 
              ungroup() %>% 
              mutate(across(tib_gl:tib_bfd, ~ .x - iSize_full)) %>% 
              rename_with(~str_c("iShape_full_", .), .cols = contains("tib")),
            by = "specimen")  %>% 
  left_join(df_tib %>% 
              select(specimen, contains("tib")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_proximal = sum(c(tib_bfp, tib_dp))/2) %>% 
              ungroup() %>% 
              mutate(across(c(tib_bfp, tib_dp), ~ .x - iSize_proximal)) %>% 
              rename_with(~str_c("iShape_proximal_", .), .cols = contains("tib")),
            by = "specimen")  %>% 
  left_join(df_tib %>% 
              select(specimen, contains("tib")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_distal = sum(c(tib_bd, tib_bfd, tib_dd))/3) %>% 
              ungroup() %>% 
              mutate(across(c(tib_bd, tib_bfd, tib_dd), ~ .x - iSize_distal)) %>% 
              rename_with(~str_c("iShape_distal_", .), .cols = contains("tib")),
            by = "specimen") %>% 
  left_join(df_tib %>% 
              select(specimen, contains("tib")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_shaft = sum(c(tib_sd, tib_sdd))/2) %>% 
              ungroup() %>% 
              mutate(across(c(tib_sd, tib_sdd), ~ .x - iSize_shaft)) %>% 
              rename_with(~str_c("iShape_shaft_", .), .cols = contains("tib")),
            by = "specimen") %>% 
  left_join(df_tib %>% 
              select(specimen, contains("tib")) %>% 
              mutate_if(is.numeric, log) %>% 
              rowwise() %>% 
              mutate(iSize_complete = sum(c(tib_bfd, tib_bfp, tib_dd, tib_dp, tib_ll, tib_sd))/6) %>% 
              ungroup() %>% 
              mutate(across(c(tib_bfd, tib_bfp, tib_dd, tib_dp, tib_ll, tib_sd), ~ .x - iSize_complete)) %>% 
              rename_with(~str_c("iShape_complete_", .), .cols = contains("tib")),
            by = "specimen")  %>%
  pivot_longer(cols = c(contains("tib_"), contains("iSize")),
               names_to = "variable",
               values_to = "value")  %>% 
  mutate(variable = str_replace_all(variable,
                                    "tib_", ""),
         element = "tibia") %>% 
  relocate(element, collection, specimen, subspecies, group, sex, castrated)


database_analysis <- list(femur = df_fem,  
                          humerus = df_hum,  
                          metacarpus = df_metc,
                          metatarsus = df_mett,
                          pelvis = df_pel,
                          radioulna = df_rad,
                          tibia = df_tib)

write_rds(database_analysis,here("output", "data", "database_list_main.rds"))

bind_rows(df_fem,
          df_hum,
          df_metc,
          df_mett,
          df_pel,
          df_rad,
          df_tib) %>% 
  write_rds(here("output", "data", "database_df_main.rds"))


# Tables  ---------------------------------------------------------

df_main <- bind_rows(
          df_fem,
          df_hum,
          df_metc,
          df_mett,
          df_pel,
          df_rad,
          df_tib) 

df_varsel <- read.xlsx(here("data", "variableselection.xlsx")) %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate_if(is.character, tolower) %>% 
  rename("variable" = "measurement",
         "shaft" = "shaft_variable_selection",
         "complete" = "complete_bone_variable_selection")

df_main %>% 
  left_join(df_varsel,
            by = c("element", "variable")) %>% 
  relocate(element, collection, specimen, subspecies, group, sex, castrated) %>% 
  write_rds(here("output", "data", "database_df_main.rds"))

df_main <- df_main %>% 
  left_join(df_varsel,
            by = c("element", "variable")) %>% 
  relocate(element, collection, specimen, subspecies, group, sex, castrated) 

table_fem <- df_main %>% 
  filter(element == "femur") %>% 
  select(element:value) %>% 
  pivot_wider(names_from = "variable",
              values_from = "value") %>% 
  select(element:pl,
         contains("iSize"),
         contains("iShape")) %>% 
  arrange(group)

table_hum <- df_main %>% 
  filter(element == "humerus") %>% 
  select(element:value) %>% 
  pivot_wider(names_from = "variable",
              values_from = "value") %>% 
  select(element:pl,
         contains("iSize"),
         contains("iShape")) %>% 
  arrange(group)

table_mec <- df_main %>% 
  filter(element == "metacarpus") %>% 
  select(element:value) %>% 
  pivot_wider(names_from = "variable",
              values_from = "value") %>% 
  select(element:dfp,
         contains("iSize"),
         contains("iShape")) %>% 
  arrange(group)

table_met <- df_main %>% 
  filter(element == "metatarsus") %>% 
  select(element:value) %>% 
  pivot_wider(names_from = "variable",
              values_from = "value") %>% 
  select(element:dfp,
         contains("iSize"),
         contains("iShape")) %>% 
  arrange(group)

table_pel <- df_main %>% 
  filter(element == "pelvis") %>% 
  select(element:value) %>% 
  pivot_wider(names_from = "variable",
              values_from = "value") %>% 
  select(element:dps,
         contains("iSize"),
         contains("iShape")) %>% 
  arrange(group)

table_rad <- df_main %>% 
  filter(element == "radioulna") %>% 
  select(element:value) %>% 
  pivot_wider(names_from = "variable",
              values_from = "value") %>% 
  select(element:dp, gl, bd, lo, sdo,
         contains("iSize"),
         contains("iShape")) %>% 
  arrange(group)

table_tib <- df_main %>% 
  filter(element == "tibia") %>% 
  select(element:value) %>% 
  pivot_wider(names_from = "variable",
              values_from = "value") %>% 
  select(element:bfd,
         contains("iSize"),
         contains("iShape")) %>% 
  arrange(group)


wb1 <- createWorkbook(creator = "Henri Wallen")
addWorksheet(wb1, "Femur") 
addWorksheet(wb1, "Humerus")
addWorksheet(wb1, "Metacarpus") 
addWorksheet(wb1, "Metatarsus")
addWorksheet(wb1, "Pelvis") 
addWorksheet(wb1, "Radioulna")
addWorksheet(wb1, "Tibia") 


writeData(wb1,
          "Femur",
          table_fem,
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Humerus",
          table_hum %>% 
            replace(is.na(.), 0),
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Metacarpus",
          table_mec,
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Metatarsus",
          table_met,
          startRow = 1,
          startCol = 1)


writeData(wb1,
          "Pelvis",
          table_pel,
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Radioulna",
          table_rad,
          startRow = 1,
          startCol = 1)

writeData(wb1,
          "Tibia",
          table_tib,
          startRow = 1,
          startCol = 1)

saveWorkbook(wb = wb1,
             file = paste0(here("output", "data"), "/",today(), "_complete_database.xlsx"),
             overwrite = T)