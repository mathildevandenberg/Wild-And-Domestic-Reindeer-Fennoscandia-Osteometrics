packages <- c("tidyverse", "here", "gghalves", "ggokabeito", "openxlsx", "lubridate", "ggbeeswarm", "ggthemes", "patchwork", "projpred", "caret", "leaps", "glmnet", "klaR")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)
rm(packages)

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

set.seed(4863)

# Data --------------------------------------------------------------------

list_database <- read_rds(here("output", "data", "database_list_main.rds"))
nimet <- list_database %>% 
  names()

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
  "unknown"
)

# Radioulna ---------------------------------------------------------------


df_rad <- list_database[[6]] %>% 
  drop_na() %>%
  mutate_if(is.numeric, scale_this) %>% 
  dplyr::select(specimen, subspecies, sex, raduln_gl:uln_sdo) %>% 
  filter(subspecies != "hybrid") %>% 
  mutate(subspecies = droplevels(subspecies)) 

df_rad_f <- list_database[[6]] %>% 
  drop_na() %>%
  mutate_if(is.numeric, scale_this) %>% 
  dplyr::select(specimen, subspecies, sex, raduln_gl:uln_sdo) %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus", "Rangifer tarandus tarandus wild")) %>%   mutate(subspecies = droplevels(subspecies)) %>% 
  filter(sex == "f")

df_rad_m <- list_database[[6]] %>% 
  drop_na() %>%
  mutate_if(is.numeric, scale_this) %>% 
  dplyr::select(specimen, subspecies, sex, raduln_gl:uln_sdo) %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", "Rangifer tarandus tarandus")) %>% 
  mutate(subspecies = droplevels(subspecies)) %>% 
  filter(sex == "m")


## Males

myfolds <- createMultiFolds(df_rad_m$subspecies, k=5, times=10)
control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")

m_rad_m <- train(subspecies~., 
                 data= df_rad_m %>% 
                   dplyr::select(-specimen, -sex) %>% 
                   as.data.frame(),
                 method="lda",
                 metric="Accuracy",
                 trControl=control)

confusionMatrix(
  data = predict(m_rad_m),
  reference = df_rad_m$subspecies
)

## Females

myfolds <- createMultiFolds(df_rad_f$subspecies, k=5, times=10)
control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")

m_rad_f <- train(subspecies~., 
                 data= df_rad_f %>% 
                   dplyr::select(-specimen, -sex) %>% 
                   as.data.frame(),
                 method="lda",
                 metric="Accuracy",
                 trControl=control)

confusionMatrix(
  data = predict(m_rad_f),
  reference = df_rad_f$subspecies
)

## varimp

cf_hum_m <- confusionMatrix(
  data = predict(m_humerus_m),
  reference = df_hum_m$subspecies
)

varImp(m_humerus_m) %>% 
  plot()

confusionMatrix(
  data = predict(m_humerus_f),
  reference = df_hum_f$subspecies
)

varImp(m_humerus_f) %>% 
  plot()


confusionMatrix(
  data = predict(m_rad_m),
  reference = df_rad_m$subspecies
)

varImp(m_rad_m) %>% 
  plot()

confusionMatrix(
  data = predict(m_rad_f),
  reference = df_rad_f$subspecies
)

varImp(m_rad_f) %>% 
  plot()



# Write rds ---------------------------------------------------------------

write_rds(m_rad_f,here("output", "data", "m_rad_f.rds"))
write_rds(m_rad_m,here("output", "data", "m_rad_m.rds"))


# Plots -------------------------------------------------------------------

varimp_rad_m <- varImp(m_rad_m)
p_rad_m <- varimp_rad_m$importance %>% 
  as_tibble(rownames = NA) %>% 
  mutate(variable = factor(rownames(.))) %>% 
  relocate(variable) %>% 
  arrange(desc(Rangifer.tarandus.fennicus), desc(Rangifer.tarandus.tarandus)) %>% 
  mutate(variable = fct_reorder(variable, Rangifer.tarandus.tarandus)) %>% 
  ggplot(aes(x = variable, y = Rangifer.tarandus.fennicus)) +
  geom_point() + 
  geom_col(width = 0.05) +
  coord_flip() +
  theme_clean() +
  labs(y = "Variable importance",
       subtitle = "Variable importance Radioulna: Male")

varimp_rad_f <- varImp(m_rad_f)

p_rad_f <- varimp_rad_f$importance %>% 
  as_tibble(rownames = NA) %>% 
  mutate(variable = factor(rownames(.))) %>% 
  relocate(variable) %>% 
  arrange(desc(Rangifer.tarandus.fennicus), desc(Rangifer.tarandus.tarandus)) %>% 
  mutate(variable = fct_reorder(variable, Rangifer.tarandus.tarandus)) %>% 
  pivot_longer(cols = Rangifer.tarandus.fennicus:Rangifer.tarandus.tarandus.wild,
               names_to = "class",
               values_to = "value") %>% 
  ggplot(aes(x = variable, y = value)) +
  facet_wrap(~class) +
  geom_point() + 
  geom_col(width = 0.05) +
  coord_flip() +
  theme_clean() +
  labs(y = "Variable importance",
       subtitle = "Variable importance Radioulna: Female")

ggsave(
  p_rad_m,
  units = "px",
  filename = here("output", "varimp_rad_m.png"),
  width = 3000,
  height = 1500,
  dpi = 300
)


ggsave(
  p_rad_f,
  units = "px",
  filename = here("output", "varimp_rad_f.png"),
  width = 3000,
  height = 1500,
  dpi = 300
)