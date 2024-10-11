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
# Pelvis --------------------------------------------------------------

df_pel <- list_database[[5]] %>% 
  pivot_wider(names_from = "variable",
              values_from = "value",
              names_prefix = "pel_") %>% 
  drop_na() %>%
  mutate_if(is.numeric, scale_this) %>% 
  dplyr::select(specimen, subspecies, sex, pel_gl:pel_dps) %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus",
                           "Rangifer tarandus tarandus",
                           "Rangifer tarandus tarandus wild")) %>% 
  mutate(subspecies = droplevels(subspecies)) %>% 
  as.data.frame()

df_pel_f <- list_database[[5]] %>% 
  pivot_wider(names_from = "variable",
              values_from = "value",
              names_prefix = "pel_") %>%
  drop_na() %>%
  mutate_if(is.numeric, scale_this) %>% 
  dplyr::select(specimen, subspecies, sex, pel_gl:pel_dps) %>% 
  filter(sex == "f") %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", 
                           "Rangifer tarandus tarandus", 
                           "Rangifer tarandus tarandus wild")) %>%   
  mutate(subspecies = droplevels(subspecies)) %>% 
  as.data.frame()



### Linear Discriminant Analysis (LDA) 


## Females

myfolds <- createMultiFolds(df_pel_f$subspecies, k=5, times=10)
control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")

m_pel_f <- train(subspecies~., 
                 data= df_pel_f %>% 
                   dplyr::select(-specimen, -sex) %>% 
                   as.data.frame(),
                 method="lda",
                 metric="Accuracy",
                 trControl=control)

confusionMatrix(
  data = predict(m_pel_f),
  reference = df_pel_f$subspecies
)

varimp_pel_f <- varImp(m_pel_f)

p_pel_f <- varimp_pel_f$importance %>% 
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
       subtitle = "Variable importance Pelvis: Female")

ggsave(
  p_pel_f,
  units = "px",
  filename = here("output", "varimp_pel_f.png"),
  width = 3000,
  height = 1500,
  dpi = 300
)