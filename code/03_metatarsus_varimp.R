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
# Metatarsus --------------------------------------------------------------

df_mett <- list_database[[4]] %>% 
  pivot_wider(names_from = "variable",
              values_from = "value",
              names_prefix = "met_") %>%  
  drop_na() %>%
  mutate_if(is.numeric, scale_this) %>% 
  dplyr::select(specimen, subspecies, sex, met_gl:met_dfp) %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus",
                           "Rangifer tarandus tarandus",
                           "Rangifer tarandus tarandus wild")) %>% 
  mutate(subspecies = droplevels(subspecies)) %>% 
  as.data.frame()

df_mett_f <- list_database[[4]] %>% 
  pivot_wider(names_from = "variable",
              values_from = "value",
              names_prefix = "met_") %>%  
  drop_na() %>%
  mutate_if(is.numeric, scale_this) %>% 
  dplyr::select(specimen, subspecies, sex, met_gl:met_dfp) %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", 
                           "Rangifer tarandus tarandus", 
                           "Rangifer tarandus tarandus wild")) %>%   
  mutate(subspecies = droplevels(subspecies)) %>% 
  filter(sex == "f") %>% 
  as.data.frame()

df_mett_m <- list_database[[4]] %>% 
  pivot_wider(names_from = "variable",
              values_from = "value",
              names_prefix = "met_") %>% 
  drop_na() %>%
  mutate_if(is.numeric, scale_this) %>% 
  dplyr::select(specimen, subspecies, sex, met_gl:met_dfp)  %>% 
  filter(sex == "m") %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", 
                           "Rangifer tarandus tarandus")) %>% 
  mutate(subspecies = droplevels(subspecies))

### Linear Discriminant Analysis (LDA) 

## Males

myfolds <- createMultiFolds(df_mett_m$subspecies, k=5, times=10)
control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")

m_mett_m <- train(subspecies~., 
                 data= df_mett_m %>% 
                   dplyr::select(-specimen, -sex) %>% 
                   as.data.frame(),
                 method="lda",
                 metric="Accuracy",
                 trControl=control)

confusionMatrix(
  data = predict(m_mett_m),
  reference = df_mett_m$subspecies
)

varimp_mett_m <- varImp(m_mett_m)
p_mett_m <- varimp_mett_m$importance %>% 
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
       subtitle = "Variable importance: Metatarsus-Male")
  
## Females

myfolds <- createMultiFolds(df_mett_f$subspecies, k=5, times=10)
control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")

m_mett_f <- train(subspecies~., 
                  data= df_mett_f %>% 
                    dplyr::select(-specimen, -sex) %>% 
                    as.data.frame(),
                  method="lda",
                  metric="Accuracy",
                  trControl=control)

confusionMatrix(
  data = predict(m_mett_f),
  reference = df_mett_f$subspecies
)

varimp_mett_f <- varImp(m_mett_f)

p_mett_f <- varimp_mett_f$importance %>% 
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
       subtitle = "Variable importance: Metatarsus-Female")

ggsave(
  p_mett_m,
  units = "px",
  filename = here("output", "varimp_mett_m.png"),
  width = 3000,
  height = 1500,
  dpi = 300
)


ggsave(
  p_mett_f,
  units = "px",
  filename = here("output", "varimp_mett_f.png"),
  width = 3000,
  height = 1500,
  dpi = 300
)