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
# Metacarpus --------------------------------------------------------------

df_mec <- list_database[[3]] %>% 
  pivot_wider(names_from = "variable",
              values_from = "value",
              names_prefix = "mec_") %>%  
  drop_na() %>%
  mutate_if(is.numeric, scale_this) %>% 
  dplyr::select(specimen, subspecies, sex, mec_gl:mec_dfp) %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus",
                           "Rangifer tarandus tarandus",
                           "Rangifer tarandus tarandus wild")) %>% 
  mutate(subspecies = droplevels(subspecies)) %>% 
  as.data.frame()

df_mec_f <- list_database[[3]] %>% 
  pivot_wider(names_from = "variable",
              values_from = "value",
              names_prefix = "mec_") %>%  
  drop_na() %>%
  mutate_if(is.numeric, scale_this) %>% 
  dplyr::select(specimen, subspecies, sex, mec_gl:mec_dfp) %>% 
  filter(sex == "f") %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", 
                           "Rangifer tarandus tarandus", 
                           "Rangifer tarandus tarandus wild")) %>%   
  mutate(subspecies = droplevels(subspecies)) %>% 
  as.data.frame()

df_mec_m <- list_database[[3]] %>% 
  pivot_wider(names_from = "variable",
              values_from = "value",
              names_prefix = "mec_") %>%  
  drop_na() %>%
  mutate_if(is.numeric, scale_this) %>% 
  dplyr::select(specimen, subspecies, sex, mec_gl:mec_dfp)  %>% 
  filter(sex == "m") %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", 
                           "Rangifer tarandus tarandus")) %>% 
  mutate(subspecies = droplevels(subspecies))

### Linear Discriminant Analysis (LDA) 

## Males

myfolds <- createMultiFolds(df_mec_m$subspecies, k=5, times=10)
control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")

m_mec_m <- train(subspecies~., 
                  data= df_mec_m %>% 
                    dplyr::select(-specimen, -sex) %>% 
                    as.data.frame(),
                  method="lda",
                  metric="Accuracy",
                  trControl=control)

confusionMatrix(
  data = predict(m_mec_m),
  reference = df_mec_m$subspecies
)

varimp_mec_m <- varImp(m_mec_m)
p_mec_m <- varimp_mec_m$importance %>% 
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
       subtitle = "Variable importance: Metacarpus-Male")

## Females

myfolds <- createMultiFolds(df_mec_f$subspecies, k=5, times=10)
control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")

m_mec_f <- train(subspecies~., 
                  data= df_mec_f %>% 
                    dplyr::select(-specimen, -sex) %>% 
                    as.data.frame(),
                  method="lda",
                  metric="Accuracy",
                  trControl=control)

confusionMatrix(
  data = predict(m_mec_f),
  reference = df_mec_f$subspecies
)

varimp_mec_f <- varImp(m_mec_f)

p_mec_f <- varimp_mec_f$importance %>% 
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
       subtitle = "Variable importance: Metacarpus-Female")

ggsave(
  p_mec_m,
  units = "px",
  filename = here("output", "varimp_mec_m.png"),
  width = 3000,
  height = 1500,
  dpi = 300
)


ggsave(
  p_mec_f,
  units = "px",
  filename = here("output", "varimp_mec_f.png"),
  width = 3000,
  height = 1500,
  dpi = 300
)