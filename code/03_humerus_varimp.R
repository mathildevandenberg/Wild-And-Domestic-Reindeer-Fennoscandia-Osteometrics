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

df_hum <- list_database[[2]] %>% 
  pivot_wider(names_from = "variable",
              values_from = "value",
              names_prefix = "hum_") %>%  
  drop_na() %>%
  mutate_if(is.numeric, scale_this) %>% 
  dplyr::select(specimen, subspecies, sex, hum_gl:hum_pl) %>% 
  filter(subspecies != "hybrid") %>% 
  mutate(subspecies = droplevels(subspecies)) 

df_hum_f <- list_database[[2]] %>% 
  pivot_wider(names_from = "variable",
              values_from = "value",
              names_prefix = "hum_") %>%   
  drop_na() %>%
  mutate_if(is.numeric, scale_this) %>% 
  dplyr::select(specimen, subspecies, sex, hum_gl:hum_pl) %>% 
  filter(sex == "f")  %>% 
  filter(subspecies %in% c("Rangifer tarandus fennicus", 
                           "Rangifer tarandus tarandus", 
                           "Rangifer tarandus tarandus wild")) %>%   
  mutate(subspecies = droplevels(subspecies))

df_hum_m <- list_database[[2]] %>% 
  pivot_wider(names_from = "variable",
              values_from = "value",
              names_prefix = "hum_") %>%  
  drop_na() %>%
  mutate_if(is.numeric, scale_this) %>% 
  dplyr::select(specimen, subspecies, sex, hum_gl:hum_pl)  %>% 
  filter(sex == "m") %>%  
  filter(subspecies %in% c("Rangifer tarandus fennicus", 
                           "Rangifer tarandus tarandus")) %>%
  mutate(subspecies = droplevels(subspecies))

### Linear Discriminant Analysis (LDA) for presentation
set.seed(4268)



myfolds <- createMultiFolds(df_hum_m$subspecies, k=5, times=10)
control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")

m_humerus_m <- train(subspecies~., 
              data= df_hum_m %>% 
                dplyr::select(-specimen, -sex) %>% 
                as.data.frame(),
              method="lda",
              metric="Accuracy",
              trControl=control)

myfolds <- createMultiFolds(df_hum_f$subspecies, k=5, times=10)
control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")

m_humerus_f <- train(subspecies~., 
              data=df_hum_f %>% 
                dplyr::select(-specimen, -sex) %>% 
                as.data.frame(),
              method="lda",
              metric="Accuracy",
              trControl=control)

confusionMatrix(
  data = predict(m_humerus_m),
  reference = df_hum_m$subspecies
)

confusionMatrix(
  data = predict(m_humerus_f),
  reference = df_hum_f$subspecies
)

confusionMatrix(
  data = predict(m_humerus_m),
  reference = df_hum_m$subspecies
)


# Write rds ---------------------------------------------------------------

write_rds(m_humerus_f,here("output", "data", "m_humerus_f.rds"))
write_rds(m_humerus_m,here("output", "data", "m_humerus_m.rds"))

# Plots -------------------------------------------------------------------

varimp_hum_m <- varImp(m_humerus_m)
p_hum_m <- varimp_hum_m$importance %>% 
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
       subtitle = "Variable importance Humerus: Male")



varimp_hum_f <- varImp(m_humerus_f)

p_hum_f <- varimp_hum_f$importance %>% 
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
       subtitle = "Variable importance Humerus: Female")

ggsave(
  p_hum_m,
  units = "px",
  filename = here("output", "varimp_hum_m.png"),
  width = 3000,
  height = 1500,
  dpi = 300
)


ggsave(
  p_hum_f,
  units = "px",
  filename = here("output", "varimp_hum_f.png"),
  width = 3000,
  height = 1500,
  dpi = 300
)