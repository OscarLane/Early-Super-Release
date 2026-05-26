# Packages ---------------------------------------------------------------------

library(tidyverse)
library(haven)
library(broom)
library(readabs)
library(lubridate)
library(skimr)
library(ivreg)
library(janitor)
library(readxl)
library(modelsummary)
library(flextable)
library(srvyr)
library(writexl)
library(ggfortify)
library(pandoc)
library(optmatch)
library(plm)
library(openxlsx)
library(fixest)
library(sandwich)
library(lmtest)
library(corrplot)
library(stats)
library(data.table)

gc()

hilda <- readRDS("data/hilda.rds")

# 1 Define full sample (2019) -----------------------------------------------

hilda_2019_clean <- hilda %>% 
  filter(
    year == 2019,  
    esbrd > 0, #all labour market states
    hgage > 0,
    hgsex > 0,
    hhstate > 0,
    savaln2_2018 > 0) %>% 
  mutate(
    savaln2_2018 = as_factor(savaln2_2018),
    wscei = ifelse(wscei < 0, NA, wscei),
    
    # Industry - 1 digit and 2 digit
    jbmi61 = ifelse(jbmi61 < 0, "No industry", jbmi61),
    jbmi62 = ifelse(jbmi62 < 0, "No industry", jbmi62),
    # Occupation - 1 and 2 digit
    jbmo61 = ifelse(jbmo61 < 0, "No occupation", jbmo61),
    jbmo62 = ifelse(jbmo62 < 0, "No occupation", jbmo62),
    
    # Converting to factors 
    home_2018 = as.factor(home_2018),
    skill_level = as.factor(skill_level),
    edhigh1 = as.factor(edhigh1),
    esbrd = as.factor(esbrd),
    children = as.factor(children),
    impatience = as.factor(impatience),
    hgsex = as.factor(hgsex),
    jbmi61 = as.factor(jbmi61),
    jbmo61 = as.factor(jbmo61),
    jbmi62 = as.factor(jbmi62),
    jbmo62 = as.factor(jbmo62),
    hhstate = as.factor(hhstate),
    casual = as.factor(casual),
    partner = as.factor(partner),
    self_employed = as.factor(ifelse(esempst == 2 | esempst == 3, 1, 0))
  )

hilda_2019_clean_altage <- hilda_2019_clean %>% 
  filter(working_age_alt == 1)

hilda_2019_clean <- hilda_2019_clean %>% 
  filter(working_age == 1) # Filtering for working age 

# 2 Model spec -----------------------

## IV main and robustness -----------------------------

iv_formula <- " ~ 
  esbrd +
  hgage + 
  female +
  jbmi61 + 
  jbmo61 +
  self_employed + 
  hhstate +
  children +
  partner +
  casual +
  home_2018 +
  savaln2_2018 +
  private_income +
  net_savings_2018 +
  withdrew_super |
  impatience +
  esbrd +
  hgage + 
  female +
  jbmi61 + 
  jbmo61 +
  self_employed + 
  hhstate +
  children +
  partner +
  casual +
  home_2018 +
  savaln2_2018 +
  private_income +
  net_savings_2018"

iv_formula_robust <- " ~ 
  esbrd +
  hgage + 
  female +
  jbmi61 + 
  jbmo61 +
  self_employed + 
  hhstate +
  children +
  partner +
  casual +
  home_2018 +
  savaln2_2018 +
  private_income +
  net_savings_2018 +
  loc_mean +
  consc_mean +
  withdrew_super |
  impatience +
  esbrd +
  hgage + 
  female +
  jbmi61 + 
  jbmo61 +
  self_employed + 
  hhstate +
  children +
  partner +
  casual +
  home_2018 +
  savaln2_2018 +
  private_income +
  net_savings_2018 +
  loc_mean +
  consc_mean"


iv_formula_employed <- str_c("employed_2020", iv_formula)
iv_formula_employed_robust <- str_c("employed_2020", iv_formula_robust)

iv_formula_hours <- str_c("hours_2020", iv_formula) %>% 
  str_remove_all("esbrd +")
iv_formula_hours_robust <- str_c("hours_2020", iv_formula_robust) %>% 
  str_remove_all("esbrd +")

iv_formula_employed_21 <- str_c("employed_2021", iv_formula)
iv_formula_employed_robust_21 <- str_c("employed_2021", iv_formula_robust)

iv_formula_hours_21 <- str_c("hours_2021", iv_formula) %>% 
  str_remove_all("esbrd +")
iv_formula_hours_robust_21 <- str_c("hours_2021", iv_formula_robust) %>% 
  str_remove_all("esbrd +")

## OLS main and robustness -----------------------------

ols_formula <- " ~ 
  esbrd +
  hgage + 
  female +
  jbmi61 + 
  jbmo61 +
  self_employed + 
  hhstate +
  children +
  partner +
  casual +
  home_2018 +
  savaln2_2018 +
  private_income +
  net_savings_2018 +
  withdrew_super"

ols_formula_robust <- " ~ 
  esbrd +
  hgage + 
  female +
  jbmi61 + 
  jbmo61 +
  self_employed + 
  hhstate +
  children +
  partner +
  casual +
  home_2018 +
  savaln2_2018 +
  private_income +
  net_savings_2018 +
  loc_mean +
  consc_mean +
  withdrew_super"

ols_formula_employed <- str_c("employed_2020", ols_formula)
ols_formula_employed_robust <- str_c("employed_2020", ols_formula_robust)

ols_formula_hours <- str_c("hours_2020", ols_formula) %>% 
  str_remove_all("esbrd +")
ols_formula_hours_robust <- str_c("hours_2020", ols_formula_robust) %>% 
  str_remove_all("esbrd +")

ols_formula_employed_21 <- str_c("employed_2021", ols_formula)
ols_formula_employed_robust_21 <- str_c("employed_2021", ols_formula_robust)

ols_formula_hours_21 <- str_c("hours_2021", ols_formula) %>% 
  str_remove_all("esbrd +")
ols_formula_hours_robust_21 <- str_c("hours_2021", ols_formula_robust) %>% 
  str_remove_all("esbrd +")

# 7 Alternative working age (25-54) models --------

## IV extensive margin ----------

iv_baseline20_altage <- hilda_2019_clean_altage %>% 
  ivreg(iv_formula_employed, data = ., weights = hhwtsc)

iv_robustness20_altage <- hilda_2019_clean_altage %>% 
  ivreg(iv_formula_employed_robust, data = ., weights = hhwtsc)

summary(iv_baseline20_altage)
summary(iv_robustness20_altage)

## OLS extensive margin ----------

ols_baseline20_altage <- hilda_2019_clean_altage %>% 
  lm(ols_formula_employed, data = ., weights = hhwtsc)

ols_robustness20_altage <- hilda_2019_clean_altage %>% 
  lm(ols_formula_employed_robust, data = ., weights = hhwtsc)

summary(ols_baseline20_altage)
summary(ols_robustness20_altage)

modelsummary(list("2020 IV" = iv_baseline20_altage, 
                  "2020 IV (robust)" = iv_robustness20_altage,
                  "2020 OLS" = ols_baseline20_altage, 
                  "2020 OLS (robust)" = ols_robustness20_altage),
             stars = TRUE, coef_map = "withdrew_super",
             gof_map = "all",
             metrics = "all",
             title = "IV & OLS - extensive margin results",
             output = "output_new/models_new/extensive_margin_models_altage.docx"
)

# 8 Alternative impatience definition specs --------

iv_formula2 <- " ~ 
  esbrd +
  hgage + 
  female +
  jbmi61 + 
  jbmo61 +
  self_employed + 
  hhstate +
  children +
  partner +
  casual +
  home_2018 +
  savaln2_2018 +
  private_income +
  net_savings_2018 +
  withdrew_super |
  impatience_2 +
  esbrd +
  hgage + 
  female +
  jbmi61 + 
  jbmo61 +
  self_employed + 
  hhstate +
  children +
  partner +
  casual +
  home_2018 +
  savaln2_2018 +
  private_income +
  net_savings_2018"

iv_formula_robust2 <- " ~ 
  esbrd +
  hgage + 
  female +
  jbmi61 + 
  jbmo61 +
  self_employed + 
  hhstate +
  children +
  partner +
  casual +
  home_2018 +
  savaln2_2018 +
  private_income +
  net_savings_2018 +
  loc_mean +
  consc_mean +
  withdrew_super |
  impatience_2 +
  esbrd +
  hgage + 
  female +
  jbmi61 + 
  jbmo61 +
  self_employed + 
  hhstate +
  children +
  partner +
  casual +
  home_2018 +
  savaln2_2018 +
  private_income +
  net_savings_2018 +
  loc_mean +
  consc_mean"


iv_formula_employed2 <- str_c("employed_2020", iv_formula2)
iv_formula_employed_robust2 <- str_c("employed_2020", iv_formula_robust2)

# 9 Alternative impatience models --------

## Summary statistics for IV -------------------------

table(hilda_2019_clean$withdrew)

table(hilda_2019_clean$impatience_2)

table(hilda_2019_clean$withdrew, hilda_2019_clean$impatience_2) %>%
  prop.table(., margin = 2) * 100

## Extensive margin -----

### 2020 ------

iv_baseline20_2 <- hilda_2019_clean %>% 
  ivreg(iv_formula_employed2, data = ., weights = hhwtsc)

iv_robustness20_2 <- hilda_2019_clean %>% 
  ivreg(iv_formula_employed_robust2, data = ., weights = hhwtsc)

summary(iv_baseline20_2)
summary(iv_robustness20_2)

## Extensive margin summaries -----

modelsummary(list("2020 IV-alt" = iv_baseline20_2, 
                  "2020 IV-alt (robust)" = iv_robustness20_2),
             stars = TRUE, coef_map = "withdrew_super",
             gof_map = "all",
             metrics = "all",
             title = "IV & OLS - extensive margin results",
             output = "output_new/models_new/extensive_margin_models_alt_instrument.docx"
)


