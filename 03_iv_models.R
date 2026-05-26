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

hilda_2019_clean <- hilda_2019_clean %>% 
  filter(working_age == 1) # Filtering for working age 

## Initial summary statistics ----

hilda_2019_clean %>% 
  tabyl(year, withdrew_2020) %>% 
  mutate(share_withdrew_2020 = `1` / (`1` + `0`))

hilda_2019_clean %>% 
  tabyl(year, withdrew_2021) %>% 
  mutate(share_withdrew_2021 = `1` / (`1` + `0`))

# How many observations in sample?

hilda_2019_clean %>% 
  summarise(
    count = n_distinct(waveid)
  )

# 11,028 observations in 2019

# How much attrition in 2020 and 2021? 

waveid_2019 <- hilda_2019_clean %>% 
  select(waveid)

waveid_2020 <- hilda %>% 
  filter(year == 2020) %>% 
  select(waveid)

waveid_2021 <- hilda %>% 
  filter(year == 2021) %>% 
  select(waveid)

retained_2020 <- sum(waveid_2019$waveid %in% waveid_2020$waveid)
retained_2021 <- sum(waveid_2019$waveid %in% waveid_2021$waveid)
n_2019 <- nrow(waveid_2019)

# Retention and attrition rates
tibble::tibble(
  year = c(2020, 2021),
  retained = c(retained_2020, retained_2021),
  base_2019 = n_2019,
  dropped = n_2019 - retained,
  retention_rate = retained / n_2019,
  attrition_rate = 1 - retention_rate
)


## Construct other frames ----

hilda_2019_clean_employed <- hilda_2019_clean %>% 
  filter(employed_2019 == 1) # Filtering for employed in 2019

hilda_2019_male <- hilda_2019_clean %>% 
  filter(hgsex == 1) # filtering to 1 = male

hilda_2019_female <- hilda_2019_clean %>% 
  filter(hgsex == 2) # filtering to 2 = female

## Summary statistics for IV -------------------------

table(hilda_2019_clean$withdrew)

table(hilda_2019_clean$impatience)

table(hilda_2019_clean$withdrew, hilda_2019_clean$impatience) %>%
  prop.table(., margin = 2) * 100

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

# 3 Run models ----------------------

## First-stage model -------

fs_formula <- " ~ 
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
  impatience"

fs_formula_robust <- " ~ 
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
  impatience"

fstage_formula_baseline <- str_c("withdrew_super", fs_formula)
fstage_formula_robust <- str_c("withdrew_super", fs_formula_robust)

fs_baseline_mod <- hilda_2019_clean %>% 
  lm(fstage_formula_baseline, data = ., weights = hhwtsc)

fs_robustness_mod <- hilda_2019_clean %>% 
  lm(fstage_formula_robust, data = ., weights = hhwtsc)

summary(fs_baseline_mod)
summary(fs_robustness_mod)

modelsummary(list("FS" = fs_baseline_mod, 
                  "FS (robust)" = fs_robustness_mod),
             stars = TRUE, coef_map = "impatience1",
             gof_map = "all",
             metrics = "all",
             title = "First stage model",
             output = "output_new/models_new/first_stage_model.docx"
)

## IV extensive margin ----------

### 2020 ------

iv_baseline20 <- hilda_2019_clean %>% 
  ivreg(iv_formula_employed, data = ., weights = hhwtsc)

iv_robustness20 <- hilda_2019_clean %>% 
  ivreg(iv_formula_employed_robust, data = ., weights = hhwtsc)

summary(iv_baseline20)
summary(iv_robustness20)

# Robustness controls reduce performance on Wu-Hausman - maybe they interfere 
# with the instrument?

### 2021 ----------------
# No effect detected in 2021

iv_baseline21 <- hilda_2019_clean %>% 
  ivreg(iv_formula_employed_21, data = ., weights = hhwtsc)

iv_robustness21 <- hilda_2019_clean %>% 
  ivreg(iv_formula_employed_robust_21, data = ., weights = hhwtsc)

summary(iv_baseline21)
summary(iv_robustness21)

# An extensive margin result only detected in 2020 in baseline model

## OLS extensive margin ----------

### 2020 ------

ols_baseline20 <- hilda_2019_clean %>% 
  lm(ols_formula_employed, data = ., weights = hhwtsc)

ols_robustness20 <- hilda_2019_clean %>% 
  lm(ols_formula_employed_robust, data = ., weights = hhwtsc)

summary(ols_baseline20)
summary(ols_robustness20)

### 2021 ----------------
# No effect detected in 2021

ols_baseline21 <- hilda_2019_clean %>% 
  lm(ols_formula_employed_21, data = ., weights = hhwtsc)

ols_robustness21 <- hilda_2019_clean %>% 
  lm(ols_formula_employed_robust_21, data = ., weights = hhwtsc)

summary(ols_baseline21)
summary(ols_robustness21)

## Extensive margin summaries -----

modelsummary(list("2020 IV" = iv_baseline20, 
                  "2020 IV (robust)" = iv_robustness20,
                  "2020 OLS" = ols_baseline20, 
                  "2020 OLS (robust)" = ols_robustness20,
                  "2021 IV" = iv_baseline21,
                  "2021 IV (robust)" = iv_robustness21,
                  "2021 OLS" = ols_baseline21,
                  "2021 OLS (robust)" = ols_robustness21),
             stars = TRUE, coef_map = "withdrew_super",
             gof_map = "all",
             metrics = "all",
             title = "IV & OLS - extensive margin results",
             output = "output_new/models_new/extensive_margin_models.docx"
)

# Small, significant OLS result in most cases

## IV intensive margin, full sample -----

### 2020 ------

iv_hrs_baseline20 <- hilda_2019_clean %>% 
  ivreg(iv_formula_hours, data = ., weights = hhwtsc)

iv_hrs_robustness20 <- hilda_2019_clean %>% 
  ivreg(iv_formula_hours_robust, data = ., weights = hhwtsc)

summary(iv_hrs_baseline20)
summary(iv_hrs_robustness20)

### 2021 ----------------
# No effect detected in 2021

iv_hrs_baseline21 <- hilda_2019_clean %>% 
  ivreg(iv_formula_hours_21, data = ., weights = hhwtsc)

iv_hrs_robustness21 <- hilda_2019_clean %>% 
  ivreg(iv_formula_hours_robust_21, data = ., weights = hhwtsc)

summary(iv_hrs_baseline21)
summary(iv_hrs_robustness21)

# No intensive margin result found

## OLS intensive margin, full sample -----

### 2020 ------

ols_hrs_baseline20 <- hilda_2019_clean %>% 
  lm(ols_formula_hours, data = ., weights = hhwtsc)

ols_hrs_robustness20 <- hilda_2019_clean %>% 
  lm(ols_formula_hours_robust, data = ., weights = hhwtsc)

summary(ols_hrs_baseline20)
summary(ols_hrs_robustness20)

### 2021 -----

ols_hrs_baseline21 <- hilda_2019_clean %>% 
  lm(ols_formula_hours_21, data = ., weights = hhwtsc)

ols_hrs_robustness21 <- hilda_2019_clean %>% 
  lm(ols_formula_hours_robust_21, data = ., weights = hhwtsc)

summary(ols_hrs_baseline21)
summary(ols_hrs_robustness21)

## Intensive margin summaries ------

modelsummary(list("2020 IV" = iv_hrs_baseline20, 
                  "2020 IV (robust)" = iv_hrs_robustness20,
                  "2021 IV" = iv_hrs_baseline21,
                  "2021 IV (robust)" = iv_hrs_robustness21,
                  "2020 OLS" = ols_hrs_baseline20, 
                  "2020 OLS (robust)" = ols_hrs_robustness20,
                  "2021 OLS" = ols_hrs_baseline21,
                  "2021 OLS (robust)" = ols_hrs_robustness21),
             stars = TRUE, coef_map = "withdrew_super",
             gof_map = "all",
             metrics = "all",
             title = "IV & OLS - intensive margin, full sample results",
             output = "output_new/models_new/intensive_margin_models.docx"
             )
# A small intensive margin result is detected

# 4 Hours, conditional on 2019 employment -----

## 2020 IV ----

iv_cond_hrs_baseline20 <- hilda_2019_clean_employed %>% 
  ivreg(iv_formula_hours, data = ., weights = hhwtsc)

iv_cond_hrs_robustness20 <- hilda_2019_clean_employed %>% 
  ivreg(iv_formula_hours_robust, data = ., weights = hhwtsc)

summary(iv_cond_hrs_baseline20)
summary(iv_cond_hrs_robustness20)

## 2020 OLS ----

ols_cond_hrs_baseline20 <- hilda_2019_clean_employed %>% 
  lm(ols_formula_hours, data = ., weights = hhwtsc)

ols_cond_hrs_robustness20 <- hilda_2019_clean_employed %>% 
  lm(ols_formula_hours_robust, data = ., weights = hhwtsc)

summary(ols_cond_hrs_baseline20)
summary(ols_cond_hrs_robustness20)

## 2021 IV ----
# No effect detected in 2021

iv_cond_hrs_baseline21 <- hilda_2019_clean_employed %>% 
  ivreg(iv_formula_hours_21, data = ., weights = hhwtsc)

iv_cond_hrs_robustness21 <- hilda_2019_clean_employed %>% 
  ivreg(iv_formula_hours_robust_21, data = ., weights = hhwtsc)

summary(iv_cond_hrs_baseline21)
summary(iv_cond_hrs_robustness21)

## Conditional employment summaries -----

modelsummary(list("2020 IV" = iv_cond_hrs_baseline20, 
                  "2020 IV (robust)" = iv_cond_hrs_robustness20,
                  "2020 OLS" = ols_cond_hrs_baseline20,
                  "2020 OLS (robust)" = ols_cond_hrs_robustness20,
                  "2021 IV" = iv_cond_hrs_baseline21,
                  "2021 IV (robust)" = iv_cond_hrs_robustness21),
             stars = TRUE, coef_map = "withdrew_super",
             gof_map = "all",
             metrics = "all",
             title = "IV - intensive margin, conditional on 2019 employment",
             output = "output_new/models_new/intensive_margin_conditonal_emp.docx"
)
# No effect conditional on 2019 employment

# 5 Full withdrawal & partial withdrawal -----------------

## Employment, full -----

iv_baseline_full20 <- hilda_2019_clean %>% 
  filter(super_amount_2020 == "Full withdrawal" | withdrew_super == 0) %>% 
  ivreg(iv_formula_employed, data = ., weights = hhwtsc)

iv_robust_full20 <- hilda_2019_clean %>% 
  filter(super_amount_2020 == "Full withdrawal" | withdrew_super == 0) %>% 
  ivreg(iv_formula_employed_robust, data = ., weights = hhwtsc)

summary(iv_baseline_full20)
summary(iv_robust_full20)

iv_baseline_full21 <- hilda_2019_clean %>% 
  filter(super_amount_2020 == "Full withdrawal" | withdrew_super == 0) %>% 
  ivreg(iv_formula_employed_21, data = ., weights = hhwtsc)

iv_robust_full21 <- hilda_2019_clean %>% 
  filter(super_amount_2020 == "Full withdrawal" | withdrew_super == 0) %>% 
  ivreg(iv_formula_employed_robust_21, data = ., weights = hhwtsc)

summary(iv_baseline_full21)
summary(iv_robust_full21)

# strong 2020 result

## Employment, partial -----

# NOTE: This only works on robust model
# Increasing variables likely removed hidden collinearity
# This is probably due to low number of partial withdrawers in sample making IV difficult

table(hilda_2019_clean$super_amount_2020)

# iv_baseline_partial20 <- hilda_2019_clean %>%
#   filter(super_amount_2020 == "Partial withdrawal" | withdrew_super == 0) %>%
#   ivreg(iv_formula_employed, data = ., weights = hhwtsc)

iv_robust_partial20 <- hilda_2019_clean %>% 
  filter(super_amount_2020 == "Partial withdrawal" | withdrew_super == 0) %>% 
  ivreg(iv_formula_employed_robust, data = ., weights = hhwtsc)

# summary(iv_baseline_partial20)
summary(iv_robust_partial20)

# iv_baseline_partial21 <- hilda_2019_clean %>% 
#   filter(super_amount_2020 == "Partial withdrawal" | withdrew_super == 0) %>% 
#   ivreg(iv_formula_employed_21, data = ., weights = hhwtsc)

iv_robust_partial21 <- hilda_2019_clean %>% 
  filter(super_amount_2020 == "Partial withdrawal" | withdrew_super == 0) %>% 
  ivreg(iv_formula_employed_robust_21, data = ., weights = hhwtsc)

# summary(iv_baseline_partial21)
summary(iv_robust_partial21)

modelsummary(list("2020 IV (robust)" = iv_robust_partial20,
                  "2021 IV (robust)" = iv_robust_partial21),
             stars = TRUE, coef_map = "withdrew_super",
             gof_map = "all",
             metrics = "all",
             title = "IV - extensive margin, conditional on partial withdrawal")

## OLS extensive margin full withdrawer ----------

ols_baseline_full20 <- hilda_2019_clean %>% 
  filter(super_amount_2020 == "Full withdrawal" | withdrew_super == 0) %>% 
  lm(ols_formula_employed, data = ., weights = hhwtsc)

ols_robust_full20 <- hilda_2019_clean %>% 
  filter(super_amount_2020 == "Full withdrawal" | withdrew_super == 0) %>% 
  lm(ols_formula_employed_robust, data = ., weights = hhwtsc)

summary(ols_baseline_full20)
summary(ols_robust_full20)

## Withdrawal summaries -----

modelsummary(list("2020 IV, full" = iv_baseline_full20, 
                  "2020 IV (robust), full" = iv_robust_full20,
                  "2021 IV, full" = iv_baseline_full21,
                  "2021 IV (robust), full" = iv_robust_full21,
                  "2020 OLS, full" = ols_baseline_full20,
                  "2020 OLS (robust), full" = ols_robust_full20),
             stars = TRUE, coef_map = "withdrew_super",
             gof_map = "all",
             metrics = "all",
             title = "IV - extensive margin, conditional on withdrawal type",
             output = "output_new/models_new/extensive_margin_full_withdrawal.docx"
)



modelsummary(list("2020 IV (robust), partial" = iv_robust_partial20,
                  "2021 IV (robust), partial" = iv_robust_partial21),
             stars = TRUE, coef_map = "withdrew_super",
             gof_map = "all",
             metrics = "all",
             title = "OLS - extensive margin, conditional on withdrawal type",
             output = "output_new/models_new/ols_extensive_margin_partial_withdrawal.docx"
)



# 6 Male/female split ---------------------------

table(hilda_2019_clean$hgsex)

### Extensive margin, male -----

iv_baseline_male20 <- hilda_2019_male %>% 
  ivreg(iv_formula_employed, data = ., weights = hhwtsc)

iv_robust_male20 <- hilda_2019_male %>% 
  ivreg(iv_formula_employed_robust, data = ., weights = hhwtsc)

summary(iv_baseline_male20)
summary(iv_robust_male20)

iv_baseline_male21 <- hilda_2019_male %>% 
  ivreg(iv_formula_employed_21, data = ., weights = hhwtsc)

iv_robust_male21 <- hilda_2019_male %>% 
  ivreg(iv_formula_employed_robust_21, data = ., weights = hhwtsc)

summary(iv_baseline_male21)
summary(iv_robust_male21)


# No strong effect detected in male subsample

### Extensive margin, female -----

iv_baseline_female20 <- hilda_2019_female %>% 
  ivreg(iv_formula_employed, data = ., weights = hhwtsc)

iv_robust_female20 <- hilda_2019_female %>% 
  ivreg(iv_formula_employed_robust, data = ., weights = hhwtsc)

summary(iv_baseline_female20)
summary(iv_robust_female20)

iv_baseline_female21 <- hilda_2019_female %>% 
  ivreg(iv_formula_employed_21, data = ., weights = hhwtsc)

iv_robust_female21 <- hilda_2019_female %>% 
  ivreg(iv_formula_employed_robust_21, data = ., weights = hhwtsc)

summary(iv_baseline_female21)
summary(iv_robust_female21)

## Extensive margin summary - male/female -----

modelsummary(list("2020 IV, male" = iv_baseline_male20, 
                  "2020 IV (robust), male" = iv_robust_male20,
                  "2021 IV, male" = iv_baseline_male21,
                  "2021 IV (robust), male" = iv_robust_male21,
                  "2020 IV, female" = iv_baseline_female20, 
                  "2020 IV (robust), female" = iv_robust_female20,
                  "2021 IV, female" = iv_baseline_female21,
                  "2021 IV (robust), female" = iv_robust_female21),
             title = "IV - extensive margin, male/female subsamples",
             stars = TRUE, coef_map = "withdrew_super",
             gof_map = "all",
             metrics = "all",
             output = "output_new/models_new/extensive_margin_male_female.docx"
              )
# Strong effect detected in female subsample



### Intensive margin, male -----

hrs_baseline_male20 <- hilda_2019_male %>% 
  ivreg(iv_formula_hours, data = ., weights = hhwtsc)

hr_robust_male20 <- hilda_2019_male %>% 
  ivreg(iv_formula_hours_robust, data = ., weights = hhwtsc)

summary(hrs_baseline_male20)
summary(hr_robust_male20)

hrs_baseline_male21 <- hilda_2019_male %>% 
  ivreg(iv_formula_hours_21, data = ., weights = hhwtsc)

hrs_robust_male21 <- hilda_2019_male %>% 
  ivreg(iv_formula_hours_robust_21, data = ., weights = hhwtsc)

summary(iv_baseline_male21)
summary(iv_robust_male21)

# No effect detected in male subsample

### Intensive margin, female -----

hrs_baseline_female20 <- hilda_2019_female %>% 
  ivreg(iv_formula_hours, data = ., weights = hhwtsc)

hrs_robust_female20 <- hilda_2019_female %>% 
  ivreg(iv_formula_hours_robust, data = ., weights = hhwtsc)

summary(hrs_baseline_female20)
summary(hrs_robust_female20)

hrs_baseline_female21 <- hilda_2019_female %>% 
  ivreg(iv_formula_hours_21, data = ., weights = hhwtsc)

hrs_robust_female21 <- hilda_2019_female %>% 
  ivreg(iv_formula_hours_robust_21, data = ., weights = hhwtsc)

summary(hrs_baseline_female21)
summary(hrs_robust_female21)

# No effect detected in female subsample

## Intensive margin summary - male/female -----

modelsummary(list("2020 IV, male" = hrs_baseline_male20, 
                  "2020 IV (robust), male" = hr_robust_male20,
                  "2021 IV, male" = hrs_baseline_male21,
                  "2021 IV (robust), male" = hrs_robust_male21,
                  "2020 IV, female" = hrs_baseline_female20, 
                  "2020 IV (robust), female" = hrs_robust_female20,
                  "2021 IV, female" = hrs_baseline_female21,
                  "2021 IV (robust), female" = hrs_robust_female21),
             title = "IV - intensive margin, male/female subsamples",
             stars = TRUE, coef_map = "withdrew_super",
             gof_map = "all",
             metrics = "all",
             output = "output_new/models_new/intensive_margin_male_female.docx"
)
# Strong effect detected in female subsample