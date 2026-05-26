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

# Descriptive stats ------------------------------------------------------------

hilda <- readRDS("data/hilda.rds")

## Count working age --------------------------------------------

## Standard definition - fairly stable, little attrition

hilda %>%
  group_by(year) %>%
  summarise(num_working_age = sum(working_age == 1, na.rm = TRUE))

## Non-standard definition - fairly stable, little attrition

hilda %>%
  group_by(year) %>%
  summarise(num_working_age = sum(working_age_alt == 1, na.rm = TRUE))

## Stability of the patience instrument ---------------------------

hilda_impat <- hilda %>% 
  mutate(impat = case_when( 
    fisavep == 1 ~ 1,  
    fisavep %in% c(2, 3, 4, 5, 6) ~ 0,  
    fisavep < 0 ~ NA_real_,
    TRUE ~ NA_real_  # This handles any other values, including NAs
  ))

hilda_impat <- hilda_impat %>%
  group_by(waveid) %>%
  mutate(
    impat_2018 = ifelse(any(year == 2018), impat[year == 2018], NA),
    impat_2016 = ifelse(any(year == 2016), impat[year == 2016], NA)) %>%
  ungroup()

h1618 <- hilda_impat %>%
  filter(year %in% c(2016, 2018)) %>%
  select(waveid, impat_2016, impat_2018)

h1618 <- h1618 %>%
  group_by(waveid) %>%
  distinct()

clean_h1618 <- h1618 %>%
  filter(!is.na(impat_2016) & !is.na(impat_2018)) %>%
  mutate(same_value = impat_2016 == impat_2018)

tab_impat <- table(clean_h1618$same_value)
prop.table(tab_impat) * 100

## Working age labour market metrics --------------------------------

hilda %>% 
  filter(hgage >= 15 & hgage <= 64) %>% 
  filter(oifcvs == 1 | oifcvs == 2) %>% 
  group_by(year, oifcvs) %>% 
  # Sum respondent-level weights
  summarise(n = sum(hhwtrp))

## Number of working age people who withdrew super --------------------------

hilda %>% 
  filter(hgage >= 15 & hgage <= 64) %>% 
  filter(year == 2019) %>% 
  summarise(emp_percent = mean(employed ==1, na.rm = TRUE))

hilda %>% 
  filter(hgage >= 15 & hgage <= 64) %>% 
  filter(year == 2019) %>% filter(employed == 1) %>%
  summarise(casual_percent = mean(casual == 1, na.rm = TRUE),
            hours_percent = mean(hours, na.rm = TRUE),
            pt_percent = mean(esdtl == 2, na.rm = TRUE))

## Hours worked for working age people who did/did not withdraw super --------

trends_vis_unmatched <- hilda %>% 
  filter(hgage >= 15 & hgage <= 64) %>% 
  group_by(year, withdrew) %>% 
  summarise(hours = weighted.mean(hours, w = hhwtrp, na.rm = TRUE)) %>% 
  #summarise(hours = mean(hours, na.rm = TRUE)) %>% 
  group_by(withdrew) %>% 
  mutate(hours_index = hours / hours[8] * 100)

trends_vis_unmatched %>% 
  ggplot(aes(x = year, y = hours, colour = withdrew)) + 
  geom_line() +
  labs(x = "Year", y = "Weekly hours usually worked",
       title = "Hours worked by withdrawal status",
       caption = "Working age population") +
  scale_x_continuous(breaks = seq(2012, 2022, 2)) +
  theme(legend.position = "bottom")
ggsave("output_new/figs/hours_worked.png",
       height = 4, width = 8)

trends_vis_unmatched %>% 
  ggplot(aes(x = year, y = hours_index, colour = withdrew)) + 
  geom_line() +
  labs(x = "Year", y = "Weekly hours usually worked",
       title = "Hours worked by withdrawal status",
       caption = "Working age population") +
  scale_x_continuous(breaks = seq(2012, 2022, 2)) +
  theme(legend.position = "bottom")
ggsave("output_new/figs/hours_worked_indexed.png",
       height = 4, width = 8)

### By occupation -----------------------------------------------

# Make 1-digit occupation
hilda <- hilda %>% 
  mutate(occupation = ifelse(jbmo61 > 0, jbmo61, NA))

occp_labels <- tribble(
  ~code, ~Occupation,
  1, "1. Managers",
  2, "2. Professionals",
  3, "3. Technicians and Trades Workers",
  4, "4. Community and Personal Service Workers",
  5, "5. Clerical and Administrative Workers",
  6, "6. Sales Workers",
  7, "7. Machinery Operators and Drivers",
  8, "8. Labourers"
)

# full sample 

hilda %>% 
  filter(hgage >= 15 & hgage <= 64) %>% 
  filter(jbm682 > 0) %>% 
  group_by(year, withdrew, occupation) %>% 
  summarise(hours = mean(hours, na.rm = TRUE)) %>% 
  left_join(occp_labels, by = c("occupation" = "code")) %>% 
  ggplot(aes(x = year, y = hours, colour = withdrew)) + 
  geom_line() +
  labs(x = "Year", 
       y = "Weekly hours usually worked",
       caption = "Note: Working age population, full sample",
       colour = NULL) +  # Omit the legend title for 'withdrew'
  scale_x_continuous(breaks = seq(2012, 2022, 2)) +
  facet_wrap(~Occupation) +
  theme_minimal(base_size = 10) +  # Applying a minimal theme with increased base size
  theme(
    plot.caption = element_text(hjust = 0),  # Align the caption to the left
    axis.title = element_text(size = 10),    # Increase axis titles
    axis.text = element_text(size = 10),     # Increase axis text
    strip.text = element_text(size = 10),    # Increase facet wrap labels
    legend.title = element_blank(),
    legend.text = element_text(size = 10),# Ensure legend title is blank
    legend.position = "bottom",              # Move legend to the bottom
    legend.direction = "horizontal"          # Arrange legend items horizontally
  )
ggsave("output_new/figs/hours_worked_occupation_full.png",
       height = 6, width = 10)

# unaffected hours

hilda %>% 
  filter(unaffected_2020 == 1) %>% 
  filter(hgage >= 15 & hgage <= 64) %>% 
  filter(jbm682 > 0) %>% 
  group_by(year, withdrew, occupation) %>% 
  summarise(hours = mean(hours, na.rm = TRUE)) %>% 
  left_join(occp_labels, by = c("occupation" = "code")) %>% 
  ggplot(aes(x = year, y = hours, colour = withdrew)) + 
  geom_line() +
  labs(x = "Year", 
       y = "Weekly hours usually worked",
       caption = "Note: Working age population, people with unaffected jobs",
       colour = NULL) +  # Omit the legend title for 'withdrew'
  scale_x_continuous(breaks = seq(2012, 2022, 2)) +
  facet_wrap(~Occupation) +
  theme_minimal(base_size = 10) +  # Applying a minimal theme with increased base size
  theme(
    plot.caption = element_text(hjust = 0),  # Align the caption to the left
    axis.title = element_text(size = 10),    # Increase axis titles
    axis.text = element_text(size = 10),     # Increase axis text
    strip.text = element_text(size = 10),    # Increase facet wrap labels
    legend.title = element_blank(),
    legend.text = element_text(size = 10),# Ensure legend title is blank
    legend.position = "bottom",              # Move legend to the bottom
    legend.direction = "horizontal"          # Arrange legend items horizontally
  )
ggsave("output_new/figs/hours_worked_occupation_unaff.png",
       height = 6, width = 10)

### By state --------------------------------------------------

states <- tribble(
  ~state_id, ~state_name,
  1, "1. NSW",
  2, "2. VIC",
  3, "3. QLD",
  4, "4. SA",
  5, "5. WA",
  6, "6. TAS",
  7, "7. NT",
  8, "8. ACT"
)

# Full sample #

hilda %>% 
  filter(hgage >= 15 & hgage <= 64) %>% 
  filter(hhstate > 0) %>% 
  group_by(year, withdrew, hhstate) %>% 
  summarise(hours = mean(hours, na.rm = TRUE)) %>% 
  left_join(states, by = c("hhstate" = "state_id")) %>% 
  ggplot(aes(x = year, y = hours, colour = withdrew)) + 
  geom_line() +
  labs(x = "Year", y = "Weekly hours usually worked",
       title = "Hours worked by withdrawal status by State",
       caption = "Note: Working age population, full sample") +
  scale_x_continuous(breaks = seq(2012, 2022, 2)) +
  facet_wrap(~state_name) +
  theme_minimal(base_size = 10) +  # Applying a minimal theme with increased base size
  theme(
    plot.caption = element_text(hjust = 0),  # Align the caption to the left
    axis.title = element_text(size = 10),    # Increase axis titles
    axis.text = element_text(size = 10),     # Increase axis text
    strip.text = element_text(size = 10),    # Increase facet wrap labels
    legend.title = element_blank(),
    legend.text = element_text(size = 10),# Ensure legend title is blank
    legend.position = "bottom",              # Move legend to the bottom
    legend.direction = "horizontal"          # Arrange legend items horizontally
  )
ggsave("output_new/figs/hours_worked_state_full.png",
       height = 6, width = 10)

# Unaffected sample #

hilda %>% 
  filter(hgage >= 15 & hgage <= 64) %>% filter(unaffected_2020 == 1) %>%
  filter(hhstate > 0) %>% 
  group_by(year, withdrew, hhstate) %>% 
  summarise(hours = mean(hours, na.rm = TRUE)) %>% 
  left_join(states, by = c("hhstate" = "state_id")) %>% 
  ggplot(aes(x = year, y = hours, colour = withdrew)) + 
  geom_line() +
  labs(x = "Year", y = "Weekly hours usually worked",
       title = "Hours worked by withdrawal status by State",
       caption = "Note: Working age population, jobs unaffected") +
  scale_x_continuous(breaks = seq(2012, 2022, 2)) +
  facet_wrap(~state_name) +
  theme_minimal(base_size = 10) +  # Applying a minimal theme with increased base size
  theme(
    plot.caption = element_text(hjust = 0),  # Align the caption to the left
    axis.title = element_text(size = 10),    # Increase axis titles
    axis.text = element_text(size = 10),     # Increase axis text
    strip.text = element_text(size = 10),    # Increase facet wrap labels
    legend.title = element_blank(),
    legend.text = element_text(size = 10),# Ensure legend title is blank
    legend.position = "bottom",              # Move legend to the bottom
    legend.direction = "horizontal"          # Arrange legend items horizontally
  )
ggsave("output_new/figs/hours_worked_state_unaff.png",
       height = 6, width = 10)

### For those who lost work in covid ----------------------------------------

hilda %>% 
  filter(hgage >= 15 & hgage <= 64) %>% 
  filter(reduced_hrs_2020 == 1) %>% 
  group_by(year, withdrew) %>% 
  summarise(hours = mean(hours, na.rm = TRUE),
            n = n()) %>% 
  ggplot(aes(x = year, y = hours, colour = withdrew)) + 
  geom_line() +
  labs(x = "Year", y = "Weekly hours usually worked",
       title = "Hours worked by withdrawal status for those who lost work/hours due to Covid-19",
       caption = "Working age population") +
  scale_x_continuous(breaks = seq(2012, 2022, 2))

hilda %>% 
  filter(hgage >= 25 & hgage <= 55) %>%  #by alternative specification
  filter(reduced_hrs_2020 == 1) %>% 
  group_by(year, withdrew) %>% 
  summarise(hours = mean(hours, na.rm = TRUE),
            n = n()) %>% 
  ggplot(aes(x = year, y = hours, colour = withdrew)) + 
  geom_line() +
  labs(x = "Year", y = "Weekly hours usually worked",
       title = "Hours worked by withdrawal status for those who lost work/hours due to Covid-19",
       caption = "Working age population") +
  scale_x_continuous(breaks = seq(2012, 2022, 2))


## Employment status for working age people who did/did not withdraw super ---

hilda %>% 
  filter(hgage >= 15 & hgage <= 64) %>% 
  group_by(year, withdrew_super) %>% 
  summarise(employed = mean(employed, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = employed, colour = as.factor(withdrew_super))) + 
  geom_line()

hilda %>% 
  filter(hgage >= 25 & hgage <= 54) %>% #using a constrained definition
  group_by(year, withdrew_super) %>% 
  summarise(employed = mean(employed, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = employed, colour = as.factor(withdrew_super))) + 
  geom_line()

# Crosstab of withdrawers vs. those who lost work

hilda %>% 
  filter(year == 2020) %>% 
  mutate(reduced_hrs = ifelse(cvdchr == 1 | cvrd == 1 | cvul == 1, 1, 0)) %>% 
  tabyl(withdrew_super, reduced_hrs)

## Share of working age people who withdrew ---------------------------------

hilda %>% 
  filter(hgage >= 15 & hgage <= 64) %>% 
  filter(year == 2021) %>% 
  tabyl(withdrew_super)

hilda %>% 
  filter(year == 2021 & hgage > 0) %>% 
  ggplot(aes(x = hgage, fill = as.factor(withdrew_super))) +
  geom_density(alpha = 0.2)


## Superannuation balances --------------------------------------------------

hilda %>% 
  filter(year == 2018) %>% 
  filter(savaln2 > 0) %>% 
  group_by(savaln2, withdrew) %>% 
  summarise(n = sum(hhwtrp)) %>% 
  #count() %>% 
  mutate(savaln2 = as_factor(savaln2)) %>% 
  ggplot(aes(x = savaln2, y = n)) +
  geom_col() +
  facet_wrap(~withdrew, scales = "free_x") +
  coord_flip() +
  labs(y = "Count",
       x = "Superannuation balance")


hilda %>% 
  filter(year == 2018) %>% 
  filter(savaln2 > 0) %>% 
  mutate(savaln2 = as_factor(savaln2)) %>% 
  ggplot(aes(x = savaln2, fill = withdrew)) +
  geom_bar() +
  coord_flip()


hilda %>% 
  filter(hgage >= 15 & hgage <= 64) %>% 
  filter(year == 2018) %>% 
  filter(savaln2 > 0) %>% 
  mutate(savaln2 = as_factor(savaln2)) %>% 
  group_by(savaln2, withdrew) %>% 
  summarise(n = sum(hhwtrp)) %>% 
  mutate(savaln2 = fct_relevel(savaln2, c("[97] Has no super funds"))) %>% 
  ggplot(aes(x = savaln2, y = n, fill = withdrew)) +
  geom_col() +
  coord_flip() +
  labs(x = "Superannuation balance",
       y = "Weighted observations",
       fill = "") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::comma)
ggsave("output_new/figs/super_balances.png",
       height = 5, width = 5)

## Amount withdrawn -------------------------------

# Version with shares of withdrawers
hilda %>% 
  filter(oifcva > 0) %>% 
  mutate(super = cut(oifcva, seq(0, 10000, by = 1000),
                     include.lowest = TRUE,
                    labels = c(
                      "$0-1k",
                      "$1-2k",
                      "$2-3k",
                      "$3-4k",
                      "$4-5k",
                      "$5-6k",
                      "$6-7k",
                      "$7-8k",
                      "$8-9k",
                      "$9-10k"
                    ))) %>% 
  count(super) %>% 
  mutate(share = n / sum(n)) %>% 
  ggplot(aes(x = super, y = share)) +
  geom_col(fill = "#0097a7") +
  geom_text(aes(label = scales::percent(share, accuracy = 1), 
                y = share + 0.02),  # Position text slightly above the bar
            size = 4, color = "black", fontface = "bold") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 0.8, by = 0.2),
                     limits = c(0, 0.8)) +
  theme_minimal(base_size = 15) +  # Applying a minimal theme with increased base size
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5),  # Center the plot title
    axis.title = element_text(size = 12),    # Increase axis titles
    axis.text = element_text(size = 12)      # Increase axis text
  ) +
  labs(x = "Amount Withdrawn ($AU)",
       y = "Share of withdrawers")

ggsave("output_new/figs/super_withdrawals.png",
       height = 4, width = 7)

replace_na_0 <- function(x) ifelse(is.na(x), 0, x)

hilda %>% 
  filter(year == 2020 | year == 2021) %>% 
  tabyl(oifcvs, year)

hilda %>% 
  filter(year == 2020 | year == 2021) %>% 
  filter(oifcvs == 1 | oifcvs == 2) %>% 
  select(waveid, year, oifcvs) %>% 
  pivot_wider(names_from = year, values_from = oifcvs) %>% 
  mutate(`2020` = replace_na_0(ifelse(`2020` == 1, 1, 0)),
         `2021` = replace_na_0(ifelse(`2021` == 1, 1, 0))) %>% 
  mutate(Total = `2020` + `2021`) %>% 
  mutate(Withdrew = case_when(
    Total == 2 ~ "Both",
    `2021` == 1 ~ "2021 only",
    `2020` == 1 ~ "2020 only",
    Total == 0 ~ "Neither"
  )) %>% 
  filter(Withdrew != "Neither") %>% 
  tabyl(Withdrew)

# Create weighted summary stats -------------------------------------------------------

hilda_2019_svy <- hilda %>%
  filter(hgage >= 15 & hgage <= 64) %>% 
  filter(year == 2019) %>% 
  mutate(self_employed = ifelse(esempst == 2 | esempst == 3, 1, 0),
         
         wscei = ifelse(wscei < 0, NA, wscei),
         jbhruc = ifelse(jbhruc < 0, NA, jbhruc),
         esbrd = ifelse(esbrd < 0, NA, esbrd),
         jbmi61 = ifelse(jbmi61 < 0, NA, jbmi61),
         jbmo61 = ifelse(jbmo61 < 0, NA, jbmo61))

hilda_2019_svy %>%
  summarise(yes_withdrew = sum(withdrew_super == 1, na.rm = TRUE),
            no_withdrew = sum(withdrew_super == 0, na.rm = TRUE))

hilda_2019_svy %>% 
  summarise(are_impatient = sum(impatience == 1, na.rm = TRUE),
            are_patient = sum(impatience == 0, na.rm = TRUE))

hilda_2019_svy %>% 
  summarise(loc_above_median = sum(loc_median == "Above", na.rm = TRUE),
            loc_below_median = sum(loc_median == "Below", na.rm = TRUE))

# Weight survey ----

hilda_svy <- hilda_2019_svy %>%
  as_survey_design(weights = hhwtrp)

# Group variables - labour force status
hilda_svy %>% 
  group_by(esbrd) %>% 
  summarise(total = survey_mean(, na.rm = TRUE))

hilda_svy %>% 
  filter(esbrd == 1) %>% 
  summarise(Hour = survey_mean(jbhruc, na.rm=TRUE),
            Causal = survey_mean(casual, na.rm=TRUE))

hilda_svy %>% 
  filter(esbrd == 1) %>% group_by(esdtl) %>% 
  summarise(total = survey_mean(, na.rm = TRUE))

# Weighted general summary stats -----------------------------------------

hilda_svy %>% 
  summarise(Age = survey_mean(hgage),
            Female = survey_mean(female),
            Partner = survey_mean(partner),
            Dependents = survey_mean(children),
            
            Selfemp = survey_mean(self_employed),
            Casual = survey_mean(casual),
            Wage = survey_mean(wscei, na.rm=TRUE), 
            Hours = survey_mean(jbhruc, na.rm=TRUE),
            Priv_income = survey_mean(private_income, na.rm = TRUE),
            Exp = survey_mean(ehtjb, na.rm=TRUE),
            
            Debt = survey_mean(debt_2018, na.rm=TRUE),
            Savings = survey_mean(savings_2018, na.rm=TRUE)) %>% 
  write_xlsx("output_new/general means.xlsx")

## Summary stats for withdrawers -----------------------------------

hilda_svy %>% 
  group_by(withdrew_super) %>% 
  summarise(Age = survey_mean(hgage),
            Female = survey_mean(female),
            Partner = survey_mean(partner),
            Dependents = survey_mean(children),
            House = survey_mean(home_2018, na.rm =TRUE),
            
            Selfemp = survey_mean(self_employed),
            Casual = survey_mean(casual),
            Wage = survey_mean(wscei, na.rm=TRUE), 
            Hours = survey_mean(jbhruc, na.rm=TRUE),
            Priv_income = survey_mean(private_income, na.rm = TRUE),
            Exp = survey_mean(ehtjb, na.rm = TRUE),
            
            Debt = survey_mean(debt_2018, na.rm=TRUE),
            Savings = survey_mean(savings_2018, na.rm=TRUE)) %>% 
  write_xlsx("output_new/withdrawer means.xlsx")

## Group variables - labour force status ----
hilda_svy %>% 
  group_by(withdrew_super, esbrd) %>% 
  summarise(total = survey_mean(, na.rm = TRUE)) %>% 
  write_xlsx("output_new/esbrd.xlsx")

## Group variables - superannuation ----
hilda_svy %>% 
  group_by(withdrew_super, savaln2_2018) %>% 
  summarise(total = survey_mean(, na.rm = TRUE)) %>%
  write_xlsx("output_new/super.xlsx")

## Group variables - state of residence ----
hilda_svy %>% 
  group_by(withdrew_super, hhstate) %>% 
  summarise(total = survey_mean(, na.rm = TRUE)) %>%
  write_xlsx("output_new/hhstate.xlsx")

## Group variables - industry ----
hilda_svy %>% 
  group_by(withdrew_super, jbmi61) %>% 
  summarise(total = survey_mean(, na.rm = TRUE)) %>%
  write_xlsx("output_new/industry.xlsx")

# Group variables - occupation
hilda_svy %>% 
  group_by(withdrew_super, jbmo61) %>% 
  summarise(total = survey_mean(, na.rm = TRUE)) %>%
  write_xlsx("output_new/occupation.xlsx")

# Summary stats for patient and impatient --------------------------

hilda_svy %>% 
  group_by(impatience) %>%
  summarise(Age = survey_mean(hgage),
            Female = survey_mean(female),
            Partner = survey_mean(partner),
            Dependents = survey_mean(children),
            House = survey_mean(home_2018, na.rm =TRUE),
            
            Selfemp = survey_mean(self_employed),
            Casual = survey_mean(casual),
            Wage = survey_mean(wscei, na.rm=TRUE), 
            Hours = survey_mean(jbhruc, na.rm=TRUE),
            Priv_income = survey_mean(private_income, na.rm = TRUE),
            Exp = survey_mean(ehtjb, na.rm = TRUE),
            
            Debt = survey_mean(debt_2018, na.rm=TRUE),
            Savings = survey_mean(savings_2018, na.rm=TRUE)) %>% 
  write_xlsx("output_new/impatience summary.xlsx")

# Group variables - labour force status
hilda_svy %>% 
  group_by(impatience, esbrd) %>% 
  summarise(total = survey_mean(, na.rm = TRUE)) %>% 
  write_xlsx("output_new/impatience esbrd.xlsx")

# Group variables - superannuation
hilda_svy %>% 
  group_by(impatience, savaln2_2018) %>% 
  summarise(total = survey_mean(, na.rm = TRUE)) %>%
  write_xlsx("output_new/impatience super.xlsx")

# Summary stats for above and below median loc ----------------------------

hilda_svy %>% 
  group_by(loc_median) %>%
  summarise(Age = survey_mean(hgage),
            Female = survey_mean(female),
            Partner = survey_mean(partner),
            Dependents = survey_mean(children),
            House = survey_mean(home_2018, na.rm =TRUE),
            
            Selfemp = survey_mean(self_employed),
            Casual = survey_mean(casual),
            Wage = survey_mean(wscei, na.rm=TRUE), 
            Hours = survey_mean(jbhruc, na.rm=TRUE),
            Priv_income = survey_mean(private_income, na.rm = TRUE),
            Exp = survey_mean(ehtjb, na.rm = TRUE),
            
            Debt = survey_mean(debt_2018, na.rm=TRUE),
            Savings = survey_mean(savings_2018, na.rm=TRUE)) %>% 
  write_xlsx("output_new/loc_median summary.xlsx")

# Group variables - labour force status
hilda_svy %>% 
  group_by(loc_median, esbrd) %>% 
  summarise(total = survey_mean(, na.rm = TRUE)) %>% 
  write_xlsx("output_new/loc_median esbrd.xlsx")

# Group variables - superannuation
hilda_svy %>% 
  group_by(loc_median, savaln2_2018) %>% 
  summarise(total = survey_mean(, na.rm = TRUE)) %>%
  write_xlsx("output_new/loc_median super.xlsx")


# Histogram for withdrawers -----------------------------------

hilda_2019_histo <- hilda %>% 
  filter(
    year == 2019,
    withdrew_super == 1
    )

hilda_2019_histo %>% 
    filter(hgage > 0) %>% 
    mutate(age = cut(hgage, seq(15, 100, by = 5),
                         include.lowest = TRUE,
                         labels = c(
                           "15-19",
                           "20-24",
                           "25-29",
                           "30-34",
                           "35-39",
                           "40-44",
                           "45-49",
                           "50-54",
                           "55-59",
                           "60-64",
                           "65-69",
                           "70-74",
                           "75-79",
                           "80-84",
                           "85-89",
                           "90-94",
                           "95-99"
                         ))) %>% 
      count(age) %>% 
      mutate(share = n / sum(n)) %>% 
      ggplot(aes(x = age, y = share)) +
      geom_col(fill = "#0097a7") +
      geom_text(aes(label = scales::percent(share, accuracy = 0.1), 
                    y = share + 0.02),  # Position text slightly above the bar
                size = 4, color = "black", fontface = "bold") +
      scale_y_continuous(labels = scales::percent,
                         breaks = seq(0, 0.8, by = 0.05),
                         limits = c(0, 0.25)) +
      theme_minimal(base_size = 15) +  # Applying a minimal theme with increased base size
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5),  # Center the plot title
        axis.title = element_text(size = 12),    # Increase axis titles
        axis.text = element_text(size = 12)      # Increase axis text
      ) +
      labs(x = "Age",
           y = "Share of withdrawers")
    
ggsave("output_new/figs/age_withdrawals.png",
       height = 4, width = 7)

# Look at dates of interviews ----

hilda %>% 
  filter(year %in% c(2020, 2021)) %>% 
  mutate(interview_date = dmy(hhhqivw)) %>% 
  ggplot(aes(x = interview_date)) +
  geom_bar() +
  scale_x_date(breaks = "2 months")
ggsave("output_new/figs/interview_dates.png")  

interview_2020 <- hilda %>% 
  filter(year %in% c(2020)) %>% 
  mutate(interview_date = dmy(hhhqivw))

sort(interview_2020$interview_date) |> table()

interview_2021 <- hilda %>% 
  filter(year %in% c(2021)) %>% 
  mutate(interview_date = dmy(hhhqivw))

sort(interview_2021$interview_date) |> table()
