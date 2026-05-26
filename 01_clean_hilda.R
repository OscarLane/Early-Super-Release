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

# Function definitions ---------------------------------------------------------

# Function to read HILDA quickly, only reading in variables we specify
read_hilda <- function(file, vars) {
  # Get wave year from filename letter
  year_letter <- str_extract(file, "Combined_[a-z]") %>% 
    str_sub(-1, -1)
  myletters <- letters[1:26]
  year <- match(year_letter, myletters) + 2000
  
  # Read desired variables
  wave_data <- read_dta(
    file = file,
    col_select = contains(c("waveid", vars))
  )
  
  # Remove prefixes
  colnames(wave_data) <- str_sub(colnames(wave_data), 2, -1)
  
  # Add year column
  wave_data <- wave_data %>% 
    mutate(year = year)
  
  wave_data
}


# Read in HILDA ----------------------------------------------------------------

oscar_path_hilda <- "~/Documents/HILDA22_General/HILDA22_Combined"
mingji_path_hilda <- "C:/Users/mingj/Documents/HILDA22_General/HILDA22_Combined"

if (dir.exists(oscar_path_hilda)) {
  hilda_path <- oscar_path_hilda
}

if (dir.exists(mingji_path_hilda)) {
  hilda_path <- mingji_path_hilda
}

hilda_vars <- c(
  "xwaveid"   # Person ID 
  ,"hhpxid"   # Partner's cross-wave id
  ,"hhrhid"   # DV: Randomised household ID
  ,"hhrpid"   # Household identifier
  ,"hhhqivw"  # Date of interview
  
  ,"esbrd"    # Current labour force status - broad
  ,"esdtl"    # Current labour force status - detailed
  ,"esempst"  # Employment status
  ,"wscei"    # Weekly gross wages & salary
  ,"jbhruc"   # Hours usually worked in a week
  ,"wth"      # Weights - households
  ,"hhwte"    # Weights - enumerated persons
  ,"hhwtrp"   # Weights - Responding person population weight
  ,"hhwtsc"   # Weights SCQ cross-sectional weight
  ,"wsfes"    # Gross annual income from wages and salaries incl. of salary sac
  ,"wsfei"    # Gross annual income from wages and salaries pre-salary sac
  ,"capj"     # Per cent of last FY in jobs
  ,"jbempt"   # Tenure (years) with current employer
  ,"hgage"    # Age
  ,"hgsex"    # Sex
  ,"ehtjb"    # Paid work experience (years)
  ,"edfts"    # Whether or not a full time student
  ,"jbm682"   # DV: ISCO-88 2-digit, Occupation current main job	
  ,"jbmo61"   # DV: C11 Occupation 1-digit ANZSCO 2006
  ,"jbmo62"   # DV: C11 Occupation 2-digit ANZSCO 2006
  ,"jbmi61"   # DV: C14 Current main job industry. ANZSIC 2006 division
  ,"jbmi62"   # DV: C14 Current main job industry. 2-digit ANZSIC 2006	
  ,"hgint"    # Flag which indicates if an enumerated person completed a person interview (either new or continuing).
  ,"hhstate"  # State
  ,"jbcasab"  # Casual worker flag
  ,"chkhru"   # Part time worker flag
  ,"esdtl"    # Detailed labour force status (FT/PT)
  
  ,"tifpiin" # DV: Financial year regular private income ($) [imputed] Negative values	
  ,"tifpiip" # DV: Financial year regular private income ($) [imputed] Positive values [weighted topcode]

  # Conscientiousness from Big Five
  ,"pnconsc"  # Conscientiousness
  
  # Locus of Control measures (Seven questions - in wave 3, 4, 7, 11, 15, 2019)
  ,"lssecd" #Personal control: Can do just about anything	[Internal locus]
  ,"lsseci" #Personal control: Cannot change important things in life [External locus]	
  ,"lssefd" #Personal control: Future depends on me	[Internal locus]
  ,"lssefh" #Personal control: Feel helpless	[External locus]	
  ,"lsselc" #Personal control: Little control	[External locus]	
  ,"lssepa" #Personal control: Pushed around	[External locus]	
  ,"lssesp" #Personal control: No way to solve problems	 [External locus]	

  #Other household variables
  ,"hhd0_4"   # DV: Number of dependent children aged 0-4 (includes partner's children)	
  ,"hhd5_9"   # DV: Number of dependent children aged 5-9 (includes partner's children)	
  ,"hhd1014"  # DV: Number of dependent children aged 10-14 (includes partner's children)	
  ,"hhd1524"  # DV: Number of dependent children aged 15-24 (includes partner's children)
  ,"mrcms"    # Current marital status

  # Own education
  ,"edhigh1"  # History: Highest education level achieved
  
  #Financial variables - savings, debts, traits
  ,"rpevown" # Do you currently own, or are you buying your own home or any other residential property? [WAVE 18]
  ,"pwobank" # DV: Own bank accounts ($) [weighted topcode]
  ,"pwjbank" # DV: Joint bank accounts ($) [weighted topcode]
  ,"pwoccdt" # DV: Own credit card debt ($) [weighted topcode]
  ,"pwothdt" # DV: Other Debt: Car loans/Investment loans/Personal loans/Hire purchase/Overdue bills ($) [weighted topcode]
  ,"fisave"  # Which of the following statements comes closest to describing your (and your family’s) savings habits ? [WAVE 18]
  ,"fisavep" # Most important time period when planning savings and spending [WAVE 18]
  
   # Covid-19 variables - economic support
  ,"bnfesp"   # Received Economic Support Payment	
  ,"bnfespa"  # DV: Bonus payment - Economic Support Payment ($) [estimated]	
  ,"bnfespr"  # Economic Support Payment included when reporting income from government	
  ,"cvipe"    # Income normally received from paid employment increase or decrease because of the coronavirus or did it not change much	
  ,"bncnws"   #"Do you currently receive any of these government pensions or allowances - Jobseeker Payment"
  ,"cvjkhav"  # Personally received or employer claimed on your behalf, any JobKeeper payments	
  
  ,"oifcva"   # Amount withdrawn from superannuation under the COVID-19 scheme for early release of super	
  ,"oifcvr"   # Withdrawn superannuation under the COVID-19 scheme for early release of super - reported earlier	
  ,"oifcvs"   # Did you withdraw superannuation under the COVID-19 scheme for early release of super	
  ,"cvwdsp"   # Withdrew money from any of your superannuation funds because of the coronavirus crisis	
  ,"cvwdspa"  # Amount withdrawn from any of your superannuation funds because of the coronavirus crisis	
 
  # Covid-19 variables - Employment
  ,"cvdchr"   # As a result of the coronavirus, kept working, but with reduced hours	
  ,"cvrd"     # As a result of the coronavirus, employment terminated or made redundant (that is, lost your job entirely)	
  ,"cvul"     # As a result of the coronavirus, temporarily stood down without pay or required to take unpaid leave	
  
  # Superannuation variables
  ,"sacfnd"   # Have capital in any super, allocated pension, roll-over or capital-annuity funds	
  ,"sacfnd2"  # What is total current value of capital in all these funds	
  ,"savaln2"  # Value of all super funds
  ,"rsapc"    # Do you make contributions to your superannuation fund above what your employer is required to put in?

  )

hilda_files <- list.files(hilda_path, full.names = TRUE)

# Limit to 2012 onwards to avoid issues with top-up sample affecting trends
hilda_files <- hilda_files[12:22]

# Apply function to all waves
hilda_raw <- map(hilda_files, ~ read_hilda(., vars = hilda_vars))

# Bind waves together
hilda_raw <- bind_rows(hilda_raw)

# Data cleaning ----------------------------------------------------------------

hilda <- hilda_raw

hilda19 <- hilda %>% filter(year==2020)

## Remove non-responding persons ------------------------------------------------

hilda <- hilda %>% 
  filter(hgint == 1)

## Numeric waveids -------------------------------------------------------------

hilda <- hilda %>% 
  mutate(waveid = as.numeric(waveid),
         hhrpid = as.numeric(hhrpid),
         hhpxid = as.numeric(hhpxid),
         hhrhid = as.numeric(hhrhid))

# Variable creation -----------------------------------------------------------

## Female dummy ----------------------------------------------------------------

hilda <- hilda %>%
  mutate(
    female = case_when(
      hgsex == 1 ~ 0,
      hgsex == 2 ~ 1,
      TRUE ~ NA_integer_
    )
  )

## Employment dummy -------------------------------------------------------------

hilda$esbrd[hilda$esbrd <0] <- NA

hilda <- hilda %>% 
  mutate(employed = case_when(
    is.na(esbrd) ~ NA,
    esbrd == 1 ~ 1,
    esbrd == 2 ~ 0,
    esbrd == 3 ~ 0,
    TRUE ~ NA_integer_
  ))

## Education skill dummies -----------------------------------------------------

#construct mapping of high, medium, low skill based on education levels
educ_lvl <- list( 
  h_skill = c("1", "2", "3"),
  m_skill = c("4", "5"),
  l_skill = c("8", "9"))

hilda$edhigh1[hilda$edhigh1 <0] <- NA

hilda <- hilda %>% #map to education level
  mutate(
    skill_level = case_when(
      edhigh1 %in% educ_lvl$h_skill ~ "high_skill",
      edhigh1 %in% educ_lvl$m_skill ~ "med_skill",
      edhigh1 %in% educ_lvl$l_skill ~ "low_skill",
      TRUE ~ NA  
    )
  )

## Employment type  ---------------------------------------------------

hilda$esdtl[hilda$esdtl <0] <- NA
hilda$esdtl[hilda$esdtl == 7] <- NA

# Net savings, debt, savings in 2018 ------------------------------------------

hilda$pwoccdt[hilda$pwoccdt < 0] <- NA #Own credit card debt ($), Responding Person File
hilda$pwothdt[hilda$pwothdt < 0] <- NA # Other debt ($), Responding Person File

hilda$pwobank[hilda$pwobank < 0] <- NA #Own bank accounts ($), Responding Person file
hilda$pwjbank[hilda$pwjbank < 0] <- NA #Joint bank accounts ($), Responding Person's share

hilda <- hilda %>%
  group_by(waveid) %>%
  mutate(
    pwoccdt_2022 = ifelse(any(year == 2022), pwoccdt[year == 2022], NA),
    pwoccdt_2018 = ifelse(any(year == 2018), pwoccdt[year == 2018], NA),
    
    pwothdt_2022 = ifelse(any(year == 2022), pwothdt[year == 2022], NA),
    pwothdt_2018 = ifelse(any(year == 2018), pwothdt[year == 2018], NA),
    
    pwobank_2022 = ifelse(any(year == 2022), pwobank[year == 2022], NA),
    pwobank_2018 = ifelse(any(year == 2018), pwobank[year == 2018], NA),
    
    pwjbank_2022 = ifelse(any(year == 2022), pwjbank[year == 2022], NA),
    pwjbank_2018 = ifelse(any(year == 2018), pwjbank[year == 2018], NA),
    
    debt_2018 = pwoccdt_2018 + pwothdt_2018,
    debt_2022 = pwoccdt_2022 + pwothdt_2022,
   
    savings_2018 = pwobank_2018 + pwjbank_2018,
    savings_2022 = pwobank_2022 + pwjbank_2022,
    
    net_savings_2018 = savings_2018 - debt_2018,
    net_savings_2022 = savings_2022 - debt_2022,
    
    debt_change = debt_2022 - debt_2018,
    savings_change = savings_2022 - savings_2018,
    net_savings_change = net_savings_2022 - net_savings_2018) %>%
  
  ungroup()

## Net savings, lower vs. upper 50 percentile -----------
net_savings_2018_median <- median(hilda$net_savings_2018, na.rm = TRUE)
hilda <- hilda %>% 
  mutate(net_savings_2018_upper_50pct = net_savings_2018 >= net_savings_2018_median)

# Employed dummy --------------------------------------------------------------
hilda <- hilda %>% 
  mutate(employed = ifelse(esbrd == 1, 1, 0))

hilda <- hilda %>%
  group_by(waveid) %>%
  mutate(
    employed_2017 = ifelse(any(year == 2017 & employed == 1), 1, 0),
    employed_2018 = ifelse(any(year == 2018 & employed == 1), 1, 0),
    employed_2019 = ifelse(any(year == 2019 & employed == 1), 1, 0),
    employed_2020 = ifelse(any(year == 2020 & employed == 1), 1, 0),
    employed_2021 = ifelse(any(year == 2021 & employed == 1), 1, 0),
    employed_2022 = ifelse(any(year == 2022 & employed == 1), 1, 0)

  ) %>%
  ungroup()


# Hours variable -------------------------------------------------------------
hilda <- hilda %>% 
  mutate(hours = case_when(
    employed == 0 ~ 0, #unemployed coded as 0
    jbhruc >= 0 ~ jbhruc,
    TRUE ~ NA
  ))

# Weekly wage variable ---------------------------------------------------------
hilda <- hilda %>% 
  mutate(wage = case_when(
    employed == 0 ~ 0,
    wscei >= 0 ~ wscei,
    TRUE ~ NA
  ))

# Total FY disposable income ----------------------------------------------------------

hilda$tifpiin[hilda$tifpiin < 0] <- NA 
hilda$tifpiip[hilda$tifpiip < 0] <- NA 

hilda <- hilda  %>%
  mutate(
    tifpiin = as.numeric(tifpiin),
    tifpiip = as.numeric(tifpiip),
    private_income = case_when(
    !is.na(tifpiin) & tifpiin > 0 ~ -tifpiin,   # flip sign if negative value available
    !is.na(tifpiip) & tifpiip > 0 ~ tifpiip,    # otherwise use other variable
    TRUE ~ NA_real_                           # keep NA if neither exists
  ))

## Hours changed (2019 to 2021) variable ----------------------------------------

hilda <- hilda %>%
  group_by(waveid) %>%
  mutate(
    hours_2016 = ifelse(any(year == 2016), hours[year == 2016], NA),
    hours_2017 = ifelse(any(year == 2017), hours[year == 2017], NA),
    hours_2018 = ifelse(any(year == 2018), hours[year == 2018], NA),
    
    hours_2019 = ifelse(any(year == 2019), hours[year == 2019], NA),
    hours_2020 = ifelse(any(year == 2020), hours[year == 2020], NA),
    hours_2021 = ifelse(any(year == 2021), hours[year == 2021], NA),
    hours_2022 = ifelse(any(year == 2022), hours[year == 2022], NA),
    
    hr_1819 = hours_2019 - hours_2018,
    hr_1718 = hours_2018 - hours_2017,
    
    hr_1920 = hours_2020 - hours_2019,
    hr_1921 = hours_2021 - hours_2019,
    hr_1922 = hours_2022 - hours_2019) %>%
  
  ungroup()

## Withdrew super indicator ----------------------------------------------------

withdrew <- hilda %>% 
  filter(oifcvs == 1) %>% 
  distinct(waveid) %>% 
  mutate(withdrew_super = 1)

hilda <- hilda %>% 
  mutate(withdrew_super = ifelse(waveid %in% withdrew$waveid, 1, 0))

hilda <- hilda %>% 
  mutate(withdrew = case_when(
    withdrew_super == 0 ~ "Did not withdraw",
    withdrew_super == 1 ~ "Withdrew"
  ))

# Withdrew 2020 & 2021

withdrew_2020 <- hilda %>% 
  filter(oifcvs == 1 & year == 2020) %>% 
  distinct(waveid) %>% 
  mutate(withdrew_2020 = 1)

withdrew_2021 <- hilda %>% 
  filter(oifcvs == 1 & year == 2021) %>% 
  distinct(waveid) %>% 
  mutate(withdrew_2020 = 1)

hilda <- hilda %>% 
  mutate(withdrew_2020 = ifelse(waveid %in% withdrew_2020$waveid, 1, 0),
         withdrew_2021 = ifelse(waveid %in% withdrew_2021$waveid, 1, 0))

## Working age indicator ---------------------------------------------------------

hilda <- hilda %>% 
  mutate(working_age = as.numeric(hgage >= 15 & hgage <= 64))

## Working age alternative indicator ---------------------------------------------------------

hilda <- hilda %>% 
  mutate(working_age_alt = as.numeric(hgage >= 25 & hgage <= 54))

# Check % of working age pop that withdrew -------

hilda %>% 
  filter(working_age == 1 & year == 2020) %>% 
  tabyl(withdrew_2020)

hilda %>% 
  filter(working_age == 1 & year == 2021) %>% 
  tabyl(withdrew_2021)


### Indicator for whether drew full amount or less than full amount ------
hilda <- hilda %>% 
  mutate(super_amount = case_when(
    oifcva > 0 & oifcva < 10000 ~ "Partial withdrawal",
    oifcva > 0 & oifcva == 10000 ~ "Full withdrawal"
  ))

super_amount <- hilda %>% 
  filter(year == 2020) %>% 
  distinct(waveid, super_amount) %>% 
  filter(!is.na(super_amount)) %>% 
  rename(super_amount_2020 = super_amount)

hilda <- hilda %>% 
  left_join(super_amount, by = "waveid")

## Indicator they withdrew in first round (prior to 30 June 2020) -----

# explanation: wave 20 questionaire specifically asked 
# 'Prior to 30 June 2020, did you withdraw superannuation 
# under the COVID-19 scheme for early release of super?'

withdrew_firstround <- hilda %>% 
  filter(year == 2020) %>% 
  filter(oifcvs == 1) %>% 
  distinct(waveid) %>% 
  mutate(withdrew_firstround = 1)

hilda <- hilda %>% 
  mutate(withdrew_firstround = ifelse(waveid %in% withdrew_firstround$waveid, 1, 0))

hilda <- hilda %>% 
  mutate(withdrew_firstround = case_when(
    withdrew_firstround == 0 ~ "Did not withdraw in first round",
    withdrew_firstround == 1 ~ "Withdrew in first round"
  ))

# Withdrew second round

withdrew_secondround <- hilda %>% 
  filter(year == 2021) %>% 
  filter(oifcvs == 1) %>% 
  distinct(waveid) %>% 
  mutate(withdrew_secondround = 1)

hilda <- hilda %>% 
  mutate(withdrew_secondround = ifelse(waveid %in% withdrew_secondround$waveid, 1, 0))

hilda <- hilda %>% 
  mutate(withdrew_secondround = case_when(
    withdrew_secondround == 0 ~ "Did not withdraw in second round",
    withdrew_secondround == 1 ~ "Withdrew in second round"
  ))

# Withdrew both rounds 

hilda <- hilda %>% 
  mutate(rounds_withdrawn = case_when(
    withdrew_firstround == "Withdrew in first round" & 
      withdrew_secondround == "Withdrew in second round" ~ "Both",
    withdrew_firstround == "Withdrew in first round" & 
      withdrew_secondround == "Did not withdraw in second round" ~ "First only",
    withdrew_firstround == "Did not withdraw in first round" & 
      withdrew_secondround == "Withdrew in second round" ~ "Second only",
    TRUE ~ "None"
  ))

## Indicator for workers (employed start of 2020) who had hours reduced/termination in 2020 ------------
reduced_hrs <- hilda %>% 
  filter(year %in% 2020) %>% 
  mutate(reduced_hrs = ifelse(cvdchr == 1 | cvrd == 1 | cvul == 1, 1, 0)) %>% 
  filter(reduced_hrs == 1) %>% 
  select(waveid, reduced_hrs)

hilda <- hilda %>% 
  mutate(reduced_hrs_2020 = ifelse(waveid %in% reduced_hrs$waveid, 1, 0))

## Indicator for workers (employed at start of 2020) unaffected in 2020 from covid policies -----

unaffected_hrs <- hilda %>% 
  filter(year %in% 2020) %>% 
  # bncnsws: 0 (for benefit recipients) and -1 (not asked) means "no" to jobseeker 
  # cvjkhav: 2 means no to jobkeeper for employed persons
  # Should be cvjkhav != 1, otherwise dropping lots of observations
  mutate(unaffected_hrs = ifelse(cvdchr == 2 & cvrd == 2 & cvul == 2 
              & (bncnws == 0 | bncnws == -1) & cvjkhav != 1, 1, 0)) %>% 
  filter(unaffected_hrs == 1) %>% 
  select(waveid, unaffected_hrs)

hilda <- hilda %>% 
  mutate(unaffected_2020 = ifelse(waveid %in% unaffected_hrs$waveid, 1, 0))

## Attach superannuation info

### 1. Retrieve 2018 superannuation ----------------------------------------------

hilda_2018 <- hilda %>% 
  filter(year == 2018,
         savaln2 > 0)

hilda_2018 <- hilda_2018 %>% 
  rename(savaln2_2018 = savaln2)

hilda_super_2018 <- hilda_2018 %>% 
  select(waveid, savaln2_2018)

### 2. Retrieve 2022 superannuation ---------------------------------------------

hilda_2022 <- hilda %>% 
  filter(year == 2022,
         savaln2 > 0)

hilda_2022 <- hilda_2022 %>% 
  rename(savaln2_2022 = savaln2)

hilda_super_2022 <- hilda_2022 %>% 
  select(waveid, savaln2_2022)

### 3. Attach 2018 and 2022 superannuation to HILDA ------------------------------

hilda <- hilda %>%
  left_join(hilda_super_2018, by = "waveid") %>%
  left_join(hilda_super_2022, by = "waveid")

## Retrieve and attach 2018 home ownership to HILDA -----------------------------

hilda_house_2018 <- hilda %>% 
  filter(year == 2018,
         rpevown > 0)  %>% 
  mutate(home_2018 = case_when(rpevown == 1 ~ 1,
                          rpevown == 2 ~ 0,
                          TRUE ~ NA_integer_)) %>% 
  select(waveid, home_2018)

hilda <- hilda %>%
  left_join(hilda_house_2018, by = "waveid")

## Casual worker indicator ---------------------------------------------------

hilda <- hilda %>% 
  mutate(casual = ifelse(jbcasab == 1, 1, 0))

## Indicator for those who had dependents in 2019 --------------------------

hilda$hhd0_4[hilda$hhd0_4 < 0] <- NA
hilda$hhd5_9[hilda$hhd5_9 < 0] <- NA
hilda$hhd1014[hilda$hhd1014 < 0] <- NA
hilda$hhd1524[hilda$hhd1524 < 0] <- NA

dependents <- hilda %>%
  filter(year == 2019) %>%
  mutate(
    children = ifelse(
      is.na(hhd0_4) & is.na(hhd5_9) & is.na(hhd1014) & is.na(hhd1524),
      NA, 
      ifelse(hhd0_4 > 0 | hhd5_9 > 0 | hhd1014 > 0 | hhd1524 > 0, 1, 0)
    )
  ) %>%
  select(waveid, children)

dependents <- dependents %>%
  filter(!is.na(children))

hilda <- hilda %>% #append back to original dataset
  left_join(dependents, by = "waveid")

## Indicator for those who had a partner in 2019 ----------------------------

hilda$mrcms[hilda$mrcms < 0] <- NA #drop undetermined values

partner_2019 <- hilda %>%
  filter(year == 2019) %>%
  mutate(partner = ifelse(is.na(mrcms), NA, ifelse(mrcms %in% c(1, 5), 1, 0))) %>%
  # 1 = registered marriage, 5 = living with someone in relationship
  select(waveid, partner)

partner_2019 <- partner_2019 %>%
  filter(!is.na(partner))

hilda <- hilda %>% #append back to original dataset
  left_join(partner_2019, by = "waveid")

# Impatience instrument --------------------------------

hilda$fisavep[hilda$fisavep <0] <- NA

impatience <- hilda %>% 
  filter(year == 2018) %>%  
  mutate(impatience = case_when( 
    fisavep %in% c(1) ~ 1,  
    fisavep %in% c(2, 3, 4, 5, 6) ~ 0,  
    TRUE ~ NA 
  )) %>%
  select(waveid, impatience)

hilda <-  hilda %>%
  left_join(impatience, by = "waveid") 

# Alternative impatience measures ------------------------------------

impatience_expand <- hilda %>% 
  filter(year == 2018) %>%  
  mutate(
    impatience_2 = case_when(
      fisavep %in% c(1, 2) ~ 1,
      fisavep %in% c(3, 4, 5, 6) ~ 0,
      fisavep < 0 ~ NA_integer_,
      TRUE ~ NA_integer_
    ),
    impatience_3 = case_when(
      fisavep %in% c(1, 2, 3) ~ 1,
      fisavep %in% c(4, 5, 6) ~ 0,
      fisavep < 0 ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(waveid, impatience_2, impatience_3)

hilda <-  hilda %>%
  left_join(impatience_expand, by = "waveid") 

# Consciousness variable ---------------------------------------------------

hilda$pnconsc[hilda$pnconsc < 0] <- NA 

# average across available waves
hilda_pnconsc <- hilda %>%
  filter(year <=2019) %>% 
  group_by(waveid) %>%
  summarise(
    consc_mean = mean(pnconsc, na.rm = TRUE))

hilda <- hilda %>%
  left_join(hilda_pnconsc, by = "waveid")

# Construct locus of control instrument ------------------------------------

hilda$lssecd[hilda$lssecd <0] <- NA
hilda$lsseci[hilda$lsseci <0] <- NA
hilda$lssefd[hilda$lssefd <0] <- NA
hilda$lssefh[hilda$lssefh <0] <- NA
hilda$lsselc[hilda$lsselc <0] <- NA
hilda$lssepa[hilda$lssepa <0] <- NA
hilda$lssesp[hilda$lssesp <0] <- NA

# Reverse the external-locus items, then average all
hilda <- hilda %>%
  mutate(
    # Convert to numeric
    across(c(lssecd, lsseci, lssefd, lssefh, lsselc, lssepa, lssesp), as.numeric),
    
    # Reverse external items
    lsseci_r = 8 - lsseci, #Personal control: Cannot change important things in life [External locus]
    lssefh_r = 8 - lssefh, #Personal control: Feel helpless	[External locus]
    lsselc_r = 8 - lsselc, #Personal control: Little control	[External locus]
    lssepa_r = 8 - lssepa, #Personal control: Pushed around	[External locus]
    lssesp_r = 8 - lssesp, #Personal control: No way to solve problems	 [External locus]
    
    # Compute mean locus of control index (higher = more internal)
    loc = rowMeans(
      cbind(lssecd, lssefd, lsseci_r, lssefh_r, lsselc_r, lssepa_r, lssesp_r),
      na.rm = TRUE
    )
  )

# average across available waves
hilda_loc <- hilda %>%
  filter(year <=2019) %>% 
  group_by(waveid) %>%
  summarise(
    loc_mean = mean(loc, na.rm = TRUE))

hilda <- hilda %>%
  left_join(hilda_loc, by = "waveid")

# construct above and below median

loc_median_val <- hilda %>% 
  filter(year == 2019) %>% 
  pull(loc_mean) %>% 
  median(na.rm = TRUE)

hilda_loc_median <- hilda %>% 
    filter(year == 2019) %>% 
    mutate(loc_median = ifelse(loc_mean >= loc_median_val, "Above", "Below")) %>% 
  select(waveid, loc_median)

hilda <- hilda %>%
  left_join(hilda_loc_median, by = "waveid")

# Save dataframe -----

saveRDS(hilda, "data/hilda.rds")
