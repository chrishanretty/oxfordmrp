library(tidyverse)
library(rio)
library(mgcv)
library(nnet)
ses16_locn <- "~/Dropbox/_2021_SES/_Data/_SES_16_Panel.dta"

ses16 <- rio::import(ses16_locn)

### Need to get the data into the same shape as the eventual LFS data
###
### (1) Age
### (2) Gender
### (3) Highest qualifications
### (4) Ethnicity
### (5) SOC2010
### (6) Private/public sector employment


### (0) Vote choice
### Levels include
       ## Con didnotvote      Green ineligible        Lab     LibDem      Other 
       ##  73         73         73         73         73         73         73 
       ## SNP 
##  73

ses16 <- ses16 %>%
    mutate(vi = characterize(w2vb15),
           vi = dplyr::recode(vi,
                              "Conservative" = "Con",
                              "Green Party" = "Green",
                              "Labour" = "Lab",
                              "Liberal Democrat" = "LibDem",
                              "Other party or independent candidate" = "Other",
                              "rise" = "Other",
                              "Scottish National Party" = "SNP",
                              "ukip" = "Other",
                              .default = "didnotvote"))

### (1) Age
### We need to add 4 on to this variable
### to match the 2020 population data we have
ses16 <- ses16 %>%
    mutate(age = w2age + 4)

### (2) Gender
ses16 <- ses16 %>%
    mutate(gender = characterize(w2profile_gender))

### (3) Highest qualifications
ses16 <- ses16 %>%
    mutate(quals = characterize(w2profile_education_level))

ses16 <- ses16 %>%
    mutate(quals = case_when(quals %in% c("University or CNAA higher degree (e.g. M.Sc, Ph.D)",
                                          "University or CNAA first degree (e.g. BA, B.Sc, B.Ed)",
                                          "University diploma",
                                          "Teaching qualification (not degree)",
                                          "Nursing qualification (e.g. SEN, SRN, SCM, RGN)") ~ "NVQ4+",
                             quals %in% c("onc",
                                          "GCE A level or Higher Certificate",
                                          "Scottish Higher Certificate") ~ "NVQ3 only",
                             quals %in% c("Recognised trade apprenticeship completed") ~ "Trade Apprenticeships",
                             quals %in% c("Youth training certificate/skillseekers",
                                          "City & Guilds certificate - advanced",
                                          "Scottish Ordinary/ Lower Certificate",
                                          "CSE grade 1, GCE O level, GCSE, School Certificate") ~
                                 "NVQ2 only",
                             quals %in% c("Clerical and commercial",
                                          "City & Guilds certificate",
                                          "CSE grades 2-5") ~ "NVQ1 only",
                             quals %in% c("No formal qualifications",
                                          "Don't know",
                                          "Prefer not to say") ~ "no qualifications (NVQ)",
                             quals %in% c("Other technical, professional or higher qualification") ~
                                 "other qualifications (NVQ)"))

### (4) Ethnicity
ses16 <- ses16 %>%
    mutate(ethn = characterize(w2profile_ethnicity),
           ethn = dplyr::recode(ethn,
                                "White British" = "white",
                                "Skipped"= NA_character_,
                                "Prefer not to say" = NA_character_,
                                .default = "ethnic minority"))

### (5) SOC2010
ses16 <- ses16 %>%
    mutate(soc2010 = characterize(w2profile_work_type),
           soc2010 = dplyr::recode(soc2010,
                                   "Manager or Senior Administrator / intermediate managerial / professional (e.g. c" = "SOC2010_1",
                                   "Professional or higher technical work / higher managerial - work that requires a" = "SOC2010_2",
                                   "Semi-Skilled or Unskilled Manual Work (e.g. machine operator, assembler, postman" = "SOC2010_9",
                                   "Skilled Manual Work (e.g. plumber, electrician, fitter)" = "SOC2010_5",
                                   "Foreman or Supervisor of Other Workers (e.g. building site foreman, supervisor o" = "SOC2010_5",
                                   "Sales or Services (e.g. commercial traveller, shop assistant, nursery nurse, car" = "SOC2010_7",
                                   "Clerical/junior managerial/professional/administrator (e.g. office worker, stude" = "SOC2010_4",
                                   "Other" = NA_character_,
                                   "Have never worked" = "SOC2010_NA"),
           soc2010 = ifelse(characterize(w2profile_work_stat) %in%
                            c("Full time student",
                              "Not working",
                              "Other",
                              "Retired",
                              "Unemployed"),
                            "SOC2010_NA",
                            soc2010))
                              
                                   
### (6) Private/public sector
ses16 <- ses16 %>%
    mutate(privpub = characterize(w1ba10),
           privpub = dplyr::recode(privpub,
                                   "Charity/voluntary sector (e.g. charitable companies, churches, trade unions)" = "Private",
                                   "Nationalised industry or public corporation (e.g. post office, BBC)" = "Public",
                                   "Never worked" = "Neither",
                                   "Other" = "Neither",
                                   "Other public sector employer (e.g. central government, Civil Service, LEA, NHS," = "Public",
                                   "Private sector firm or company (e.g. limited company, PLC, small business)" = "Private"))
           

### Just select variables we're interested in
###
ses16 <- ses16 %>%
    dplyr::select(vi, gender, quals, age, ethn, soc2010, privpub,
                  w2fullw8) %>%
    filter(complete.cases(.))

### Cut age
age_breaks <- seq(from = 14,
                  to = 79,
                  by = 5)
age_breaks <- c(age_breaks, Inf)

age_labels <- paste(lag(age_breaks[-1]) + 1, age_breaks[-1], sep = "-")
age_labels[1] <- "16-19"
age_labels <- age_labels



mod_0 <- multinom(vi ~ age + gender + quals + ethn +
                    soc2010 + privpub,
                data = ses16,
                MaxNWts = 5000,
                maxit = 500,
                hessian = TRUE)

mod <- multinom(vi ~ poly(age, 2) + gender + quals + ethn +
                    privpub,
                data = ses16,
                MaxNWts = 5000,
                maxit = 500,
                weights = ses16$w2fullw8,
                hessian = TRUE)


## library(msgl)

## ses16 <- ses16 %>%
##     mutate(age_cut = cut(age, age_breaks, age_labels)) 

## x <- ses16 %>%
##     dplyr::select(-vi, -age) %>% 
##     sapply(function(x)as.numeric(factor(x)))

## y <- ses16 %>%
##     pull(vi)

## f <- cv(x, y, lambda = 0.1)

## f2 <- msgl::fit(x, y, lambda = 0.34)
