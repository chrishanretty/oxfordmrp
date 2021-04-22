library(tidyverse)
library(rio)
       
### Read in the Dec. 2020 LFS/APS data
aps <- rio::import("data/apsp_jd20_eul_pwta18.dta")

### Select the variables we're interested in
### (1) Age
### (2) Gender
### (3) Highest qualifications
### (4) Ethnicity
### (5) SOC2010
### (6) Private/public sector employment

aps <- aps %>%
    mutate_at(vars(COUNTRY,
                   SEX,
                   SNGDEGB,
                   HIQUAL15,
                   ETH11S,
                   SC10MMJ,
                   PUBLICR), characterize) %>% 
    filter(COUNTRY %in% c("Scotland", "Scotland North of Caledonian Canal")) %>% 
    dplyr::select(AGE, SEX,
                  SNGDEGB,
                  HIQUAL15,
                  ETH11S,
                  SC10MMJ,
                  PUBLICR)
        

### (1) Age
### Age is okay at present -- we'll use a continuous version for the imputation
### and categorize later.

aps <- aps %>%
    mutate(age = AGE)

### (2) Gender
aps <- aps %>%
    mutate(gender = SEX)

### (3) Highest qualifications
### If they have a degree, they're Level +4
### Otherwise, go through the qualifications
aps <- aps %>%
    mutate(quals = dplyr::recode(HIQUAL15,
                                 "A-level or equivalent" = "NVQ3 only",
                                 "Access qualifications" = "NVQ3 only",
                                 "Advanced Welsh Baccalaureate" = "NVQ3 only",
                                 "Advanced/Progression (14-19) Diploma" = "NVQ3 only",
                                 "AS-level or equivalent" = "NVQ3 only",
                                 "Basic skills qualification" = "NVQ1 only",
                                 "BTEC/SCOTVEC First or General certificate" = "NVQ2 only",
                                 "BTEC/SCOTVEC First or General diploma etc" = "NVQ2 only",
                                 "City & Guilds Advanced Craft/Part 1" = "NVQ3 only",
                                 "City & Guilds Craft/Part 2" = "NVQ3 only",
                                 "City & Guilds foundation/Part 1" = "NVQ2 only",
                                 "CSE below grade 1, GCSE below grade C" = "NVQ1 only",
                                 "Diploma in higher education" = "NVQ4+",
                                 "Does not apply" = "other qualifications (NVQ)",
                                 "Don?t know" = "no qualifications (NVQ)",
                                 "Entry level  Award" = "NVQ1 only",
                                 "Entry level  Certificate" = "NVQ1 only",
                                 "Entry level  Diploma" = "NVQ1 only",
                                 "Entry level qualification" = "NVQ1 only",
                                 "First degree/foundation degree" = "NVQ4+",
                                 "Foundation (14-19) Diploma" = "NVQ3 only",
                                 "GNVQ/GSVQ advanced" = "NVQ3 only",
                                 "GNVQ/GSVQ foundation level" = "NVQ1 only",
                                 "GNVQ/GSVQ intermediate" = "NVQ2 only",
                                 "Higher (14-19) Diploma" = "NVQ3 only",
                                 "Higher degree" = "NVQ4+",
                                 "HNC/HND/BTEC higher etc" = "NVQ4+",
                                 "Intermediate Welsh Baccalaureate" = "NVQ2 only",
                                 "International Baccalaureate" = "NVQ3 only",
                                 "Key skills qualification" = "NVQ1 only",
                                 "Level 1  Award" = "NVQ1 only",
                                 "Level 1  Certificate" = "NVQ1 only",
                                 "Level 1  Diploma" = "NVQ1 only",
                                 "Level 2  Award" = "NVQ2 only",
                                 "Level 2  Certificate" = "NVQ2 only",
                                 "Level 2  Diploma" = "NVQ2 only",
                                 "Level 3 Award" = "NVQ3 only",
                                 "Level 3 Certificate" = "NVQ3 only",
                                 "Level 3 Diploma" = "NVQ3 only",
                                 "Level 4 Award" = "NVQ4+",
                                 "Level 4 Certificate" = "NVQ4+",
                                 "Level 4 Diploma" = "NVQ4+",
                                 "Level 5 Award" = "NVQ4+",
                                 "Level 5 Certificate" = "NVQ4+",
                                 "Level 5 Diploma" = "NVQ4+",
                                 "Level 6 Award" = "NVQ4+",
                                 "Level 6 Certificate" = "NVQ4+",
                                 "Level 6 Diploma" = "NVQ4+",
                                 "Level 7 Award" = "NVQ4+",
                                 "Level 7 Certificate" = "NVQ4+",
                                 "Level 7 Diploma" = "NVQ4+",
                                 "Level 8 Award" = "NVQ4+",
                                 "Level 8 Certificate" = "NVQ4+",
                                 "Level 8 Diploma" = "NVQ4+",
                                 "No answer" = "no qualifications (NVQ)",
                                 "No qualifications" = "no qualifications (NVQ)",
                                 "Nursing etc" = "NVQ4+",
                                 "NVQ level 1 or equivalent" = "NVQ1 only",
                                 "NVQ level 2 or equivalent" = "NVQ2 only",
                                 "NVQ level 3" = "NVQ3 only",
                                 "NVQ level 4" = "NVQ4+",
                                 "NVQ level 5" = "NVQ4+",
                                 "O-level, GCSE grade A*-C or equivalent" = "NVQ2 only",
                                 "OND/ONC/BTEC/SCOTVEC National etc" = "NVQ3 only",
                                 "Other degree" = "NVQ4+",
                                 "Other higher education below degree" = "NVQ4+",
                                 "Other qualification" = "other qualifications (NVQ)",
                                 "RSA advanced diploma" = "NVQ4+",
                                 "RSA diploma" = "NVQ3 only",
                                 "RSA higher diploma" = "NVQ4+",
                                 "RSA other" = "NVQ1 only",
                                 "SCE higher or equivalent" = "NVQ3 only",
                                 "Scottish 6 year certificate/CSYS" = "NVQ3 only",
                                 "Scottish Baccalaureate" = "NVQ3 only",
                                 "Scottish National below level 3" = "NVQ2 only",
                                 "Scottish National Level 3" = "NVQ1 only",
                                 "Scottish National Level 4" = "NVQ1 only",
                                 "Scottish National Level 5" = "NVQ2 only",
                                 "SCOTVEC modules" = "NVQ1 only",
                                 "Teaching foundation stage" = "NVQ4+",
                                 "Teaching further education" = "NVQ4+",
                                 "Teaching level not stated" = "NVQ4+",
                                 "Teaching primary education" = "NVQ4+",
                                 "Teaching secondary education" = "NVQ4+",
                                 "Trade apprenticeship" = "Trade Apprenticeships",
                                 "YT/YTP certificate" = "NVQ2 only"))

### (4) Ethnicity
aps <- aps %>%
    mutate(ethn = ETH11S,
           ethn = dplyr::recode(ethn,
                                "White" = "white",
                                "No answer" = NA_character_,
                                .default = "ethnic minority"))


### (5) SOC2010
aps <- aps %>%
    mutate(soc2010 = as.numeric(factor(SC10MMJ)),
           soc2010 = paste0("SOC2010_", soc2010),
           soc2010 = dplyr::recode(soc2010,
                                   "SOC2010_10" = "SOC2010_NA",
                                   "SOC2010_11" = "SOC2010_NA"))

### (6) Private/public/neither
aps <- aps %>%
    mutate(privpub = dplyr::recode(PUBLICR,
                                   "Private" = "Private",
                                   "Public" = "Public",
                                   "Does not apply" = "Neither",
                                   "No answer" = NA_character_))

### Restrict to 
### Now make predictions based on the previous model
for (i in c("gender", "quals", "ethn", "privpub")) {
    print(i)
    a1 <- ses16[,i]
    a2 <- aps[,i]
    stopifnot(length(a2)> 0)
    stopifnot(sort(unique(a1)) == sort(unique(a2)))
}

## Generate predictions
nrow(aps)
aps <- aps %>%
    filter(complete.cases(.))
nrow(aps)

### Multiply it many times
aps <- replicate(50, aps, simplify = FALSE)
aps <- bind_rows(aps)

aps_preds <- predict(mod_2, newdata = aps, type = "prob")

aps$past_vote <- apply(aps_preds, 1, function(x)sample(names(x), size = 1, prob = x))

aps$past_vote[which(aps$age < 20)] <- "ineligible"

### Group age
age_breaks <- seq(from = 14,
                  to = 79,
                  by = 5)
age_breaks <- c(age_breaks, Inf)

age_labels <- paste(lag(age_breaks[-1]) + 1, age_breaks[-1], sep = "-")
age_labels[1] <- "16-19"
age_labels <- age_labels

aps$age <- cut(aps$age, age_breaks, age_labels)

aps %>%
    distinct(age, gender, quals, ethn, soc2010, privpub) %>%
    nrow()

aps <- aps %>%
    filter(!is.na(age))

## ### Group this now
aps <- aps %>%
    group_by(past_vote, age, gender, quals, ethn, soc2010, privpub) %>%
    dplyr::summarize(count = n(), .groups = "drop") %>%
    mutate(w8 = count / sum(count))

names(aps) <- sub("ethn", "ethnicity", names(aps))

saveRDS(aps, file = "working/joint.rds")

