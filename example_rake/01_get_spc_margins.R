### Purpose of this script:
###
### To get marginal distributions at SPC level of the following variables
###
### (1) Age
### (2) Gender
### (3) Highest qualifications
### (4) Ethnicity
### (5) SOC2010
### (6) Private/public sector employment
###
### plus
### (7) 2016 SP regional vote
###
### Script output is a data-frame with four columns:
###  - geogcode
###  - variable
###  - category
###  - count
###
### and the output lives in working/targets.rds
###

### Create function to fill in missings in categories
fillin <- function(df) {
    df <- df %>%
        group_by(geogcode) %>%
        mutate(to_allocate = 1 - sum(prop, na.rm = TRUE),
               to_allocate = ifelse(is.na(prop),
                                    to_allocate,
                                    0))

### Add on the general proportions
    aux <- df %>%
        group_by(category) %>%
        dplyr::summarize(auxprop = mean(prop, na.rm = TRUE))

    df <- merge(df, aux,
                by = "category")

    df <- df %>%
        group_by(geogcode) %>%
        mutate(extra = to_allocate * ((to_allocate > 0) * auxprop) /
                   sum((to_allocate > 0)* auxprop, na.rm = TRUE),
               extra = coalesce(extra, 0))

    df <- df %>%
        mutate(prop = coalesce(prop, 0) + extra)

    df %>%
        dplyr::select(geogcode, variable, category, prop)
}


### Load libraries
###

library(tidyverse)
library(rio)

### #########################################################
### (1) AGE
### #########################################################

age_breaks <- seq(from = 14,
                  to = 79,
                  by = 5)
age_breaks <- c(age_breaks, Inf)

age_labels <- paste(lag(age_breaks[-1]) + 1, age_breaks[-1], sep = "-")
age_labels[1] <- "16-19"
age_labels <- age_labels

### Read in the small area estimates originally found at
### https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/spc-population-estimates
###

age <- read_csv("data/spc-19-tabs_2019.csv", skip = 3) %>%
    dplyr::select(geogcode = `Area code`,
                  Sex,
                  `16`:`90+`)

### Only areas with codes, only people rather than gender-specific
age <- age %>%
    filter(grepl("S16", geogcode)) %>%
    filter(Sex == "Persons") %>%
    dplyr::select(-Sex)

### Coerce all but geogcode to numeric
age <- age %>%
    mutate_at(vars(-geogcode), as.numeric)

### Pivot longer and aggregate
age <- age %>%
    pivot_longer(cols = `16`:`90+`,
                 names_to = "age",
                 values_to = "count")

age <- age %>% 
    mutate(age_num = as.numeric(sub("[^0-9]", "", age)),
           age_cut = cut(age_num,
                         breaks = age_breaks,
                         labels = age_labels))

age <- age %>%
    group_by(geogcode, age_cut) %>%
    dplyr::summarize(count = sum(count, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(variable = "age") %>%
    dplyr::select(geogcode,
                  variable,
                  category = age_cut,
                  count)


### #########################################################
### (2) GENDER
### #########################################################

### Read in the small area estimates originally found at
### https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/spc-population-estimates
###

gender <- read_csv("data/spc-19-tabs_2019.csv", skip = 3) %>%
    dplyr::select(geogcode = `Area code`,
                  gender = Sex,
                  count = `All Ages`)

### Only areas with codes, not persons but rather gender-specific
gender <- gender %>%
    filter(grepl("S16", geogcode)) %>%
    filter(gender != "Persons") %>%
    mutate(variable = "gender") %>% 
    dplyr::select(geogcode,
                  variable,
                  category = gender,
                  count) %>%
    mutate(count = as.numeric(count),
           category = sub("s$", "", category))
                  

### #########################################################
### (3) EDUCATION
### #########################################################

### (3a) EDUCATION for the 16 to 64 group
### 
### (no quals, other quals., NVQ1 only, NVQ2 only, apprenticeship,
### NVQ3 only, NVQ4 or greater, 65+). Source: NomisWeb, APS, Dec 2019 data
###
### Note that these are by gender

quals_url <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1920991233...1920991305&date=latestMINUS3&variable=291,292,300,301,309,310,318,319,327,328,336,337,345,346&measures=20599,21001,21002,21003"

quals <- read_csv(quals_url)

quals <- quals %>%
    filter(MEASURES_NAME == "Numerator") %>% 
    dplyr::select(geogname = GEOGRAPHY_NAME,
                  geogcode = GEOGRAPHY_CODE,
                  category = VARIABLE_NAME,
                  count = OBS_VALUE) %>%
    mutate(category = sub("% with ", "", category),
           category = sub(" aged 16-64", "", category),
           category = sub(" - males", "", category),
           category = sub(" - females", "", category),
           variable = "quals",
           count = coalesce(count, 0)) %>%
    group_by(geogname, geogcode, category, variable) %>%
    dplyr::summarize(count = sum(count), .groups = "drop")

quals <- quals %>%
    mutate(geogcode = dplyr::recode(geogcode,
                                    "S16000120" = "S16000147",
                                    "S16000145" = "S16000148"))

### Transform these into proportions and multiply by the sum total of
### the 16 to 64 population

quals <- quals %>%
    group_by(geogcode) %>%
    mutate(prop = count / sum(count)) %>%
    dplyr::select(geogcode,
                  variable,
                  category, prop)

quals <- merge(quals,
               age %>%
               filter(category %in% age_labels[1:10]) %>%
               group_by(geogcode) %>%
               dplyr::summarize(tot = sum(count), .groups = "drop"),
               by = "geogcode",
               all = TRUE)

quals <- quals %>%
    mutate(count = round(prop * tot))

### (3a) EDUCATION for the 65+ group
quals2_url <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1920991233...1920991305&date=latestMINUS59&variable=290,299,308,317,326,335,344&measures=20599,21001,21002,21003"


quals2 <- read_csv(quals2_url)

quals2 <- quals2 %>%
    filter(MEASURES_NAME == "Numerator") %>% 
    dplyr::select(geogname = GEOGRAPHY_NAME,
                  geogcode = GEOGRAPHY_CODE,
                  category = VARIABLE_NAME,
                  count = OBS_VALUE) %>%
    mutate(category = sub("% with ", "", category),
           category = sub(" - aged 16-64", "", category),
           variable = "quals",
           count = coalesce(count, 0)) %>%
    group_by(geogname, geogcode, category, variable) %>%
    dplyr::summarize(count = sum(count), .groups = "drop")

quals2 <- quals2 %>%
    mutate(geogcode = dplyr::recode(geogcode,
                                    "S16000120" = "S16000147",
                                    "S16000145" = "S16000148"))

### Transform these into proportions and multiply by the sum total of
### the 16 to 64 population

quals2 <- quals2 %>%
    group_by(geogcode) %>%
    mutate(prop = count / sum(count)) %>%
    dplyr::select(geogcode,
                  variable,
                  category, prop)

quals2 <- merge(quals2,
               age %>%
               filter(category %in% age_labels[11:14]) %>%
               group_by(geogcode) %>%
               dplyr::summarize(tot = sum(count), .groups = "drop"),
               by = "geogcode",
               all = TRUE)

quals2 <- quals2 %>%
    mutate(count = round(prop * tot))

### Combine the two quals data frames and aggregate
quals <- merge(quals, quals2,
               all = TRUE) %>%
    group_by(geogcode, variable, category) %>%
    dplyr::summarize(count = sum(count, na.rm = TRUE),
              .groups = "drop")

### #########################################################
### (4) Ethnicity
### #########################################################

### Ethnicity (white/non-white) Source: NomisWeb, APS, Sep. 2020 data
###
### We're just going to get percentages and make them tally w/ the population data

ethn_url <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1920991233...1920991305&date=latest&variable=557,563&measures=20599,21001,21002,21003"

ethn <- read_csv(ethn_url)

ethn <- ethn %>%
    filter(MEASURES_NAME == "Variable") %>% 
    dplyr::select(geogname = GEOGRAPHY_NAME,
                  geogcode = GEOGRAPHY_CODE,
                  category = VARIABLE_NAME,
                  prop = OBS_VALUE) %>%
    mutate(prop = prop / 100,
           category = sub("% of ", "", category, fixed = TRUE),
           category = sub(" - aged 16+", "", category, fixed = TRUE),
           variable = "ethnicity")

### For some reason, Glasgow Provan and Strathkelvin and Bearsden have old codes
ethn <- ethn %>%
    mutate(geogcode = dplyr::recode(geogcode,
                                    "S16000120" = "S16000147",
                                    "S16000145" = "S16000148"))

ethn <- fillin(ethn)

### Merge this with total population figures to get counts
ethn <- merge(ethn,
              age %>%
              group_by(geogcode) %>%
              dplyr::summarize(tot = sum(count), .groups = "drop"),
              all = TRUE,
              by = "geogcode")

ethn <- ethn %>%
    mutate(count = round(prop * tot)) %>%
    dplyr::select(geogcode, variable, category, count)


### #########################################################
### (5) SOC2010
### #########################################################

### SOC2010 (nine category), plus employment rate for the 16+
### Source: NomisWeb, APS, Sep. 2020 data
###
### We're just going to get percentages and make them tally w/ the population data


soc_url <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1920991233...1920991305&date=latest&variable=44,1532...1540&measures=20599,21001,21002,21003"

soc_base <- read_csv(soc_url)

### We're going to pull out the employment rate
emp_rate <- soc_base %>%
    filter(VARIABLE_NAME == "Employment rate - aged 16+") %>%
    filter(MEASURES_NAME == "Variable") %>% 
    dplyr::select(geogcode = GEOGRAPHY_CODE,
                  category = VARIABLE_NAME,
                  prop = OBS_VALUE) %>%
    mutate(emp = prop / 100) %>%
    dplyr::select(geogcode, emp)

### Let's the the soc proportions
soc <- soc_base %>%
    filter(VARIABLE_NAME != "Employment rate - aged 16+") %>%
    filter(MEASURES_NAME == "Variable") %>% 
    dplyr::select(geogcode = GEOGRAPHY_CODE,
                  category = VARIABLE_NAME,
                  prop = OBS_VALUE) %>%
    mutate(category = sub("% all in employment who are - ", "", category, fixed = TRUE),
           category = sub(":.*", "", category),
           category = paste0("SOC2010_", category),
           variable = "soc2010",
           prop = prop / 100)    

emp_rate <- emp_rate %>%
    mutate(geogcode = dplyr::recode(geogcode,
                                    "S16000120" = "S16000147",
                                    "S16000145" = "S16000148"))

soc <- soc %>%
    mutate(geogcode = dplyr::recode(geogcode,
                                    "S16000120" = "S16000147",
                                    "S16000145" = "S16000148"))

### Fillin the soc proportiosn at this stage
soc <- fillin(soc)

### Merge the SOC proportions with the emp rate and multiply
soc <- merge(soc, emp_rate,
             by = "geogcode",
             all = TRUE) %>%
    mutate(prop = prop * emp) %>%
    dplyr::select(-emp)

### Create a fill-up category which is equal to one minus emp
fillup <- emp_rate %>%
    mutate(prop = 1 - emp,
           variable = "soc2010",
           category = "SOC2010_NA") %>%
    dplyr::select(-emp)

soc <- merge(soc, fillup, all = TRUE)

### Now let's get the counts by merging with the age categories
soc <- merge(soc,
             age %>%
             group_by(geogcode) %>%
             dplyr::summarize(tot = sum(count), .groups = "drop"),
             all = TRUE,
             by = "geogcode")

soc <- soc %>%
    mutate(count = round(prop * tot)) %>%
    dplyr::select(geogcode, variable, category, count)

### #########################################################
### (6) Private/public sector employment
### #########################################################

privpub_url <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1920991233...1920991305&date=latest&variable=44,1463,1464&measures=20599,21001,21002,21003"

privpub_base <- read_csv(privpub_url)

### We're going to pull out the employment rate
emp_rate <- privpub_base %>%
    filter(VARIABLE_NAME == "Employment rate - aged 16+") %>%
    filter(MEASURES_NAME == "Variable") %>% 
    dplyr::select(geogcode = GEOGRAPHY_CODE,
                  category = VARIABLE_NAME,
                  prop = OBS_VALUE) %>%
    mutate(emp = prop / 100) %>%
    dplyr::select(geogcode, emp)

### Let's the the privpub proportions
privpub <- privpub_base %>%
    filter(VARIABLE_NAME != "Employment rate - aged 16+") %>%
    filter(MEASURES_NAME == "Variable") %>% 
    dplyr::select(geogcode = GEOGRAPHY_CODE,
                  category = VARIABLE_NAME,
                  prop = OBS_VALUE) %>%
    mutate(category = sub("all persons employed in", "", category, fixed = TRUE),
           category = sub(" as.*", "", category),
           variable = "privpub",
           prop = prop / 100)    


emp_rate <- emp_rate %>%
    mutate(geogcode = dplyr::recode(geogcode,
                                    "S16000120" = "S16000147",
                                    "S16000145" = "S16000148"))

privpub <- privpub %>%
    mutate(geogcode = dplyr::recode(geogcode,
                                    "S16000120" = "S16000147",
                                    "S16000145" = "S16000148"))

### Merge the PRIVPUB proportions with the emp rate and multiply
privpub <- merge(privpub, emp_rate,
             by = "geogcode",
             all = TRUE) %>%
    mutate(prop = prop * emp) %>%
    dplyr::select(-emp)

### Create a fill-up category which is equal to one minus emp
fillup <- emp_rate %>%
    mutate(prop = 1 - emp,
           variable = "privpub",
           category = "Not employed") %>%
    dplyr::select(-emp)

privpub <- merge(privpub, fillup, all = TRUE)

### Now let's get the counts by merging with the age categories
privpub <- merge(privpub,
             age %>%
             group_by(geogcode) %>%
             dplyr::summarize(tot = sum(count), .groups = "drop"),
             all = TRUE,
             by = "geogcode")

privpub <- privpub %>%
    mutate(count = round(prop * tot),
           category = dplyr::recode(category,
                                    " private sector" = "Private",
                                    " public sector" = "Public",
                                    "Not employed" = "Neither")) %>%
    dplyr::select(geogcode, variable, category, count)


### #########################################################
### (7) SPC vote
### #########################################################

sp16_url <- "https://www.electoralcommission.org.uk/sites/default/files/excel_doc/Electoral-Data-Results-May-2016-Scottish-Parliament-elections-.xls"

sp16 <- rio::import(sp16_url, which = 5, skip = 1) %>%
    dplyr::select(geogcode = `ONS Code`,
                  Con = CON...10,
                  Lab = LAB...11,
                  LibDem = LIB...12,
                  SNP = SNP...13,
                  Green = SGRN...14,
                  Other1 = IND...16,
                  Other2 = SOL,
                  Other3 = RISE,
                  Other4 = CHP,
                  Other5 = WEP,
                  Other6 = SLBR,
                  Other7 = ABBUP,
                  Other8 = COMP,
                  Other9 = NF,
                  Other10 = ANWP,
                  Other11 = Others...26) %>%
    filter(!is.na(geogcode)) %>% 
    pivot_longer(cols = Con:Other11) %>%
    mutate(category = sub("[0-9]+", "", name),
           variable = "past_vote") %>%
    group_by(geogcode, variable, category) %>%
    dplyr::summarize(count = sum(value, na.rm = TRUE))

### Change some codes
sp16 <- sp16 %>%
    mutate(geogcode = dplyr::recode(geogcode,
                                    "S16000120" = "S16000147",
                                    "S16000145" = "S16000148"))

### We need some top-up category, composed of 2021 people who *could
### not vote* in 2016 and people who *did not vote*.
###
### The 2021 people who could not vote are everyone in the 16 to 19
### category, and 1/5th of the 20:24 age group.
ineligible <- age %>%
    filter(category %in% c("16-20", "20-24")) %>%
    mutate(count = ifelse(category == "20-24",
                          count / 5,
                          count),
           variable = "past_vote",
           category = "ineligible") %>%
    group_by(geogcode, variable, category) %>%
    dplyr::summarize(count = sum(count, na.rm = TRUE),
              .groups = "drop")

### Add this on
sp16 <- merge(sp16, ineligible, all = TRUE)

### The "did not vote" category is going to bring the vote totals up
### to observed population cout
tot <- age %>%
    group_by(geogcode) %>%
    dplyr::summarize(total = sum(count))

tot <- merge(tot,
             sp16 %>%
             group_by(geogcode) %>%
             dplyr::summarize(voters = sum(count),
                       .groups = "drop"),
             all = TRUE) %>%
    mutate(dnv = total - voters)

tot <- tot %>%
    mutate(variable = "past_vote",
           count = dnv,
           category = "didnotvote") %>%
    dplyr::select(geogcode, variable, category, count)

sp16 <- merge(sp16, tot, all = TRUE) %>%
    mutate(count = round(count))

### #########################################################
### Combine these
### #########################################################

sum(is.na(age$count))
sum(is.na(gender$count))
sum(is.na(quals$count))
sum(is.na(ethn$count))
sum(is.na(soc$count))
sum(is.na(privpub$count))
sum(is.na(sp16$count))

###

tgts <- bind_rows(list(age,
                       gender,
                       quals,
                       ethn,
                       soc,
                       privpub,
                       sp16))

tgts <- tgts %>%
    group_by(geogcode, variable) %>%
    mutate(prop = count / sum(count))

saveRDS(tgts, file = "working/targets.rds")
