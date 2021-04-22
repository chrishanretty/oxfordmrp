### Load libraries
library(tidyverse)
library(brms)
library(rio)

has_labels <- function(x)!is.null(attr(x, "labels"))

## Load in the post-strat frame
ps <- readRDS("data/psw_2016.rds")

## Load in the auxiliary data
aux <- read.csv("data/BES-2015-General-Election-results-file-v2.21.csv") %>%
    select(ONSConstID, Winner = Winner15, Con15, Lab15, LD15, UKIP15, 
           Turnout15,
           OldWinner = Winner10, Con10, Lab10, LD10, UKIP10,
           c11PopulationDensity, c11Male,
           c11EthnicityWhiteBritish, 
           c11QualLevel4) %>%
    mutate_at(vars(ends_with("10")), coalesce, 0.0) %>% 
    mutate_at(vars(ends_with("15")), coalesce, 0.0)

## Load in the individual level data (BES 2015 post-election wave) and
## restrict to the variables we need
ind <- rio::import("data/BES2017_W13_v1.5.dta") %>%
    select(onscode,
           profile_past_vote_2015, profile_turnout_2015,
           profile_eurefvote,
           gender,
           education,
           age,
           starts_with("lr"),
           starts_with("al"),
           prPreference,
           leftRight) %>%
    filter(onscode != "") %>% 
    mutate_at(vars(profile_past_vote_2015,
                profile_turnout_2015,
                profile_eurefvote,
                gender,
                education), factorize)

### Recode to match PS variables
### (1) GE2016
ind <- ind %>%
    mutate(GE2015 = recode(profile_past_vote_2015,
                           "Conservative" = "Con15",
                           "Labour" = "Lab15",
                           "Liberal Democrat" = "LD15",
                           "United Kingdom Independence Party (UKIP)" = "UKIP15",
                           "Scottish National Party (SNP)" = "SNP15",
                           "Plaid Cymru" = "PC15",
                           "Green Party" = "Green15",
                           "Don't know" = "DidNotVote15",
                           "Other" = "Other15",
                           "British National Party (BNP)" = "Other15",
                           .default = "DidNotVote15"))

## (2) Leave/Remain
ind <- ind %>%
    mutate(Ref2016 = recode(profile_eurefvote,
                            "Leave the EU" = "Leave16",
                            "Stay/remain in the EU" = "Remain16",
                            "Don't know" = "DidNotVote16",
                            .default = "DidNotVote16"))

### (3) Gender
ind <- ind %>%
    mutate(Gender = recode(gender,
                           "Male" = "Men",
                           "Female" = "Women"))

### (4) Education
ind <- ind %>%
    mutate(Education = recode(education,
                              "City & Guilds certificate" = "Level1",
                              "City & Guilds certificate - advanced" = "Level2",
                              "City and Guild certificate" = "Level1",
                              "City and Guild certificate - advanced" = "Level2",
                              "Clerical and commercial" = "Level1",
                              "CSE grade 1, GCE O level, GCSE, School Certificate" = "Level2",
                              "CSE grades 2-5" = "Level1",
                              "Don't know" = "None",
                              "GCE A level or Higher Certificate" = "Level3",
                              "No formal qualifications" = "None",
                              "Nursing qualification (e.g. SEN, SRN, SCM, RGN)" = "Level4",
                              "Nursing qualification (eg SEN, SRN, SCM, RGN)" = "Level4",
                              "ONC" = "Level2",
                              "onc" = "Level2",
                              "Other technical, professional or higher qualification" = "Other",
                              "Prefer not to say" = "None",
                              "Recognised trade apprenticeship completed" = "Level2",
                              "Scottish Higher Certificate" = "Level3",
                              "Scottish Ordinary/ Lower Certificate" = "Level2",
                              "Teaching qualification (not degree)" = "Level4",
                              "University diploma" = "Level4",
                              "University or CNAA first degree (e.g. BA, B.Sc, B.Ed)" = "Level4",
                              "University or CNAA higher degree (e.g. M.Sc, Ph.D)" = "Level4",
                              "University or CNAA first degree (eg BA, B.Sc, B.Ed)" = "Level4",
                              "University or CNAA higher degree (eg M.Sc, Ph.D)" = "Level4",
                              "Youth training certificate/skillseekers" = "Level2"))

## (5) Age

age_breaks <- c(-Inf, 17, 19, 24,
                29, 39, 49, 64, 74, Inf)
age_labels <- c("17 or under",
                "18 to 19",
                "20 to 24",
                "25 to 29",
                "30 to 39",
                "40 to 49",
                "50 to 64",
                "65 to 74",
                "75+")

ind <- ind %>%
    mutate(Age = cut(age,
                     breaks = age_breaks,
                     labels = age_labels)) %>%
    filter(Age != "17 or under")

### Constituency identifier
ind <- ind %>%
    mutate(ONSConstID = onscode)

### What about the dependent variables?
ind <- ind %>%
    mutate(prefers_pr = case_when(prPreference == 2 ~ 1L,
                                  prPreference == 1 ~ 0L,
                                  TRUE ~ NA_integer_))

### Filter only to observations with have non-missing values
ind <- ind %>%
    filter(!is.na(prefers_pr))

### Create interactions
ind <- ind %>%
    mutate(ge_by_ref = interaction(GE2015, Ref2016),
           educ_by_age = interaction(Education, Age))

ps <- ps %>%
    mutate(ge_by_ref = interaction(GE2015, Ref2016),
           educ_by_age = interaction(Education, Age))

method <- "notmanual"

if (method == "manual") { 
### Scale auxiliary variables
    aux <- aux %>%
        mutate_if(is.numeric, scale)

    ind <- merge(ind, aux, by = "ONSConstID", all.x = TRUE)
    ps <- merge(ps, aux, by = "ONSConstID", all.x = TRUE)

### Create the model
    mod <- brm(prefers_pr ~ (1|GE2015) + (1|Ref2016) + (1|Gender) + (1|Education) +
                   (1|Age) + (1|ONSConstID) + 
                   (1|Winner) + Con15 + Lab15 + LD15 + UKIP15 + Turnout15 +
                   c11PopulationDensity + c11Male + c11EthnicityWhiteBritish + c11QualLevel4,
               data = ind,
               chains = 4,
               cores = 4,
               prior = set_prior("normal(0, 1)", class = "b"),
               control = list(adapt_delta = 0.95,
                              max_treedepth = 12))


### Get predictions out
    pp <- posterior_linpred(mod,
                            newdata = ps,
                            nsamples = 2,
                            allow_new_levels = TRUE)
    pp <- t(pp)

    holder <- list()
    for (i in 1:ncol(pp)) {
        print(".")
        out <- rbinom(n = length(ps$count),
                      size = ps$count,
                      prob = plogis(pp[,i]))
        out <- aggregate(out,
                         list(const = ps$ONSConstID),
                         sum)
        tot <- aggregate(ps$count,
                         list(const = ps$ONSConstID),
                         sum)
        out$x <- out$x / tot$x
        out$iter <- i
        holder[[i]] <- out
    }

    holder <- bind_rows(holder) %>%
        group_by(const) %>%
        summarize(mean = mean(x),
                  lo = quantile(x, 0.05),
                  hi = quantile(x, 1 - 0.05))

} else {
### Or
source("mrp.R")
mod <- mrp(prefers_pr ~ GE2015 + Ref2016 + Gender + Education +
               Age | Winner + Con15 + Lab15 + LD15 + UKIP15 + Turnout15 +
               c11PopulationDensity + c11Male + c11EthnicityWhiteBritish + c11QualLevel4,
           surv = ind,
           ps = ps,
           aux = aux,
           type = "binary",
           const = "ONSConstID",
           weight.var = "count")

    holder <- mod$constsmry$intervals

}

plot.df <- holder %>%
    arrange(mean) %>%
    mutate(r = 1:n())

ggplot(plot.df, aes(x = r, y = mean, ymin = `5%`,
                    ymax = `95%`)) +
    geom_pointrange(alpha = 0.5) +
    scale_x_continuous("Rank") +
    scale_y_continuous("Support for PR",
                       labels = scales::percent) + 
    coord_flip() + 
    theme_bw()
    
