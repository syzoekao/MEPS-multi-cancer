rm(list = ls())

library(survey)
library(dplyr)
library(foreign)
library(rlang)
library(parallel)
library(ggplot2)
library(data.table)
library(ggpubr)
library(sjstats)
library(ggeffects)
library(pscl)

source("R/function.R")

options(survey.lonely.psu = "adjust")

fyc_all <- readRDS("Data/fyc_all.RDS")


can_cols <- c("can_melano", "can_nonmel", "can_skindk", 
              "can_skinnm", "can_allskin", 
              "can_lung", "can_colrec", 
              "can_prosta", "can_breast", "can_cervix", 
              "can_any", "can_mult", "no_cancer")

cancer_names <- c("melanoma", "NMSC", "unknown skin", 
                  "NMSC/unknown", "all skin", 
                  "lung", "colorectal", 
                  "prostate", "breast", "cervical", 
                  "any", "multiple", "no cancer")

ins_key <- c("<65, any private", "<65, other", "<65, other", 
             "65+, medicare/other", "65+, any private", "65+, medicare/other", 
             "exclude", "exclude")
names(ins_key) <- c("1.Young, Any private", "2.Young, Public only", "3.Young, Uninsured", 
                    "4.65+, Medicare Only", "5.65+, Medicare + Private", "6.65+, Medicare + Public", 
                    "7.65+, Uninsured", "8.65+, No Medicare")

fyc_all[, ins_recode2 := recode_factor(ins_recode, !!!ins_key)]

key_control <- c("age65", "sex_recode", "race_recode", "edu_recode", "married", 
                 "npec_recode", "ins_recode2", "region_recode")

fyc_all$edu_recode <- factor(fyc_all$edu_recode, levels = c("<=high school", ">=some colloege"))
fyc_all$married <- factor(fyc_all$married, levels = c("Not married", "Married"))
fyc_all$npec_recode <- factor(fyc_all$npec_recode, levels = c("0", "1", "2", "3+"))

fyc_all[, exclude_set := ifelse(ins_recode2 == "exclude" | age65 == "0-17" | perwt == 0, 1, 0)]
fyc_all$exclude_set[is.na(fyc_all$exclude_set)] <- 1
fyc_all$exclude_set[is.na(fyc_all$no_cancer)] <- 1

outcome_ls <- c("totexp_gdp", "faminc_pce", "totslf_pce", "totprv_pce", "totmcr_pce", "totmcd_pce", 
                "tototr_pce", "ambexp_pce", "iptexp_pce", "rxexp_pce", "tototr2_pce", 
                "ddnwrk_recode", "unable")

keep_vars <- c("varpsu", "varstr", "perwt", "svy_year",
               key_control, can_cols, 
               outcome_ls, "exclude_set")

new_fyc <- fyc_all[, ..keep_vars]


svyds <- svydesign(
  id = ~varpsu,
  strata = ~varstr,
  weights = ~perwt,
  data = new_fyc,
  survey.lonely.psu = "adjust", 
  nest = TRUE)


subdf <- subset(svyds, (exclude_set == 0 & age65 == "18-64") & 
                  (can_any == 1 | no_cancer == 1))
subdf$variables$ins_recode2 <- droplevels(subdf$variables$ins_recode2)
subdf$variables$totexp_gdp_plus1 <- subdf$variables$totexp_gdp + 1
subdf$variables$log_totexp_gdp <- log(subdf$variables$totexp_gdp + 1)
subdf$variables$totexp_gdp_i <- as.integer(subdf$variables$totexp_gdp)

wt <- stats::weights(subdf)
scale_wt <- wt / mean(wt)



library(glmmTMB)
out_hur <- glmmTMB(totexp_gdp ~ can_any + sex_recode + ins_recode2 + race_recode + 
                     region_recode + edu_recode + married + svy_year, 
                   family = ziGamma(link = "log"), 
                   ziformula = ~ can_any + sex_recode + ins_recode2 + race_recode + 
                     region_recode + edu_recode + married + svy_year, 
                   data = subdf$variables, weights = scale_wt)


summary(out_hur)
pred_y <- predict(out_hur, type = "response")


hist(subdf$variables$totexp_gdp[subdf$variables$totexp_gdp < 1000000], breaks = 100)
hist(attr(pred_y,"var"), add = T, col = rgb(0.6, 0.2, 0.2, 0.3), breaks = 100)

mar_eff <- ggemmeans(out_hur, terms = c("can_any", "svy_year"))


