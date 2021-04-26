rm(list = ls())

library(survey)
library(dplyr)
library(foreign)
library(parallel)
library(ggplot2)
library(data.table)
library(ggpubr)
library(margins)
library(jtools)
library(MASS)
library(sjstats)

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

key_control <- c("age65", "ageg", "sex_recode", "race_recode", "edu_recode", "married", 
                 "npec_recode", "ins_recode2", "region_recode")

fyc_all$edu_recode <- factor(fyc_all$edu_recode, levels = c("<=high school", ">=some colloege"))
fyc_all$married <- factor(fyc_all$married, levels = c("Not married", "Married"))
fyc_all$npec_recode <- factor(fyc_all$npec_recode, levels = c("0", "1", "2", "3+"))

fyc_all[, exclude_set := ifelse(ins_recode2 == "exclude" | age65 == "0-17" | perwt == 0, 1, 0)]
fyc_all$exclude_set[is.na(fyc_all$exclude_set)] <- 1
fyc_all$exclude_set[is.na(fyc_all$no_cancer)] <- 1
fyc_all[, year_center := svy_year - 2013]
fyc_all[, unable := 0]
fyc_all$unable[fyc_all$nwk31 == 3 & fyc_all$empst31 == 4] <- 1
fyc_all$unable[fyc_all$nwk42 == 3 & fyc_all$empst42 == 4] <- 1
fyc_all$unable[fyc_all$nwk53 == 3 & fyc_all$empst53 == 4] <- 1
fyc_all[, b_empst_old := 0]
fyc_all$b_empst_old[(fyc_all$empst31 == 1) & (fyc_all$empst53 == 1) & 
                      (fyc_all$empst42 == 1) & (fyc_all$age65 == ">=65")] <- 1 
fyc_all[, b_empst_young := 0]
fyc_all$b_empst_young[(fyc_all$empst31 == 1) & (fyc_all$empst53 == 1) & 
                      (fyc_all$empst42 == 1) & (fyc_all$age65 == "18-64")] <- 1


outcome_ls <- c("totexp_gdp", "totslf_pce", "totprv_pce", "totmcr_pce", "totmcd_pce", 
                "tototr_pce", "ambexp_pce", "iptexp_pce", "rxexp_pce", "tototr2_pce", 
                "ddnwrk_recode", "unable")

keep_vars <- c("varpsu", "varstr", "perwt", "svy_year", "year_center", 
               key_control, can_cols, "can_cond1", "b_empst_young", "b_empst_old", 
               outcome_ls, "exclude_set")

new_fyc <- fyc_all[, ..keep_vars]

rhs <- c("can_cond1", "ageg", "sex_recode", "race_recode", 
         "npec_recode", "region_recode", "edu_recode", "married", 
         "year_center")


svyds <- svydesign(
  id = ~varpsu,
  strata = ~varstr,
  weights = ~perwt,
  data = new_fyc,
  survey.lonely.psu = "adjust", 
  nest = TRUE)


exp_ls <- c("totexp_gdp", "totslf_pce", "totprv_pce", "totmcr_pce", "totmcd_pce", 
            "tototr_pce", "ambexp_pce", "iptexp_pce", "rxexp_pce", "tototr2_pce")
exp_name_ls <- c("total health care expenditures ($)", "OOP ($)", 
                 "expenditures paid by private insurance ($)", "expenditures paid by Medicare ($)", 
                 "expenditures paid by Medicaid ($)", "expenditures paid by other sources ($)", 
                 "expenditures for ambulatory care ($)", "expenditures for inpatient care ($)", 
                 "expenditures for prescription medications ($)", "expenditures for other services ($)")

g_dist_ls <- lapply(c(1:length(exp_ls)), function(x) {
  plot_hist(design = svyds, exp_ls[x], exp_name_ls[x])
})
names(g_dist_ls) <- exp_ls

## Plot missed work days
tmpdf <- svyds
tmpdf <- update(tmpdf, y = ddnwrk_recode)
tmpdf <- update(tmpdf, dum = ifelse(y > 0, 1, 0))
tmpdf <- update(tmpdf, missed_work = ifelse(dum == 1, "yes", "no"))
tmpdf$variables$groups <- NA
tmpdf$variables$groups[tmpdf$variables$age65 == "18-64" & tmpdf$variables$can_any == 0] <- "age 18-64: no cancer dx" 
tmpdf$variables$groups[tmpdf$variables$age65 == "18-64" & tmpdf$variables$can_any == 1] <- "age 18-64: cancer survivor" 
tmpdf$variables$groups[tmpdf$variables$age65 == ">=65" & tmpdf$variables$can_any == 0] <- "age 65+: no cancer dx" 
tmpdf$variables$groups[tmpdf$variables$age65 == ">=65" & tmpdf$variables$can_any == 1] <- "age 65+: cancer survivor" 

subds <- subset(tmpdf, exclude_set == 0 & (b_empst_young == 1 | b_empst_old == 1))
subds$variables$groups <- factor(subds$variables$groups, 
                                 levels = c("age 18-64: no cancer dx", "age 18-64: cancer survivor", 
                                            "age 65+: no cancer dx", "age 65+: cancer survivor"))
tmpdf <- data.table(subds$variables)
tmpdf[, log_y := log(y)]
plot_df <- tmpdf[, list(N = .N), by = .(groups, missed_work)]
plot_df[, prop := round(N / sum(N), 3), by = .(groups)][order(groups, missed_work)]

g_bar <- ggplot(plot_df) +
  geom_bar(aes(x = missed_work, y = prop), color = "royalblue", 
           fill = "white", size = 1, stat = "identity") +
  geom_text(aes(x = missed_work, y = prop, label = prop), vjust = 1, fontface = "bold") + 
  facet_wrap(.~groups) +
  ylab("fraction") + 
  xlab("had missed work days?") + 
  ggtitle("(A)") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 14, face = "bold", colour = "gray20"), 
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA), 
        axis.text.x = element_text(size = 12), # , angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        legend.position = "bottom")

g_hist <- ggplot(tmpdf[y > 0], aes(x = y)) +
  geom_histogram(aes(y = ..density..), bins = 50, color = "royalblue", 
                 fill = "royalblue", position = "identity") +
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::percent) + 
  xlab("missed work days, if >0") +
  ggtitle("(B)") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 14, face = "bold", colour = "gray20"), 
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA), 
        axis.text.x = element_text(size = 12), # , angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        legend.position = "bottom")

g_dist_ls$ddnwrk <- ggarrange(g_bar, g_hist, nrow = 2, heights = c(0.65, 0.35))


save_ls <- list()
save_ls$g_dist_ls <- g_dist_ls

#### Expenditures
print("======== 18-64: both sex =======")
beg_time <- Sys.time()
cost_est_young_all <- lapply(exp_ls, function(j) {
  gamma_hurdle(j, design = svyds, sel_age = "18-64", sex = NULL, rhs = rhs)   
})
names(cost_est_young_all) <- exp_ls
save_ls$cost_est_young_all <- cost_est_young_all
print(Sys.time() - beg_time)

print("======== 18-64: women =======")
beg_time <- Sys.time()
cost_est_young_female <- lapply(exp_ls, function(j) {
  gamma_hurdle(j, design = svyds, sel_age = "18-64", sex = "female", rhs = rhs)     
})
names(cost_est_young_female) <- exp_ls
save_ls$cost_est_young_female <- cost_est_young_female
print(Sys.time() - beg_time)

print("======== 18-64: men =======")
beg_time <- Sys.time()
cost_est_young_male <- lapply(exp_ls, function(j) {
  gamma_hurdle(j, design = svyds, sel_age = "18-64", sex = "male", rhs = rhs)   
})
names(cost_est_young_male) <- exp_ls
save_ls$cost_est_young_male <- cost_est_young_male
print(Sys.time() - beg_time)


print("======== 65+: both sex =======")
beg_time <- Sys.time()
cost_est_old_all <- lapply(exp_ls, function(j) {
  gamma_hurdle(j, design = svyds, sel_age = ">=65", sex = NULL, rhs = rhs)   
})
names(cost_est_old_all) <- exp_ls
save_ls$cost_est_old_all <- cost_est_old_all
print(Sys.time() - beg_time)

print("======== 65+: women =======")
beg_time <- Sys.time()
cost_est_old_female <- lapply(exp_ls, function(j) {
  gamma_hurdle(j, design = svyds, sel_age = ">=65", sex = "female", rhs = rhs)   
})
names(cost_est_old_female) <- exp_ls
save_ls$cost_est_old_female <- cost_est_old_female
print(Sys.time() - beg_time)

print("======== 65+: men =======")
beg_time <- Sys.time()
cost_est_old_male <- lapply(exp_ls, function(j) {
  gamma_hurdle(j, design = svyds, sel_age = ">=65", sex = "male", rhs = rhs)    
})
names(cost_est_old_male) <- exp_ls
save_ls$cost_est_old_male <- cost_est_old_male
print(Sys.time() - beg_time)


#### Unable to work
print("======== 18-64: both sex =======")
beg_time <- Sys.time()
unable_young_all <- logit(y = "unable", design = svyds, 
                          sel_age = "18-64", sex = NULL, rhs = rhs) 
save_ls$unable_young_all <- unable_young_all
print(Sys.time() - beg_time)

print("======== 18-64: women =======")
beg_time <- Sys.time()
unable_young_female <- logit(y = "unable", design = svyds, 
                          sel_age = "18-64", sex = "female", rhs = rhs) 
save_ls$unable_young_female <- unable_young_female
print(Sys.time() - beg_time)

print("======== 18-64: men =======")
beg_time <- Sys.time()
unable_young_male <- logit(y = "unable", design = svyds, 
                          sel_age = "18-64", sex = "male", rhs = rhs) 
save_ls$unable_young_male <- unable_young_male
print(Sys.time() - beg_time)


print("======== 65+: both sex =======")
beg_time <- Sys.time()
unable_old_all <- logit(y = "unable", design = svyds, 
                          sel_age = ">=65", sex = NULL, rhs = rhs) 
save_ls$unable_old_all <- unable_old_all
print(Sys.time() - beg_time)

print("======== 65+: women =======")
beg_time <- Sys.time()
unable_old_female <- logit(y = "unable", design = svyds, 
                          sel_age = ">=65", sex = "female", rhs = rhs) 
save_ls$unable_old_female <- unable_old_female
print(Sys.time() - beg_time)

print("======== 65+: men =======")
beg_time <- Sys.time()
unable_old_male <- logit(y = "unable", design = svyds, 
                           sel_age = ">=65", sex = "male", rhs = rhs) 
save_ls$unable_old_male <- unable_old_male
print(Sys.time() - beg_time)


#### Missed work days
rhs <- c(rhs[!rhs %in% "poly(year_center, 3)"], "year_center")

print("======== 18-64: both sex =======")
beg_time <- Sys.time()
wkday_young_all <- nb_hurdle(y = "ddnwrk_recode", design = svyds, 
                          sel_age = "18-64", sex = NULL, rhs = rhs) 
save_ls$wkday_young_all <- wkday_young_all
print(Sys.time() - beg_time)

print("======== 18-64: female =======")
beg_time <- Sys.time()
wkday_young_female <- nb_hurdle(y = "ddnwrk_recode", design = svyds, 
                             sel_age = "18-64", sex = "female", rhs = rhs) 
save_ls$wkday_young_female <- wkday_young_female
print(Sys.time() - beg_time)

print("======== 18-64: male =======")
beg_time <- Sys.time()
wkday_young_male <- nb_hurdle(y = "ddnwrk_recode", design = svyds, 
                                sel_age = "18-64", sex = "male", rhs = rhs) 
save_ls$wkday_young_male <- wkday_young_male
print(Sys.time() - beg_time)


print("======== >=65: both sex =======")
beg_time <- Sys.time()
wkday_old_all <- nb_hurdle(y = "ddnwrk_recode", design = svyds, 
                           sel_age = ">=65", sex = NULL, rhs = rhs) 
save_ls$wkday_old_all <- wkday_old_all
print(Sys.time() - beg_time)

print("======== >=65: female =======")
beg_time <- Sys.time()
wkday_old_female <- nb_hurdle(y = "ddnwrk_recode", design = svyds, 
                                sel_age = ">=65", sex = "female", rhs = rhs) 
save_ls$wkday_old_female <- wkday_old_female
print(Sys.time() - beg_time)

print("======== >=65: male =======")
beg_time <- Sys.time()
wkday_old_male <- nb_hurdle(y = "ddnwrk_recode", design = svyds, 
                              sel_age = ">=65", sex = "male", rhs = rhs) 
save_ls$wkday_old_male <- wkday_old_male
print(Sys.time() - beg_time)

saveRDS(save_ls, "Results/est_out.RDS")



#### Making tables
rm(list = ls())
library(data.table)
library(ggplot2)
library(dplyr)

source("R/function.R")

res0 <- readRDS("Results/est_out.RDS")

table_ls <- list()

res_contents <- names(res0)

cost_pop <- grep("cost_", res_contents, value = T)
cost_outcome <- names(res0$cost_est_young_all)

cost_est <- data.frame(expand.grid(subpop = cost_pop, y = cost_outcome))

cost_dt <- lapply(c(1:nrow(cost_est)), function(x) {
  subpop0 <- as.character(cost_est$subpop[x])
  y0 <- as.character(cost_est$y[x])
  bind_estimates(subpop = subpop0, y = y0, res = res0)
})
cost_model <- rbindlist(lapply(cost_dt, `[[`, 1))
cost_pred_margin <- rbindlist(lapply(cost_dt, `[[`, 2))

key_exp <- exp_name_ls
names(key_exp) <- exp_ls

key_exp <- gsub("\\s*\\([^\\)]+\\)", "", key_exp)
key_exp[1] <- "any health care expenditures"
key_exp[2] <- "any OOP"
key_exp <- gsub("expenditures paid by", "any", key_exp)
key_exp <- gsub("expenditures for", "any", key_exp)

cost_est <- data.table(cost_est)
cost_est[, y := recode(y, !!!key_exp)]
cost_est[, age := ifelse(grepl("young", subpop), "Age 18-64", "Age 65+")]
cost_est[, population := ifelse(grepl("all", subpop), "Both Men and Women", 
                                ifelse(grepl("female", subpop), "Women", "Men"))]
cost_est[, group := paste0(y, ": ", population)]

tmp_ix <- cost_est[, list(min_ix = min(.I), 
                          max_ix = max(.I), 
                          N = .N), by = "group"]

plot_dat_ls <- lapply(cost_dt, `[[`, 3)

table_ls$g_cost_odds <- lapply(c(1:nrow(tmp_ix)), 
                               plot_odds_ratio, plot_dat_ls = plot_dat_ls, tmp_ix = tmp_ix)

table_ls$g_cost_odds$tmp_ix <- tmp_ix

coef_order <- unique(cost_model$term)
cost_model[, term := factor(term, levels = coef_order)]
cost_model[, outcome:= factor(outcome, levels = cost_outcome)]
cost_model <- cost_model[order(outcome, pop, age, part, term)]

table_ls$coef_exptot_all <- subset_tables(cost_model, outcome_sel = "totexp_gdp", pop_sel = "all")
table_ls$coef_exptot_female <- subset_tables(cost_model, outcome_sel = "totexp_gdp", pop_sel = "female")
table_ls$coef_exptot_male <- subset_tables(cost_model, outcome_sel = "totexp_gdp", pop_sel = "male")

expsrc_ls <- c("totslf_pce", "totprv_pce", "totmcr_pce", "totmcd_pce", "tototr_pce")
table_ls$coef_expsrc_all <- subset_tables(cost_model, outcome_sel = expsrc_ls, pop_sel = "all")
table_ls$coef_expsrc_female <- subset_tables(cost_model, outcome_sel = expsrc_ls, pop_sel = "female")
table_ls$coef_expsrc_male <- subset_tables(cost_model, outcome_sel = expsrc_ls, pop_sel = "male")

expsvc_ls <- c("ambexp_pce", "iptexp_pce", "rxexp_pce", "tototr2_pce")
table_ls$coef_expsvc_all <- subset_tables(cost_model, outcome_sel = expsvc_ls, pop_sel = "all")
table_ls$coef_expsvc_female <- subset_tables(cost_model, outcome_sel = expsvc_ls, pop_sel = "female")
table_ls$coef_expsvc_male <- subset_tables(cost_model, outcome_sel = expsvc_ls, pop_sel = "male")

table_ls$marg_exp_all <- subset_tables(cost_pred_margin, outcome_sel = exp_ls, pop_sel = "all", expenditure_margin = T)
table_ls$marg_exp_female <- subset_tables(cost_pred_margin, outcome_sel = exp_ls, pop_sel = "female", expenditure_margin = T)
table_ls$marg_exp_male <- subset_tables(cost_pred_margin, outcome_sel = exp_ls, pop_sel = "male", expenditure_margin = T)


unable_pop <- grep("unable_", res_contents, value = T)
unable_dt <- lapply(unable_pop, function(x) {
  subpop0 <- x
  y0 <- NULL
  bind_estimates(subpop = subpop0, y = y0, res = res0)
})
unable_model <- rbindlist(lapply(unable_dt, `[[`, 1))
unable_pred_margin <- rbindlist(lapply(unable_dt, `[[`, 2))

unable_est <- data.table(unable_pop = unable_pop)
unable_est[, group := gsub("unable", "Employment Disability %", unable_pop)]
unable_est[, group := gsub("_young_", " ", group)]
unable_est[, group := gsub("_old_", " ", group)]
unable_est[, group := gsub("all", ": Both Men and Women", group)]
unable_est[, group := gsub("female", ": Women", group)]
unable_est[, group := gsub("male", ": Men", group)]

tmp_ix <- unable_est[, list(min_ix = min(.I), 
                          max_ix = max(.I), 
                          N = .N), by = "group"]

plot_dat_ls <- lapply(unable_dt, `[[`, 3)

table_ls$g_unable_odds <- lapply(c(1:nrow(tmp_ix)), 
                               plot_odds_ratio, 
                               plot_dat_ls = plot_dat_ls, tmp_ix = tmp_ix)
table_ls$g_unable_odds$tmp_ix <- tmp_ix


table_ls$coef_unable_all <- subset_tables(unable_model, outcome_sel = "unable to work", pop_sel = "all")
table_ls$coef_unable_female <- subset_tables(unable_model, outcome_sel = "unable to work", pop_sel = "female")
table_ls$coef_unable_male <- subset_tables(unable_model, outcome_sel = "unable to work", pop_sel = "male")

table_ls$marg_unable_all <- subset_tables(unable_pred_margin, outcome_sel = "unable to work", pop_sel = "all")
table_ls$marg_unable_female <- subset_tables(unable_pred_margin, outcome_sel = "unable to work", pop_sel = "female")
table_ls$marg_unable_male <- subset_tables(unable_pred_margin, outcome_sel = "unable to work", pop_sel = "male")


wkday_pop <- grep("wkday_", res_contents, value = T)
wkday_dt <- lapply(wkday_pop, function(x) {
  subpop0 <- x
  y0 <- NULL
  bind_estimates(subpop = subpop0, y = y0, res = res0)
})
wkday_model <- rbindlist(lapply(wkday_dt, `[[`, 1))
wkday_pred_margin <- rbindlist(lapply(wkday_dt, `[[`, 2))

wkday_est <- data.table(wkday_pop = wkday_pop)
wkday_est[, group := gsub("wkday", "Any Missed Work Days", wkday_pop)]
wkday_est[, group := gsub("_young_", " ", group)]
wkday_est[, group := gsub("_old_", " ", group)]
wkday_est[, group := gsub("all", ": Both Men and Women", group)]
wkday_est[, group := gsub("female", ": Women", group)]
wkday_est[, group := gsub("male", ": Men", group)]

tmp_ix <- wkday_est[, list(min_ix = min(.I), 
                            max_ix = max(.I), 
                            N = .N), by = "group"]

plot_dat_ls <- lapply(wkday_dt, `[[`, 3)

table_ls$g_wkday_odds <- lapply(c(1:nrow(tmp_ix)), 
                                 plot_odds_ratio, 
                                 plot_dat_ls = plot_dat_ls, tmp_ix = tmp_ix)
table_ls$g_wkday_odds$tmp_ix <- tmp_ix



table_ls$coef_wkday_all <- subset_tables(wkday_model, outcome_sel = "missed work days", pop_sel = "all")
table_ls$coef_wkday_female <- subset_tables(wkday_model, outcome_sel = "missed work days", pop_sel = "female")
table_ls$coef_wkday_male <- subset_tables(wkday_model, outcome_sel = "missed work days", pop_sel = "male")

table_ls$marg_wkday_all <- subset_tables(wkday_pred_margin, outcome_sel = "missed work days", pop_sel = "all")
table_ls$marg_wkday_female <- subset_tables(wkday_pred_margin, outcome_sel = "missed work days", pop_sel = "female")
table_ls$marg_wkday_male <- subset_tables(wkday_pred_margin, outcome_sel = "missed work days", pop_sel = "male")


saveRDS(table_ls, "Results/est_table.RDS")





