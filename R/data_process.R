rm(list = ls())

library(survey)
library(dplyr)
library(foreign)
library(rlang)
library(parallel)
library(ggplot2)
library(data.table)
library(ggpubr)

source("R/function.R")

options(survey.lonely.psu = "adjust")

data_path <- "MEPS data/"
yr_vec <- paste0(c(8:18))
yr_vec <- ifelse(nchar(yr_vec) == 1, paste0("0", yr_vec), yr_vec)

system.time(
  yr_df <- mclapply(yr_vec, function(x) {
    get_fyc_annual(yr = x, data_path = data_path)
  }, mc.cores = 6)
)

fyc_all <- rbindlist(yr_df, use.names = T)
fyc_all[, `:=` (perwt = perwtf / (max(svy_year) - min(svy_year) + 1), 
                no_cancer = 1 - can_any, 
                sex_recode = factor(sex), 
                edu_recode = ifelse(edu != "Some college or more", 
                                    ">=some colloege", "<=high school"), 
                ins_recode2 = ifelse(ins_recode == "1.Young, Any private", "<65, any private", 
                                     ifelse(ins_recode %in% c("2.Young, Public only", "3.Young, Uninsured"), "<65, other", 
                                            ifelse(ins_recode %in% c("4.65+, Medicare with all others", "5.65+, Any private, TRICARE/CHAMPVA"), 
                                                   "65+, medicare and private", "65+, other"))), 
                region_recode = factor(region_recode),
                age_two_group = ifelse(ageg != "0-17", "18-85", "0-17"), 
                married = ifelse(is.na(married), "Not married", married), 
                person = 1, 
                tmp_marry = NULL, 
                tmp_marry_rd = NULL)]
levels(fyc_all$sex_recode) <- c("male", "female")
levels(fyc_all$region_recode) <- c("northeast", "midwest", "south", "west")




output_list <- list()
plot_list <- list()

#### Sample size by survey year and by age group
n_by_year <- fyc_all[, .N, by = .(svy_year)]
n_by_age <- fyc_all[, .N, by = .(ageg)][order(ageg)]
n_by_age_year <- fyc_all[, .N, 
                         by = .(svy_year, age65)][
                           order(age65, svy_year)]

output_list$raw_counts$n_by_year <- n_by_year
output_list$raw_counts$n_by_age <- n_by_age
output_list$raw_counts$n_by_age_year <- n_by_age_year


age_color <- c("yellowgreen", "deepskyblue", "tomato")

uwt_n_by_age_year <- ggplot(n_by_age_year) + 
  geom_line(aes(x = svy_year, y = N / 1000, color = age65), 
            size = 1) + 
  geom_point(aes(x = svy_year, y = N / 1000, color = age65), 
             shape = 21, fill = "white", size = 2, stroke = 1.5) + 
  xlab("survey year") + 
  ylab("sample size (1,000 people)") + 
  ggtitle("Sample size") + 
  scale_color_manual(values = age_color) + 
  scale_x_continuous(breaks = seq(2008, 2018, 1)) + 
  theme_bw() +
  theme(legend.positio = "bottom")

plot_list$uwt_n_by_age_year <- uwt_n_by_age_year

#### Check important variables by year
dmg_ls <- c("sex_recode", "race_recode", "region_recode", "edu_recode", "ins_recode", 
            "married", "income_level")

dmg_tab <- lapply(dmg_ls, function(x) {
  if (x != "faminc") {
    by_cate <- c("svy_year", x)
    out <- fyc_all[, .N, by = by_cate]
    out <- out[order(svy_year, eval(parse(text = paste0(x))))]
    tmp_form <- as.formula(paste0("svy_year ~ ", x))
    dcast(out, tmp_form) 
  } else {
    by_cate <- c("svy_year")
    out <- fyc_all[, list(m_faminc = mean(eval(parse(text = paste0(x))))), by = by_cate]
    out <- out[order(svy_year)]
    out
  }
})
names(dmg_tab) <- dmg_ls

output_list$dmg_tab <- dmg_tab

exp_ls <- c(paste0("tot", c("exp", "slf", "prv", "mcr", "mcd", "otr")), 
            paste0(c("obv", "opt", "amb", "ipt", "rx", "ert", "hha", "hhn", "dvt", "vis", "oth"), "exp"), 
            "tototr2")

exp_tab <- lapply(exp_ls, function(x) {
  by_cate <- c("svy_year")
  out <- fyc_all[, list(m_exp = mean(eval(parse(text = paste0(x))))), by = by_cate]
  out <- out[order(svy_year)]
  setnames(out, c("svy_year", "exp"))
  out[, type := x]
})
names(exp_tab) <- exp_ls

exp_tab <- rbindlist(exp_tab)
exp_tab_wide <- dcast(exp_tab, svy_year ~ type, value.var = "exp")

output_list$exp_tab <- exp_tab_wide

tsdx_ls <- paste0("tsdx_", c("colrec", "breast", "cervix", "prosta", "lung", "melano", "sknm", "allskin"))

tsdx_tab <- lapply(tsdx_ls, function(x) {
  by_cate <- c("svy_year")
  out <- fyc_all[, list(m_tsdx = mean(eval(parse(text = paste0(x))), na.rm = T)), by = by_cate]
  out <- out[order(svy_year)]
  setnames(out, c("svy_year", "tsdx"))
  out[, type := x]
})
names(tsdx_tab) <- tsdx_ls

tsdx_tab <- rbindlist(tsdx_tab)
tsdx_tab_wide <- dcast(tsdx_tab, svy_year ~ type, value.var = "tsdx")

output_list$tsdx_tab <- tsdx_tab_wide

prod_ls <- c("ddnwrk_recode", "ddbdys_recode", "totddl", "empst", "empst2", "unable", "unable2", "unable3")

prod_tab <- lapply(prod_ls, function(x) {
  by_cate <- c("svy_year")
  out <- fyc_all[, list(m_prod = mean(eval(parse(text = paste0(x))), na.rm = T)), by = by_cate]
  out <- out[order(svy_year)]
  setnames(out, c("svy_year", "prod"))
  out[, type := x]
})
names(prod_tab) <- prod_ls

prod_tab <- rbindlist(prod_tab)
prod_tab_wide <- dcast(prod_tab, svy_year ~ type, value.var = "prod")

output_list$prod_tab <- prod_tab_wide


#### Cancer sample by demographics
can_cols <- c("can_melano", "can_nonmel", "can_skindk", 
              "can_skinnm", "can_allskin", 
              "can_lung", "can_colrec", 
              "can_prosta", "can_breast", "can_cervix", 
              "can_any", "can_mult")

cancer_names <- c("melanoma", "NMSC", "unknown skin", 
                  "NMSC/unknown", "all skin", 
                  "lung", "colorectal", 
                  "prostate", "breast", "cervical", 
                  "any", "multiple")

fyc_all[, `:=` (can_na = ifelse(!is.na(age65) & is.na(can_any), 1, 0), 
                npec_recode = ifelse(is.na(npec_recode), 0, npec_recode))]

calc_freq_by_char <- function(x, dt) {
  if (x == "overall") {
    out <- dt[ageg != "0-17", 
              lapply(.SD, sum, na.rm = T), 
              .SDcols = c(can_cols, "no_cancer")]
    out <- out[, lapply(.SD, format, big.mark = ","), .SDcols = c(can_cols, "no_cancer")]
    out[, `:=` (category = "overall", 
                level = "N")]
    setcolorder(out, c("category", "level", can_cols, "no_cancer"))
  } else {
    out <- dt[ageg != "0-17", 
              lapply(.SD, sum, na.rm = T), 
              by = c(x), 
              .SDcols = c(can_cols, "no_cancer")][
                order(eval(parse(text = paste0(x))))]
    out <- melt(out, id.vars = x)
    out[, pct := paste0(round(100 * value / sum(value), 2), "%"), by = c("variable")]
    out[, n_pct := paste0(format(value, big.mark = ","), " (", pct, ")")]
    tmp_fml<- as.formula(paste0(x, " ~ variable"))
    out_wide <- dcast(out, tmp_fml, value.var = "n_pct")
    out_wide[, `:=` (category = x)]
    setnames(out_wide, x, "level")
    setcolorder(out_wide, c("category", "level", can_cols, "no_cancer"))
  }
}

calc_freq_by_char_mutual_excl <- function(x, dt) {
  if (x == "overall") {
    out <- dt[ageg != "0-17", list(N = .N), by = can_cond1][order(can_cond1)]
    tmp_fml<- as.formula(paste0(x, " ~ can_cond1"))
    out_wide <- dcast(out, .~can_cond1)
    out_wide[, `:=` (`.` = NULL)]
    out_wide[, `:=` (category = "overall", 
                level = "N")]
    setcolorder(out_wide, c("category", "level"))
  } else {
    out <- dt[ageg != "0-17", list(N = .N), by = c("can_cond1", x)][
                order(can_cond1, eval(parse(text = paste0(x))))]
    out[, pct := paste0(round(100 * N / sum(N), 2), "%"), by = .(can_cond1)]
    out[, n_pct := paste0(format(N, big.mark = ","), " (", pct, ")")]
    tmp_fml<- as.formula(paste0(x, " ~ can_cond1"))
    out_wide <- dcast(out, tmp_fml, value.var = "n_pct")
    out_wide[, `:=` (category = x)]
    setnames(out_wide, x, "level")
    setcolorder(out_wide, c("category", "level"))
  }
}

tmp_ls <- c("overall", "ageg", "sex_recode", "race_recode", "edu_recode", "married", "npec_recode", 
       "ins_recode", "income_level", "region_recode")

cancer_by_char <- lapply(tmp_ls, calc_freq_by_char, dt = fyc_all)
cancer_by_char <- rbindlist(cancer_by_char)

output_list$cancer_by_char <- cancer_by_char


cancer_by_char2 <- lapply(tmp_ls, calc_freq_by_char_mutual_excl, dt = fyc_all)
cancer_by_char2 <- rbindlist(cancer_by_char2)
cancer_by_char2[, `:=` (multiple = NULL, other = NULL)]

cancer_by_char2_f <- lapply(tmp_ls, calc_freq_by_char_mutual_excl, dt = fyc_all[sex_recode == "female"])
cancer_by_char2_f <- rbindlist(cancer_by_char2_f)
cancer_by_char2_f <- cancer_by_char2_f[, .(category, level, `no cancer`)]
colnames(cancer_by_char2_f)[colnames(cancer_by_char2_f) %in% "no cancer"] <- c("female: no cancer")

cancer_by_char2_m <- lapply(tmp_ls, calc_freq_by_char_mutual_excl, dt = fyc_all[sex_recode == "male"])
cancer_by_char2_m <- rbindlist(cancer_by_char2_m)
cancer_by_char2_m <- cancer_by_char2_m[, .(category, level, `no cancer`)]
colnames(cancer_by_char2_m)[colnames(cancer_by_char2_m) %in% "no cancer"] <- c("male: no cancer")

cancer_by_char2 <- merge(cancer_by_char2, cancer_by_char2_f, by = c("category", "level"), all.x = T)
cancer_by_char2 <- merge(cancer_by_char2, cancer_by_char2_m, by = c("category", "level"), all.x = T)

output_list$cancer_by_char2 <- cancer_by_char2

### By survey year

cancer_by_svy_ct <- fyc_all[ageg != "0-17", 
                            lapply(.SD, sum, na.rm = T), 
                            by = .(svy_year), 
                            .SDcols = c(can_cols)][
                              order(svy_year)]

ct_long <- melt(cancer_by_svy_ct, id = c("svy_year"))
levels(ct_long$variable) <- c(cancer_names)
output_list$cancer_by_svy <- cancer_by_svy_ct

cancer_by_svy_ct2 <- fyc_all[ageg != "0-17", list(N = .N), 
                            by = .(can_cond1, svy_year)][
                              order(can_cond1, svy_year)][
                                !can_cond1 %in% c("multiple", "other")]
cancer_by_svy_ct2 <- dcast(cancer_by_svy_ct2, svy_year ~ can_cond1, value.var = "N")
output_list$cancer_by_svy2 <- cancer_by_svy_ct2

ct_long2 <- melt(cancer_by_svy_ct2, id = c("svy_year"))

uwt_N_by_svy_can <- ggplot(data = ct_long2, 
                           aes(y = value, x = svy_year)) + 
  geom_bar(# aes(color = age_group), 
           color = "yellowgreen", 
           fill = "white", stat = "identity", 
           position = "dodge", width = 0.8, size = 0.8) + 
  # scale_color_manual(values = age_color[1]) + 
  facet_wrap(.~variable, ncol = 3, scales = "free_y") +
  xlab("Survey Year") + 
  ylab("Frequency") + 
  ggtitle("Number of Cancer Survivors 2008-2018 (unweighted)") + 
  scale_x_continuous(breaks = seq(2008, 2018, 1)) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 12, face = "bold", colour = "gray20"), 
        axis.text.x = element_text(size = 10, angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.position = "bottom")

plot_list$uwt_N_by_svy_can <- uwt_N_by_svy_can

## By survey year and age
cancer_by_svy_age65_ct <- fyc_all[, lapply(.SD, sum, na.rm = T), 
                           by = .(svy_year, age65), 
                           .SDcols = c(can_cols, "no_cancer", "can_na")][
                             order(svy_year, age65)]

ct_long <- melt(cancer_by_svy_age65_ct, id = c("svy_year", "age65"))
levels(ct_long$variable) <- c(cancer_names, "no cancer", "missing")
ct_long <- ct_long[!variable %in% c("no cancer", "missing")]
setnames(ct_long, "age65", "age_group")

output_list$cancer_by_svy_age_group <- cancer_by_svy_age65_ct[age65 != "0-17"]

cancer_by_svy_age65_ct2 <- fyc_all[, list(N = .N), 
                                  by = .(svy_year, age65, can_cond1)][
                                    order(can_cond1, svy_year, age65)][
                                      !can_cond1 %in% c("multiple", "other")]
cancer_by_svy_age65_ct2 <- cancer_by_svy_age65_ct2[age65 %in% c("18-64", ">=65")]
cancer_by_svy_age65_ct2 <- dcast(cancer_by_svy_age65_ct2, svy_year + age65 ~ can_cond1, value.var = "N")
output_list$cancer_by_svy_age65_ct2 <- cancer_by_svy_age65_ct2

ct_long2 <- melt(cancer_by_svy_age65_ct2, id = c("svy_year", "age65"))
setnames(ct_long2, "age65", "age_group")

output_list$cancer_by_svy_age_group2 <- cancer_by_svy_age65_ct2

uwt_N_by_age_can <- ggplot(data = ct_long2, 
       aes(y = value, x = svy_year)) + 
  geom_bar(aes(color = age_group), 
           fill = "white", stat = "identity", 
           position = "dodge", width = 0.8, size = 0.8) + 
  scale_color_manual(values = age_color[-1]) + 
  facet_wrap(.~variable, ncol = 3, scales = "free_y") +
  xlab("Survey Year") + 
  ylab("Frequency") + 
  ggtitle("Number of Cancer Survivors 2008-2018 (unweighted)") + 
  scale_x_continuous(breaks = seq(2008, 2018, 1)) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 12, face = "bold", colour = "gray20"), 
        axis.text.x = element_text(size = 10, angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.position = "bottom")

plot_list$uwt_N_by_age_can <- uwt_N_by_age_can

## By survey year and sex
cancer_by_svy_sex_ct <- fyc_all[age65 != "0-17", lapply(.SD, sum, na.rm = T), 
                                  by = .(svy_year, sex_recode), 
                                  .SDcols = c(can_cols, "no_cancer", "can_na")][
                                    order(svy_year, sex_recode)]

ct_long <- melt(cancer_by_svy_sex_ct, id = c("svy_year", "sex_recode"))
levels(ct_long$variable) <- c(cancer_names, "no cancer", "missing")
ct_long <- ct_long[!variable %in% c("no cancer", "missing")]
setnames(ct_long, "sex_recode", "sex")

output_list$cancer_by_svy_sex <- cancer_by_svy_sex_ct

uwt_N_by_sex_can <- ggplot(data = ct_long, 
                           aes(y = value, x = svy_year)) + 
  geom_bar(aes(color = sex), 
           fill = "white", stat = "identity", 
           position = "dodge", width = 0.8, size = 0.8) + 
  scale_color_manual(values = age_color[-1]) + 
  facet_wrap(.~variable, ncol = 3, scales = "free_y") +
  xlab("Survey Year") + 
  ylab("Frequency") + 
  ggtitle("Number of Cancer Survivors 2008-2018 (unweighted)") + 
  scale_x_continuous(breaks = seq(2008, 2018, 1)) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 12, face = "bold", colour = "gray20"), 
        axis.text.x = element_text(size = 10, angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.position = "bottom")

plot_list$uwt_N_by_sex_can <- uwt_N_by_sex_can



## Adjust population using survey weight

calc_weighted_count <- function(cancerx, cond = NULL, svyds) {
  tmp_fml <- paste0("~ svy_year + ", cancerx)
  if (!is.null(cond)) tmp_fml <- paste0(tmp_fml, " + ", cond) 
  tmp_fml <- as.formula(tmp_fml)
  out <- svyby(~person, 
               by = tmp_fml, 
               FUN = svytotal, design = svyds)
  out <- data.table(out)
  setnames(out, cancerx, "tmp")
  out <- out[tmp == 1]
  out[, `:=` (tmp = NULL, 
              cancer_type = cancerx)]
}

svyds<- svydesign(
  id = ~varpsu,
  strata = ~varstr,
  weights = ~perwt,
  data = fyc_all,
  survey.lonely.psu="adjust", 
  nest = TRUE)

svy_tot_can <- mclapply(can_cols, function(x, cond = "age_two_group") {
  calc_weighted_count(cancerx = x, cond = cond, svyds = svyds)
}, mc.cores = 5)

cancer_recode_key <- cancer_names
names(cancer_recode_key) <- can_cols

svy_tot_can <- rbindlist(svy_tot_can)
svy_tot_can$cancer_type <- recode(svy_tot_can$cancer_type, !!!cancer_recode_key)
svy_tot_can$cancer_type <- factor(svy_tot_can$cancer_type, levels = cancer_names)
svy_tot_can <- svy_tot_can[age_two_group != "0-17"]

wt_N_by_svy_can <- ggplot(data = svy_tot_can, 
                          aes(y = person / 1000, x = svy_year)) + 
  geom_bar(color = "yellowgreen", fill = "white", stat = "identity", 
           position = "dodge", width = 0.8, size = 0.8) + 
  geom_errorbar(aes(ymin = (person - 2 * se) / 1000, ymax = (person + 2 * se) / 1000),
                color = "yellowgreen", width = 0, position = position_dodge(0.75)) +  
  facet_wrap(.~cancer_type, ncol = 3, scales = "free_y") +
  xlab("Survey Year") + 
  ylab("1,000 peope") + 
  ggtitle("Number of Cancer Survivors 2008-2018 (weighted)") + 
  scale_x_continuous(breaks = seq(2008, 2018, 1)) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 12, face = "bold", colour = "gray20"), 
        axis.text.x = element_text(size = 10, angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.position = "bottom")

svy_tot_can[, n_se := paste0(format(round(person), big.mark = ","), "<br>(",
                             format(round(se), big.mark = ","), ")")]
svy_tot_can_wide <- dcast(svy_tot_can, svy_year ~ cancer_type, value.var = "n_se")

output_list$wt_N_by_svy_can <- svy_tot_can_wide

plot_list$wt_N_by_svy_can <- wt_N_by_svy_can


svy_tot_age_can <- mclapply(can_cols, function(x, cond = "age65") {
  calc_weighted_count(cancerx = x, cond = cond, svyds = svyds)
}, mc.cores = 5)

cancer_recode_key <- cancer_names
names(cancer_recode_key) <- can_cols

svy_tot_age_can <- rbindlist(svy_tot_age_can)
svy_tot_age_can$cancer_type <- recode(svy_tot_age_can$cancer_type, !!!cancer_recode_key)
svy_tot_age_can$cancer_type <- factor(svy_tot_age_can$cancer_type, levels = cancer_names)
setnames(svy_tot_age_can, "age65", "age_group")
svy_tot_age_can <- svy_tot_age_can[age_group != "0-17"]

wt_N_by_age_can <- ggplot(data = svy_tot_age_can, 
       aes(y = person / 1000, x = svy_year)) + 
  geom_bar(aes(color = age_group), 
           fill = "white", stat = "identity", 
           position = "dodge", width = 0.8, size = 0.8) + 
  geom_errorbar(aes(ymin = (person - 2 * se) / 1000, ymax = (person + 2 * se) / 1000, 
                    color = age_group),
                width = 0, position = position_dodge(0.75)) +  
  scale_color_manual(values = age_color[-1]) + 
  facet_wrap(.~cancer_type, ncol = 3, scales = "free_y") +
  xlab("Survey Year") + 
  ylab("1,000 peope") + 
  ggtitle("Number of Cancer Survivors 2008-2018 (weighted)") + 
  scale_x_continuous(breaks = seq(2008, 2018, 1)) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 12, face = "bold", colour = "gray20"), 
        axis.text.x = element_text(size = 10, angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.position = "bottom")

svy_tot_age_can[, n_se := paste0(format(round(person), big.mark = ","), "<br>(",
                                 format(round(se), big.mark = ","), ")")]
svy_tot_age_can_wide <- dcast(svy_tot_age_can, svy_year + age_group ~ cancer_type, value.var = "n_se")

output_list$wt_N_by_age_can <- svy_tot_age_can_wide

plot_list$wt_N_by_age_can <- wt_N_by_age_can


#### Adjust price

p_ix <- read.csv(paste0(data_path, "price index.csv")) %>%
  select(Year, GDP, CPI, PCE) %>% 
  filter(Year >= 2008) %>% 
  data.table()

tmp <- p_ix[Year == 2018]

p_ix[, `:=` (gdp2018 = tmp$GDP / GDP, 
             cpi2018 = tmp$CPI / CPI, 
             pce2018 = tmp$PCE / PCE, 
             svy_year = Year, 
             Year = NULL, 
             GDP = NULL, 
             CPI = NULL, 
             PCE = NULL)]
setkey(p_ix, "svy_year")

fyc_all <- fyc_all[p_ix, on = "svy_year"]

pce_tots <- c("faminc", "totslf", "totprv", "totmcr", "totmcd", "tototr", 
              "ambexp", "iptexp", "rxexp", "tototr2")
fyc_all[, `:=` (totexp_gdp = totexp * gdp2018)]
fyc_all[, paste0(pce_tots, "_pce") := lapply(.SD, function(x) x * pce2018), 
           .SDcols = pce_tots]

fyc_all[, lapply(.SD, mean, na.rm = T), 
        .SDcols = c("totexp_gdp", pce_tots), 
        .(svy_year)]

saveRDS(fyc_all, "Data/fyc_all.RDS")

## Adjust total expense

calc_weighted_avg_exp <- function(cancerx, outcome = "totexp_gdp", cond = NULL, svyds) {
  tmp_fml <- paste0("~ svy_year + ", cancerx)
  if (!is.null(cond)) tmp_fml <- paste0(tmp_fml, " + ", cond) 
  tmp_fml <- as.formula(tmp_fml)
  tmp_outcome <- as.formula(paste0("~ ", outcome))
  out <- svyby(tmp_outcome, 
               by = tmp_fml, 
               FUN = svymean, design = svyds)
  out <- data.table(out)
  setnames(out, cancerx, "tmp")
  out <- out[tmp == 1]
  out[, `:=` (tmp = NULL, 
              cancer_type = cancerx)]
}

svyds<- svydesign(
  id = ~varpsu,
  strata = ~varstr,
  weights = ~perwt,
  data = fyc_all,
  survey.lonely.psu="adjust", 
  nest = TRUE)

svy_totexp_can <- mclapply(c(can_cols, "no_cancer"), function(x, outcome = "totexp_gdp", cond = "age_two_group") {
  calc_weighted_avg_exp(cancerx = x, outcome = outcome, cond = cond, svyds = svyds)
}, mc.cores = 5)

cancer_recode_key <- c(cancer_names, "no cancer")
names(cancer_recode_key) <- c(can_cols, "no_cancer")

svy_totexp_can <- rbindlist(svy_totexp_can)
svy_totexp_can$cancer_type <- recode(svy_totexp_can$cancer_type, !!!cancer_recode_key)
svy_totexp_can$cancer_type <- factor(svy_totexp_can$cancer_type, levels = cancer_recode_key)
svy_totexp_can <- svy_totexp_can[age_two_group != "0-17"]

wt_totexp_by_svy_can <- ggplot(data = svy_totexp_can, 
                          aes(y = totexp_gdp, x = svy_year)) + 
  geom_bar(color = "yellowgreen", fill = "white", stat = "identity", 
           position = "dodge", width = 0.8, size = 0.8) + 
  geom_errorbar(aes(ymin = (totexp_gdp - 2 * se), ymax = (totexp_gdp + 2 * se)),
                color = "yellowgreen", width = 0, position = position_dodge(0.75)) +  
  facet_wrap(.~cancer_type, ncol = 3, scales = "free_y") +
  xlab("Survey Year") + 
  ylab("$") + 
  ggtitle("Total Expense of Cancer Survivors 2008-2018 (weighted)") + 
  scale_x_continuous(breaks = seq(2008, 2018, 1)) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 12, face = "bold", colour = "gray20"), 
        axis.text.x = element_text(size = 10, angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.position = "bottom")

svy_totexp_can[, n_se := paste0(format(round(totexp_gdp), big.mark = ","), "<br>(",
                             format(round(se), big.mark = ","), ")")]
svy_totexp_can_wide <- dcast(svy_totexp_can, svy_year ~ cancer_type, value.var = "n_se")

output_list$wt_totexp_by_svy_can <- svy_totexp_can_wide

plot_list$wt_totexp_by_svy_can <- wt_totexp_by_svy_can


svy_totexp_age_can <- mclapply(c(can_cols, "no_cancer"), function(x, outcome = "totexp_gdp", cond = "age65") {
  calc_weighted_avg_exp(cancerx = x, outcome = outcome, cond = cond, svyds = svyds)
}, mc.cores = 5)

svy_totexp_age_can <- rbindlist(svy_totexp_age_can)
svy_totexp_age_can$cancer_type <- recode(svy_totexp_age_can$cancer_type, !!!cancer_recode_key)
svy_totexp_age_can$cancer_type <- factor(svy_totexp_age_can$cancer_type, levels = cancer_recode_key)
svy_totexp_age_can <- svy_totexp_age_can[age65 != "0-17"]
setnames(svy_totexp_age_can, "age65", "age_group")


wt_totexp_by_age_can <- ggplot(data = svy_totexp_age_can, 
                               aes(y = totexp_gdp, x = svy_year)) + 
  geom_bar(aes(color = age_group), fill = "white", stat = "identity", 
           position = "dodge", width = 0.8, size = 0.8) + 
  geom_errorbar(aes(ymin = (totexp_gdp - 2 * se), ymax = (totexp_gdp + 2 * se), 
                    color = age_group),
                width = 0, position = position_dodge(0.75)) +  
  facet_wrap(.~cancer_type, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_color[-1]) + 
  xlab("Survey Year") + 
  ylab("$") + 
  ggtitle("Total Expense of Cancer Survivors 2008-2018 (weighted)") + 
  scale_x_continuous(breaks = seq(2008, 2018, 1)) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 12, face = "bold", colour = "gray20"), 
        axis.text.x = element_text(size = 10, angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.position = "bottom")

svy_totexp_age_can[, v_se := paste0(format(round(totexp_gdp), big.mark = ","), "<br>(",
                                format(round(se), big.mark = ","), ")")]
svy_totexp_age_can_wide <- dcast(svy_totexp_age_can, 
                                 svy_year + age_group ~ cancer_type, 
                                 value.var = "v_se")

output_list$wt_totexp_by_age_can <- svy_totexp_age_can_wide

plot_list$wt_totexp_by_age_can <- wt_totexp_by_age_can


## Medicare payment

svy_totmcr_can <- mclapply(c(can_cols, "no_cancer"), function(x, outcome = "totmcr_pce", cond = "age_two_group") {
  calc_weighted_avg_exp(cancerx = x, outcome = outcome, cond = cond, svyds = svyds)
}, mc.cores = 5)

cancer_recode_key <- c(cancer_names, "no cancer")
names(cancer_recode_key) <- c(can_cols, "no_cancer")

svy_totmcr_can <- rbindlist(svy_totmcr_can)
svy_totmcr_can$cancer_type <- recode(svy_totmcr_can$cancer_type, !!!cancer_recode_key)
svy_totmcr_can$cancer_type <- factor(svy_totmcr_can$cancer_type, levels = cancer_recode_key)
svy_totmcr_can <- svy_totmcr_can[age_two_group != "0-17"]

wt_totmcr_by_svy_can <- ggplot(data = svy_totmcr_can, 
                               aes(y = totmcr_pce, x = svy_year)) + 
  geom_bar(color = "yellowgreen", fill = "white", stat = "identity", 
           position = "dodge", width = 0.8, size = 0.8) + 
  geom_errorbar(aes(ymin = (totmcr_pce - 2 * se), ymax = (totmcr_pce + 2 * se)),
                color = "yellowgreen", width = 0, position = position_dodge(0.75)) +  
  facet_wrap(.~cancer_type, ncol = 3, scales = "free_y") +
  xlab("Survey Year") + 
  ylab("$") + 
  ggtitle("Medicare Payment among Cancer Survivors 2008-2018 (weighted)") + 
  scale_x_continuous(breaks = seq(2008, 2018, 1)) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 12, face = "bold", colour = "gray20"), 
        axis.text.x = element_text(size = 10, angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.position = "bottom")

svy_totmcr_can[, n_se := paste0(format(round(totmcr_pce), big.mark = ","), "<br>(",
                                format(round(se), big.mark = ","), ")")]
svy_totmcr_can_wide <- dcast(svy_totmcr_can, svy_year ~ cancer_type, value.var = "n_se")

output_list$wt_totmcr_by_svy_can <- svy_totmcr_can_wide

plot_list$wt_totmcr_by_svy_can <- wt_totmcr_by_svy_can


svy_totmcr_age_can <- mclapply(c(can_cols, "no_cancer"), function(x, outcome = "totmcr_pce", cond = "age65") {
  calc_weighted_avg_exp(cancerx = x, outcome = outcome, cond = cond, svyds = svyds)
}, mc.cores = 5)

svy_totmcr_age_can <- rbindlist(svy_totmcr_age_can)
svy_totmcr_age_can$cancer_type <- recode(svy_totmcr_age_can$cancer_type, !!!cancer_recode_key)
svy_totmcr_age_can$cancer_type <- factor(svy_totmcr_age_can$cancer_type, levels = cancer_recode_key)
svy_totmcr_age_can <- svy_totmcr_age_can[age65 != "0-17"]
setnames(svy_totmcr_age_can, "age65", "age_group")


wt_totmcr_by_age_can <- ggplot(data = svy_totmcr_age_can, 
                               aes(y = totmcr_pce, x = svy_year)) + 
  geom_bar(aes(color = age_group), fill = "white", stat = "identity", 
           position = "dodge", width = 0.8, size = 0.8) + 
  geom_errorbar(aes(ymin = (totmcr_pce - 2 * se), ymax = (totmcr_pce + 2 * se), 
                    color = age_group),
                width = 0, position = position_dodge(0.75)) +  
  facet_wrap(.~cancer_type, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_color[-1]) + 
  xlab("Survey Year") + 
  ylab("$") + 
  ggtitle("Medicare Payment among Cancer Survivors 2008-2018 (weighted)") + 
  scale_x_continuous(breaks = seq(2008, 2018, 1)) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 12, face = "bold", colour = "gray20"), 
        axis.text.x = element_text(size = 10, angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.position = "bottom")

svy_totmcr_age_can[, v_se := paste0(format(round(totmcr_pce), big.mark = ","), "<br>(",
                                    format(round(se), big.mark = ","), ")")]

svy_totmcr_age_can_wide <- dcast(svy_totmcr_age_can, 
                                 svy_year + age_group ~ cancer_type, 
                                 value.var = "v_se")

output_list$wt_totmcr_by_age_can <- svy_totmcr_age_can_wide

plot_list$wt_totmcr_by_age_can <- wt_totmcr_by_age_can


## Medicaid payment

svy_totmcd_can <- mclapply(c(can_cols, "no_cancer"), function(x, outcome = "totmcd_pce", cond = "age_two_group") {
  calc_weighted_avg_exp(cancerx = x, outcome = outcome, cond = cond, svyds = svyds)
}, mc.cores = 5)

cancer_recode_key <- c(cancer_names, "no cancer")
names(cancer_recode_key) <- c(can_cols, "no_cancer")

svy_totmcd_can <- rbindlist(svy_totmcd_can)
svy_totmcd_can$cancer_type <- recode(svy_totmcd_can$cancer_type, !!!cancer_recode_key)
svy_totmcd_can$cancer_type <- factor(svy_totmcd_can$cancer_type, levels = cancer_recode_key)
svy_totmcd_can <- svy_totmcd_can[age_two_group != "0-17"]

wt_totmcd_by_svy_can <- ggplot(data = svy_totmcd_can, 
                               aes(y = totmcd_pce, x = svy_year)) + 
  geom_bar(color = "yellowgreen", fill = "white", stat = "identity", 
           position = "dodge", width = 0.8, size = 0.8) + 
  geom_errorbar(aes(ymin = (totmcd_pce - 2 * se), ymax = (totmcd_pce + 2 * se)),
                color = "yellowgreen", width = 0, position = position_dodge(0.75)) +  
  facet_wrap(.~cancer_type, ncol = 3, scales = "free_y") +
  xlab("Survey Year") + 
  ylab("$") + 
  ggtitle("Medicaid Payment among Cancer Survivors 2008-2018 (weighted)") + 
  scale_x_continuous(breaks = seq(2008, 2018, 1)) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 12, face = "bold", colour = "gray20"), 
        axis.text.x = element_text(size = 10, angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.position = "bottom")

svy_totmcd_can[, n_se := paste0(format(round(totmcd_pce), big.mark = ","), "<br>(",
                                format(round(se), big.mark = ","), ")")]
svy_totmcd_can_wide <- dcast(svy_totmcd_can, svy_year ~ cancer_type, value.var = "n_se")

output_list$wt_totmcd_by_svy_can <- svy_totmcd_can_wide

plot_list$wt_totmcd_by_svy_can <- wt_totmcd_by_svy_can


svy_totmcd_age_can <- mclapply(c(can_cols, "no_cancer"), function(x, outcome = "totmcd_pce", cond = "age65") {
  calc_weighted_avg_exp(cancerx = x, outcome = outcome, cond = cond, svyds = svyds)
}, mc.cores = 5)

svy_totmcd_age_can <- rbindlist(svy_totmcd_age_can)
svy_totmcd_age_can$cancer_type <- recode(svy_totmcd_age_can$cancer_type, !!!cancer_recode_key)
svy_totmcd_age_can$cancer_type <- factor(svy_totmcd_age_can$cancer_type, levels = cancer_recode_key)
svy_totmcd_age_can <- svy_totmcd_age_can[age65 != "0-17"]
setnames(svy_totmcd_age_can, "age65", "age_group")


wt_totmcd_by_age_can <- ggplot(data = svy_totmcd_age_can, 
                               aes(y = totmcd_pce, x = svy_year)) + 
  geom_bar(aes(color = age_group), fill = "white", stat = "identity", 
           position = "dodge", width = 0.8, size = 0.8) + 
  geom_errorbar(aes(ymin = (totmcd_pce - 2 * se), ymax = (totmcd_pce + 2 * se), 
                    color = age_group),
                width = 0, position = position_dodge(0.75)) +  
  facet_wrap(.~cancer_type, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_color[-1]) + 
  xlab("Survey Year") + 
  ylab("$") + 
  ggtitle("Medicaid Payment among Cancer Survivors 2008-2018 (weighted)") + 
  scale_x_continuous(breaks = seq(2008, 2018, 1)) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 12, face = "bold", colour = "gray20"), 
        axis.text.x = element_text(size = 10, angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.position = "bottom")

svy_totmcd_age_can[, v_se := paste0(format(round(totmcd_pce), big.mark = ","), "<br>(",
                                    format(round(se), big.mark = ","), ")")]

svy_totmcd_age_can_wide <- dcast(svy_totmcd_age_can, 
                                 svy_year + age_group ~ cancer_type, 
                                 value.var = "v_se")

output_list$wt_totmcd_by_age_can <- svy_totmcd_age_can_wide

plot_list$wt_totmcd_by_age_can <- wt_totmcd_by_age_can



## Adjust loss of work days

svy_ddnwrk_can <- mclapply(c(can_cols, "no_cancer"), function(x, outcome = "ddnwrk_recode", cond = "age_two_group") {
  calc_weighted_avg_exp(cancerx = x, outcome = outcome, cond = cond, svyds = svyds)
}, mc.cores = 5)

cancer_recode_key <- c(cancer_names, "no cancer")
names(cancer_recode_key) <- c(can_cols, "no_cancer")

svy_ddnwrk_can <- rbindlist(svy_ddnwrk_can)
svy_ddnwrk_can$cancer_type <- recode(svy_ddnwrk_can$cancer_type, !!!cancer_recode_key)
svy_ddnwrk_can$cancer_type <- factor(svy_ddnwrk_can$cancer_type, levels = cancer_recode_key)
svy_ddnwrk_can <- svy_ddnwrk_can[age_two_group != "0-17"]

wt_ddnwrk_by_svy_can <- ggplot(data = svy_ddnwrk_can, 
                               aes(y = ddnwrk_recode, x = svy_year)) + 
  geom_bar(color = "yellowgreen", fill = "white", stat = "identity", 
           position = "dodge", width = 0.8, size = 0.8) + 
  geom_errorbar(aes(ymin = (ddnwrk_recode - 2 * se), ymax = (ddnwrk_recode + 2 * se)),
                color = "yellowgreen", width = 0, position = position_dodge(0.75)) +  
  facet_wrap(.~cancer_type, ncol = 3, scales = "free_y") +
  xlab("Survey Year") + 
  ylab("days") + 
  ggtitle("Days Missed Work among Cancer Survivors 2008-2018 (weighted)") + 
  scale_x_continuous(breaks = seq(2008, 2018, 1)) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 12, face = "bold", colour = "gray20"), 
        axis.text.x = element_text(size = 10, angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.position = "bottom")

svy_ddnwrk_can[, n_se := paste0(format(round(ddnwrk_recode), big.mark = ","), "<br>(",
                                format(round(se), big.mark = ","), ")")]
svy_ddnwrk_can_wide <- dcast(svy_ddnwrk_can, svy_year ~ cancer_type, value.var = "n_se")

output_list$wt_ddnwrk_by_svy_can <- svy_ddnwrk_can_wide

plot_list$wt_ddnwrk_by_svy_can <- wt_ddnwrk_by_svy_can


svy_ddnwrk_age_can <- mclapply(c(can_cols, "no_cancer"), function(x, outcome = "ddnwrk_recode", cond = "age65") {
  calc_weighted_avg_exp(cancerx = x, outcome = outcome, cond = cond, svyds = svyds)
}, mc.cores = 5)

svy_ddnwrk_age_can <- rbindlist(svy_ddnwrk_age_can)
svy_ddnwrk_age_can$cancer_type <- recode(svy_ddnwrk_age_can$cancer_type, !!!cancer_recode_key)
svy_ddnwrk_age_can$cancer_type <- factor(svy_ddnwrk_age_can$cancer_type, levels = cancer_recode_key)
svy_ddnwrk_age_can <- svy_ddnwrk_age_can[age65 != "0-17"]
setnames(svy_ddnwrk_age_can, "age65", "age_group")


wt_ddnwrk_by_age_can <- ggplot(data = svy_ddnwrk_age_can, 
                               aes(y = ddnwrk_recode, x = svy_year)) + 
  geom_bar(aes(color = age_group), fill = "white", stat = "identity", 
           position = "dodge", width = 0.8, size = 0.8) + 
  geom_errorbar(aes(ymin = (ddnwrk_recode - 2 * se), ymax = (ddnwrk_recode + 2 * se), 
                    color = age_group),
                width = 0, position = position_dodge(0.75)) +  
  facet_wrap(.~cancer_type, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_color[-1]) + 
  xlab("Survey Year") + 
  ylab("$") + 
  ggtitle("Days Missed Work among Cancer Survivors 2008-2018 (weighted)") + 
  scale_x_continuous(breaks = seq(2008, 2018, 1)) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text.x = element_text(size = 12, face = "bold", colour = "gray20"), 
        axis.text.x = element_text(size = 10, angle = 60, hjust = 1), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.position = "bottom")

svy_ddnwrk_age_can[, v_se := paste0(format(round(ddnwrk_recode), big.mark = ","), "<br>(",
                                    format(round(se), big.mark = ","), ")")]
svy_ddnwrk_age_can_wide <- dcast(svy_ddnwrk_age_can, 
                                 svy_year + age_group ~ cancer_type, 
                                 value.var = "v_se")

output_list$wt_ddnwrk_by_age_can <- svy_ddnwrk_age_can_wide

plot_list$wt_ddnwrk_by_age_can <- wt_ddnwrk_by_age_can


saveRDS(plot_list, "Results/plot_list.RDS")
saveRDS(output_list, "Results/summary_out_list.RDS")


