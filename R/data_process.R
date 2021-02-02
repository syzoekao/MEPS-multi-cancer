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
yr_vec <- paste0(c(2:18))
yr_vec <- ifelse(nchar(yr_vec) == 1, paste0("0", yr_vec), yr_vec)

yr_df <- mclapply(yr_vec, function(x) {
  get_MEPS_annual(yr = x, data_path = data_path)
}, mc.cores = 5)

cond_df <- rbindlist(lapply(yr_df, "[[", 1))
cond_df[, `:=` (year = as.numeric(year))]
cond_df[, year_cate := ifelse(year <= 2006, "2002-2006", 
                             ifelse(year > 2006 & year <= 2011, "2007-2011", "2012-2018"))]
cond_df[, PERWT_new := ifelse(year_cate == "2002-2006", PERWT / (2006 - 2002 + 1), 
                              ifelse(year_cate == "2007-2011", PERWT / (2011 - 2007 + 1), 
                                     PERWT / (2018 - 2012 + 1)))]


src_df <- rbindlist(lapply(yr_df, "[[", 1))
src_df[, `:=` (year = as.numeric(year))]
src_df[, year_cate := ifelse(year <= 2006, "2002-2006", 
                              ifelse(year > 2006 & year <= 2011, "2007-2011", "2012-2018"))]
src_df[, PERWT_new := ifelse(year_cate == "2002-2006", PERWT / (2006 - 2002 + 1), 
                              ifelse(year_cate == "2007-2011", PERWT / (2011 - 2007 + 1), 
                                     PERWT / (2018 - 2012 + 1)))]

## Define survey design and calculate estimates --------------------------------

PERSdsgn <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT,
  data = cond_df,
  survey.lonely.psu="adjust", 
  nest = TRUE)


# Totals (people, events, expenditures)
raw_totals <- cond_df %>% 
  group_by(year, Condition) %>%
  summarize(persons = n()) %>%
  ungroup()

g1 <- ggplot(data = raw_totals) + 
  geom_point(aes(x = year, y = persons, color = Condition), 
             shape = 21, stroke = 1.5, size = 2) +
  scale_y_continuous(labels = scales::comma) + 
  ggtitle("Number of individuals treated with cancer (unweighted)") +
  theme_bw() + 
  theme(legend.position = "bottom")

totals <- svyby(~persons, 
                by = ~year + Condition, 
                FUN = svytotal, design = PERSdsgn)

g2 <- ggplot(data = totals) + 
  geom_point(aes(x = year, y = persons, color = Condition), 
             shape = 21, stroke = 1.5, size = 2) +
  scale_y_continuous(labels = scales::comma) + 
  ggtitle("Number of individuals treated with cancer (weighted)") +
  theme_bw() + 
  theme(legend.position = "bottom")


# Totals (people, events, expenditures)
PERSdsgn <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT_new,
  data = cond_df,
  survey.lonely.psu="adjust", 
  nest = TRUE)


totals <- svyby(~persons, 
                by = ~year_cate + Condition, 
                FUN = svytotal, design = PERSdsgn)

PERSdsgn_skin <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT_new,
  data = cond_df %>% filter(Condition != "other cancer sites"),
  survey.lonely.psu="adjust", 
  nest = TRUE)

totals_skin <- svyby(~persons, 
                     by = ~year_cate, 
                     FUN = svytotal, design = PERSdsgn_skin)
totals_skin <- totals_skin %>% 
  mutate(Condition = "all skin cancer")

totals <- bind_rows(totals, totals_skin) %>%
  arrange(Condition, year_cate) %>%
  mutate(rownames = NULL)

saveRDS(list(fig = g1, totals = totals), 
        "Results/results.RDS")

# Mean expenditure per person with care
means <- svyby(~pers_XP,
               by = ~year_cate + Condition, 
               FUN = svymean, design = PERSdsgn)

# Median expenditure per person with care
medians <- svyby(~pers_XP,
                 by = ~year_cate + Condition, 
                 FUN = svyquantile, quantiles = c(0.5), 
                 design = PERSdsgn, ci = TRUE)

