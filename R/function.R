library(dplyr)
library(foreign)
library(rlang)

get_MEPS_annual <- function(yr, data_path) {
  
  tmpWT <- paste0("PERWT", yr, "F")
  tmpAGE <- paste0("AGE", yr, "X")
  
  #### load data
  FYC <- readRDS(paste0(data_path, "FYC/yr20", yr, ".rds")) %>%
    filter(UQ(sym(tmpAGE)) >= 18) %>%
    mutate(sex_cate = ifelse(SEX == 1, "Male", "Female"), 
           age_cate = ifelse(UQ(sym(tmpAGE)) >= 18 & UQ(sym(tmpAGE)) < 65, "18-64", "65+"), 
           tmp_cancer = ifelse(CANCERDX == 1, "yes", 
                                    ifelse(CANCERDX == 2, "no", "other")), 
           melanoma = ifelse(CAMELANO == 1, 1, 0), 
           non_melanoma = ifelse(CASKINDK == 1 | CASKINNM == 1, 1, 0), 
           cancer_survivor = ifelse(CANCERDX == 1, "yes", 
                                    ifelse(CANCERDX == 2, "no", "other"))) %>%
    mutate(cancer_survivor = factor(cancer_survivor, levels = c("no", "yes", "other")), 
           sex_cate = factor(sex_cate, levels = c("Male", "Female")), 
           age_cate = factor(age_cate, levels = c("18-64", "65+"))) %>% 
    mutate(person = 1)
  
  #### creating mutual exclusive cancer type
  FYC$cancer_type <- ifelse(FYC$melanoma == 1 & FYC$non_melanoma == 1, "both melanoma & non-melanoma", 
                            ifelse(FYC$melanoma == 1 & FYC$non_melanoma == 0, "melanoma skin cancer", 
                                   ifelse(FYC$melanoma == 0 & FYC$non_melanoma == 1, "non-melanoma skin cancer", 
                                          "other cancer sites")))
  FYC$cancer_type[FYC$cancer_survivor == "no"] <- "cancer_free" 
  FYC$cancer_type <- factor(FYC$cancer_type, levels = c("cancer_free", "other cancer sites", "non-melanoma skin cancer", 
                                                        "melanoma skin cancer", "both melanoma & non-melanoma"))
  
  #### creating race variable
  FYC$race <- ifelse(FYC$RACEWX == 1 & FYC$HISPANX != 1, "Non-Hispanic White", 
                     ifelse(FYC$RACEBX == 1 & FYC$HISPANX != 1, "Non-Hispanic Black", 
                            ifelse(FYC$HISPANX == 1, "Hispanic", "Non-Hispanic Other")))
  FYC$race <- factor(FYC$race, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Non-Hispanic Other"))
  
  #### Expenses
  ## By source of payment
  FYC <- FYC %>% 
    mutate(XP_tot = UQ(sym(paste0("TOTEXP", yr))), # total expenditure
           XP_oop = UQ(sym(paste0("TOTSLF", yr))), # out of pocket
           XP_medicare = UQ(sym(paste0("TOTMCR", yr))), # medicare
           XP_medicaid = UQ(sym(paste0("TOTMCD", yr))), # medicaid
           XP_private = UQ(sym(paste0("TOTPRV", yr))), # private insurance
           XP_other = XP_tot - XP_oop - XP_medicare - XP_medicaid - XP_private, # other sources
           XP_other = replace(XP_other, XP_other < 0, 0)) 
  
  ## By type of services
  FYC <- FYC %>%
    mutate(RX_XP = UQ(sym(paste0("RXEXP", yr))), # prescribed medicine expenditure
           IP_XP = UQ(sym(paste0("IPTEXP", yr))), # inpatient stay expenditure
           AM_XP = UQ(sym(paste0("OPTEXP", yr))) + UQ(sym(paste0("OBVEXP", yr))), # ambulatory care expenditure
           OT_XP = XP_tot - RX_XP - IP_XP - AM_XP)  # other type of expenditure
  
  
  # Aggregate to person-level, by Condition -------------------------------------
  cond_pers <- all_events %>%
    group_by(DUPERSID, VARSTR, VARPSU, Condition) %>%
    summarize(
      PERWT = mean(UQ(sym(tmpWT)), na.rm = T),
      maxWT = max(UQ(sym(tmpWT)), na.rm = T), 
      minWT = min(UQ(sym(tmpWT)), na.rm = T), 
      pers_XP = sum(XPX, na.rm = T),
      pers_XP_oop = sum(XP_oop, na.rm = T), 
      pers_XP_medicare = sum(XP_medicare, na.rm = T), 
      pers_XP_medicaid = sum(XP_medicaid, na.rm = T), 
      pers_XP_private = sum(XP_private, na.rm = T), 
      pers_XP_other = sum(XP_other, na.rm = T), 
      n_events = sum(n_events, na.rm = T)) %>%
    ungroup %>%
    mutate(persons = 1, 
           year = paste0("20", yr))
  
  
  ## Merge demographics data
  cond_pers <- cond_pers %>% 
    left_join(., FYC %>% select(DUPERSID, sex_cate, age_cate), by = "DUPERSID") %>%
    filter(!is.na(age_cate))
  
  
  # Aggregate to person-level, by data sources -------------------------------------
  src_pers <- all_events %>%
    group_by(data, DUPERSID, VARSTR, VARPSU, Condition) %>%
    summarize(
      PERWT = mean(UQ(sym(tmpWT)), na.rm = T),
      maxWT = max(UQ(sym(tmpWT)), na.rm = T), 
      minWT = min(UQ(sym(tmpWT)), na.rm = T), 
      pers_XP = sum(XPX, na.rm = T)) %>%
    ungroup %>%
    mutate(persons = 1, 
           year = paste0("20", yr))
  
  
  ## Merge demographics data
  src_pers <- src_pers %>% 
    left_join(., FYC %>% select(DUPERSID, sex_cate, age_cate), by = "DUPERSID") %>%
    filter(!is.na(age_cate))
  
  
  return(list(cond_pers = cond_pers, src_pers = src_pers))
}
