library(dplyr)
library(foreign)
library(rlang)
library(data.table)

age_color <- c("yellowgreen", "deepskyblue", "tomato")


exp_ls <- c("totexp_gdp", "totslf_pce", "totprv_pce", "totmcr_pce", "totmcd_pce", 
            "tototr_pce", "ambexp_pce", "iptexp_pce", "rxexp_pce", "tototr2_pce")
exp_name_ls <- c("total health care expenditures ($)", "OOP ($)", 
                 "expenditures paid by private insurance ($)", "expenditures paid by Medicare ($)", 
                 "expenditures paid by Medicaid ($)", "expenditures paid by other sources ($)", 
                 "expenditures for ambulatory care ($)", "expenditures for inpatient care ($)", 
                 "expenditures for prescription medications ($)", "expenditures for other services ($)")


quote_list <- function(x) {
  lapply(x, function(x) sym(toupper(x)))
}

split_string <- function(x) {
  strsplit(x, " ")[[1]]
}

split_quote_list <- function(x) {
  split_set <- strsplit(x, " ")[[1]]
  quote_list(split_set)
}

get_fyc_annual <- function(yr, data_path, mutual_exclude_can = TRUE) {
  # data_path <- "MEPS data/"
  # yr <- "17"
  print(yr)
  
  svy_rd <- c(31, 42, 53)
  
  tmpAGE <- sym(paste0("AGE", yr, "X"))
  
  all_vars <- c()
  
  #### ID and Weights
  idwt_set <- "DUID DUPERSID PANEL FAMID08 PERWT08F FAMWT08F FAMWT08C SAQWT08F DIABW08F VARSTR VARPSU"
  idwt_set <- gsub("08", yr, idwt_set)
  idwt_set <- split_string(idwt_set)
  all_vars <- c(all_vars, idwt_set)
  
  #### Reference period dates
  ref_d_set <- c(paste0("BEGRFM", svy_rd), 
                 paste0("BEGRFD", svy_rd), 
                 paste0("BEGRFY", svy_rd), 
                 paste0("ENDRFM", c(svy_rd, yr)), 
                 paste0("ENDRFD", c(svy_rd, yr)), 
                 paste0("ENDRFY", c(svy_rd, yr)))
  all_vars <- c(all_vars, ref_d_set)
  
  #### Demographics
  dem_set <- c(paste0("REGION", c(svy_rd, yr)), "SEX", 
               "RACEX", "HISPANX", "RACETHNX", "RACEWX", "RACEBX",
               paste0("AGE", c(svy_rd, yr), "X"), "EDUCYR", "EDRECODE", 
               paste0("TTLP", yr, "X"), paste0(c("FAMINC", "POVLEV", "POVCAT"), yr), 
               paste0("MARRY", c(svy_rd, yr), "X"), paste0("MSA", c(svy_rd, yr)))
  all_vars <- c(all_vars, dem_set)
  
  #### Perceived health status 
  hlth_status <- c(paste0("RTHLTH", svy_rd), paste0("MNHLTH", svy_rd))
  all_vars <- c(all_vars, hlth_status)
  
  #### Priority conditions
  hlth_cond <- "HIBPDX CHDDX ANGIDX MIDX OHRTDX STRKDX EMPHDX CHBRON31 CHBRON53 CHOLDX CANCERDX DIABDX JTPAIN31 JTPAIN53 ARTHDX ASTHDX ASSTIL31 ASSTIL53 ASATAK31 ASATAK53 ADHDADDX"
  hlth_cond <- split_string(hlth_cond)
  all_vars <- c(all_vars, hlth_cond)
  
  #### Selected cancer sites
  cancer_name <- c("bladdr", "bone", "brain", "esoph", "kidney", "calarynx", 
                   "leukem", "liver", "mouth", "muscle", "cerdx", "ovary", 
                   "pancrs", "stomch", "testis", "throat", "thyrod", 
                   "uterus", "ncerdx", "other", 
                   "breast", "cervix", "colon", "rectum", "lung", "lymph", 
                   "melano", "skinnm", "skindk", "prosta")
  cancer_name_abv <- c("brst", "cerv", "colo", "rect", "lung", "leuk", 
                       "mela", "sknm", "skdk", "prst")
  cancer_set <- toupper(paste0("ca", cancer_name))
  cancer_abv <- toupper(paste0(cancer_name_abv, "aged"))
  all_vars <- c(all_vars, cancer_set)
  all_vars <- c(all_vars, cancer_abv)
  
  #### Preventive care variables
  prev_care_set <- "DENTCK53 BPCHEK53 BPMONT53 CHOLCK53 CHECK53 FLUSHT53 PSA53 HYSTER53 PAPSMR53 BRSTEX53 MAMOGR53 BMINDX53 NOFAT53 EXRCIS53 SEATBE53 ASPRIN53 NOASPR53 STOMCH53 LSTETH53"
  prev_care_set <- split_string(prev_care_set)
  all_vars <- c(all_vars, prev_care_set)
  
  #### Self-Administered Questionnaire
  saq_set <- "SAQELIG ADPRX42 ADCMPM42 ADCMPD42 ADCMPY42 ADILCR42 ADILWW42 ADRTCR42 ADRTWW42 ADAPPT42 ADNDCR42 ADEGMC42 ADSMOK42 ADNSMK42 ADDRBP42 ADSPEC42 ADSPRF42 PCS42 MCS42 K6SUM42 PHQ242 ADINSA42 ADINSB42 ADRISK42 ADOVER42"
  saq_set <- split_string(saq_set)
  all_vars <- c(all_vars, saq_set)
  
  #### Access to care
  access_care <- "ACCELI42 HAVEUS42 YNOUSC42 NOREAS42 SELDSI42 NEWARE42 DKWHRU42 USCNOT42 PERSLA42 DIFFPL42 INSRPL42 MYSELF42 CARECO42 OTHREA42"
  access_care <- split_string(access_care)
  all_vars <- c(all_vars, access_care)
  
  #### Additional reason of no usual source of care (USC)
  no_usc <- "MDUNAB42 DNUNAB42 PMUNAB42 MDDLAY42 DNDLAY42 PMDLAY42 MDUNRS42 DNUNRS42 PMUNRS42 MDDLRS42 DNDLRS42 PMDLRS42"
  no_usc <- split_string(no_usc)
  all_vars <- c(all_vars, no_usc)
  
  #### Insurance
  ins_set <- "INSCOV DNTINS PMDINS MCREV MCDEV OPAEV OPBEV PRVEV TRIEV "
  ins_set <- gsub(" ", paste0(yr, " "), ins_set)
  ins_set <- split_string(ins_set)
  all_vars <- c(all_vars, ins_set)
  
  #### Utilization 
  utltion <- "OBTOTV08 OPTOTV08 ERTOT08 IPDIS08 IPNGTD08 DVTOT08 RXTOT08 OBVEXP08 OPTEXP08 OPFEXP08 OPDEXP08 IPTEXP08 RXEXP08 ERTEXP08 HHAEXP08 DVTEXP08 VISEXP08 OTHEXP08"
  utltion <- gsub("08", yr, utltion)
  utltion <- split_string(utltion)
  all_vars <- c(all_vars, utltion)
  
  #### Expenditure
  exp_set <- "TOTVA08 TOTTRI08 TOTOFD08 TOTSTL08 TOTWCP08 TOTOPR08 TOTOPU08 TOTOSR08 TOTPTR08 TOTOTH08 TOTTCH08 TOTEXP08 TOTSLF08 TOTPRV08 TOTMCR08 TOTMCD08 OBVEXP08 OPFEXP08 IPTEXP08 RXEXP08 ERTEXP08 HHAEXP08 HHNEXP08 DVTEXP08 VISEXP08 OTHEXP08"
  exp_set <- gsub("08", yr, exp_set)
  exp_set <- split_string(exp_set)
  all_vars <- c(all_vars, exp_set)
  
  #### Missed workdays
  miss_wd_set <- c(paste0("DDNWRK", c(svy_rd, yr)), paste0("DDBDYS", svy_rd))
  all_vars <- c(all_vars, miss_wd_set)
  
  #### Employment disability
  disability_set <- c("EVRWRK", paste0("UNABLE", svy_rd), 
                      paste0("NWK", svy_rd), 
                      paste0("EMPST", svy_rd), 
                      "EMPST", "EMPST2")
  all_vars <- c(all_vars, disability_set)
  
  all_vars <- unique(all_vars)
  
  #### load data
  FYC <- readRDS(paste0(data_path, "FYC/yr20", yr, ".rds"))
  
  #### Keep variables that were contained in the year of FYC
  var_sub <- colnames(FYC)[colnames(FYC) %in% all_vars]
  var_na <- all_vars[!(all_vars %in% var_sub)] # variables not available
  var_sub <- quote_list(var_sub)
  
  FYC <- FYC %>%
    select(!!!var_sub)
  
  #### Create columns that are not available in the year of FYC
  FYC[, var_na] <- NA
  
  #### Replace survey year with "" from column names 
  #### Make all column names lowercase
  c_names <- gsub(yr, "", colnames(FYC))
  c_names <- tolower(c_names)
  colnames(FYC) <- c_names
  
  #### Recode or create variables
  ### Age
  FYC <- FYC %>%
    mutate(age = agex,
           age = ifelse(age < 0, NA, age), 
           ageg = cut(age, 
                      breaks = c(-Inf, 18, 50, 65, 80, Inf), 
                      labels = c("0-17", "18-49", "50-64", "65-79", ">=80"), 
                      right = F), 
           age65 = cut(age, 
                       breaks = c(-Inf, 18, 65, Inf), 
                       labels = c("0-17", "18-64", ">=65"), 
                       right = F))
  
  ### Cancer types and time since dx
  FYC <- FYC %>%
    mutate_at(tolower(cancer_set), function(x) {
      ifelse(x == 2, 0, 
             ifelse(x < 0, NA, x))
    }) %>% 
    mutate(can_colrec = ifelse(cacolon == 1 | carectum == 1, 1, 0), 
           can_colrec = tidyr::replace_na(can_colrec, 0), 
           can_breast = ifelse(cabreast == 1, 1, 0), 
           can_breast = tidyr::replace_na(can_breast, 0), 
           can_cervix = ifelse(cacervix == 1, 1, 0), 
           can_cervix = tidyr::replace_na(can_cervix, 0), 
           can_lung = ifelse(calung == 1, 1, 0), 
           can_lung = tidyr::replace_na(can_lung, 0), 
           can_melano = ifelse(camelano == 1, 1, 0), 
           can_melano = tidyr::replace_na(can_melano, 0), 
           can_skinnm = ifelse(caskinnm == 1 | caskindk == 1, 1, 0), # combine non-melanoma and unknown skin cancer
           can_skinnm = tidyr::replace_na(can_skinnm, 0), 
           can_nonmel = ifelse(caskinnm == 1, 1, 0), 
           can_nonmel = tidyr::replace_na(can_nonmel, 0), 
           can_skindk = ifelse(caskindk == 1, 1, 0), 
           can_skindk = tidyr::replace_na(can_skindk, 0), 
           can_allskin = ifelse(camelano == 1 | caskinnm == 1 | caskindk == 1, 1, 0), 
           can_allskin = tidyr::replace_na(can_allskin, 0), 
           can_prosta = ifelse(caprosta == 1, 1, 0), 
           can_prosta = tidyr::replace_na(can_prosta, 0), 
           can_any = ifelse(cancerdx == 1, 1, 0), 
           can_any = tidyr::replace_na(can_any, 0))
  
  FYC <- FYC %>% 
    rowwise() %>% 
    mutate(can_mult = ifelse(sum(c_across(tolower(c(cancer_set[!cancer_set %in% c("CANCERDX", "CARECTUM", "CACOLON")], "can_colrec"))), 
                               na.rm = T) > 1, 1, 0)) %>%
    ungroup()
    
  
  if (isTRUE(mutual_exclude_can)) {
    FYC$can_cond1 <- "no cancer"
    FYC$can_cond1[FYC$can_colrec == 1 & FYC$can_mult == 0] <- "colorectal"
    FYC$can_cond1[FYC$can_breast == 1 & FYC$can_mult == 0] <- "breast"
    FYC$can_cond1[FYC$can_cervix == 1 & FYC$can_mult == 0] <- "cervix"
    FYC$can_cond1[FYC$can_lung == 1 & FYC$can_mult == 0] <- "lung"
    FYC$can_cond1[FYC$can_melano == 1 & FYC$can_mult == 0] <- "melanoma"
    FYC$can_cond1[FYC$can_nonmel == 1 & FYC$can_mult == 0] <- "nmsc"
    FYC$can_cond1[FYC$can_skindk == 1 & FYC$can_mult == 0] <- "unknown skin"
    FYC$can_cond1[FYC$can_prosta == 1 & FYC$can_mult == 0] <- "prostate"
    FYC$can_cond1[FYC$can_mult == 1] <- "multiple"
    FYC$can_cond1[FYC$can_any == 1 & FYC$can_cond1 == "no cancer"] <- "other"
    tmplvl <- c("no cancer", "colorectal", "breast", "cervix", "lung", "melanoma", "nmsc", "unknown skin", "prostate", "multiple", "other")
    FYC <- FYC %>%
      mutate(can_cond1 = factor(can_cond1, levels = tmplvl))
    
    FYC$can_cond2 <- "no cancer"
    FYC$can_cond2[FYC$can_colrec == 1 & FYC$can_mult == 0] <- "colorectal"
    FYC$can_cond2[FYC$can_breast == 1 & FYC$can_mult == 0] <- "breast"
    FYC$can_cond2[FYC$can_cervix == 1 & FYC$can_mult == 0] <- "cervix"
    FYC$can_cond2[FYC$can_lung == 1 & FYC$can_mult == 0] <- "lung"
    FYC$can_cond2[(FYC$can_melano == 1 | FYC$can_nonmel == 1 | FYC$can_skindk == 1) & FYC$can_mult == 0] <- "all skin"
    FYC$can_cond2[FYC$can_prosta == 1 & FYC$can_mult == 0] <- "prostate"
    FYC$can_cond2[FYC$can_mult == 1] <- "multiple"
    FYC$can_cond2[FYC$can_any == 1 & FYC$can_cond2 == "no cancer"] <- "other"
    tmplvl <- c("no cancer", "colorectal", "breast", "cervix", "lung", "all skin", "prostate", "multiple", "other")
    FYC <- FYC %>%
      mutate(can_cond2 = factor(can_cond2, levels = tmplvl))
  }
  
  FYC <- FYC %>%
    rowwise() %>%
    mutate(# colorectal cancer 
           agedx_colon = Inf, 
           agedx_colon = ifelse(!is.na(age) & (coloaged > 0), coloaged, agedx_colon), 
           agedx_rectum = Inf, 
           agedx_rectum = ifelse(!is.na(age) & (rectaged > 0), rectaged, agedx_rectum), 
           agedx_colrec = min(agedx_colon, agedx_rectum, na.rm = T), 
           tsdx_colrec = age - agedx_colrec, 
           tsdx_colrec = ifelse(is.infinite(tsdx_colrec), NA, tsdx_colrec), 
           # breast cancer
           tsdx_breast = ifelse(!is.na(age) & (brstaged > 0), age - brstaged, NA), 
           # cervical cancer
           tsdx_cervix = ifelse(!is.na(age) & (cervaged > 0), age - cervaged, NA), 
           # lung cancer
           tsdx_lung = ifelse(!is.na(age) & (lungaged > 0), age - lungaged, NA), 
           # melanoma
           tsdx_melano = ifelse(!is.na(age) & (melaaged > 0), age - melaaged, NA), 
           # non-melanoma
           agedx_skdk = Inf, 
           agedx_skdk = ifelse(!is.na(age) & (skdkaged > 0), skdkaged, agedx_skdk),
           agedx_sknm = Inf, 
           agedx_sknm = ifelse(!is.na(age) & (sknmaged > 0), sknmaged, agedx_sknm), 
           agedx_skinnm = min(agedx_skdk, agedx_sknm), 
           tsdx_sknm = age - agedx_skinnm, 
           # all skin cancer
           tsdx_allskin = max(tsdx_melano, tsdx_sknm, na.rm = T), 
           tsdx_sknm = ifelse(is.infinite(tsdx_sknm), NA, 
                              ifelse(tsdx_sknm < 0, 0, tsdx_sknm)), 
           tsdx_allskin = ifelse(is.infinite(tsdx_allskin), NA, 
                                 ifelse(tsdx_allskin < 0, 0, tsdx_allskin)), 
           # prostate cancer
           tsdx_prosta = ifelse(!is.na(age) & (prstaged > 0), age - prstaged, NA)
           ) %>%
    ungroup() 
  
  ### Race/Ethnicity
  FYC <- FYC %>% 
    mutate(race_recode = ifelse(racewx == 1 & hispanx != 1, "Non-Hispanic White", 
                                ifelse(racebx == 1 & hispanx != 1, "Non-Hispanic Black", 
                                       ifelse(hispanx == 1, "Hispanic", "Non-Hispanic Other"))), 
           race_recode = factor(race_recode, levels = c("Non-Hispanic White", 
                                                        "Non-Hispanic Black", 
                                                        "Hispanic", 
                                                        "Non-Hispanic Other")))
  
  ### Education attainment (changing code in 2011-2015)
  if (yr %in% paste0(c(11:15))) {
    FYC <- FYC %>% 
      mutate(edu = ifelse(edrecode %in% c(1, 2), "Less than high school", 
                          ifelse(edrecode %in% c(13), "High school graduate", 
                                 ifelse(edrecode >= 14, "Some college or more", "Unknown"))), 
             edu = factor(edu, levels = c("Less than high school", 
                                          "High school graduate", 
                                          "Some college or more", 
                                          "Unknown")))
  } else {
    FYC <- FYC %>%
      mutate(edu = ifelse(educyr >= 0 & educyr <= 11, "Less than high school", 
                          ifelse(educyr == 12, "High school graduate", 
                                 ifelse(educyr >= 13, "Some college or more", "Unknown"))), 
             edu = factor(edu, levels = c("Less than high school", 
                                          "High school graduate", 
                                          "Some college or more", 
                                          "Unknown"))) 
  }
  
  ### MSA (variable deleted since 2013)
  
  ### Marital status
  FYC <- FYC %>%
    mutate(tmp_marry_rd = 1 * ((marry31x == 7) | (marry42x == 7) | (marry53x == 7)), 
           tmp_marry = 1 * (marryx == 1) + tmp_marry_rd, 
           tmp_marry = ifelse(marryx < 0, NA, tmp_marry), 
           married = ifelse(tmp_marry >= 1, "Married", "Not married"))
  
  ### Poverty level (povcat)
  pov_key <- c("1.Low", "1.Low", "1.Low", "2.Middle", "3.High")
  names(pov_key) <- c(1:5)
  FYC$income_level <- recode(FYC$povcat, !!!pov_key)
  
  ### Region
  FYC <- FYC %>%
    mutate(region_recode = ifelse(region > 0, region, NA))
  
  ### Insurance
  ins_level_key <- c(`1` = "1.Young, Any private", `2` = "2.Young, Public only", `3` = "3.Young, Uninsured", 
                     `4` = "4.65+, Medicare Only", `5` = "5.65+, Medicare + Private", 
                     `6` = "6.65+, Medicare + Public", `7` = "7.65+, Uninsured", 
                     `8` = "8.65+, No Medicare")
  FYC <- FYC %>% mutate(ins_recode = inscov)
  FYC$ins_recode[FYC$age65 == ">=65"] <- 7
  FYC$ins_recode[(FYC$age65 == ">=65") & ((FYC$mcrev == 2) & (FYC$prvev == 1 | FYC$triev == 1 | FYC$mcdev == 1 | FYC$opaev == 1 | FYC$opbev == 1))] <- 8
  FYC$ins_recode[((FYC$age65 == ">=65") & (FYC$mcrev == 1))] <- 4
  FYC$ins_recode[((FYC$age65 == ">=65") & (FYC$mcrev == 1)) & (FYC$prvev == 1 | FYC$triev == 1)] <- 5
  FYC$ins_recode[((FYC$age65 == ">=65") & (FYC$mcrev == 1)) & (FYC$mcdev == 1 | FYC$opaev == 1 | FYC$opbev == 1)] <- 6
  
  FYC$ins_recode <- recode(FYC$ins_recode, !!!ins_level_key)
  
  ### Perceived health status
  
  ### Chronic conditions
  FYC <- FYC %>% 
    mutate(hbp = 1 * (hibpdx == 1), 
           hbp = ifelse(hibpdx < 0, NA, hbp), 
           chd = 1 * (chddx == 1), 
           chd = ifelse(chddx < 0, NA, chd), 
           angi = 1 * (angidx == 1), 
           angi = ifelse(angidx < 0, NA, angi), 
           mi = 1 * (midx == 1), 
           mi = ifelse(midx < 0, NA, mi), 
           ohrt = 1 * (ohrtdx == 1), 
           ohrt = ifelse(ohrtdx < 0, NA, ohrt), 
           strk = 1 * (strkdx == 1), 
           strk = ifelse(strkdx < 0, NA, strk), 
           emph = 1 * (emphdx == 1), 
           emph = ifelse(emphdx < 0, NA, emph), 
           chbr = 1 * (((chbron31 == 1) + (chbron53 == 1)) >= 1), 
           chbr = ifelse(chbron31 < 0 & chbron53 < 0, NA, chbr), 
           chol = 1 * (choldx == 1), 
           chol = ifelse(choldx < 0, NA, chol), 
           cancer = 1 * (cancerdx == 1), 
           cancer = ifelse(cancerdx < 0, NA, cancer), 
           diab = 1 * (diabdx == 1), 
           diab = ifelse(diabdx < 0, NA, diab), 
           jtpain = 1 * ((jtpain31 == 1) + (jtpain53 == 1) != 0), 
           jtpain = ifelse(jtpain31 < 0 & jtpain53 < 0, NA, jtpain),  
           arth = 1 * (arthdx == 1), 
           arth = ifelse(arthdx < 0, NA, arth), 
           asth = 1 * (asthdx == 1) * ((asstil31 == 1) + (asstil53 == 1) != 0), 
           asth = ifelse(asthdx < 0, NA, 
                         ifelse((asthdx == 1 & asstil31 < 0 & asstil53 < 0), NA, asth)), 
           adhdad = 1 * (adhdaddx == 1), 
           adhdad = ifelse(adhdaddx < 0, NA, adhdad), 
           heartpe = 1 * (chddx == 1 | angidx == 1 | midx == 1 | ohrtdx == 1), 
           heartpe = ifelse(chddx < 0 & angidx < 0 & midx < 0 | ohrtdx < 0, NA, heartpe)
           ) %>%
    mutate(npec = arth + asth + diab + emph + chd + hbp + strk + chol + angi + mi, 
           npec_recode = ifelse(npec >= 3, "3+", npec))
  
  ### BMI
  FYC <- FYC %>%
    mutate(bmi = ifelse(bmindx53 >= 0 & bmindx53 < 25, 0, 
                        ifelse(bmindx53 >= 25, 1, 2)))
  
  ### Government payment source variable and expenditure variables
  FYC <- FYC %>%
    mutate(totgov = totmcr + totmcd + totva + totwcp, 
           tototr = totva + tottri + totofd + totstl + 
             totwcp + totopr + totopu + totosr, 
           ambexp = obvexp + optexp, 
           tototr2 = ertexp + hhaexp + dvtexp + visexp + othexp + hhnexp)
  
  ### Income variable: ttlpx (person's total income); faminc (family income) 
  
  ### Days missed work due to illness
  if (yr %in% paste0(c(15:18))) {
    FYC <- FYC %>%
      mutate(ddnwrk_recode = (ddnwrk * (ddnwrk > 0) + 0 * (ddnwrk <= 0)))
  } else {
    FYC <- FYC %>%
      mutate(ddnwrk_recode = (ddnwrk31 * (ddnwrk31 > 0) + 0 * (ddnwrk31 <= 0)) +
               (ddnwrk42 * (ddnwrk42 > 0) + 0 * (ddnwrk42 <= 0)) + 
               (ddnwrk53 * (ddnwrk53 > 0) + 0 * (ddnwrk53 <= 0)))
  }
  
  ### Days spent in bed since start
  FYC <- FYC %>%
    mutate(ddbdys31 = tidyr::replace_na(ddbdys31, 0), 
           ddbdys42 = tidyr::replace_na(ddbdys42, 0), 
           ddbdys53 = tidyr::replace_na(ddbdys53, 0)) %>% 
    mutate(ddbdys_recode = (ddbdys31 * (ddbdys31 > 0) + 0 * (ddbdys31 <= 0)) +
             (ddbdys42 * (ddbdys42 > 0) + 0 * (ddbdys42 <= 0)) + 
             (ddbdys53 * (ddbdys53 > 0) + 0 * (ddbdys53 <= 0)))
  
  
  ### Total missed work days
  FYC <- FYC %>%
    mutate(totddl = ddnwrk_recode + ddbdys_recode)
  
  ### Employment status & Disability
  if (yr %in% paste0(c(17, 18))) {
    FYC <- FYC %>%
      mutate(empst = 1 * ((empst31 == 4) & (empst42 == 4) & (empst53 == 4)), 
             empst2 = 1 * ((empst31 == 4) | (empst42 == 4) | (empst53 == 4)), 
             unable = 1 * (unable31 == 1), 
             unable2 = 1 * ((nwk31 == 3) | (nwk42 == 3) | (nwk53 == 3)), 
             unable3 = 1 * ((unable31 == 1) & (empst31 == 4)))
  } else {
    FYC <- FYC %>%
      mutate(empst = 1 * ((empst31 == 4) & (empst42 == 4) & (empst53 == 4)), 
             empst2 = 1 * ((empst31 == 4) | (empst42 == 4) | (empst53 == 4)), 
             unable = 1 * ((unable31 == 1) & (unable53 == 1)), 
             unable2 = 1 * ((nwk31 == 3) | (nwk42 == 3) | (nwk53 == 3)), 
             unable3 = 1 * (((unable31 == 1) & (empst31 == 4)) | 
                              ((unable53 == 1) & (empst53 == 4)))) 
  }
  
  ### Utilization ?
  
  ### Health score ? 

  
  #### Add survey year
  FYC$svy_year <- as.numeric(paste0("20", yr))
  
  FYC <- data.table(FYC)
  
  return(FYC)
}


plot_hist <- function(design, exp_var, exp_name) {
  # design <- svyds
  # exp_name <- "total health care expenditure (dollars)"
  tmp_exp <- parse(text = exp_var)
  design <- update(design, y = eval(tmp_exp))
  n_topcode <- length(design$variables$y[design$variables$y > 1000000])
  design <- update(design, y = ifelse(y > 1000000, 1000000, y))
  design <- update(design, dum = ifelse(y > 0, 1, 0))
  design <- update(design, had_expenditure = ifelse(dum == 1, "yes", "no"))
  design$variables$groups <- NA
  design$variables$groups[design$variables$age65 == "18-64" & design$variables$can_any == 0] <- "age 18-64: no cancer dx" 
  design$variables$groups[design$variables$age65 == "18-64" & design$variables$can_any == 1] <- "age 18-64: cancer survivor" 
  design$variables$groups[design$variables$age65 == ">=65" & design$variables$can_any == 0] <- "age 65+: no cancer dx" 
  design$variables$groups[design$variables$age65 == ">=65" & design$variables$can_any == 1] <- "age 65+: cancer survivor" 
  
  subds <- subset(design, exclude_set == 0)
  subds$variables$groups <- factor(subds$variables$groups, 
                                   levels = c("age 18-64: no cancer dx", "age 18-64: cancer survivor", 
                                              "age 65+: no cancer dx", "age 65+: cancer survivor"))
  
  tmpdf <- data.table(subds$variables)
  tmpdf[, log_y := log(y)]
  plot_df <- tmpdf[, list(N = .N), by = .(groups, had_expenditure)]
  plot_df[, prop := round(N / sum(N), 3), by = .(groups)][order(groups, had_expenditure)]
  
  g_bar <- ggplot(plot_df) +
    geom_bar(aes(x = had_expenditure, y = prop), color = "royalblue", 
             fill = "white", size = 1, stat = "identity") +
    geom_text(aes(x = had_expenditure, y = prop, label = prop), vjust = 1, fontface = "bold") + 
    facet_wrap(.~groups) +
    ylab("fraction") + 
    xlab("are expenditures positive?") + 
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
  
  g_hist <- ggplot(tmpdf, aes(x = y)) +
    geom_histogram(aes(y = ..density..), bins = 50, color = "royalblue", 
                   fill = "royalblue", position = "identity") +
    scale_x_continuous(labels = scales::comma) + 
    scale_y_continuous(labels = scales::percent) + 
    xlab(exp_name) +
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
  
  g_hist_trans <- ggplot(tmpdf, aes(x = log_y)) +
    geom_histogram(aes(y = ..density..), bins = 50, color = "royalblue", 
                   fill = "royalblue", position = "identity") +
    scale_x_continuous(labels = scales::comma) + 
    scale_y_continuous(labels = scales::percent) + 
    xlab(paste0("log of ", exp_name, ", if >0")) +
    ggtitle("(C)") + 
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
  
  g_hist_all <- ggarrange(g_hist, g_hist_trans)
  g_out <- ggarrange(g_bar, g_hist_all, nrow = 2, heights = c(0.65, 0.35))
  return(g_out)
}

cal_difference <- function(y, beta_zero, beta_non_zero = NULL, 
                           mod_zero, mod_non_zero = NULL,
                           d0, d1) {
  mod_zero$coefficients <- beta_zero[y, ]
  if (!is.null(mod_non_zero)) mod_non_zero$coefficients <- beta_non_zero[y, ]
  
  if (!is.null(mod_non_zero)) {
    pred0 <- predict(mod_zero, newdata = model.frame(d0), design = d0, type = "response") * 
      predict(mod_non_zero, newdata = model.frame(d0), design = d0, type = "response")
    pred1 <- predict(mod_zero, newdata = model.frame(d1), design = d1, type = "response") * 
      predict(mod_non_zero, newdata = model.frame(d1), design = d1, type = "response") 
  } else {
    pred0 <- predict(mod_zero, newdata = model.frame(d0), design = d0, type = "response")
    pred1 <- predict(mod_zero, newdata = model.frame(d1), design = d1, type = "response")
  }
  
  pred_eff <- mean(pred1) - mean(pred0)
  pred_eff
}


estimate_me <- function(tmp_cond, df, m_zero, m_non_zero = NULL, n_sim = 50) {
  print(tmp_cond)
  vce_zero <- vcov(m_zero)
  if (!is.null(m_non_zero)) vce_non_zero <- vcov(m_non_zero)
  b_zero <- coef(m_zero)
  if (!is.null(m_non_zero)) {
    b_non_zero <- coef(m_non_zero)
  } else {
    b_non_zero <- NULL
  }
  beta_zero <- mvrnorm(n_sim, b_zero, vce_zero)
  if (!is.null(m_non_zero)) {
    beta_non_zero <- mvrnorm(n_sim, b_non_zero, vce_non_zero)
  } else {
    beta_non_zero <- NULL
  }
  
  d0 <- update(df, can_cond1 = "no cancer")
  d1 <- update(df, can_cond1 = tmp_cond)
  
  if (!is.null(m_non_zero)) {
    pred0 <- predict(m_zero, newdata = model.frame(d0), design = d0, type = "response") * 
      predict(m_non_zero, newdata = model.frame(d0), design = d0, type = "response")
    pred1 <- predict(m_zero, newdata = model.frame(d1), design = d1, type = "response") * 
      predict(m_non_zero, newdata = model.frame(d1), design = d1, type = "response")    
  } else {
    pred0 <- predict(m_zero, newdata = model.frame(d0), design = d0, type = "response")
    pred1 <- predict(m_zero, newdata = model.frame(d1), design = d1, type = "response") 
  }

  ame <- mean(pred1) - mean(pred0)
  
  beg_time <- Sys.time()
  sim_eff <- mclapply(c(1:n_sim), cal_difference, 
                      beta_zero = beta_zero, beta_non_zero = beta_non_zero, 
                      mod_zero = m_zero, mod_non_zero = m_non_zero,
                      d0 = d0, d1 = d1, mc.cores = 7)
  print(Sys.time() - beg_time)
  
  sim_eff <- unlist(sim_eff)
  se <- sd(sim_eff)
  z <- ame / se
  p <- 2 * pnorm(abs(z), lower.tail = F)
  bd <- sort(ame + c(-1, 1) * qnorm(0.025) * se)
  lb <- bd[1]
  ub <- bd[2]
  
  return(c(ame = ame, se = se, z = z, lb = lb, ub = ub, p = p))
}


gamma_hurdle <- function(y, design, sel_age = "18-64", sex = NULL, rhs) {
  print(y)
  if (is.null(sex)) {
    subds <- subset(design, exclude_set == 0 & age65 == sel_age)
  } else {
    subds <- subset(design, (exclude_set == 0 & age65 == sel_age) & sex_recode == sex)
  }
  
  subds <- update(subds, ins_recode2 = droplevels(ins_recode2))
  subds <- update(subds, dum = ifelse(eval(parse(text = y)) > 0, 1, 0))
  
  if (!is.null(sex)) rhs <- rhs[!rhs %in% "sex_recode"]
  
  fml <- as.formula(paste0("dum ~ ", paste0(rhs, collapse = " + ")))
  m_bin <- svyglm(fml, family = binomial(link = "logit"), design = subds)
  
  if (y == "totprv_pce" & sel_age == "18-64") rhs <- rhs[!rhs %in% c("ins_recode2")]
  if (y == "rxexp_pce" & sel_age == "18-64" & !is.null(sex))  {
    if (sex == "male") {
      rhs <- rhs[!rhs %in% c("edu_recode")] # this is not significant
    }
  }
  fml <- as.formula(paste0(y, " ~ ", paste0(rhs, collapse = " + ")))
  m_gamma <- svyglm(fml, family = Gamma(link = "log"), design = subset(subds, dum == 1), maxit = 100)
  
  coef_bin <- broom::tidy(m_bin, conf.int = T, exp = TRUE)
  coef_gamma <- broom::tidy(m_gamma, conf.int = T)
  
  can_vec <- levels(subds$variables$can_cond1)
  can_vec <- can_vec[-1]
  
  if (!is.null(sex)) {
    if (sex == "male") can_vec <- "prostate"
    if (sex == "female") can_vec <- c("breast", "cervix")
  } else {
    can_vec <- can_vec[!can_vec %in% c("prostate", "breast", "cervix")]
  }
  
  beg_time <- Sys.time()
  can_me_out <- lapply(can_vec, estimate_me, df = subds, 
                       m_zero = m_bin, m_non_zero = m_gamma, n_sim = 50)
  print(Sys.time() - beg_time)
  
  names(can_me_out) <- can_vec
  
  can_me_out <- data.table(do.call(rbind, can_me_out))
  can_me_out[, cancer_condition := can_vec]
  colnames(can_me_out) <- gsub(".pred1", "", colnames(can_me_out))
  setcolorder(can_me_out, c("cancer_condition"))
  
  out_ls <- list(coef_bin = coef_bin, coef_gamma = coef_gamma, 
                 pred_margin = can_me_out)
  return(out_ls)
}


logit <- function(y, design, sel_age = "18-64", sex = NULL, rhs) {
  if (is.null(sex)) {
    subds <- subset(design, exclude_set == 0 & age65 == sel_age)
  } else {
    subds <- subset(design, (exclude_set == 0 & age65 == sel_age) & sex_recode == sex)
  }
  if (!is.null(sex)) rhs <- rhs[!rhs %in% "sex_recode"]
  
  fml <- as.formula(paste0("unable ~ ", paste0(rhs, collapse = " + ")))
  m_bin <- svyglm(fml, family = binomial(link = "logit"), design = subds, maxit = 100)
  coef_bin <- broom::tidy(m_bin, conf.int = T, exp = TRUE)
  
  can_vec <- levels(subds$variables$can_cond1)
  can_vec <- can_vec[-1]
  
  if (!is.null(sex)) {
    if (sex == "male") can_vec <- "prostate"
    if (sex == "female") can_vec <- c("breast", "cervix")
  } else {
    can_vec <- can_vec[!can_vec %in% c("prostate", "breast", "cervix")]
  }
  
  beg_time <- Sys.time()
  can_me_out <- lapply(can_vec, estimate_me, df = subds, 
                       m_zero = m_bin, m_non_zero = NULL, n_sim = 50)
  print(Sys.time() - beg_time)
  
  names(can_me_out) <- can_vec
  
  can_me_out <- data.table(do.call(rbind, can_me_out))
  can_me_out[, cancer_condition := can_vec]
  colnames(can_me_out) <- gsub(".pred1", "", colnames(can_me_out))
  setcolorder(can_me_out, c("cancer_condition"))
  
  out_ls <- list(coef_bin = coef_bin, 
                 pred_margin = can_me_out)
  return(out_ls)
}

nb_hurdle <- function(y, design, sel_age = "18-64", sex = NULL, rhs) {
  
  if (sel_age == "18-64") {
    if (is.null(sex)) {
      subds <- subset(design, exclude_set == 0 & b_empst_young == 1)
    } else {
      subds <- subset(design, (exclude_set == 0 & b_empst_young == 1) & sex_recode == sex)
    }
  }
  
  if (sel_age == ">=65") {
    if (is.null(sex)) {
      subds <- subset(design, exclude_set == 0 & b_empst_old == 1)
    } else {
      subds <- subset(design, (exclude_set == 0 & b_empst_old == 1) & sex_recode == sex)
    }
  }
  
  if (!is.null(sex)) rhs <- rhs[!rhs %in% "sex_recode"]
  subds <- update(subds, ins_recode2 = droplevels(ins_recode2))
  subds <- update(subds, ageg = droplevels(ageg))
  subds <- update(subds, can_cond1 = droplevels(can_cond1))
  subds <- update(subds, dum = ifelse(eval(parse(text = y)) > 0, 1, 0))
  
  fml <- as.formula(paste0("dum ~ ", paste0(rhs, collapse = " + ")))
  m_bin <- svyglm(fml, family = binomial(link = "logit"), design = subds, maxit = 100)
  
  hurdle_ds <- subset(subds, dum == 1)
  fml <- as.formula(paste0(y, " ~ ", paste0(rhs, collapse = " + ")))
  
  print("===== negative binomial glm =====")
  beg_time <- Sys.time()
  m_nb <- sjstats::svyglm.nb(fml, design = hurdle_ds)
  print(Sys.time() - beg_time)
  
  coef_bin <- broom::tidy(m_bin, conf.int = T, exp = TRUE)
  
  tmp_coef <- capture.output(m_nb)[1:26]
  tmp_coef[grep("ins_recode2<65, other", tmp_coef)] <- gsub("ins_recode2<65, other", "ins_recode2<65:other", tmp_coef[grep("ins_recode2<65, other", tmp_coef)])
  tmp_coef[grep("race_recodeNon-Hispanic Black", tmp_coef)] <- gsub("race_recodeNon-Hispanic Black", "race_recodeNon-HispanicBlack", tmp_coef[grep("race_recodeNon-Hispanic Black", tmp_coef)])
  tmp_coef[grep("race_recodeNon-Hispanic Other", tmp_coef)] <- gsub("race_recodeNon-Hispanic Other", "race_recodeNon-HispanicOther", tmp_coef[grep("race_recodeNon-Hispanic Other", tmp_coef)])
  tmp_coef[grep("edu_recode>=some colloege", tmp_coef)] <- gsub("edu_recode>=some colloege", "edu_recode>=SomeColloege", tmp_coef[grep("edu_recode>=some colloege", tmp_coef)])
  
  tmp_coef <- lapply(tmp_coef, function(x) gsub("[[:blank:]]+", ",", x))
  tmp_coef <- lapply(tmp_coef, function(x) strsplit(x, ",")[[1]])
  tmp_coef <- lapply(tmp_coef, function(x) x[2:7])
  coef_nb <- do.call(rbind, tmp_coef)
  clnames <- coef_nb[1, ]
  coef_nb <- data.frame(coef_nb[2:nrow(coef_nb), ])
  colnames(coef_nb) <- clnames
  coef_nb <- as_tibble(coef_nb)
  
  can_vec <- levels(subds$variables$can_cond1)
  can_vec <- can_vec[-1]
  
  if (!is.null(sex)) {
    if (sex == "male") can_vec <- "prostate"
    if (sex == "female") can_vec <- c("breast", "cervix")
  } else {
    can_vec <- can_vec[!can_vec %in% c("prostate", "breast", "cervix")]
  }
  
  beg_time <- Sys.time()
  can_me_out <- lapply(can_vec, estimate_me, df = subds, 
                       m_zero = m_bin, m_non_zero = m_nb, n_sim = 50)
  print(Sys.time() - beg_time)
  
  names(can_me_out) <- can_vec
  
  can_me_out <- data.table(do.call(rbind, can_me_out))
  can_me_out[, cancer_condition := can_vec]
  colnames(can_me_out) <- gsub(".pred1", "", colnames(can_me_out))
  setcolorder(can_me_out, c("cancer_condition"))
  
  out_ls <- list(coef_bin = coef_bin, coef_nb = coef_nb, 
                 pred_margin = can_me_out)
  return(out_ls)
}

bind_estimates <- function(subpop, y = NULL, res) {
  # y <- "totexp_gdp"
  # subpop <- "cost_est_young_female"
  
  if (grepl("unable", subpop)) {
    tab <- res[[subpop]]
    coef_names <- names(tab)
    y <- "unable to work"
  } else if (grepl("wkday", subpop)) {
    tab <- res[[subpop]]
    coef_names <- names(tab)
    y <- "missed work days"
  } else {
    tab <- res[[subpop]][[y]]
    coef_names <- names(tab) 
  }
  
  if (all(c("coef_bin", "coef_gamma") %in% coef_names)) {
    m_gamma <- data.table(tab$coef_gamma)
    m_bin <- data.table(tab$coef_bin)
    m_gamma[, part := "Gamma glm"]
    m_gamma[, estimate := exp(estimate)]
    m_gamma[, conf.low := exp(conf.low)]
    m_gamma[, conf.high := exp(conf.high)]
    m_bin[, part := "Logit"]
    m_combine <- rbindlist(list(m_gamma, m_bin))
    m_combine[, p.value := ifelse(p.value < 0.001, "<0.001", paste0(round(p.value, 4)))]
  } else if (all(c("coef_bin", "coef_nb") %in% coef_names)) {
    m_nb <- data.table(tab$coef_nb)
    m_bin <- data.table(tab$coef_bin)
    m_nb[, part := "Negative Binomial glm"]
    m_bin[, part := "Logit"]
    m_bin[, statistic := NULL]
    m_bin[, p.value := ifelse(p.value < 0.001, "<0.001", paste0(round(p.value, 4)))]
    
    setnames(m_nb, "irr", "estimate")
    cols <- c("estimate", "std.error", "conf.low", "conf.high")
    m_nb[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
    
    m_combine <- rbindlist(list(m_nb, m_bin), use.names = T)
    m_combine[, statistic := NA]
  } else {
    m_combine <- data.table(tab$coef_bin)
    m_combine[, part := "Logit"]
    m_combine[, p.value := ifelse(p.value < 0.001, "<0.001", paste0(round(p.value, 4)))]
  }
  
  
  m_combine$term <- gsub("can_cond1", "cancer condition: ", m_combine$term)
  m_combine$term <- gsub("sex_recode", "", m_combine$term)
  m_combine$term <- gsub("ins_recode2", "insurance type: ", m_combine$term)
  m_combine$term <- gsub("race_recode", "race: ", m_combine$term)
  m_combine$term <- gsub("npec_recode", "# comorbid condition: ", m_combine$term)
  m_combine$term <- gsub("region_recode", "region: ", m_combine$term)
  m_combine$term <- gsub("edu_recode", "education: ", m_combine$term)
  m_combine$term <- gsub("married", "", m_combine$term)
  m_combine$term <- gsub("poly[(]year_center, 3[)]", "year^", m_combine$term)
  
  if (grepl("young", subpop)) {
    pop_age <- "age 18-64"
  } else {
    pop_age <- "age >=65"
  }
  
  if (grepl("male", subpop)) pop <- "male"
  if (grepl("female", subpop)) pop <- "female"
  if (grepl("all", subpop)) pop <- "all"
  
  m_combine[, `:=` (est = estimate, 
                    estimate = (paste0(round(estimate, 2), 
                                       " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")),
                    p_value = p.value, 
                    outcome = y, 
                    age = pop_age, 
                    pop = pop)]
  
  m_combine <- m_combine[!term %in% c("cancer condition: multiple", "cancer condition: other")]
  
  plot_dat <- m_combine[part == "Logit" & term != "(Intercept)"]
  
  m_combine[, `:=` (std.error = NULL, statistic = NULL, p.value = NULL, 
                    conf.low = NULL, conf.high = NULL)]
  setcolorder(m_combine, c("outcome", "part", "pop", "age", "term", "estimate", "p_value"))
  
  ## Predictive margin
  pred_margin <- data.table(tab$pred_margin)
  if (any(unlist(lapply(c("gdp", "pce"), grepl, y)))) {
    n_digit <- 0
  } else {
    n_digit <- 2
  }
  pred_margin[, margin := paste0(round(ame, n_digit), 
                                 " (", round(lb, n_digit), ", ", round(ub, n_digit), ")")]
  pred_margin[, p := ifelse(p < 0.001, "<0.001", paste0(round(p, 4)))]
  pred_margin <- pred_margin[, .(cancer_condition, margin, p)]
  pred_margin[, `:=` (outcome = y, 
                      age = pop_age, 
                      pop = pop)]
  setcolorder(pred_margin, c("outcome", "pop", "age", "cancer_condition", "margin", "p"))
  
  ## Plot data
  var_order <- unique(plot_dat$term)
  plot_dat[, term := factor(term, levels = rev(var_order))]
  
  return(list(coef_est = m_combine, pred_margin = pred_margin, plot_dat = plot_dat))
}


subset_tables <- function(obj, outcome_sel, pop_sel, expenditure_margin = FALSE) {
  
  sub_object <- obj[outcome %in% outcome_sel & pop %in% pop_sel]  
  
  if (any(grepl("margin", colnames(sub_object)))) {
    can_order <- unique(sub_object$cancer_condition)
    sub_object[, cancer_condition := factor(cancer_condition, levels = can_order)]
    sub_object <- sub_object[order(pop, age, cancer_condition)]
    
    sub_object <- dcast(sub_object, pop + age + outcome ~ cancer_condition, 
                        value.var = c("margin", "p"), fun.aggregate = toString)
    
    col_order <- c("pop", "age", "outcome", 
                   unlist(lapply(can_order, function(x) paste0(c("margin_", "p_"), x))))
    setcolorder(sub_object, col_order)
    sub_object <- sub_object[order(pop, age, outcome)]
    
    if (isTRUE(expenditure_margin)) {
      if (!all(sub_object$outcome %in% exp_ls)) stop("There are missin expenditures in the outcome variable")
      key_factor <- gsub("\\s*\\([^\\)]+\\)", "", exp_name_ls)
      names(key_factor) <- exp_ls
      sub_object[, expenditure := recode_factor(outcome, !!!key_factor)]
      
      key_type <- c("total expenditure", 
                    "source of payments", "source of payments", "source of payments", "source of payments", "source of payments", 
                    "type of services", "type of services", "type of services", "type of services")
      names(key_type) <- exp_ls
      sub_object[, exp_type := recode(outcome, !!!key_type)]
      
      sub_object <- sub_object[order(pop, age, expenditure)]
      sub_object[, outcome := NULL]
      
      setcolorder(sub_object, c("pop", "age", "exp_type", "expenditure")) 
    }
  } else {
    term_order <- unique(sub_object$term)
    sub_object <- dcast(sub_object, part + pop + age + term ~ outcome, value.var = c("estimate", "p_value"), fun.aggregate = toString)
    col_order <- c("part", "pop", "age", "term", 
                   unlist(lapply(outcome_sel, function(x) paste0(c("estimate_", "p_value_"), x))))
    setcolorder(sub_object, col_order)
    sub_object[, term := factor(term, levels = term_order)]
    sub_object[, part := factor(part)]
    sub_object$part <- relevel(sub_object$part, ref = "Logit")
    sub_object <- sub_object[order(pop, age, part, term)]
  }
  return(sub_object)
}


plot_odds_ratio <- function(x, plot_dat_ls, tmp_ix) {
  ix1 <- tmp_ix$min_ix[x]
  ix2 <- tmp_ix$max_ix[x]
  gtitle <- tmp_ix$group[x]
  plot_dat <- rbindlist(plot_dat_ls[c(ix1, ix2)])
  
  if (grepl("Both Men and Women", gtitle)) {
    plot_dat <- plot_dat[!term %in% c("cancer condition: breast", "cancer condition: cervix", "cancer condition: prostate")]
  } # else {
    # if (grepl("Men", gtitle)) plot_dat <- plot_dat[!term %in% c("cancer condition: colorectal", "cancer condition: lung", "cancer condition: melanoma", 
    #                                                             "cancer condition: nmsc/unknown", "cancer condition: multiple", "cancer condition: other")]
    # if (grepl("Women", gtitle)) plot_dat <- plot_dat[!term %in% c("cancer condition: colorectal", "cancer condition: lung", "cancer condition: melanoma", 
    #                                                               "cancer condition: nmsc/unknown", "cancer condition: prosta", 
    #                                                               "cancer condition: multiple", "cancer condition: other")]
  # }
  
  var_order <- unique(plot_dat$term)
  plot_dat[, term := factor(term, levels = rev(var_order))]
  
  max_val <- max(plot_dat$conf.high)
  max_ix <- which(plot_dat$conf.high == max_val)
  if (max_val >= 100) plot_dat <- plot_dat[-max_ix]
  
  g_out <- ggplot(data = plot_dat) + 
    geom_hline(yintercept = 1, size = 0.3, color = "gray60") + 
    geom_errorbar(aes(x = term, ymax = conf.high, ymin = conf.low, color = age), 
                  size = 0.8, position = position_dodge(width = 0.8), width = 0) + 
    geom_point(aes(x = term, y = est, color = age), 
               position = position_dodge(width = 0.8), pch = 21, fill = "white", size = 1.5, stroke = 1) +
    scale_color_manual(values = c("tomato", "deepskyblue")) + 
    ylab("Odds Ratio") +
    ggtitle(gtitle) + 
    facet_wrap(. ~ age) + 
    coord_flip()
  
  g_out <- g_out + theme_bw() + 
    theme(plot.title = element_text(size = 16, hjust = 0.5), 
          strip.text.x = element_text(size = 14, face = "bold", colour = "gray20"), 
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA), 
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          axis.title.x = element_text(size = 14), 
          axis.title.y = element_blank(), 
          legend.position = "bottom") 
  g_out
}

