library(dplyr)
library(foreign)
library(rlang)
library(data.table)

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

get_fyc_annual <- function(yr, data_path) {
  # data_path <- "MEPS data/"
  # yr <- "12"
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
    rowwise() %>%
    mutate(can_mult = ifelse(sum(c_across(tolower(cancer_set[!cancer_set %in% "CANCERDX"])), 
                                 na.rm = T) > 1, 1, 0)) %>%
    ungroup() %>%
    mutate(can_colrec = 0, 
           can_colrec = ifelse(cacolon == 1 | carectum == 1, 1, can_colrec), 
           can_breast = 0, 
           can_breast = ifelse(cabreast == 1, 1, can_breast), 
           can_cervix = 0, 
           can_cervix = ifelse(cacervix == 1, 1, can_cervix), 
           can_lung = 0, 
           can_lung = ifelse(calung == 1, 1, can_lung), 
           can_melano = 0, 
           can_melano = ifelse(camelano == 1, 1, can_melano), 
           can_skinnm = 0, 
           can_skinnm = ifelse(caskinnm == 1 | caskindk == 1, 1, can_skinnm), # combine non-melanoma and unknown skin cancer
           can_nonmel = 0, 
           can_nonmel = ifelse(caskinnm == 1, 1, can_nonmel), 
           can_skindk = 0, 
           can_skindk = ifelse(caskindk == 1, 1, can_skindk), 
           can_allskin = 0, 
           can_allskin = ifelse(camelano == 1 | caskinnm == 1 | caskindk == 1, 1, can_allskin), 
           can_prosta = 0, 
           can_prosta = ifelse(caprosta == 1, 1, can_prosta), 
           can_any = 0, 
           can_any = ifelse(cancerdx == 1, 1, can_any))
  
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
  
  ### Poverty level
  
  ### Region
  FYC <- FYC %>%
    mutate(region_recode = ifelse(region > 0, region, NA))
  
  ### Insurance
  ins_level_key <- c(`1` = "1.Young, Any private", `2` = "2.Young, Public only", `3` = "3.Young, Uninsured", 
                     `4` = "4.65+, Medicare with all others", `5` = "5.65+, Any private, TRICARE/CHAMPVA", 
                     `6` = "6.65+, Edited Medicaid/SCHIP, other public Type A, Type B", 
                     `7` = "7.65+, No Medicare")
  FYC <- FYC %>%
    mutate(ins_recode = inscov)
  FYC$ins_recode[FYC$age65 == ">=65"] <- 7
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
