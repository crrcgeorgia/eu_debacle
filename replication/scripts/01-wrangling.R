### Load data

cb20 <- haven::read_dta("dataset/CB2020_Georgia_response_13.01.2021.dta")
ua22 <- haven::read_dta("dataset/Ukraine_Ukraine_Study_data_2022_03_14_public.dta")
omjul22 <- haven::read_dta("dataset/Omnibus_Wave13_July2022_without_NR_06.08.2022.dta")


### Recode data

ua22 %>%
  mutate(
    ## Covariates
    nato_supp = recode_data(g6_1),
    eu_supp = recode_data(g6_2),
    eeu_supp = recode_data(g6_3),
    party = case_when(
      g8 == 1 ~ 1, # government
      g8 %in% c(2:999) ~ 2, # opposition
      g8 == -5 ~ 3, # no party
      g8 %in% c(-1, -2) ~ 4, # DK/RA
      g9 == 1 ~ 1, # government
      g9 %in% c(2:999) ~ 2, # opposition
      T ~ NA_real_
    ),
    party = factor(party, levels =c(1:4), labels = c("Government", "Opposition", "No party", "DK/RA")),
    ) %>%
    labelled::set_variable_labels(
      party = "Party affiliation",
      eu_supp = "Support or oppose EU membership?"
    )-> ua_coded


cb20 %>%
  mutate(
    nato_supp = recode_data(NATOSUPP),
    eu_supp = recode_data(EUSUPP),
    eeu_supp = recode_data(EEUSUPNA),
    ## Covariates
    party = case_when(
      PARTYSUPP == 301 ~ 1, # government
      PARTYSUPP %in% c(302:999) ~ 2, # opposition
      PARTYSUPP == -5 ~ 3, # no party
      PARTYSUPP %in% c(-1, -2) ~ 4, # DK/RA
      PARTYSUPO == 301 ~ 1, # government
      PARTYSUPO %in% c(302:999) ~ 2, # opposition
      T ~ NA_real_
    ),
    party = factor(party, levels =c(1:4), labels = c("Government", "Opposition", "No party", "DK/RA")),
  ) %>%
  labelled::set_variable_labels(
    party = "Party affiliation",
    eu_supp = "Support or oppose EU membership?"
  )-> cb_coded


omjul22 %>%
  mutate(
    ## Covariates
    nato_supp = recode_data(e1_1),
    eu_supp = recode_data(e1_2),
    eeu_supp = recode_data(e1_3),
    party = case_when(
      m9 == 1 ~ 1, # government
      m9 %in% c(2:999) ~ 2, # opposition
      m9 == -5 ~ 3, # no party
      m9 %in% c(-1, -2) ~ 4, # DK/RA
      T ~ NA_real_
    ),
    party = factor(party, levels =c(1:4), labels = c("Government", "Opposition", "No party", "DK/RA")),
    displaced = case_when(
      d8 %in% c(-2, -1) ~ 0,
      d8 %in% c(-9, -3) ~ NA_real_,
      T ~ as.numeric(d8)
    ),
    ethnic = case_when(
      d4 == 3 ~ 1,
      T ~ 0
    ),
    employed = case_when(
      d2 == 1 ~ 1, # Private sector
      d2 == 2 ~ 2, # Public sector
      T ~ 3 # Not working
    ),
    agegroup = case_when(
      age < 35 ~ 1,
      age >= 35 & age < 55 ~ 2,
      T ~ 3
    ),
    across(starts_with("d7_"), ~case_when(
      .x %in% c(-2, -7, -1, -5, -9) ~ NA_real_,
      .x == 1 ~ 1,
      T ~ 0,
    )),
    wealth = rowSums(across(starts_with("d7_"))),
    internet = case_when(
      m2 %in% c(1:4) ~ 1,
      m2 %in% c(-9, -3) ~ NA_real_,
      T ~ 0
    ),
    e2_rec = case_when(
      e2 == 1 ~ 1,
      T ~ 0
    ),
    e3_rec = as.numeric(e3),
    e3_rec = case_when(
      e3_oth == 1 ~ 14,
      e3_oth == 2 ~ 7,
      e3_oth == 9 ~ 11,
      T ~ as.numeric(e3_rec)
    ),
    education = case_when(
      d3 %in% c(1, 2) ~ 1,
      d3 == 3 ~ 2,
      d3 %in% c(4, 5) ~ 3,
      T ~ NA_real_
    ),
    e4_rec = case_when(
      e4 %in% c(1, 2) ~ 1,
      e4 %in% c(3, 4) ~ 3,
      e4 %in% c(-2, -1) ~ 2,
      T ~ NA_real_
    ),
    e7_rec = case_when(
      e7 %in% c(1, 2) ~ 1,
      e7 %in% c(3, 4) ~ 3,
      e7 %in% c(-2, -1) ~ 2,
      T ~ NA_real_
    ),
    education = factor(education, levels =c(1, 2, 3), labels = c("Secondary or lower", "Vocational", "Higher")),
    agegroup = factor(agegroup, levels = c(1, 2, 3), labels = c("18-34", "35-54", "55+")),
    e4_rec = factor(e4_rec, levels = c(1, 2, 3), labels = c("False", "DK/RA", "True")),
    e7_rec = factor(e7_rec, levels = c(1, 2, 3), labels = c("Not acceptable", "DK/RA", "Acceptable")),
    employed = factor(employed, levels = c(1:3), labels = c("Private", "Public", "Not working")),
    settlement = as_factor(stratum),
    ethnic = factor(ethnic, levels = c(0, 1), labels = c("Other", "Georgian")),
    sex = as_factor(sex),
    ## Substantive variables
    across(starts_with("m8_"), ~case_when(
      .x %in% c(-9, -3) ~ NA_real_,
      .x %in% c(-1, -2) ~ 98,
      .x %in% c(-5) ~ 98,
      .x %in% c(0) ~ 2,
      T ~ as.numeric(.x)
    )),
  ) %>%
  labelled::add_value_labels(
    e3_rec = c(
      labelled::val_labels(omjul22$e3),
      "EU does not want/need Georgia" = 14
    )
  ) %>%
  labelled::set_variable_labels(
    party = "Party affiliation",
    agegroup = "Age groups",
    wealth = "Wealth index",
    employed = "Employment status",
    education = "Education",
    ethnic = "Ethnic ID",
    e3_rec = "The main reason why Georgia was not granted an EU candidate status?",
    e4_rec = "True that Georgia would gain candidate status if engaged in war against Russia?",
    e7_rec = attr(omjul22$e7, "label"),
    eu_supp = "Support or oppose EU membership?"
  )-> om_coded

#### Make survey objects
om_coded %>%
  as_survey_design(ids = 1,
                   weights = weight, data = om_coded) -> om_svy

cb_coded %>%
  as_survey_design(ids = 1,
                   weights = INDWT, data = cb_coded) -> cb_svy
ua_coded %>%
  as_survey_design(ids = 1,
                   weights = weight, data = ua_coded) -> ua_svy


