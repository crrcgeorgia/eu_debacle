###

get_freq(om_svy, "e2")

e2_awareness <- svyglm(e2_rec~sex+stratum+ethnic+agegroup+wealth+employed+education+internet+party,
                       design = om_svy, family = "binomial")

summary(e2_awareness)

broom::tidy(e2_awareness)

ggeffects::ggpredict(e2_awareness, c("agegroup", "employed", "education", "party"))

across_names <- c("agegroup", "employed", "education", "party")

map_df(across_names, get_cross, data = om_svy, var = "e2", strip_part = "AAARG") -> e2_xtab

###

get_freq(om_svy, "e3_rec") -> e3_freq

###

###

get_freq(om_svy, "e4_rec") -> e4_rec_freq


om_svy %>%
  svy_vglm(
    e4_rec~sex+stratum+ethnic+agegroup+wealth+employed+education+internet+party,
    design = .,
    family = multinomial(refLevel=1)
  ) -> e4_mod

summary(e4_mod)

across_names <- c("stratum", "ethnic", "wealth", "education", "party")

map_df(across_names, get_cross, data = om_svy, var = "e4_rec", strip_part = "AAARG") -> e4_rec_xtab



get_freq(om_svy, "e5")

get_freq(om_svy, "e7")

get_freq(om_svy, "e7_rec")

om_svy %>%
  svy_vglm(
    e7_rec~sex+stratum+ethnic+agegroup+wealth+employed+education+internet+party,
    design = .,
    family = multinomial(refLevel=1)
  ) -> e7_mod

summary(e7_mod)

across_names <- c("ethnic", "agegroup", "employed", "education", "party")

map_df(across_names, get_cross, data = om_svy, var = "e7_rec", strip_part = "AAARG") -> e7_rec_xtab

across_names <- c(paste0("e6_", rep(1:6)))

map_df(across_names, get_multi_freq, data = om_svy, strip_part = "Who does EU imply under deoligarchization: ") -> e6_all

###

get_cross(om_svy, "e6_1", "party", "Who does EU imply under deoligarchization: ") -> e6_1_party




get_freq(om_svy, "eu_supp")
get_freq(cb_svy, "eu_supp")
get_freq(ua_svy, "eu_supp")


