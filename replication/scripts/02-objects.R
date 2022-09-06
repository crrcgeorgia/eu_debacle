# A profile of Azerbaijani media consumers
## Needs and interests
### -	Q2, Importance for a respondent to keep up with news 

cb_eu <- get_freq(cb_svy, "eu_supp") %>%
  mutate(
    org = "EU",
    wave = "Caucasus Barometer, December 2020"
  ) %>%
  rename(
    variable = 1
  )

ua_eu <- get_freq(ua_svy, "eu_supp") %>%
  mutate(
    org = "EU",
    wave = "Survey on the war in Ukraine, March 2022"
  ) %>%
  rename(
    variable = 1
  )

om_eu <- get_freq(om_svy, "eu_supp") %>%
  mutate(
    org = "EU",
    wave = "CRRC Omnibus, July 2022"
  ) %>%
  rename(
    variable = 1
  )

bind_rows(
  cb_eu, ua_eu, om_eu
) %>%
  mutate(
    variable = str_replace(variable, "Totally support", "Fully support"),
    variable = str_replace(variable, "Mostly support", "Rather support"),
    variable = str_replace(variable, "Not support at all", "Do not support at all"),
    variable = str_replace(variable, "Don't support at all", "Do not support at all"),
    variable = str_replace(variable, "Don't Know", "Don't know"),
    variable = str_replace(variable, "Generally not support|Mostly do not support", "Rather not support"),
    variable = str_replace(variable, "Partially support and partially not support|Partially support, pardially don't support", "Partially support, partially not"),
  )-> eu


across_names <- c("party")

map_df(across_names, get_cross, data = cb_svy, var = "eu_supp", strip_part = "AARG")%>%
  mutate(
    org = "EU",
    wave = "Caucasus Barometer, December 2020"
  ) %>%
  filter(by_var == "Government")-> eu_cb_party

map_df(across_names, get_cross, data = ua_svy, var = "eu_supp", strip_part = "AARG")%>%
  mutate(
    org = "EU",
    wave = "Survey on the war in Ukraine, March 2022"
  ) %>%
  filter(by_var == "Government")-> eu_ua_party

map_df(across_names, get_cross, data = om_svy, var = "eu_supp", strip_part = "AARG")%>%
  mutate(
    org = "EU",
    wave = "CRRC Omnibus, July 2022"
  ) %>%
  filter(by_var == "Government")-> eu_om_party

bind_rows(
  eu_cb_party, eu_ua_party, eu_om_party
) %>%
  mutate(
    variable = str_replace(variable, "Totally support", "Fully support"),
    variable = str_replace(variable, "Mostly support", "Rather support"),
    variable = str_replace(variable, "Not support at all", "Do not support at all"),
    variable = str_replace(variable, "Don't support at all", "Do not support at all"),
    variable = str_replace(variable, "Don't Know", "Don't know"),
    variable = str_replace(variable, "Generally not support|Mostly do not support", "Rather not support"),
    variable = str_replace(variable, "Partially support and partially not support|Partially support, pardially don't support", "Partially support, partially not"),
  )-> eu_party


get_freq(om_svy, "e5")
get_freq(om_svy, "e7")
get_cross(data = om_svy, var = "e6_1", cross = "party", strip_part = "AARG") -> e6_1_party
  
