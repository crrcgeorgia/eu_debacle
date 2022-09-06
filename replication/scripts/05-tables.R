### Simple frequency table (%, question text)

get_freq_table <- function(data, var, move_stuff) {
  
  label <- attr(data$variables[[var]], "label")
  
  data %>%
    mutate(
      !!as.name(var) := recode_data(!!as.name(var))
    )%>%
    filter(!is.na(!!as.name(var))) %>%
    group_by(!!as.name(var)) %>%
    summarize(proportion = survey_mean()) %>%
    labelled::set_variable_labels(
      !!as.name(var) := label,
      proportion := "Proportion",
      proportion_se := "Standard error of proportion",
    ) %>%
    set_names(c("variable", "proportion", "proportion_se"))%>%
    select(variable, proportion) %>%
    mutate(
      variable = fct_relevel(variable, move_stuff, after = Inf),
      proportion = round(proportion*100, 0),
    )%>%
    arrange(variable)%>%
    knitr::kable(caption = paste0(label, " (%, variable: ", var, ")")) %>%
    kableExtra::kable_classic(full_width = F)
}

get_freq(data = om_svy, var = "eu_supp", move_stuff = c("Don't know", "Refuse to answer", "Not applicable"))

get_freq_table <- function(data, var, cross, strip_part, move_stuff) {
  
  label <- attr(data$variables[[var]], "label")
  
  cross_label <- attr(data$variables[[cross]], "label")
  
  data %>%
    mutate(
      !!as.name(var) := recode_data(!!as.name(var)),
      !!as.name(cross) := recode_data(!!as.name(cross))
    )%>%
    filter(!is.na(!!as.name(var))) %>%
    group_by(!!as.name(cross), !!as.name(var)) %>%
    summarize(proportion = survey_mean()) %>%
    rename(
      variable = !!as.name(var),
      by_var = !!as.name(cross)
    ) %>%
    mutate(
      x_lab = as.character(cross_label),
      variable_label =  str_replace(label, strip_part, ""),
      variable_label = str_trim(variable_label),
      variable_name = var,
    ) %>%
    labelled::set_variable_labels(
      proportion := "Proportion",
      proportion_se := "Standard error of proportion",
    ) %>%
    select(by_var, variable, proportion) %>%
    mutate(
      variable = fct_relevel(variable, move_stuff, after = Inf),
      proportion = round(proportion*100, 0),
    )%>%
    arrange(variable)%>%
    pivot_wider(names_from = "by_var", values_from = "proportion") %>%
    knitr::kable(caption = paste0(label, "(%, variable: ", var, ")"))
}

### Simple crosstab

get_cross_table <- function(data, var, cross, strip_part, move_stuff) {
  
  label <- attr(data$variables[[var]], "label")
  
  cross_label <- attr(data$variables[[cross]], "label")
  
  data %>%
    mutate(
      !!as.name(var) := recode_data(!!as.name(var)),
      !!as.name(cross) := recode_data(!!as.name(cross))
    )%>%
    filter(!is.na(!!as.name(var))) %>%
    group_by(!!as.name(cross), !!as.name(var)) %>%
    summarize(proportion = survey_mean()) %>%
    rename(
      variable = !!as.name(var),
      by_var = !!as.name(cross)
    ) %>%
    mutate(
      x_lab = as.character(cross_label),
      variable_label =  str_replace(label, strip_part, ""),
      variable_label = str_trim(variable_label),
      variable_name = var,
    ) %>%
    labelled::set_variable_labels(
      proportion := "Proportion",
      proportion_se := "Standard error of proportion",
    ) %>%
    select(by_var, variable, proportion) %>%
    mutate(
      variable = fct_relevel(variable, move_stuff, after = Inf),
      proportion = round(proportion*100, 0),
    )%>%
    arrange(variable)%>%
    pivot_wider(names_from = "by_var", values_from = "proportion") %>%
    knitr::kable(caption = paste0(label, "(%, variable: ", var, ")"))
}


### table maker functions

get_cross_table <- function(data, var, cross, strip_part, move_stuff) {
  
  label <- attr(data$variables[[var]], "label")
  
  cross_label <- attr(data$variables[[cross]], "label")
  
  data %>%
    mutate(
      !!as.name(var) := recode_data(!!as.name(var)),
      !!as.name(cross) := recode_data(!!as.name(cross))
    )%>%
    filter(!is.na(!!as.name(var))) %>%
    group_by(!!as.name(cross), !!as.name(var)) %>%
    summarize(proportion = survey_mean()) %>%
    rename(
      variable = !!as.name(var),
      by_var = !!as.name(cross)
    ) %>%
    mutate(
      x_lab = as.character(cross_label),
      variable_label =  str_replace(label, strip_part, ""),
      variable_label = str_trim(variable_label),
      variable_name = var,
    ) %>%
    labelled::set_variable_labels(
      proportion := "Proportion",
      proportion_se := "Standard error of proportion",
    ) %>%
    select(by_var, variable, proportion) %>%
    mutate(
      variable = fct_relevel(variable, move_stuff, after = Inf),
      proportion = round(proportion*100, 0),
    )%>%
    arrange(variable)%>%
    pivot_wider(names_from = "by_var", values_from = "proportion") %>%
    flextable()%>%
    set_caption(label, style = "heading 4", autonum = NULL)%>%
    autofit()
  
  #-> table_print
  
  # cat(knitr::knit_print(table_print))

  # cat(knitr::knit_print(table_print))
  # knitr::kable(caption = paste0(label, "(%, variable: ", var, ")"))
}

get_freq_table <- function(data, var, move_stuff) {
  
  label <- attr(data$variables[[var]], "label")
  
  data %>%
    mutate(
      !!as.name(var) := recode_data(!!as.name(var))
    )%>%
    filter(!is.na(!!as.name(var))) %>%
    group_by(!!as.name(var)) %>%
    summarize(proportion = survey_mean()) %>%
    labelled::set_variable_labels(
      !!as.name(var) := label,
      proportion := "Proportion",
      proportion_se := "Standard error of proportion",
    ) %>%
    set_names(c("variable", "proportion", "proportion_se"))%>%
    select(variable, proportion) %>%
    mutate(
      variable = fct_relevel(variable, move_stuff, after = Inf),
      proportion = round(proportion*100, 0),
    )%>%
    arrange(variable)%>%
    flextable()%>%
    set_caption(label)%>%
    autofit()
  # -> table_print
  
  # cat(knitr::knit_print(table_print))
  # knitr::kable(caption = paste0(label, " (%, variable: ", var, ")")) %>%
  # kableExtra::kable_classic(full_width = F)
}

get_cross_table2 <- function(data, var, cross, strip_part, move_stuff) {
  
  label <- attr(data$variables[[var]], "label")
  
  cross_label <- attr(data$variables[[cross]], "label")
  
  data %>%
    mutate(
      !!as.name(var) := recode_data(!!as.name(var)),
      !!as.name(cross) := recode_data(!!as.name(cross))
    )%>%
    filter(!is.na(!!as.name(var))) %>%
    group_by(!!as.name(cross), !!as.name(var)) %>%
    summarize(proportion = survey_mean()) %>%
    rename(
      variable = !!as.name(var),
      by_var = !!as.name(cross)
    ) %>%
    mutate(
      x_lab = as.character(cross_label),
      variable_label =  str_replace(label, strip_part, ""),
      variable_label = str_trim(variable_label),
      variable_name = var,
    ) %>%
    labelled::set_variable_labels(
      proportion := "Proportion",
      proportion_se := "Standard error of proportion",
    ) %>%
    select(by_var, variable, proportion) %>%
    mutate(
      cross = cross_label,
      # variable = fct_relevel(variable, move_stuff, after = Inf),
      proportion = round(proportion*100, 0),
    ) %>%
     pivot_wider(names_from = "variable", values_from = "proportion") %>%
    relocate(any_of(move_stuff), .after = last_col()) %>%
    relocate(cross, .before = by_var)
}

get_cross_table2(data = om_svy, var = "e1_1", cross = "agegroup", strip_part = " ", move_stuff = c("Don't know", "Refuse to answer", "Not applicable"))

across_names <- c("national", "sex", "agegroup")

map_df(.x = across_names, .f = get_cross_table2, data = om_svy, var  = "eu_supp",
    strip_part = " ", move_stuff = c("Don't know", "Refuse to answer", "Not applicable")) %>%
  group_by(cross) %>%
  mutate(
    is_last_val_in_group = row_number() == max(row_number())
  ) %>%
  flextable(col_keys = names(.)[-length(names(.))]) %>%
  bold(j = 1, i = ~ !is.na(cross), bold = TRUE, part = "body" ) %>% 
  width(width = 1.5) %>% 
  set_header_labels(cross = NA, by_var = NA) %>%
  merge_v(j = c("cross"), target = c("cross")) %>%
  hline(i = ~is_last_val_in_group == TRUE, border = fp_border()) %>%
  fix_border_issues(part = "all")

