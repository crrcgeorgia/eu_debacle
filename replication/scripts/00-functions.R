### Recode data

recode_data <- function(x){
  x = as_factor(x)
  x[str_detect(x, regex("Break(.*)off|Breakoff|Legal skip|Interviewer error", ignore_case = T))] = NA
  x = fct_relevel(x, c("Other"), after=0)
  x = fct_relevel(x, c("Don't know", "Don t know", "Don't know"), after=0)
  x = fct_relevel(x, c("Refuse to answer", "Don t know", "Don't know"), after=0)
  x = fct_relevel(x, c("Not applicable"), after=0)
  x = droplevels(x)
}


### Fonts


loadfonts(device = "win")

### Themes

theme_crrc <- function () { 
  theme_minimal(base_size=12) %+replace%
    theme(
      axis.title = element_blank(),
      title = element_blank(),
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(family = "Lato", face="bold", size = 14),
      text = element_text(family= "Lato"),
      plot.title = element_text(size=14, face="bold", family="Lato", hjust = 0),
      plot.subtitle = element_text(size=12, family="Lato", hjust=0),
      axis.text = element_text(size=12, family="Lato", color = "black"),
      legend.position = "none"
    )
}


### Get data

### same but also accesses labels

get_freq <- function(data, var) {
  
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
    )
}


### get multiple frequency

get_multi_freq <- function(data, var, strip_part) {
  
  label <- attr(data$variables[[var]], "label")
  
  data %>%
    mutate(
      !!as.name(var) := recode_data(!!as.name(var))
    )%>%
    filter(!is.na(!!as.name(var))) %>%
    group_by(!!as.name(var)) %>%
    summarize(proportion = survey_mean()) %>%
    rename(
      variable = !!as.name(var)
    ) %>%
    mutate(
      variable_label =  str_replace(label, strip_part, ""),
      variable_label = str_trim(variable_label),
      variable_name = var,
    ) %>%
    labelled::set_variable_labels(
      proportion := "Proportion",
      proportion_se := "Standard error of proportion",
    )
}

### Get a crosstab

get_cross <- function(data, var, cross, strip_part) {
  
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
    )
}



### CHARTING

##### Base color for categories: #0092A7

base_color <- "#0092A7"

color_general_categories <- function(data, var) {
  
  get_freq(data, {{var}}) -> freq
  
  list(c("#999999", "#444444"), rep(base_color, unlist(nrow(freq)-2))) %>%unlist()
}


color_yesno <- c("#444444", "#999999", "#3EBCD2",  "#DB444C")

color_three <- c("#444444", "#999999", "#3EBCD2", "#FACA58",  "#DB444C")

color_five_cat <- c("#d7263d", "#f46036", "#2e294e", "#1b998b", "#c5d86d")

color_nine_cat <- c("#C7303C", "#1270A8", "#00788D", "#00786B", "#C28812", "#667100", "#925977", "#826636", "#576E79")

color_intensity <- c("#444444","#999999", "#00786B", "#2E9284", "#69C9B9", "#FFDA82", "#FACA58", "#E7B030", "#C28812")

color_intensity2 <- c("#444444","#999999", "#00786B", "#2E9284", "#FACA58", "#E7B030", "#C28812")

##### Define chart making functions
### TODO: (1) soft-code export directory, (2) different sizes for PPT and publications

### single_categorical_variable, unsorted

single_cat_unsorted <- function(data, var, palette, limit) {
  get_freq(data, var) %>%
    data.frame()%>%
    ggplot(aes(!!as.name(var), proportion, fill=!!as.name(var), label=ifelse(proportion <= 0.005, "", round(proportion*100, 0))))+
    geom_text(hjust = -1.1, nudge_x = 0.1, family="Lato", size=4, fontface = "bold")+
    scale_fill_manual(values=palette(data, var))+
    ylim(0, limit)+
    geom_col()+
    coord_flip()+
    theme_crrc()
  # ggsave(paste0("D://", var, ".pdf"), width=13, height=5)
  
}

### single_categorical_variable, sorted

single_cat_sorted <- function(data, var, palette, limit) {
  get_freq(data, var) %>%
    data.frame()%>%
    mutate(
      !!as.name(var) := fct_reorder(!!as.name(var), proportion),
      !!as.name(var) := fct_relevel(!!as.name(var), c("Other"), after=0),
      !!as.name(var) := fct_relevel(!!as.name(var), c("Don't know", "Don t know", "Don't know"), after=0),
      !!as.name(var) := fct_relevel(!!as.name(var), c("Refuse to answer", "Don t know", "Don't know"), after=0),
      !!as.name(var) := fct_relevel(!!as.name(var), c("Not applicable"), after=0),
    ) %>%
    ggplot(aes(!!as.name(var), proportion, fill=!!as.name(var), label=ifelse(proportion <= 0.005, "", round(proportion*100, 0))))+
    geom_text(hjust = -1.1, nudge_x = 0.1, family="Lato", size=4, fontface = "bold")+
    scale_fill_manual(values=palette(data, var))+
    ylim(0, limit)+
    #  labs(
    #   title = attr(data$variables[[var]], "label")
    #  )+
    geom_col()+
    coord_flip()+
    theme_crrc()
  # ggsave(paste0("D://", var, ".pdf"), width=13, height=5)
}
