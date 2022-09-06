color_five <- c( "#A81829",  "#DB444C", "#FACA58", "#3EBCD2", "#006BA2", "#A4BDC9", "#444444")
color_three <- c("#444444", "#A4BDC9", "#006BA2", "#FACA58", "#A81829")
color_three_nomiddle <- c("#006BA2", "#FACA58", "#A81829")
color_three_3 <- c("#444444", "#A4BDC9", "#006BA2", "#FACA58", "#A81829")


single_cat_sorted(data = om_svy, var = "e3_rec", palette = color_general_categories,
                  limit = 0.5) -> e3_chart

e3_chart +
  labs(
    title = "In your opinion, what is the main reason that Georgia was not granted the EU candidate status?",
    subtitle = "(% of those aware, spontaneous answers. Single choice. July 2022 Omnibus survey)"
  )

ggsave("visuals/e3_chart.pdf", device = cairo_pdf,  width = 35, height = 10, units = "cm")
ggsave("visuals/e3_chart.png", width = 35, height = 10, units = "cm")

### , 
### q2 crosstab

eu %>%
  mutate(
    wave = factor(wave, levels = c("Caucasus Barometer, December 2020", "Survey on the war in Ukraine, March 2022", "CRRC Omnibus, July 2022")),
    variable = factor(variable, levels = c("Do not support at all", "Rather not support", "Partially support, partially not", "Rather support", "Fully support",
                                                                        "Don't know", "Refuse to answer"))
  ) %>%
  ggplot(aes(wave, proportion, fill=variable))+
  geom_col(position = position_stack(reverse = T))+
  scale_fill_manual(values= color_five,
                    guide = guide_legend(reverse = F))+
  scale_y_continuous(limits = c(0, 1.01), expand = c(0, 0))+
  coord_flip()+
  geom_text(aes(label=ifelse(proportion <= 0.005, "", round(proportion*100, 0))),
            position = position_stack(vjust = 0.5, reverse = T), family="Lato", size=4, fontface = "bold")+
  facet_wrap(~org, scales = "free", ncol=1)+
  labs(
    title = "Do you support Georgia's membership in the EU",
    subtitle = "(%, 2020-2022)"
  )+
  theme_crrc()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_blank(),
        strip.text = element_blank()
          # ggtext::element_markdown(
          # hjust = 0,
        # ),
)

ggsave("visuals/eu.pdf", device = cairo_pdf,  width = 35, height = 10, units = "cm")
ggsave("visuals/eu_en.png", width = 35, height = 10, units = "cm")


eu_party %>%
  mutate(
    wave = factor(wave, levels = c("Caucasus Barometer, December 2020", "Survey on the war in Ukraine, March 2022", "CRRC Omnibus, July 2022")),
    variable = factor(variable, levels = c("Do not support at all", "Rather not support", "Partially support, partially not", "Rather support", "Fully support",
                                                                        "Don't know", "Refuse to answer"))
  ) %>%
  ggplot(aes(wave, proportion, fill=variable))+
  geom_col(position = position_stack(reverse = T))+
  scale_fill_manual(values= color_five,
                    guide = guide_legend(reverse = F))+
  scale_y_continuous(limits = c(0, 1.01), expand = c(0, 0))+
  coord_flip()+
  geom_text(aes(label=ifelse(proportion <= 0.005, "", round(proportion*100, 0))),
            position = position_stack(vjust = 0.5, reverse = T), family="Lato", size=4, fontface = "bold")+
  facet_wrap(~org, scales = "free", ncol=1)+
  labs(
    title = "Do you support Georgia's membership in...",
    subtitle = "(% among GD supporters, 2020-2022)"
  )+
  theme_crrc()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_blank(),
        strip.text = element_blank()
        # ggtext::element_markdown(
        # hjust = 0,
        # ),
        
)


ggsave("visuals/eu_xparty.pdf", device = cairo_pdf,  width = 35, height = 10, units = "cm")
ggsave("visuals/eu_en_xparty.png", width = 35, height = 10, units = "cm")

e6_all %>%
  filter(variable == "Mentioned" | (variable %in% c("Don't know", "Refuse to answer", "Does not imply specific individual/individuals") & variable_name == across_names[1]))%>%
  mutate(
    variable = as.character(variable),
    group = ifelse(variable %in% c("Don't know", "Refuse to answer", "Does not imply specific individual/individuals"), variable, variable_label),
    group = fct_reorder(group, proportion),
    group = fct_relevel(group, c("Refuse to answer", "Don't know", "Does not imply specific individual/individuals", "Other"), after=0),
  ) %>%
  dplyr::select(variable_name, group, proportion) %>%
  ggplot(aes(group, proportion, group=group, fill=group, label=ifelse(proportion <= 0.005, "", round(proportion*100, 0))))+
  geom_col()+
  scale_fill_manual(values=list(c("#999999", "#444444", "#f2cc8f"), rep(base_color, length(across_names))) %>%unlist())+
  ylim(0, 1.01)+
  coord_flip()+
  geom_text(hjust = -1.1, nudge_x = 0.1, family="Lato", size=4, fontface = "bold")+
  theme_crrc() -> e6_chart


e6_chart +
  labs(
    title = "Who does the EU imply under 'de-oligarchization'?",
    subtitle = "(% of mentioned, July 2022 Omnibus survey.)"
  )

ggsave("visuals/e6_all.pdf", device = cairo_pdf,  width = 35, height = 10, units = "cm")
ggsave("visuals/e6_en_all.png", width = 35, height = 10, units = "cm")


# ggsave("visuals/bi_ms.png", width=11.2, height=6.35)


### ms_xtab


e4_rec_xtab %>%
  filter(!is.na(by_var))%>%
  filter(!by_var %in% c(1:5, 7:11))%>%
  mutate(
    by_var = fct_rev(by_var),
    by_var = sjlabelled::add_labels(by_var, labels = c(`Lowest` = 0, `Median` = 6, `Highest` = 12)),
    by_var = fct_relevel(by_var, "Other", "Georgian"),
    variable = fct_rev(variable),
    # x_lab = factor(x_lab),
    # x_lab = fct_relevel(x_lab, "Party affiliation", Inf),
  ) %>%
  arrange(variable)%>%
  ggplot(aes(by_var, proportion, fill=variable))+
  geom_col(position = position_stack(reverse = T))+
  scale_fill_manual(values= color_three_nomiddle,
                    guide = guide_legend(reverse = F))+
  scale_y_continuous(limits = c(0, 1.01), expand = c(0, 0))+
  coord_flip()+
  geom_text(aes(label=ifelse(proportion <= 0.005, "", round(proportion*100, 0))),
            position = position_stack(vjust = 0.5, reverse = T), family="Lato", size=4, fontface = "bold")+
  # facet_grid(~x_lab+variable_label, scales = "free_y", space = "free", margins = "variable_label")+
  # facet_col(vars(x_lab))+
  facet_col(~x_lab, scales = "free_y", space = "free")+
  labs(
    title = "Some Georgian politicians said that Georgia would have been granted an EU candidate status\nonly if it got involved in the war against Russia/opened up a second front.\nTo what extent do you think this is true or fales?",
    subtitle = '(% by major population groups. Options "Not true at all" and "rather not true" were grouped as "False",\noptions "Absolutely true" and "rather true" were grouped to "True." July 2022 Omnibus survey)'
  )+
  theme_crrc()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_blank(),
        strip.text = element_markdown(
          hjust = 0,
        )
)

ggsave("visuals/e4_xtab.pdf", device = cairo_pdf,  width = 35, height = 15, units = "cm")
ggsave("visuals/e4_en_xtab.png", width = 35, height = 15, units = "cm")

e7_rec_xtab %>%
  filter(!is.na(by_var))%>%
  mutate(
    by_var = fct_rev(by_var),
    by_var = fct_relevel(by_var, "Other", "Georgian"),
    # x_lab = factor(x_lab),
    # x_lab = fct_relevel(x_lab, "Party affiliation", Inf),
  ) %>%
  ggplot(aes(by_var, proportion, fill=variable))+
  geom_col(position = position_stack(reverse = T))+
  scale_fill_manual(values= rev(color_three),
                    guide = guide_legend(reverse = F))+
  scale_y_continuous(limits = c(0, 1.01), expand = c(0, 0))+
  coord_flip()+
  geom_text(aes(label=ifelse(proportion <= 0.005, "", round(proportion*100, 0))),
            position = position_stack(vjust = 0.5, reverse = T), family="Lato", size=4, fontface = "bold")+
  # facet_grid(~x_lab+variable_label, scales = "free_y", space = "free", margins = "variable_label")+
  # facet_col(vars(x_lab))+
  facet_col(~x_lab, scales = "free_y", space = "free")+
  labs(
    title = attr(om_coded$e7_rec, "label"),
    subtitle = "(% by major population groups, July 2022 Omnibus survey)"
  )+
  theme_crrc()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_blank(),
        strip.text = element_markdown(
          hjust = 0,
        )
)

ggsave("visuals/e7_xtab.pdf", device = cairo_pdf,  width = 35, height = 15, units = "cm")
ggsave("visuals/e7_en_xtab.png", width = 35, height = 15, units = "cm")



