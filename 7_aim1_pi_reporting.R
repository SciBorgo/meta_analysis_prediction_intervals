

# Aim 1: estimate prediction interval reporting

# Plot colours
colour_blind_pal = cbPalette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Load data
d <- readRDS('data-prediction intervals.rds') %>%
  mutate(estimates = as.integer(estimates),
         ci_level = as.integer(ci_level))

# Missing data
missing_vars <- sapply(d, function(x) sum(is.na(x))) %>%
  .[which(.>0)]

missing_vars

# From each discipline
nrow(d)
tab = table(d$discipline)
tab

#### Study descriptive
d %>%
  group_by(discipline) %>%
  summarise(median_k = median(estimates, na.rm = T),
            q25_k = quantile(estimates, probs = 0.25, na.rm = T),
            q75_k = quantile(estimates, probs = 0.75, na.rm = T))

d %>%
  filter(discipline == 'sport') %>%
  group_by(md, smd, risk_ratio, odds_ratio, hazard_ratio) %>%
  count() %>%
  mutate(`%` = round((n/tab[[2]])*100, digits = 0)) %>%
  arrange(-`%`)

d %>%
  filter(discipline == 'med') %>%
  group_by(md, smd, risk_ratio, odds_ratio, hazard_ratio) %>%
  count() %>%
  mutate(`%` = round((n/tab[[1]])*100, digits = 0)) %>%
  arrange(-`%`)

d %>%
  group_by(discipline, ci_level) %>%
  count()

# Measures of heterogeneity
d %>%
  group_by(discipline, forest_plot_shown) %>%
  count()

d %>% filter(i_square>=0) %>%
  group_by(discipline) %>%
  count()

d %>% filter(tau_square>=0) %>%
  group_by(discipline) %>%
  count()

# Figure 2: Reported I2 values
d %>%
  mutate(i_square = as.numeric(i_square),
         Discipline = recode_factor(discipline,
                                    'sport' = 'Sport',
                                    'med' = 'Medicine')) %>%
  ggplot(aes(x = i_square))+
  geom_histogram(aes(y = ..density.., fill = Discipline), bins = 25,  colour = 'black')+
  theme_classic()+
  facet_grid(~Discipline) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.title=element_blank(),
        legend.position = c(0.85,0.85)) +
  labs(x = 'I-squared (%)', y = 'Density') +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"))
ggsave(file = "figure2.png", units="in", width = 6, height = 2.5, dpi = 900)


# Figure 3: Reported tau-squared values
d %>%
  filter(tau_square<100) %>%
  mutate(tau_square = as.numeric(tau_square),
         Discipline = recode_factor(discipline,
                                    'sport' = 'Sport',
                                    'med' = 'Medicine')) %>%
  ggplot(aes(x = (tau_square)))+
  geom_histogram(aes(y = ..density.., fill = Discipline), bins = 2000,  colour = 'white')+
  theme_classic()+
  facet_grid(~Discipline) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.title=element_blank(),
        legend.position = c(0.85,0.85)) +
  labs(x = 'Tau-squared', y = 'Density') +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  scale_x_continuous(n.breaks = 6) +
  coord_cartesian(xlim = c(0,1)) +
  ylim(0,12)
ggsave(file = "figure3.png", units="in", width = 6, height = 2.5, dpi = 900)


d %>% filter(tau_square>1) -> dtau
table(dtau$tau_square, dtau$discipline) %>% as.data.frame() %>% group_by(Var2) %>%
  summarise(sum = sum(Freq))

# Compare I-squared and tau-squared
wilcox.test(as.numeric(d$i_square)~d$discipline)
wilcox.test(as.numeric(d$tau_square)~d$discipline)


# Explore I2 versus estimates
d %>%
  ggplot(aes(x = as.numeric(i_square), y = log(estimates))) +
  geom_point()+
  stat_smooth(method = 'lm') +
  theme_classic() +
  facet_grid(~discipline)




#### Prediction interval reporting
table(d$pi_reported) # Overall
table(d$discipline, d$pi_reported) # By discipline

# Fit GLM
fit <- glm(pi_reported ~ discipline,
           data = d,
           family = binomial(link = "logit"))

# Check fit
plot(fit)

# Summary
tidy(fit, conf.int = T, conf.level = 0.95) # Summary
tidy(fit, conf.int = T, conf.level = 0.95, exponentiate = T) # Summary with exp()

# Probs
fit %>%
  emmeans(specs = ~discipline, mode = "lin") %>%
  regrid("log") %>%
  summary(type = "response", infer = TRUE, by=NULL)


# Figure 4: Reporting of prediction interval by year
d %>%
  group_by(discipline, year) %>%
  filter(year<2022) %>%
  count() %>%
  left_join(d %>%
              group_by(discipline, year) %>%
              summarise(pi_reported = sum(pi_reported)),
            by = c('discipline','year')) %>%
  mutate(prop = pi_reported/n,
         year = as.factor(year),
         Discipline = recode_factor(discipline,
                                    'sport' = 'Sport',
                                    'med' = 'Medicine')) %>%
  ggplot(aes(x = year, y = prop, group = Discipline, colour = Discipline))+
  geom_line()+
  geom_point()+
  theme_classic(base_size = 11)+
  scale_y_continuous(n.breaks = 6)+
  labs(x = 'Year', y = 'Proportion of studies reporting\na prediction interval')+
  scale_color_manual(values = c("#56B4E9","#E69F00"))+
  theme(legend.position = c(0.2, 0.9),
        legend.title=element_blank())
ggsave(file = "figure4.png", units="in", width = 5.5, height = 3.25, dpi = 900)



# # Figure 2B: Proportion of studies reporting tau-squared by year
# d %>%
#   group_by(discipline, year) %>%
#   filter(year<2022) %>%
#   count() %>%
#   left_join(d %>%
#               mutate(tau_reported = ifelse(tau_square >=0, TRUE, FALSE)) %>%
#               filter(tau_reported == TRUE) %>%
#               group_by(discipline, year) %>%
#               summarise(tau_reported = n()),
#             by = c('discipline','year')) %>%
#   mutate(prop = tau_reported/n,
#          year = as.factor(year),
#          Discipline = recode_factor(discipline,
#                                     'sport' = 'Sport',
#                                     'med' = 'Medicine')) %>%
#   ggplot(aes(x = year, y = prop, group = Discipline, colour = Discipline))+
#   geom_line()+
#   geom_point()+
#   theme_classic(base_size = 11)+
#   scale_y_continuous(n.breaks = 6)+
#   labs(x = 'Year', y = 'Proportion of studies where a\nPI could be constructed')+
#   scale_color_manual(values = c("#56B4E9","#E69F00"))+
#   theme(legend.title=element_blank())



#### End

