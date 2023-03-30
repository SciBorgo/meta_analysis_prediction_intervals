

# Aim 1B: papers that reported intervals.
# Load data and filter for included studies
d <- readRDS('data-prediction intervals.rds')

# Check if conclusion between CI and PI would be different
dsub <- d %>%
  pivot_longer(cols = md:risk_ratio) %>%
  filter(value == 'TRUE' & pi_reported == 'TRUE') %>%
  mutate(effect_size = recode_factor(name,
                                     'hazard_ratio' = 'ratio',
                                     'odds_ratio' = 'ratio',
                                     'risk_ratio' = 'ratio',
                                     'md' = 'md_smd',
                                     'smd' = 'md_smd'))

d_pi <- dsub %>%
  mutate(does_ci_exclude_zero = ifelse(ci_lower <= 0 & ci_upper <= 0 | ci_lower >= 0 & ci_upper >= 0, TRUE, FALSE),
         does_pi_exclude_zero = ifelse(pi_lower <= 0 & pi_upper <= 0 | pi_lower >= 0 & pi_upper >= 0, TRUE, FALSE))

table(d_pi$does_ci_exclude_zero)
table(d_pi$does_pi_exclude_zero) # 5/22 would have changed result based on PI

d_pi %>%
  mutate(ci = paste(ci_lower, ci_upper),
         pi = paste(pi_lower, pi_upper)) %>%
  write.csv('table1.csv', row.names = F)


# Ratio of PI width to CI width
d_pi %>% mutate(ci_width = ci_lower-ci_upper,
                pi_width = pi_lower-pi_upper) %>%
  select(row_id, discipline, ci_lower, ci_upper, ci_width, pi_lower, pi_upper, pi_width) %>%
  mutate(ratio_ci_to_pi = pi_width/ci_width) %>%
  group_by(discipline) %>%
  summarise(median = median(ratio_ci_to_pi),
            q_25 = quantile(ratio_ci_to_pi, probs = 0.25),
            q_75 = quantile(ratio_ci_to_pi, probs = 0.75))



# Plot
d_pi %>%
  filter(effect_size == 'ratio') %>%
  mutate(ci_lower = as.numeric(ci_lower),
         ci_upper = as.numeric(ci_upper)) %>%
  ggplot(aes(y = reorder(as.factor(pubmed),-ci_lower), group = as.factor(row_id),
             width = 0.5))+
  geom_errorbar(aes(xmin = pi_lower, xmax = pi_upper),
                colour = 'red',
                width = 0)+
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper))+
  theme_classic(base_size = 10)+
  geom_vline(xintercept = 1, colour = 'blue') +
  #facet_grid(~discipline, scales = 'free') +
  labs(x = 'Ratio', y = 'PubMed ID')




# Calculate PI
d %>% mutate(tau_square = as.numeric(tau_square)) %>%
  filter(tau_square>0) -> t

# MD/SMD
table(t$md)
table(t$smd)

# Ratios
table(t$odds_ratio)
table(t$hazard_ratio)
table(t$risk_ratio)


# Start with MD and SMD
df <- t %>% filter(md == 'TRUE' | smd == 'TRUE')

t_dist_fun <- function(k){qt(p = .975, df={{k}}-2)}
z_95 = 1.96

df <- df %>% mutate(
  se = ((as.numeric(ci_upper)-as.numeric(ci_lower))/(2*z_95)),
  pooled_effect = as.numeric(pooled_effect),
  estimates = as.integer(estimates),
  pi_lower_calc = pooled_effect-t_dist_fun(k = estimates)*sqrt(se+tau_square),
  pi_upper_calc = pooled_effect+t_dist_fun(k = estimates)*sqrt(se+tau_square),
  does_ci_exclude_zero = ifelse(ci_lower <= 0 & ci_upper <= 0 | ci_lower >= 0 & ci_upper >= 0, TRUE, FALSE),
  does_pi_exclude_zero = ifelse(pi_lower_calc <= 0 & pi_upper_calc <= 0 | pi_lower_calc >= 0 & pi_upper_calc >= 0, TRUE, FALSE)
)


df %>%
  mutate(ci_lower = as.numeric(ci_lower),
         ci_upper = as.numeric(ci_upper)) %>%
  ggplot(aes(y = reorder(as.factor(pubmed),-ci_lower), group = as.factor(row_id),
             width = 0.5))+
  geom_errorbar(aes(xmin = pi_lower_calc, xmax = pi_upper_calc),
                colour = 'red',
                width = 0)+
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper))+
  theme_bw(base_size = 10)+
  geom_vline(xintercept = 0, colour = 'blue') #+
  # facet_grid(~'Prediction interval (red) and confidence interval (black) when both reported') +
  # labs(x = 'mean difference, standardised mean difference, or ratio', y = 'PubMed ID')

table(df$does_ci_exclude_zero)
table(df$does_pi_exclude_zero) # 5/22 would have changed result based on PI


dsub = df$does_ci_exclude_zero == df$does_pi_exclude_zero

table(dsub)

120/(186)


#### End
