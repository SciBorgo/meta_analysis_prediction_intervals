

# Data extraction errors

d_main <- read_csv('MA Prediction Intervals.csv') %>%
  clean_names() %>%
  select(-journal,
         -year,
         -discipline,
         -reviewer:-random_model,
         -studies,
         -p_value,
         -notes,
         -study_link)
  
d_val <- read_csv('MA PI validation.csv') %>%
  clean_names() %>%
  select(-reviewer,
         -validator,
         -studies,
         -p_value,
         -notes,
         -study_link) %>%
  rename(meta_analysis_v = meta_analysis,
         model_type_v = model_type,
         random_model_v = random_model,
         included_v = included,
         md_v = md,
         smd_v = smd,
         odds_ratio_v = odds_ratio,
         hazard_ratio_v = hazard_ratio,
         risk_ratio_v = risk_ratio,
         auc_v = auc,
         estimates_v = estimates,
         pooled_effect_v = pooled_effect,
         ci_level_v = ci_level,
         ci_lower_v = ci_lower,
         ci_upper_v = ci_upper,
         i_square_v = i_square,
         tau_square_v = tau_square,
         pi_reported_v = pi_reported,
         pi_level_v = pi_level,
         pi_lower_v = pi_lower,
         pi_upper_v = pi_upper,
         which_figure_v = which_figure,
         forest_plot_shown_v = forest_plot_shown)

# Join
d_check <- left_join(d_val, d_main, by = c('row_id','pubmed'))

# Check included
d_check %>% mutate(included_match = if_else(included_v == included,
                                            true = TRUE,
                                            false = FALSE)) %>%
  select(row_id, included_v, included, included_match) %>%
  filter(included_match == 0)

binom.confint(x = 296,
              n = 300,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()


# Select only included studies
dsub <- d_check %>% filter(included_v == 'TRUE') %>%
  filter(!row_id == 464) # Exclude study with row_id 464 as a fixed effect model was used

k = nrow(dsub)  

# Check effect sizes
dsub %>% mutate(md_match = if_else(md_v == md, true = TRUE, false = FALSE)) %>%
  select(row_id, md_v, md, md_match) %>%
  filter(md_match == 0) -> a

dsub %>% mutate(smd_match = if_else(smd_v == smd,  true = TRUE, false = FALSE)) %>%
  select(row_id, smd_v, smd, smd_match) %>%
  filter(smd_match == 0) -> b

dsub %>% mutate(odds_ratio_match = if_else(odds_ratio_v == odds_ratio, true = TRUE, false = FALSE)) %>%
  select(row_id, odds_ratio_v, odds_ratio, odds_ratio_match) %>%
  filter(odds_ratio_match == 0) -> c

dsub %>% mutate(risk_ratio_match = if_else(risk_ratio_v == risk_ratio, true = TRUE, false = FALSE)) %>%
  select(row_id, risk_ratio_v, risk_ratio, risk_ratio_match) %>%
  filter(risk_ratio_match == 0) -> d

dsub %>% mutate(hazard_ratio_match = if_else(hazard_ratio_v == hazard_ratio, true = TRUE, false = FALSE)) %>%
  select(row_id, hazard_ratio_v, hazard_ratio, hazard_ratio_match) %>%
  filter(hazard_ratio_match == 0) -> e

table(dsub$auc)

# Disagreements
a
b
c
d
e

# Pull rows with errors
a %>% select(row_id) -> m1
b %>% select(row_id) -> m2
c %>% select(row_id) -> m3
d %>% select(row_id) -> m4
e %>% select(row_id) -> m5

dat <- rbind(m1,m2,m3,m4,m5)
length(unique(dat$row_id))

# Note: 6 of the 10 disagreements were due to an error in the validation set, not the main data set
correct_in_main = 6
binom.confint(x = k-(length(unique(dat$row_id))-correct_in_main),
              n = k,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()




# Estimates
dsub %>% mutate(estimate_match = estimates_v-estimates) %>%
  filter(!estimate_match == 0) %>%
  select(row_id, estimates_v, estimates, estimate_match) -> p
p

# row_id 371 incorrect in main
# row_id 390 correct in main data set
# row_id 788 correct in main data set
# row_id 1017 incorrect in main
# row_id 1065 incorrect in main
# row_id 1072 correct in main
# row_id 1163 incorrect in main
# row_id 1174 incorrect in main
# row_id 1182 correct in main

correct_in_main = 4
binom.confint(x = k-(nrow(p)-correct_in_main),
              n = k,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()

# Pooled effect
dsub %>% mutate(pooled_match = as.numeric(pooled_effect_v)-as.numeric(pooled_effect)) %>%
  filter(!pooled_match == 0) %>%
  select(row_id, pooled_effect_v, pooled_effect, pooled_match) -> q
q

# row_id 371 incorrect in main
# row_id 788 correct in main
# row_id 828 correct in main
# row_id 1072 correct in main
# row_id 1179 correct in main
# row_id 1182 correct in main

correct_in_main = 5
binom.confint(x = k-(nrow(q)-correct_in_main),
              n = k,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()

# CI %
dsub %>% mutate(ci_level_match = as.numeric(ci_level_v)-as.numeric(ci_level)) %>%
  filter(!ci_level_match == 0) %>%
  select(row_id, ci_level_v, ci_level, ci_level_match) -> r

binom.confint(x = k-nrow(r),
              n = k,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()

# CI lower bound
dsub %>% mutate(ci_lower_match = as.numeric(ci_lower_v)-as.numeric(ci_lower)) %>%
  filter(!ci_lower_match == 0) %>%
  select(row_id, ci_lower_v, ci_lower, ci_lower_match) -> s
s

# row_id 371 incorrect in main
# row_id 788 correct in main
# row_id 1072 correct in main
# row_id 1179 correct in main
# row_id 1182 correct in main

correct_in_main = 4
binom.confint(x = k-(nrow(s)-correct_in_main),
              n = k,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()

# CI upper bound
dsub %>% mutate(ci_upper_match = as.numeric(ci_upper_v)-as.numeric(ci_upper)) %>%
  filter(!ci_upper_match == 0) %>%
  select(row_id, ci_upper_v, ci_upper, ci_upper_match) -> t
t

# row_id 371 incorrect in main
# row_id 732 incorrect in main
# row_id 788 correct in main
# row_id 998 incorrect in main
# row_id 1072 correct in main
# row_id 1179 correct in main
# row_id 1182 correct in main

correct_in_main = 4
binom.confint(x = k-(nrow(t)-correct_in_main),
              n = k,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()

# Forest plot shown
dsub %>% mutate(forest_plot_shown_match = if_else(forest_plot_shown_v == forest_plot_shown,
                                                  true = TRUE,
                                                  false = FALSE)) %>%
  select(row_id, forest_plot_shown_v, forest_plot_shown, forest_plot_shown_match) %>%
  filter(forest_plot_shown_match == FALSE) -> z

z

# row_id 119 incorrect in main
# row_id 145 correct in main
# row_id 261 incorrect in main
# row_id 318 incorrect in main
# row_id 440 correct in main - appendix but in table, not a figure
# row_id 640 correct in main
# row_id 650 correct in main
# row_id 1072 incorrect in main
# row_id 1121 correct in main
# row_id 1363 correct in main
# row_id 1378 incorrect in main

correct_in_main = 6
binom.confint(x = k-(nrow(z)-correct_in_main),
              n = k,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()


# I-squared
dsub %>% mutate(i_square_match = as.numeric(i_square_v)-as.numeric(i_square)) %>%
  filter(!i_square_match == 0) %>%
  select(row_id, i_square_v, i_square, i_square_match) -> u
u

# row_id 339 incorrect in main
# row_id 371 incorrect in main
# row_id 788 correct in main
# row_id 945 incorrect in main
# row_id 1182 correct in main

correct_in_main = 2
binom.confint(x = k-(nrow(u)-correct_in_main),
              n = k,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()

# Tau-squared
dsub %>% mutate(tau_square_match = as.numeric(tau_square_v)-as.numeric(tau_square)) %>%
  filter(!tau_square_match == 0) %>%
  select(row_id, tau_square_v, tau_square, tau_square_match) -> v
v

# row_id 945 incorrect in main
# row_id 1182 correct in main

correct_in_main = 1
binom.confint(x = k-(nrow(v)-correct_in_main),
              n = k,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()

# Was a prediction interval reported
dsub %>% mutate(pi_reported_match = if_else(pi_reported_v == pi_reported,
                                            true = TRUE,
                                            false = FALSE)) %>%
  select(row_id, pi_reported_v, pi_reported, pi_reported_match) %>%
  filter(pi_reported_match == 0)

binom.confint(x = k,
              n = k,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()

# Level of prediction interval
dsub %>% mutate(pi_level_match = as.numeric(pi_level_v)-as.numeric(pi_level)) %>%
  filter(!pi_level_match == 0) %>%
  select(row_id, pi_level_v, pi_level, pi_level_match) -> w
w

binom.confint(x = k-nrow(w),
              n = k,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()

# Prediction interval lower bound
dsub %>% mutate(pi_lower_match = as.numeric(pi_lower_v)-as.numeric(pi_lower)) %>%
  filter(!pi_lower_match == 0) %>%
  select(row_id, pi_lower_v, pi_lower, pi_lower_match) -> x
x

binom.confint(x = k-nrow(x),
              n = k,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()

# Prediction interval upper bound
dsub %>% mutate(pi_upper_match = as.numeric(pi_upper_v)-as.numeric(pi_upper)) %>%
  filter(!pi_upper_match == 0) %>%
  select(row_id, pi_upper_v, pi_upper, pi_upper_match) -> y
y

binom.confint(x = k-nrow(y),
              n = k,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()



#### End


