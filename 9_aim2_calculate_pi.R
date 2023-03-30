

# Aim 2: Calculate prediction intervals for studies that report tau^2

# Load data and filter for included studies
d <- readRDS('data-prediction intervals.rds') %>%
  pivot_longer(cols = md:risk_ratio) %>%
  mutate(effect_size = recode_factor(name,
                                     'hazard_ratio' = 'ratio',
                                     'odds_ratio' = 'ratio',
                                     'risk_ratio' = 'ratio',
                                     'md' = 'md_smd',
                                     'smd' = 'md_smd'),
         tau_square = as.numeric(tau_square)) %>%
  filter(value == 'TRUE' & tau_square >=0)

# If correction >= 2, then 6 studies with estimates = 2 can't be used!
k_correction <- 2 # = 1 is best case, 2 is more conservative

# Drop studies with only 2 estimates because a PI can't be calculated for them with a k_correction of 2
d <- d %>% filter(estimates > 2)

table(d$effect_size)

# tau_square reporting
d %>% group_by(discipline) %>%
  count()

d %>% filter(tau_square == '0') %>%
  group_by(discipline) %>%
  count()



#### Start with md and smd
d_md_smd <- d %>%
  filter(effect_size == 'md_smd') %>%
  mutate(tau_square_num = as.numeric(tau_square),
         ci_lower = as.numeric(ci_lower),
         ci_upper = as.numeric(ci_upper),
         pooled_effect_num = as.numeric(pooled_effect))

# check numeric conversion NAs, negative tau flagged
d_md_smd %>% filter( is.na(pooled_effect_num) | tau_square < 0) %>%
  select(row_id, tau_square, tau_square_num, pooled_effect, pooled_effect_num)

alpha <- 0.05 # should only be 0.05 unless changes to CI calc can be made
z_95 = qnorm(p = 1 - alpha/2) # Z-score for calculation of standard error
t_dist_fun <- function(k,alpha){qt(p = 1 - {{alpha}}/2, df={{k}}-k_correction)} # t distribution needed for calculation of prediction intervals

interval_ex_zero <- function(a,b){sign(a)*sign(b) >= 0}
interval_ex_one <- function(a,b){sign(a)*sign(b) >= 1}

df_md_smd <- d_md_smd %>% mutate(
  se = ((ci_upper-ci_lower)/(2*z_95)),
  se_square = se^2,
  estimates = as.integer(estimates),
  pooled_effect = as.numeric(pooled_effect),
  tau_square_lower = tau_square, # already coded as zeros
  tau_square_upper = ifelse(tau_square == 0, 0.005, tau_square),
  pi_lower_calc_tau_l = pooled_effect-t_dist_fun(k = estimates, alpha = alpha)*sqrt(se_square+tau_square_lower),
  pi_upper_calc_tau_l = pooled_effect+t_dist_fun(k = estimates, alpha = alpha)*sqrt(se_square+tau_square_lower),
  pi_lower_calc_tau_u = pooled_effect-t_dist_fun(k = estimates, alpha = alpha)*sqrt(se_square+tau_square_upper),
  pi_upper_calc_tau_u = pooled_effect+t_dist_fun(k = estimates, alpha = alpha)*sqrt(se_square+tau_square_upper),
  does_ci_exclude_null = interval_ex_zero(ci_lower, ci_upper),
  does_pi_exclude_null_tau_l = interval_ex_zero(pi_lower_calc_tau_l, pi_upper_calc_tau_l),
  does_pi_exclude_null_tau_u = interval_ex_zero(pi_lower_calc_tau_u, pi_upper_calc_tau_u)
)

table(df_md_smd$does_ci_exclude_null)
table(df_md_smd$does_pi_exclude_null_tau_l) # lower bound of tau (best case)
table(df_md_smd$does_pi_exclude_null_tau_u) # upper bound of tau 

# select important variables
df_md_smd_plot <- df_md_smd %>%
  select(estimates, se, pooled_effect, ci_lower, ci_upper, pi_lower_calc_tau_l, pi_upper_calc_tau_l, pi_lower_calc_tau_u, pi_upper_calc_tau_u, discipline, effect_size,
         does_ci_exclude_null, does_pi_exclude_null_tau_l, does_pi_exclude_null_tau_u)

df_md_smd_plot



#### Deal with ratios
d_ratio <- d %>%
  filter(effect_size == 'ratio') %>%
  mutate(ci_lower = as.numeric(ci_lower),
         ci_upper = as.numeric(ci_upper),
         pooled_effect = as.numeric(pooled_effect),
         tau = sqrt(tau_square), 
         tau_log = ifelse(tau > 1, log(tau), tau),
         tau_square_log = tau_log^2)

df_ratio <- d_ratio %>% mutate(
  se = ((log(ci_upper)-log(ci_lower))/(2*z_95)),
  se_square = se^2,
  estimates = as.integer(estimates),
  tau_square_lower = tau_square_log, # already coded as zeros
  tau_square_upper = ifelse(tau_square_log == 0, 0.005, tau_square_log),
  pi_lower_calc_tau_l = pooled_effect-t_dist_fun(k = estimates, alpha = alpha)*sqrt(se_square+tau_square_lower),
  pi_upper_calc_tau_l = pooled_effect+t_dist_fun(k = estimates, alpha = alpha)*sqrt(se_square+tau_square_lower),
  pi_lower_calc_tau_u = pooled_effect-t_dist_fun(k = estimates, alpha = alpha)*sqrt(se_square+tau_square_upper),
  pi_upper_calc_tau_u = pooled_effect+t_dist_fun(k = estimates, alpha = alpha)*sqrt(se_square+tau_square_upper),
  does_ci_exclude_null = interval_ex_zero(ci_lower, ci_upper),
  does_pi_exclude_null_tau_l = interval_ex_one(pi_lower_calc_tau_l, pi_upper_calc_tau_l),
  does_pi_exclude_null_tau_u = interval_ex_one(pi_lower_calc_tau_u, pi_upper_calc_tau_u)
)

table(df_ratio$does_ci_exclude_null)
table(df_ratio$does_pi_exclude_null_tau_l) # lower bound of tau (best case)
table(df_ratio$does_pi_exclude_null_tau_u) # upper bound of tau 

# select important variables
df_ratio_plot <- df_ratio %>%
  select(estimates, se, pooled_effect, ci_lower, ci_upper, pi_lower_calc_tau_l, pi_upper_calc_tau_l, pi_lower_calc_tau_u, pi_upper_calc_tau_u, discipline, effect_size,
         does_ci_exclude_null, does_pi_exclude_null_tau_l, does_pi_exclude_null_tau_u)

# Join data sets
dsub <- union(df_ratio_plot, df_md_smd_plot)

# Summary
dsub %>% group_by(discipline, does_ci_exclude_null) %>% count() # confidence interval
dsub %>% group_by(discipline, does_pi_exclude_null_tau_l) %>% count() # lower bound of tau (best case)
dsub %>% group_by(discipline, does_pi_exclude_null_tau_u) %>% count() # upper bound of tau 

# Discrepancy for sport
binom.confint(x = 121,
              n = 200,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()

# Discrepancy for sport
binom.confint(x = 29,
              n = 94,
              conf.level = 0.95,
              methods = "exact") %>%
  as_tibble()

# ratio pi to ci
dsub %>% mutate(ci_width = ci_lower-ci_upper,
             pi_width = pi_lower_calc_tau_l-pi_upper_calc_tau_l,
             ratio_ci_to_pi = pi_width/ci_width) %>%
  group_by(discipline) %>%
  summarise(median = round(median(ratio_ci_to_pi),1),
            q_25 = round(quantile(ratio_ci_to_pi, probs = 0.25),1),
            q_75 = round(quantile(ratio_ci_to_pi, probs = 0.75),1))



#### End


