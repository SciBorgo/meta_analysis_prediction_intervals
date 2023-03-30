

# Data wrangling and cleaning
# Load data
d <- read_csv('MA Prediction Intervals_final.csv') %>%
  clean_names()

d %>% # sports medicine
  filter(discipline == 'sport') %>%
  group_by(journal) %>%
  count() %>%
  arrange(-n)
  
d %>% # medicine
  filter(discipline == 'med') %>%
  group_by(journal) %>%
  count() %>%
  arrange(-n)

# Missing data
vis_miss(d)

# Checks
names(d)
length(unique(d$pubmed))
table(d$meta_analysis) # excluded because not meta-analysis
d_false <- d %>% filter(meta_analysis == 'FALSE')
table(d_false$model_type)

# Papers by journal
#with(d %>% filter(discipline == 'med'), table(journal))
#with(d %>% filter(discipline == 'sport'), table(journal))

dsub <- d %>% filter(meta_analysis == 'TRUE',
                     model_type %in% c('Standard'),
                     random_model == 'TRUE',
                     auc == 'FALSE')

# Papers included
#with(dsub %>% filter(discipline == 'med'), table(journal))
#with(dsub %>% filter(discipline == 'sport'), table(journal))

## Data checks
# Ratios
dsub %>% filter(odds_ratio == 'TRUE'|risk_ratio == 'TRUE'|hazard_ratio == 'TRUE') -> d_ratios
d_ratios %>% filter(pooled_effect<0) %>% select(row_id, pubmed, pooled_effect)
d_ratios %>% filter(ci_lower<0) %>% select(row_id, pubmed, ci_lower)
d_ratios %>% filter(ci_upper<0) %>% select(row_id, pubmed, ci_upper)


# Check summary is inside the confidence interval
dsub %>% mutate(mistake = as.numeric(pooled_effect) < as.numeric(ci_lower) | as.numeric(pooled_effect) > as.numeric(ci_upper)) %>%
  filter(mistake == 'TRUE') %>%
  select(row_id, pubmed, discipline, mistake, which_figure, estimates, pooled_effect, ci_lower, ci_upper)

# row_id 26 is an reporting error in the paper
# row_id 888 is a reporting error in the paper
# row_id 1414 reporting error in paper

df <- dsub %>% filter(!row_id %in% c(26,621,648,888,1139,1414)) # remove negative ratios and reporting errors

df %>% # sports medicine
  filter(discipline == 'sport') %>%
  group_by(journal) %>%
  count() %>%
  arrange(-n)

df %>% # medicine
  filter(discipline == 'med') %>%
  group_by(journal) %>%
  count() %>%
  arrange(-n)


# Save
saveRDS(df, file = 'data-prediction intervals.rds')


#### End

