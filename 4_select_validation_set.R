

# Validation set
# July 6, 2022

# Set seed
set.seed(06-07-2022)

# Load data
dval <- read_xlsx('validation-set-06-07-22.xlsx')

# Sports medicine
dval_sport <- dval %>%  filter(discipline == 'sport')
dsub_sport <- dval_sport %>% slice_sample(n = 150)
with(dsub_sport, table(year))

# Medicine
dval_med <- dval %>%  filter(discipline == 'med')
dsub_med <- dval_med %>% slice_sample(n = 150)
with(dsub_med, table(year))

# Join datasets
dsub <- union(dsub_sport,dsub_med)
with(dsub, table(year))

# Save
dsub %>% write.csv(file = 'data-validation-sample-300.csv', row.names = F)



#### End
