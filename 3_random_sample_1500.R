

# Random sample 1,500 papers for screening
# 750 articles from medicine
# 750 articles from sports and exercise medicine

# Set seed
set.seed(07-06-2022)

# Load data
d1 <- read_csv('data-medicine-articles.csv') %>%
  mutate(discipline = 'med')

d2 <- read_csv('data-sports-med-articles.csv') %>%
  mutate(discipline = 'sport')

# Random sample from medicine
dsub_med <- d1 %>% slice_sample(n = 750)
with(dsub_med, table(year))

# Random sample from sports medicine
dsub_sport <- d2 %>% slice_sample(n = 750)
with(dsub_sport, table(year))

# Join random samples
dsub <- union(dsub_med,dsub_sport)
with(dsub, table(year))

# Export random sample dataset
dsub %>% mutate(row_id = row_number()) %>%
  write.csv(file = 'data-random-sample-1500.csv', row.names = F)



#### End
  
  
  
