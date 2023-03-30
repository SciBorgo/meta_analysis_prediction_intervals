

# MA PI reporting
# David N Borg
# May, 2022

# Load these from "Sport extractions" folder
# Sports medicine
load("/Users/david/Downloads/Sport extractions/batch-bjsm.RData"); d1 = meta
load("/Users/david/Downloads/Sport extractions/batch-sports-med.RData"); d2 = meta
load("/Users/david/Downloads/Sport extractions/batch-ajsm.RData"); d3 = meta
load("/Users/david/Downloads/Sport extractions/batch-msse.RData"); d4 = meta
load("/Users/david/Downloads/Sport extractions/batch-jsams.RData"); d5 = meta
load("/Users/david/Downloads/Sport extractions/batch-scando.RData"); d6 = meta
load("/Users/david/Downloads/Sport extractions/batch-res-sports-med.RData"); d7 = meta
load("/Users/david/Downloads/Sport extractions/batch-ejprm.RData"); d8 = meta
load("/Users/david/Downloads/Sport extractions/batch-archives.RData"); d9 = meta
load("/Users/david/Downloads/Sport extractions/batch-jrm.RData"); d10 = meta

# Merge
m1 <- union(d1,d2)
m2 <- union(m1,d3)
m3 <- union(m2,d4)
m4 <- union(m3,d5)
m5 <- union(m4,d6)
m6 <- union(m5,d7)
m7 <- union(m6,d8)
m8 <- union(m7,d9)
m9 <- union(m8,d10)

data <- m9

# Remove duplicates
df = dplyr::filter(data, duplicated(pubmed)==F)

# Check paper numbers
with(df, table(journal, year))
with(df, table(year))

# Save
df %>% write.csv(file = 'data-sports-med-articles.csv', row.names = F)







# Load these from "Med extractions" folder
# Medicine
d1 <- read_csv('batch1.csv')
d2 <- read_csv('batch-jama.csv')
d3 <- read_csv('batch-new-eng.csv')
d4 <- read_csv('plos-med.csv')
d5 <- read_csv('enviro-health-per.csv')
d6 <- read_csv('epi.csv')

# Join
m1 <- union(d1,d2)
m2 <- union(m1,d3)
m3 <- union(m2,d4)
m4 <- union(m3,d5)
m5 <- union(m4,d6)

# Save data
m5 %>% write.csv(file = 'data-medicine-articles.csv', row.names = F)



#### END



