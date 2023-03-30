

# MA PI reporting
# David N Borg
# May, 2022

# Search parameters
# Get Pubmed IDs from selected journals from 2002 to 2022 (unless otherwise commented)
# Restrict to reviews (and meta-analyses)
# This does include articles with no abstract
types = c('Meta-Analysis') # Article types to include
types.search = paste(paste(types, '[PT]', sep=''), collapse=' OR ', sep='')

# Data extraction batches; not years commented were years searched
journals = c('Br J Sports Med') # c(2003,2005:2022)
journals = c('Sports Med') # c(2002:2004,2008:2022)
journals = c('Am J Sports Med') # c(2003:2022)
journals = c('Med Sci Sports Exerc') # c(2002:2007,2009:2011,2013:2022)
journals = c('J Sci Med Sport') #c(2002,2006,2008:2022)
journals = c('Scand J Med Sci Sports') #c(2004,2005:2007,2010:2022)
journals = c('Res Sports Med') #c(2010,2018,2019:2020,2021)
journals = c('Eur J Phys Rehabil Med') # c(2008,2009:2022)
journals = c('Arch Phys Med Rehabil') #c(2002,2004,2006,2008,2009:2022)
journals = c('J Rehabil Med')

# Pull papers
numbers = ids = NULL
for (j in journals){
  for (year in c(2003,2004,2009,2010:2012,2014:2022)){
    query = paste('"', j, '"[SO] AND ', year, '[PDAT] AND (', types.search , ')', sep='')
    journal.search = entrez_search(db='pubmed', term=query, retmax=50000)
    nframe = data.frame(journal=j, year=year, count=journal.search$count) 
    numbers = rbind(numbers, nframe)
    frame = data.frame(journal=j, year=year, pubmed=journal.search$ids)
    ids = rbind(ids, frame)
  }
}

# Plot papers each year
ggplot(data=numbers, aes(x=year, y=count, col=factor(journal)))+
  geom_point()+
  geom_line()+
  ylab('Number of abstracts')+
  xlab('Year')+
  guides(color=guide_legend(ncol=2))+
  theme_bw()+
  theme(legend.position=c(0.3,0.84), panel.grid.minor = element_blank())

meta = ids

# Remove duplicates
meta = dplyr::filter(meta, duplicated(pubmed)==F)

# Check paper numbers
with(meta, table(journal, year))


setwd("C:/Users/borgdn/OneDrive - Queensland University of Technology/Prediction intervals")
#save(meta, file = 'batch-bjsm.RData')
#save(meta, file = 'batch-sports-med.RData')
#save(meta, file = 'batch-ajsm.RData')
#save(meta, file = 'batch-msse.RData')
#save(meta, file = 'batch-jsams.RData')
#save(meta, file = 'batch-scando.RData')
#save(meta, file = 'batch-res-sports-med.RData')
#save(meta, file = 'batch-ejprm.RData')
#save(meta, file = 'batch-archives.RData')
#save(meta, file = 'batch-jrm.RData')



#### End

