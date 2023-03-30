

# MA PI reporting
# David N Borg
# May, 2022

# Search parameters
# Get Pubmed IDs from selected journals from 2002 to 2022 (unless otherwise commented)
# Restrict to journal articles and reviews
# This does include articles with no abstract
types = c('Meta-Analysis') # article types to include
types.search = paste(paste(types, '[PT]', sep=''), collapse=' OR ', sep='')

# Data extraction batches; uncomment and run each of these seperately
#journals = c('Ann Intern Med','BMJ','Lancet')
#journals = c('JAMA') # 2005 to 2022
#journals = c('N Engl J Med') # c(2005,2007,2008,2009:2017,2020)
journals = c('Epidemiology')
#journals = c('PLoS Med') #c(2005:2022)
#journals = c('Environ Health Perspect')

# Pull papers
numbers = ids = NULL
for (j in journals){
  for (year in c(2002:2006,2008,2010:2015,2017:2019,2021)){
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
  scale_color_manual('Journal', values=cbPalette)+
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

#write.csv(meta, file = 'batch1.csv', row.names = F)
#write.csv(meta, file = 'batch-jama.csv', row.names = F)
#write.csv(meta, file = 'batch-new-eng.csv', row.names = F)
#write.csv(meta, file = 'plos-med.csv', row.names = F)
#write.csv(meta, file = 'enviro-health-per.csv', row.names = F)
#write.csv(meta, file = 'epi.csv', row.names = F)


#### End



