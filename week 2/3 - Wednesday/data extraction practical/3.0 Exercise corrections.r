## Exercises corrections
# Choi and An 2010
appendix1 <- tabulizer::extract_tables(file = './Week 2/2 - Tuesday/examples/Choi and An 2010 Altitudinal distribution of moths (Lepidoptera) in Mt. Jirisan National Park,  South Korea.pdf', pages = 6:17, encoding = 'UTF-8', method = 'stream')

# Choi and Thein 2018
download.file(url = 'https://esj-journals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1007%2Fs11284-017-1555-z&file=ere0237-sup-0001.pdf',
              destfile = './Week 2/3 - Wednesday/data extraction practical/cache/Choi_2018.pdf',
              mode = 'wb'
)


tableS1_raw <- tabulizer::extract_tables(file = './Week 2/3 - Wednesday/data extraction practical/cache/Choi_2018.pdf', pages = 1:3, encoding = 'UTF-8', method = 'stream')
tableS1 <- tableS1_raw
column_names <- tableS1[[1]]
tableS1[[2]] <- tableS1[[2]][-(1:2),] # deleting first two rows
tableS1[[2]] <- matrix( # splitting the strings with ' ' and reorganising them in a matrix
  unlist(sapply(tableS1[[2]], strsplit, split = ' ', fixed = TRUE)),
  ncol = 9, byrow = TRUE
)
tableS1[[2]][, 5] <- paste(tableS1[[2]][, 5], tableS1[[2]][, 6], sep = ' ')
tableS1[[2]] <- tableS1[[2]][, -6]





# Longino_2018
download.file(url = 'https://www.ecography.org/sites/ecography.org/files/appendix/ecog-03871.zip',
              destfile = './Week 2/3 - Wednesday/data extraction practical/cache/Longino_2018.zip',
              mode = 'wb'
)

dir.create('./Week 2/3 - Wednesday/data extraction practical/raw data/Longino/', showWarnings = FALSE)
unzip(zipfile = './Week 2/3 - Wednesday/data extraction practical/cache/Longino_2018.zip',
      exdir = './Week 2/3 - Wednesday/data extraction practical/raw data/Longino')

ddata <- read.table('./week 2/3 - Wednesday/data extraction practical/raw data/Longino/ECOG-03871/TableA3.csv', sep = ',', dec = '.', header = TRUE)
