# extraction of a pdf table

mammals_raw <- tabulizer::extract_tables(file = './Week 2/3 - Wednesday/data extraction practical/cache/wiles_2005.pdf', pages = 47:49, encoding = 'UTF-8')

save(mammals_raw, file = './Week 2/3 - Wednesday/data extraction practical/raw data/mammals_raw')
