# Structuring data

mammals <- lapply(mammals_raw, function(mat) as.data.frame(mat))


# harmonising column names for all and deleting first row for all
mammals <- lapply(mammals, function(dt) setNames(dt, c('english_name','species', unlist(dt[1,])[-(1:2)])))
mammals <- lapply(mammals, function(dt) dt[-1,])

ddata <- do.call("rbind", mammals)

ddata$family <- ifelse(grepl('AE$', ddata$species), # If species ends with "AE" (here $ means "end of the string")
                       ddata$species, # write family name
                       NA_character_) # write NA
ddata$family <- zoo::na.locf(ddata$family)  #complete NA values with last non-missing value

ddata$species <- ifelse(ddata$species == '', # if species name is missing, extract the last two words of english_name
                        stringi::stri_extract_first_regex(ddata$english_name, '(?<= )[A-Za-z]+ [a-z]+$'),
                        ddata$species)

ddata <- ddata[ !is.na(ddata$species) & ddata$species != ddata$family & !grepl('Total', ddata$english_name), ]

