# Extraction of a checklist

txt <- pdftools::pdf_text('./Week 2/3 - Wednesday/data extraction practical/cache/jimenez-uzcategui_2014')
txt <- txt[3:15]

txt <- lapply(txt, gsub, pattern = '\\(|\\)', replacement = '')

# extracting strings of interest
species_names <- lapply(stringi::stri_extract_all_regex(txt, '(?<=\r\n[0-9]{1,3}\\. ).+(?=\r\n)|(?<=^[0-9]{1,3}\\. ).+(?=\r\n)'),
                        gsub, pattern = ' [A-Z].*$|ined.', replacement = '')
status <- stringi::stri_extract_all_regex(txt, '(?<=\r\n {1,15}Origin: ).*(?=, [A-Za-z]*\\.\r\n)')
distribution <- stringi::stri_extract_all_regex(txt, '(?<= {1,15}Galapagos Distribution: ).*?(?=\\. *)')

# cleaning strings
species_names[[13]] <- NULL

distribution[[5]] <- append(distribution[[5]], 'Española, Floreana, Isabela, Marchena, Pinta, San Cristóbal, Santa Cruz, Santa Fé, Santiago', 1)
distribution[[11]] <- append(distribution[[11]], 'Fernandina, Floreana, Isabela, Marchena, Pinzón, San Cristóbal, Santa Cruz, Santiago', 2)
distribution[[12]] <- c(distribution[[12]], 'Española, Fernandina, Floreana, Isabela, Pinta, San Cristóbal, Santa Cruz, Santa Fé, Santiago')

ddata <- data.frame(species = unlist(species_names),
                    status = unlist(status),
                    distribution = unlist(distribution)
)

ddata[ddata$species %in% c('Aegialomys galapagoensis','Megaoryzomys curioi','Megaoryzomys sp. 1','Nesoryzomys darwini','Nesoryzomys indefessus','Nesoryzomys sp. 1','Nesoryzomys sp. 2','Nesoryzomys sp. 3'), "status"] <- 'NativeExtinct'
