## Morning session: Literature search
During the session, you will use the web of science to do systematic literature searchs

**Make sure you can use web of science. If can't, please login the VPN from University**


## Afternoon session: Data extraction

Please run the following code before the class:
```
install.packages(c('rdryad','suppdata','tabulizer','pdftools','tesseract','taxize','parzer','stringi','zoo','docxtractr','readxl'), dependencies = TRUE)
```

And run this code to download the data sets:
It would help A LOT if you actually followed the folder structure here.  
```
# download data:
# Martinez-Rica_1988.pdf
download.file(url = 'https://digital.csic.es/bitstream/10261/95966/1/Pirineos%20131%20-%20MartinezRica_amphibians_reptiles_Pyrenees.pdf',
              destfile = './Week 2/3 - Wednesday/data extraction practical/cache/Martinez-Rica_1988.pdf',
              mode = 'wb')

# Bruelheide_2020 environmental data supplementary file FOR Extraction from a docx document
download.file(
  url = 'https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fddi.13184&file=ddi13184-sup-0001-AppendixS1-S4.docx',
  destfile = './Week 2/3 - Wednesday/data extraction practical/cache/bruelheide_2020.docx',
  mode = 'wb')
# suppdata::suppdata(x = '10.1111/ddi.13184', si = 1)

# Jimenez-Uzcategui FOR checklist extraction
download.file(
  url = 'https://www.researchgate.net/profile/Gustavo_Jimenez-Uzcategui/publication/258365450_CDF_Checklist_of_Galapagos_Mammals/links/561fcda408aea35f267e0eeb.pdf',
  destfile = './Week 2/3 - Wednesday/data extraction practical/cache/jimenez-uzcategui_2014',
  mode = 'wb')
  
# Wiles_2005 pdf article FOR pdf table extraction
download.file(
  url = 'https://micronesica.org/sites/default/files/8_wiles.pdf',
  destfile = './Week 2/3 - Wednesday/data extraction practical/cache/wiles_2005.pdf',
  mode = 'wb')
```

And please have a look to these files at least to make sure they are intact and at best to see the kind of data we want to extract.  
