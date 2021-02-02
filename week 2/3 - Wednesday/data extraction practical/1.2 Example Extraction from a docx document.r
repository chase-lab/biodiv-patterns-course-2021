# Extraction from a docx document

# environment data: area this is supplementary data 1 from https://doi.org/10.1111/ddi.13184
env <- docxtractr::docx_extract_tbl(
  docxtractr::read_docx('./Week 2/3 - Wednesday/data extraction practical/cache/bruelheide_2020.docx'),
  tbl_number = 1,
  header = TRUE,
  trim = TRUE
)

