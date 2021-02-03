# Extraction from a pdf with OCR
pdftools::pdf_ocr_text(pdf = './Week 2/3 - Wednesday/data extraction practical/cache/Martinez-Rica_1988.pdf',
                       pages = 6:7)

# Does not work, may be because of the landscape orientation
picture_paths <- pdftools::pdf_convert(pdf = "./Week 2/3 - Wednesday/data extraction practical/cache/Martinez-Rica_1988.pdf", pages = 6:7, dpi = 600, format = 'png', antialias = 'text')

lst <- lapply(picture_paths, magick::image_read)
lst <- lapply(lst, magick::image_rotate, 90)
ocr <- lapply(lst, magick::image_ocr, language = 'eng')
