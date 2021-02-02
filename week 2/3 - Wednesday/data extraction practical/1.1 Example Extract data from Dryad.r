# Extract data from Dryad

ddata <- readxl::read_xlsx(
  rdryad::dryad_download('10.5061/dryad.vc80r13')[[1]],
  sheet = 2
)

# suppdata smartly downloads supplementary files from papers and data repositories such as dryad and figshare
# ddata <- readxl::read_xlsx(
#   suppdata::suppdata(x = '10.5061/dryad.vc80r13', si = 'Gibb et al. Journal of Animal Ecology - Data.xlsx'),
#   sheet = 2
# )
