#Sys.setlocale(, 'German')
Sys.setlocale("LC_TIME","de_CH")

# before start: setup credentials
#source("local/keyring.R")

source("01_import_googlesheet.R")
source("21_plot_map.R")
source("22_plot_year.R")
source("31_upload_ftp.R")

