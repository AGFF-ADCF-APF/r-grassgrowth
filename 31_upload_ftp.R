
library(RCurl)

mapfile_ftp <- paste0("Graswachstumkarte_", Jahr, "KW",week, ".svg")
mapfile_current_ftp <- paste0("Graswachstumskarte_aktuell", ".svg")
curvefile_ftp <- paste0("Graswachstumskurve_", Jahr, ".svg")
curvefile_plotly_ftp <- paste0("Graswachstumskurve_ohneLegende_", Jahr, ".html")


## configure your own ftp settings
#ftpserver <- ""
#ftpuser <- ""
#ftppasswd <- kb$get("")

#weeknum
ftpurl <- paste("ftp://",ftpuser,":",ftppasswd,"@",ftpserver,"/archive/",mapfile_ftp,sep="")
ftpUpload(mapfile,ftpurl)

#current
ftpurl <- paste("ftp://",ftpuser,":",ftppasswd,"@",ftpserver,"/",mapfile_current_ftp,sep="")
ftpUpload(mapfile,ftpurl)

#curves
ftpurl <- paste("ftp://",ftpuser,":",ftppasswd,"@",ftpserver,"/",curvefile_ftp,sep="")
ftpUpload(curvefile,ftpurl)

#curves
ftpurl <- paste("ftp://",ftpuser,":",ftppasswd,"@",ftpserver,"/",curvefile_plotly_ftp,sep="")
ftpUpload(curvefile_plotly,ftpurl)

