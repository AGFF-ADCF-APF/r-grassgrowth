
library(RCurl)

mapfile_ftp <- paste("Graswachstumkarte_", Jahr, "KW",week, ".svg", sep="")
mapfile_current_ftp <- paste("Graswachstumskarte_aktuell", ".svg", sep="")
curvefile_ftp <- paste("Graswachstumskurve_", Jahr, ".svg", sep="")


ftpserver <- "w006fba4.kasserver.com"
ftpuser <- "f0168d82"
ftppasswd <- "R2Fuciw2pdmETJCdxMdC"
#weeknum
ftpurl <- paste("ftp://",ftpuser,":",ftppasswd,"@",ftpserver,"/archive/",mapfile_ftp,sep="")
ftpUpload(mapfile,ftpurl)

#current
ftpurl <- paste("ftp://",ftpuser,":",ftppasswd,"@",ftpserver,"/",mapfile_current_ftp,sep="")
ftpUpload(mapfile,ftpurl)

#curves
ftpurl <- paste("ftp://",ftpuser,":",ftppasswd,"@",ftpserver,"/",curvefile_ftp,sep="")
ftpUpload(curvefile,ftpurl)

