library(outbreaks)
library(incidence)
library(RCurl)

ftpfile <- paste("Graswachstum_", Jahr, "KW",week, ".svg", sep="")
ftpserver <- "w006fba4.kasserver.com"
ftpuser <- "f0168d82"
ftppasswd <- "R2Fuciw2pdmETJCdxMdC"
ftpurl <- paste("ftp://",ftpuser,":",ftppasswd,"@",ftpserver,"/",ftpfile,sep="")

ftpUpload(mapfile,ftpurl)

mapfile
curvefile

