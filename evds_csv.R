library(tidyverse)
library(stringr)
library(RCurl)
library(XML)

evds_csv <- function(anahtar, veriseti, baslangic_tarihi, bitis_tarihi, islem = "avg", formul = "0", frekans = "1") {
  adres <- "https://evds2.tcmb.gov.tr/service/evds/"
  seri <- paste("series=",veriseti, sep="")
  tarihler <- paste("&startDate=",baslangic_tarihi,"&endDate=",bitis_tarihi, sep="")
  tamamlayici <- paste("&type=csv&key=",anahtar,sep="")
  gozlem <- paste("&aggregationTypes=", islem, sep = "")
  formula <- paste("&formulas=", formul, sep = "")
  frek <- paste("&frequency=", frekans, sep = "")
  veriadresi<-paste(adres, seri, tarihler, tamamlayici, gozlem, formula,frek, sep="")
  csvveri <- getURL(veriadresi, .opts = list(ssl.verifypeer = FALSE))
  veridf = read_csv(csvveri)
  return(veridf)
}
<<<<<<< HEAD

anahtar <- "GvQZCLvAbW"
a <- evds_csv(anahtar = anahtar, veriseti = "TP.DK.USD.S.YTL",baslangic_tarihi = "01-01-2015",
         bitis_tarihi = format(Sys.Date(),"%d-%m-%Y"),frekans = "2")
a
=======
>>>>>>> 05395dacc284f42deabcc1feba21dac2e29958b8
