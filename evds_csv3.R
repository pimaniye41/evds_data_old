library(tidyverse)
library(stringr)
library(RCurl)
library(XML)

evds_csv2 <- function(anahtar, 
                      veriseti1, 
                      veriseti2, 
                      baslangic_tarihi, bitis_tarihi, 
                      islem1 = "avg", formul1 = "0", frekans1 = "1",
                      islem2 = "avg", formul2 = "0", frekans2 = "1",
                      islem3 = "avg", formul3 = "0", frekans3 = "1") {
  
  adres <- "https://evds2.tcmb.gov.tr/service/evds/"
  tarihler <- paste("&startDate=",baslangic_tarihi,"&endDate=",bitis_tarihi, sep="")
  tamamlayici <- paste("&type=csv&key=",anahtar,sep="")
  
  seri1 <- paste("series=",veriseti1, sep="")
  gozlem1 <- paste("&aggregationTypes=", islem1, sep = "")
  formula1 <- paste("&formulas=", formul1, sep = "")
  frek1 <- paste("&frequency=", frekans1, sep = "")
  
  seri2 <- paste("series=",veriseti2, sep="")
  gozlem2 <- paste("&aggregationTypes=", islem2, sep = "")
  formula2 <- paste("&formulas=", formul2, sep = "")
  frek2 <- paste("&frequency=", frekans2, sep = "")
  
  seri3 <- paste("series=",veriseti3, sep="")
  gozlem3 <- paste("&aggregationTypes=", islem3, sep = "")
  formula3 <- paste("&formulas=", formul3, sep = "")
  frek3 <- paste("&frequency=", frekans3, sep = "")
  
  veriadresi1<-paste(adres, seri1, tarihler, tamamlayici, gozlem1, formula1,frek1, sep="")
  csvveri1 <- getURL(veriadresi1, .opts = list(ssl.verifypeer = FALSE))
  veridf1 = read_csv(csvveri1)
  
  veriadresi2 <- paste(adres, seri2, tarihler, tamamlayici, gozlem2, formula2, frek2, sep="")
  csvveri2 <- getURL(veriadresi2, .opts = list(ssl.verifypeer = FALSE))
  veridf2 = read_csv(csvveri2)
  
  veriadresi3 <- paste(adres, seri3, tarihler, tamamlayici, gozlem3, formula3, frek3, sep="")
  csvveri3 <- getURL(veriadresi3, .opts = list(ssl.verifypeer = FALSE))
  veridf3 = read_csv(csvveri3)
  
  veridfbind = full_join(veridf1,veridf2,veridf3)
  return(veridfbind)
}

