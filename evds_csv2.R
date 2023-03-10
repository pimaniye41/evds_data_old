library(tidyverse)
library(stringr)
library(RCurl)
library(XML)

evds_csv2 <- function(anahtar, 
                      veriseti1, 
                      veriseti2, 
                      baslangic_tarihi, bitis_tarihi, 
                      islem1 = "avg", formul1 = "0", frekans1 = "1",
                      islem2 = "avg", formul2 = "0", frekans2 = "1") {
  adres <- "https://evds2.tcmb.gov.tr/service/evds/"
  tarihler <- paste("&startDate=",baslangic_tarihi,"&endDate=",bitis_tarihi, sep="")
  tamamlayici <- paste("&type=csv&key=",anahtar,sep="")
  
  seri1 <- paste("series=",veriseti1, sep="")
  gozlem1 <- paste("&aggregationTypes=", islem1, sep = "")
  formula1 <- paste("&formulas=", formul1, sep = "")
  frek1 <- paste("&frequency=", frekans1, sep = "")
  
  seri2 <- paste("series=",veriseti2, sep="")
  gozlem2 <- paste("&aggregationTypes=", islem1, sep = "")
  formula2 <- paste("&formulas=", formul1, sep = "")
  frek2 <- paste("&frequency=", frekans1, sep = "")
  
  veriadresi1<-paste(adres, seri1, tarihler, tamamlayici, gozlem1, formula1,frek1, sep="")
  csvveri1 <- getURL(veriadresi1, .opts = list(ssl.verifypeer = FALSE))
  veridf1 = read_csv(csvveri1)
  
  veriadresi2 <- paste(adres, seri2, tarihler, tamamlayici, gozlem2, formula2, frek2, sep="")
  csvveri2 <- getURL(veriadresi2, .opts = list(ssl.verifypeer = FALSE))
  veridf2 = read_csv(csvveri2)
  
  veridfbind = full_join(veridf1,veridf2)
  return(veridfbind)
}

anahtar <- "********"
a <- evds_csv2(anahtar = anahtar, veriseti1 = "TP.DK.USD.A.YTL",
               veriseti2 = "TP.DK.EUR.A.YTL",
               baslangic_tarihi = "01-01-2015",
              bitis_tarihi = "10-03-2023",frekans1 = "5")
a
ggplot(a, aes(x = Tarih, group = 1))+
  geom_line(aes(y = TP_DK_EUR_A_YTL, color = "EUR"))+
  geom_line(aes(y = TP_DK_USD_A_YTL, color = "USD"))
