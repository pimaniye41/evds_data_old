library(tidyverse);library(XML);library(RCurl)

evds_csv2_demo <- function(anahtar, 
                           veriseti1, 
                           veriseti2, 
                           baslangic_tarihi, bitis_tarihi, 
                           islem1 = "", formul1 = "0", frekans1 = "1",
                           islem2 = "", formul2 = "0", frekans2 = "1") {
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
  
  veriadresi1<-paste(adres, seri1, tarihler, tamamlayici, gozlem1, formula1,frek1, sep="")
  csvveri1 <- getURL(veriadresi1, .opts = list(ssl.verifypeer = FALSE))
  veridf1 = read_csv(csvveri1)
  
  veriadresi2 <- paste(adres, seri2, tarihler, tamamlayici, gozlem2, formula2, frek2, sep="")
  csvveri2 <- getURL(veriadresi2, .opts = list(ssl.verifypeer = FALSE))
  veridf2 = read_csv(csvveri2)
  
  veridfbind <- full_join(veridf1,veridf2) #BURDAKİ SELECTI ONE ALIP YEARWEEKİ SİL PİVOTU ONA GÖRE YAP
  veridfbind <- veridfbind %>% select(!UNIXTIME)
  veridfbind <- if("YEARWEEK" %in% colnames(veridfbind)){
    veridfbind <- pivot_longer(data = veridfbind,
                               cols = c(3:ncol(veridfbind)),
                               names_to = "seri",
                               values_to = "deger")}
  else {pivot_longer(data = veridfbind,
                     cols = c(2:ncol(veridfbind)),
                     names_to = "seri",
                     values_to = "deger")
  }
  veridfbind$deger <- as.numeric(veridfbind$deger)
  
  veridfbind$Tarih <- if(nchar(veridfbind$Tarih[1])<10){
    as.character(veridfbind$Tarih)}
  else{
    as.Date(veridfbind$Tarih,format = "%d-%m-%Y")
  }
  veridfbind <- if(is.character(veridfbind$Tarih)){
    veridfbind} 
  else{arrange(veridfbind, Tarih)
  }
  return(veridfbind)
}


evds_csv2_demo(anahtar = anahtar,veriseti1 = "TP.AB.C1",veriseti2 = "TP.AB.C2",
               baslangic_tarihi = "01-01-2001",bitis_tarihi = format(Sys.Date(),"%d-%m-%Y"),
               frekans1 = "3",frekans2 = "3")
