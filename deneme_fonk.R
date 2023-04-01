library(tidyverse);library(XML);library(RCurl)

evds_csv_demo <- function(anahtar, veriseti, baslangic_tarihi, bitis_tarihi, islem = "avg", formul = "0", frekans = "1") {
  adres <- "https://evds2.tcmb.gov.tr/service/evds/"
  seri <- paste("series=",veriseti, sep="")
  tarihler <- paste("&startDate=",baslangic_tarihi,"&endDate=",bitis_tarihi, sep="")
  tamamlayici <- paste("&type=csv&key=",anahtar,sep="")
  gozlem <- paste("&aggregationTypes=", islem, sep = "")
  formula <- paste("&formulas=", formul, sep = "")
  frek <- paste("&frequency=", frekans, sep = "")
  veriadresi<-paste(adres, seri, tarihler, tamamlayici, gozlem, formula,frek, sep="")
  csvveri <- getURL(veriadresi, .opts = list(ssl.verifypeer = FALSE))
  
  veridf <- read_csv(csvveri)
  veridf <- veridf %>% select(!UNIXTIME)
  
  veridf <- if("YEARWEEK" %in% colnames(veridf)){
    veridf <- pivot_longer(data = veridf,
                           cols = c(3:ncol(veridf)),
                           names_to = "seri",
                           values_to = "deger")}
  else {pivot_longer(data = veridf,
                     cols = c(2:ncol(veridf)),
                     names_to = "seri",
                     values_to = "deger")
  }
  
  veridf$deger <- as.numeric(veridf$deger)
  
  veridf$Tarih <- if(nchar(veridf$Tarih[1])<10){
    as.character(veridf$Tarih)}
  else{
    as.Date(veridf$Tarih,format = "%d-%m-%Y")
    }
  veridf <- if(is.character(veridf$Tarih)){
    veridf} 
  else{arrange(veridf, Tarih)
  }
  return(veridf)
}

evds_csv_demo(anahtar = anahtar,veriseti = "TP.KTF10",baslangic_tarihi = "01-01-2021",
              bitis_tarihi = format(Sys.Date(),"%d-%m-%Y"))

###### ##### ##### ###### ##### 

evds_csv2_demo <- function(anahtar, 
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
  gozlem2 <- paste("&aggregationTypes=", islem2, sep = "")
  formula2 <- paste("&formulas=", formul2, sep = "")
  frek2 <- paste("&frequency=", frekans2, sep = "")
  
  veriadresi1<-paste(adres, seri1, tarihler, tamamlayici, gozlem1, formula1,frek1, sep="")
  csvveri1 <- getURL(veriadresi1, .opts = list(ssl.verifypeer = FALSE))
  veridf1 = read_csv(csvveri1)
  
  veriadresi2 <- paste(adres, seri2, tarihler, tamamlayici, gozlem2, formula2, frek2, sep="")
  csvveri2 <- getURL(veriadresi2, .opts = list(ssl.verifypeer = FALSE))
  veridf2 = read_csv(csvveri2)
  
  veridfbind <- full_join(veridf1,veridf2)
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



evds_csv2_demo(anahtar = anahtar,veriseti1 = "TP.DK.USD.A",veriseti2 = "TP.DK.EUR.A",
               baslangic_tarihi = "01-01-2023",bitis_tarihi = format(Sys.Date(),"%d-%m-%Y"),
               frekans1 = "2",frekans2 = "2")

#### #### --------------------

evds_csv3_demo <- function(anahtar, 
                           veriseti1, 
                           veriseti2, 
                           veriseti3,
                           baslangic_tarihi, bitis_tarihi, 
                           islem1 = "avg", formul1 = "0", frekans1 = "2",
                           islem2 = "avg", formul2 = "0", frekans2 = "2",
                           islem3 = "avg", formul3 = "0", frekans3 = "2") {
  
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
  
  veridfbind_1 = full_join(veridf1,veridf2,join_by(Tarih, UNIXTIME))
  veridfbind = full_join(veridfbind_1,veridf3,join_by(Tarih, UNIXTIME))
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

evds_csv3_demo(anahtar = anahtar,veriseti1 = "TP.DK.USD.A.YTL",
               veriseti2 = "TP.DK.EUR.A.YTL",veriseti3 = "TP.DK.GBP.A.YTL",
               baslangic_tarihi = "01-01-2021",bitis_tarihi = format(Sys.Date(),"%d-%m-%Y"),
               frekans1 = "5",frekans2 = "5",frekans3 = "5")


#----------------------------------------#

evds_csv4_demo <- function(anahtar, 
                           veriseti1, 
                           veriseti2, 
                           veriseti3,
                           veriseti4,
                           baslangic_tarihi, bitis_tarihi, 
                           islem1 = "avg", formul1 = "0", frekans1 = "2",
                           islem2 = "avg", formul2 = "0", frekans2 = "2",
                           islem3 = "avg", formul3 = "0", frekans3 = "2",
                           islem4 = "avg", formul4 = "0", frekans4 = "2") {
  
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
  
  seri4 <- paste("series=",veriseti4, sep="")
  gozlem4 <- paste("&aggregationTypes=", islem4, sep = "")
  formula4 <- paste("&formulas=", formul4, sep = "")
  frek4 <- paste("&frequency=", frekans4, sep = "")
  
  veriadresi1<-paste(adres, seri1, tarihler, tamamlayici, gozlem1, formula1,frek1, sep="")
  csvveri1 <- getURL(veriadresi1, .opts = list(ssl.verifypeer = FALSE))
  veridf1 = read_csv(csvveri1)
  
  veriadresi2 <- paste(adres, seri2, tarihler, tamamlayici, gozlem2, formula2, frek2, sep="")
  csvveri2 <- getURL(veriadresi2, .opts = list(ssl.verifypeer = FALSE))
  veridf2 = read_csv(csvveri2)
  
  veriadresi3 <- paste(adres, seri3, tarihler, tamamlayici, gozlem3, formula3, frek3, sep="")
  csvveri3 <- getURL(veriadresi3, .opts = list(ssl.verifypeer = FALSE))
  veridf3 = read_csv(csvveri3)
  
  veriadresi4 <- paste(adres, seri4, tarihler, tamamlayici, gozlem4, formula4, frek4, sep="")
  csvveri4 <- getURL(veriadresi4, .opts = list(ssl.verifypeer = FALSE))
  veridf4 = read_csv(csvveri4)
  
  veridfbind_2 = full_join(veridf1,veridf2,join_by(Tarih, UNIXTIME))
  veridfbind_1 = full_join(veridfbind_2,veridf3,join_by(Tarih, UNIXTIME))
  veridfbind = full_join(veridfbind_1,veridf4,join_by(Tarih, UNIXTIME))
  
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
evds_csv4_demo(anahtar = anahtar,
               veriseti1 = "TP.DK.USD.A.YTL",
               veriseti2 = "TP.DK.EUR.A.YTL",
               veriseti3 = "TP.DK.GBP.A.YTL",
               veriseti4 = "TP.DK.KWD.A.YTL",
               baslangic_tarihi = "01-03-2023",bitis_tarihi = format(Sys.Date(),"%d-%m-%Y"))






