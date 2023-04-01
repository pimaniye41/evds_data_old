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

evds_csv_demo(anahtar = anahtar,veriseti = "TP.KTF10",baslangic_tarihi = "01-01-2022",
              bitis_tarihi = format(Sys.Date(),"%d-%m-%Y"))
