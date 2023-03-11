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
  
  veridf <- read_csv(csvveri)
  veridf <- veridf %>% select(!UNIXTIME)
  veridf <- pivot_longer(data = veridf, 
                         cols = (!c(Tarih, YEARWEEK)),
                         names_to = "veriseti",
                         values_to = "deger")
  veridf$Tarih <- as.Date(veridf$Tarih, format = "%d-%m-%Y")
  veridf <- arrange(veridf, Tarih)
  return(veridf)
}
strftime()
df <- evds_csv(anahtar = "GvQZCLvAbW",veriseti = "TP.DK.EUR.A.YTL",
         baslangic_tarihi = "01-01-2023",
         bitis_tarihi = format(Sys.Date(),"%d-%m-%Y"),frekans = "3")

ggplot(df, aes(x = Tarih,group= veriseti))+
  geom_point(aes(y = deger, color = YEARWEEK))+
  theme(axis.text.x = element_text(angle = 90))
