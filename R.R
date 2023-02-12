

library(httr) #memanggil data
library(dplyr) #cleaning data
library(ggplot2) #visualisasi data
library(lubridate) #visualisasi data
library(tidyr) #visualisasi data

###akses api
data <- GET ("https://data.covid19.go.id/public/api/update.json")
data

###memanggil data
data_jateng <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_TENGAH.json")
cov_jateng_raw <- content(data_jateng, as="parsed", simplifyVector= TRUE)
names(cov_jateng_raw)
View(cov_jateng_raw)
cov_jateng_raw$kasus_total
cov_jateng_raw$meninggal_persen
cov_jateng_raw$sembuh_persen
cov_jateng <- cov_jateng_raw$list_perkembangan
str(cov_jateng)
head(cov_jateng)

###cLEANING DATA
library(dplyr)
new_cov_jateng <-
  cov_jateng %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
str(new_cov_jateng)  

###VISUALISASI DATA
library(ggplot2)
ggplot(new_cov_jateng, aes(x = tanggal, y = kasus_baru)) +
  geom_col()

#Kasus Harian Positif Covid19
library(ggplot2)
ggplot(new_cov_jateng, aes(tanggal, kasus_baru)) +
  geom_col(fill = "salmon") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Positif COVID-19 di Jawa Tengah",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme(plot.title.position = "plot")

#Apakah pekan ini lebih baik dari pekan kemarin?#
library(ggplot2)
ggplot(new_cov_jateng, aes(tanggal, meninggal)) +
  geom_col(fill = "darkslategray4") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Meninggal Akibat COVID-19 di Jawa Tengah",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme(plot.title.position = "plot")

#Kasus Pekanan Positif Covid19
library(ggplot2)
ggplot(cov_jateng_pekanan, aes(pekan_ke, jumlah))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(breaks=9:38, expand=c(0,0))+
  scale_fill_manual(values=c("TRUE"="seagreen3", "FALSE"="salmon"))+
  labs(
    x=NULL,
    y="Jumlah kasus",
    title="Kasus Pekanan Positif COVID-19 di Jawa Tengah",
    subtitle="Kolom hijau menunjukan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya",
    caption="Sumber data: covid.19.go.id"
  )+
  theme(plot.title.position="plot")

##Akumulasi Kasus Covid19
library(dplyr)
cov_jateng_akumulasi <- 
  new_cov_jateng %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )

tail(cov_jateng_akumulasi)

#Membuat Line Chart
library(ggplot2)
ggplot(data = cov_jateng_akumulasi, aes(x = tanggal, y = akumulasi_aktif)) +
  geom_line()
