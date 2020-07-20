
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

pend_miskin <- read.csv("data_input/penduduk_miskin_all.csv")
jumlah_penduduk <- read.csv("data_input/jumlah_penduduk.csv")
theme_algoritma <- readRDS("assets/theme_algoritma.rds")

pend_clean <- pend_miskin %>% 
  filter(tahun != "2017") %>% 
  mutate_if(is.character, as.factor)

jml_pend_clean <- jumlah_penduduk %>% 
  filter(tahun != "2017") %>% 
  mutate_if(is.character, as.factor)

jml_pend <- jml_pend_clean %>% 
  mutate(proyeksi_jumlah_penduduk = proyeksi_jumlah_penduduk * 1000)

merge_penduduk <- merge(pend_clean,jml_pend_clean, by=c("kode_kota_kabupaten","nama_kota_kabupaten", "tahun"))