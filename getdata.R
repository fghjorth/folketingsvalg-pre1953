setwd("~/Documents/GitHub/folketingsvalg-pre1953")

library(tesseract)
library(magick)
library(stringr)
library(tidyverse)

# 1939-1950 -----

# source: https://danmarkshistorien.dk/leksikon-og-kilder/vis/materiale/valgtema-besaettelse-og-befrielse-1939-1950/

getyrres <- function(year,lettervec){
  
  imgx <- image_read(paste0("images/",year,".jpg"))
  
  ocrx <- image_ocr(imgx)
  
  ocrxtib1 <- ocrx %>% 
    str_split(.,"\n") %>% 
    pluck(1) %>% 
    as_tibble() %>% 
    rename(original=value)
  
  eligible <- ocrxtib1$original[str_which(ocrxtib1$original,"stemmeberettigede")] %>% 
    str_remove_all(.,"\\.") %>% 
    str_extract(.,"[0-9]{7,8}") %>% 
    as.numeric()
  
  validvotes <- ocrxtib1$original[str_which(ocrxtib1$original,"gyldige")] %>% 
    str_remove_all(.,"\\.") %>% 
    str_extract(.,"[0-9]{7,8}") %>% 
    as.numeric()
  
  ocrxtib2 <- ocrxtib1 %>% 
    mutate(partyvotes=str_extract(original,"[0-9]{1,3},[0-9]{3}"),
           partyvotes=as.numeric(str_remove(partyvotes,","))) %>% 
    filter(!is.na(partyvotes)) %>% 
    mutate(letter=lettervec)
  
  ocrxtib_wide <- ocrxtib2 %>% 
    dplyr::select(partyvotes,letter) %>% 
    pivot_wider(names_from = letter,values_from=partyvotes,names_prefix="party_") %>% 
    mutate(total_valid=validvotes,
           electorate=eligible,
           year=year) %>% 
    dplyr::select(year,everything())
  
  return(ocrxtib_wide)
}

yr1939 <- getyrres(1939,c("s","rv","k","v","rfb","bp","dkp","n","ds","sp","ns" ))
yr1943 <- getyrres(1943,c("s","rv","k","v","rfb","bp","n","ds"))
yr1945 <- getyrres(1945,c("s","rv","k","v","rfb","dkp","ds"))
yr1947 <- getyrres(1947,c("s","rv","k","v","rfb","dkp","ds","ufp"))
yr1950 <- getyrres(1950,c("s","rv","k","v","rfb","dkp","ufp"))

#pre-1939 elections ----

#1935
#source: https://www.dst.dk/Site/Dst/Udgivelser/GetPubFile.aspx?id=20219&sid=valg1935 , p. 21
yr1935 <- tibble(letter=c("dkp","n","rfb","ff","k","rv","sp","s","v"),
                 partyvotes=c(27135,16257,41199,52793,293393,151507,12617,759102,292247)) %>% 
  pivot_wider(names_from = letter,values_from=partyvotes,names_prefix="party_") %>% 
  mutate(total_valid=(1651132-4694),
         electorate=2044997,
         year=1935) %>% 
  dplyr::select(year,everything())

#1932
#source: https://www.dst.dk/Site/Dst/Udgivelser/GetPubFile.aspx?id=20220&sid=valg1932 , p. 15
yr1932 <- tibble(letter=c("dkp","rfb","k","rv","sp","s","v"),
                 partyvotes=c(17179,41238,289531,145220,9868,660839,381862)) %>% 
  pivot_wider(names_from = letter,values_from=partyvotes,names_prefix="party_") %>% 
  mutate(total_valid=(1551121-4039),
         electorate=1902835,
         year=1932) %>% 
  dplyr::select(year,everything())

#1929
#source: https://www.dst.dk/Site/Dst/Udgivelser/GetPubFile.aspx?id=20220&sid=valg1932 , p. 15
yr1929 <- tibble(letter=c("dkp","rfb","k","rv","sp","s","v"),
                 partyvotes=c(3656,25810,233935,151746,9787,593191,402121)) %>% 
  pivot_wider(names_from = letter,values_from=partyvotes,names_prefix="party_") %>% 
  mutate(total_valid=(1423150-2904),
         electorate=1786092,
         year=1929) %>% 
  dplyr::select(year,everything())

# combine -----

votes <- bind_rows(yr1929,yr1932,yr1935,yr1939,yr1943,yr1945,yr1947,yr1950)

write_csv(votes,"votes.csv")
