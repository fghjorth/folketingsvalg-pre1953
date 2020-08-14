setwd("~/Documents/GitHub/folketingsvalg-pre1953")

library(tesseract)
library(magick)
library(stringr)
library(tidyverse)

lettervec <- 

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

votes <- bind_rows(yr1939,yr1943,yr1945,yr1947,yr1950)

write_csv(votes,"votes.csv")
