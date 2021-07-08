#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(httr)
library(jsonlite)

## Lets make the predictions

#* @param api
#* @param ortalama
#* @get /predict

predictions = function(api,ortalama){
  api = as.character(api)
  ortalama = as.numeric(ortalama)
  
  res = GET(api)
  
  veri = fromJSON(rawToChar(res$content))
  
  data=veri$address$geo$lat
  
  data = as.numeric(data)
  
  sonuclar = c(1:50)
  
  for(i in 1:50){
    ornek = sample(data , 8 )
    
    p_value = t.test(ornek , mu = ortalama)$p.value
    
    if(p_value<0.05){
      sonuclar[i] = "Red"
    }
    else if(p_value>0.05){
      sonuclar[i] = "Kabul"
    }
  }
  
  sonuclar

  cikti = data.frame(kabul = length(which(sonuclar=="Kabul"))/length(sonuclar),
             red =length(which(sonuclar=="Red"))/length(sonuclar))
  
  return(cikti)
}















