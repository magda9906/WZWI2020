library(tm);
library(memoise);
library(tidyRSS);
library(httr);
library(XML);
library(wordcloud2);
library(stringi);
library(solrium);

shapes<<- list("Circle" = "circle",
              "Diamond"= "diamond",
              "Pentagon" = "pentagon",
              "Star"="star",
              "Triangle" = "triangle");

polaczenie <- SolrClient$new(host="127.0.0.1", port = 8983, path="/solr/rdzen1/select");
aktualnosci<-tidyfeed(feed = "http://www.rss.gofin.pl/zasilki.xml");
stop<-as.vector(unlist(read.csv("stop_words_pl.txt", header = FALSE, sep = ",", fileEncoding = "UTF-8")));

#
getTermMatrix <- memoise(function(shape){
  
})

lematyzacja<-function(tekst)
{
  parametry<-list(lpmn="any2txt|wcrft2", text=tekst, user="magdalena.czechura@onet.pl");
  odpowiedz<-POST("http://ws.clarin-pl.eu/nlprest2/base/process", body=parametry, encode = "json", verbose());
  zawartosc<-content(odpowiedz, "text", encoding = "UTF-8");
  xml<-xmlParse(zawartosc, encoding = "UTF-8");
  slowa<-xpathSApply(xml, '//chunkList/chunk/sentence/tok/lex/base', xmlValue, encoding = "UTF-8");
  return(paste(slowa, collapse = " "));
}



