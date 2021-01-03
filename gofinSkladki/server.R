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

lematyzacja<-function(tekst)
{
  parametry<-list(lpmn="any2txt|wcrft2", text=tekst, user="magdalena.czechura@onet.pl");
  odpowiedz<-POST("http://ws.clarin-pl.eu/nlprest2/base/process", body=parametry, encode = "json", verbose());
  zawartosc<-content(odpowiedz, "text", encoding = "UTF-8");
  xml<-xmlParse(zawartosc, encoding = "UTF-8");
  slowa<-xpathSApply(xml, '//chunkList/chunk/sentence/tok/lex/base', xmlValue, encoding = "UTF-8");
  return(paste(slowa, collapse = " "));
}

function(input, output){
  
  polaczenie <- SolrClient$new(host="127.0.0.1", port = 8983, path="/solr/rdzen1/select")
  
  #aktualnosci
  aktualnosci<-tidyfeed(feed = "http://www.rss.gofin.pl/zasilki.xml");
  stop<-as.vector(unlist(read.csv("stop_words_pl.txt", header = FALSE, sep = ",", fileEncoding = "UTF-8")));
  
  opis<-aktualnosci$entry_content;
  
  dane <- data.frame(matrix(ncol=2, nrow=10));
  colnames(dane)[1]<-"id";
  colnames(dane)[2]<-"content";
  dane$id<-c(1,2,3,4,5,6,7,8,9,10);
  dane$content<-c(opis);
  
  solrium::add(x=dane, conn=polaczenie, name="rdzen1");
  
  baza<-solr_search(conn=polaczenie, params=list(q="*:*", rows=-1));
  
  dokumenty<-Corpus(VectorSource(stri_enc_toutf8(baza)));
  
  dokumenty<-tm_map(dokumenty, removePunctuation);
  dokumenty<-tm_map(dokumenty, removeNumbers);
  dokumenty<-tm_map(dokumenty, removeWords, stop);
  
  for (d in 1:length(dokumenty)) {
    
    dokumenty[[d]]$content<-lematyzacja(dokumenty[[d]]$content);
    dokumenty[[d]]$content<-stri_enc_toutf8(dokumenty[[d]]$content);
    
  }
  
  
  tdml<-TermDocumentMatrix(dokumenty);
  m1<-as.matrix(tdml);
  v<-sort(rowSums(m1), decreasing = TRUE);
  d<-data.frame(words=names(v), freq=v);
  
  output$cloud<- renderWordcloud2({
    
    wordcloud2(d, size=input$size, shape = input$selection, minSize = input$freq);
  })
  
  
}