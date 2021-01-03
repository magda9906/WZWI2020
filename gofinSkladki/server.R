library(tm);
library(memoise);
library(tidyRSS);
library(httr);
library(XML);
library(wordcloud2);
library(stringi);
library(solrium);
library(dbscan);
library(cluster);

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
  dokumenty<-tm_map(dokumenty, removeWords, "wwwgofinplskladkizasilkiemerytury");
  
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
  
  
  #klasteryzacja
  
  tabela<-DocumentTermMatrix(dokumenty, control=list(weighting=weightTfIdf));
  
  
  #--------------------------------------------------------------------
  
  klasteryzacja.i<-kmeans(tabela, centers=2, iter.max=100);
  
  klastry.i<-klasteryzacja.i$cluster;
  
  tabela.wynikowa.i<-cbind(tabela, klastry.i);
  
  klasteryzacja.i$centers
  
  klasteryzacja.i$size
  
  #--------------------------------------------------------------------
  
  klasteryzacja.h<-agnes(tabela, metric="euclidean", method="average");
  
  klastry.h<-cutree(klasteryzacja.h, k=2);
  
  tabela.wynikowa.h<-cbind(tabela, klastry.h);
  
  output$cluster<-renderPlot({
    plot(klasteryzacja.h, which.plots=2);
  })
  
  #--------------------------------------------------------------------
  
  min.points<-10;
  epsilon<-0.2;
  
  klasteryzacja.dbscan<-dbscan(tabela, eps=epsilon, minPts=min.points);
  klastry.db<-klasteryzacja.dbscan$cluster;
  
  tabela.wynikowa.db<-cbind(tabela, klastry.db);
  
  #--------------------------------------------------------------------
  
  
  tabela.wynikowa.<-cbind(tabela, klastry.i, klastry.h, klastry.db);
  
  
}