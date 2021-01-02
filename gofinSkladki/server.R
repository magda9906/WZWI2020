library(tm);
library(memoise);
library(tidyRSS);
library(httr);
library(XML);
library(wordcloud2);
library(stringi);
library(solrium);


function(input, output){
  
polaczenie <- SolrClient$new(host="127.0.0.1", port = 8983, path="/solr/rdzen1/select")

#aktualnosci

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

