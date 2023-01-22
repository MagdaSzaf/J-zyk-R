library(RSelenium)
library(rvest)
#usuwa obiekty (rozwiazanie problemu z zajetym portem):
#rm(rd)
#rm(remDr)

miasta<-c("bydgoszcz","gdansk","katowice","lublin","lodz","poznan","szczecin","warszawa","wroclaw","krakow")


rd <- RSelenium::rsDriver(browser = "chrome",chromever = "108.0.5359.71" )
#rd <- RSelenium::rsDriver(browser = "chrome",chromever = "108.0.5359.22" )
remDr <- rd[['client']]

#miasta<-c("bydgoszcz", "katowice")
miastaDF<-NULL
for(miasto in miasta){
  Sys.sleep(5)
  url<-sprintf("https://www.otodom.pl/pl/oferty/sprzedaz/mieszkanie/%s?areaMax=38&page=1", miasto)
  read_html(url)%>%html_nodes("eoupkm71 css-190hi89 e11e36i3")
  
  remDr$navigate(url)
  Sys.sleep(1)
  pageFromSelenium <- remDr$getPageSource()[[1]] %>% rvest::read_html()
  przyciski <- pageFromSelenium%>%html_elements(".eoupkm71.css-190hi89.e11e36i3")
  ileStron <-   as.numeric(przyciski[ (length(przyciski))-1 ]  %>% html_text())
  
  
  wektorLinkow<-c()
  for ( i in 1:ileStron){
    urll<- sprintf("https://www.otodom.pl/pl/oferty/sprzedaz/mieszkanie/%s?areaMax=38&page=%i", miasto, i)
    remDr$navigate(urll)
    Sys.sleep(1)
    webElement<- remDr$findElement("css","body")
    webElement$sendKeysToElement(list(key="end"))
    Sys.sleep(1)
    webElement$sendKeysToElement(list(key="end"))
    Sys.sleep(1)
    pageFromSeleniumL <- remDr$getPageSource()[[1]] %>% rvest::read_html()
    linki<- (pageFromSeleniumL%>%html_elements(".css-p74l73.es62z2j20") ) %>%
      html_node("a")%>%html_attr("href")
    wektorLinkow<-c(wektorLinkow,linki)
    
  }
  
  wektorLinkow<-unique(wektorLinkow)
  
  w<-1
  data<-"16.01.2023"
  zrobWiersz<- function(w,wektorLinkow,miasto,data,remDr){
    urll<- paste0("https://www.otodom.pl",wektorLinkow[w])
    remDr$navigate(urll)
    Sys.sleep(1)
    webElement<- remDr$findElement("css","body")
    webElement$sendKeysToElement(list(key="end"))
    Sys.sleep(1)
    webElement$sendKeysToElement(list(key="end"))
    Sys.sleep(1)
    pageFromSeleniumL <- remDr$getPageSource()[[1]] %>% rvest::read_html()
    cena<-pageFromSeleniumL%>%html_element(".css-8qi9av.eu6swcv19")%>%html_text()
    
    v<-pageFromSeleniumL%>%html_elements(".css-1qzszy5.estckra8")%>%html_text()
    indexy<- seq(1,length(v))
    nazwyKolumn <- v[indexy%%2==1]
    wartosci <-  v[indexy%%2==0]
    
    df1<-  data.frame( t(wartosci) )
    names(df1)<-nazwyKolumn
    
    if( !any(is.na(names(df1) )) ) {
      df1<- cbind(df1,miasto)
      df1<- cbind(df1,data=data)
      df1<-cbind(cena=cena,df1)
      
    }
    df1
  }
  
  #install.packages("gtools")
  library(gtools)
  
  
  liczbaLinkow<-length(wektorLinkow)
  for( l in 1:4 ){
    skip<-FALSE
    tryCatch(
      temp<-zrobWiersz(l,wektorLinkow,miasto,data,remDr=remDr),
      error=function(e){
        print(e)
        skip<<-TRUE
        
      } 
    )
    
    if(skip){next}
    print(names(temp))
    if ( !any(is.na(names(temp))) ){
      if( is.null(miastaDF) )  
        miastaDF<-temp
      else{
        miastaDF<-smartbind(miastaDF,temp )
      }
    }
  }
}


#install.packages(c("DBI","RMySQL","rstudioapi"))
library(DBI)
library(RMySQL)
library(rstudioapi)
View(miastaDF)

con <- DBI::dbConnect(RMySQL::MySQL(),
                      encoding ="UTF-8",
                      host = "xx.xxx.xx.x",
                      user = "test",
                      dbname = "rzajecia23",
                      password = "xxxx"
)

dbGetQuery(con,'SET NAMES utf8')
dbGetQuery(con,'set character set "utf8"')
dbWriteTable(con, "szaf_miasta", miastaDF, append = FALSE,overwrite=TRUE)

#install.packages("dplyr")
library(dplyr)

dbListTables(con)
szaf<- tbl(con,"szaf_miasta")
szaf%>%select(cena)

dbDisconnect(con)
