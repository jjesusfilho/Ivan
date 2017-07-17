library(lubridate)
library(purrr)
library(dplyr)
library(magrittr)
library(forecast)
library(tseries)
library(cowplot)

df$data<-parse_date_time(df$data_julgamento,orders="dmy")

## Criando a variável mês
df$mes<-as.Date(cut(df$data,breaks = "month"))


una<-df %>% 
  filter(orgao_julgador=="3ª turma criminal") %>% ## Use este para trabalhar somente com a terceira turma
  #filter(ano>2005) %>% ## Use este para trabalhar com todas as turmas.
  group_by(mes) %>%
  count(unanime)

una<-una %>% 
  group_by(mes) %>% 
  mutate(freq=n*100/sum(n))

una<-spread(una,unanime,freq)
una<-na.omit(una[-3])

## plotando os dados

una %>%
  ggplot(aes(x = mes, y = sim)) +
  geom_line(alpha = 0.5, color = palette_light()[[1]]) +
  labs(title = "Distribuição percentual do voto unânime ao longo do tempo", y = "Percentual voto unânime",x="Ano") +
  theme_tq()


## Ententendo o que aconteceu em 2014


relator<-df %>% 
  filter(orgao_julgador=="3ª turma criminal")  %>% 
  count(relator,mes,unanime) %>% 
  group_by(mes) %>% 
  mutate(freq=n*100/sum(n)) %>% 
  group_by(mes,unanime) %>%
  mutate(soma=sum(freq))

relator<-relator[str_which(relator$mes,"2013"),]
relator<-relator %>% filter(unanime=="sim")
relator$relator<-ifelse(str_detect(relator$relator,"HUMBERTO"),"HUMBERTO ULHÔA",relator$relator)

## 

una2<-df %>% 
  filter(orgao_julgador=="3ª turma criminal") %>% ## Use este para trabalhar somente com a terceira turma
  




s<-spread(una,unanime,freq)
s<-na.omit(s[5])
s<-ts(s$sim,start=c(2012,1),frequency=12) ## Troque para 2006 para trabalhar com todas as turmas.

## Plota a série temporal

plot(s)

## Visutaliza a tendência por ano. Para tanto agrega os valores por ano.

layout(1:2)
plot(aggregate(s))
boxplot(s ~ cycle(s))




### Comparação dos meses com as respectivas médias.

ratio<-map_chr(1:12,function(x){
    window(s,start=c(2006,x),freq=T) %>% 
      mean()/mean(s) %>% 
    set_names(month(1:12,label=T))
    })

names(ratio)<-as.character(month(1:12,label=T))

## Comparação via boxplot
ggplot(filter(una,unanime=="sim"),aes(x=mes,y=freq,group=mes))+
  geom_boxplot(aes(fill=mes))+
  guides(fill="none")





### Decompondo a série temporal

plot(decompose(s,type="multiplicative"))

### Detectando a tendência

tendencia<-ma(s,order=12)


## Seasanal plot
seasonplot(s,ylab="percentual", xlab="Meses", 
           main="plot sazonal: unanimidade nas decisões",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

## Autocorrelação

acf(s)

acf(s)$acf
plot(s[1:65],s[2:66])


## Dickey-Fuller test

count_s = diff(s, differences = 1)
plot(count_s)
adf.test(count_s, alternative = "stationary")


## Transformação da série. Não vejo diferença alguma

lambda <- BoxCox.lambda(s)

plot(BoxCox(s,lambda))


