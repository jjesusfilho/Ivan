library(stringr)
library(stringi)
a<-readRDS("roubo1.rds")
b<-readRDS("roubo2.rds")
d<-readRDS("roubo3.rds")
df<-rbind(a,b,d[1:8])
rm(a,b,d)

df$orgao_julgador<-stri_trans_general(df$orgao_julgador,"Latin-ASCII")

df$orgao_julgador<-tolower(df$orgao_julgador)

df<-df[str_which(df$orgao_julgador,"criminal"),]

df$decisao<-stri_trans_general(df$decisao,"Latin-ASCII")

df$decisao<-tolower(df$decisao)
df$unanime<-ifelse(str_detect(df$decisao,"unani.*"),"sim","nao")

df$ano<-str_extract(df$data_julgamento,"\\d{4}")

df_turmas<-df[str_which(df$orgao_julgador,"\\d+"),]

unanimidade<-df_turmas %>% dplyr::count(ano,unanime,orgao_julgador)

unanimidade<-unanimidade %>% dplyr::group_by(ano) %>% 
  mutate(percentual=round(n*100/sum(n),digits=2))

df3<-df_turmas %>% filter(orgao_julgador=="3ª turma criminal")

una3<-df3 %>% dplyr::count(ano,unanime)

una3<-una3 %>% group_by(ano) %>% 
  mutate(percentual=round(n*100/sum(n),digits=3))



