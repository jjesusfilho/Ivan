library(stringr)
library(stringi)

h<-readRDS("homicidio.rds")
drogas<-readRDS("trafico.rds")

h$crime<-"homicidio"
drogas$crime<-"trafico"
roubo<-df[1:8]
roubo$crime<-"roubo"

df<-rbind(h,roubo,drogas)
df<-unique(df)

df$orgao_julgador<-stri_trans_general(df$orgao_julgador,"Latin-ASCII")

df$orgao_julgador<-tolower(df$orgao_julgador)

df<-df[str_which(df$orgao_julgador,"criminal"),]

df$decisao<-stri_trans_general(df$decisao,"Latin-ASCII")

df$decisao<-tolower(df$decisao)
df$unanime<-ifelse(str_detect(df$decisao,"unani.*"),"sim","nao")

df$ano<-str_extract(df$data_julgamento,"\\d{4}")

df_turmas<-df[str_which(df$orgao_julgador,"\\d+"),]

unanimidade<-df %>% dplyr::count(ano,unanime)

unanimidade<-unanimidade %>% dplyr::group_by(ano) %>% 
  mutate(percentual=round(n*100/sum(n),digits=2))

df3<-df_turmas %>% filter(orgao_julgador=="3ª turma criminal")

una3<-df3 %>% dplyr::count(ano,unanime)

una3<-una3 %>% group_by(ano) %>% 
  mutate(percentual=round(n*100/sum(n),digits=3))



