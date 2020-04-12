############################################################################
#                                                                          #
#  Script do post: A Covid-19 em números               Data: 12/04/2020    #
#  Autor: Thiago Valentim                                                  #
#                                                                          #
############################################################################

corona <- read.csv("COVID19_20200412.csv",header=TRUE,sep=";") 

names(corona) <- c("regiao","sigla","date","casosNovos","casosAcumulados",
                   "obitosNovos","obitosAcumulados")

library(tidyverse)
library(lubridate)
library(brazilmaps)
library(gridExtra)
library(ggspatial)
library(ggrepel)

corona$date <- as.Date(corona$date,format="%d/%m/%y")
brasil <- aggregate(. ~ date, corona, sum)
brasil <- brasil[-c(1:27),]
brasil$date <- substr(brasil$date,6,10)

taxa_m <- round(100*(max(brasil$obitosAcumulados)/max(brasil$casosAcumulados)),1)
taxa_m<-paste(taxa_m,"%")       


brasil %>%
  group_by(date)%>%
  ggplot(.,aes(x=date,y=casosAcumulados))+geom_point()+
  geom_line(aes(y = casosAcumulados, group="",colour = "Casos Acumulados"))+
  geom_point(aes(y=obitosAcumulados, group="",colour = "Óbitos Acumulados"))+
  geom_line(aes(y=obitosAcumulados, group="",colour = "Óbitos Acumulados"))+
  geom_point(aes(y=casosAcumulados, group="",colour = "Casos Acumulados"))+
  scale_color_manual(values=c("blue", "red"))+
  ylab("Quantidade registrada")+xlab("Data")+ 
  labs(title="Covid-19 no Brasil: casos acumulados",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim")+
  theme(legend.position="bottom",legend.title=element_blank(), 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1.0, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  annotate("text",x=12,y=max(brasil$casosAcumulados-500),hjust=0,vjust=0,
           label=paste("Letalidade =",bquote(.(taxa_m))),colour="red",size=4.5)+
  annotate("segment", x = 20, xend = 21, y = 1500, yend = 100,
           colour = "black", size=1.0, alpha=0.6, 
           arrow = arrow(length = unit(2, "mm")))+
  annotate(geom = "text", x = 16, y = 1900, label = "Primeiro óbito", hjust = "left")

brasil %>%
  group_by(date)%>%
  ggplot(.,aes(x=date,y=casosNovos))+geom_point()+
  geom_line(aes(y = casosNovos, group="",colour = "Casos Acumulados"))+
  geom_point(aes(y=obitosNovos, group="",colour = "Óbitos Acumulados"))+
  geom_line(aes(y=obitosNovos, group="",colour = "Óbitos Acumulados"))+
  geom_point(aes(y=casosNovos, group="",colour = "Casos Acumulados"))+
  scale_color_manual(values=c("blue", "red"))+
  ylab("Quantidade registrada")+xlab("Data")+ 
  labs(title="Covid-19 no Brasil: casos diários",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim")+
  theme(legend.position="bottom",legend.title=element_blank(),
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1.0, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  annotate("text",x=12,y=max(brasil$casosNovos-100),hjust=0,vjust=0,
           label=paste("Letalidade =",bquote(.(taxa_m))),colour="red",size=4.5)+
  annotate("segment", x = 20, xend = 21, y = 230, yend = 20,
           colour = "black", size=1.0, alpha=0.6, 
           arrow = arrow(length = unit(2, "mm")))+
  annotate(geom = "text", x = 17, y = 300, label = "Primeiro óbito", hjust = "left")

  
##################################################################################

n <- length(corona[corona$sigla=="RN",]$sigla)
nordeste <- rep(c("RN","PB","PE","SE","MA","CE","BA","PI","AL"),each=n)
pop_ne <- rep(c(3409000,3944000,9278000,2220000,6851000,8843000,15130000,3195000,
                3322000),each=n)
dia<-rep(substr(corona[corona$sigla =="RN",]$date,6,10),9)
info <- data.frame(nordeste,pop_ne,dia)

hoje <- "04-12"  #deve ser inserida a data de hoje em mês-dia
hoje2<- as.Date("12/04/2020",format="%d/%m/%y")

corona <- as_tibble(corona)
corona <- corona %>%   
  mutate(label = if_else(date == hoje2,
                   as.character(sigla), NA_character_))%>% 
  mutate(date = ymd(date)) %>%  
  mutate(date = substr(date,6,10))



############## Estados do Nordeste (Absolutos)

corona %>%
  filter(sigla %in% c("RN","PB","PE","SE","MA","CE","BA","PI","AL"))%>%
  left_join(info, c("date" = "dia","sigla"="nordeste")) %>%
  filter(date %in% substr(seq(as.Date("2020/03/04"),
                              as.Date("2020/04/12"),by = "day"),6,10))%>%
  group_by(date)%>%
  ggplot(., aes(x = date, y = casosAcumulados, group = sigla, colour = sigla)) +
  geom_line(cex=1.1)+
  labs(x = "Data", y = "Casos confirmados", colour = "Estado",
       title="Covid-19: acumulados dos casos por estado",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.5,segment.colour = "transparent")


##### Estados do nordeste (proporcional)

corona %>%
  filter(sigla %in% c("RN","PB","PE","SE","MA","CE","BA","PI","AL"))%>%
  left_join(info, c("date" = "dia","sigla"="nordeste")) %>%
  mutate(prop = 100000*(casosAcumulados/pop_ne))%>%
  filter(date %in% substr(seq(as.Date("2020/03/04"),
                              as.Date("2020/04/12"),by = "day"),6,10))%>%
  group_by(date)%>%
  ggplot(., aes(x = date, y = prop, group = sigla, colour = sigla)) +
  geom_line(cex=1.1)+
  labs(x = "Data", y = "Casos confirmados a cada 100k hab.", colour = "Estado",
       title="Covid-19: acumulados dos casos por estado (proporcional)",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  #scale_colour_viridis_d()+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.5,segment.colour = "transparent")

##########################################################################

##### Códigos para o mapa (Script do Professor Marcus Nunes)

codigos <- structure(
  list(codigo = c(11L, 12L, 13L, 14L, 15L, 16L, 17L, 
                  21L, 22L, 23L, 24L, 25L, 26L, 27L, 
                  28L, 29L, 31L, 32L, 33L, 35L, 
                  41L, 42L, 43L, 50L, 51L, 52L, 53L), 
       estado = structure(c(22L, 1L, 4L, 23L, 14L, 3L, 
                            27L, 10L, 18L, 6L, 20L, 15L, 17L, 2L, 26L, 
                            5L, 13L, 8L, 19L, 25L, 16L, 24L, 21L, 12L, 
                            11L, 9L, 7L), 
                          .Label = c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", 
                                     "Ceará", "Distrito Federal", "Espírito Santo", 
                                     "Goiás", "Maranhão", "Mato Grosso", 
                                     "Mato Grosso do Sul", "Minas Gerais", "Pará", 
                                     "Paraíba", "Paraná", "Pernambuco", 
                                     "Piauí", "Rio de Janeiro", "Rio Grande do Norte", 
                                     "Rio Grande do Sul", "Rondônia", "Roraima", 
                                     "Santa Catarina", "São Paulo", "Sergipe", 
                                     "Tocantins"), class = "factor"), 
       uf = structure(c(21L, 1L, 3L, 22L, 14L, 4L, 27L, 10L, 
                        17L, 6L, 20L, 15L, 16L, 2L, 25L, 5L, 
                        11L, 8L, 19L, 26L, 18L, 24L, 23L, 12L, 13L, 9L, 7L), 
                      .Label = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", 
                                 "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", 
                                 "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", 
                                 "SC", "SE", "SP", "TO"), class = "factor")), 
  class = "data.frame", row.names = c(NA, -27L))

pop <- c(1777225,881935,4144597,605761,8602865,845731,1572866,
         7075181,3273227,9132078,3506853,4018127,9557071,3337357,2298696,14873064,
         21168791,4018650,17264943,45919049,
         11433957,7164788,11377239,
         2778986,3484466,7018354,3015268)

teste <- corona %>%
  filter(date == hoje) %>%
  select(sigla, casosAcumulados) %>%
  print(n = Inf)%>%
  mutate(codigo = codigos[,1],prop = (100000*casosAcumulados/pop))

mapa_br <- get_brmap(geo = "State")

########### Mapa do Brasil

names(mapa_br)[2] <- "codigo"
mapa_br <- merge(mapa_br,teste,by.y="codigo")
names(mapa_br)
View(mapa_br)
a<-ggplot(mapa_br) +
  geom_sf(aes(fill = casosAcumulados)) +
  labs(fill = "Número de casos confirmados",
       title="Covid-19: casos confirmados",
       subtitle ="12/04/2020", 
       caption="Fonte: Ministério da Saúde    Autor: Thiago Valentim") +
  scale_fill_gradient(low="white", high="red3")+
  theme(panel.grid = element_line(colour = "grey95"),
        legend.position = "bottom",
        legend.text = element_text(size=6))+
  annotation_scale(location = "bl", width_hint = 0.50) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.08, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = c(-75, -35), ylim = c(-35, 5))

b<-ggplot(mapa_br) +
  geom_sf(aes(fill = prop)) +
  labs(fill = "Número de casos para 100k hab.",
       title="Incidência da Covid-19",
       subtitle ="12/04/2020", 
       caption="Fonte: Ministério da Saúde    Autor: Thiago Valentim") +
  scale_fill_gradient(low="white", high="red3")+
  theme(panel.grid = element_line(colour = "grey95"),
        legend.position = "bottom",
        legend.text = element_text(size=6))+
  annotation_scale(location = "bl", width_hint = 0.50) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.08, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = c(-75, -35), ylim = c(-35, 5))

grid.arrange(a,b,nrow=1)

################################################################################

##### Letalidade do Nordeste

corona %>%
  filter(sigla %in% c("RN","PB","PE","SE","MA","CE","BA","PI","AL"))%>%
  left_join(info, c("date" = "dia","sigla"="nordeste")) %>%
  filter(date == substr(as.Date("2020/04/12"),6,10))%>%
  mutate(let = 100*(obitosAcumulados/casosAcumulados))%>%
  ggplot(., aes(x=reorder(sigla,desc(let)), y=let,fill=sigla)) + geom_col()+
  ylab("Letalidade (%)")+xlab("Estados")+ 
  labs(title="Latalidade da Covid-19 na Região Nordeste",
       caption="Fonte: Ministério da Saúde    Autor: Thiago Valentim",
       fill="Estados")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

#################################################################################
