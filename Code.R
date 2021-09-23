library(tidytuesdayR)
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load(2021, week = 39)
tuesdata$nominees->nominees
glimpse(nominees)
library(ggrepel)

#top 5 shows by nominees
nominees%>%select("title","distributor","type","category","year")%>%
  distinct(title,distributor,category,.keep_all = TRUE)%>%
  group_by(distributor)%>%
  filter(type=="Nominee")%>%
  count()%>%
  arrange(-n)

#top 5 shows by winners
nominees%>%select("title","distributor","type","category","year")%>%
  distinct(title,distributor,category,.keep_all = TRUE)%>%
  group_by(distributor)%>%
  filter(type=="Winner")%>%
  count()%>%
  arrange(-n)

#netflix nomations and winners
nominees%>%filter(distributor=="Netflix")%>%filter(type=="Nominee")%>%
  select(distributor,category,title,type,year)%>%
  group_by(year,distributor)%>%
  count()->Nnom
Nnom
nominees%>%filter(distributor=="Netflix")%>%filter(type=="Winner")%>%
  select(distributor,category,title,type,year)%>%
  group_by(year,distributor)%>%
  count()->Nwin

cbind(Nnom,Nwin)->n
n
n[c(1,2,3,6)]->net
colnames(net)<-c("Year","Distributor","Lost","Won")
net%>%mutate(nominations=(Lost+Won))%>%mutate(ratio=(Won/nominations)*100)->net
net%>%select(Year,Distributor,nominations,Won)->net1
net1


#HBO nomations and winners
nominees%>%filter(distributor=="HBO")%>%
  select(distributor,category,title,type,year)%>%
  group_by(year,distributor)%>%
  summarise(Nominee=sum(type=="Nominee"), winner=sum(type=="Winner"))->hnet

hnet
colnames(hnet)<-c("Year","Distributor","Lost","Won")
hnet%>%mutate(nominations=(Lost+Won))%>%mutate(ratio=(Won/nominations)*100)->hnet
hnet%>%select(Year,Distributor,nominations,Won)->hnet1
hnet1


#NBC nomations and winners
nominees%>%filter(distributor=="NBC")%>%
  select(distributor,category,title,type,year)%>%
  group_by(year,distributor)%>%
  summarise(Nominee=sum(type=="Nominee"), winner=sum(type=="Winner"))->NBnet

NBnet
colnames(NBnet)<-c("Year","Distributor","Lost","Won")
NBnet%>%mutate(nominations=(Lost+Won))%>%mutate(ratio=(Won/nominations)*100)->NBnet
NBnet%>%select(Year,Distributor,nominations,Won)->NBnet1
NBnet1

#CBS nomations and winners
nominees%>%filter(distributor=="CBS")%>%
  select(distributor,category,title,type,year)%>%
  group_by(year,distributor)%>%
  summarise(Nominee=sum(type=="Nominee"), winner=sum(type=="Winner"))->CBnet

CBnet
colnames(CBnet)<-c("Year","Distributor","Lost","Won")
CBnet%>%mutate(nominations=(Lost+Won))%>%mutate(ratio=(Won/nominations)*100)->CBnet
CBnet%>%select(Year,Distributor,nominations,Won)->CBnet1
CBnet1


#ABC nomations and winners
nominees%>%filter(distributor=="ABC")%>%
  select(distributor,category,title,type,year)%>%
  group_by(year,distributor)%>%
  summarise(Nominee=sum(type=="Nominee"), winner=sum(type=="Winner"))->ABCnet

ABCnet
colnames(ABCnet)<-c("Year","Distributor","Lost","Won")
ABCnet%>%mutate(nominations=(Lost+Won))%>%mutate(ratio=(Won/nominations)*100)->ABCnet
ABCnet%>%select(Year,Distributor,nominations,Won)->ABCnet1
ABCnet1


rbind(net1,hnet1,NBnet1,CBnet1,ABCnet1)->plot1
rbind(net,hnet,NBnet,CBnet,ABCnet)->plot2
colnames(plot1)<-c("Year","Distributor","Nominations","Wins")

ggplot(plot2, aes(x=Year,y=ratio,colour=Distributor,label=paste0(Distributor,",",round(ratio,digits = 1),"%")))+
  geom_point(size=2.5,alpha=0.75)+geom_label_repel(max.overlaps = 2,show.legend = FALSE)+
  theme(plot.background=element_rect("black"),
        panel.background = element_rect("black"),
        axis.title = element_text(colour = "white", size=15, face = "bold"),
        axis.text=element_text(colour = "white"),
        panel.grid = element_blank())+
    labs(x = "Year", y = "Share of wins to nominations(%)")+
   theme(legend.position = "top",
        legend.box.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box = "horizontal",
        legend.text = element_text(colour="white",size = 10),
        legend.key = element_rect(fill = NA))+
  guides(color=guide_legend(override.aes=list(fill=NA, size=10, shape=16)))+
  labs(title="How Has The Probability Of Winning An Emmy Changed Over The Years?",
       subtitle="The probability is calculated in percentage as the number of wins to the number of nominations. 
       The list of the top 5 distributors is narrowed down to those that have the cumulative highest Emmy nominations",
       caption="Data: Emmys.com via Tidy Tuesday| Design: @annapurani93")+
  theme(plot.title = element_text(colour="white", size=20, face = "bold", hjust=0.5),
        plot.subtitle = element_text(colour="white", size=15, face = "bold", hjust=0.5),
        plot.caption = element_text(colour="White", size=12, face = "bold"))->p2

 

ggplot(plot1, aes(x=Nominations,y=Wins,colour=Distributor,label=paste0(Distributor,",",Year)))+
  geom_point(size=2.5,alpha=0.75)+geom_label_repel(max.overlaps = 1,show.legend = FALSE)+
  theme(plot.background=element_rect("black"),
        panel.background = element_rect("black"),
        axis.title = element_text(colour = "white", size=15, face = "bold"),
        axis.text=element_text(colour = "white"),
        panel.grid = element_blank())+
  labs(x = "Nominations", y = "Wins")+
  xlim(0,600)+
  theme(legend.position = "top",
        legend.box.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box = "horizontal",
        legend.text = element_text(colour="white",size = 12),
        legend.key = element_rect(fill = NA))+
  guides(color=guide_legend(override.aes=list(fill=NA, size=10, shape=16)))+
  labs(title="Nominations Vs Wins: Who Leads the Show Among Netflix, HBO, CBS, ABC and NBC?",
              caption="Data: Emmys.com via Tidy Tuesday| Design: @annapurani93")+
  theme(plot.title = element_text(colour="white", size=20, face = "bold", hjust=0.5),
        plot.subtitle = element_text(colour="white", size=15, face = "bold", hjust=0.5),
        plot.caption = element_text(colour="White", size=12, face = "bold"))->p1


ggsave(dpi=500, width = 12, height=6, units = "in",type="cairo","p1.png",p1)
ggsave(dpi=500, width = 12, height=6, units = "in",type="cairo","p2.png",p2)


