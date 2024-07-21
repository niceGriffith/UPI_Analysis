library(xlsx)
library(ggplot2)
dat<-read.xlsx(file ="C:\\Users\\Debaditya\\Documents\\Project_Work\\UPI_P2P_P2M.xlsx"
          ,sheetIndex = 1,as.data.frame = 1)
freq_val_p2p<-(dat[9:11]/dat$Vol_total)*100
p2p_tot<-((dat[9:11]))
p2p_tot<-p2p_tot[,1]+p2p_tot[,2]+p2p_tot[,3]

freq_val_p2m<-(dat[13:15]/dat$Vol_total)*100
p2m_tot<-dat[,16]
freq_tot<-data.frame(dat$Month,(p2m_tot/dat$Vol_total)*100,(p2p_tot/dat$Vol_total)*100)
colnames(freq_tot)<-c("dates","P2M","P2P")

###Adding Fonts
sysfonts::font_add("Economica","C:\\Users\\Debaditya\\AppData\\Local\\Microsoft\\Windows\\Fonts\\Economica-Bold.ttf")

###Area Chart
library(viridis)
pp<-freq_tot %>% 
  mutate(dates = as.Date(dates)) %>% 
  gather(variable, value, c("P2M","P2P")) %>% 
  ggplot(aes(x = dates, y = value, fill = variable)) +
  geom_area(alpha=0.6 , size=.5, colour="white")+ 
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis(discrete = T,option = "D") +
  theme_ipsum(plot_title_margin = 20,plot_title_face = "bold") + labs(x="",y="Percentage Share")+
  theme(
    legend.text = element_text(family = "Economica"),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid = element_line(linetype = "dashed",colour = "yellow"),
    axis.text= element_text(colour = "black",family = "Economica",size=10),
    axis.text.x = element_text(margin = margin(10,20,20,20)),
    axis.title.x = element_text(colour = "black",family = "Economica",hjust=0.5,size = 11),
    axis.title.y = element_text(colour = "black",family = "Economica",hjust=0.5,size = 15),
    plot.title = element_text(colour = "black",hjust=0.5,face ="bold",family="Economica",size=18),
    plot.subtitle = element_text(colour = "grey50",hjust=0.5,family = "Economica",face = "bold",size=12),
    plot.caption = element_text(family = "Economica",hjust=0.5),
    plot.margin = margin(15,15,15,15))+
  ggtitle("The Race between P2P AND P2M",subtitle = "2022-2024")
pp
ggsave("p2pvp2m.png",pp,dpi=400,width = 7,height = 5)

###

pp<-ggplot()+
  geom_line(mapping = aes(y=freq_val_p2p$P2P_LE_500_VOL,x=dat$Month,color="<500_P2P"),cex=1.2)+ geom_point(mapping = aes(y=freq_val_p2p$P2P_LE_500_VOL,x=dat$Month),cex=1.5,color="yellow")+
  geom_line(mapping = aes(y=freq_val_p2p$P2P_501_2000_VOL,x=dat$Month,color="500-2000_P2P"),cex=1.2)+
  geom_point(mapping = aes(y=freq_val_p2p$P2P_501_2000_VOL,x=dat$Month),cex=1.5,color="violet")+
  geom_line(mapping = aes(y=freq_val_p2p$P2P_GE_2000_VOL,x=dat$Month,color=">2000_P2P"),cex=1.2)+geom_point(mapping = aes(y=freq_val_p2p$P2P_GE_2000_VOL,x=dat$Month),cex=1.5,color="cyan")+
  geom_line(mapping = aes(y=freq_val_p2m$P2M_LE_500_VOL,x=dat$Month,color="<500_P2M"),cex=1.2)+
  geom_point(mapping = aes(y=freq_val_p2m$P2M_LE_500_VOL,x=dat$Month),cex=1.5,color="pink")+
  geom_line(mapping = aes(y=freq_val_p2m$P2M_501_2000_VOL,x=dat$Month,color="500-2000_P2M"),cex=1.2)+
  geom_point(mapping = aes(y=freq_val_p2m$P2M_501_2000_VOL,x=dat$Month),cex=1.5,color="lightblue")+
  geom_line(mapping = aes(y=freq_val_p2m$P2M_GE_2000_VOL,x=dat$Month,color=">2000_P2M"),cex=1.2)+geom_point(mapping = aes(y=freq_val_p2m$P2M_GE_2000_VOL,x=dat$Month),cex=1.5,color="green")+
  theme_ft_rc()+
  theme(
    legend.text = element_text(family = "Economica"),
    legend.title = element_text(family = "Economica"),
    panel.grid = element_line(linetype = "dashed",colour = "yellow"),
    axis.line = element_line(colour = "gray"),
    axis.text = element_text(colour = "gray",family = "Economica"),
    axis.title.x = element_text(colour = "gray",family = "Economica",hjust=0.5,size = 12),
    axis.title.y = element_text(colour = "gray",family = "Economica",hjust=0.5,size = 12),
    plot.title = element_text(colour = "cyan",hjust=0,face ="bold",family="Economica",size=15),
    plot.subtitle = element_text(colour = "cyan",hjust=0,family = "Economica",face = "bold",size=8),
    plot.caption = element_text(family = "Economica",hjust=0.5))+
  labs(color="Payment Category",title="Category Wise payment over time ",
       subtitle = "in percentage of total payment volume",x="",y="Percentage",
       caption = "Source: NPCI")
pp

ggsave("pay_cat_all.png",pp,dpi=400,width = 7,height = 5)


####P2M####
ggplot()+geom_line(mapping = aes(y=freq_val_p2m$P2M_LE_500_VOL,x=dat$Month,color="<500_P2M"),cex=1.2)+
  geom_line(mapping = aes(y=freq_val_p2m$P2M_501_2000_VOL,x=dat$Month,color="500-2000_P2M"),cex=1.2)+
  geom_line(mapping = aes(y=freq_val_p2m$P2M_GE_2000_VOL,x=dat$Month,color=">2000_P2M"),cex=1.2)+
  theme(
    plot.background = element_rect(fill="white",colour ="white"),
    panel.background = element_rect(fill="white",colour="white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "gray4"),
    axis.text = element_text(colour = "gray4"),
    axis.title = element_text(colour = "gray4"),
    plot.title = element_text(colour = "cyan4",hjust=.5,face = "bold.italic",family ="serif",size=15),
    plot.subtitle = element_text(colour = "cyan4",hjust=.5,face = "bold",size=8))+
  labs(color="Payment Category",title="Category Wise payment over time ",
       subtitle = "in fractions of total payment volume",x="Time",y="Frequency")

