library(xlsx)
library(ggplot2)
dat<-read.xlsx(file ="C:\\Users\\Debaditya\\Documents\\Project_Work\\UPI_P2P_P2M.xlsx"
          ,sheetIndex = 1,as.data.frame = 1)
freq_val_p2p<-(dat[9:11]/dat$P2P_TOTAL_VOL)
freq_val_p2m<-(dat[13:15]/dat$P2M_TOTAL_VOL)
ggplot()+
  geom_line(mapping = aes(y=freq_val_p2p$P2P_LE_500_VOL,x=dat$Month,color="<500_P2P"),cex=1.2)+
  geom_line(mapping = aes(y=freq_val_p2p$P2P_501_2000_VOL,x=dat$Month,color="500-2000_P2P"),cex=1.2)+
  geom_line(mapping = aes(y=freq_val_p2p$P2P_GE_2000_VOL,x=dat$Month,color=">2000_P2P"),cex=1.2)+
  geom_line(mapping = aes(y=freq_val_p2m$P2M_LE_500_VOL,x=dat$Month,color="<500_P2M"),cex=1.2)+
  geom_line(mapping = aes(y=freq_val_p2m$P2M_501_2000_VOL,x=dat$Month,color="500-2000_P2M"),cex=1.2)+
  geom_line(mapping = aes(y=freq_val_p2m$P2M_GE_2000_VOL,x=dat$Month,color=">2000_P2M"),cex=1.2)+
  theme(
    legend.position = c(1,0.8),
      legend.justification = c("right", "top"),
      legend.margin = margin(6, 6, 6, 6),
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

