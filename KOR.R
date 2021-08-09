
######################## Figure for McH1-7 manuscript###########################
#plot replicate ddPCR values
library(plyr)
library(ggplot2)
library("ggforce")
library(dplyr)

tank<-read.table("kor_tank_long.txt",sep="\t",header=TRUE)
tanksum<-ddply(tank,c("Sample","coral","date","type","group"), summarise, N=length(copies.per.ng),mean=mean(copies.per.ng),sd=sd(copies.per.ng),se=sd/sqrt(N))
tanksum$date <- factor(tanksum$date, levels = c("pre", "post", "1d","3d","7d","21d","28d"))
tanksum$type <- factor(tanksum$type, levels = c("water","tissue"))
water<-filter(tanksum, type=="water")
tissue<-filter(tanksum, type=="tissue")

pdf("KOR_ddPCR_tank_All.pdf",width=8.5)
p<-ggplot(tanksum, aes(x=date, y=mean, color=type, group=group, shape=coral)) +
  geom_line() +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.1)+
  geom_point(size=4) +
  theme_bw()+
  scale_color_manual(values=c("#0072B2","#E69F00"), name="Sample Type")+
  scale_shape_manual(values=c(1,0,5,2), name="Genotype")+
  theme(text=element_text(size=14,face="bold"))+
  ylab("Copies of korormicin gene per ng DNA")+
  theme(axis.title.x = element_blank())+
  facet_grid(type~.,scales="free")
p
dev.off()

pdf("KOR_ddPCR_tank_Water.pdf",width=11)
pW<-ggplot(water, aes(x=date, y=mean, color=type, group=group, shape=coral)) +
  geom_line() +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.1)+
  geom_point(size=4) +
  theme_bw()+
  scale_color_manual(values=c("#0072B2","#E69F00"), name="Sample Type")+
  scale_shape_manual(values=c(1,0,5,2), name="Genotype")+
  theme(text=element_text(size=14,face="bold"))+
  ylab("Copies of korormicin gene per ng DNA")+
  theme(axis.title.x = element_blank())+
  facet_zoom(ylim=c(0,2))
pW
dev.off()