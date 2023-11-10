setwd("F:\\FoliarFungalDiseases")
library(readxl)
data <- read_excel("Plotdata.xlsx",sheet = 1)
str(data)
data$Site<-as.character(data$Site)
data$Plot<-as.character(data$Plot)
data$CWMLT<-as.numeric(data$CWMLT)
data$CWMHn<-as.numeric(data$CWMHn)
data$Soil_PCA1<-as.numeric(data$Soil_PCA1)

########## Figure 2a ############################################################
library(performance)
lm0<-lm(scale(PL)~scale(MAT)+
          scale(MAP)+
          scale(Soil_PCA1)+
          scale(SR)+
          scale(CWMHn)+
          scale(CWMLT),
        data = data)

check_collinearity(lm0)

library(brms)
nm0<-brm(scale(PL)~scale(MAT)+
           scale(MAP)+
           scale(Soil_PCA1)+
           scale(SR)+
           scale(CWMHn)+
           scale(CWMLT)+(1|Site),
         data = data)
summary(nm0)
saveRDS(nm0, "nm0.rds")
PL_null<-readRDS("nm0.rds")
summary(PL_null)

library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidybayes)
p2a<-PL_null%>%
  spread_draws(b_scaleMAT, 
               b_scaleMAP, 
               b_scaleSoil_PCA1,
               b_scaleSR,
               b_scaleCWMHn,
               b_scaleCWMLT) %>%
  rename("MAT"=b_scaleMAT, 
         "MAP" =b_scaleMAP,  
         "SR"=b_scaleSR, 
         "CWMheight"= b_scaleCWMHn,
         "CWMthickness"=b_scaleCWMLT,
         "Soil PCA1"=b_scaleSoil_PCA1 )%>%
  gather(key="variabe", value="estimates",
         "MAT",  "MAP",
         "SR", "CWMheight", 
         "CWMthickness",
         "Soil PCA1") %>%
  mutate(variabe = fct_relevel(variabe, 
                               "CWMthickness",
                               "CWMheight",
                               "SR",
                               "Soil PCA1",
                               "MAP",
                               "MAT" ))%>%
  ggplot(aes(y = variabe, x = estimates,
             fill = stat(x< 0 ),
  )) +
  scale_fill_manual(values = c( "skyblue","gray80"))+
  stat_halfeye()+
  ylab("") +
  xlab("Estimates") +
  theme_bw()+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"))+
  theme(strip.text.x = element_text(size =12),
        legend.position='none')+
  geom_vline(xintercept=0,linetype="dashed",color="red")
graph2ppt(x=p2a,file='PL_null', width = 4, height = 4) 


########## Figure 2b ############################################################
lm1<-lm(scale(SpeciesTurnoverEffects)~scale(MAT)+
          scale(MAP)+
          scale(Soil_PCA1)+
          scale(SR)+
          scale(CWMHn)+
          scale(CWMLT),
        data = data)

check_collinearity(lm1)

nm1<-brm(scale(SpeciesTurnoverEffects)~scale(MAT)+
           scale(MAP)+
           scale(Soil_PCA1)+
           scale(SR)+
           scale(CWMHn)+
           scale(CWMLT)+(1|Site),
         data = data)

saveRDS(nm1, "nm1.rds")
SpeciesTurnoverEffects_null<-readRDS("nm1.rds")
summary(SpeciesTurnoverEffects_null)

p2b<-SpeciesTurnoverEffects_null%>%
  spread_draws(b_scaleMAT, 
               b_scaleMAP, 
               b_scaleSoil_PCA1,
               b_scaleSR,
               b_scaleCWMHn,
               b_scaleCWMLT) %>%
  rename("MAT"=b_scaleMAT, 
         "MAP" =b_scaleMAP,
         "Soil PCA1"=b_scaleSoil_PCA1,
         "SR"=b_scaleSR, 
         "CWMheight"= b_scaleCWMHn,
         "CWMthickness"=b_scaleCWMLT )%>%
  gather(key="variabe", value="estimates",
         "MAT", "Soil PCA1",  "MAP",
         "SR", "CWMheight", 
         "CWMthickness") %>%
  mutate(variabe = fct_relevel(variabe, 
                               "CWMthickness",
                               "CWMheight",
                               "SR",
                               "Soil PCA1",
                               "MAP",
                               "MAT" ))%>%
  ggplot(aes(y = variabe, x = estimates,
             fill = stat(x< 0 ),
  )) +
  scale_fill_manual(values = c( "skyblue","gray80"))+
  stat_halfeye()+
  ylab("") +
  xlab("Estimates") +
  theme_bw()+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"))+
  theme(strip.text.x = element_text(size =12),
        legend.position='none')+
  geom_vline(xintercept=0,linetype="dashed",color="red")
graph2ppt(x=p2b,file='SpeciesTurnoverEffects_null', width = 4, height = 4) 



########## Figure 2c############################################################
lm2<-lm(scale(IntraspecificVariation)~scale(MAT)+
          scale(MAP)+
          scale(Soil_PCA1)+
          scale(SR)+
          scale(CWMHn)+
          scale(CWMLT),
        data = data)

check_collinearity(lm2)

nm2<-brm(scale(IntraspecificVariation)~scale(MAT)+
           scale(MAP)+
           scale(Soil_PCA1)+
           scale(SR)+
           scale(CWMHn)+
           scale(CWMLT)+(1|Site),
         data = data)

saveRDS(nm2, "nm2.rds")
IntraspecificVariation_null<-readRDS("nm2.rds")
summary(IntraspecificVariation_null)

p2c<-IntraspecificVariation_null%>%
  spread_draws(b_scaleMAT, 
               b_scaleMAP,
               b_scaleSoil_PCA1,
               b_scaleSR,
               b_scaleCWMHn,
               b_scaleCWMLT) %>%
  rename("MAT"=b_scaleMAT, 
         "MAP" =b_scaleMAP,
         "Soil PCA1"=b_scaleSoil_PCA1,
         "SR"=b_scaleSR, 
         "CWMheight"= b_scaleCWMHn,
         "CWMthickness"=b_scaleCWMLT)%>%
  gather(key="variabe", value="estimates",
         "MAT",  "MAP", 
         "Soil PCA1",
         "SR", "CWMheight", 
         "CWMthickness") %>%
  mutate(variabe = fct_relevel(variabe, 
                               "CWMthickness",
                               "CWMheight",
                               "SR",
                               "Soil PCA1",
                               "MAP",
                               "MAT" ))%>%
  ggplot(aes(y = variabe, x = estimates,
             fill = stat(x< 0 ),
  )) +
  scale_fill_manual(values = c( "skyblue","gray80"))+
  stat_halfeye()+
  ylab("") +
  xlab("Estimates") +
  theme_bw()+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"))+
  theme(strip.text.x = element_text(size =12),
        legend.position='none')+
  geom_vline(xintercept=0,linetype="dashed",color="red")
graph2ppt(x=p2c,file='IntraspecificVariation_null', width = 4, height = 4) 



library(glmm.hp)
library(lme4)
data<-na.omit(data)%>%mutate(MAT = scale(MAT),
                             MAP = scale(MAP),
                             SR = scale(SR),
                             CWMLT = scale(CWMLT), 
                             CWMHn = scale(CWMHn),
                             Soil_PCA1 = scale(Soil_PCA1))
mod1<- lmer(PL~MAT+MAP+SR+CWMHn+Soil_PCA1+CWMLT+(1|Site), data =data)
glmm.hp(mod1)


mod2<- lmer(SpeciesTurnoverEffects~ MAT+MAP+SR+CWMHn+Soil_PCA1+CWMLT+(1 | Site), data =data)

glmm.hp(mod2)


mod3<- lmer(IntraspecificVariation~ MAT+MAP+SR+CWMHn+Soil_PCA1+CWMLT+(1 | Site), data =data)
glmm.hp(mod3)


R2data <- read_excel("Plotdata.xlsx",sheet=2)
library(ggcharts)
windowsFonts(A=windowsFont("Times New Roman"),
             B=windowsFont("Arial"))
R2data<-R2data%>%mutate(Factors=factor(Factors,levels = rev(unique(Factors)),ordered = F))
p2d<-ggplot(R2data, aes(y=perc1, x=Factors)) + 
  geom_bar(position="dodge", stat="identity",color="skyblue",fill="skyblue") + 
  coord_flip() +
  ylab("Relative effect of estimates (%)") +
  xlab("Factors") +
  theme_bw()+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"))+
  theme(strip.text.x = element_text(size =12),
        legend.position='none')

p2e<-ggplot(R2data, aes(y=perc2, x=Factors)) + 
  geom_bar(position="dodge", stat="identity",color="skyblue",fill="skyblue") + 
  coord_flip() +
  ylab("Relative effect of estimates (%)") +
  xlab("Factors") +
  theme_bw()+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"))+
  theme(strip.text.x = element_text(size =12),
        legend.position='none')


p2f<-ggplot(R2data, aes(y=perc3, x=Factors)) + 
  geom_bar(position="dodge", stat="identity",color="skyblue",fill="skyblue") + 
  coord_flip() +
  ylab("Relative effect of estimates (%)") +
  xlab("Factors") +
  theme_bw()+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"))+
  theme(strip.text.x = element_text(size =12),
        legend.position='none')

########## SEM model ############################################################
m0<-bf(scale(SpeciesTurnoverEffects)~scale(MAT)+scale(MAP)+scale(SR)++scale(CWMHn)+scale(CWMLT)+scale(Soil_PCA1)+(1|Site))
m1<-bf(scale(IntraspecificVariation)~scale(MAT)+scale(MAP)+scale(SR)++scale(CWMHn)+scale(CWMLT)+scale(Soil_PCA1)+(1|Site))
m2<-bf(scale(SR)~scale(Soil_PCA1)+scale(MAT)+scale(MAP)+(1|Site))
m3<-bf(scale(CWMLT)~scale(Soil_PCA1)+scale(MAT)+scale(MAP)+(1|Site))
m4<-bf(scale(CWMHn)~scale(Soil_PCA1)+scale(MAT)+scale(MAP)+(1|Site))
m5<-bf(scale(Soil_PCA1)~scale(MAT)+scale(MAP)+(1|Site))

sem1<-brm(m0 + m1 + m2 + m3+m4+m5+ set_rescor(FALSE),
          control = list(adapt_delta = 0.99,max_treedepth = 12),
          iter =10000,
          data = data)

saveRDS(sem1, "tl_10m_sem_edge_controlled.rds")

sem1<-readRDS("tl_10m_sem_edge_controlled.rds")
summary(sem1)



########## S1 ############################################################
library(ggcorrplot)
library(corrplot)
Soildata <- read_excel("Plotdata.xlsx",sheet=1)%>%dplyr::select('Site','Plot','SWC','pH','STC',	'STN',	'STP')
str(Soildata)
Soildata$SWC<-as.numeric(Soildata$SWC)
Soildata$pH<-as.numeric(Soildata$pH)
Soildata$STC<-as.numeric(Soildata$STC)
Soildata$STN<-as.numeric(Soildata$STN)
Soildata$STP<-as.numeric(Soildata$STP)
Soildata$Site<-as.character(Soildata$Site)

Soildata<-Soildata%>%na.omit() %>%dplyr::rename("Soil water content"= SWC ,
                                                "pH"=pH, 
                                                "Soil total phosphorus" = STP,  
                                                "Soil total nitrogen"= STN,
                                                "Soil total carbon"= STC)
xm<-Soildata[,c(3,4,5,6,7)]
as_tibble(xm)
corr<-round(cor(xm),1)
corrplot(corr,method="ellipse",type = "full",addCoef.col="black")


########## S2 ############################################################
data <- read_excel("Plotdata.xlsx",sheet=1)%>%dplyr::select('pH',	'SWC','STC',	'STN',	'STP')
data$STC<-as.numeric(data$STC)
data$STN<-as.numeric(data$STN)
data$STP<-as.numeric(data$STP)
data$SWC<-as.numeric(data$SWC)
data$pH<-as.numeric(data$pH)
str(data)
library(vegan)

pca <- rda(data %>% dplyr::select('SWC'	,'pH',	'STC','STN','STP')%>%na.omit() %>% mutate(SWC = scale(SWC),
                                                                                         pH = scale(pH),
                                                                                         STC = scale(STC),
                                                                                         STP = scale(STP), 
                                                                                         STN = scale(STN)))%>% summary()
pca$sites %>% write.csv('PC1PC2.csv')
pca$cont$importance

pca.result <- as.data.frame(pca$sites)%>%dplyr::select(PC1,PC2)
pca.arrow <- as.data.frame(pca$species) %>%dplyr::select(PC1,PC2)
ps1<-ggplot(data = pca.result)+
  geom_point(aes(x = PC1,y = PC2),shape = 21,size = 4)+
  scale_fill_discrete()+
  stat_ellipse(aes(x = PC1,y = PC2),geom ="polygon",level = 0.95,size = 0.5,alpha = 0.2)+
  scale_color_grey()+
  geom_segment(data = pca.arrow,aes(x = 0,xend = PC1,y = 0,yend = PC2),arrow = arrow(length = unit(0.35,"cm")))



########## S3 ############################################################
data <- read_excel("Plotdata.xlsx",sheet=1)%>%dplyr::select(Site,STEresid,IVEresid,MAP,MAT,SR,CWMHn,CWMLT,Soil_PCA1)
str(data)
data$CWMLT<-as.numeric(data$CWMLT)
data$STEresid<-as.numeric(data$STEresid)
data$IVEresid<-as.numeric(data$IVEresid)
data$Soil_PCA1<-as.numeric(data$Soil_PCA1)
data<-data%>%na.omit()
yresid1 <- resid(lm(STEresid ~ scale(MAP)+scale(SR)+scale(CWMHn)+scale(CWMLT)+scale(Soil_PCA1), data))%>%write.csv("yresid1.csv")
xresid1 <- resid(lm(scale(MAT) ~ scale(MAP)+scale(SR)+scale(CWMHn)+scale(CWMLT)+scale(Soil_PCA1), data))%>%write.csv("xresid1.csv")
yresid2 <- resid(lm(STEresid ~ scale(MAT)+scale(SR)+scale(CWMHn)+scale(CWMLT)+scale(Soil_PCA1), data))%>%write.csv("yresid2.csv")
xresid2 <- resid(lm(scale(MAP) ~ scale(MAT)+scale(SR)+scale(CWMHn)+scale(CWMLT)+scale(Soil_PCA1), data))%>%write.csv("xresid2.csv")

yresid3 <- resid(lm(STEresid ~ scale(MAT)+scale(MAP)+scale(CWMHn)+scale(CWMLT)+scale(Soil_PCA1), data))%>%write.csv("yresid3.csv")
xresid3 <- resid(lm(scale(SR) ~ scale(MAT)+scale(MAP)+scale(CWMHn)+scale(CWMLT)+scale(Soil_PCA1), data))%>%write.csv("xresid3.csv")


yresid4 <- resid(lm(STEresid ~ scale(MAT)+scale(MAP)+scale(SR)+scale(CWMLT)+scale(Soil_PCA1), data))%>%write.csv("yresid4.csv")
xresid4 <- resid(lm(scale(CWMHn) ~ scale(MAT)+scale(MAP)+scale(SR)+scale(CWMLT)+scale(Soil_PCA1), data))%>%write.csv("xresid4.csv")

yresid5 <- resid(lm(STEresid ~ scale(MAT)+scale(MAP)+scale(SR)+scale(CWMHn)+scale(Soil_PCA1), data))%>%write.csv("yresid5.csv")

xresid5 <- resid(lm(scale(CWMLT) ~ scale(MAT)+scale(MAP)+scale(SR)+scale(CWMHn)+scale(Soil_PCA1), data))%>%write.csv("xresid5.csv")

yresid6 <- resid(lm(STEresid ~ scale(MAT)+scale(MAP)+scale(SR)+scale(CWMHn)+scale(CWMLT), data))%>%write.csv("yresid6.csv")
xresid6 <- resid(lm(scale(Soil_PCA1) ~ scale(MAT)+scale(MAP)+scale(SR)+scale(CWMHn)+scale(CWMLT), data))%>%write.csv("xresid6.csv")

yresid7 <- resid(lm(IVEresid ~ scale(MAP)+scale(SR)+scale(CWMHn)+scale(CWMLT)+scale(Soil_PCA1), data))%>%write.csv("yresid7.csv")

yresid8 <- resid(lm(IVEresid ~ scale(MAT)+scale(SR)+scale(CWMHn)+scale(CWMLT)+scale(Soil_PCA1), data))%>%write.csv("yresid8.csv")

yresid9 <- resid(lm(IVEresid ~ scale(MAT)+scale(MAP)+scale(CWMHn)+scale(CWMLT)+scale(Soil_PCA1), data))%>%write.csv("yresid9.csv")

yresid10 <- resid(lm(IVEresid ~ scale(MAT)+scale(MAP)+scale(SR)+scale(CWMLT)+scale(Soil_PCA1), data))%>%write.csv("yresid10.csv")

yresid11 <- resid(lm(IVEresid ~ scale(MAT)+scale(MAP)+scale(SR)+scale(CWMHn)+scale(Soil_PCA1), data))%>%write.csv("yresid11.csv")

yresid12 <- resid(lm(IVEresid ~ scale(MAT)+scale(MAP)+scale(SR)+scale(CWMHn)+scale(CWMLT), data))%>%write.csv("yresid12.csv")

data <- read_excel("Plotdata.xlsx",sheet=3)

ps3a<-ggplot(data=data,aes(x = xresid1,y = yresid1)) +
  geom_point(alpha = 0.6,size = 2,shape=21,color="#007abb")+
  geom_smooth(aes(x = xresid1,y = yresid1),data,method = "lm",se = T, color= "#d1a363",size = 1.5, linetype = 1,alpha = 0.6)+
  theme_bw()+ 
  theme(axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, colour = "black",hjust = 0.5),
        axis.text.y = element_text(size = 14, colour = "black"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),
        axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 1))+
  theme(panel.grid = element_blank())+
  theme(plot.margin = unit(x = c(0.2,0.4,0.2,0),units = "cm"))+
  xlab(label = "MAT residuals")+
  ylab(label = "Species turnover effects residuals")+
  #ylim(0, 25)+
  #xlim(0, 1350)+
  theme(plot.title = element_text(size = 16,hjust = 16,angle = 0))+ 
  #theme_bw() +
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank(),
        legend.position = "none")
ps3a
graph2ppt(x=ps3a,file='ps3a', width = 4, height = 4) 



ps3b<-ggplot(data=data,aes(x = xresid2,y = yresid2)) +
  geom_point(alpha = 0.6,size = 2,shape=21,color="#007abb")+
  geom_smooth(aes(x = xresid2,y = yresid2),data,method = "lm",se = T, color= "#d1a363",size = 1.5, linetype = 1,alpha = 0.6)+
  theme_bw()+ 
  theme(axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, colour = "black",hjust = 0.5),
        axis.text.y = element_text(size = 14, colour = "black"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),
        axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 1))+
  theme(panel.grid = element_blank())+
  theme(plot.margin = unit(x = c(0.2,0.4,0.2,0),units = "cm"))+
  xlab(label = "MAP residuals")+
  #ylab(label = "Species turnover effects residuals")+
  #ylim(0, 25)+
  #xlim(0, 1350)+
  theme(plot.title = element_text(size = 16,hjust = 16,angle = 0))+ 
  #theme_bw() +
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank(),
        legend.position = "none")
ps3b
graph2ppt(x=ps3b,file='ps3b', width = 4, height = 4) 


ps3d<-ggplot(data=data,aes(x = xresid3,y = yresid3)) +
  geom_point(alpha = 0.6,size = 2,shape=21,color="#007abb")+
  #geom_smooth(aes(x = xresid3,y = yresid3),data,method = "lm",se = T, color= "#d1a363",size = 1.5, linetype = 1,alpha = 0.6)+
  theme_bw()+ 
  theme(axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, colour = "black",hjust = 0.5),
        axis.text.y = element_text(size = 14, colour = "black"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),
        axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 1))+
  theme(panel.grid = element_blank())+
  theme(plot.margin = unit(x = c(0.2,0.4,0.2,0),units = "cm"))+
  xlab(label = "SR residuals")+
  ylab(label = "Species turnover effects residuals")+
  #ylim(0, 25)+
  #xlim(0, 1350)+
  theme(plot.title = element_text(size = 16,hjust = 16,angle = 0))+ 
  #theme_bw() +
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank(),
        legend.position = "none")
ps3d
graph2ppt(x=ps3d,file='ps3d', width = 4, height = 4) 


ps3e<-ggplot(data=data,aes(x = xresid4,y = yresid4)) +
  geom_point(alpha = 0.6,size = 2,shape=21,color="#007abb")+
  geom_smooth(aes(x = xresid4,y = yresid4),data,method = "lm",se = T, color= "#d1a363",size = 1.5, linetype = 1,alpha = 0.6)+
  theme_bw()+ 
  theme(axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, colour = "black",hjust = 0.5),
        axis.text.y = element_text(size = 14, colour = "black"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),
        axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 1))+
  theme(panel.grid = element_blank())+
  theme(plot.margin = unit(x = c(0.2,0.4,0.2,0),units = "cm"))+
  xlab(label = "CWMheight residuals")+
  ylab(label = "Species turnover effects residuals")+
  #ylim(0, 25)+
  #xlim(0, 1350)+
  theme(plot.title = element_text(size = 16,hjust = 16,angle = 0))+ 
  #theme_bw() +
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank(),
        legend.position = "none")
ps3e
graph2ppt(x=ps3e,file='ps3e', width = 4, height = 4)



ps3f<-ggplot(data=data,aes(x = xresid5,y = yresid5)) +
  geom_point(alpha = 0.6,size = 2,shape=21,color="#007abb")+
  #geom_smooth(aes(x = xresid5,y = yresid5),data,method = "lm",se = T, color= "#d1a363",size = 1.5, linetype = 1,alpha = 0.6)+
  theme_bw()+ 
  theme(axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, colour = "black",hjust = 0.5),
        axis.text.y = element_text(size = 14, colour = "black"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),
        axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 1))+
  theme(panel.grid = element_blank())+
  theme(plot.margin = unit(x = c(0.2,0.4,0.2,0),units = "cm"))+
  xlab(label = "CWMthickness residuals")+
  ylab(label = "Species turnover effects residuals")+
  #ylim(0, 25)+
  xlim(-1, 2.5)+
  theme(plot.title = element_text(size = 16,hjust = 0.5,angle = 0))+ 
  #theme_bw() +
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank(),
        legend.position = "none")
ps3f
graph2ppt(x=ps3f,file='ps3f', width = 4, height = 4)


ps3d<-ggplot(data=data,aes(x = xresid6,y = yresid6)) +
  geom_point(alpha = 0.6,size = 2,shape=21,color="#007abb")+
  #geom_smooth(aes(x = xresid6,y = yresid6),data,method = "lm",se = T, color= "#d1a363",size = 1.5, linetype = 1,alpha = 0.6)+
  theme_bw()+ 
  theme(axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, colour = "black",hjust = 0.5),
        axis.text.y = element_text(size = 14, colour = "black"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),
        axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 1))+
  theme(panel.grid = element_blank())+
  theme(plot.margin = unit(x = c(0.2,0.4,0.2,0),units = "cm"))+
  xlab(label = "Soil_PCA1 residuals")+
  ylab(label = "Species turnover effects residuals")+
  #ylim(0, 25)+
  xlim(-2, 2)+
  theme(plot.title = element_text(size = 16,hjust = 0.5,angle = 0))+ 
  #theme_bw() +
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank(),
        legend.position = "none")
ps3d
graph2ppt(x=ps3d,file='ps3d', width = 4, height = 4)


ps3g<-ggplot(data=data,aes(x = xresid1,y = yresid7)) +
  geom_point(alpha = 0.6,size = 2,shape=21,color="#007abb")+
  #geom_smooth(aes(x = xresid1,y = yresid7),data,method = "lm",se = T, color= "#d1a363",size = 1.5, linetype = 1,alpha = 0.6)+
  theme_bw()+ 
  theme(axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, colour = "black",hjust = 0.5),
        axis.text.y = element_text(size = 14, colour = "black"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),
        axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 1))+
  theme(panel.grid = element_blank())+
  theme(plot.margin = unit(x = c(0.2,0.4,0.2,0),units = "cm"))+
  xlab(label = "MAT residuals")+
  ylab(label = "Intraspecific variation effects residuals")+
  #ylim(0, 25)+
  #xlim(0, 1350)+
  theme(plot.title = element_text(size = 16,hjust = 0.5,angle = 0))+ 
  #theme_bw() +
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank(),
        legend.position = "none")
ps3g
graph2ppt(x=ps3g,file='ps3g', width = 4, height = 4)


ps3h<-ggplot(data=data,aes(x = xresid2,y = yresid8)) +
  geom_point(alpha = 0.6,size = 2,shape=21,color="#007abb")+
  geom_smooth(aes(x = xresid2,y = yresid8),data,method = "lm",se = T, color= "#d1a363",size = 1.5, linetype = 1,alpha = 0.6)+
  theme_bw()+ 
  theme(axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, colour = "black",hjust = 0.5),
        axis.text.y = element_text(size = 14, colour = "black"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),
        axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 1))+
  theme(panel.grid = element_blank())+
  theme(plot.margin = unit(x = c(0.2,0.4,0.2,0),units = "cm"))+
  xlab(label = "MAP residuals")+
  ylab(label = "Intraspecific variation effects residuals")+
  #ylim(0, 25)+
  #xlim(0, 1350)+
  theme(plot.title = element_text(size = 16,hjust = 0.5,angle = 0))+ 
  #theme_bw() +
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank(),
        legend.position = "none")
ps3h
graph2ppt(x=ps3h,file='ps3h', width = 4, height = 4)



ps3j<-ggplot(data=data,aes(x = xresid3,y = yresid9)) +
  geom_point(alpha = 0.6,size = 2,shape=21,color="#007abb")+
  #geom_smooth(aes(x = xresid3,y = yresid3),data,method = "lm",se = T, color= "#d1a363",size = 1.5, linetype = 1,alpha = 0.6)+
  theme_bw()+ 
  theme(axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, colour = "black",hjust = 0.5),
        axis.text.y = element_text(size = 14, colour = "black"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),
        axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 1))+
  theme(panel.grid = element_blank())+
  theme(plot.margin = unit(x = c(0.2,0.4,0.2,0),units = "cm"))+
  xlab(label = "SR residuals")+
  ylab(label = "Intraspecific variation effects residuals")+
  #ylim(0, 25)+
  #xlim(0, 1350)+
  theme(plot.title = element_text(size = 16,hjust = 0.5,angle = 0))+ 
  #theme_bw() +
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank(),
        legend.position = "none")
ps3j
graph2ppt(x=ps3j,file='ps3j', width = 4, height = 4)


ps3k<-ggplot(data=data,aes(x = xresid4,y = yresid10)) +
  geom_point(alpha = 0.6,size = 2,shape=21,color="#007abb")+
  geom_smooth(aes(x = xresid4,y = yresid10),data,method = "lm",se = T, color= "#d1a363",size = 1.5, linetype = 1,alpha = 0.6)+
  theme_bw()+ 
  theme(axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.text.x = element_text(size =14, colour = "black",hjust = 0.5),
        axis.text.y = element_text(size = 14, colour = "black"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),
        axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 1))+
  theme(panel.grid = element_blank())+
  theme(plot.margin = unit(x = c(0.2,0.4,0.2,0),units = "cm"))+
  xlab(label = "CWMheight residuals")+
  ylab(label = "Intraspecific variation effects residuals")+
  #ylim(0, 25)+
  #xlim(0, 1350)+
  theme(plot.title = element_text(size = 16,hjust = 0.5,angle = 0))+ 
  #theme_bw() +
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank(),
        legend.position = "none")
ps3k
graph2ppt(x=ps3k,file='ps3k', width = 4, height = 4)


ps3l<-ggplot(data=data,aes(x = xresid5,y = yresid11)) +
  geom_point(alpha = 0.6,size = 2,shape=21,color="#007abb")+
  #geom_smooth(aes(x = xresid5,y = yresid11),data,method = "lm",se = T, color= "#d1a363",size = 1.5, linetype = 1,alpha = 0.6)+
  theme_bw()+ 
  theme(axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, colour = "black",hjust = 0.5),
        axis.text.y = element_text(size = 14, colour = "black"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),
        axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 1))+
  theme(panel.grid = element_blank())+
  theme(plot.margin = unit(x = c(0.2,0.4,0.2,0),units = "cm"))+
  xlab(label = "CWMthickness residuals")+
  ylab(label = "Intraspecific variation effects residuals")+
  #ylim(0, 25)+
  xlim(-1, 2.5)+
  theme(plot.title = element_text(size = 16,hjust = 0.5,angle = 0))+ 
  #theme_bw() +
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank(),
        legend.position = "none")
ps3l
graph2ppt(x=ps3l,file='ps3l', width = 4, height = 4)


ps3i<-ggplot(data=data,aes(x = xresid6,y = yresid12)) +
  geom_point(alpha = 0.6,size = 2,shape=21,color="#007abb")+
  #geom_smooth(aes(x = xresid6,y = yresid12),data,method = "lm",se = T, color= "#d1a363",size = 1.5, linetype = 1,alpha = 0.6)+
  theme_bw()+ 
  theme(axis.title.x = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, colour = "black",hjust = 0.5),
        axis.text.y = element_text(size = 14, colour = "black"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),
        axis.ticks.length.x = unit(0.2,'cm'), 
        axis.ticks.length.y = unit(0.2,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),    ## 设置刻度标签的粗细
        axis.ticks.y = element_line(colour = "black",size = 1))+
  theme(panel.grid = element_blank())+
  theme(plot.margin = unit(x = c(0.2,0.4,0.2,0),units = "cm"))+
  xlab(label = "Soil_PCA1 residuals")+
  ylab(label = "Intraspecific variation effects residuals")+
  #ylim(0, 25)+
  xlim(-2, 2)+
  theme(plot.title = element_text(size = 16,hjust = 0.5,angle = 0))+ 
  #theme_bw() +
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank(),
        legend.position = "none")
ps3i
graph2ppt(x=ps3i,file='ps3i', width = 4, height = 4)
