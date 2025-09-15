#R script to make main figures 2-6 and all SI figures
# 1. Loading up libraries -------------------------------------------------
library(tidyverse)
library(patchwork)
library(data.table)
library(ggh4x)
library(viridis)
library(forcats)
library(RColorBrewer)
library(wesanderson)
library(doParallel)
# 2. Default plotting themes/labels ---------------------------------------
#setting default theme for plots
mytheme <-
  theme_classic() + theme(
    legend.position = "bottom",
    #this puts legend on the bottom
    axis.title = (element_text(color = "black")),
    #Makes the axis line black and  thicker
    text = element_text(size = 20),
    axis.text = element_text(color = "black")
  )
theme_set(mytheme)

#Mkaing the different paletes
pal <- rev(mako(9)[c(1, 3,4,5,6, 7, 8)])
##Labels for facets
#vector of labels for strength of selection
A.labs<-c("Weak preference \n (v = 50)","Moderate preference \n  (v = 12.5)","Strong preference \n  (v = 1)")
#name the vector to match actual values in data frame
names(A.labs)<-c("50.0","12.5","1.0")
#vector of labels for variation
M.labs <- c("1% migration (m)","2% migration (m)","3% migration (m)","4% migration (m)","5% migration (m)")
#name the vector to match actual values in data frame
names(M.labs) <- c("1%","2%","3%","4%","5%")
#vector of labels for tradeoff
T.labs <- c("No tradeoff", "Tradeoff")
#name the vector to match actual values in data frame
names(T.labs) <- c("false", "true")
#vector of labels for category
C.labs <- c("Scenario 3: \n Eco. Div.; Pref. Div.", "Scenario 1:\n No Eco. Div.; Pref. Div.","Scenario 2: \n Eco Div.; No Pref. Div.")
names(C.labs) <- c("EE","NoE","NS")
#vector of labels for Population
P.labs<-c("N = 2000", "N = 10000")
names(P.labs)<-c("Small","Large")

#vector of labels for Selection
sel.labs<-c("Sexual", "Viability")
names(sel.labs)<-c("Sexual selection","Viability selection")

#Divergence labels for divergence
D.labs<-c("Mean Div. = 2SD\nWithin Pop. SD = 2.5", "Mean Div. = 4SD\nWithin Pop. SD=2.5",
          "Mean Div. = 16SD\nWithin Pop. SD = 2.5","Mean Div. = 4SD\nWithin Pop. SD = 10","Mean Div. = 2SD\nWithin Pop. SD = 10")
names(D.labs)<-c("2","4","16","4HV","2HV")
# 3. Loading up data ------------------------------------------------------
# 3.a Summary files for Preference and ecological divergence ------------
#Loading up summary files of Ecological divergence and Preference divergence (EE) for large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0)
setwd("~/Desktop/MyPapers/CFC_speciation/Revision/Scripts/Dryad/Data")
EE<-readRDS("Data_n/EE_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of Ecological divergence and Preference divergence (EE) for small population size (2,000) and risks of sperm competition (0.25,0.5,0.75,1.0)
EE_S<-readRDS("Data_n/EE_small_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of Ecological divergence and Preference divergence (EE) for large population size (10,000) and intensities of sperm competition (2,3,4,5,6,7,8,9,10)
EE_I<-readRDS("Data_n/EE_I_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of Ecological divergence and Preference divergence (EE) for small population size (2,000) and intensities of sperm competition (2,3,4,5,6,7,8,9,10)
EE_S_I<-readRDS("Data_n/EE_I_small_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

# 3.b Summary files for only preference divergence, no ecological divergence ------------
#Loading up summary files of only preference divergence divergence (NoE) for large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0)

NoE<-readRDS("Data_n/NoE_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only preference divergence divergence (NoE) for small population size (2,000) and risks of sperm competition (0.25,0.5,0.75,1.0)
NoE_S<-readRDS("Data_n/NoE_Small_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only preference divergence divergence (NoE) for large population size (10,000) and intensities of sperm competition (2-10)

NoE_I<-readRDS("Data_n/NoE_I_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only preference divergence divergence (NoE) for small population size (2,000) and intensities of sperm competition (2-10)

NoE_S_I<-readRDS("Data_n/NoE_I_Small_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

# 3.c Summary files for no preference divergence and only ecological divergence ------------

#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0)

NS<-readRDS("Data_n/NS_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for small population size (2,000) and risks of sperm competition (0.25,0.5,0.75,1.0)

NS_S<-readRDS("Data_n/NS_small_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for large population size (10,000) and intensities of sperm competition (2-10)

NS_I<-readRDS("Data_n/NS_I_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for small population size (2,000) and intensities of sperm competition (2-10)

NS_S_I<-readRDS("Data_n/NS_I_small_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

# 3.d Merge all data and select final generation ------------

#First merginig all data after filtering out for final generation.
#then reordering the different factors for plotting.
NsumFinal<-rbind(
  EE%>%filter(Generation==2999),
  EE_I%>%filter(Generation==2999),
  EE_S%>%filter(Generation==2999),
  EE_S_I%>%filter(Generation==2999),
  NS%>%filter(Generation==2999),
  NS_I%>%filter(Generation==2999),
  NS_S%>%filter(Generation==2999),
  NS_S_I%>%filter(Generation==2999),
  NoE%>%filter(Generation==2999),
  NoE_I%>%filter(Generation==2999),
  NoE_S%>%filter(Generation==2999),
  NoE_S_I%>%filter(Generation==2999)
)%>%
  mutate(A=recode(A,"50" = "50.0"))%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0","3","4","5","6","7","8","9","10")))%>%
  mutate(PopSize=factor(PopSize,levels=c("Small","Large")))%>%
  mutate(Cat=factor(Cat,levels=c("NoE","NS","EE")))
# 4. Figure 2. Divergence neutral loci heatmap ----------------------------------------------------------
#please note that arrow annotations for risk and intensity were added in illustrator.
ggplot(NsumFinal%>%filter(Tradeoff=="false",PopSize=="Large"), aes(x = Level, y = M, fill = DivBPA_mean)) +
  geom_tile(size = 0.05,color="grey") +
  facet_nested(
    Cat~A,
    labeller = labeller(
      A=A.labs,
      Cat = C.labs
    )
  ) +
  scale_fill_distiller(
    limits=c(NA,1),
    trans = "log10",
    palette = "Blues",
    na.value = "grey",
    direction = 1,
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black",frame.linewidth=0.5
    ))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0),labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")) +
  labs(y = "Migration rate (m)", x = "Risk (qr) or intensity (qi) of sperm competition", fill =
         "Divergence in \n neutral loci")+
  theme(legend.position = "left")+
  geom_vline(aes(xintercept=4),size=1)+
  theme(panel.border = element_rect(color="black",fill=NA))

#ggsave("Figure_2.pdf",height = 1.3*183,width=(2*183),units="mm")

# 5. Figure 3. Divergence Female Preference--------------------------------------------------------------------
#please note that arrow annotations for risk and intensity were added in illustrator.
ggplot(NsumFinal%>%filter(Tradeoff=="false",PopSize=="Large"), aes(x = Level, y = M, fill = DivFA_mean)) +
  geom_tile(size = 0.05,color="grey") +
  facet_nested(
    Cat~A,
    labeller = labeller(
      A=A.labs,
      Cat = C.labs
    )
  ) +
  scale_fill_distiller(
    palette = "BuPu",
    na.value = "grey",
    direction = 1,
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black",frame.linewidth=0.5
    ))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0),labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")) +
  labs(y = "Migration rate (m)", x = "Risk (qr) or intensity (qi) of sperm competition", fill =
         "Divergence in \n cryptic preferences (f)")+
  theme(legend.position = "left")+
  geom_vline(aes(xintercept=4),size=1)+
  theme(panel.border = element_rect(color="black",fill=NA))
#ggsave("Figure_3.pdf",height = 1.3*183,width=(2*183),units="mm")

# 6. Figure 4 NoE --------------------------------------------------------
# * 6. a.Figure 4a Neutral loci -----------------------------------------------------------
#Loading up summary files of only preference divergence divergence (NoE) for large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0)

NoE_sel<-readRDS("Data-sel/NoE_Sum.rds")%>%
  filter(Generation==2999)%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))
#Loading up summary files of only preference divergence divergence (NoE) for small population size (2,000) and risks of sperm competition (0.25,0.5,0.75,1.0)
NoE_S_sel<-readRDS("Data-sel/NoE_S_Sum.rds")%>%
  filter(Generation==2999)%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only preference divergence divergence (NoE) for large population size (10,000) and intensities of sperm competition (2-10)

NoE_I_sel<-readRDS("Data-sel/NoE_I_Sum.rds")%>%
  filter(Generation==2999)%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only preference divergence divergence (NoE) for small population size (2,000) and intensities of sperm competition (2-10)

NoE_S_I_sel<-readRDS("Data-sel/NoE_I_S_Sum.rds")%>%
  filter(Generation==2999)%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

NoEnd<-rbind(NoE_S_sel,NoE_S_I_sel,NoE_I_sel,NoE_sel)%>%
  mutate(A=recode(A,"50" = "50.0"))%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0","3","4","5","6","7","8","9","10")))%>%
  mutate(PopSize=factor(PopSize,levels=c("Small","Large")))%>%
  mutate(Cat=factor(Cat,levels=c("NoE","NS","EE")))

(NoEND<-NoEnd%>%
   filter(Cat=="NoE",M=="1%",Tradeoff=="false")%>%
   ggplot(aes(x=Level,y=DivBPA_mean,fill=PopSize,color=PopSize,linetype=PopSize))+
   geom_point(size=2)+
   geom_line(aes(group=PopSize))+
   geom_errorbar(
     aes(
       ymin = DivBPA_mean -DivBPA_sd/sqrt(50)*2,
       ymax =DivBPA_mean+ DivBPA_sd/sqrt(50)*2)
   )+
   #geom_hline(aes(yintercept=1),linetype="dotted",alpha=0.4)+
   geom_vline(aes(xintercept=4),alpha=0.4,size=1)+
   facet_nested(~A,labeller = labeller(A = A.labs))+
   ylab("Divergence in \n neutral loci")+
   xlab("")+
   scale_y_continuous(limits=c(0,1),breaks=c(0,0.25,0.5,0.75,1))+
   theme(panel.border = element_rect(color="black",fill=NA),plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = c(0.055,0.6),legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
   scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
   scale_fill_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
   labs(linetype="Population \n Size")+
   scale_x_discrete(labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))
# * 6. b.Figure 4b Female divergence end ---------------------------------------------------
LNoE<-read_csv("Data-sel/NoE_last.csv")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))
LNoE_small<-read_csv("Data-sel/NoE_S_last.csv")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))
LNoE_I<-read_csv("Data-sel/NoE_I_last.csv")%>% 
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))
LNoE_small_I<-read_csv("Data-sel/NoE_I_S_last.csv")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

LNoE_all<-rbind(LNoE,LNoE_small,LNoE_I,LNoE_small_I)%>%
  mutate(A=factor(A,levels=c("50","12.5","1")))%>%
  mutate(A=recode(A,"50" = "50.0","1"="1.0"))%>%
  mutate(Level=factor(Level))%>%
  mutate(Level=recode(Level,"1"="1.0/2"))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))%>%
  mutate(PopSize=factor(PopSize,levels=c("Small","Large")))

(NoED<-NsumFinal%>%
    filter(Cat=="NoE",M=="1%",Tradeoff=="false")%>%
    ggplot(aes(x=Level,y=DivFA_mean,fill=PopSize,color=PopSize,linetype=PopSize))+
    geom_hline(aes(yintercept=40),linetype="dotted",alpha=0.4)+
    geom_vline(aes(xintercept=4),alpha=0.4,size=1)+
    geom_point(size=2)+
    geom_line(aes(group=PopSize))+
    geom_errorbar(
      aes(
        ymin = DivFA_mean -DivFA_sd/sqrt(50)*2,
        ymax =DivFA_mean+ DivFA_sd/sqrt(50)*2)
    )+
    facet_nested(~A,labeller = labeller(A = A.labs))+
    ylab("Divergence in \n preferences (f)")+
    xlab("")+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(), 
          axis.ticks.x=element_blank())+
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank())+
    #theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = c(0.055,0.6),legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    scale_fill_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    labs(linetype="Population \n Size")+
    scale_x_discrete(labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))

# * 6. c.Figure 4c Male and  Female migrant success ------------------------------------------
#Male migrant sucess
(NoEms<-
   LNoE_all%>%
   mutate(Nmig=ifelse(PopSize=="Small",10,50),
          NNonmig=ifelse(PopSize=="Small",990,4950))%>%
   mutate(FRelMig_a=(F_SurvOff_Mig_a/Nmig)/(F_SurvOff_NMig_a/NNonmig))%>%
   mutate(MRelMig_a=(M_SurvOff_Mig_a/Nmig)/(M_SurvOff_NMig_a/NNonmig))%>%
   select(Cat,A,An,M,Level,Tradeoff,Generation,FRelMig_a,MRelMig_a,PopSize)%>%
   group_by(Cat,A,An,M,Level,Tradeoff,PopSize)%>%
   summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
   filter(Cat=="NoE",M=="1%",Tradeoff=="FALSE")%>%
   ggplot(aes(x=Level,y=MRelMig_a_mean,fill=PopSize,color=PopSize,linetype=PopSize))+
   geom_point(size=2)+
   geom_hline(aes(yintercept=1.05),linetype="dashed",color="rosybrown1")+
   geom_line(aes(group=PopSize))+
   geom_errorbar(
     aes(
       ymin = MRelMig_a_mean -MRelMig_a_sd/sqrt(50)*2,
       ymax =MRelMig_a_mean+ MRelMig_a_sd/sqrt(50)*2)
   )+
   geom_vline(aes(xintercept=4),alpha=0.4,size=1)+
   facet_nested(~A,labeller = labeller(A = A.labs))+
   ylab("Relative male\nmigrant fitness")+
   xlab("")+
   theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
         strip.text.x = element_blank())+
   theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(), 
         axis.ticks.x=element_blank())+
   scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
   scale_fill_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
   labs(linetype="Population \n Size")+
   scale_x_discrete(labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))
# * 6. e.Figure 4D Beta selection cryptic preferences ----------------------------------------------------------------

(NoEB<-LNoE_all%>%
   mutate(AF_SonOff_Beta=abs(F_SonOff_Beta_b-F_SonOff_Beta_a))%>%
   mutate(AF_SurvOff_Beta=abs(F_SurvOff_Beta_b-F_SurvOff_Beta_a))%>%
   select(Cat,A,An,M,Level,Tradeoff,Generation,AF_SonOff_Beta,AF_SurvOff_Beta,PopSize)%>%
   group_by(Cat,A,An,M,Level,Tradeoff,PopSize)%>%
   summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
   filter(Cat=="NoE",M=="1%",Tradeoff=="FALSE")%>%
   ggplot(aes(x=Level,y=AF_SonOff_Beta_mean,fill=PopSize,color=PopSize,linetype=PopSize))+
   #geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed")+
   geom_point(size=2)+
   geom_line(aes(group=PopSize))+
   geom_errorbar(
     aes(
       ymin = AF_SonOff_Beta_mean -AF_SonOff_Beta_sd/sqrt(50)*2,
       ymax =AF_SonOff_Beta_mean+ AF_SonOff_Beta_sd/sqrt(50)*2)
   )+
   facet_nested(~A,labeller = labeller(A = A.labs))+
   geom_vline(aes(xintercept=4),alpha=0.4,size=1)+
   ylab("Divergent selection (b)\nacting on f")+
   xlab("Risk (qr) or intensity (qi) of sperm competition")+
   theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
         strip.text.x = element_blank())+
   theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
   scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
   scale_fill_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
   labs(linetype="Population \n Size")+
   scale_x_discrete(labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))
# * 6. f. putting it all together -------------------------------------------
(NoEND/NoED/NoEms/NoEB)+plot_annotation(tag_levels = "A",tag_suffix = ")")
#ggsave("Figure4.pdf",height = 1.7*183,width=(1.5*183),units="mm")
# 7. Figure 5. Female Divergence NS overtime -----------------------------------------------------------
#Runs with selection
NS_sel<-readRDS("Data-sel/NS_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))
#Runs with selection
NS_I_sel<-readRDS("Data-sel/NS_I_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Putting together data frame and reordering/naming factors
NS_Sum<-rbind(NS_sel,NS_I_sel)%>%
  mutate(A=recode(A,"50" = "50.0"))%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=recode(Level,"1.0"="1.0/2"))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))%>%
  mutate(PopSize=factor(PopSize,levels=c("Small","Large")))

#Making plot
NS_Sum%>%
  filter(PopSize=="Large",Tradeoff=="false",Level%in%c("0.25","1.0/2","3","4","5","7","9"),M%in%c("5%","4%","3%","2%","1%"))%>%
  mutate(M=factor(M,levels=c("5%","4%","3%","2%","1%")))%>%
  ggplot(aes(x=Generation,y=DivFA_mean,color=Level,fill=Level))+
  geom_hline(aes(yintercept=0),alpha=0.4,size=1,linetype="dashed")+
  geom_line(size=1)+
  geom_ribbon(
    aes(
      ymin = DivFA_mean - DivFA_sd/sqrt(50)*2,
      ymax = DivFA_mean + DivFA_sd/sqrt(50)*2,
      fill=Level),
    alpha = 0.3,
    color = NA
  )+
  facet_nested(M~A,labeller = labeller(A = A.labs,M=M.labs))+
  ylab("Divergence in cryptic preference (f)")+
  xlab("Generation")+
  scale_color_manual(name = "Risk/intensity of \n sperm competition", values = pal)+
  scale_fill_manual(name = "Risk/intensity of \n sperm competition", values = pal)+
  theme(panel.border = element_rect(color="black",fill=NA),legend.position = c(0.1,0.85),legend.background = element_rect(fill=NA),legend.text = element_text(size=16),legend.title = element_text(size=16),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"))+guides(fill=guide_legend(ncol=2),color=guide_legend(ncol=2))
#ggsave("Figure5.pdf",height = 1.6*183,width=(1.5*183),units="mm")

# 8. Figure 6. Selection overtime -----------------------------------------------------------
#Reading in data for when there is initially no divergent cryptic preferences, large population size and risks of sperm competition
RNS2<-readRDS("Data-sel/NS_runs.rds")%>%
  filter(Tradeoff=="false")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Reading in data for when there is initially no divergent cryptic preferences, large population size and intensities of sperm competition
RNS_I2<-readRDS("Data-sel/NS_I_runs.rds")%>% 
  filter(Tradeoff=="false")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Putting them together
RNS_all2<-rbind(RNS2,RNS_I2)%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level))%>%
  mutate(Level=recode(Level,"1.0"="1.0/2"))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))

#Pivoting data to show different sources of selection
#Sexual selection
RNS2O<-RNS_all2%>%
  mutate(AF_SonOff_Beta=abs(F_SonOff_Beta_b-F_SonOff_Beta_a))%>%
  select(Cat,A,An,M,Level,Tradeoff,Generation,AF_SonOff_Beta,PopSize)%>%
  group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
  summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
  #filter(M%in%c("1%","4%"),Level%in%c("0.25","0.75","1.0/2","3","5","7","9"))%>%
  mutate(Type="Sexual selection")
#Natural Selection
RNS2S<-RNS_all2%>%
  #mutate(AF_SonOff_Beta=abs(F_SonOff_Beta_b-F_SonOff_Beta_a))%>%
  mutate(AF_SurvOff_Beta=abs(F_SurvOff_Beta_b-F_SurvOff_Beta_a))%>%
  select(Cat,A,An,M,Level,Tradeoff,Generation,AF_SurvOff_Beta,PopSize)%>%
  group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
  summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
  # filter(M%in%c("1%","4%"),Level%in%c("0.25","0.75","1.0/2","3","5","7","9"))%>%
  mutate(Type="Viability selection")

#putting it all together
RNS_all2OS<-rbind(RNS2S,RNS2O)

# 6A Selection strength
(F6a<-RNS_all2OS%>%
    filter(M%in%c("1%","2%","3%","4%","5%"),A%in%c("1.0"),Level%in%c("0.25","1.0/2","3","4","5","7","9"))%>%
    filter(Generation<1000)%>%
    ggplot(aes(x=Generation,y=mean,color=Level,fill=Level))+
    geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed",size=1)+
    geom_line(size=1)+
    geom_ribbon(
      aes(
        ymin = mean - sd/sqrt(50)*2,
        ymax = mean + sd/sqrt(50)*2,
        fill=Level),
      alpha = 0.3,
      color = NA
    )+
    facet_nested(.~M+Type,labeller = labeller(A = A.labs,M=M.labs,Type=sel.labs))+
    ylab("Divergent selection (b)\nacting on f")+
    xlab("Generation")+
    scale_color_manual(name = "Risk/intensity of sperm competition", values = pal)+
    scale_fill_manual(name = "Risk/intensity of sperm competition", values = pal)+
    theme(panel.border = element_rect(color="black",fill=NA),legend.position = "top",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+guides(fill=guide_legend(nrow=1),color=guide_legend(nrow=1)))

# 6B Correlation between preference and ecological trait
(F6bF<-RNS_all2%>%
    filter(M%in%c("1%","2%","3%","4%","5%"),A=="1.0",Level%in%c("0.25","1.0/2","3","4","5","7","9"))%>%
    filter(Generation<1000)%>%
    mutate(cor.fe_a=abs(cor.fe_a))%>%
    select(Cat,A,An,M,Level,Tradeoff,Generation,cor.fe_a,PopSize)%>%
    group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
    summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
    filter(Generation<1000)%>%
    ggplot(aes(x=Generation,y=mean,color=Level,fill=Level))+
    geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed",size=1)+
    geom_line(size=1)+
    geom_ribbon(
      aes(
        ymin = mean - sd/sqrt(50)*2,
        ymax = mean + sd/sqrt(50)*2,
        fill=Level),
      alpha = 0.3,
      color = NA
    )+
    facet_nested(.~M,labeller = labeller(A = A.labs,M=M.labs,Type=sel.labs))+
    ylab("Genetic correlation\nbetween f and e")+
    xlab("Generation")+
    scale_color_manual(name = "Risk/intensity of \n sperm competition", values = pal)+
    scale_fill_manual(name = "Risk/intensity of \n sperm competition", values = pal)+
    theme(panel.border = element_rect(color="black",fill=NA),legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1))+guides(fill=guide_legend(ncol=2),color=guide_legend(ncol=2)))
# 5C. Correlation between male and female trait
(F6c<-RNS_all2%>%
    filter(M%in%c("1%","2%","3%","4%","5%"),A=="1.0",Level%in%c("0.25","1.0/2","3","4","5","7","9"))%>%
    #mutate(cor.mf_a=abs(cor.mf_a))%>%
    select(Cat,A,An,M,Level,Tradeoff,Generation,cor.mf_a,PopSize)%>%
    group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
    summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
    filter(Generation<1000)%>%
    ggplot(aes(x=Generation,y=mean,color=Level,fill=Level))+
    geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed",size=1)+
    geom_line(size=1)+
    geom_ribbon(
      aes(
        ymin = mean - sd/sqrt(50)*2,
        ymax = mean + sd/sqrt(50)*2,
        fill=Level),
      alpha = 0.3,
      color = NA
    )+
    facet_nested(.~M,labeller = labeller(Cat=C.labs,A = A.labs,M=M.labs))+
    ylab("Genetic correlation\nbetween f and t")+
    xlab("Generation")+
    scale_color_manual(name = "Risk/intensity of \n sperm competition", values = pal)+
    scale_fill_manual(name = "Risk/intensity of \n sperm competition", values = pal)+
    theme(panel.border = element_rect(color="black",fill=NA),legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1)))


((F6a/F6c/F6bF)&theme(text=element_text(size=18)))+plot_annotation(tag_levels = "A",tag_suffix = ")")


# 9. Figure 7: sensitivity analyses -------------------------------------------------------------
# 9.a Loading up results  ------------------------------------------------------
# 9.a.1 Summary files for Preference and ecological divergence ------------
#Loading up summary files of Ecological divergence and Preference divergence (EE) for large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0) and 4SD
EE_4<-readRDS("Processed_Data_4/EE_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="4")%>%
  mutate(Cat="EE")
#Loading up summary files of Ecological divergence and Preference divergence (EE) for large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0), high within population variation, initial 4SD divergence
EE_4_HV<-readRDS("Processed_Data_4H/EE_L_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="4HV")%>%
  mutate(Cat="EE")
#Loading up summary files of Ecological divergence and Preference divergence (EE) for large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0) and 2SD
EE_2<-readRDS("Processed_Data_2/EE_2_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="2")%>%
  mutate(Cat="EE")
#Loading up summary files of Ecological divergence and Preference divergence (EE) for large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0), high within population variation, initial 2SD divergence
EE_2_HV<-readRDS("Processed_Data_2H/EE_2_H_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="2HV")%>%
  mutate(Cat="EE")
#Loading up summary files of Ecological divergence and Preference divergence (EE) for large population size (10,000) and intensities of sperm competition (2,3,4,5,6,7,8,9,10) and 4SD
EE_4_I<-readRDS("Processed_Data_4/EE_I_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="4")%>%
  mutate(Cat="EE")
#Loading up summary files of Ecological divergence and Preference divergence (EE) for large population size (10,000) and intensities of sperm competition (2,3,4,5,6,7,8,9,10). High within population variation and 4SD initial divergence

EE_4_I_HV<-readRDS("Processed_Data_4H/EE_I_L_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="4HV")%>%
  mutate(Cat="EE")

#Loading up summary files of Ecological divergence and Preference divergence (EE) for small population size (2,000) and intensities of sperm competition (2,3,4,5,6,7,8,9,10) and 2SD
EE_2_I<-readRDS("Processed_Data_2/EE_I_2_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="2")%>%
  mutate(Cat="EE")

#Loading up summary files of Ecological divergence and Preference divergence (EE) for large population size (10,000) and intensities of sperm competition (2,3,4,5,6,7,8,9,10). High within population variation and 2SD initial divergence
EE_2_I_HV<-readRDS("Processed_Data_2H/EE_I_2_H_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="2HV")%>%
  mutate(Cat="EE")
# 9.a.2 Summary files for only preference divergence, no ecological divergence ------------
#Loading up summary files of only preference divergence divergence (NoE) for small population size (2,000) and risks of sperm competition (0.25,0.5,0.75,1.0) and 4SD
NoE_4<-readRDS("Processed_Data_4/NoE_S_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="4")
#Loading up summary files of only preference divergence divergence (NoE) for small population size (2,000) and risks of sperm competition (0.25,0.5,0.75,1.0). High within population variation and 4SD initial divergence
NoE_4_HV<-readRDS("Processed_Data_4H/NoE_small_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="4HV")%>%
  mutate(Cat="NoE")
#Loading up summary files of only preference divergence divergence (NoE) for small population sizes (2,000) and risks of sperm competition (0.25,0.5,0.75,1.0) and 2SD
NoE_2<-readRDS("Processed_Data_2/NoE_S_2_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="2")

#Loading up summary files of only preference divergence divergence (NoE) for small population size (2,000) and risks of sperm competition (0.25,0.5,0.75,1.0). High within population variation and 2SD initial divergence
NoE_2_HV<-readRDS("Processed_Data_2H/NoE_S_2_H_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="2HV")%>%
  mutate(Cat="NoE")
#Loading up summary files of only preference divergence divergence (NoE) for small population size (2,000) and intensities of sperm competition (2-10)
NoE_4_I<-readRDS("Processed_Data_4/NoE_I_S_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="4")
#Loading up summary files of only preference divergence divergence (NoE) for small population size (2,000) and intensities of sperm competition (2-10). High within pop variation, initial divergence SD=4
NoE_4_I_HV<-readRDS("Processed_Data_4H/NoE_I_small_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="4HV")%>%
  mutate(Cat="NoE")
#Loading up summary files of only preference divergence divergence (NoE) for small population size (2,000) and intensities of sperm competition (2-10). 2SD initial divergence
NoE_2_I<-readRDS("Processed_Data_2/NoE_I_S_2_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="2")

#Loading up summary files of only preference divergence divergence (NoE) for small population size (2,000) and intensities of sperm competition (2-10). High within population variation; 2SD initial divergence

NoE_2_I_HV<-readRDS("Processed_Data_2H/NoE_I_S_2_H_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="2HV")%>%
  mutate(Cat="NoE")

# 9.a.3 Summary files for no preference divergence and only ecological divergence ------------
#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0). Initial divergence of 4SD
NS_4<-readRDS("Processed_Data_4/NS_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="4")

#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0).Initial divergence of 4SD and high within population variation

NS_4_HV<-readRDS("Processed_Data_4H/NS_L_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="4HV")%>%
  mutate(Cat="NS")

#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for Large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0). Initial divergence of 2SD.
NS_2<-readRDS("Processed_Data_2/NS_2_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="2")

#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for Large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0). Initial divergence of 2SD, high within population variation
NS_2_HV<-readRDS("Processed_Data_2H/NS_2_H_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="2HV")%>%
  mutate(Cat="NS")
#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for large population size (10,000) and intensities of sperm competition (2-10). Initial divergence 4SD.

NS_4_I<-readRDS("Processed_Data_4/NS_I_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="4")

#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for large population size (10,000) and intensities of sperm competition (2-10). Initial divergence 4SD, high within populatio variation.
NS_4_I_HV<-readRDS("Processed_Data_4H/NS_I_L_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="4HV")%>%
  mutate(Cat="NS")

#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for large population size (10,000) and intensities of sperm competition (2-10). Initial divergence 2SD. 
NS_2_I<-readRDS("Processed_Data_2/NS_I_2_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="2")

#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for large population size (10,000) and intensities of sperm competition (2-10). Initial divergence 2SD, high within population variation

NS_2_I_HV<-readRDS("Processed_Data_2H/NS_I_2_H_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="2HV")%>%
  mutate(Cat="NS")
# 9.b Merge all data and select final generation ------------
#First merginig all data after filtering out for final generation.
#then reordering the different factors for plotting.
NsumFinalDiv42<-rbind(
  EE_4%>%filter(Generation==2999),
  EE_4_I%>%filter(Generation==2999),
  EE_2%>%filter(Generation==2999),
  EE_2_I%>%filter(Generation==2999),
  NS_4%>%filter(Generation==2999),
  NS_4_I%>%filter(Generation==2999),
  NS_2%>%filter(Generation==2999),
  NS_2_I%>%filter(Generation==2999),
  NoE_4%>%filter(Generation==2999),
  NoE_4_I%>%filter(Generation==2999),
  NoE_2%>%filter(Generation==2999),
  NoE_2_I%>%filter(Generation==2999),
  EE_4_HV%>%filter(Generation==9999),
  EE_4_I_HV%>%filter(Generation==9999),
  EE_2_HV%>%filter(Generation==2999),
  EE_2_I_HV%>%filter(Generation==2999),
  NS_4_HV%>%filter(Generation==9999),
  NS_4_I_HV%>%filter(Generation==9999),
  NS_2_HV%>%filter(Generation==2999),
  NS_2_I_HV%>%filter(Generation==2999),
  NoE_4_HV%>%filter(Generation==2999),
  NoE_4_I_HV%>%filter(Generation==2999),
  NoE_2_HV%>%filter(Generation==2999),
  NoE_2_I_HV%>%filter(Generation==2999)
)%>%
  mutate(A=recode(A,"50" = "50.0"))%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0","3","4","5","6","7","8","9","10")))%>%
  mutate(PopSize=factor(PopSize,levels=c("Small","Large")))%>%
  mutate(Cat=factor(Cat,levels=c("NoE","NS","EE")))
# 9.c Plotting Figure 7 ---------------------------------------------------
#New color palette
palz <- viridis(9)[c(1,7,5)]
(Fig7<-NsumFinalDiv42%>%
   mutate(Div=factor(Div,levels=c("2","2HV","4","4HV")))%>%
   filter(M%in%c("1%","2%","3%"))%>%
   filter(A=="1.0")%>%
   mutate(M=factor(M,levels=c("5%","4%","3%","2%","1%")))%>%
   #filter(Cat!="NoE")%>%
   ggplot(aes(x = Level, y = DivBPA_mean,color= Cat,group=Cat)) +   geom_vline(aes(xintercept=4),alpha=0.4,size=1)+
   geom_point()+
   geom_errorbar(aes(ymin=DivBPA_mean-DivBPA_sd/sqrt(50)*2,ymax=DivBPA_mean+DivBPA_sd/sqrt(50)*2))+
   geom_line()+
   facet_nested(
     M~Div,
     #scales="free",
     labeller = labeller(
       A=A.labs,
       Cat = C.labs,Div=D.labs,
       M=M.labs
     )
   )+
   scale_color_manual(values=palz,labels=c("Scenario 1: No Eco. Div.; Pref. Div.","Scenario 2: Eco Div.; No Pref. Div.","Scenario 3:Eco. Div.; Pref. Div."),name="")+
   theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "top") +
   scale_x_discrete(labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10"))+
   ylab("Divergence in neutral loci")+
   xlab("Risk (qr) or intensity (qi) of sperm competition"))
#ggsave("Figure7.pdf",height = 1*183,width=(1.5*183),units="mm")

# Supplemental figures ----------------------------------------------------
# 10. Figure S1. Divergence All neutral loci heatmap ----------------------------------------------------------
#Neutral divergence across all sensitivity analyses.
ggplot(NsumFinal%>%filter(), aes(x = Level, y = M, fill = DivBPA_mean)) +
  geom_tile(size = 0.05,color="grey") +
  facet_nested(
    Cat+PopSize~A+Tradeoff,
    labeller = labeller(
      A=A.labs,
      Cat = C.labs,
      Tradeoff=T.labs,
      PopSize=P.labs
    )
  ) +
  scale_fill_distiller(
    limits=c(NA,1),
    trans="log10",
    palette = "Blues",
    na.value = "grey",
    direction = 1,
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black",frame.linewidth=0.5
    ))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0),labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")) +
  labs(y = "Migration rate (m)", x = "Risk (qr) or intensity (qi) of sperm competition", fill =
         "Divergence in \n neutral loci")+
  theme(legend.position = "left")+
  geom_vline(aes(xintercept=4),size=1)+
  theme(panel.border = element_rect(color="black",fill=NA))

#ggsave("Figure_S1.pdf",height = 1.3*183,width=(2.5*183),units="mm")
# 11. Figure S2 CSP only. -----------------------------------------------------
#this figure shows how divergence in neutral loci and cryptic preferences change over time across simulation replicates when there is No ecological divergence but there are initially diverged preferences.

#First load in data of individual runs for risks of sperm competition at small population size
RNoE<-readRDS("Data_n/NoE_small_runs.rds")%>%
  filter(M=="10",Tradeoff=="false")

#First load in data of individual runs for intesntieis of sperm competition at small population size 
RNoE_I<-readRDS("Data_n/NoE_I_small_runs.rds")%>% 
  filter(M=="10",Tradeoff=="false")

#Combine data sets for single graph
RNoE_all<-rbind(RNoE,RNoE_I)%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level))%>%
  mutate(Level=recode(Level,"1.0"="1.0/2"))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))

#Make panel A just showing the divergence of cryptic preferences
(S2a<-RNoE_all%>%
    filter(A=="1.0")%>%
    ggplot(aes(group=Rep,x=Generation,y=DivFA))+
    #scale_color_manual(values=pal2)+
    geom_hline(aes(yintercept=40),linetype="dashed",alpha=0.5)+
    geom_path(alpha=0.3,show.legend = F)+
    theme(panel.border = element_rect(color="black",fill=NA))+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(), 
          axis.ticks.x=element_blank())+
    facet_nested(.~Level,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("Divergence in \n cryptic preference")+
    xlab(""))

#Make panel B just showing the divergence of neutral loci
(S2b<-RNoE_all%>%
    filter(A=="1.0")%>%
    ggplot(aes(group=Rep,x=Generation,y=DivBPA))+
    #scale_color_manual(values=pal2)+
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank())+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
    geom_hline(aes(yintercept=1),linetype="dashed",alpha=0.5)+
    geom_path(alpha=0.3,show.legend = F)+
    facet_nested(.~Level,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("Divergence in \n neutral loci"))


(S2a/S2b)+plot_annotation(tag_levels = "A",tag_suffix = ")")
#ggsave("FigureS2.pdf",height = 0.75*183,width=(1.7*183),units="mm")

# 12. Figure S3. No inital CSP ----------------------------------------------------
#this figure shows how divergence in neutral loci and cryptic preferences change over time across simulation replicates when there is ecological divergence but no initial preference divergence.
RNS<-readRDS("Data_n/NS_runs.rds")%>%
  filter(Tradeoff=="false")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Reading in data for when there is initially no divergent cryptic preferences, large population size and intensities of sperm competition
RNS_I<-readRDS("Data_n/NS_I_runs.rds")%>% 
  filter(Tradeoff=="false")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Putting them together
RNS_all<-rbind(RNS,RNS_I)%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level))%>%
  mutate(Level=recode(Level,"1.0"="1.0/2"))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))
#Make panel A just showing the divergence of cryptic preferences
(S3a<-RNS_all%>%
   filter(A=="1.0",M=="4%")%>%
   ggplot(aes(group=Rep,x=Generation,y=DivFA))+
   geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5,size=1)+
   geom_path(alpha=0.3,show.legend = F)+
   theme(panel.border = element_rect(color="black",fill=NA))+
   theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(), 
         axis.ticks.x=element_blank())+
   facet_nested(.~Level,labeller = labeller(A = A.labs,M=M.labs))+
   ylab("Divergence in \n cryptic preference")+
   xlab(""))



#Make panel B just showing the divergence of neutral loci

(S3b<-RNS_all%>%
    filter(A=="1.0",M=="4%")%>%
    ggplot(aes(group=Rep,x=Generation,y=DivBPA))+
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank())+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
    geom_hline(aes(yintercept=1),linetype="dashed",alpha=0.5,size=1)+
    geom_path(alpha=0.3,show.legend = F)+
    facet_nested(.~A+Level,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("Divergence in \n neutral loci"))

#Putting the plot together
(S3a/S3b)+plot_annotation(tag_levels = "A",tag_suffix = ")")
#ggsave("FigureS3.pdf",height = 0.75*183,width=(1.7*183),units="mm")
# 13. Figure S4 a Divergence All Female Preference--------------------------------------------------------------------
#Figure S4 is similar plot as figure 3 but showing all the sensitivity analyses.
ggplot(NsumFinal%>%filter(), aes(x = Level, y = M, fill = DivFA_mean)) +
  geom_tile(size = 0.05,color="grey") +
  facet_nested(
    Cat+PopSize~A+Tradeoff,
    labeller = labeller(
      A=A.labs,
      Cat = C.labs,
      Tradeoff=T.labs,
      PopSize=P.labs
    )
  )  +
  scale_fill_distiller(
    palette = "BuPu",
    na.value = "grey",
    direction = 1,
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black",frame.linewidth=0.5
    ))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0),labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")) +
  labs(y = "Migration rate (m)", x = "Risk (qr) or intensity (qi) of sperm competition", fill =
         "Divergence in \n preferences (f)")+
  theme(legend.position = "left")+
  geom_vline(aes(xintercept=4),size=1)+
  theme(panel.border = element_rect(color="black",fill=NA))

#ggsave("Figure_S4.pdf",height = 1.3*183,width=(2.5*183),units="mm")
# 14. Figure S5 divergence in male trait -------------------------------------------
#Similar graph as figure S4 but focused on male trait (t)
ggplot(NsumFinal%>%filter(), aes(x = Level, y = M, fill = DivMA_mean)) +
  geom_tile(size = 0.05,color="grey") +
  facet_nested(
    Cat+PopSize~A+Tradeoff,
    labeller = labeller(
      A=A.labs,
      Cat = C.labs,
      Tradeoff=T.labs,
      PopSize=P.labs
    )
  )  +
  scale_fill_distiller(
    palette = "BuPu",
    na.value = "grey",
    direction = 1,
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black",frame.linewidth=0.5
    ))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0),labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")) +
  labs(y = "Migration rate (m)", x = "Risk (qr) or intensity (qi) of sperm competition", fill =
         "Divergence in \n sperm trait (t)")+
  theme(legend.position = "left")+
  geom_vline(aes(xintercept=4),size=1)+
  theme(panel.border = element_rect(color="black",fill=NA))
#ggsave("Figure_S5.pdf",height = 1.3*183,width=(2.5*183),units="mm")
# 15. Figure S6. Correlation between divergence in cryptic preference and neutral loci for Scenario 1 ---------------------------------------------------
### You can skip 15a-15m for computational efficiency
### Results from this analysis were saved and can be loaded up directly to generate the graph
# * 15.a. Calculate 0.25 correlations --------------------------------------------------------------------
D25<-data.frame(Gen=unique(RNoE_all$Generation),P=0,S=0,Level="0.25")
count=1
for(i in unique(RNoE_all$Generation)){
  Temp<-RNoE_all%>%filter(Generation==2999,A=="1.0",Level=="0.25")%>%
    pull(DivBPA)
  Temp2<-RNoE_all%>%filter(Generation==i,A=="1.0",Level=="0.25")%>%
    pull(DivFA)
  D25[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D25[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 15.b. Calculate 0.5 correlations --------------------------------------------------------------------
D05<-data.frame(Gen=unique(RNoE_all$Generation),P=0,S=0,Level="0.5")
count=1
for(i in unique(RNoE_all$Generation)){
  Temp<-RNoE_all%>%filter(Generation==2999,A=="1.0",Level=="0.5")%>%
    pull(DivBPA)
  Temp2<-RNoE_all%>%filter(Generation==i,A=="1.0",Level=="0.5")%>%
    pull(DivFA)
  D05[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D05[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 15.c. Calculate 0.75 correlations --------------------------------------------------------------------
D75<-data.frame(Gen=unique(RNoE_all$Generation),P=0,S=0,Level="0.75")
count=1
for(i in unique(RNoE_all$Generation)){
  Temp<-RNoE_all%>%filter(Generation==2999,A=="1.0",Level=="0.75")%>%
    pull(DivBPA)
  Temp2<-RNoE_all%>%filter(Generation==i,A=="1.0",Level=="0.75")%>%
    pull(DivFA)
  D75[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D75[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 15.d. Calculate 1 correlations --------------------------------------------------------------------
D1<-data.frame(Gen=unique(RNoE_all$Generation),P=0,S=0,Level="1.0/2")
count=1
for(i in unique(RNoE_all$Generation)){
  Temp<-RNoE_all%>%filter(Generation==2999,A=="1.0",Level=="1.0/2")%>%
    pull(DivBPA)
  Temp2<-RNoE_all%>%filter(Generation==i,A=="1.0",Level=="1.0/2")%>%
    pull(DivFA)
  D1[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D1[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 15.e. Calculate 3 correlations --------------------------------------------------------------------
D3<-data.frame(Gen=unique(RNoE_all$Generation),P=0,S=0,Level="3")
count=1
for(i in unique(RNoE_all$Generation)){
  Temp<-RNoE_all%>%filter(Generation==2999,A=="1.0",Level=="3")%>%
    pull(DivBPA)
  Temp2<-RNoE_all%>%filter(Generation==i,A=="1.0",Level=="3")%>%
    pull(DivFA)
  D3[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D3[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 15.f. Calculate 4 correlations --------------------------------------------------------------------
D4<-data.frame(Gen=unique(RNoE_all$Generation),P=0,S=0,Level="4")
count=1
for(i in unique(RNoE_all$Generation)){
  Temp<-RNoE_all%>%filter(Generation==2999,A=="1.0",Level=="4")%>%
    pull(DivBPA)
  Temp2<-RNoE_all%>%filter(Generation==i,A=="1.0",Level=="4")%>%
    pull(DivFA)
  D4[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D4[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 15.g. Calculate 5 correlations -----------------------------------------------------------------------
D5<-data.frame(Gen=unique(RNoE_all$Generation),P=0,S=0,Level="5")
count=1
for(i in unique(RNoE_all$Generation)){
  Temp<-RNoE_all%>%filter(Generation==2999,A=="1.0",Level=="5")%>%
    pull(DivBPA)
  Temp2<-RNoE_all%>%filter(Generation==i,A=="1.0",Level=="5")%>%
    pull(DivFA)
  D5[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D5[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 15.h. Calculate 6 correlations -----------------------------------------------------------------------
D6<-data.frame(Gen=unique(RNoE_all$Generation),P=0,S=0,Level="6")
count=1
for(i in unique(RNoE_all$Generation)){
  Temp<-RNoE_all%>%filter(Generation==2999,A=="1.0",Level=="6")%>%
    pull(DivBPA)
  Temp2<-RNoE_all%>%filter(Generation==i,A=="1.0",Level=="6")%>%
    pull(DivFA)
  D6[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D6[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 15.i. Calculate 7 correlations -----------------------------------------------------------------------
D7<-data.frame(Gen=unique(RNoE_all$Generation),P=0,S=0,Level="7")
count=1
for(i in unique(RNoE_all$Generation)){
  Temp<-RNoE_all%>%filter(Generation==2999,A=="1.0",Level=="7")%>%
    pull(DivBPA)
  Temp2<-RNoE_all%>%filter(Generation==i,A=="1.0",Level=="7")%>%
    pull(DivFA)
  D7[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D7[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}
# * 15.j. Calculate 8 correlations -----------------------------------------------------------------------
D8<-data.frame(Gen=unique(RNoE_all$Generation),P=0,S=0,Level="8")
count=1
for(i in unique(RNoE_all$Generation)){
  Temp<-RNoE_all%>%filter(Generation==2999,A=="1.0",Level=="8")%>%
    pull(DivBPA)
  Temp2<-RNoE_all%>%filter(Generation==i,A=="1.0",Level=="8")%>%
    pull(DivFA)
  D8[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D8[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}
# * 15.k. Calculate 9 correlations -----------------------------------------------------------------------
D9<-data.frame(Gen=unique(RNoE_all$Generation),P=0,S=0,Level="9")
count=1
for(i in unique(RNoE_all$Generation)){
  Temp<-RNoE_all%>%filter(Generation==2999,A=="1.0",Level=="9")%>%
    pull(DivBPA)
  Temp2<-RNoE_all%>%filter(Generation==i,A=="1.0",Level=="9")%>%
    pull(DivFA)
  D9[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D9[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 15.l. Calculate 10 correlations -----------------------------------------------------------------------
D10<-data.frame(Gen=unique(RNoE_all$Generation),P=0,S=0,Level="10")
count=1
for(i in unique(RNoE_all$Generation)){
  Temp<-RNoE_all%>%filter(Generation==2999,A=="1.0",Level=="10")%>%
    pull(DivBPA)
  Temp2<-RNoE_all%>%filter(Generation==i,A=="1.0",Level=="10")%>%
    pull(DivFA)
  D10[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D10[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 15.m. Putting the correlations all together ----------------------------------
#Different Levels parameter values
#D<-rbind(D25,D05,D75,D1,D3,D4,D5,D6,D7,D8,D9,D10)
#write_csv(D,"Rsquares_NoE_small.csv")
# * 15.n. Making the plot Figure S6 ---------------------------------------------------------
#Reading in data if analysis stage is skipped.
DNoE<-read.csv("RSQ/Rsquares_NoE_small.csv")
DNoE%>%
  mutate(A="1.0")%>%
  mutate(Cat="NoE")%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))%>%
  ggplot(aes(x=Gen,y=P))+
  geom_hline(aes(yintercept=0),alpha=0.5,linetype="dashed",size=1)+
  geom_line(show.legend = F)+
  #scale_color_manual(values=pal2)+
  facet_nested(.~Cat+Level,labeller = labeller(A = A.labs,Cat=C.labs))+
  xlab("Generation used for divergence in cryptic preference")+
  ylab("Correlation between cryptic preference divergence  \n and neutral loci divergence in final generation")+
  scale_x_continuous(breaks=c(0,1500,3000))+
  theme(text=element_text(size=16))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("FigureS6_cor.pdf",height = 0.75*183,width=(1.5*183),units="mm")

# 16. Figure S7: correlation between preference and neutral loci -----------------------------------------------------------
RNoE_S7<-readRDS("Processed_Data_cor/NoE_cor_runs.rds")%>%
  filter(M=="10",Tradeoff=="false")

#First load in data of individual runs for intesntieis of sperm competition at small population size 
RNoE_I_S7<-readRDS("Processed_Data_cor/NoE_I_cor_runs.rds")%>% 
  filter(M=="10",Tradeoff=="false")

#Combine data sets for single graph
RNoE_all_S7<-rbind(RNoE_S7,RNoE_I_S7)%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level))%>%
  mutate(Level=recode(Level,"1.0"="1.0/2"))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))

#Make panel A just showing the divergence of cryptic preferences
#note that cor.me for this file is indeed neutral loci and cryptic preference
RNoE_all_S7%>%
  mutate(cor.me_a=abs(cor.me_a),cor.fe_a=abs(cor.fe_a),cor.me_b=abs(cor.me_b),cor.fe_b=abs(cor.fe_b))%>%
  group_by(Generation,A,Level)%>%
  select(Generation,A,Level,cor.fe_a,cor.me_a,cor.fe_b,cor.me_b)%>%
  summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
  filter(A=="1.0")%>%
  mutate(Cat="NoE")%>%
  ggplot(aes(x=Generation,y=cor.me_b_mean))+
  geom_hline(aes(yintercept=0),alpha=0.4,size=1,linetype="dashed")+
  geom_line(size=1)+
  geom_ribbon(
    aes(
      ymin = cor.me_b_mean - cor.me_b_sd/sqrt(50)*2,
      ymax = cor.me_b_mean + cor.me_b_sd/sqrt(50)*2),
    alpha = 0.3,
    color = NA
  )+
  facet_nested(.~Cat+Level,labeller = labeller(A = A.labs,Cat=C.labs))+
  theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Correlation between\npreference (f) and neutral loci")+
  xlab("Generation")
#ggsave("FigureS7.pdf",height = 0.75*183,width=(1.7*183),units="mm")

# 17. Figure S8:Scenario 1 SI --------------------------------------------------
(NoEfs<-
   LNoE_all%>%
   mutate(Nmig=ifelse(PopSize=="Small",10,50),
          NNonmig=ifelse(PopSize=="Small",990,4950))%>%
   mutate(FRelMig_a=(F_SurvOff_Mig_a/Nmig)/(F_SurvOff_NMig_a/NNonmig))%>%
   mutate(MRelMig_a=(M_SurvOff_Mig_a/Nmig)/(M_SurvOff_NMig_a/NNonmig))%>%
   select(Cat,A,An,M,Level,Tradeoff,Generation,FRelMig_a,MRelMig_a,PopSize)%>%
   group_by(Cat,A,An,M,Level,Tradeoff,PopSize)%>%
   summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
   filter(Cat=="NoE",M=="1%",Tradeoff=="FALSE")%>%
   ggplot(aes(x=Level,y=FRelMig_a_mean,fill=PopSize,color=PopSize,linetype=PopSize))+
   geom_point(size=2)+
   geom_line(aes(group=PopSize))+
   geom_errorbar(
     aes(
       ymin = FRelMig_a_mean -FRelMig_a_sd/sqrt(50)*2,
       ymax =FRelMig_a_mean+ FRelMig_a_sd/sqrt(50)*2)
   )+
   geom_vline(aes(xintercept=4),alpha=0.4)+
   facet_nested(~Cat+A,labeller = labeller(A = A.labs,Cat=C.labs))+
   ylab("Relative female\n migrant fitness")+
   xlab("")+
   theme(panel.border = element_rect(color="black",fill=NA),legend.position="top",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(), 
         axis.ticks.x=element_blank())+
   scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
   scale_fill_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
   labs(linetype="Population \n Size")+
   scale_x_discrete(labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))


(Noms<-LNoE_all%>%
    mutate(AM_SonOff_Beta=abs(M_Off_Beta_a-M_Off_Beta_b))%>%
    #remove extreme outliers
    filter(abs(AM_SonOff_Beta)<0.8)%>%
    select(Cat,A,An,M,Level,Tradeoff,Generation,AM_SonOff_Beta,PopSize,cor.mf_a)%>%
    group_by(Cat,A,An,M,Level,Tradeoff,PopSize)%>%
    summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
    filter(Cat=="NoE",M=="1%",Tradeoff=="FALSE")%>%
    ggplot(aes(x=Level,y=AM_SonOff_Beta_mean,fill=PopSize,color=PopSize,linetype=PopSize))+
    #geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed")+
    geom_point(size=2)+
    geom_line(aes(group=PopSize))+
    geom_errorbar(
      aes(
        ymin = AM_SonOff_Beta_mean -AM_SonOff_Beta_sd/sqrt(50)*2,
        ymax =AM_SonOff_Beta_mean+ AM_SonOff_Beta_sd/sqrt(50)*2)
    )+
    facet_nested(~A,labeller = labeller(A = A.labs))+
    geom_vline(aes(xintercept=4),alpha=0.4,size=1)+
    ylab("Divergent selection\n(b) acting on t")+
    xlab("")+
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank(),axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(),legend.position = "none")+
    scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    scale_fill_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    labs(linetype="Population \n Size")+
    scale_x_discrete(labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))

(Nomd<-LNoE_all%>%
    #mutate(AM_SonOff_Beta=abs(M_Off_Beta_a-M_Off_Beta_b))%>%
    #filter(abs(AM_SonOff_Beta)<1)%>%
    select(Cat,A,An,M,Level,Tradeoff,Generation,DivMA,PopSize,cor.mf_a)%>%
    group_by(Cat,A,An,M,Level,Tradeoff,PopSize)%>%
    summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
    filter(Cat=="NoE",M=="1%",Tradeoff=="FALSE")%>%
    ggplot(aes(x=Level,y=DivMA_mean,fill=PopSize,color=PopSize,linetype=PopSize))+
    #geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed")+
    geom_point(size=2)+
    geom_line(aes(group=PopSize))+
    geom_errorbar(
      aes(
        ymin = DivMA_mean -DivMA_sd/sqrt(50)*2,
        ymax =DivMA_mean+ DivMA_sd/sqrt(50)*2)
    )+
    facet_nested(~A,labeller = labeller(A = A.labs))+
    geom_vline(aes(xintercept=4),alpha=0.4,size=1)+
    ylab("Divergence in\nsperm trait (t)")+
    xlab("")+
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank(),axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(),legend.position = "none")+
    scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    scale_fill_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    labs(linetype="Population \n Size")+
    scale_x_discrete(labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))

(Nomf<-LNoE_all%>%
    mutate(AF_SonOff_Beta=abs(F_SonOff_Beta_b-F_SonOff_Beta_a))%>%
    mutate(AF_SurvOff_Beta=abs(F_SurvOff_Beta_b-F_SurvOff_Beta_a))%>%
    select(Cat,A,An,M,Level,Tradeoff,Generation,AF_SonOff_Beta,AF_SurvOff_Beta,PopSize,cor.mf_a)%>%
    group_by(Cat,A,An,M,Level,Tradeoff,PopSize)%>%
    summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
    filter(Cat=="NoE",M=="1%",Tradeoff=="FALSE")%>%
    ggplot(aes(x=Level,y=cor.mf_a_mean,fill=PopSize,color=PopSize,linetype=PopSize))+
    #geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed")+
    geom_point(size=2)+
    geom_line(aes(group=PopSize))+
    geom_errorbar(
      aes(
        ymin = cor.mf_a_mean -cor.mf_a_sd/sqrt(50)*2,
        ymax =cor.mf_a_mean+ cor.mf_a_sd/sqrt(50)*2)
    )+
    facet_nested(~A,labeller = labeller(A = A.labs))+
    geom_vline(aes(xintercept=4),alpha=0.4,size=1)+
    ylab("Correlation between \n f and t")+
    xlab("Risk (qr) or intensity (qi) of sperm competition")+
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank())+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    scale_fill_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    labs(linetype="Population \n Size")+
    scale_x_discrete(labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))

(NoEfs/Noms/Nomd/Nomf)+plot_annotation(tag_levels = "A",tag_suffix = ")")
#ggsave("FigureS8.pdf",height = 1.75*183,width=183*1.5,units="mm")

# 18. Figure S9: Migrant mating results -----------------------------------------------------------
#Read in data for small simulation
Small_sim<-read_csv("MigrantMating/Simulation_migrantmating.csv")

#Pivot longer
Small_sim_sum<-Small_sim%>%
  pivot_longer(everything(),names_to = "Level",values_to="Migrant matings")%>%
  group_by(Level)%>%
  summarise(Mean=mean(`Migrant matings`),SD=sd(`Migrant matings`))%>%
  mutate(Level=factor(Level,levels=c("0.0","0.25","0.5","0.75","2.0","3.0","4.0","5.0","6.0","7.0","8.0","9.0","10.0")))%>%
  mutate(PopSize="N=2000")%>%
  mutate(MeanScaled=Mean/1000,SDScaled=SD/1000)
#Read in large data summary
Large_sim<-read_csv("MigrantMating/Simulation_migrantmating-L.csv")
Large_sim_sum<-Large_sim%>%
  pivot_longer(everything(),names_to = "Level",values_to="Migrant matings")%>%
  group_by(Level)%>%
  summarise(Mean=mean(`Migrant matings`),SD=sd(`Migrant matings`))%>%
  mutate(Level=factor(Level,levels=c("0.0","0.25","0.5","0.75","2.0","3.0","4.0","5.0","6.0","7.0","8.0","9.0","10.0")))%>%
  mutate(PopSize="N=10000")%>%
  mutate(MeanScaled=Mean/5000,SDScaled=SD/5000)
#Read in analytical predictions from Equations S4 and S5
Analytical<-read.csv("MigrantMating/Analytical.csv")%>%
  mutate(PopSize="Predicted")%>%
  mutate(Level=factor(Level,levels=c("0","0.25","0.5","0.75","2","3","4","5","6","7","8","9","10")))%>%
  mutate(Level=recode(Level,"0"="0.0","2"="2.0","3"="3.0","4"="4.0","5"="5.0","6"="6.0","7"="7.0","8"="8.0","9"="9.0","10"="10.0"))

rbind(Small_sim_sum,Large_sim_sum)%>%
  mutate(PopSize=factor(PopSize,levels=c("N=2000","N=10000","Predicted")))%>%
  #filter(Level%in%c("2.0","3.0","4.0","5.0","6.0","7.0","8.0","9.0","10.0"))%>%
  ggplot(aes(x=Level,y=MeanScaled,color=PopSize,shape=PopSize))+
  geom_point(position = position_dodge(width=0.6))+
  scale_y_log10()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(0.85,0.85),legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"))+
  geom_point(data=Analytical,aes(y=Predicted))+
  scale_color_manual(name="",values=c("#83B4D7","#3F5384","black"))+
  labs(shape="")+
  scale_x_discrete(labels= c("0","0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10"))+
  ylab("Proportion of succesful migrant male matings")+
  xlab("Risk (qr) or intensity (qi) of sperm competition")

# 19. Figure S10 ----------------------------------------------------------
NoE_all<-rbind(NoE_S,NoE_S_I,NoE,NoE_I)
(a12<-NoE_all%>%
    filter(A=="1.0",M=="1%",Tradeoff=="false")%>%
    mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0","3","4","5","6","7","8","9","10")))%>%
    mutate(PopSize=factor(PopSize,levels=c("Small","Large")))%>%
    mutate(PopSize=recode(PopSize,"Small"="2,000","Large"="10,000"))%>%
    ggplot(aes(x=Generation,y=cor.mf_a_mean,color=PopSize))+
    geom_path()+
    theme(panel.border = element_rect(color="black",fill=NA))+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "top",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(), 
          axis.ticks.x=element_blank())+
    facet_nested(.~Level,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("Correlation (f and t)")+
    scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    xlab(""))

(b12<-NoE_all%>%
    filter(A=="1.0",M=="1%",Tradeoff=="false")%>%
    mutate(PopSize=factor(PopSize,levels=c("Small","Large")))%>%
    mutate(PopSize=recode(PopSize,"Small"="2,000","Large"="10,000"))%>%
    mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0","3","4","5","6","7","8","9","10")))%>%
    ggplot(aes(x=Generation,y=SD.Female_a_mean,color=PopSize))+
    geom_path()+
    theme(panel.border = element_rect(color="black",fill=NA))+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    facet_nested(.~Level,labeller = labeller(A = A.labs,M=M.labs))+
    scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    ylab("SD preference (f)")+
    xlab(""))

(c12<-NoE_all%>%
    filter(A=="1.0",M=="1%",Tradeoff=="false")%>%
    mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0","3","4","5","6","7","8","9","10")))%>%
    mutate(PopSize=factor(PopSize,levels=c("Small","Large")))%>%
    mutate(PopSize=recode(PopSize,"Small"="2,000","Large"="10,000"))%>%
    ggplot(aes(x=Generation,y=SD.Male_a_mean,color=PopSize))+
    #scale_color_manual(values=pal2)+
    #geom_hline(aes(yintercept=40),linetype="dashed",alpha=0.5)+
    geom_path(show.legend = F)+
    theme(panel.border = element_rect(color="black",fill=NA))+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_nested(.~Level,labeller = labeller(A = A.labs,M=M.labs))+
    scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    ylab("SD sperm trait (t)")+
    xlab("Generation"))

((a12/b12/c12)&theme(text=element_text(size=18)))+plot_annotation(tag_levels = "A",tag_suffix = ")")
#ggsave("FigureS10.pdf",height = 1.25*183,width=(1.7*183),units="mm")

# 20. Figure S11: no ejaculate depletion sensitivity analysis -------------
# 20. a. Read in the data ------------------------------------------------
NoE_S_ED<-readRDS("Processed_DataND/NoE_noD_Sum.rds")%>%
  filter(Generation==2999)%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))
NoE_S_I_ED<-readRDS("Processed_DataND/NoE_I_noD_Sum.rds")%>%
  filter(Generation==2999)%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))
NoEnd_ED<-rbind(NoE_S_ED,NoE_S_I_ED)%>%
  mutate(A=recode(A,"50" = "50.0"))%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0","3","4","5","6","7","8","9","10")))%>%
  mutate(PopSize=factor(PopSize,levels=c("Small","Large")))%>%
  mutate(Cat=factor(Cat,levels=c("NoE","NS","EE")))
# * 20.b. panel a ---------------------------------------------------------
(NoEND_ED<-NoEnd_ED%>%
    filter(Cat=="NoE",M=="1%",Tradeoff=="false")%>%
    ggplot(aes(x=Level,y=DivBPA_mean,fill=PopSize,color=PopSize,linetype=PopSize))+
    geom_point(size=2)+
    geom_line(aes(group=PopSize))+
    geom_errorbar(
      aes(
        ymin = DivBPA_mean -DivBPA_sd/sqrt(50)*2,
        ymax =DivBPA_mean+ DivBPA_sd/sqrt(50)*2)
    )+
    #geom_hline(aes(yintercept=1),linetype="dotted",alpha=0.4)+
    geom_vline(aes(xintercept=4),alpha=0.4,size=1)+
    facet_nested(~A,labeller = labeller(A = A.labs))+
    ylab("Divergence in \n neutral loci")+
    xlab("")+
    scale_y_continuous(limits=c(0,1),breaks=c(0,0.25,0.5,0.75,1))+
    theme(panel.border = element_rect(color="black",fill=NA),plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = c(0.055,0.6),legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    scale_fill_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    labs(linetype="Population \n Size")+
    scale_x_discrete(labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))
# * 20.c Female divergence end ---------------------------------------------------
LNoE_small_ED<-read_csv("Processed_DataND/NoE_noD_last.csv")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))
LNoE_small_I_ED<-read_csv("Processed_DataND/NoE_I_noD_last.csv")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

LNoE_all_ED<-rbind(LNoE_small_ED,LNoE_small_I_ED)%>%
  mutate(A=factor(A,levels=c("50","12.5","1")))%>%
  mutate(A=recode(A,"50" = "50.0","1"="1.0"))%>%
  mutate(Level=factor(Level))%>%
  mutate(Level=recode(Level,"1"="1.0/2"))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))%>%
  mutate(PopSize=factor(PopSize,levels=c("Small","Large")))

(NoED_ED<-NoEnd_ED%>%
    #filter(Cat=="NoE",M=="1%",Tradeoff=="false")%>%
    ggplot(aes(x=Level,y=DivFA_mean,fill=PopSize,color=PopSize,linetype=PopSize))+
    geom_hline(aes(yintercept=40),linetype="dotted",alpha=0.4)+
    geom_vline(aes(xintercept=4),alpha=0.4,size=1)+
    geom_point(size=2)+
    geom_line(aes(group=PopSize))+
    geom_errorbar(
      aes(
        ymin = DivFA_mean -DivFA_sd/sqrt(50)*2,
        ymax =DivFA_mean+ DivFA_sd/sqrt(50)*2)
    )+
    facet_nested(~A,labeller = labeller(A = A.labs))+
    ylab("Divergence in \n preferences (f)")+
    xlab("")+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(), 
          axis.ticks.x=element_blank())+
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank())+
    #theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = c(0.055,0.6),legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    scale_fill_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
    labs(linetype="Population \n Size")+
    scale_x_discrete(labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))

# * 20.d. Figure S11c Male and  Female migrant success ------------------------------------------
#Male migrant sucess
(NoEms_ED<-
   LNoE_all_ED%>%
   mutate(Nmig=ifelse(PopSize=="Small",10,50),
          NNonmig=ifelse(PopSize=="Small",990,4950))%>%
   mutate(FRelMig_a=(F_SurvOff_Mig_a/Nmig)/(F_SurvOff_NMig_a/NNonmig))%>%
   mutate(MRelMig_a=(M_SurvOff_Mig_a/Nmig)/(M_SurvOff_NMig_a/NNonmig))%>%
   select(Cat,A,An,M,Level,Tradeoff,Generation,FRelMig_a,MRelMig_a,PopSize)%>%
   group_by(Cat,A,An,M,Level,Tradeoff,PopSize)%>%
   summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
   filter(Cat=="NoE",M=="1%",Tradeoff=="FALSE")%>%
   ggplot(aes(x=Level,y=MRelMig_a_mean,fill=PopSize,color=PopSize,linetype=PopSize))+
   geom_point(size=2)+
   geom_hline(aes(yintercept=1.05),linetype="dashed",color="rosybrown1")+
   geom_line(aes(group=PopSize))+
   geom_errorbar(
     aes(
       ymin = MRelMig_a_mean -MRelMig_a_sd/sqrt(50)*2,
       ymax =MRelMig_a_mean+ MRelMig_a_sd/sqrt(50)*2)
   )+
   geom_vline(aes(xintercept=4),alpha=0.4,size=1)+
   facet_nested(~A,labeller = labeller(A = A.labs))+
   ylab("Relative male\nmigrant fitness")+
   xlab("")+
   theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
         strip.text.x = element_blank())+
   theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(), 
         axis.ticks.x=element_blank())+
   scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
   scale_fill_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
   labs(linetype="Population \n Size")+
   scale_x_discrete(labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))
# * 20.e.Figure S11D Beta selection cryptic preferences ----------------------------------------------------------------

(NoEB_ED<-LNoE_all_ED%>%
   mutate(AF_SonOff_Beta=abs(F_SonOff_Beta_b-F_SonOff_Beta_a))%>%
   mutate(AF_SurvOff_Beta=abs(F_SurvOff_Beta_b-F_SurvOff_Beta_a))%>%
   select(Cat,A,An,M,Level,Tradeoff,Generation,AF_SonOff_Beta,AF_SurvOff_Beta,PopSize)%>%
   group_by(Cat,A,An,M,Level,Tradeoff,PopSize)%>%
   summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
   filter(Cat=="NoE",M=="1%",Tradeoff=="FALSE")%>%
   ggplot(aes(x=Level,y=AF_SonOff_Beta_mean,fill=PopSize,color=PopSize,linetype=PopSize))+
   #geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed")+
   geom_point(size=2)+
   geom_line(aes(group=PopSize))+
   geom_errorbar(
     aes(
       ymin = AF_SonOff_Beta_mean -AF_SonOff_Beta_sd/sqrt(50)*2,
       ymax =AF_SonOff_Beta_mean+ AF_SonOff_Beta_sd/sqrt(50)*2)
   )+
   facet_nested(~A,labeller = labeller(A = A.labs))+
   geom_vline(aes(xintercept=4),alpha=0.4,size=1)+
   ylab("Divergent selection (b)\nacting on f")+
   xlab("Risk (qr) or intensity (qi) of sperm competition")+
   theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
         strip.text.x = element_blank())+
   theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
   scale_color_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
   scale_fill_manual(name="Population \n Size",values=c("#83B4D7","#3F5384"))+
   labs(linetype="Population \n Size")+
   scale_x_discrete(labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))
# * 20.f. putting it all together -------------------------------------------
(NoEND_ED/NoED_ED/NoEms_ED/NoEB_ED)+plot_annotation(tag_levels = "A",tag_suffix = ")")
#ggsave("FigureS11.pdf",height = 1.7*183,width=(1.5*183),units="mm")



# 21.  Figure S12 - divergent selecttion -----------------------------------
(s12dvs<-RNS_all2%>%
   filter(M%in%c("1%","2%","3%","4%","5%"),A%in%c("1.0"),Level%in%c("0.25","1.0/2","3","4","5","7","9"))%>%
   filter(Generation<1000)%>%
   mutate(AM_Off_Beta=abs(M_Off_Beta_a-M_Off_Beta_b))%>%
   select(Cat,A,An,M,Level,Tradeoff,Generation,AM_Off_Beta,PopSize)%>%
   group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
   summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
   ggplot(aes(x=Generation,y=mean,color=Level,fill=Level))+
   geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed",size=1)+
   geom_line(size=1)+
   geom_ribbon(
     aes(
       ymin = mean - sd/sqrt(50)*2,
       ymax = mean + sd/sqrt(50)*2,
       fill=Level),
     alpha = 0.3,
     color = NA
   )+
   facet_nested(.~Cat+M,labeller = labeller(A = A.labs,M=M.labs,Cat=C.labs))+
   ylab("Divergent selection (b)\nacting on t")+
   xlab("")+
   scale_color_manual(name = "Risk/intensity of sperm competition", values = pal)+
   scale_fill_manual(name = "Risk/intensity of sperm competition", values = pal)+
   theme(panel.border = element_rect(color="black",fill=NA),legend.position = "top",axis.text.x=element_blank(), 
         axis.ticks.x=element_blank())+guides(fill=guide_legend(nrow=1),color=guide_legend(nrow=1)))
(s12divma<-RNS_all2%>%
    filter(M%in%c("1%","2%","3%","4%","5%"),A%in%c("1.0"),Level%in%c("0.25","1.0/2","3","4","5","7","9"))%>%
    filter(Generation<1000)%>%
    #mutate(AM_Off_Beta=abs(M_Off_Beta_a-M_Off_Beta_b))%>%
    #mutate(DIVE=abs(Mean.E_a-Mean.E_b))%>%
    select(Cat,A,An,M,Level,Tradeoff,Generation,DivMA,PopSize)%>%
    group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
    summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
    ggplot(aes(x=Generation,y=mean,color=Level,fill=Level))+
    geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed",size=1)+
    geom_line(size=1)+
    geom_ribbon(
      aes(
        ymin = mean - sd/sqrt(50)*2,
        ymax = mean + sd/sqrt(50)*2,
        fill=Level),
      alpha = 0.3,
      color = NA
    )+
    facet_nested(.~M,labeller = labeller(A = A.labs,M=M.labs,Type=sel.labs))+
    ylab("Divergence in\n sperm trait (t)")+
    xlab("")+
    scale_color_manual(name = "Risk/intensity of sperm competition", values = pal)+
    scale_fill_manual(name = "Risk/intensity of sperm competition", values = pal)+
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank(),axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(),legend.position = "none"))

(s12dive<-RNS_all2%>%
    filter(M%in%c("1%","2%","3%","4%","5%"),A%in%c("1.0"),Level%in%c("0.25","1.0/2","3","4","5","7","9"))%>%
    filter(Generation<1000)%>%
    #mutate(AM_Off_Beta=abs(M_Off_Beta_a-M_Off_Beta_b))%>%
    mutate(DIVE=abs(Mean.E_a-Mean.E_b))%>%
    select(Cat,A,An,M,Level,Tradeoff,Generation,DIVE,PopSize)%>%
    group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
    summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
    ggplot(aes(x=Generation,y=mean,color=Level,fill=Level))+
    geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed",size=1)+
    geom_line(size=1)+
    geom_ribbon(
      aes(
        ymin = mean - sd/sqrt(50)*2,
        ymax = mean + sd/sqrt(50)*2,
        fill=Level),
      alpha = 0.3,
      color = NA
    )+
    facet_nested(.~M,labeller = labeller(A = A.labs,M=M.labs,Type=sel.labs))+
    ylab("Divergence in\n ecological trait (e)")+
    xlab("Generation")+
    scale_color_manual(name = "Risk/intensity of sperm competition", values = pal)+
    scale_fill_manual(name = "Risk/intensity of sperm competition", values = pal)+
    
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank(),legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1)))
((s12dvs/s12divma/s12dive)&theme(text=element_text(size=18)))+plot_annotation(tag_levels = "A",tag_suffix = ")")
#ggsave("FigureS12.pdf",height =1.5*183,width=(1.5*183),units="mm")
# 22. Figure S13 Example populations no initial preference divergence -------------------------------------------
#select example populations
pops<-c(6,17, 33, 12 ,21 ,27)
(dp<-RNS_all2%>%
    filter(A%in%c("1.0"),Level%in%c("4","6","8","10"),M=="4%",Rep%in%pops)%>%
    filter(Generation<200)%>%
    ggplot(aes(group=Rep,x=Generation,y=DivFA,color=factor(Rep)))+
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank())+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(), 
          axis.ticks.x=element_blank())+
    #geom_hline(aes(yintercept=40),linetype="dashed",alpha=0.5)+
    geom_path(alpha=0.5,show.legend = F)+
    xlab("")+
    facet_nested(.~A+Level,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("Divergence in \n preference (f)"))

(dn<-RNS_all2%>%
    filter(A%in%c("1.0"),Level%in%c("4","6","8","10"),M=="4%",Rep%in%pops)%>%
    filter(Generation<200)%>%
    ggplot(aes(group=Rep,x=Generation,y=DivBPA,color=factor(Rep)))+
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank())+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"))+
    geom_path(alpha=0.5,show.legend = F)+
    facet_nested(.~A+Level,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("Divergence in \n netural loci"))

(dme<-RNS_all2%>%
    filter(A%in%c("1.0"),Level%in%c("4","6","8","10"),M=="4%",Rep%in%pops)%>%
    filter(Generation<200)%>%
    ggplot(aes(group=Rep,x=Generation,y=abs(cor.fe_b),color=factor(Rep)))+
    #geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5)+
    geom_path(alpha=0.5,show.legend = F)+
    theme(panel.border = element_rect(color="black",fill=NA))+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(), 
          axis.ticks.x=element_blank())+
    facet_nested(.~A+Level,labeller = labeller(A = A.labs,M=M.labs))+
    xlab("")+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(), 
          axis.ticks.x=element_blank())+
    ylab("Genetic correlation\nbetween f and e"))

(dmb<-RNS_all2%>%
    filter(A%in%c("1.0"),Level%in%c("4","6","8","10"),M=="4%",Rep%in%pops)%>%
    filter(Generation<200)%>%
    ggplot(aes(group=Rep,x=Generation,y=abs(F_SurvOff_Beta_b),color=factor(Rep)))+
    #geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5)+
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank())+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(), 
          axis.ticks.x=element_blank())+
    geom_path(alpha=0.5,show.legend = F)+
    xlab("")+
    facet_nested(.~A+Level,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("Divergent selection \n (bdiff) acting on f"))
(dme/dmb/dp/dn)+plot_annotation(tag_levels = "A",tag_suffix = ")")
#ggsave("FigureS13.pdf",height = 1.6*183,width=(1.5*183),units="mm")
# 23. Figure S14. Correlation between sperm trait and ecological trait ---------------------------------------------------------------
(FS14A<-RNS_all2%>%
    filter(M%in%c("1%","2%","3%","4%","5%"),A=="1.0",Level%in%c("0.25","1.0/2","3","4","5","7","9"))%>%
    filter(Generation<1000)%>%
    mutate(cor.me_a=abs(cor.me_a))%>%
    select(Cat,A,An,M,Level,Tradeoff,Generation,cor.me_a,PopSize)%>%
    group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
    summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
    filter(Generation<1000)%>%
    ggplot(aes(x=Generation,y=mean,color=Level,fill=Level))+
    geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed",size=1)+
    geom_line(size=1)+
    geom_ribbon(
      aes(
        ymin = mean - sd/sqrt(50)*2,
        ymax = mean + sd/sqrt(50)*2,
        fill=Level),
      alpha = 0.3,
      color = NA
    )+
    facet_nested(.~Cat+M,labeller = labeller(A = A.labs,M=M.labs,Cat=C.labs))+
    ylab("Genetic correlation\nbetween t and e")+
    xlab("")+
    scale_color_manual(name = "Risk/intensity of sperm competition", values = pal)+
    scale_fill_manual(name = "Risk/intensity of sperm competition", values = pal)+
    theme(panel.border = element_rect(color="black",fill=NA),legend.position = "top",axis.text.x=element_blank(), 
          axis.ticks.x=element_blank())+guides(fill=guide_legend(nrow=1),color=guide_legend(nrow=1)))


(sdE<-RNS_all2%>%
    filter(M%in%c("1%","2%","3%","4%","5%"),A%in%c("1.0"),Level%in%c("0.25","1.0/2","3","4","5","7","9"))%>%
    filter(Generation<1000)%>%
    #mutate(AM_Off_Beta=abs(M_Off_Beta_a-M_Off_Beta_b))%>%
    #mutate(DIVE=abs(Mean.E_a-Mean.E_b))%>%
    select(Cat,A,An,M,Level,Tradeoff,Generation,SD.E_a,PopSize)%>%
    group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
    summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
    ggplot(aes(x=Generation,y=mean,color=Level,fill=Level))+
    geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed",size=1)+
    geom_line(size=1)+
    geom_ribbon(
      aes(
        ymin = mean - sd/sqrt(50)*2,
        ymax = mean + sd/sqrt(50)*2,
        fill=Level),
      alpha = 0.3,
      color = NA
    )+
    facet_nested(.~M,labeller = labeller(A = A.labs,M=M.labs,Type=sel.labs))+
    ylab("SD ecological trait (e)")+
    xlab("")+
    scale_color_manual(name = "Risk/intensity of sperm competition", values = pal)+
    scale_fill_manual(name = "Risk/intensity of sperm competition", values = pal)+
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank(),axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(),legend.position = "none")+guides(fill=guide_legend(nrow=1),color=guide_legend(nrow=1)))

(sdm<-RNS_all2%>%
    filter(M%in%c("1%","2%","3%","4%","5%"),A%in%c("1.0"),Level%in%c("0.25","1.0/2","3","4","5","7","9"))%>%
    filter(Generation<1000)%>%
    #mutate(AM_Off_Beta=abs(M_Off_Beta_a-M_Off_Beta_b))%>%
    #mutate(DIVE=abs(Mean.E_a-Mean.E_b))%>%
    select(Cat,A,An,M,Level,Tradeoff,Generation,SD.Male_a,PopSize)%>%
    group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
    summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
    ggplot(aes(x=Generation,y=mean,color=Level,fill=Level))+
    geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed",size=1)+
    geom_line(size=1)+
    geom_ribbon(
      aes(
        ymin = mean - sd/sqrt(50)*2,
        ymax = mean + sd/sqrt(50)*2,
        fill=Level),
      alpha = 0.3,
      color = NA
    )+
    facet_nested(.~M,labeller = labeller(A = A.labs,M=M.labs,Type=sel.labs))+
    ylab("SD sperm trait (t)")+
    xlab("")+
    scale_color_manual(name = "Risk/intensity of sperm competition", values = pal)+
    scale_fill_manual(name = "Risk/intensity of sperm competition", values = pal)+
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank(),axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(),legend.position = "none")+guides(fill=guide_legend(nrow=1),color=guide_legend(nrow=1)))


(sdf<-RNS_all2%>%
    filter(M%in%c("1%","2%","3%","4%","5%"),A%in%c("1.0"),Level%in%c("0.25","1.0/2","3","4","5","7","9"))%>%
    filter(Generation<1000)%>%
    #mutate(AM_Off_Beta=abs(M_Off_Beta_a-M_Off_Beta_b))%>%
    #mutate(DIVE=abs(Mean.E_a-Mean.E_b))%>%
    select(Cat,A,An,M,Level,Tradeoff,Generation,SD.Female_a,PopSize)%>%
    group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
    summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
    ggplot(aes(x=Generation,y=mean,color=Level,fill=Level))+
    geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed",size=1)+
    geom_line(size=1)+
    geom_ribbon(
      aes(
        ymin = mean - sd/sqrt(50)*2,
        ymax = mean + sd/sqrt(50)*2,
        fill=Level),
      alpha = 0.3,
      color = NA
    )+
    facet_nested(.~M,labeller = labeller(A = A.labs,M=M.labs,Type=sel.labs))+
    ylab("SD preference (f)")+
    xlab("Generation")+
    scale_color_manual(name = "Risk/intensity of sperm competition", values = pal)+
    scale_fill_manual(name = "Risk/intensity of sperm competition", values = pal)+
    
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank(),legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1))+guides(fill=guide_legend(nrow=1),color=guide_legend(nrow=1)))
((FS14A/sdE/sdm/sdf)&theme(text=element_text(size=18)))+plot_annotation(tag_levels = "A",tag_suffix = ")")
#ggsave("FigureS14.pdf",height =1.75*183,width=(1.5*183),units="mm")
# 24. Figure S15. Correlation between divergence in cryptic preference and neutral loci for when there is no inital cryptic preference divergence  ---------------------------------------------------------------------
### You can skip 24a-24m for computational efficiency. otherwise rerun and save results, changing the migration rate you are filtering for. (e.g.,change m=4% to m =3%)
### Results from this analysis were saved and can be loaded up directly to generate the graph
# * 24.a. Calculate 0.25 correlations --------------------------------------------------------------------
D25NS<-data.frame(Gen=unique(RNS_all$Generation),P=0,S=0,Level="0.25")
count=1
for(i in unique(RNS_all$Generation)){
  Temp<-RNS_all%>%filter(Generation==2999,A=="1.0",Level=="0.25",M=="4%")%>%
    pull(DivBPA)
  Temp2<-RNS_all%>%filter(Generation==i,A=="1.0",Level=="0.25",M=="4%")%>%
    pull(DivFA)
  D25NS[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D25NS[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 24.b. Calculate 0.5 correlations --------------------------------------------------------------------
D05NS<-data.frame(Gen=unique(RNS_all$Generation),P=0,S=0,Level="0.5")
count=1
for(i in unique(RNS_all$Generation)){
  Temp<-RNS_all%>%filter(Generation==2999,A=="1.0",Level=="0.5",M=="4%")%>%
    pull(DivBPA)
  Temp2<-RNS_all%>%filter(Generation==i,A=="1.0",Level=="0.5",M=="4%")%>%
    pull(DivFA)
  D05NS[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D05NS[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 24.c. Calculate 0.75 correlations --------------------------------------------------------------------
D75NS<-data.frame(Gen=unique(RNS_all$Generation),P=0,S=0,Level="0.75")
count=1
for(i in unique(RNS_all$Generation)){
  Temp<-RNS_all%>%filter(Generation==2999,A=="1.0",Level=="0.75",M=="4%")%>%
    pull(DivBPA)
  Temp2<-RNS_all%>%filter(Generation==i,A=="1.0",Level=="0.75",M=="4%")%>%
    pull(DivFA)
  D75NS[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D75NS[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}
# * 24.d. Calculate"1.0/2"correlations --------------------------------------------------------------------
D1NS<-data.frame(Gen=unique(RNS_all$Generation),P=0,S=0,Level="1.0/2")
count=1
for(i in unique(RNS_all$Generation)){
  Temp<-RNS_all%>%filter(Generation==2999,A=="1.0",Level=="1.0/2",M=="4%")%>%
    pull(DivBPA)
  Temp2<-RNS_all%>%filter(Generation==i,A=="1.0",Level=="1.0/2",M=="4%")%>%
    pull(DivFA)
  D1NS[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D1NS[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}
# * 24.e. Calculate 3 correlations --------------------------------------------------------------------
D3NS<-data.frame(Gen=unique(RNS_all$Generation),P=0,S=0,Level="3")
count=1
for(i in unique(RNS_all$Generation)){
  Temp<-RNS_all%>%filter(Generation==2999,A=="1.0",Level=="3",M=="4%")%>%
    pull(DivBPA)
  Temp2<-RNS_all%>%filter(Generation==i,A=="1.0",Level=="3",M=="4%")%>%
    pull(DivFA)
  D3NS[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D3NS[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 24.f. Calculate 4 correlations --------------------------------------------------------------------
D4NS<-data.frame(Gen=unique(RNS_all$Generation),P=0,S=0,Level="4")
count=1
for(i in unique(RNS_all$Generation)){
  Temp<-RNS_all%>%filter(Generation==2999,A=="1.0",Level=="4",M=="4%")%>%
    pull(DivBPA)
  Temp2<-RNS_all%>%filter(Generation==i,A=="1.0",Level=="4",M=="4%")%>%
    pull(DivFA)
  D4NS[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D4NS[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 24.g. Calculate 5 correlations -----------------------------------------------------------------------
D5NS<-data.frame(Gen=unique(RNS_all$Generation),P=0,S=0,Level="5")
count=1
for(i in unique(RNS_all$Generation)){
  Temp<-RNS_all%>%filter(Generation==2999,A=="1.0",Level=="5",M=="4%")%>%
    pull(DivBPA)
  Temp2<-RNS_all%>%filter(Generation==i,A=="1.0",Level=="5",M=="4%")%>%
    pull(DivFA)
  D5NS[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D5NS[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}
# * 24.h. Calculate 6 correlations -----------------------------------------------------------------------
D6NS<-data.frame(Gen=unique(RNS_all$Generation),P=0,S=0,Level="6")
count=1
for(i in unique(RNS_all$Generation)){
  Temp<-RNS_all%>%filter(Generation==2999,A=="1.0",Level=="6",M=="4%")%>%
    pull(DivBPA)
  Temp2<-RNS_all%>%filter(Generation==i,A=="1.0",Level=="6",M=="4%")%>%
    pull(DivFA)
  D6NS[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D6NS[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 24.i. Calculate 7 correlations -----------------------------------------------------------------------
D7NS<-data.frame(Gen=unique(RNS_all$Generation),P=0,S=0,Level="7")
count=1
for(i in unique(RNS_all$Generation)){
  Temp<-RNS_all%>%filter(Generation==2999,A=="1.0",Level=="7",M=="4%")%>%
    pull(DivBPA)
  Temp2<-RNS_all%>%filter(Generation==i,A=="1.0",Level=="7",M=="4%")%>%
    pull(DivFA)
  D7NS[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D7NS[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}
# * 24.j. Calculate 8 correlations -----------------------------------------------------------------------
D8NS<-data.frame(Gen=unique(RNS_all$Generation),P=0,S=0,Level="8")
count=1
for(i in unique(RNS_all$Generation)){
  Temp<-RNS_all%>%filter(Generation==2999,A=="1.0",Level=="8",M=="4%")%>%
    pull(DivBPA)
  Temp2<-RNS_all%>%filter(Generation==i,A=="1.0",Level=="8",M=="4%")%>%
    pull(DivFA)
  D8NS[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D8NS[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}
# * 24.k. Calculate 9 correlations -----------------------------------------------------------------------
D9NS<-data.frame(Gen=unique(RNS_all$Generation),P=0,S=0,Level="9")
count=1
for(i in unique(RNS_all$Generation)){
  Temp<-RNS_all%>%filter(Generation==2999,A=="1.0",Level=="9",M=="4%")%>%
    pull(DivBPA)
  Temp2<-RNS_all%>%filter(Generation==i,A=="1.0",Level=="9",M=="4%")%>%
    pull(DivFA)
  D9NS[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D9NS[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}

# * 24.l. Calculate 10 correlations -----------------------------------------------------------------------
D10NS<-data.frame(Gen=unique(RNS_all$Generation),P=0,S=0,Level="10")
count=1
for(i in unique(RNS_all$Generation)){
  Temp<-RNS_all%>%filter(Generation==2999,A=="1.0",Level=="10",M=="4%")%>%
    pull(DivBPA)
  Temp2<-RNS_all%>%filter(Generation==i,A=="1.0",Level=="10",M=="4%")%>%
    pull(DivFA)
  D10NS[count,2]<-cor(Temp2,Temp, method=c("pearson"))
  D10NS[count,3]<-cor(Temp2,Temp, method=c("spearman"))
  count=count+1
}
# * 24.m. Putting the correlations all together ----------------------------------
#Different Levels parameter values
#DNS<-rbind(D25NS,D05NS,D75NS,D1NS,D3NS,D4NS,D5NS,D6NS,D7NS,D8NS,D9NS,D10NS)
#write_csv(DNS,"Rsquares_NS_large-4%.csv")
# *24.n. Making Figure S15. plot ---------------------------------------------------------
#Reading in data if analysis stage is skipped.
DNS_1<-read.csv("RSQ/Rsquares_NS_large_1m.csv")%>%
  mutate(M="1%")
DNS_2<-read.csv("RSQ/Rsquares_NS_large_2m.csv")%>%
  mutate(M="2%")
DNS_3<-read.csv("RSQ/Rsquares_NS_large_3m.csv")%>%
  mutate(M="3%")
DNS_4<-read.csv("RSQ/Rsquares_NS_large_4m.csv")%>%
  mutate(M="4%")
DNS<-rbind(DNS_1,DNS_2,DNS_3,DNS_4)%>%
  mutate(M=factor(M,levels=c("5%","4%","3%","2%","1%")))

(corNS<-DNS%>%
    #mutate(M="1%")%>%
    mutate(A="1.0")%>%
    mutate(Cat="NS")%>%
    mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))%>%
    ggplot(aes(x=Gen,y=P))+
    geom_hline(aes(yintercept=0),alpha=0.5,linetype="dashed")+
    geom_line(show.legend = F)+
    facet_nested(M~Cat+Level,labeller = labeller(A = A.labs,Cat=C.labs,M=M.labs))+
    xlab("Generation used for divergence in cryptic preference")+
    ylab("Correlation between cryptic preference divergence  \n and neutral loci divergence in final generation")+
    theme(text=element_text(size=16))+
    scale_x_continuous(breaks=c(0,1500,3000))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

#ggsave("FigureS9.pdf",height = 1.1*183,width=(1.5*183),units="mm")
# 25. Figure S16 ----------------------------------------------------------
(FigureS16<-NsumFinalDiv42%>%
   mutate(Div=factor(Div,levels=c("2","2HV","4","4HV")))%>%
   filter(M%in%c("1%","2%","3%"))%>%
   filter(A=="1.0")%>%
   mutate(M=factor(M,levels=c("5%","4%","3%","2%","1%")))%>%
   filter(A=="1.0")%>%
   ggplot(aes(x = Level, y = DivFA_mean,color= Cat,group=Cat)) + geom_vline(aes(xintercept=4),alpha=0.4,size=1)+
   geom_point()+
   geom_errorbar(aes(ymin=DivFA_mean-DivFA_sd/sqrt(50)*2,ymax=DivFA_mean+DivFA_sd/sqrt(50)*2))+
   geom_line()+
   facet_nested(
     M~Div,
     labeller = labeller(
       A=A.labs,
       Cat = C.labs,
       Div=D.labs,
       M=M.labs
     )
   )+
   scale_color_manual(values=palz,labels=c("Scenario 1: No Eco. Div.; Pref. Div.","Scenario 2: Eco Div.; No Pref. Div.","Scenario 3:Eco. Div.; Pref. Div."),name="")+
   theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "top") +
   scale_x_discrete(labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10"))+
   ylab("Divergence in cryptic preferences (f)")+
   xlab("Risk (qr) or intensity (qi) of sperm competition") )
#ggsave("FigureS16.pdf",height = 1*183,width=(1.5*183),units="mm")
# 26. Figure S17. Overtime Divergence High variation runs -------------------------------------------
RNS_HV<-readRDS("Processed_Data_4H/NS_L_runs.rds")%>%
  filter(Generation<5000)%>%
  filter(Tradeoff=="false")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Reading in data for when there is initially no divergent cryptic preferences, large population size and intensities of sperm competition
RNS_I_HV<-readRDS("Processed_Data_4H/NS_I_L_runs.rds")%>% 
  filter(Generation<5000)%>%
  filter(Tradeoff=="false")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Putting them together
RNS_all_HV<-rbind(RNS_HV,RNS_I_HV)%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level))%>%
  mutate(Level=recode(Level,"1.0"="1.0/2"))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))

S17a<-RNS_all_HV%>%
  filter(M%in%c("1%"),A=="1.0")%>%
  ggplot(aes(group=Rep,x=Generation,y=DivFA))+
  #scale_color_manual(values=pal2)+
  geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5)+
  #geom_smooth(se=F)+
  geom_path(alpha=0.3,show.legend = F)+
  theme(panel.border = element_rect(color="black",fill=NA))+
  theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_nested(.~Level,labeller = labeller(A = A.labs,M=M.labs))+
  scale_x_continuous(breaks=c(0,2500,5000))+
  ylab("Divergence in \n cryptic preferences (f)")+
  xlab("Generation")

S17b<-RNS_all_HV%>%
  filter(M%in%c("1%"),A=="1.0")%>%
  filter(Generation<5000)%>%
  ggplot(aes(group=Rep,x=Generation,y=DivBPA))+
  #scale_color_manual(values=pal2)+
  geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5)+
  #geom_smooth(se=F)+
  geom_path(alpha=0.3,show.legend = F)+
  theme(panel.border = element_rect(color="black",fill=NA))+
  theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_nested(.~Level,labeller = labeller(A = A.labs,M=M.labs))+
  scale_x_continuous(breaks=c(0,2500,5000))+
  ylab("Divergence in \n neutral loci")+
  xlab("Generation")
(S17a/S17b)+plot_annotation(tag_levels = "A",tag_suffix = ")")

#ggsave("FigureS17.pdf",height = 0.75*183,width=(1.7*183),units="mm")
# 27. Figure S18 ----------------------------------------------------------
# *27.b. NoE (Scenario 2): 2SD divergence ---------------------------------------------------
RNS2_2<-readRDS("Processed_Data_2/NS_2_runs.rds")%>%
  filter(Tradeoff=="false")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))
#Reading in data for when there is initially no divergent cryptic preferences, large population size and intensities of sperm competition
RNS_I_2<-readRDS("Processed_Data_2/NS_I_2_runs.rds")%>% 
  filter(Tradeoff=="false")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Putting them together
RNS_all_2<-rbind(RNS2_2,RNS_I_2)%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level))%>%
  mutate(Level=recode(Level,"1.0"="1.0/2"))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))

#Pivoting data to show different sources of selection
#Sexual selection
RNS2O_2<-RNS_all_2%>%
  mutate(AF_SonOff_Beta=abs(F_SonOff_Beta_b-F_SonOff_Beta_a))%>%
  select(Cat,A,An,M,Level,Tradeoff,Generation,AF_SonOff_Beta,PopSize)%>%
  group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
  summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
  #filter(M%in%c("1%","4%"),Level%in%c("0.25","0.75","1.0/2","3","5","7","9"))%>%
  mutate(Type="Sexual selection")


#Natural Selection
RNS2S_2<-RNS_all_2%>%
  #mutate(AF_SonOff_Beta=abs(F_SonOff_Beta_b-F_SonOff_Beta_a))%>%
  mutate(AF_SurvOff_Beta=abs(F_SurvOff_Beta_b-F_SurvOff_Beta_a))%>%
  select(Cat,A,An,M,Level,Tradeoff,Generation,AF_SurvOff_Beta,PopSize)%>%
  group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
  summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
  # filter(M%in%c("1%","4%"),Level%in%c("0.25","0.75","1.0/2","3","5","7","9"))%>%
  mutate(Type="Viability selection")
# *27.c. NoE (Scenario 2): 4SD divergence ---------------------------------------------------
RNS2_4<-readRDS("Processed_Data_4/NS_runs.rds")%>%
  filter(Tradeoff=="false")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))
#Reading in data for when there is initially no divergent cryptic preferences, large population size and intensities of sperm competition
RNS_I_4<-readRDS("Processed_Data_4/NS_I_runs.rds")%>% 
  filter(Tradeoff=="false")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Putting them together
RNS_all_4<-rbind(RNS2_4,RNS_I_4)%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level))%>%
  mutate(Level=recode(Level,"1.0"="1.0/2"))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))

#Pivoting data to show different sources of selection
#Sexual selection
RNS2O_4<-RNS_all_4%>%
  mutate(AF_SonOff_Beta=abs(F_SonOff_Beta_b-F_SonOff_Beta_a))%>%
  select(Cat,A,An,M,Level,Tradeoff,Generation,AF_SonOff_Beta,PopSize)%>%
  group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
  summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
  #filter(M%in%c("1%","4%"),Level%in%c("0.25","0.75","1.0/2","3","5","7","9"))%>%
  mutate(Type="Sexual selection")


#Natural Selection
RNS2S_4<-RNS_all_4%>%
  #mutate(AF_SonOff_Beta=abs(F_SonOff_Beta_b-F_SonOff_Beta_a))%>%
  mutate(AF_SurvOff_Beta=abs(F_SurvOff_Beta_b-F_SurvOff_Beta_a))%>%
  select(Cat,A,An,M,Level,Tradeoff,Generation,AF_SurvOff_Beta,PopSize)%>%
  group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
  summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
  # filter(M%in%c("1%","4%"),Level%in%c("0.25","0.75","1.0/2","3","5","7","9"))%>%
  mutate(Type="Viability selection")

# *27.c. EE (Scenario 3); 2SD divergence ---------------------------------------------------
REE2_2<-readRDS("Processed_Data_2/EE_2_runs.rds")%>%
  filter(Tradeoff=="false")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))
#Reading in data for when there is initially no divergent cryptic preferences, large population size and intensities of sperm competition
REE_I_2<-readRDS("Processed_Data_2/EE_I_2_runs.rds")%>% 
  filter(Tradeoff=="false")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Putting them together
REE_all_2<-rbind(REE2_2,REE_I_2)%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level))%>%
  mutate(Level=recode(Level,"1.0"="1.0/2"))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))

#Pivoting data to show different sources of selection
#Sexual selection
REE2O_2<-REE_all_2%>%
  mutate(AF_SonOff_Beta=abs(F_SonOff_Beta_b-F_SonOff_Beta_a))%>%
  select(Cat,A,An,M,Level,Tradeoff,Generation,AF_SonOff_Beta,PopSize)%>%
  group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
  summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
  #filter(M%in%c("1%","4%"),Level%in%c("0.25","0.75","1.0/2","3","5","7","9"))%>%
  mutate(Type="Sexual selection")


#Natural Selection
REE2S_2<-REE_all_2%>%
  #mutate(AF_SonOff_Beta=abs(F_SonOff_Beta_b-F_SonOff_Beta_a))%>%
  mutate(AF_SurvOff_Beta=abs(F_SurvOff_Beta_b-F_SurvOff_Beta_a))%>%
  select(Cat,A,An,M,Level,Tradeoff,Generation,AF_SurvOff_Beta,PopSize)%>%
  group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
  summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
  # filter(M%in%c("1%","4%"),Level%in%c("0.25","0.75","1.0/2","3","5","7","9"))%>%
  mutate(Type="Viability selection")
# *27.d. EE (Scenario 3); 4SD divergence ---------------------------------------------------
REE2_4<-readRDS("Processed_Data_4/EE_runs.rds")%>%
  filter(Tradeoff=="false")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))
#Reading in data for when there is initially no divergent cryptic preferences, large population size and intensities of sperm competition
REE_I_4<-readRDS("Processed_Data_4/EE_I_runs.rds")%>% 
  filter(Tradeoff=="false")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Putting them together
REE_all_4<-rbind(REE2_4,REE_I_4)%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level))%>%
  mutate(Level=recode(Level,"1.0"="1.0/2"))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))

#Pivoting data to show different sources of selection
#Sexual selection
REE2O_4<-REE_all_4%>%
  mutate(AF_SonOff_Beta=abs(F_SonOff_Beta_b-F_SonOff_Beta_a))%>%
  select(Cat,A,An,M,Level,Tradeoff,Generation,AF_SonOff_Beta,PopSize)%>%
  group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
  summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
  #filter(M%in%c("1%","4%"),Level%in%c("0.25","0.75","1.0/2","3","5","7","9"))%>%
  mutate(Type="Sexual selection")


#Natural Selection
REE2S_4<-REE_all_4%>%
  #mutate(AF_SonOff_Beta=abs(F_SonOff_Beta_b-F_SonOff_Beta_a))%>%
  mutate(AF_SurvOff_Beta=abs(F_SurvOff_Beta_b-F_SurvOff_Beta_a))%>%
  select(Cat,A,An,M,Level,Tradeoff,Generation,AF_SurvOff_Beta,PopSize)%>%
  group_by(Generation,Cat,A,An,M,Level,Tradeoff,PopSize)%>%
  summarise_all(funs(mean = mean,sd = sd), na.rm = T)%>%
  # filter(M%in%c("1%","4%"),Level%in%c("0.25","0.75","1.0/2","3","5","7","9"))%>%
  mutate(Type="Viability selection")
#putting it all together
RNS_div2<-rbind(RNS2S_2,RNS2O_2)
RNS_div4<-rbind(RNS2S_4,RNS2O_4)
EE_div2<-rbind(REE2S_2,REE2O_2)
EE_div4<-rbind(REE2S_4,REE2O_4)
S_alldiv<-rbind(RNS_div2%>%mutate(Div="2"),RNS_div4%>%mutate(Div="4"),EE_div4%>%mutate(Div="4",Cat="EE"),EE_div2%>%mutate(Cat="EE",Div="2"))

# 5A Selection strength
(Fs18a<-S_alldiv%>%
    filter(M%in%c("1%","3%"),A%in%c("1.0"),Level%in%c("0.25","1.0/2","3","4","5","7","9"))%>%
    mutate(M=factor(M,levels=c("3%","1%")))%>%
    filter(Generation<1000)%>%
    mutate(Cat=factor(Cat,levels=c("NS","EE")))%>%
    ggplot(aes(x=Generation,y=mean,color=Level,fill=Level))+
    geom_hline(aes(yintercept=0),alpha=0.4,linetype="dashed",size=1)+
    geom_line(size=1)+
    geom_ribbon(
      aes(
        ymin = mean - sd/sqrt(50)*2,
        ymax = mean + sd/sqrt(50)*2,
        fill=Level),
      alpha = 0.3,
      color = NA
    )+
    facet_nested(Cat+M~Div+Type,labeller = labeller(A = A.labs,M=M.labs,Type=sel.labs,Cat=C.labs,Div=D.labs))+
    ylab("Divergent selection (b)\nacting on f")+
    xlab("Generation")+
    scale_color_manual(name = "Risk/intensity of sperm competition", values = pal)+
    scale_fill_manual(name = "Risk/intensity of sperm competition", values = pal)+
    theme(panel.border = element_rect(color="black",fill=NA),legend.position = "top",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+guides(fill=guide_legend(nrow=1),color=guide_legend(nrow=1)))

#ggsave("S18.pdf",height = 1.5*183,width=(1.25*183),units="mm")
# 28. High Starting Variation Supplemental Analysis loading up data ---------------------------
#Loading up summary files of Ecological divergence and Preference divergence (EE) for large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0)
EE_HV<-readRDS("Processed_Data_4H/EE_L_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of Ecological divergence and Preference divergence (EE) for small population size (2,000) and risks of sperm competition (0.25,0.5,0.75,1.0)
EE_S_HV<-readRDS("Processed_Data_4H/EE_small_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of Ecological divergence and Preference divergence (EE) for large population size (10,000) and intensities of sperm competition (2,3,4,5,6,7,8,9,10)
EE_I_HV<-readRDS("Processed_Data_4H/EE_I_L_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of Ecological divergence and Preference divergence (EE) for small population size (2,000) and intensities of sperm competition (2,3,4,5,6,7,8,9,10)
EE_S_I_HV<-readRDS("Processed_Data_4H/EE_I_small_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only preference divergence divergence (NoE) for large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0)

NoE_HV<-readRDS("Processed_Data_4H/NoE_L_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only preference divergence divergence (NoE) for small population size (2,000) and risks of sperm competition (0.25,0.5,0.75,1.0)

NoE_S_HV<-readRDS("Processed_Data_4H/NoE_small_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only preference divergence divergence (NoE) for large population size (10,000) and intensities of sperm competition (2-10)

NoE_I_HV<-readRDS("Processed_Data_4H/NoE_I_L_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only preference divergence divergence (NoE) for small population size (2,000) and intensities of sperm competition (2-10)

NoE_S_I_HV<-readRDS("Processed_Data_4H/NoE_I_small_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for large population size (10,000) and risks of sperm competition (0.25,0.5,0.75,1.0)

NS_HV<-readRDS("Processed_Data_4H/NS_L_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for small population size (2,000) and risks of sperm competition (0.25,0.5,0.75,1.0)

NS_S_HV<-readRDS("Processed_Data_4H/NS_small_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for large population size (10,000) and intensities of sperm competition (2-10)

NS_I_HV<-readRDS("Processed_Data_4H/NS_I_L_Sum.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))

#Loading up summary files of only ecological divergence and no preference divergence divergence (NS) for small population size (2,000) and intensities of sperm competition (2-10)

NS_S_I_HV<-readRDS("Processed_Data_4H/NS_I_small_Sum.rds")%>%
  mutate(PopSize="Small")%>%
  mutate(M=recode(M,"10"="1%","20"="2%","30"="3%","40"="4%","50"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))
#First merginig all data after filtering out for final generation.
#then reordering the different factors for plotting.
NsumFinal_HV<-rbind(
  EE_HV%>%filter(Generation==9999),
  EE_I_HV%>%filter(Generation==9999),
  EE_S_HV%>%filter(Generation==2999),
  EE_S_I_HV%>%filter(Generation==2999),
  NS_HV%>%filter(Generation==9999),
  NS_I_HV%>%filter(Generation==9999),
  NS_S_HV%>%filter(Generation==2999),
  NS_S_I_HV%>%filter(Generation==2999),
  NoE_HV%>%filter(Generation==9999),
  NoE_I_HV%>%filter(Generation==9999),
  NoE_S_HV%>%filter(Generation==2999),
  NoE_S_I_HV%>%filter(Generation==2999)
)%>%
  mutate(A=recode(A,"50" = "50.0"))%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0","3","4","5","6","7","8","9","10")))%>%
  mutate(PopSize=factor(PopSize,levels=c("Small","Large")))%>%
  mutate(Cat=factor(Cat,levels=c("NoE","NS","EE")))

# 29. Figure S19. Divergence All neutral loci heatmap ----------------------------------------------------------
#Neutral divergence across all sensitivity analyses.
ggplot(NsumFinal_HV%>%filter(), aes(x = Level, y = M, fill = DivBPA_mean)) +
  geom_tile(size = 0.05,color="grey") +
  facet_nested(
    Cat+PopSize~A,
    labeller = labeller(
      A=A.labs,
      Cat = C.labs,
      Tradeoff=T.labs,
      PopSize=P.labs
    )
  ) +
  scale_fill_distiller(
    # limits=c(NA,NA),
    #trans="log10",
    palette = "Blues",
    na.value = "grey",
    direction = 1,
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black",frame.linewidth=0.5
    ))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0),labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")) +
  labs(y = "Migration rate", x = "Risk (qr) or intensity (qi) of sperm competition", fill =
         "Divergence in \n neutral loci")+
  theme(legend.position = "left")+
  geom_vline(aes(xintercept=4),size=1)+
  theme(panel.border = element_rect(color="black",fill=NA))+
  theme(text=element_text(size=20))
#ggsave("Figure_S19.pdf",height = 1.4*183,width=(1.8*183),units="mm")
# 30. Figure S20. Divergence in preferences ---------------------------------------------------------
#Figure S4 is similar plot as figure 3 but showing all the sensitivity analyses.
ggplot(NsumFinal_HV%>%filter(), aes(x = Level, y = M, fill = DivFA_mean)) +
  geom_tile(size = 0.05,color="grey") +
  facet_nested(
    Cat+PopSize~A,
    labeller = labeller(
      A=A.labs,
      Cat = C.labs,
      Tradeoff=T.labs,
      PopSize=P.labs
    )
  )  +
  scale_fill_distiller(
    palette = "BuPu",
    na.value = "grey",
    direction = 1,
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black",frame.linewidth=0.5
    ))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0),labels= c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")) +
  labs(y = "Migration rate (m)", x = "Risk (qr) or intensity (qi) of sperm competition", fill =
         "Divergence in \n female preferences")+
  theme(legend.position = "left")+
  geom_vline(aes(xintercept=4.5))+
  theme(panel.border = element_rect(color="black",fill=NA))
#ggsave("Figure_S20_HV.pdf",height = 1.4*183,width=(1.8*183),units="mm")
# 31. Figure S21: 4SD vs 2SD divergence -----------------------------------
RNS_2HV<-readRDS("Processed_Data_2H/NS_2_H_runs.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="2HV")

RNS_2HV_I<-readRDS("Processed_Data_2H/NS_I_2_H_runs.rds")%>%
  mutate(PopSize="Large")%>%
  mutate(M=recode(M,"50"="1%","100"="2%","150"="3%","200"="4%","250"="5%"))%>%
  mutate(M=factor(M,levels=c("1%","2%","3%","4%","5%")))%>%
  mutate(Div="2HV")

RNS_all_HV_24<-rbind(RNS_2HV,RNS_2HV_I,RNS_all_HV%>%mutate(Div="4HV"))%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level))%>%
  mutate(Level=recode(Level,"1.0"="1.0/2"))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0/2","3","4","5","6","7","8","9","10")))

(S21a<-RNS_all_HV_24%>%
    mutate(Rep2=paste(Rep,Div))%>%
    filter(A=="1.0",M=="3%")%>%
    ggplot(aes(group=Rep2,x=Generation,y=Mean.E_a-40,color=Div))+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5,size=1)+
    geom_hline(aes(yintercept=20),linetype="dashed",alpha=0.5,size=1,color="#F8766D")+
    geom_hline(aes(yintercept=40),linetype="dashed",alpha=0.5,size=1,color="#00BFC4")+
    geom_path(alpha=0.2)+
    theme(panel.border = element_rect(color="black",fill=NA))+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "top",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x=element_blank(), 
          axis.ticks.x=element_blank())+
    facet_nested(.~Cat+Level,labeller = labeller(A = A.labs,M=M.labs,Div=D.labs,Cat=C.labs))+
    ylab("Deviation from\necological optimum")+
    xlab("")+
    scale_color_manual(values=c("#F8766D","#00BFC4"),labels=c("Mean Div. = 2SD\nWithin Pop. SD = 10","Mean Div. = 4SD\nWithin Pop. SD = 10"),name="")+
    guides(color = guide_legend(override.aes = list(shape=15, size = 5,alpha=1))))



#Make panel B just showing the divergence of neutral loci
(S21b<-RNS_all_HV_24%>%
    mutate(Rep2=paste(Rep,Div))%>%
    filter(A=="1.0",M=="3%")%>%
    ggplot(aes(group=Rep2,x=Generation,y=Surv_Tot_Mig_a,color=Div))+
    theme(panel.border = element_rect(color="black",fill=NA),strip.background = element_blank(),
          strip.text.x = element_blank())+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
    geom_path(alpha=0.3,show.legend = F)+
    facet_nested(.~Cat+Level,labeller = labeller(A = A.labs,M=M.labs,Div=D.labs))+
    ylab("Surviving migrants"))

(S21a/S21b)+plot_annotation(tag_levels = "A",tag_suffix = ")")
#ggsave("FigureS21.pdf",height = 1*183,width=(1.5*183),units="mm")
# 32. Figure S22. Example High Variation populations ----------------------------------------------------------------
(S22a<-RNS_all_HV%>%
   filter(Rep%in%c(27,46))%>%
   filter(M%in%c("1%"),A=="1.0",Level%in%c("10"))%>%
   select(Rep,Level,Generation,Mean.Female_a,Mean.Female_b)%>%
   pivot_longer(cols=Mean.Female_a:Mean.Female_b,names_to ="Population" ,values_to = "Male",names_prefix = "Mean.Male_")%>%
   mutate(Group=paste(Rep,Population))%>%
   filter(Generation<3000)%>%
   ggplot(aes(group=Group,x=Generation,y=Male,linetype=Population))+
   # scale_color_brewer(palette = "Dark2")+
   #scale_color_manual(values=pal)+
   geom_hline(aes(yintercept=0),linetype="dashed")+
   #geom_smooth(se=F)+
   geom_path(show.legend = F,size=1)+
   theme(panel.border = element_rect(color="black",fill=NA))+
   theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
   facet_nested(.~Rep,labeller = labeller(A = A.labs,M=M.labs))+
   ylab("Mean preference (f)")+
   xlab("Generation"))

(S22b<-RNS_all_HV%>%
    filter(Rep%in%c(27,46))%>%
    filter(M%in%c("1%"),A=="1.0",Level%in%c("10"))%>%
    select(Rep,Level,Generation,SD.Female_a,SD.Female_b)%>%
    pivot_longer(cols=SD.Female_a:SD.Female_b,names_to ="Population" ,values_to = "Male",names_prefix = "SD.Male_")%>%
    mutate(Group=paste(Rep,Population))%>%
    filter(Generation<3000)%>%
    ggplot(aes(group=Group,x=Generation,y=Male,linetype=Population))+
    #scale_color_manual(values=pal2)+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5)+
    #geom_smooth(se=F)+
    geom_path(show.legend = F,size=1)+
    theme(panel.border = element_rect(color="black",fill=NA))+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_nested(.~Rep,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("SD preference (f)")+
    xlab("Generation"))


(S22c<-RNS_all_HV%>%
    filter(Rep%in%c(27,46))%>%
    filter(M%in%c("1%"),A=="1.0",Level%in%c("10"))%>%
    select(Rep,Level,Generation,cor.mf_a,cor.mf_b)%>%
    pivot_longer(cols=cor.mf_a:cor.mf_b,names_to ="Population" ,values_to = "Male",names_prefix = "SD.Male_")%>%
    mutate(Group=paste(Rep,Population))%>%
    filter(Generation<3000)%>%
    ggplot(aes(group=Group,x=Generation,y=Male,linetype=Population))+
    #scale_color_manual(values=pal2)+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5)+
    #geom_smooth(se=F)+
    geom_path(show.legend = F,size=1)+
    theme(panel.border = element_rect(color="black",fill=NA))+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_nested(.~Rep,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("Correlation between f and t")+
    xlab("Generation"))


(S22d<-RNS_all_HV%>%
    filter(Rep%in%c(27,46))%>%
    filter(M%in%c("1%"),A=="1.0",Level%in%c("10"))%>%
    select(Rep,Level,Generation,Mean.Male_a,Mean.Male_b)%>%
    pivot_longer(cols=Mean.Male_a:Mean.Male_b,names_to ="Population" ,values_to = "Male",names_prefix = "Mean.Male_")%>%
    mutate(Group=paste(Rep,Population))%>%
    filter(Generation<3000)%>%
    ggplot(aes(group=Group,x=Generation,y=Male,linetype=Population))+
    # scale_color_brewer(palette = "Dark2")+
    #scale_color_manual(values=pal)+
    geom_hline(aes(yintercept=0),linetype="dashed")+
    #geom_smooth(se=F)+
    geom_path(show.legend = F,size=1)+
    theme(panel.border = element_rect(color="black",fill=NA))+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_nested(.~Rep,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("Mean sperm trait (t)")+
    xlab("Generation"))

(S22e<-RNS_all_HV%>%
    filter(Rep%in%c(27,46))%>%
    filter(M%in%c("1%"),A=="1.0",Level%in%c("10"))%>%
    select(Rep,Level,Generation,SD.Male_a,SD.Male_b)%>%
    pivot_longer(cols=SD.Male_a:SD.Male_b,names_to ="Population" ,values_to = "Male",names_prefix = "SD.Male_")%>%
    mutate(Group=paste(Rep,Population))%>%
    filter(Generation<3000)%>%
    ggplot(aes(group=Group,x=Generation,y=Male,linetype=Population))+
    #scale_color_manual(values=pal2)+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5)+
    #geom_smooth(se=F)+
    geom_path(show.legend = F,size=1)+
    theme(panel.border = element_rect(color="black",fill=NA))+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_nested(.~Rep,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("SD sperm trait (t)")+
    xlab("Generation"))

(S22f<-RNS_all_HV%>%
    filter(Rep%in%c(27,46))%>%
    filter(M%in%c("1%"),A=="1.0",Level%in%c("10"))%>%
    select(Rep,Level,Generation,M_Off_Mig_a,M_Off_Mig_b)%>%
    pivot_longer(cols=M_Off_Mig_a:M_Off_Mig_b,names_to ="Population" ,values_to = "Male",names_prefix = "M_Off_Mig_")%>%
    mutate(Group=paste(Rep,Population))%>%
    filter(Generation<3000)%>%
    ggplot(aes(group=Group,x=Generation,y=Male,linetype=Population))+
    #scale_color_manual(values=pal2)+
    #scale_color_manual(values=pal)+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5)+
    #geom_smooth(se=F)+
    geom_path(show.legend = F,size=1)+
    theme(panel.border = element_rect(color="black",fill=NA))+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_nested(.~Rep,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("Migrant male fitness")+
    xlab("Generation"))

(S22g<-RNS_all_HV%>%
    filter(Rep%in%c(27,46))%>%
    filter(M%in%c("1%"),A=="1.0",Level%in%c("10"))%>%
    select(Rep,Level,Generation,Mean.E_a,Mean.E_b)%>%
    pivot_longer(cols=Mean.E_a:Mean.E_b,names_to ="Population" ,values_to = "Male",names_prefix = "Mean.E_")%>%
    mutate(Group=paste(Rep,Population))%>%
    filter(Generation<3000)%>%
    ggplot(aes(group=Group,x=Generation,y=Male,linetype=Population))+
    # scale_color_brewer(palette = "Dark2")+
    #scale_color_manual(values=pal)+
    geom_hline(aes(yintercept=0),linetype="dashed")+
    #geom_smooth(se=F)+
    geom_path(show.legend = F,size=1)+
    theme(panel.border = element_rect(color="black",fill=NA))+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_nested(.~Rep,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("Mean ecological trait (e)")+
    xlab("Generation"))

(S22h<-RNS_all_HV%>%
    filter(Rep%in%c(27,46))%>%
    filter(M%in%c("1%"),A=="1.0",Level%in%c("10"))%>%
    select(Rep,Level,Generation,SD.E_a,SD.E_b)%>%
    pivot_longer(cols=SD.E_a:SD.E_b,names_to ="Population" ,values_to = "Male",names_prefix = "Surv_Tot_Mig_")%>%
    mutate(Group=paste(Rep,Population))%>%
    filter(Generation<3000)%>%
    ggplot(aes(group=Group,x=Generation,y=Male,linetype=Population))+
    #scale_color_manual(values=pal2)+
    #scale_color_manual(values=pal)+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5)+
    #geom_smooth(se=F)+
    geom_path(show.legend = F,size=1)+
    theme(panel.border = element_rect(color="black",fill=NA))+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_nested(.~Rep,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("SD ecological trait (e)")+
    xlab("Generation"))

(S22i<-RNS_all_HV%>%
    filter(Rep%in%c(27,46))%>%
    filter(M%in%c("1%"),A=="1.0",Level%in%c("10"))%>%
    select(Rep,Level,Generation,Surv_Tot_Mig_a,Surv_Tot_Mig_b)%>%
    pivot_longer(cols=Surv_Tot_Mig_a:Surv_Tot_Mig_b,names_to ="Population" ,values_to = "Male",names_prefix = "Surv_Tot_Mig_")%>%
    mutate(Group=paste(Rep,Population))%>%
    filter(Generation<3000)%>%
    ggplot(aes(group=Group,x=Generation,y=Male,linetype=Population))+
    #scale_color_manual(values=pal2)+
    #scale_color_manual(values=pal)+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=0.5)+
    #geom_smooth(se=F)+
    geom_path(show.legend = F,size=1)+
    theme(panel.border = element_rect(color="black",fill=NA))+
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),legend.position = "none",legend.background = element_rect(fill=NA),legend.text = element_text(size=11),legend.title = element_text(size=12),legend.title.align=0.5,legend.box.background = element_rect(colour = "black",linetype = "dashed"),legend.key = element_rect(color = "black"),axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_nested(.~Rep,labeller = labeller(A = A.labs,M=M.labs))+
    ylab("Suviving migrants")+
    xlab("Generation"))

(((S22a+S22b+S22c)/(S22d+S22e+S22f)/(S22g+S22h+S22i))&theme(text=element_text(size=18)))+plot_annotation(tag_levels = "A",tag_suffix = ")")

#ggsave("FigureS22.pdf",height = 1.6*183,width=(2*183),units="mm")

# 33. Figure S23,S24 ----------------------------------------------------------
#Putting data set together
Allsum<-rbind(
  EE,
  EE_I,
  EE_S,
  EE_S_I,
  NS,
  NS_I,
  NS_S,
  NS_S_I,
  NoE,
  NoE_I,
  NoE_S,
  NoE_S_I
)%>%
  mutate(A=recode(A,"50" = "50.0"))%>%
  mutate(A=factor(A,levels=c("50.0","12.5","1.0")))%>%
  mutate(Level=factor(Level,levels=c("0.25","0.5","0.75","1.0","3","4","5","6","7","8","9","10")))%>%
  mutate(PopSize=factor(PopSize,levels=c("Small","Large")))%>%
  mutate(Cat=factor(Cat,levels=c("NoE","NS","EE")))

#Making new pallete to match colors of other figures
#Migration rate 1% Figure S23
ggplot(Allsum%>%filter(M=="1%",Tradeoff=="false",A%in%c("50.0","1.0"))%>%
         filter(), aes(x = Generation, y =Mean.Sperm_a_mean)) +
  geom_line(size=1)+
  geom_ribbon(
    aes(
      ymin = Mean.Sperm_a_mean - Mean.Sperm_a_sd/sqrt(50)*2,
      ymax = Mean.Sperm_a_mean + Mean.Sperm_a_sd/sqrt(50)*2),
    alpha = 0.3,
    color = NA
  )+
  ylab("Mean sperm number (s)")+
  xlab("Generation")+
  scale_x_continuous(breaks=c(0,1500,3000))+
  theme(panel.border = element_rect(color="black",fill=NA),legend.position ="none",axis.text.x=element_text(angle=45,hjust = 1))+guides(fill=guide_legend(ncol=2),color=guide_legend(ncol=2))+
  facet_nested(
    Cat+PopSize~A+Level,
    labeller = labeller(
      A=A.labs,
      Cat = C.labs,
      Tradeoff=T.labs,
      PopSize=P.labs
    )
  ) 
#ggsave("Figure_S23.png",height = 1.5*183,width=(3*183),units="mm")

#Migration rate 5% Figure S24
ggplot(Allsum%>%filter(M=="5%",Tradeoff=="false",A%in%c("50.0","1.0"))%>%
         filter(), aes(x = Generation, y =Mean.Sperm_a_mean)) +
  geom_line(size=1)+
  geom_ribbon(
    aes(
      ymin = Mean.Sperm_a_mean - Mean.Sperm_a_sd/sqrt(50)*2,
      ymax = Mean.Sperm_a_mean + Mean.Sperm_a_sd/sqrt(50)*2),
    alpha = 0.3,
    color = NA
  )+
  ylab("Mean sperm number (s)")+
  xlab("Generation")+
  scale_x_continuous(breaks=c(0,1500,3000))+
  theme(panel.border = element_rect(color="black",fill=NA),legend.position ="none",axis.text.x=element_text(angle=45,hjust = 1))+guides(fill=guide_legend(ncol=2),color=guide_legend(ncol=2))+
  facet_nested(
    Cat+PopSize~A+Level,
    labeller = labeller(
      A=A.labs,
      Cat = C.labs,
      Tradeoff=T.labs,
      PopSize=P.labs
    )
  ) 
#ggsave("Figure_S24.png",height = 1.5*183,width=(3*183),units="mm")
