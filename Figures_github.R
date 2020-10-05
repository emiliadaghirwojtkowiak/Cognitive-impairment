########################
## DATA VISUALIZATION-M1 ##
########################

rm(list=ls())

#############################################
## Boxplot -centered and standardized data ##
#############################################

setwd("C:/dane/socha_edyta/dane przygotowane_ES/vector_danych");
dta <- read.csv("MAT1_DV_COV.csv")
dta <-na.omit(dta)

setwd("C:/dane/socha_edyta/dane przygotowane_ES/plots/plotsM1")
tiff("DV_boxplot_CaCo_M1.tiff", width = 10, height = 12, units = 'in', res = 300)

raw.fig <- ggplot(data = dta, aes(x=as.factor(casecont), y=DV)) + 
  geom_boxplot(fill='#A4A4A4', color="black") 
raw.fig + facet_wrap(~ name , ncol = 4) +
  labs(fill = "Patients", labels = c("A", "B"))+
  scale_fill_discrete(name = "Patients",
                      breaks=c("0", "1"),
                      labels=c("Mild cognitive impairment", "Moderate dementia") ) +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("0", "1")) +
  theme(plot.title = element_text(size = rel(7)),strip.text.x = element_text(size = 17))+
  theme(legend.title = element_text(colour="black", size=15, face="bold"))+
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text=element_text(size=17),
        axis.title=element_text(size=17,face="bold")) +
  geom_jitter(shape=16,size=1,  position=position_jitter(0.2))

###########################
# Histogram of the data ###
###########################

setwd("C:/dane/socha_edyta/dane przygotowane_ES/vector_danych");
dta <- read.csv("MAT2_DV_COV.csv")
mydata <-dta
tiff("DV_dens_M2.tiff", width = 10, height = 12, units = 'in', res = 300)


p <- ggplot(mydata, aes(DV)) +
  facet_wrap(~mydata$name , ncol = 4) +
  labs(x = "Centered and standardized aminoacids concentrations", y = "Density") +
  guides(colour = "none") +
  geom_density(alpha=.2, fill="#FF6666") +
  theme(plot.title = element_text(size = rel(7)),strip.text.x = element_text(size = 17))+
  theme(legend.title = element_text(colour="black", size=15, face="bold"))+
  theme(axis.title.x = element_text(colour = 'black', size = 17)) +
  theme(axis.title.y = element_text(colour = 'black', size = 17)) 

p

################################
## Data in a function of age ###
################################

tiff("DV_vs_AGE_M2.tiff", width = 10, height = 12, units = 'in', res = 300)

yvsage <- ggplot(mydata, aes(age,DV, colour = factor(mydata$casecont))) +
  scale_color_manual(values=c("blue", "red")) +
  geom_point(size = 2)+  geom_smooth(method = "loess", se = T ,colour = 'black') +
  labs(x = "AGE [years]", y = "Centered and standardized aminoacids concentrations") +
  facet_wrap(~ mydata$name , ncol = 4) +
  guides(colour = "none") + 
  theme(axis.text.x = element_text(colour = 'black', size = 15))+
  theme(axis.text.y = element_text(colour = 'black', size = 15)) +
  theme(axis.title.x = element_text(colour = 'black', size = 15))+
  theme(axis.title.y = element_text(colour = 'black', size = 15))+
  theme(plot.title = element_text(size = rel(5)),strip.text.x = element_text(size = 13.5))
yvsage + ylim(-2, 4) ;



###################################
# Peak distribution by casecont ##
###################################

tiff("DV_vs_CASECONT_M2.tiff", width = 10, height = 12, units = 'in', res = 300)

p <- ggplot(mydata, aes(DV, fill = factor(casecont, labels = c("Mild cognitive impairment ", "Moderate dementia ")))) +
  geom_density(alpha=.3) +
  theme(legend.position="top") +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  facet_wrap(~ mydata$name , ncol = 4) +
  labs(x = "Centered and standardized aminoacids' concentrations", y = "Density") +
  theme(plot.title = element_text(size = rel(7)),strip.text.x = element_text(size = 17))+
  theme(legend.title = element_text(colour="black", size=15, face="bold"))+
  theme(axis.title.x = element_text(colour = 'black', size = 17)) +
  theme(axis.title.y = element_text(colour = 'black', size = 17)) +
  
  labs(fill = "Group")

p 

#############################
# Peak distribution by sex ##
#############################

tiff("DV_vs_SEX_M2.tiff", width = 10, height = 12, units = 'in', res = 300)

mydata9 <- na.omit(mydata9)
s <- ggplot(mydata, aes(DV, fill = factor(sex, labels = c("Male", "Female")))) +
  geom_density(alpha=.3,na.rm = T) +
  theme(legend.position="top") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~ mydata$name , ncol = 4) +
  labs(x = "Centered and standardized aminoacids' concentrations", y = "Density") +
  theme(plot.title = element_text(size = rel(7)),strip.text.x = element_text(size = 17))+
  theme(legend.title = element_text(colour="black", size=15, face="bold"))+
  theme(axis.title.x = element_text(colour = 'black', size = 17)) +
  theme(axis.title.y = element_text(colour = 'black', size = 17)) +
  
  labs(fill = "Sex")
s 

