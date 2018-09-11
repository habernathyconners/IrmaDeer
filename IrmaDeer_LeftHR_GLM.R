# this code brings in the seasonal home range data of Irma deer 
# and examines covariates that caused animals to leaves via GLM, 
# and teststhe model's GOF

# Authors: Abernathy-Conners et al. 

####################################################
# bring home range data 
####################################################
DeerHR<-read.csv("./IrmaDeer_LeftHR_HRCovars.csv")
head(DeerHR)

# animal ID = uniq_id
# sex = male or female animal
# velocity = speed animal was moving on the day of Irma
# max_dist = maximum distance animal travelled from it's
#seasonal home range on the day of Irma
#ud_size = the home range area (km2) per animal
# elev_mean = mean ASTER elevation within an animals seasonal homerange 
# elev_max = max ASTER elevation within an animals seasonal homerange 
# elev_min = min ASTER elevation within an animals seasonal homerange 
# pflat_area_prop = proportion of pine forest within an animals seasonal homerange 
#leftUD = 1 = an animal left it's seasonal home range during Irma, 0 = an animal did not leave it's
#seasonal home range during Irma. 

#scale covars as needed-
DeerHR$ud_size<-scale(DeerHR$ud_size)
DeerHR$elev_mean<-scale(DeerHR$elev_mean)
DeerHR$elev_min<-scale(DeerHR$elev_min)
DeerHR$elev_max<-scale(DeerHR$elev_max)
DeerHR$pflat_area_prop<-scale(DeerHR$pflat_area_prop)

#set the factors in your data -
DeerHR$sex<-as.factor(DeerHR$sex)
DeerHR$uniq_id<-as.factor(DeerHR$uniq_id)
DeerHR$leftUD<-as.factor(DeerHR$leftUD)

# examine covars to examine potential correlations
pearsonRbd <- cor(DeerHR[,6:10], method="pearson")
cor.mat <- pearsonRbd
cor.mat[cor.mat < 0.6] <- NA 
View(cor.mat)

# remove correlated variables 
DeerHR<-DeerHR[,-c(6,8)]

##############################
# GLM
##############################

g<-glm(leftUD ~ sex + ud_size + elev_max + pflat_area_prop,  data=DeerHR, family='binomial', na.action = "na.fail")
MuMIn::dredge(g, rank="AIC")

fm.best<-glm(leftUD ~ ud_size + pflat_area_prop+elev_max,  data=DeerHR, family='binomial')
summary(fm.best)

##############################
# GOF
##############################

h<-ResourceSelection::hoslem.test(top$y, top$fitted, g=10)
# X-squared = 3.1477, df = 8, p-value = 0.9247
# no evidence of poor fit 

