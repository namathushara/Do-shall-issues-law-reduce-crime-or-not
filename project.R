library(ggplot2)
library(data.table)
library(foreign)
library(panelView)
library(corrplot)
library(scatterplot3d)
library(lmtest)
library(car)
library(plm)
library(sandwich)

data=read.dta("C:/Users/rahul/Desktop/thussu/Econ/dat/guns.dta")
View(data)
summary(data)
str(data)
dim(data)

panelView( log(vio)~ incarc_rate + pop + avginc+density+shall, 
          data = data, index = c("stateid","year"), 
          xlab = "Year", ylab = "State" ,by.timing = TRUE, background = "white", 
          cex.main = 20, cex.axis= 8, cex.lab = 12, cex.legend = 12)
corr<-cor(data)
corrplot(corr,method='pie')

boxplot(log(vio)~year,data=data, main="Violent Crime rate across years",
        xlab="Year", ylab="log Violent Crime rate",col='gold')
hist(data$vio,col='steelblue',xlab='crime rate',ylab='frequency',main='Violent crime rate')
hist(log(data$vio),col='steelblue',xlab='crime rate',ylab='frequency',main='Violent crime rate')



scatterplot3d(data$incarc_rate,data$year,log(data$vio),
              main="crimerate with incarcrate over years",
              xlab='incarc_rate',zlab='crime rate',ylab='year',
              color="steelblue")
scatterplot3d(data$avginc,data$year,log(data$vio),
              main="crimerate with avg_income over years",
              xlab='Avg Income',zlab='crime rate',ylab='year',
              color="steelblue")
hist(data$pop,col='orange')
hist(data$avginc,col='orange')
hist(data$shall,col='orange',xlab='law',ylab='frequency',main='shall carry Law')
hist(data$density,col='orange',xlab='population',ylab='frequency',main='Population per square mile')
hist(data$incarc_rate,col='orange',xlab='incarc_rate',ylab='frequency',main='Incarcenation rate')
hist(data$pm1029,col='orange',xlab='Male population',ylab='frequency',main='Percentage of Male population')
plot(data$year,data$shall)

boxplot(log(vio)~shall,data=data, main="Crime rate vs Shall carry law",
        xlab="shall", ylab="Violent crime rate",col='gold')


ggplot(data, aes(x = log(vio))) +
  geom_point(aes(y=pb1064),col='black')+
  geom_point(aes(y=pw1064),col='red')+
  geom_point(aes(y=pm1029),col='steelblue')+ 
  labs(title='crimerate vs white,black,men population',x='crime rate',y='white black men population')

#heteroskedasticity
bptest(log(vio)~ incarc_rate + pop + avginc+density+shall+pb1064+
         pw1064+pm1029,data = data)
plot(lm(log(vio)~ incarc_rate + pop + avginc+density+shall+pb1064+
              pw1064+pm1029 ,data = data))
#pooled ols
model<-plm(log(vio)~ incarc_rate + pop + avginc+density+shall+pb1064+
        pw1064+pm1029 ,data = data,model='pooling',index = c("stateid","year"))
summary(model)

# hetero pooled ols
coeftest(model, vcov=vcovHC(model,type="HC1" ,cluster="group",sandwich = TRUE))  

#fixed effects
modelf<-plm(log(vio)~ incarc_rate + pop + avginc+density+shall+pb1064+
             pw1064+pm1029 ,data = data,model='within',index = "stateid")
summary(modelf)
coeftest(modelf, vcov=vcovHC(modelf,type="HC1" ,cluster="group",sandwich = TRUE))  
sum(modelf$residuals^2)
#random
modelr<-plm(log(vio)~ incarc_rate + pop + avginc+density+shall+pb1064+
              pw1064+pm1029 ,data = data,model='random',index = c("stateid","year"))
summary(modelr)

phtest(modelf,modelr)

data$fact<-factor(data$year)

modelft<-plm(log(vio)~ incarc_rate + pop + avginc+density+shall+pb1064+
              pw1064+pm1029+fact,data = data,model='within',index = "stateid")
summary(modelft)
sum(modelft$residuals^2)
coeftest(modelft, vcov=vcovHC(modelft,type="HC1" ,cluster="group",sandwich = TRUE))  

linearHypothesis(modelft, c('fact78=0','fact79=0','fact80=0','fact81=0',
                            'fact82=0','fact83=0','fact84=0','fact85=0',
                            'fact86=0','fact87=0','fact88=0','fact89=0',
                            'fact90=0','fact91=0','fact92=0','fact93=0',
                            'fact94=0','fact95=0','fact96=0','fact97=0',
                            'fact98=0','fact99=0'), white.adjust = "hc1")

levels(data$fact)
