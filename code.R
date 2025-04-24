
rm(list=ls())

#Ρουτίνες που θα χρειαστώ

# function για έλεγχο υποθέσεων
check<-function(x){
  temp<-list()
  par(mfrow=c(2,3))
  
  plot(x, which=2, cex=1.5, col='blue') # κανονικότητα
  
  plot( x$fit, rstandard(x),col='blue',pch=16)
  abline(h=1.96, col='red', lwd=2, lty=2)
  abline(h=-1.96, col='red', lwd=2, lty=2)
  
  plot( x$fit, rstandard(x)^2  , ylim=c(0,4.2), pch=16, cex=2, col='blue')
  abline(h=1.96^2, col='red', lwd=2, lty=2) # residuals^2
  
  plot( x, which=5,pch=16, col='blue') # cooks distance
  
  plot( rstandard(x), type='l') # ανεξαρτησία
  acf(x$res) # acf
  
  temp[[1]]<-summary(x)
  temp[[2]]<-shapiro.test(rstandard(x))
  temp[[3]]<-lillie.test(rstandard(x))
  qfits <- quantcut( x$fit )
  temp[[4]]<-leveneTest(rstandard(x),qfits)
  temp[[5]]<-dwt(x, method='normal') # autocorrelation
  temp[[6]]<-vif(x) # multicolloniarity>10
  
  print(temp)
  
  par(mfrow=c(1,1))
}

quantcut <- function(x, digits=6){ cut( x, breaks=quantile(x),  include.lowest=TRUE, dig.lab = digits) }

getmode <- function(x) {
  tabx<-table(x)
  names(which.max(tabx))
}

#Πακέτα που θα χρειαστώ
pack<-c('sjmisc','foreign','psych','sjPlot','corrplot','gmodels',
        'gplots','lawstat','Hmisc','car','nortest',"PerformanceAnalytics",'gtools')

install.packages(pack)

library(gtools)
library(nortest)
library(lawstat)
library(Hmisc)
library(gplots)
library(gmodels)
library(sjPlot)
library(foreign)
library(sjmisc)
library(psych)
library(corrplot)
library(car)
library(PerformanceAnalytics)

# Παρουσιάστε τα κατάλληλα περιγραφικά μέτρα για τις ανωτέρω μεταβλητές 
# και εξετάστε τις μεταξύ τους σχέσεις.  

library(foreign)
#diabazw ta dedomena
data <- read.spss(file.choose(), to.data.frame=T)
attach(data)

View(data)
str(data)
view_df(data)
attributes(data)
#Έχω 4 κατηγορικές και 3 ποσοτικές

####	Περιγραφικά μέτρα για κάθε μεταβλητή	###
library(psych)
describe(data)

# Διατάξιμες. Μπορούμε να χρησιμόποιησουμε μετρα για ποσοτικες διότι εχώ πολλές
# κατηγορίες
frq(data[,-c(1:5)],out='v',  
    auto.grp = 10)
library(sjmisc)
descr(data[,-c(1:5)],
      show = c('mean','md','sd','range','skew'),
      out = 'v')
getmode(write)
getmode(math)
getmode(socst)

summary(data[,2:5]) # αναφορικες


#### Διαγράμματα  ####

# Βαθμολογιών

#write
boxplot(write,main='Boxplot of write',col = 'green')

hist(write,prob=TRUE,include.lowest = TRUE,border = 0,col='darkblue')
curve(dnorm(x,mean(write),sd(write)),add=TRUE,lwd=3,col=2)

#math
boxplot(math,main='Boxplot of math',col='red')

hist(math,prob=TRUE,include.lowest = TRUE,border = 0,col='darkblue')
curve(dnorm(x,mean(math),sd(math)),add=TRUE,lwd=3,col=2)

#socst
boxplot(socst,main='Boxplot of socst',col='cyan')

hist(socst,prob=TRUE,include.lowest = TRUE,border = 0,col='darkblue')
curve(dnorm(x,mean(socst),sd(socst)),add=TRUE,lwd=3,col=2)

# Διάγραμμα συσχέτισης
corrplot(cor(data[,-c(1,2:5)]),type='lower',order='AOE',method='ellipse')
#scatter matrix
pairs(data[,-c(1,2:5)],pch=16,col='cyan4',main='Scattermatrix',upper.panel=NULL )

# barplot κατηγορικών
par(mfrow=c(2,2))
barplot(table(genre),col = 2)
barplot(table(race),col = 3)
barplot(table(schtyp),col = 4)
barplot(table(prog),col = 5)
mtext('Barplots of categorical variables',
      side = 3,line = - 2,cex=1.5, outer = TRUE)
par(mfrow=c(1,1))

table(schtyp,prog)
table(schtyp,race)
table(schtyp,genre)
table(prog,genre)


plot(write, socst,pch=16, col=as.numeric(genre))
legend('topleft',pch=16,legend=levels(genre),
       col=1:length(levels(genre)),bty='n')

plot(math, math,pch=16, col=as.numeric(genre))
legend('topleft',pch=16,legend=levels(genre),
       col=1:length(levels(genre)),bty='n')

plot(socst, math,pch=16, col=as.numeric(schtyp))
legend('topleft',pch=16,legend=levels(schtyp),
       col=1:length(levels(schtyp)),bty='n')

boxplot(write~genre)
boxplot(write~race)




######  Σχέσεις ανα Δύο   #########

library(gmodels)
library(gplots)

#######   Πίνακες Συνάφειας   #############

## Schtyp ∼ Prog **

# έλεγχος ανεξαρτησίας του X^2

chisq.test(table(schtyp,prog)) # απορ

chisq.test(table(schtyp,prog))$ex
# Η προυπόθεση του χ^2 test ειναι οκ

fisher.test(table(schtyp,prog))

# απορρίπτω την υπόθεση οτι οι προτιμήσεις των παιδιών
# για το προγραμμα σπουδών δεν διαφέρει αναλόγως το 
# είδος του σχολείου που φοίτησε

CrossTable(schtyp, prog, digits=1, format='SPSS', 
           expected=FALSE, prop.r=TRUE, prop.c=F, prop.t=F, prop.chisq=F, chisq = T, fisher=T, mcnemar=FALSE)

## Race~Prog

# έλεγχος ανεξαρτησίας του X^2
chisq.test(table(race,prog)) #Δεν απορ αλλα δεν μπορω να το εμπιστευτω

chisq.test(table(race,prog))$ex
# Η προυπόθεση του χ^2 test δεν ειναι οκ

fisher.test(table(race,prog)) 
# δεν απορρίπτω την υπόθεση οτι οι προτιμήσεις των παιδιών
# για το προγραμμα σπουδών δεν διαφέρει αναλόγως την φυλη που ανήκουν

CrossTable(race, prog, digits=1, format='SPSS', 
           expected=FALSE, prop.r=TRUE, prop.c=F, prop.t=F, prop.chisq=F, chisq = T, fisher=T, mcnemar=FALSE)

## genre~race

# έλεγχος ανεξαρτησίας του X^2
chisq.test(table(race,genre)) #Δεν απορ

chisq.test(table(race,genre))$ex
# Η προυπόθεση του χ^2 test ειναι οριακα οκ

fisher.test(table(race,genre)) 
# δεν απορρίπτω την υπόθεση οτι τα ποσοστά της φυλής των παιδιών
# είναι τα ίδια για ανδρες και γυναικες

CrossTable(race,genre, digits=1, format='SPSS', 
           expected=FALSE, prop.r=TRUE, prop.c=F, prop.t=F, prop.chisq=F, chisq = T, fisher=T, mcnemar=FALSE)

barplot(table(genre,race),beside = TRUE,col=c('cyan','darkorange'),main = 'Barplot of each category')
legend('topleft',legend = c('male','female'),fill=c('cyan','darkorange'))
## genre~ schtyp

# έλεγχος ανεξαρτησίας του X^2
chisq.test(table(schtyp,genre)) #Δεν απορ

chisq.test(table(schtyp,genre))$ex
# Η προυπόθεση του χ^2 test ειναι  οκ

fisher.test(table(schtyp,genre)) 
# δεν απορρίπτω την υπόθεση οτι το ποσοστό μεταξύ ανδρών και γυναικών ειναι 
# ίδιο από ιδιωτικά και δημόσια σχολεία

CrossTable(schtyp,genre, digits=1, format='SPSS', 
           expected=FALSE, prop.r=TRUE, prop.c=F, prop.t=F, prop.chisq=F, chisq = T, fisher=T, mcnemar=FALSE)

## genre~ prog

# έλεγχος ανεξαρτησίας του X^2
chisq.test(table(prog,genre)) #Δεν απορ

chisq.test(table(prog,genre))$ex
# Η προυπόθεση του χ^2 test ειναι  οκ

fisher.test(table(prog,genre)) 
# δεν απορρίπτω την υπόθεση οτι το ποσοστό μεταξύ ανδρών και γυναικών ειναι 
# ίδιο για κάθε προγραμμα σπουδών

CrossTable(prog,genre, digits=1, format='SPSS', 
           expected=FALSE, prop.r=TRUE, prop.c=F, prop.t=F, prop.chisq=F, chisq = T, fisher=T, mcnemar=FALSE)

barplot(table(genre,prog),beside = TRUE,col=2:3,main = 'Barplot of each category')
legend('topleft',legend = c('male','female'),fill=2:3)

## schtyp ~ race

# έλεγχος ανεξαρτησίας του X^2
chisq.test(table(race,schtyp)) #Δεν απορ

chisq.test(table(race,schtyp))$ex
# Η προυπόθεση του χ^2 test ειναι δεν οκ

fisher.test(table(race,schtyp)) 
# δεν απορρίπτω την υπόθεση οτι το ποσοστό σε δημοσια και ιδιωτική ειναι 
# το ίδιο για κάθε φυλή 

CrossTable(race,schtyp, digits=1, format='SPSS', 
           expected=FALSE, prop.r=TRUE, prop.c=F, prop.t=F, prop.chisq=F, chisq = T,
           fisher=T, mcnemar=FALSE)

barplot(table(schtyp,race),beside = TRUE,col=c('darkblue','darkorange'),main = 'Barplot of each category')
legend('topleft',legend = c('public','private'),fill=c('darkblue','darkorange'))

#####  Paired Samples   ##########

### math-socst
#qqplot
qqnorm(math-socst)
qqline(math-socst,lwd=2)
#έλεγχοι κανονικότητας
lillie.test(math-socst) 
shapiro.test(math-socst)# απορρίπτω την κανονικότητα αλλα έχω μεγάλο δείγμα
# ο μέσος κατάλληλο περιγρ μέτρο ?
describe(math-socst) # μικρή διαφορά μέσoυ και διάμεσου  
# μικρή απόκλιση απο την συμμετρία
# πλατύκυρτη
hist(math-socst,prob=TRUE)
curve(dnorm(x,mean(math-socst),sd(math-socst)),add=TRUE,lwd=2)

symmetry.test(math-socst) # δεν απορρίπτω την συμμετρία
wilcox.test(math-socst) # δεν απορρίπτω την ισότητα των μέσων

# Ξεχωριστά boxplot δεν λαμβάνουν υπόψιν την συσχέτιση
boxplot(math,socst,col=c('red','darkgreen'),ylab='Βαθμοί')
axis(1,c(1,2),c('math','socst'))
title('Boxplot των βαθμών στα μαθηματικά και τις κοινωνικές επιστήμες')

# boxplot διαφοράς
boxplot(math-socst,col=c('blue'),ylab='Βαθμοί',xlab='math-socst')
title('Boxplot διαφοράς στα μαθηματικά και τις κοινωνικές επιστήμες')
abline(h=0,col='red',lty=2,lwd=2)

### math~write

#qqplot
qqnorm(math-write)
qqline(math-write,lwd=2)
#έλεγχοι κανονικότητας
lillie.test(math-write) 
shapiro.test(math-write)# δεν απορρίπτω την κανονικότητα 
# ο μέσος κατάλληλο περιγρ. μέτρο ? NAI
describe(math-write) # μικρή διαφορά μέσoυ και διάμεσου  
# μικρή απόκλιση απο την συμμετρία
# μεσόκυρτη
hist(math-write,prob=TRUE)
curve(dnorm(x,mean(math-write),sd(math-write)),add=TRUE,lwd=2)


symmetry.test(math-write) # δεν απορρίπτω την συμμετρία
t.test(math,write,paired = TRUE) # δεν απορρίπτω την ισότητα των μέσων

# Errorbar διαφοράς
math_write_diff<-math-write
temp <- t.test(math,write,paired = TRUE) 
errbar( 1, mean(math_write_diff),  yplus=temp$conf[2], yminus=temp$conf[1],
        bty='l',ylab='Βαθμοί',xlab='math-write')
abline(h=0, col=2, lwd=2, lty=2)
title('Errorbar των βαθμών στα μαθηματικά και στην έκθεση')

### socst~write

#qqplot
qqnorm(socst-write)
qqline(socst-write,lwd=2)
#έλεγχοι κανονικότητας
lillie.test(socst-write) 
shapiro.test(socst-write)# απορρίπτω την κανονικότητα 
# ο μέσος κατάλληλο περιγρ. μέτρο ?
describe(socst-write) # μεγάλη διαφορά μέσoυ και διάμεσου 
# μικρή απόκλιση απο την συμμετρία
# μεσόκυρτη
hist(socst-write,prob=TRUE)
curve(dnorm(x,mean(socst-write),sd(socst-write)),add=TRUE,lwd=2)


symmetry.test(socst-write) # απορρίπτω την συμμετρία
wilcox.test(socst-write) # δεν απορρίπτω την ισότητα των μέσων

# Ξεχωριστά boxplot δεν λαμβάνουν υπόψιν την συσχέτιση
boxplot(socst,write,col=c('red','darkgreen'),ylab='Βαθμοί')
axis(1,c(1,2),c('socst','write'))
title('Boxplot των βαθμών στις κοινωνικές επιστήμες και στην έκθεση')

#boxplot διαφοράς
boxplot(socst-write,col=c('blue'),ylab='Βαθμοί',xlab='socst-write')
title('Boxplot διαφοράς στις κοινωνικές επιστήμες και στην έκθεση')
abline(h=0,col='red',lty=2,lwd=2)


###   Kατηγορική με Βαθμολογίες    ######


##  Genre ∼ Math,Socst,Write 

#Genre ∼ Math
anova1 <- aov( math~genre, data)


##Normality of the residuals##
lillie.test(anova1$res)
shapiro.test(anova1$res)
qqnorm(anova1$res)
qqline(anova1$res) # Δεν απορρίπτω την κανονικότητα

##Homogeneity of Variance##
leveneTest(anova1) # δεν απορρίπτω την ισότητα διακυμανσ.

## Ελεγχος
summary( anova1) # δεν απορρίπτω την ίση βαθμολογία στα μαθηματικά
# ανάμεσα στα φύλα

# Διαγράμματα
round(TukeyHSD(anova1)$genre,3)
plot(TukeyHSD(anova1))

plotmeans(math~genre, data, connect=F, xlab='', las=2)
title('Error bar Βαθμών μαθηματικών ανάλογα το φύλο')

### genre ∼ Socst ###
anova2 <- aov( socst~genre, data)


##Normality of the residuals##
lillie.test(anova2$res)
shapiro.test(anova2$res)
qqnorm(anova2$res)
qqline(anova2$res) # απορρίπτω την κανονικότητα αλλα 
# έχω μεγάλο δείγμα

# ο μέσος κατάλληλο περιγρ. μέτρο ?

tapply(socst,genre,describe)

boxplot(socst~genre,col=c('red','darkgreen'),ylab='Βαθμοί')
title('Βαθμοί κοινωνικών επιστημών ανάλογα το φύλο')

# Και στις δύο περιπτώσεις έχω ασσυμετρία αλλα οι μέσοι δεν 
# αποκλίνουν πολυ από την διάμεσο απότε

# NAI EINAI καταλληλο περιγραφικό μέτρο

##Homogeneity of Variance##
leveneTest(anova2) # δεν απορρίπτω την ισότητα διακυμανσ.

## Ελεγχος
summary( anova2) # δεν απορρίπτω την ίση βαθμολογία στις
#  κοινωνικές επιστήμες ανάμεσα στα φύλα

# Διαγράμματα
round(TukeyHSD(anova2)$genre,3)
plot(TukeyHSD(anova2))

plotmeans(socst~genre, data, connect=F, xlab='', las=2)
title('Error bar Βαθμών κοινωνικών επιστημών ανάλογα το φύλο')

### genre ∼ Writε *** ##
anova3 <- aov( write~genre, data)


##Normality of the residuals##
lillie.test(anova3$res)
shapiro.test(anova3$res)
qqnorm(anova3$res)
qqline(anova3$res) # απορρίπτω την κανονικότητα αλλα 
# έχω μεγάλο δείγμα

# ο μέσος κατάλληλο περιγρ. μέτρο ?

tapply(write,genre,describe)

boxplot(write~genre,col=c('red','darkgreen'),ylab='Βαθμοί')
title('Βαθμοί έκθεσης ανάλογα το φύλο')

# για τους άνδρες φαίνεται να είναι πλατύκυρτη 
# όμως κατα τ'αλλα έχω μικρες αποκλίσεις απο την συμμετρία και κύρτωση αρα
# NAI EINAI καταλληλο μέτρο

##Homogeneity of Variance##
leveneTest(anova3) #  απορρίπτω την ισότητα διακυμανσ.

## Ελεγχος ισότητας με άνισες διακυμάνσεις
oneway.test(write~genre,data,var.equal=FALSE) 

## Kruskal Wallis	
kruskal.test(write~genre,data)

#απορρίπτω και στις δύο περιπτώσεις την ισότητα των μέσων

#error bars
plotmeans(write~genre, data, connect=F, xlab='')
title('Error Bars των μέσων βαθμών έκθεσης ανάλογα το φύλο')


##  Schtyp ∼ Math,Socst,Writε

#Schtyp ∼ Math 

anova4 <- aov( math~schtyp, data)


##Normality of the residuals##
lillie.test(anova4$res)
shapiro.test(anova4$res)
qqnorm(anova4$res)
qqline(anova4$res) # απορρίπτω την κανονικότητα αλλα 
# έχω μεγάλο δείγμα

# ο μέσος κατάλληλο περιγρ. μέτρο ?

tapply(math,schtyp,describe)

boxplot(math~schtyp,col=c('red','darkgreen'),ylab='Βαθμοί')
title('Βαθμοί μαθηματικών ανάλογα τον τύπο σχολείου')

# NAI είναι δεν έχω μεγάλη ασυμμετρία και κύρτωση

##Homogeneity of Variance##
leveneTest(anova4) # δεν απορρίπτω την ισότητα διακυμανσ.

#Ελεγχος ΑΝΟVA
summary(anova4) # δεν απορρίπτω την ισότητα των μέσων

#error bars
plotmeans(math~schtyp, data, connect=F, xlab='')
title('Error Bars των μέσων βαθμών μαθηματικών ανάλογα τον τύπο σχολείου')


#Schtyp ∼ Socst

anova5 <- aov( socst~schtyp, data)

##Normality of the residuals##
lillie.test(anova5$res)
shapiro.test(anova5$res)
qqnorm(anova5$res)
qqline(anova5$res) # απορρίπτω την κανονικότητα αλλα 
# έχω μεγάλο δείγμα


# ο μέσος κατάλληλο περιγρ. μέτρο ?

tapply(socst,schtyp,describe)

boxplot(socst~schtyp,col=c('red','darkgreen'),ylab='Βαθμοί')
title('Βαθμοί κοινωνικών επιστημών ανάλογα τον τύπο σχολείου')
# NAI ειναι κατάλληλο περιγραφικό μέτρο

##Homogeneity of Variance##
leveneTest(anova5) # δεν απορρίπτω την ισότητα διακυμανσ.

#Ελεγχος ΑΝΟVA
summary(anova5) # δεν απορρίπτω την ισότητα των μέσων

#error bars
plotmeans(socst~schtyp, data, connect=F, xlab='')
title('Error Bars των μέσων βαθμών κοινωνικών επιστημών ανάλογα τον τύπο σχολείου')


#Schtyp ∼ Writε '

anova6 <- aov( write~schtyp, data)

##Normality of the residuals##
lillie.test(anova6$res)
shapiro.test(anova6$res)
qqnorm(anova6$res)
qqline(anova6$res) # απορρίπτω την κανονικότητα αλλα 
# έχω μεγάλο δείγμα


# ο μέσος κατάλληλο περιγρ. μέτρο ?

tapply(write,schtyp,describe)

#boxplot

boxplot(write~schtyp,col=c('red','darkgreen'),ylab='Βαθμοί')
title('Βαθμοί έκθεσης ανάλογα τον τύπο σχολείου')

# NAI ειναι κατάλληλο περιγραφικό μέτρο

##Homogeneity of Variance##
leveneTest(anova6) # απορρίπτω την ισότητα διακυμανσ.

## Ελεγχος ισότητας με άνισες διακυμάνσεις
oneway.test(write~schtyp,data,var.equal=FALSE) # απορρίπτει

## Kruskal Wallis	
kruskal.test(write~schtyp,data) # δεν απορρίπτει


#error bars
plotmeans(write~schtyp, data, connect=F, xlab='')
title('Error Bars των μέσων βαθμών έκθεσης ανάλογα τον τύπο σχολείου')



## race ∼ Math,Socst,Write


#race ∼ Math ***

anova7 <- aov( math~race, data)

##Normality of the residuals##
lillie.test(anova7$res)
shapiro.test(anova7$res)
qqnorm(anova7$res)
qqline(anova7$res) # δεν απορρίπτω την κανονικότητα 


##Homogeneity of Variance##
leveneTest(anova7) # απορρίπτω την ισότητα διακυμανσ.

## Ελεγχος ισότητας με άνισες διακυμάνσεις
oneway.test(math~race,data,var.equal=FALSE) # απορρίπτει

## Kruskal Wallis	
kruskal.test(math~race,data) # απορρίπτει

#error bars
plotmeans(math~race, data, connect=F, xlab='')
title('Error Bars των μέσων βαθμών μαθηματικών ανάλογα την φυλή')

#pairwise για να δω ποιες φυλές διαφέρουν
pairwise.t.test( math,race,p.adjust.method = "holm")
# δεν απορριπτω την διαφορά μεσων ανάμεσα σε ισπανούς-αφροαμερικάνους
# και λευκούς και ασιάτες

#race ∼ Socst *

anova8 <- aov( socst~race, data)

##Normality of the residuals##
lillie.test(anova8$res)
shapiro.test(anova8$res)
qqnorm(anova8$res)
qqline(anova8$res) # απορρίπτω την κανονικότητα αλλα έχω μεγάλο δείγμα

# ο μέσος κατάλληλο περιγρ. μέτρο ?
tapply(socst,schtyp,describe)

boxplot(socst~race,col=c('red','darkgreen'),ylab='Βαθμοί')
title('Βαθμοί κοινωνικών επιστημών ανάλογα την φυλή')
# NAI EINAI

##Homogeneity of Variance##
leveneTest(anova8) #  δεν απορρίπτω την ισότητα διακυμανσ.


## Ελεγχος 
summary(anova8)
# απορρίπτω την ισότητα των μέσων

plotmeans(socst~race, data, connect=F, xlab='')
title('Error Bars των μέσων βαθμών κοινωνικών επιστημών ανάλογα την φυλή')

#pairwise για να δω ποιες φυλές διαφέρουν
pairwise.t.test(socst,race,p.adjust.method = 'holm')
#απορρίπτω την ισότητα των λευκών και των ισπανών


# Βλεπουμε οτι απορρίπτεται η ισότητα μεταξυ των Λευκών και Ισπανών

# race ∼ Writε ***

anova9 <- aov( write~race, data)

##Normality of the residuals##
lillie.test(anova9$res)
shapiro.test(anova9$res)
qqnorm(anova9$res)
qqline(anova9$res) # απορρίπτω την κανονικότητα αλλα έχω μεγάλο δείγμα

# ο μέσος κατάλληλο περιγρ. μέτρο ?
tapply(write,schtyp,describe)

boxplot(write~race,col=c('red','darkgreen'),ylab='Βαθμοί')
title('Βαθμοί έκθεσης ανάλογα το φυλή') 
#NAI είναι

##Homogeneity of Variance##
leveneTest(anova9) #  δεν απορρίπτω την ισότητα διακυμανσ.

## Ελεγχος 
summary(anova9)
# απορρίπτω την ισότητα των μέσων

plotmeans(write~race, data, connect=F, xlab='')
title('Error Bars των μέσων βαθμών έκθεσης ανάλογα την φυλή')

#pairwise για να δω ποιες φυλές διαφέρουν
pairwise.t.test(write,race,p.adjust.method = 'holm')
#απορρίπτω την ισότητα των λευκών-ισπανών ,ισπανών-ασιατών
# , αφροαμερικ-λευκων , ασιατών και αφροαμερικανών


## prog~ math,socst,write

#prog ∼ Math ***
anova10 <- aov( math~prog, data)

##Normality of the residuals##
lillie.test(anova10$res)
shapiro.test(anova10$res)
qqnorm(anova10$res)
qqline(anova10$res) # απορρίπτω την κανονικότητα αλλα έχω μεγάλο δείγμα

# ο μέσος κατάλληλο περιγρ. μέτρο ?
tapply(math,prog,describe)

boxplot(math~prog,col=c('red','darkgreen'),ylab='Βαθμοί')
title('Βαθμοί μαθηματικών ανάλογα το πρόγραμμα σπουδών')
#NAI EINAI

##Homogeneity of Variance##
leveneTest(anova10) #  απορρίπτω την ισότητα διακυμανσ.

## Ελεγχος ισότητας με άνισες διακυμάνσεις
oneway.test(math~prog,data,var.equal=FALSE) # απορρίπτει

## Kruskal Wallis	
kruskal.test(math~prog,data) # απορρίπτει

#error bars
plotmeans(math~prog, data, connect=F, xlab='')
title('Error Bars των μέσων βαθμών μαθηματικών ανάλογα το πρόγραμμα σπουδών')

#pairwise για να δω ποιa προγράμματα διαφέρουν
pairwise.t.test(math,prog,p.adjust.method = 'holm')
#απορρίπτω την ισότητα των general-academic , academic-vocation

#prog ∼ Socst ***

anova11 <- aov( socst~prog, data)

##Normality of the residuals##
lillie.test(anova11$res)
shapiro.test(anova11$res)
qqnorm(anova11$res)
qqline(anova11$res) # απορρίπτω την κανονικότητα αλλα έχω μεγάλο δείγμα

# ο μέσος κατάλληλο περιγρ. μέτρο ?
tapply(socst,prog,describe)

boxplot(socst~prog,col=c('red','darkgreen'),ylab='Βαθμοί')
title('Βαθμοί κοινωνικών επιστημών ανάλογα το πρόγραμμα σπουδών')
#NAI EINAI

##Homogeneity of Variance##
leveneTest(anova11) # δεν απορρίπτω την ισότητα διακυμανσ.

#Ελεγχος
summary(anova11) #απορρίπτω την ισότητα των μέσων

plotmeans(socst~prog, data, connect=F, xlab='')
title('Error Bars των μέσων βαθμών κοινωνικών επιστημών ανάλογα το πρόγραμμα σπουδών')

#pairwise για να δω ποιa προγράμματα διαφέρουν
pairwise.t.test(socst,prog,p.adjust.method = 'bonferroni')
# Βλεπουμε οτι απορρίπτεται η ισότητα κάθε κατηγορίας

# prog ∼ Writε ***
anova12 <- aov( write~prog, data)

##Normality of the residuals##
lillie.test(anova12$res)
shapiro.test(anova12$res)
qqnorm(anova12$res)
qqline(anova12$res) # απορρίπτω την κανονικότητα αλλα έχω μεγάλο δείγμα

# ο μέσος κατάλληλο περιγρ. μέτρο ?
tapply(write,prog,describe)

boxplot(write~prog,col=c('red','darkgreen'),ylab='Βαθμοί')
title('Βαθμοί έκθεσης ανάλογα το πρόγραμμα σπουδών') 
#NAI ειναι

##Homogeneity of Variance##
leveneTest(anova12) # δεν απορρίπτω την ισότητα διακυμανσ.

#Ελεγχος
summary(anova12) #απορρίπτω την ισότητα των μέσων

plotmeans(write~prog, data, connect=F, xlab='')
title('Error Bars των μέσων βαθμών έκθεσης ανάλογα το πρόγραμμα σπουδών')

#pairwise για να δω ποιa προγράμματα διαφέρουν
pairwise.t.test(write,prog,p.adjust.method = 'bonferroni')
# Βλεπουμε οτι απορρίπτεται η ισότητα κάθε κατηγορίας


# b)#########################################
# Υπάρχουν διαφορές στις επιδόσεις των μαθητών στην έκθεση ανάλογα με το φύλο του και το 
# πρόγραμμα σπουδών που έχουν ακολουθήσει;
##########################################
datab<-data[,c('write','genre','prog')]
datab$factor=factor=factor(paste(genre,prog,sep='_'))

anovab<-aov(write~prog+genre,data)

##Normality of the residuals##
lillie.test(anovab$res)
shapiro.test(anovab$res)
qqnorm(anovab$res)
qqline(anovab$res) # απορρίπτω την κανονικότητα αλλα έχω μεγάλο δείγμα

# ο μέσος κατάλληλο περιγρ. μέτρο ?
tapply(write,factor,describe)

boxplot(write~genre+prog,ylab='Βαθμοί')
title('Βαθμοί έκθεσης ανάλογα το πρόγραμμα σπουδών και το φύλο') 
# NAI διότι οι μέσοι πλησιάζουν τις διάμεσους

##Homogeneity of Variance##
leveneTest(write~prog*genre,data=datab) # δεν απορρίπτω την ισότητα διακυμανσ.
# Ελεγχος ANOVA
summary(anovab) # απορρίπτω την υπόθεση οτι οι μέσοι ειναι ίσοι

plotmeans(write~factor, datab, connect=F, xlab='')
title('Error Bars των μέσων βαθμών έκθεσης ανάλογα το πρόγραμμα σπουδών και 
      το φύλο')

#pairwise για να δω ποιa προγράμματα διαφέρουν
pairwise.t.test(write,factor,p.adjust.method = 'holm')
# Βλεπουμε οτι απορρίπτεται η ισότητα μεταξύ των : female_academic~female_vocation,
# female_academic~male_general, female_academic~male_vocation,
# female_general~ male_vocation,  female_vocation~male_vocation
# male_academic~ male_vocation

# το male_voaction με όλα συν αλλα δυο του female_academic



#c) #######################################################
#  Εκτιμήστε δύο διαφορετικά γραμμικά μοντέλα που να εξετάζουν 
# τη σχέση του βαθμού που πήρε στις κοινωνικές επιστήμες και στα 
# μαθηματικά αντίστοιχα, σε σχέση με τις υπόλοιπες μεταβλητές 
# (συμπεριλαμβανόμενου και της επίδοσής του στην έκθεση αλλά και 
# της βαθμολογίας στο άλλο μάθημα δλδ μαθηματικά ή κοινωνικές
# επιστήμες).
########################################################
library(PerformanceAnalytics)
library(car)
library(sjPlot)
################    socst   ##########################

# Αρχικά πρεπει να δω τις συσχετίσεις και αν έχει νόημα ενα γραμ.
# μοντέλο
sinexis <- as.data.frame( cbind(socst,math,write) ) 
tab_corr( sinexis, corr.method= "spearman", p.numeric = TRUE)

# διάγραμμα συσχετ
corrplot(cor(sinexis,method = 'spearman'),method = 'ellipse',type='lower')

# scatter matrix
pairs(sinexis,upper.panel = NULL,pch=16)
chart.Correlation(sinexis, histogram=TRUE, pch=19)


full<-lm(socst~.-id,data = data)
check(full) # έλεγχος υποθέσεων
# όλες οι υποθέσεις φαίνονται να μην απορρίπτονται

Anova(full,type=3)

# τρεχώ stepwise BIC για να βρω το βέλτιστο μοντέλο
n<-nrow(data)
step(full,direction = 'both',k=log(n)) # socst ~ write + math

halfb<-lm(socst~write+math,data = data)
summary(halfb)
check(halfb) # απορρίπτω μόνο την κανονικότητα

Anova(halfb,type=3)  # ολα στατ σημαντικά

# τρεχώ stepwise AIC για να βρω το βέλτιστο μοντέλο
n<-nrow(data)
step(full,direction = 'both' ) # socst ~ prog + write + math
# to AIC προσθέτει και το prog

halfa<-lm(socst~write+math+prog,data = data)
summary(halfa)
check(halfa)  # OK!!! Οι υποθέσεις παλι λιγο με την κανονικότητα
# δεν απορρίπτω στο όριο

Anova(halfa,type=3) # ολα στατ σημαντικά

# Συγκρίνω τα μοντέλα
tab_model(halfa,halfb)
# προτιμω νμζ το A

# δοκιμάζω γραμ. μετασχηματισμό
library(MASS)
boxcox(halfa, lambda=seq(-5,5,0.1) )
temp<-boxcox(halfa, lambda=seq(-5,5,0.1) )
temp$x[ temp$y==max(temp$y) ]
# λ=2
box<-lm(((socst^2)-1)/2~write+math+prog,data = data)
summary(box)
check(box) # Δεν απορρίπτω την κανονικότητα

tab_model(halfa,box)
anova(box)
# Ακυρο 


#Cross Validation
install.packages('DAAG')
library(DAAG)

CVlm(data.frame(socst, write, math,prog),form.lm =  halfa)

################    math   ##########################

full2<-lm(math~.-id,data = data)
check(full2) # έλεγχος υποθέσεων
# όλες οι υποθέσεις φαίνονται να μην απορρίπτονται έκτος της
# κανονικότητας αλλά και πάλι έχω μεγάλο δείγμα και φαίνεται 
# να χαλάει μόνο στις ουρές

Anova(full2,type=3)

# τρεχώ stepwise BIC για να βρω το βέλτιστο μοντέλο
n<-nrow(data)
step(full2,direction = 'both',k=log(n)) # math ~ genre + prog + write + socst

halfb2<-lm(math ~ genre + prog + write + socst,data = data)
summary(halfb2)
check(halfb2) # δεν απορρίπτω καμία υπόθεση !!!

anova(halfb2)
Anova(halfb2,type=3)  # ολα στατ σημαντικά


# τρεχώ stepwise AIC για να βρω το βέλτιστο μοντέλο
n<-nrow(data)
step(full2,direction = 'both' ) # math ~ genre + race + prog + write + socst
# to AIC προσθέτει και το race

halfa2<-lm(math ~ genre +prog + write + socst+race,data = data)
summary(halfa2)
check(halfa2)  # OK!!! Οι υποθέσεις 

anova(halfa2)
Anova(halfa2,type=3) # ολα στατ σημαντικά εκτος του race που ειναι 
# οριακα στο 0.05

# Συγκρίνω τα μοντέλα
tab_model(halfa2,halfb2)
# προτιμω νμζ το b για 3 βαθμούς ελευθερίας
# αφαιρώ εξηγώ τα 354 απο το άθροισμα των καταλοίπων


#d)########################################################
# Εκτιμήστε δύο διαφορετικά γραμμικά μοντέλα που να εξετάζουν 
#τη σχέση του βαθμού που πήρε στις κοινωνικές επιστήμες και
# στα μαθηματικά αντίστοιχα, σε σχέση
# με τις υπόλοιπες μεταβλητές (εκτός της επίδοσής του στην έκθεση).   
############################################

# SOCST

# Ορίζω το μοντέλο 
full3<-lm(socst~.-write-id,data=data)
check(full3)
#απορρίπτω την κανονικότητα

#ANOVA
anova(full3)

Anova(full3,type=3)

#BIC
n <- nrow(data)
step(full3, direction='both', k=log(n)) # socst ~ prog + math

halfb3<-lm(socst ~ math+prog,data=data)
tab_model(halfb3)
# Το ελέγχω
check(halfb3) # απορρίπτω την κανονικότητα 

# ΑΝΟVA
anova(halfb3)
Anova(halfb3) # όλα στατ σημαντικά

# Συγκριση με το full
tab_model(full3,halfb3)


# AIC
n <- nrow(data)
step(full3, direction='both') # socst ~ prog + math

# και το AIC kai το BIC καταλήγουν στο ίδιο μοντέλο


#### MATH    #############

# Ορίζω το μοντέλο 
full4<-lm(math~.-write-id,data=data)
check(full4)
# δεν απορρίπτω καμία υπόθεση


#ANOVA
anova(full4)

Anova(full4,type=3)

#BIC
n <- nrow(data)
step(full4, direction='both', k=log(n)) # math ~ race + prog + socst

halfb4<-lm(math ~ race + prog + socst,data=data)
tab_model(halfb4)

# Το ελέγχω
check(halfb4) # δεν απορρίπτω τις υποθέσεις 

# ΑΝΟVA
anova(halfb4)
Anova(halfb4) # όλα στατ σημαντικά

# Συγκριση με το full
tab_model(full4,halfb4) 


# AIC
n <- nrow(data)
step(full4, direction='both') # math ~ race + prog + socst

#   e   ###############################
# Δοκιμάστε μοντέλα με διαφορετικές κωδικοποιήσεις των ανεξάρτητων 
# μεταβλητών (δηλαδή διαφορετικά επίπεδα αναφοράς) και δείτε πως
# αυτό επηρεάζει τα συμπεράσματα από τα μοντέλα.
########################################

####  halfb3 ## socst ~ prog + math ###########
summary(halfb3)

contrasts(data$prog)<-contr.sum(3)[c(3,1:2),] # το πρωτο επίπεδο θα έχει αρνητ. άσσους
halfb3_new<-lm(socst ~ prog + math,data=data)

#Το μοντέλο
check(halfb3_new) # απορρίπτω μόνο κανονικότητα όπως πριν

#ANOVA
anova(halfb3_new)

tab_model(halfb3_new,halfb3)
model.matrix(halfb3_new)

# απλα αλλάζουν τα coefficients δεν αλλάζει το μοντέλο απλώς
# οι εκτιμήσεις των παραμέτρων

halfb3$coefficients # ο συνολικός μέσος εδώ ειναι το αθροισμα (3*int+ prog1+prog2)/3
#vs
halfb3_new$ coefficients # το οποίο ειναι ίσο με το intercept εδώ που είναι ήδη ο συνολικό μέσος
# ο συντελεστης της συνεχής ανεξάρτητης μεταβλητής παρέμεινε ίδιος
# δεν αλλάζει ερμηνεία ειναι η αναμενόμενη μεταβολή της απόκρισης οταν αυξάνεται 
# κατά μία μονάδα ενώ όλα τα άλλα είναι σταθερά

# και αν βάλω στο sum to zero την πρώτη γραμμη με αρνητ. άσσους 
# αφαιρώντας τους συντελεστές των dummies απο το intercept στο 
# sum to zero παιρνω το intercept του cornell constraint

