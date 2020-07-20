#Created by Adel Chaouch Orozco
#University of Reading, 2020

# Create a subset only for words
ldtwords = ldtset[ldtset$target.sta=="word",]
# Normalising response times
# -1/1000
ldtwords$invRT = -1000/ldtwords$RT
# Log transformation
ldtwords$logRT = log(ldtwords$RT)
# Box-Cox transformation
newldt$boxRT = BoxCox(newldt$RT, lambda)
# Factorising and coding the condition variable to help with the plotting and the contrast coding in LME
ldtwords$prime.type = as.character(ldtwords$prime.type)
ldtwords$prime.type[ldtwords$prime.type == "related"] = "rel"
ldtwords$prime.type[ldtwords$prime.type == "control"] = "con"
ldtwords$prime.type = as.factor(ldtwords$prime.type)
# The same with accuracy 
ldtwords$accuracy = as.character(ldtwords$accuracy) 
ldtwords$accuracy[ldtwords$accuracy == "yes"] = "1"
ldtwords$accuracy[ldtwords$accuracy == "no"] = "0"
ldtwords$accuracy = as.factor(ldtwords$accuracy)
# Establish the "Control" condition as the intercept so we can compare all other conditions to that
ldtwords$prime.type = relevel(ldtwords$prime.type, "con")
contrasts(ldtwords$prime.type)
# Change to sum coding
contrasts(ldtwords$prime.type) = c(-0.5,0.5)
contrasts(ldtwords$accuracy) = c(-0.5,0.5)
contrasts(ldtwords$target.lan) = c(-0.5,0.5)
contrasts(ldtwords$awar) = c(-0.5,0.5)
# Subsetting
# Creating the subset that only contains word targets and correct responses
ldtrt = subset(ldtwords, target.sta=="word"& accuracy=="1")
# Creating Z-scores
ldtrt$zprof = scale(ldtrt$prof, center = TRUE, scale = TRUE)
ldtrt$ztime = scale(ldtrt$time, center = TRUE, scale = TRUE)
ldtrt$zbil.dom = scale(ldtrt$bil.dom, center = TRUE, scale = TRUE)
ldtrt$zage.acq = scale(ldtrt$age.acq, center = TRUE, scale = TRUE)
ldtrt$zage = scale(ldtrt$age, center = TRUE, scale = TRUE)
ldtrt$zprime.freq = scale(ldtrt$prime.freq, center = TRUE, scale = TRUE)
ldtrt$ztarget.freq = scale(ldtrt$target.freq, center = TRUE, scale = TRUE)
# Accuracy per subject
# For words and nonwords
acc = table(ldtset$subject[ldtset$accuracy=="yes"])/table(ldtset$subject)*100
ldt.eng.acc60 = subset(ldtset, target.lan=="eng")
accengsub = table(ldt.eng.acc60$subject[ldt.eng.acc60$accuracy=="yes"])/table(ldt.eng.acc60$subject)*100
ldt.spa.acc60 = subset(ldtset, target.lan=="spa")
accspasub = table(ldt.spa.acc60$subject[ldt.spa.acc60$accuracy=="yes"])/table(ldt.spa.acc60$subject)*100
# Only for words
ldtaccword = subset(ldtset, target.sta=="word")
wordaccu = table(ldtaccword$subject[wordacc$accuracy=="yes"])/table(ldtaccword$subject)*100
ldt.eng.acc60 = subset(ldtaccword, target.lan=="eng")
accengsub = table(ldt.eng.acc60$subject[ldt.eng.acc60$accuracy=="yes"])/table(ldt.eng.acc60$subject)*100
ldt.spa.acc60 = subset(ldtaccword, target.lan=="spa")
accspasub = table(ldt.spa.acc60$subject[ldt.spa.acc60$accuracy=="yes"])/table(ldt.spa.acc60$subject)*100
#Create subsets for modelling
engset = subset(ldtrt, target.lan=="eng")
spaset = subset(ldtrt, target.lan=="spa")
# Error rates for word targets
summary(ldt.eng.acc60[ldt.eng.acc60$prime.type=="rel",]$accuracy)
summary(ldt.eng.acc60[ldt.eng.acc60$prime.type=="con",]$accuracy)
summary(ldt.spa.acc60[ldt.spa.acc60$prime.type=="rel",]$accuracy)
summary(ldt.spa.acc60[ldt.spa.acc60$prime.type=="con",]$accuracy)
summary(ldt.spa.acc60[ldt.spa.acc60$target.sta=="word",]$accuracy)
# Extract subject means and do normalitiy tests with RT, logRT, invRT, and BoxRT
# All targets
subMeans = aggregate(RT~subject, data = ldtrt, mean)
colnames(subMeans) = c("subject", "meanRT")
plot(subMeans) 
shapiro.test(subMeans$meanRT) 
subQQ = qqnorm(subMeans$meanRT); qqline(subMeans$meanRT, col="darkblue")
subMeans = aggregate(logRT~subject, data = ldtrt, mean)
colnames(subMeans) = c("subject", "meanRT")
plot(subMeans) 
shapiro.test(subMeans$meanRT) 
subQQ = qqnorm(subMeans$meanRT); qqline(subMeans$meanRT, col="darkblue")
subMeans = aggregate(invRT~subject, data = ldtrt, mean)
colnames(subMeans) = c("subject", "meanRT")
plot(subMeans) 
shapiro.test(subMeans$meanRT) 
subQQ = qqnorm(subMeans$meanRT); qqline(subMeans$meanRT, col="darkblue")
subMeans = aggregate(boxRT~subject, data = ldtrt, mean)
colnames(subMeans) = c("subject", "meanRT")
plot(subMeans) 
shapiro.test(subMeans$meanRT) 
subQQ = qqnorm(subMeans$meanRT); qqline(subMeans$meanRT, col="darkblue")
# L1 targets
spaMeans = aggregate(RT~subject, data=spaset,mean)
colnames(spaMeans) = c("subject", "meanRT")
plot(spaMeans)
shapiro.test(spaMeans$meanRT) # p.value = 0.01201
spaQQ = qqnorm(spaMeans$meanRT); qqline(spaMeans$meanRT, col="darkblue")
spaMeans = aggregate(logRT~subject, data=spaset,mean)
colnames(spaMeans) = c("subject", "meanRT")
plot(spaMeans)
shapiro.test(spaMeans$meanRT) 
spaQQ = qqnorm(spaMeans$meanRT); qqline(spaMeans$meanRT, col="darkblue")
spaMeans = aggregate(invRT~subject, data=spaset,mean)
colnames(spaMeans) = c("subject", "meanRT")
plot(spaMeans)
shapiro.test(spaMeans$meanRT) 
spaQQ = qqnorm(spaMeans$meanRT); qqline(spaMeans$meanRT, col="darkblue")
spaMeans = aggregate(boxRT~subject, data=spaset,mean)
colnames(spaMeans) = c("subject", "meanRT")
plot(spaMeans)
shapiro.test(spaMeans$meanRT) # p.value = 0.01201
spaQQ = qqnorm(spaMeans$meanRT); qqline(spaMeans$meanRT, col="darkblue")
# L2 targets
engMeans = aggregate(RT~subject, data=engset,mean)
colnames(engMeans) = c("subject", "meanRT")
plot(engMeans)
shapiro.test(engMeans$meanRT) 
engQQ = qqnorm(engMeans$meanRT); qqline(engMeans$meanRT, col="darkblue")
engMeans = aggregate(logRT~subject, data=engset,mean)
colnames(engMeans) = c("subject", "meanRT")
plot(engMeans)
shapiro.test(engMeans$meanRT) 
engQQ = qqnorm(engMeans$meanRT); qqline(engMeans$meanRT, col="darkblue")
engMeans = aggregate(invRT~subject, data=engset,mean)
colnames(engMeans) = c("subject", "meanRT")
plot(engMeans)
shapiro.test(engMeans$meanRT) 
engQQ = qqnorm(engMeans$meanRT); qqline(engMeans$meanRT, col="darkblue")
engMeans = aggregate(boxRT~subject, data=engset,mean)
colnames(engMeans) = c("subject", "meanRT")
plot(engMeans)
shapiro.test(engMeans$meanRT) 
engQQ = qqnorm(engMeans$meanRT); qqline(engMeans$meanRT, col="darkblue")
# Models
# Joint analysis
jointmod = lmer(invRT~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                             target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                             target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                             target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                             (target.lan+prime.type+ztarget.freq+zprime.freq+target.lan:prime.type|subject)+
                             (target.lan*prime.type|primecode)+(target.lan*prime.type|targetcode),REML = FALSE, ldtrt)

jointmod2 = lmer(invRT~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                         target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                         target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+
                         (target.lan+prime.type+ztarget.freq+zprime.freq+target.lan:prime.type|subject)+
                         (target.lan*prime.type|primecode)+(target.lan+prime.type|targetcode),REML = FALSE, ldtrt)

jointmod3 = lmer(invRT~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                         target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                         target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+
                         (target.lan+prime.type+ztarget.freq+target.lan:prime.type|subject)+
                         (target.lan*prime.type|primecode)+(target.lan+prime.type|targetcode),REML = FALSE, ldtrt)

jointmod4 = lmer(invRT~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                         target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                         target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+
                         (target.lan+prime.type+ztarget.freq+target.lan:prime.type|subject)+
                         (target.lan*prime.type|primecode)+(target.lan|targetcode),REML = FALSE, ldtrt)

jointmod5 = lmer(invRT~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                         target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                         target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+
                         (target.lan+prime.type+ztarget.freq+target.lan:prime.type|subject)+
                         (target.lan:prime.type+target.lan|primecode)+(target.lan|targetcode),REML = FALSE, ldtrt)

jointmod6 = lmer(invRT~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                         target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                         target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+
                         (target.lan+ztarget.freq+target.lan:prime.type|subject)+
                         (target.lan:prime.type+target.lan|primecode)+(target.lan|targetcode),REML = FALSE, ldtrt)

jointmod7 = lmer(invRT~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                         target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                         target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+
                         (target.lan+target.lan:prime.type|subject)+
                         (target.lan:prime.type+target.lan|primecode)+(target.lan|targetcode),REML = FALSE, ldtrt)

jointmod8 = lmer(invRT~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                         target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                         target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+
                         (target.lan|subject)+
                         (target.lan:prime.type+target.lan|primecode)+(target.lan|targetcode),REML = FALSE, ldtrt)

jointmod9 = lmer(invRT~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                         target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                         target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                         target.lan:prime.type:zprime.freq:ztarget.freq+
                         (target.lan|subject)+
                         (target.lan|primecode)+(target.lan|targetcode),REML = FALSE, ldtrt)

jointmod10 = lmer(invRT~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                          target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                          target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                          target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                          target.lan:prime.type:zprime.freq:ztarget.freq+
                          (target.lan|subject)+
                          (1|primecode)+(target.lan|targetcode),REML = FALSE, ldtrt)

jointmod11 = lmer(invRT~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                          target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                          target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                          target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                          target.lan:prime.type:zprime.freq:ztarget.freq+
                          (1|subject)+
                          (1|primecode)+(target.lan|targetcode),REML = FALSE, ldtrt)

jointmod12 = lmer(invRT~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                          target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                          target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                          target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                          target.lan:prime.type:zprime.freq:ztarget.freq+
                          (1|subject)+
                          (1|primecode)+(1|targetcode),REML = FALSE, ldtrt)
#Analysis after after absolute residuals outlier removals (Baayen and Milin, 2010)
ldtrtout = newldt[abs(scale(resid(jointmod12))) < 2.5,]
jointmodout = lmer(invRT~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                           target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                           target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                           target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                           target.lan:prime.type:zprime.freq:ztarget.freq+
                           (1|subject)+(1|primecode)+(1|targetcode),REML = FALSE, newldtout)
cor(fitted(jointmod12), newldt$invRT)^2
cor(fitted(jointmodout), newldtout$invRT)^2
# Accuracy analysis
actjointmod = glmer(accuracy~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                       target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                       target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                       target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                       target.lan:prime.type:zprime.freq:ztarget.freq+
                       (target.lan+prime.type+ztarget.freq+zprime.freq+target.lan:prime.type|subject)+
                       (target.lan*prime.type|primecode)+(target.lan*prime.type|targetcode), ldtwords,family="binomial")

actjointmod2 = glmer(accuracy~target.lan+target.lan:prime.type+target.lan:zprof+target.lan:zbil.dom+
                        target.lan:prime.type:zprof+target.lan:prime.type:zbil.dom+target.lan:prime.type:zprime.freq+target.lan:prime.type:ztarget.freq+
                        target.lan:prime.type:zprime.freq:ztarget.freq+target.lan:prime.type:zprof:zprime.freq+target.lan:prime.type:zprof:ztarget.freq+
                        target.lan:prime.type:zbil.dom:zprime.freq+target.lan:prime.type:zbil.dom:ztarget.freq+
                        target.lan:prime.type:zprime.freq:ztarget.freq+
                        (target.lan+prime.type+ztarget.freq+zprime.freq+target.lan:prime.type|subject)+
                        (target.lan:prime.type+target.lan|primecode)+(target.lan*prime.type|targetcode), ldtwords,family="binomial",Control: glmerControl(optimizer = "bobyqa"))
