# Create a subset only for words
ldtwords = ldtset[ldtset$target.sta=="word",]
# Normalising response times
# -1/1000
ldtwords$invRT = -1000/ldtwords$RT
# Log transformation
ldtwords$logRT = log(ldtwords$RT)
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
# Subsetting
# Creating the subset that only contains word targets and correct responses
ldtrt = subset(ldt.acc60, target.sta=="word"& accuracy=="1")
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
# Extract subject means and do normalitiy tests with RT, logRT, and invRT
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
# Summarise priming effects
sum = ldtrt %>%
  group_by(subject,target.lan) %>%
  summarize(priming = mean(RT[prime.type=="con"])-mean(RT[prime.type=="rel"]))
# Models
# L1-L2 
engmod = lmer(invRT~prime.type+order+target.freq+prof+
                (1|subject)+(1|itemcode), REML=FALSE, engset)
#L2-L1
spamod = lmer(invRT~prime.type+order+prime.type:bil.dom+
                (1|subject)+(1|itemcode), REML=FALSE, spaset)
spamodfreq = lmer(invRT~prime.type+order+prime.type:bil.dom+prime.type:target.freq+prime.type:prime.freq+
                    (1|subject)+(1|itemcode), REML=FALSE, spaset)
# Joint analysis
jointmod = lmer(invRT~target.freq:target.lan + prime.type +
                  order:target.lan+
                  (1+target.lan|subject)+(1|itemcode), REML=FALSE, ldtrt)

jointmod2 = lmer(invRT~target.freq:target.lan + prime.type + prime.type:target.lan+
                   order:target.lan+
                   (1+target.lan|subject)+(1|itemcode), REML=FALSE, ldtrt)
# Accuracy analysis
#L1-L2

engaccmod = glmer(accuracy~awar+prof+
                    (1|subject)+(1|itemcode),ldtaccword[ldtaccword$target.lan=="eng",], 
                  family="binomial", control = glmerControl(optimizer = "bobyqa"))
#L2-L1
spaaccmod = glmer(accuracy~order+target.freq+time+
                    (1|subject)+(1|itemcode),ldtaccword[ldtaccword$target.lan=="spa",], 
                  family="binomial", control = glmerControl(optimizer = "bobyqa"))
# Accuracy joint analysis
accjointmod = glmer(accuracy~target.lan+order+ztarget.freq+ztime+
                      target.lan:zprof+
                      (1|subject)+(1|itemcode),ldtset, 
                    family="binomial", control = glmerControl(optimizer = "bobyqa"))
