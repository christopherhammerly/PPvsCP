#####################################################################
###                                                               ###
###             PP vs CP in attraction                            ###
###               Judgement + Chunked SPR analysis pipeline       ###
###             Christopher Hammerly                              ###
###             UMass Amherst - 09.13.16                          ###
###                                                               ###
#####################################################################

#   Key to the experimental conditions:
#
#   Cond      Modifier    Gramm   Num 
#
#   cond-A    RC          G       Sg
#   cond-B    RC          G       Pl
#   cond-C    RC          U       Sg
#   cond-D    RC          U       Pl
#   cond-E    PP          G       Sg
#   cond-F    PP          G       Pl
#   cond-G    PP          U       Sg
#   cond-H    PP          U       Pl
#
#
#   was/were = 1-14
#   lexical = 15-30
#   is/are =  31-40
#   has/have = 41-48
#
#   TO DO:
#     - Create the mixture and bimodal models a la Which Flowers for judgment data
#
#
#


#   Packages necessary for analysis

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(ez)
library(lme4)
library(grid)
library(gridExtra)
library(car)

#   A function that gets around the issue of turning factors numeric

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#   Create column names for the data file.

mycols <- c("Subject","MD5","TrialType","Number","Element","Experiment","Item", "Question", "Response","null","RT","null")

#   Read in data.

data.raw <- read.csv('/Users/chrishammerly/PPvsCP/results/results.csv',
                     header = 0, 
                     sep = ",",
                     quote = "",
                     comment.char = "#",
                     col.names=mycols,
                     fill = TRUE)

data.raw$Subject <- as.factor(data.raw$Subject)

#   Remove subjects who were not paid on MTurk due to violating exclusion criteria (determined in
#   exclusion-pipeline.R)

excluded.subjects <- c("1473195073","1473339213","1473340057","1473356919","1473208710","1473265118","1473375948","1473432095","1473432829")
data.raw <- droplevels(subset(data.raw, !(Subject %in% excluded.subjects)))

#   Create frame for demographic information

data.demo <- droplevels(subset(data.raw, Experiment == 'background' | Experiment == "exit"))
data.demo <- data.demo %>%
  select(Subject,Question,Response) %>%
  spread(Question,Response) %>%
  droplevels()

#   Create graphic that shows where participants are from by state

states <- tolower(data.demo$state)
states <- gsub(" $","", states, perl=T)
for (i in 1:length(states)) {
  if (nchar(states[i]) > 2) {
    states[i] = state.abb[match(states[i] ,tolower(state.name))]
  }
}

states <- toupper(states)
data.demo$state <- states

state.data <- data.frame(states = table(states))
state.data$x <- state.center$x[match(state.data$states.states ,state.abb)]
state.data$y<- state.center$y[match(state.data$states.states ,state.abb)]
names(state.data) <- c("State","Frequency","x","y")

#   TO DO: Figure out how to include all frequencies, maybe do shading by state

map <- get_map(location = 'USA', zoom = 4,maptype="roadmap")
location.plot <- ggmap(map) + 
  geom_point(aes(x = x, y = y, size = Frequency), data = state.data, alpha = .5)+ 
  scale_size_continuous(range = range(state.data$Frequency))

pdf('location.pdf')
location.plot
dev.off()

#   Regioning information

W <- data.frame(Region = "W", state = c("CA","NV","OR","WA","ID","MT","WY","CO","UT"))
SW <- rbind(W,data.frame(Region="SW",state=c("AZ","NM","OK","TX")))
MW <- rbind(SW,data.frame(Region="MW",state=c("ND","SD","NE","KS","MN","IA","MO","WI","MI","IL","IN","OH")))
NE <- rbind(MW,data.frame(Region="NE",state=c("NY","RI","CT","VT","NH","ME","MA","NJ","PA")))
Region.key <- rbind(NE,data.frame(Region="SE",state=c("AR","AL","LA","MS","TN","KY","WV","DC","VA","NC","SC","GA","FL","DE","MD")))
Region.key$state <- as.character(Region.key$state)

#   Add regions and states to raw data file

data.demo.state <- data.demo[c(1,11)]
data.raw <- data.raw  %>%
  right_join(data.demo.state) %>%
  right_join(Region.key)

#   Add columns for factors and levels

exp.data <- data.raw

exp.data$Modifier <- ifelse(exp.data$Experiment=='cond-A' | exp.data$Experiment=='cond-B' | exp.data$Experiment=='cond-C' | exp.data$Experiment=='cond-D', 'RC', 'PP')
exp.data$Grammaticality <- ifelse(exp.data$Experiment=='cond-A' | exp.data$Experiment=='cond-B' | exp.data$Experiment=='cond-E' | exp.data$Experiment=='cond-F', 'grammatical', 'ungrammatical')
exp.data$Attractor <- ifelse(exp.data$Experiment=='cond-A' | exp.data$Experiment=='cond-C' | exp.data$Experiment=='cond-E' | exp.data$Experiment=='cond-G', 'SG', 'PL')

#   Add column for verb type

exp.data$Item <- as.numeric.factor(exp.data$Item)

exp.data$Verb <- ifelse(exp.data$Item >=1 & exp.data$Item <= 14, "was/were", 
                        ifelse(exp.data$Item >=15 & exp.data$Item <= 30, "lexical",
                               ifelse(exp.data$Item >=31 & exp.data$Item <= 40, "is/are",
                                      ifelse(exp.data$Item >=40 & exp.data$Item <= 48,"has/have","filler"))))
data.raw <- exp.data

#   Calculate z-scores for judgments over all items including fillers and plot them

data.all.judgments <- subset(data.raw , Experiment %in% c('cond-A', 'cond-B', 'cond-C','cond-D','cond-E','cond-F','cond-G','cond-H','filler') & TrialType == 'Question')
data.all.judgments$Response <- as.numeric(as.character(data.all.judgments$Response))

data.all.judgments$z <- ave(data.all.judgments$Response, data.all.judgments$Subject, FUN = scale)

ggplot(data.all.judgments,aes(x=z))+
  geom_histogram(binwidth=.1)+
  ggtitle("JUDGMENT Z-SCORE DISTRIBUTION\n FOR ALL ITEMS")

#   Separate and remove fillers from acceptability data

data.acceptability <- subset(data.all.judgments, Experiment %in% c('cond-A', 'cond-B', 'cond-C','cond-D','cond-E','cond-F','cond-G','cond-H'))


#   Separate SPR data and rename column headings

data.spr <- subset(exp.data, Experiment %in% c('cond-A', 'cond-B', 'cond-C','cond-D','cond-E','cond-F','cond-G','cond-H') & TrialType == 'DashedSentence')
names(data.spr) <- c("Subject","MD5","TrialType","Number","Element","Experiment","Item", "region", "fragment","RT","null","sentence","state","Region","Modifier","Grammaticality","Attractor","Verb")

data.spr$RT <- as.numeric(as.character(data.spr$RT))

#   Exclude trials where a chunk is read slower than 4000ms and faster than 100ms

bad.trials <- filter(data.spr, RT > 4000 | RT < 100)

bad.trials$ID <- paste(bad.trials$Subject,bad.trials$Item)

data.acceptability$ID <- paste(data.acceptability$Subject,data.acceptability$Item)
data.spr$ID <- paste(data.spr$Subject,data.spr$Item)

data.acceptability <- droplevels((subset(data.acceptability, !(ID %in% bad.trials$ID))))
data.spr <- droplevels((subset(data.spr, !(ID %in% bad.trials$ID))))

### a sanity check to ensure the correct number of subjects have been processed through in both files
print(data.acceptability %>% summarise(number = n_distinct(Subject)))
print(data.spr %>% summarise(number = n_distinct(Subject)))


#################################################
###                                           ###
###             Judgment Analysis             ###
###                                           ###
#################################################

#   Histogram of judgments from experimental items only (i.e. no fillers)

judge.all.exp <- ggplot(data.acceptability,aes(x=Response))+
  geom_histogram(binwidth=1)+
  ggtitle("JUDGMENT DISTRIBUTION\n FOR EXPERIMENTAL ITEMS")

pdf('judge.all.exp.PDF')
judge.all.exp
dev.off()

#   Very rough box plots to get an idea of reponses by item and by subject

boxplot(data.acceptability$Response ~ data.acceptability$Item)

boxplot(data.acceptability$Response ~ data.acceptability$Subject)

#   Plot z-scores for experimental items

ggplot(data.acceptability,aes(x=z))+
  geom_histogram(binwidth=.1)+
  ggtitle("JUDGMENT Z-SCORE DISTRIBUTION\n FOR EXPERIMENTAL ITEMS")

#   Histograms of judgments for all condtions. This organizes them into a 2 x 4 grid.

PP.SPP <- subset(data.acceptability, Experiment == "cond-H")
RC.SPP <- subset(data.acceptability, Experiment == "cond-D")
PP.SSS <- subset(data.acceptability, Experiment == "cond-E")
PP.SSP <- subset(data.acceptability, Experiment == "cond-G")
RC.SSS <- subset(data.acceptability, Experiment == "cond-A")
RC.SSP <- subset(data.acceptability, Experiment == "cond-C")
PP.SPS <- subset(data.acceptability, Experiment == "cond-F")
RC.SPS <- subset(data.acceptability, Experiment == "cond-B")

PP.SPP.hist <- ggplot(PP.SPP,aes(x=Response))+
  geom_histogram(binwidth=1)+
  ggtitle("PP *SPP")+
  ylim(0,90)
RC.SPP.hist <- ggplot(RC.SPP,aes(x=Response))+
  geom_histogram(binwidth=1)+
  ggtitle("RC *SPP")+
  ylim(0,90)
PP.SPS.hist <- ggplot(PP.SPS,aes(x=Response))+
  geom_histogram(binwidth=1)+
  ggtitle("PP SPS")+
  ylim(0,150)
RC.SPS.hist <- ggplot(RC.SPS,aes(x=Response))+
  geom_histogram(binwidth=1)+
  ggtitle("RC SPS")+
  ylim(0,150)
PP.SSS.hist <- ggplot(PP.SSS,aes(x=Response))+
  geom_histogram(binwidth=1)+
  ggtitle("PP SSS")+
  ylim(0,160)
PP.SSP.hist <- ggplot(PP.SSP,aes(x=Response))+
  geom_histogram(binwidth=1)+
  ggtitle("PP *SSP")+
  ylim(0,90)
RC.SSS.hist <- ggplot(RC.SSS,aes(x=Response))+
  geom_histogram(binwidth=1)+
  ggtitle("RC SSS")+
  ylim(0,160)
RC.SSP.hist <- ggplot(RC.SSP,aes(x=Response))+
  geom_histogram(binwidth=1)+
  ggtitle("RC *SSP")+
  ylim(0,90)

pdf('judgement.dist.by.cond.pdf')
grid.arrange(PP.SSS.hist, PP.SSP.hist, PP.SPS.hist, PP.SPP.hist, RC.SSS.hist, RC.SSP.hist, RC.SPS.hist, RC.SPP.hist, ncol = 4, nrow = 2)
dev.off()


#   Create a data frame with the mean response for each participant on each condition

subj.by.cond <- data.acceptability %>% 
  group_by(Subject, Modifier, Grammaticality, Attractor) %>% 
  summarise(mean.resp = mean(Response))

#   Create a data frame with the mean of the subject means and the SEM for each condition
#   Followed by a hack that splits modifier type that makes graphing easier later

cond.summ <- subj.by.cond %>%
  group_by(Modifier, Grammaticality, Attractor) %>%
  summarise(mean_cond = mean(mean.resp),
            SEM = sd(mean.resp)/sqrt(n_distinct(Subject)))

RC.summ <- subset(cond.summ, Modifier == "RC")
PP.summ <- subset(cond.summ, Modifier == "PP")

#   Difference scores and CIs for modifier type

differences.by.subj.modifier <- data.acceptability %>% 
  group_by(Subject,Grammaticality,Attractor) %>%
  filter(Modifier=='PP') %>%
  summarise(PP.mean = mean(Response))

differences.by.subj.modifier <- data.acceptability %>% 
  group_by(Subject,Grammaticality,Attractor) %>%
  filter(Modifier=='RC') %>%
  summarise(RC.mean = mean(Response)) %>%
  right_join(differences.by.subj.modifier)
  
differences.by.subj.modifier <- differences.by.subj.modifier %>%
  group_by(Subject,Grammaticality,Attractor) %>%
  summarise(Difference = PP.mean-RC.mean) %>%
  right_join(differences.by.subj.modifier)

differences.summary.modifier <- differences.by.subj.modifier                                  %>%
  group_by(Grammaticality,Attractor)                                                       %>%
  summarise(
    N = n_distinct(Subject),
    mean.diff = mean(PP.mean-RC.mean),
    mean.sem = sd(PP.mean-RC.mean)/sqrt(n_distinct(Subject))) %>%
  mutate(ci.lower.mean = mean.diff - qt(.975,df=N-1)*mean.sem,
         ci.upper.mean = mean.diff + qt(.975,df=N-1)*mean.sem
  )

#   Difference scores and CIs for grammaticality

differences.by.subj.grammaticality <- data.acceptability %>% 
  group_by(Subject,Modifier,Attractor) %>%
  filter(Grammaticality=='grammatical') %>%
  summarise(grammatical.mean = mean(Response))

differences.by.subj.grammaticality <- data.acceptability %>% 
  group_by(Subject,Modifier,Attractor) %>%
  filter(Grammaticality=='ungrammatical') %>%
  summarise(ungrammatical.mean = mean(Response)) %>%
  right_join(differences.by.subj.grammaticality)

differences.by.subj.grammaticality <- differences.by.subj.grammaticality %>%
  group_by(Subject,Modifier,Attractor) %>%
  summarise(Difference = grammatical.mean-ungrammatical.mean) %>%
  right_join(differences.by.subj.grammaticality)

differences.summary.grammaticality <- differences.by.subj.grammaticality                                  %>%
  group_by(Modifier,Attractor)                                                       %>%
  summarise(
    N = n_distinct(Subject),
    mean.diff = mean(grammatical.mean-ungrammatical.mean),
    mean.sem = sd(grammatical.mean-ungrammatical.mean)/sqrt(n_distinct(Subject))) %>%
  mutate(ci.lower.mean = mean.diff - qt(.975,df=N-1)*mean.sem,
         ci.upper.mean = mean.diff + qt(.975,df=N-1)*mean.sem
  )

#   Difference scores and CIs for attractor number

differences.by.subj.attractor <- data.acceptability %>% 
  group_by(Subject,Modifier,Grammaticality) %>%
  filter(Attractor=='SG') %>%
  summarise(SG.mean = mean(Response))

differences.by.subj.attractor <- data.acceptability %>% 
  group_by(Subject,Modifier,Grammaticality) %>%
  filter(Attractor=='PL') %>%
  summarise(PL.mean = mean(Response)) %>%
  right_join(differences.by.subj.attractor)

differences.by.subj.attractor <- differences.by.subj.attractor %>%
  group_by(Subject,Modifier,Grammaticality) %>%
  summarise(Difference = PL.mean - SG.mean) %>%
  right_join(differences.by.subj.attractor)

differences.summary.attractor <- differences.by.subj.attractor                                  %>%
  group_by(Modifier,Grammaticality)                                                       %>%
  summarise(
    N = n_distinct(Subject),
    mean.diff = mean(PL.mean-SG.mean),
    mean.sem = sd(PL.mean-SG.mean)/sqrt(n_distinct(Subject))) %>%
  mutate(ci.lower.mean = mean.diff - qt(.975,df=N-1)*mean.sem,
         ci.upper.mean = mean.diff + qt(.975,df=N-1)*mean.sem
  )

#   Figrues with rating means and SEM for each condition

dodge <- position_dodge(width=0.9)

raw.RC <- ggplot(RC.summ,aes(x=Grammaticality,y=mean_cond,fill=Attractor))+
  geom_bar(position = 'dodge',stat = "identity")+
  geom_linerange(stat='identity',position = position_dodge(width = 0.9),mapping=aes(ymax = mean_cond+SEM,ymin=mean_cond-SEM)) +
  scale_fill_manual(values=c("navy","maroon"))+
  ylab("MEAN RATING")+
  ylim(0,7)+
  ggtitle("RC Modifier")
raw.PP <- ggplot(PP.summ,aes(x=Grammaticality,y=mean_cond,fill=Attractor))+
  geom_bar(position = 'dodge',stat = "identity")+
  geom_linerange(stat='identity',position = position_dodge(width = 0.9),mapping=aes(ymax = mean_cond+SEM,ymin=mean_cond-SEM)) +
  scale_fill_manual(values=c("navy","maroon"))+
  ylab("MEAN RATING")+
  ylim(0,7)+
  ggtitle("PP Modifier")

pdf("Means-SEM-by-condition.PDF")
grid.arrange(raw.PP, raw.RC, ncol=2)
dev.off()

#   Figures to display difference scores

modifier.diff <- ggplot(aes(y = mean.diff, x = Grammaticality, fill = Attractor), data = differences.summary.modifier)+
  geom_bar(position='dodge',stat = "identity")+
  geom_errorbar(aes(ymin=ci.lower.mean, ymax=ci.upper.mean), position = dodge, width=0.1)+
  scale_fill_manual(values=c("navy","maroon","grey"))+
  ylab("PP - RC")+
  ggtitle("JUDGEMENT DIFFERENCE SCORES\n FOR MODIFIER TYPE")

grammaticality.diff <- ggplot(aes(y = mean.diff, x = Modifier, fill = Attractor), data = differences.summary.grammaticality)+
  geom_bar(position='dodge',stat = "identity")+
  geom_errorbar(aes(ymin=ci.lower.mean, ymax=ci.upper.mean), position = dodge, width=0.1)+
  scale_fill_manual(values=c("navy","maroon","grey"))+
  ylab("Grammatical - Ungrammatical")+
  ggtitle("JUDGEMENT DIFFERENCE SCORES\n FOR GRAMMATICALITY")

attractor.diff <- ggplot(aes(y = mean.diff, x = Modifier, fill = Grammaticality), data = differences.summary.attractor)+
  geom_bar(position='dodge',stat = "identity")+
  geom_errorbar(aes(ymin=ci.lower.mean, ymax=ci.upper.mean), position = dodge, width=0.1)+
  scale_fill_manual(values=c("navy","maroon","grey"))+
  ylab("Plural - Singular")+
  ggtitle("JUDGEMENT DIFFERENCE SCORES\n FOR ATTRACTOR NUMBER")

pdf("attractor.diff.judgment.PDF")
attractor.diff
dev.off()

#   PLOT FOR THE CONFIDENCE INTERVALS

ggplot(data=differences.summary.attractor,aes(x=mean.sem,y=Modifier,color = Grammaticality))+
  geom_point()+ 
  theme(text = element_text(size=10))+ 
  geom_errorbarh(aes(xmax = ci.upper.mean, xmin = ci.lower.mean, height=.1))+ 
  theme(axis.title.y = element_blank(), axis.text.y=element_text(colour="Black"))+ 
  scale_x_continuous(name="Number mismatch effect (+/- 95% CI)") +
  scale_y_discrete(limits = rev(levels(differences.summary.attractor$Modifier)) )+
  scale_color_manual(values=c("Blue","Red"))

#   Hypothesis testing

data.acceptability$Modifier <- as.factor(data.acceptability$Modifier)
data.acceptability$Grammaticality <- as.factor(data.acceptability$Grammaticality)
data.acceptability$Attractor <- as.factor(data.acceptability$Attractor)

#   Three way ANOVA

ezANOVA(data = data.acceptability, dv = Response, wid = Subject, within = c(Modifier, Grammaticality, Attractor), detailed = TRUE)

#   WHAT I THINK IS GOING ON:

#   Main effect of grammaticality such that grammatical sentences are rated higher than ungrammatical
#   Main effect of attractor number such that plural is rated higher than singular
#   No main effect of modifier
#
#   Two-way interaction between modifer and grammaticality such that ungrammatical sentences with a PP
#       modifier are rated higher than ungrammatical sentences with an RC modifier
#   Two-way interaction between grammaticality and attractor such that plural attractors are rated
#       higher than singular attractors in ungrammatical conditions
#
#   Three-way interaction between modifier, grammaticality, and attractor number such that the two-way 
#       interaction that lead plural attractors to be rated higher than singular attractors in the 
#       ungrammatical condition is greater for sentences with PP modifiers than RC modifiers


#   Set up and two way ANOVA for ungrammatical conditions only

ungrammatical.data.acceptability <- subset(data.acceptability, Grammaticality == "ungrammatical")

ezANOVA(data=ungrammatical.data.acceptability, dv = Response, wid = Subject, within = c(Modifier,Attractor))

#   Main effect of modifier such that sentences with PPs are rated higher than RCs
#   Main effect of attractor such that plural attractors are rated higher than singular
#   Interaction between modifier type and attactor number such that sentences with plural attractors
#        are rated higher when the attractor is embedded in a PP compared to a RC

#   !!! CAUTION !!! EXPLORATORY:
#   Set up and two way ANOVA for grammatical conditions only

grammatical.data.acceptability <- subset(data.acceptability, Grammaticality == "grammatical")

ezANOVA(data=grammatical.data.acceptability, dv = Response, wid = Subject, within = c(Modifier,Attractor))

#   Main effect of modifier such that sentences with RCs are rated higher than PPs
#   Main effect of attractor such that plural attractors are rated lower than singular
#   No interaction



#   Planned comparisons/contrasts

#   Set up t-test to compare the dfferences between singular and plural in ungrammatical sentences with
#   PP modifiers

ungrammatical.PP.diff <- subset(differences.by.subj.attractor, Modifier == "PP" & Grammaticality == "ungrammatical")
ggplot(ungrammatical.PP.diff,aes(x=Difference))+
  geom_histogram(binwidth=.1)+
  ggtitle("DISTRUBTION OF PL - SG DIFFERENCE SCORES\n FOR UNGRAMMATICAL PP MODIFIERS")

t.test(ungrammatical.PP.diff$Difference)

#   Set up t-test to compare the dfferences between singular and plural in ungrammatical sentences with
#   RC modifiers

ungrammatical.RC.diff  <- subset(differences.by.subj.attractor, Modifier == "RC" & Grammaticality == "ungrammatical")
ggplot(ungrammatical.RC.diff,aes(x=Difference))+
  geom_histogram(binwidth=.1)+
  ggtitle("DISTRUBTION OF PL - SG DIFFERENCE SCORES\n FOR UNGRAMMATICAL RC MODIFIERS")

t.test(ungrammatical.RC.diff$Difference)

#   Set up t-test to compare the dfferences between singular and plural in grammatical sentences with
#   PP modifiers

grammatical.PP.diff <- subset(differences.by.subj.attractor, Modifier == "PP" & Grammaticality == "grammatical")
ggplot(grammatical.PP.diff,aes(x=Difference))+
  geom_histogram(binwidth=.1)+
  ggtitle("DISTRUBTION OF PL - SG DIFFERENCE SCORES\n FOR GRAMMATICAL PP MODIFIERS")

t.test(grammatical.PP.diff$Difference)

#   Set up t-test to compare the dfferences between singular and plural in grammatical sentences with
#   PP modifiers

grammatical.RC.diff <- subset(differences.by.subj.attractor, Modifier == "RC" & Grammaticality == "grammatical")
ggplot(grammatical.RC.diff,aes(x=Difference))+
  geom_histogram(binwidth=.1)+
  ggtitle("DISTRUBTION OF PL - SG DIFFERENCE SCORES\n FOR GRAMMATICAL RC MODIFIERS")

t.test(grammatical.RC.diff$Difference)

#################################################
###                                           ###
###                 SPR Analysis              ###
###                                           ###
#################################################

#   Raw SPR RT distribution for all items and regions

raw.spr.dist <- ggplot(data.spr,aes(x=RT))+
  geom_histogram(binwidth=50)+
  xlim(0,7000)+
  ggtitle("RAW SPR RT DISTRIBUTION")

#   Add log transformed RT as a column in data.spr. This will be used for most analyses

data.spr$logRT <- log(data.spr$RT)

#   Log RT distribution across experimental conditions

log.spr.dist <- ggplot(data.spr,aes(x=logRT))+
  geom_histogram(binwidth=.1)+
  xlim(4,9)+
  ggtitle("SPR LOG(RT) DISTRIBUTION")

pdf("spr.dist.PDF")
grid.arrange(raw.spr.dist,log.spr.dist,ncol = 2)
dev.off()

#   Split the data from region 3 and 4

region3.spr <- droplevels(subset(data.spr, region == 3))
region4.spr <- droplevels(subset(data.spr, region == 4))

#   Separate out the distributions by condition for region 3

PP.attract.spr <- subset(data.spr, Experiment == "cond-H" & region == 3)
RC.attract.spr <- subset(data.spr, Experiment == "cond-D" & region == 3)
PP.gramm.attract.spr <- subset(data.spr, Experiment == "cond-F" & region == 3)
RC.gramm.attract.spr <- subset(data.spr, Experiment == "cond-B" & region == 3)

PP.gramm.spr <- subset(data.spr, Experiment == "cond-E" & region == 3)
PP.ungramm.spr <- subset(data.spr, Experiment == "cond-G" & region == 3)
RC.gramm.spr <- subset(data.spr, Experiment == "cond-A" & region == 3)
RC.ungramm.spr <- subset(data.spr, Experiment == "cond-C" & region == 3)

#   Plot the distributions by condition for region 3

PP.SPP.spr <- ggplot(PP.attract.spr,aes(x=RT))+
  geom_histogram(binwidth=50)+
  xlim(0,7000)+
  ggtitle("SPR RT DISTRIBUTION\n FOR PP SPP REGION 3")
RC.SPP.spr <- ggplot(RC.attract.spr,aes(x=RT))+
  geom_histogram(binwidth=50)+
  xlim(0,7000)+
  ggtitle("SPR RT DISTRIBUTION\n FOR RC SPP REGION 3")
PP.SSS.spr <- ggplot(PP.gramm.spr,aes(x=RT))+
  geom_histogram(binwidth=50)+
  xlim(0,7000)+
  ggtitle("SPR RT DISTRIBUTION\n FOR PP SSS REGION 3")
RC.SSS.spr <- ggplot(RC.gramm.spr,aes(x=RT))+
  geom_histogram(binwidth=50)+
  xlim(0,7000)+
  ggtitle("SPR RT DISTRIBUTION\n FOR RC SSS REGION 3")
PP.SSP.spr <- ggplot(PP.ungramm.spr,aes(x=RT))+
  geom_histogram(binwidth=50)+
  xlim(0,7000)+
  ggtitle("SPR RT DISTRIBUTION\n FOR PP SSP REGION 3")
RC.SSP.spr <- ggplot(RC.ungramm.spr,aes(x=RT))+
  geom_histogram(binwidth=50)+
  xlim(0,7000)+
  ggtitle("SPR RT DISTRIBUTION\n FOR RC SSP REGION 3")
PP.SPS.spr <- ggplot(PP.gramm.attract.spr,aes(x=RT))+
  geom_histogram(binwidth=50)+
  xlim(0,7000)+
  ggtitle("SPR RT DISTRIBUTION\n FOR PP SPS REGION 3")
RC.SPS.spr <- ggplot(RC.gramm.attract.spr,aes(x=RT))+
  geom_histogram(binwidth=50)+
  xlim(0,7000)+
  ggtitle("SPR RT DISTRIBUTION\n FOR RC SPS REGION 3")
grid.arrange(PP.SSS.spr,PP.SSP.spr,PP.SPS.spr,PP.SPP.spr,RC.SSS.spr,RC.SSP.spr,RC.SPS.spr, RC.SPP.spr,ncol = 4, nrow = 2)

#   Condition summaries for raw RT: Gives mean and SEM by region. This is plotted later

RT.subj.by.cond <- data.spr %>%
  group_by(Subject, Experiment, Modifier, Grammaticality, Attractor, region) %>%
  summarise(average = mean(RT))

RT.cond.summ <- RT.subj.by.cond %>%
  group_by(Experiment, Modifier, Grammaticality, Attractor, region) %>%
  summarise(mean = mean(average),
            SEM = sd(average)/sqrt(n_distinct(Subject)))


#   Summaries by condition and region for log transformed RT

log.RT.subj.by.cond <- data.spr %>%
  group_by(Subject, Experiment, Modifier, Grammaticality, Attractor, region) %>%
  summarise(average = mean(logRT))

log.RT.cond.summ <- log.RT.subj.by.cond %>%
  group_by(Experiment, Modifier, Grammaticality, Attractor, region) %>%
  summarise(mean = mean(average),
            SEM = sd(average)/sqrt(n_distinct(Subject)))

#   Difference scores and CIs for attractor number for region 3 using log(RT)

log.RT.differences.by.subj.attractor <- region3.spr %>% 
  group_by(Subject,Modifier,Grammaticality) %>%
  filter(Attractor=='SG') %>%
  summarise(SG.mean = mean(logRT))

log.RT.differences.by.subj.attractor <- region3.spr %>% 
  group_by(Subject,Modifier,Grammaticality) %>%
  filter(Attractor=='PL') %>%
  summarise(PL.mean = mean(logRT)) %>%
  right_join(log.RT.differences.by.subj.attractor)

log.RT.differences.by.subj.attractor <- log.RT.differences.by.subj.attractor %>%
  group_by(Subject,Modifier,Grammaticality) %>%
  summarise(Difference = PL.mean - SG.mean) %>%
  right_join(log.RT.differences.by.subj.attractor)

log.RT.differences.summary.attractor <- log.RT.differences.by.subj.attractor %>%
  group_by(Modifier,Grammaticality) %>%
  summarise(
    N = n_distinct(Subject),
    mean.diff = mean(PL.mean-SG.mean),
    mean.sem = sd(PL.mean-SG.mean)/sqrt(n_distinct(Subject))) %>%
  mutate(ci.lower.mean = mean.diff - qt(.975,df=N-1)*mean.sem,
         ci.upper.mean = mean.diff + qt(.975,df=N-1)*mean.sem
  )

#   Difference scores and CIs for attractor number for region 4 using log RT

log.RT.differences.by.subj.attractor.r4 <- region4.spr %>% 
  group_by(Subject,Modifier,Grammaticality) %>%
  filter(Attractor=='SG') %>%
  summarise(SG.mean = mean(logRT))

log.RT.differences.by.subj.attractor.r4 <- region4.spr %>% 
  group_by(Subject,Modifier,Grammaticality) %>%
  filter(Attractor=='PL') %>%
  summarise(PL.mean = mean(logRT)) %>%
  right_join(log.RT.differences.by.subj.attractor.r4)

log.RT.differences.by.subj.attractor.r4 <- log.RT.differences.by.subj.attractor.r4 %>%
  group_by(Subject,Modifier,Grammaticality) %>%
  summarise(Difference = PL.mean - SG.mean) %>%
  right_join(log.RT.differences.by.subj.attractor.r4)

log.RT.differences.summary.attractor.r4 <- log.RT.differences.by.subj.attractor.r4 %>%
  group_by(Modifier,Grammaticality) %>%
  summarise(
    N = n_distinct(Subject),
    mean.diff = mean(PL.mean-SG.mean),
    mean.sem = sd(PL.mean-SG.mean)/sqrt(n_distinct(Subject))) %>%
  mutate(ci.lower.mean = mean.diff - qt(.975,df=N-1)*mean.sem,
         ci.upper.mean = mean.diff + qt(.975,df=N-1)*mean.sem
  )

#### Plotting raw RT data

RC.spr.raw <- ggplot(subset(RT.cond.summ,Experiment %in% c("cond-A","cond-B","cond-C","cond-D")),aes(x=region,y=mean,color=Experiment,base=6,group=Experiment))+ 
  labs(y="Reading time",x="Region",group=1) +geom_point(stat = "identity",size=1)+
  geom_errorbar(aes(ymax = mean+SEM,ymin=mean-SEM,width=0.05))+ 
  theme(text = element_text(size=10))+stat_identity(geom="line")+
  scale_colour_manual(values = c("steelblue2","navyblue","firebrick2","firebrick4"),
                      name="Conditions",
                      labels= c("Grammatical, SG","Grammatical, PL","Ungrammatical, SG","Ungrammatical, PL"),
                      breaks= c("cond-A","cond-B","cond-C","cond-D"))+
  ylim(500,1500)+
  ggtitle("RC-MODIFIER SPR RAW RT")


PP.spr.raw <- ggplot(subset(RT.cond.summ,Experiment %in% c("cond-E","cond-F","cond-G","cond-H")),aes(x=region,y=mean,color=Experiment,base=6,group=Experiment))+ 
  labs(y="Reading time",x="Region",group=1) +geom_point(stat = "identity",size=1)+
  geom_errorbar(aes(ymax = mean+SEM,ymin=mean-SEM,width=0.05))+ 
  theme(text = element_text(size=10))+stat_identity(geom="line")+
  scale_colour_manual(values = c("steelblue2","navyblue","firebrick2","firebrick4"),
                      name="Conditions",
                      labels= c("Grammatical, SG","Grammatical, PL","Ungrammatical, SG","Ungrammatical, PL"),
                      breaks= c("cond-E","cond-F","cond-G","cond-H"))+
  ylim(500,1500)+
  ggtitle("PP-MODIFIER SPR RAW RT")

pdf(file = "raw-spr.PDF", width = 20, height = 10)
grid.arrange(PP.spr.raw,RC.spr.raw,ncol=2)
dev.off()

#### Plotting log transformed RT data

log.RC.spr <- ggplot(subset(log.RT.cond.summ,Experiment %in% c("cond-A","cond-B","cond-C","cond-D")),aes(x=region,y=mean,color=Experiment,base=6,group=Experiment))+ 
  labs(y="Reading time",x="Region",group=1) +geom_point(stat = "identity",size=1)+
  geom_errorbar(aes(ymax = mean+SEM,ymin=mean-SEM,width=0.05))+ 
  theme(text = element_text(size=10))+stat_identity(geom="line")+
  scale_colour_manual(values = c("steelblue2","navyblue","firebrick2","firebrick4"),
                      name="Conditions",
                      labels= c("Grammatical, SG","Grammatical, PL","Ungrammatical, SG","Ungrammatical, PL"),
                      breaks= c("cond-A","cond-B","cond-C","cond-D"))+
  ylim(6.25,7.25)+
  ggtitle("RC-MODIFIER SPR LOG TRANSFORMED RT")

log.PP.spr <- ggplot(subset(log.RT.cond.summ,Experiment %in% c("cond-E","cond-F","cond-G","cond-H")),aes(x=region,y=mean,color=Experiment,base=6,group=Experiment))+ 
  labs(y="Reading time",x="Region",group=1) +geom_point(stat = "identity",size=1)+
  geom_errorbar(aes(ymax = mean+SEM,ymin=mean-SEM,width=0.05))+ 
  theme(text = element_text(size=10))+stat_identity(geom="line")+
  scale_colour_manual(values = c("steelblue2","navyblue","firebrick2","firebrick4"),
                      name="Conditions",
                      labels= c("Grammatical, SG","Grammatical, PL","Ungrammatical, SG","Ungrammatical, PL"),
                      breaks= c("cond-E","cond-F","cond-G","cond-H"))+
  ylim(6.25,7.25)+
  ggtitle("PP-MODIFIER SPR LOG TRANSFORMED RT")

pdf(file = "log-spr.PDF", width = 20, height = 10)
grid.arrange(log.PP.spr,log.RC.spr,ncol=2)
dev.off()

#   Difference score plotting for regions 3 and 4 for attractor number

difference.region3 <- ggplot(aes(y = mean.diff, x = Modifier, fill = Grammaticality), data = log.RT.differences.summary.attractor)+
  geom_bar(position='dodge',stat = "identity")+
  geom_errorbar(aes(ymin=ci.lower.mean, ymax=ci.upper.mean), position = dodge, width=0.1)+
  scale_fill_manual(values=c("navy","maroon","grey"))+
  ylab("Plural - Singular")+
  ggtitle("LOG RT DIFFERENCE SCORES\n FOR ATTRACTOR NUMBER IN REGION 3")

difference.region4 <- ggplot(aes(y = mean.diff, x = Modifier, fill = Grammaticality), data = log.RT.differences.summary.attractor.r4)+
  geom_bar(position='dodge',stat = "identity")+
  geom_errorbar(aes(ymin=ci.lower.mean, ymax=ci.upper.mean), position = dodge, width=0.1)+
  scale_fill_manual(values=c("navy","maroon","grey"))+
  ylab("Plural - Singular")+
  ggtitle("LOG RT DIFFERENCE SCORES\n FOR ATTRACTOR NUMBER IN REGION 4")

pdf(file = "difference-attractor-spr.PDF",height = 5, width = 10)
grid.arrange(difference.region3,difference.region4,ncol=2)
dev.off()

#   Hypothesis testing for SPR

#   2x2x2 ANOVAs on Region 3 and Region 4

ezANOVA(region3.spr, dv = logRT, wid = Subject, within = c(Grammaticality,Modifier,Attractor))

ezANOVA(region4.spr, dv = logRT, wid = Subject, within = c(Grammaticality,Modifier,Attractor))

#   2x2 ANOVAs on the ungrammatical trials only for regions 3 and 4

region3.ungrammatical.spr <- droplevels(subset(region3.spr, Grammaticality == "ungrammatical"))
region4.ungrammatical.spr <- droplevels(subset(region4.spr, Grammaticality == "ungrammatical"))

ezANOVA(region3.ungrammatical.spr, dv = logRT, wid = Subject, within = c(Modifier, Attractor))
ezANOVA(region4.ungrammatical.spr, dv = logRT, wid = Subject, within = c(Modifier, Attractor))

#   2x2 ANOVA on the grammatical trials only for regions 3 and 4

region3.grammatical.spr <- droplevels(subset(region3.spr, Grammaticality == "grammatical"))
region4.grammatical.spr <- droplevels(subset(region4.spr, Grammaticality == "grammatical"))

ezANOVA(region3.grammatical.spr, dv = logRT, wid = Subject, within = c(Modifier, Attractor))
ezANOVA(region4.grammatical.spr, dv = logRT, wid = Subject, within = c(Modifier, Attractor))

#   Planned comparisons/contrasts for SPR

#   Set up t-test to compare the dfferences between singular and plural in ungrammatical sentences with
#   PP modifiers in log RT in region 3

log.RT.ungrammatical.PP.diff <- subset(log.RT.differences.by.subj.attractor, Modifier == "PP" & Grammaticality == "ungrammatical")
ggplot(log.RT.ungrammatical.PP.diff,aes(x=Difference))+
  geom_histogram(binwidth=.1)+
  ggtitle("DISTRUBTION OF PL - SG LOG RT DIFFERENCE SCORES\n FOR UNGRAMMATICAL PP MODIFIERS")

t.test(log.RT.ungrammatical.PP.diff$Difference)

#   Set up t-test to compare the dfferences between singular and plural in ungrammatical sentences with
#   RC modifiers in log RT in region 3

log.RT.ungrammatical.RC.diff  <- subset(log.RT.differences.by.subj.attractor, Modifier == "RC" & Grammaticality == "ungrammatical")
ggplot(log.RT.ungrammatical.RC.diff,aes(x=Difference))+
  geom_histogram(binwidth=.1)+
  ggtitle("DISTRUBTION OF PL - SG LOG RT DIFFERENCE SCORES\n FOR UNGRAMMATICAL RC MODIFIERS")

t.test(log.RT.ungrammatical.RC.diff$Difference)

#   Set up t-test to compare the dfferences between singular and plural in grammatical sentences with
#   PP modifiers in log RT in region 3

log.RT.grammatical.PP.diff <- subset(log.RT.differences.by.subj.attractor, Modifier == "PP" & Grammaticality == "grammatical")
ggplot(log.RT.grammatical.PP.diff,aes(x=Difference))+
  geom_histogram(binwidth=.1)+
  ggtitle("DISTRUBTION OF PL - SG LOG RT DIFFERENCE SCORES\n FOR GRAMMATICAL PP MODIFIERS")

t.test(log.RT.grammatical.PP.diff$Difference)

#   Set up t-test to compare the dfferences between singular and plural in grammatical sentences with
#   PP modifiers in log RT in region 3

log.RT.grammatical.RC.diff <- subset(log.RT.differences.by.subj.attractor, Modifier == "RC" & Grammaticality == "grammatical")
ggplot(log.RT.grammatical.RC.diff,aes(x=Difference))+
  geom_histogram(binwidth=.1)+
  ggtitle("DISTRUBTION OF PL - SG LOG RT DIFFERENCE SCORES\n FOR GRAMMATICAL RC MODIFIERS")

t.test(log.RT.grammatical.RC.diff$Difference)

#   Set up t-test to compare the dfferences between singular and plural in ungrammatical sentences with
#   PP modifiers in log RT in region 4

log.RT.ungrammatical.PP.diff.r4 <- subset(log.RT.differences.by.subj.attractor.r4, Modifier == "PP" & Grammaticality == "ungrammatical")
ggplot(log.RT.ungrammatical.PP.diff,aes(x=Difference))+
  geom_histogram(binwidth=.1)+
  ggtitle("DISTRUBTION OF PL - SG LOG RT DIFFERENCE SCORES\n FOR UNGRAMMATICAL PP MODIFIERS")

t.test(log.RT.ungrammatical.PP.diff.r4$Difference)

#   Set up t-test to compare the dfferences between singular and plural in ungrammatical sentences with
#   RC modifiers in log RT in region 4

log.RT.ungrammatical.RC.diff.r4  <- subset(log.RT.differences.by.subj.attractor.r4, Modifier == "RC" & Grammaticality == "ungrammatical")
ggplot(log.RT.ungrammatical.RC.diff,aes(x=Difference))+
  geom_histogram(binwidth=.1)+
  ggtitle("DISTRUBTION OF PL - SG LOG RT DIFFERENCE SCORES\n FOR UNGRAMMATICAL RC MODIFIERS")

t.test(log.RT.ungrammatical.RC.diff.r4$Difference)

#   Set up t-test to compare the dfferences between singular and plural in grammatical sentences with
#   PP modifiers in log RT in region 4

log.RT.grammatical.PP.diff.r4 <- subset(log.RT.differences.by.subj.attractor.r4, Modifier == "PP" & Grammaticality == "grammatical")
ggplot(log.RT.grammatical.PP.diff,aes(x=Difference))+
  geom_histogram(binwidth=.1)+
  ggtitle("DISTRUBTION OF PL - SG LOG RT DIFFERENCE SCORES\n FOR GRAMMATICAL PP MODIFIERS")

t.test(log.RT.grammatical.PP.diff.r4$Difference)

#   Set up t-test to compare the dfferences between singular and plural in grammatical sentences with
#   PP modifiers in log RT in region 4

log.RT.grammatical.RC.diff.r4 <- subset(log.RT.differences.by.subj.attractor.r4, Modifier == "RC" & Grammaticality == "grammatical")
ggplot(log.RT.grammatical.RC.diff.r4,aes(x=Difference))+
  geom_histogram(binwidth=.1)+
  ggtitle("DISTRUBTION OF PL - SG LOG RT DIFFERENCE SCORES\n FOR GRAMMATICAL RC MODIFIERS")

t.test(log.RT.grammatical.RC.diff.r4$Difference)

#################################################
###                                           ###
###           Contingency Analysis            ###
###                                           ###
#################################################

quantile(data.acceptability$z, probs = c(0.33,0.66))

#   25%          75% 
#   -0.8150370   0.8476354 

grammatical.trials <- droplevels(subset(data.acceptability, data.acceptability$z > 0.6235734))
ungrammatical.trials <- droplevels(subset(data.acceptability, data.acceptability$z < -0.4971533))

summary(grammatical.trials$Experiment)
summary(ungrammatical.trials$Experiment)

#   Bin each SPR trial into gramamtical and ungrammatical bins depending on judgment

data.spr$bin <- ifelse(data.spr$ID %in% grammatical.trials$ID,"grammatical", ifelse(data.spr$ID %in% ungrammatical.trials$ID,"ungrammatical","middle"))

data.spr.trimmed <- droplevels(subset(data.spr, bin != "middle"))
#   FIGURE OUT HOW IT MAKES SENSE TO ACTUALLY ANALYZE AND PRESENT THIS

#   Isolate the SPP trials and see if differences emerge depending on binning

data.spr.SPP <- droplevels(subset(data.spr.trimmed, Experiment == "cond-D" | Experiment == "cond-H"))

SPP.log.RT.subj.by.cond <- data.spr.SPP %>%
  group_by(Subject, bin, Modifier, region) %>%
  summarise(average = mean(logRT))

SPP.log.RT.cond.summ <- SPP.log.RT.subj.by.cond %>%
  group_by(bin,Modifier, region) %>%
  summarise(mean = mean(average),
            SEM = sd(average)/sqrt(n_distinct(Subject)))

SPP.log.RT.cond.summ$ID <- paste(SPP.log.RT.cond.summ$bin,SPP.log.RT.cond.summ$Modifier)

SPP.contigency <- ggplot(SPP.log.RT.cond.summ,aes(x=region,y=mean,color=ID,base=6,group=ID))+ 
  labs(y="Reading time",x="Region",group=1) +
  geom_point(stat = "identity",size=1)+
  geom_errorbar(aes(ymax = mean+SEM,ymin=mean-SEM,width=0.05))+ 
  theme(text = element_text(size=10))+
  stat_identity(geom="line")+
  scale_colour_manual(values = c("steelblue2","firebrick2","navyblue","firebrick4"),
                      name="Bin + Modifier",
                      labels= c("grammatical PP","ungrammatical PP","grammatical RC","ungrammatical RC"),
                      breaks= c("grammatical PP","ungrammatical PP","grammatical RC","ungrammatical RC"))+
  ylim(6,7.25)+
  ggtitle("SPR LOG TRANSFORMED RT\nSPP CONDITION BY BIN AND MODIFIER")

pdf("spp.contingency.PDF")
SPP.contigency
dev.off()

#   The generalization here is that in the ungrammatical *SPP condition, in the cases where the sentence
#   was ultimately judged like an ungrammatical sentence, the sentence was read slower in the region
#   containing the verb. The cases where it was judged as a grammatical sentence diverge. When there is
#   a PP modifier the critical region is read more slowly relative to the cases with an RC modifier.

#   Isolate the SPS trials and see if differences emerge depending on binning

data.spr.SPS <- droplevels(subset(data.spr.trimmed, Experiment == "cond-B" | Experiment == "cond-F"))

SPS.log.RT.subj.by.cond <- data.spr.SPS %>%
  group_by(Subject, bin, Modifier, region) %>%
  summarise(average = mean(logRT))

SPS.log.RT.cond.summ <- SPS.log.RT.subj.by.cond %>%
  group_by(bin,Modifier, region) %>%
  summarise(mean = mean(average),
            SEM = sd(average)/sqrt(n_distinct(Subject)))

SPS.log.RT.cond.summ$ID <- paste(SPS.log.RT.cond.summ$bin,SPS.log.RT.cond.summ$Modifier)

SPS.contingency <- ggplot(SPS.log.RT.cond.summ,aes(x=region,y=mean,color=ID,base=6,group=ID))+ 
  labs(y="Reading time",x="Region",group=1) +
  geom_point(stat = "identity",size=1)+
  geom_errorbar(aes(ymax = mean+SEM,ymin=mean-SEM,width=0.05))+ 
  theme(text = element_text(size=10))+
  stat_identity(geom="line")+
  scale_colour_manual(values = c("steelblue2","firebrick2","navyblue","firebrick4"),
                      name="Bin + Modifier",
                      labels= c("grammatical PP","ungrammatical PP","grammatical RC","ungrammatical RC"),
                      breaks= c("grammatical PP","ungrammatical PP","grammatical RC","ungrammatical RC"))+
  ylim(6,7.25)+
  ggtitle("SPR LOG TRANSFORMED RT\nSPS CONDITION BY BIN AND MODIFIER")

pdf("sps.contingency.PDF")
SPS.contingency
dev.off()

#   z-scoring and binning by condition

PP.SSS$z.by.cond <- ave(PP.SSS$Response, PP.SSS$Subject,FUN = scale)
PP.SPS$z.by.cond <- ave(PP.SPS$Response, PP.SPS$Subject,FUN = scale)
PP.SSP$z.by.cond <- ave(PP.SSP$Response, PP.SSP$Subject,FUN = scale)
PP.SPP$z.by.cond <- ave(PP.SPP$Response, PP.SPP$Subject,FUN = scale)

RC.SSS$z.by.cond <- ave(RC.SSS$Response, RC.SSS$Subject,FUN = scale)
RC.SPS$z.by.cond <- ave(RC.SPS$Response, RC.SPS$Subject,FUN = scale)
RC.SSP$z.by.cond <- ave(RC.SSP$Response, RC.SSP$Subject,FUN = scale)
RC.SPP$z.by.cond <- ave(RC.SPP$Response, RC.SPP$Subject,FUN = scale)

all.trials <- rbind(PP.SSS,PP.SPS,PP.SSP,PP.SPP,RC.SSS,RC.SPS,RC.SSP,RC.SPP)
all.trials$z.by.cond <- as.numeric(all.trials$z.by.cond)

ggplot(PP.SPP,aes(x=z.by.cond))+
  geom_histogram(binwidth=.1)+
  xlim(-3,3)+
  ggtitle("SPR LOG(RT) DISTRIBUTION")

quantile(all.trials$z.by.cond, probs = c(0.33,0.66),na.rm = TRUE)

grammatical.trials <- droplevels(subset(all.trials, all.trials$z.by.cond > 0.5201565))
ungrammatical.trials <- droplevels(subset(all.trials, all.trials$z.by.cond < -0.4082483))

data.spr$bin <- ifelse(data.spr$ID %in% grammatical.trials$ID,"grammatical", ifelse(data.spr$ID %in% ungrammatical.trials$ID,"ungrammatical","middle"))
data.spr.trimmed <- droplevels(subset(data.spr, bin != "middle"))

data.spr.SPP <- droplevels(subset(data.spr.trimmed, Experiment == "cond-D" | Experiment == "cond-H"))

SPP.log.RT.subj.by.cond <- data.spr.SPP %>%
  group_by(Subject, bin, Modifier, region) %>%
  summarise(average = mean(logRT))

SPP.log.RT.cond.summ <- SPP.log.RT.subj.by.cond %>%
  group_by(bin,Modifier, region) %>%
  summarise(mean = mean(average),
            SEM = sd(average)/sqrt(n_distinct(Subject)))

SPP.log.RT.cond.summ$ID <- paste(SPP.log.RT.cond.summ$bin,SPP.log.RT.cond.summ$Modifier)

ggplot(SPP.log.RT.cond.summ,aes(x=region,y=mean,color=ID,base=6,group=ID))+ 
  labs(y="Reading time",x="Region",group=1) +
  geom_point(stat = "identity",size=1)+
  geom_errorbar(aes(ymax = mean+SEM,ymin=mean-SEM,width=0.05))+ 
  theme(text = element_text(size=10))+
  stat_identity(geom="line")+
  scale_colour_manual(values = c("steelblue2","firebrick2","navyblue","firebrick4"),
                      name="Modifier",
                      labels= c("grammatical PP","ungrammatical PP","grammatical RC","ungrammatical RC"),
                      breaks= c("grammatical PP","ungrammatical PP","grammatical RC","ungrammatical RC"))+
  ylim(6.25,7.25)+
  ggtitle("SPR LOG TRANSFORMED RT\nSPP CONDITION BY BIN AND MODIFIER")

#   Isolate the SPS trials and see if differences emerge depending on binning

data.spr.SPS <- droplevels(subset(data.spr.trimmed, Experiment == "cond-B" | Experiment == "cond-F"))

SPS.log.RT.subj.by.cond <- data.spr.SPS %>%
  group_by(Subject, bin, Modifier, region) %>%
  summarise(average = mean(logRT))

SPS.log.RT.cond.summ <- SPS.log.RT.subj.by.cond %>%
  group_by(bin,Modifier, region) %>%
  summarise(mean = mean(average),
            SEM = sd(average)/sqrt(n_distinct(Subject)))

SPS.log.RT.cond.summ$ID <- paste(SPS.log.RT.cond.summ$bin,SPS.log.RT.cond.summ$Modifier)

ggplot(SPS.log.RT.cond.summ,aes(x=region,y=mean,color=ID,base=6,group=ID))+ 
  labs(y="Reading time",x="Region",group=1) +
  geom_point(stat = "identity",size=1)+
  geom_errorbar(aes(ymax = mean+SEM,ymin=mean-SEM,width=0.05))+ 
  theme(text = element_text(size=10))+
  stat_identity(geom="line")+
  scale_colour_manual(values = c("steelblue2","firebrick2","navyblue","firebrick4"),
                      name="Modifier",
                      labels= c("grammatical PP","ungrammatical PP","grammatical RC","ungrammatical RC"),
                      breaks= c("grammatical PP","ungrammatical PP","grammatical RC","ungrammatical RC"))+
  ylim(6.25,7.25)+
  ggtitle("SPR LOG TRANSFORMED RT\nSPS CONDITION BY BIN AND MODIFIER")

#   Correlation between z-score and RT

#   Add the z-score value associated with the trial to the SPR data frame

data.spr.spread <- data.spr %>%
  select(Subject,ID,bin, region,logRT) %>%
  spread(region,logRT) %>%
  droplevels()

spr.plus.judgment <- all.trials %>%
  right_join(data.spr.spread)

spr.plus.judgment$bin <- as.factor(spr.plus.judgment$bin)

#   All experimental trials correlation between RT and judgments

scatterplot(spr.plus.judgment$z,spr.plus.judgment$`3`,
            xlab="Judgement Z-Scores", ylab="Log RT",
            main="Log RT by Z-Scores in All Conditions")
cor(spr.plus.judgment$z,spr.plus.judgment$`3`)

#   Correlation between RT and judgments of all trials of *SPP PP condition

spr.plus.judgment.SPP.PP <- droplevels(subset(spr.plus.judgment, Experiment == "cond-H"))
with(spr.plus.judgment.SPP.PP, 
     scatterplot(z,`3`,
            xlab="Judgement Z-Scores", ylab="Log RT",
            main="Log RT by Z-Scores in *SPP with PP modifier",
            col = bin))


cor(spr.plus.judgment.SPP.PP$z,spr.plus.judgment.SPP.PP$`3`)

#   Correlation between RT and judgments of all trials of *SPP RC condition

spr.plus.judgment.SPP.RC <- droplevels(subset(spr.plus.judgment, Experiment == "cond-D"))
scatterplot(spr.plus.judgment.SPP.RC$z,spr.plus.judgment.SPP.RC$`3`,
            xlab="Judgement Z-Scores", ylab="Log RT",
            main="Log RT by Z-Scores in *SPP with RC modifier")


cor(spr.plus.judgment.SPP.RC$z,spr.plus.judgment.SPP.RC$`3`)

#   Correlation between RT and judgments of all trials of SPS PP condition

spr.plus.judgment.SPS.PP <- droplevels(subset(spr.plus.judgment, Experiment == "cond-F"))
scatterplot(spr.plus.judgment.SPS.PP$z,spr.plus.judgment.SPS.PP$`3`,
            xlab="Judgement Z-Scores", ylab="Log RT",
            main="Log RT by Z-Scores in SPS with PP modifier")
cor(spr.plus.judgment.SPS.PP$z,spr.plus.judgment.SPS.PP$`3`)

#   Correlation between RT and judgments of all trials of SPS RC condition

spr.plus.judgment.SPS.RC <- droplevels(subset(spr.plus.judgment, Experiment == "cond-B"))
scatterplot(spr.plus.judgment.SPS.RC$z,spr.plus.judgment.SPS.RC$`3`,
            xlab="Judgement Z-Scores", ylab="Log RT",
            main="Log RT by Z-Scores in SPS with RC modifier")
cor(spr.plus.judgment.SPS.RC$z,spr.plus.judgment.SPS.RC$`3`)

#################################################
###                                           ###
###             Verb Type Analysis            ###
###                                           ###
#################################################

be.past.data.acceptability <- subset(data.acceptability, Verb == "was/were")
be.pres.data.acceptability <- subset(data.acceptability, Verb == "is/are")
lexical.data.acceptability <- subset(data.acceptability, Verb == "lexical")
has.have.data.acceptability <- subset(data.acceptability, Verb == "has/have")

#   calculate means and SEM by condition for was/were

be.past.subj.by.cond <- be.past.data.acceptability %>% 
  group_by(Subject, Modifier, Grammaticality, Attractor) %>% 
  summarise(mean.resp = mean(Response))

be.past.cond.summ <- be.past.subj.by.cond %>%
  group_by(Modifier, Grammaticality, Attractor) %>%
  summarise(mean_cond = mean(mean.resp),
            SEM = sd(mean.resp)/sqrt(n_distinct(Subject)))

RC.be.past.summ <- subset(be.past.cond.summ, Modifier == "RC")
PP.be.past.summ <- subset(be.past.cond.summ, Modifier == "PP")

#   calculate means and SEM by condition for is/are

be.pres.subj.by.cond <- be.pres.data.acceptability %>% 
  group_by(Subject, Modifier, Grammaticality, Attractor) %>% 
  summarise(mean.resp = mean(Response))

be.pres.cond.summ <- be.pres.subj.by.cond %>%
  group_by(Modifier, Grammaticality, Attractor) %>%
  summarise(mean_cond = mean(mean.resp),
            SEM = sd(mean.resp)/sqrt(n_distinct(Subject)))

RC.be.pres.summ <- subset(be.pres.cond.summ, Modifier == "RC")
PP.be.pres.summ <- subset(be.pres.cond.summ, Modifier == "PP")

#   calculate means and SEM by condition for lexical verbs

lexical.subj.by.cond <- lexical.data.acceptability %>% 
  group_by(Subject, Modifier, Grammaticality, Attractor) %>% 
  summarise(mean.resp = mean(Response))

lexical.cond.summ <- lexical.subj.by.cond %>%
  group_by(Modifier, Grammaticality, Attractor) %>%
  summarise(mean_cond = mean(mean.resp),
            SEM = sd(mean.resp)/sqrt(n_distinct(Subject)))

RC.lexical.summ <- subset(lexical.cond.summ, Modifier == "RC")
PP.lexical.summ <- subset(lexical.cond.summ, Modifier == "PP")

#   calculate means and SEM by condition for has/have

has.have.subj.by.cond <- has.have.data.acceptability %>% 
  group_by(Subject, Modifier, Grammaticality, Attractor) %>% 
  summarise(mean.resp = mean(Response))

has.have.cond.summ <- has.have.subj.by.cond %>%
  group_by(Modifier, Grammaticality, Attractor) %>%
  summarise(mean_cond = mean(mean.resp),
            SEM = sd(mean.resp)/sqrt(n_distinct(Subject)))

RC.has.have.summ <- subset(has.have.cond.summ, Modifier == "RC")
PP.has.have.summ <- subset(has.have.cond.summ, Modifier == "PP")

#   plotting verb stuff

#   was/were

be.past.RC <- ggplot(RC.be.past.summ,aes(x=Grammaticality,y=mean_cond,fill=Attractor))+
  geom_bar(position = 'dodge',stat = "identity")+
  geom_linerange(stat='identity',position = position_dodge(width = 0.9),mapping=aes(ymax = mean_cond+SEM,ymin=mean_cond-SEM)) +
  scale_fill_manual(values=c("navy","maroon"))+
  ylab("MEAN RATING")+
  ylim(0,7)+
  ggtitle("RC Modifier")
be.past.PP <- ggplot(PP.be.past.summ,aes(x=Grammaticality,y=mean_cond,fill=Attractor))+
  geom_bar(position = 'dodge',stat = "identity")+
  geom_linerange(stat='identity',position = position_dodge(width = 0.9),mapping=aes(ymax = mean_cond+SEM,ymin=mean_cond-SEM)) +
  scale_fill_manual(values=c("navy","maroon"))+
  ylab("MEAN RATING")+
  ylim(0,7)+
  ggtitle("PP Modifier")
be.past <- grid.arrange(be.past.PP, be.past.RC, ncol=2, top=textGrob("Verb Type: was/were", gp=gpar(fontsize=15,font=8)))

#   is/are

be.pres.RC <- ggplot(RC.be.pres.summ,aes(x=Grammaticality,y=mean_cond,fill=Attractor))+
  geom_bar(position = 'dodge',stat = "identity")+
  geom_linerange(stat='identity',position = position_dodge(width = 0.9),mapping=aes(ymax = mean_cond+SEM,ymin=mean_cond-SEM)) +
  scale_fill_manual(values=c("navy","maroon"))+
  ylab("MEAN RATING")+
  ylim(0,7)+
  ggtitle("RC Modifier")
be.pres.PP <- ggplot(PP.be.pres.summ,aes(x=Grammaticality,y=mean_cond,fill=Attractor))+
  geom_bar(position = 'dodge',stat = "identity")+
  geom_linerange(stat='identity',position = position_dodge(width = 0.9),mapping=aes(ymax = mean_cond+SEM,ymin=mean_cond-SEM)) +
  scale_fill_manual(values=c("navy","maroon"))+
  ylab("MEAN RATING")+
  ylim(0,7)+
  ggtitle("PP Modifier")
be.pres <- grid.arrange(be.pres.PP, be.pres.RC, ncol=2, top=textGrob("Verb Type: is/are", gp=gpar(fontsize=15,font=8)))

#   lexical

lexical.RC <- ggplot(RC.lexical.summ,aes(x=Grammaticality,y=mean_cond,fill=Attractor))+
  geom_bar(position = 'dodge',stat = "identity")+
  geom_linerange(stat='identity',position = position_dodge(width = 0.9),mapping=aes(ymax = mean_cond+SEM,ymin=mean_cond-SEM)) +
  scale_fill_manual(values=c("navy","maroon"))+
  ylab("MEAN RATING")+
  ylim(0,7)+
  ggtitle("RC Modifier")
lexical.PP <- ggplot(PP.lexical.summ,aes(x=Grammaticality,y=mean_cond,fill=Attractor))+
  geom_bar(position = 'dodge',stat = "identity")+
  geom_linerange(stat='identity',position = position_dodge(width = 0.9),mapping=aes(ymax = mean_cond+SEM,ymin=mean_cond-SEM)) +
  scale_fill_manual(values=c("navy","maroon"))+
  ylab("MEAN RATING")+
  ylim(0,7)+
  ggtitle("PP Modifier")
lexical <- grid.arrange(lexical.PP, lexical.RC, ncol=2, top=textGrob("Verb Type: Lexical", gp=gpar(fontsize=15,font=8)))

#   has/have

has.have.RC <- ggplot(RC.has.have.summ,aes(x=Grammaticality,y=mean_cond,fill=Attractor))+
  geom_bar(position = 'dodge',stat = "identity")+
  geom_linerange(stat='identity',position = position_dodge(width = 0.9),mapping=aes(ymax = mean_cond+SEM,ymin=mean_cond-SEM)) +
  scale_fill_manual(values=c("navy","maroon"))+
  ylab("MEAN RATING")+
  ylim(0,7)+
  ggtitle("RC Modifier")
has.have.PP <- ggplot(PP.has.have.summ,aes(x=Grammaticality,y=mean_cond,fill=Attractor))+
  geom_bar(position = 'dodge',stat = "identity")+
  geom_linerange(stat='identity',position = position_dodge(width = 0.9),mapping=aes(ymax = mean_cond+SEM,ymin=mean_cond-SEM)) +
  scale_fill_manual(values=c("navy","maroon"))+
  ylab("MEAN RATING")+
  ylim(0,7)+
  ggtitle("PP Modifier")
has <- grid.arrange(has.have.PP, has.have.RC, ncol=2, top=textGrob("Verb Type: has/have", gp=gpar(fontsize=15,font=8)))

grid.arrange(be.pres,be.past,lexical,has, ncol = 2, nrow = 2)
