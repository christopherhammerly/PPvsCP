##########################################################################
######              Mechanical Turk Exclusion Pipeline:             ######
######                Ibex Farm edition                             ######
######              Christopher Hammerly & Brian Dillon             ######
######              Cognitive Science of Language Lab               ######
######              UMass Amherst                                   ######
######              Created: August 2016                            ######
######              Last Modified: August 2016                      ######
##########################################################################

#####################################################################
###                         Introduction                          ###
#####################################################################

#   The purpose of this script is to make it as easy as possible to see if 
#   an Amazon Mechanical Turk participant has run up against any exclusion criteria
#   on an Ibex Farm experiment.
#
#   This is not a pipleline for excluding particular trials for your analysis. 
#   We assume this can be done in your analysis pipeline. The purpose is 
#   to decide whether or not to pay a given Turker for completing your HIT.
#
#   It is organized by the following criteria:
#
#       - Demographics
#       - Instruction Questions
#       - Catch fillers
#       - Judgment scale usage
#       - Repeat participants
#
#   The goal is for each of these to be malleable enough to easily fit the specifications
#   of your criteria. 
#
#   The list is not exhaustive - add as you see fit and document how to use it!

#####################################################################
###                           Libraries                           ###
#####################################################################

#   This section includes the packages necessary to execute the pipeline.
#
#   If you do not have a given package, you can install it with the following command:
#   install.packages("PACKAGE.NAME")

library(tidyr)
library(dplyr)
library(ez)

#####################################################################
###                           Data Prep                           ###
#####################################################################

#   Create column names for the data file. These labels are geared towards acceptablity judgments

mycols <- c("Subject","MD5","TrialType","Number","Element","Experiment","Item", "Question", "Response","null","RT","null")

#   Read in data. Add the filepath where your results are kept.

data.raw <- read.csv('/Users/chrishammerly/PPvsCP/results/results.csv',
                     header = 0, 
                     sep = ",",
                     quote = "",
                     comment.char = "#",
                     col.names=mycols,
                     fill = TRUE)
data.raw$Subject <- as.factor(data.raw$Subject)

#   Segregate the judgment data. These are all labled "Question" in Ibex

data.judge <- droplevels(subset(data.raw, TrialType == 'Question' & Experiment != "Practice"))
data.judge$Response <- as.numeric(as.character(data.judge$Response))

#   Segregate the demographic data and Worker ID. Demographic questions 
#   should be in a form with a uniquely identifying label to allow them 
#   to be pulled out.

data.demo <- droplevels(subset(data.raw, Experiment == 'background' | Experiment == "exit"))
data.demo <- data.demo %>%
  select(Subject,Question,Response) %>%
  spread(Question,Response) %>%
  droplevels()

#   Segregate the instruction questions. Again, the form for these questions
#   should be unique to allow easy identification.

data.instructions <- droplevels(subset(data.raw, Experiment == 'intro'))
data.instructions <- data.instructions %>%
  select(Subject,Question,Response) %>%
  spread(Question,Response) %>%
  droplevels()


#   List of bad subjects

bad.subjects <- {}

#####################################################################
###                           Demographics                        ###
#####################################################################

#   A regular expression to identify those who reported English as their native language 
data.demo$English <- grepl('[E|e]nglish|ENGLISH',data.demo$natlang)

data.demo$English <- as.numeric(data.demo$English)

## Here is an attempt with an if statement. This produces an error

for (cur.subj in levels(data.demo$Subject)) {
  if (data.demo$English[cur.subj]==0) {
  bad.subjects <- c(bad.subjects, cur.subj)
  cat(cur.subj, "was exluded due to non-English native language")
  }
}


##  Here is an attempt with an ifelse statement. There are no errors, but it puts all
##  of the subject numbers on the bad.subjects list.

for (cur.subj in levels(data.demo$Subject)) {
  ifelse(data.demo$English[] == 0, x <- cur.subj, print("good"))
  bad.subjects <- c(bad.subjects, x)
}



#####################################################################
###                         Instuctions                           ###
#####################################################################

#   Score questions
correct.answers = c("space","yes","silent","all","no","1","maximized")
names(correct.answers) = c("advancekey","hands","read","scale","screen","unacceptableresponse","window")
all.answers = as.matrix(data.instructions [,c("advancekey","hands","read","scale","screen","unacceptableresponse","window")])
data.instructions$accurate.answers = apply(all.answers, 1, identical, correct.answers)

#   New version of creating bad subjects list

for (cur.subj in levels(data.instructions$Subject)) {
  instructions <- ifelse(data.instructions$accurate.answers == FALSE, cur.subj, "good")
}

#   BELOW IS MY FIRST STAB AT THIS
#   Add subjects who did not answer all questions correctly to bad.subjects list
for (cur.subj in levels(data.instructions$Subject)) {
  if (data.instructions$accurate.answers == "FALSE"){
    bad.subjects$Subject <- cur.subj
    cat(cur.subj, "was exluded based on instruction questions")
  }
}

######################################################################
###                       Catch fillers                            ###
######################################################################

#   Grammatical: Items 57-60
#   Ungrammatical: Items 93-96

#   Segregate catch fillers

grammatical.catch <- droplevels(subset(data.judge, Item == 57 | Item == 58 | Item == 59 | Item == 60 ))

ungrammatical.catch <- droplevels(subset(data.judge, Item == 93 | Item == 94 | Item == 95 | Item == 96 ))

#   Calculate means for each subject

mean.grammatical.catch <- grammatical.catch %>%
  group_by(Subject) %>%
  summarise(mean = mean(Response))

mean.ungrammatical.catch <- ungrammatical.catch %>%
  group_by(Subject) %>%
  summarise(mean = mean(Response))

#   NEW CODE

for (cur.subj in levels(data.judge$Subject)) {
  fillers <- ifelse(mean.ungrammatical.catch$mean > mean.grammatical.catch$mean, cur.subj, "good")
}

#   Add subjects whose mean rating on the grammatical catch fillers is less than their
#   mean rating on the ungrammatical catch fillers to bad.subjects list

for (cur.subj in levels(data.judge$Subject)) {
  if (mean.ungrammatical.catch$mean > mean.grammatical.catch$mean) {
    bad.subjects$Subject <- cur.subj
    cat(cur.subj, "was exluded based on catch fillers")
  }
}

#####################################################################
###                         Scale usage                           ###
#####################################################################

#   New code -- I'm not entirely sure that this works perfectly

for (cur.subj in levels(data.judge$Subject)) {
  cur.data.judge <- subset(data.judge, Subject == cur.subj)
  cur.table <- table(cur.data.judge$Response)
  scale <- ifelse(length(cur.table) == 1 | length(cur.table) == 2, cur.subj, "good")
}

#   Old Code

for (cur.subj in levels(data.judge$Subject)) {
  cur.data.judge <- subset(data.judge, Subject == cur.subj)
  cur.table <- table(cur.data.judge$Response)
  if (length(cur.table) == 1 | length(cur.table) == 2) {
    bad.subjects$Subject <- cur.subj 
    cat(cur.subj, "was exluded based on scale usage")
  }
}

#####################################################################
###                             Repeats                           ###
#####################################################################

#   New version, also somewhat buggy

for (cur.subj in levels(data.demo$Subject)) {
  repeats <- ifelse(duplicated(data.demo$worker_id == TRUE), cur.subj, "good")
}


#   OLD VERSION

for (cur.subj in levels(data.demo$Subject)) {
  if (duplicated(data.demo$worker_id) == "TRUE") {
    bad.subjects$Subject <- cur.subj
    cat(cur.subj, "was exluded based on repeat worker ID")
  }
}

#####################################################################
###                         Bad Subjects                          ###
#####################################################################

print(language)
print(instructions)
print(fillers)
print(scale)
print(repeats)

#   OUTDATED: The command below will print the subject numbers of subjects who should
#   be excluded.

print(bad.subjects)
