#################################################### dta
# File name: "recoding_script.R"
# Goal: Recode data for matching analysis
# Dependency: "salta_data.Rdata"
# Output: "datamatch.Rdata"
####################################################

#Load the salta_data from where it is stored
load("~/Documents/CS112 /Final Project/dataverse_files/salta_data.RData")

attach(salta.data)

#___________________________________________________#
# Polling place 
#___________________________________________________#

polling.place <- escuela

#___________________________________________________#
# Voting System
#___________________________________________________#

# electronic (VE) or traditional (VT) voting?

system <- sistema

EV <- NULL
EV[system == "VE"] <- 1
EV[system == "VT"] <- 0

#___________________________________________________# 
# Recode outcome variables 
#___________________________________________________#

# Poll workers qualified enough?

#proportion table (Looks at proportion of responses that are a particular response)
prop.table(table(capaci_autoridades))

capable.auth <- NULL
#No training coded as 0
capable.auth[capaci_autoridades == "Nada Capacitadas"] <- 0
#Little training coded as 0
capable.auth[capaci_autoridades == "Poco Capacitadas"] <- 0
#Pretty qualified coded as 1
capable.auth[capaci_autoridades == "Bastante Capacitadas"] <- 1
#Very qualified coded as 1
capable.auth[capaci_autoridades == "Muy Capacitadas"] <- 1

# Quality of voting experience?
#proportion table (Looks at proportion of responses that are a particular response)
prop.table(table(calif_votac))

eval.voting <- NULL
#Coded very bad as 0
eval.voting[calif_votac == "Muy Malo"] <- 0
#Coded bad as 0
eval.voting[calif_votac == "Malo"] <- 0
#Coded OK as 0
eval.voting[calif_votac == "Bueno"] <- 0
#Coded very good as 1
eval.voting[calif_votac == "Muy Bueno"] <- 1

# Difficulty of voting experience?
prop.table(table(facil))

easy.voting <- NULL
#Code very easy as 1
easy.voting[as.numeric(facil) == 2] <- 1
#Code easy as 0
easy.voting[as.numeric(facil) == 3] <- 0
#Code difficult as 0
easy.voting[as.numeric(facil) == 4] <- 0
#Code very difficult as 0
easy.voting[as.numeric(facil) == 5] <- 0

# How sure vote counted?
#Confidence in the fairness of the election process

prop.table(table(cuàn_seguro))

sure.counted <- NULL
#Coded as 1 if very sure or sure
sure.counted[as.numeric(cuàn_seguro) == 2] <- 1
sure.counted[as.numeric(cuàn_seguro) == 3] <- 1
#Coded as 0 if unsure or very unsure
sure.counted[as.numeric(cuàn_seguro) == 4] <- 0
sure.counted[as.numeric(cuàn_seguro) == 5] <- 0

# How confident vote secret?
#Confidence on ballot secrecy

prop.table(table(cuàn_confiado))

conf.secret <- NULL
#1 if ‘very confident’ or ‘confident’
conf.secret[as.numeric(cuàn_confiado) == 2] <- 1
conf.secret[as.numeric(cuàn_confiado) == 3] <- 1
#0 if ‘not confident’ or ‘not at all confident’
conf.secret[as.numeric(cuàn_confiado) == 4] <- 0
conf.secret[as.numeric(cuàn_confiado) == 5] <- 0

# Believe provincial elections are clean?

prop.table(table(elecc_limpias))

how.clean <- NULL
#1 if ‘very clean’ or ‘somewhat clean’
how.clean[as.numeric(elecc_limpias) == 2] <- 1
how.clean[as.numeric(elecc_limpias) == 3] <- 1
#0 if ‘a little clean’ or ‘not at all clean’
how.clean[as.numeric(elecc_limpias) == 4] <- 0
how.clean[as.numeric(elecc_limpias) == 5] <- 0

# How quick was process?

prop.table(table(rapidez_proceso))

speed <- NULL

#1 if ‘very quick’ or ‘quick’;
speed[as.numeric(rapidez_proceso) == 2] <- 1
speed[as.numeric(rapidez_proceso) == 3] <- 1
#0 if ‘slow’ or ‘very slow’
speed[as.numeric(rapidez_proceso) == 4] <- 0
speed[as.numeric(rapidez_proceso) == 5] <- 0

# Agree replacing VT by VE?
#Substitution of traditional voting by e-voting
prop.table(table(reemplazoVTxVE))

agree.evoting <- NULL
agree.evoting[as.numeric(reemplazoVTxVE) == 2] <- 1
agree.evoting[as.numeric(reemplazoVTxVE) == 3] <- 1
agree.evoting[as.numeric(reemplazoVTxVE) == 4] <- 0
agree.evoting[as.numeric(reemplazoVTxVE) == 5] <- 0

# Select candidates electronically?
#Preferred method for selecting candidates from different political parties

prop.table(table(sist_voto_categ))

eselect.cand <- NULL
eselect.cand[as.numeric(sist_voto_categ) == 2] <- 0
eselect.cand[as.numeric(sist_voto_categ) == 3] <- 1

#___________________________________________________#
# Recode covariates 
#___________________________________________________#

age <- edad
age.group <- NULL
age.group[age < 30] <- 1
age.group[age > 29 & age < 40] <- 2
age.group[age > 39 & age < 50] <- 3
age.group[age > 49 & age < 65] <- 4
age.group[age > 64] <- 5

male <- NULL
male[sexo == "MASCULINO"] <- 1
male[sexo == "FEMENINO"] <- 0

educ <- NULL
educ[educ_enc == "Sin Estudios" | educ_enc == "Primario Incompleto"] <- 1
educ[educ_enc == "Primario Completo"] <- 2
educ[educ_enc == "Secundario Incompleto"] <- 3
educ[educ_enc == "Secundario Completo"] <- 4
educ[educ_enc == "Terciario Incompleto"] <- 5
educ[educ_enc == "Terciario Completo"] <- 6
educ[educ_enc == "Universitario Incompleto"] <- 7
educ[educ_enc == "Universitario Completo" | educ_enc == "Posgrado"] <- 9

white.collar <- NULL
white.collar[ocupac == "EMPLEADO PUBLICO"|ocupac == "COMERCIANTE SIN EMPLEADOS" | ocupac == "EMPLEADO SECTOR PRIVADO" | ocupac == "PROF/COMERCIANTE EMPLEADOS A CARGO"] <- 1
white.collar[ocupac != "EMPLEADO PUBLICO"&ocupac != "COMERCIANTE SIN EMPLEADOS"&ocupac != "EMPLEADO SECTOR PRIVADO"&ocupac != "PROF/COMERCIANTE EMPLEADOS A CARGO"] <- 0

not.full.time <- NULL
not.full.time[ocupac == "ESTUDIANTE" | ocupac == "AMA DE CASA" | ocupac == "DESOCUPADO" | ocupac == "SUBSIDIADO/PLANES/ASIGNACIONES" | ocupac == "TRABAJOS TEMPORARIOS" | ocupac == "EMPLEADO INFORMAL" | ocupac == "JUBILADO/PENSIONADO" | ocupac == "RENTISTA"] <- 1
not.full.time[ocupac == "ESTUDIANTE" | ocupac != "AMA DE CASA" & ocupac != "DESOCUPADO" & ocupac != "SUBSIDIADO/PLANES/ASIGNACIONES" & ocupac != "TRABAJOS TEMPORARIOS" & ocupac != "EMPLEADO INFORMAL" & ocupac != "JUBILADO/PENSIONADO" & ocupac != "RENTISTA"] <- 0

internet.work <- NULL
internet.work[internet_trabajar == "No"] <- 0
internet.work[internet_trabajar == "Si"] <- 1

internet.play <- NULL
internet.play[internet_jugar == "No"] <- 0
internet.play[internet_jugar == "Si"] <- 1

atm <- NULL
atm[cajeros == "No"] <- 0
atm[cajeros == "Si"] <- 1

cell <- NULL
cell[celular == "No"] <- 0
cell[celular == "Si"] <- 1

pc.own <- NULL
pc.own[PC_propia == "No"] <- 0
pc.own[PC_propia == "Si"] <- 1

tech <- internet.work+internet.play+atm+cell+pc.own+1

table(randazzo)
table(figueroa)
table(alperovich)

info1 <- NULL
info1[as.numeric(randazzo) == 1 | as.numeric(randazzo) == 2 | as.numeric(randazzo) == 3] <- 1
info1[as.numeric(randazzo) == 4 | as.numeric(randazzo) == 5] <- 0

info2 <- NULL
info2[as.numeric(figueroa) == 1 | as.numeric(figueroa) == 2 | as.numeric(figueroa) == 3] <- 1
info2[as.numeric(figueroa) == 4 | as.numeric(figueroa) == 5] <- 0


info3 <- NULL
info3[as.numeric(alperovich) == 1 | as.numeric(alperovich) == 2 | as.numeric(alperovich) == 3] <- 1
info3[as.numeric(alperovich) == 4 | as.numeric(alperovich) == 5] <- 0

pol.info <- 1 + info1 + info2 + info3

table(pol.info)

#___________________________________________________#
# Create and save dataframe for matching analysis 
#___________________________________________________#

datamatch <- data.frame(polling.place, EV, age.group, educ, male, tech, pol.info, white.collar, not.full.time, capable.auth, eval.voting, easy.voting, sure.counted, conf.secret, how.clean, speed, agree.evoting, eselect.cand)

save(datamatch, file="datamatch.Rdata")


########################################################################
# File name: "matching_script.R"
# Goal: Estimate effect of e-voting using matching
# Dependency: "datamatch.Rdata" 
########################################################################
install.packages(c("MatchIt","Zelig","rbounds"))

library(MatchIt)
library(Zelig)
library(rbounds)

load("datamatch.Rdata")

#Identify the outcome variables in the dataset
outcomes <- datamatch[10:18]

#get the names of the outcome variables
outcomes.lbls <- names(outcomes)

#Count the number of outcome variables
n.outcomes <- dim(outcomes)[2]


# Drop observations with missing values in COVARIATES
#Remember 10 to 18 are outcome variables so we don't wanna drop those observations?
datamatch[, 10:18][is.na(datamatch[, 10:18]) == "TRUE"] <- 99999
datamatch <- na.omit(datamatch)

#__________________________ Table 2, pre-matching __________________________#

#If they  voted electronically or not
EV <- datamatch[2]

##Identify the covariates in the dataset
covariates <- datamatch[c("age.group", "educ", "white.collar", "not.full.time", "male", "tech", "pol.info")]
##Get covariate names
covariate.lbls <- names(covariates)

#Count the number of covariates in the dataset
n.covariates <- dim(covariates)[2]

####Replicate the 'Before Matching' half of table 2
tab2.pre <- matrix(NA, nrow = n.covariates, ncol = 4)
rownames(tab2.pre) <- covariate.lbls
colnames(tab2.pre) <- c("EV", "TV", "Diff", "p-value*")

#covariates[EV == 1,] only gets people that voted electronically
#covariates[EV == 0,] only gets people that voted traditionally
tab2.pre[, 1:2] <- cbind(apply(covariates[EV == 1,], 2, mean), apply(covariates[EV == 0,], 2, mean))
tab2.pre[, 3] <- tab2.pre[, 1] - tab2.pre[, 2]


for (i in c(1, 2, 6, 7)){
  tab2.pre[i, 4] <- ks.boot(covariates[, i][EV == 1], covariates[, i][EV == 0], nboots = 500)$ks.boot.pvalue
}
for (i in c(3, 4, 5)){
  tab2.pre[i, 4] <- prop.test(table(covariates[, i], EV$EV), n = apply(table(covariates[,i],EV$EV),2, sum))$p.value
}


######## Run Original Paper's Matching Procedure (This is used to create the second half of Table 2)
#__________________________ Matching (with MatchIt) ________________________#

print("Matching")

set.seed(36466)

#Nearest neighbour matching in terms of propensity scores
m.out <- matchit(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + tech:pol.info + white.collar + not.full.time + male, caliper = 0.05, data = datamatch, method = "nearest", verbose = "TRUE")

#save(m.out, file = "m.out.Rdata")

print("Balance Improvement")
print(summary(m.out))

#pdf("balance.pdf")

#plot(m.out)
#plot(m.out, type = "hist")
#plot(m.out, type = "jitter")

#dev.off()

#___________________________________________________________________________#

# Get Matched sample
datamatched <- match.data(m.out)
datamatched[datamatched == 99999] <- NA 

save(datamatched, file = "datamatched.Rdata")

####Create 'After Matching" half of Table2
#__________________________ Table 2, post-matching _________________________#

####Get treatment indicator in the matched dataset
EV.post <- datamatched[2]

##Get covariates in the matched dataset
covariates.post <- datamatched[, covariate.lbls]

######Replicate the second half of the Original Paper's Table 2
tab2.post <- matrix(NA, nrow = n.covariates, ncol = 4)
rownames(tab2.post) <- covariate.lbls
colnames(tab2.post) <- c("EV", "TV", "Diff", "p-value*")

tab2.post[, 1:2] <- cbind(apply(covariates.post[EV.post == 1, ], 2, mean), apply(covariates.post[EV.post == 0,], 2, mean))
tab2.post[, 3] <- tab2.post[, 1] - tab2.post[, 2]
for (i in c(1, 2, 6 , 7)){
  tab2.post[i, 4]<-ks.boot(covariates.post[,i][EV.post==1],covariates.post[,i][EV.post==0], nboots = 500)$ks.boot.pvalue
}
for (i in c(3, 4, 5)){
  tab2.post[i, 4] <- prop.test(table(covariates.post[, i], EV.post$EV), n = apply(table(covariates.post[, i], EV.post$EV),2 , sum))$p.value
}

tab2 <- cbind(tab2.pre, tab2.post)
tab2[3:5, c(1:3, 5:7)] <- tab2[3:5, c(1:3, 5:7)] * 100

###FINAL TOUCHES TO THE TABLE 2 REPLICATION
########################################################
### Table 2 REPLICATION###
#######################################################

#Create copy of paper's table 2 to round the Diff columns
table2= tab2

#Round before matching results
table2[,c(1:3)] <- round(table2[,c(1:3)],1)

#Round after matching results
table2[,c(5:7)] <- round(table2[,c(5:7)],1)

#Change rownames to match the rownames presented in the Original Paper
rownames(table2)= c("Age group (1-5)","Education (1-8)","White collar (%)", "Not full time worker (%)",
                    "Male (%)","Technology count (1-6)","Political information (1-4)")

print(table2, digits = 2)

######BEGIN TABLE 2 EXTENSION BY CARRYING OUT GENETIC MATCHING ON THE UNMATCHED DATASET
##### DIFFERENT COMBINATIONS WERE TRIED IN ORDER TO MAXIMIZE THE MINIMUM P-VALUE OBTAINED

######################### GENETIC MATCHING ################################################
library(Matching)

#Select covariates in the dataset to be genetically matched on (These are the same covariates that 
#were matched on in the original paper)
X= cbind(datamatch$age.group, I(datamatch$age.group^2), I(datamatch$age.group^3), 
         datamatch$age.group:datamatch$educ, datamatch$age.group:datamatch$tech,
         datamatch$educ, I(datamatch$educ^2), datamatch$tech, I(datamatch$tech^2), 
         datamatch$pol.info, datamatch$educ:datamatch$pol.info,
         datamatch$age.group:datamatch$pol.info, datamatch$tech:datamatch$pol.info,
         datamatch$white.collar, datamatch$not.full.time, datamatch$male)

#Get the treatment indicator 
Tr= datamatch$EV

#Run genetic matching function on the selected covariates
genout <- GenMatch(X = X, Tr = Tr, pop.size=2000, max.generations=50, wait.generations=40, ties=FALSE)


matchout.gen <- Match(X = X, Tr = Tr, Weight.matrix=genout, ties=FALSE)

summary(matchout.gen)

###Assess Balance on covariates
match_balance_gen = MatchBalance(Tr ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + 
                                   age.group:tech + educ + I(educ^2) + tech + I(tech^2) + pol.info + 
                                   educ:pol.info + age.group:pol.info + tech:pol.info + white.collar+ 
                                   not.full.time + male, data = datamatch, match.out = matchout.gen, 
                                 nboots=1000)


######## Get matched dataset from Match function
matched_dataset= rbind(datamatch[matchout.gen$index.treated,],datamatch[matchout.gen$index.control,])

#####Reassign missing outcome values as NA
matched_dataset[matched_dataset == 99999] <- NA 
View(matched_dataset)


############# NEW TRY  (KEPT HERE BECAUSE IT PRODUCED THE EXACT SAME MINIMUM P-VALUE)

#Run genetic matching function on the selected covariates
genout2 <- GenMatch(X = X, Tr = Tr, pop.size=2000, max.generations=50, wait.generations=40, ties=FALSE,
                    exact=c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,
                            FALSE,FALSE))


matchout.gen2 <- Match(X = X, Tr = Tr, Weight.matrix=genout, ties=FALSE, exact=c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,
                                                                                 FALSE,FALSE))

summary(matchout.gen2)

###Assess Balance on covariates
match_balance_gen2 = MatchBalance(Tr ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + 
                                   age.group:tech + educ + I(educ^2) + tech + I(tech^2) + pol.info + 
                                   educ:pol.info + age.group:pol.info + tech:pol.info + white.collar+ 
                                   not.full.time + male, data = datamatch, match.out = matchout.gen2, 
                                 nboots=1000)

######## Get matched dataset from Match function
matched_dataset2= rbind(datamatch[matchout.gen2$index.treated,],datamatch[matchout.gen2$index.control,])

#####Reassign missing outcome values as NA
matched_dataset2[matched_dataset2 == 99999] <- NA 


##Compare the covariate balance in the matched datasets obtained from the two best genetic matching runs
###(The better one is the one with a higher 'Before Matching Minimum p.value')

##No exact matching
MatchBalance(EV ~ age.group + educ + tech + pol.info + white.collar + not.full.time + male, data = matched_dataset, nboots=1000)
##Exact matching employed here
MatchBalance(EV ~ age.group + educ + tech + pol.info + white.collar + not.full.time + male, data = matched_dataset2, nboots=1000)

####matched_dataset(the one that was created without employing exact matching) seems to be slightly
#### better balanced than matched_dataset2

###Look at covariate balance attained in the original paper's matched dataset
MatchBalance(EV ~ age.group + educ + tech + pol.info + white.collar + not.full.time + male, data = datamatched, nboots=1000)

#Plot balance on all covariates achieved by the original paper's matching
############
install.packages(c("ggplot2", "ggpubr", "cobalt"))
library(ggplot2)
library(cobalt)
library(ggpubr)

age.group_plot1= bal.plot(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + 
                            I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + 
                            tech:pol.info + white.collar+ not.full.time + male, 
                          data = datamatched, var.name = "age.group")

educ_plot1= bal.plot(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + 
                       I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + 
                       tech:pol.info + white.collar+ not.full.time + male, 
                     data = datamatched, var.name = "educ")

white.collar_plot1= bal.plot(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + 
                               I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + 
                               tech:pol.info + white.collar+ not.full.time + male, 
                             data = datamatched, var.name = "white.collar")

not.full.time_plot1= bal.plot(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + 
                                I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + 
                                tech:pol.info + white.collar+ not.full.time + male, 
                              data = datamatched, var.name = "not.full.time")

male_plot1= bal.plot(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + 
                       I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + 
                       tech:pol.info + white.collar+ not.full.time + male, 
                     data = datamatched, var.name = "male")

tech_plot1= bal.plot(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + 
                       I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + 
                       tech:pol.info + white.collar+ not.full.time + male, 
                     data = datamatched, var.name = "tech")

pol.info_plot1= bal.plot(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + 
                           I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + 
                           tech:pol.info + white.collar+ not.full.time + male, 
                         data = datamatched, var.name = "pol.info")

ggarrange(age.group_plot1, educ_plot1, white.collar_plot1, not.full.time_plot1, male_plot1, tech_plot1,
          pol.info_plot1 + rremove("x.text"), labels = c("A", "B", "C","D","E","F","G"))

#############

#Plot balance on all covariates achieved by the genetic matching

age.group_plot= bal.plot(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + 
                           I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + 
                           tech:pol.info + white.collar+ not.full.time + male, 
                         data = matched_dataset, var.name = "age.group")

educ_plot= bal.plot(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + 
                      I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + 
                      tech:pol.info + white.collar+ not.full.time + male, 
                    data = matched_dataset, var.name = "educ")

white.collar_plot= bal.plot(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + 
                              I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + 
                              tech:pol.info + white.collar+ not.full.time + male, 
                            data = matched_dataset, var.name = "white.collar")

not.full.time_plot= bal.plot(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + 
                               I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + 
                               tech:pol.info + white.collar+ not.full.time + male, 
                             data = matched_dataset, var.name = "not.full.time")

male_plot= bal.plot(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + 
                      I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + 
                      tech:pol.info + white.collar+ not.full.time + male, 
                    data = matched_dataset, var.name = "male")

tech_plot= bal.plot(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + 
                      I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + 
                      tech:pol.info + white.collar+ not.full.time + male, 
                    data = matched_dataset, var.name = "tech")

pol.info_plot= bal.plot(EV ~ age.group + I(age.group^2) + I(age.group^3) + age.group:educ + age.group:tech + educ + 
                          I(educ^2) + tech + I(tech^2) + pol.info + educ:pol.info + age.group:pol.info + 
                          tech:pol.info + white.collar+ not.full.time + male, 
                        data = matched_dataset, var.name = "pol.info")

ggarrange(age.group_plot, educ_plot, white.collar_plot, not.full.time_plot, male_plot, tech_plot,
          pol.info_plot + rremove("x.text"), labels = c("A", "B", "C","D","E","F","G"))


#__________________________ Table 2 Extension (Including Genetic Matching Results) _________________________#

##Get treatment indicator
evoting.post <- matched_dataset[2]

##Get covariates from the gentically matched dataset
cov.post <- matched_dataset[, covariate.lbls]

###Calculate the balance statistics from the genetically matched dataset
tab2.extension <- matrix(NA, nrow = n.covariates, ncol = 4)
rownames(tab2.extension) <- covariate.lbls
colnames(tab2.extension) <- c("EV", "TV", "Diff", "p-value*")

tab2.extension[, 1:2] <- cbind(apply(cov.post[evoting.post == 1, ], 2, mean), apply(cov.post[evoting.post == 0,], 2, mean))
tab2.extension[, 3] <- tab2.extension[, 1] - tab2.extension[, 2]
for (i in c(1, 2, 6 , 7)){
  tab2.extension[i, 4]<-ks.boot(cov.post[,i][evoting.post==1],cov.post[,i][evoting.post==0], nboots = 500)$ks.boot.pvalue
}
for (i in c(3, 4, 5)){
  tab2.extension[i, 4] <- prop.test(table(cov.post[, i], evoting.post$EV), n = apply(table(cov.post[, i], evoting.post$EV),2 , sum))$p.value
}

### Create a new table that is the same as the original paper's Table 2 (This table will be extended on
### by including the balance statistics of the Genetic Matching)
table2_extension <- cbind(table2,tab2.extension)


#Round after matching results for the genetic matching
table2_extension[,c(9:11)] <- round(table2_extension[,c(9:11)],2)

#Rename rownames
rownames(table2_extension)= c("Age group (1-5)","Education (1-8)","White collar (%)", "Not full time worker (%)",
                    "Male (%)","Technology count (1-6)","Political information (1-4)")


View(table2_extension)


######### NOW SHOW EXTENSION'S IMPACT ON THE CAUSAL ESTIMATES OF E-VOTING #############################
######## EXTENSION OF TABLE 3 (Computing  the causal effects after the genetic matching) #############


#__________________________ Table 3, pre-matching __________________________#

datamatch[datamatch == 99999] <- NA

outcomes.pre <- datamatch[10:18]


tab3.pre <- matrix(NA,nrow = n.outcomes,ncol = 5)
rownames(tab3.pre) <- outcomes.lbls
colnames(tab3.pre) <- c("N", "prop.ev", "prop.tv", "diff", "p-value")

#Get unmatched datasets causal effects
for (i in 1:n.outcomes) {
  tab3.pre[i, 1] <- length(na.omit(outcomes.pre[, i]))
  #Proportion of both e-voting and 
  tab3.pre[i, 2:3] <- rev(prop.table(table(outcomes.pre[,i],datamatch$EV),2)[2,])*100
  tab3.pre[i, 4] <- tab3.pre[i, 2] - tab3.pre[i, 3]	
  tab3.pre[i, 5] <- prop.test(table(outcomes.pre[, i], datamatch$EV)[2, ], n = apply(table(outcomes.pre[, i], datamatch$EV), 2, sum))$p.value
}

datamatch[, 10:18][is.na(datamatch[, 10:18]) == "TRUE"] <- 99999


#__________________________ Table 3, post-matching _________________________#

##Get outcome variable
outcomes.post <- datamatched[10:18]

##Create table to store causal effects for the original paper's matched dataset
tab3.post <- matrix(NA, nrow = n.outcomes, ncol = 5)
rownames(tab3.post) <- outcomes.lbls
colnames(tab3.post) <- c("N", "prop.ev", "prop.tv", "diff", "p-value")

#Get original paper's matched dataset's causal effects
for (i in 1:n.outcomes) {
  tab3.post[i, 1] <- length(na.omit(outcomes.post[, i]))
  tab3.post[i, 2:3] <- rev(prop.table(table(outcomes.post[, i], datamatched$EV), 2)[2, ]) * 100
  tab3.post[i, 4] <- tab3.post[i, 2] - tab3.post[i, 3]	
  tab3.post[i, 5] <- prop.test(table(outcomes.post[, i], datamatched$EV)[2, ], n = apply(table(outcomes.post[, i], datamatched$EV), 2, sum))$p.value
}

tab3 <- cbind(tab3.pre, tab3.post)

tab3 <- tab3[rev(order(tab3[, 9])), ]


#__________________________ Table 3 Extension (Genetic Matching Causal Effects) _________________________#

##Replace 99999 values with NA (as was done in the previous datasets)
matched_dataset[matched_dataset == 99999] <- NA

##Get outcome variable
outcomes.post <- matched_dataset[10:18]

##Create table to store causal effects for the genetically matched dataset
table3.post <- matrix(NA, nrow = n.outcomes, ncol = 5)
rownames(table3.post) <- outcomes.lbls
colnames(table3.post) <- c("N", "prop.ev", "prop.tv", "diff", "p-value")

#Get genetically matched dataset's causal effects
for (i in 1:n.outcomes) {
  table3.post[i, 1] <- length(na.omit(outcomes.post[, i]))
  table3.post[i, 2:3] <- rev(prop.table(table(outcomes.post[, i], matched_dataset$EV), 2)[2, ]) * 100
  table3.post[i, 4] <- table3.post[i, 2] - table3.post[i, 3]	
  table3.post[i, 5] <- prop.test(table(outcomes.post[, i], matched_dataset$EV)[2, ], n = apply(table(outcomes.post[, i], matched_dataset$EV), 2, sum))$p.value
}


####Add the causal effect estimates from the genetic match to table 3
table3_extension <- cbind(tab3, table3.post)
table3_extension <- table3_extension[rev(order(table3_extension[, 9])), ]

### Extended Table 3 ###
View(table3_extension)
print(table3_extension, digits = 4)




