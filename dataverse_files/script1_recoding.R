####################################################
# File name: "recoding_script.R"
# Goal: Recode data for matching analysis
# Dependency: "salta_data.Rdata"
# Output: "datamatch.Rdata"
####################################################

load("salta_data.Rdata")

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

# poll workers qualified enough?

prop.table(table(capaci_autoridades))

capable.auth <- NULL
capable.auth[capaci_autoridades == "Nada Capacitadas"] <- 0
capable.auth[capaci_autoridades == "Poco Capacitadas"] <- 0
capable.auth[capaci_autoridades == "Bastante Capacitadas"] <- 1
capable.auth[capaci_autoridades == "Muy Capacitadas"] <- 1

# quality of voting experience?

prop.table(table(calif_votac))

eval.voting <- NULL
eval.voting[calif_votac == "Muy Malo"] <- 0
eval.voting[calif_votac == "Malo"] <- 0
eval.voting[calif_votac == "Bueno"] <- 0
eval.voting[calif_votac == "Muy Bueno"] <- 1

# difficulty of voting experience?

prop.table(table(facil))

easy.voting <- NULL
easy.voting[as.numeric(facil) == 2] <- 1
easy.voting[as.numeric(facil) == 3] <- 0
easy.voting[as.numeric(facil) == 4] <- 0
easy.voting[as.numeric(facil) == 5] <- 0

# how sure vote counted?

prop.table(table(cuàn_seguro))

sure.counted <- NULL
sure.counted[as.numeric(cuàn_seguro) == 2] <- 1
sure.counted[as.numeric(cuàn_seguro) == 3] <- 1
sure.counted[as.numeric(cuàn_seguro) == 4] <- 0
sure.counted[as.numeric(cuàn_seguro) == 5] <- 0

# how confident vote secret?

prop.table(table(cuàn_confiado))

conf.secret <- NULL
conf.secret[as.numeric(cuàn_confiado) == 2] <- 1
conf.secret[as.numeric(cuàn_confiado) == 3] <- 1
conf.secret[as.numeric(cuàn_confiado) == 4] <- 0
conf.secret[as.numeric(cuàn_confiado) == 5] <- 0

# believe provincial elections are clean?

prop.table(table(elecc_limpias))

how.clean <- NULL
how.clean[as.numeric(elecc_limpias) == 2] <- 1
how.clean[as.numeric(elecc_limpias) == 3] <- 1
how.clean[as.numeric(elecc_limpias) == 4] <- 0
how.clean[as.numeric(elecc_limpias) == 5] <- 0

# how quick was process?

prop.table(table(rapidez_proceso))

speed <- NULL
speed[as.numeric(rapidez_proceso) == 2] <- 1
speed[as.numeric(rapidez_proceso) == 3] <- 1
speed[as.numeric(rapidez_proceso) == 4] <- 0
speed[as.numeric(rapidez_proceso) == 5] <- 0

# agree replacing VT by VE?

prop.table(table(reemplazoVTxVE))

agree.evoting <- NULL
agree.evoting[as.numeric(reemplazoVTxVE) == 2] <- 1
agree.evoting[as.numeric(reemplazoVTxVE) == 3] <- 1
agree.evoting[as.numeric(reemplazoVTxVE) == 4] <- 0
agree.evoting[as.numeric(reemplazoVTxVE) == 5] <- 0

# select candidates electronically?

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
