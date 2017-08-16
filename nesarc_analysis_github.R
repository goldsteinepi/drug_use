#################
# NESARC wave 2 analysis for recreational drug use
# Citation: Goldstein ND, Burstyn I, LeVasseur MT, Welles SL. Drug use among men by sexual behavior, race and ethnicity: prevalence estimates from a nationally representative US sample. Int J Drug Policy. 2016 Jan 18. pii: S0955-3959(16)00031-1.
# 3/5/14 -- Neal Goldstein
#################


### FUNCTIONS ###

library(gmodels) #CrossTable
library(survey) #complex survey sampling


### READ DATA ###

load("NESARCw2.RData")


### COUNTS ###

#limit to black, white, hispanic
NESARCw2_men = NESARCw2_men[NESARCw2_men$race==0 | NESARCw2_men$race==1 | NESARCw2_men$race==4, ]

#counts
CrossTable(NESARCw2_men$sex_men, NESARCw2_men$race, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)


### COMPLEX SURVEY ESTIMATES  ###

#some strata may have one sampled unit: http://r-survey.r-forge.r-project.org/survey/example-twostage.html
options(survey.lonely.psu="remove")

#specify survey design
NESARCw2_complex = svydesign(id=~psu, strata=~stratum, weights=~weight, data=NESARCw2_men)

#proportion by race, ethnicity
svymean(~factor(black), NESARCw2_complex, na.rm=T)
svymean(~factor(white), NESARCw2_complex, na.rm=T)
svymean(~factor(hispanic), NESARCw2_complex, na.rm=T)

#proportion of MSM
svymean(~factor(sex_men), NESARCw2_complex, na.rm=T)

#proportion of MSM by race, ethnicity
svymean(~factor(sex_men), subset(NESARCw2_complex,black==1), na.rm=T)
svymean(~factor(sex_men), subset(NESARCw2_complex,white==1), na.rm=T)
svymean(~factor(sex_men), subset(NESARCw2_complex,hispanic==1), na.rm=T)

#check dist of age
svyquantile(~age, NESARCw2_complex, quantile=c(0.25,0.5,0.75))
svyquantile(~age, subset(NESARCw2_complex,hispanic==1), quantile=c(0.25,0.5,0.75))
svyquantile(~age, subset(NESARCw2_complex,black==1), quantile=c(0.25,0.5,0.75))
svyquantile(~age, subset(NESARCw2_complex,white==1), quantile=c(0.25,0.5,0.75))
svyquantile(~age, subset(NESARCw2_complex,drug_sedative_any==1 | drug_tranquilizer_any==1 | drug_opioid_any==1 | drug_amphetamine_any==1 | drug_cannabis_any==1 | drug_crackcocaine_any==1 | drug_hallucinogen_any==1 | drug_inhalant_any==1 | drug_heroin_any==1), quantile=c(0.25,0.5,0.75))
svyquantile(~age, subset(NESARCw2_complex,drug_sedative_any==0 & drug_tranquilizer_any==0 & drug_opioid_any==0 & drug_amphetamine_any==0 & drug_cannabis_any==0 & drug_crackcocaine_any==0 & drug_hallucinogen_any==0 & drug_inhalant_any==0 & drug_heroin_any==0), quantile=c(0.25,0.5,0.75))

svymean(~age, NESARCw2_complex, na.rm=T)
svymean(~age, subset(NESARCw2_complex,sex_men==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,sex_men==0), na.rm=T)
svymean(~age, subset(NESARCw2_complex,black==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,white==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,hispanic==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,hispanic==0), na.rm=T)
svymean(~age, subset(NESARCw2_complex,black==1 & sex_men==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,black==1 & sex_men==0), na.rm=T)
svymean(~age, subset(NESARCw2_complex,white==1 & sex_men==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,white==1 & sex_men==0), na.rm=T)
svymean(~age, subset(NESARCw2_complex,hispanic==1 & sex_men==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,hispanic==1 & sex_men==0), na.rm=T)

svymean(~age, subset(NESARCw2_complex,drug_sedative_any==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_sedative_any==0), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_tranquilizer_any==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_tranquilizer_any==0), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_opioid_any==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_opioid_any==0), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_amphetamine_any==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_amphetamine_any==0), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_cannabis_any==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_cannabis_any==0), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_crackcocaine_any==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_crackcocaine_any==0), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_hallucinogen_any==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_hallucinogen_any==0), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_inhalant_any==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_inhalant_any==0), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_heroin_any==1), na.rm=T)
svymean(~age, subset(NESARCw2_complex,drug_heroin_any==0), na.rm=T)


### DRUG USE: ALL MEN ###

#sedative
round(100 * svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T)), 1)
svychisq(~sex_men+drug_sedative_any, design=NESARCw2_complex, statistic="adjWald")

CrossTable(NESARCw2_men$sex_men, NESARCw2_men$drug_sedative_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#tranquilizer
round(100 * svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T)), 1)
svychisq(~sex_men+drug_tranquilizer_any, design=NESARCw2_complex, statistic="adjWald")

CrossTable(NESARCw2_men$sex_men, NESARCw2_men$drug_tranquilizer_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#opioid
round(100 * svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T)), 1)
svychisq(~sex_men+drug_opioid_any, design=NESARCw2_complex, statistic="adjWald")

CrossTable(NESARCw2_men$sex_men, NESARCw2_men$drug_opioid_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#amphetamine
round(100 * svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T)), 1)
svychisq(~sex_men+drug_amphetamine_any, design=NESARCw2_complex, statistic="adjWald")

CrossTable(NESARCw2_men$sex_men, NESARCw2_men$drug_amphetamine_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#cannabis
round(100 * svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T)), 1)
svychisq(~sex_men+drug_cannabis_any, design=NESARCw2_complex, statistic="adjWald")

CrossTable(NESARCw2_men$sex_men, NESARCw2_men$drug_cannabis_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#crackcocaine
round(100 * svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T)), 1)
svychisq(~sex_men+drug_crackcocaine_any, design=NESARCw2_complex, statistic="adjWald")

CrossTable(NESARCw2_men$sex_men, NESARCw2_men$drug_crackcocaine_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#hallucinogen
round(100 * svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T)), 1)
svychisq(~sex_men+drug_hallucinogen_any, design=NESARCw2_complex, statistic="adjWald")

CrossTable(NESARCw2_men$sex_men, NESARCw2_men$drug_hallucinogen_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#inhalant
round(100 * svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T)), 1)
svychisq(~sex_men+drug_inhalant_any, design=NESARCw2_complex, statistic="adjWald")

CrossTable(NESARCw2_men$sex_men, NESARCw2_men$drug_inhalant_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#heroin
round(100 * svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==0), na.rm=T)), 1)
svychisq(~sex_men+drug_heroin_any, design=NESARCw2_complex, statistic="adjWald")

CrossTable(NESARCw2_men$sex_men, NESARCw2_men$drug_heroin_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)


### DRUG USE: BLACK MEN ###

#sedative
round(100 * svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T)), 1)
svychisq(~sex_men+drug_sedative_any, design=subset(NESARCw2_complex,black==1), statistic="adjWald")

CrossTable(NESARCw2_black$sex_men, NESARCw2_black$drug_sedative_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#tranquilizer
round(100 * svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T)), 1)
svychisq(~sex_men+drug_tranquilizer_any, design=subset(NESARCw2_complex,black==1), statistic="adjWald")

CrossTable(NESARCw2_black$sex_men, NESARCw2_black$drug_tranquilizer_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#opioid
round(100 * svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T)), 1)
svychisq(~sex_men+drug_opioid_any, design=subset(NESARCw2_complex,black==1), statistic="adjWald")

CrossTable(NESARCw2_black$sex_men, NESARCw2_black$drug_opioid_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#amphetamine
round(100 * svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T)), 1)
svychisq(~sex_men+drug_amphetamine_any, design=subset(NESARCw2_complex,black==1), statistic="adjWald")

CrossTable(NESARCw2_black$sex_men, NESARCw2_black$drug_amphetamine_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#cannabis
round(100 * svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T)), 1)
svychisq(~sex_men+drug_cannabis_any, design=subset(NESARCw2_complex,black==1), statistic="adjWald")

CrossTable(NESARCw2_black$sex_men, NESARCw2_black$drug_cannabis_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#crackcocaine
round(100 * svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T)), 1)
svychisq(~sex_men+drug_crackcocaine_any, design=subset(NESARCw2_complex,black==1), statistic="adjWald")

CrossTable(NESARCw2_black$sex_men, NESARCw2_black$drug_crackcocaine_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#hallucinogen
round(100 * svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T)), 1)
svychisq(~sex_men+drug_hallucinogen_any, design=subset(NESARCw2_complex,black==1), statistic="adjWald")

CrossTable(NESARCw2_black$sex_men, NESARCw2_black$drug_hallucinogen_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#inhalant
round(100 * svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T)), 1)
svychisq(~sex_men+drug_inhalant_any, design=subset(NESARCw2_complex,black==1), statistic="adjWald")

CrossTable(NESARCw2_black$sex_men, NESARCw2_black$drug_inhalant_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#heroin
round(100 * svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==1 & black==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==0 & black==1), na.rm=T)), 1)
svychisq(~sex_men+drug_heroin_any, design=subset(NESARCw2_complex,black==1), statistic="adjWald")

CrossTable(NESARCw2_black$sex_men, NESARCw2_black$drug_heroin_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)


### DRUG USE: WHITE MEN ###

#sedative
round(100 * svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T)), 1)
svychisq(~sex_men+drug_sedative_any, design=subset(NESARCw2_complex,white==1), statistic="adjWald")

CrossTable(NESARCw2_white$sex_men, NESARCw2_white$drug_sedative_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#tranquilizer
round(100 * svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T)), 1)
svychisq(~sex_men+drug_tranquilizer_any, design=subset(NESARCw2_complex,white==1), statistic="adjWald")

CrossTable(NESARCw2_white$sex_men, NESARCw2_white$drug_tranquilizer_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#opioid
round(100 * svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T)), 1)
svychisq(~sex_men+drug_opioid_any, design=subset(NESARCw2_complex,white==1), statistic="adjWald")

CrossTable(NESARCw2_white$sex_men, NESARCw2_white$drug_opioid_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#amphetamine
round(100 * svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T)), 1)
svychisq(~sex_men+drug_amphetamine_any, design=subset(NESARCw2_complex,white==1), statistic="adjWald")

CrossTable(NESARCw2_white$sex_men, NESARCw2_white$drug_amphetamine_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#cannabis
round(100 * svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T)), 1)
svychisq(~sex_men+drug_cannabis_any, design=subset(NESARCw2_complex,white==1), statistic="adjWald")

CrossTable(NESARCw2_white$sex_men, NESARCw2_white$drug_cannabis_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#crackcocaine
round(100 * svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T)), 1)
svychisq(~sex_men+drug_crackcocaine_any, design=subset(NESARCw2_complex,white==1), statistic="adjWald")

CrossTable(NESARCw2_white$sex_men, NESARCw2_white$drug_crackcocaine_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#hallucinogen
round(100 * svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T)), 1)
svychisq(~sex_men+drug_hallucinogen_any, design=subset(NESARCw2_complex,white==1), statistic="adjWald")

CrossTable(NESARCw2_white$sex_men, NESARCw2_white$drug_hallucinogen_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#inhalant
round(100 * svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T)), 1)
svychisq(~sex_men+drug_inhalant_any, design=subset(NESARCw2_complex,white==1), statistic="adjWald")

CrossTable(NESARCw2_white$sex_men, NESARCw2_white$drug_inhalant_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#heroin
round(100 * svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==1 & white==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==0 & white==1), na.rm=T)), 1)
svychisq(~sex_men+drug_heroin_any, design=subset(NESARCw2_complex,white==1), statistic="adjWald")

CrossTable(NESARCw2_white$sex_men, NESARCw2_white$drug_heroin_any, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)


### DRUG USE: HISPANIC MEN ###

#sedative
round(100 * svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_sedative_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T)), 1)
svychisq(~sex_men+drug_sedative_any, design=subset(NESARCw2_complex,hispanic==1), statistic="adjWald")

CrossTable(NESARCw2_men$sex_men[NESARCw2_men$hispanic==1], NESARCw2_men$drug_sedative_any[NESARCw2_men$hispanic==1], prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#tranquilizer
round(100 * svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_tranquilizer_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T)), 1)
svychisq(~sex_men+drug_tranquilizer_any, design=subset(NESARCw2_complex,hispanic==1), statistic="adjWald")

CrossTable(NESARCw2_men$sex_men[NESARCw2_men$hispanic==1], NESARCw2_men$drug_tranquilizer_any[NESARCw2_men$hispanic==1], prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#opioid
round(100 * svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_opioid_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T)), 1)
svychisq(~sex_men+drug_opioid_any, design=subset(NESARCw2_complex,hispanic==1), statistic="adjWald")

CrossTable(NESARCw2_men$sex_men[NESARCw2_men$hispanic==1], NESARCw2_men$drug_opioid_any[NESARCw2_men$hispanic==1], prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#amphetamine
round(100 * svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_amphetamine_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T)), 1)
svychisq(~sex_men+drug_amphetamine_any, design=subset(NESARCw2_complex,hispanic==1), statistic="adjWald")

CrossTable(NESARCw2_men$sex_men[NESARCw2_men$hispanic==1], NESARCw2_men$drug_amphetamine_any[NESARCw2_men$hispanic==1], prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#cannabis
round(100 * svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_cannabis_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T)), 1)
svychisq(~sex_men+drug_cannabis_any, design=subset(NESARCw2_complex,hispanic==1), statistic="adjWald")

CrossTable(NESARCw2_men$sex_men[NESARCw2_men$hispanic==1], NESARCw2_men$drug_cannabis_any[NESARCw2_men$hispanic==1], prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#crackcocaine
round(100 * svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_crackcocaine_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T)), 1)
svychisq(~sex_men+drug_crackcocaine_any, design=subset(NESARCw2_complex,hispanic==1), statistic="adjWald")

CrossTable(NESARCw2_men$sex_men[NESARCw2_men$hispanic==1], NESARCw2_men$drug_crackcocaine_any[NESARCw2_men$hispanic==1], prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#hallucinogen
round(100 * svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_hallucinogen_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T)), 1)
svychisq(~sex_men+drug_hallucinogen_any, design=subset(NESARCw2_complex,hispanic==1), statistic="adjWald")

CrossTable(NESARCw2_men$sex_men[NESARCw2_men$hispanic==1], NESARCw2_men$drug_hallucinogen_any[NESARCw2_men$hispanic==1], prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#inhalant
round(100 * svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_inhalant_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T)), 1)
svychisq(~sex_men+drug_inhalant_any, design=subset(NESARCw2_complex,hispanic==1), statistic="adjWald")

CrossTable(NESARCw2_men$sex_men[NESARCw2_men$hispanic==1], NESARCw2_men$drug_inhalant_any[NESARCw2_men$hispanic==1], prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)

#heroin
round(100 * svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==1 & hispanic==1), na.rm=T)), 1)
round(100 * svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T), 1)
round(100 * confint(svymean(~factor(drug_heroin_any), design=subset(NESARCw2_complex,sex_men==0 & hispanic==1), na.rm=T)), 1)
svychisq(~sex_men+drug_heroin_any, design=subset(NESARCw2_complex,hispanic==1), statistic="adjWald")

CrossTable(NESARCw2_men$sex_men[NESARCw2_men$hispanic==1], NESARCw2_men$drug_heroin_any[NESARCw2_men$hispanic==1], prop.r=F, prop.c=F, prop.t=F, prop.chisq=F, chisq=F)


### RACE COMPARISONS ###

#MSM
svychisq(~black+drug_sedative_any, design=subset(NESARCw2_complex,sex_men==1), statistic="adjWald")
svychisq(~black+drug_tranquilizer_any, design=subset(NESARCw2_complex,sex_men==1), statistic="adjWald")
svychisq(~black+drug_opioid_any, design=subset(NESARCw2_complex,sex_men==1), statistic="adjWald")
svychisq(~black+drug_amphetamine_any, design=subset(NESARCw2_complex,sex_men==1), statistic="adjWald")
svychisq(~black+drug_cannabis_any, design=subset(NESARCw2_complex,sex_men==1), statistic="adjWald")
svychisq(~black+drug_crackcocaine_any, design=subset(NESARCw2_complex,sex_men==1), statistic="adjWald")
svychisq(~black+drug_hallucinogen_any, design=subset(NESARCw2_complex,sex_men==1), statistic="adjWald")
svychisq(~black+drug_inhalant_any, design=subset(NESARCw2_complex,sex_men==1), statistic="adjWald")
svychisq(~black+drug_heroin_any, design=subset(NESARCw2_complex,sex_men==1), statistic="adjWald")

#non-MSM
svychisq(~black+drug_sedative_any, design=subset(NESARCw2_complex,sex_men==0), statistic="adjWald")
svychisq(~black+drug_tranquilizer_any, design=subset(NESARCw2_complex,sex_men==0), statistic="adjWald")
svychisq(~black+drug_opioid_any, design=subset(NESARCw2_complex,sex_men==0), statistic="adjWald")
svychisq(~black+drug_amphetamine_any, design=subset(NESARCw2_complex,sex_men==0), statistic="adjWald")
svychisq(~black+drug_cannabis_any, design=subset(NESARCw2_complex,sex_men==0), statistic="adjWald")
svychisq(~black+drug_crackcocaine_any, design=subset(NESARCw2_complex,sex_men==0), statistic="adjWald")
svychisq(~black+drug_hallucinogen_any, design=subset(NESARCw2_complex,sex_men==0), statistic="adjWald")
svychisq(~black+drug_inhalant_any, design=subset(NESARCw2_complex,sex_men==0), statistic="adjWald")
svychisq(~black+drug_heroin_any, design=subset(NESARCw2_complex,sex_men==0), statistic="adjWald")



