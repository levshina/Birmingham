#add-on packages: ca, FactoMineR, rgl

#Simple Correspondence Analysis
library(ca)
speak.ca <- ca(speak)
plot(speak.ca) #the first two dimensions, by default
plot(speak.ca, dim = 2:3) #dimensions 2 and 3 
plot3d.ca(speak.ca) #interactive 3D plot
summary(speak.ca)

#Multiple Correspondence Analysis
#Prepare the data for analyses: only Germanic languages, conflate all values other than the let-causative, including NA
causatives_germ <- causatives[, c(1, 4, 5, 7, 8, 20)]
causatives_germ$ENG <- as.factor(y.eng)
causatives_germ$GER <- as.factor(y.ger)
causatives_germ$DUT <- as.factor(y.dut)
causatives_germ$SWE <- as.factor(y.swe)
causatives_germ$NOR <- as.factor(y.nor)

levels(causatives_germ$ENG) <- c("Other", "let")
levels(causatives_germ$GER) <- c("Other", "lassen")
levels(causatives_germ$DUT) <- c("Other", "laten")
levels(causatives_germ$SWE) <- c("Other", "lata")
levels(causatives_germ$NOR) <- c("Other", "la")

causatives_germ$ENG[is.na(causatives_germ$ENG)]<- "Other"
causatives_germ$GER[is.na(causatives_germ$GER)]<- "Other"
causatives_germ$DUT[is.na(causatives_germ$DUT)]<- "Other"
causatives_germ$SWE[is.na(causatives_germ$SWE)]<- "Other"
causatives_germ$NOR[is.na(causatives_germ$NOR)]<- "Other"

causatives_germ$Film <- as.character(causatives_germ$Film)
causatives_germ$Film[causatives_germ$Film == "Avatar "] <- "Avatar"
causatives_germ$Film <- as.factor(causatives_germ$Film)

library(FactoMineR)
causatives.mca <- MCA(causatives_germ, quali.sup = 1) #one supplementary qualitative variable (Film)
plot(causatives.mca)
plot(causatives.mca, inv = "ind") #make the row names invisible