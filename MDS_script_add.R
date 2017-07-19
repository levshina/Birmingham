library(cluster)
causatives.dist <- daisy(causatives[, 3:20])
causatives.mds <- mds(causatives.dist) 
plot(causatives.mds$conf, type = "n", main = "Germanic letting verbs")
points(jitter(causatives.mds$conf[causatives$ENG == "let",], amount = 0.1), col = "blue", pch = 16)
points(jitter(causatives.mds$conf[causatives$GER == "lassen",], amount = 0.1), col = "red", pch = 16)
points(jitter(causatives.mds$conf[causatives$DUT == "laten",], amount = 0.1), col = "green", pch = 16)
points(jitter(causatives.mds$conf[causatives$SWE == "lata",], amount = 0.1), col = "gray", pch = 16)
points(jitter(causatives.mds$conf[causatives$NOR == "la",], amount = 0.1), col = "purple", pch = 16)

legend("bottomleft", legend = c("let", "lassen", "laten", "lata", "la"), col = c("blue", "red", "green", "gray", "purple"), pch = 16, bty = "n")

y.eng <- ifelse(causatives$ENG == "let", 1, 0)
y.ger <- ifelse(causatives$GER == "lassen", 1, 0)
y.dut <- ifelse(causatives$DUT == "laten", 1, 0)
y.swe <- ifelse(causatives$SWE == "lata", 1, 0)
y.nor <- ifelse(causatives$NOR == "la", 1, 0)

library(fields)
Krig.eng <- Krig(causatives.mds$conf, y.eng, lambda = 0.05)
Krig.ger <- Krig(causatives.mds$conf, y.ger, lambda = 0.05)
Krig.dut <- Krig(causatives.mds$conf, y.dut, lambda = 0.05)
Krig.swe <- Krig(causatives.mds$conf, y.swe, lambda = 0.05)
Krig.nor <- Krig(causatives.mds$conf, y.nor, lambda = 0.05)

par(mfrow = c(2, 3))
surface(Krig.eng, main = "ENG")
surface(Krig.ger, main = "GER")
surface(Krig.dut, main = "DUT")
surface(Krig.swe, main = "SWE")
surface(Krig.nor, main = "NOR")


