#add-on packages: cluster, fields, grDevices, googleVis, smacof

#creating a distance matrix with Gower distances
library(cluster)
causatives.dist <- daisy(causatives[, 3:20])
summary(causatives.dist)
causatives.dist[1:3]
causatives[c(1, 2),]
causatives[c(1, 3),]
causatives[c(1, 4),]

#metric MDS (for ratio-scaled data)
library(smacof)
causatives.mds <- mds(causatives.dist) #by default, metric MDS. For non-metric, try type = "ordinal". 

#exploring the MDS solution
causatives.mds$stress
d1 <- mds(causatives.dist, ndim = 1)$stress 
d2 <- mds(causatives.dist, ndim = 2)$stress
d3 <- mds(causatives.dist, ndim = 3)$stress
d4 <- mds(causatives.dist, ndim = 4)$stress
d5 <- mds(causatives.dist, ndim = 5)$stress

#making a scree plot
plot(1:5, c(d1, d2, d3, d4, d5), type = "b", xlab = "n of dimensions", ylab = "Stress")

#Shepard plot
plot(causatives.mds, "Shepard")

#exploring the contexts

plot(causatives.mds, "conf")

library(googleVis)
text.df <- data.frame(Text = causatives$Text, Dim1 = causatives.mds$conf[, 1], Dim2 = causatives.mds$conf[, 2], ENG = causatives$ENG)
bubbles <- gvisBubbleChart(text.df, idvar = "Text", xvar = "Dim1", yvar = "Dim2", colorvar = "ENG", options = list(sizeAxis = '{maxSize: 10}', vAxis = '{minValue:-0.8, maxValue:0.8}', height = 500, width = 500, bubble="{textStyle:{color: 'none'}}"))
plot(bubbles)

#investigating form-meaning mapping
plot(causatives.mds$conf, type = "n")
text(causatives.mds$conf, labels = causatives$ENG)

#adding jitter
jitter(1:5, amount = 0.1)
jitter(1:5, amount = 0.01)

plot(causatives.mds$conf, type = "n", main = "fare vs. face")
points(jitter(causatives.mds$conf[causatives$ROM == "face",], amount = 0.1), col = "blue", pch = 16)
points(jitter(causatives.mds$conf[causatives$ITA == "fare",], amount = 0.1), col = "red", pch = 16)
legend("bottomright", legend = c("fare", "face"), col = c("red", "blue"), pch = 16, bty = "n")

#transparent colours
library(grDevices)
plot(causatives.mds$conf, type = "n", main = "fare vs. face")
points(causatives.mds$conf[causatives$ROM == "face",], col = adjustcolor("blue", alpha.f = 0.5), pch = 16, cex = 1.5)
points(causatives.mds$conf[causatives$ITA == "fare",], col = adjustcolor("red", alpha.f = 0.5), pch = 16, cex = 1.5)
legend("bottomright", legend = c("fare", "face"), col = c("red", "blue"), pch = 16, bty = "n")

#Kriging
y.ita <- ifelse(causatives$ITA == "fare", 1, 0)
y.fra <- ifelse(causatives$FRA == "faire", 1, 0)
y.spa <- ifelse(causatives$SPA == "hacer", 1, 0)
y.por <- ifelse(causatives$POR == "fazer", 1, 0)
y.rom <- ifelse(causatives$ROM == "face", 1, 0)

library(fields)
Krig.rom <- Krig(causatives.mds$conf, y.rom, lambda = 0.05) #try different lambda values
Krig.ita <- Krig(causatives.mds$conf, y.ita, lambda = 0.05)
Krig.fra <- Krig(causatives.mds$conf, y.fra, lambda = 0.05)
Krig.spa <- Krig(causatives.mds$conf, y.spa, lambda = 0.05)
Krig.por <- Krig(causatives.mds$conf, y.por, lambda = 0.05)

par(mfrow = c(2, 3))
surface(Krig.ita, main = "ITA")
surface(Krig.fra, main = "FRA")
surface(Krig.spa, main = "SPA")
surface(Krig.por, main = "POR")
surface(Krig.rom, main = "ROM")

#A 3D perspective plot
surface(Krig.ita, main = "ITA", type = "p", theta = 40, phi = 40)