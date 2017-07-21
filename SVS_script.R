#add-on packages: cluster, lsa

#Transform frequencies into Pointwise Mutual Information scores
verbframes.exp <- chisq.test(verbframes)$expected
verbframes.pmi <- log2(verbframes/verbframes.exp)

#Transform PMI into Positive PMI
verbframes.ppmi <- verbframes.pmi
verbframes.ppmi[verbframes.ppmi < 0] <- 0

#An ad hoc list of verbs
verblist <- c("give" ,"send", "offer","hand", "create", "build", 
              "produce", "jump", "come", "go", "walk", "tell", "say", "speak", 
              "talk", "communicate","think","believe","assume", "understand",
              "discuss", "expect", "see",  "hear",  "feel",  "watch", "listen",  
              "start", "begin", "finish")

#Make a selection for plotting
verbframes.short <- verbframes.ppmi[rownames(verbframes) %in% verblist,]

#Compute cosine similarity ("t" swaps the rows and columns)
library(lsa)
verbframes.cos <- cosine(t(verbframes.short))

#Assign zeros to diagonal elements (similarity of a word to itself)
diag(verbframes.cos) <- 0

#Find which verbs are maximally similar
which(verbframes.cos == max(verbframes.cos), arr.ind = TRUE)
max(verbframes.cos)

#Look at how similar give is to the other verbs
verbframes.cos[rownames(verbframes.cos) == "give", ]
#Round the numbers up to three decimal points
round(verbframes.cos[rownames(verbframes.cos) == "give", ], 3)

#Transform the cosine scores into distances
verbframes.dist <- as.dist(1 - verbframes.cos)
verbframes.clust <- hclust(verbframes.dist, method = "ward.D2")
plot(verbframes.clust)
rect.hclust(verbframes.clust, k = 3)

#Compare the cluster [come, go, walk, jump] with the cluster [hear, listen, talk, speak]  
verbs1 <- c("come", "go", "walk", "jump")
verbs2 <- c("hear", "listen", "talk", "speak")

cluster1 <- verbframes.short[rownames(verbframes.short) %in% verbs1,]
cluster2 <- verbframes.short[rownames(verbframes.short) %in% verbs2,]

cluster1.means <- colMeans(cluster1)
cluster2.means <- colMeans(cluster2)

diff <- cluster1.means - cluster2.means
#Make a snake plot
plot(sort(diff), 1:length(diff), type = "n", xlab = "cluster 2 <---> cluster 1", yaxt = "n", ylab = "")
text(sort(diff), 1:length(diff), names(sort(diff)))

#Try divisive clustering (partitioning around medoids) with 3 clusters
library(cluster)
verbframes.pam <- pam(verbframes.dist, k = 3)
verbframes.pam$clustering