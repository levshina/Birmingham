#BP based on one variable
speak.bp <- prop.table(as.matrix(speak), 1)
speak.dist <- dist(speak.bp, method = "canberra")
speak.clust <- hclust(speak.dist, method = "ward.D2")
plot(speak.clust)

#BP based on several variables

bp <- function (data) 
{
  y <- c()
  for (v in 1:ncol(data)) {
    y1 <- prop.table(table(data[, v]))
    names(y1) <- paste(colnames(data)[v], names(y1), sep = ".")
    y <- c(y, y1)
  }
  return(y)
}

see.split <- split(see, see$Sense)
see.split <- lapply(see.split, function(x) x = x[, -1])
see.split.bp <- lapply(see.split, bp)
see.bp <- do.call(rbind, see.split.bp)
see.dist <- dist(see.bp, method = "canberra")
see.clust <- hclust(see.dist, method = "ward.D2") 
plot(see.clust)