
#' Finding PP tree structure using various indices
#' 
#' Find tree structure using various projection pursuit indices of classification in each split.
#' @usage PPtree_split(PPmethod,  i.class, i.data,size.p, weight = TRUE, ...) 
#' @param PPmethod method for projection pursuit, LDA, PDA, Lp, Gini, Enp
#' @param i.data A training data  without class information
#' @param i.class class information
#' @param size.p proportion of sample variable in each split
#' @param weight weight flag using in LDA index
#' @return Tree.Struct Tree structure of PPtree result
#' @return Alpha.Keep 1D projections of each split
#' @return C.Keep spliting rules for each split
#' @export
#' @examples
#' training<-train_fn(iris[,5],.9)
#' data1<-iris[,5:1]
#' Tree.result <- PPtree_split("LDA", data1[training,1], data1[training,2:5],size.p=0.9)
PPtree_split<-function (PPmethod, i.class, i.data,size.p=0.9, weight = TRUE, r = NULL, 
          lambda = NULL, cooling = 0.999, temp = 1, energy = 0.01, 
          ...) 
{
  i.data <- as.matrix(i.data)
  Find.proj <- function(i.class, i.data, PPmethod, r, lambda, 
                        ...) {
    n <- nrow(i.data) #number of rows of data without class
    p <- ncol(i.data) #number of columns of data without class 
    g <- table(i.class) #table of class data
    g.name <- as.numeric(as.factor(names(g)))#I have added as.factor, change the classes to numbers
    G <- length(g) #number of classes
    
    a <- PPtree::PP.optimize.anneal(PPmethod, 1, i.data, i.class, 
                            std = TRUE, cooling, temp, energy, r, lambda) #optimal projection and optimal index
    proj.data <- as.matrix(i.data) %*% a$proj.best #projected data
    sign <- sign(a$proj.best[abs(a$proj.best) == max(abs(a$proj.best))])# sign of the abs maximum optimal projected value
    index <- (1:p) * (abs(a$proj.best) == max(abs(a$proj.best)))#identify the index place
    index <- index[index > 0]
    if (G == 2) {
      class <- i.class
    }
    else {
      m <- tapply(c(proj.data), i.class, mean) #mean by class
      sd <- tapply(c(proj.data), i.class, sd) # sd by class
      sd.sort <- sort.list(sd) #sort sd by class
      m.list <- sort.list(m)#sort mean by class
      m.sort <- sort(m) #sort mean values of classes
      m.name <- as.numeric(as.factor(names(m.sort))) #sort names
      G <- length(m) #numeber of classes
      dist <- 0
      split <- 0
      for (i in 1:(G - 1)) { #for in classes -1
        if (m[m.list[i + 1]] - m[m.list[i]] > dist) { #dif in means is bigger than dist split
          split <- i #number of split is number of classes -1
          dist <- m[m.list[i + 1]] - m[m.list[i]] #mean diff between classes
        }
      }
      class <- rep(0, n) #zeros for class 
      for (i in 1:split) class <- class + (i.class == m.name[i]) #for each split the name
      class <- 2 - class
      g <- table(class)
      g.name <- as.numeric(names(g))
      G <- length(g)
      n <- nrow(i.data)
      a <- PPtree::PP.optimize.anneal(PPmethod, 1, i.data, class, 
                              std = TRUE, cooling, temp, energy, r, lambda)
      if (sign != sign(a$proj.best[index])) 
        a$proj.best <- -a$proj.best
      proj.data <- as.matrix(i.data) %*% a$proj.best
    }
    m.LR <- tapply(proj.data, class, mean)
    m.LR.sort <- sort(m.LR)
    LR.name <- as.numeric(names(m.LR.sort))
    var.LR <- tapply(proj.data, class, var)
    median.LR <- tapply(proj.data, class, median)
    IQR.LR <- tapply(proj.data, class, IQR)
    n.LR <- table(class)
    n.name <- as.numeric(names(n.LR))
    var.T <- sum(var.LR * n.LR)/sum(n.LR)
    if (LR.name[1] != n.name[1]) {
      temp <- n.LR[1]
      n.LR[1] <- n.LR[2]
      n.LR[2] <- temp
    }
    c1 <- (m.LR[1] + m.LR[2])/2
    c2 <- (m.LR[1] * n.LR[1] + m.LR[2] * n.LR[2])/sum(n.LR)
    max.LR <- tapply(proj.data, class, max)
    min.LR <- tapply(proj.data, class, min)
    c3 <- (m.LR[1] * var.LR[2] + m.LR[2] * var.LR[1])/sum(var.LR)
    c4 <- (m.LR[1] * sqrt(var.LR[2]/n.LR[2]) + m.LR[2] * 
             sqrt(var.LR[1]/n.LR[1]))/(sqrt(var.LR[1]/n.LR[1]) + 
                                         sqrt(var.LR[2]/n.LR[2]))
    c5 <- (m.LR[1] * IQR.LR[2] + m.LR[2] * IQR.LR[1])/sum(IQR.LR)
    c6 <- (m.LR[1] * (IQR.LR[2]/n.LR[2]) + m.LR[2] * (IQR.LR[1]/n.LR[1]))/((IQR.LR[1]/n.LR[1]) + 
                                                                             (IQR.LR[2]/n.LR[2]))
    C <- c(c1, c2, c3, c4, c5, c6)
    Index <- a$index.best
    Alpha <- t(a$proj.best)
    IOindexR <- NULL
    IOindexL <- NULL
    sort.LR <- as.numeric(names(sort(m.LR)))
    IOindexL <- class == sort.LR[1]
    IOindexR <- class == sort.LR[2]
    list(Index = Index, Alpha = Alpha, C = C, IOindexL = IOindexL, 
         IOindexR = IOindexR)
  }
  Tree.construct <- function(i.class, i.data, Tree.Struct,size.p, 
                             id, rep, rep1, rep2, Alpha.Keep, index.var,C.Keep, PPmethod, r = NULL, 
                             lambda = NULL, ...) {
    i.class <- as.integer(i.class)
    aux <- var_select(i.data,size.p)
    n <- nrow(i.data)
    g <- table(i.class)
    G <- length(g)
    if (length(Tree.Struct) == 0) {
      Tree.Struct <- matrix(1:(2 * G - 1), ncol = 1)
      Tree.Struct <- cbind(Tree.Struct, 0, 0, 0, 0)
    }
    if (G == 1) {
      Tree.Struct[id, 3] <- as.numeric(names(g))
      list(Tree.Struct = Tree.Struct, Alpha.Keep = Alpha.Keep,index.var, 
           C.Keep = C.Keep, rep = rep, rep1 = rep1, rep2 = rep2)
    }
    else {
      Tree.Struct[id, 2] <- rep1
      rep1 <- rep1 + 1
      Tree.Struct[id, 3] <- rep1
      rep1 <- rep1 + 1
      Tree.Struct[id, 4] <- rep2
      rep2 <- rep2 + 1
      a <- Find.proj(i.class, i.data[,aux], PPmethod, r, lambda)
      C.Keep <- rbind(C.Keep, a$C)
      Tree.Struct[id, 5] <- a$Index
      
      a1 <- rep(0,dim(i.data)[2])
      a1[aux]<-a$Alpha
      Alpha.Keep <- rbind( Alpha.Keep, a1)
      index.var <- rbind(index.var,aux)
       t.class <- i.class
      t.data <- i.data
      t.class <- t.class * a$IOindexL
      t.n <- length(t.class[t.class == 0])
      t.index <- sort.list(t.class)
      t.index <- sort(t.index[-(1:t.n)])
      t.class <- t.class[t.index]
      t.data <- i.data[t.index, ]
      b <- Tree.construct(t.class, t.data, Tree.Struct, size.p,
                          Tree.Struct[id, 2], rep, rep1, rep2, Alpha.Keep,index.var, 
                          C.Keep, PPmethod, r, lambda)
      Tree.Struct <- b$Tree.Struct
      Alpha.Keep <- b$Alpha.Keep
      C.Keep <- b$C.Keep
      index.var <- b$index.var
      rep <- b$rep
      rep1 <- b$rep1
      rep2 <- b$rep2
      t.class <- i.class
      t.data <- i.data
      t.class <- (t.class * a$IOindexR)
      t.n <- length(t.class[t.class == 0])
      t.index <- sort.list(t.class)
      t.index <- sort(t.index[-(1:t.n)])
      t.class <- t.class[t.index]
      t.data <- i.data[t.index, ]
      n <- nrow(t.data)
      G <- length(table(t.class))
      b <- Tree.construct(t.class, t.data, Tree.Struct, size.p,
                          Tree.Struct[id, 3], rep, rep1, rep2, Alpha.Keep, index.var,
                          C.Keep, PPmethod, r, lambda)
      Tree.Struct <- b$Tree.Struct
      Alpha.Keep <- b$Alpha.Keep
      C.Keep <- b$C.Keep
      index.var <- b$index.var
      rep <- b$rep
      rep1 <- b$rep1
      rep2 <- b$rep2
    }
    list(Tree.Struct = Tree.Struct, Alpha.Keep = Alpha.Keep, index.var=index.var,
         C.Keep = C.Keep, rep = rep, rep1 = rep1, rep2 = rep2)
  }
  C.Keep <- NULL
  Alpha.Keep <- NULL
  Tree.Struct <- NULL
  index.var <- NULL
  id <- 1
  rep1 <- 2
  rep2 <- 1
  rep <- 1
  if (PPmethod == "LDA" && weight) 
    method <- 1
  else if (PPmethod == "LDA" && !weight) 
    method <- 2
  else if (PPmethod == "Lp") 
    method <- 3
  else if (PPmethod == "Gini") 
    method <- 4
  else if (PPmethod == "Ent") 
    method <- 5
  else if (PPmethod == "PDA") 
    method <- 6
  else stop("Wrong PPmethod")
  Tree.final <- Tree.construct(i.class, i.data, Tree.Struct, size.p,
                               id, rep, rep1, rep2, Alpha.Keep, index.var,C.Keep, PPmethod, r, 
                               lambda)
  Tree.Struct <- Tree.final$Tree.Struct
  colnames(Tree.Struct) <- c("id", "L.node.ID", "R.F.node.ID", 
                             "Coef.ID", "Index")
  Alpha.Keep <- Tree.final$Alpha.Keep
  C.Keep <- Tree.final$C.Keep
  index.var <- Tree.final$index.var
  list(Tree.Struct = Tree.Struct, Alpha.Keep =Alpha.Keep,  
       C.Keep = C.Keep)
}