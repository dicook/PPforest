#' Find tree structure using LDA projection pursuit indice of classification in each split.
#'
#' @usage LDAtree_split(fr,data, weight = TRUE,std=TRUE,size.p=0.9, ...) 
#' @param fr as.formula argument, as.formula('class~.')
#' @param data  is a the complete data frame with class information in the first column
#' @param weight weight flag using in LDA index?
#' @param std=TRUE remove the variables with 0 variance.
#' @param size.p proportion of sample variable in each split
#' @return Tree.Struct Tree structure of PPtree result
#' @export
#' @examples
#' training<-train_fn2(iris[,5],.9)
#' data1<-iris[training$id,5:1]
#' Tree.result <- LDAtree_split(as.formula('Species~.'), data=data1 ,size.p=0.9)

LDAtree_split <- function (fr,data, weight = TRUE,std=TRUE,size.p=0.9, ...) 
{
  mf <- model.frame(fr, data=data)
  i.class <- model.response(mf)
  i.data  <- data[, -1]
  i.data <- as.matrix(i.data)
  
  Find.proj <- function(i.class, i.data, ...) {
    
    i.data.ori <- i.data  #original data set
    
    pp <- ncol(i.data)
    if (std) { #remove the variable with zero variance same as in PP.optimize anneal
      remove <- (1:pp) * (apply(i.data, 2, sd) == 0)
      remove <- remove[remove != 0]
      if (length(remove) != 0) {
        i.data <- i.data[, -remove]
      }
    }
    
    v.rnd <- var_select(i.data,size.p) #random variable selection
    vari <- dim(i.data.ori)[2] # number of variables
    i.data <- i.data[,v.rnd] #data with selected variables
    
    
    #--
    n <- nrow(i.data)
    p <- ncol(i.data)
    g <- table(i.class)
    g.name <- as.numeric(factor(names(g)))
    G <- length(g)
    
    a.proj.best <- MASS::lda(i.data, i.class)[[4]][, 1]
    proj.data <- as.matrix(i.data) %*% a.proj.best
    sign <- sign(a.proj.best[abs(a.proj.best) == max(abs(a.proj.best))])
    index <- (1:p) * (abs(a.proj.best) == max(abs(a.proj.best)))
    index <- index[index > 0]
    if (G == 2) {
      class <- i.class
    }
    else {
      m <- tapply(c(proj.data), i.class, mean)
      sd <- tapply(c(proj.data), i.class, sd)
      sd.sort <- sort.list(sd)
      m.list <- sort.list(m)
      m.sort <- sort(m)
      m.name <- as.numeric(factor(names(m.sort)))
      G <- length(m)
      dist <- 0
      split <- 0
      for (i in 1:(G - 1)) {
        if (m[m.list[i + 1]] - m[m.list[i]] > dist) {
          split <- i
          dist <- m[m.list[i + 1]] - m[m.list[i]]
        }
      }
      class <- rep(0, n)
      for (i in 1:split) class <- class + (as.numeric(as.factor(i.class)) == 
                                             m.name[i])
      class <- 2 - class
      g <- table(class)
      g.name <- as.numeric(names(g))
      G <- length(g)
      n <- nrow(i.data)
      a.proj.best <- MASS::lda(i.data, factor(class))[[4]][, 
                                                     1]
      if (sign != sign(a.proj.best[index])) 
        a.proj.best <- -a.proj.best
      proj.data <- as.matrix(i.data) %*% a.proj.best
    }
    m.LR <- tapply(proj.data, class, mean)
    m.LR.sort <- sort(m.LR)
    LR.name <- as.numeric(names(m.LR.sort))
    var.LR <- tapply(proj.data, class, var)
    IQR.LR <- tapply(proj.data, class, IQR)
    median.LR <- tapply(proj.data, class, median)
    n.LR <- table(class)
    n.name <- as.numeric(names(n.LR))
    var.T <- sum(var.LR * n.LR)/sum(n.LR)
    if (LR.name[1] != n.name[1]) {
      temp <- n.LR[1]
      n.LR[1] <- n.LR[2]
      n.LR[2] <- temp
    }
    c1 <- (m.LR[1] + m.LR[2])/2
    c2 <- (m.LR[1] * n.LR[2] + m.LR[2] * n.LR[1])/sum(n.LR)
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
    
    a1 <- rep(0, pp) # zeros lenght original variables    
    a1[v.rnd] <- t(a.proj.best) #best.projt with selected variables and 0 in no selected original length      
    Alpha <- a1
    IOindexR <- NULL
    IOindexL <- NULL
    sort.LR <- as.numeric(names(sort(m.LR)))
    IOindexL <- class == sort.LR[1]
    IOindexR <- class == sort.LR[2]
    list(Alpha = Alpha, C = C, IOindexL = IOindexL, IOindexR = IOindexR)
  }
  
  Tree.construct <- function(i.class, i.data, Tree.Struct, 
                             id, rep, rep1, rep2, Alpha.Keep, C.Keep, ...) {
    i.class <- as.integer(i.class)
    n <- nrow(i.data)
    g <- table(i.class)
    G <- length(g)
    if (length(Tree.Struct) == 0) {
      Tree.Struct <- matrix(1:(2 * G - 1), ncol = 1)
      Tree.Struct <- cbind(Tree.Struct, 0, 0, 0, 0)
    }
    if (G == 1) {
      Tree.Struct[id, 3] <- as.numeric(names(g))
      list(Tree.Struct = Tree.Struct, Alpha.Keep = Alpha.Keep, 
           C.Keep = C.Keep, rep = rep, rep1 = rep1, rep2 = rep2)
    }
    else {
      Tree.Struct[id, 2] <- rep1
      rep1 <- rep1 + 1
      Tree.Struct[id, 3] <- rep1
      rep1 <- rep1 + 1
      Tree.Struct[id, 4] <- rep2
      rep2 <- rep2 + 1
      a <- Find.proj(i.class, i.data)
      C.Keep <- rbind(C.Keep, a$C)
      Alpha.Keep <- rbind(Alpha.Keep, a$Alpha)
      t.class <- i.class
      t.data <- i.data
      t.class <- t.class * a$IOindexL
      t.n <- length(t.class[t.class == 0])
      t.index <- sort.list(t.class)
      t.index <- sort(t.index[-(1:t.n)])
      t.class <- t.class[t.index]
      t.data <- i.data[t.index, ]
      b <- Tree.construct(t.class, t.data, Tree.Struct, 
                          Tree.Struct[id, 2], rep, rep1, rep2, Alpha.Keep, 
                          C.Keep)
      Tree.Struct <- b$Tree.Struct
      Alpha.Keep <- b$Alpha.Keep
      C.Keep <- b$C.Keep
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
      b <- Tree.construct(t.class, t.data, Tree.Struct, 
                          Tree.Struct[id, 3], rep, rep1, rep2, Alpha.Keep, 
                          C.Keep)
      Tree.Struct <- b$Tree.Struct
      Alpha.Keep <- b$Alpha.Keep
      C.Keep <- b$C.Keep
      rep <- b$rep
      rep1 <- b$rep1
      rep2 <- b$rep2
    }
    list(Tree.Struct = Tree.Struct, Alpha.Keep = Alpha.Keep, 
         C.Keep = C.Keep, rep = rep, rep1 = rep1, rep2 = rep2)
  }
  C.Keep <- NULL
  Alpha.Keep <- NULL
  Tree.Struct <- NULL
  id <- 1
  rep1 <- 2
  rep2 <- 1
  rep <- 1
  Tree.final <- Tree.construct(i.class, i.data, Tree.Struct, 
                               id, rep, rep1, rep2, Alpha.Keep, C.Keep)
  Tree.Struct <- Tree.final$Tree.Struct
  colnames(Tree.Struct) <- c("id", "L.node.ID", "R.F.node.ID", 
                             "Coef.ID", "Index")
  Alpha.Keep <- Tree.final$Alpha.Keep
  C.Keep <- Tree.final$C.Keep
  list(Tree.Struct = Tree.Struct, Alpha.Keep = Alpha.Keep, 
       C.Keep = C.Keep)
}
