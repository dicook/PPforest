#' Tree with PDA 
#' 
#' Find tree structure using penalized discriminant(PD) in each split. 
#' @usage PDA.Tree(origclass, origdata, weight = TRUE, lambda=0.1, ...)
#' @param origclass class information
#' @param origdata A training data  without class information
#' @param weight weight flag in PDA index
#' @param lambda a parameter for PDA index
#' @param ... arguments to be passed to methods
#' @return Tree.Struct Tree structure of PPtree result
#' @return projbest.node 1-dim optimal projections of each split node
#' @return splitCutoff.node cutoff values of each split node 
#' @return origclass original class 
#' @return origdata original data
#' @references Lee, YD, Cook, D., Park JW, and Lee, EK(2013) 
#' PPtree: Projection pursuit classification tree, 
#' Electronic Journal of Statistics, 7:1369-1386.
#' @export
#' @keywords tree
#' @examples
#' training<-train_fn2(iris[,5],.9)
#' data1<-iris[training$id,5:1]
#' Tree.result <- PDAtree_split(fr=as.formula('Species~.'), weight=TRUE,std=TRUE,data=data1 ,size.p=0.9,lambda=.1)
#' 
PDAtree_split<-function(fr,data, weight = TRUE,std=TRUE,size.p=0.9,lambda=0.1,...) 
{
  mf <- model.frame(fr, data=data)
  origclass <- model.response(mf)
  origdata  <- data[, -1]
  origdata <- as.matrix(origdata)
  
  
   
    Find.proj <- function(origclass, origdata,lambda, weight, ...) {
      
      i.data.ori <- origdata  #original data set
      
      
      pp <- ncol(origdata)
      if (std) { #remove the variable with zero variance same as in PP.optimize anneal
        #remove <- (1:pp) *apply( (apply(i.data,2, function(x) tapply(x, i.class, sd)) == 0),2,function(x) sum(x)!=0)
        remove <- (1:pp) * (apply(origdata, 2, sd) == 0)
        remove <- remove[remove!= 0]
        if (length(remove) != 0) {
          origdata <- origdata[, -remove]
        }
      }
      
      v.rnd <- var_select(origdata,size.p) #random variable selection
      vari <- dim(i.data.ori)[2] # number of variables
      origdata <- origdata[,v.rnd] #data with selected variables
      
      
      ########---
      
        n <- nrow(origdata)
        p <- ncol(origdata)
        g <- table(origclass)
        g.name <- as.numeric(factor(names(g)))
        G <- length(g)
        a.proj.best <- PDAopt(as.numeric(as.factor(origclass)),
                                                    origdata,weight, q=1, lambda)$projbest        
        proj.data <- as.matrix(origdata) %*% a.proj.best
        sign <- sign(a.proj.best[abs(a.proj.best) == max(abs(a.proj.best))])
        index <- (1:p) * (abs(a.proj.best) == max(abs(a.proj.best)))
        index <- index[index > 0]
        if (G == 2) {
            class <- origclass
        }
        else {
            m <- tapply(c(proj.data), origclass, mean)
            sd <- tapply(c(proj.data), origclass, sd)
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
            for (i in 1:split) class <- class + (as.numeric(factor(origclass)) == m.name[i])
            class <- 2 - class
            g <- table(class)
            g.name <- as.numeric(names(g))
            G <- length(g)
            n <- nrow(origdata)
            a.proj.best <- PDAopt(as.numeric(as.factor(class)),
                                                        origdata,weight,q=1,lambda)$projbest
            if (sign != sign(a.proj.best[index])) 
                a.proj.best <- -a.proj.best
            proj.data <- as.matrix(origdata) %*% a.proj.best
        }
        m.LR <- tapply(proj.data, class, mean)
        temp.list<-sort.list(m.LR)
        m.LR<-m.LR[temp.list]
      sd.LR <- tapply(proj.data, class, function(x) ifelse(length(x)>1,sd(x),0))[temp.list]
      IQR.LR <- tapply(proj.data, class, function(x) ifelse(length(x)>1,IQR(x),0))[temp.list]
      median.LR <- tapply(proj.data, class, median)[temp.list]
      n.LR <- table(class)[temp.list]
        
      c1 <- (m.LR[1] + m.LR[2])/2
      c2 <- (m.LR[1] * n.LR[2] + m.LR[2] * n.LR[1])/sum(n.LR)
      c3 <- ifelse(sum(sd.LR==0)!=0,c1,(m.LR[1] * sd.LR[2] + m.LR[2] * sd.LR[1])/sum(sd.LR))
      c4 <-  ifelse(sum(sd.LR==0)!=0,c2,(m.LR[1] * sd.LR[2]/sqrt(n.LR[2]) + 
               m.LR[2] * sd.LR[1]/sqrt(n.LR[1]))/
               (sd.LR[1]/sqrt(n.LR[1])+sd.LR[2]/sqrt(n.LR[2])))
      c5 <- (median.LR[1] + median.LR[2])/2
      c6 <- (median.LR[1] * n.LR[2] + median.LR[2] * n.LR[1])/sum(n.LR)    
      c7 <- ifelse(sum(IQR.LR==0)!=0,c5,(median.LR[1] * IQR.LR[2] + median.LR[2] * IQR.LR[1])/sum(IQR.LR))      
      c8 <- ifelse(sum(IQR.LR==0)!=0,c6,(median.LR[1] * (IQR.LR[2]/sqrt(n.LR[2])) + 
                 median.LR[2] * (IQR.LR[1]/sqrt(n.LR[1])))/
               ((IQR.LR[1]/sqrt(n.LR[1]))+(IQR.LR[2]/sqrt(n.LR[2]))))
      C <- c(c1, c2, c3, c4,c5,c6,c7,c8)
        Index <-PDAindex(as.numeric(as.factor(class)),proj.data,weight,lambda)
      a1 <- rep(0, pp) # zeros lenght original variables    
      a1[v.rnd] <- t(a.proj.best) #best.projt with selected variables and 0 in no selected original length      
      Alpha <- a1
        IOindexR <- NULL
        IOindexL <- NULL
        sort.LR <- as.numeric(names(sort(m.LR)))
        IOindexL <- class == sort.LR[1]
        IOindexR <- class == sort.LR[2]
        list(Index=Index,Alpha = Alpha, C = C, IOindexL = IOindexL, 
            IOindexR = IOindexR)
    }
    Tree.construct <- function(origclass, origdata, Tree.Struct,  id, rep, rep1, rep2, projbest.node, splitCutoff.node, lambda,...) {
        origclass <- as.integer(origclass)
        n <- nrow(origdata)
        g <- table(origclass)
        G <- length(g)
        if (length(Tree.Struct) == 0) {
            Tree.Struct <- matrix(1:(2 * G - 1), ncol = 1)
            Tree.Struct <- cbind(Tree.Struct, 0, 0, 0, 0)
        }
        if (G == 1) {
            Tree.Struct[id, 3] <- as.numeric(names(g))
            list(Tree.Struct = Tree.Struct, projbest.node = projbest.node, 
                splitCutoff.node = splitCutoff.node, rep = rep, rep1 = rep1, rep2 = rep2)
        }
        else {
            Tree.Struct[id, 2] <- rep1
            rep1 <- rep1 + 1
            Tree.Struct[id, 3] <- rep1
            rep1 <- rep1 + 1
            Tree.Struct[id, 4] <- rep2
            rep2 <- rep2 + 1
            a <- Find.proj(origclass, origdata,lambda,weight)
            splitCutoff.node <- rbind(splitCutoff.node, a$C)
            projbest.node <- rbind(projbest.node, a$Alpha)
            Tree.Struct[id, 5] <- a$Index            
            t.class <- origclass
            t.data <- origdata
            t.class <- t.class * a$IOindexL
            t.n <- length(t.class[t.class == 0])
            t.index <- sort.list(t.class)
            t.index <- sort(t.index[-(1:t.n)])
            t.class <- t.class[t.index]
            t.data <- origdata[t.index, ]
            b <- Tree.construct(t.class, t.data, Tree.Struct,  Tree.Struct[id, 2], rep, rep1, rep2, projbest.node, splitCutoff.node,lambda/G)
            Tree.Struct <- b$Tree.Struct
            projbest.node <- b$projbest.node
            splitCutoff.node <- b$splitCutoff.node
            rep <- b$rep
            rep1 <- b$rep1
            rep2 <- b$rep2
            t.class <- origclass
            t.data <- origdata
            t.class <- (t.class * a$IOindexR)
            t.n <- length(t.class[t.class == 0])
            t.index <- sort.list(t.class)
            t.index <- sort(t.index[-(1:t.n)])
            t.class <- t.class[t.index]
            t.data <- origdata[t.index, ]
            n <- nrow(t.data)
            G <- length(table(t.class))
            b <- Tree.construct(t.class, t.data, Tree.Struct, 
                Tree.Struct[id, 3], rep, rep1, rep2, projbest.node, splitCutoff.node,  lambda)
            Tree.Struct <- b$Tree.Struct
            projbest.node <- b$projbest.node
            splitCutoff.node <- b$splitCutoff.node
            rep <- b$rep
            rep1 <- b$rep1
            rep2 <- b$rep2
        }
        list(Tree.Struct = Tree.Struct, projbest.node = projbest.node, 
            splitCutoff.node = splitCutoff.node, rep = rep, rep1 = rep1, rep2 = rep2)
    }
    splitCutoff.node <- NULL
    projbest.node <- NULL
    Tree.Struct <- NULL
    id <- 1
    rep1 <- 2
    rep2 <- 1
    rep <- 1

    Tree.final <- Tree.construct(origclass, origdata, Tree.Struct, 
        id, rep, rep1, rep2, projbest.node, splitCutoff.node,lambda)
    Tree.Struct <- Tree.final$Tree.Struct
    colnames(Tree.Struct)<-c("id","L.node.ID","R.F.node.ID","Coef.ID","Index")   
    projbest.node <- Tree.final$projbest.node
    splitCutoff.node <- Tree.final$splitCutoff.node
    treeobj<-list(Tree.Struct = Tree.Struct, projbest.node = projbest.node, 
        splitCutoff.node = splitCutoff.node,origclass = origclass,origdata= origdata)
    class(treeobj)<-append(class(treeobj),"PPtree")
    return(treeobj)
}