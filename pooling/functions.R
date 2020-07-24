#GROUP TESTING
#CREATED: Eli P. Fenichel, Yale University
#LAST UPDATED: July 15, 2020
#============================================================================
#Probability of a groups testing positive accounting for sensitivity of test
#with prevalence m, group size = group, population = pop
group.test.b <- function(group.size, m, pop, s, s.loss){
  n.group <- pop/group.size
  shat <- s - group.size*s.loss
  p.pos <- 1-(1 - shat*m)^group.size
  return(p.pos)
}

#Group results 
group.results <- function(group.size, m, pop, s, s.loss){
  p.pos <- group.test.b(group.size, m, pop, s, s.loss)
  #number of groups
  n.group <- pop/group.size
  pos.groups <- p.pos * n.group
  neg.groups <- n.group - pos.groups
  
  #number of people
  neg.peop <- neg.groups*group.size 
  pop.peop <- pos.groups*group.size
  
  res <- c(pos.groups, neg.groups, pop.peop,  neg.peop)
  names(res) <- c("pos.groups", "neg.groups", "pos.peop", "neg.peop")
  
  return(res)
}

#the expected number of people in negative groups that are actually false positives
#with group size = group.size, prevalence m, sensitivity s, in a population p.
group.miss <- function(group.size, m, pop, s, s.loss = 0 ) {
  #===========================================================
  shat <- s - group.size*s.loss
  neg.groups <- group.results(group.size, m, pop, s, s.loss)[2]
  prob.neg <- 1 - group.test.b (group.size, m, pop, s, s.loss)
  
  t.prob <- (1-shat )*m + (1-m)
  exp.n.miss <- sapply(c(1:(group.size-1)), function (x) 
    choose(group.size, group.size-x) * 
      ((1-m)/ t.prob)^(group.size-x) * 
      ((1-shat )*m/ t.prob)^x )
  
  exp.m<- t(c(1:(group.size-1))) %*% exp.n.miss 
  gm <- exp.m * neg.groups
  
  res <- c(gm, exp.m)
  names(res) <- c("numb.undetected.pos", "exp.und.group")
  return(res)
}

#==============================================================

pop.results <- function(pool, prev, other.data = pool.sen ){
  sensitivity = other.data[which(pool.sen[,1]==pool),2]
  pop.size <- as.data.frame(c(20:10000))
  colnames(pop.size) <- "p.size"
  
  out <- lapply(c(20:10000), 
                function(X) group.results(pool,
                                          m = prev,
                                          pop = X,
                                          sensitivity, 0)
  )
  false.neg1 <- lapply(c(20:10000),
                       function(X) group.miss(pool, 
                                              prev, 
                                              pop = X,
                                              sensitivity, 0)[1]
  )
  
  out <- cbind(pop.size, 
               do.call("rbind", out ),
               do.call("rbind", false.neg1)
  )
  
  out  <- out  %>% 
    mutate(n.groups = pos.groups+neg.groups) %>%
    mutate(total.tests = n.groups + pos.peop) %>%
    mutate(pos.found = p.size*prev - numb.undetected.pos)
  
  return(out)
}
#==============================================================

prev.results <- function(pool, pop, other.data = pool.sen ){
  sensitivity = other.data[which(pool.sen[,1]==pool),2]
  prev <- as.data.frame(seq(0.001, 0.3, by = 0.0005))
  colnames(prev) <- "prevalence"
  
  out <- lapply(seq(0.001, 0.3, by = 0.0005), 
                function(X) group.results(pool,
                                          m = X,
                                          pop = pop,
                                          sensitivity, 0)
  )
  false.neg1 <- lapply(seq(0.001, 0.3, by = 0.0005),
                       function(X) group.miss(pool, 
                                              m = X, 
                                              pop = pop,
                                              sensitivity, 0)[1]
  )
  
  out <- cbind(prev, 
               do.call("rbind", out ),
               do.call("rbind", false.neg1)
  )
  
  out  <- out  %>% 
    mutate(n.groups = pos.groups+neg.groups) %>%
    mutate(total.tests = n.groups + pos.peop) %>%
    mutate(pos.found = pop*prev - numb.undetected.pos)
  
  return(out)
}



