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
#loop over prevalence
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

#=========================================================

#adjust prevalence and population for testing constraint

group.results.df <- function(pool, m, pop, sensitivity, tests){
  tmp <- as.data.frame(t(group.results(pool,
                                m = m,
                                pop = pop ,
                                sensitivity, 0)))
  tmp <- tmp %>% mutate(total.tests = pos.groups + neg.groups + pos.peop)
  return((unlist(tmp$total.tests)-tests)^2)
}


max.tests <- function(tests, pool, m, pop, sensitivity){
  opt.out <- optim(5, group.results.df, pool = pool, m = m, sensitivity = sensitivity, tests = tests, method = "BFGS")
  test.pop <- opt.out$par
  tmp <- group.results(pool,
                m = m,
                pop = test.pop,
                sensitivity, 0)
  
  return(tmp)
}


#fix 500 tests
test.constraint<- function(pool, pop, assume.select = 0, tests = 2000, other.data = pool.sen ){
  #assume.select = 0 if people are tests at random, if 1 > assume.select > 0 , this 
  #is the % of the positives always in the test population. 
  sensitivity = other.data[which(pool.sen[,1]==pool),2]
  prev <- as.data.frame(seq(0.001, 0.3, by = 0.0005))
  prev.length <- length(prev)
  colnames(prev) <- "prevalence"
  
  if(assume.select == 0){
    prev <- prev
    untested.cases <- as.data.frame((pop/pool - tests)*prev)
    colnames(untested.cases) <- "untested.cases"
    test.pop <- tests*pool
  } else {
    #this block does not work
    cases <- prev*pop
    cases.selected <- assume.select * cases
    res.cases <- ((1-assume.select) * cases)
    res.prev <- res.cases/ (pop - cases.selected)
    add.cases <- (tests - cases.selected) * res.prev
    test.prev <- (cases.selected + add.cases)/tests
  }
  
  out <- lapply(seq(0.001, 0.3, by = 0.0005),
                function(X) max.tests (tests =tests,
                                       pool,
                                       m = X,
                                       pop = pop,
                                       sensitivity)
  )
  out<-as.data.frame(do.call("rbind", out ))
  out <- out %>% mutate(people.tested = pos.peop + neg.peop)
  
  
  prev.seq <- seq(0.001, 0.3, by = 0.0005)
  
  #test.popx<-unlist(rowSums(out[,1:2]))

  
  # out2 <- lapply(seq(0.001, 0.3, by = 0.0005),
  #               function(X) group.results(pool,
  #                                         m = prev.seq[X],
  #                                         pop = out[X,]$people.tested,
  #                                         sensitivity, 0)
  # )



  false.neg1 <- lapply(c(1:length(prev.seq)),
                       function(X) group.miss(pool,
                                              m = prev.seq[X],
                                              pop = out[X,]$people.tested,
                                              sensitivity, 0)[1]
  )

  out3 <- cbind(prev,
               out,
               do.call("rbind", false.neg1),
               untested.cases
  )

  out3  <- out3  %>%
    mutate(n.groups = pos.groups+neg.groups) %>%
    mutate(total.tests = n.groups + pos.peop) %>%
    mutate(untested.cases = (pop - people.tested) * prevalence) %>%
    mutate(pos.missed = numb.undetected.pos + untested.cases)
  
  return(out3)
}




