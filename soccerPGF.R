library(mpoly)

#probabilities (win, draw, loss)
#top against subtop

#Super strong
# probs1a <- c(0.9,0.05,0.05) #home
# probs1b <- c(0.8,0.05,0.15) #away

#very strong
# probs1a <- c(0.8,0.1,0.1) #home
# probs1b <- c(0.7,0.1,0.2) #away
# 
# #strong
# probs1a <- c(0.7,0.2,0.1) #home
# probs1b <- c(0.6,0.2,0.2) #away
# 
# #better
probs1a <- c(0.6,0.2,0.2) #home
probs1b <- c(0.45,0.2,0.35) #away
# 
# #equal
# probs1a <- c(0.5,0.2,0.3) #home
# probs1b <- c(0.3,0.2,0.5) #away



#subtop against subtop
 probs2a <- c(0.5,0.2,0.3) #home
 probs2b <- c(0.3,0.2,0.5) #away

#knock-out (top against subtop)
m1a <- mp(paste(probs1a[1],"w^3 +",probs1a[2], "w x +",probs1a[3]," x^3"))
m1b <- mp(paste(probs1b[1],"w^3 +",probs1b[2], "w x +",probs1b[3]," x^3"))

r12 <- m1a*m1b
r12
coefs12 <- unlist(lapply(r12,function(el) rev(el)[1]))
resko <-sum(coefs12[1:2]) + sum(coefs12[3:4])/2
resko

#Group system

#4-dim polynomial

#round 1 (& 6)
#top against subtop1 (home and away); subtop2 against subtop3 (home and away)
m1a <- mp(paste(probs1a[1],"w^3 +",probs1a[2], "w x +",probs1a[3]," x^3"))
m1b <- mp(paste(probs1b[1],"w^3 +",probs1b[2], "w x +",probs1b[3]," x^3"))
m2a <- mp(paste(probs2a[1],"y^3 +",probs2a[2], "y z +",probs2a[3]," z^3"))
m2b <- mp(paste(probs2b[1],"y^3 +",probs2b[2], "y z +",probs2b[3]," z^3"))

r16 <- (m1a*m1b)*(m2a*m2b)

#round 2 (& 5)
#top against subtop2; subtop1 against subtop3
m1a <- mp(paste(probs1a[1],"w^3 +",probs1a[2], "w y +",probs1a[3]," y^3"))
m1b <- mp(paste(probs1b[1],"w^3 +",probs1b[2], "w y +",probs1b[3]," y^3"))
m2a <- mp(paste(probs2a[1],"x^3 +",probs2a[2], "x z +",probs2a[3]," z^3"))
m2b <- mp(paste(probs2b[1],"x^3 +",probs2b[2], "x z +",probs2b[3]," z^3"))

r25 <- (m1a*m1b)*(m2a*m2b)

#round 3 (& 4)
#top against subtop3; subtop1 against subtop2
m1a <- mp(paste(probs1a[1],"w^3 +",probs1a[2], "w z +",probs1a[3]," z^3"))
m1b <- mp(paste(probs1b[1],"w^3 +",probs1b[2], "w z +",probs1b[3]," z^3"))
m2a <- mp(paste(probs2a[1],"x^3 +",probs2a[2], "x y +",probs2a[3]," y^3"))
m2b <- mp(paste(probs2b[1],"x^3 +",probs2b[2], "x y +",probs2b[3]," y^3"))

r34 <- (m1a*m1b)*(m2a*m2b)

r1to6 <- r16*r25*r34
length(r1to6)
expos <- exponents(r1to6)

#function that assigns contributions of each term, accounting for ties
fexpo <- function(expo){
  w <- expo[1]
  sexpo <- sort(expo,decreasing=TRUE)
  if(w >  sexpo[3]) return(1) #w ranks 1 or 2
  if(w < sexpo[2]) return(0) #w ranks 3 or 4
  if(w == sexpo[1] & w == sexpo[2] & w == sexpo[3] & w == sexpo[4]) return(1/2) #w ranks 1,2,3,4
  if(w == sexpo[1] & w == sexpo[2] & w == sexpo[3]) return(2/3) #w ranks 1,2,3
  if(w == sexpo[2] & w == sexpo[3] & w == sexpo[4]) return(1/3) #w ranks 2,3,4
  if(w == sexpo[2] & w == sexpo[3]) return(1/2) #w ranks 2,3
  }
weights <- unlist(lapply(expos,fexpo))
coefs <- unlist(lapply(r1to6,function(el) rev(el)[1]))

result <- sum(weights*coefs)

#probability to be eliminated, top club, group system
1-result

#probability to be eliminated, top club, knock-out system
1-resko

#relative risk
(1-resko)/(1-result)

