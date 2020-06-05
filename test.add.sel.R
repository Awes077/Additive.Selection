

source("Additive.Selection.Source.R")


alpha <- 0.01

pop <- generate_trait_loci(10,3000,0.5)

sim_test <- nr_multi_gen(pop, 0, 100,alpha)



probs <- get_mate_probs(pop,0)


pop$trait[1]



probs$trait


het <- sim_test[[2]]

fin_pop <- sim_test[[1]]

hist(fin_pop$trait)
hist(pop$trait)

var(pop$trait)
var(fin_pop$trait)

next_test <- mate(fin_pop,3000)
var(next_test$trait)


mid_test <- multi_gen(pop,10,100,alpha)

mid_het <- mid_test[[2]]

mid_fin <- mid_test[[1]]


hist(mid_fin$trait)

var(mid_fin$trait)
var(pop$trait)
het[100,]
mid_het[100,]


low_test <- multi_gen(pop,0,100,alpha)
mid_test <- multi_gen(pop,10,100,alpha)
high_test <- multi_gen(pop,20,100,alpha)

low_fin <- low_test[[1]]
low_het <- low_test[[2]]
mid_fin <- mid_test[[1]]
mid_het <- mid_test[[2]]
high_fin <- high_test[[1]]
high_het <- high_test[[2]]


low_next_gen <- mate(low_fin,3000)

mid_next_gen<- mate(mid_fin, 3000)

high_next_gen <- mate(high_fin,3000)


start_next <- non_random_mate(pop, 3000, 0)


var(low_next_gen$trait)
var(mid_next_gen$trait)
var(high_next_gen$trait)
var(start_next$trait)


var(start_next$trait)

var(next_gen_mid$trait)

var(test$trait)


fin_pop$trait



pop <- seq(0,10,1)
diffs <- (pop)^4
probs <- exp(-0.01*diffs)
plot(y=probs,x=0:10)





length(which(fin_pop[,1:20]==1))/320
