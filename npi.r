########################################################################################################
###
###  Simple Memory Retrieval Model of Agreement Phenomena (based on ACT-R theory)
###
###  Original code by:
###  Rick Lewis & William Badecker (rickl@umich.edu)
###
###  Simplified and modified by:
###  Pedro Alcocer (pealco@umd.edu)
###
###  Version 3.1
###  26 Apr 2011
###
########################################################################################################

#library(reshape)
#library(gplots)
library(doMC)

registerDoMC()

source("lib/hrr.R")
source("lib/run-model.r")

history = NULL

## Experiment definitions
# source("ever.r")
source('position.r')

## Parameter settings
source("parameters.r")


## Generate matrix defining combinatoric space of parameter values.
## Each column is a parameter; each row is a distinct comibination of 
## parameters defining a model simulation to run.

parameters = list(cat.penalty, F, G, ans, mas, d, match.penalty, 
                  VAR.fan, var.mismatch.penalty, modulate.by.distinct,
                  distinctiveness)
num.parameters = length(parameters)


## The total number of combinations is the product of the number of 
## values for each parameter
num.combinations = 1


## Set up matrix of parameter combinations. Rows are model experiments
## columns are parameters.
num.params = length(parameters)
p.matrix = matrix(nrow=num.combinations, ncol=num.params)

cumulative.num.combs = 1
for (p in 1:num.params) {
      p.matrix[,p] = parameters[p][[1]]
      cumulative.num.combs = cumulative.num.combs * length(p.matrix[,p])
}


##  Now set up matrix of unique model runs.
count.conditions = function(e) {
  length(e$conditions)
}

total.conditions = sum(as.vector(lapply(experiments, count.conditions),
                                  mode="integer"))
model.runs = data.frame()

for (e in 1:num.experiments) {
  exp.name = experiments[[e]]$name
  exp.conds = experiments[[e]]$conditions
  for (c in 1:length(exp.conds)) {
    cond = exp.conds[[c]]
    
    model.runs = rbind(model.runs,
        data.frame(experiment         = exp.name,
                   condition          = cond$condition,
                   retrievals         = cond$retrievals,
                   items              = cond$items,
                   critical.retrieval = cond$critical.retrieval,
                   correct.item       = cond$correct.item,
                   distractor.item    = cond$distractor.item,
                   model              = NA
                   )
                )
  }
}

# Duplicate the parameter matrix "total.conditions" number of times.
total.runs = total.conditions * num.combinations

full.parameter.matrix = matrix(data=t(p.matrix), nrow=total.runs,
                                ncol=num.params, byrow=TRUE)

colnames(full.parameter.matrix) = c("cat.penalty", "F", "G", "ans", 
    "mas", "d", "match.penalty", "VAR.fan", "var.mismatch.penalty", 
    "modulate.by.distinct", "distinctiveness") 

## Finally, form the complete model run matrix.
all.runs = as.data.frame(cbind(full.parameter.matrix, model.runs))
final.samples = matrix(data=NA, nrow=default.trials, ncol=total.runs)
final.winners = matrix(data=NA, nrow=default.trials, ncol=total.runs)

## Loop over all runs and run the models

pdf(file="output/activation-plots.pdf",width=11,height=5)

all.model.results = foreach(r = 1:total.runs, .combine="rbind") %do% {
    ## select out row corresponding to this run
    this.run = all.runs[r,]      
    
    ## now set the model parameters according to this combination of values
    set.parameters(this.run[1:num.parameters])
    
    ## and uun the model
    item.file = as.character(this.run$items)
    retrieval.file = as.character(this.run$retrievals)

    results = run.model()
    
    plot.activation(moments, history, this.run$correct.item, this.run$distractor.item, this.run$experiment, this.run$condition);
       
    # Looking at this by calling the tables gives us the last experiment
    # moments = retrievals
    # history = what item was retrieved for each of the 5000 runs


    ## now extract and print the relevant measures.
    
    # Percent error.
    crit.ret = results$summary[[this.run$critical.retrieval]]

    # Get the probability of retrieving the incorrect item,
    # subtract from 100 = percent error
    model.result = crit.ret$retrieval.prob[this.run$distractor.item] * 100 

    #error rates
    cat('\n', 'Cond',r, '\n', 'error = ', (model.result), '\n');
    #print(100-model.result)

    #Winners
    #print(results$winner);

    #Retrieval winner
    final.winners[,r] = results$winner;

    # Latencies
    final.samples[,r] = results$latencies[[this.run$critical.retrieval]];
    cat(' Latency = ', mean(final.samples[,r]), '\n\n');


    model.result
}
dev.off()


for (r in 1:total.runs) {
    all.runs[r,]$model = all.model.results[r,]
}



write.csv(all.runs, "output/all.runs.error.txt")
write.csv(final.samples, "output/final.latencies.txt")
write.csv(final.winners, "output/final.winner.txt")


##########################
##########################

## PLOTS
## all plots to pdf

# STRUCTURE-BASED PLOTS

# Latency plots
Cond1 = mean(final.samples[,1])
Cond2 = mean(final.samples[,2])
Cond3 = mean(final.samples[,3])
Cond4 = mean(final.samples[,4])

pdf(file="output/Latencies.pdf", width = 10, height = 11)
bars = c(Cond1, Cond2, Cond3, Cond4);
barplot(bars, ylim = c(0,650), names.arg=c("Accessible", "Inaccessible", "Both", "No Licensor"), main=" ")
dev.off()

pdf(file="output/LatBoxPlot.pdf")
boxplot(final.samples, names=c("Accessible", "Inaccessible", "Both", "No Licensor"))
dev.off()


# Latency Density Distributions
Cond1 = density(final.samples[,1]);
Cond2 = density(final.samples[,2]);
Cond3 = density(final.samples[,3]);
Cond4 = density(final.samples[,4]);

pdf("output/LatencyDist.pdf", width = 10, height = 10)
plot(Cond1, col="steelblue", main="Latency Distributions", 
     xlab="Reaction Time",xlim=c(0,800));
lines(Cond2, col="olivedrab");
lines(Cond3, col="maroon");
lines(Cond4, col="green");
legend(600, .0034, c("Accessible", "Inaccessible", "Both", "Neither"),
       lty=c(1,1,1,1), col=c("steelblue","olivedrab","maroon","green"),
       lwd=c(2,2,2,2))
dev.off()

# Accuracy plots

Cond1 = all.runs[1,]$model
Cond2 = all.runs[2,]$model
Cond3 = all.runs[3,]$model
Cond4 = all.runs[4,]$model

pdf(file="output/RetrievalError.pdf", width = 10, height = 10)
bars = c(Cond1, Cond2, Cond3, Cond4);
barplot(bars, ylim=c(0, 50), main=" ",
        names.arg=c("Accessible","Inaccessible","Both","No Licensor"))
dev.off()
