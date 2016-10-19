# NOTES:
# Modified the does not exist penalty to 1,000 (to reflect that search has terminated after 1,000)	July 18, 2011 DP

library(foreach)

# Note: this file modifies ACT-R to respect category as hard constraint

# Base level activation computation.  
# Assumes a matrix "history" that has a column vector for each past retrieval 
# moment, where the column vector identifies the winning item for each trial.

compute.base.levels = function(moment) {
    if (!d) {
        return(matrix(0, nrow=num.items, ncol=trials))
    }
    
    # time since last retrieval for each retrieval (converted from ms)
    tj = (moment - moments) / 1000
        
    # only pay attention to past, not future
    past = history[, tj>0]
    
    # the decay function
    tjd = tj[tj>0] ^ -d
    decay = matrix(tjd, nrow=trials, ncol=length(tjd), byrow=T)
    
    base.levels = foreach(c = 1:num.items, .combine="rbind") %do% {
        retrievals = past == c # boolean matrix
        activations = retrievals * decay
        b = log(rowSums(activations, na.rm=TRUE)) # sum over all j retrievals
        b[is.infinite(b)] = 0 # don't propagate through infinite values
        b
    }

    return(base.levels)

}

# Retrieval time computation. 
# computes distribution of retrieval times for all items given a 
# retrieval cue feature vector a moment in time to do the retrieval
# Updates the history matrix w/ the winning items for this retrieval
# moment over all the trials

retrieve = function(cue.names, retrieval.cues, retrieval.moment) {


    num.cues = length(retrieval.cues[retrieval.cues != "NULL"])

    # compute base level activations
    base.levels = compute.base.levels(retrieval.moment)

    # compute match between items and cues (a boolean matrix)
    cues = matrix(data=as.matrix(retrieval.cues), nrow=num.items, ncol=num.features, byrow=TRUE)
    is.nil = item.features == "nil"    
    is.variable.cue = cues == "VAR"

    match = (item.features == cues)
    match.inc.var = match | (is.variable.cue & !is.nil)

    # checks which items exist at this moment
    exists = matrix(creation.moment < retrieval.moment, nrow=num.items, ncol=num.features)

    # checks which items match category cue
    item.category = item.features[, cue.names=="cat"]
    cue.category = cues[, cue.names=="cat"]
    matches.category = matrix((item.category == cue.category), nrow=num.items, ncol=trials)

    # compute fan for each feature: number of existing items matching each feature
    fan = colSums(match.inc.var & exists) +  VAR.fan * is.variable.cue[1,]
    strength = mas - log(fan)                       # fan equation


    # compute source activation available for each cue (source spread over
    # cues) and multiply by the fan (S * W in act-r equation).

    # THIS IS NEW: We make VAR cues provide half the activation of other
    # cues (because they only provide only half of the {feature, value} pair)
    #cue.weights = 1 - as.integer(is.variable.cue[1,])/2
    #cue.weights = cue.weights/sum(cue.weights[retrieval.cues!="NULL"])

    #W = G * cue.weights    
    W = G/num.cues
    
    sw = matrix(strength * W, nrow=num.items, ncol=num.features, byrow=TRUE)
    
    # compute extra activation for each item; sum must ignore NA's because
    # those correspond to features that are not retrieval cues.
    extra = rowSums(match.inc.var * sw, na.rm=TRUE)    
    
    # compute mismatch penalty
    is.retrieval.cue = (cues != "NULL") & (cues != "VAR")

    if (var.mismatch.penalty) {
      mismatch = (!match & is.retrieval.cue)  | (is.variable.cue & is.nil)
    } else {
      mismatch = (!match & is.retrieval.cue)
    }

    # mismatch = (!match & is.retrieval.cue)  | (is.variable.cue & is.nil)
    penalty = rowSums(mismatch * match.penalty)
    
    features = unique(c(as.vector(item.features), retrieval.cues))
    
    featureHRRs = lapply(rep(10000, length(features)), newHRR)
    completeHRR = newHRR(10000)
    names(featureHRRs) <- features
    retrievalHRR = Reduce(function(x,y) {x+y}, 
                          featureHRRs[retrieval.cues[retrieval.cues!="NULL"]])
    
    itemHRRs = list()

    for( i in rownames(item.features)) {
      temp <- Reduce(function(x,y) {x+y}, featureHRRs[item.features[i,]])

      if (retrieval.moment > change && i == names(penalty)[2]) {
        temp = temp * completeHRR
      }

      penalty[i] = cosine(temp, retrievalHRR)
    }

    # compute activation boost/penalty
    activation.adjustment = extra + penalty
    boost = matrix(activation.adjustment, ncol=trials, nrow=num.items) 

    # add to base-level
    if (modulate.by.distinct) {
      # compute how distinctive each item is (proportional to base-level activation)
      d.boost = distinctiveness + base.levels   
      activation = base.levels + boost * d.boost
    } else {
      activation = base.levels + boost
    }
    
    noise = matrix(rlogis(trials*num.items, 0, ans), ncol=trials, nrow=num.items)
    noisy.activation = activation + noise

    # make items that don't exist yet, or that don't match category cues,  have activation of -999
    exists = matrix(creation.moment <  retrieval.moment, nrow=num.items, ncol=trials)
    exists.matches.cat = exists & matches.category
    exists.but.doesnt.match.cat = exists & !matches.category

    #doesnt.exist.penalty = 9999* !exists
    doesnt.exist.penalty = 1000* !exists
    doesnt.match.cat.penalty = 9999* !matches.category

    final.activation  = noisy.activation * exists + (-999 * !exists) + (cat.penalty * !matches.category)
    activation.mean = rowMeans(final.activation)
    activation.sd = apply(final.activation, 1, sd, na.rm=TRUE)


    # compute latency given the noisy activation, and mean and sd over all the
    # monte carlo trials. Make non-existent items have a retrieval time of 9999.
    retrieval.latency = (F * exp(-noisy.activation))*1000
    final.latency  = retrieval.latency*exists + doesnt.exist.penalty + doesnt.match.cat.penalty

    latency.mean = rowMeans(final.latency)
    latency.sd = apply(final.latency, 1, sd, na.rm=TRUE)

    # find winning item for each trial, and count # of times each item won
    winner = apply(final.latency, 2, which.min)
    winner.latency = apply(final.latency, 2, min)    
    counts = rep(0, num.items)
    
    item.winners = NULL
    for (c in 1:num.items) {
	    counts[c] = sum(winner == c)
        item.winners = cbind(item.winners, winner==c)
    }

    retrieval.prob.lower = rep(NA, num.items)
    retrieval.prob.upper = rep(NA, num.items)
    
    # probability of retrieval
    retrieval.prob = counts / trials
    winner.latency.mean = mean(winner.latency)
    winner.latency.sd = sd(winner.latency)

    summary =  data.frame(item=c(item.name, "WINNER"),
                           retrieval.prob=c(retrieval.prob, 1.0),
                           retrieval.prob.lower=c(retrieval.prob.lower, NA),
                           retrieval.prob.upper=c(retrieval.prob.upper, NA),                           
                           latency.mean=c(latency.mean, winner.latency.mean),
                           latency.sd=c(latency.sd, winner.latency.sd),
                           activation.mean=c(activation.mean, NA),
                           activation.sd=c(activation.sd, NA))


    return(list(summary=summary, winner=winner, latency.mean=latency.mean, final.latency=final.latency, winner.latency=winner.latency))    
}
