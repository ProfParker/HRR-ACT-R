########################################################################################################
###
###  Simple Memory Retrieval Model of Agreement Phenomena (based on ACT-R theory)
###    With HTML trace output and PDF activation plots
###        
###
###  Rick Lewis & William Badecker (rickl@umich.edu)
###  Version 3.0
###  7 Feb 2007
###
###
#########################################################################################################


## act-r library
source("lib/actr.r")


## SET PARAMETERS function: sets the parameters as global variables

set.parameters = function(p) {
    cat.penalty <<- p$cat.penalty  
    F <<- p$F           # Latency Factor
    G <<- p$G           # Total Source Activation
    ans <<- p$ans       # Activation Noise Parameter for Logistic
    mas <<- p$mas       # Fan Parameter
    d <<- p$d           # base level decay
    match.penalty <<- p$match.penalty   #match penalty
    VAR.fan <<- p$VAR.fan # fan from VAR retrieval cues

    ## do VAR cues impose a mismatch penalty?
    var.mismatch.penalty <<- p$var.mismatch.penalty

    ## distinctiveness parameters
    modulate.by.distinct <<- p$modulate.by.distinct
    distinctiveness <<- p$distinctiveness
}

## RUN THE MODEL given the global parameter settings

run.model = function(quiet=TRUE) {
    trials <<- default.trials  
    
    ## Read in the item definitions: each item is a feature vector
    items <<- read.delim(file=item.file,header=FALSE,colClasses="character")
    num.columns <<- length(items)
    num.features <<- length(items[,1])-2
    
    creation.moment <<- c()
    for (cm in items[2,2:num.columns]) {
        creation.moment <<- c(creation.moment,as.integer(cm))
    }
    
    item.name <<- c()
    for (it in items[1,2:num.columns]) {
        item.name <<- c(item.name,it)
    }
    
    num.items <<- length(item.name)
    item.features <<- t(items[3:(num.features+2),2:num.columns])
    
    ## create initial history matrix to be the creation moments
    history <<- matrix(1:num.items,nrow=trials, ncol=num.items, byrow=TRUE)
    moments <<- creation.moment
        
    ## Read in the schedule of retrievals
    retrievals = read.delim(file=retrieval.file,header=FALSE)
    cue.names = retrievals[2:(num.features+1),1]
    retrievals = t(retrievals[,2:length(retrievals)])
    num.retrievals = length(retrievals[,1])
    retrieval.cue.list = retrievals[,2:(num.features+1)]

    features <- c(c(t(retrieval.cue.list)), as.vector(item.features))
    features <- unique(features)
        
    ## Now do each retrieval, incrementally updating the history matrix
    complete.results = NULL
    
    for (rr in 1:num.retrievals) {

        cues = retrieval.cue.list[rr,]
        moment = as.integer(retrievals[rr, 1])
        
        result = retrieve(cue.names, cues, moment)
        
        complete.results$summary = append(complete.results$summary, list(result$summary))
        complete.results$latencies[[rr]] = result$winner.latency
				complete.results$winner = result$winner
        
        ## update the history with what just happened at this moment
        moments <<- c(moments, moment)
        history <<- cbind(history, result$winner)          
    }

    return(complete.results)
}

plot.activation <- function(moments, history, correct.item, distractor, experiment, condition) {
    min.time <- 0;
    max.time <- moments[length(moments)] + 200;

    #print("Computing complete history of activation values at times....");

    ticks <- seq(min.time,max.time,10);
    num.ticks <- length(ticks);

    base.activations <- matrix(nrow=num.items, ncol=num.ticks);
    
    ##  First compute the history of activation values at each time point
    for (j in 1:num.ticks) {
      t <- ticks[j];
      
      #if (round(t/100)==t/100) {print(t)};
      base.levels <- compute.base.levels(t);
      
      ## make items that don't exist yet have activation of 0
      exists <- matrix(creation.moment <  t, ncol=trials, nrow=num.items);    
      activation  <- base.levels*exists + 0*!exists;
      
      ## take mean activation over all the monte carlo trials
      base.activations[,j] <- rowMeans(activation);

			# To obtain base activations, just progress up the items 1, 2, 3, . . .n, where n = the indices of the items in the experiment.
			#print(base.activations[1,])
    }


#Main Activation Plot

#lty=1 for dashed lines
    
    plotting.items <- c(correct.item, distractor);
    clrs <- c("#2EB8E9", "#E5007C");
    
    #print(paste("Plotting exp",experiment,"condition:",condition,"with items:",correct.item,distractor));
    #print(paste(" ... and",num.items,"total items"));
    
    plot(base.activations[1,] ~ ticks,
	 type="n", #lwd=2,col=clrs[1],
	 main=paste("",
              experiment , condition),
	 ylab="", xlab="");
    
    for (j in 1:length(plotting.items)) {
      c <- plotting.items[j];
      #print(paste("Plotting item",c));
      
      #Change line width lwd = 2
      lines(base.activations[c,] ~ ticks,
            type="l",lwd=2,col=clrs[j]);  
    };
    
    if (correct.item) {
      ## add a legend
      width <- max.time - min.time;
      height <- max(base.activations);
      legend(75, height-0.5, c("Target","Attractor"), lty=1,lwd=4,bty="n",
             cex=1,
             col = clrs[1:2],);
	
    }



    plotting.items <- 1:num.items;

    clrs <- c("black","purple","green","blue","orange");

    clrs[correct.item] <- "black";
    clrs[distractor] <- "red";

    plot(base.activations[1,] ~ ticks,
	 type="n", #lwd=2,col=clrs[1],
	 main=paste("Mean activation of items,", trials,"trial",
           "Exp:", experiment, "Condition:", condition),
	 ylab="Activation", xlab="Time");
    
    for (j in 1:length(plotting.items)) {
      c <- plotting.items[j];
      
      #print(paste("Plotting item",c));
      
      lines(base.activations[c,] ~ ticks,
            type="l",lwd=2,col=clrs[j]);  
    };
    
    width <- max.time - min.time;
    height <- max(base.activations);
    legend(0.5*width+min.time, height-0.5, item.name, lty=1,lwd=4,bty="n",
           cex=1,
           col = clrs[1:num.items],);
}
