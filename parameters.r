## Parameter settings
## parameter values found here :: http://www-abc.mpib-berlin.mpg.de/actrdb/

## number of monte carlo trials per experiment
default.trials = 5000

## Discrete parameter space to search.  Each parameter now contains a list
## of possible values rather than a single value.  

## Latency factor
F = c(.6); #scaling factor for latencies

## Extra category penalty
cat.penalty = c(-999);

## Total source activation
G = c(1); #default = Total Source Activation G = 1.0	(default, L&V (2008))

## Activation noise parameter for logistic distribution
ans  = c(0.45)

## Fan parameter
mas = c(1.5)  #default = 1.5 L&V (2008)

### Base level decay parameter
d = c(0.5)	#default = .5 L&V (2008)


## Match penalty
match.penalty = c(-.6)	# mismatch penalty in database; L&V 2008 = .6

## Following are non-ACT-R modifications that we turn off by default
var.mismatch.penalty = c(FALSE);
VAR.fan = c(0);    # additional fan associated with VAR retrieval cues
modulate.by.distinct = c(FALSE);
distinctiveness = c(0);               
# Note that the quantitative parameter has no
# effect when modulate.by.distinct is FALSE. So this is some  wasted
# effort in the simple code below.
