## Experiment definitions



pre = list(
  name = "pre-verbal ever",
  description ="pre-verbal ever",
  conditions = list(
    
    
    list(
      condition = "Intrusive Licensor",
      retrievals = "definitions/position/pre.txt",
      items      = "definitions/position/pre.b.txt",
      correct.item = 1,
      distractor.item = 2,
      critical.retrieval = 3),   
    
    list(
      condition = "No Licensor",
      retrievals = "definitions/position/pre.txt",
      items      = "definitions/position/pre.c.txt",
      correct.item = 1,
      distractor.item = 2,
      critical.retrieval = 3)
    
  )
) 

post = list(
  name = "post-verbal ever",
  description ="post-verbal ever",
  conditions = list(
    
    
    list(
      condition = "Intrusive Licensor",
      retrievals = "definitions/position/post.txt",
      items      = "definitions/position/post.b.txt",
      correct.item = 1,
      distractor.item = 2,
      critical.retrieval = 4),   
    
    list(
      condition = "No Licensor",
      retrievals = "definitions/position/post.txt",
      items      = "definitions/position/post.c.txt",
      correct.item = 1,
      distractor.item = 2,
      critical.retrieval = 4)
    
  )
) 


## Complete list of experiments
experiments = list(post)
num.experiments = length(experiments)
# change = 100000
change = 6000
