options(scipen = 999999)
gc()


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(srvyr)
library(survey)
library(lmtest)
library(car)
library(DescTools)
library(insight)

# Model 1 - Demographic factors -----------------------------------------------

####
## Import data
####

# 1970
load("./output/pt3/model_output_70.RData", verbose = TRUE)

# N
n_obs(model_output_70[[1]])#mod 1

# Log-likelihood
loglikelihood(model_output_70[[1]]) # mod 1
loglikelihood(model_output_70[[2]]) # mod 2
loglikelihood(model_output_70[[3]]) # mod 3

# Pseudo R2 (McFadden)

1 - model_output_70[[1]]$deviance / model_output_70[[1]]$null.deviance # mod 1
1 - model_output_70[[2]]$deviance / model_output_70[[2]]$null.deviance # mod 2
1 - model_output_70[[3]]$deviance / model_output_70[[3]]$null.deviance # mod 3

# AIC analysis

model_output_70[[1]]$aic #AIC MOD 1
model_output_70[[2]]$aic #AIC MOD 2
model_output_70[[3]]$aic #AIC MOD 3

# VIF test

car::vif(model_output_70[[1]]) #VIF mod 1
car::vif(model_output_70[[2]]) #VIF mod 2
car::vif(model_output_70[[3]]) #VIF mod 3


rm(model_output_70)
gc()

# 1980
load("./output/pt3/model_output_80.RData", verbose = TRUE)

# N
n_obs(model_output_80[[1]])#mod 1

# Log-likelihood
loglikelihood(model_output_80[[1]]) # mod 1
loglikelihood(model_output_80[[2]]) # mod 2
loglikelihood(model_output_80[[3]]) # mod 3



# Pseudo R2 (McFadden)

1 - model_output_80[[1]]$deviance / model_output_80[[1]]$null.deviance # mod 1
1 - model_output_80[[2]]$deviance / model_output_80[[2]]$null.deviance # mod 2
1 - model_output_80[[3]]$deviance / model_output_80[[3]]$null.deviance # mod 3

# AIC analysis

model_output_80[[1]]$aic #AIC MOD 1
model_output_80[[2]]$aic #AIC MOD 2
model_output_80[[3]]$aic #AIC MOD 3

# VIF test

car::vif(model_output_80[[1]]) #VIF mod 1
car::vif(model_output_80[[2]]) #VIF mod 2
car::vif(model_output_80[[3]]) #VIF mod 3

rm(model_output_80)
gc()

# 1991
load("./output/pt3/model_output_91.RData", verbose = TRUE)

# N
n_obs(model_output_91[[1]])#mod 1

# Log-likelihood
loglikelihood(model_output_91[[1]]) # mod 1
loglikelihood(model_output_91[[2]]) # mod 2
loglikelihood(model_output_91[[3]]) # mod 3

# Pseudo R2 (McFadden)

1 - model_output_91[[1]]$deviance / model_output_91[[1]]$null.deviance # mod 1
1 - model_output_91[[2]]$deviance / model_output_91[[2]]$null.deviance # mod 2
1 - model_output_91[[3]]$deviance / model_output_91[[3]]$null.deviance # mod 3

# AIC analysis

model_output_91[[1]]$aic #AIC MOD 1
model_output_91[[2]]$aic #AIC MOD 2
model_output_91[[3]]$aic #AIC MOD 3

# VIF test

car::vif(model_output_91[[1]]) #VIF mod 1
car::vif(model_output_91[[2]]) #VIF mod 2
car::vif(model_output_91[[3]]) #VIF mod 3

rm(model_output_91)
gc()

# 2000
load("./output/pt3/model_output_00.RData", verbose = TRUE)

# N
n_obs(model_output_00[[1]])#mod 1

# Log-likelihood
loglikelihood(model_output_00[[1]]) # mod 1
loglikelihood(model_output_00[[2]]) # mod 2
loglikelihood(model_output_00[[3]]) # mod 3

# Pseudo R2 (McFadden)

1 - model_output_00[[1]]$deviance / model_output_00[[1]]$null.deviance # mod 1
1 - model_output_00[[2]]$deviance / model_output_00[[2]]$null.deviance # mod 2
1 - model_output_00[[3]]$deviance / model_output_00[[3]]$null.deviance # mod 3

# AIC analysis

model_output_00[[1]]$aic #AIC MOD 1
model_output_00[[2]]$aic #AIC MOD 2
model_output_00[[3]]$aic #AIC MOD 3

# VIF test

car::vif(model_output_00[[1]]) #VIF mod 1
car::vif(model_output_00[[2]]) #VIF mod 2
car::vif(model_output_00[[3]]) #VIF mod 3

rm(model_output_00)
gc()

# 2010
load("./output/pt3/model_output_10.RData", verbose = TRUE)

# N
n_obs(model_output_10[[1]])#mod 1

# Log-likelihood
loglikelihood(model_output_10[[1]]) # mod 1
loglikelihood(model_output_10[[2]]) # mod 2
loglikelihood(model_output_10[[3]]) # mod 3

# Pseudo R2 (McFadden)

1 - model_output_10[[1]]$deviance / model_output_10[[1]]$null.deviance # mod 1
1 - model_output_10[[2]]$deviance / model_output_10[[2]]$null.deviance # mod 2
1 - model_output_10[[3]]$deviance / model_output_10[[3]]$null.deviance # mod 3

# AIC analysis

model_output_10[[1]]$aic #AIC MOD 1
model_output_10[[2]]$aic #AIC MOD 2
model_output_10[[3]]$aic #AIC MOD 3

# VIF test

car::vif(model_output_10[[1]]) #VIF mod 1
car::vif(model_output_10[[2]]) #VIF mod 2
car::vif(model_output_10[[3]]) #VIF mod 3

rm(model_output_10)
gc()