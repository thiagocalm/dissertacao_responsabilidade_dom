
#' Estimate Oaxaca-Blinder decomposition for generalized linear model.

#' @description Perform the Oaxaca-Blinder decomposition for generalized linear model (GLM) with empirical standard error,
#' as suggested for Powers and Pullum (2006). It is possible to permorm twofold and threefold, or both, using different kind
#' of reference groups, based on recent econometric propositions.
#' @details This fuction is an adptation of `GeneralOaxaca`'s package, proposed by Aurelien Nicosia and Simon Baillargeon-Ladouceur (2015), but deprecated.
#' See Powers and Pullum (2006) for a empirical standard error reference. See  Reimers (1983), Cotton (1988), 
#' Neumark (1988), Jann (2008), and Yun (2004) for econometric propositions.
#' @param formula An object of class "`formula`". If it is not a formula object, it will be coerced to that class.
#' @param family a description of the error distribution and link function to be used in the model.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the
#' variables in the model (formula). If notfound in data, the variables are taken from `enviroment(formula)`.
#' @param groupInd is an indicator variable that is TRUE (or equal to 1) when an observation belongs to group A, and FALSE (or equal to 0)
#' when it belongs to Group B.
#' @param groupRef Group of reference for the decomposition, by default Group A. It can be used Group B, Group 1 for Jann (2008) proposes,
#' or Group 2 for Neumark (1988) proposes.
#' @param results a character that indicates what kind of output is desirable. Bu default "all" returns both twofolds and threefolds.
#' It can be used "twofold" or "threefold".
#' @return OaxacaBlinder returns the following results:
#' \itemize{
#'  \item{regoutput}{ list of two elements (names GroupA and GroupB) with the standard generalized linear model output in each group.}
#'  \item{twofold}{ list of two elements (overall abd variables) with the twofold decomposition based on the respect groupInd.}
#'  \item{threefold}{ list of two elements (overall abd variables) with the threefold decomposition based on the respect groupInd.}
#'  \item{n}{ the size of each respective group.}
#'  \item{summaryStat}{ descriptive statistic of the independent variable in each group.}
#'  \item{groupRef}{ a message describing what group reference has been used in the perform.}
#' }
#' @export


GLM_OaxacaBlinder <- function(formula, family = stats::binomial, data, groupInd, groupRef = "A",
                              results = "all"){
  call <- match.call()
  if (is.character(family)) 
    family <- get(family, mode = "function", envir = parent.frame())
  if (is.function(family)) 
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }
  if (!is.character(groupInd)) {
    print(groupInd)
    stop("'groupInd' must to be a charater object")
  }
  if (missing(data)) {
    data <- environment(formula)
  }
  ## Inputs
  # groupInd
  formula_res <- as.formula(glue::glue_collapse(paste0(c(formula,"+",groupInd))))
  groupInd <- as.vector(data[,colnames(data)==groupInd])
  # Group A
  dataA <- subset(data, groupInd == 1)
  nA <- length(dataA[, 1])
  glmA <- stats::glm(formula, family, data = dataA, x = TRUE)
  betaA <- glmA$coefficients
  Xa <- glmA$x
  varbA <- summary(glmA)$cov.scaled
  # Group B
  dataB <- subset(data, groupInd == 0)
  nB <- length(dataB[, 1])
  glmB <- stats::glm(formula, family, data = dataB, x = TRUE)
  betaB <- glmB$coefficients
  Xb <- glmB$x
  varbB <- summary(glmB)$cov.scaled
  # Irrestrict
  glm_ir <- stats::glm(formula, family, data = data, x = TRUE)
  beta_ir <- glm_ir$coefficients
  varb_ir <- summary(glm_ir)$cov.scaled
  # Restrict
  glm_res <- stats::glm(formula_res, family, data = data, x = TRUE)
  beta_res <- head(glm_res$coefficients,-1)
  varb_res <- head(summary(glm_res)$cov.scaled,c(-1,-1))
  # Regression outputs
  regoutput <- list(GroupA = glmA, GroupB = glmB, Irrestrict = glm_ir, Restrict = glm_res)
  ###
  # Decomposition
  ###
  # Inputs
  family <- glmA$family
  expect <- function(X, beta) mean(family$linkinv((X) %*% beta))
  expect_det <- function(X, beta) mean((X) * beta)
  ### Twofold
  if(results == "twofold"){
    if (groupRef == "A") {
      # Partes da decomposição da diferenca
      EbetaAXa <- mean(glmA$fitted.values)
      EbetaAXb <- expect(Xb, betaA)
      EbetaBXb <- mean(glmB$fitted.values)
      # Efeitos dos componentes
      char = EbetaAXa - EbetaAXb
      coef = EbetaAXb - EbetaBXb
    }
    if (groupRef == "B") {
      # Partes da decomposição da diferenca
      EbetaBXa <- expect(Xa, betaB)
      EbetaBXb <- mean(glmB$fitted.values)
      EbetaAXb <- expect(Xb, betaA)
      EbetaAXa <- mean(glmA$fitted.values)
      # Efeitos dos componentes
      char = EbetaBXa - EbetaBXb
      coef = EbetaAXa - EbetaBXa
    }
    if (groupRef == "1") {
      # Partes da decomposição da diferenca
      EbetaNXa <- expect(Xa, beta_ir)
      EbetaNXb <- expect(Xb, beta_ir)
      EbetaAXa <- mean(glmA$fitted.values)
      EbetaBXb <- mean(glmB$fitted.values)
      # Efeitos dos componentes
      char = EbetaNXa - EbetaNXb
      coef = (EbetaAXa - EbetaNXa) + (EbetaNXb - EbetaBXb)
    }
    if (groupRef == "2") {
      # Partes da decomposição da diferenca
      EbetaJXa <- expect(Xa, beta_res)
      EbetaJXb <- expect(Xb, beta_res)
      EbetaAXa <- mean(glmA$fitted.values)
      EbetaBXb <- mean(glmB$fitted.values)
      # Efeitos dos componentes
      char = EbetaJXa - EbetaJXb
      coef = (EbetaAXa - EbetaJXa) + (EbetaJXb - EbetaBXb)
    }
    diff = char + coef
    prop = c(char/diff, coef/diff, diff/diff) * 100
    C <- c(char, coef, diff)
    twofold_overall <- cbind(C, prop)
    colnames(twofold_overall) <- c("value", "prop (%)")
    rownames(twofold_overall) <- c("char", "coeff", "diff tot")
    ## Detalied Decomposition
    if (groupRef == "A") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xb[,i], betaA[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xb) %*% betaA)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xa[,i], betaB[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xa) %*% betaB)))
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    if (groupRef == "B") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaB[i]) - expect_det(Xb[,i], betaB[i]))
        d <- ((mean((Xa) %*% betaB)) - (mean((Xb) %*% betaB)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xb[,i], betaA[i]) - expect_det(Xb[,i], betaB[i]))
        d <- ((mean((Xb) %*% betaA)) - (mean((Xb) %*% betaB)))
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    if (groupRef == "1") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], beta_ir[i]) - expect_det(Xb[,i], beta_ir[i]))
        d <- ((mean((Xa) %*% beta_ir)) - (mean((Xb) %*% beta_ir)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n1 <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xa[,i], beta_ir[i]))
        n2 <- (expect_det(Xb[,i], beta_ir[i]) - expect_det(Xb[,i], betaB[i]))
        n <- n1 - n2
        d1 <- ((mean((Xa) %*% betaA)) - (mean((Xa) %*% beta_ir)))
        d2 <- ((mean((Xb) %*% beta_ir)) - (mean((Xb) %*% betaB)))
        d <- d1 - d2
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    if (groupRef == "2") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], beta_res[i]) - expect_det(Xb[,i], beta_res[i]))
        d <- ((mean((Xa) %*% beta_res)) - (mean((Xb) %*% beta_res)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n1 <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xa[,i], beta_res[i]))
        n2 <- (expect_det(Xb[,i], beta_res[i]) - expect_det(Xb[,i], betaB[i]))
        n <- n1 - n2
        d1 <- ((mean((Xa) %*% betaA)) - (mean((Xa) %*% beta_res)))
        d2 <- ((mean((Xb) %*% beta_res)) - (mean((Xb) %*% betaB)))
        d <- d1 - d2
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    # Applying weights to the attributes
    variables <- row.names(summary(glmA)$coefficients)
    effect_char <- cbind(round(Wxk * as.numeric(twofold_overall[1,1]),4),
                         round(Wxk * as.numeric(twofold_overall[1,2]),2))
    effect_coeff <- cbind(round(Wbk * as.numeric(twofold_overall[2,1]),4),
                          round(Wbk * as.numeric(twofold_overall[2,2]),2))
    ## Standard Error
    ## NOTE: it is in progress. The code has been implemented, but it is necessary evaluate the outputs.
    # functions
    dwA.F <- function(b1,b2,x2) {
      # derivative of Wdb wrt b1 = -derivative of Wdb wrt b2
      dwA1 <- NULL
      A <- x2%*%(b1-b2)
      for (i in 1:length(b1)){
        dwA1[i] <- x2[i]/A - (x2[i]^2*(b1[i]-b2[i]))/A^2
      }
      return(dwA1)
    }
    # logistic CDF
    CDF.lgt <- function(b,x) {
      xb <- x%*%b
      F <- exp(xb)/(1 + exp(xb))
      return(F)
    }
    # logistic pdf
    pdf.lgt <- function(b,x) {
      xb <- x%*%b
      f <- exp(xb)/(1 + exp(xb))^2
      return(f)
    }
    # inputs
    wb <- dwA.F(betaA,betaB,Xb)
    varb.b12 <- varbA + varbB
    K <- length(betaA)
    # Efeito caracteristica
    dEdb <- NULL
    for (k in 1:length(betaA)){
      dEdb[k] <- Wxk[k]*(mean(glmA$fitted.values*Xa[,k]) -
                           mean(pdf.lgt(betaA,Xb)*Xb[,k])) + effect_char[k,1]
    }
    VarChar <- matrix(0,K,K)
    for (k in 1:K){
      for (l in 1:K){
        VarChar[k,l] <- dEdb[k]*dEdb[l]*varbA[k,l]
      }
    }
    se_char <- sqrt(diag(VarChar))
    # Efeito coeficiente
    dCoefdbA <- NULL # É a derivada do efeito coeficiente em função de Beta1
    dCoefdbB <- NULL # É a derivada do efeito coeficiente em função de Beta2
    for (k in 1:length(betaB)){
      dCoefdbA[k] <- Wbk[k]*mean(glmA$fitted.values*Xa[,k]) + wb[k]*mean(CDF.lgt(betaA,Xb))
      dCoefdbB[k] <- wb[k]*mean(CDF.lgt(betaB,Xb)) - Wbk[k]*mean(glmB$fitted.values*Xb[,k])
    }
    VarCoef <- matrix(rep(0,K*K),K,K)
    for (k in 1:K){
      for (l in 1:K){
        VarCoef[k,l] <- (dCoefdbA[k])*(dCoefdbA[l])*varbA[k,l]
        + (dCoefdbB[k])*(dCoefdbB[l])*varbB[k,l]
      }
    }
    se_coef <- sqrt(diag(VarCoef))
    # Confident interval (95%)
    confint_char <- paste0("[",round(effect_char[,1]-se_char*1.96,4),"; ",
                           round(effect_char[,1]+se_char*1.96,4),"]")
    confint_coef <- paste0("[",round(effect_coeff[,1]-se_char*1.96,4),"; ",
                           round(effect_coeff[,1]+se_coef*1.96,4),"]")
    # Summaries
    twofold_variable <- cbind(variables, effect_char, confint_char, round(se_char,4),
                              effect_coeff, confint_coef, round(se_coef,4))
    row.names(twofold_variable) <- NULL
    colnames(twofold_variable) <- c("variables", "value (char)", "prop (% char)",
                                    "conf. int. 95% (char)","stand. error (char)",
                                    "value (coeff)", "prop (% coeff)",
                                    "conf. int. 95% (coeff)","stand. error (coeff)")
    # outputs
    twofold <- list(overall = twofold_overall,
                    variables = twofold_variable)
    # Configuring outputs
    n <- list(nA = nA, nB = nB)
    summaryStat <- list(summaryA = summary(glmA$y), summaryB = summary(glmB$y), 
                        meandiff = mean(glmA$y) - mean(glmB$y))
    out_twofold <- list(regoutput = regoutput,
                        twofold = twofold,
                        n = n,
                        summaryStat = summaryStat,
                        groupRef = paste(c("Your reference's group is:", groupRef), sep = " "))
    class(out_twofold) <- " GLM Blinder-Oaxaca"
    return(out_twofold)
  }
  ### Threefold
  if(results == "threefold"){
    if (groupRef == "A") {
      # Partes da decomposição da diferenca
      EbetaBXa <- expect(Xa, betaB)
      EbetaAXa <- mean(glmA$fitted.values)
      EbetaAXb <- expect(Xb, betaA)
      EbetaBXb <- mean(glmB$fitted.values)
      # Efeitos dos componentes
      char3 = EbetaAXa - EbetaAXb
      coef3 = EbetaAXa - EbetaBXa
      int = (EbetaAXa - EbetaBXa)*(EbetaBXb - EbetaAXb)
    }
    if (groupRef == "B") {
      # Partes da decomposição da diferenca
      EbetaBXa <- expect(Xa, betaB)
      EbetaAXa <- mean(glmA$fitted.values)
      EbetaAXb <- expect(Xb, betaA)
      EbetaBXb <- mean(glmB$fitted.values)
      # Efeitos dos componentes
      char3 = EbetaBXa - EbetaBXb
      coef3 = EbetaAXb - EbetaBXb
      int = (EbetaAXa - EbetaBXa)*(EbetaBXb - EbetaAXb)
    }
    if (groupRef == "1") {
      # Partes da decomposição da diferenca
      EbetaBXa <- expect(Xa, betaB)
      EbetaAXa <- mean(glmA$fitted.values)
      EbetaAXb <- expect(Xb, betaA)
      EbetaBXb <- mean(glmB$fitted.values)
      # Efeitos dos componentes
      char3 = EbetaAXa - EbetaAXb
      coef3 = EbetaAXa - EbetaBXa
      int = (EbetaAXa - EbetaBXa)*(EbetaBXb - EbetaAXb)
    }
    if (groupRef == "2") {
      # Partes da decomposição da diferenca
      EbetaBXa <- expect(Xa, betaB)
      EbetaAXa <- mean(glmA$fitted.values)
      EbetaAXb <- expect(Xb, betaA)
      EbetaBXb <- mean(glmB$fitted.values)
      # Efeitos dos componentes
      char3 = EbetaAXa - EbetaAXb
      coef3 = EbetaAXa - EbetaBXa
      int = (EbetaAXa - EbetaBXa)*(EbetaBXb - EbetaAXb)
    }
    diff = char3 + coef3 + int
    prop = c(char3/diff, coef3/diff, int/diff, diff/diff) * 100
    C <- c(char3, coef3, int, diff)
    threefold_overall <- cbind(C, prop)
    colnames(threefold_overall) <- c("value", "prop (%)")
    rownames(threefold_overall) <- c("char", "coeff", "int", "diff tot")
    ## Detalied Decomposition
    if (groupRef == "A") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xb[,i], betaA[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xb) %*% betaA)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xa[,i], betaB[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xa) %*% betaB)))
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    if (groupRef == "B") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaB[i]) - expect_det(Xb[,i], betaB[i]))
        d <- ((mean((Xa) %*% betaB)) - (mean((Xb) %*% betaB)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xb[,i], betaA[i]) - expect_det(Xb[,i], betaB[i]))
        d <- ((mean((Xb) %*% betaA)) - (mean((Xb) %*% betaB)))
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    if (groupRef == "1") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xb[,i], betaA[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xb) %*% betaA)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xa[,i], betaB[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xa) %*% betaB)))
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    if (groupRef == "2") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xb[,i], betaA[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xb) %*% betaA)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xa[,i], betaB[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xa) %*% betaB)))
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    # Applying weights to the attributes
    variables <- row.names(summary(glmA)$coefficients)
    effect_char <- cbind(round(Wxk * as.numeric(threefold_overall[1,1]),4),
                         round(Wxk * as.numeric(threefold_overall[1,2]),2))
    effect_coeff <- cbind(round(Wbk * as.numeric(threefold_overall[2,1]),4),
                          round(Wbk * as.numeric(threefold_overall[2,2]),2))
    effect_int <- cbind(round(Wxk * as.numeric(threefold_overall[3,1]),4),
                        round(Wxk * as.numeric(threefold_overall[3,2]),2))
    ## Standard error
    ## NOTE: it is in progress. We need to implement it here, such in the twofold decomp.
    # Summary
    threefold_variable <- cbind(variables, effect_char, effect_coeff, effect_int)
    row.names(threefold_variable) <- NULL
    colnames(threefold_variable) <- c("variables", "value (char)", "prop (% char)",
                                      "value (coeff)", "prop (% coeff)",
                                      "value (int)", "prop (% int)")
    # outputs
    threefold <- list(overall = threefold_overall,
                      variables = threefold_variable)
    ## Configuring outputs
    n <- list(nA = nA, nB = nB)
    summaryStat <- list(summaryA = summary(glmA$y), summaryB = summary(glmB$y), 
                        meandiff = mean(glmA$y) - mean(glmB$y))
    out_threefold <- list(regoutput = regoutput,
                          # twofold = twofold,
                          threefold = threefold, 
                          n = n,
                          summaryStat = summaryStat,
                          groupRef = paste(c("Your reference's group is:", groupRef), sep = " "))
    class(out_threefold) <- " GLM Blinder-Oaxaca"
    out_threefold
  }
  ### All results (Both, Twofold and Threefold)
  if(results == "all"){
    ### Twofold
    if (groupRef == "A") {
      # Partes da decomposição da diferenca
      EbetaAXa <- mean(glmA$fitted.values)
      EbetaAXb <- expect(Xb, betaA)
      EbetaBXb <- mean(glmB$fitted.values)
      # Efeitos dos componentes
      char = EbetaAXa - EbetaAXb
      coef = EbetaAXb - EbetaBXb
    }
    if (groupRef == "B") {
      # Partes da decomposição da diferenca
      EbetaBXa <- expect(Xa, betaB)
      EbetaBXb <- mean(glmB$fitted.values)
      EbetaAXb <- expect(Xb, betaA)
      EbetaAXa <- mean(glmA$fitted.values)
      # Efeitos dos componentes
      char = EbetaBXa - EbetaBXb
      coef = EbetaAXa - EbetaBXa
    }
    if (groupRef == "1") {
      # Partes da decomposição da diferenca
      EbetaNXa <- expect(Xa, beta_ir)
      EbetaNXb <- expect(Xb, beta_ir)
      EbetaAXa <- mean(glmA$fitted.values)
      EbetaBXb <- mean(glmB$fitted.values)
      # Efeitos dos componentes
      char = EbetaNXa - EbetaNXb
      coef = (EbetaAXa - EbetaNXa) + (EbetaNXb - EbetaBXb)
    }
    if (groupRef == "2") {
      # Partes da decomposição da diferenca
      EbetaJXa <- expect(Xa, beta_res)
      EbetaJXb <- expect(Xb, beta_res)
      EbetaAXa <- mean(glmA$fitted.values)
      EbetaBXb <- mean(glmB$fitted.values)
      # Efeitos dos componentes
      char = EbetaJXa - EbetaJXb
      coef = (EbetaAXa - EbetaJXa) + (EbetaJXb - EbetaBXb)
    }
    diff = char + coef
    prop = c(char/diff, coef/diff, diff/diff) * 100
    C <- c(char, coef, diff)
    twofold_overall <- cbind(C, prop)
    colnames(twofold_overall) <- c("value", "prop (%)")
    rownames(twofold_overall) <- c("char", "coeff", "diff tot")
    ## Detalied Decomposition
    if (groupRef == "A") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xb[,i], betaA[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xb) %*% betaA)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xa[,i], betaB[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xa) %*% betaB)))
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    if (groupRef == "B") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaB[i]) - expect_det(Xb[,i], betaB[i]))
        d <- ((mean((Xa) %*% betaB)) - (mean((Xb) %*% betaB)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xb[,i], betaA[i]) - expect_det(Xb[,i], betaB[i]))
        d <- ((mean((Xb) %*% betaA)) - (mean((Xb) %*% betaB)))
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    if (groupRef == "1") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], beta_ir[i]) - expect_det(Xb[,i], beta_ir[i]))
        d <- ((mean((Xa) %*% beta_ir)) - (mean((Xb) %*% beta_ir)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n1 <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xa[,i], beta_ir[i]))
        n2 <- (expect_det(Xb[,i], beta_ir[i]) - expect_det(Xb[,i], betaB[i]))
        n <- n1 + n2
        d1 <- ((mean((Xa) %*% betaA)) - (mean((Xa) %*% beta_ir)))
        d2 <- ((mean((Xb) %*% beta_ir)) - (mean((Xb) %*% betaB)))
        d <- d1 + d2
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    if (groupRef == "2") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], beta_res[i]) - expect_det(Xb[,i], beta_res[i]))
        d <- ((mean((Xa) %*% beta_res)) - (mean((Xb) %*% beta_res)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n1 <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xa[,i], beta_res[i]))
        n2 <- (expect_det(Xb[,i], beta_res[i]) - expect_det(Xb[,i], betaB[i]))
        n <- n1 - n2
        d1 <- ((mean((Xa) %*% betaA)) - (mean((Xa) %*% beta_res)))
        d2 <- ((mean((Xb) %*% beta_res)) - (mean((Xb) %*% betaB)))
        d <- d1 - d2
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    # Applying weights to the attributes
    variables <- row.names(summary(glmA)$coefficients)
    effect_char <- cbind(round(Wxk * as.numeric(twofold_overall[1,1]),4),
                         round(Wxk * as.numeric(twofold_overall[1,2]),2))
    effect_coeff <- cbind(round(Wbk * as.numeric(twofold_overall[2,1]),4),
                          round(Wbk * as.numeric(twofold_overall[2,2]),2))
    ## Standard Error
    ## NOTE: it is in progress. The code has been implemented, but it is necessarie evaluate the outputs.
    # functions
    dwA.F <- function(b1,b2,x2) {
      # derivative of Wdb wrt b1 = -derivative of Wdb wrt b2
      dwA1 <- NULL
      A <- x2%*%(b1-b2)
      for (i in 1:length(b1)){
        dwA1[i] <- x2[i]/A - (x2[i]^2*(b1[i]-b2[i]))/A^2
      }
      return(dwA1)
    }
    # logistic CDF
    CDF.lgt <- function(b,x) {
      xb <- x%*%b
      F <- exp(xb)/(1 + exp(xb))
      return(F)
    }
    # logistic pdf
    pdf.lgt <- function(b,x) {
      xb <- x%*%b
      f <- exp(xb)/(1 + exp(xb))^2
      return(f)
    }
    # inputs
    wb <- dwA.F(betaA,betaB,Xb)
    varb.b12 <- varbA + varbB
    K <- length(betaA)
    # Efeito caracteristica
    dEdb <- NULL
    for (k in 1:length(betaA)){
      dEdb[k] <- Wxk[k]*(mean(pdf.lgt(betaA,Xa)*Xa[,k]) -
                           mean(pdf.lgt(betaA,Xb)*Xb[,k])) + effect_char[k,1]
    }
    VarChar <- matrix(0,K,K)
    for (k in 1:K){
      for (l in 1:K){
        VarChar[k,l] <- dEdb[k]*dEdb[l]*varbA[k,l]
      }
    }
    se_char <- sqrt(diag(VarChar))
    # Efeito coeficiente
    dCoefdbA <- NULL # É a derivada do efeito coeficiente em função de Beta1
    dCoefdbB <- NULL # É a derivada do efeito coeficiente em função de Beta2
    for (k in 1:length(betaB)){
      dCoefdbA[k] <- Wbk[k]*mean(glmA$fitted.values*Xa[,k]) + wb[k]*mean(CDF.lgt(betaA,Xb))
      dCoefdbB[k] <- wb[k]*mean(CDF.lgt(betaB,Xb)) - Wbk[k]*mean(glmB$fitted.values*Xb[,k])
    }
    VarCoef <- matrix(rep(0,K*K),K,K)
    for (k in 1:K){
      for (l in 1:K){
        VarCoef[k,l] <- (dCoefdbA[k])*(dCoefdbA[l])*varbA[k,l]
        + (dCoefdbB[k])*(dCoefdbB[l])*varbB[k,l]
      }
    }
    se_coef <- sqrt(diag(VarCoef))
    # Confident interval (95%)
    confint_char <- paste0("[",round(effect_char[,1]-se_char*1.96,4),"; ",
                           round(effect_char[,1]+se_char*1.96,4),"]")
    confint_coef <- paste0("[",round(effect_coeff[,1]-se_char*1.96,4),"; ",
                           round(effect_coeff[,1]+se_coef*1.96,4),"]")
    # Summaries
    twofold_variable <- cbind(variables, effect_char, confint_char, round(se_char,4),
                              effect_coeff, confint_coef, round(se_coef,4))
    row.names(twofold_variable) <- NULL
    colnames(twofold_variable) <- c("variables", "value (char)", "prop (% char)",
                                    "conf. int. 95% (char)","stand. error (char)",
                                    "value (coeff)", "prop (% coeff)",
                                    "conf. int. 95% (coeff)","stand. error (coeff)")
    # outputs
    twofold <- list(overall = twofold_overall,
                    variables = twofold_variable)
    ### Threefold
    if (groupRef == "A") {
      # Partes da decomposição da diferenca
      EbetaBXa <- expect(Xa, betaB)
      EbetaAXa <- mean(glmA$fitted.values)
      EbetaAXb <- expect(Xb, betaA)
      EbetaBXb <- mean(glmB$fitted.values)
      # Efeitos dos componentes
      char3 = EbetaAXa - EbetaAXb
      coef3 = EbetaAXa - EbetaBXa
      int = (EbetaAXa - EbetaBXa)*(EbetaBXb - EbetaAXb)
    }
    if (groupRef == "B") {
      # Partes da decomposição da diferenca
      EbetaBXa <- expect(Xa, betaB)
      EbetaAXa <- mean(glmA$fitted.values)
      EbetaAXb <- expect(Xb, betaA)
      EbetaBXb <- mean(glmB$fitted.values)
      # Efeitos dos componentes
      char3 = EbetaBXa - EbetaBXb
      coef3 = EbetaAXb - EbetaBXb
      int = (EbetaAXa - EbetaBXa)*(EbetaBXb - EbetaAXb)
    }
    if (groupRef == "1") {
      # Partes da decomposição da diferenca
      EbetaBXa <- expect(Xa, betaB)
      EbetaAXa <- mean(glmA$fitted.values)
      EbetaAXb <- expect(Xb, betaA)
      EbetaBXb <- mean(glmB$fitted.values)
      # Efeitos dos componentes
      char3 = EbetaAXa - EbetaAXb
      coef3 = EbetaAXa - EbetaBXa
      int = (EbetaAXa - EbetaBXa)*(EbetaBXb - EbetaAXb)
    }
    if (groupRef == "2") {
      # Partes da decomposição da diferenca
      EbetaBXa <- expect(Xa, betaB)
      EbetaAXa <- mean(glmA$fitted.values)
      EbetaAXb <- expect(Xb, betaA)
      EbetaBXb <- mean(glmB$fitted.values)
      # Efeitos dos componentes
      char3 = EbetaAXa - EbetaAXb
      coef3 = EbetaAXa - EbetaBXa
      int = (EbetaAXa - EbetaBXa)*(EbetaBXb - EbetaAXb)
    }
    diff = char3 + coef3 + int
    prop = c(char3/diff, coef3/diff, int/diff, diff/diff) * 100
    C <- c(char3, coef3, int, diff)
    threefold_overall <- cbind(C, prop)
    colnames(threefold_overall) <- c("value", "prop (%)")
    rownames(threefold_overall) <- c("char", "coeff", "int", "diff tot")
    ## Detalied Decomposition
    if (groupRef == "A") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xb[,i], betaA[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xb) %*% betaA)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xa[,i], betaB[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xa) %*% betaB)))
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    if (groupRef == "B") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaB[i]) - expect_det(Xb[,i], betaB[i]))
        d <- ((mean((Xa) %*% betaB)) - (mean((Xb) %*% betaB)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xb[,i], betaA[i]) - expect_det(Xb[,i], betaB[i]))
        d <- ((mean((Xb) %*% betaA)) - (mean((Xb) %*% betaB)))
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    if (groupRef == "1") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xb[,i], betaA[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xb) %*% betaA)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xa[,i], betaB[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xa) %*% betaB)))
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    if (groupRef == "2") {
      ## Pesos para decomposicao detalhada
      # Peso atribuido ao efeito coeficiente
      Wxk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xb[,i], betaA[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xb) %*% betaA)))
        r <- n/d
        Wxk <- rbind(Wxk,r)
      }
      # Peso atribuido ao efeito coeficiente
      Wbk <- NULL
      for(i in 1:ncol(Xa)){
        n <- (expect_det(Xa[,i], betaA[i]) - expect_det(Xa[,i], betaB[i]))
        d <- ((mean((Xa) %*% betaA)) - (mean((Xa) %*% betaB)))
        r <- n/d
        Wbk <- rbind(Wbk,r)
      }
    }
    # Applying weights to the attributes
    variables <- row.names(summary(glmA)$coefficients)
    effect_char <- cbind(round(Wxk * as.numeric(threefold_overall[1,1]),4),
                         round(Wxk * as.numeric(threefold_overall[1,2]),2))
    effect_coeff <- cbind(round(Wbk * as.numeric(threefold_overall[2,1]),4),
                          round(Wbk * as.numeric(threefold_overall[2,2]),2))
    effect_int <- cbind(round(Wxk * as.numeric(threefold_overall[3,1]),4),
                        round(Wxk * as.numeric(threefold_overall[3,2]),2))
    threefold_variable <- cbind(variables, effect_char, effect_coeff, effect_int)
    row.names(threefold_variable) <- NULL
    colnames(threefold_variable) <- c("variables", "value (char)", "prop (% char)",
                                      "value (coeff)", "prop (% coeff)",
                                      "value (int)", "prop (% int)")
    # outputs
    threefold <- list(overall = threefold_overall,
                      variables = threefold_variable)
    ## Configuring outputs for two and threefolds
    n <- list(nA = nA, nB = nB)
    summaryStat <- list(summaryA = summary(glmA$y), summaryB = summary(glmB$y), 
                        meandiff = mean(glmA$y) - mean(glmB$y))
    out_all <- list(regoutput = regoutput,
                    twofold = twofold,
                    threefold = threefold,
                    n = n,
                    summaryStat = summaryStat,
                    groupRef = paste(c("Your reference's group is:", groupRef), sep = " "))
    class(out_all) <- " GLM Blinder-Oaxaca"
    out_all
  }
}