library(tercen)
library(dplyr)
library(reshape)

# Set appropriate options
#options("tercen.serviceUri"="http://tercen:5400/api/v1/")
#options("tercen.workflowId"= "4133245f38c1411c543ef25ea3020c41")
#options("tercen.stepId"= "2b6d9fbf-25e4-4302-94eb-b9562a066aa5")
#options("tercen.username"= "admin")
#options("tercen.password"= "admin")

alternative_options <- list("Two sided" = "two.sided",
                            "Greater"   = "greater",
                            "Less"      = "less")

checkFun = function(val) {
  if (length(val) == 0) return(NA)
  if (length(val) == 1) return(val)
  if (length(val) > 1 ) stop("Pairing/grouping not consistent for paired t-test")
}

ttestFunPaired <- function(df, var.equal = TRUE, alternative = 'two.sided') { 
  data <- melt(df, measure.vars = ".y")
  data <- try(as.matrix(cast(data, pairing ~ group, fun.aggregate = checkFun)))
  
  result <- NULL
  p <- tstat <- delta <- NaN
  if (!inherits(data, "try-error") && ncol(data) == 2) {
    test_result = try(t.test(x = data[,1], y = data[,2], paired = TRUE, alternative = alternative, var.equal = var.equal))
    if (!inherits(test_result, "try-error")){
      p     <- as.numeric(test_result$p.value)
      tstat <- as.numeric(test_result$statistic)
      delta <- as.numeric(test_result$estimate)
    }
  }
  data.frame(.ri = df$.ri[1], .ci = df$.ci[1], p = p, tstat = tstat, delta = delta)
}

ttestFun <- function(df, var.equal = TRUE, alternative = 'two.sided') {
  test_result <- try(t.test(.y ~ group, data = df, alternative = alternative, var.equal = var.equal))
  if(!inherits(test_result, "try-error")){
    p     <- as.numeric(test_result$p.value)
    tstat <- as.numeric(test_result$statistic)
    delta <- as.numeric(test_result$estimate[1] - test_result$estimate[2])
  } else {
    p <- tstat <- delta <- NaN
  }
  data.frame(.ri = df$.ri[1], .ci = df$.ci[1], p = p, tstat = tstat, delta = delta)
}

ctx = tercenCtx()

# check color (= grouping)
if (length(ctx$colors) != 1) stop("Grouping for two sample ttest must be defined using exactly one data color.")

color_values <- ctx$select(ctx$colors) %>% pull()
num_group_levels <- 2
if (length(levels(as.factor(color_values))) != num_group_levels) {
  stop(paste("Color (grouping) should contain exactly", num_group_levels, "groups."))
}

# properties
paired_test     <- ifelse(is.null(ctx$op.value('Paired T-test')), FALSE, as.logical(ctx$op.value('Paired T-test')))
equal_variance  <- ifelse(is.null(ctx$op.value('Equal Variance')), TRUE, as.logical(ctx$op.value('Equal Variance')))
alternative     <- alternative_options[[ifelse(is.null(ctx$op.value('Alternative')), 'Two sided', ctx$op.value('Alternative'))]]
sign_off_effect <- ifelse(is.null(ctx$op.value('Sign of effect')), 'Normal', ctx$op.value('Sign of effect'))

data <- ctx %>% 
  select(.ri, .ci, .y) %>%
  mutate(group = color_values)

result <- NULL
if (paired_test) {
  if (length(ctx$labels) != 1) stop("Pairing for two sample ttest must be defined using exactly one label.")
  
  result <- data %>% 
    mutate(pairing = ctx$select(ctx$labels) %>% pull()) %>% 
    group_by(.ri, .ci) %>%
    do(ttestFunPaired(., equal_variance, alternative))
} else {
  result <- data %>% 
    group_by(.ri, .ci) %>%
    do(ttestFun(., equal_variance, alternative))
}

if (sign_off_effect == "Reverse") {
  result <- result %>% mutate(delta = -delta,
                              tstat = -tstat)
}

result %>%
  mutate(logp = -log10(p)) %>%
  mutate(fdr = p.adjust(p, method = "fdr")) %>%
  mutate(Top = as.numeric(as.factor(rank(p)), ties.method = "min")) %>%
  ctx$addNamespace() %>%
  ctx$save()
