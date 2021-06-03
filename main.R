library(tercen)
library(dplyr)
library(reshape)
library(pgCheckInput)

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

if (length(ctx$colors) != 1) stop("Grouping for two sample ttest must be defined using exactly one data color.")

# properties
paired_test     <- ifelse(is.null(ctx$op.value('Paired T-test')), FALSE, as.logical(ctx$op.value('Paired T-test')))
equal_variance  <- ifelse(is.null(ctx$op.value('Equal Variance')), TRUE, as.logical(ctx$op.value('Equal Variance')))
alternative     <- alternative_options[[ifelse(is.null(ctx$op.value('Alternative')), 'Two sided', ctx$op.value('Alternative'))]]
sign_off_effect <- ifelse(is.null(ctx$op.value('Sign of effect')), 'Normal', ctx$op.value('Sign of effect'))

data <- ctx %>% 
  select(.ri, .ci, .y) %>%
  mutate(group = ctx$select(ctx$colors) %>% pull())

check(ExactNumberOfFactors, ctx, groupingType = "xAxis", nFactors = 1, altGroupingName = "Grouping factor")
check(ExactNumberOfGroups, data, factorName = "group", nLevels = 2)

result <- NULL
if (paired_test) {
  if (length(ctx$labels) != 1) stop("Grouping for two sample ttest must be defined using exactly one label.")
  
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
