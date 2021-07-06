# two_sample_ttest_operator

##### Description

The `two_sample_ttest_operator` is an operator that determines if there is a significant difference between the means of two groups of matched pairs.

##### Usage

Input projection|.
-----------|--------
`color`    | represents the groups to compare
`y-axis`   | measurement value
`labels`   | represents the pairing

Input parameters|.
---|---
`Paired T-test`   | logical, indicating whether to perform pairing, default FALSE
`Equal Variance`  | logical, whether to treat the two variances as being equal.
`Alternative`     | character, Alternative option it can be either "Two sided", "Greater" or "Less". Default is "Two-sided".
`Sign of effect`  | character, whether the output should be reversed ("Reverse") or not ("Normal"). Default value is "Normal".

Output relations|.
---|---
`p`     | numeric, p-value for the test
`tstat` | numeric, the value of the t-statistic
`delta` | numeric, the estimated difference in means

##### Details

The operator is the `t.test` function in base R.

##### References

see the `base::t.test` function of the R package for the documentation, 

##### See Also

[two_sample_ttest_operator](https://github.com/tercen/two_sample_ttest_operator)
