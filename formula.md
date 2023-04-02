
# Reliability
## ICC(2,k)
The ICC(2,k) refers to a two-way random effects model with consistency agreement. In this model, both raters (judges) and subjects are considered random samples from larger populations. The "2" in ICC(2,k) indicates that it's a two-way random effects model, while the "k" signifies that there are k different raters or judges involved in the assessment. The ICC(2,k) is focused on the consistency of the ratings, meaning that the raters' scores should be proportional to each other, even if they don't necessarily have the same mean.

ICC(2,k) = (MSR - MSE) / (MSR + (MSC - MSE) / n)

$$\text{ICC}(2,k) = \frac{\text{MS}_{R} - \text{MS}_{E}}{\text{MS}_{R} + (\text{MS}_{C} - \text{MS}_{E}) / n}$$

where:

-   MSR is the mean square for rows (subjects)
-   MSC is the mean square for columns (raters)
-   MSE is the mean square for the residual (error) term
-   k is the number of raters or judges
-   n is the number of subjects


# Proportions
Proportion is given by the number of events divided by the total number of observations:
$$ p = \frac{k}{n} $$
where p is the proportion, k is the number of events, and n is the total number of observations (sample size).

## **Standard Error (SE(p))**
The standard error of the proportion is given by:
$$ SE(p) = \sqrt{\frac{p(1 - p)}{n}} $$

# Pooling
- Harrer, M., Cuijpers, P., Furukawa, T. A., & Ebert, D. D. (2021). Doing Meta-Analysis with R: A Hands-On Guide 
- Barendregt, J. J., Doi, S. A., Lee, Y. Y., Norman, R. E., & Vos, T. (2013). Meta-analysis of prevalence.
## Transformations
### **Logit transformation**
The logit transformation is given by:
$$ l = log(\frac{p}{1 - p})$$
where p is the prevalence proportion, and l the logit transformed prevalence.

#### variance is given by:
$$ Var(l) = \frac{1}{Np} + \frac{1}{N(1-p)}$$
#### back transformation is given by:
$$ p = \frac{exp(l)}{1 + exp(l)} $$

### **Double arcsine transformation**
#### The double arcsine transformation is given by:
$$ t = arcsin(\sqrt{\frac{n}{N + 1}}) + arcsin(\sqrt{\frac{n + 1}{N + 1}}) $$
where t is the double arcsine transformed prevalence, and n the sample size.

#### the variance of t is given by:
$$ Var(t) = \frac{1}{N + 1} + 0.5 $$

#### The back transformation is given by:
$$ p = (\sin t/2)^2 $$

### **Sampling Variance**
With the main meta-analysis methods based on the inverse variance method (or modifications thereof ), the binomial equation for variance (expressed as a proportion) can be used to obtain the individual study weights:
$$ Var(p) = \frac{p(1 - p)}{n} $$
where p is the prevalence proportion, and N the population size.

### **Weights**
The weights are calculated using the inverse variance method:
$$ w_i = \frac{1}{Var(p_i)} $$
where w is the weight, p is the prevalence proportion, and N the population size.
  
### **Pooled prevalence estimate**
Thus, the pooled prevalence estimate P then becomes (according to the inverse variance method):
$$ P = \frac{\sum_{i=1}^k \frac{p_i}{Var(p_i)}}{\sum_{i=1}^k \frac{1}{Var(p_i)}}$$
### **Pooled Standard Error (SE(P))**
The standard error of the pooled prevalence estimate is then:
$$ SE(P) = \sqrt{\sum_{i=1}^k \frac{1}{Var(p_i)}}$$

### **Pooled Confidence Interval (CI)**
The confidence interval for the pooled prevalence estimate is then:
$$ CI(P) = P \pm Z_{\alpha/2} SE(P) $$
where Zα/2 denotes the appropriate factor from the standard Normal distribution for the desired confidence percentage (eg, Z0.025=1.96).
  
## Hetereogeneity
### **Cochran's Q test**
The Cochran's Q test is given by (Hoaglin, D. C. (2016); Harrer et al., (2021)):
$$Q = k \sum_{i=1}^n w_i(x_i - \bar{x}_w)^2$$


where:
-   $Q$: The weighted sum of squared differences between each individual study's effect size and the weighted mean effect size. This serves as a measure of the heterogeneity in the effect sizes across studies.
-   $k$: The number of studies included in the meta-analysis.
-   $w_i$: The weight assigned to each study based on its precision, which is usually calculated as the inverse of the variance of the effect size estimate for that study. A higher weight indicates a more precise estimate.
-   $x_i$: The effect size estimate for the $i$-th study.
-   $\bar{x}_w$: The weighted mean effect size, calculated as the sum of each study's effect size estimate multiplied by its weight, divided by the sum of all weights. This serves as a summary measure of the effect size across all studies, where studies with higher precision (i.e., lower variance) are given more weight in the calculation.

### **I-squared statistic**
The I-squared statistic is given by:
$$ I^2 = \frac{Q - (k - 1)}{Q} $$
where Q is the Cochran's Q test, and k the number of studies.

### **H-squared statistic**
The H-squared statistic is given by:
$$ H^2 = \frac{q}{k - 1} $$
where q is the Q test, and k the number of studies.