Harrer, M., Cuijpers, P., Furukawa, T. A., & Ebert, D. D. (2021). Doing Meta-Analysis with R: A Hands-On Guide (1st ed.)
Barendregt, J. J., Doi, S. A., Lee, Y. Y., Norman, R. E., & Vos, T. (2013). Meta-analysis of prevalence.

## **Proportions**
Proportion is given by the number of events divided by the total number of observations:
$$ p = \frac{k}{n} $$
where p is the proportion, k is the number of events, and n is the total number of observations (sample size).

## **Standard Error (SE(p))**
The standard error of the proportion is given by:
$$ SE(p) = \sqrt{\frac{p(1 - p)}{n}} $$

# Pooling
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
The Cochran's Q test is given by:
$$ Q = \sum_{i=1}^k \frac{(p_i - P)^2}{Var(p_i)} $$
or
$$ Q = \sum_{i=1}^k w_i(\hat{\theta}_i - \hat{\theta})^2 $$
where p is the prevalence proportion, and N the population size.

### **I-squared statistic**
The I-squared statistic is given by:
$$ I^2 = \frac{Q - (k - 1)}{Q} $$
where Q is the Cochran's Q test, and k the number of studies.

### **H-squared statistic**
The H-squared statistic is given by:
$$ H^2 = \frac{q}{k - 1} $$
where q is the Q test, and k the number of studies.