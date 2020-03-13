Calculating CFR for COVID-19
================
Bindoff, A.
13/03/2020

The case fatality rate (CFR) *during* an epidemic is known to be
inaccurate because (a) the true number of cases is unknown, and (b) many
cases are still active and a proportion of those may die. We refer to
the statistic often cited as the CFR for COVID-19 at present as the
‘naive’ CFR reflecting that it is an unsophisticated estimate of the
true CFR, which can be defined by the equation,

\[CFR = \frac{deaths + \widehat{\omega}(active \ cases)}{deaths + recoveries + active \ cases}\]

where \(\widehat{\omega} \in [0,1]\) is the unknown proportion of active
cases which will resolve by death (a **numerator** error). At the end of
the epidemic, the number of active cases becomes zero and we are left
with \(\frac{deaths}{(deaths + recoveries)}\).

In addition, because the number of undetected cases is unknown we have a
**denominator** error. This has been widely reported, but without much
regard for the interplay between the numerator and denominator errors.
At this stage we don’t know what the magnitude of the likely numerator
or denominator errors are, although the proportion of active cases which
will not recover is probably smaller than the naive CFR because time to
death is less than time to recovery. My feeling, contrary to popular
opinion, is that naive CFR may in fact *underestimate* CFR for COVID-19
at this stage, and in this vignette I will attempt to show (graphically)
why I believe this to be the case.

For simplicity we will not consider conditional estimates of CFR (age,
comorbidity, or the capacity of different regions to respond with
life-saving acute care).

Our model of CFR is,

\[CFR = \frac{deaths + \widehat{\omega}*(active \ cases)}{total \ cases \ detected + \widehat{\rho}*(total \ cases \ detected)}\]

where \(\widehat{\rho} \in [0,1]\) is the proportion of total cases that
are assumed to be undetected at the current time and
\(\widehat{\omega} \in [0,1]\) is the proportion of active and detected
cases at the current time that will not recover (making the simplifying,
conservative assumption that because the undetected cases are
asymptomatic or mild, the patient is unlikely to die).

We will use total cases, recoveries, and deaths as of 13th March 2020 to
provide context.

``` r
cfr <- function(w_hat, deaths, recoveries, active_cases, p_uncounted = 0){
  (deaths + w_hat*active_cases)/((deaths + recoveries + active_cases) + (p_uncounted*(deaths + recoveries + active_cases)))
}
```

``` r
# total cases 13/3/20 = 134748
# active cases = 59382 (.4407 x total cases)
d <- expand.grid(w_hat = seq(0, 0.06, by = .01),
                 active_cases = 59382,
                 deaths = 4983,
                 recoveries = 70383,
                 p_uncounted = c(0, .05, .10, .2, .50))

d$cfr <- cfr(w_hat = d$w_hat,
             active_cases = d$active_cases,
             deaths = d$deaths, 
             recoveries = d$recoveries,
             p_uncounted = d$p_uncounted)

crude_cfr <- 4983/134748
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
