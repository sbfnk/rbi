# Collection of SIR models for LibBi

## Deterministic SIR model, observations of prevalence

``` r
model_str <- readLines("bi/SIR_deter_prev.bi")
cat(paste(model_str, "\n"))
```

    ## model SIR_deterministic { 
    ##    const N = 1000; // population size 
    ##    const d_infection = 14; // duration of infection: 2 weeks 
    ##     
    ##    state S, I, R;  // susceptible, infectious, recovered 
    ##   
    ##    obs Prevalence;  // observations 
    ##   
    ##    param R0; // basic reproduction number 
    ##   
    ##    sub parameter { 
    ##      R0 ~ uniform(1, 3) 
    ##    } 
    ##   
    ##    sub initial { 
    ##      S <- N - 1 
    ##      I <- 1 
    ##      R <- 0 
    ##    } 
    ##   
    ##    sub transition { // daily time step 
    ##      inline i_beta = R0 / d_infection 
    ##      inline i_gamma = 1 / d_infection 
    ##      ode { 
    ##        dS/dt = - i_beta * S * I / N 
    ##        dI/dt = i_beta * S * I / N - i_gamma * I 
    ##        dR/dt = i_gamma * I 
    ##      } 
    ##    } 
    ##   
    ##    sub observation { 
    ##      Prevalence ~ poisson(I) 
    ##    } 
    ##  }

``` r
sir_model <- bi_model(lines = model_str)
```

## Deterministic SIR model, observations of incidence

``` r
model_str <- readLines("bi/SIR_deter.bi")
cat(paste(model_str, "\n"))
```

    ## model SIR_deterministic { 
    ##    const N = 1000; // population size 
    ##    const d_infection = 14; // duration of infection: 2 weeks 
    ##     
    ##    state S, I, R, Z;  // susceptible, infectious, recovered 
    ##   
    ##    obs Incidence;  // observations 
    ##   
    ##    param rep; //reporting rate 
    ##    param R0; // basic reproduction number 
    ##   
    ##    sub parameter { 
    ##      rep ~ uniform(0,1) 
    ##      R0 ~ uniform(1, 3) 
    ##    } 
    ##   
    ##    sub initial { 
    ##      S <- N - 1 
    ##      I <- 1 
    ##      R <- 0 
    ##    } 
    ##   
    ##    sub transition { // daily time step 
    ##      inline i_lambda = R0 / d_infection * I / N 
    ##      inline i_gamma = 1 / d_infection 
    ##   
    ##      Z <- (t_now % 7 == 0 ? 0 : Z) // reset incidence 
    ##   
    ##      ode { 
    ##        dS/dt = - i_lambda * S 
    ##        dI/dt = i_lambda * S - i_gamma * I 
    ##        dR/dt = i_gamma * I 
    ##        dZ/dt = i_lambda * S 
    ##      } 
    ##    } 
    ##   
    ##    sub observation { 
    ##      Incidence ~ poisson(rep * Z) 
    ##    } 
    ##  }

``` r
sir_model <- bi_model(lines = model_str)
```

## Stochastic SIR model (SDE), observations of incidence

``` r
model_str <- readLines("bi/SIR_stoch_SDE.bi")
cat(paste(model_str, "\n"))
```

    ## model SIR_stoch_SDE { 
    ##    const h = 7; // incidence time step: 1 week 
    ##    const N = 1000; // population size 
    ##    const d_infection = 14; // duration of infection: 2 weeks 
    ##     
    ##    noise n_transmission;  // noise term 
    ##    noise n_recovery;  // noise term 
    ##   
    ##    state S, I, R, Z;  // susceptible, infectious, recovered 
    ##   
    ##    obs Incidence;  // observations 
    ##   
    ##    param rep; //reporting rate 
    ##    param R0; // basic reproduction number 
    ##   
    ##    sub parameter { 
    ##      rep ~ uniform(0,1) 
    ##      R0 ~ uniform(1,3) 
    ##    } 
    ##   
    ##    sub initial { 
    ##      S <- N - 1 
    ##      I <- 1 
    ##      R <- 0 
    ##      Z <- 1 
    ##    } 
    ##   
    ##    sub transition { 
    ##   
    ##      inline i_gamma = 1 / d_infection 
    ##      inline i_lambda = R0 / d_infection * I / N 
    ##   
    ##      n_transmission ~ wiener() // noise terms 
    ##      n_recovery ~ wiener() // noise terms 
    ##   
    ##      Z <- (t_now % 7 == 0 ? 0 : Z) // reset incidence 
    ##   
    ##      ode (alg='RK4(3)', h=1e-1, atoler=1e-2, rtoler=1e-5) { 
    ##        dS/dt = - i_lambda * S - sqrt(i_lambda) * n_transmission 
    ##        dI/dt = i_lambda * S - i_gamma * I + sqrt(i_lambda) * n_transmission - sqrt(i_gamma) * n_recovery 
    ##        dR/dt = i_gamma * I + sqrt(i_gamma) * n_recovery 
    ##        dZ/dt = i_lambda * S + sqrt(i_lambda) * n_transmission 
    ##      } 
    ##    } 
    ##   
    ##    sub observation { 
    ##      Incidence ~ poisson(rep * Z) 
    ##    } 
    ##  }

``` r
sir_model <- bi_model(lines = model_str)
```

## Stochastic SIR model (jump), observations of incidence

``` r
model_str <- readLines("bi/SIR_stoch_jump.bi")
cat(paste(model_str, "\n"))
```

    ## model SIR_stoch_jump { 
    ##    const time_step = 1; // time step 
    ##    const h = 7; // incidence time step: 1 week 
    ##    const N = 1000; // population size 
    ##    const d_infection = 14; // duration of infection: 2 weeks 
    ##     
    ##    noise n_transmission;  // random transmission 
    ##    noise n_recovery;  // random recovery 
    ##   
    ##    state S, I, R, Z;  // susceptible, infectious, recovered 
    ##   
    ##    obs Incidence;  // observations 
    ##   
    ##    param rep; //reporting rate 
    ##    param R0; // basic reproduction number 
    ##   
    ##    sub parameter { 
    ##      rep ~ uniform(0,1) 
    ##      R0 ~ uniform(1,3) 
    ##    } 
    ##   
    ##    sub initial { 
    ##      S <- N - 1 
    ##      I <- 1 
    ##      R <- 0 
    ##      Z <- 1 
    ##    } 
    ##   
    ##    sub transition (delta = time_step) { 
    ##      inline i_gamma = 1 / d_infection 
    ##      inline i_lambda = R0 / d_infection * I / N 
    ##   
    ##      Z <- (t_now % h == 0 ? 0 : Z) // reset incidence every h time steps 
    ##   
    ##      n_transmission ~ binomial(S, 1 - exp(-i_lambda * time_step)) 
    ##      n_recovery ~ binomial(I, 1-exp(-i_gamma * time_step)) 
    ##   
    ##      S <- S - n_transmission 
    ##      I <- I + n_transmission - n_recovery 
    ##      R <- R + n_recovery 
    ##      Z <- Z + n_transmission 
    ##    } 
    ##   
    ##    sub observation { 
    ##      Incidence ~ poisson(rep * Z) 
    ##    } 
    ##  }

``` r
sir_model <- bi_model(lines = model_str)
```

## Example observation data frame

``` r
obs <- data.frame(
  value = c(1, 6, 2, 26, 99, 57, 78, 57, 15, 9, 4, 1, 1, 1, 0, 2, 0), 
  time = c(0, 7, 14, 21, 28, 35, 42, 49, 56, 63, 70, 77, 84, 91, 98, 105, 112)
)
```
