model SIR_deterministic {
  const N = 1000; // population size
  const d_infection = 14; // duration of infection: 2 weeks
  
  state S, I, R;  // susceptible, infectious, recovered

  obs Prevalence;  // observations

  param R0; // basic reproduction number

  sub parameter {
    R0 ~ uniform(1, 3)
  }

  sub initial {
    S <- N - 1
    I <- 1
    R <- 0
  }

  sub transition { // daily time step
    inline i_beta = R0 / d_infection
    inline i_gamma = 1 / d_infection
    ode {
      dS/dt = - i_beta * S * I / N
      dI/dt = i_beta * S * I / N - i_gamma * I
      dR/dt = i_gamma * I
    }
  }

  sub observation {
    Prevalence ~ poisson(I)
  }
}
