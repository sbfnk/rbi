model SIR_stoch_jump {
  const time_step = 1; // time step
  const h = 7; // incidence time step: 1 week
  const N = 1000; // population size
  const d_infection = 14; // duration of infection: 2 weeks
  
  noise n_transmission;  // random transmission
  noise n_recovery;  // random recovery

  state S, I, R, Z;  // susceptible, infectious, recovered

  obs Incidence;  // observations

  param rep; //reporting rate
  param R0; // basic reproduction number

  sub parameter {
    rep ~ uniform(0,1)
    R0 ~ uniform(1,3)
  }

  sub initial {
    S <- N - 1
    I <- 1
    R <- 0
    Z <- 1
  }

  sub transition (delta = time_step) {
    inline i_gamma = 1 / d_infection
    inline i_lambda = R0 / d_infection * I / N

    Z <- (t_now % h == 0 ? 0 : Z) // reset incidence every h time steps

    n_transmission ~ binomial(S, 1 - exp(-i_lambda * time_step))
    n_recovery ~ binomial(I, 1-exp(-i_gamma * time_step))

    S <- S - n_transmission
    I <- I + n_transmission - n_recovery
    R <- R + n_recovery
    Z <- Z + n_transmission
  }

  sub observation {
    Incidence ~ poisson(rep * Z)
  }
}
