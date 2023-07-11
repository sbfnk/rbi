model SIR_deterministic {
  const N = 1000; // population size
  const d_infection = 14; // duration of infection: 2 weeks
  
  state S, I, R, Z;  // susceptible, infectious, recovered

  obs Incidence;  // observations

  param rep; //reporting rate
  param R0; // basic reproduction number

  sub parameter {
    rep ~ uniform(0,1)
    R0 ~ uniform(1, 3)
  }

  sub initial {
    S <- N - 1
    I <- 1
    R <- 0
  }

  sub transition { // daily time step
    inline i_lambda = R0 / d_infection * I / N
    inline i_gamma = 1 / d_infection

    Z <- (t_now % 7 == 0 ? 0 : Z) // reset incidence

    ode {
      dS/dt = - i_lambda * S
      dI/dt = i_lambda * S - i_gamma * I
      dR/dt = i_gamma * I
      dZ/dt = i_lambda * S
    }
  }

  sub observation {
    Incidence ~ poisson(rep * Z)
  }
}
