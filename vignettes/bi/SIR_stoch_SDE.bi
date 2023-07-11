model SIR_stoch_SDE {
  const h = 7; // incidence time step: 1 week
  const N = 1000; // population size
  const d_infection = 14; // duration of infection: 2 weeks
  
  noise n_transmission;  // noise term
  noise n_recovery;  // noise term

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

  sub transition {

    inline i_gamma = 1 / d_infection
    inline i_lambda = R0 / d_infection * I / N

    n_transmission ~ wiener() // noise terms
    n_recovery ~ wiener() // noise terms

    Z <- (t_now % 7 == 0 ? 0 : Z) // reset incidence

    ode (alg='RK4(3)', h=1e-1, atoler=1e-2, rtoler=1e-5) {
      dS/dt = - i_lambda * S - sqrt(i_lambda) * n_transmission
      dI/dt = i_lambda * S - i_gamma * I + sqrt(i_lambda) * n_transmission - sqrt(i_gamma) * n_recovery
      dR/dt = i_gamma * I + sqrt(i_gamma) * n_recovery
      dZ/dt = i_lambda * S + sqrt(i_lambda) * n_transmission
    }
  }

  sub observation {
    Incidence ~ poisson(rep * Z)
  }
}
