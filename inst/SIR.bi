model SIR {
  const h = 7
  const N = 1000
  const d_infection = 14
  noise n_transmission
  noise n_recovery
  state S
  state I
  state R
  state Z
  obs Incidence
  param p_rep
  param p_R0
  sub parameter {
    p_rep ~ uniform(0,1)
    p_R0 ~ uniform(1,3)
  }
  sub initial {
    S <- N - 1
    I <- 1
    R <- 0
    Z <- 1
  }
  sub transition {
    n_transmission ~ wiener()
    n_recovery ~ wiener()
    Z <- (t_now % 7 == 0 ? 0 : Z)
    inline i_beta = p_R0 / d_infection * exp(n_transmission)
    inline i_gamma = 1 / d_infection * exp(n_recovery)
    ode (alg='RK4(3)', h=1e-1, atoler=1e-2, rtoler=1e-5) {
      dS/dt = - i_beta * S * I / N
      dI/dt = i_beta * S * I / N - i_gamma * I
      dR/dt = i_gamma * I
      dZ/dt = i_beta * S * I / N
    }
  }
  sub observation {
    Incidence ~ poisson(p_rep * Z)
  }
  sub proposal_initial {
    S <- N - 1
    I <- 1
    R <- 0
    Z <- 1
  }
  sub proposal_parameter {
    p_rep ~ truncated_gaussian(mean = p_rep, std = 0.03, lower = 0, upper = 1)
    p_R0 ~ truncated_gaussian(mean = p_R0, std = 0.2, lower = 1, upper = 3)
  }
}
