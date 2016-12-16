/**
 * SIR model.
 */
model SIR {
  const h = 7; // incidence time step
  const N = 1000; // population size
  
  noise n_transmission;  // noise term
  noise n_recovery;  // noise term

  state S, I, R, Z;  // susceptible, infectious, recovered

  obs Incidence;  // observations

  param p_rep; //reporting rate
  param p_R0; // basic reproduction number
  param p_d_infection; // duration of infection

  sub parameter {
    p_rep ~ uniform(0,1)
    p_R0 ~ uniform(1,3)
    p_d_infection ~ gamma(shape = 7, scale = 2)
  }

  sub initial {
    S <- N - 1
    I <- 1
    R <- 0
    Z <- 1
  }

  sub transition {

    n_transmission ~ wiener() // noise terms
    n_recovery ~ wiener() // noise terms

    Z <- (t_now % 7 == 0 ? 0 : Z) // reset incidence

    inline i_beta = p_R0 / p_d_infection * exp(n_transmission)
    inline i_gamma = 1 / p_d_infection * exp(n_recovery)

    ode (alg='RK4', h=1e-1, atoler=1e-2, rtoler=1e-5) {
      dS/dt = - i_beta * S * I / N
      dI/dt = i_beta * S * I / N - i_gamma * I
      dR/dt = i_gamma * I
      dZ/dt = i_beta * S * I / N
    }
  }

  sub observation {
    Incidence ~ truncated_gaussian(mean = p_rep * Z, std = sqrt(p_rep * (1 - p_rep) * Z + 1), lower = 0)
  }

  sub proposal_initial {
    S <- N - 1
    I <- 1
    R <- 0
    Z <- 1
  }
  sub proposal_parameter {
    inline _old_mean_ = p_rep
    p_rep ~ truncated_gaussian(mean = p_rep, std = 1 * 0.0366811323187316, lower = 0, upper = 1)
    inline _old_mean_diff_ = p_rep - _old_mean_
    p_R0 ~ truncated_gaussian(mean = p_R0 + (-1.88911967183379) * _old_mean_diff_, std = 1 * 0.2, lower = 1, upper = 3)
    p_d_infection ~ gaussian(mean = p_d_infection + (-4.63156538162316) * _old_mean_diff_, std = 1 * 1.25785030854732)
  }
}
