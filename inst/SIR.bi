/**
 * SIR model.
 */
model SIR {
  const h = 7; // incidence time step
  const N = 1000; // population size
  
  noise n_transmission;  // noise term
  noise n_recovery;  // noise term

  state S, I, R, Z;  // susceptible, infectious, recovered

  input N; // population size

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
    Z <- 0
  }

  sub transition {

    n_transmission ~ wiener() // noise terms
    n_recovery ~ wiener() // noise terms

    Z <- (t_now % h == 0 ? 0 : Z) // reset incidence

    inline i_beta = p_R0 / p_d_infection * exp(n_transmission)
    inline i_gamma = 1 / p_d_infection * exp(n_recovery)

    ode {
      dS/dt = - i_beta * S * I / N
      dI/dt = i_beta * S * I / N - i_gamma * I
      dR/dt = i_gamma * I
      dZ/dt = i_beta * S * I / N
    }
  }

  sub observation {
    Incidence ~ max(gaussian(mean = p_rep * Z, std = sqrt(p_rep * (1 - p_rep) * Z)), 0)
  }
}
