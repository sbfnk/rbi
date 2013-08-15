/**
 * Lotka-Volterra-like phytoplankton-zooplankton (PZ) model.
 * 
 * @author Lawrence Murray <lawrence.murray@csiro.au>
 * $Rev$
 * $Date$
 */
model PZ {
  const c = 0.25   // zooplankton clearance rate
  const e = 0.3    // zooplankton growth efficiency
  const m_l = 0.1  // zooplankton linear mortality
  const m_q = 0.1  // zooplankton quadratic mortality

  param mu, sigma  // mean and standard deviation of phytoplankton growth
  state P, Z       // phytoplankton, zooplankton
  noise alpha      // stochastic phytoplankton growth rate
  obs P_obs        // observations of phytoplankton
  
  sub parameter {
    mu ~ uniform(0.0, 1.0)
    sigma ~ uniform(0.0, 0.5)
  }
  
  sub proposal_parameter {
    mu ~ gaussian(mu, 0.02);
    sigma ~ gaussian(sigma, 0.01);
  }

  sub initial {
    P ~ log_normal(log(2.0), 0.2)
    Z ~ log_normal(log(2.0), 0.1)
  }

  sub transition(delta = 1.0) {
    alpha ~ gaussian(mu, sigma)
    ode {
      dP/dt = alpha*P - c*P*Z
      dZ/dt = e*c*P*Z - m_l*Z - m_q*Z*Z
    }
  }

  sub observation {
    P_obs ~ log_normal(log(P), 0.2)
  }
}
