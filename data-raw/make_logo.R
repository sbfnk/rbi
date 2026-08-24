## Build the rbi hex logo: an epidemic wave tracked by a particle filter,
## pinned by observations and fanning out once the data run out.
set.seed(11)

W <- 1200
H <- 1385

## chart panel inside the hexagon
x0 <- 120; x1 <- 1060
ytop <- 660; ybot <- 1190

n <- 300
tt <- seq(0, 1, length.out = n)
split_at <- 0.55

## latent signal: an epidemic wave, the sort of state LibBi filters
mu <- 0.14 + 0.72 * exp(-0.5 * ((tt - 0.36) / 0.16)^2) +
  0.22 * exp(-0.5 * ((tt - 0.86) / 0.16)^2)

## the filter keeps trajectories close to the data, then they diverge
ramp <- pmax(0, tt - split_at) / (1 - split_at)
shrink <- 0.09 + 0.91 * ramp^1.2

## smooth wiggles: coarse random walk, spline-interpolated
smooth_walk <- function() {
  k <- 16
  kx <- seq(0, 1, length.out = k)
  ky <- cumsum(rnorm(k, 0, 1)) / sqrt(k)
  spline(kx, ky - ky[1], xout = tt)$y
}

## squash towards the panel edges so nothing escapes the hexagon
squash <- function(y) 0.5 + 0.47 * tanh((y - 0.5) / 0.47)

n_traj <- 55
traj <- replicate(n_traj, squash(mu + smooth_walk() * 0.28 * shrink))

## observations, only where the filter has data
obs_i <- round(seq(8, split_at * n, length.out = 8))
obs_x <- tt[obs_i]
obs_y <- mu[obs_i] + rnorm(length(obs_i), 0, 0.015)

sx <- function(x) x0 + x * (x1 - x0)
sy <- function(y) ybot - y * (ybot - ytop)

pts <- function(x, y) paste(sprintf("%.1f,%.1f", sx(x), sy(y)), collapse = " ")

navy <- "#1f3864"
blue <- "#4a7fbd"

lines_svg <- vapply(seq_len(n_traj), function(i) {
  sprintf(
    '      <polyline points="%s" fill="none" stroke="%s" stroke-width="3.8" stroke-opacity="0.34"/>',
    pts(tt, traj[, i]), blue
  )
}, character(1))

## posterior mean, running through the middle of the cloud
mean_svg <- sprintf(
  '      <polyline points="%s" fill="none" stroke="%s" stroke-width="10" stroke-linecap="round" stroke-linejoin="round"/>',
  pts(tt[tt <= 0.92], squash(mu)[tt <= 0.92]), navy
)

obs_svg <- vapply(seq_along(obs_x), function(i) {
  sprintf('    <circle cx="%.1f" cy="%.1f" r="14" fill="%s"/>',
          sx(obs_x[i]), sy(obs_y[i]), navy)
}, character(1))

hex <- "600,8 1194,350 1194,1035 600,1377 6,1035 6,350"

svg <- c(
  sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">', W, H, W, H),
  '  <defs>',
  '    <clipPath id="hexclip">',
  sprintf('      <polygon points="%s"/>', hex),
  '    </clipPath>',
  sprintf('    <linearGradient id="fade" x1="%.0f" y1="0" x2="%.0f" y2="0" gradientUnits="userSpaceOnUse">', sx(0.86), sx(1)),
  '      <stop offset="0" stop-color="#fff"/>',
  '      <stop offset="1" stop-color="#000"/>',
  '    </linearGradient>',
  '    <mask id="fademask">',
  sprintf('      <rect x="0" y="0" width="%.0f" height="%d" fill="#fff"/>', sx(0.86), H),
  sprintf('      <rect x="%.0f" y="0" width="%.0f" height="%d" fill="url(#fade)"/>', sx(0.86), sx(1) - sx(0.86), H),
  '    </mask>',
  '  </defs>',
  sprintf('  <polygon points="%s" fill="#f6f7f9"/>', hex),
  '  <g clip-path="url(#hexclip)">',
  '    <g mask="url(#fademask)">',
  lines_svg,
  '    </g>',
  mean_svg,
  obs_svg,
  '  </g>',
  sprintf('  <polygon points="%s" fill="none" stroke="%s" stroke-width="16"/>', hex, navy),
  sprintf('  <text x="600" y="580" text-anchor="middle" font-family="Inter, Fira Sans, sans-serif" font-weight="600" font-size="300" fill="%s" letter-spacing="4">rbi</text>', navy),
  '</svg>'
)

writeLines(svg, "man/figures/logo.svg")

## rendered with:
##   inkscape man/figures/logo.svg -o logo-full.png -w 1200
##   magick logo-full.png -resize 240x -strip man/figures/logo.png
