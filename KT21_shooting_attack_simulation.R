# Seed -- makes the following stochastic analysis reproducible
set.seed(123)

# Set up function to simulate a single shooting attack
shooting_attack <- function(shots, bs, dmg, as, MW = 0, APx = 0, Px = 0, ceaseless = F, lethal = 6, df = 3){
  
  # Assign normal and crit damage
  normal_damage <- dmg[1]
  critical_damage <- dmg[2]
  
  # Make hit rolls
  hit_rolls <- sample(1:6, shots, T)
  
  # Check if any are sixes for the Penetrating special rule
  ones <- hit_rolls == 1
  if(ceaseless & any(ones)){
    n_ones <- sum(ones)
    hit_rolls[ones] <- sample(1:6, n_ones, T)
  }
  sixes <- hit_rolls == 6
  if(any(sixes) & APx == 0) APx <- Px
  
  # Make defence rolls
  defence_rolls <- sample(1:6, max(0, df - APx), T)
  
  # Count the hits and hit types
  crit_hits <- sum(hit_rolls >= lethal)
  crit_saves <- sum(defence_rolls == 6)
  normal_hits <- sum(hit_rolls >= bs & hit_rolls < lethal)
  normal_saves <- sum(defence_rolls >= as & defence_rolls < 6)
  
  # Pair off criticals first
  n_crits <- max(0, crit_hits - crit_saves)
  
  # Immediately wipe off those critical saves from the books
  crit_saves <- max(0, crit_saves - crit_hits)

  # If there are more critical hits remaining, the defender can use a pair of normal saves to block it
  # Generally only makes sense to do this if there are less than two normal hits
  if(n_crits > 0 & normal_saves > 1 & normal_hits < 2){
    n_crits <- n_crits - 1
    normal_saves <- normal_saves - 2
  } 
  
  # Count remaining critical saves (can be used to block normal saves)
  crit_saves <- max(0, crit_saves - n_crits)
  
  # Cancel any remaining normal hits, also using any unused critical saves
  n_normals <- max(0, normal_hits - normal_saves - crit_saves)
  
  # Apply damage from any unsaved attacks
  return(normal_damage * n_normals + critical_damage * n_crits + crit_hits * MW)
  
}

# Do simulations of shooting attacks
n_sims <- 1e5 # 100,000 times for each combo of armour save and weapon profile
saves <- 2:6
n_saves <- length(saves)
empty <- numeric(n_sims)
bolter_damage <- heavy_damage <- frag_damage <- krak_damage <- flamer_damage <- melta_damage <- grav_damage <- normal_plasma_damage <- super_plasma_damage <- vector("list", 5)
for(i in 1:n_saves){
  if(saves[i] <= 3){ 
    grav_lethal <- 4
  } else {
    grav_lethal <- 6
  }
  bolter_damage[[i]] <- heavy_damage[[i]] <- frag_damage[[i]] <- krak_damage[[i]] <- flamer_damage[[i]] <- grav_damage[[i]] <- normal_plasma_damage[[i]] <- super_plasma_damage[[i]] <- melta_damage[[i]] <- empty
  for(j in 1:n_sims){
    bolter_damage[[i]][j] <- shooting_attack(shots = 4, bs = 3, dmg = c(3, 4), as = saves[i])
    heavy_damage[[i]][j] <- shooting_attack(shots = 5, bs = 3, dmg = c(4, 5), as = saves[i], Px = 1)
    frag_damage[[i]][j] <- shooting_attack(shots = 4, bs = 3, dmg = c(3, 5), as = saves[i])
    krak_damage[[i]][j] <- shooting_attack(shots = 4, bs = 3, dmg = c(5, 7), as = saves[i], APx = 1)
    flamer_damage[[i]][j] <- shooting_attack(shots = 5, bs = 2, dmg = c(2, 2), as = saves[i])
    grav_damage[[i]][j] <- shooting_attack(shots = 4, bs = 3, dmg = c(4,5), as = saves[i], lethal = grav_lethal, APx = 1)
    normal_plasma_damage[[i]][j] <- shooting_attack(shots = 4, bs = 3, dmg = c(5,6), as = saves[i], APx = 1)
    super_plasma_damage[[i]][j] <- shooting_attack(shots = 4, bs = 3, dmg = c(5,6), as = saves[i], APx = 2)
    melta_damage[[i]][j] <- shooting_attack(shots = 4, bs = 3, dmg = c(5, 3), as = saves[i], APx = 2, MW = 4)
  }
}

# Compile results into a dataframe
df <- data.frame(
  saves = saves, 
  bolter = unlist(lapply(bolter_damage, mean)),
  hb = unlist(lapply(heavy_damage, mean)),
  fm = unlist(lapply(frag_damage, mean)),
  km = unlist(lapply(krak_damage, mean)),
  flamer = unlist(lapply(flamer_damage, mean)),
  grav = unlist(lapply(grav_damage, mean)),
  normal_plasma = unlist(lapply(normal_plasma_damage, mean)),
  super_plasma = unlist(lapply(super_plasma_damage, mean)),
  melta = unlist(lapply(melta_damage, mean))
)

# Define function to estimate probability mass above or at a threshold
emp_prob <- function(x, threshold) mean(x >= threshold)

# Compute the probabilities for the special weapons
emp_prob(bolter_damage[[2]], 12)
emp_prob(bolter_damage[[2]], 13)
emp_prob(flamer_damage[[2]], 12)
emp_prob(flamer_damage[[2]], 13)
emp_prob(grav_damage[[2]], 12)
emp_prob(grav_damage[[2]], 13)
emp_prob(normal_plasma_damage[[2]], 12)
emp_prob(normal_plasma_damage[[2]], 13)
emp_prob(super_plasma_damage[[2]], 12)
emp_prob(super_plasma_damage[[2]], 13)
emp_prob(melta_damage[[2]], 12)
emp_prob(melta_damage[[2]], 13)

# Compute the probabilities for the heavy weapons
emp_prob(heavy_damage[[2]], 12)
emp_prob(heavy_damage[[2]], 13)
emp_prob(frag_damage[[2]], 12)
emp_prob(frag_damage[[2]], 13)
emp_prob(krak_damage[[2]], 12)
emp_prob(krak_damage[[2]], 13)

# Sisters of silence, battle
emp_prob(heavy_damage[[2]], 8)
emp_prob(krak_damage[[2]], 8)

# Sister leaders
emp_prob(heavy_damage[[2]], 9)
emp_prob(krak_damage[[2]], 9)

# necron immortals, stealth suits
emp_prob(heavy_damage[[2]], 10)
emp_prob(krak_damage[[2]], 10)

# tactical marines
emp_prob(heavy_damage[[2]], 11)
emp_prob(krak_damage[[2]], 11)

# Assault intercessor sgt
emp_prob(heavy_damage[[2]], 14)
emp_prob(krak_damage[[2]], 14)

# Heavy intercessors
emp_prob(heavy_damage[[2]], 18)
emp_prob(krak_damage[[2]], 18)
