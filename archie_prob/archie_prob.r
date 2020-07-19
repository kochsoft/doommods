# Dedicated to decino and caused by his Arch-Vile video
# https://www.youtube.com/watch?v=xsVWdMVYJgk&t=40

# This is for seeing probabilities to stun the arch-vile.

# Assumptions:
# * Rational numbers are always truncated to integers prior to comparisons with rng.
# * Let r in rng in {0,..,255}. A probability p in [0,1] is assumed to be
#   met for 'forward==TRUE' if floor(256*(1-p)) <= r
#   else for 'forward == FALSE' if r < floor(256*p).
# * As stated in the video the probability to enter its pain state
#   is assumed (&t=40) to be 0.0313.
# * A strike consists of so many consecutive hits. 7 for a shotgun,
#   20 for the super shotgun, 1 for a pistol.

# Textual Results:
# ================
# Concerning Super Shotguns:
# > disp_pain(rng, p_pain, n_pellets=20, forward=TRUE );
#   20 pellets, p_pain is 0.0313. Forward probabilities (pain state requires high numbers): Pain count 165, Pain prob 0.6445.
# > disp_pain(rng, p_pain, 20, FALSE);
#   20 pellets, p_pain is 0.0313. Backward probabilities (pain state requires low numbers): Pain count 130, Pain prob 0.5078.
#
# Concerning Zombie Shotguns:
# > disp_pain(rng, p_pain,  7, TRUE );
#   7 pellets, p_pain is 0.0313. Forward probabilities (pain state requires high numbers): Pain count 71, Pain prob 0.2773.
# > disp_pain(rng, p_pain,  7, FALSE);
#   7 pellets, p_pain is 0.0313. Backward probabilities (pain state requires low numbers): Pain count 49, Pain prob 0.1914.
#
# Concerning Trusty Pistol:
# > disp_pain(rng, p_pain,  1, TRUE );
#   1 pellets, p_pain is 0.0313. Forward probabilities (pain state requires high numbers): Pain count 12, Pain prob 0.0469.
# > disp_pain(rng, p_pain,  1, FALSE);
#   1 pellets, p_pain is 0.0313. Backward probabilities (pain state requires low numbers): Pain count 7, Pain prob 0.0273.

# https://www.doomworld.com/forum/topic/89069-shotgun-minimum-damage-and-rng/
rng = c(0, 8, 109, 220, 222, 241, 149, 107,  75, 248, 254, 140,  16,  66 ,
    74,  21, 211,  47,  80, 242, 154,  27, 205, 128, 161,  89,  77,  36 ,
    95, 110,  85,  48, 212, 140, 211, 249,  22,  79, 200,  50,  28, 188 ,
    52, 140, 202, 120,  68, 145,  62,  70, 184, 190,  91, 197, 152, 224 ,
    149, 104,  25, 178, 252, 182, 202, 182, 141, 197,   4,  81, 181, 242 ,
    145,  42,  39, 227, 156, 198, 225, 193, 219,  93, 122, 175, 249,   0 ,
    175, 143,  70, 239,  46, 246, 163,  53, 163, 109, 168, 135,   2, 235 ,
    25,  92,  20, 145, 138,  77,  69, 166,  78, 176, 173, 212, 166, 113 ,
    94, 161,  41,  50, 239,  49, 111, 164,  70,  60,   2,  37, 171,  75 ,
    136, 156,  11,  56,  42, 146, 138, 229,  73, 146,  77,  61,  98, 196 ,
    135, 106,  63, 197, 195,  86,  96, 203, 113, 101, 170, 247, 181, 113 ,
    80, 250, 108,   7, 255, 237, 129, 226,  79, 107, 112, 166, 103, 241 ,
    24, 223, 239, 120, 198,  58,  60,  82, 128,   3, 184,  66, 143, 224 ,
    145, 224,  81, 206, 163,  45,  63,  90, 168, 114,  59,  33, 159,  95 ,
    28, 139, 123,  98, 125, 196,  15,  70, 194, 253,  54,  14, 109, 226 ,
    71,  17, 161,  93, 186,  87, 244, 138,  20,  52, 123, 251,  26,  36 ,
    17,  46,  52, 231, 232,  76,  31, 221,  84,  37, 216, 165, 212, 106 ,
    197, 242,  98,  43,  39, 175, 254, 145, 190,  84, 118, 222, 187, 136 ,
    120, 163, 236, 249);

# Does a hit of so many pellets lead to a pain-state?
#  rng: The rng table.
#  index0: The position in the table the rng pointer points at right now.
#  p_pain: Probability of entering a pain-state. Decino: "Archie has 0.0313".
#  n_pellets: Number of pellets served in the strike examined.
#  forward: See comments above. Is a random experiment set up thus that
#    success would require the rolling of high or of low numbers?
# returns TRUE if the given parameters would lead to a pain state. FALSE else.
is_painful <- function(rng, index0, p_pain, n_pellets, forward)
{
  index = ((index0:(index0+n_pellets-1))%%length(rng)) + 1; # << R vectors are indexed starting at 1.
  thresh_pain = NULL;
  ouch = NULL;
  if (forward)
  {
    thresh_pain = floor((1.-p_pain) * 256.);
    ouch = sum(rng[index] >= thresh_pain) > 0;
  } else {
    thresh_pain = floor(p_pain * 256.);
    ouch = sum(rng[index] < thresh_pain) > 0;
  }
  return (ouch);
}

# Will iterate through all possible starting positions of the RNG pointer and
# check if the described hit-scenario would lead to a pain state.
# returns the number of cases when this was observed. Since the RNG table has
# 256 entries this value/256 is the probability for a pain-state.
pain_count <- function(rng, p_pain, n_pellets, forward)
{
  N = length(rng);
  ouchs = 0;
  for (j in 1:N)
  {
    if (is_painful(rng, j, p_pain, n_pellets, forward))
    {
      ouchs = ouchs + 1;
    }
  }
  return (ouchs);
}

# Uses above functions to generate textual output.
disp_pain <- function(rng, p_pain, n_pellets, forward)
{
  pc = pain_count(rng, p_pain, n_pellets, forward);
  pp = pc/length(rng);
  tp = if (forward) "Forward" else "Backward";
  hilo = if (forward) "high" else "low";
  cat(sprintf("  %d pellets, p_pain is %1.4f. %s probabilities (pain state requires %s numbers): Pain count %d, Pain prob %1.4f.\n", n_pellets, p_pain, tp, hilo, pc, pp));  
}


p_pain = 0.0313;

cat("\nConcerning Super Shotguns:\n")
disp_pain(rng, p_pain, 20, TRUE );
disp_pain(rng, p_pain, 20, FALSE);

cat("\nConcerning Zombie Shotguns:\n")
disp_pain(rng, p_pain,  7, TRUE );
disp_pain(rng, p_pain,  7, FALSE);

cat("\nConcerning Trusty Pistol:\n")
disp_pain(rng, p_pain,  1, TRUE );
disp_pain(rng, p_pain,  1, FALSE);
