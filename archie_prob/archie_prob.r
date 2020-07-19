# Dedicted to decino and caused by his Arch-Vile video
# https://www.youtube.com/watch?v=xsVWdMVYJgk&t=40

# This is for seeing probabilities to stun the arch-vile.

# Assumptions:
# * Rational numbers are always truncated to integers prior to comparisons with rng.
# * Let r in rng in {0,..,255}. Seeing that 8/255 equals 0.0313 quite precisely, I assume
#   that we either need a value <= 8 or >= 247 for an Arch-Vile pain state.
#   I label the former with the property forward == FALSE and the latter forward == TRUE.
#   Note that this is imperfect: A threshhold of 255 would still be liable to pain if
#   255 is rolled. However, using 256 as maximum would miss the 0.0313 that is neatly
#   matched with 8/255 using 255 as denominator.
# * As stated in the video the probability to enter its pain state
#   is assumed (&t=40) to be 0.0313 == 8/255.
# * A strike consists of so many consecutive hits. 7 for a shotgun,
#   20 for the super shotgun, 1 for a pistol.

# Textual Results:
# ================
# Concerning Super Shotguns:
# > disp_pain(rng, thresh_pain, 20, TRUE );
#   20 pellets, thresh_pain is 247. Forward probabilities (pain state requires high numbers): Pain count 165, Pain prob 0.6445.
# > disp_pain(rng, thresh_pain, 20, FALSE);
#   20 pellets, thresh_pain is 9. Backward probabilities (pain state requires low numbers): Pain count 131, Pain prob 0.5117.
#
# Concerning Zombie Shotguns:
# > disp_pain(rng, thresh_pain,  7, TRUE );
#   7 pellets, thresh_pain is 247. Forward probabilities (pain state requires high numbers): Pain count 71, Pain prob 0.2773.
# > disp_pain(rng, thresh_pain,  7, FALSE);
#   7 pellets, thresh_pain is 9. Backward probabilities (pain state requires low numbers): Pain count 50, Pain prob 0.1953.
#
# Concerning Trusty Pistol:
# > disp_pain(rng, thresh_pain,  1, TRUE );
#   1 pellets, thresh_pain is 247. Forward probabilities (pain state requires high numbers): Pain count 12, Pain prob 0.0469.
# > disp_pain(rng, thresh_pain,  1, FALSE);
#   1 pellets, thresh_pain is 9. Backward probabilities (pain state requires low numbers): Pain count 8, Pain prob 0.0312.

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
#  thresh_pain: Value in {0,..,255} for determining when a pain state should
#    be entered. Is a low or a high value considered "good resistance"?
#    depends on 'forward parameter' below. Decino: "Archie has p==0.0313 for pain".
#    [Note that restricting the values to 255 instead of 256 will either lead
#     to "perfect resistance" or "perfect sensitivity" being impossible. I choose
#     this limit because 8/255 meets decino's probability of 0.0313 so neatly.]
#  n_pellets: Number of pellets served in the strike examined.
#  forward: See comments above. Is a random experiment set up thus that
#    success would require the rolling of high or of low numbers? TRUE means 'high'.
#    I do not know, but for me it feels right that forward==TRUE is the gospel. Emotionally.
# returns TRUE if the given parameters would lead to a pain state. FALSE else.
is_painful <- function(rng, index0, thresh_pain, n_pellets, forward)
{
  index = ((index0:(index0+n_pellets-1))%%length(rng)) + 1; # << R vectors are indexed starting at 1.
  ouch = NULL;
  if (forward)
  {
    ouch = sum(rng[index] >= thresh_pain) > 0;
  } else {
    ouch = sum(rng[index] <= 255-thresh_pain) > 0;
  }
  return (ouch);
}

# Will iterate through all possible starting positions of the RNG pointer and
# check if the described hit-scenario would lead to a pain state.
# returns the number of cases when this was observed. Since the RNG table has
# 256 entries this value/256 is the probability for a pain-state.
pain_count <- function(rng, thresh_pain, n_pellets, forward)
{
  N = length(rng);
  ouchs = 0;
  for (j in 1:N)
  {
    if (is_painful(rng, j, thresh_pain, n_pellets, forward))
    {
      ouchs = ouchs + 1;
    }
  }
  return (ouchs);
}

# Uses above functions to generate textual output.
disp_pain <- function(rng, thresh_pain, n_pellets, forward)
{
  pc = pain_count(rng, thresh_pain, n_pellets, forward);
  pp = pc/length(rng);
  tp = if (forward) "Forward" else "Backward";
  hilo = if (forward) "high" else "low";
  thresh_pain_adj = if (forward) thresh_pain else (256-thresh_pain);
  cat(sprintf("  %d pellets, thresh_pain is %d. %s probabilities (pain state requires %s numbers): Pain count %d, Pain prob %1.4f.\n", n_pellets, thresh_pain_adj, tp, hilo, pc, pp));
}

thresh_pain = 247; # 255-8. Note that 8/255 == 0.0313 being the probability given by decino.

cat("\nConcerning Super Shotguns:\n")
disp_pain(rng, thresh_pain, 20, TRUE );
disp_pain(rng, thresh_pain, 20, FALSE);

cat("\nConcerning Zombie Shotguns:\n")
disp_pain(rng, thresh_pain,  7, TRUE );
disp_pain(rng, thresh_pain,  7, FALSE);

cat("\nConcerning Trusty Pistol:\n")
disp_pain(rng, thresh_pain,  1, TRUE );
disp_pain(rng, thresh_pain,  1, FALSE);
