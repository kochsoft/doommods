# Dedicated to decino and caused by his BFG video
# https://www.youtube.com/watch?v=MsCqLQJ1EOc

# This is for seeing the stats of a point blank BFG spray with all 40
# rays and the orb hitting the same target.

# Assumptions:
# + All (40*15)D8 RNG rolls are consecutive (severely reducing variance
#   due to the 256 element random table wrapping around twice for one
#   single BFG strike.
# + Since the orb hits several frames earlier, its 100*1D8 damage is
#   considered to be 'independent' since there is no telling how many
#   rng numbers have been drawn between orb strike and BFG spray.

# Textual Results:
# > summary(data_total_sim)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   2752    2964    3159    3134    3300    3507 
# > summary(data_spray)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   2652    2679    2688    2686    2695    2707 
# > summary(data_orb)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   100.0   300.0   500.0   447.7   600.0   800.0
#
# Assuming independence of both this leads to min == 2752 and max == 3507

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

rng_d8 = bitwAnd(rng, 7) + 1;

spray <- function(tab, j0)
{
  n = 15 * 40;
  indices = ((j0:(j0+n-1)) %% length(tab)) + 1;  # << R vectors are indexed starting at 1.
  damage = 0;
  for (j in indices) { damage = damage + tab[j]; }
  return (damage);
}

orb <- function(tab, j0)
{
  j = (j0 %% length(tab)) + 1;
  return (100 * tab[j]);
}

indices = 1:length(rng_d8);
data_spray = NULL;
data_orb = NULL;
for (j in indices)
{
  data_spray = c(data_spray, spray(rng_d8, j));
  data_orb = c(data_orb, orb(rng_d8, j));
}

data_total_sim = NULL;
for (orb in data_orb)
{
  data_total_sim = c(data_total_sim, data_spray + orb);
}

# Generates above textual output.
summary(data_total_sim);
summary(data_spray);
summary(data_orb);

png("bfg_plots.png");
par(mfrow=c(1,2));
plot(density(data_spray), main="Low-Var Spray");
boxplot(data_total_sim, main="Point Blank Total");
dev.off();
