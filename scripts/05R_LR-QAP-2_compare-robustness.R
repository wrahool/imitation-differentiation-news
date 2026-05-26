library(sna)

load("results/new_qap_neg_asymmetry_results.RData")
load("results/new_qap_neg_asymmetry_results-10seasons.RData")
load("results/new_qap_neg_asymmetry_results-adfontes.RData")

# do L outlets differentiate from R more than than do from L  (LL baseline)
# focus on x2

# after controlling for reciprocity, all topics
qap_asym_neg_60seasons$`LL baseline All A` # yes, sig, positively (4.3 times more likely)
qap_asym_neg_10seasons$`LL baseline All A` # non sig
qap_asym_neg_adfontes$`LL baseline All A` # non sig

# without controlling for reciprocity, all topics
qap_asym_neg_60seasons$`LL baseline All B` # yes, sig, positively (4.28 times more likely)
qap_asym_neg_10seasons$`LL baseline All B` # non sig
qap_asym_neg_adfontes$`LL baseline All B` # non sig

# after controlling for reciprocity, political topics
qap_asym_neg_60seasons$`LL baseline Political A` # yes, sig, positively (4.39 times  more likely)
qap_asym_neg_10seasons$`LL baseline Political A` # non sig
qap_asym_neg_adfontes$`LL baseline Political A` # non sig

# without controlling for reciprocity, political topics
qap_asym_neg_60seasons$`LL baseline Political B` # yes, sig, positively (4.72 times more likely)
qap_asym_neg_10seasons$`LL baseline Political B` # non sig
qap_asym_neg_adfontes$`LL baseline Political B` # non sig

# after controlling for reciprocity, entertainment topics
qap_asym_neg_60seasons$`LL baseline Entertainment A` # non sig
qap_asym_neg_10seasons$`LL baseline Entertainment A` # non sig
qap_asym_neg_adfontes$`LL baseline Entertainment A` # non sig

# without controlling for reciprocity, entertainment topics
qap_asym_neg_60seasons$`LL baseline Entertainment B` # non sig
qap_asym_neg_10seasons$`LL baseline Entertainment B` # non sig
qap_asym_neg_adfontes$`LL baseline Entertainment B` # non sig

# do R outlets differentiate from L more than than do from R  (RR baseline)
# focus on x1

# after controlling for reciprocity, all topics
qap_asym_neg_60seasons$`RR baseline All A` # non sig
qap_asym_neg_10seasons$`RR baseline All A` # non sig
qap_asym_neg_adfontes$`RR baseline All A` # non sig

# without controlling for reciprocity, all topics
qap_asym_neg_60seasons$`RR baseline All B` # non sig
qap_asym_neg_10seasons$`RR baseline All B` # non sig
qap_asym_neg_adfontes$`RR baseline All B` # non sig

# after controlling for reciprocity, political topics
qap_asym_neg_60seasons$`RR baseline Political A` # non sig
qap_asym_neg_10seasons$`RR baseline Political A` # non sig
qap_asym_neg_adfontes$`RR baseline Political A` # non sig

# without controlling for reciprocity, political topics
qap_asym_neg_60seasons$`RR baseline Political B` # non sig
qap_asym_neg_10seasons$`RR baseline Political B` # non sig
qap_asym_neg_adfontes$`RR baseline Political B` # non sig

# after controlling for reciprocity, entertainment topics
qap_asym_neg_60seasons$`RR baseline Entertainment A` # non sig
qap_asym_neg_10seasons$`RR baseline Entertainment A` # non sig
qap_asym_neg_adfontes$`RR baseline Entertainment A` # non sig

# without controlling for reciprocity, entertainment topics
qap_asym_neg_60seasons$`RR baseline Entertainment B` # non sig
qap_asym_neg_10seasons$`RR baseline Entertainment B` # non sig
qap_asym_neg_adfontes$`RR baseline Entertainment B` # non sig

