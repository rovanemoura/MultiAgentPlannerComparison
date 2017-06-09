#ifndef STATE_SAMPLER_H
#define STATE_SAMPLER_H

#include "globals.h"
#include "ff_heuristic.h"
#include "ff_heuristic.h"
#include "option_parser.h"

#include <vector>

class StateSampler {

    FFHeuristic* ff;

    void compute_sink_fluents();
    std::vector<std::vector<bool> > is_sink;

    std::vector<std::vector<std::pair<int, int> > > invariant_exactly_one;

public:
    StateSampler();
    std::vector<std::pair<int, int> > sample_random_state();
};

#endif
