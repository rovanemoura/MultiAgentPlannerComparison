#ifndef STATE_SAMPLER_H
#define STATE_SAMPLER_H

#include "globals.h"
#include "ff_heuristic.h"
#include "ff_heuristic.h"
#include "option_parser.h"

#include <vector>

class StateSampler {

    FFHeuristic* ff;

    std::vector<std::vector<std::pair<int, int> > > fluents;

    void compute_sink_fluents();
    std::vector<std::vector<bool> > is_sink;

    std::vector<std::vector<std::pair<int, int> > > invariant_exactly_one;

    std::vector<bool> satisfied_invariants;
    std::vector<std::vector<int> > potential_invariants;

    std::vector<std::vector<std::vector<bool> > > satisfied_invariants_fluent;
    std::vector<std::vector<std::vector<std::vector<int> > > > potential_invariants_fluent;

    bool check_invariants(std::pair<int, int> fluent);
    void remove_from_invariants(std::pair<int, int> fluent);

public:
    StateSampler();
    std::vector<std::pair<int, int> > sample_random_state();
};

#endif
