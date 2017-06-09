#ifndef ADP_HEURISTIC_H
#define ADP_HEURISTIC_H

#include "adp_additive_heuristic.h"

#include <vector>

/*
  TODO: In a better world, this should not derive from
        AdditiveHeuristic. Rather, the common parts should be
        implemented in a common base class. That refactoring could be
        made at the same time at which we also unify this with the
        other relaxation heuristics and the additional FF heuristic
        implementation in the landmark code.
*/


class ADPHeuristic : public ADPAdditiveHeuristic {
    // Relaxed plans are represented as a set of operators implemented
    // as a bit vector.
    typedef std::vector<bool> RelaxedPlan;
    RelaxedPlan relaxed_plan;
    void mark_preferred_operators_and_relaxed_plan(
        const State &state, ADP_Proposition *goal);
    void mark_preferred_operators_and_relaxed_plan_by_agent(
        const State &state, ADP_Proposition *goal, int agent);
protected:
    virtual void initialize();
    virtual int compute_heuristic(const GlobalState &global_state);
public:
    ADPHeuristic(const Options &options);
    ~ADPHeuristic();
};

#endif
