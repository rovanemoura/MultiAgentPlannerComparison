#ifndef LAZY_SEARCH_H
#define LAZY_SEARCH_H

#include <vector>

#include "open_lists/open_list.h"
#include "search_engine.h"
#include "state.h"
#include "scalar_evaluator.h"
#include "search_space.h"
#include "search_progress.h"

class Heuristic;
class Operator;
class Options;

typedef pair<state_var_t *, const Operator *> OpenListEntryLazy;

typedef std::vector<const Operator *> Plan;

class LazySearch : public SearchEngine {
protected:
    OpenList<OpenListEntryLazy> *open_list;

    // Search Behavior parameters
    bool reopen_closed_nodes; // whether to reopen closed nodes upon finding lower g paths
    enum {original, pref_first, shuffled} succ_mode;

    vector<Heuristic *> heuristics;
    vector<Heuristic *> preferred_operator_heuristics;
    vector<Heuristic *> estimate_heuristics;

    State current_state;
    state_var_t *current_predecessor_buffer;
    const Operator *current_operator;
    int current_g;
    int current_real_g;

    virtual void initialize();
    virtual int step();

    void generate_successors();
    int fetch_next_state();

    void reward_progress();

    void get_successor_operators(vector<const Operator *> &ops);

    // Vidal
    virtual void reset(State new_state, vector<pair<int, int> > new_goals){
        SearchEngine::reset(new_state, new_goals);
        current_state = new_state;
        current_predecessor_buffer = NULL;
        current_operator = NULL;
        current_g = 0;
        current_real_g = 0;
        open_list->clear();
    }
public:

    LazySearch(const Options &opts);
    virtual ~LazySearch();
    void set_pref_operator_heuristics(vector<Heuristic *> &heur);

    std::pair<State*, Plan> search(State new_state, vector<pair<int, int> > new_goals); // Vidal

    virtual void statistics() const;
};

#endif
