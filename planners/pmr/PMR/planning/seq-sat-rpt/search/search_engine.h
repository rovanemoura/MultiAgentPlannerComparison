#ifndef SEARCH_ENGINE_H
#define SEARCH_ENGINE_H

#include <vector>

class Heuristic;
class OptionParser;
class Options;

#include "operator.h"
#include "search_space.h"
#include "search_progress.h"
#include "operator_cost.h"

class SearchEngine {
public:
    typedef std::vector<const Operator *> Plan;
private:
    bool solved;
    Plan plan;
protected:
    SearchSpace search_space;
    SearchProgress search_progress;
    int bound;
    OperatorCost cost_type;

    enum {FAILED, SOLVED, IN_PROGRESS};
    virtual int step() = 0;

    void set_plan(const Plan &plan);
    bool check_goal_and_set_plan(const State &state);
    int get_adjusted_cost(const Operator &op) const;

    int node_limit; // Vidal

    // Vidal
    virtual void reset(State, vector<pair<int, int> > new_goals){
        solved = false;
        g_goal = new_goals;
        search_space.clear();
        search_progress.reset();
    }

public:
    SearchEngine(const Options &opts);
    virtual ~SearchEngine();
    virtual void statistics() const;
    virtual void heuristic_statistics() const {}
    virtual void save_plan_if_necessary() const;
    bool found_solution() const;
    const Plan &get_plan() const;
    void search();
    SearchProgress get_search_progress() const {return search_progress; }
    void set_bound(int b) {bound = b; }
    int get_bound() {return bound; }
    static void add_options_to_parser(OptionParser &parser);

    virtual void initialize(){} // Vidal: made public

    inline void set_limit_nodes(int node_limit_){ node_limit = node_limit_;} // Vidal;

    // Vidal: search with limit and custom initial state and goals
    virtual std::pair<State*, Plan> search(State, std::vector<std::pair<int, int> >) {
        return(make_pair<State*, Plan>(NULL,Plan()));
    }
};

#endif
