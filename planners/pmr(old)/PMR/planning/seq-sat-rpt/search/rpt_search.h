#ifndef RPT_SEARCH_H
#define RPT_SEARCH_H

#include "search_engine.h"
#include "globals.h"
#include "option_parser.h"
#include "state_sampler.h"
#include "ff_heuristic.h"
#include <sstream>
#include <string>

#include <vector>

typedef std::vector<const Operator *> Plan;

class Options;

class RPTNode {
protected:
    enum {DEAD_END = -1}; // same as in heuristic
    typedef std::vector<bool> RelaxedPlan;
    State *state;
    RPTNode *previous; 
    Plan* plan_to_previous;
    
    // cached best supporters to compute ff_heuristic more quickly, if first=false then the proposition is not reachable from the RPT node
    std::vector<vector <pair<bool, const UnaryOperator *> > > cached_supporters;
    
    // get a relaxed plan from the cached best supporters
    bool collect_cached_plan(pair<int, int>, RelaxedPlan &relaxed_plan);
    
public:
    
    int heuristic;  // estimated distance to the original goal

    int ind;    //Reuse index

    RPTNode(State* state, RPTNode* previous, Plan* plan_to_previous, FFHeuristic* ff);
    int compute_cached_ff_heuristic(vector<pair<int, int> > goal_propositions);
    
    struct Comp {
        bool operator()(RPTNode* a, RPTNode* b){ return a->heuristic > b->heuristic; }
    };
    
    inline State* get_state(){ return state;}
    
    inline Plan* get_plan(){ return plan_to_previous;}
    
    inline RPTNode* get_previous(){ return previous;}
};


class RPTSearch : public SearchEngine {
    SearchEngine *local_search;

    const vector<ParseTree> engine_configs;

    vector<const Operator *> input_plan;

    // Maximum number of expanded nodes in every local search
    int nodes;

    // Probability of sampling instead of expanding towards the goal
    float p;

    //Probability of reuse
    float r;
   
   //previous plan for reuse
    std::string planr;    

    // Total expanded nodes by the different "local searches"
    int expanded_nodes;


    SearchEngine *get_search_engine(int engine_config_start_index);

    StateSampler sampler;

    virtual void initialize();
    virtual int step();

    // "closed list" of the nodes
    std::vector<RPTNode*> all_nodes;

    // to ensure that nodes are expanded towards the goal only once we have an additional list
    // this list is ordered (nodes closer to the goal first for efficiency reasons)
    priority_queue<RPTNode*, vector<RPTNode*>, RPTNode::Comp> unexpanded_nodes;
    priority_queue<RPTNode*, vector<RPTNode*>, RPTNode::Comp> unreutilized_nodes;
    // Heuristic - TODO: I should adapt this to used the cached version and not the current modified forward ff
    FFHeuristic* ff;

public:
    RPTSearch(const Options &opts);
    virtual ~RPTSearch();
    void statistics() const;
    SearchEngine* get_engine() {return local_search;}

    const State* last_state;
};

#endif
