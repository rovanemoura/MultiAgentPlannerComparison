#ifndef ADP_ADDITIVE_HEURISTIC_H
#define ADP_ADDITIVE_HEURISTIC_H

#include "priority_queue.h"
#include "adp_relaxation_heuristic.h"

#include <cassert>

class State;


class ADPAdditiveHeuristic : public ADPRelaxationHeuristic {
    /* Costs larger than MAX_COST_VALUE are clamped to max_value. The
       precise value (100M) is a bit of a hack, since other parts of
       the code don't reliably check against overflow as of this
       writing. With a value of 100M, we want to ensure that even
       weighted A* with a weight of 10 will have f values comfortably
       below the signed 32-bit int upper bound.
     */
    static const int MAX_COST_VALUE = 100000000;

    AdaptiveQueue<ADP_Proposition *> queue;
    bool did_write_overflow_warning;

    void setup_exploration_queue();
    void setup_exploration_queue_state(const State &state);
    void relaxed_exploration();
    void mark_preferred_operators(const State &state, ADP_Proposition *goal);

    void setup_exploration_queue_state_for_agent(const State &state, int agent);


    void enqueue_if_necessary(ADP_Proposition *prop, int cost, ADP_UnaryOperator *op) {
        assert(cost >= 0);
        if (prop->cost == -1 || prop->cost > cost) {
            prop->cost = cost;
            prop->reached_by = op;
            queue.push(cost, prop);
        }
        assert(prop->cost != -1 && prop->cost <= cost);
    }

    void enqueue_if_necessary_for_agent(ADP_Proposition *prop, int cost, ADP_UnaryOperator *op, int agent, int layer) {
       
        assert(cost >= 0);
        if(prop->agent_id == agent){
            if (prop->cost == -1 || (prop->cost > cost && prop->layer >= layer)) {
                prop->cost = cost;
                prop->agent_reached_by[agent] = op;
                prop->reached_by = op;
                prop->layer = layer;
                queue.push(cost, prop);
            }
            assert(prop->cost != -1 && prop->cost <= cost);
        }
        else if(prop->agent_id > -1){//from another agent - should only be added in initial state.
            prop->cost = cost;
            queue.push(cost, prop);
        }
        else {//prop->agent_id must be -1 therefore public proposition
            if (prop->cost_by_agent[agent] == -1 || (prop->cost_by_agent[agent] > cost && prop->layer >= layer)){
                prop->cost_by_agent[agent] = cost;
                if(prop->cost == -1 || prop->cost > cost){
                    prop->cost = cost;
                    if(cost > 0){
                        prop->best_agent = agent;
                        prop->reached_by = op;
                        prop->layer = layer;
                    }
                }
                prop->agent_reached_by[agent] = op;
                queue.push(cost, prop);
            }
        }
    }    

    void increase_cost(int &cost, int amount) {
        assert(cost >= 0);
        assert(amount >= 0);
        cost += amount;
        if (cost > MAX_COST_VALUE) {
            write_overflow_warning();
            cost = MAX_COST_VALUE;
        }
    }

    void write_overflow_warning();
protected:
    int goal_left_value;
    bool switch_to_single_agent;
    int times_redistributed;
    int max_layer; //first layer is layer 0
    int current_layer;
    bool layer_solved;
    std::vector<std::vector<ADP_Proposition *> > agent_subgoals;
    int current_goal_agent;
    std::vector<ADP_Proposition *> current_goals;

    std::vector<std::vector<std::pair<int, int> > > explored_state;  //cost or -1 then agent

    virtual void initialize();
    virtual int compute_heuristic(const GlobalState &global_state);

    // Common part of h^add and h^ff computation.
    int compute_add_and_ff(const State &state);

    //Multiagent Planning 
    int compute_add_and_ff_agent(const State &state);
    void reset_public();
    bool coordination_point_calculation(const State &state);
    void relaxed_exploration_for_agent(int agent, int layer);
    void full_exploration_for_agent(int agent, int layer);
    bool distribute_goals_current_layer(int current_layer);
    void explore_next_layer(int layer);
    void setup_exploration_queue_state_for_agent_based_on_previous_explore(int agent);
    void full_exploration_for_agent_in_subsequent_layer(int agent, int layer);
    void extract_one_layer_relaxed_plan(ADP_Proposition *goal, int extract_agent);
    void get_next_goal_set();
    void calculate_goals_left_value();


public:
    ADPAdditiveHeuristic(const Options &options);
    ~ADPAdditiveHeuristic();
};

#endif
