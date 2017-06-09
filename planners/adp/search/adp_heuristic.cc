#include "adp_heuristic.h"

#include "global_state.h"
#include "option_parser.h"
#include "plugin.h"
#include "task_tools.h"

#include <cassert>

using namespace std;

// construction and destruction
ADPHeuristic::ADPHeuristic(const Options &opts)
    : ADPAdditiveHeuristic(opts) {
}

ADPHeuristic::~ADPHeuristic() {
}

// initialization
void ADPHeuristic::initialize() {
    cout << "Initializing ADP heuristic..." << endl;
    ADPAdditiveHeuristic::initialize();
    relaxed_plan.resize(task_proxy.get_operators().size(), false);

    if(no_of_agents > 1)
        coordination_point_calculation(task_proxy.get_initial_state());
}

void ADPHeuristic::mark_preferred_operators_and_relaxed_plan(
    const State &state, ADP_Proposition *goal) {
    if (!goal->marked) { // Only consider each subgoal once.
        goal->marked = true;
        ADP_UnaryOperator *unary_op = goal->reached_by;
        if (unary_op) { // We have not yet chained back to a start node.
            for (size_t i = 0; i < unary_op->precondition.size(); ++i)
                mark_preferred_operators_and_relaxed_plan(
                    state, unary_op->precondition[i]);
            int operator_no = unary_op->operator_no;
            if (operator_no != -1) {
                // This is not an axiom.
                relaxed_plan[operator_no] = true;

                if (unary_op->cost == unary_op->base_cost) {
                    // This test is implied by the next but cheaper,
                    // so we perform it to save work.
                    // If we had no 0-cost operators and axioms to worry
                    // about, it would also imply applicability.
                    OperatorProxy op = task_proxy.get_operators()[operator_no];
                    if (is_applicable(op, state))
                        set_preferred(op);
                }
            }
        }
    }
}

void ADPHeuristic::mark_preferred_operators_and_relaxed_plan_by_agent(
        const State &state, ADP_Proposition *goal, int agent) {
    if (!goal->marked) { // Only consider each subgoal once.
        goal->marked = true;
        
        ADP_UnaryOperator *unary_op = goal->agent_reached_by[agent];
        
        if (unary_op) { // We have not yet chained back to a start node.
            for (int i = 0; i < unary_op->precondition.size(); i++)
                mark_preferred_operators_and_relaxed_plan_by_agent(
                                                                   state, unary_op->precondition[i], agent);
            int operator_no = unary_op->operator_no;
            if (operator_no != -1) {
                // This is not an axiom.
                relaxed_plan[operator_no] = true;
                if(unary_op->agent_id > -1){
                    if (unary_op->cost == unary_op->base_cost) {
                        // This test is implied by the next but cheaper,
                        // so we perform it to save work.
                        // If we had no 0-cost operators and axioms to worry
                        // about, it would also imply applicability.
                        const OperatorProxy op = task_proxy.get_operators()[operator_no];
                        if (is_applicable(op, state))
                            set_preferred(op);
                    }
                }
                else{
                    for(int agent = 0; agent < no_of_agents; agent++){
                        if (unary_op->agent_cost[agent] == unary_op->base_cost) {
                            // This test is implied by the next but cheaper,
                            // so we perform it to save work.
                            // If we had no 0-cost operators and axioms to worry
                            // about, it would also imply applicability.
                            const OperatorProxy op = task_proxy.get_operators()[operator_no];
                            if (is_applicable(op, state))
                                set_preferred(op);
                            break;
                        } 
                    }
                    
                }
            }
        }
    }
}

int ADPHeuristic::compute_heuristic(const GlobalState &global_state) {
    State state = convert_global_state(global_state);
    int h_ff = 0;

    //STANDARD FF (SINGLEAGENT PLANNING HEURISTIC)
    if(no_of_agents < 2){
        int h_add = compute_add_and_ff(state);
        if (h_add == DEAD_END)
            return h_add;

        // Collecting the relaxed plan also sets the preferred operators.
        for (size_t i = 0; i < goal_propositions.size(); ++i)
            mark_preferred_operators_and_relaxed_plan(state, goal_propositions[i]);

        for (size_t op_no = 0; op_no < relaxed_plan.size(); ++op_no) {
            if (relaxed_plan[op_no]) {
                relaxed_plan[op_no] = false; // Clean up for next computation.
                h_ff += task_proxy.get_operators()[op_no].get_cost();
            }
        }
    }
    //MULTIAGENT PLANNING HEURISTIC
    else{
        int h_add = compute_add_and_ff_agent(state); //Perform FF search for current agent and goals
        
        if(h_add == DEAD_END){ //If it's a dead end then do a new goal decomp return dead end
            if(!coordination_point_calculation(state))
                return DEAD_END;
            return 1000000;
        }
        
        if(h_add == 0){ //If at end of current search then start a new one.
            if(!coordination_point_calculation(state))
                return DEAD_END;        
            h_add = compute_add_and_ff_agent(state);        
        }

        for(int i = 0; i < current_goals.size(); i++){
            mark_preferred_operators_and_relaxed_plan_by_agent(state, current_goals[i], current_goal_agent);
        }
        
        for (size_t op_no = 0; op_no < relaxed_plan.size(); ++op_no) {
            if (relaxed_plan[op_no]) {
                relaxed_plan[op_no] = false; // Clean up for next computation.
                h_ff += task_proxy.get_operators()[op_no].get_cost();
            }
        }
        h_ff += goal_left_value;
    }

    return h_ff;


}


static Heuristic *_parse(OptionParser &parser) {
    parser.document_synopsis("FF heuristic", "See also Synergy.");
    parser.document_language_support("action costs", "supported");
    parser.document_language_support("conditional effects", "supported");
    parser.document_language_support(
        "axioms",
        "supported (in the sense that the planner won't complain -- "
        "handling of axioms might be very stupid "
        "and even render the heuristic unsafe)");
    parser.document_property("admissible", "no");
    parser.document_property("consistent", "no");
    parser.document_property("safe", "yes for tasks without axioms");
    parser.document_property("preferred operators", "yes");

    Heuristic::add_options_to_parser(parser);
    Options opts = parser.parse();
    if (parser.dry_run())
        return 0;
    else
        return new ADPHeuristic(opts);
}

static Plugin<Heuristic> _plugin("adp", _parse);
