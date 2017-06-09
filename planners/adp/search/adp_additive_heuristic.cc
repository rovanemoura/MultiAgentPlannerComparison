#include "adp_additive_heuristic.h"

#include "global_state.h"
#include "option_parser.h"
#include "plugin.h"
#include "task_tools.h"

#include <cassert>
#include <vector>
using namespace std;




// construction and destruction
ADPAdditiveHeuristic::ADPAdditiveHeuristic(const Options &opts)
    : ADPRelaxationHeuristic(opts),
      did_write_overflow_warning(false) {
}

ADPAdditiveHeuristic::~ADPAdditiveHeuristic() {
}

void ADPAdditiveHeuristic::write_overflow_warning() {
    if (!did_write_overflow_warning) {
        // TODO: Should have a planner-wide warning mechanism to handle
        // things like this.
        cout << "WARNING: overflow on h^add! Costs clamped to "
             << MAX_COST_VALUE << endl;
        cerr << "WARNING: overflow on h^add! Costs clamped to "
             << MAX_COST_VALUE << endl;
        did_write_overflow_warning = true;
    }
}

// initialization
void ADPAdditiveHeuristic::initialize() {
    cout << "Initializing additive heuristic..." << endl;
    ADPRelaxationHeuristic::initialize();

    //multiagent stuff
    if(no_of_agents > 1){
        max_layer = 0;
        current_goal_agent = 0;
        if(explored_state.size() == 0){
            explored_state.resize(task_proxy.get_variables().size());
            for(int var = 0; var < task_proxy.get_variables().size(); var++){
                explored_state[var].resize(task_proxy.get_variables()[var].get_domain_size());
            }
        }
        agent_subgoals.resize(no_of_agents);
        agent_goal_propositions.resize(no_of_agents);
        goal_left_value = 0;
    }
}

// heuristic computation
void ADPAdditiveHeuristic::setup_exploration_queue() {
    queue.clear();

    for (size_t var = 0; var < propositions.size(); ++var) {
        for (size_t value = 0; value < propositions[var].size(); ++value) {
            ADP_Proposition &prop = propositions[var][value];
            prop.cost = -1;
            prop.marked = false;
        }
    }

    // Deal with operators and axioms without preconditions.
    for (size_t i = 0; i < unary_operators.size(); ++i) {
        ADP_UnaryOperator &op = unary_operators[i];
        op.unsatisfied_preconditions = op.precondition.size();
        op.cost = op.base_cost; // will be increased by precondition costs

        if (op.unsatisfied_preconditions == 0)
            enqueue_if_necessary(op.effect, op.base_cost, &op);
    }
}

void ADPAdditiveHeuristic::setup_exploration_queue_state(const State &state) {
    for (FactProxy fact : state) {
        ADP_Proposition *init_prop = get_proposition(fact);
        enqueue_if_necessary(init_prop, 0, 0);
    }
}

void ADPAdditiveHeuristic::setup_exploration_queue_state_for_agent(const State &state, int agent) {  
    queue.clear();

    //Reset propositions
    for(int i = 0; i < agent_propositions[agent].size(); i++){  
        ADP_Proposition* prop = agent_propositions[agent][i];
        prop->cost = -1;
        prop->marked = false;
        prop->layer = -1;
    }

    for(int i = 0; i < public_propositions.size(); i++){
        ADP_Proposition* prop = public_propositions[i];
        prop->cost_by_agent[agent] = -1;
        //prop->marked = false;
    }
    


    //Reset operators.
    for(int i = 0; i < agent_operators[agent].size(); i++){
        ADP_UnaryOperator* op = agent_operators[agent][i];
        op->unsatisfied_preconditions = op->precondition.size();
        op->cost = op->base_cost;
        
        if(op->unsatisfied_preconditions == 0){
            enqueue_if_necessary_for_agent(op->effect, op->base_cost, op, agent, 0);
        }
    }


    for(int i = 0; i < agent_joint_operators[agent].size(); i++){
        ADP_UnaryOperator* op = agent_joint_operators[agent][i];
        op->unsatisfied_preconditions = op->precondition.size();
        op->agent_cost[agent] = op->base_cost;
        if(op->unsatisfied_preconditions == 0){
            enqueue_if_necessary_for_agent(op->effect, op->base_cost, op, agent, 0);
            
        }
    }


    for(int i = 0; i < public_operators.size(); i++){
        ADP_UnaryOperator* op = public_operators[i];
        op->unsatisfied_preconditions = op->precondition.size();
        op->agent_cost[agent] = op->base_cost;
        if(op->unsatisfied_preconditions == 0){
            enqueue_if_necessary_for_agent(op->effect, op->base_cost, op, agent, 0);
        }
    }


    //For each agent_variable if that is true in state 0 then queue it.
    for (FactProxy fact : state) {
        for(int var = 0; var < agent_vars[agent].size(); ++var){
            if(fact.get_variable().get_id() == agent_vars[agent][var]){
                ADP_Proposition *init_prop = get_proposition(fact);
                enqueue_if_necessary_for_agent(init_prop, 0, 0, agent, 0);
            }
        }
        for(int var = 0; var < public_vars.size(); var++){
            if(fact.get_variable().get_id() == public_vars[var]){
                ADP_Proposition *init_prop = get_proposition(fact);
                enqueue_if_necessary_for_agent(init_prop, 0, 0, agent, 0);
            }
        }
    }


    //REMOVED THIS AND REPLACED WITH ABOVE CODE
    // for(int var = 0; var < agent_vars[agent].size(); var++){
    //     ADP_Proposition *init_prop = &propositions[agent_vars[agent][var]][state[agent_vars[agent][var]]];
    //     enqueue_if_necessary_for_agent(init_prop, 0, 0, agent, 0);
    // }
    // for(int var = 0; var < public_vars.size(); var++){
    //     ADP_Proposition *init_prop = &propositions[public_vars[var]][state[public_vars[var]]];
    //     enqueue_if_necessary_for_agent(init_prop, 0, 0, agent, 0);
    // }
}

void ADPAdditiveHeuristic::setup_exploration_queue_state_for_agent_based_on_previous_explore(int agent){
    queue.clear();
    
    for(int i = 0; i < agent_operators[agent].size(); i++){
        ADP_UnaryOperator* op = agent_operators[agent][i];
        op->unsatisfied_preconditions = op->precondition.size();
        op->cost = op->base_cost;
        if(op->unsatisfied_preconditions == 0)
            enqueue_if_necessary_for_agent(op->effect, op->base_cost, op, agent, 0);
    }
    for(int i = 0; i < agent_joint_operators[agent].size(); i++){
        ADP_UnaryOperator* op = agent_joint_operators[agent][i];
        op->unsatisfied_preconditions = op->precondition.size();
        op->agent_cost[agent] = op->base_cost;
        if(op->unsatisfied_preconditions == 0)
            enqueue_if_necessary_for_agent(op->effect, op->base_cost, op, agent, 0);
    }
    for(int i = 0; i < public_operators.size(); i++){
        ADP_UnaryOperator* op = public_operators[i];
        op->unsatisfied_preconditions = op->precondition.size();
        op->agent_cost[agent] = op->base_cost;
        if(op->unsatisfied_preconditions == 0)
            enqueue_if_necessary_for_agent(op->effect, op->base_cost, op, agent, 0);
    }
    
    //Makes use of explored_state
    for(int var = 0; var < explored_state.size(); var++){
        for(int val = 0; val < explored_state[var].size(); val++){
            if(explored_state[var][val].first > -1){
                if(propositions[var][val].agent_id == -1 && propositions[var][val].cost_by_agent[agent] > -1){
                    queue.push(propositions[var][val].cost_by_agent[agent], &propositions[var][val]);
                }
                else{
                    queue.push(propositions[var][val].cost, &propositions[var][val]);
                    
                }
            }
        }
    }
    
}



void ADPAdditiveHeuristic::relaxed_exploration() {
    int unsolved_goals = goal_propositions.size();
    while (!queue.empty()) {
        pair<int, ADP_Proposition *> top_pair = queue.pop();
        int distance = top_pair.first;
        ADP_Proposition *prop = top_pair.second;
        int prop_cost = prop->cost;
        assert(prop_cost >= 0);
        assert(prop_cost <= distance);
        if (prop_cost < distance)
            continue;
        if (prop->is_goal && --unsolved_goals == 0)
            return;
        const vector<ADP_UnaryOperator *> &triggered_operators =
            prop->precondition_of;
        for (size_t i = 0; i < triggered_operators.size(); ++i) {
            ADP_UnaryOperator *unary_op = triggered_operators[i];
            increase_cost(unary_op->cost, prop_cost);
            --unary_op->unsatisfied_preconditions;
            assert(unary_op->unsatisfied_preconditions >= 0);
            if (unary_op->unsatisfied_preconditions == 0)
                enqueue_if_necessary(unary_op->effect,
                                     unary_op->cost, unary_op);
        }
    }
}

void ADPAdditiveHeuristic::relaxed_exploration_for_agent(int agent, int layer) {
    int unsolved_goals = current_goals.size();
    
    while (!queue.empty()) {
        pair<int, ADP_Proposition *> top_pair = queue.pop();
        int distance = top_pair.first;
        ADP_Proposition *prop = top_pair.second;
        int prop_cost = prop->cost;
        if(prop->agent_id == -1)
            prop_cost = prop->cost_by_agent[agent];
        assert(prop_cost >= 0);
        assert(prop_cost <= distance);
        if (prop_cost < distance)
            continue;
        
        if (prop->is_goal && prop->goal_agent == agent && --unsolved_goals == 0)
            return;
        
        const vector<ADP_UnaryOperator *> &triggered_operators = prop ->precondition_of;
        for (int i = 0; i < triggered_operators.size(); i++) {            
            ADP_UnaryOperator *unary_op = triggered_operators[i];
            if(unary_op->agent_id == agent){
                increase_cost(unary_op->cost, prop_cost);
                unary_op->unsatisfied_preconditions--;
                assert(unary_op->unsatisfied_preconditions >= 0);
                if (unary_op->unsatisfied_preconditions == 0)
                    enqueue_if_necessary_for_agent(unary_op->effect,
                                                   unary_op->cost, unary_op, agent, layer);
            }
            else if(unary_op->agent_id < 0){
                increase_cost(unary_op->agent_cost[agent], prop_cost);
                unary_op->unsatisfied_preconditions--;
                if(unary_op->unsatisfied_preconditions == 0)
                    enqueue_if_necessary_for_agent(unary_op->effect,
                                                   unary_op->agent_cost[agent], unary_op, agent, layer);
                
            }
        }
    }
}

void ADPAdditiveHeuristic::full_exploration_for_agent(int agent, int layer){
    while (!queue.empty()) {
        pair<int, ADP_Proposition *> top_pair = queue.pop();
        int distance = top_pair.first;
        ADP_Proposition *prop = top_pair.second;
        int prop_cost = prop->cost;
        if(prop->agent_id == -1)
            prop_cost = prop->cost_by_agent[agent];
        
        assert(prop_cost >= 0);
        assert(prop_cost <= distance);
        
        if (prop_cost < distance)
            continue;
        
        const vector<ADP_UnaryOperator *> &triggered_operators =
        prop->precondition_of;
        for (int i = 0; i < triggered_operators.size(); i++) {            
            ADP_UnaryOperator *unary_op = triggered_operators[i];
            if(unary_op->agent_id == agent){
                increase_cost(unary_op->cost, prop_cost);
                unary_op->unsatisfied_preconditions--;
                assert(unary_op->unsatisfied_preconditions >= 0);
                if (unary_op->unsatisfied_preconditions == 0){
                    enqueue_if_necessary_for_agent(unary_op->effect,
                                                   unary_op->cost, unary_op, agent, layer);
                }
                
            }
            else if(unary_op->agent_id < 0){
                increase_cost(unary_op->agent_cost[agent], prop_cost);
                unary_op->unsatisfied_preconditions--;
                if(unary_op->unsatisfied_preconditions == 0){
                    enqueue_if_necessary_for_agent(unary_op->effect,
                                                   unary_op->agent_cost[agent], unary_op, agent, layer);
                }
                
            }
        }
    }
}

void ADPAdditiveHeuristic::full_exploration_for_agent_in_subsequent_layer(int agent, int layer){
    while (!queue.empty()) {
        pair<int, ADP_Proposition *> top_pair = queue.pop();
        //int distance = top_pair.first;
        ADP_Proposition *prop = top_pair.second;
        //cout << "Dealing with " << prop->id;
        int prop_cost = prop->cost;
        prop_cost++;
        
        assert(prop_cost >= 0);
        
        const vector<ADP_UnaryOperator *> &triggered_operators = prop->precondition_of;
        for (int i = 0; i < triggered_operators.size(); i++) {            
            ADP_UnaryOperator *unary_op = triggered_operators[i];
            if(unary_op->agent_id == agent){
                increase_cost(unary_op->cost, prop_cost);
                unary_op->unsatisfied_preconditions--;
                assert(unary_op->unsatisfied_preconditions >= 0);
                if (unary_op->unsatisfied_preconditions == 0){
                    enqueue_if_necessary_for_agent(unary_op->effect,
                                                   unary_op->cost, unary_op, agent, layer);
                }
                
            }
            else if(unary_op->agent_id < 0){
                increase_cost(unary_op->agent_cost[agent], prop_cost);
                unary_op->unsatisfied_preconditions--;
                if(unary_op->unsatisfied_preconditions == 0){
                    enqueue_if_necessary_for_agent(unary_op->effect,
                                                   unary_op->agent_cost[agent], unary_op, agent, layer);
                }
                
            }
        }
    }
}


void ADPAdditiveHeuristic::mark_preferred_operators(
    const State &state, ADP_Proposition *goal) {
    if (!goal->marked) { // Only consider each subgoal once.
        goal->marked = true;
        ADP_UnaryOperator *unary_op = goal->reached_by;
        if (unary_op) { // We have not yet chained back to a start node.
            for (size_t i = 0; i < unary_op->precondition.size(); ++i)
                mark_preferred_operators(state, unary_op->precondition[i]);
            int operator_no = unary_op->operator_no;
            if (unary_op->cost == unary_op->base_cost && operator_no != -1) {
                // Necessary condition for this being a preferred
                // operator, which we use as a quick test before the
                // more expensive applicability test.
                // If we had no 0-cost operators and axioms to worry
                // about, this would also be a sufficient condition.
                OperatorProxy op = task_proxy.get_operators()[operator_no];
                if (is_applicable(op, state))
                    set_preferred(op);
            }
        }
    }
}

void ADPAdditiveHeuristic::extract_one_layer_relaxed_plan(ADP_Proposition *goal, int extract_agent){

    
    if (!goal->marked) { // Only consider each subgoal once and only care if in layer 1...?
        goal->marked = true;
//        cout << "EXTRACTING" << g_fact_names[goal->var][goal->val] << "for " << extract_agent << " with agent_id " << goal->agent_id << " best agent " << goal->best_agent << " cost " << goal->cost << " and layer " << goal->layer << endl;
        
        ADP_UnaryOperator *unary_op = goal->reached_by;
        
        if (unary_op) { // We have not yet chained back to a start node.
            
            for (int i = 0; i < unary_op->precondition.size(); i++){
                
                if(unary_op->precondition[i]->agent_id == -1 && unary_op->precondition[i]->best_agent > -1){
                    //public action
                    if(unary_op->precondition[i]->best_agent != extract_agent){
                        if(unary_op->precondition[i]->layer == 0){
                        //    cout << "Adding subgoal " << unary_op->precondition[i]->id << " for agent " << unary_op->precondition[i]->best_agent << endl;
                            agent_subgoals[unary_op->precondition[i]->best_agent].push_back(unary_op->precondition[i]);
                        }
                        else{
                            extract_one_layer_relaxed_plan(unary_op->precondition[i], unary_op->precondition[i]->best_agent);
                        }
                        
                    }
                    else{
                        extract_one_layer_relaxed_plan(unary_op->precondition[i], extract_agent);
                    }

                }
                else if(unary_op->precondition[i]->agent_id > -1){
                    //internal action
                    if(unary_op->precondition[i]->agent_id == extract_agent){
                        extract_one_layer_relaxed_plan(unary_op->precondition[i], extract_agent);
                    }
                    else{
                        if(unary_op->precondition[i]->layer == 0){
                         //   cout << "Adding subgoal " << unary_op->precondition[i]->id << " for agent " << unary_op->precondition[i]->agent_id << endl;
                            agent_subgoals[unary_op->precondition[i]->agent_id].push_back(unary_op->precondition[i]);
                        }
                        else{
                            extract_one_layer_relaxed_plan(unary_op->precondition[i], unary_op->precondition[i]->agent_id);
                        }

                    }
                }               
            }
        }
    }
}

int ADPAdditiveHeuristic::compute_add_and_ff(const State &state) {
    setup_exploration_queue();
    setup_exploration_queue_state(state);
    relaxed_exploration();

    int total_cost = 0;
    for (size_t i = 0; i < goal_propositions.size(); ++i) {
        int prop_cost = goal_propositions[i]->cost;
        if (prop_cost == -1)
            return DEAD_END;
        increase_cost(total_cost, prop_cost);
    }
    return total_cost;
}

int ADPAdditiveHeuristic::compute_add_and_ff_agent(const State &state) {
    reset_public();
    setup_exploration_queue_state_for_agent(state, current_goal_agent);
    relaxed_exploration_for_agent(current_goal_agent, 0);
    
    int total_cost = 0;
    for(int i = 0; i < current_goals.size(); i++){
        if(current_goals[i]->agent_id == -1){
            if(current_goals[i]->cost_by_agent[current_goal_agent] > -1)
                total_cost += current_goals[i]->cost_by_agent[current_goal_agent];
            else
                return DEAD_END;
        }
        else{
            if(current_goals[i]->cost > -1)
                total_cost += current_goals[i]->cost;
            else
                return DEAD_END;
        }
        
    }
    return total_cost;
}

void ADPAdditiveHeuristic::reset_public(){
    for(int i = 0; i < no_of_agents; i++){
        agent_subgoals[i].clear();
        agent_goal_propositions[i].clear();
    }
    //This resets the common parts of the public propositions
    //These should not be reset between agents or layers!
    for(int i = 0; i < public_propositions.size(); i++){
        ADP_Proposition* prop = public_propositions[i];
       prop->cost = -1;
       prop->layer = -1;
       prop->best_agent = prop->agent_id;
       prop->marked = false;
    }
}

bool ADPAdditiveHeuristic::distribute_goals_current_layer(int current_layer){
    //Each goal is assigned to the agent with the lowest estimated cost.
    //Returns true if all goals are reachable.     
    
    bool all_reachable = true;
    for(int i = 0; i < goal_propositions.size(); i++){
        if(goal_propositions[i]->goal_agent == -1){
            if(goal_propositions[i]->agent_id > -1){
                if(goal_propositions[i]->cost > -1){
                    if(goal_propositions[i]->cost >= 0){
                        agent_goal_propositions[goal_propositions[i]->agent_id].push_back(goal_propositions[i]);
                        goal_propositions[i]->goal_agent = goal_propositions[i]->agent_id;
                        goal_propositions[i]->layer = current_layer;
                    }
                }
                else
                    all_reachable = false;
            }
            else{
                int best_cost = MAX_COST_VALUE;
                int best_agent = -1;
                for(int j = 0; j < no_of_agents; j++){
                    int test_cost = goal_propositions[i]->cost_by_agent[j];
                    if(test_cost > -1 && test_cost < best_cost){
                        best_cost = test_cost;
                        best_agent = j;
                    }
                }
                if(best_agent == -1){
                    all_reachable = false;
                }
                else{
                    if(best_cost >= 0){
                        agent_goal_propositions[best_agent].push_back(goal_propositions[i]);
                        goal_propositions[i]->goal_agent = best_agent;
                        goal_propositions[i]->layer = current_layer;
                        goal_propositions[i]->cost = best_cost;
                    }
                }
            }
        }
    }
    return all_reachable;
}


void ADPAdditiveHeuristic::explore_next_layer(int layer){
    //First collate the state - this is each reached proposition with the agent that best reached it
    for(int var = 0; var < g_variable_domain.size(); var++){
        for(int val = 0; val < g_variable_domain[var]; val++){
            explored_state[var][val].first = propositions[var][val].cost;
            explored_state[var][val].second = propositions[var][val].best_agent;
        }
    }   
    
    //cout << "--next layer--" << endl;
    for(int agent = 0; agent < no_of_agents; agent++){
        setup_exploration_queue_state_for_agent_based_on_previous_explore(agent);
        full_exploration_for_agent_in_subsequent_layer(agent, layer);
      //  output_goal_costs(agent);
    }
}

/*
    Sets all goal_agents back to -1
    reset_public - resets all (public) propositions
*/
bool ADPAdditiveHeuristic::coordination_point_calculation(const State &state){   
    //Returns false for dead end
    
    for(int i = 0; i < goal_propositions.size(); i++){
         goal_propositions[i]->goal_agent = -1;
    }

   
    int layer = 0;   
    reset_public(); //Resets the public propositions and their best cost/agents.
    
    for(int agent = 0; agent < no_of_agents; agent++){
        setup_exploration_queue_state_for_agent(state, agent);
        full_exploration_for_agent(agent, layer);
        //output_goal_costs(agent);   
    }
//     //Keep exploring each layer until all goals are reachable.
    while(!distribute_goals_current_layer(layer)){
        layer++;
        explore_next_layer(layer);
        if(layer > 10){    //HACK! should be until no longer adding stuff but 5 more than enough layers for normal domains.
            return false;
        }
    }

    if(max_layer < layer && layer < 10){
//        if(max_layer > 0){
//            return false;
//        }
        max_layer = layer;
        cout << "Max Layer = " << max_layer << endl;
    }
    
    if(layer > 0){
        //cout << "Finding subgoals" << endl;
        for(int i = 0; i < goal_propositions.size(); i++){
            if(goal_propositions[i]->layer > 0){
                //Adds subgoals found to the set of agent_subgoals
                extract_one_layer_relaxed_plan(goal_propositions[i], goal_propositions[i]->goal_agent);
            }
        }
    }
    
    get_next_goal_set();

    calculate_goals_left_value();
    
    return true;
}

void ADPAdditiveHeuristic::calculate_goals_left_value(){
    int not_found = 0;
    int goal_layers = 0;
    for(int i = 0; i < goal_propositions.size(); i++){
        if(goal_propositions[i]->cost != 0)
            not_found++;
        goal_layers += goal_propositions[i]->layer;
    }  
    goal_left_value = 1000*goal_layers + 100 * not_found;
}

void ADPAdditiveHeuristic::get_next_goal_set(){
    //Getting next goal set.
    
    current_goals.clear();
    int most_goals = 0;

    for(int agent = 0; agent < no_of_agents; agent++){
        int agent_goals = 0;
        for(int i = 0; i < agent_goal_propositions[agent].size(); i++){
            if(agent_goal_propositions[agent][i]->layer == 0 && agent_goal_propositions[agent][i]->cost > 0){
                agent_goals++;
            }
        }
        for(int i = 0; i < agent_subgoals[agent].size(); i++){
            if(agent_subgoals[agent][i]->layer == 0 && agent_subgoals[agent][i]->cost > 0){
                agent_goals += 10;
            }
        }
        if(agent_goals > most_goals){
            most_goals = agent_goals;
            current_goal_agent = agent;
        }
    }
    
    for(int i = 0; i < agent_subgoals[current_goal_agent].size(); i++){
        if(agent_subgoals[current_goal_agent][i]->layer == 0){// && agent_subgoals[current_goal_agent][i]->cost > 0){
            current_goals.push_back(agent_subgoals[current_goal_agent][i]);
        }
    }
    for(int i = 0; i < agent_goal_propositions[current_goal_agent].size(); i++){
        if(agent_goal_propositions[current_goal_agent][i]->layer == 0){// && agent_goal_propositions[current_goal_agent][i]->cost > 0){
            current_goals.push_back(agent_goal_propositions[current_goal_agent][i]);
        }
    }
}


int ADPAdditiveHeuristic::compute_heuristic(const GlobalState &global_state) {
    State state = convert_global_state(global_state);
    int h = compute_add_and_ff(state);
    if (h != DEAD_END) {
        for (size_t i = 0; i < goal_propositions.size(); ++i)
            mark_preferred_operators(state, goal_propositions[i]);
    }
    return h;
}

static Heuristic *_parse(OptionParser &parser) {
    parser.document_synopsis("Additive heuristic", "");
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
        return new ADPAdditiveHeuristic(opts);
}

static Plugin<Heuristic> _plugin("add", _parse);
