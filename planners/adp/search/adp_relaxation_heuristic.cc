#include "adp_relaxation_heuristic.h"

#include "global_operator.h"
#include "global_state.h"
#include "globals.h"
#include "task_proxy.h"
#include "utilities.h"
#include "utilities_hash.h"
#include "causal_graph.h"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <unordered_map>
#include <vector>

using namespace std;

// construction and destruction
ADPRelaxationHeuristic::ADPRelaxationHeuristic(const Options &opts)
    : Heuristic(opts) {
}

ADPRelaxationHeuristic::~ADPRelaxationHeuristic() {
}

bool ADPRelaxationHeuristic::dead_ends_are_reliable() const {
    return !has_axioms();
}

// initialization
void ADPRelaxationHeuristic::initialize() {
    cout << "Calculating Agents..." << endl;
    split_into_agents();

    if(no_of_agents > 1){
        bool refining = true;
        while(refining){ //Loop until no longer has an effect in practice this almost never happens twice
            int prev_agents = no_of_agents;
            cout << "Extending agent variables..." << endl;
            for(int agent = 0; agent < no_of_agents; agent++){
                int vars = agent_vars[agent].size();
                for(int i = 0; i < vars; i++){
                    extend_agent_vars_full_graph(agent, agent_vars[agent][i]);
                }
            }
                   
            //After extending we should collect together all those that share variables
            cout << "Combining..." << endl;
            bool combining = true;
            while(combining){
                int is_agents = -1;
                int is_agents2 = -1;
                for(int i = 0; i < g_variable_name.size(); i++){
                    if(is_agents2 != -1)
                        break;
                    is_agents = -1;
                    is_agents2 = -1;
                    for(int agent = 0; agent < no_of_agents; agent++){
                        if(is_agents2 != -1)
                            break;
                        for(int agent_var = 0; agent_var < agent_vars[agent].size(); agent_var++){
                            if(agent_vars[agent][agent_var] == i){
                                if(is_agents == -1){
                                    is_agents = agent;
                                    break;
                                }
                                else{
                                    is_agents2 = agent;
                                    break;
                                }
                            }
                        }
                    }
                }
                if(is_agents == -1 || is_agents2 == -1){
                    combining = false;
                }
                else{
                    for(int i = 0; i < agent_vars[is_agents].size(); i++){
                        if(find(agent_vars[is_agents2].begin(), agent_vars[is_agents2].end(), agent_vars[is_agents][i]) == agent_vars[is_agents2].end()){
                            agent_vars[is_agents2].push_back(agent_vars[is_agents][i]);
                        }
                    }
                    agent_vars.erase(agent_vars.begin() + is_agents);
                    no_of_agents = agent_vars.size();
                }
            }
            
            cout << "Refining agents by joint action space..." << endl;
            
            vector<string> op_names;
            vector<vector < OperatorProxy> > op_numbers;
            for (int i = 0; i < task_proxy.get_operators().size(); i++){
                string new_name = task_proxy.get_operators()[i].get_name();
                new_name = new_name.substr(0, new_name.find(' '));             
                if(find(op_names.begin(), op_names.end(), new_name) == op_names.end()){//ie a new name
                    op_names.push_back(new_name);
                }
            }
            op_numbers.resize(op_names.size());
            for (int i = 0; i < task_proxy.get_operators().size(); i++){
                string new_name = task_proxy.get_operators()[i].get_name();
                new_name = new_name.substr(0, new_name.find(' ')); 
                vector<string>::iterator it = find(op_names.begin(), op_names.end(), new_name);
                int val = op_names.end()-it;
                op_numbers[val-1].push_back(task_proxy.get_operators()[i]);        
            }  
            for(int i = 0; i < op_numbers.size(); i++){
                check_operator_for_jointness(op_numbers[i]);
            }
            cout << prev_agents << "->" << no_of_agents << " agents left" << endl;
            
            if(no_of_agents < 2 || prev_agents == no_of_agents){
                refining = false;
            }

        }

        cout << agent_vars.size() << " agents left!" << endl;
    }
    else{
        cout << "No agents found (this will be solved as if 1 agent)." << endl;
    }

    if(no_of_agents > 1){

        //Get public variables (all variables that don't belong to an agent).
        for(int i = 0; i < g_variable_name.size(); i++){
            bool global_v = true;
            for(int agent = 0; agent < agent_vars.size(); agent++){
                if(find(agent_vars[agent].begin(), agent_vars[agent].end(), i) != agent_vars[agent].end()){
                    global_v = false;
                    break;
                }
            }
            if(global_v){
                public_vars.push_back(i);
            }
        }   
        
        // Report decomposition results
        // if (no_of_agents > 1){  
        //     cout << no_of_agents << " agents found!" << endl;
        //     for(int i = 0; i < no_of_agents; i++){
        //         cout << "Agent" << i << ":" << endl;
        //         for(int j = 0; j < agent_vars[i].size(); j++){
        //             cout << " ->" << g_fact_names[agent_vars[i][j]][0] << endl;                
        //         }
        //     }
        //     cout << "With " << public_vars.size() << " public" << endl;
        // }
        
        // Collect agent variables to associated_agent_by_var.
        for(int i = 0; i < task_proxy.get_variables().size(); i++){
            bool agent_found = false;
            for(int agent = 0; agent < no_of_agents; agent++){
                if(find(agent_vars[agent].begin(), agent_vars[agent].end(), i) != agent_vars[agent].end()){
                    associated_agent_by_var.push_back(agent);
                    agent_found = true;
                }
            }
            if(!agent_found)
                associated_agent_by_var.push_back(-1);
        }
        assert(associated_agent_by_var.size() == task_proxy.get_variables().size());
        
        cout << "Building propositions..." << endl;    
        // Build Proposition_ADPs - each agent has associated agent or -1 for public.
        agent_propositions.resize(no_of_agents);
        int prop_id = 0;
        VariablesProxy variables = task_proxy.get_variables();
        propositions.resize(variables.size());
        for (FactProxy fact : variables.get_facts()) {
            propositions[fact.get_variable().get_id()].push_back(ADP_Proposition(prop_id++, associated_agent_by_var[fact.get_variable().get_id()], no_of_agents));
        }
        
        //Creating agent_prop and public_prop sets.
        for(int i = 0; i < propositions.size(); i++){
            for(int j = 0; j < propositions[i].size(); j++){
                if(associated_agent_by_var[i] != -1){
                    agent_propositions[associated_agent_by_var[i]].push_back(&propositions[i][j]);
                }
                else{
                    public_propositions.push_back(&propositions[i][j]);
                }
            }
        }
        
        // Build goal propositions.
        for (FactProxy goal : task_proxy.get_goals()) {
            ADP_Proposition *prop = get_proposition(goal);
            prop->is_goal = true;
            goal_propositions.push_back(prop);
        }
        
        // Build unary operators for operators and axioms.
        int op_no = 0;
        for (OperatorProxy op : task_proxy.get_operators())
            build_unary_operators(op, op_no++);
        for (OperatorProxy axiom : task_proxy.get_axioms())
            build_unary_operators(axiom, -1);

        // Simplify unary operators.
        simplify();

        // Cross-reference unary operators.
        for (size_t i = 0; i < unary_operators.size(); ++i) {
            ADP_UnaryOperator *op = &unary_operators[i];
            for (size_t j = 0; j < op->precondition.size(); ++j)
                op->precondition[j]->precondition_of.push_back(op);
        }

        cout << "Creating agent operator sets..." << endl;
        //Create pointers to agent operator sets. 
        agent_operators.resize(no_of_agents);
        agent_joint_operators.resize(no_of_agents);
        // int internal = 0;
        // int partial_internal = 0;
        // int not_influenced_or_influencing = 0;
        // int influenced_only = 0;
        // int influencing_only = 0;
        // int influenced_and_influencing = 0;
        for (int i = 0; i < unary_operators.size(); i++){
            ADP_UnaryOperator *op = &unary_operators[i];
            std::set< int> used_by;
            bool is_influenced = false;
            bool is_influencing = false;
            for (int j = 0; j < op->precondition.size(); j++){
                if(op->precondition[j]->agent_id != -1)
                    used_by.insert(op->precondition[j]->agent_id);
                else{
                    is_influenced = true;
                    op->is_influenced = true;
                }
            }
            if(used_by.size() == 0){
                //This is a public action
                op->agent_id = -1;
                op->agent_cost.resize(no_of_agents);
                public_operators.push_back(op);
                is_influencing = true;
                op->is_influencing = true;
            }
            else if(used_by.size() == 1){
                //This action is for one agent only.
                if(op->effect->agent_id == *used_by.begin()){
                    // internal++;
                }
                else{
                    if(op->effect->agent_id == -1){
                        // partial_internal++;
                        op->is_influencing = true;
                        is_influencing = true;
                    }
                    else{
                        cout << "This action " << endl;
                        g_operators[op->operator_no].dump();
                        cout << "Is internal but effects another agent!" << endl;
                        cout << "This should be impossible!" << endl;
                        cout << "The agent decomposition algorithm probably has a bug" << endl;
                        exit(1);
                    }
                }
                op->agent_id = *used_by.begin();
                agent_operators[*used_by.begin()].push_back(op);
            }
            else{
                //This is a joint operator.
                op->agent_id = -2;
                op->agent_cost.resize(no_of_agents);//Note not all agents can do this action though.
                set<int>::iterator it;
                for(it = used_by.begin(); it != used_by.end(); it++){
                    agent_joint_operators[*it].push_back(op);
                }
                // if(op->effect->agent_id == -1){
                //     is_influencing = true;
                //     op->is_influencing = true;
                // }
                
            }
            // if(!is_influenced && !is_influencing){
            //     ++not_influenced_or_influencing;
            // }
            // else if(is_influenced && !is_influencing){
            //     ++influenced_only;
            // }
            // else if(!is_influenced && is_influencing){
            //     ++influencing_only;
            //     g_operators[op->operator_no].set_influencing(true);
            // }
            // else{
            //     ++influenced_and_influencing;
            //     g_operators[op->operator_no].set_influencing(true);
            // }
        }
        
        //Output action decomposition.
        // int internal_ops = 0;
        // int joint_ops = 0;
        // int internal_variables = 0;
        // for(int i=0; i < no_of_agents; i++){
        //     internal_variables += agent_vars[i].size();
        //     internal_ops += agent_operators[i].size();
        //     joint_ops += agent_joint_operators[i].size();
        // }
        // cout << no_of_agents << " agents" << endl;
        // cout << internal_variables << " agent variables" << endl;
        // cout << public_vars.size() << " public variables" << endl;
        // cout << internal_ops << " total internal actions" << endl;
        // cout << "  " << not_influenced_or_influencing << " not influenced or influencing" << endl;
        // cout << "  " << influenced_only << " influenced only" << endl;
        // cout << "  " << influencing_only << " influencing only" << endl;
        // cout << "  " << influenced_and_influencing << " both influenced and influencing (" << influenced_and_influencing - public_operators.size() << ") without public" << endl;
        // cout << public_operators.size() << " total public actions" << endl;
        // cout << joint_ops << " total joint actions" << endl;
    }
    else{
        // Build propositions.
        int prop_id = 0;
        VariablesProxy variables = task_proxy.get_variables();
        propositions.resize(variables.size());
        for (FactProxy fact : variables.get_facts()) {
            propositions[fact.get_variable().get_id()].push_back(ADP_Proposition(prop_id++));
        }

        // Build goal propositions.
        for (FactProxy goal : task_proxy.get_goals()) {
            ADP_Proposition *prop = get_proposition(goal);
            prop->is_goal = true;
            goal_propositions.push_back(prop);
        }

        // Build unary operators for operators and axioms.
        int op_no = 0;
        for (OperatorProxy op : task_proxy.get_operators())
            build_unary_operators(op, op_no++);
        for (OperatorProxy axiom : task_proxy.get_axioms())
            build_unary_operators(axiom, -1);

        // Simplify unary operators.
        simplify();

        // Cross-reference unary operators.
        for (size_t i = 0; i < unary_operators.size(); ++i) {
            ADP_UnaryOperator *op = &unary_operators[i];
            for (size_t j = 0; j < op->precondition.size(); ++j)
                op->precondition[j]->precondition_of.push_back(op);
        }
    }

}

ADP_Proposition *ADPRelaxationHeuristic::get_proposition(const FactProxy &fact) {
    int var = fact.get_variable().get_id();
    int value = fact.get_value();
    assert(in_bounds(var, propositions));
    assert(in_bounds(value, propositions[var]));
    return &propositions[var][value];
}

void ADPRelaxationHeuristic::build_unary_operators(const OperatorProxy &op, int op_no) {
    int base_cost = op.get_cost();
    vector<ADP_Proposition *> precondition_props;
    for (FactProxy precondition : op.get_preconditions()) {
        precondition_props.push_back(get_proposition(precondition));
    }
    for (EffectProxy effect : op.get_effects()) {
        ADP_Proposition *effect_prop = get_proposition(effect.get_fact());
        EffectConditionsProxy eff_conds = effect.get_conditions();
        for (FactProxy eff_cond : eff_conds) {
            precondition_props.push_back(get_proposition(eff_cond));
        }
        unary_operators.push_back(ADP_UnaryOperator(precondition_props, effect_prop, op_no, base_cost));
        precondition_props.erase(precondition_props.end() - eff_conds.size(), precondition_props.end());
    }
}

void ADPRelaxationHeuristic::split_into_agents(){
    no_of_agents = 0;

    for(int i = 0; i < task_proxy.get_variables().size(); ++i){
        //cout << task_proxy.get_variables()[i].get_id() << ":" << task_proxy.get_variables()[i].get_fact(0).get_name() << endl;
        if(g_causal_graph->get_predecessors(i).size() == 0 && g_causal_graph->get_successors(i).size() > 0){
            //cout << "ROOT NODE" << endl;
            agent_vars.push_back(vector<int> (1, i));
        }
        else{
            bool all_pre_two_way = true;
            for(int j = 0; j < g_causal_graph->get_predecessors(i).size(); j++){
                int testing_pre = g_causal_graph->get_predecessors(i)[j];
                //need testing_pre to have i as a successor
                if(find(g_causal_graph->get_successors(i).begin(), g_causal_graph->get_successors(i).end(), testing_pre) == g_causal_graph->get_successors(i).end()){
                    //We have a real predecessor
                    all_pre_two_way = false;
                    break;
                }
            }
            if(all_pre_two_way){
                //So all predecessors can be removed :: need to make sure there is at least one successor that isn't.
                bool all_post_two_way = true;
                for(int j = 0; j < g_causal_graph->get_successors(i).size(); j++){
                    int testing_suc = g_causal_graph->get_successors(i)[j];
                    //need testing_pre to have i as a successor
                    if(find(g_causal_graph->get_predecessors(i).begin(), g_causal_graph->get_predecessors(i).end(), testing_suc) == g_causal_graph->get_predecessors(i).end()){
                        //We have a real successor
                        all_post_two_way = false;
                        break;
                    }
                }
                if(!all_post_two_way){
                    //cout << "ROOT NODE IGNORING TWO-WAY CYCLES" << endl;
                    agent_vars.push_back(vector<int> (1,i));
                }
            }
        }
    }

    no_of_agents = agent_vars.size();
}

void ADPRelaxationHeuristic::extend_agent_vars_full_graph(int agent, int var_to_expand){    
    const vector<int> &succs = g_causal_graph->get_successors(var_to_expand);

    for(int i = 0; i < succs.size(); i++){
        const vector<int> succs_pres = g_causal_graph->get_predecessors(succs[i]);
        if(succs_pres.size() > 1){
            bool valid = true;
            //if there exists a predecessor that is not in the agent set then it's false.
            for(int j = 0; j < succs_pres.size(); j++){
                if(find(agent_vars[agent].begin(), agent_vars[agent].end(), succs_pres[j]) == agent_vars[agent].end()){
                    valid = false;
                    break;
                }
            }
            if(valid){
                //If not already there than add as an extension
                if(find(agent_vars[agent].begin(), agent_vars[agent].end(), succs[i]) == agent_vars[agent].end()){
                    agent_vars[agent].push_back(succs[i]);
                    extend_agent_vars_full_graph(agent, succs[i]);
                }
                
            }
        }
        else{
            if(find(agent_vars[agent].begin(), agent_vars[agent].end(), succs[i]) == agent_vars[agent].end()){
                agent_vars[agent].push_back(succs[i]);
                extend_agent_vars_full_graph(agent, succs[i]);
            }
        }
    }
}

void ADPRelaxationHeuristic::check_operator_for_jointness(vector<OperatorProxy> ops) {
    std::vector<std::vector<int> > old_agent_vars;
    for(int i = 0; i < agent_vars.size(); i++){
        old_agent_vars.push_back(agent_vars[i]);
    }    
    
    set<int> relevant_agents_by_action;
    for(int i = 0; i < ops.size(); i++){
        relevant_agents_by_action.clear();
        const OperatorProxy op = ops[i];
        
        std::vector<int> uses_var(task_proxy.get_variables().size());
        for(int i=0; i < uses_var.size(); i++){
            uses_var.at(i) = 0;
        }

        for(int j = 0; j < op.get_preconditions().size(); j++){
            uses_var.at(op.get_preconditions()[j].get_variable().get_id()) = 1;
        }
        for(int j = 0; j < op.get_effects().size(); ++j){
            //I'm ignoreing EffectConditionsProxy because I don't know what it does. I'm hoping it's just for domains with conditional effects
            if(op.get_effects()[j].get_conditions().size() > 0){
                cout << "ERROR Probably shouldn't ignore EffectConditionsProxy" << endl;
                exit(1);
            }
            uses_var.at(op.get_effects()[j].get_fact().get_variable().get_id()) = 1;
        }

        // const PreconditionsProxy &prevail = op.get_preconditions();
        // const EffectsProxy &pre_post = op.get_effects();
        
        
        // for (int i = 0; i < prevail.size(); i++){
        //     uses_var.at(prevail[i].var) =  1;
        // }
        // for (int i = 0; i < pre_post.size(); i++){
        //     uses_var.at(pre_post[i].var) = 1;
        // }
 
        for(int agent = 0; agent < no_of_agents; agent++)
            for(int j = 0; j < agent_vars[agent].size(); j++)
                if(uses_var[agent_vars[agent][j]] == 1)
                    relevant_agents_by_action.insert(agent);
        
        if(relevant_agents_by_action.size() > 1){//Only update if it is actually a joint operator.
            set<int>::iterator it = relevant_agents_by_action.end();
            for(it--; it != relevant_agents_by_action.begin(); it--){
                for(int i = 0; i < agent_vars[*it].size(); i++){
                    agent_vars[*relevant_agents_by_action.begin()].push_back(agent_vars[*it][i]);
                }
                agent_vars.erase(agent_vars.begin() + *it);
            }
        }
        no_of_agents = agent_vars.size();
    }
    
    // if(agent_vars.size() < 2){
    //     cout << "IGNORING JOINT CONDITION!!!!!!!!!!!!!!" << endl;
    //     agent_vars = old_agent_vars;
    //     no_of_agents = agent_vars.size();
    // }
    
}

void ADPRelaxationHeuristic::simplify() {
    // Remove duplicate or dominated unary operators.

    /*
      Algorithm: Put all unary operators into an unordered_map
      (key: condition and effect; value: index in operator vector.
      This gets rid of operators with identical conditions.

      Then go through the unordered_map, checking for each element if
      none of the possible dominators are part of the unordered_map.
      Put the element into the new operator vector iff this is the case.

      In both loops, be careful to ensure that a higher-cost operator
      never dominates a lower-cost operator.
    */


    cout << "Simplifying " << unary_operators.size() << " unary operators..." << flush;

    typedef pair<vector<ADP_Proposition *>, ADP_Proposition *> HashKey;
    typedef unordered_map<HashKey, int> HashMap;
    HashMap unary_operator_index;
    unary_operator_index.reserve(unary_operators.size());

    for (size_t i = 0; i < unary_operators.size(); ++i) {
        ADP_UnaryOperator &op = unary_operators[i];
        sort(op.precondition.begin(), op.precondition.end(),
             [] (const ADP_Proposition * p1, const ADP_Proposition * p2) {
                 return p1->id < p2->id;
             }
             );
        HashKey key(op.precondition, op.effect);
        pair<HashMap::iterator, bool> inserted = unary_operator_index.insert(
            make_pair(key, i));
        if (!inserted.second) {
            // We already had an element with this key; check its cost.
            HashMap::iterator iter = inserted.first;
            int old_op_no = iter->second;
            int old_cost = unary_operators[old_op_no].base_cost;
            int new_cost = unary_operators[i].base_cost;
            if (new_cost < old_cost)
                iter->second = i;
            assert(unary_operators[unary_operator_index[key]].base_cost ==
                   min(old_cost, new_cost));
        }
    }

    vector<ADP_UnaryOperator> old_unary_operators;
    old_unary_operators.swap(unary_operators);

    for (HashMap::iterator it = unary_operator_index.begin();
         it != unary_operator_index.end(); ++it) {
        const HashKey &key = it->first;
        int unary_operator_no = it->second;
        int powerset_size = (1 << key.first.size()) - 1; // -1: only consider proper subsets
        bool match = false;
        if (powerset_size <= 31) { // HACK! Don't spend too much time here...
            for (int mask = 0; mask < powerset_size; ++mask) {
                HashKey dominating_key = make_pair(vector<ADP_Proposition *>(), key.second);
                for (size_t i = 0; i < key.first.size(); ++i)
                    if (mask & (1 << i))
                        dominating_key.first.push_back(key.first[i]);
                HashMap::iterator found = unary_operator_index.find(
                    dominating_key);
                if (found != unary_operator_index.end()) {
                    int my_cost = old_unary_operators[unary_operator_no].base_cost;
                    int dominator_op_no = found->second;
                    int dominator_cost = old_unary_operators[dominator_op_no].base_cost;
                    if (dominator_cost <= my_cost) {
                        match = true;
                        break;
                    }
                }
            }
        }
        if (!match)
            unary_operators.push_back(old_unary_operators[unary_operator_no]);
    }

    cout << " done! [" << unary_operators.size() << " unary operators]" << endl;
}
