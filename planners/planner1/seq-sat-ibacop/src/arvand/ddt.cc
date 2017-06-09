#include "ddt.h"
#include "globals.h"
#include "walker.h"
#include "successor_generator.h"
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <climits>
#include "math.h"



DDT::DDT() : current_state(*g_initial_state){
	max_r_id = 0;	
	walking_heuristic = 0;
	current_predecessor = 0;
	current_operator = 0;
}



DDT::~DDT() {
}


int DDT::step() {
	
	int id = get_c_id(current_state);
	//cout << "id: " << id << endl;
	if (id == UNKNOWN) {
 		heuristic->evaluate(current_state);
		evaluated_states ++;
		
		if (!heuristic->is_dead_end()) {
			int h = heuristic->get_heuristic();
			if(h < total_min){
				cout << "h: " << h << " [ " << evaluated_states  << " ]"<< endl;
				total_min = h;
			}
			contexts[current_c_id].update_h(h);

			if (check_goal(&current_state)){
				return SOLVED;
			}
			
			if(!is_exit_point(current_state, contexts[current_c_id].r_id)){
				std::map<State, DDTNode>::iterator it = memory.insert(make_pair(current_state, 
									DDTNode(current_state, current_predecessor, current_operator, contexts[current_c_id].r_id))).first;
				const State *parent_ptr = &(it->first); 
				generate_successors(parent_ptr);
			}else{
				new_context();
			}
		}
	} else {
		std::set<int>::iterator it = contexts[current_c_id].ids.find(id);
		if (it == contexts[current_c_id].ids.end()){
			if(!is_exit_point(current_state, contexts[current_c_id].r_id)){
				contexts[current_c_id].merge(contexts[id]);
			}
		}
	}
	fetch_next_state();
	step_counter ++;
	return IN_PROGRESS;
}



int DDT::get_c_id(State& state){
	
	std::map<State, DDTNode>::iterator it = memory.find(state);
	if(it == memory.end())
		return -1;
	return it->second.r_id;
}

bool DDT::is_exit_point(State& state, int r_id){
	Walker walker(false);
	return walker.is_exit_point(state, memory, r_id);
}

void DDT::new_context(){
	// cout << "new context: " << max_r_id << endl;
	std::map<State, DDTNode>::iterator it = memory.insert(make_pair(current_state, DDTNode(current_state, current_predecessor, current_operator, max_r_id++))).first;
	const State *parent_ptr = &(it->first);
	contexts.push_back(SearchContext(current_state, parent_ptr, heuristic->get_heuristic(), max_r_id - 1));
	//contexts[contexts.size() - 1].make_permanent();
}

int DDT::fetch_next_state() {
	int c_id = select_search_context();
	current_c_id = c_id;
	pair<const State*, const Operator*> next = contexts[current_c_id].remove_min();
	current_predecessor = next.first;
	current_operator = next.second;
	current_state = State(*current_predecessor, *current_operator);
	return IN_PROGRESS;
}

int DDT::select_search_context(){
	//cout << "selecting next context ..." << endl;
	if(step_counter < 50 && !contexts[current_c_id].is_empty())
		return current_c_id;
	return select_roulette_wheel();
}

int DDT::select_greedy(){
	assert(contexts.size() > 0);
	vector<int> unknowns;
	vector<int> top;
	float top_score = FLT_MAX;
	int count = 0;
	for (int i = 0; i < contexts.size(); ++i) {
		if(contexts[i].is_active()){
			count ++;
			float score = contexts[i].get_score();
			//cout << i << ": " << score << endl;
			if(score == SearchContext::UNKNOWN)
				unknowns.push_back(i);
			else if(score < top_score){
				top.clear();
				top.push_back(i);
				top_score = score;
			}else if(score == top_score){
				top.push_back(i);
			}
		}
	}
	//cout << "c#: " << count << endl;//" container size: " << contexts.size() <<  endl; 
	if(unknowns.size() > 0){
		int i = random() % unknowns.size();
		int index = unknowns[i];
		return index;
	}else if(top.size() > 0){
		int i = random() % top.size();
		int index = top[i];
		return index;
	}
	cout << "all the search space is searched " << endl;
	exit(0);
}

int DDT::select_roulette_wheel(){
	assert(contexts.size() > 0);
	vector<int> unknowns;
	vector<pair<float, int> > knowns;
	int count = 0;
	float denominator = 0;
	float temp = 1000;
	for (int i = 0; i < contexts.size(); ++i) {
		if(contexts[i].is_active()){
			count ++;
			float score = contexts[i].get_score();
			if(score == SearchContext::UNKNOWN)
				unknowns.push_back(i);
			else {
				float value = exp(-score/temp);
				denominator += value; 
				knowns.push_back(make_pair(value, i));
			}
		}
	}
	
	if(unknowns.size() > 0){
		float r = float(random()) / RAND_MAX;
		if(r < 0.5 || knowns.size() == 0){
			int i = random() % unknowns.size();
			int index = unknowns[i];
			return index;
		}
	}
	float r = float(random()) / RAND_MAX;
    float ptr = 0;
	if(knowns.size() > 0){
	    for (int i = 0; i < knowns.size(); ++i) {
	    	float score_i = knowns[i].first / denominator;
	    	if(r > ptr && r < (ptr + score_i)){
	    		return knowns[i].second;
	    	}
	    	ptr += score_i;
	    }
	    return knowns.back().second;
	}
	cout << "all the search space is searched " << endl;
	exit(0);
}


void DDT::generate_successors(const State *parent_ptr) {
	vector<const Operator *> ops;
	g_successor_generator->generate_applicable_ops(current_state, ops);
	if (!heuristic->is_dead_end()) {
		int h = heuristic->get_heuristic();
		for (int j = 0; j < ops.size(); j++) {
			contexts[current_c_id].insert(parent_ptr, ops[j], h);
		}
	}
}

void DDT::statistics(){
	//save_plan(plan, g_output_filename, num_plans, true);

	int plan_cost = compute_cost(plan);
	cout << "Total time: " << g_timer << endl;
    cout << "cost: " << plan_cost << "." << endl;
}



void DDT::add_heuristic(FDFFHeuristic * h, bool use_estimates, bool use_preferred_operators) {
	assert(use_estimates);
	if (use_preferred_operators) {
		cout << "Warning: preferred ops are not supported in DDT" << endl;
	}
	heuristic = h;
	Walker::set_heuristic(heuristic);
}

void DDT::add_walking_heuristic(LandmarksCountHeuristic *heuristic, bool use_estimates, bool use_preferred_operators){
	assert(use_estimates);
	if (use_preferred_operators) {
		cout << "Warning: preferred ops are not supported in DDT" << endl;
	}
	walking_heuristic = heuristic;
}

void DDT::initialize() {
	cout << "DDT ..." << endl;
	goal_visited = false;
	
	heuristic->evaluate(current_state);
	if (heuristic->is_dead_end()) {
		assert(heuristic->dead_ends_are_reliable());
		cout << "Initial state is a dead end." << endl;
		exit(0);
	}
	initial_value = heuristic->get_heuristic();
	cout << "initial heuristic value: " << initial_value << endl;
	evaluated_states = 0;
	g_initial_state->dump();
	Walker::init_op_stats(g_operators.size());
	new_context();
	total_min = INT_MAX;
	current_c_id = 0;
	step_counter = 0;
}





