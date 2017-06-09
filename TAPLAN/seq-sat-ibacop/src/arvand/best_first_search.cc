/*********************************************************************
 * Author: Malte Helmert (helmert@informatik.uni-freiburg.de)
 * (C) Copyright 2003-2004 Malte Helmert
 * Modified by: Silvia Richter (silvia.richter@nicta.com.au)
 * (C) Copyright 2008 NICTA
 *
 * This file is part of LAMA.
 *
 * LAMA is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 3
 * of the license, or (at your option) any later version.
 *
 * LAMA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 *********************************************************************/

#include "best_first_search.h"

#include "globals.h"
#include "heuristic.h"
#include "successor_generator.h"
#include "operator.h"
#include "ff_heuristic.h"
#include "landmarks_count_heuristic.h"

#include <cassert>
using namespace std;

OpenListInfo::OpenListInfo(Heuristic *heur, bool only_pref) {
	heuristic = heur;
	only_preferred_operators = only_pref;
	priority = 0;
}

OpenListEntry::OpenListEntry(const State *_parent, const Operator *_op, int _parent_heur) {
	parent = _parent;
	op = _op;
	parent_heur = _parent_heur;
}

BestFirstSearchEngine::BestFirstSearchEngine(int s_num, MTRand_int32 *rg, bool r) :
	current_state(*g_initial_state), rand_gen(rg) {
	generated_states = 0;
	current_predecessor = 0;
	current_operator = 0;

    byte_limit = 1000000;
    rand_open = r;
    search_num = s_num;
    pref_priority_reward = 1000;
}

BestFirstSearchEngine::~BestFirstSearchEngine() {
}

void BestFirstSearchEngine::add_heuristic(Heuristic *heuristic, bool use_estimates, bool use_preferred_operators) {
	assert(use_estimates || use_preferred_operators);
	heuristics.push_back(heuristic);
	best_heuristic_values.push_back(-1);
	if (use_estimates) {
		open_lists.push_back(OpenListInfo(heuristic, false));
		open_lists.push_back(OpenListInfo(heuristic, true));
	}
	if (use_preferred_operators)
		preferred_operator_heuristics.push_back(heuristic);
}

void BestFirstSearchEngine::initialize() {
	cout << "Conducting best first search." << endl;
	assert(!open_lists.empty());
}

void BestFirstSearchEngine::statistics() const {
	cout << "Expanded " << g_closed_list.size() << " state(s)." << endl;
	cout << "Generated " << generated_states << " state(s)." << endl;
}

bool BestFirstSearchEngine::expand_closed_node(const State *parent_ptr) {
    return parent_ptr->get_search_num() < search_num;
}
int BestFirstSearchEngine::step() {
	// Invariants:
	// - current_state is the next state for which we want to compute the heuristic.
	// - current_predecessor is a permanent pointer to the predecessor of that state.
	// - current_operator is the operator which leads to current_state from predecessor.

	// Evaluate only if g-cost of state is lower than bound
	if (g_best_sol_cost != -1 && current_state.get_g_value() >= g_best_sol_cost) {
		return fetch_next_state();
	}
    
    bool expand_node = false;
    const State *parent_ptr = 0;
    // if not in closed list
	if (!g_closed_list.contains(current_state)) {
	
	    calculate_heuristic_and_store();
        
		parent_ptr = g_closed_list.insert(current_state, 
                current_predecessor, current_operator);

        g_closed_list.update_num_bytes(current_state.approx_num_bytes());
        expand_node = true;
		
	} else {
	
	    parent_ptr = g_closed_list.find(current_state);
	    
	    // if seen on previous search but not during current search
	    if(expand_closed_node(parent_ptr)) {
			// We need a const_cast here, as we have to modify parent, but the 
			// STL Map underlying closed_list returns a const_iterator. However, cast
			// is safe as modification of parent does not effect its position in closed_list.
			State *modifiable_parent_ptr = const_cast<State*> (parent_ptr);
			// Change g-value and reached landmarks in state
			// NOTE: Landmarks change, but currently, we aren't taking this into
			// account for the heuristic. Instead, we just use cached value
			
			// update search number
			modifiable_parent_ptr->set_search_num(search_num);
			
			// only changing ancestor if leads to shorter path
			if (current_state.get_g_value() < parent_ptr->get_g_value()) {
			    modifiable_parent_ptr->change_ancestor(*current_predecessor, *current_operator);
			    g_closed_list.update(current_state, current_predecessor, current_operator);
	        }
	    
    	    set_heuristics_from_closed_list(parent_ptr);
    	    expand_node = true;
    	    
	    }
	}
	
    /* TODO  Actually get this to work, currently during ignoring byte limit
     *
    // check if have surpassed given memory
    if(g_closed_list.size()%200 == 0) {
        if(memory_estimate() >= byte_limit)
            return OUT_OF_MEMORY;
    } 
    //For testing memory usage
	if(g_closed_list.size()%500 == 0) {
        cout << endl;
        for(int i = 0; i < open_lists.size(); i++) {
            cout << "Open list elements: " << open_lists[i].open.size() << endl;
            cout << "Open list memory: " << open_lists[i].open.approx_num_bytes()/1000 << endl;
        }
        cout << "Closed List Size: " << g_closed_list.size() << endl;
        cout << "Close list memory: " << g_closed_list.approx_num_bytes()/1000 << endl;
        cout << "Engi Memory: " << memory_estimate()/1000 << " KB." << endl;
    	print_peak_memory();
    }
    */
	if (expand_node && !is_dead_end()) {
	   
		if (check_goal())
			return SOLVED;
		if (check_progress()) {
			report_progress();
			reward_progress();
			
		}
		generate_successors(parent_ptr);
	}
	
	return fetch_next_state();
}

bool BestFirstSearchEngine::is_dead_end() {
	// If a reliable heuristic reports a dead end, we trust it.
	// Otherwise, all heuristics must agree on dead-end-ness.
	int dead_end_counter = 0;
	for (int i = 0; i < heuristics.size(); i++) {
		if (heuristics[i]->is_dead_end()) {
			if (heuristics[i]->dead_ends_are_reliable())
				return true;
			else
				dead_end_counter++;
		}
	}
	return dead_end_counter == heuristics.size();
}

bool BestFirstSearchEngine::check_goal() {
	// Any heuristic reports 0 if this is a goal state, so we can
	// pick an arbitrary one.
	Heuristic *heur = open_lists[0].heuristic;
	if (!heur->is_dead_end() && heur->get_heuristic() == 0) {
		// We actually need this silly !heur->is_dead_end() check because
		// this state *might* be considered a non-dead end by the
		// overall search even though heur considers it a dead end
		// (e.g. if heur is the CG heuristic, but the FF heuristic is
		// also computed and doesn't consider this state a dead end.
		// If heur considers the state a dead end, it cannot be a goal
		// state (heur will not be *that* stupid). We may not call
		// get_heuristic() in such cases because it will barf.

		// If (and only if) using action costs the heuristic might report 0
		// even though the goal is not reached - check.
		if (g_use_metric)
			for (int i = 0; i < g_goal.size(); i++)
				if (current_state[g_goal[i].first] != g_goal[i].second)
					return false;
		// cout << "Solution found!" << endl;
		Plan plan;
		g_closed_list.trace_path(current_state, plan);
		set_plan(plan);
		return true;
	} else {
		return false;
	}
}

bool BestFirstSearchEngine::check_progress() {
	bool progress = false;
	for (int i = 0; i < heuristics.size(); i++) {
		if (heuristics[i]->is_dead_end())
			continue;
		int h = heuristics[i]->get_heuristic();
		int &best_h = best_heuristic_values[i];
		if (best_h == -1 || h < best_h) {
			best_h = h;
			progress = true;
		}
	}
	return progress;
}

void BestFirstSearchEngine::report_progress() {
	cout << "Best heuristic value: ";
	for (int i = 0; i < heuristics.size(); i++) {
		cout << best_heuristic_values[i];
		if (i != heuristics.size() - 1)
			cout << "/";
	}
	cout << " [expanded " << g_closed_list.size() << " state(s)]" << endl;
}

void BestFirstSearchEngine::reward_progress() {
	// Boost the "preferred operator" open lists somewhat whenever
	// progress is made. This used to be used in multi-heuristic mode
	// only, but it is also useful in single-heuristic mode, at least
	// in Schedule.
	//
	// TODO: Test the impact of this, and find a better way of rewarding
	// successful exploration. For example, reward only the open queue
	// from which the good state was extracted and/or the open queues
	// for the heuristic for which a new best value was found.

    if(pref_priority_reward != 0) {
	    for (int i = 0; i < open_lists.size(); i++)
		    if (open_lists[i].only_preferred_operators)
			    open_lists[i].priority -= pref_priority_reward;
    }
}

void BestFirstSearchEngine::get_preferred_from_heuristics() {
    g_preferred_operators.clear();
    
	for (int i = 0; i < preferred_operator_heuristics.size(); i++) {
		Heuristic *heur = preferred_operator_heuristics[i];
		if (!heur->is_dead_end())
			heur->get_preferred_operators(g_preferred_operators);
	}
}

void BestFirstSearchEngine::calculate_heuristic_and_store() {
    for (int i = 0; i < heuristics.size(); i++) {
		heuristics[i]->set_recompute_heuristic();
        heuristics[i]->set_recompute_heuristic(current_state);
		heuristics[i]->evaluate(current_state);
	
        if (!heuristics[i]->is_dead_end())    
		    current_state.add_h_value(heuristics[i]->get_heuristic());
        else
            current_state.add_h_value(Heuristic::DEAD_END);
    }
       
    get_preferred_from_heuristics();
    current_state.set_pref_ops(g_preferred_operators);
    current_state.set_search_num(search_num);
}

void BestFirstSearchEngine::set_heuristics_from_closed_list(const State *state_ptr) {
    g_preferred_operators.clear();
    state_ptr->get_preferred_ops(g_preferred_operators);
    
    g_heuristic_values.clear();
    state_ptr->get_h_values(g_heuristic_values);
    
    // sets all of the heuristics to return the appropriate value
    // assumes heuristics are called in the same order
    for (int i = 0; i < heuristics.size(); i++) {
        heuristics[i]->set_heuristic(g_heuristic_values[i]);
    }

}
void BestFirstSearchEngine::generate_successors(const State *parent_ptr) {
	vector<const Operator *> all_operators;
	g_successor_generator->generate_applicable_ops(current_state, all_operators);

	for (int i = 0; i < open_lists.size(); i++) {
		Heuristic *heur = open_lists[i].heuristic;
		if (!heur->is_dead_end()) {
			int h = heur->get_heuristic();
			//cout << h << endl;
			OpenList<OpenListEntry> &open = open_lists[i].open;
			vector<const Operator *> &ops = open_lists[i].only_preferred_operators ? g_preferred_operators : all_operators;
            
            if(rand_open && ops.size() > 1) {
                for(unsigned j = 0; j < ops.size() - 1; j++) {
                    unsigned index = rand_gen->get_32bit_int() % (ops.size() - j);
                    const Operator *to_move = ops[j];
                    ops[j] = ops[index + j];
                    ops[index + j] = to_move;
                }
            }
			
            for (int j = 0; j < ops.size(); j++) {
				// Tie braker criterium ensures breadth-first search on plateaus
				// (will be equal to depth of node if no action costs are used,
				// and cost of node otherwise)
				int tie_braker = parent_ptr->get_g_value() + ops[j]->get_cost();
				open.insert(make_pair(h, tie_braker), OpenListEntry(parent_ptr, ops[j], h));
			}
		}
	}
	generated_states += all_operators.size();
}

int BestFirstSearchEngine::fetch_next_state() {
	OpenListInfo *open_info = select_open_queue();
	if (!open_info) {
		cout << "Completely explored state space -- no solution!" << endl;
		return FAILED;
	}

	OpenListEntry next = open_info->open.remove_min();
	open_info->priority++;

	current_predecessor = next.parent;
	current_operator = next.op;
	current_state = State(*current_predecessor, *current_operator);

	return IN_PROGRESS;
}

OpenListInfo *BestFirstSearchEngine::select_open_queue() {
	OpenListInfo *best = 0;
	for (int i = 0; i < open_lists.size(); i++)
		if (!open_lists[i].open.empty() && (best == 0 || open_lists[i].priority < best->priority)) {
			//cout << i << endl;
			best = &open_lists[i];
		}
	return best;
}

size_t BestFirstSearchEngine::memory_estimate() const {

    size_t size = 0;
    for(int i = 0; i < open_lists.size(); i++) {
        size += open_lists[i].open.approx_num_bytes();
    }
    size += g_closed_list.approx_num_bytes();
    
    return size;
}

