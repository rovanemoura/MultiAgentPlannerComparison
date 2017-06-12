/*********************************************************************
 * Author: Silvia Richter (silvia.richter@nicta.coma.au)
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

#include "wa_star_search.h"
#include "globals.h"
#include "heuristic.h"
#include "successor_generator.h"
#include "operator.h"
#include "ff_heuristic.h"
#include "landmarks_count_heuristic.h"

#include <cassert>
using namespace std;

WAStarSearchEngine::WAStarSearchEngine(int s_num, MTRand_int32 *rg, int w, bool r) :
	BestFirstSearchEngine(s_num, rg, r) {
	weight = w;
}

void WAStarSearchEngine::debug_print_partial_plan(const State& state) {
	if (current_operator != 0) {
		Plan plan;
		g_closed_list.trace_path(state, plan);
		for (int i = 0; i < plan.size(); i++) {
			cout << plan[i]->get_name() << " (" << plan[i]->get_cost() << ")" << endl;
		}
	}
}

void WAStarSearchEngine::initialize() {
	cout << "Conducting weighted A* search, weight is " << weight << "." << endl;
	assert(!open_lists.empty());
	
	generated_states = 0;
    current_predecessor = 0;
    current_operator = 0;
    generated_states = 0;
}

bool WAStarSearchEngine::expand_closed_node(const State *parent_ptr) {
    return (parent_ptr->get_search_num() < search_num || 
        current_state.get_g_value() < parent_ptr->get_g_value());
}

void WAStarSearchEngine::generate_successors(const State *parent_ptr) {
	vector<const Operator *> all_operators;
	g_successor_generator->generate_applicable_ops(current_state, all_operators);
	
	get_preferred_from_heuristics();

	for (int i = 0; i < open_lists.size(); i++) {
		Heuristic *heur = open_lists[i].heuristic;
		if (!heur->is_dead_end()) {
			int parent_h = heur->get_heuristic();
			//cout << parent_h << endl;
			int parent_g = parent_ptr->get_g_value();
			int parent_f = weight * parent_h + parent_g;
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
				int h = parent_f + ops[j]->get_cost();
				int tie_braker = parent_g + ops[j]->get_cost();
				open.insert(make_pair(h, tie_braker), OpenListEntry(parent_ptr, ops[j], parent_h));
			}
		}
	}
	generated_states += all_operators.size();
}


