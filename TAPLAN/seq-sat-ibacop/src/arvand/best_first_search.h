/*********************************************************************
 * Author: Malte Helmert (helmert@informatik.uni-freiburg.de)
 * (C) Copyright 2003-2004 Malte Helmert
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

#ifndef BEST_FIRST_SEARCH_H
#define BEST_FIRST_SEARCH_H

#include <vector>

#include "closed_list.h"
#include "open_list.h"
#include "search_engine.h"
#include "state.h"
#include "mtrand.h"

class Heuristic;
class Operator;

//typedef pair<const State *, const Operator *> OpenListEntry;

struct OpenListEntry {
    OpenListEntry(const State *parent, const Operator *op, int parent_heur);
    const State *parent; 
    const Operator *op; 
    int parent_heur;
};

struct OpenListInfo {
    OpenListInfo(Heuristic *heur, bool only_pref);
    Heuristic *heuristic;
    bool only_preferred_operators;
    OpenList<OpenListEntry> open;
    int priority; // low value indicates high priority
};

class BestFirstSearchEngine : public SearchEngine {
protected:
    std::vector<Heuristic *> heuristics;
    std::vector<Heuristic *> preferred_operator_heuristics;
    std::vector<OpenListInfo> open_lists;

    std::vector<int> best_heuristic_values;
    int generated_states;

    State current_state;
    const State *current_predecessor;
    const Operator *current_operator;
    
    bool is_dead_end();
    bool check_goal();
    bool check_progress();
    void report_progress();
    void reward_progress();
    void generate_successors(const State *parent_ptr);
    virtual int fetch_next_state();
    OpenListInfo *select_open_queue();

    virtual void initialize();
    virtual int step();
    
    // randomize order of generated nodes before added them
    // to the open list
    bool rand_open;

    // which search are currently performing
    int search_num;

    int pref_priority_reward;

    MTRand_int32 *rand_gen;
    int byte_limit;

    vector<const Operator *> g_preferred_operators;
    vector<int> g_heuristic_values;
    
    void get_preferred_from_heuristics();
    void calculate_heuristic_and_store();
    void set_heuristics_from_closed_list(const State *state_ptr);
    
public:
    BestFirstSearchEngine(int s_num, MTRand_int32 *rg, bool r=false);
    ~BestFirstSearchEngine();
    virtual void add_heuristic(Heuristic *heuristic, bool use_estimates,
		       bool use_preferred_operators);
    virtual void statistics() const;

    std::vector<OpenListInfo>& get_open_list(){
    	return open_lists;
    }

    inline const Operator *get_current_operator() const {
        // Used by LandmarksCountHeuristic
	return current_operator;
	
    }

    void set_priority_reward(int s) {pref_priority_reward = s;}
    void set_byte_limit(int b) {byte_limit = b;}

    // should expand this node, depends on the algorithm condition
    virtual bool expand_closed_node(const State *parent_ptr);
    
    size_t memory_estimate() const;
};

#endif
