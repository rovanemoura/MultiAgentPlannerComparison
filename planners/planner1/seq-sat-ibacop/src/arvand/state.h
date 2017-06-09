/*********************************************************************
 * Author: Malte Helmert (helmert@informatik.uni-freiburg.de)
 * (C) Copyright 2003-2004 Malte Helmert
 * Modified by: Silvia Richter (silvia.richter@nicta.com.au),
 *              Matthias Westphal (westpham@informatik.uni-freiburg.de)             
 * (C) Copyright 2008 NICTA and Matthias Westphal
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

#ifndef STATE_H
#define STATE_H

#include <iostream>
#include <vector>
#include <ext/hash_set>
#include "landmarks_types.h"
#include <cassert>

using namespace std;
using namespace __gnu_cxx;

class Operator;
class PrePost;
class LandmarkNode;

class State {
	friend void read_everything(istream &in, bool generate_landmarks, 
            bool reasonable_orders);
	vector<int> vars; // values for vars
	hash_set<const LandmarkNode *, hash_pointer> reached_lms;
	int reached_lms_cost;

	int g_value; // min. cost of reaching this state from the initial state
	void set_landmarks_for_initial_state();
	void update_reached_lms(const Operator &op);
	bool landmark_is_leaf(const LandmarkNode& node, const hash_set<const LandmarkNode*, hash_pointer>& reached) const;
	bool check_lost_landmark_children_needed_again(const LandmarkNode& node) const;
	
	// TODO Better memory management of this
	// also not copying this currently
	vector<int> heuristic_values;
	vector<const Operator *> pref_operators;
	
	int search_num;

public:
	State(istream &in);
	State(const State &predecessor, const Operator &op);
	
    //State(const State &to_copy);

    int &operator[](int index) {
		return vars[index];
	}
	int operator[](int index) const {
		return vars[index];
	}
	void dump() const;
	
	// TODO: Does this determine the hashing function?
	bool operator<(const State &other) const;

	int get_g_value() const { return g_value; }
	
	void set_search_num(int s_num) {
		search_num = s_num;
	}
	
	int get_search_num() const { return search_num;}
	
	// adds h_value list to our own
	void add_h_value(int h_value);
	
	// get the list of heuristic values
	void get_h_values(vector<int> &h_vec) const;
	
	// add the given preferred operators to our preferred operators
	void set_pref_ops(const vector<const Operator *> &prefs);
	
	// appends preferred operators to given vector
	void get_preferred_ops(vector<const Operator *> &prefs) const;
	
	void change_ancestor(const State &new_predecessor, const Operator &new_op);

	int check_partial_plan(hash_set<const LandmarkNode*, hash_pointer>& reached) const;
	int get_needed_landmarks(hash_set<const LandmarkNode*, hash_pointer>& needed) const;

    size_t approx_num_bytes() const;
};

#endif
