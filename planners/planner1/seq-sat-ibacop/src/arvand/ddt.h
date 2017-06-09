#ifndef DDT_H_
#define DDT_H_

#include "state.h"
#include "heuristic.h"
#include "vector"
#include "operator.h"
#include "search_engine.h"
#include "mrw.h"
#include "fd_ff_heuristic.h"
#include "best_first_search.h"
#include "successor_generator.h"
#include <map>
#include "float.h"
#include <climits>
using namespace std;

struct WalkInfo;
class Walker;
// class Parameters;

typedef vector<const Operator*> Path;



class DDTNode{

public:	
	State state;
	const State* parent;
	const Operator* op;
	int r_id;
	DDTNode(State _state, const State* p, const Operator* _op, int _id) : state(_state), parent(p), op(_op), r_id(_id) {}  
};

class SearchContext{
	
	vector<OpenList<OpenListEntry>* > open_lists;
	Path path;
	bool disabled;
	float alpha;
	int current_list;
	bool empty;
	bool compare(const pair<int,int>& a, const pair<int,int>& b) const {
	    if(a.first != b.first)
	    	return a.first < b.first;
	    else
	    	return a.second < b.second;
	};

public: 
	int r_id;
	int h_min;
	float m;
	std::set<int> ids;
	enum {UNKNOWN = -1};
	SearchContext( State current_state, const State* parent_ptr, int h, int _id ){
		vector<const Operator *> ops;
		g_successor_generator->generate_applicable_ops(current_state, ops);
		for (int j = 0; j < ops.size(); j++) {
			int tie_braker = parent_ptr->get_g_value() + ops[j]->get_cost();
			OpenList<OpenListEntry>* open = new OpenList<OpenListEntry>;
			open->insert(make_pair(h, tie_braker), OpenListEntry(parent_ptr, ops[j], h));
			open_lists.push_back(open);
		}
		current_list = 0;
		alpha = 0.4;
		r_id = _id;
		h_min = h;
		disabled = false;
		ids.insert(r_id);
		m = UNKNOWN;
		empty = false;
	}
	
	pair<const State*, const Operator*> remove_min(){
		//assert(!is_empty());
		pair<int, int> min = make_pair(INT_MAX, INT_MAX);
		vector<int> top;
		for (int i = 0; i < open_lists.size(); ++i) {
			if(open_lists[i]->empty()){
				continue;
			}
			pair<int, int> temp = open_lists[i]->min();
			//cout << "top: " << temp.first << " " << temp.second << endl;
			if(compare(temp , min)){
				min = temp;
				top.clear();
				top.push_back(i);
			}else if(!compare(min, temp)){
				top.push_back(i);
			}
		}
		assert(top.size() != 0);
		int i = random() % top.size();
		int index = top[i];
		OpenListEntry entry = open_lists[index]->remove_min();
		pair<const State*, const Operator*> result = make_pair( entry.parent, entry.op );
		current_list = index;
		return result;
	}
	
	void merge(SearchContext& sc){
		//cout << "merging " << r_id << " " << sc.r_id << endl;
		if(sc.h_min < h_min){
			h_min = sc.h_min;
			m = sc.m;
		}else if (sc.h_min == h_min){
			m = max(m, sc.m);
		}
		std::set<int>::iterator curr, end = sc.ids.end();
		for(curr = sc.ids.begin(); curr != end; ++curr) {
			ids.insert(*curr);
		}
		for (int i = 0; i < sc.open_lists.size(); ++i) {
			if(!sc.open_lists[i]->empty())
				open_lists.push_back(sc.open_lists[i]);
		}
		sc.disable();
	}
	
	void insert(const State* parent_ptr, const Operator* op, int h){
		int tie_braker = parent_ptr->get_g_value() + op->get_cost();
		open_lists[current_list]->insert(make_pair(h, tie_braker), OpenListEntry(parent_ptr, op, h));
	}
	
	void update_h(int h){
		int d = 0;
		if(h_min > h){
			d = h_min - h;
		}
		if(m == UNKNOWN)
			m = d;
		else
			m = ( 1 - alpha) * m + alpha * d;  
		if(m < 0.001)
			m = 0.001;
		
		h_min = min(h, h_min);
		//cout << "id: " << r_id << " m: " << m << endl;
	}
	
	void disable(){
		disabled = true;
	}
	bool is_active(){
		
		return (!disabled && !is_empty());
	}
	float get_score(){
		//cout << "id: " << r_id << " m: " << m << endl;
		if(m == UNKNOWN)
			return UNKNOWN;
		else
			return (h_min/m);
		
	}
	
	bool is_empty(){
		if(empty)
			return true;
		for (int i = 0; i < open_lists.size(); ++i) {
			if(!open_lists[i]->empty())
				return false;
		}
		empty = true;
		//cout << "new context is exhusted" << endl;
		return true;
	}
	
};

/*class RRT_Parameters: public Parameters{
// This class is a container for the parameters used in RRT
	
public:	
	float p;
	RRT_Parameters() {}
	RRT_Parameters(float r, float temp, int l, int t) : Parameters(r, temp, l, t) {
		p = 0.4;
	}

};*/


class DDT : public SearchEngine {
private:
	int initial_value;
	int evaluated_states;
	int num_jumps;
	float avg_branching;
	float failure_percentage;
	bool goal_visited;
	int total_min;
	int step_counter;
	
	State current_state;
	FDFFHeuristic* heuristic;
	LandmarksCountHeuristic* walking_heuristic;
	Path plan; 
	//RRT_Parameters params;
	//DDT variables: 
	std::vector<SearchContext> contexts;
	std::map<State, DDTNode> memory;
    const State *current_predecessor;
    const Operator *current_operator;
    OpenList<OpenListEntry> open;
    int max_r_id;
    int current_c_id;
    bool first_step;
    int get_c_id(State& state);
    bool is_exit_point(State& state, int r_id);
    void new_context();
    int fetch_next_state();
    void generate_successors(const State *parent_ptr);
    void statistics();
    int select_search_context();
    int select_roulette_wheel();
    int select_greedy();

protected:
	virtual int step();
    virtual void initialize();

public:
	enum{UNKNOWN = -1, A_LOT = 10000000};
    virtual void add_heuristic(FDFFHeuristic *heuristic, bool use_estimates, bool use_preferred_operators);
    virtual void add_walking_heuristic(LandmarksCountHeuristic *heuristic, bool use_estimates, bool use_preferred_operators);
    DDT();
	virtual ~DDT();
};

#endif /*DDT_H_*/
