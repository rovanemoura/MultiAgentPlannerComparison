#include "rpt_search.h"
#include "plugin.h"
#include "operator_cost.h"
#include "successor_generator.h"
#include "ext/tree_util.hh"
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <locale>

#include <vector>
#include <algorithm>

#include <limits>

RPTNode::RPTNode(State* state, RPTNode* previous, Plan* plan_to_previous, FFHeuristic* ff): state(state), previous(previous), plan_to_previous(plan_to_previous) {
    // compute best supporters for every proposition to make the RP heuristic computation faster
    ff->build_goals(g_original_goal);
    ff->setup_exploration_queue();
    ff->setup_exploration_queue_state(*state);
    ff->relaxed_exploration(true);

    // load the best supporters
    std::vector<std::vector<Proposition> > props = ff->get_propositions();
    cached_supporters.resize(props.size());
    for (int i = 0; i < props.size(); i++) {
        cached_supporters.reserve(props[i].size());
        for (int j = 0; j < props[i].size(); j++) {
            if (props[i][j].cost == -1) {
	        cached_supporters[i].push_back(make_pair<bool, const UnaryOperator *>(false, NULL));
	    } else {
	        cached_supporters[i].push_back(make_pair<bool, const UnaryOperator *>(true, props[i][j].reached_by));
            }
        }
    }

    // get the distance to the original goal
    heuristic = compute_cached_ff_heuristic(g_original_goal);
}

int RPTNode::compute_cached_ff_heuristic(vector<pair<int, int> > goal_propositions) {
    RelaxedPlan relaxed_plan(g_operators.size(),false);
    for(int i = 0; i < goal_propositions.size(); i++)
        if(!collect_cached_plan(goal_propositions[i], relaxed_plan))
            return DEAD_END;

    int h_ff = 0;
    for (int op_no = 0; op_no < relaxed_plan.size(); op_no++) {
        if (relaxed_plan[op_no]) {
            h_ff += get_adjusted_action_cost(g_operators[op_no], ONE); // hardcoded cost type
        }
    }

    return h_ff;

}

bool RPTNode::collect_cached_plan(pair<int, int> prop, RelaxedPlan &relaxed_plan){
    if(!cached_supporters[prop.first][prop.second].first)  // unreachable prop
        return false;

    const UnaryOperator *uop = cached_supporters[prop.first][prop.second].second;
    if(uop) { // We have not chained back to a start node yet
       const Operator *op = &g_operators[uop->operator_no];
       if(!op->is_axiom() && !relaxed_plan[uop->operator_no]){ // if the op wasn't in the RP
           relaxed_plan[uop->operator_no] = true;
           for(int i = 0; i < uop->precondition.size(); i++)
               if(!collect_cached_plan(make_pair(uop->precondition[i]->var, uop->precondition[i]->val), relaxed_plan))
                   return false;
        }
    }
    return true;
}


RPTSearch::RPTSearch(const Options &opts)
    : SearchEngine(opts),
      engine_configs(opts.get_list<ParseTree>("engine_configs")),
      nodes(opts.get<int>("nodes")), p(opts.get<float>("p")), r(opts.get<float>("r")), planr(opts.get<std::string>("planr")),
      ff(NULL), last_state(NULL) { }

RPTSearch::~RPTSearch() { }

void RPTSearch::initialize() {
    local_search = get_search_engine(0);

    cout << "Local search: ";
    kptree::print_tree_bracketed(engine_configs[0], cout);
    cout << endl;

    cout << "Initializing local search with a limit of " << nodes << " nodes." << endl;
    local_search->initialize();
    local_search->set_limit_nodes(nodes);

    // TODO: share ff with state sampler?
    Options op_ff;
    op_ff.set("cost_type", 1);
    ff = new FFHeuristic(op_ff);
    ff->evaluate(*g_initial_state); // just to initialize it

    RPTNode* initial_node;
    initial_node = new RPTNode(g_initial_state, NULL, NULL, ff);

    all_nodes.push_back(initial_node);
    unexpanded_nodes.push(initial_node);
    unreutilized_nodes.push(initial_node);


    std::ifstream input(planr.c_str());

    for (std::string line; std::getline(input, line,'('); )
    {
        std::getline(input, line,')');
	for (int op_t = 0; op_t < g_operators.size(); op_t++) {
             if(strcmp((g_operators[op_t].get_name()).c_str(),(line).c_str())==0){
		input_plan.push_back(&g_operators[op_t]);
		//cout << "op" << g_operators[op_t].get_name() << endl;
	     }
        }

    }
    input.close();
}

SearchEngine *RPTSearch::get_search_engine(int engine_configs_index) {
    OptionParser parser(engine_configs[engine_configs_index], false);
    SearchEngine *engine = parser.start_parsing<SearchEngine *>();

    return engine;
}

int RPTSearch::step() {
    RPTNode* closest = NULL;
    pair<const State*, Plan> reached_node;

    // keep track of the size of the tree
    if (all_nodes.size() % 20 == 0)
        cout << "Tree size: " << all_nodes.size() << " unexpanded " << unexpanded_nodes.size() << endl;
    double random = rand()%100;
    cout << "random " << random << endl;
    cout << "p " << p << endl;
    cout << "r " << r << endl;

    if (random < (p*100)) { // sample
        //cout << "Sample" << endl;
        vector<pair<int, int> > sampled_goals = sampler.sample_random_state();
        /* cout << "Sampled state:" << endl;
        for (int i = 0; i < sampled_goals.size(); i++) {
            cout << "Fluent: " << g_fact_names[sampled_goals[i].first][sampled_goals[i].second] << " related to " << g_fact_names[sampled_goals[i].first][0] << endl;
        } */

        // find the nearest neighbour
        int min_heuristic = numeric_limits<int>::max();
        for(int i = 0; i < all_nodes.size(); i++){
            int distance_to_sample = numeric_limits<int>::max();
            distance_to_sample = all_nodes[i]->compute_cached_ff_heuristic(sampled_goals);
            if(distance_to_sample >= 0 && distance_to_sample < min_heuristic){
                closest = all_nodes[i];
                min_heuristic = distance_to_sample;
            }
        }

        reached_node = local_search->search(*(closest->get_state()), sampled_goals);
        expanded_nodes += local_search->get_search_progress().get_expanded();

        if(reached_node.first == NULL) {
            return IN_PROGRESS;
        } else {
            // a new node is created
            State* state_aux = new State(*reached_node.first);
            Plan* plan_aux = new Plan(reached_node.second);
            closest = new RPTNode(state_aux, closest, plan_aux, ff);
            int distance;
            distance = closest->heuristic;
            if(distance == -1){
                return IN_PROGRESS;  // updating should be better if there's a collision
            } else if(distance == 0) {     // it may happen that the reached stated is a goal state
                // this code is similar to the regular case, make a function
                last_state = reached_node.first;
                Plan plan_aux = reached_node.second;
                RPTNode* previous_node = closest->get_previous();
                int edges = 1;
                while(previous_node->get_plan()){  // the plans are concatenated
                    plan_aux.insert(plan_aux.begin(), previous_node->get_plan()->begin(), previous_node->get_plan()->end());
                    previous_node = previous_node->get_previous();
                    edges++;
                }
                set_plan(plan_aux);
                cout << "Total number of edges: " << edges << endl;
                cout << "Total number of expanded nodes: " << expanded_nodes << endl;
                cout << "Tree size: " << all_nodes.size() << " unexpanded " << unexpanded_nodes.size() << endl;
                return SOLVED;
            } else {
                /* cout << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!! distance " << distance << endl;
                state_aux->dump_pddl();
                Plan plan_aux = reached_node.second;
        for (int i = 0; i < plan_aux.size(); i++) {
            cout << plan_aux[i]->get_name() << " (" << plan_aux[i]->get_cost() << ")" << endl;
        }*/
                all_nodes.push_back(closest);
		unreutilized_nodes.push(closest);
                // don't insert the new node in unexpanded because it is expanded right after this
                // NEREAAAAA put the node in unreused nodes!!!
            }
        }
    }

    else if((random > (p*100)) && (random <= ((p+r)*100)) ){
	  //cout << "----reuse----" << random << endl;
          //Reuse phase
          std::string line;
	  closest = unreutilized_nodes.top();
	  unreutilized_nodes.pop();

	 // vector<pair<int, int> > sampled_goals = sampler.sample_random_state();
         // reached_node = local_search->search(*(closest->get_state()), sampled_goals);
         // expanded_nodes += local_search->get_search_progress().get_expanded();

            // a new node is created
            State* state_aux = closest->get_state();
            //Plan* plan_aux = closest->get_plan();
         //   closest = new RPTNode(state_aux, closest, plan_aux, ff);

	    //bool firstime = true;

	    int index = closest->ind;
	    bool reused = false;
            bool success = false;
	    Plan p;

		for (int i = index; i < input_plan.size(); i++) {

		 // vector<const Operator *> applicable_ops;
	   	 // g_successor_generator->generate_applicable_ops(*state_aux, applicable_ops);
		 //for (int j = 0; j < applicable_ops.size(); j++) {
		 //   const Operator *ap = applicable_ops[j];
		//	cout << "oppp" << ap->get_name() << endl;
		  //}

			const Operator *op = input_plan[i];

			//cout << "fhhhh " << op->get_name() << endl;

			if(op->is_applicable(*state_aux)){
				//cout << "f" << op->get_name() << endl;
				State succ_state(*state_aux, *op);
				p.insert(p.end(),input_plan[i]);
				*state_aux = succ_state;
				//*plan_aux = p;

				reused = true;
        			success = true;


			if(reused){
				if((i+1)<input_plan.size()){
					op = input_plan[i+1];
					//cout << "r" << op->get_name() << endl;
					if(op->is_applicable(*state_aux));
					else {
					  reused = false;
					  closest->ind = i;
					  break; 
					}
				}
			
			  }

			}
			//cout << "rrrrrrrrr " << op->get_name() << endl;
			

			
                }

				Plan* plan_aux = new Plan(p);
				closest = new RPTNode(state_aux, closest, plan_aux, ff);

				vector<pair<int,int> > temp_goal(g_original_goal);
				reached_node = local_search->search(*(closest->get_state()), temp_goal);
				    // if the original goal is reached
				    if (local_search->found_solution()) {
					last_state = reached_node.first;
					Plan plan_aux = reached_node.second;
					RPTNode* previous_node = closest;
					int edges = 1;
					while (previous_node->get_plan()) {  // the plans are concatenated
					    plan_aux.insert(plan_aux.begin(), previous_node->get_plan()->begin(), previous_node->get_plan()->end());
					    previous_node = previous_node->get_previous();
					    edges++;
					}
					set_plan(plan_aux);
					cout << "Total number of edges: " << edges << endl;
					cout << "Total number of expanded nodes: " << expanded_nodes << endl;
					cout << "Tree size: " << all_nodes.size() << " unexpanded " << unexpanded_nodes.size() << endl;
					return SOLVED;
				    }

		//for (int i = 0; i < p.size(); i++) {
               //		cout << p[i]->get_name() << " (" << p[i]->get_cost() << ")" << endl;
            	//}

		if(success){
	    	all_nodes.push_back(closest);
	    	//all_nodes.push_back(newNode);
	    	//unreutilized_nodes.pop();
		unexpanded_nodes.push(closest);
		}
   }
    


    else {  // get the closest non-expanded goal to the goal
	//cout << "----elseeee----" << random << endl;
       // cout << "Closest" << endl;
        closest = unexpanded_nodes.top();
        unexpanded_nodes.pop();
    }

    //cout << "Extension" << endl;
    // closest->get_state()->dump_pddl(); cout << "--------------------" << endl;
    vector<pair<int,int> > temp_goal(g_original_goal);

    reached_node = local_search->search(*(closest->get_state()), temp_goal);
    expanded_nodes += local_search->get_search_progress().get_expanded();

    // if the original goal is reached
    if (local_search->found_solution()) {
        last_state = reached_node.first;
        Plan plan_aux = reached_node.second;
        RPTNode* previous_node = closest;
        int edges = 1;
        while (previous_node->get_plan()) {  // the plans are concatenated
            plan_aux.insert(plan_aux.begin(), previous_node->get_plan()->begin(), previous_node->get_plan()->end());
            previous_node = previous_node->get_previous();
            edges++;
        }
        set_plan(plan_aux);
        cout << "Total number of edges: " << edges << endl;
        cout << "Total number of expanded nodes: " << expanded_nodes << endl;
        cout << "Tree size: " << all_nodes.size() << " unexpanded " << unexpanded_nodes.size() << endl;
        return SOLVED;
    }

    // add the last expanded state to the list of nodes
    if (reached_node.first != NULL) {     // if there was no solution, no new node is added
        State* state_aux = new State(*reached_node.first);
        Plan* plan_aux = new Plan(reached_node.second);
        RPTNode* new_node = new RPTNode(state_aux, closest, plan_aux, ff);

        // if the original goal is not reachable from the reached state, no new node is added
        if(new_node->heuristic != -1){
            unexpanded_nodes.push(new_node);
	    unreutilized_nodes.push(new_node);
            all_nodes.push_back(new_node);
            return IN_PROGRESS;  // updating should be better
        }
    } else {
        // crappy removal, I should make it a set
        for (int i = 0; i < all_nodes.size(); i++) {
            if (all_nodes[i] == closest) {
                all_nodes.erase(all_nodes.begin()+i);
                break;
            }
        }
    }
    return IN_PROGRESS;
}

void RPTSearch::statistics() const { }

static SearchEngine *_parse(OptionParser &parser) {
    parser.add_list_option<ParseTree>("engine_configs", "");
    parser.add_option<int>("nodes", 50000, "Maximum number of expanded nodes in every local search");
    parser.add_option<float>("p", 0.5, "Probability of sampling instead of expanding towards the goal");
    parser.add_option<float>("r", 0.5, "Probability of reusing a plan instead of expanding towards the goal");
    parser.add_option<string>("planr", "/home/nluis/Descargas/seq-sat-rpt/03.soln");

    SearchEngine::add_options_to_parser(parser);
    Options opts = parser.parse();

    opts.verify_list_non_empty<ParseTree>("engine_configs");

    if (parser.help_mode()) {
        return 0;
    } else if (parser.dry_run()) {
        //check if the supplied search engines can be parsed
        vector<ParseTree> configs = opts.get_list<ParseTree>("engine_configs");
        for (size_t i(0); i != configs.size(); ++i) {
            OptionParser test_parser(configs[i], true);
            test_parser.start_parsing<SearchEngine *>();
        }
        return 0;
    } else {
        RPTSearch *engine = new RPTSearch(opts);
        return engine;
    }
}

static Plugin<SearchEngine> _plugin("rpt", _parse);
