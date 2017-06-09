/*********************************************************************
 * Author: Malte Helmert (helmert@informatik.uni-freiburg.de)
 * (C) Copyright 2003-2004 Malte Helmert
 * Modified by: Silvia Richter (silvia.richter@nicta.com.au),
 *              Matthias Westphal (westpham@informatik.uni-freiburg.de)             
 * (C) Copyright 2008 NICTA and Matthias Westphal
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
#include "wa_star_search.h"
#include "ff_heuristic.h"
#include "fd_ff_heuristic.h"
#include "globals.h"
#include "operator.h"
#include "landmarks_graph.h"
#include "landmarks_graph_rpg_sasp.h"
#include "landmarks_count_heuristic.h"
#include "string.h"
#include "mrw.h"
#include "config_parser.h"

#include <cassert>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <sys/times.h>
#include <climits>

using namespace std;

// outputs error info
void output_help_information();

// processes the command line
bool process_command_line(int argc, const char *argv[]);

// adds the heuristics to a best first search engine
void add_heuristics(BestFirstSearchEngine* engine);

// adds the heursitics to a mrw search engine
void add_heuristics(MRW *engine);

void run_aras();

bool poly_time_method = false;

// heuristics to use in mrw
bool mrw_fd_ff_heuristic = false;
bool mrw_landmarks_heuristic = false;
bool mrw_lama_ff_heuristic = false;
bool mrw_lama_ff_s_heuristic = false;
bool mrw_lama_ff_c_heuristic = false;

// run monte carlo rw
bool monte_carlo_rw = false;

// still don't know what his does
bool reasonable_orders = true;

// generator seed
int seed = -1;
// should we generate landmarks
bool generate_landmarks = false;

// lama params
LAMA_Parameters *lama_params = NULL;

// heuristics used for lama
// keep them around anytime version of LAMA
LandmarksCountHeuristic *lama_lm_heur = NULL;
FFHeuristic *lama_ff_heur = NULL;
FFHeuristic *lama_ffc_heur = NULL;
FFHeuristic *lama_ffs_heur = NULL;
FDFFHeuristic *lama_fd_ff_heur = NULL;

int main(int argc, const char **argv) {

	if (!process_command_line(argc, argv)) {
		cerr << endl << "Usage should be as follows" << endl;
        output_help_information();
        exit(1);
	}

    // prints out mrw configurations information
    for(int i = 0; i < g_params_list.size(); i++) {
        cout << "MRW Param " << i << ":" << endl;
        g_params_list[i]->print_values();
        cout << endl << endl;
    }

    // prints out restart info is doing mrw run
    if(!g_params_list.empty()) {
        g_mrw_shared->print_values();
        cerr << endl << endl;
    }

    if(lama_params != NULL) {
        cout << "LAMA Param:" << endl;
        lama_params->print_values();
        cout << endl << endl;
    }

    if (lama_params != NULL && !g_params_list.empty()) {
        cerr << "Does not currently handle both mrw and lama configurations" << endl;
        exit(1);
    }
    
	if (seed == -1) {
		seed = 2011;
		//seed = time(null);
	}
	cout << "seed: " << seed << endl;
	srandom(seed);
	/*for (int i = 0; i < 100; ++i) {
		cout << random() << endl;
	}*/

    // what does this do?
	cin >> poly_time_method;
	if (poly_time_method) {
		cout << "Poly-time method not implemented in this branch." << endl;
		cout << "Starting normal solver." << endl;
	}

	// Read input and generate landmarks
	g_lgraph = NULL;
	Timer landmark_timer;
	read_everything(cin, generate_landmarks, reasonable_orders);
    landmark_timer.stop();
	if(g_lgraph != NULL) {
		cout << "Landmarks generation time: " << landmark_timer << endl;
	}

	// Check whether landmarks were found, fix parameter settings
	if (generate_landmarks && g_lgraph->number_of_landmarks() == 0) {
		cout << "No landmarks found. This should only happen if task is unsolvable." << endl;
		
		// fix mrw params when no landmarks
		if(monte_carlo_rw){
		    cout << "All landmark configs changed to use FD_FF" << endl;
		    // turn off use of landmarks with mrw
		    mrw_landmarks_heuristic = false;
		
		    // Any mrw parameters using LM are changed to use FD_FF
            for(int i = 0; i < g_params_list.size(); i++) {
                if(g_params_list[i]->heur == MRW_Parameters::LM)
                    g_params_list[i]->heur = MRW_Parameters::FD_FF;
            }
        }
        
        // fix lama_params when no landmarks
        if(lama_params != NULL) {
            
            lama_params->lm_heur = false;
            lama_params->lm_preferred = false;
        
            cout << "Landmarks removed from LAMA heuristics list" << endl;
            // if no other heuristics are set as to be used
            if(!lama_params->lama_ff_heur && !lama_params->fd_ff_heur &&
                !lama_params->lama_ffs_heur && !lama_params->lama_ffc_heur) {
            
                cout << "ffsFFS added to LAMA heuristics list." << endl;
                // use lama_ffs
                lama_params->lama_ffs_heur = true;
                lama_params->lama_ffs_preferred = true;    
            
                // turn off duplicate operators
                lama_params->lama_ff_preferred = false;
                lama_params->lama_ffc_preferred = false;
            } else {
                
            }
        }
	}

    // performs monte carlo random walks    
	if(monte_carlo_rw){
        
        // initialize parameter learner
        p_learner = new UCB(g_mrw_shared->ucb_const, new MTRand_int32(seed));
        seed+= 11;

        // create pool for smart restarts
	    if(g_mrw_shared->restart_type == Shared_MRW_Parameters::S_RESTART){
		    g_walk_pool = new WalkPool(g_mrw_shared->pool_size, 
                    g_mrw_shared->act_level, new MTRand_int32(seed));
            seed+= 29;
        }
    
        // heuristic indices
        int ff_index = -1;
        int landmarks_index = -1;
        int lama_ff_index = -1; 
        int lama_ff_s_index = -1;
        int lama_ff_c_index = -1;

        int h_index = 0;

        if(mrw_fd_ff_heuristic) {    
            ff_index = h_index;
            h_index++;
        }  
        if(mrw_landmarks_heuristic) {
            landmarks_index = h_index;
            h_index++;
        } 
        if(mrw_lama_ff_heuristic) {
            lama_ff_index = h_index;
            h_index++;
        }
        if(mrw_lama_ff_s_heuristic) {
            lama_ff_s_index = h_index;
            h_index++;
        }
        if(mrw_lama_ff_c_heuristic) {
            lama_ff_c_index = h_index;
            h_index++;
        }

        // record index of heuristic in mrw
        for(int i = 0; i < g_params_list.size(); i++) {

            if(g_params_list[i]->heur == MRW_Parameters::FD_FF)
                g_params_list[i]->heur_index = ff_index;
            else if(g_params_list[i]->heur == MRW_Parameters::LM)
                g_params_list[i]->heur_index = landmarks_index;
            else if (g_params_list[i]->heur == MRW_Parameters::LAMA_FF)
                g_params_list[i]->heur_index = lama_ff_index;
            else if (g_params_list[i]->heur == MRW_Parameters::LAMA_FF_S)
                g_params_list[i]->heur_index = lama_ff_s_index;
            else if (g_params_list[i]->heur == MRW_Parameters::LAMA_FF_C)
                g_params_list[i]->heur_index = lama_ff_c_index;
        }

        MRW * engine = new MRW(new MTRand_int32(seed));
        add_heuristics(engine);
        seed+= 1984;
		
		engine->search();
		exit(0);
	}
   
    // PERFORMING LAMA SEARCH
    // if no weights added, perform greedy-best first search
    if(lama_params->weights.size() == 0)
        lama_params->weights.push_back(-1);

	int iteration_no = 0;
    bool solution_found = false;
	
	int wastar_weight = -1;

    MTRand_int32 rand_gen(seed);
    seed+= 77;
	
	do {
		cout << endl << endl << "Search iteration " << iteration_no << endl;

        wastar_weight = lama_params->weights[iteration_no];

		// Initialize search engine and heuristics (this is cheap and we want to vary search type
		// and heuristics, so we initialize freshly in each iteration)
		BestFirstSearchEngine* engine;
        if(wastar_weight != -1)
			engine = new WAStarSearchEngine(iteration_no, &rand_gen, wastar_weight);
		else
			engine = new BestFirstSearchEngine(iteration_no, &rand_gen);
            
        engine->set_priority_reward(lama_params->pref_reward);
        engine->set_byte_limit(lama_params->mem_limit);
        cout << "adding heuristics ..."<< endl;
		add_heuristics(engine);

        cout << "Peak Memory after adding heuristics to BFS" << endl;
        print_peak_memory();

		Timer search_timer;
		engine->search();
		search_timer.stop();
        if(engine->found_solution()) {
            save_plan(engine->get_plan(), g_output_filename, true);
        }
		engine->statistics();

		cout << "Search time: " << search_timer << endl;
		cout << "Total time: " << g_timer << endl;
		solution_found |= engine->found_solution();
		
        iteration_no++;
        delete engine;
	}
	while(iteration_no < lama_params->weights.size());

    
	return solution_found ? 0 : 1;
}

void add_heuristics(BestFirstSearchEngine* engine){
	
	if(lama_params->lm_heur || lama_params->lm_preferred) {
		
        if(lama_lm_heur == NULL)
            lama_lm_heur = new LandmarksCountHeuristic(*g_lgraph, lama_params->lm_preferred, new FFHeuristic);

		engine->add_heuristic(lama_lm_heur, lama_params->lm_heur, lama_params->lm_preferred);
	}

    if(lama_params->lama_ff_heur || lama_params->lama_ff_preferred) {    
        
        if(lama_ff_heur == NULL)
            lama_ff_heur = new FFHeuristic;
		
        engine->add_heuristic(lama_ff_heur, lama_params->lama_ff_heur, lama_params->lama_ff_preferred);
	}
    
    if(lama_params->lama_ffs_heur || lama_params->lama_ffs_preferred) {

        if(lama_ffs_heur == NULL)
            lama_ffs_heur = new FFHeuristic(FFHeuristic::PLAN_LENGTH);    
	
        engine->add_heuristic(lama_ffs_heur, lama_params->lama_ffs_heur, lama_params->lama_ffs_preferred);
	}

    if(lama_params->lama_ffc_heur || lama_params->lama_ffc_preferred) {

        if(lama_ffc_heur == NULL)
            lama_ffc_heur = new FFHeuristic(FFHeuristic::PLAN_COST);

		engine->add_heuristic(lama_ffc_heur, lama_params->lama_ffc_heur, lama_params->lama_ffc_preferred);
	}

    if(lama_params->fd_ff_heur || lama_params->fd_ff_preferred) {

        if(lama_fd_ff_heur == NULL)
            lama_fd_ff_heur = new FDFFHeuristic;

		engine->add_heuristic(lama_fd_ff_heur, lama_params->fd_ff_heur, lama_params->fd_ff_preferred);
	}
}

void add_heuristics(MRW* engine){
    if(mrw_fd_ff_heuristic) {    
        engine->add_heuristic(new FDFFHeuristic);
    }  
    if(mrw_landmarks_heuristic) {
        LandmarksCountHeuristic *mrw_lm_heur = new LandmarksCountHeuristic(*g_lgraph, false, new FFHeuristic);
		engine->add_heuristic(mrw_lm_heur);
    } 
    if(mrw_lama_ff_heuristic) {
        engine->add_heuristic(new FFHeuristic);
    }
    if(mrw_lama_ff_s_heuristic) {
        engine->add_heuristic(new FFHeuristic(FFHeuristic::PLAN_LENGTH));
    }
    if(mrw_lama_ff_c_heuristic) {
        engine->add_heuristic(new FFHeuristic(FFHeuristic::PLAN_COST));
    }
}

bool process_command_line(int argc, const char *argv[]) {

    bool iterative_flag = false;
    bool f_prune_flag = false;
    bool aras_flag = false;

    // loop over args
    for(int i = 1; i < argc; ++i) {
        string arg = string(argv[i]);
       
        // get solution file
        if(arg.compare("-o") == 0) {
            i++;
            g_output_filename = string(argv[i]);
        // groups file
	} else if(arg.compare("-g") == 0) {
            i++;
            g_all_groups_file = string(argv[i]);
        // entering a lama configuration
        } else if(arg.compare("-lama_conf") == 0) {
            i++;

            if(lama_params != NULL) {
                cerr << "Cannot enter multiple LAMA configurations" << endl;
                return false;
            }

            if(!lama_param_string_parser(argv[i], lama_params))
                return false;

            if(lama_params->lm_heur || lama_params->lm_preferred)
                generate_landmarks = true;

        // entering a mrw configuration
        } else if(arg.compare("-mrw_conf") == 0) {
            i++;
            MRW_Parameters *param;
            
            if(!mrw_param_string_parser(argv[i], param))
                return false;
            else
                g_params_list.push_back(param);
    
        // entering a file of configurations
        } else if(arg.compare("-conf_file") == 0) {
            i++;

            vector<LAMA_Parameters *> lama_params_list;

            if(!parse_config_file(argv[i], g_params_list, lama_params_list))
                return false;
            
            if(lama_params_list.size() > 1 || (!lama_params_list.empty() && lama_params != NULL)) {
                cerr << "Cannot enter multiple LAMA configurations" << endl;
                return false;
            }

            if(!lama_params_list.empty()) {
                lama_params = lama_params_list.back();
                
                if(lama_params->lm_heur || lama_params->lm_preferred)
                    generate_landmarks = true;
            }

        // entering a random seed
        } else if(arg.compare("-seed") == 0 || /*TODO REMOVE -s option*/ 
                arg.compare("-s") == 0) {
            i++;
            seed = atoi(argv[i]);
        // get restart type
        } else if(arg.compare("-res_type") == 0) {

            if(g_mrw_shared->restart_type != -1) {
                cerr << "Cannot set restart type more than once\n";
                return false; 
            }
            i++;
            string res_type = string(argv[i]);
            
            if(res_type.compare("BASIC") == 0)
                g_mrw_shared->restart_type = Shared_MRW_Parameters::BASIC;
            else if(res_type.compare("SMART") == 0)
                g_mrw_shared->restart_type = Shared_MRW_Parameters::S_RESTART;
            else {
                cerr << "Invalid restart type entered" << endl;
            }
        // get pool activation
        } else if(arg.compare("-pool_act") == 0) {
            if(g_mrw_shared->act_level != -1) {
                cerr << "Cannot set pool activation level more than once\n";
                return false;
            }
            i++;
            int value = atoi(argv[i]);
            if(value <= 0) {
                cerr << "Invalid pool activation entered" << endl;
                return false;
            }
            g_mrw_shared->act_level = value;
        // get pool size
        } else if(arg.compare("-pool_size") == 0) {
            if(g_mrw_shared->pool_size != -1) {
                cerr << "Cannot set pool size more than once\n";
                return false;
            }
            i++;
            int value = atoi(argv[i]);
            if(value <= 0) {
                cerr << "Invalid pool size entered" << endl;
                return false;
            }
            g_mrw_shared->pool_size = value;
        } else if(arg.compare("-iterative") == 0) {

            // prevent duplicates
            if(iterative_flag) {
                cerr << "Can't set -iterative multiple times " << endl;
                return false;
            }
            g_mrw_shared->iterative = true;
            iterative_flag = true;
        } else if(arg.compare("-ucb_const") == 0) {
            if(g_mrw_shared->ucb_const != -1) {
                cerr << "Can't set ucb constant value multiple times" << endl;
                return false;
            }
            i++;
            float value = atof(argv[i]);
            if(value <= 0) {
                cerr << "Invalid UCB constant entered" << endl;
                return false;
            }
            g_mrw_shared->ucb_const = value;
        } else if(arg.compare("-run_aras") == 0) {

            // prevent duplicates
            if(aras_flag) {
                cerr << "Can't set -run_aras multiple times " << endl;
                return false;
            }
            g_mrw_shared->run_aras = true;
            aras_flag = true;
        } else if(arg.compare("-aras_mem") == 0) {
            if(g_mrw_shared->aras_byte_limit != -1) {
                cerr << "Can't set aras memory limit multiple times" << endl;
                return false;
            }
            i++;
            int value = atoi(argv[i]);
            if(value <= 0) {
                cerr << "Invalid aras memory limit entered" << endl;
                return false;
            }
            g_mrw_shared->aras_byte_limit = value;
        } else if(arg.compare("-aras_time") == 0) {
            if(g_mrw_shared->aras_time_limit != -1) {
                cerr << "Can't set aras time limit multiple times" << endl;
                return false;
            }
            i++;
            int value = atoi(argv[i]);
            if(value <= 0) {
                cerr << "Invalid aras time limit entered" << endl;
                return false;
            }
            g_mrw_shared->aras_time_limit = value;
        } else {
            // invalid entry
            cerr << "unknown option:" << argv[i] << " entered" << endl;
            return false;
        }
    }

    // ensure g_smart_restarts set properly
    if(g_mrw_shared->restart_type != Shared_MRW_Parameters::S_RESTART) {
        if(g_mrw_shared->act_level != -1) {
            cerr << "Restart activation level set without smart restarting\n";
            return false;
        } else if(g_mrw_shared->pool_size != -1) {
            cerr << "Pool size set without smart restarting\n";
            return false;
        }
    }

    // if no config was entered, use default mrw config
    if(lama_params == NULL && g_params_list.size() == 0) {
        
        MRW_Parameters* default_mrw_params = new MRW_Parameters();    
        default_mrw_params->set_unset_params();
        g_params_list.push_back(default_mrw_params);
    }

    if(!g_params_list.empty()) {
        monte_carlo_rw = true;
        g_mrw_shared->set_unset_params();
    } else {
        // entered MRW config global info, but no mrw config
        if(g_mrw_shared->restart_type != -1 ||
                g_mrw_shared->act_level != -1 ||
                g_mrw_shared->pool_size != -1 ||
                g_mrw_shared->ucb_const != -1 ||
                f_prune_flag ||
                iterative_flag) {
            cerr << "Restart information not applicable for lama" << endl;
            return false;
        }
    }

    // make sure generate all applicable heuristics
    for(int i = 0; i < g_params_list.size(); i++) {

        if(g_params_list[i]->heur == MRW_Parameters::FD_FF) {
            mrw_fd_ff_heuristic = true;
        } else if(g_params_list[i]->heur == MRW_Parameters::LM) {
            generate_landmarks = true;
            mrw_landmarks_heuristic = true;
            if(g_params_list[i]->walk_type == MRW_Parameters::MHA) {
                cerr << "MHA not capable of handling landmarks" << endl;
                return false;
            }
        } else if(g_params_list[i]->heur == MRW_Parameters::LAMA_FF) {
            mrw_lama_ff_heuristic = true;
        } else if(g_params_list[i]->heur == MRW_Parameters::LAMA_FF_S) {
            mrw_lama_ff_s_heuristic = true;
        } else if(g_params_list[i]->heur == MRW_Parameters::LAMA_FF_C) {
            mrw_lama_ff_c_heuristic = true;
        }
        
    }

    return true;
}

void output_help_information() {
    cerr << "search [options] -g all.groups -o sol_file < output" << endl;
    cerr << "options can include the following: " << endl;
    cerr << "\t-mrw_conf \"CONF\" : for entering a MRW configuration (see below)" << endl;
    cerr << "\t-lama_conf \"CONF\" : for entering a lama configuration (see below)" << endl;
    cerr << "\t-conf_file FILE : takes in a file of configurations with one config" << endl;
    cerr << "\t\tper line. Each line should begin with -lama_conf or -mrw_conf." << endl;
    cerr << "\t\tConfiguration input is the same as command line config input" << endl;
    cerr << "\t-seed n : random number seed set to n" << endl;
    cerr << "\t-res_type BASIC|SMART : changes restart type to desired type" << endl;
    cerr << "\t-pool_act n : changes activation level to n" << endl;
    cerr << "\t-pool_size n : changes pool_size to n" << endl;
    cerr << "\t-iterative : turns on iteratively running mrw" << endl;
    cerr << "\t-ucb_const n : the ucb constant to use" << endl;
    cerr << "\t-run_aras : uses aras to improve plans found" << endl;
    cerr << "\t-aras_mem n : limits aras memory usage to n bytes. No limit by default" << endl;
    cerr << "\t-aras_time n : limits aras time limit to n bytes. No limit by default" << endl;
    
    cerr << endl;
    cerr << "-mrw_conf \"CONF\": CONF is an entered MRW configuration." << endl;
    cerr << "CONF includes options of the following form: " << endl;
    cerr << "\t-nodeep : changes deepening to false" << endl;
    cerr << "\t-bounding NONE|F|G: no prunning, f-pruning and g-pruning, respectively  " << endl;
    cerr << "\t-tie_break : switchs tie-breaking on" << endl;
	cerr << "\t-conservative : uses conservative steps" << endl;
	cerr << "\t-alpha n : changes alpha to n" << endl;
    cerr << "\t-e_rate n : changes extending_rate to n" << endl;
    cerr << "\t-e_period n : changes extending_period to n" << endl;
    cerr << "\t-walk_temp n : changes the walk bias temperature to n" << endl;
    cerr << "\t-len_walk n : changes initial length of walks to n" << endl;
    cerr << "\t-walk_type PURE|MDA|MHA : changes walk type to desired type" << endl;
    cerr << "\t-num_walk n : changes maximum length of walks to n" << endl;
    cerr << "\t-len_jump n : changes length of jumps to min(n, len_walk)" << endl;
    cerr << "\t-max_steps n : changes maximum number of steps without progress before restarting to n" << endl;
    cerr << "\t-stdev n : changes standard deviation of walk length to desired type" << endl;
    cerr << "\t-heur FD_FF|LM|LAMA_FF|LAMA_FF_S|LAMA_FF_C : changes heuristic to desired type" << endl;
    cerr << "\t\tNOTE: FD_FF and FF result in the same heuristic" << endl;
    cerr << "\t-bounding [NONE|G_PRUNNING|F_PRUNNING] : changes solution bounding to given value" << endl;
    cerr << "\t-step_type STATE|PATH|H_PATH : changes step type" << endl;
    cerr << endl;
    cerr << "-lama_conf \"CONF\": CONF is an entered Lama configuration." << endl;
    cerr << "CONF includes options of the following form: " << endl;
    cerr << "\t-heur LAMA_FF|LM|FD_FF|LAMA_FF_S|LAMA_FF_C : the heuristic" << endl;
    cerr << "\t-pref LAMA_FF|LM|FD_FF : preferred operators" << endl; 
    cerr << "\t-weight_list [w1,w2,...,wn] : wi's are weights that will be used in order" << endl;
    cerr << "\t-rand_open : sets randomization of generated nodes" << endl;
    cerr << "\t-p_reward n : sets the preference priority reward to n" << endl;
    cerr << "\t-mem_limit n : limits the memory usage of lama to be only n bytes" << endl;
    cerr << endl;
}

