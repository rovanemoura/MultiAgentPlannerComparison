#include "parameters.h"

#include <iostream>

using namespace std;

MRW_Parameters::MRW_Parameters() {
    alpha = -1;
    extending_rate = -1;
    extending_period = -1;
    walk_bias_temp = -1;
    length_walk = -1;
    walk_type = -1;
    num_walk = -1;
    length_jump = -1;
    max_steps = -1;
    step_type = -1;
    bounding = -1;
    deepening = DEFAULT_DEEP;
    tie_breaking = DEFAULT_TIE;
    h_path_temp = DEFAULT_HP_TEMP;
    conservative_steps = DEFAULT_CONS;
    bounding_factor = -1;

    length_stdev = -1;
    heur = -1;
    heur_index = -1;
}

MRW_Parameters::MRW_Parameters(float r, int l, int t) : extending_rate(r),  length_walk(l), walk_type(t){

    if(walk_type == MRW_Parameters::MHA)
        walk_bias_temp = DEFAULT_MHA_TEMP;
    else if(walk_type == MRW_Parameters::MDA)
        walk_bias_temp = DEFAULT_MDA_TEMP;

    step_type = DEFAULT_STEP_TYPE;
    alpha = DEFAULT_ALPHA;
	extending_period = DEFAULT_E_PERIOD;
	num_walk = DEFAULT_NUM_WALK;
	length_jump = length_walk;
	max_steps = DEFAULT_MAX_STEPS;
	deepening = DEFAULT_DEEP;
	bounding = DEFAULT_BOUNDING;
    length_stdev = DEFAULT_STDEV;
    bounding_factor = DEFAULT_BD_FACTOR;
    heur = DEFAULT_HEUR;
}

MRW_Parameters::MRW_Parameters(const MRW_Parameters &p) {
    
    alpha = p.alpha;
    extending_rate = p.extending_rate;
    extending_period = p.extending_period;
    walk_bias_temp = p.walk_bias_temp;
    length_walk = p.length_walk;
    walk_type = p.walk_type;
    num_walk = p.num_walk;
    length_jump = p.length_jump;
    max_steps = p.max_steps;
    deepening = p.deepening;
    step_type = p.step_type;
    tie_breaking = p.tie_breaking;
    h_path_temp = p.h_path_temp;
    conservative_steps = p.conservative_steps;
    length_stdev = p.length_stdev;
    heur = p.heur;
    bounding = p.bounding;
    heur_index = p.heur_index;
    bounding_factor = p.bounding_factor;
}

void MRW_Parameters::set_unset_params() {

    if(alpha == -1)
        alpha = DEFAULT_ALPHA;
    if(extending_rate == -1)
         extending_rate = DEFAULT_E_RATE;
    if(extending_period == -1)
        extending_period = DEFAULT_E_PERIOD;
    if(walk_type == -1)
        walk_type = DEFAULT_WALK_TYPE;
    
    if(length_jump == -1) {
        // if no length jump entered but length walk was,
        // set as length walk, ow use default
        if(length_walk != -1)
            length_jump = length_walk;
        else
            length_jump = DEFAULT_JUMP;
    }
    if(length_walk == -1)
        length_walk = DEFAULT_LEN_WALK;

    // if length_jump is too large
    if(length_jump > length_walk)
        length_jump = length_walk;
    
    if(step_type == -1)
    	step_type = DEFAULT_STEP_TYPE;
    if(num_walk == -1)
        num_walk = DEFAULT_NUM_WALK;
    if(bounding == -1)
    	bounding = DEFAULT_BOUNDING;
    if(bounding_factor == -1)
    	bounding_factor = DEFAULT_BD_FACTOR;

    if(max_steps == -1)
        max_steps = DEFAULT_MAX_STEPS;
    if(length_stdev == -1)
        length_stdev = DEFAULT_STDEV;
    if(heur == -1)
        heur = DEFAULT_HEUR;

    if(walk_bias_temp == -1 &&
            walk_type == MRW_Parameters::MHA)
        walk_bias_temp = DEFAULT_MHA_TEMP;
    if(walk_bias_temp == -1 &&
            walk_type == MRW_Parameters::MDA)
        walk_bias_temp = DEFAULT_MDA_TEMP;
}

void MRW_Parameters::print_values() {
    
    cout << "Walk Type: ";
    if(walk_type == MRW_Parameters::PURE)
        cout << "PURE" << endl;
    else if(walk_type == MRW_Parameters::MDA)
        cout << "MDA" << endl;
    if(walk_type == MRW_Parameters::MHA)
        cout << "MHA" << endl;

    cout << "Heuristic: ";

    if(heur == MRW_Parameters::FD_FF)
        cout << "FD FF" << endl;
    else if(heur == MRW_Parameters::LM)
        cout << "LM" << endl;
    else if(heur == MRW_Parameters::LAMA_FF)
        cout << "LAMA FF" << endl;
    else if(heur == MRW_Parameters::LAMA_FF_S)
        cout << "LAMA FFs" << endl;
    else if(heur == MRW_Parameters::LAMA_FF_C)
        cout << "LAMA FFc" << endl;

    cout << "Step Type: ";
    if(step_type == MRW_Parameters::STATE)
        cout << "STATE" << endl;
    else if(step_type == MRW_Parameters::PATH)
        cout << "PATH" << endl;
    if(step_type == MRW_Parameters::H_PATH)
        cout << "H_PATH" << endl;

    cout << "Extending Rate: " << extending_rate << endl;
    cout << "Length Walk: " << length_walk << endl;

    cout << "Alpha: " << alpha << endl;
    cout << "Extending Period: " << extending_period << endl;
    cout << "Walk Bias Temp: " << walk_bias_temp << endl;
    cout << "Num Walks: " << num_walk << endl;
    cout << "Jump Length: " << length_jump << endl;
    cout << "Max Steps: " << max_steps << endl;
    cout << "Bounding Factor: " << bounding_factor << endl;
    cout << "Deepening: ";
    if(deepening)
        cout << "true" << endl;
    else
        cout << "false" << endl;

    cout << "Tie-breaking: ";
    if(tie_breaking)
        cout << "true" << endl;
    else
        cout << "false" << endl;

    cout << "Conservative Steps: ";
    if(conservative_steps)
        cout << "true" << endl;
    else
        cout << "false" << endl;

    cout << "solution_bounding: ";
    if(bounding == MRW_Parameters::NONE)
        cout << "none" << endl;
    else if(bounding == MRW_Parameters::G_PRUNNING)
        cout << "g_prunning" << endl;
    else if(bounding == MRW_Parameters::F_PRUNNING)
        cout << "f_prunning" << endl;



    cout << "Length STDev: " << length_stdev << endl;
}

LAMA_Parameters::LAMA_Parameters() {
    lama_ff_heur = false;
    lama_ff_preferred = false;

    lama_ffc_heur = false;
    lama_ffc_preferred = false;

    lama_ffs_heur = false;
    lama_ffs_preferred = false;

    fd_ff_heur = false;
    fd_ff_preferred = false;

    lm_heur = false;
    lm_preferred = false;

    rand_open = false;
    
    weights.clear();

    pref_reward = DEFAULT_PREF_REWARD;
    mem_limit = DEFAULT_MEM_LIMIT;
}

LAMA_Parameters::LAMA_Parameters(const LAMA_Parameters &p) {
    lama_ff_heur = p.lama_ff_heur;
    lama_ff_preferred = p.lama_ff_preferred;

    lama_ffc_heur = p.lama_ffc_heur;
    lama_ffc_preferred = p.lama_ffc_preferred;

    lama_ffs_heur = p.lama_ffs_heur;
    lama_ffs_preferred = p.lama_ffs_preferred;

    fd_ff_heur = p.fd_ff_heur;
    fd_ff_preferred = p.fd_ff_preferred;
        
    lm_heur = p.lm_heur;
    lm_preferred = p.lm_preferred;
        
    rand_open = p.rand_open;

    pref_reward = p.pref_reward;
            
    mem_limit = p.mem_limit;

    for(unsigned i = 0; i < p.weights.size(); i++)
        weights.push_back(p.weights[i]);
}

void LAMA_Parameters::print_values() {
    cout << "Use LAMA FF: ";
    if(lama_ff_heur)
        cout << "true" << endl;
    else
        cout << "false" << endl;

    cout << "Use LAMA FF Preferred Ops: ";
    if(lama_ff_preferred)
        cout << "true" << endl;
    else
        cout << "false" << endl;

    cout << "Use LAMA FFc: ";
    if(lama_ffc_heur)
        cout << "true" << endl;
    else
        cout << "false" << endl;

    cout << "Use LAMA FFc Preferred Ops: ";
    if(lama_ffc_preferred)
        cout << "true" << endl;
    else
        cout << "false" << endl;
    
    cout << "Use LAMA FFs: ";
    if(lama_ffs_heur)
        cout << "true" << endl;
    else
        cout << "false" << endl;

    cout << "Use LAMA FFs Preferred Ops: ";
    if(lama_ffs_preferred)
        cout << "true" << endl;
    else
        cout << "false" << endl;
    
    cout << "Use FD FF: ";
    if(fd_ff_heur)
        cout << "true" << endl;
    else
        cout << "false" << endl;

    cout << "Use FD FF Preferred Ops: ";
    if(fd_ff_preferred)
        cout << "true" << endl;
    else
        cout << "false" << endl;


    cout << "Use LM: ";
    if(lm_heur)
        cout << "true" << endl;
    else
        cout << "false" << endl;
    
    cout << "Use LM Preferred Ops: ";

    if(lm_preferred)
        cout << "true" << endl;
    else
        cout << "false" << endl;


    cout << "Randomizing Generated States: ";
    if(rand_open)
        cout << "true" << endl;
    else
        cout << "false" << endl;

    cout << "Preference Priority Reward: " << pref_reward << endl;

    cout << "Memory Limit: " << mem_limit << endl;
    if(weights.size() == 0)
        cout << "Using Greedy Best-first search" << endl;
    else if(weights.size() == 1)
        cout << "Using Weighted Search with weight " << weights[0] << endl;
    else {
        cout  << "Using Iterative Weighted Search with the following weights in order: " 
            << endl << "\t";
        for(unsigned i = 0; i < weights.size(); i++) {
            cout << weights[i];
            cout << " ";
        }
        cout << endl;
    }
}

void Shared_MRW_Parameters::set_unset_params() {
    if(restart_type == -1)
        restart_type = DEFAULT_RES_TYPE;
    if(pool_size == -1 && restart_type == S_RESTART)
        pool_size = DEFAULT_POOL_SIZE;
    if(act_level == -1 && restart_type == S_RESTART)
        act_level = DEFAULT_POOL_ACT;
    if(ucb_const == -1)
        ucb_const = DEFAULT_UCB_CONST;
    if(aras_byte_limit == -1)
        aras_byte_limit = DEFAULT_ARAS_MEM;
}

void Shared_MRW_Parameters::print_values() {
    cout << "Shared MRW Parameters: ";

    if(restart_type == S_RESTART) {
        cout << "SMART" << endl;

        cout << "\tPool Size: " << pool_size << endl;
        cout << "\tActivation Level: " << act_level << endl;
    } else if(restart_type == BASIC) 
        cout << "BASIC" << endl;

    cout << "Iterative search: ";
    if(iterative)
        cout << "true" << endl;
    else
        cout << "false" << endl;
        
    cout << "UCB Constant Value: " << ucb_const << endl;

    cout << "Use Aras: ";
    if(run_aras) {
        cout << "true" << endl;
        cout << "regression graph: ";
	if(reg_aras){
	    cout << "true" << endl;
	}else
	    cout << "false" << endl;
        cout << "\tAras Memory Limit: ";

        if(aras_byte_limit == -1)
            cout << "NONE" << endl;
        else
            cout << aras_byte_limit << endl;

        cout << "\tAras Time Limit: ";

        if(aras_time_limit == -1)
            cout << "NONE" << endl;
        else
            cout << aras_time_limit << endl;
    }else{
        cout << "false" << endl;
    }
}

