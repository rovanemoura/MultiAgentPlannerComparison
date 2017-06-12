#ifndef PARAMS_H
#define PARAMS_H

#include <vector>

// default values for MRW parameters
#define DEFAULT_ALPHA 0.9
#define DEFAULT_E_RATE 1.5
#define DEFAULT_E_PERIOD 0.1
#define DEFAULT_MHA_TEMP 10
#define DEFAULT_MDA_TEMP 0.5
#define DEFAULT_HP_TEMP 0.5
#define DEFAULT_BD_FACTOR 0.9
#define DEFAULT_LEN_WALK 1
#define DEFAULT_WALK_TYPE PURE
#define DEFAULT_STEP_TYPE STATE
#define DEFAULT_NUM_WALK 2000
#define DEFAULT_JUMP 1
#define DEFAULT_MAX_STEPS 7
#define DEFAULT_DEEP true
#define DEFAULT_HEUR FD_FF
#define DEFAULT_STDEV 0
#define DEFAULT_TIE false 
#define DEFAULT_CONS false
#define DEFAULT_BOUNDING NONE

// default values for shared MRW parameters
#define DEFAULT_RES_TYPE BASIC
#define DEFAULT_POOL_ACT 50
#define DEFAULT_POOL_SIZE 50
#define DEFAULT_ITERATIVE false
#define DEFAULT_UCB_CONST 0.2
#define DEFAULT_ARAS_MEM 2000000
#define DEFAULT_ARAS_REG true 
// default lama parameters
#define DEFAULT_PREF_REWARD 1000
#define DEFAULT_MEM_LIMIT 2000000
class MRW_Parameters{
// This class is a container for the parameters used in MRW
	
public:	
	enum {PURE = 0, MDA = 1, MHA = 2}; 
    enum {FD_FF = 0, LM = 1, LAMA_FF = 2, LAMA_FF_S = 3, LAMA_FF_C = 4};
    enum {STATE = 0, PATH = 1, H_PATH = 2};
    enum {NONE = 0, G_PRUNNING = 1, F_PRUNNING = 2};

	float alpha;
	float extending_rate;
	float extending_period;
    float walk_bias_temp;
    float h_path_temp;
    float bounding_factor;
	int length_walk;
	int walk_type;
	int step_type;

	int num_walk;
	int length_jump;
	int max_steps;
	bool deepening;
	bool tie_breaking;
	bool conservative_steps;
	int bounding;

	
    int heur;
    int heur_index;
    float length_stdev;

    /**
     * Sets all params to a dummy variable (-1).
     * deepening and alternating set to default values.
     */
	MRW_Parameters();

    /**
     * Sets all values to default values except the
     * extending rate (r), the walk length (l), and the
     * walk type (t)
     */
	MRW_Parameters(float r, int l, int t);

    /**
     * Copies all values over
     **/
    MRW_Parameters(const MRW_Parameters &p);   

    /**
     * Fills in params set as dummy value with the default value
     */
    void set_unset_params();

    /**
     * Prints the values of the parameters
     */
    void print_values();
};


class LAMA_Parameters{
// This class is a container for the parameters used in MRW
	
public:
    // 2008 LAMA heuristic
    bool lama_ff_heur;
    bool lama_ff_preferred;

    // relaxed plan cost
    bool lama_ffc_heur;
    bool lama_ffc_preferred;

    // relaxed plan length
    bool lama_ffs_heur;
    bool lama_ffs_preferred;

    // fast downward heuristic
    bool fd_ff_heur;
    bool fd_ff_preferred;

    // landmark count heuristic
    bool lm_heur;
    bool lm_preferred;

    std::vector<int> weights;
    bool rand_open;

    int pref_reward;
    int mem_limit;

    LAMA_Parameters();

    LAMA_Parameters(const LAMA_Parameters &p);

    /**
     * Prints the values of the parameters
     */
    void print_values();
};

class Shared_MRW_Parameters {
// this class is a container for params related to restarting
//
public:
	enum {BASIC = 0, S_RESTART = 1};
    
    int restart_type;
    int pool_size;
    int act_level;
    bool iterative;
    float ucb_const;
    bool run_aras;
    bool reg_aras;
    int aras_byte_limit;
    int aras_time_limit;

    Shared_MRW_Parameters() {
        restart_type = -1;
        pool_size = -1;
        act_level = -1;
        ucb_const = -1;
        run_aras = false;
        reg_aras = DEFAULT_ARAS_REG;  
        aras_byte_limit = -1;
        aras_time_limit = -1;
        iterative = DEFAULT_ITERATIVE;
    }

    void print_values();

    void set_unset_params();
};
#endif

