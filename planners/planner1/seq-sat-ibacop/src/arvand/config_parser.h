#ifndef CONF_PARSER
#define CONF_PARSER

#include "string_utils.h"
#include "mrw.h"

/**
 * Statuses used for configuration parsing
 */
enum {S_DUP_STATUS = -2,
    S_BAD_STATUS = -1,
    S_NEW = 0, 
    S_ALPHA = 1, 
    S_E_RATE = 2, 
    S_E_PERIOD = 3, 
    S_TEMP = 4, 
    S_LENGTH_WALK = 6, 
    S_WALK_TYPE = 7, 
    S_NUM_WALK = 8, 
    S_L_JUMP= 9,
    S_MAX_STEPS = 10, 
    S_STDEV = 14,
    S_HEUR = 15,
	S_STEP_TYPE = 16,
	S_BOUNDING = 17};

enum {S_LAMA_DUP_STATUS = -2,
    S_LAMA_BAD_STATUS = -1,
    S_LAMA_NEW = 0,
    S_LAMA_HEUR = 1,
    S_LAMA_W_LIST = 2,
    S_LAMA_PREFERRED = 3,
    S_LAMA_P_REWARD = 4,
    S_LAMA_MEM_LIMIT = 5};

/**
 * Parses a string representing a set of parameter settings. Unset 
 * values are set as a default.
 *
 * -nodeep : changes deepening to false
 * -alt : changes alternating to true
 * -alpha n : changes alpha to n
 * -e_rate n : changes extending_rate to n
 * -e_period n : changes extending_period to n
 * -mha_temp n : changes mha_temperature to n
 * -mda_temp n : changes mda_temperature to n
 * -len_walk n : changes length_walk to n
 * -walk_type [PURE|MDA|MHA|MIXED] : changes walk type to desired type
 * -num_walk n : changes number of walks to n
 * -len_jump n : changes length_jump to min(n, len_walk)
 * -max_steps n : changes max_steps to n
 * -pool_act n : changes sr_n to n
 * -pool_size n : changes pool_size to n
 * -res_type [BASIC|SMART] : changes restart type to desired type
 * -stdev n : changes standard deviation of walk length to desired type
 * -heur [FF|LM] : changes heuristic to desired type
 */
bool mrw_param_string_parser(std::string conf_string, MRW_Parameters * &param);

bool lama_param_string_parser(std::string conf_string, LAMA_Parameters * &param);

/**
 * Reads a walk type list from a string. String is assumed
 * to have the form [x1,x2,x3,...,xn] where each of the xi's
 * is one of PURE, MDA, or MHA.
 * Returns true if the line is successfully parsed and false
 * otherwise.
 */
bool get_walk_type_list(string str_list, std::vector<int> &list);
bool get_mrw_bounding_type(string str_type, MRW_Parameters * param);

/**
 * Parses a configuration file. Lines should begin with -mrw_conf
 * or -lama_conf
 *
 * filename - name of configuration file
 * mrw_params - place to store new mrw params
 * lama_params - place to store new lama params
 */
bool parse_config_file(const char * filename, std::vector<MRW_Parameters *> &mrw_params, 
        std::vector<LAMA_Parameters *> &lama_params);
#endif

