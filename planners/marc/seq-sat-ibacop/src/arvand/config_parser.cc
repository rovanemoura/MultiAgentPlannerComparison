#include "config_parser.h"
#include <iostream>
#include <fstream>

// Helper Functions
int get_mrw_status(string str_status, MRW_Parameters *param);
bool get_mrw_walk_type(string str_type, MRW_Parameters * param);
bool get_mrw_step_type(string str_type, MRW_Parameters * param);
bool get_mrw_heur_type(string str_heur, MRW_Parameters *param);
bool extract_mrw_float_value(string str, MRW_Parameters *param, int status);
bool extract_mrw_int_value(string str, MRW_Parameters *param, int status);

bool get_weight_list(string str_list, std::vector<int> &list);
int get_lama_status(string str_status);
bool get_lama_heuristic(string str_heur, LAMA_Parameters *param);
bool get_lama_preferred(string str_heur, LAMA_Parameters *param);



bool mrw_param_string_parser(std::string conf_string, MRW_Parameters * &param) {

    param = new MRW_Parameters();

    bool deep_flag = false;
    bool tie_flag = false;
    bool conservative_flag = false;
    std::vector<std::string> tokens = split(conf_string, ' ');

    int status = S_NEW;
    for(int i = 0; i < tokens.size(); i++) {
        
        // TODO handle other whitespace
        if(tokens[i].size() == 0)
            continue;

        // change status to look for corresponding value
        if(tokens[i][0] == '-') {
            if(status != S_NEW) {
                cerr << "MRW Config arg " << tokens[i] << " called improperly" << endl;
                delete param;
                return false;
            }

            // handle boolean flags
            if(tokens[i].compare("-nodeep") == 0) {
                
                // prevent duplicates
                if(deep_flag) {    
                    cerr << "Can't set -nodeep multiple times " << endl;
                    delete param;
                    return false;
                }
                param->deepening = false;
                deep_flag = true;
            } else if(tokens[i].compare("-tie_break") == 0) {
                // prevent duplicates
                if(tie_flag) {
                    cerr << "Can't set -tie_break multiple times " << endl;
                    delete param;
                    return false;
                }
                param->tie_breaking = true;
                tie_flag = true;
            } else if(tokens[i].compare("-conservative") == 0) {
                // prevent duplicates
                if(conservative_flag) {
                    cerr << "Can't set -conservative multiple times " << endl;
                    delete param;
                    return false;
                }
                param->conservative_steps = true;
                conservative_flag = true;
            }else {
                status = get_mrw_status(tokens[i], param);
                if(status == S_BAD_STATUS) {
                    cerr << "Invalid config option " << tokens[i] << endl;
                    delete param;
                    return false;
                } else if(status == S_DUP_STATUS) {
                    cerr << "Can't set " << tokens[i] << "option twice in same config" << endl;
                    delete param;
                    return false;
                }
            }


        } else {
             
            // handle float extraction
            if(status == S_ALPHA || status == S_E_RATE || 
                    status == S_E_PERIOD || status == S_STDEV) {
           
                if(!extract_mrw_float_value(tokens[i], param, status)) {
                    cerr << "Entered invalid config value of " << tokens[i] << " for arg " << tokens[i - 1] << endl;
                    delete param;
                    return false;
                }


            } // hand int extraction 
            else if(status == S_LENGTH_WALK || status == S_NUM_WALK ||
                    status == S_L_JUMP || status == S_MAX_STEPS) {

                if(!extract_mrw_int_value(tokens[i], param, status)) {
                    cerr << "Entered invalid config value of " << tokens[i] << " for arg" << tokens[i - 1] << endl;
                    delete param;
                    return false;
                }
            } // handle walk type 
            else if(status == S_WALK_TYPE){

                if(!get_mrw_walk_type(tokens[i], param)) {
                    cerr << "Entered invalid config walk type of " << tokens[i] << endl;
                    delete param;
                    return false;
                }
            } // handle step type
            else if(status == S_STEP_TYPE){

                if(!get_mrw_step_type(tokens[i], param)) {
                    cerr << "Entered invalid config step type of " << tokens[i] << endl;
                    delete param;
                    return false;
                }
            }// handle heuristic type
            else if(status == S_HEUR) {
                 if(!get_mrw_heur_type(tokens[i], param)) {
                    cerr << "Entered invalid config heuristic type of " << tokens[i] << endl;
                    delete param;
                    return false;
                 }
            } else if(status == S_NEW) {
                cerr << "Invalid conf arg given: " << tokens[i] << endl;
                delete param;
                return false;
            } else if (status == S_BOUNDING){
                if(!get_mrw_bounding_type(tokens[i], param)) {
                   cerr << "Entered invalid config bounding type of " << tokens[i] << endl;
                   delete param;
                   return false;
                }
            }

            // reset as ready for new status
            status = S_NEW;
        }
    }

    param->set_unset_params();
    return true;
}


bool lama_param_string_parser(std::string conf_string, LAMA_Parameters * &param) {

    if(param == NULL)
        param = new LAMA_Parameters();
        
    std::vector<std::string> tokens = split(conf_string, ' ');
    int status = S_LAMA_NEW;
  
    bool rand_open_flag = false;
    bool p_reward_flag = false;
    bool mem_limit_flag = false;

    for(int i = 0; i < tokens.size(); i++) {
    
        // TODO handle other whitespace
        if(tokens[i].size() == 0)
            continue;

        // change status to look for corresponding value
        if(tokens[i][0] == '-') {
            if(status != S_NEW) {
                cerr << "Lama config arg " << tokens[i] << " called improperly" << endl;
                delete param;
                return false;
            } else if(tokens[i].compare("-rand_open") == 0) {

                // prevent duplicates
                if(rand_open_flag) {
                    cerr << "Can't set -rand_open multiple times " << endl;
                    delete param;
                    return false;
                }
                param->rand_open = false;
                rand_open_flag = true;
            } else {
                   
                status = get_lama_status(tokens[i]);

                if(status == S_LAMA_BAD_STATUS) {
                    cerr << "Invalid lama config option " << tokens[i] << endl;
                    delete param;
                    return false;
                } else if(status == S_LAMA_DUP_STATUS) {
                    cerr << "Can't set " << tokens[i] << " option twice in same lama config" << endl;
                    delete param;
                    return false;
                }
            }
        } else {

            if(status == S_LAMA_HEUR) {
                if(!get_lama_heuristic(tokens[i], param)) {
                    delete param;
                    return false;
                 }
            } else if(status == S_LAMA_W_LIST) {
                if(!get_weight_list(tokens[i], param->weights)) {
                    cerr << "Error found in Lama config" << endl;
                    delete param;
                    return false;
                }
            } else if(status == S_LAMA_PREFERRED) { 
                if(!get_lama_preferred(tokens[i], param)) {
                    delete param;
                    return false;
                }
            } else if(status == S_LAMA_MEM_LIMIT) {
    
                //TODO Do this better
                int value = atoi(tokens[i].c_str());

                if(value <= 0) {
                    delete param;
                    cerr << "Invalid LAMA Memory Limit entered entered" << endl;
                    return false;
                }

                // prevent duplicates
                if(mem_limit_flag) {
                    cerr << "Can't set -mem_limit multiple times " << endl;
                    delete param;
                    return false;
                }
                mem_limit_flag = true;
                param->mem_limit = value;
            
            } else if(status == S_LAMA_P_REWARD) {
    
                //TODO Do this better
                int value = atoi(tokens[i].c_str());

                if(value == 0 && tokens[i].compare("0") != 0 ) {
                    delete param;
                    cerr << "Invalid Preference Priority Reward entered" << endl;
                    return false;
                }

                // prevent duplicates
                if(p_reward_flag) {
                    cerr << "Can't set -rand_open multiple times " << endl;
                    delete param;
                    return false;
                }
                p_reward_flag = true;
                param->pref_reward = value;
            
            } else if(status == S_LAMA_NEW){
                cerr << "Invalid lama config arg given: " << tokens[i] << endl;
                delete param;
                return false;
            }

            status = S_LAMA_NEW;
        }
    }    

    // no lama heuristics entered
    if(!param->lama_ff_heur && !param->lama_ffs_heur && !param->lama_ffc_heur 
        && !param->fd_ff_heur && !param->lm_heur) {
        
        cerr << "Can't give a lama config without any heuristics" << endl;
        delete param;
        return false;    
    }
    
    if((param->lama_ff_preferred && param->lama_ffs_preferred) ||
       (param->lama_ffs_preferred && param->lama_ffc_preferred) ||
       (param->lama_ff_preferred && param->lama_ffc_preferred)) {
        
        cerr << "WARNING: Have entered duplicate preferred operators" << endl;   
    }
    
    return true;
}


/** 
 * Gets the status indicated by the string
 */
int get_mrw_status(string str_status, MRW_Parameters *param) {

    if(str_status.compare("-alpha") == 0) {
        if(param->alpha != -1)
            return S_DUP_STATUS;
        else
            return S_ALPHA;
    } else if(str_status.compare("-e_rate") == 0) {
        if(param->extending_rate != -1)
            return S_DUP_STATUS;
        else
            return S_E_RATE;
    } else if(str_status.compare("-e_period") == 0) {
        if(param->extending_period != -1)
            return S_DUP_STATUS;
        else
            return S_E_PERIOD;
    } else if(str_status.compare("-walk_temp") == 0) {
        if(param->walk_bias_temp != -1)
            return S_DUP_STATUS;
        else
            return S_TEMP;
    } else if(str_status.compare("-len_walk") == 0) {
        if(param->length_walk != -1)
            return S_DUP_STATUS;
        else
            return S_LENGTH_WALK;
    } else if(str_status.compare("-walk_type") == 0) {
        if(param->walk_type != -1)
            return S_DUP_STATUS;
        else
            return S_WALK_TYPE;

    } else if(str_status.compare("-step_type") == 0) {
        if(param->step_type != -1)
            return S_DUP_STATUS;
        else
            return S_STEP_TYPE;

    } else if(str_status.compare("-num_walk") == 0) {
        if(param->num_walk != -1)
            return S_DUP_STATUS;
        else
            return S_NUM_WALK;
    } else if(str_status.compare("-len_jump") == 0) {
        if(param->length_jump != -1)
            return S_DUP_STATUS;
        else
            return S_L_JUMP;
    } else if(str_status.compare("-max_steps") == 0) {
        if(param->max_steps != -1)
            return S_DUP_STATUS;
        else
            return S_MAX_STEPS;
    } else if(str_status.compare("-stdev") == 0) {
        if(param->length_stdev != -1)
            return S_DUP_STATUS;
        else
            return S_STDEV;
    } else if(str_status.compare("-heur") == 0) {
        if(param->heur != -1)
            return S_DUP_STATUS;
        else
            return S_HEUR;
    } else if(str_status.compare("-bounding") == 0) {
        if(param->bounding != -1)
            return S_DUP_STATUS;
        else
            return S_BOUNDING;
    }

    return S_BAD_STATUS;
}

int get_lama_status(string str_status){

    if(str_status.compare("-heur") == 0) {
        return S_LAMA_HEUR;
    } else if(str_status.compare("-weight_list") == 0) {
        return S_LAMA_W_LIST;
    } else if(str_status.compare("-pref") == 0) {
        return S_LAMA_PREFERRED;
    } else if(str_status.compare("-p_reward") == 0) {
        return S_LAMA_P_REWARD;
    } else if(str_status.compare("-mem_limit") == 0) {
        return S_LAMA_MEM_LIMIT;
    }

    return S_LAMA_BAD_STATUS;
}

bool get_lama_heuristic(string str_heur, LAMA_Parameters *param) {
  
    if(str_heur.compare("LAMA_FF") == 0) {

        if(param->lama_ff_heur) {
            cerr << "Entered LAMA FF heuristic multiple times for the same config" << endl;
            return false;
        }
        param->lama_ff_heur = true;
    } else if(str_heur.compare("LAMA_FF_S") == 0) {
        
        if(param->lama_ffs_heur) {
            cerr << "Entered LAMA FFs heuristic multiple times for the same config" << endl;
            return false;
        }
        param->lama_ffs_heur = true;
    } else if(str_heur.compare("LAMA_FF_C") == 0) {
        
        if(param->lama_ffc_heur) {
            cerr << "Entered LAMA FFc heuristic multiple times for the same config" << endl;
            return false;
        }
        param->lama_ffc_heur = true;
    } else if(str_heur.compare("FD_FF") == 0) {
        
        if(param->fd_ff_heur) {
            cerr << "Entered FD FF heuristic multiple times for the same config" << endl;
            return false;
        }
        param->fd_ff_heur = true;
    } else if(str_heur.compare("LM") == 0) {
        
        if(param->lm_heur) {
            cerr << "Entered Landmark Count heuristic multiple times for the same config" << endl;
            return false;
        }
        param->lm_heur = true;
    } else {
        cerr << str_heur << " is an invalid heuristic" << endl;
        return false;
    } 

    return true;
}

bool get_lama_preferred(string str_pref, LAMA_Parameters *param) {
   
    if(str_pref.compare("LAMA_FF") == 0) {

        if(param->lama_ff_preferred) {
            cerr << "Entered LAMA FF preferred operators multiple times for the same config" << endl;
            return false;
        }
        param->lama_ff_preferred = true;
    } else if(str_pref.compare("LAMA_FF_S") == 0) {
        
        if(param->lama_ffs_preferred) {
            cerr << "Entered LAMA FFs preferred operators multiple times for the same config" << endl;
            return false;
        }
        param->lama_ffs_preferred = true;
    } else if(str_pref.compare("LAMA_FF_C") == 0) {
        
        if(param->lama_ffc_preferred) {
            cerr << "Entered LAMA FFc preferred operators multiple times for the same config" << endl;
            return false;
        }
        param->lama_ffc_preferred = true;
    } else if(str_pref.compare("FD_FF") == 0) {
        
        if(param->fd_ff_preferred) {
            cerr << "Entered FD FF preferred operators multiple times for the same config" << endl;
            return false;
        }
        param->fd_ff_preferred = true;
    } else if(str_pref.compare("LM") == 0) {
        
        if(param->lm_preferred) {
            cerr << "Entered Landmark Count preferred operators multiple times for the same config" << endl;
            return false;
        }
        param->lm_preferred = true;
    } else {
        cerr << str_pref << " is an invalid preferred operator" << endl;
        return false;
    } 

    return true;
}

bool get_weight_list(string str_list, std::vector<int> &list) {
    if(str_list[0] != '[' || str_list[str_list.size()-1] != ']') {
        cerr << "Bad List formatting" << endl;
        return false;
    } 
    
    // split string by commas, eliminating square brackets first
    std::vector<std::string> tokens = split(str_list.substr(1, str_list.size()-2), ',');

    // want best-first search
    if(tokens.size() == 0) {
        return true;
    }

    for(int i = 0; i < tokens.size(); i++) {
        int value = atoi(tokens[i].c_str());

        // -1 is for greed-best-first-search
        if((value < 0 && value != -1) || (value == 0 && tokens[i].compare("0") != 0)) {
            cerr << "Invalid list element \"" << tokens[i] << " entered." << endl;
            return false;
        }

        list.push_back(value);
    }
    return true;
}

bool get_walk_type_list(string str_list, std::vector<int> &list) {
    if(str_list[0] != '[' || str_list[str_list.size()-1] != ']') {
        cerr << "Bad List formatting" << endl;
        return false;
    } 
    
    // split string by commas, eliminating square brackets first
    std::vector<std::string> tokens = split(str_list.substr(1, str_list.size()-2), ',');

    if(tokens.size() == 0) {
        cerr << "Entered walk type list" << endl;
        return false;
    }

    for(int i = 0; i < tokens.size(); i++) {
        int value = 0;

        if(tokens[i].compare("PURE") == 0)
            value = MRW_Parameters::PURE;
        else if(tokens[i].compare("MDA") == 0)
            value = MRW_Parameters::MDA;
        else if(tokens[i].compare("MHA") == 0)
            value = MRW_Parameters::MHA;
        else {
            cerr << "Invalid walk type entered in walk type list" << endl;
            return false;
        }

        list.push_back(value);
    }
    return true;
}

/**
 * Gets the walk type given by the string.
 * Returns false if invalid entry is given.
 */
bool get_mrw_walk_type(string str_type, MRW_Parameters * param) {

    if(str_type.compare("PURE") == 0)
        param->walk_type = MRW_Parameters::PURE;
    else if (str_type.compare("MDA") == 0)
        param->walk_type = MRW_Parameters::MDA;
    else if (str_type.compare("MHA") == 0)
        param->walk_type = MRW_Parameters::MHA;
    else
        return false;
    
    return true;
}

bool get_mrw_step_type(string str_type, MRW_Parameters * param){
    if(str_type.compare("STATE") == 0)
        param->step_type = MRW_Parameters::STATE;
    else if (str_type.compare("PATH") == 0)
        param->step_type = MRW_Parameters::PATH;
    else if (str_type.compare("H_PATH") == 0)
        param->step_type = MRW_Parameters::H_PATH;
    else
        return false;

    return true;
}


bool get_mrw_bounding_type(string str_type, MRW_Parameters * param){
    if(str_type.compare("NONE") == 0)
        param->bounding = MRW_Parameters::NONE;
    else if (str_type.compare("G") == 0)
        param->bounding = MRW_Parameters::G_PRUNNING;
    else if (str_type.compare("F") == 0)
        param->bounding = MRW_Parameters::F_PRUNNING;
    else
        return false;
    return true;
}


/**
 * Gets the heuristic type indicated by the string.
 * Returns false if invalid entry is given.
 */
bool get_mrw_heur_type(string str_heur, MRW_Parameters *param) {
    if(str_heur.compare("FF") == 0 || str_heur.compare("FD_FF") == 0)
        param->heur = MRW_Parameters::FD_FF;
    else if(str_heur.compare("LM") == 0)
        param->heur = MRW_Parameters::LM;
    else if(str_heur.compare("LAMA_FF") == 0)
        param->heur = MRW_Parameters::LAMA_FF;
    else if(str_heur.compare("LAMA_FF_S") == 0)
        param->heur = MRW_Parameters::LAMA_FF_S;
    else if(str_heur.compare("LAMA_FF_C") == 0)
        param->heur = MRW_Parameters::LAMA_FF_C;
    else
        return false;

    return true;
}

/**
 * Extracts the float value and stores it in the appropriate part of param.
 * The appropriate part is given by the status. Assumes it is being given
 * a valid status.
 */
bool extract_mrw_float_value(string str, MRW_Parameters *param, int status) {

    //TODO Do this better
    float value = atof(str.c_str());
    if(value <= 0) {
        // handle errors
        return false;
    }

    if(status == S_ALPHA)
        param->alpha = value;
    else if(status == S_E_RATE)
        param->extending_rate = value;
    else if(status == S_E_PERIOD)
        param->extending_period = value;
    else if(status == S_TEMP)
        param->walk_bias_temp = value;
    else if(status == S_STDEV)
        param->length_stdev = value;

    return true;
}

/**
 * Extracts the int value and stores it in the appropriate part of param.
 * The appropriate part is given by the status. Assumes it is being given
 * a valid status.
 */
bool extract_mrw_int_value(string str, MRW_Parameters *param, int status) {

    //TODO Do this better
    int value = atoi(str.c_str());
    if(value <= 0)
        return false;

    if(status == S_LENGTH_WALK)
        param->length_walk = value;
    else if(status == S_NUM_WALK)
        param->num_walk = value;
    else if(status == S_L_JUMP)
        param->length_jump = value;
    else if(status == S_MAX_STEPS)
        param->max_steps = value;
    
    return true;
}


bool parse_config_file(const char * filename, std::vector<MRW_Parameters *> &mrw_params, 
        std::vector<LAMA_Parameters *> &lama_params) {

    string line;
    ifstream file_in(filename);

    if(file_in.is_open()) {

        int count = 1;
        while(!file_in.eof()) {
            getline(file_in, line);

            if(line.size() == 0)
                continue;

            if(line.find("-mrw_conf") != 0 &&
                    line.find("-lama_conf") != 0) {
                cerr << "In file " << filename << ", line " << count;
                cerr << " does not begin with either -mrw_conf or -lama_conf" << endl;
                return false;
            }

            string to_parse;
            int index = line.find('\"');
            if(index != string::npos) {
                if(line[line.size()-1] != '\"') {
                    cerr << "In file " << filename << ", line " << count;
                    cerr << " is missing a \" at the end" << endl;
                    return false;
                }

                to_parse = line.substr(index + 1, line.size() - index - 2);
            } else {
                index = line.find('-', 1);
                if(index == string::npos) {
                    cerr << "In file " << filename << ", line " << count;
                    cerr << " does not have any options" << endl;
                    return false;
                }
                to_parse = line.substr(index, line.size() - index);
            }

            if(line.find("-mrw_conf") == 0) {
                MRW_Parameters * params = NULL;
                if(!mrw_param_string_parser(to_parse, params)) {
                    cerr << "Invalid config on line " << count << " of file ";
                    cerr << filename << endl;
                    return false;
                } 
                else
                    mrw_params.push_back(params);

            } else if(line.find("-lama_conf") == 0) {
                LAMA_Parameters * params = NULL;
                if(!lama_param_string_parser(to_parse, params)) {
                    cerr << "Invalid config on line " << count << " of file ";
                    cerr << filename << endl;
                    return false;
                }
                else
                    lama_params.push_back(params);

            } 
            count++;
        }
    } else {
        cerr << "Unable to open " << filename << endl;
        return false;
    }

    return true;
}


