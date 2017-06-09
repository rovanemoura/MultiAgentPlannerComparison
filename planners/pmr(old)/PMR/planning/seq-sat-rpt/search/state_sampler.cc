#include "state_sampler.h"
#include "operator.h"

#include <algorithm>
#include <set>

using namespace std;

StateSampler::StateSampler() {

    fluents.resize(g_variable_domain.size());
    for (int i = 0; i < g_variable_domain.size(); i++) {
        fluents[i].resize(g_variable_domain[i]);
        fluents[i].clear();
        for (int j = 0; j < g_variable_domain[i]; j++)
            fluents[i].push_back(make_pair(i,j));
    }

    // create the structures necessary to check the invariants
    // first add the regular variables to the invariants
    for (int i = 0; i < g_variable_domain.size(); i++) {
        vector<pair<int,int> > invariant;
        for (int j = 0; j < g_variable_domain[i]; j++)
            invariant.push_back(make_pair(i,j));
        invariant_exactly_one.push_back(invariant);
    }

    // now the exactly one
    for (int i = 0; i < g_invariant_exactly_one.size(); i++) {
        invariant_exactly_one.push_back(g_invariant_exactly_one[i]);
    }

    // add the index of the invariants to the fluents
    // so they know in which invariants they appear
    satisfied_invariants.resize(invariant_exactly_one.size());
    satisfied_invariants_fluent.resize(fluents.size());
    for (int i = 0; i < fluents.size(); i++) {
        satisfied_invariants_fluent[i].resize(fluents[i].size());
        for (int j = 0; j < fluents[i].size(); j++)
            satisfied_invariants_fluent[i][j].resize(invariant_exactly_one.size(), false);
    }

    for (int i = 0; i < invariant_exactly_one.size(); i++) {
        for (int j = 0; j < invariant_exactly_one[i].size(); j++) {
            pair<int,int> aux_fluent = invariant_exactly_one[i][j];
            satisfied_invariants_fluent[aux_fluent.first][aux_fluent.second][i] = true;
        }
    }

    // add the negative interactions between eligible fluents and invariants
    potential_invariants.resize(invariant_exactly_one.size());
    for (int i = 0; i < invariant_exactly_one.size(); i++) {
        potential_invariants[i].resize(invariant_exactly_one[i].size());
    }

    potential_invariants_fluent.resize(fluents.size());
    for (int i = 0; i < fluents.size(); i++) {
        potential_invariants_fluent[i].resize(fluents[i].size());
        for (int j = 0; j < potential_invariants_fluent[i].size(); j++) {
            potential_invariants_fluent[i][j].resize(invariant_exactly_one.size());
        }
    }

    for (int i = 0; i < fluents.size(); i++) {
        for (int j = 0; j < fluents[i].size(); j++) {
            pair<int,int> aux_fluent = fluents[i][j];
            // now find the negative interaction
            for (int k = 0; k < invariant_exactly_one.size(); k++) {
                bool satisfies = false;
                vector<int> conflicts;
                for (int l = 0; !satisfies && l < invariant_exactly_one[k].size(); l++) {
                    // we find which fluents are mutex with the fluent
                    if (aux_fluent == invariant_exactly_one[k][l])
                        satisfies = true;
                    else if (are_mutex(aux_fluent, invariant_exactly_one[k][l]))
                        conflicts.push_back(l);
                }

                if (!satisfies && !conflicts.empty()) {
                    potential_invariants_fluent[aux_fluent.first][aux_fluent.second][k] = conflicts;
                }
            }
        }
    }

    Options op_ff;
    op_ff.set("cost_type", 1);
    ff = new FFHeuristic(op_ff);
    ff->evaluate(*g_initial_state); // just to initialize it
}

bool comparator_sample(vector<pair<int, int> > i,vector<pair<int, int> > j) { return (i.size()>j.size()); }

vector<pair<int, int> > StateSampler::sample_random_state(){
    vector<pair<int, int> > chosen_props;

    int attempt = 0;

    // it may be that a given set of goals is a dead end or spurious
    // so the process is repeated until a valid one is found
    while(true){
        if (attempt % 1000 == 0 && attempt != 0)
            cout << "attempt: " << attempt << endl;

        // reset everything
        chosen_props.clear();
        for (int i = 0; i < satisfied_invariants.size(); i++)
            satisfied_invariants[i] = false;
        for (int i = 0; i < potential_invariants.size(); i++)
            for (int j = 0; j < potential_invariants[i].size(); j++)
                potential_invariants[i][j] = 0;

        attempt++;

        // randomize the position of the variables and values
        vector<vector<pair<int, int> > > aux_fluents(fluents);
        random_shuffle (aux_fluents.begin(), aux_fluents.end());
        sort(aux_fluents.begin(), aux_fluents.end(),comparator_sample);
        for (int i = 0; i < aux_fluents.size(); i++)
            random_shuffle (aux_fluents[i].begin(), aux_fluents[i].end());

        vector<int> last_chosen(aux_fluents.size(),0);
        while (chosen_props.size() < g_variable_domain.size()) {
            int var = chosen_props.size();
            bool variable_assigned = false;
            while (last_chosen[var] < aux_fluents[var].size()) {
                pair<int, int> aux_fluent = aux_fluents[var][last_chosen[var]];

                bool mutex = false;
                for (int k = 0; !mutex && k < chosen_props.size(); k++) {
                    mutex = are_mutex(aux_fluent,chosen_props[k]);
                }

                if (!mutex && check_invariants(aux_fluent)) {
                    chosen_props.push_back(aux_fluent);
                    variable_assigned = true;
                    last_chosen[var]++;
                    break;
                }
                last_chosen[var]++;
            }
            if (!variable_assigned) {
                remove_from_invariants(chosen_props.back());
                chosen_props.pop_back();
                last_chosen[var] = 0;
            }
        }

        State s(chosen_props);
        ff->evaluate(s);
        if (ff->is_dead_end()) {
            continue; // goal unreachable from the sampled state; try again
        }
        break;
    }
    return chosen_props;
}

bool StateSampler::check_invariants(std::pair<int, int> fluent) {

    // we check that the fluent does not invalidate an invariant
    bool invalidates = false;
    vector<vector<int> > conflicts = potential_invariants_fluent[fluent.first][fluent.second];
    for (int i = 0; !invalidates && i < conflicts.size(); i++) {
        if (conflicts[i].empty() || satisfied_invariants_fluent[fluent.first][fluent.second][i])
            continue;
        // we store the values not invalidated yet
        vector<int> non_invalidated;
        for (int j = 0; j < potential_invariants[i].size(); j++) {
            if (potential_invariants[i][j] == 0) {
                non_invalidated.push_back(j);
            }
        }
        // we check that the conflict set is not a super set of the non_validated set
        vector<int> intersection;
        set_intersection(non_invalidated.begin(), non_invalidated.end(), conflicts[i].begin(), conflicts[i].end(), back_inserter(intersection));
        invalidates = intersection.size() == non_invalidated.size();
    }

    if (invalidates) {
        return false;
    }

    // we increase the number of conflicts
    for (int i = 0; i < conflicts.size(); i++)
        for (int j = 0; j < conflicts[i].size(); j++)
            potential_invariants[i][conflicts[i][j]]++;

    // we set the invariant to satisfied
    vector<bool> sat_invariants = satisfied_invariants_fluent[fluent.first][fluent.second];
    for (int i = 0; i < sat_invariants.size(); i++)
        if(sat_invariants[i])       
            satisfied_invariants[i] = true;

    return true;
}

void StateSampler::remove_from_invariants(std::pair<int, int> fluent) {
    // we decrease the number of conflicts
    vector<vector<int> > conflicts = potential_invariants_fluent[fluent.first][fluent.second];
    for (int i = 0; i < conflicts.size(); i++)
        for (int j = 0; j < conflicts[i].size(); j++)
            potential_invariants[i][conflicts[i][j]]--;

    // we set the invariant to not satisfied
    vector<bool> sat_invariants = satisfied_invariants_fluent[fluent.first][fluent.second];
    for (int i = 0; i < sat_invariants.size(); i++)
        if(sat_invariants[i])       
            satisfied_invariants[i] = false;
}
