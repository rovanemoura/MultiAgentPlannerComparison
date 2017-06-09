#include "state_sampler.h"
#include "operator.h"

#include <algorithm>
#include <set>

using namespace std;

StateSampler::StateSampler() {

    compute_sink_fluents();

    fluents.resize(g_variable_domain.size());
    for (int i = 0; i < g_variable_domain.size(); i++) {
        fluents[i].resize(g_variable_domain[i]);
        fluents[i].clear();
        for (int j = 0; j < g_variable_domain[i]; j++) {
            pair<int,int> fluent_candidate = make_pair(i,j);
            // if (!is_sink[fluent_candidate.first][fluent_candidate.second])
                fluents[i].push_back(fluent_candidate);
        }
    }

    // create the structures necessary to check the invariants
    // first add the regular variables to the invariants
    for (int i = 0; i < g_variable_domain.size(); i++) {
        vector<pair<int,int> > invariant;
        for (int j = 0; j < g_variable_domain[i]; j++) {
            if (!is_sink[i][j])
                invariant.push_back(make_pair(i,j));
        }
        if (!invariant.empty())
            invariant_exactly_one.push_back(invariant);
    }
    cout << "Invariants from variables -> " << invariant_exactly_one.size() << endl;

    // now the exactly one
    for (int i = 0; i < g_invariant_exactly_one.size(); i++) {
        vector<pair<int,int> > invariant;
        for (int j = 0; j < g_invariant_exactly_one[i].size(); j++) {
            if (!is_sink[g_invariant_exactly_one[i][j].first][g_invariant_exactly_one[i][j].second])
                invariant.push_back(g_invariant_exactly_one[i][j]);
        }
        if (!invariant.empty())
            invariant_exactly_one.push_back(invariant);
    }
    cout << "Invariants in total -> " << invariant_exactly_one.size() << endl;

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
            if (is_sink[i][j])
                continue;
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
                    // cout << print_fluent(aux_fluent.first, aux_fluent.second) << " mutex with " << print_fluent(invariant_exactly_one[potential_invariants_fluent[aux_fluent.first][aux_fluent.second][0].first][potential_invariants_fluent[aux_fluent.first][aux_fluent.second][0].second].first, invariant_exactly_one[potential_invariants_fluent[aux_fluent.first][aux_fluent.second][0].first][potential_invariants_fluent[aux_fluent.first][aux_fluent.second][0].second].second) << endl;
                }
            }
        }
    }

    
    /* for (int i = 0; i < fluents.size(); i++) {
        for (int j = 0; j < fluents[i].size(); j++) {
            if (is_sink[i][j])
                continue;
            pair<int,int> aux_fluent = fluents[i][j];
            cout << print_fluent(aux_fluent.first, aux_fluent.second) << endl;
            vector<pair<int, int> > conflicts = potential_invariants_fluent[i][j];
            // cout << "conflicts size: " << potential_invariants_fluent[i][j].size() << endl;
            // cout << invariant_exactly_one.size() << " " << invariant_exactly_one[potential_invariants_fluent[i][j][0].first].size();
            for (int k = 0; k < conflicts.size(); k++) {
                pair<int,int> conflicting_fluent = invariant_exactly_one[conflicts[k].first][conflicts[k].second];
                cout << "  " << print_fluent(conflicting_fluent.first, conflicting_fluent.second) << endl;
            }
        }
    } */

    Options op_ff;
    op_ff.set("cost_type", 1);
    ff = new FFHeuristic(op_ff);
    ff->evaluate(*g_initial_state); // just to initialize it
}

vector<pair<int, int> > StateSampler::sample_random_state(){
    vector<pair<int, int> > chosen_props;

    int attempt = 0;

    // it may be that a given set of goals is a dead end or spurious
    // so the process is repeated until a valid one is found
    while(true){
        if (attempt % 10 == 0 && attempt != 0)
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
        for (int i = 0; i < aux_fluents.size(); i++)
            random_shuffle (aux_fluents[i].begin(), aux_fluents[i].end());

        vector<int> last_chosen(aux_fluents.size(),0);
        while (chosen_props.size() < g_variable_domain.size()) {
            int var = chosen_props.size();
            bool variable_assigned = false;
            // cout << "Var: " << var << " - " << last_chosen[var] << " out of " << aux_fluents[var].size() << endl;
            while (last_chosen[var] < aux_fluents[var].size()) {
                // cout << "  Attempt: " << last_chosen[var] << endl;
                pair<int, int> aux_fluent = aux_fluents[var][last_chosen[var]];
                if (is_sink[aux_fluent.first][aux_fluent.second]) {
                    last_chosen[var]++; continue;
                }

                bool mutex = false;
                for (int k = 0; !mutex && k < chosen_props.size(); k++) {
                    mutex = are_mutex(aux_fluent,chosen_props[k]);
                    // if (mutex) {
                    //     cout << "  Mutex: " << print_fluent(aux_fluent.first, aux_fluent.second) << " with " << print_fluent(chosen_props[k].first, chosen_props[k].second) << endl;
                    // }
                }

                if (!mutex && check_invariants(aux_fluent)) {
                    chosen_props.push_back(aux_fluent);
                    variable_assigned = true;
                    // cout << "Added: " << chosen_props.size()-1 << " - " << last_chosen[var] << ": " << print_fluent(aux_fluent.first, aux_fluent.second) << endl;
                    last_chosen[var]++;
                    break;
                }
                last_chosen[var]++;
            }
            if (!variable_assigned) {
                /* if (var > 35 && attempt > 3) {
                    for (int a = 0; a < chosen_props.size();a++) {
                        cout << "    " << print_fluent(chosen_props[a].first, chosen_props[a].second) << endl;
                    }
                    cout << "??? " << g_fact_names[aux_fluents[var][0].first][0] << endl;

                    exit(0);
                } */
                remove_from_invariants(chosen_props.back());
                chosen_props.pop_back();
                // cout << "Removed: " << chosen_props.size() << " - " << last_chosen[var]-1 << endl;
                last_chosen[var] = 0;
            }
        }

        // cout << "-------------------------------------------------------------" << endl;

        // for (int i = 0; i < chosen_props.size(); i++) {
        //     cout << "Fluent numbers: " << chosen_props[i].first << " - " << chosen_props[i].second << endl;
        //     cout << "Fluent: " << g_fact_names[chosen_props[i].first][chosen_props[i].second] << " related to " << g_fact_names[chosen_props[i].first][0] << endl;
        // }

/*        for (int i = 0; sampling && i < aux_fluents.size(); i++) {
            bool variable_satisfied = false;
            for (int j = 0; !variable_satisfied && j < aux_fluents[i].size(); j++) {
                pair<int, int> aux_fluent = aux_fluents[i][j];
                bool mutex = false;
                for (int k = 0; !mutex && k < chosen_props.size(); k++) {
                    mutex = are_mutex(aux_fluent,chosen_props[k]);
                }
                if (!mutex) {
                    chosen_props.push_back(aux_fluent);
                    variable_satisfied = true;
                }
            }
            if (!variable_satisfied)
                sampling = false;
        }*/
        cout << chosen_props.size() << " and " << g_variable_domain.size() << endl;
        // goal unreachable from the partial state; try again
        State s(chosen_props);
        ff->evaluate(s);
        cout << "test" << endl;
        if (ff->is_dead_end()) {
            cout << "dead end!! - " << attempt << endl;
            continue;
        }
        cout << "it worked!" << endl;
        break;
    }
    return chosen_props;
}

void StateSampler::compute_sink_fluents() {

    is_sink.resize(g_variable_domain.size());
    for (int i = 0; i < g_variable_domain.size(); i++) {
        is_sink[i].resize(g_variable_domain[i]);
        for (int j = 0; j < g_variable_domain[i]; j++)
            is_sink[i][j] = true;
    }

    for (unsigned i = 0; i < g_goal.size(); i++)
        is_sink[g_goal[i].first][g_goal[i].second] = false;

    for (unsigned i = 0; i < g_operators.size(); i++){
        if (g_operators[i].spurious)
            continue;

        const vector<PrePost> &pre_post = g_operators[i].get_pre_post();

        for (unsigned j = 0; j < pre_post.size(); j++) {
            int var = pre_post[j].var;
            int pre = pre_post[j].pre;
            int post = pre_post[j].post;
            if (pre != -1) {
                is_sink[var][pre] = false;  // var-pre is deleted: not sink
            } else {
                for (unsigned k = 0; k < g_variable_domain[var]; k++) {
                    if (!is_sink[var][k] || k == post)
                        continue;

                    pair<int,int> candidate = make_pair(var,k);
                    bool mutex = false;

                    // prevail
                    const vector<Prevail> &prevail = g_operators[i].get_prevail();
                    for (unsigned index_mutex = 0; !mutex && index_mutex < prevail.size(); index_mutex++)
                        mutex = are_mutex(candidate,make_pair(prevail[index_mutex].var,prevail[index_mutex].prev));

                    // augmented preconditions
                    const vector<pair<int,int> > augmented_preconditions = g_operators[i].augmented_preconditions;
                    for (unsigned index_mutex = 0; !mutex && index_mutex < augmented_preconditions.size(); index_mutex++)
                        mutex = are_mutex(candidate,augmented_preconditions[index_mutex]);

                    // other pre-posts
                    for (unsigned index_mutex = 0; !mutex && index_mutex < pre_post.size(); index_mutex++)
                        if (var != pre_post[index_mutex].var && pre_post[index_mutex].pre != -1)
                            mutex = are_mutex(candidate,make_pair(pre_post[index_mutex].var,pre_post[index_mutex].pre));

                    if (!mutex)
                        is_sink[var][k] = false;
                }
            }
        }
    }

}

bool StateSampler::check_invariants(std::pair<int, int> fluent) {

    // we check that the fluent does not invalidate an invariant
    bool invalidates = false;
    vector<vector<int> > conflicts = potential_invariants_fluent[fluent.first][fluent.second];
    // cout << print_fluent(fluent.first, fluent.second) << endl;
    for (int i = 0; !invalidates && i < conflicts.size(); i++) {
        if (conflicts[i].empty() || satisfied_invariants_fluent[fluent.first][fluent.second][i])
            continue;
        // cout << "aaa " << invalidates << conflicts[i].empty() << conflicts[i].size() << " " << potential_invariants[i].size() << endl;
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
        /* if (invalidates) {
            cout << "Invariant violated by " << print_fluent(fluent.first,fluent.second) << endl;
            for (int j = 0; j < non_invalidated.size(); j++)
                cout << "  " << print_fluent(invariant_exactly_one[i][j].first,invariant_exactly_one[i][j].second) << endl;
        }*/
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
