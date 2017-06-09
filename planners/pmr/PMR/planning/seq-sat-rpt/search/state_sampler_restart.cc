#include "state_sampler.h"
#include "operator.h"

#include <algorithm>
#include <set>

using namespace std;

StateSampler::StateSampler() {

    compute_sink_fluents();

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

    Options op_ff;
    op_ff.set("cost_type", 1);
    ff = new FFHeuristic(op_ff);
    ff->evaluate(*g_initial_state); // just to initialize it
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

bool comparator_sample(vector<pair<int, int> > i,vector<pair<int, int> > j) { return (i.size()>j.size()); }

vector<pair<int, int> > StateSampler::sample_random_state(){
    vector<pair<int, int> > chosen_fluents;

    int attempt = 0;

    // it may be that a given set of goals is a dead end or spurious
    // so the process is repeated until a valid one is found
    while(true){
        // reset everything
        chosen_fluents.clear();
        attempt++;

        if (attempt % 100 == 0)
            cout << "attempt: " << attempt << endl;

        // randomize and then sort invariants per size
        // this leads to fewer attempts while still getting
        // some random orders between invariants of the same size
        random_shuffle(invariant_exactly_one.begin(), invariant_exactly_one.end());
        sort(invariant_exactly_one.begin(), invariant_exactly_one.end(),comparator_sample);
        

        // pick fluents from the invariants at random
        bool failed = false;
        for (int i = 0; i < invariant_exactly_one.size(); i++) {
            bool satisfied = false;
            vector<pair<int, int> > candidates;
            for (int j = 0; !satisfied && j < invariant_exactly_one[i].size(); j++) {
                bool mutex = false;
                for (int k = 0; !satisfied && k < chosen_fluents.size(); k++) {
                    if (chosen_fluents[k] == invariant_exactly_one[i][j])
                        satisfied = true;
                    else if (are_mutex(chosen_fluents[k],invariant_exactly_one[i][j]))
                        mutex = true;
                }
                if (!mutex)
                    candidates.push_back(invariant_exactly_one[i][j]);
            }
            if (satisfied)
                continue;
            else if (candidates.empty())
                failed = true;
            else
                chosen_fluents.push_back(candidates[rand() % candidates.size()]);
        }
        if(!failed)
            break;
        if (attempt % 100 == 0)
            cout << "chosen_fluents: " << chosen_fluents.size() << endl;
    }
    return chosen_fluents;
}



