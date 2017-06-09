#ifndef ADP_RELAXATION_HEURISTIC_H
#define ADP_RELAXATION_HEURISTIC_H

#include "heuristic.h"

#include <vector>

class FactProxy;
class GlobalState;
class OperatorProxy;

struct ADP_Proposition;
struct ADP_UnaryOperator;

struct ADP_UnaryOperator {
    int operator_no; // -1 for axioms; index into g_operators otherwise
    std::vector<ADP_Proposition *> precondition;
    ADP_Proposition *effect;
    int base_cost;

    int unsatisfied_preconditions;
    int cost; // Used for h^max cost or h^add cost;
              // includes operator cost (base_cost)
    ADP_UnaryOperator(const std::vector<ADP_Proposition *> &pre, ADP_Proposition *eff,
                  int operator_no_, int base)
        : operator_no(operator_no_), precondition(pre), effect(eff),
          base_cost(base) {}

    //Agent Additions
    int agent_id; //-1 for public, -2 for joint
    std::vector<int > agent_cost;  
    bool is_influencing;
    bool is_influenced;
};

struct ADP_Proposition {
    bool is_goal;
    int id;
    std::vector<ADP_UnaryOperator *> precondition_of;

    int cost; // Used for h^max cost or h^add cost
    ADP_UnaryOperator *reached_by;
    bool marked; // used when computing preferred operators for h^add and h^FF

    //Agent Additions
    int goal_agent;//Which agent is assigned this goal
    int agent_id;
    int best_agent;
    int layer;
    std::vector< int> cost_by_agent; //For public propositions.
    std::vector<ADP_UnaryOperator *> agent_reached_by;


    ADP_Proposition(int id_) {
        id = id_;
        is_goal = false;
        cost = -1;
        reached_by = 0;
        marked = false;
    }

    ADP_Proposition(int id_, int agent, int no_of_agents){
        id = id_;
        is_goal = false;
        cost = -1;
        reached_by = 0;
        marked = false;

        //Agent Additions
        agent_id = agent;
        best_agent = agent;
        if(agent == -1){
            for(int i = 0; i < no_of_agents; i++){
                cost_by_agent.push_back(-1);
            }
        }
        goal_agent = -1;
        agent_reached_by.resize(no_of_agents);
    }
};

class ADPRelaxationHeuristic : public Heuristic {
    void build_unary_operators(const OperatorProxy &op, int operator_no);
    void simplify();
    //private multiagent
    void split_into_agents();
    void extend_agent_vars_full_graph(int agent, int var_to_expand);
    void check_operator_for_jointness(std::vector<OperatorProxy> ops);

protected:
    std::vector<ADP_UnaryOperator> unary_operators;
    std::vector<std::vector<ADP_Proposition> > propositions;
    std::vector<ADP_Proposition *> goal_propositions;


    int no_of_agents;

    std::vector<std::vector<int> > agent_vars; 
    std::vector<int> public_vars;
    std::vector<int> associated_agent_by_var; //-1 if public var - agent number otherwise.

    std::vector<std::vector<ADP_Proposition *> > agent_propositions;
    std::vector<ADP_Proposition *> public_propositions;
    std::vector<std::vector<ADP_Proposition *> > agent_goal_propositions;
    std::vector<std::vector<ADP_Proposition *> > agent_current_goals;
    
    std::vector<std::vector<ADP_UnaryOperator *> > agent_operators;
    std::vector<std::vector<ADP_UnaryOperator *> > agent_joint_operators;
    std::vector<ADP_UnaryOperator *> public_operators;

    ADP_Proposition *get_proposition(const FactProxy &fact);
    virtual void initialize();
    virtual int compute_heuristic(const GlobalState &state) = 0;
public:
    ADPRelaxationHeuristic(const Options &options);
    virtual ~ADPRelaxationHeuristic();
    virtual bool dead_ends_are_reliable() const;
};

#endif
