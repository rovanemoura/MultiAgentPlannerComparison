# -*- coding: utf-8 -*-
from __future__ import print_function

import logging
import subprocess
import sys
sys.path.append('.')
from . import aliases
from . import arguments
from . import run_components

def apply_action(state,action):
    #print (dir(state))
    for cond,pred in action.del_effects:
        state.remove(pred)
    for cond,pred in action.add_effects:
        if pred not in state:
            state.append(pred)

def main():
    args = arguments.parse_args()
    logging.basicConfig(level=getattr(logging, args.log_level.upper()),
                        format="%(levelname)-8s %(message)s",
                        stream=sys.stdout)
    logging.debug("processed args: %s" % args)
    #print (args.translate_inputs)
    import copy

    #exit(0)
    if args.show_aliases:
        aliases.show_aliases()
        sys.exit()

    try:
        ### TBD - MAKE THESE ARGUMENTS ###############
        sys.path.append('translate')
        import translate
        exit(0)
        agent_prefix = "rover"
        agent_count = 4
        ta_agent = "rover0"
        plan_file = "/tmp/sas_plan"
        ###########################
        agent_list = []
        plan_fd = open(plan_file)
        plan = plan_fd.readlines()
        task,inst_actions = translate.get_task_actions(args.translate_inputs[0],args.translate_inputs[1])
        # Create an action map
        inst_action_map = {}
        #orig_init = copy.deepcopy(task.init)
        for act in inst_actions:
            inst_action_map[act.name] = act
        for i in range(0,agent_count):
            agent_list.append(agent_prefix+str(i))
        agent_plan = []
        for action in plan:
            agent_actions = []
            for agent in agent_list:
                new_action = action.replace(ta_agent,agent).strip()
                #new_action = new_action.replace(ta_agent+')',agent+')')
                agent_actions.append(new_action)
            #check if we can directly use one of the actions
            applicable_action = False
            inst_set = set(task.init)
            for ag_action in agent_actions:
                if not applicable_action and ag_action in inst_action_map.keys() and set(inst_action_map[ag_action].precondition) <=inst_set:
                    print ("Expanded action : "+ag_action)
                    applicable_action = True
                    apply_action(task.init,inst_action_map[ag_action])
            if not applicable_action:
            # If we were unable to apply the action directly we need to plan to reach for the preconditions of one of the actions
                plan_found = False
                print ("Expanded action : Need to plan for "+ag_action)
                for ag_action in agent_actions:
                    if not plan_found:
                        if ag_action  in inst_action_map.keys():
                            task.goal = inst_action_map[ag_action].precondition
                        translate.create_sas_for_task(task)
                        #run preprocess
                        #run search
                        #read the plan to sub plan
                        
                        if plan_found:
                            for p_action in sub_plan:
                                print ("Expanded action : "+p_action)
                                apply_action(task.init,inst_action_map[p_action]) 
                
                

                    
                
            #exit(0) 
    except subprocess.CalledProcessError as err:
        print(err)
        sys.exit(err.returncode)


if __name__ == "__main__":
    main()
