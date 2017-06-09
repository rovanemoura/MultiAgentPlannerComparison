# -*- coding: utf-8 -*-
from __future__ import print_function

import logging
import subprocess
import sys
#sys.path.append('.')
from . import aliases
from . import arguments
from . import run_components
import os
import re
import threading
import queue
import copy
#print (sys.argv)
#print (sys.argc)

def write_tofile(to_write, output_file):
    file_ = open(output_file, 'w')
    file_.write(to_write)
    file_.close()

def apply_action(state,action):
    #print ("applied action "+action.name+" effects "+str(action.del_effects))
    #if action.name == "(lift depot0 hoist0 crate3 pallet7)":
    #    print (state)

    for cond,pred in action.del_effects:
        try:
        #if pred.negated:
        #    pred = pred.negate()
            state.remove(pred)
        except:
            print ("pred not present "+str(pred))
            #print (action.name)
            #exit(0)
    for cond,pred in action.add_effects:
        if pred not in state:
            state.append(pred)
    return state

def reverse_action(state,action):
    #print ("applied action "+action.name+" effects "+str(action.del_effects))
    #if action.name == "(lift depot0 hoist0 crate3 pallet7)":
    #    print (state)

    for cond,pred in action.add_effects:
        try:
        #if pred.negated:
        #    pred = pred.negate()
            state.remove(pred)
        except:
            print ("pred not present "+str(pred))
            #print (action.name)
            #exit(0)
    for cond,pred in action.del_effects:
        if pred not in state:
            state.append(pred)
    return state


def main():
    args = arguments.parse_args()
    logging.basicConfig(level=getattr(logging, args.log_level.upper()),
                        format="%(levelname)-8s %(message)s",
                        stream=sys.stdout)
    logging.debug("processed args: %s" % args)
    import copy
    orig_args = copy.deepcopy(args)
    sys.argv = sys.argv[:2]
    IGNORE_ACTIONS = []
    REPLACEABLE_ACTIONS_FLAG = True
    action_args = re.compile('\((.*)\)')
    try:
        PACKAGE_PARENT = '../translate/'
        SCRIPT_DIR = os.path.dirname(os.path.realpath(os.path.join(os.getcwd(), os.path.expanduser(__file__))))
        sys.path.append(os.path.normpath(os.path.join(SCRIPT_DIR, PACKAGE_PARENT)))
        sys.path.append('translate')
        import timers
        with timers.timing("PLAN EXPANSION", block=True):
            import translate
            import normalize
            import pddl
            import json
            def dump_pddl(task,goals):
                # Start problem def
                to_write = "(define (problem tiny_problem) "
                to_write += "(:domain " + task.domain_name + ")\n"
                # Objects
                to_write += "(:objects\n"
                for obj in task.objects:
                    to_write += "\t"+obj.name+" - "+obj.type_name+"\n"
                to_write += ")"
                # Init
                to_write += "(:init\n"
                for pred in task.init:
                    if not isinstance(pred,pddl.Assign): 
                        if pred.predicate != '=' :
                            if not pred.negated:
                                to_write += "\t("+pred.predicate+" "+ ' '.join(pred.args)+")\n"
                            else:
                                to_write += "\t(not ("+pred.predicate+" "+ ' '.join(pred.args)+"))\n"
                    else:
                        to_write +=  "\t(= ("+pred.fluent.symbol +" "+ ", ".join(map(str, pred.fluent.args))+") "+str(pred.expression.value)+")\n"
                        #exit(0)
                to_write += ")"
                # Goals
                to_write += "(:goal (and \n"
                for goal in goals:
                    if not goal.negated:
                        to_write += "\t("+goal.predicate+" "+ ' '.join(goal.args)+")\n"
                    else:
                        to_write += "\t(not ("+goal.predicate+" "+ ' '.join(goal.args)+"))\n"
                to_write += "))"
                if task.use_min_cost_metric:
                  to_write += "(:metric minimize (total-cost))\n"
                #Endmatter
                to_write += ")"
                return to_write
            # Threads to define  
            class plannerThread(threading.Thread):
                def __init__(self,id,pfile_path,domain_file,work_queue,output_queue):
                    threading.Thread.__init__(self)
                    self.id = str(id)
                    self.counter = 0
                    self.work_queue = work_queue
                    self.output_queue = output_queue # A priority queue
                    self.pfile_path = pfile_path
                    self.domain_file = domain_file

                def run(self):
                    while True:            
                        pddl_string,task,goal_list,inst_actions,relaxed_reachable, atoms,axioms,reachable_action_params,call_args = self.work_queue.get()
                        pfile = os.path.join(self.pfile_path,"pfile_"+self.id+"_"+str(self.counter)+".pddl")
                        sfile = os.path.join(self.pfile_path,"pfile_"+self.id+"_"+str(self.counter)+".sol")
                        write_tofile(pddl_string, pfile)
                        status = run_components.run_planner(self.domain_file, pfile, sfile)
                        #print ("\n "+str(status))
                        if status == 0:
                            new_plan_fd = open(sfile)
                            sub_plan = new_plan_fd.readlines()
                            self.output_queue.put((-1 * len(sub_plan),sub_plan))
                        elif status == 137 or -9:
                            try:
                                task.goal = pddl.conditions.Conjunction(goal_list)
                                normalize.normalize(task)
                                translate.create_sas_file(task,inst_actions,relaxed_reachable, atoms,axioms,reachable_action_params,run_components.SRC_DIR+'/output.sas')
                                run_components.run_preprocess(args)
                                status = run_components.run_search(args)
                            except:
                                status = 1
                            #run search
                            if status == 0:
                                new_plan_fd = open(run_components.SRC_DIR+'/sas_plan')
                                sub_plan = new_plan_fd.readlines()[:-1]
                                self.output_queue.put((-1 * len(sub_plan),sub_plan))
                                #    status = run_components.run_planner2(self.domain_file, pfile, sfile)
                        else:
                            self.output_queue.put((0,[]))
                        self.work_queue.task_done()
                        self.counter+=1 



            output_file = "final_plan.sol"
            small_plan_dir = os.path.join(run_components.SRC_DIR,"small_problems")
            op_fd = open(output_file,'w')
            domain_file = args.translate_inputs[0]
            plan_file = args.translate_inputs[2]
            agent_file = args.translate_inputs[3]
            private_pred_file = args.translate_inputs[4]
            ta_file = args.translate_inputs[5]
            private_map_file = args.translate_inputs[6]
            agent_goals = args.translate_inputs[7]
            ###########################
            with open(plan_file) as plan_fd:
                plan = list(map(str.lower,plan_fd.readlines()))
            with open(agent_file) as ag_fd:
                agent_list = list(map(str.strip,ag_fd.readlines()))
            with open(private_pred_file) as pr_fd:
                private_preds = pr_fd.readlines()
            with open(ta_file) as pr_fd:
                ta_agent = pr_fd.readline().strip()
            with open(private_map_file) as pr_fd:
                private_map = json.load(pr_fd)
            with open(agent_goals) as ag_fd:
                agent_goals = list(map(str.strip,ag_fd.readlines()))
            task,inst_actions,IGNORE_ACTIONS,relaxed_reachable, atoms,axioms,reachable_action_params = translate.get_task_actions(args.translate_inputs[0],args.translate_inputs[1],agent_list)
            # Create an action map
            inst_action_map = {}
            action_map = {}
            inst_action_name_map = {}
            for act in inst_actions:
                inst_action_map[act.name] = act
                pred_name = action_args.findall(act.name)[0].split()[0]
                inst_action_name_map.setdefault(pred_name, []).append(act.name)
            for act in task.actions:
                action_map[act.name] = act
            original_goal_set = set(task.goal.parts)
            agent_plan = []
            small_count = 0
            solved_goals = []
            # Identify each possible action in the given plan
            # We get this by replacing TA in the plan with individual agents and replacing any private objects with their specific objects
            agent_plan = []
            for action in plan:
               action_name = action_args.findall(action)[0].split()[0]
               if  action_name not in IGNORE_ACTIONS: #Ignore action which only has agent specific effects
                    agent_actions = []
                    for agent in agent_list:
                        new_action = action.replace(ta_agent,agent).strip()
                        obj_map = private_map[agent]
                        for ta_obj in obj_map.keys():
                            new_action = new_action.replace(ta_obj,obj_map[ta_obj])
                        if new_action not in agent_actions:
                            agent_actions.append(new_action)
                    agent_plan.append(agent_actions)
            possible_effects = []
            possible_preconditions = []
            for agent_actions in agent_plan:
                agent_effects = set()
                agent_preconditions = set()
                for ag_action in agent_actions: 
                    precond = []
                    effects = []
                    effects_pred = []
                    orig_arg_list = action_args.findall(ag_action)[0].split()
                    action_name = orig_arg_list[0]
                    arg_list = orig_arg_list[1:]
                    var_map = dict([(par.name, arg) for par, arg in zip(action_map[action_name].parameters,arg_list)])
                    precond,effects = action_map[orig_arg_list[0]].instantiate_mini(var_map)
                    effects_pred = [pred for cond,pred in effects]
                    for pred in precond:
                        agent_preconditions.add(pred)    
                    for pred in effects_pred:
                        agent_effects.add(str(pred))
                possible_effects.append(list(agent_effects))
                possible_preconditions.append(list(agent_preconditions))

            possible_preconditions.append(list(original_goal_set))
            #for p_index in range(len(possible_effects)):
            #    remove_preds = copy.copy(possible_effects[p_index])
            #    for p_index_2 in range(p_index+1,len(possible_preconditions)):
            #        for pred in remove_preds:
            #            if pred in possible_preconditions[p_index_2]:
            #                remove_preds.remove(pred)
              # Now we know the predicates
            #    for pred in remove_preds:
            #        possible_effects[p_index].remove(pred)
            
            goal_step = {}
            goal_map = {}
            for goal in original_goal_set:
                goal_map[goal.__str__()] = goal
                if len(set(goal.args)&set(agent_list)) == 0:
                    goal_step[goal.__str__()] = 0
                    for p_index in range(len(possible_effects)):
                        if str(goal) in possible_effects[p_index]:
                            goal_step[goal.__str__()] = p_index + 1
 

            #################
            ### Start threads 
            thread_count = 5 ### could try other values
            t_id = 0
            work_queue = queue.Queue()
            output_queue = queue.PriorityQueue()
            while t_id < thread_count:
                p_thread = plannerThread(t_id,small_plan_dir,domain_file,work_queue,output_queue) 
                p_thread.daemon = True
                p_thread.start()
                print ("Started planning Thread with id "+str(t_id))
                t_id+=1
            #################
            ignored_actions = []
            #print(goal_step)
            #exit(0)
            plan_step = 0
            for p_index in range(len( agent_plan)):
               plan_step = p_index + 1
               action_name = action_args.findall(agent_plan[p_index][0])[0].split()[0]
               inst_set = set(task.init)
               solved_goals = list(inst_set & original_goal_set)
               agent_actions = agent_plan[p_index]
               applicable_action = False
               inst_set = set(task.init)
               for ag_action in agent_actions:
                   # check if there is any action that is directly applicable
                   if not applicable_action and ag_action in inst_action_map.keys() and set(inst_action_map[ag_action].precondition) <=inst_set:
                       op_fd.write(ag_action+"\n")
                       applicable_action = True
                       task.init = apply_action(task.init,inst_action_map[ag_action])
               # Make sure all thread queues are free
               work_queue.queue.clear()
               output_queue.queue.clear()

               if not applicable_action:
               # If we were unable to apply the action directly we need to plan to reach for the preconditions of one of the actions
                   plan_found = False
                   possible_plan = []
                   REPLACEABLE_ACTION_FLAG =  True # This flag determines whether we plan for the effects of an action or for preconditions of that action                   
                   index = 0
                   action_queue = [] 

                   small_problem_list = []
                   small_problem_objects = []

                   while index < len(agent_actions):
                       ag_action = agent_actions[index]
                       curr_ag = agent_list[index]
                       possible_action_list = [] 
                       goal_list = []
                       poss_action = None


                       if REPLACEABLE_ACTION_FLAG:
                           orig_arg_list = action_args.findall(ag_action)[0].split()[1:]
                           var_map = dict([(par.name, arg) for par, arg in zip(action_map[action_name].parameters,orig_arg_list)])                           
                           precond,inst_effects = action_map[action_name].instantiate_mini(var_map)
                           orig_arg_set = set()
                           for ind in range(len(orig_arg_list)):
                                   orig_arg_set.add((orig_arg_list[ind],ind))
                           goal_found = False
                           object_list = set()
                           pred_list = []
                           ag_pred_list = []
                           for cond,pred in inst_effects:
                               if len(set(pred.args) & set(agent_list)) == 0 and pred in possible_effects[p_index]:
                                   pred_list.append(pred)
                           goal_list = pred_list
                           poss_action = ag_action
                       else:
                           if ag_action  in inst_action_map.keys():
                               poss_action = ag_action

                       if poss_action:
                          if  not REPLACEABLE_ACTION_FLAG:
                              goal_list = inst_action_map[poss_action].precondition
                          for g in goal_step.keys():
                             if goal_step[g] < plan_step:
                                 goal_list.append(goal_map[g])
                          positive_goal = set()
                          neg_goal = set()
                          if len(goal_list) > 0:
                              for g in goal_list:
                                  if g.negated:
                                      neg_goal.add(g.negate())
                                  else:
                                      positive_goal.add(g)
                          inst_set = set(task.init)
                          if len(goal_list) > 0 and positive_goal <=inst_set and len(neg_goal & inst_set)==0:
                               #print ("Action "+poss_action +" Skipped with goals "+str(goal_list))
                               plan_found = True
                          else:
                               #run preprocess
                               goal_list.sort()
                               pddl_string = dump_pddl(task,goal_list)
                               #print ("Adding the action "+poss_action)
                               if pddl_string not in small_problem_list:
                                   small_problem_list.append(pddl_string)
                                   small_problem_objects.append((task,goal_list))
                               #work_queue.put(pddl_string)
                       index = index + 1
                   for sp_index in range(len(small_problem_list)):
                       t,g = small_problem_objects[sp_index]
                       work_queue.put((small_problem_list[sp_index],t,g,inst_actions,relaxed_reachable, atoms,axioms,reachable_action_params,args))
                   work_queue.join()
                   if not plan_found:
                       if output_queue.qsize() > 0: 
                           best_plan_length,best_plan = output_queue.get()
                           output_queue.task_done()
                           output_queue.queue.clear()
                       else:
                           best_plan_length = 0
                       if best_plan_length == 0:
                           if not REPLACEABLE_ACTION_FLAG:
                               REPLACEABLE_ACTION_FLAG = True
                               index = 0
                           else:
                               print ("unable to expand to this action "+action)
                               #exit(0)
                       else:
                           for p_action in best_plan:
                               op_fd.write(p_action)
                               task.init = apply_action(task.init,inst_action_map[p_action.strip().lower()])
                           ignored_actions = []
               

            inst_set = set(task.init)
            solved_goals = solved_goals + list(inst_set & original_goal_set)
            #original_goal_set = original_goal_set -  set(solved_goals)
            #print (original_goal_set - set(solved_goals))
            plan_found = False
            if len(original_goal_set - set(solved_goals) ) >=1:
               goal_list = list(original_goal_set) # - set(solved_goals))
               pddl_string =dump_pddl(task,goal_list)
               pfile_name = os.path.join(small_plan_dir,"small_prob_"+str(small_count)+".pddl")
               sol_file = os.path.join(small_plan_dir,"small_prob_"+str(small_count)+".sol")
               write_tofile(pddl_string,pfile_name)
               status = run_components.run_planner(domain_file,pfile_name,sol_file,10)
               #run search
               
               if status ==0 : #and os.path.exists('/home/ssreedh3/FFD_test/src/sas_plan'):
                   new_plan_fd = open(sol_file)
                   sub_plan = new_plan_fd.readlines()
                   new_plan_fd.close()
                   #print ("Expanded action : @ "+str(sub_plan))
                   if len(sub_plan) >= 1:
                       for p_action in sub_plan:
                        op_fd.write(p_action)
            op_fd.close()        
    except subprocess.CalledProcessError as err:
        print(err)
        sys.exit(err.returncode)


if __name__ == "__main__":
    main()
