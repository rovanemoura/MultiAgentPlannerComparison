
import sys
import os
#from sets import Set

DFILE_KEYWORDS = ["requirements", "types", "predicates", "action", "private","functions","constants"]
DFILE_REQ_KEYWORDS = ["typing","strips","multi-agent","unfactored-privacy"]
DFILE_SUBKEYWORDS = ["parameters", "precondition", "effect", "duration"]
PFILE_KEYWORDS = ["objects", "init", "goal","private","metric"]
AFILE_KEYWORDS = ["agents"]
TA_NAME = ''
verbose = False


class Predicate(object):
  """A loose interpretation of a predicate used for all similar collections.

  Without a name it is a parameter list.
  It can be typed (or not).
    If typed then args = [[var, type], ...]
    Else args = [var, ...]
  It can be negated.
  It may contain variables or objects in its arguments.
  """
  def __init__(self, name, args, is_typed, is_negated):
    self.name = name
    self.args = args
    self.arity = len(args)
    self.is_typed = is_typed
    self.is_negated = is_negated
    self.ground_facts = set()
    self.agent_param = -1

  def pddl_rep(self):
    """Returns the PDDL version of the instance."""
    rep = ''
    if self.is_negated:
      rep += "(not "
    if self.name != "":
      rep += "(" + self.name + " "
    else:
      rep += "("
    for argument in self.args:
      if self.is_typed:
        rep += argument[0] + " - " + argument[1] + " "
      else:
        rep += argument + " "
    rep = rep[:-1]
    rep += ")"
    if self.is_negated:
      rep += ")"
    return rep

  def __repr__(self):
    return self.pddl_rep()



class Action(object):
  """Represents a simple non-temporal action."""
  def __init__(self, name, parameters, precondition, effect):
    self.name = name
    self.parameters = parameters
    self.precondition = precondition
    self.effect = effect
    self.duration = 1
    self.agent = ""
    self.agent_type = ""

  def pddl_rep(self):
    """Returns the PDDL version of the instance."""
    rep = ''
    rep += "(:action " + self.name + "\n"
    rep += "\t:parameters " + str(self.parameters) + "\n"
    if len(self.precondition) > 1:
      rep += "\t:precondition (and\n"
    else:
      rep += "\t:precondition \n"
    for precon in self.precondition:
      rep += "\t\t" + str(precon) + "\n"
    if len(self.precondition) > 1:
      rep += "\t)\n"
    if len(self.effect) > 1:
      rep += "\t:effect (and\n"
    else:
      rep += "\t:effect \n"
    for eff in self.effect:
      rep += "\t\t" + str(eff) + "\n"
    if len(self.effect) > 1:
      rep += "\t)\n"
    rep += ")\n"
    return rep

  def __repr__(self):
    return self.name #+ str(self.parameters)

class Function(object):
  def __init__(self, obj_list):
    self.obj_list = obj_list

  def pddl_rep(self):
    """Returns the PDDL version of the instance."""
    rep = '('
    for argument in self.obj_list:
      rep += argument + " "
    rep = rep[:-1]
    rep += ") - number"
    return rep

  def __repr__(self):
    return self.pddl_rep()

class GroundFunction(object):
  def __init__(self, obj_list):
    self.obj_list = obj_list

  def pddl_rep(self):
    """Returns the PDDL version of the instance."""
    rep = '(' + self.obj_list[0] + " ("
    for argument in self.obj_list[1:-1]:
      rep += argument + " "
    rep = rep[:-1]
    rep += ") " + self.obj_list[-1] + ") "
    return rep

  def __repr__(self):
    return self.pddl_rep()



class PlanningProblem(object):
  def __init__(self, domainfile, problemfile):
    self.domain = '' #String
    self.requirements = set() #[String]
    self.type_list = set() #{String}
    self.type_list.add('object')
    self.types = {} #Key = supertype_name, Value = type
    self.predicates = [] #[Predicate]
    self.functions = []
    self.ground_functions = []
    self.actions = [] #[Action]
    self.agent_types = set()
    self.agents = set()
    self.agent_location_predicate = {}
    self.private_objects = {}
    self.private_object_list = set()
    self.private_object_map = {}
    self.private_predicate_list = set()
    self.problem = '' #String
    self.object_list = set() #{String}
    self.objects = {} #Key = type, Value = object_name
    self.constants = {} #Key = type, Value = object_name
    self.init = [] #List of Predicates
    self.goal = [] #List of Predicates
    self.metric = False
    
    self.parse_domain(domainfile)
    self.parse_problem(problemfile)

    for t in self.agent_types:
      self.agents = self.agents | self.get_objects_of_type(t)
    
    self.requirements = self.requirements - {"multi-agent","unfactored-privacy"}
    
  def parse_domain(self, domainfile):
    """Parses a PDDL domain file."""
    
    with open(domainfile) as dfile:
      dfile_array = self._get_file_as_array(dfile)
    #Deal with front/end define, problem, :domain
    if dfile_array[0:4] != ['(', 'define', '(', 'domain']:
      print ('PARSING ERROR: Expected (define (domain ... at start of domain file')
      sys.exit()
    self.domain = dfile_array[4]
    dfile_array = dfile_array[6:-1]
    opencounter = 0
    keyword = ''
    obj_list = []
    is_obj_list = True
    #print (dfile_array)
    #exit(0)
    for word in dfile_array:
      if word == '(':
        opencounter += 1
      elif word == ')':
        opencounter -= 1
      elif word.startswith(':'):
        if word[1:] not in DFILE_KEYWORDS:
          pass
        elif keyword != 'requirements':
          keyword = word[1:]
        #if keyword  == 'action':
        #  obj_list = []
      if opencounter == 0:
        if keyword == 'functions':
            obj_list = []
        if keyword == 'action':
          #print(obj_list)
          #exit(0)
          self.actions.append(obj_list)
          obj_list = []
        if keyword == 'types':
          for element in obj_list:
            self.types.setdefault('object', []).append(element)
            self.type_list.add('object')
            self.type_list.add(element)
          obj_list = []
        keyword = ''

      if keyword == 'requirements': #Requirements list
        if word != ':requirements':
          if not word.startswith(':'):
            print ('PARSING ERROR: Expected requirement to start with :')
            sys.exit()
          elif word[1:] not in DFILE_REQ_KEYWORDS:
            print ('WARNING: Unknown Rquierement ' + word[1:])
            #print 'Requirements must only be: ' + str(DFILE_REQ_KEYWORDS)
            #sys.exit()
          else:
            self.requirements.add(word[1:])
      elif keyword == 'action':
        obj_list.append(word)
        #print (obj_list)
      elif not word.startswith(':'):
        if keyword == 'types': #Typed list of objects
          if is_obj_list:
            if word == '-':
              is_obj_list = False
            else:
              obj_list.append(word)
          else:
            #word is type
            for element in obj_list:
              if not word in self.type_list:
                self.types.setdefault('object', []).append(word)
                self.type_list.add(word)
              self.types.setdefault(word, []).append(element)
              self.type_list.add(element)
              self.type_list.add(word)
            is_obj_list = True
            obj_list = []
        elif keyword == 'constants': #Typed list of objects
          if is_obj_list:
            if word == '-':
              is_obj_list = False
            else:
              obj_list.append(word)
          else:
            #word is type
            for element in obj_list:
              if word in self.type_list:
                self.constants.setdefault(word, []).append(element)
                #self.object_list.add(element)
              else:
                print (self.type_list)
                print ("ERROR unknown type " + word)
                sys.exit()
            is_obj_list = True
            obj_list = []
        elif keyword == 'predicates' or keyword == 'private' or keyword == 'pr_predicates': #Internally typed predicates
          if word == ')':
            if keyword == 'private':
              #print ("...skip agent: " +  str(obj_list[:3]))
              #print ("parse predicate: " + p_name + " " + str(obj_list))
              #agent_name = obj_list[:3]
              obj_list = obj_list[3:]
              keyword = 'pr_predicates'
            if len(obj_list) == 0:
              #print "...skip )"
              continue
            p_name = obj_list[0]
            #print "parse predicate: " + p_name + " " + str(obj_list)
            pred_list = self._parse_name_type_pairs(obj_list[1:],self.type_list)
            self.predicates.append(Predicate(p_name, pred_list, True, False))
            if keyword == 'pr_predicates':
              self.private_predicate_list.add(p_name)
            obj_list = []
          elif word != '(':
            obj_list.append(word)
        elif keyword == 'functions': #functions
          #print (word)
          #print (obj_list)
          if word == ')':
            p_name = obj_list[0]
            if obj_list[0] == '-':
              obj_list = obj_list[2:]
            #print ("function: " + word + " - " + str(obj_list))
            self.functions.append(Function(obj_list))
            obj_list = []
          elif word != '(':
            obj_list.append(word)
    #print (self.functions)
    #Work on the actions
    #print (self.actions)
    #exit(0)
    new_actions = []
    for action in self.actions:
      act_name = action[1]
      act = {}
      action = action[2:]
      keyword = ''
      for word in action:
        if word.startswith(':'):
          keyword = word[1:]
        else:
          act.setdefault(keyword, []).append(word)
      self.agent_types.add(act.get('agent')[2])
      agent = self._parse_name_type_pairs(act.get('agent'),self.type_list)
      param_list = agent + self._parse_name_type_pairs(act.get('parameters')[1:-1],self.type_list)
      up_params = Predicate('', param_list, True, False)
      pre_list = self._parse_unground_propositions(act.get('precondition'))
      eff_list = self._parse_unground_propositions(act.get('effect'))
      new_act = Action(act_name, up_params, pre_list, eff_list)
      #print (agent)
      ag_type,ag_name = agent[0]
      if ag_type not in self.agent_location_predicate.keys():
        self.agent_location_predicate[ag_type] = []
        for pre_item in pre_list:
            if ag_type in pre_item.args:
                self.agent_location_predicate[ag_type].append(pre_item.name)
      else:
        pre_list_name = []
        for pre_item in pre_list:
            pre_list_name.append(pre_item.name)
        for item in self.agent_location_predicate[ag_type]:
            if item not in pre_list_name:
                self.agent_location_predicate[ag_type].remove(item)
      
      #exit(0)
      #self.agent_location_predicate[ag_type]
      print (self.agent_location_predicate)
      new_actions.append(new_act)
    self.actions = new_actions

  def parse_problem(self, problemfile):
    """The main method for parsing a PDDL files."""

    with open(problemfile) as pfile:
      pfile_array = self._get_file_as_array(pfile)
    #Deal with front/end define, problem, :domain
    if pfile_array[0:4] != ['(', 'define', '(', 'problem']:
      print ('PARSING ERROR: Expected (define (problem ... at start of problem file')
      sys.exit()
    self.problem = pfile_array[4]
    if pfile_array[5:8] != [')', '(', ':domain']:
      print ('PARSING ERROR: Expected (:domain ...) after (define (problem ...)')
      sys.exit()
    if self.domain != pfile_array[8]:
      print ('ERROR - names don\'t match between domain and problem file.')
      #sys.exit()
    if pfile_array[9] != ')':
      print ('PARSING ERROR: Expected end of domain declaration')
      sys.exit()
    pfile_array = pfile_array[10:-1]

    opencounter = 0
    keyword = ''
    is_obj_list = True
    is_function = False
    obj_list = []
    int_obj_list = []
    int_opencounter = 0
    for word in pfile_array:
      if word == '(':
        opencounter += 1
      elif word == ')':
        if keyword == 'objects':
          obj_list = []
        opencounter -= 1
      elif word.startswith(':'):
        if word[1:] not in PFILE_KEYWORDS:
          print ('PARSING ERROR: Unknown keyword: ' + word[1:])
          print ('Known keywords: ' + str(PFILE_KEYWORDS))
        else:
          keyword = word[1:]
      if opencounter == 0:
        keyword = ''

      if not word.startswith(':'):
        if keyword == 'objects' or keyword == 'private' or keyword == 'pr_objects': #Typed list of objects
          #print "word: " + word
          #print "obj_list: " + str(obj_list)
          if keyword == 'private':
              #print "...skip agent: " +  word
              self.private_object_map.setdefault(word, {})
              obj_list = []
              keyword = 'pr_objects'
              pr_agent = word
              #self.private_objects.append(word)
              continue
          if is_obj_list:
            if word == '-':
              is_obj_list = False
            elif word != ')':
              obj_list.append(word)
          else:
            #word is type
            for element in obj_list:
              if word in self.type_list:
                self.objects.setdefault(word, []).append(element)   
                self.object_list.add(element)
                if keyword == 'pr_objects':
                    self.private_objects.setdefault(word, []).append(element) 
                    self.private_object_list.add(element)
                    self.private_object_map[pr_agent][word] = element
              else:
                print (self.type_list)
                print ("ERROR unknown type " + word)
                sys.exit()
            is_obj_list = True
            obj_list = []
        elif keyword == 'init':
           if word == ')':
             if obj_list[0] == '=' and is_function == False:
               is_function = True
             else:
               if is_function:
                 #print "function: " + str(obj_list)
                 self.ground_functions.append(GroundFunction(obj_list))
                 is_function = False
               else:
                 #print ("predicate: " + str(obj_list))
                 self.init.append(Predicate(obj_list[0], obj_list[1:],False, False))
               obj_list = []
           elif word != '(':
             obj_list.append(word)
        elif keyword == 'goal':
          if word == '(':
            int_opencounter += 1
          elif word == ')':
            int_opencounter -= 1
          obj_list.append(word)
          if int_opencounter == 0:
              self.goal = self._parse_unground_propositions(obj_list)
              obj_list = []
        elif keyword == 'metric':
          self.metric = True
          obj_list = []

  def get_type_of_object(self,obj):
    for t in self.objects.keys():
      if obj in self.objects[t]:
        return t
    for t in self.constants.keys():
      if obj in self.constants[t]:
        return t
    for t in self.private_objects.keys():
      if obj in self.private_objects[t]:
        return t

  def get_objects_of_type(self,of_type):
    #print "get objects of type " + of_type
    selected_types = {of_type}
    pre_size = 0
    while len(selected_types) > pre_size:
      pre_size = len(selected_types)
      for t in selected_types:
        if t in self.types:
          selected_types = selected_types | set(self.types[t])
    #print selected_types
    selected_objects = set()
    for t in selected_types:
      if t in self.objects:
        selected_objects = selected_objects | set(self.objects[t])
      if t in self.constants:
        selected_objects = selected_objects | set(self.constants[t])
    return selected_objects

  def print_domain(self):
    """Prints out the planning problem in (semi-)readable format."""
    print ('\n*****************')
    print ('DOMAIN: ' + self.domain)
    print ('REQUIREMENTS: ' + str(self.requirements))
    print ('TYPES: ' + str(self.types))
    print ('PREDICATES: ' + str(self.predicates))
    print ('ACTIONS: ' + str(self.actions))
    print ('FUNCTIONS: ' + str(self.functions))
    print ('CONSTANTS: ' + str(self.constants))
    print ('****************')

  def print_problem(self):
    """Prints out the planning problem in (semi-)readable format."""
    print ('\n*****************')
    print ('PROBLEM: ' + self.problem)
    print ('OBJECTS: ' + str(self.objects))
    print ('INIT: ' + str(self.init))
    print ('GOAL: ' + str(self.goal))
    print ('AGENTS: ' + str(self.agents))
    print ('PRIVATE OBJECTS: ' + str(self.private_objects))
    print ('PRIVATE OBJECTS MAP: ' + str(self.private_object_map))
    print ('****************')
  
  def print_TA_problem(self):
    """Prints out the planning problem in (semi-)readable format."""
    print ('\n*****************')
    print ('PROBLEM: ' + self.problem)
    print ('OBJECTS: ' + str(self.TA_objects))
    print ('INIT: ' + str(self.init))
    print ('GOAL: ' + str(self.goal))
    print ('AGENTS: ' + str(TA_NAME))
    print ('PRIVATE OBJECTS: ' + str(self.TA_private_objects))
    print ('PRIVATE OBJECTS MAP: ' + str(self.TA_private_object_map))
    print ('****************')


         
    
  #Get string of file with comments removed - comments are rest of line after ';'
  def _get_file_as_array(self, file_):
    """Returns the file split into array of words.

    Removes comments and separates parenthesis.
    """
    file_as_string = ""
    for line in file_:
      if ";" in line:
        line = line[:line.find(";")]
      line = (line.replace('\t', '').replace('\n', ' ')
          .replace('(', ' ( ').replace(')', ' ) '))
      file_as_string += line
    file_.close()
    return file_as_string.strip().split()

  def _parse_name_type_pairs(self, array, types):
    """Parses array creating paris of form (name, type).

    Expects array such as [?a, -, agent, ...]."""
    pred_list = []
    if len(array)%3 != 0:
      print ("Expected predicate to be typed " + str(array))
      sys.exit()
    for i in range(0, int(len(array)/3)):
      if array[3*i+1] != '-':
        print ("Expected predicate to be typed")
        sys.exit()
      if array[3*i+2] in types:
        pred_list.append((array[3*i], array[3*i+2]))
      else:
        print ("PARSING ERROR {} not in types list".format(array[3*i+2]))
        print ("Types list: {}".format(self.type_list))
        sys.exit()
    return pred_list

  def _parse_unground_proposition(self, array):
    """Parses a variable proposition returning dict."""
    negative = False
    if array[1] == 'not':
      negative = True
      array = array[2:-1]
    return Predicate(array[1], array[2:-1], False, negative)

  def _parse_unground_propositions(self, array):
    """Parses possibly conjunctive list of unground propositions.

    Expects array such as [(and, (, at, ?a, ?x, ), ...].
    """
    prop_list = []
    if array[0:3] == ['(', 'and', '(']:
      array = array[2:-1]
    #Split array into blocks
    opencounter = 0
    prop = []
    for word in array:
      if word == '(':
        opencounter += 1
      if word == ')':
        opencounter -= 1
      prop.append(word)
      if opencounter == 0:
        prop_list.append(self._parse_unground_proposition(prop))
        prop = []
    #print array[:array.index(')') + 1]
    return prop_list

  def write_pddl_domain(self, output_file):
    """Writes an unfactored MA-PDDL domain file for this planning problem."""
    file_ = open(output_file, 'w')
    to_write = "(define (domain " + self.domain + ")\n"
    #Requirements
    to_write += "\t(:requirements"
    for r in self.requirements:
      to_write += " :"+r
    to_write += ")\n"
    #Types
    to_write += "(:types\n"
    for type_ in self.types:
      to_write += "\t"
      for key in self.types.get(type_):
        to_write += key + " "
      to_write += "- " + type_
      to_write += "\n"
    to_write += ")\n"
    #Constants
    if len(self.constants) > 0:
      to_write += "(:constants\n"
      for t in self.constants.keys():
        to_write += "\t"
        for c in self.constants[t]:
          to_write += c + " "
        to_write += " - " + t + "\n" 
      to_write += ")\n"
    #Public predicates
    to_write += "(:predicates\n"
    predicate_set = set()
    for item in self.predicates:
        predicate_set.add("\t{}\n".format(item.pddl_rep()))
    for predicate in predicate_set:
      to_write += predicate
    to_write += ")\n"
    #Functions
    if len(self.functions) > 0:
      to_write += "(:functions\n"
      for function in self.functions:
        to_write += "\t{}\n".format(function.pddl_rep())
      to_write += ")\n"
    #Actions
    for action in self.actions:
      to_write += "\n{}\n".format(action.pddl_rep())
    
    #Endmatter
    to_write += ")" #Close domain defn
    file_.write(to_write)
    file_.close()

  def compile_to_TA_problem(self):
    global TA_NAME
    self.TA_objects = {}
    self.TA_object_list = []
    self.TA_private_objects = {}
    self.TA_private_object_list = []
    self.TA_private_object_map = {}
    agent_list =  list(self.agents)
    TA_NAME = agent_list[0]
    #print (self.agents)
    # objects
    for object_types in self.objects.keys():
        self.TA_objects[object_types] = []
        self.TA_private_objects[object_types] = []
        for object in self.objects[object_types]:
            if object in agent_list:
                if len(self.TA_objects[object_types]) == 0:
                    self.TA_objects[object_types].append(TA_NAME)
                    self.TA_object_list.append(TA_NAME)
            elif object in self.private_object_list:
                if len(self.TA_private_objects[object_types]) == 0:
                    self.TA_private_objects[object_types].append(object)
                    self.TA_private_object_list.append(object)
            else:
                self.TA_objects[object_types].append(object)
                self.TA_object_list.append(object)

    for agent in self.agents:
        self.TA_private_object_map[agent] = {}
        if agent in self.private_object_map.keys():
            object_dict = self.private_object_map[agent]
            for ty in object_dict.keys():
                if  object_dict[ty] not in self.agents:
                    self.TA_private_object_map[agent][self.TA_private_objects[ty][0]] = object_dict[ty]
    # Init
    self.TA_init = []
    agent_loc_pred_count = {}
    #for pred_list in self.agent_location_predicate.values():
    #    for pred in pred_list:
    #        agent_loc_pred_count[pred] = 0
    print (agent_loc_pred_count)
    for predicate in self.init:
        #print(predicate.name)
        #print (predicate.args)
        agent_specific = False
        if predicate.is_typed:
            for index in range(len(predicate.args)):
                arg,type = predicate.args[index]
                if arg in agent_list:
                    #print(predicate.name)
                    agent_specific = True
                    predicate.args[index] = [TA_NAME,type]
                elif arg in self.private_object_list:
                    predicate.args[index] = [self.TA_private_objects[type][0],type]
        else:
            for index in range(len(predicate.args)):
                arg = predicate.args[index]
                if arg in agent_list:
                    #print(predicate.name)
                    #print (predicate.args)
                    agent_specific = True
                    predicate.args[index] = TA_NAME
                    #print (predicate.args)
                elif arg in self.private_object_list:
                    predicate.args[index] = self.TA_private_objects[self.get_type_of_object(arg)][0]
        #if predicate.name in self.private_predicate_list:
            #if private_pred_count[predicate.name] == 0:
            #    private_pred_count[predicate.name] = 1
        #    self.TA_init.append(predicate)
        #else:
        if agent_specific and predicate.name in agent_loc_pred_count.keys():
            if agent_loc_pred_count[predicate.name] == 0:
                self.TA_init.append(predicate)
                agent_loc_pred_count[predicate.name] = 1
            else:
                print (predicate)
        else:
            #print (predicate.name)
            self.TA_init.append(predicate)

    # Goals
    self.TA_goals = []
    self.TA_agent_goals = []
    for predicate in self.goal:
        agent_specific = False
        for index in range(len(predicate.args)):
            if predicate.is_typed:
                arg,type = predicate.args[index]
            else:
                arg = predicate.args[index]
            if arg in agent_list:
                agent_specific = True
        if agent_specific:
            self.TA_agent_goals.append(predicate)
        else:
            self.TA_goals.append(predicate)



  def write_pddl_problem(self, output_file):
    file_ = open(output_file, 'w')
    to_write = "(define (problem " + self.problem +") "
    to_write += "(:domain " + self.domain + ")\n"
    #Objects
    to_write += "(:objects\n"
    for obj in self.object_list:
      to_write += "\t" + obj + " - " + self.get_type_of_object(obj) + "\n"
    to_write += ")\n"
    to_write += "(:init\n"
    for predicate in self.init:
      to_write += "\t{}\n".format(predicate)
    for function in self.ground_functions:
      to_write += "\t{}\n".format(function)
    to_write += ")\n"
    to_write += "(:goal\n\t(and\n"
    for goal in self.goal:
      to_write += "\t\t{}\n".format(goal)
    to_write += "\t)\n)\n"
    if self.metric:
      to_write += "(:metric minimize (total-cost))\n" 
    #Endmatter
    to_write += ")"
    file_.write(to_write)
    file_.close()

  def write_pddl_TA_problem(self, output_file):
    file_ = open(output_file, 'w')
    to_write = "(define (problem " + self.problem +") "
    to_write += "(:domain " + self.domain + ")\n"
    #Objects
    to_write += "(:objects\n"
    for obj_type in self.TA_objects.keys():
      for obj in self.TA_objects[obj_type]:
        to_write += "\t" + obj + " - " + obj_type + "\n"
    for obj_type in self.TA_private_objects.keys():
      for obj in self.TA_private_objects[obj_type]:
        to_write += "\t" + obj + " - " + obj_type + "\n"
    to_write += ")\n"
    to_write += "(:init\n"
    init_set = set()
    for predicate in self.TA_init:
      init_set.add("\t{}\n".format(predicate))
    to_write += ''.join(list(init_set))
    for function in self.ground_functions:
      to_write += "\t{}\n".format(function)
    to_write += ")\n"
    to_write += "(:goal\n\t(and\n"
    for goal in self.TA_goals:
      to_write += "\t\t{}\n".format(goal)
    to_write += "\t)\n)\n"
    if self.metric:
      to_write += "(:metric minimize (total-cost))\n"
    #Endmatter
    to_write += ")"
    file_.write(to_write)
    file_.close()




  def write_addl(self, output_file):
    file_ = open(output_file, 'w')
    to_write = "(define (problem " + self.problem +") "
    to_write += "(:domain " + self.domain + ")\n"
    #Objects
    to_write += "(:agents"
    for obj in self.agents:
      to_write += " " + obj 
    to_write += ")\n"
    to_write += ")"
    file_.write(to_write)
    file_.close()

  def write_agent_list(self, output_file):
    file_ = open(output_file, 'w')
    to_write = ""
    for obj in self.agents:
      to_write += obj + "\n"
    file_.write(to_write)
    file_.close()

  def write_private_list(self, output_file):
    file_ = open(output_file, 'w')
    to_write = ""
    for obj in self.private_predicate_list:
      to_write += obj + "\n"
    file_.write(to_write)
    file_.close()

  def write_ta_name(self, output_file):
    file_ = open(output_file, 'w')
    to_write = TA_NAME 
    file_.write(to_write)
    file_.close()

  def write_private_map(self, output_file):
    import json
    file_ = open(output_file, 'w')
    to_write = json.dumps(self.TA_private_object_map)
    file_.write(to_write)
    file_.close()

  def write_private_objects(self, output_file):
    import json
    file_ = open(output_file, 'w')
    to_write = ""
    for obj in self.TA_private_object_list:
      to_write += obj + "\n"
    file_.write(to_write)
    file_.close()

  def write_agent_goals(self, output_file):
    import json
    file_ = open(output_file, 'w')
    to_write = ""
    for goal in self.TA_agent_goals:
      to_write += "{}\n".format(goal)
    file_.write(to_write)
    file_.close()


if __name__ == "__main__":
  if len(sys.argv) < 4:
    print ('Requires 2 args')
    print ('arg1: folder')
    print ('arg2: domain')
    print ('arg3: problem')
    print ('arg4: output folder')
  else:
    pp = PlanningProblem(sys.argv[1] + "/" + sys.argv[2] + ".pddl", sys.argv[1] + "/" + sys.argv[3] + ".pddl")
    
    if verbose:
        pp.print_domain()
        pp.print_problem()
        #pp.compile_to_TA_problem()
        #pp.print_TA_problem()
        #pp.print_TA_problem()
    #exit(0)
    if not os.path.exists(sys.argv[4]):
      os.mkdir(sys.argv[4])

    pp.write_pddl_domain(sys.argv[4] + "/" + sys.argv[2] + ".pddl")
    pp.write_pddl_problem(sys.argv[4] + "/" + sys.argv[3] + ".pddl")
    pp.compile_to_TA_problem()
    #pp.print_TA_problem()
    pp.write_pddl_TA_problem(sys.argv[4] + "/" + sys.argv[3] + "_ta" + ".pddl")
    #pp.write_addl(sys.argv[4] + "/" + sys.argv[3] + ".addl")
    pp.write_agent_list(sys.argv[4] + "/" + sys.argv[3] + ".agents")
    pp.write_ta_name(sys.argv[4] + "/" + sys.argv[3] + ".ta_name")
    pp.write_private_list(sys.argv[4] + "/" + sys.argv[3] + ".private")
    pp.write_private_map(sys.argv[4] + "/" + sys.argv[3] + ".private_map")
    pp.write_private_objects(sys.argv[4] + "/" + sys.argv[3] + ".private_objects")
    pp.write_agent_goals(sys.argv[4] + "/" + sys.argv[3] + ".agent_goals")
    




