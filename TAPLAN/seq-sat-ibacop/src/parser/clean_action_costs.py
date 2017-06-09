#!/usr/bin/python2.7


from pyparsing import OneOrMore, nestedExpr
import os
import sys
import collections


# -----------------------------------------------------------------------------
# action_to_pddl
#
# Devuelve la accion en formato pddl ignorando los action costs.
# -----------------------------------------------------------------------------
def action_to_pddl(action):
    text = ""

    if(isinstance(action, basestring)):
        text += action + " "

    else:
        if(len(action) > 0):
            if(action[0] != "increase"):
                text += "( "
                in_effects = False
                in_precondition = False

                for i in action:
                    if(isinstance(i, basestring)):
                        text += i + " "
                        if(i.lower() == ":effect"):
                            in_effects = True

                        elif(i.lower() == ":precondition"):
                            in_precondition = True
                    else:
                        text_aux = action_to_pddl(i)

                        if(in_effects):
                            in_effects = False

                            if((len(text_aux) == 0) or (text_aux.strip() == "( )")):
                                print "\n\nWarning: Se ha eliminado la accion al no contener efectos una vez eliminados los action costs: " , action
                                return ""

                        elif(in_precondition):
                            in_precondition = False

                            if(text_aux.strip() == "( )"):
                                print "\n\nWarning: Se ha insertado un predicado estatico en las precondiciones porque no habia ninguna: " , action
                                text_aux = " (tmpasdfghjalvaritomelosito) "

                        text += text_aux

                text += ") "
        else:
            text += " ( ) "

    return text


# -----------------------------------------------------------------------------
# list_to_pddl
#
# Devuelve la lista "element" en formato pddl.
# -----------------------------------------------------------------------------
def list_to_pddl(element):
    text = ""

    if(isinstance(element, basestring)):
        text += element + " "

    else:
        text += "( "

        for i in element:
            if(isinstance(i, basestring)):
                text += i + " "
            else:
                text += list_to_pddl(i)

        text += ") "

    return text


# -----------------------------------------------------------------------------
# get_num_of_when
#
# Cuenta el numero de when que hay en la accion.
# -----------------------------------------------------------------------------
def get_num_of_when(action):
    num = 0

    if(isinstance(action, basestring)):
        if(action == "when"):
            num += 1
    else:
        for i in action:
            if(i == "when"):
                num += 1
            elif(not isinstance(i, basestring)):
                num += get_num_of_when(i)

    return num


# -----------------------------------------------------------------------------
# get_increase
#
# Obtiene el efecto que modifica el coste total. En caso de no existir, se
# devuelve una cadena vacia de caracteres.
# -----------------------------------------------------------------------------
def get_increase(effects):
    increase = ""

    for element in effects:
        if(element == "increase"):
            #increase += "( "
            increase += list_to_pddl(effects)
            #increase += ")"
        elif not isinstance(element, basestring):
            increase += get_increase(element)

    return increase


# -----------------------------------------------------------------------------
# clean_domain_action_costs
#
# Elimina los elementos de action cost del dominio y los almacena en otro fichero.
# -----------------------------------------------------------------------------
def clean_domain_action_costs(original_domain, modified_domain, action_costs_domain_file):
    text = ""
    functions = ""
    action_costs = []

    # Leemos el dominio, lo almacenamos en un string y quitamos comentario
    pddl_lines = open(original_domain, 'r').readlines()

    for line in pddl_lines:

        if((len(line) > 0) and (line[len(line)-1] == '\n')):
            line = line[:-1] + " "	# Quitamos el \n

        line = line.lower()		# Todo a minuscula
        begin = line.find(";")		# Buscamos comentarios en la linea

        if(begin >= 0):			# Quitamos los comentarios de la linea
            line = line[0:begin]

        text += line


    # Parseamos el conetenido del pddl
    data = OneOrMore(nestedExpr()).parseString(text)

    if(len(data) == 1):

        # Creamos el fichero de salida
        new_file = open(modified_domain, 'w')
        new_file.write("(")

        for element in data[0]:

            if(element[0] == ":requirements"):
                requirements = "( "
                for req in element:

                    if(req != ":action-costs"):
                        requirements += req + " "

                requirements += " )"
                new_file.write(requirements + "\n")


            elif(element[0] == ":functions"):
                #functions = "( "
                functions += list_to_pddl(element)
                functions += " |"
                #functions += ") |"


            elif(element[0] == ":action"):
                name = element[1]

                effect_index = -1
                for index in xrange(1, len(element)):
                    if(element[index] == ":effect"):
                        effect_index = index + 1

                increase = ""
                numOfWhen = 0

                if(effect_index != -1):
                    increase = get_increase(element[effect_index])	# element[effect_index] es la lista de los efectos
                    numOfWhen = get_num_of_when(element)		# element es la lista completa de la accion
                    effects_aux = list_to_pddl(element[effect_index])	# element[effect_index] es la lista de los efectos

                    if(effects_aux.find("forall") >= 0):
                        numOfWhen = numOfWhen * 100

                    if(increase == ""):			# si la accion no tiene action costs
                        increase = "without_action_costs"

                    action_costs.append(str(name) + " | " + str(increase) + " | " + str(numOfWhen))
                    new_file.write(action_to_pddl(element) + "\n")

                else:
                    print "\n\nWarning: La accion no tiene etiqueta de efectos, por lo tanto se elimina la accion: ", element


            elif(element[0] == ":predicates"):
                predicates = list_to_pddl(element)
                predicates = predicates[0: predicates.rfind(")")]
                predicates += " (tmpasdfghjalvaritomelosito) )"
                new_file.write(predicates + "\n")

            else:
                new_file.write(list_to_pddl(element) + "\n")

        new_file.write(")")
        new_file.close()

    else:
        print("\nError: wrong domain pddl file\n")
        sys.exit(-1)

    output = open(action_costs_domain_file, 'w')

    if(len(functions) > 0):
        output.write(functions + "\n")
        print("\nFunctions: " + str(functions) + "\n")

    for i in action_costs:
        output.write(i + "\n")
        print("Action cost: " + str(i))
    print("")
    output.close()


# -----------------------------------------------------------------------------
# clean_problem_action_costs
#
# Elimina los elementos de action cost del dominio y los almacena en otro fichero.
# -----------------------------------------------------------------------------
def clean_problem_action_costs(original_problem, modified_problem, action_costs_problem_file):
    text = ""

    # Leemos el problema, lo almacenamos en un string y quitamos comentario
    pddl_lines = open(original_problem, 'r').readlines()

    for line in pddl_lines:

        if((len(line) > 0) and (line[len(line)-1] == '\n')):
            line = line[:-1] + " "	# Quitamos el \n

        line = line.lower()		# Todo a minuscula
        begin = line.find(";")		# Buscamos comentarios en la linea

        if(begin >= 0):			# Quitamos los comentarios de la linea
            line = line[0:begin]

        text += line


    # Parseamos el conetenido del pddl
    data = OneOrMore(nestedExpr()).parseString(text)

    # Abrimos el fichero de elementos descartados
    output = open(action_costs_problem_file, 'w')    

    if(len(data) == 1):

        # Creamos el fichero de salida
        new_file = open(modified_problem, 'w')
        new_file.write("(")

        for element in data[0]:

            if(element[0] == ":init"):
                new_file.write("(:init\n")

                for i in xrange(1, len(element)):
                    if(element[i][0] == "="):
                        output.write(list_to_pddl(element[i]) + "\n")
                        print("Fluent: " + list_to_pddl(element[i]) + "\n")
                    else:
                        new_file.write(list_to_pddl(element[i]) + "\n")

                new_file.write(" (tmpasdfghjalvaritomelosito) )\n")


            elif(element[0] == ":metric"):
                #metric = "( "
                metric = list_to_pddl(element)
                #metric += " )"
                output.write(metric + "\n")
                print("Metric: " + str(metric) + "\n")

            else:
                new_file.write(list_to_pddl(element) + "\n")

        new_file.write(")")
        new_file.close()

    else:
        print("\nError: wrong problem pddl file\n")
        sys.exit(-1)

    output.close()


# main
# -----------------------------------------------------------------------------
if __name__ == '__main__':

    if len(sys.argv) != 3:
        raise SystemExit("Usage: %s <DOMAINFILE> <PROBLEMFILE>" % sys.argv[0])

    else:
        # Calculamos la ruta absoluta del script
        pathname = os.path.dirname(sys.argv[0])
        scriptpath = os.path.abspath(pathname)

        if (os.path.isfile(sys.argv[1])):
            original_domain = os.path.abspath(sys.argv[1])

            if (os.path.isfile(sys.argv[2])):
                original_problem = os.path.abspath(sys.argv[2])

            else:
                print("The problem file does not exist: %s \n" % sys.argv[2])
                sys.exit(-1)

        else:
            print("The domain file does not exist: %s \n" % sys.argv[1])
            sys.exit(-1)


        modified_domain   	  = scriptpath + "/domain_without_action_costs.txt"
        modified_problem  	  = scriptpath + "/problem_without_action_costs.txt"
        action_costs_domain_file  = scriptpath + "/action_costs_from_domain_file.txt"
        action_costs_problem_file = scriptpath + "/action_costs_from_problem_file.txt"

        clean_domain_action_costs(original_domain, modified_domain, action_costs_domain_file)
        clean_problem_action_costs(original_problem, modified_problem, action_costs_problem_file)

