#!/usr/bin/python2.7


from pyparsing import OneOrMore, nestedExpr
import os
import sys
import collections


# -----------------------------------------------------------------------------
# action_to_pddl
#
# Devuelve la accion en formato pddl anadiendo los action costs.
# -----------------------------------------------------------------------------
def action_to_pddl(action, increase):
    text = ""

    if(isinstance(action, basestring)):
        text += action + " "

    else:

        text += "( "
        in_effects = False

        for i in action:
            if(isinstance(i, basestring)):
                text += i + " "
                if(i.lower() == ":effect"):
                    in_effects = True
            else:
                text += action_to_pddl(i, increase)
                if(in_effects):
                    text = text[0:text.rfind(")")]
                    text += increase + " )"
                    in_effects = False

        text += ") "

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
# restore_domain_action_costs
#
# Restaura los elementos de action cost del dominio.
# -----------------------------------------------------------------------------
def restore_domain_action_costs(input_domain, output_domain, action_costs_domain_file):
    text = ""
    functions = ""
    action_costs = []

    # Leemos el dominio de entrada, lo almacenamos en un string y quitamos comentario
    pddl_lines = open(input_domain, 'r').readlines()

    for line in pddl_lines:

        if((len(line) > 0) and (line[len(line)-1] == '\n')):
            line = line[:-1] + " "	# Quitamos el \n

        begin = line.find(";")		# Buscamos comentarios en la linea

        if(begin >= 0):			# Quitamos los comentarios de la linea
            line = line[0:begin]

        text += line


    # Leemos el ficheros de los action costs quitados del dominio
    data = open(action_costs_domain_file, 'r').readlines()
    for i in data:

        if((len(i) > 0) and (i[len(i)-1] == '\n')):
            i = i[:-1] + " "		# Quitamos el \n

        elements = i.split("|")

        if(len(elements) == 2):		# Functions
            functions = elements[0]

        elif(len(elements) == 3):	# Actions
            elements[0] = elements[0].strip()
            elements[0] = elements[0].upper()
            elements[1] = elements[1].strip()

            if(elements[1] != "without_action_costs"):
                action_costs.append(elements[0] + " | " + elements[1])

        else:
            print("\nUnexpected String: " + str(i) + "\n")


    # Si el dominio inicial no tiene costes, es necesario meter costes unitarios en las acciones originales
    if((len(functions.strip()) == 0) and (len(action_costs) == 0)):
        print("\nWarning: El dominio inicial no tenia action costs, por lo tanto se insertan en las acciones originales con coste 1")
        functions = "(:functions (total-cost))"

        # Volvemos a leer el ficheros de los action costs quitados del dominio
        data = open(action_costs_domain_file, 'r').readlines()
        for i in data:

            if((len(i) > 0) and (i[len(i)-1] == '\n')):
                i = i[:-1] + " "		# Quitamos el \n

            elements = i.split("|")

            if(len(elements) == 3):	# Actions
                elements[0] = elements[0].strip()
                elements[0] = elements[0].upper()
                action_costs.append(elements[0] + " | (increase (total-cost) 1)")

            else:
                print("\nUnexpected String: " + str(i) + "\n")


    print("\nFunctions: " + str(functions) + "\n")
    for j in action_costs:
        print("Action cost: " + str(j))


    # Parseamos el conetenido del pddl
    data = OneOrMore(nestedExpr()).parseString(text)

    if(len(data) == 1):

        # Creamos el fichero de salida
        new_file = open(output_domain, 'w')
        new_file.write("(")

        for element in data[0]:

            if(element[0].lower() == ":requirements"):
                requirements = "( "

                for req in element:
                    requirements += req + " "

                requirements += ":action-costs)"
                new_file.write(requirements + "\n")


            elif(element[0].lower() == ":predicates"):
                new_file.write(list_to_pddl(element) + "\n")
                new_file.write(functions + "\n")


            elif(element[0].lower() == ":action"):
                action_name = element[1].strip()
                begin = action_name.find("_---_")

                if(begin < 0):
                    begin = action_name.find("-___-")

                    if(begin < 0):
                        print("\nWARNING: accion sin separador: " + str(action_name) + "\n")
                        begin = len(action_name)

                if(len(action_costs) > 0):
                    action_found = False

                    for action_cost_element in action_costs:
                        element_list = action_cost_element.split("|")
                        action_cost_name = element_list[0].strip()

                        if(action_name[0:begin] == action_cost_name and action_name.find("condeff") < 0 and not action_found):
                            new_file.write(action_to_pddl(element, element_list[1]) + "\n")
                            action_found = True

                    if(not action_found):
                        new_file.write(list_to_pddl(element) + "\n")

                else:
                    new_file.write(list_to_pddl(element) + "\n")

            else:
                new_file.write(list_to_pddl(element) + "\n")

        new_file.write(")")
        new_file.close()

    else:
        print("\nError: wrong domain pddl file\n")
        sys.exit(-1)



# -----------------------------------------------------------------------------
# restore_problem_action_costs
#
# Restaura los elementos de action cost del problema.
# -----------------------------------------------------------------------------
def restore_problem_action_costs(input_problem, output_problem, action_costs_problem_file):
    text = ""
    metric = ""
    fluents = []

    # Leemos el problema, lo almacenamos en un string y quitamos comentario
    pddl_lines = open(input_problem, 'r').readlines()

    for line in pddl_lines:

        if((len(line) > 0) and (line[len(line)-1] == '\n')):
            line = line[:-1] + " "	# Quitamos el \n

        line = line.lower()		# Todo a minuscula
        begin = line.find(";")		# Buscamos comentarios en la linea

        if(begin >= 0):			# Quitamos los comentarios de la linea
            line = line[0:begin]

        text += line


    # Leemos el ficheros de los action costs quitados del problem
    data = open(action_costs_problem_file, 'r').readlines()
    for i in data:

        if((len(i) > 0) and (i[len(i)-1] == '\n')):
            i = i[:-1] + " "		# Quitamos el \n

        i = i.lower()			# Todo a minusculas

        if(i.find(":metric") >= 0):	# Metric
            metric += i

        elif(i.find("=") >= 0):		# Fluents
            fluents.append(i)

        else:
            print("\nUnexpected String: " + str(i) + "\n")


    # Si el dominio inicial no tiene costes, es necesario meter costes unitarios en las acciones originales
    if((len(metric.strip()) == 0) and (len(fluents) == 0)):
        print("\nWarning: El problema inicial no tenia action costs, por lo tanto se inserta la metrica y fluent por defecto")
        metric = "(:metric minimize (total-cost))"
        fluents.append("(= (total-cost) 0)")


    print("\nMetric: " + str(metric) + "\n")
    for j in fluents:
        print("Fluent: " + str(j))


    # Parseamos el conetenido del pddl
    data = OneOrMore(nestedExpr()).parseString(text)


    if(len(data) == 1):

        # Creamos el fichero de salida
        new_file = open(output_problem, 'w')
        new_file.write("(")

        for element in data[0]:

            if(element[0] == ":init"):
                new_file.write("(:init\n")

                for i in fluents:
                    new_file.write(i + "\n")

                for i in xrange(1, len(element)):
                    new_file.write(list_to_pddl(element[i]) + "\n")

                new_file.write(" )\n")


            elif(element[0] == ":goal"):
                new_file.write(list_to_pddl(element) + "\n")
                new_file.write(metric + "\n")

            else:
                new_file.write(list_to_pddl(element) + "\n")

        new_file.write(")")
        new_file.close()

    else:
        print("\nError: wrong problem pddl file\n")
        sys.exit(-1)



# main
# -----------------------------------------------------------------------------
if __name__ == '__main__':

    if len(sys.argv) != 3:
        raise SystemExit("Usage: %s <DOMAINFILE> <PROBLEMFILE>" % sys.argv[0])

    else:
        # Calculamos la ruta del validate
        pathname = os.path.dirname(sys.argv[0])
        scriptpath = os.path.abspath(pathname)


        if (os.path.isfile(sys.argv[1])):
            input_domain = os.path.abspath(sys.argv[1])

            if (os.path.isfile(sys.argv[2])):
                input_problem = os.path.abspath(sys.argv[2])

            else:
                print("The problem file does not exist: %s \n" % sys.argv[2])
                sys.exit(-1)

        else:
            print("The domain file does not exist: %s \n" % sys.argv[1])
            sys.exit(-1)


        output_domain   	  = scriptpath + "/domain_ready.txt"
        output_problem  	  = scriptpath + "/problem_ready.txt"
        action_costs_domain_file  = scriptpath + "/action_costs_from_domain_file.txt"
        action_costs_problem_file = scriptpath + "/action_costs_from_problem_file.txt"


        restore_domain_action_costs(input_domain, output_domain, action_costs_domain_file)
        restore_problem_action_costs(input_problem, output_problem, action_costs_problem_file)
