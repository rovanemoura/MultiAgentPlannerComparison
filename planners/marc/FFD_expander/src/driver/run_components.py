# -*- coding: utf-8 -*-

import logging
import os
import os.path
import subprocess
import sys
import signal
from . import portfolio_runner
from .plan_manager import PlanManager


DRIVER_DIR = os.path.abspath(os.path.dirname(__file__))
SRC_DIR = os.path.dirname(DRIVER_DIR)
TRANSLATE = os.path.join(SRC_DIR, "translate", "translate.py")
PREPROCESS = os.path.join(SRC_DIR, "preprocess", "preprocess")
SEARCH_DIR = os.path.join(SRC_DIR, "search")
FF = os.path.join(SRC_DIR, "Metric-FF","ff")


def timeout_handler(signum, frame):
    raise TimeoutError

def call_cmd(cmd, args, debug,timeout_command=[], stdin=None):
    if not os.path.exists(cmd):
        target = " debug" if debug else ""
        raise IOError(
            "Could not find %s. Please run \"./build_all%s\"." %
            (cmd, target))
    sys.stdout.flush()
    if stdin:
        with open(stdin) as stdin_file:
            try:
                ret = subprocess.check_call(timeout_command + [cmd] + args, stdin=stdin_file)
            except:
                return 1
    else:
        try:
            #print (timeout_command[cmd] + args)
            ret = subprocess.check_call(timeout_command + [cmd] + args)
        except subprocess.CalledProcessError as e:
            #print (dir(subprocess.CalledProcessError))
            return e.returncode
    return ret


def run_translate(args):
    logging.info("Running translator.")
    logging.info("translator inputs: %s" % args.translate_inputs)
    logging.info("translator arguments: %s" % args.translate_options)
    call_cmd(TRANSLATE, args.translate_inputs + args.translate_options,
             debug=args.debug)


def run_preprocess(args):
    logging.info("Running preprocessor.")
    logging.info("preprocessor input: %s" % args.preprocess_input)
    logging.info("preprocessor arguments: %s" % args.preprocess_options)
    call_cmd(PREPROCESS, args.preprocess_options, debug=args.debug,
             stdin=args.preprocess_input)


def run_planner(domain,problem,sol_path,timeout_mins=1):
    #signal.signal(signal.SIGALRM, timeout_handler)
    #signal.alarm(timeout_secs)
    ff_args = ["-o",domain,"-f",problem,"-s",sol_path]
    timeout_cmd = ["timeout","-sKILL",str(timeout_mins)+'m']
    #try:
    status = call_cmd(FF,ff_args,False,timeout_cmd)
        #signal.alarm(0)
    #except TimeoutError:
    #    return 124
    return (status)

def run_search(args):
    #ffd timeout
    timeout_mins=1
    timeout_cmd = ["timeout","-sKILL",str(timeout_mins)+'m']
    logging.info("Running search.")
    logging.info("search input: %s" % args.search_input)

    plan_manager = PlanManager(args.plan_file)
    plan_manager.delete_existing_plans()

    if args.debug:
        executable = os.path.join(SEARCH_DIR, "downward-debug")
    else:
        executable = os.path.join(SEARCH_DIR, "downward-release")
    logging.info("search executable: %s" % executable)

    if args.portfolio:
        assert not args.search_options
        logging.info("search portfolio: %s" % args.portfolio)
        portfolio_runner.run(
            args.portfolio, executable, args.search_input, plan_manager)
    else:
        if not args.search_options:
            raise ValueError(
                "search needs --alias, --portfolio, or search options")
        if "--help" not in args.search_options:
            args.search_options.extend(["--internal-plan-file", args.plan_file])
        logging.info("search arguments: %s" % args.search_options)
        return call_cmd(executable, args.search_options, debug=args.debug,timeout_command=timeout_cmd,
                 stdin=args.search_input)
