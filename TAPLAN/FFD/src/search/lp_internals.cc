#include "lp_internals.h"

#ifdef USE_LP
#include "lp_solver.h"
#include "utilities.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Woverflow"
#include <OsiSolverInterface.hpp>

#ifdef COIN_HAS_CLP
#include <OsiClpSolverInterface.hpp>
#endif

#ifdef COIN_HAS_CPX
#include <OsiCpxSolverInterface.hpp>
#endif

#ifdef COIN_HAS_GRB
#include <OsiGrbSolverInterface.hpp>
#endif

#pragma GCC diagnostic pop

using namespace std;

// CPLEX warning that is misleadingly reported with the severity of a critical error.
static const string CPLEX_WARNING_COMPRESS = "CPX0000  Compressing row and column files.";
// CPLEX warning from writeMps if no column names are defined.
static const string CPLEX_WARNING_WRITE_MPS_COLUMNS = "CPX0000  Default column names x1, x2 ... being created.";
static const string CPLEX_WARNING_WRITE_MPS_ROWS = "CPX0000  Default row    names c1, c2 ... being created.";
static const string CPLEX_ERROR_OOM = "CPX0000  CPLEX Error  1001: Out of memory.";
static const string CPLEX_ERROR_OOM_PRE = "CPX0000  Insufficient memory for presolve.";
static const string CPLEX_ERROR_OOM_DEVEX = "CPX0000  Not enough memory for devex.";

/*
  CPLEX sometimes does not report errors as exceptions and only prints an
  error message. This class will report any error messages as usual but will
  exit with a critical error afterwards.
*/
class ErrorCatchingCoinMessageHandler : public CoinMessageHandler {
public:
    ErrorCatchingCoinMessageHandler()
        : CoinMessageHandler() {
        setLogLevel(0);
    }

    ~ErrorCatchingCoinMessageHandler() {
    }

    virtual void checkSeverity() {
        /*
          Note that currentMessage_ should be used here but it doesn't help for clpex:
            currentMessage_.severity() is always "I"
            currentMessage_.externalNumber() is always 0
            currentMessage_.detail() is always empty
            currentMessage_.message() also is empty (NFI)
        */
        if (messageBuffer_ == CPLEX_WARNING_COMPRESS ||
            messageBuffer_ == CPLEX_WARNING_WRITE_MPS_COLUMNS ||
            messageBuffer_ == CPLEX_WARNING_WRITE_MPS_ROWS) {
            CoinMessageHandler::checkSeverity();
        } else if (messageBuffer_ == CPLEX_ERROR_OOM ||
                   messageBuffer_ == CPLEX_ERROR_OOM_PRE ||
                   messageBuffer_ == CPLEX_ERROR_OOM_DEVEX) {
            exit_with(EXIT_OUT_OF_MEMORY);
        } else {
            exit_with(EXIT_CRITICAL_ERROR);
        }
    }
};

unique_ptr<OsiSolverInterface> create_lp_solver(LPSolverType solver_type) {
    string missing_symbol;
    OsiSolverInterface *lp_solver = 0;
    switch (solver_type) {
    case LPSolverType::CLP:
#ifdef COIN_HAS_CLP
        lp_solver = new OsiClpSolverInterface;
#else
        missing_symbol = "COIN_HAS_CLP";
#endif
        break;
    case LPSolverType::CPLEX:
#ifdef COIN_HAS_CPX
        lp_solver = new OsiCpxSolverInterface;
        lp_solver->passInMessageHandler(new ErrorCatchingCoinMessageHandler);
#else
        missing_symbol = "COIN_HAS_CPX";
#endif
        break;
    case LPSolverType::GUROBI:
#ifdef COIN_HAS_GRB
        lp_solver = new OsiGrbSolverInterface;
#else
        missing_symbol = "COIN_HAS_GRB";
#endif
        break;
    default:
        ABORT("Unknown LP solver type.");
    }
    if (lp_solver) {
        lp_solver->messageHandler()->setLogLevel(0);
        return unique_ptr<OsiSolverInterface>(lp_solver);
    } else {
        cerr << "You must build the planner with the " << missing_symbol << " symbol defined" << endl;
        exit_with(EXIT_CRITICAL_ERROR);
    }
}

__attribute__((noreturn))
void handle_coin_error(const CoinError &error) {
    cerr << "Coin threw exception: " << error.message() << endl
         << " from method " << error.methodName() << endl
         << " from class " << error.className() << endl;
    exit_with(EXIT_CRITICAL_ERROR);
}

#endif
