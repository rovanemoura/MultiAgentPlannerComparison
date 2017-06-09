/*********************************************************************
 * Author: Silvia Richter (silvia.richter@nicta.coma.au)
 * (C) Copyright 2008 NICTA
 *
 * This file is part of LAMA.
 *
 * LAMA is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 3
 * of the license, or (at your option) any later version.
 *
 * LAMA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 *********************************************************************/

#ifndef WA_STAR_SEARCH_H
#define WA_STAR_SEARCH_H

#include <vector>
#include <set>
#include "best_first_search.h"

class WAStarSearchEngine : public BestFirstSearchEngine {
private:
    int weight;
    
    void debug_print_partial_plan(const State& state);
protected:
    virtual void initialize();
    virtual void generate_successors(const State *parent_ptr);
public:
    WAStarSearchEngine(int s_num, MTRand_int32 *rg, int w=1, bool r=false);
    
    // should expand this node, depends on the algorithm condition
    virtual bool expand_closed_node(const State *parent_ptr);
};
#endif
