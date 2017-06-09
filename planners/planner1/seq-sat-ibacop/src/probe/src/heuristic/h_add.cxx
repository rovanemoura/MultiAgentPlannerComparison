/*
  Miguel Ramirez, Nir Lipovetzky, Hector Geffner
  C^3: A planner for the sequential, satisficing track of the IPC-6
  Copyright (C) 2008  

  Modified By: Nir Lipovetzky
  PROBE: A planner for the sequential, satisficing track of the IPC-7
  Copyright (C) 2011  

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
#include "heuristic/h_add.hxx"

namespace NFF
{

        Additive_Heuristic::Additive_Heuristic()
                : m_task( PDDL::Task::instance() ),
                  m_Nf( m_task.fluents().size() ), 
                  m_goal_op(m_task.end())
        {
                m_values.resize( m_Nf );
                m_h1_precs.resize( m_task.useful_ops().size() );
                m_best_supports.resize( m_Nf );
                m_metric_values.resize( m_Nf );
                m_metric_h1_precs.resize( m_task.useful_ops().size() );
                m_metric_best_supports.resize( m_Nf );

        }

        Additive_Heuristic::~Additive_Heuristic()
        {
        }

        void Additive_Heuristic::initialize_values( State* s )
        {
                m_h1_precs[0] = 0;
                m_metric_h1_precs[0] = 0.0f;
                for ( unsigned i = 1; i < m_h1_precs.size(); i++ )
                {
                        m_h1_precs[i] = std::numeric_limits<unsigned>::max();
                        m_metric_h1_precs[i] = std::numeric_limits<float>::max();
                }

                if ( s == NULL )
                {
                        PDDL::Operator* start_op = m_task.useful_ops()[m_task.start()];
                        for ( unsigned i = 1; i < m_values.size(); i++ )
                        {
                                m_best_supports[i] = m_metric_best_supports[i] = 0;
                                /**
                                 * check if an instance for the disjuntive facts is in s
                                 */
                                if(m_task.is_star_fluent(i))
                                {
                                        Atom_Vec* common_facts = m_task.get_common_predicate_facts(i);
                                        bool atom_in_s=false;
                                        for(Atom_Vec::iterator it_p = common_facts->begin(); it_p != common_facts->end(); it_p++)
                                        {
                                                if(start_op->adds().isset(*it_p))
                                                {
                                                        atom_in_s = true;
                                                        break;
                                                }
                                        }
                                        m_values[i] = ( atom_in_s ? 0 : std::numeric_limits<unsigned>::max() ); 	
                                        m_metric_values[i] = ( atom_in_s ? 0.0f : std::numeric_limits<float>::max() ); 	
		
                                }
                                else
                                {
                                        m_values[i] = ( start_op->adds().isset(i) ? 0 : std::numeric_limits<unsigned>::max() );
                                        m_metric_values[i] = ( start_op->adds().isset(i) ? 0.0f : std::numeric_limits<float>::max() );
                                }
                        }
		
                }
                else
                {
                        for ( unsigned i = 1; i < m_values.size(); i++ )
                        {

                                m_best_supports[i] = m_metric_best_supports[i] = 0;
                                
                                /**
                                 * check if an instance for the disjuntive facts is in s
                                 */
                                if(m_task.is_star_fluent(i))
                                {
                                        Atom_Vec* common_facts = m_task.get_common_predicate_facts(i);
                                        bool atom_in_s=false;
                                        for(Atom_Vec::iterator it_p = common_facts->begin(); it_p != common_facts->end(); it_p++)
                                        {
                                                if(s->atom_set().isset(*it_p))
                                                {
                                                        atom_in_s = true;
                                                        break;
                                                }
                                        }
                                        m_values[i] = ( atom_in_s ? 0 : std::numeric_limits<unsigned>::max() ); 	
                                        m_metric_values[i] = ( atom_in_s ? 0.0f : std::numeric_limits<float>::max() ); 	
		
                                }
                                else
                                {		
                                        m_values[i] = ( s->atom_set().isset(i) ? 0 : std::numeric_limits<unsigned>::max() ); 	
                                        m_metric_values[i] = ( s->atom_set().isset(i) ? 0.0f : std::numeric_limits<float>::max() ); 	
                                }
                        }
                }
        }

        void Additive_Heuristic::compute( State* s, bool metric )
        {
                initialize_values(s);
	
                bool fixed_point;
                if ( !metric )
                {
                        do
                        {
                                fixed_point = true;
                                for ( unsigned o_idx = 1; o_idx < m_task.useful_ops().size(); o_idx++ )
                                {
                                        PDDL::Operator* op = m_task.useful_ops()[o_idx];
                                        value_op(o_idx) = eval( op->prec_vec() );
                                        if ( value_op(o_idx) == std::numeric_limits<unsigned>::max() ) continue;
                                        for ( unsigned i = 0; i < op->add_vec().size(); i++ )
                                        {
                                                unsigned p = op->add_vec()[i];
                                                if ( value(p) == 0) continue;
                                                unsigned v = std::add(1u, value_op(o_idx));
                                                if ( v <= value(p) )
                                                {
                                                        if(v == value(p))
                                                        {
                                                                if( support(p) == o_idx) continue;
                                                                Atom_Vec& prev_del = m_task.useful_ops()[support(p)]->del_vec();
					      
                                                                unsigned nprev_del = 0;
					      
                                                                for(unsigned j=0; j<prev_del.size();j++)
                                                                {
                                                                        if( s->atom_set().isset(prev_del[j]) )
                                                                                nprev_del++;
                                                                }
					      
                                                                Atom_Vec& del = op->del_vec();
					      
                                                                unsigned ndel = 0;
					      
                                                                for(unsigned j=0; j<del.size();j++)
                                                                {
                                                                        if( s->atom_set().isset(del[j]) )
                                                                                ndel++;
                                                                }
					      
                                                                if(nprev_del <= ndel)
                                                                        continue;
					      
                                                        }
                                                        value(p) = v;
                                                        support(p) = o_idx;
                                                        fixed_point = false;
                                                }
                                        }
                                }
	
                        } while (!fixed_point);
                }
                else	
                {
                        do
                        {
                                fixed_point = true;
                                for ( unsigned o_idx = 1; o_idx < m_task.useful_ops().size(); o_idx++ )
                                {
                                        PDDL::Operator* op = m_task.useful_ops()[o_idx];
                                        metric_value_op(o_idx) = metric_eval( op->prec_vec() );
                                        if ( metric_value_op(o_idx) == std::numeric_limits<float>::max() ) continue;
                                        for ( unsigned i = 0; i < op->add_vec().size(); i++ )
                                        {
                                                unsigned p = op->add_vec()[i];
                                                if ( metric_value(p) <= 1e-7 ) continue;
                                                float v = std::add( m_task.op_cost(o_idx), metric_value_op(o_idx));
                                                const float diff = v - metric_value(p);
                                                if ( diff < 1e-7 )
                                                {
                                                        if(fabs(diff) < 1e-7 )
                                                        {
                                                                if( metric_support(p) == o_idx) continue;
                                                                Atom_Vec& prev_del = m_task.useful_ops()[metric_support(p)]->del_vec();
				      
                                                                unsigned nprev_del = 0;
				      
                                                                for(unsigned j=0; j<prev_del.size();j++)
                                                                {
                                                                        if( s->atom_set().isset(prev_del[j]) )
                                                                                nprev_del++;
                                                                }
				      
                                                                Atom_Vec& del = op->del_vec();
				      
                                                                unsigned ndel = 0;
				      
                                                                for(unsigned j=0; j<del.size();j++)
                                                                {
                                                                        if( s->atom_set().isset(del[j]) )
                                                                                ndel++;
                                                                }
				      
                                                                if(nprev_del <= ndel)
                                                                        continue;
				      
                                                        }
                                                        metric_value(p) = v;
                                                        metric_support(p) = o_idx;
                                                        fixed_point = false;
                                                }
                                        }
                                }
                        } while (!fixed_point);
                }
        }

        void Additive_Heuristic::compute_with_persist( State* s, Atom_Vec* pw, Bool_Vec* useable, Operator_Vec* forbidden, bool metric)
        {
                initialize_values(s);

                bool del_useable = false;
      
                if(useable==NULL)	  
                {
                        useable = new Bool_Vec(m_task.useful_ops().size(), true);
                        del_useable = true;
                }

                for ( unsigned o_idx = 1; o_idx < m_task.useful_ops().size(); o_idx++ )
                {
                        if( m_task.reachable(o_idx) && (*useable)[o_idx] )
                        {
                                PDDL::Operator* op_ptr = m_task.useful_ops()[o_idx];
                                for(unsigned p_idx = 0; p_idx < pw->size(); p_idx++)
                                {
                                        if( op_ptr->dels().isset(pw->at(p_idx)) || m_task.fast_op_edeletes(o_idx).isset( pw->at(p_idx) ) )
                                        {
                                                (*useable)[o_idx]=false;
                                                break;
                                        }
                                }
                        }
                }
                if(forbidden != NULL)
                        for(unsigned i=0; i < forbidden->size(); i++)
                                (*useable)[ (*forbidden)[i] ] = false;

                bool fixed_point;
                if ( !metric )
                {
                        do
                        {
                                fixed_point = true;
                                for ( unsigned o_idx = 1; o_idx < m_task.useful_ops().size(); o_idx++ )
                                {

                                        PDDL::Operator* op = m_task.useful_ops()[o_idx];
                                        value_op(o_idx) = eval( op->prec_vec() );

                                        if(!(*useable)[o_idx])
                                                continue;


                                        if ( value_op(o_idx) == std::numeric_limits<unsigned>::max() ) continue;
                                        for ( unsigned i = 0; i < op->add_vec().size(); i++ )
                                        {
                                                unsigned p = op->add_vec()[i];
                                                if ( value(p) == 0) continue;
                                                unsigned v = std::add(1u, value_op(o_idx));
                                                if ( v <= value(p) )
                                                {
                                                        if(v == value(p))
                                                        {
                                                                if( support(p) == o_idx) continue;
                                                                Atom_Vec& prev_del = m_task.useful_ops()[support(p)]->del_vec();
			    
                                                                unsigned nprev_del = 0;
					      
                                                                for(unsigned j=0; j<prev_del.size();j++)
                                                                {
                                                                        if( s->atom_set().isset(prev_del[j]) )
                                                                                nprev_del++;
                                                                }
					      
                                                                Atom_Vec& del = op->del_vec();
					      
                                                                unsigned ndel = 0;
					      
                                                                for(unsigned j=0; j<del.size();j++)
                                                                {
                                                                        if( s->atom_set().isset(del[j]) )
                                                                                ndel++;
                                                                }                                

                                                                if(nprev_del <= ndel)
                                                                        continue;
					      
                                                        }

                                                        value(p) = v;
                                                        support(p) = o_idx;
                                                        fixed_point = false;
                                                }
                                        }
                                }
	
                        } while (!fixed_point);
                }
                else
                {
                        do
                        {
                                fixed_point = true;
                                for ( unsigned o_idx = 1; o_idx < m_task.useful_ops().size(); o_idx++ )
                                {

                                        PDDL::Operator* op = m_task.useful_ops()[o_idx];
                                        metric_value_op(o_idx) = metric_eval( op->prec_vec() );

                                        if(!(*useable)[o_idx])
                                                continue;

                                        if ( metric_value_op(o_idx) == std::numeric_limits<float>::max() ) continue;
                                        for ( unsigned i = 0; i < op->add_vec().size(); i++ )
                                        {
                                                unsigned p = op->add_vec()[i];
                                                if ( metric_value(p) <= 1e-7 ) continue;
                                                float v = std::add( m_task.op_cost(o_idx), metric_value_op(o_idx));
                                                const float diff = v - metric_value(p);
                                                if ( diff < 1e-7 )
                                                {
                                                        if(fabs(diff) < 1e-7 )
                                                        {
                                                                if( metric_support(p) == o_idx) continue;
                                                                Atom_Vec& prev_del = m_task.useful_ops()[metric_support(p)]->del_vec();
				      
                                                                unsigned nprev_del = 0;
				      
                                                                for(unsigned j=0; j<prev_del.size();j++)
                                                                {
                                                                        if( s->atom_set().isset(prev_del[j]) )
                                                                                nprev_del++;
                                                                }
				      
                                                                Atom_Vec& del = op->del_vec();
				      
                                                                unsigned ndel = 0;
				      
                                                                for(unsigned j=0; j<del.size();j++)
                                                                {
                                                                        if( s->atom_set().isset(del[j]) )
                                                                                ndel++;
                                                                }

                                                                if(nprev_del <= ndel)
                                                                        continue;
				      
                                                        }
                                                        metric_value(p) = v;
                                                        metric_support(p) = o_idx;
                                                        fixed_point = false;
                                                }
                                        }
                                }
                        } while (!fixed_point);

                }

                if( del_useable )
                        delete useable;


        }


        void Additive_Heuristic::print_supporters()
        {

                for(unsigned i = 1; i < m_task.fluents().size(); i++)
                {
                        std::cout << std::endl;
                        m_task.print_operator( m_best_supports[i], std::cout );
                        std::cout << "Best sup of ";
                        m_task.print_fluent( m_task.fluents()[ i ], std::cout );	       
                        std::cout << std::endl;
                }

        }


        void Additive_Heuristic::print_metric_supporters()
        {

                for(unsigned i = 1; i < m_task.fluents().size(); i++)
                {
                        std::cout << std::endl;
                        m_task.print_operator( m_metric_best_supports[i], std::cout );
                        std::cout << "Best sup of ";
                        m_task.print_fluent( m_task.fluents()[ i ], std::cout );	       
                        std::cout << std::endl;
                }

        }


}
