/***************************
*           check.c        *
****************************/




void
check_level (int max_time)
{
  int level;
  for (level = 0; level <= max_time; level++)
    if (vectlevel[level]->level != level)

      {
	printf ("\n ERROR level %d", level);
	exit (0);
      }
}



// Print info about all levels
int
check_plan_for_boolean_action (int max_time)
{
  int j, level, pos;
  unsigned int uid_block, uid_mask;
  inform_list fact, act;
  FtConn *vertex_noop;
  EfConn *vertex_act;
  int true, error, mutex, temp;
  int cel;
  int el;
  if (max_time != GpG.curr_plan_length)
    printf ("\nCheck Plan max_time error... %d %d", max_time,
	    GpG.curr_plan_length);
  max_time = GpG.curr_plan_length;

  //  return ; //// ATTENZIONE
  printf ("\nCheck Plan... %s:%d, max %d", __FILE__, __LINE__,
	  GpG.curr_plan_length);
  for (level = 0; level <= max_time && level < GpG.max_plan_length; level++)

    {
      if (vectlevel[level]->modified || vectlevel[level]->level != level)
	printf ("\n MODIFIED %d, vectlevel %d, level %d",
		vectlevel[level]->modified, vectlevel[level]->level, level);
      if (vectlevel[level]->level != level)

	{
	  printf ("\n ERROR level %d", level);
	  exit (0);
	}
      for (pos = 0; pos < GpG.max_num_facts; pos++)
	if (CHECK_FACT_POS (pos, level))

	  {
	    error = 0;
	    fact = &vectlevel[level]->fact[pos];
	    uid_block = GUID_BLOCK (pos);
	    uid_mask = GUID_MASK (pos);
	    if (fact->w_is_true && level > 0)

	      {
		temp = 0;

		// Check if it is supported
		for (j = 0; j < gft_conn[pos].num_A; j++)
		  if (CHECK_ACTION_POS
		      ((cel = gft_conn[pos].A[j]), level - 1)
		      && CONVERT_ACTION_TO_INFORM (cel, level - 1)
		      && CONVERT_ACTION_TO_INFORM (cel, level - 1)->w_is_used)
		    temp++;
		if (CHECK_NOOP_POS (pos, level - 1)
		    && CONVERT_NOOP_TO_INFORM (pos, level - 1)
		    && CONVERT_NOOP_TO_INFORM (pos, level - 1)->w_is_used)
		  temp++;
		if (temp != fact->w_is_true)
		  printf ("\nFact error %s w_is_true %d count %d level %d ",
			  print_ft_name_string (pos, temp_name),
			  fact->w_is_true, temp, level);
	      }
	    if (fact->w_is_goal && level < max_time)

	      {
		temp = 0;

		// Check if it is supported
		for (j = 0; j < gft_conn[pos].num_PC; j++)
		  if (CHECK_ACTION_POS ((el = gft_conn[pos].PC[j]), level))
		    if ((CONVERT_ACTION_TO_INFORM (el, level)
			 && CONVERT_ACTION_TO_INFORM (el, level)->w_is_used
			 && CONVERT_ACTION_TO_INFORM (el, level)->w_is_goal))
		      temp++;

		// aggiungere cicli per overall atend axl
		if (CHECK_NOOP_POS (pos, level)
		    && CONVERT_NOOP_TO_INFORM (pos, level)->w_is_goal)
		  temp++;
		if (level >= GpG.curr_plan_length)

		  {
		    if (fact->w_is_goal > 1)
		      printf
			("\nFact error %s Goal  w_is_goal %d count %d level %d ",
			 print_ft_name_string (pos, temp_name),
			 fact->w_is_goal, temp, level);
		  }

		else if (temp != fact->w_is_goal)

		  {
		    printf ("\nFact error %s w_is_goal %d count %d level %d ",
			    print_ft_name_string (pos, temp_name),
			    fact->w_is_goal, temp, level);
		    for (j = 0; j < gft_conn[pos].num_PC; j++)
		      if (CHECK_ACTION_POS
			  ((el = gft_conn[pos].PC[j]), level)
			  && CHECK_ACTION_POSTION_OF_LEVEL (el, level))
			if ((CONVERT_ACTION_TO_INFORM (el, level)->w_is_used
			     && CONVERT_ACTION_TO_INFORM (el,
							  level)->w_is_goal))
			  printf ("   name %s %d %d",
				  print_op_name_string (el, temp_name),
				  CONVERT_ACTION_TO_INFORM (el,
							    level)->
				  w_is_goal, CONVERT_ACTION_TO_INFORM (el,
								       level)->
				  position);
		    if (CONVERT_NOOP_TO_INFORM (pos, level)->w_is_goal)
		      printf ("   name %s %d %d",
			      print_noop_name_string (pos, temp_name),
			      CONVERT_NOOP_TO_INFORM (pos, level)->w_is_goal,
			      CONVERT_NOOP_TO_INFORM (pos, level)->position);
		  }
	      }
	    true = (vectlevel[level]->fact_vect[uid_block] & uid_mask);
	    if ((fact->w_is_true != 0) != (true != 0))
	      printf ("\nFact error %s w_is_true %d bit %d  level %d position %d",
		      print_ft_name_string (pos, temp_name), fact->w_is_true,
		      true, level, pos);
	    true = vectlevel[level]->prec_vect[uid_block] & uid_mask;
	    if ((fact->w_is_goal > 0) != (true != 0))
	      printf ("\nFact error %s w_is_goal %d bit %d level %d ",
		      print_ft_name_string (pos, temp_name), fact->w_is_goal,
		      true, level);
	    true = vectlevel[level]->false_crit_vect[uid_block] & uid_mask;
	    if ((fact->w_is_goal && fact->w_is_true == 0) != (true != 0))
	      printf ("\nFact error %s w_is_goal %d w_is_true %d bit %d  level %d",
		      print_ft_name_string (pos, temp_name), fact->w_is_goal,
		      fact->w_is_true, true, level);
	    true = vectlevel[level]->true_crit_vect[uid_block] & uid_mask;
	    if ((fact->w_is_goal && fact->w_is_true == 1) != (true != 0))
	      printf ("\nFact error %s w_is_goal %d w_is_true %d bit %d level %d ",
		      print_ft_name_string (pos, temp_name), fact->w_is_goal,
		      fact->w_is_true, true, level);
	  }
      if (level >= max_time)
	break;

      //      for(pos=0; pos<GpG.max_num_actions; pos++)
      pos = vectlevel[level]->action.position;
      if (CHECK_ACTION_POS (pos, level))

	{
	  error = 0;
	  act = &vectlevel[level]->action;
	  vertex_act = &gef_conn[pos];
	  uid_block = GUID_BLOCK (pos);
	  uid_mask = GUID_MASK (pos);

	  //      true = vectlevel[level]->act_vect[uid_block] & uid_mask;
	  //if((act->w_is_used>0) != (true!=0))
	  //  printf("\nAerr %s w_is_used %d bit %d  level %d",print_op_name_string(pos),  act->w_is_used, true, level );
	  mutex = check_mutex (act);
	  if (act->w_is_used && (mutex >= 0))
	    printf ("\nAction error %s w_is_used %d bit %d  level %d",
		    print_op_name_string (pos, temp_name), act->w_is_used,
		    mutex, level);

	  // count_goal_facts(pos, level);
	  //  if( (act->w_is_goal>0)  != (true!=0))
	  // printf("\nAerr goal %s w_is_goal %d bit %d  level %d", print_op_name_string(pos) , act->w_is_goal, true, level );
	}
      for (pos = 0; pos < GpG.max_num_facts; pos++)
	if (CHECK_NOOP_POS (pos, level))

	  {
	    error = 0;
	    act = &vectlevel[level]->noop_act[pos];
	    vertex_noop = &gnoop_conn[pos];
	    uid_block = GUID_BLOCK (pos);
	    uid_mask = GUID_MASK (pos);
	    true = vectlevel[level]->noop_act_vect[uid_block] & uid_mask;
	    if ((act->w_is_used > 0) != (true != 0))
	      printf ("\nNoop error %s w_is_used %d bit %d  level %d",
		      print_op_name_string (pos, temp_name), act->w_is_used,
		      true, level);
	    true = vectlevel[level]->noop_prec_act_vect[uid_block] & uid_mask;

	    /*
	       mutex=check_mutex_noop(pos,level);
	       if(act->w_is_used && (mutex>=0))
	       printf("\nAerr NOOP_ERR %s w_is_used %d bit %d  level %d",print_noop_name_string(pos), act->w_is_used, mutex, level );
	     */
	    if ((act->w_is_goal > 0) != (true != 0))
	      printf ("\nAction error goal %s w_is_goal %d bit %d  level %d",
		      print_noop_name_string (pos, temp_name),
		      act->w_is_goal, true, level);
	    if ((true != 0)
		&& !(CONVERT_FACT_TO_INFORM (pos, level)->w_is_goal
		     && CONVERT_FACT_TO_INFORM (pos, level + 1)->w_is_goal))
	      printf
		("\nAction error prec %s w_is_goal %d bit %d  f1 %s %d f2 %s %d level %d",
		 print_noop_name_string (pos, temp_name), act->w_is_goal,
		 true, print_ft_name_string (pos, temp_name),
		 CONVERT_FACT_TO_INFORM (pos, level + 1)->w_is_goal,
		 print_ft_name_string (pos, temp_name),
		 CONVERT_FACT_TO_INFORM (pos, level + 1)->w_is_goal, level);
	  }
    }
  printf ("\n");
  return 0;
}





/******************************
*         dg_heuristics.c     *
*******************************/





void
choose_act_to_remove ()
{
  int level = 0, pos = -1, num_removed = 0;
  int num_act;
  float sum, num;



  printf ("\nDG_Remove act");

  do
    {
      num_act = random () % ((int) GpG.best_cost);

      for (sum = 0.0, num = 0, level = 0;
	   level < GpG.curr_plan_length && sum < num_act; level++)
	if (vectlevel[level]->num_actions)
	  {


	    /* 
	       for (i=0, j=0; i<=GpG.max_num_ft_block && sum<num_act; i++, j+=32) { 

	       temp =vectlevel[level]->act_vect[i] ; 
	       k=32;             
	       while (temp) {  
	       k--; 
	       if ( temp & FIRST_1) 
	       { 

	       if(DEBUG3 && vectlevel[level]->actions[j+k].w_is_used<=0) 
	       printf("\n ERROR: act %s ", print_op_name_string(j+k, temp_name)); 

	       sum+=get_action_cost(j+k); 

	       if( sum >= num_act) 
	       break; 
	       } 

	       temp<<=1; 
	       } 
	     */
	    sum += get_action_cost (vectlevel[level]->action.position);

	    if (sum >= num_act)
	      {

		pos = vectlevel[level]->action.position;

		// Additional check 
		if (vectlevel[level] == NULL || pos == -1
		    || vectlevel[level]->action.w_is_used == 0)
		  break;
		else
		  {

		    if (DEBUG6)
		      {

			printf
			  ("\n__________ RANDOM CHOICE %s level %d pos %d cost %.2f time %.2f",
			   print_op_name_string (pos, temp_name), level, pos,
			   get_action_cost (pos), get_action_time (pos,
								   level));
		      }

		    remove_action_from_vectlevel (vectlevel[level]->action.
						  position, level,
						  GpG.approximation_level);
		    num_removed++;

		  }
	      }

	  }
    }
  while (num_removed < (GpG.num_actions / 10));
  //#ifdef __TEST__ 
  // if(DEBUG2) 
  {
    fprintf (stderr, "/ ");
  }
  //#endif 

  return;

}





/* Check if an action is already inserted into the list for compute the cost */

Bool
ef_already_inserted_in_gft_conn (int fact_pos, float cost, float duration,
				 int best_ef)
{
  dg_inform *idx;
  Bool chk;

  //LAZZA
  // if(fact_pos<0)
  // precondiz numerica
  // else

  idx = Hvar.dg_facts_array[fact_pos];
  chk = FALSE;
  while (idx)
    {
      if (idx->best_act == best_ef)
	{
	  chk = TRUE;
	  break;
	}
      idx = idx->next;
    }
  return (chk);
}








// SISTEMARE


void
compute_dg_prec_fact (inform_list Fact, node_cost_list n_cost)
{




}





void
reset_facts_mutex ()
{
  Hvar.num_facts_mutex = 0;
}

void
insert_facts_mutex (inform_list fact)
{

  if (Hvar.num_facts_mutex >= MAX_LENGTH_H)
    return;

  if (Hvar.num_facts_mutex >= MAX_LENGTH_H)
    {
      printf("\nToo many facts in queue, increase MAX_LENGTH_H: now %d\n\n",MAX_LENGTH_H);
#ifdef __MY_OUTPUT__
      printf ("\n ERROR vector size in file %s:%d\n\n", __FILE__, __LINE__);
#endif 
      exit (1);
    }


  Hvar.facts_mutex[Hvar.num_facts_mutex++] = fact;


}





Bool
is_ef_inlistef (int action)
{

  Bool found = FALSE;
  if (Hvar.num_actions_define_cost == 0)
    {
      return (found);
    }

  if (GET_BIT (Hvar.bit_vect_actions, action))
    found = TRUE;



  return (found);

}

Bool
is_ft_inlistft (int fact_pos)
{

  Bool found = FALSE;
  if (Hvar.num_facts_define_cost == 0)
    {
      return (found);
    }

  if (GET_BIT (Hvar.bit_vect_facts, fact_pos))
    {
      found = TRUE;
    }

  return (found);
}




static int
compute_relative_weight (node_cost_list n_cost1, node_cost_list n_cost2)
{
  float max_w, max_c, max_t, c1, c2;
  int result;



  max_w = MAX (n_cost1->weight, n_cost2->weight);

  max_c = max_w / MAX (n_cost1->act_cost, n_cost2->act_cost);
  max_t = max_w / MAX (n_cost1->act_time, n_cost2->act_time);

  c1 =
    (n_cost1->weight + n_cost1->act_cost * GpG.weight_cost * max_c +
     n_cost1->act_time * GpG.weight_time * max_t);
  c2 =
    (n_cost2->weight + n_cost2->act_cost * GpG.weight_cost * max_c +
     n_cost2->act_time * GpG.weight_time * max_t);


  if (c1 > c2)
    result = 1;
  else if (c1 < c2)
    result = -1;
  else
    result = 0;

#ifdef __TEST__
  printf ("\n Result relative weight %d  c1 %f  c2 %f ", result, c1, c2);
#endif

  return result;

}










/*******************************
*           inst_final.c       *
********************************/




void
empty_neighborhood (void)
{
}



//L.A.Z.Z.A. FOR LORINI 
int
get_inertial_facts (Facts ** inertial_facts)
{
  int i, j;
  int num_inertials = 0;
  Facts *f;
  Facts *tmp;
  *inertial_facts = NULL;
  for (i = 0; i < gnum_full_initial; i++)
    {
      //se un fatto dei full non figura nei ginitial, e' inerziale
      for (f = ginitial; f; f = f->next)
	if (are_facts_same (&gfull_initial[i], f->fact))
	  break;
      if (f == NULL)
	{
	  tmp = new_Facts ();
	  tmp->fact->predicate = gfull_initial[i].predicate;
	  for (j = 0; j < garity[gfull_initial[i].predicate]; j++)
	    {
	      tmp->fact->args[j] = gfull_initial[i].args[j];
	    }
	  tmp->next = *inertial_facts;
	  *inertial_facts = tmp;
	  num_inertials++;
	}
    }
  return num_inertials;
}

Bool
are_facts_same (Fact * f1, Fact * f2)
{
  int i;
  if (f1->predicate != f2->predicate)
    return FALSE;
  for (i = 0; i < garity[f1->predicate]; i++)
    if (f1->args[i] != f2->args[i])
      return FALSE;
  return TRUE;
}

//END L.A.Z.Z.A. FOR LORINI 



void
print_fact_mutex ()
{

  int i, j, total;
  long total_mutex = 0;

#ifdef __TEST_PDDL__		// || TRUE
  printf ("\n-----------------------------------------------------------\n ");
  printf ("ARRAY FT_FT");
  printf ("\n-----------------------------------------------------------\n ");
#endif

  for (i = 0; i < gnum_ft_conn; i++)
    if (gft_conn[i].in_F)
      {
#ifdef __TEST_PDDL__		// || TRUE
	printf ("\n\n ");
	print_ft_name (i);
	printf (" MUTEX:\n ");
#endif
	total = 0;
	for (j = 0; j < gnum_ft_conn; j++)
	  if (GET_BIT (FT_FT_mutex[i], j))
	    {
	      total++;
#ifdef __TEST_PDDL__		// || TRUE
	      print_ft_name (j);
	      printf (" - ");
#endif
	    }
#ifdef __TEST_PDDL__		// || TRUE
	printf ("\n total mutex of fact: %d\n", total);
#endif
	total_mutex += total;
      }
#ifdef __TEST_PDDL__		// || TRUE
  printf ("\n\n Total mutexes %ld  (mutex pairs %ld)", total_mutex,
	  total_mutex / 2);
#endif
  total_ft_ft_mutex = total_mutex;
  //    printf("\n\n Total mutex pairs between facts: %ld",total_mutex/2);    
}
















/*******************************
*           levels.c       *
********************************/



void
insert_second_action (inform_list inform_act)  
{
  int i,  j1, j,  k,  temp,  level,  num,  num_min = 0,  choice;
   FtConn * tofix;
   register int cel;
   EfConn * act;
       num = 0;
   act = CONVERT_ACTION_TO_VERTEX (inform_act->position);
   level = *inform_act->level;
   for (i = 0, temp = 0; i < act->num_add_effect && temp == 0; i++)
    {
       k = 32;
       j1 = act->bit_add_effect[i].uid_block * 32;
       if ((temp =
	    act->bit_add_effect[i].uid_mask & *(vectlevel[level]->
						true_crit_vect +
						act->bit_add_effect[i].
						uid_block)))
	 while (temp)
	  {			/* counts the no. of  eliminated fact's precondition actions */
	     k--;
	     if (temp & FIRST_1)
	      break;
	     temp <<= 1;
	   }
     }
    tofix = &gft_conn[j1 + k];
      for (j = 0; j < gef_conn[inform_act->position].num_A; j++)
    {
       if (CHECK_FACT_POS
	   ((cel = gef_conn[inform_act->position].A[j]), level - 1)
	   &&  !CONVERT_ACTION_TO_INFORM (cel, level - 1)->w_is_used)
	 exit (0);		//GPG2 rifare 
      //     temp_vect[num++]=CONVERT_ACTION_TO_INFORM(cel,level-1 ); 
       if (num > GpG.num_act_cons)
	 break;
     }
    if (num == 0)
     return;
    if (num == 1)
     choice = 0;
   
  else
   if ((GpG.rnd % GpG.denominator) < GpG.numerator)
     choice = random () % num;

  else if (num_min == 1)
    choice = pos_temp_vect[0];
  else
    exit (0);			//GPG2 rifare 
  // choice=pos_temp_vect[(random()%num_min)]; 
    //   inform_act=temp_vect[choice]; 
    // GPG2 check    insert_remove_action( inform_act, 1 ); 
}







void
free_stored_act_vect (action * act_vect[], int level)  
{
   action * temp_ac,  *temp;
    for (temp_ac = act_vect[level]; temp_ac;)
    {
       temp = temp_ac;
       temp_ac = temp_ac->next;
       free (temp->name);
       free (temp);
     }
   act_vect[level] = NULL;
 }





void
insert_ac_vect (action * act_vect[], EfConn * op, int level, int position)  
{
   action * temp_ac;
    temp_ac = (action *) calloc (1, sizeof (action));
    
#ifdef __TEST__ 
    temp_ac->name =
    (char *) new_token (strlen (gef_conn[op->position].name) + 1);
   
#else /*   */
    temp_ac->name = new_token (256);
   
#endif /*   */
     temp_ac->position = op->position;
    temp_ac->next = act_vect[level];
   act_vect[level] = temp_ac;
 }


  






// Check if it is possible to move down the actions of the next level 
// return 0 if this is possible 1 otherwise 
int
check_max_action_position (int level)
{
  int l1;


  if (level > GpG.fixpoint_plan_length && level < GpG.curr_plan_length)
    return 0;
  else
    return 1;

  for (l1 = level + 1; l1 <= GpG.fixpoint_plan_length; l1++)
    if (CHECK_ACTION_POS (GET_ACTION_POSITION_OF_LEVEL (l1), l1) == FALSE)
      return 1;

  return (0);

}



int
get_cost (char *action)
{
  cost_list *current;
  float distance, cost;


  current = first_cost->next;
  if (strstr (action, "NOOP") != 0)
    return (0);			//noop costo 0 

  while (current != NULL)
    {

      if (strstr (action, current->action) != 0)
	{
	  cost = current->cost;

	  if (current->sign == 1)
	    {
	      distance = getdistance (action);
	      cost = cost * distance;
	      if (cost == 0.0)
		cost = 1.0;
	    }
	  return ((int) cost);
	}
      current = current->next;
    }
  return (1);			//se non trovo l'azione assegno costo 1 per default 
}



int
get_time (char *action)
{
  cost_list *current;
  float distance, cost;


  current = first_time->next;
  if (strstr (action, "NOOP") != 0)
    return (0);			//noop costo 0 

  while (current != NULL)
    {

      if (strstr (action, current->action) != 0)
	{
	  cost = current->cost;

	  if (current->sign == 1)
	    {
	      distance = getdistance (action);
	      cost = cost * distance;
	    }
	  return ((int) cost);
	}
      current = current->next;
    }
  return (1);			//se non trovo l'azione assegno tempo di esecuzione 1 per default 
}




// IVAN 


// Insert an action, if not present 

void
remove_act_vect (action * act, int level)
{
  action *temp_act, *loc_act;

  loc_act = NULL;
  for (temp_act = act_vect[level]; temp_act;
       loc_act = temp_act, temp_act = temp_act->next)
    {
      if (strcmp (temp_act->name, act->name) == SAME)
	{
	  if (loc_act == NULL)
	    act_vect[level] = temp_act->next;
	  else
	    loc_act->next = temp_act->next;
	  temp_act->next = NULL;
	  free (temp_act->name);
	  temp_act->name = NULL;
	  free (temp_act);
	  return;
	}

    }

#ifdef __TEST__
  if (DEBUG4)
    printf ("\nRemove %s from act_vect %d impossible!",
	    print_op_name_string (act->position, temp_name), level);
#endif
}








/* Per impostare il costo di ciascuna azione */

static float
set_action_cost (int pos, int level)
{
  float cost = 1.0, time = 1.0;


  if (pos >= MAX_MAX_NODES)
    {
      printf("\nToo many actions in queue, increase MAX_MAX_NODES: now %d\n\n",MAX_MAX_NODES);
#ifdef __MY_OUTPUT__
      printf ("\n ERROR vector size in file %s:%d\n\n", __FILE__, __LINE__);
#endif    
      exit (1);
    }


  action_times[pos] = time;
  action_costs[pos] = cost;

  if (gef_conn[pos].cost > action_costs[pos])
    action_costs[pos] = gef_conn[pos].cost;
  if (gef_conn[pos].duration > action_times[pos])
    action_times[pos] = gef_conn[pos].duration;

  //  action_costs[pos]=gef_conn[pos].cost; 
  //   action_times[pos]=gef_conn[pos].duration; 
  return 0;

}




static void  restore_orig_vectors() 
{ 
  register int level; 
 

  for(level=0; level<= GpG.curr_plan_length; level++) 
    
    if( vectlevel[level]->modified > 0) { 
      
#ifdef __TEST__ 
      printf("\n REStore level %d",level); 
#endif   
      
      memcpy(vectlevel[level]->~prec_vect,  vectlevel[level]->orig_prec_vect, GpG.max_num_ft_block * 4); 
      memcpy(vectlevel[level]->facT_vect,  vectlevel[level]->orig_fact_vect, GpG.max_num_ft_block * 4); 
      memcpy(vectlevel[level]->true_crit_vect,  vectlevel[level]->orig_true_crit_vect, GpG.max_num_ft_block * 4); 
      memcpy(vectlevel[level]->false_crit_vect,  vectlevel[level]->orig_false_crit_vect, GpG.max_num_ft_block * 4); 
      memcpy(vectlevel[level]->noop_act_vect,  vectlevel[level]->orig_noop_act_vect, GpG.max_num_ft_block * 4); 
      memcpy(vectlevel[level]->noop_prec_act_vect,  vectlevel[level]->orig_noop_prec_act_vect, GpG.max_num_ft_block * 4); 
      vectlevel[level]->modified=0;  
    } 
  //      else  vectlevel[level]->modified=0; 
  
 
#ifdef TEST_GR 
  check_plan(GpG.curr_plan_length); 
#endif 
 
} 
 






//Update temporaney the bitvectors for  the action infAction  
void
insert_temp_action (register inform_list infAction)
{
  int level, block;
  register int temp, i, y;
  EfConn *act;
  min_array_list ma_ptr;


  level = *infAction->level;
  act = &gef_conn[infAction->position];

#ifdef __TEST__
  if (infAction->action_fact == IS_FACT)
    {
      MSG_ERROR ("");
      exit (1);
    }
#endif

  // If the action is in the subgraph, we want to remove it, 
  // else if the action is not in the subgraph, we want to insert it. 

  store_bit_vect (level);
  store_bit_vect (level + 1);
  // store_bit_vect(level+2); 

#ifdef __TEST__
  printf ("\n\n   TEMP IA %s is_used %d time %d pos %d", act->name,
	  infAction->w_is_used, level, infAction->position);
#endif

  // Set to 1 the bit in the act_vect mask, relative to the inserted action 
  //LPGI     if ( !act->is_noop ) 
  //       vectlevel[level]->act_vect[GUID_BLOCK(act->position)] |= GUID_MASK(act->position); 
  //     else 
  //      { 
  // vectlevel[level]->noop_act_vect[act->uid_block] |= act->uid_mask; 
  //     vectlevel[level]->noop_prec_act_vect[act->uid_block] |= act->uid_mask; 
  //       } 

  // Setto le precondizioni dell'azione 
  for (ma_ptr = act->bit_precond, i = 0; i < act->num_precond; i++)
    {
      vectlevel[level]->prec_vect[(ma_ptr + i)->uid_block] |=
	(ma_ptr + i)->uid_mask;
      vectlevel[level]->true_crit_vect[(ma_ptr + i)->uid_block] |= ((ma_ptr + i)->uid_mask & vectlevel[level]->facT_vect[(ma_ptr + i)->uid_block]);	// potrebbe non essere un tru  critic vector, ma non avendo informazione aggiuntive lo forzo cosi. PS: fact_vect e' un bitarray negato 
      vectlevel[level]->false_crit_vect[(ma_ptr + i)->uid_block] |=
	((ma_ptr + i)->uid_mask & ~vectlevel[level]->facT_vect[(ma_ptr +i)->uid_block]);


    }

  // Setto gli effetti additivi dell'azione 
  level++;
  //  if(level>=GpG.curr_plan_length) 
  //   return; 

  for (ma_ptr = act->bit_add_effect, i = 0; i < act->num_add_effect; i++)
    {
      //per prima cosa propago in avanti con temporaney_noop gli effetti additivi dell'azione finche' vectlevel[level]->modified>0 

      if (level <= GpG.max_temp_level && level < GpG.curr_plan_length)
	{
	  temp = ~vectlevel[level]->facT_vect[(ma_ptr + i)->uid_block] & (ma_ptr + i)->uid_mask;	// FATTI CHE VENGONO RESI VERI DA infAction 
	  y = 32;
	  block = ((ma_ptr + i)->uid_block) * 32;
	  /* scan if the actions ME and used are noop */
	  while (temp)
	    {
	      y--;

	      if ((temp & FIRST_1) && CHECK_NOOP_POS (block + y, level))
		{
		  if (check_mutex_noop (block + y, level) < 0)
		    insert_temp_action (&vectlevel[level]->noop_act[block + y]);	//LPGI noop! 
		}
	      temp <<= 1;
	    }
	}

      vectlevel[level]->fact_vect[(ma_ptr + i)->uid_block] |= ((ma_ptr + i)->uid_mask);
      vectlevel[level]->true_crit_vect[(ma_ptr + i)->uid_block] |= ((ma_ptr + i)->uid_mask & vectlevel[level]->false_crit_vect[(ma_ptr + i)->uid_block]);	// se prima era false_critic_vect ora sara' true_critc_vect 
      vectlevel[level]->false_crit_vect[(ma_ptr + i)->uid_block] &=
	~((ma_ptr +
	   i)->uid_mask & vectlevel[level]->false_crit_vect[(ma_ptr +
							     i)->uid_block]);
      // se era un false_critic_vect ora non lo e' piu' 

    }

}




#ifdef RIFARE

// Se individuo l'azione inf_ptr  puo' essere posizionata in avanti senza problemi restituisco TRUE 
int can_move_up_action(inform_list inf_ptr) 
{ 
  int l1, lev_noop, end_lev, level, temp, noop_prec_actions[NUMINTS]; 
  unsigned int 	v_uid_block; 
  EfConn 	*vert_ptr; 
  register int 	i; 

 
 
  end_lev=GpG.curr_plan_length-1; 
  level=*inf_ptr->level; 
  // creo un vectlevel contenente tutte le noop precondizioni di almeno una azione del successivo livello e supportate nel corrispondente  livello 
 
  reset_bitarray(noop_prec_actions,  gnum_ft_block); 
  if(level==GpG.max_plan_length) 
    lev_noop=level; 
  else 
    lev_noop=level+1; 
  l1=level+1; 
  
  for(i=0; i< gnum_ft_block  ; i++) 
    noop_prec_actions[i]= (~vectlevel[l1]->facT_vect[i]) & vectlevel[l1]->prec_vect[i]; 
  
  vert_ptr=&gef_conn[inf_ptr->position]; 
  v_uid_block=GUID_BLOCK(inf_ptr->position); 
  
  for(temp = 0,i=0; i< gnum_ft_block && temp==0; i++) {  
    temp += count_bit1( gef_conn[inf_ptr->position].ft_exclusive_vect[i] & noop_prec_actions[i]  ); 
    
#ifdef TEST_GR 
    if(temp) { 
      k=32; 
      temp1= gef_conn[inf_ptr->position].ft_exclusive_vect[i] & noop_prec_actions[i] 
	
	while(temp1) { 
	  k--; 
	  if (temp1 & FIRST_1)  
	  printf("\n MOVE UP FACT MUTEX %s, act %s ",print_ft_name_string(i*32+k, temp_name), print_op_name_string(vert_ptr->position, temp_name)); 
	  temp1<<=1;	  
	} 
    } 
    
#endif 
	 
 
  } // end for sui fatti 
       
  if(temp==0) { 
    if(DEBUG6) 
      printf("\n\n IRA  POSSO SPOSTARE L'AZIONE %s IN AVANTI LEVEL %d",print_op_name_string(vert_ptr->position, temp_name), l1); 
    return(TRUE); 
  } 
  else 
    if(DEBUG6) 
      printf("\n\nNON  POSSO SPOSTARE L'AZIONE %s IN AVANTI",print_op_name_string(vert_ptr->position, temp_name)); 
  
  return (FALSE); 
  
} 
 
 


// Se individuo una azione che puo' essere spostatain avanti senza problemi restituisco il corrispondente indice 
int move_up_action(int level, inform_list *temp_vect, int num_actions) 
{ 
  int 	pos, l1, lev_noop, end_lev, noop_pos; 
  unsigned int 	v_uid_block; 
  inform_list 	inf_ptr=NULL; 
  EfConn  	*vert_ptr; 
  int 		noop_prec_actions[NUMINTS], add_eff_action[NUMINTS]; 
  register int 	temp, i, j; 
  int 	el, cel; 

 
  
  end_lev=GpG.curr_plan_length-1; 
  // creo un vectlevel contenente tutte le noop precondizioni di almeno una azione del successivo livello e supportate nel corrispondente  livello 
  
  reset_bitarray(noop_prec_actions,  gnum_ft_block); 
  if(level==GpG.max_plan_length) 
    lev_noop=level; 
  else 
    lev_noop=level+1; 
  l1=level+1; 
 
  for(i=0; i< gnum_ft_block  ; i++) 
      noop_prec_actions[i]= (~vectlevel[l1]->facT_vect[i]) & vectlevel[l1]->prec_vect[i]; 
     
  for(pos=0; pos < num_actions; pos++) { 
    inf_ptr=temp_vect[pos]; 
    vert_ptr=&gef_conn[inf_ptr->position]; 
    v_uid_block=GUID_BLOCK(vert_ptr->position); 
    // DA MIGLIORARE 
    reset_bitarray(add_eff_action, gnum_ft_block); 
    
    // Rimuovo i fatti che sono precondizione unicamente di vert_ptr 
 
    for(j=0; j< gef_conn[inf_ptr->position].num_A; j++) {
      cel=gef_conn[inf_ptr->position].A[j];
      
      if(cel<0)
	continue; 
      // Rimuovo la corrispondente noop da noop_prec_actions   
      if( CHECK_FACT_POS(cel, l1)  &&  vectlevel[l1]->fact[cel].w_is_true) { 
	noop_pos=cel; 
	add_eff_action[GUID_BLOCK(noop_pos)] |= GUID_MASK(noop_pos); 
	
      } 
    }
    
    for(temp=0,i=0; i<gnum_ft_block  && temp==0; i++) {  
      temp+=count_bit1(gef_conn[inf_ptr->position].ft_exclusive_vect[i] & noop_prec_actions[i]  & ~add_eff_action[i] ); 
      
      
#ifdef TEST_GR 
      if(temp) { 
	k=32; 
	temp1= gef_conn[inf_ptr->position].ft_exclusive_vect[i] & noop_prec_actions[i] ; 
	
	while(temp1) { 
	  k--; 
	  if (temp1 & FIRST_1)  
	    printf("\n MOVE UP FACT MUTEX %s, act %s ",print_noop_name_string(i*32+k, temp_name), print_op_name_string(pos, temp_name)); 
	  temp1<<=1;	  
	} 
      } 
      
#endif 
      
    } 
    
    if(temp) // Non posso spostare l'azione 
      continue; 
    
    for(j=0, temp=0; j< gef_conn[inf_ptr->position].num_PC && !temp; j++) {
      el=gef_conn[inf_ptr->position].PC[j];
      
      if(el<0)
	continue; 
      
      if( CHECK_FACT_POS(el, l1)) { 
	noop_pos=el; 
	// Verifico se la precondizione puo' essere propagata nel livello corente 
	if(check_mutex_noop(noop_pos, level)>=0) 
	  temp++; 
      } 
    }
    
    if(temp==0)	{   
      if(DEBUG6) 
	printf("\n\n POSSO SPOSTARE L'AZIONE %s IN AVANTI LEVEL %d", print_op_name_string(inf_ptr->position, temp_name), l1); 
      return(pos); 
    } 
    
  } 
  
  if(DEBUG6) 
    printf("\n\nNON  POSSO SPOSTARE NESSUNA AZIONE %s IN AVANTI", print_op_name_string(inf_ptr->position, temp_name)); 
  return (-1); 
  
} 
 
 


// Se individuo una azione che puo' essere spostata in "basso" senza problemi restituisco il corrispondente indice 
int move_down_action(int level, inform_list *temp_vect, int num_actions) 
{ 
  int pos, ret_pos, ret_level, l1, l2, noop_pos, v_pos=0, down; 
  unsigned int v_uid_block; 
  inform_list inf_ptr=NULL; 
  EfConn *vert_ptr=NULL; 
  int noop_prec_actions[NUMINTS], prec_eff_action[NUMINTS]; 
  register int temp, i, j; 
  int cel; 
  int el; 

 
 
  reset_bitarray(noop_prec_actions, gnum_ft_block); 
  ret_pos=-1; ret_level=level; // Livello in cui posizionare l'azione ret_pos 
  l1=level+1; 
  for(i=0; i<GpG.max_num_facts; i++) { 
    
    // insert a noop precond
    if(CHECK_FACT_POS(i,level) && vectlevel[level]->fact[i].w_is_goal ) { 
      noop_pos=i; 
      noop_prec_actions[GUID_BLOCK(noop_pos)] |= GUID_MASK(noop_pos); 
    } 
  } 
  
  for(pos=0; pos < num_actions; pos++) { 
    inf_ptr=temp_vect[pos]; 
    vert_ptr=&gef_conn[inf_ptr->position]; 
    v_uid_block=GUID_BLOCK(inf_ptr->position); 
    v_pos=inf_ptr->position; 
    // DA MIGLIORARE 
    reset_bitarray(prec_eff_action,  gnum_ft_block); 
  
    // Rimuovo i fatti che sono precondizione unicamente di vert_ptr 
    for(j=0, temp=0; j< gef_conn[inf_ptr->position].num_PC; j++) {
      el=gef_conn[inf_ptr->position].PC[j];
      
      if(el<0)
	continue; 
      // Rimuovo la corrispondente noop da noop_prec_actions  
      if( CHECK_FACT_POS(el, level)  && vectlevel[level]->fact[el].w_is_goal==1) { 
	noop_pos=el; 
	prec_eff_action[GUID_BLOCK(noop_pos)] |= GUID_MASK(noop_pos); 
      } 
      
    }
    
    // Verifico se vert_ptr e' mutex con qualche precondizione 
    
    for(temp=0,i=0; i< gnum_ft_block && temp==0; i++) 
      temp+=count_bit1(vert_ptr->ft_exclusive_vect[i] & noop_prec_actions[i]& ~prec_eff_action[i] ); 
       
    if(temp) // Non posso spostare l'azione 
      continue; 
    
    // Verifico se qualche azione mi blocca gli effetti critici di vert_ptr 
    
    for(temp=0,j=0; j< gef_conn[inf_ptr->position].num_A && temp==0; j++) {
      cel=gef_conn[inf_ptr->position].A[j];
      
      if(cel<0)
	continue; 
      
      if( CHECK_FACT_POS(cel, l1)  
	  &&   vectlevel[l1]->fact[cel].w_is_true  &&   vectlevel[l1]->fact[cel].w_is_goal) { 
	noop_pos=cel                                               ; 
	// Verifico se l'effetto additivo  puo' essere propagato nel livello corente 
	if(CHECK_NOOP_POS( noop_pos,  level)==FALSE) // Non e' possibile inserire la noop e quindi mi fermo 
	  temp=1; 
	
	if(check_mutex_noop( noop_pos,  level)>=0) 
	  temp++; 
	
      }
    }
    
    if(temp==0)	{ 
      if(DEBUG6) 
	printf("\n\n POSSO SPOSTARE L'AZIONE %s IN BASSO LEVEL %d",print_op_name_string(vert_ptr->position, temp_name), level); 
      // Controllo  se posso ulteriormente abbassare l'azione 
      
      for(l2=level-1, down=FALSE; l2>0 && temp==0 && CHECK_ACTION_POS(v_pos,l2); l2--) { 
	if(gef_conn[v_pos].ef_exclusive_vect[GUID_BLOCK(vectlevel[l2]->action.position)] &  GUID_MASK(vectlevel[l2]->action.position)) 
	  temp++; 
	
	if(temp==0) 
	  down=TRUE; 
	
      } 
 
      ret_pos=pos; 
      
      if(down==TRUE) {   
	ret_level=l2+1;  
	if(DEBUG6) 
	  printf("\n ABBASSO SEMPLICENTENTE %s level %d",print_ft_name_string(vert_ptr->position, temp_name), ret_level); 
	break; 
      } 
      
      break; // Non considero ulteriori azioni 
    } 
    
    
  } 
  
  if(ret_pos<0 || ret_level<=0) { 
    if(DEBUG6) 
      printf("\n\nNON  POSSO SPOSTARE NESSUNA AZIONE %s IN BASSO",print_op_name_string(vert_ptr->position, temp_name)); 
    return (-1); 
  } 
  
  else { 
    insert_remove_action(inf_ptr->position, *inf_ptr->level, C_T_REMOVE_ACTION, 0 );
    //      remove_action_from_vectlevel(inf_ptr->position, *inf_ptr->level, 0); 
    // if(down==FALSE) 
    //	up_vectlevel(ret_level); 
    //      insert_action_in_vectlevel(v_pos, ret_level,1); 
    insert_remove_action(v_pos, ret_level, C_T_INSERT_ACTION  ,GpG.approximation_level );
    return(ret_pos); 
  } 
  
} 

#endif





// -1 if inf_action is not mutex with other actions in the action subgraph 
// else "num" is the first action mutex with inf_action 
int
check_mutex (inform_list inf_action)
{
  register int k, temp;
  int level, i, j;
  EfConn *v_action;

  return (-1);

  v_action = CONVERT_ACTION_TO_VERTEX (inf_action->position);
  level = *inf_action->level;
  for (i = 0, j = 0; i < gnum_ef_block; i++, j += 32)
    {
      temp = 0;			// v_action->ef_exclusive_vect[i] &  vectlevel[level]->act_vect[i]; 
      k = 32;
      while (temp)
	{
	  k--;
	  if (temp & FIRST_1)
	    return (j + k);
	  temp <<= 1;
	}
    }
  return (-1);
}



static void
gmemcpy (int *dest, int *source, int size)
{

  memcpy (dest, source, size * 4);

}



// Salvo i bit array su cui andremo ad effettuare operazioni 
// copio, ad esempio, act_vect in orig_act_vect 
void
store_bit_vect (int level)
{

#ifdef TEST_GR
  printf ("\n Store???? level %d", level);
#endif

  if (vectlevel[level] == NULL)
    {

#ifdef TEST_GR
      printf ("\nNO  Store level %d", level);
#endif
      return;
    }

  if (vectlevel[level]->modified <= 0)
    {
      gmemcpy (vectlevel[level]->orig_prec_vect, vectlevel[level]->prec_vect,
	       GpG.max_num_ft_block);
      gmemcpy (vectlevel[level]->orig_fact_vect, vectlevel[level]->facT_vect,
	       GpG.max_num_ft_block);
      gmemcpy (vectlevel[level]->orig_true_crit_vect,
	       vectlevel[level]->true_crit_vect, GpG.max_num_ft_block);
      gmemcpy (vectlevel[level]->orig_false_crit_vect,
	       vectlevel[level]->false_crit_vect, GpG.max_num_ft_block);
      gmemcpy (vectlevel[level]->orig_noop_act_vect,
	       vectlevel[level]->noop_act_vect, GpG.max_num_ft_block);
      gmemcpy (vectlevel[level]->orig_noop_prec_act_vect,
	       vectlevel[level]->noop_prec_act_vect, GpG.max_num_ft_block);

#ifdef __TEST__
      printf ("\n Store level %d", level);
#endif

      vectlevel[level]->modified = 1;
    }
}





float
getdistance (char *action)
{
  float temp;
  int c1x, c1y, c2x, c2y, i;


  i = 0;

  while (action[i] != '\000')
    {
      if (action[i + 1] == '\n')
	return (0);

      if (action[i] == '_' && action[i + 1] == '_' && action[i + 4] == '_')
	{
	  c1x = (((int) action[i + 2]) - 48) * 10;
	  c1x += atoi (&action[i + 3]);
	  c1y = (((int) action[i + 5]) - 48) * 10;
	  c1y += atoi (&action[i + 6]);
	  i += 7;

	  while (action[i] != '\000')
	    {
	      if (action[i + 1] == '\n')
		return (0);

	      if (action[i] == '_' && action[i + 1] == '_'
		  && action[i + 4] == '_')
		{
		  c2x = (((int) action[i + 2]) - 48) * 10;
		  c2x += atoi (&action[i + 3]);
		  c2y = (((int) action[i + 5]) - 48) * 10;
		  c2y += atoi (&action[i + 6]);

		  i += 7;
		  temp =
		    ((c1x - c2x) * (c1x - c2x)) + ((c1y - c2y) * (c1y - c2y));
		  temp = (float) sqrt (temp);
		  return (temp);
		}
	      i++;
	    }
	}
      i++;
    }
  return (0);

}








#ifdef __RIFARE__
void
down_vectlevel (int act_level)
{
  int i, level_1, level;
  def_level_list temp_level, temp_level_2, max_temp_level;
  // g_node_list *v_facts, *v_actions; 

  // !!!!!!!!!!!!!!!!ATTENZIONE DA SISTEMARE 

  // 
  if (GpG.down_vectlevel == FALSE)
    return;
  if (act_level >= GpG.curr_plan_length || act_level <= 0)
    return;

  level = act_level;
  //for(level=0;level<GpG.curr_plan_length; ) 
  if (vectlevel[level]->num_actions <= 0
      && check_max_action_position (level) <= 0)
    {

#ifdef __TEST__
      fprintf (stderr, " DL %d  ", level);
      printf ("\n Down Level %d  ", level);
#endif

      max_temp_level = vectlevel[level];

      temp_level = vectlevel[GpG.curr_plan_length];

      for (i = GpG.curr_plan_length; i > level; i--)
	{
	  temp_level_2 = vectlevel[i - 1];
	  vectlevel[i - 1] = temp_level;
	  temp_level->level = i - 1;
	  temp_level = temp_level_2;
	}
      vectlevel[GpG.curr_plan_length] = max_temp_level;

      vectlevel[GpG.curr_plan_length]->level = GpG.curr_plan_length;

      // Reset the last level  

      level_1 = GpG.curr_plan_length;
      for (i = 0; i < GpG.max_num_facts; i++)
	{
	  vectlevel[level_1]->fact[i].w_is_goal = 0;
	  vectlevel[level_1]->fact[i].w_is_true = 0;
	  vectlevel[level_1]->fact[i].w_is_used = 0;
	  vectlevel[level_1]->fact[i].prec = 0;

	  if (vectlevel[level_1]->fact[i].false_position >= 0)
	    {
	      //  fprintf(stderr,"\n err: fact down_vectlevel %d",act_level); 
	      remove_false_fact (&vectlevel[level_1]->fact[i]);	// !!!!!!!!!!!!!!!!ATTENZIONE DA SISTEMARE 

	    }

	  vectlevel[level_1]->fact[i].false_position = -1;

	}
      //      for(i=0; i<GpG.max_num_actions; i++) 
      {

	vectlevel[level_1]->action.w_is_goal = 0;
	vectlevel[level_1]->action.w_is_true = 0;
	vectlevel[level_1]->action.w_is_used = 0;
	vectlevel[level_1]->action.prec = 0;
	/* 
	   if(vectlevel[level_1]->action.false_position>=0) 
	   { 
	   // fprintf(stderr,"\n err: act down_vectlevel %d",act_level); 
	   remove_false_action(&vectlevel[level_1]->action); // !!!!!!!!!!!!!!!!ATTENZIONE DA SISTEMARE 


	   }
	 */
	vectlevel[level_1]->action.false_position = -1;
      }


      reset_bitarray (vectlevel[level_1]->facT_vect, gnum_ft_block);
      vectlevel[level_1]->num_fact = 0;
      reset_bitarray (vectlevel[level_1]->true_crit_vect, gnum_ft_block);
      vectlevel[level_1]->num_true_crit = 0;
      reset_bitarray (vectlevel[level_1]->false_crit_vect, gnum_ft_block);
      vectlevel[level_1]->num_false_crit = 0;
      //      reset_bitarray(vectlevel[level_1]->act_vect, gnum_ft_block); 
      reset_bitarray (vectlevel[level_1]->noop_act_vect, gnum_ft_block);
      reset_bitarray (vectlevel[level_1]->noop_prec_act_vect, gnum_ft_block);

      vectlevel[level_1]->num_actions = 0;
      vectlevel[level_1]->num_prec = 0;
#ifdef TEST_GR
      printf ("\n DOWN LEVEL %d", level);
#endif
      GpG.curr_plan_length--;
    }
  else
    level++;


}
#endif




#ifdef RIFARE
// cerca l'azione del livello level con il costo maggiore e ne restituisce la posizione 
int
search_second_max_action (int level)
{

  register int k, temp;
  int i, j, pos;

  pos = -1;
  for (i = 0, j = 0; i < GpG.max_num_ft_block; i++, j += 32)
    if ((temp = vectlevel[level]->act_vect[i]))
      {
	k = 32;
	while (temp)
	  {
	    k--;
	    if (temp & FIRST_1)
	      if (vectlevel[level]->max_action_time !=
		  get_action_time (j + k, level)
		  || vectlevel[level]->max_action_time_position != (j + k))
		pos = j + k;


	    temp <<= 1;
	  }
      }
  return pos;

}

#endif















/*****************************
*           numeric.c        *
******************************/


void
reset_modifieds (void)
{
  int i;
  for (i = 0; i < GpG.curr_plan_length; i++)
    {
      memset (vectlevel[i]->numeric->modified_vars_start, 0,  sizeof (int) * gnum_block_compvar);
      memset (vectlevel[i]->numeric->modified_vars_end, 0,  sizeof (int) * gnum_block_compvar);
    }
}



/*
void swap_efconns(int e1, int e2)
{
    static EfConn park;
    memcpy(&park,&gef_conn[e1],sizeof(EfConn));
    memcpy(&gef_conn[e1],&gef_conn[e2],sizeof(EfConn));
    memcpy(&gef_conn[e2],&park,sizeof(EfConn));
}
*/

/*
//inizializza le grandezze numeriche primarie
void initialize_numvars()
{
}


void apply_num_effects_of_action(int act_pos, int level)
{
    int i;
    int num_eff;
    float *value_before;
    float *value_after;

    //puntatore ai valori correnti delle grandezze primarie e secondarie
#if __TEST__
    value_before=(float*)calloc(gnum_comp_var,sizeof(float));
    value_after=(float*)calloc(gnum_comp_var,sizeof(float));
#else
    value_before=vectlevel[level]->numeric->values;
    value_after=vectlevel[level+1]->numeric->values;
#endif
    //inizializzo le gcomp_var ai valori del livello level
    check_consistency();
#if !(__TEST__)
    for(i=0;i<gnum_comp_var;i++)
    {
	GCOMP_VAR_VALUE_BEFORE(i) = value_before[i];
	//le cvar che non tocco mantengono il loro valore precedente
	gcomp_var[i].value = value_before[i];
    }
#endif
    check_consistency();
    //per tutti gli effetti dell'azione
    for(i=0;i<gef_conn[act_pos].num_A;i++)
	//se si tratta di un effetto numerico
	if(gef_conn[act_pos].A[i]<0)
	{
	    num_eff=-gef_conn[act_pos].A[i];
	    //applica questo effetto numerico
	    eval_comp_var_non_recursive( &gcomp_var[num_eff],num_eff);
	}
    print_intlist(gcvars_to_update);
    //salvo i valori nell'array di uscita
    for(i=0;i<gnum_comp_var;i++)
    {
	value_after[i] = gcomp_var[i].value;
	GCOMP_VAR_VALUE_BEFORE(i)=gcomp_var[i].value;
    }
    //QUI: sulla base di gvars_to_update, aggiornare le cvars
    //nota: la update_cvars deve a sua volta aggiornare la lista gvars_to_update, finchè non la esaurisce 
    update_cvars();
    check_consistency();
#if __TEST__
    free(value_before);
    free(value_after);
#endif
}

void remove_num_effects_of_action(int act_pos, int level)
{
}

void propagate_num_effects(int level)
{
}
*/

/*
float eval_comp_var_non_recursive( CompositeNumVar* cv, int cv_index)
{
//versione non ricorsiva della eval_comp_var


    switch (cv->operator)
    {
    case INCREASE_OP:
      gcomp_var[cv->first_op].value += GCOMP_VAR_VALUE_BEFORE(cv->second_op);
      //qui: aggiornare la lista delle compvar che devono essere aggiornate
      if(!is_int_in_intlist( cv->first_op, gcvars_to_update ))
	  add_intlist_to_intlist( gcomp_var[ cv->first_op ].affects, &gcvars_to_update );
      break;
    case DECREASE_OP:
      gcomp_var[cv->first_op].value -= GCOMP_VAR_VALUE_BEFORE(cv->second_op);
      //qui: aggiornare la lista delle compvar che devono essere aggiornate
      if(!is_int_in_intlist( cv->first_op, gcvars_to_update ))
	  add_intlist_to_intlist( gcomp_var[ cv->first_op ].affects, &gcvars_to_update );
      break;
    case SCALE_UP_OP:
      gcomp_var[cv->first_op].value *= GCOMP_VAR_VALUE_BEFORE(cv->second_op);
      //qui: aggiornare la lista delle compvar che devono essere aggiornate
      if(!is_int_in_intlist( cv->first_op, gcvars_to_update ))
	  add_intlist_to_intlist( gcomp_var[ cv->first_op ].affects, &gcvars_to_update );
      break;
    case SCALE_DOWN_OP:
      gcomp_var[cv->first_op].value /=  GCOMP_VAR_VALUE_BEFORE(cv->second_op);
      //qui: aggiornare la lista delle compvar che devono essere aggiornate
      if(!is_int_in_intlist( cv->first_op, gcvars_to_update ))
	  add_intlist_to_intlist( gcomp_var[ cv->first_op ].affects, &gcvars_to_update );
      break;
    case ASSIGN_OP:
      gcomp_var[cv->first_op].value = GCOMP_VAR_VALUE_BEFORE(cv->second_op);
      //qui: aggiornare la lista delle compvar che devono essere aggiornate
      if(!is_int_in_intlist( cv->first_op, gcvars_to_update ))
	  add_intlist_to_intlist( gcomp_var[ cv->first_op ].affects, &gcvars_to_update );
      break;

    case MUL_OP:
	cv->value = gcomp_var[cv->first_op].value *  GCOMP_VAR_VALUE_BEFORE(cv->second_op);
	if(!is_int_in_intlist( cv_index, gcvars_to_update ))
	    add_intlist_to_intlist( cv->affects, &gcvars_to_update );
	break;
    case DIV_OP:
	cv->value = gcomp_var[cv->first_op].value /  GCOMP_VAR_VALUE_BEFORE(cv->second_op);
	if(!is_int_in_intlist( cv_index, gcvars_to_update ))
	    add_intlist_to_intlist( cv->affects, &gcvars_to_update );
	break;
    case PLUS_OP:
	cv->value = gcomp_var[cv->first_op].value + GCOMP_VAR_VALUE_BEFORE(cv->second_op);
	if(!is_int_in_intlist( cv_index, gcvars_to_update ))
	    add_intlist_to_intlist( cv->affects, &gcvars_to_update );
	break;
    case MINUS_OP:
	cv->value = gcomp_var[cv->first_op].value - GCOMP_VAR_VALUE_BEFORE(cv->second_op);
	if(!is_int_in_intlist( cv_index, gcvars_to_update ))
	    add_intlist_to_intlist( cv->affects, &gcvars_to_update );
	break;
    case UMINUS_OP:
	cv->value = -gcomp_var[cv->first_op].value;
	if(!is_int_in_intlist( cv_index, gcvars_to_update ))
	    add_intlist_to_intlist( cv->affects, &gcvars_to_update );
	break;

    default:
	printf ("\nShouldn get Operator %d here",cv->operator);
	exit(1);
	return -1;
	break;
    }
    return -1;
}

void check_consistency(void)
{
    int i;
    CompositeNumVar* cv;
    for(i=0;i<gnum_comp_var;i++)
    {
	cv=&gcomp_var[i];
	switch(cv->operator)
	{
	case MUL_OP:
	    if ((cv->value - ( gcomp_var[cv->first_op].value * gcomp_var[cv->second_op].value ) )>MAX_APPROX)
	    {
		printf("\n\n inconsistency in cvars array");
		printf("\nop: *");
		printf("\nfirst   : %5d:%8.2f",cv->first_op,gcomp_var[cv->first_op].value);
		printf("\nsecond  : %5d:%8.2f",cv->second_op,gcomp_var[cv->second_op].value);
		printf("\ncorrect : %5.2f",gcomp_var[cv->first_op].value * gcomp_var[cv->second_op].value);
		printf("\nreported: %5.2f\n\n",cv->value);
		exit(1);
	    }
	    break;
	case DIV_OP:
	    if (gcomp_var[cv->second_op].value == 0)
	    {
		printf("\n\n check_consistency: div by 0\n\n");
		exit(1);
	    }
	    if ((cv->value - ( gcomp_var[cv->first_op].value / gcomp_var[cv->second_op].value ) )>MAX_APPROX)
	    {
		printf("\n\n inconsistency in cvars array\n\n");
		printf("\nop: /");
		printf("\nfirst   : %5d:%8.2f",cv->first_op,gcomp_var[cv->first_op].value);
		printf("\nsecond  : %5d:%8.2f",cv->second_op,gcomp_var[cv->second_op].value);
		printf("\ncorrect : %5.2f",gcomp_var[cv->first_op].value / gcomp_var[cv->second_op].value);
		printf("\nreported: %5.2f\n\n",cv->value);
		exit(1);
	    }
	    break;
	case PLUS_OP:
	    if ((cv->value - ( gcomp_var[cv->first_op].value + gcomp_var[cv->second_op].value ) )>MAX_APPROX)
	    {
		printf("\n\n inconsistency in cvars array\n\n");
		printf("\nop: +");
		printf("\nfirst   : %5d:%8.2f",cv->first_op,gcomp_var[cv->first_op].value);
		printf("\nsecond  : %5d:%8.2f",cv->second_op,gcomp_var[cv->second_op].value);
		printf("\ncorrect : %5.2f",gcomp_var[cv->first_op].value + gcomp_var[cv->second_op].value);
		printf("\nreported: %5.2f\n\n",cv->value);
		exit(1);
	    }
	    break;
	case MINUS_OP:
	    if ((cv->value - ( gcomp_var[cv->first_op].value - gcomp_var[cv->second_op].value ) )>MAX_APPROX)
	    {
		printf("\n\n inconsistency in cvars array\n\n");
		printf("\nop: -");
		printf("\nfirst   : %5d:%8.2f",cv->first_op,gcomp_var[cv->first_op].value);
		printf("\nsecond  : %5d:%8.2f",cv->second_op,gcomp_var[cv->second_op].value);
		printf("\ncorrect : %5.2f",gcomp_var[cv->first_op].value - gcomp_var[cv->second_op].value);
		printf("\nreported: %5.2f\n\n",cv->value);
		exit(1);
	    }
	    break;
	case UMINUS_OP:
	    if ((cv->value  + gcomp_var[cv->first_op].value )>MAX_APPROX)
	    {
		printf("\n\n inconsistency in cvars array\n\n");
		printf("\nop: unary -");
		printf("\nfirst   : %5d:%8.2f",cv->first_op,gcomp_var[cv->first_op].value);
		printf("\nsecond  : %5d:%8.2f",cv->second_op,gcomp_var[cv->second_op].value);
		printf("\ncorrect : %5.2f",-gcomp_var[cv->first_op].value);
		printf("\nreported: %5.2f\n\n",cv->value);
		exit(1);
	    }
	    break;
	case LESS_THAN_OP:
	case LESS_THAN_OR_EQUAL_OP:
	case EQUAL_OP:
	case GREATER_THAN_OP:
	case GREATER_OR_EQUAL_OP:
	    //per i precedenti, forse mettere un check se il valore del float è 1.0 se vero e 0.0 se falso
	case INCREASE_OP:
	case DECREASE_OP:
	case SCALE_UP_OP:
	case SCALE_DOWN_OP:      
	case ASSIGN_OP:
	case MINIMIZE_OP:
	case MAXIMIZE_OP:
	case FIX_NUMBER:
	case VARIABLE_OP:
	    //no need to check anything
	    break;
	default:
	    printf ("\nOperator %d not yet supported in consistency check\n\n",cv->operator);
	    exit(1);
	    break;
	}
    }
}

void add_intlist_to_intlist(IntList* to_queue, IntList** first_list) 
{
    IntList* tmpil;
    IntList* copy_il;
//    IntList* new_ils=NULL;
    copy_il = copy_intlist(to_queue);
    if(!*first_list)
    {
	*first_list=copy_il;
	return;
    }
    //toglie dalla lista copy gli elementi che sono già presenti in *first_list
    //va a pescare l'ultimo elemento della lista
    for(tmpil=*first_list;tmpil;tmpil=tmpil->next)
	if (!tmpil->next)
	{
	    tmpil->next=copy_il;
	    break;
	}
}

IntList* copy_intlist(IntList* src)
{
    IntList* tmpil;
    IntList* ret;
    IntList* el;
    IntList* prev=NULL;
    for(tmpil=src;tmpil;tmpil=tmpil->next)
    {
	el=new_IntList();
	if (tmpil==src)
	    ret=el;
	el->item=tmpil->item;
	if(prev)
	    prev->next=el;
	prev=el;
    }
    return ret;
}

void update_cvars(void)
{
    IntList* nextil;
    //aggiorna le cvar, che poichè inserite nella lista, devono essere aggiornate
    while(gcvars_to_update)
    {
	nextil=gcvars_to_update->next;
	printf("\nEvaluating gcomp var no. %d",gcvars_to_update->item);
	eval_comp_var_non_recursive( &gcomp_var[gcvars_to_update->item],gcvars_to_update->item);
	//a sua volta la gcvar appena esaminata richiederà aggiornamento di altre gcvars, che vengono aggiunte in coda
	free(gcvars_to_update);
	gcvars_to_update=nextil;
	print_intlist(gcvars_to_update);
    }
    gcvars_to_update=NULL;
}

void print_intlist(IntList* il)
{
    IntList* tmpil;

    printf("\nINTLIST: ");
    for(tmpil=il;tmpil;tmpil=tmpil->next)
	printf("%5d",tmpil->item);
}

Bool is_int_in_intlist(int n, IntList* il)
{
    IntList* tmpil;

    for(tmpil=il;tmpil;tmpil=tmpil->next)
	if(tmpil->item==n)
	    return TRUE;
    return FALSE;
}



IntList* free_intlist(IntList* ilist)
{
    IntList* nextil;
    //aggiorna le cvar, che poichè inserite nella lista, devono essere aggiornate
    while(ilist)
    {
	nextil=ilist->next;
	free(ilist);
	ilist=nextil;
    }
    return (IntList*)NULL;
}
*/




//LAZZA 03052002 x vicinato numerico
float
try_num_eff_in_level (int cv_index, float *in_vect, float *out_vect)
{
  CompositeNumVar *cv = &gcomp_var[cv_index];
  int first_op = cv->first_op;
  int second_op = cv->second_op;
  float tmp;

  assert (cv_index >= 0);
  first_op = gcomp_var[cv_index].first_op;
  second_op = gcomp_var[cv_index].second_op;

  switch (cv->operator)
    {
    case FIX_NUMBER:
    case VARIABLE_OP:
      out_vect[cv_index] = in_vect[cv_index];
      return out_vect[cv_index];
      break;
    case MUL_OP:
      out_vect[cv_index] =
	try_num_eff_in_level (first_op, in_vect,
			      out_vect) * try_num_eff_in_level (second_op,
								in_vect,
								out_vect);
      return out_vect[cv_index];
      break;
    case DIV_OP:
      tmp = try_num_eff_in_level (second_op, in_vect, out_vect);
      if (tmp == 0)
	{
	  printf ("\n\nWARNING: Division by zero in try_num_eff\n\n");
	  out_vect[cv_index] = 0;
	  return out_vect[cv_index];
	}
      out_vect[cv_index] =
	try_num_eff_in_level (first_op, in_vect, out_vect) / tmp;
      return out_vect[cv_index];
    case PLUS_OP:
      out_vect[cv_index] =
	try_num_eff_in_level (first_op, in_vect,
			      out_vect) + try_num_eff_in_level (second_op,
								in_vect,
								out_vect);
      return out_vect[cv_index];
      break;
    case MINUS_OP:
      out_vect[cv_index] =
	try_num_eff_in_level (first_op, in_vect,
			      out_vect) - try_num_eff_in_level (second_op,
								in_vect,
								out_vect);
      return out_vect[cv_index];
      break;
    case UMINUS_OP:
      out_vect[cv_index] =
	-try_num_eff_in_level (first_op, in_vect, out_vect);
      return out_vect[cv_index];
      break;
    case INCREASE_OP:
      out_vect[first_op] +=
	try_num_eff_in_level (second_op, in_vect, out_vect);
      return out_vect[first_op];
      break;
    case DECREASE_OP:
      out_vect[first_op] -=
	try_num_eff_in_level (second_op, in_vect, out_vect);
      return out_vect[first_op];
      break;
    case SCALE_UP_OP:
      out_vect[first_op] *=
	try_num_eff_in_level (second_op, in_vect, out_vect);
      return out_vect[first_op];
      break;
    case SCALE_DOWN_OP:
      out_vect[first_op] /= try_num_eff_in_level (second_op, in_vect,
						  out_vect);
      return out_vect[first_op];
      break;
    case ASSIGN_OP:
      out_vect[first_op] =
	try_num_eff_in_level (second_op, in_vect, out_vect);
      return out_vect[first_op];
      break;
    default:
      printf ("\n\nnot considered\n\n");
      exit (2);
      break;
      /*
         case MINIMIZE_OP:
         return eval_comp_var( &(gcomp_var[cv->first_op]), cv->first_op );
         case MAXIMIZE_OP:
         return - eval_comp_var( &(gcomp_var[cv->first_op]), cv->first_op );
         case EQUAL_OP:
         cv->value = ( eval_comp_var( &(gcomp_var[cv->first_op]), cv->first_op ) == eval_comp_var( &(gcomp_var[cv->second_op]), cv->second_op ) );
         return cv->value;
         case GREATER_THAN_OP:
         cv->value = ( eval_comp_var( &(gcomp_var[cv->first_op]), cv->first_op ) >   eval_comp_var( &(gcomp_var[cv->second_op]), cv->second_op ) );
         return cv->value;
         case GREATER_OR_EQUAL_OP:
         cv->value = ( eval_comp_var( &(gcomp_var[cv->first_op]), cv->first_op ) >= eval_comp_var( &(gcomp_var[cv->second_op]), cv->second_op ) );
         return cv->value;
         case LESS_THAN_OP:
         cv->value = ( eval_comp_var( &(gcomp_var[cv->first_op]), cv->first_op ) <  eval_comp_var( &(gcomp_var[cv->second_op]), cv->second_op ) );
         return cv->value;
         case LESS_THAN_OR_EQUAL_OP:
         cv->value = ( eval_comp_var( &(gcomp_var[cv->first_op]), cv->first_op ) <= eval_comp_var( &(gcomp_var[cv->second_op]), cv->second_op ) );
         return cv->value;
         default:
         printf ("\nOperator %d not yet supported in expression evaluation\n\n",cv->operator);
         exit(1);
         return -1;
         break;
       */
    }

  return 0;
}

//endlazza 03052002


//LAZZA
float compute_numeric_fact_cost_old (inform_list Fact, node_cost_list n_cost, int action_level){
  register int temp;
  int level, curr_level, k, j;
// int next_level;
//    FtConn *tofix; 
  static action_set neighbors;
  auto float total, cost, prec, mutex, prec_act_cost;
  inform_list inf_fact, inf_noop;
  int best_action = -1, best_level = 0, best_act_type = 0;
  int el;
  int cel;
//    int ind_level;  
  node_cost loc_n_cost, best_n_cost;


  level = *Fact->level;

  prec_act_cost = 0.0;
  inf_noop = NULL;
#ifdef __TEST__
  if (Fact->action_fact != IS_FACT)
    {
      MSG_ERROR ("compute_numeric_fact_cost; debug me please");
      exit (0);
    }
  if (DEBUG2)
    {
      printf ("\n COMPUTE NUMERIC FACT COST %d level %d name :",
	      Fact->position, level);
      //      print_cvar_tree(Fact->position);
    }
#endif
  if (is_num_prec_satisfied_in_common_level (Fact->position)
      && level <= action_level)
    {

      n_cost->weight = 0.0;
      n_cost->act_cost = 0.0;

      if (GpG.temporal_plan)
	n_cost->act_time = Fact->time_f;


      return (0.0);
    }
  // Il costo e' stato precedentemente calcolato 
  total = get_numeric_fact_cost (Fact, &loc_n_cost);
  if (total >= 0 && total < MAX_COST
      && GpG.accurate_cost < COMPUTE_DG_LIST_COST)
    {
#ifdef __TEST__

      printf
	("\n\n ***********************Fatto %s, weight  %f cost %f time %f level %d",
	 print_ft_name_string (Fact->position, temp_name), total,
	 loc_n_cost.act_cost, loc_n_cost.act_time, level);
#endif
      n_cost->weight = loc_n_cost.weight;
      n_cost->act_cost = loc_n_cost.act_cost;
      n_cost->act_time = loc_n_cost.act_time;
      return total;
    }
  //in caso di iterazione ricorsiva torno indietro fino al livello zero: sotto zero però basta
  //dato che non faccio iterazione ricorsiva con i fatti numerici, il check dovrebbe essere superfluo
  if (level <= 0)
    return 0.0;
#ifdef __TEST__
  printf ("\n ***********************Start level %d end lev %d ", level,
	  level);
#endif
  total = 0.0;
  best_action = -1;
  best_n_cost.weight = MAX_COST;
  best_n_cost.act_cost = MAX_COST;
  best_n_cost.act_time = MAX_COST;
  //qui c'era il ciclo for con curr_level da level a 0
  curr_level = level;
  neighbors.num_A = 0;
  memset (neighbors.A, 0, MAX_NUM_ACTIONS * sizeof (int));
  create_neighborhood_for_compvar (-Fact->position, 1, 0, &neighbors, 1,
				   *Fact->level);
  for (j = 0; j < neighbors.num_A; j++)
    {
      cel = neighbors.A[j];
      if (Hvar.temp_removed_act == cel && neighbors.num_A > 1)
	{
#ifdef __TEST__
	  printf ("\n Non considera azione %d : ", cel);
	  print_op_name (cel);
#endif
	  continue;
	}
      if (CHECK_ACTION_POS (cel, curr_level))
	{
	  if (GpG.accurate_cost <= COMPUTE_ADD_COST)
	    {
	      cost = fast_insertion_action_cost (cel, curr_level, action_level);	//, &loc_n_cost );
	      loc_n_cost.act_cost = get_action_cost (cel);
	      loc_n_cost.act_time = get_action_time (cel, curr_level);
	    }
	  else
	    cost =
	      dg_insertion_action_cost (cel, curr_level, action_level,
					&loc_n_cost);
	  if (best_n_cost.weight >= cost)
	    {
	      if (best_n_cost.weight == cost
		  && weight_cost (&best_n_cost) <= weight_cost (&loc_n_cost))
		continue;
	      best_action = cel;
	      best_level = curr_level;
	      best_act_type = IS_ACTION;
	      best_n_cost.weight = loc_n_cost.weight;
	      best_n_cost.act_cost = loc_n_cost.act_cost;
	      best_n_cost.act_time = loc_n_cost.act_time;
#ifdef __TEST__
	      printf
		("\n\n\n Comp_fact_cost  BEST ACT %s  time %d inc %.2f act_cost %.2f act_time %.2f  ",
		 print_op_name_string (best_action, temp_name), best_level,
		 best_n_cost.weight, best_n_cost.act_cost,
		 best_n_cost.act_time);
#endif
	      if (best_n_cost.weight <= 0)
		break;		// Non esamino ulteriori candidati 
	    }
	}
#ifdef __TEST__
      else
	{
	  printf
	    ("\n L'azione %s non puo' essere applicata al livello %d, lev: %d",
	     print_op_name_string (cel, temp_name), curr_level,
	     gef_conn[cel].level);
	}
#endif
    }

  if (best_action < 0)		//WARNING 
    {
      Hvar.weight_facts_define_cost = MAX_COST;
      n_cost->weight = MAX_COST;
      n_cost->act_cost = MAX_COST;
#ifdef __TEST__
      printf
	("\nL'unica azione che posso scegliere e' una noop weight %f cost %f",
	 n_cost->weight, n_cost->act_cost);
#endif
      return (MAX_COST);

      // Definisco il costo per rendere vere le precondizioni non supportate 
    }
#ifdef __TEST__
  printf ("\n\n     BEST_action   %s  time %d pos %d ",
	  print_op_name_string (best_action, temp_name), best_level,
	  best_action);
#endif
  n_cost->weight = 0.0;
  n_cost->act_cost = 0.0;
  n_cost->act_time = 0.0;
  // Setto effetti di best action 
  if (GpG.accurate_cost >= COMPUTE_DG_LIST_COST)
    {
      if (insert_action_inlist (best_action) == 0)	// inserisco subito best action per evitare cicli di inserimento
	{
	  Hvar.weight_facts_define_cost++;
	  return 1.0;		// Azione precedentem considerata
	}
    }
  // cost  of  Best_action
  // Precondizioni at start
  for (j = 0, k = 0, prec = 0.0; j < gef_conn[best_action].num_PC; j++)
    {
      el = gef_conn[best_action].PC[j];
      if (el < 0)
	{
	  //se la preco numerica è soddisfatta, non mi costa nulla
	  if (is_num_prec_satisfied_in_common_level (el))
	    continue;
	  temp =
	    compute_numeric_fact_cost_old (inf_fact, &loc_n_cost, action_level);
	  continue;		//LAZZA
	}
      if (CHECK_FACT_POS (el, best_level))
	{
#ifdef __TEST__
	  if (DEBUG2)
	    printf
	      ("\n\n\n %d  *********************** COMPUTE DG MAX COST ACT %s fact %s",
	       ++k, print_op_name_string (best_action, temp_name),
	       print_ft_name_string (el, temp_name));
#endif
	  inf_fact = CONVERT_FACT_TO_INFORM (el, best_level);
	  if (fact_is_supported (el, best_level))
	    {
#ifdef __TEST__
	      printf ("\n Level %d Supported fact %s ", best_level,
		      print_ft_name_string (el, temp_name));
#endif
	      continue;
	    }
	  if (GpG.accurate_cost >= COMPUTE_DG_LIST_COST
	      && GET_BIT (Hvar.bit_vect_facts, el))
	    {
#ifdef __TEST__
	      printf
		("\nFact %s already supported in COMPUTE_DG_LIST_COST, level %d",
		 print_ft_name_string (el, temp_name), best_level);
#endif
	      continue;
	    }
	  temp = compute_max_fact_cost (inf_fact, &loc_n_cost, action_level);
	  if (GpG.accurate_cost == COMPUTE_DG_MAX_COST)
	    {
	      if (n_cost->act_cost < loc_n_cost.act_cost
		  && loc_n_cost.act_cost < MAX_COST)
		n_cost->act_cost = loc_n_cost.act_cost;
	      if (n_cost->act_time < loc_n_cost.act_time
		  && loc_n_cost.act_time < MAX_COST)
		n_cost->act_time = loc_n_cost.act_time;
#ifdef __TEST__
	      if (DEBUG2)
		{
		  if (best_act_type == IS_ACTION)
		    printf
		      ("\n %d  *********************** END COMPUTE DG MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f  act_duration %.2f",
		       k, print_op_name_string (best_action, temp_name),
		       print_ft_name_string (el, temp_name), prec,
		       loc_n_cost.weight, loc_n_cost.act_cost,
		       loc_n_cost.act_time, get_action_time (best_action,
							     best_level));
		  else
		    printf
		      ("\n %d  *********************** END COMPUTE DG MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f",
		       k, print_noop_name_string (best_act_type, temp_name),
		       print_ft_name_string (el, temp_name), prec,
		       loc_n_cost.weight, loc_n_cost.act_cost,
		       loc_n_cost.act_time);
		}
#endif
	      if (prec < temp)
		{
		  prec = temp;
		  n_cost->weight = loc_n_cost.weight;
#ifdef __TEST__
		  if (DEBUG2)
		    {
		      if (best_act_type == IS_ACTION)
			printf
			  ("\n\n %d *********************** BEST  MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
			   k, print_op_name_string (best_action, temp_name),
			   print_ft_name_string (el, temp_name), prec,
			   n_cost->weight, n_cost->act_cost,
			   n_cost->act_time);
		      else
			printf
			  ("\n %d BEST  MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
			   k, print_noop_name_string (best_action, temp_name),
			   print_ft_name_string (el, temp_name), prec,
			   n_cost->weight, n_cost->act_cost,
			   n_cost->act_time);
		    }
#endif
		  if (FALSE
		      && (temp *=
			  local_search.lamda_prec) > local_search.best_cost)
		    {
		      if (DEBUG2)
			printf ("\n MAX_ACT_COST stop");
		      return temp;	// Ho superato il cost limite e quindi non ricerco ulteriormente 
		    }
		}
	    }
	  else if (GpG.accurate_cost == COMPUTE_DG_SUM_COST)
	    {
	      if (loc_n_cost.act_cost < MAX_COST)
		n_cost->act_cost += loc_n_cost.act_cost;
	      if (loc_n_cost.act_time < MAX_COST)
		n_cost->act_time += loc_n_cost.act_time;
#ifdef __TEST__
	      if (DEBUG2)
		printf
		  ("\n %d  *********************** END COMPUTE DG ADD COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f  time  %.2f",
		   k, print_op_name_string (best_action, temp_name),
		   print_ft_name_string (el, temp_name), prec,
		   loc_n_cost.weight, loc_n_cost.act_cost,
		   loc_n_cost.act_time);
#endif
	      prec += temp;
	      n_cost->weight += loc_n_cost.weight;
#ifdef __TEST__
	      if (DEBUG2)
		printf
		  ("\n %d BEST  ADD COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
		   k, print_op_name_string (best_action, temp_name),
		   print_ft_name_string (el, temp_name), prec, n_cost->weight,
		   n_cost->act_cost, n_cost->act_time);
#endif
	    }
	  else if (GpG.accurate_cost >= COMPUTE_DG_LIST_COST)
	    {
	      if (n_cost->act_time < loc_n_cost.act_time
		  && loc_n_cost.act_time < MAX_COST)
		n_cost->act_time = loc_n_cost.act_time;
#ifdef __TEST__
	      if (DEBUG2)
		{
		  printf
		    ("\n %d  *********************** END COMPUTE DG MAX COST ACT %s cost %.2f weight %.2f act_cost %.2f act_time %.2f  act_duration %.2f fact :",
		     k, print_op_name_string (best_action, temp_name), prec,
		     loc_n_cost.weight, loc_n_cost.act_cost,
		     loc_n_cost.act_time, get_action_time (best_action,
							   best_level));
		  print_ft_name (el);
		}
#endif
	      if (prec < loc_n_cost.weight)
		{
		  prec = loc_n_cost.weight;
#ifdef __TEST__
		  if (DEBUG2)
		    {
		      printf
			("\n %d BEST  MAX COST ACT %s  cost %.2f weight %.2f act_cost %.2f act_time %.2f  fact:",
			 k, print_op_name_string (best_action, temp_name),
			 prec, loc_n_cost.weight, loc_n_cost.act_cost,
			 loc_n_cost.act_time);
		      print_ft_name (el);
		    }
#endif
		}
	    }
	  else
	    //LAZZA
	  if (GpG.accurate_cost == COMPUTE_MAX_COST)
	    {
	      if (n_cost->act_cost < loc_n_cost.act_cost
		  && loc_n_cost.act_cost < MAX_COST)
		n_cost->act_cost = loc_n_cost.act_cost;

	      if (n_cost->act_time < loc_n_cost.act_time
		  && loc_n_cost.act_time < MAX_COST)
		n_cost->act_time = loc_n_cost.act_time;

#ifdef __TEST__
	      if (DEBUG2)
		{
		  if (best_act_type == IS_ACTION)
		    printf
		      ("\n %d +++++++++++++ END COMPUTE MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f  act_duration %.2f",
		       k, print_op_name_string (best_action, temp_name),
		       print_ft_name_string (el, temp_name), prec,
		       loc_n_cost.weight, loc_n_cost.act_cost,
		       loc_n_cost.act_time, get_action_time (best_action,
							     best_level));
		  else
		    printf
		      ("\n %d +++++++++++++ END COMPUTE MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f",
		       k, print_noop_name_string (best_act_type, temp_name),
		       print_ft_name_string (el, temp_name), prec,
		       loc_n_cost.weight, loc_n_cost.act_cost,
		       loc_n_cost.act_time);
		}
#endif

	      if (prec < temp)
		{
		  prec = temp;
		  n_cost->weight = loc_n_cost.weight;


#ifdef __TEST__
		  if (DEBUG2)
		    {
		      if (best_act_type == IS_ACTION)
			printf
			  ("\n %d BEST  MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
			   k, print_op_name_string (best_action, temp_name),
			   print_ft_name_string (el, temp_name), prec,
			   n_cost->weight, n_cost->act_cost,
			   n_cost->act_time);
		      else
			printf
			  ("\n %d BEST  MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
			   k, print_noop_name_string (best_action, temp_name),
			   print_ft_name_string (el, temp_name), prec,
			   n_cost->weight, n_cost->act_cost,
			   n_cost->act_time);
		    }
#endif



		  if (FALSE
		      && (temp *=
			  local_search.lamda_prec) > local_search.best_cost)
		    {

		      if (DEBUG2)
			printf ("\n MAX_ACT_COST stop");
		      return temp;	// Ho superato il cost limite e quindi non ricerco ulteriormente 
		    }

		}
	    }
	  else if (GpG.accurate_cost == COMPUTE_ADD_COST)
	    {
	      if (loc_n_cost.act_cost < MAX_COST)
		n_cost->act_cost += loc_n_cost.act_cost;
	      if (loc_n_cost.act_time < MAX_COST)
		n_cost->act_time += loc_n_cost.act_time;


#ifdef __TEST__
	      if (DEBUG2)
		printf
		  ("\n %d +++++++++++++ END COMPUTE ADD COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f  time  %.2f",
		   k, print_op_name_string (best_action, temp_name),
		   print_ft_name_string (el, temp_name), prec,
		   loc_n_cost.weight, loc_n_cost.act_cost,
		   loc_n_cost.act_time);
#endif
	      prec += temp;
	      n_cost->weight += loc_n_cost.weight;


#ifdef __TEST__
	      if (DEBUG2)
		printf
		  ("\n %d BEST  ADD COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
		   k, print_op_name_string (best_action, temp_name),
		   print_ft_name_string (el, temp_name), prec, n_cost->weight,
		   n_cost->act_cost, n_cost->act_time);

#endif
	    }
	  //ENDLAZZA

	}
#ifdef __TEST__
      else
	printf
	  ("\n ERRORE 1002  Fatto %s non presente al corrispondente livello %d, first level %d",
	   print_ft_name_string (el, temp_name), level - 1,
	   CONVERT_FACT_TO_VERTEX (el)->level);
#endif
    }
  // Precondizioni overall
  if (gef_conn[best_action].sf != NULL)
    {
      for (j = 0 /*,k=0, prec=0.0 */ ;
	   j < gef_conn[best_action].sf->num_PC_overall; j++)
	{
	  el = gef_conn[best_action].sf->PC_overall[j];
	  if (el < 0)
	    {
	      //se la preco numerica è soddisfatta, non mi costa nulla
	      if (is_num_prec_satisfied_in_common_level (el))
		continue;
	      temp =
		compute_numeric_fact_cost_old (inf_fact, &loc_n_cost,
					   action_level);
	      continue;		//LAZZA
	    }
	  if (is_fact_in_additive_effects_start (best_action, el))
	    continue;
	  if (vectlevel[best_level]->noop_act[el].w_is_used > 0)
	    continue;
	  if (CHECK_FACT_POS (el, best_level))
	    {
#ifdef __TEST__
	      if (DEBUG2)
		printf
		  ("\n %d ***********************+ COMPUTE DG MAX COST ACT %s fact %s",
		   ++k, print_op_name_string (best_action, temp_name),
		   print_ft_name_string (el, temp_name));
#endif
	      inf_fact = CONVERT_FACT_TO_INFORM (el, best_level);
	      if (fact_is_supported (el, best_level))
		{
#ifdef __TEST__
		  printf ("\n Level %d Supported fact %s ", best_level,
			  print_ft_name_string (el, temp_name));
#endif
		  continue;
		}
	      if (GpG.accurate_cost >= COMPUTE_DG_LIST_COST
		  && GET_BIT (Hvar.bit_vect_facts, el))
		{
#ifdef __TEST__
		  printf
		    ("\nFact %s already supported in COMPUTE_DG_LIST_COST, level %d",
		     print_ft_name_string (el, temp_name), best_level);
#endif
		  continue;
		}
	      temp =
		compute_max_fact_cost (inf_fact, &loc_n_cost, action_level);
	      if (GpG.accurate_cost == COMPUTE_DG_MAX_COST)
		{
		  if (n_cost->act_cost < loc_n_cost.act_cost
		      && loc_n_cost.act_cost < MAX_COST)
		    n_cost->act_cost = loc_n_cost.act_cost;
		  if (n_cost->act_time < loc_n_cost.act_time
		      && loc_n_cost.act_time < MAX_COST)
		    n_cost->act_time = loc_n_cost.act_time;
#ifdef __TEST__
		  if (DEBUG2)
		    {
		      if (best_act_type == IS_ACTION)
			printf
			  ("\n %d  *********************** END COMPUTE DG MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f  act_duration %.2f",
			   k, print_op_name_string (best_action, temp_name),
			   print_ft_name_string (el, temp_name), prec,
			   loc_n_cost.weight, loc_n_cost.act_cost,
			   loc_n_cost.act_time, get_action_time (best_action,
								 best_level));
		      else
			printf
			  ("\n %d  *********************** END COMPUTE DG MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f",
			   k, print_noop_name_string (best_act_type,
						      temp_name),
			   print_ft_name_string (el, temp_name), prec,
			   loc_n_cost.weight, loc_n_cost.act_cost,
			   loc_n_cost.act_time);
		    }
#endif
		  if (prec < temp)
		    {
		      prec = temp;
		      n_cost->weight = loc_n_cost.weight;
#ifdef __TEST__
		      if (DEBUG2)
			{
			  if (best_act_type == IS_ACTION)
			    printf
			      ("\n %d  *********************** BEST  MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
			       k, print_op_name_string (best_action,
							temp_name),
			       print_ft_name_string (el, temp_name), prec,
			       n_cost->weight, n_cost->act_cost,
			       n_cost->act_time);
			  else
			    printf
			      ("\n %d BEST  MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
			       k, print_noop_name_string (best_action,
							  temp_name),
			       print_ft_name_string (el, temp_name), prec,
			       n_cost->weight, n_cost->act_cost,
			       n_cost->act_time);
			}
#endif
		      if (FALSE
			  && (temp *=
			      local_search.lamda_prec) >
			  local_search.best_cost)
			{
			  if (DEBUG2)
			    printf ("\n MAX_ACT_COST stop");
			  return temp;	// Ho superato il cost limite e quindi non ricerco ulteriormente 
			}
		    }
		}
	      else if (GpG.accurate_cost == COMPUTE_DG_SUM_COST)
		{
		  if (loc_n_cost.act_cost < MAX_COST)
		    n_cost->act_cost += loc_n_cost.act_cost;
		  if (loc_n_cost.act_time < MAX_COST)
		    n_cost->act_time += loc_n_cost.act_time;
#ifdef __TEST__
		  if (DEBUG2)
		    printf
		      ("\n %d  *********************** END COMPUTE DG ADD COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f  time  %.2f",
		       k, print_op_name_string (best_action, temp_name),
		       print_ft_name_string (el, temp_name), prec,
		       loc_n_cost.weight, loc_n_cost.act_cost,
		       loc_n_cost.act_time);
#endif
		  prec += temp;
		  n_cost->weight += loc_n_cost.weight;
#ifdef __TEST__
		  if (DEBUG2)
		    printf
		      ("\n %d BEST  ADD COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
		       k, print_op_name_string (best_action, temp_name),
		       print_ft_name_string (el, temp_name), prec,
		       n_cost->weight, n_cost->act_cost, n_cost->act_time);
#endif
		}
	      //LAZZA
	      else if (GpG.accurate_cost == COMPUTE_MAX_COST)
		{
		  if (n_cost->act_cost < loc_n_cost.act_cost
		      && loc_n_cost.act_cost < MAX_COST)
		    n_cost->act_cost = loc_n_cost.act_cost;

		  if (n_cost->act_time < loc_n_cost.act_time
		      && loc_n_cost.act_time < MAX_COST)
		    n_cost->act_time = loc_n_cost.act_time;

#ifdef __TEST__
		  if (DEBUG2)
		    {
		      if (best_act_type == IS_ACTION)
			printf
			  ("\n %d +++++++++++++ END COMPUTE MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f  act_duration %.2f",
			   k, print_op_name_string (best_action, temp_name),
			   print_ft_name_string (el, temp_name), prec,
			   loc_n_cost.weight, loc_n_cost.act_cost,
			   loc_n_cost.act_time, get_action_time (best_action,
								 best_level));
		      else
			printf
			  ("\n %d +++++++++++++ END COMPUTE MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f",
			   k, print_noop_name_string (best_act_type,
						      temp_name),
			   print_ft_name_string (el, temp_name), prec,
			   loc_n_cost.weight, loc_n_cost.act_cost,
			   loc_n_cost.act_time);
		    }
#endif

		  if (prec < temp)
		    {
		      prec = temp;
		      n_cost->weight = loc_n_cost.weight;


#ifdef __TEST__
		      if (DEBUG2)
			{
			  if (best_act_type == IS_ACTION)
			    printf
			      ("\n %d BEST  MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
			       k, print_op_name_string (best_action,
							temp_name),
			       print_ft_name_string (el, temp_name), prec,
			       n_cost->weight, n_cost->act_cost,
			       n_cost->act_time);
			  else
			    printf
			      ("\n %d BEST  MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
			       k, print_noop_name_string (best_action,
							  temp_name),
			       print_ft_name_string (el, temp_name), prec,
			       n_cost->weight, n_cost->act_cost,
			       n_cost->act_time);
			}
#endif



		      if (FALSE
			  && (temp *=
			      local_search.lamda_prec) >
			  local_search.best_cost)
			{

			  if (DEBUG2)
			    printf ("\n MAX_ACT_COST stop");
			  return temp;	// Ho superato il cost limite e quindi non ricerco ulteriormente 
			}

		    }
		}
	      else if (GpG.accurate_cost == COMPUTE_ADD_COST)
		{
		  if (loc_n_cost.act_cost < MAX_COST)
		    n_cost->act_cost += loc_n_cost.act_cost;
		  if (loc_n_cost.act_time < MAX_COST)
		    n_cost->act_time += loc_n_cost.act_time;


#ifdef __TEST__
		  if (DEBUG2)
		    printf
		      ("\n %d +++++++++++++ END COMPUTE ADD COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f  time  %.2f",
		       k, print_op_name_string (best_action, temp_name),
		       print_ft_name_string (el, temp_name), prec,
		       loc_n_cost.weight, loc_n_cost.act_cost,
		       loc_n_cost.act_time);
#endif
		  prec += temp;
		  n_cost->weight += loc_n_cost.weight;


#ifdef __TEST__
		  if (DEBUG2)
		    printf
		      ("\n %d BEST  ADD COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
		       k, print_op_name_string (best_action, temp_name),
		       print_ft_name_string (el, temp_name), prec,
		       n_cost->weight, n_cost->act_cost, n_cost->act_time);

#endif
		}
	      //ENDLAZZA

	    }
#ifdef __TEST__
	  else
	    printf
	      ("\n ERRORE 1002  Fatto %s non presente al corrispondente livello %d, first level %d",
	       print_ft_name_string (el, temp_name), level - 1,
	       CONVERT_FACT_TO_VERTEX (el)->level);
#endif
	}
    }
  // Precondizioni at end
  if (gef_conn[best_action].sf != NULL)
    {
      for (j = 0 /*,k=0, prec=0.0 */ ;
	   j < gef_conn[best_action].sf->num_PC_end; j++)
	{
	  el = gef_conn[best_action].sf->PC_end[j];
	  if (el < 0)
	    {
	      //se la preco numerica è soddisfatta, non mi costa nulla
	      if (is_num_prec_satisfied_in_common_level (el))
		continue;
	      temp =
		compute_numeric_fact_cost_old (inf_fact, &loc_n_cost,
					   action_level);
	      continue;		//LAZZA
	    }
	  if (is_fact_in_additive_effects (best_action, el)
	      || (is_fact_in_additive_effects_start (best_action, el)
		  && !is_fact_in_delete_effects (best_action, el)))
	    continue;
	  if (CHECK_FACT_POS (el, best_level + 1))
	    {
#ifdef __TEST__
	      if (DEBUG2)
		printf
		  ("\n %d  *********************** COMPUTE DG MAX COST ACT %s fact %s",
		   ++k, print_op_name_string (best_action, temp_name),
		   print_ft_name_string (el, temp_name));
#endif
	      inf_fact = CONVERT_FACT_TO_INFORM (el, best_level + 1);
	      if (fact_is_supported (el, best_level + 1))
		{
#ifdef __TEST__
		  printf ("\n Level %d Supported fact %s ", best_level + 1,
			  print_ft_name_string (el, temp_name));
#endif
		  continue;
		}
	      if (GpG.accurate_cost >= COMPUTE_DG_LIST_COST
		  && GET_BIT (Hvar.bit_vect_facts, el))
		{
#ifdef __TEST__
		  printf
		    ("\nFact %s already supported in COMPUTE_DG_LIST_COST, level %d",
		     print_ft_name_string (el, temp_name), best_level);
#endif
		  continue;
		}
	      temp =
		compute_max_fact_cost (inf_fact, &loc_n_cost, action_level);
	      if (GpG.accurate_cost == COMPUTE_DG_MAX_COST)
		{
		  if (n_cost->act_cost < loc_n_cost.act_cost
		      && loc_n_cost.act_cost < MAX_COST)
		    n_cost->act_cost = loc_n_cost.act_cost;
		  if (n_cost->act_time < loc_n_cost.act_time
		      && loc_n_cost.act_time < MAX_COST)
		    n_cost->act_time = loc_n_cost.act_time;
#ifdef __TEST__
		  if (DEBUG2)
		    {
		      if (best_act_type == IS_ACTION)
			printf
			  ("\n %d *********************** END COMPUTE DG MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f  act_duration %.2f",
			   k, print_op_name_string (best_action, temp_name),
			   print_ft_name_string (el, temp_name), prec,
			   loc_n_cost.weight, loc_n_cost.act_cost,
			   loc_n_cost.act_time, get_action_time (best_action,
								 best_level));
		      else
			printf
			  ("\n %d +++++++++++++ END COMPUTE DG MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f",
			   k, print_noop_name_string (best_act_type,
						      temp_name),
			   print_ft_name_string (el, temp_name), prec,
			   loc_n_cost.weight, loc_n_cost.act_cost,
			   loc_n_cost.act_time);
		    }
#endif
		  if (prec < temp)
		    {
		      prec = temp;
		      n_cost->weight = loc_n_cost.weight;
#ifdef __TEST__
		      if (DEBUG2)
			{
			  if (best_act_type == IS_ACTION)
			    printf
			      ("\n %d  ***********************BEST  MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
			       k, print_op_name_string (best_action,
							temp_name),
			       print_ft_name_string (el, temp_name), prec,
			       n_cost->weight, n_cost->act_cost,
			       n_cost->act_time);
			  else
			    printf
			      ("\n %d BEST  MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
			       k, print_noop_name_string (best_action,
							  temp_name),
			       print_ft_name_string (el, temp_name), prec,
			       n_cost->weight, n_cost->act_cost,
			       n_cost->act_time);
			}
#endif
		      if (FALSE
			  && (temp *=
			      local_search.lamda_prec) >
			  local_search.best_cost)
			{
			  if (DEBUG2)
			    printf ("\n MAX_ACT_COST stop");
			  return temp;	// Ho superato il cost limite e quindi non ricerco ulteriormente 
			}
		    }
		}
	      else if (GpG.accurate_cost == COMPUTE_DG_SUM_COST)
		{
		  if (loc_n_cost.act_cost < MAX_COST)
		    n_cost->act_cost += loc_n_cost.act_cost;
		  if (loc_n_cost.act_time < MAX_COST)
		    n_cost->act_time += loc_n_cost.act_time;
#ifdef __TEST__
		  if (DEBUG2)
		    printf
		      ("\n %d  *********************** END COMPUTE DG ADD COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f  time  %.2f",
		       k, print_op_name_string (best_action, temp_name),
		       print_ft_name_string (el, temp_name), prec,
		       loc_n_cost.weight, loc_n_cost.act_cost,
		       loc_n_cost.act_time);
#endif
		  prec += temp;
		  n_cost->weight += loc_n_cost.weight;
#ifdef __TEST__
		  if (DEBUG2)
		    printf
		      ("\n %d BEST  ADD COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
		       k, print_op_name_string (best_action, temp_name),
		       print_ft_name_string (el, temp_name), prec,
		       n_cost->weight, n_cost->act_cost, n_cost->act_time);
#endif
		}
	      //LAZZA
	      else if (GpG.accurate_cost == COMPUTE_MAX_COST)
		{
		  if (n_cost->act_cost < loc_n_cost.act_cost
		      && loc_n_cost.act_cost < MAX_COST)
		    n_cost->act_cost = loc_n_cost.act_cost;

		  if (n_cost->act_time < loc_n_cost.act_time
		      && loc_n_cost.act_time < MAX_COST)
		    n_cost->act_time = loc_n_cost.act_time;

#ifdef __TEST__
		  if (DEBUG2)
		    {
		      if (best_act_type == IS_ACTION)
			printf
			  ("\n %d +++++++++++++ END COMPUTE MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f  act_duration %.2f",
			   k, print_op_name_string (best_action, temp_name),
			   print_ft_name_string (el, temp_name), prec,
			   loc_n_cost.weight, loc_n_cost.act_cost,
			   loc_n_cost.act_time, get_action_time (best_action,
								 best_level));
		      else
			printf
			  ("\n %d +++++++++++++ END COMPUTE MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f",
			   k, print_noop_name_string (best_act_type,
						      temp_name),
			   print_ft_name_string (el, temp_name), prec,
			   loc_n_cost.weight, loc_n_cost.act_cost,
			   loc_n_cost.act_time);
		    }
#endif

		  if (prec < temp)
		    {
		      prec = temp;
		      n_cost->weight = loc_n_cost.weight;


#ifdef __TEST__
		      if (DEBUG2)
			{
			  if (best_act_type == IS_ACTION)
			    printf
			      ("\n %d BEST  MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
			       k, print_op_name_string (best_action,
							temp_name),
			       print_ft_name_string (el, temp_name), prec,
			       n_cost->weight, n_cost->act_cost,
			       n_cost->act_time);
			  else
			    printf
			      ("\n %d BEST  MAX COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
			       k, print_noop_name_string (best_action,
							  temp_name),
			       print_ft_name_string (el, temp_name), prec,
			       n_cost->weight, n_cost->act_cost,
			       n_cost->act_time);
			}
#endif



		      if (FALSE
			  && (temp *=
			      local_search.lamda_prec) >
			  local_search.best_cost)
			{

			  if (DEBUG2)
			    printf ("\n MAX_ACT_COST stop");
			  return temp;	// Ho superato il cost limite e quindi non ricerco ulteriormente 
			}

		    }
		}
	      else if (GpG.accurate_cost == COMPUTE_ADD_COST)
		{
		  if (loc_n_cost.act_cost < MAX_COST)
		    n_cost->act_cost += loc_n_cost.act_cost;
		  if (loc_n_cost.act_time < MAX_COST)
		    n_cost->act_time += loc_n_cost.act_time;


#ifdef __TEST__
		  if (DEBUG2)
		    printf
		      ("\n %d +++++++++++++ END COMPUTE ADD COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f  time  %.2f",
		       k, print_op_name_string (best_action, temp_name),
		       print_ft_name_string (el, temp_name), prec,
		       loc_n_cost.weight, loc_n_cost.act_cost,
		       loc_n_cost.act_time);
#endif
		  prec += temp;
		  n_cost->weight += loc_n_cost.weight;


#ifdef __TEST__
		  if (DEBUG2)
		    printf
		      ("\n %d BEST  ADD COST ACT %s fact %s cost %.2f weight %.2f act_cost %.2f act_time %.2f ",
		       k, print_op_name_string (best_action, temp_name),
		       print_ft_name_string (el, temp_name), prec,
		       n_cost->weight, n_cost->act_cost, n_cost->act_time);

#endif
		}
	      //ENDLAZZA

	    }
#ifdef __TEST__
	  else
	    printf
	      ("\n ERRORE 1002  Fatto %s non presente al corrispondente livello %d, first level %d",
	       print_ft_name_string (el, temp_name), level,
	       CONVERT_FACT_TO_VERTEX (el)->level);
#endif
	}
    }
  //***************************************************************************************** saetti end
  // Inserisco temporaneamente l'azione migliore, considero il costo per renderla vera quando le sue precondizioni sono ormai supportate 
  if (best_act_type != IS_ACTION)
    {
      total = 0.0;
      // GPG3 XXXX mutex=0.0; 
      mutex = count_mutex_noop_at_start (best_action, best_level);
    }
  else
    {
      total = 1.0;		// Utilizzare invece il costo delee azioni ??? 
      n_cost->act_cost += get_action_cost (best_action);
      n_cost->act_time += get_action_time (best_action, best_level);	// get_action_time(best_action, best_level); 
      mutex = count_mutex_noop_at_start (best_action, best_level);	// SISTEMARE gestione mutex con weight
      Hvar.weight_facts_define_cost += mutex;
      total += mutex;
    }
  n_cost->weight = total;
  /*
     if(DEBUG2) 
     printf("\n\n   *********************** >>>>>>>>>Comp_numeric_fact_cost  END %s  time %d pos %d total %f PREC %.2f ME %.2f act_cost %.2f act_time %.2f -- Hvar.weight_facts_define_cost %.2f", print_cvar_tree(-tofix->position),  level,  tofix->position, total, prec, mutex,n_cost->act_cost , n_cost->act_time, Hvar.weight_facts_define_cost); 
   */
  // Setto effetti di best action 
  if (GpG.accurate_cost >= COMPUTE_DG_LIST_COST)
    {				//  insert_action_inlist( best_action );
      for (j = 0; j < gef_conn[best_action].num_A; j++)
	{
	  el = gef_conn[best_action].A[j];
	  //effetto numerico: applico i suoi effetti su common_vectlevel 
	  if (el < 0)
	    {
	      apply_numeric_effect_in_common_level (el);
	    }
	  SET_BIT (Hvar.bit_vect_facts, el);
	}
      //LAZZA
      //stesso lavoro per gli effetti at start
      if (gef_conn[best_action].sf != NULL)
	for (j = 0; j < gef_conn[best_action].sf->num_A_start; j++)
	  {
	    el = gef_conn[best_action].sf->A_start[j];
	    //eff numerico: applico i suoi effetti su common_vectlevel 
	    if (el < 0)
	      {
		apply_numeric_effect_in_common_level (el);
	      }
	    SET_BIT (Hvar.bit_vect_facts, el);
	  }
      //ENDLAZZA
    }
  for (j = best_level + 1; j <= level; j++)
    set_numeric_fact_cost (CONVERT_FACT_TO_INFORM (Fact->position, j),
			   n_cost);
  return total;


}





void
apply_numeric_effects_at_start_of_action (int act_pos, int level)
{
  int i;
  DescNumEff *numeric_effs;
  int num_numeric_effs;

  //preliminari
  numeric_effs = gef_conn[act_pos].numeric_effects;
  num_numeric_effs = gef_conn[act_pos].num_numeric_effects;
  //0. Aggiorna il bit array used_vars del livello  
  set_usedvars_array (act_pos, level);
  //1. Per ciascuno degli effetti numerici at start dell'azione act_pos    
  if (numeric_effs)
    for (i = 0; i < num_numeric_effs; i++)
      if (numeric_effs[i].is_at_start)
	apply_numeric_effect_at_start (act_pos, level, i);
  //2. ricalcola le grandezze del livello level+1 che dipendono da grandezze che hanno subito modifiche
  refresh_cvars (level + 1);
  //richiama la propagazione dei valori da variare da questo livello in poi
  propagate_cvars (level, GpG.curr_plan_length);
}

void
apply_numeric_effects_at_end_of_action (int act_pos, int level)
{
  int i;
  DescNumEff *numeric_effs;
  int num_numeric_effs;

  //preliminari
  numeric_effs = gef_conn[act_pos].numeric_effects;
  num_numeric_effs = gef_conn[act_pos].num_numeric_effects;
  //0. Aggiorna il bit array used_vars del livello  
  set_usedvars_array (act_pos, level);
  //1. Per ciascuno degli effetti numerici at end dell'azione act_pos    
  if (numeric_effs)
    for (i = 0; i < num_numeric_effs; i++)
      if (!numeric_effs[i].is_at_start)
	apply_numeric_effect_at_end (act_pos, level, i);
  //2. ricalcola le grandezze del livello level+1 che dipendono da grandezze che hanno subito modifiche
  refresh_cvars (level + 1);
  //richiama la propagazione dei valori da variare da questo livello in poi
  propagate_cvars (level, GpG.curr_plan_length);
}



//verifica le precondizioni numeriche di una azione
Bool
verify_num_preconds_start (int act_pos, int level)
{
  int i;

  for (i = 0; i < gef_conn[act_pos].num_PC; i++)
    if (gef_conn[act_pos].PC[i] < 0)
      if (!is_num_prec_satisfied (-gef_conn[act_pos].PC[i], level))
	return FALSE;
  //solo se tutte le condizioni sono verificate allora ritorna true
  return TRUE;
}

Bool
verify_num_preconds_end (int act_pos, int level)
{
  int i;

  for (i = 0; i < gef_conn[act_pos].sf->num_PC_end; i++)
    if (gef_conn[act_pos].sf->PC_end[i] < 0)
      if (!is_num_prec_satisfied (-gef_conn[act_pos].sf->PC_end[i], level))
	return FALSE;
  //solo se tutte le condizioni sono verificate allora ritorna true
  return TRUE;
}

Bool
verify_num_preconds_overall (int act_pos, int level)
{
  int i;

  for (i = 0; i < gef_conn[act_pos].sf->num_PC_overall; i++)
    if (gef_conn[act_pos].sf->PC_overall[i] < 0)
      if (!is_num_prec_satisfied
	  (-gef_conn[act_pos].sf->PC_overall[i], level))
	return FALSE;
  //solo se tutte le condizioni sono verificate allora ritorna true
  return TRUE;
}

//LAZZA 24042002
void
call_adj ()			//char* fact_file_name)
{
  char adj_command[MAX_LENGTH];
  char cmd[MAX_LENGTH];
  char sol_file_name[MAX_LENGTH];
  int num_plan, timeout, out;
  float cpu_time;
  struct tms end_time;
  strcpy (adj_command, glpg_path);
  strcat (adj_command, "../ADAPT/adj");
  strcpy (sol_file_name, "sol_adj.sol");
  timeout = 900;
  num_plan = 0;

  //genera il file
  lazza_store_plan (&GpG.temp_plan_actions, sol_file_name);
  times (&end_time);
  cpu_time =
    (float) ((end_time.tms_utime - glob_start_time.tms_utime +
	      end_time.tms_stime - glob_start_time.tms_stime) / 100.0);
  //chiamata a adj
  strcpy (cmd, "");
  sprintf (cmd, "%s -o %s -f %s -P %s -T %d -Z %f &\0", adj_command,
	   gops_file, gfct_file, sol_file_name, timeout, cpu_time);
  out = system (cmd);
  exit (0);
}



//ENDLAZZA 24042002






/*******************************
*           output.c       *
********************************/



void
print_file_op_name (FILE * outfile, int index, int level)
{

  int i;
  Action *a = gop_conn[index].action;

  if (a->norm_operator || a->pseudo_action)
    {
      fprintf (outfile, "%d: (%s", level, a->name);
      for (i = 0; i < a->num_name_vars; i++)
	{
	  fprintf (outfile, " %s", gconstants[a->name_inst_table[i]]);
	}
    }
  fprintf (outfile, ")");
}



void
print_plan (void)
{

  int i;
  FILE *OutFile = NULL;

  printf ("\n\nff: found legal plan as follows");
  printf ("\n\nstep ");
  for (i = 0; i < gnum_plan_ops; i++)
    {
      printf ("%4d: ", i);
      print_op_name (gplan_ops[i]);
      printf ("\n     ");
    }
  if (strlen (gcmd_line.out_file_name) > 1)
    {
      OutFile = fopen (gcmd_line.out_file_name, "w");
      if (OutFile == NULL)
	{
	  printf ("\nCan't open file %s for writing\n",
		  gcmd_line.out_file_name);
	  exit (2);
	}
      for (i = 0; i < gnum_plan_ops; i++)
	{
	  print_file_op_name (OutFile, gplan_ops[i], i);
	}
    }
  if (strlen (gcmd_line.out_file_name) > 1)
    fclose (OutFile);
}




void
print_noop_vect (int level)
{
  int i, j, k, temp;


  /* Stampa le noop inserite */
  printf ("\n\n NOOP  %d ", level);

  for (i = 0, j = 0; i <= GpG.max_num_ft_block; i++, j += 32)
    {
      temp = vectlevel[level]->noop_act_vect[i];
      k = 32;

      while (temp)
	{
	  k--;
	  if (temp & FIRST_1)
	    printf (" -  %s ", print_noop_name_string (j + k, temp_name));
	  temp <<= 1;
	}
    }

  /* Stampa le noop precondizione */
  printf ("\n\n PREC NOOP %d ", level);

  for (i = 0, j = 0; i <= GpG.max_num_ft_block; i++, j += 32)
    {
      temp = vectlevel[level]->noop_prec_act_vect[i];
      k = 32;

      while (temp)
	{
	  k--;
	  if (temp & FIRST_1)
	    printf (" -  %s ", print_noop_name_string (j + k, temp_name));
	  temp <<= 1;
	}
    }


}




/* Store adapted plan on file */
void
ivan_store_adapted_plan (int levels, char *fact_file_name_string, double time)
{
  int i, curr_plan_length = 0, num_actions = 0;
  char cNameFile[256];
  static int num_plan = 0;
  FILE *fp;
  PlanAction *ptr_act;



  curr_plan_length = 0;
  num_actions = 0;
  num_plan++;

  //  sprintf(cNameFile,"plan_%s_%d.SOL\0",fact_file_name_string,num_plan); 
  sprintf (cNameFile, "plan_%s_%d.SOL", fact_file_name_string, num_plan);
  if ((fp = fopen (cNameFile, "w")) == NULL)
    {
      MSG_ERROR (" cannot open file ! \n\n");
      exit (1);
    }
  fprintf (fp,
	   ";;;Problem:\t%s\t time:\t%f\t levels:\t%d total cost \t %f duration %.2f\n",
	   fact_file_name_string, time, levels, GpG.best_cost, GpG.best_time);


  for (ptr_act = GpG.gplan_actions, i = 0; ptr_act;
       ptr_act = ptr_act->next, i++)
    {
      print_file_op_name (fp, ptr_act->act_pos, (int) ptr_act->start_time);
      fprintf (fp, " ;;   cost %.2f duration %.2f \n", get_action_cost (ptr_act->act_pos), get_action_time (ptr_act->act_pos, 0));	// IVAN: Cercare il livello associato per la get_action_time

    }


  fclose (fp);

  GpG.curr_plan_length = curr_plan_length;
  GpG.num_actions = num_actions;
}




void
lazza_store_plan (PlanAction ** plan_actions, char *plan_file)
{
  PlanAction *p;
  int curr_plan_length = 0, num_actions = 0;
  char cNameFile[256];
  static int num_plan = 100;
  FILE *fp;

  curr_plan_length = 0;

  num_actions = 0;
  num_plan++;

  sprintf (cNameFile, "%s", plan_file);
  if ((fp = fopen (cNameFile, "w")) == NULL)
    {
      MSG_ERROR (" cannot open file ! \n\n");
      exit (1);
    }

  for (p = *plan_actions; p; p = p->next)
    {
      fprintf (fp, "\n");
      //      fprintf(fp,"%5.2f: ",p->start_time+0.01);
      fprintf (fp, "%3d: ", (int) (p->start_time + 0.01));
      print_file_action_name_underscores (fp, p->act_pos);
      //fprintf(fp,"\t\t[%5.2f ]",p->duration);
      //LAZZA
      //      if(!vectlevel[plan_level])
      //        break;
      /*
         while(vectlevel[plan_level]->action.position==-1)
         {
         printf("\nLivello %3d vuoto",plan_level);
         plan_level++;
         if(!vectlevel[plan_level])
         break;     
         }
         printf("\n-----------------------------------------------------------\nVALORI NUMERICI al liv. %d:\n",plan_level);
         for(k=0;k<gnum_comp_var;k++)
         {
         printf("%3d:%8.2f",k,vectlevel[plan_level]->numeric->values[k]);
         printf("%c\t",GET_BIT(gis_inertial,k)?' ':'*');
         }
         check_consistency(plan_level);

         printf("\n\n");
         printf("%5.2f: ",p->start_time+0.01);
         print_op_name(p->act_pos  );
         printf("\t\t[%5.2f ]",p->duration);
         plan_level++;
         //ENDLAZZA
         }
         if(!vectlevel[plan_level])
         return;
         fprintf(fp,"\n\n");
         printf("\n-----------------------------------------------------------\nVALORI NUMERICI al liv. %d:\n",plan_level);
         for(k=0;k<gnum_comp_var;k++)
         {
         printf("%3d:%8.2f",k,vectlevel[plan_level]->numeric->values[k]);
         printf("%c\t",GET_BIT(gis_inertial,k)?' ':'*');
         }
         check_consistency(plan_level);
         printf("\n\n");
         //ENDLAZZA
       */
    }
}





void
print_file_action_name_underscores (FILE * outfile, int index)
{

  int i;
  Action *a = gop_conn[index].action;

  if (!a->norm_operator && !a->pseudo_action)
    {
      /*    fprintf(outfile,""); */
    }
  else
    {
      fprintf (outfile, " %s", a->name);
      for (i = 0; i < a->num_name_vars; i++)
	{
	  fprintf (outfile, "_%s", gconstants[a->name_inst_table[i]]);
	}
    }
  // fprintf(outfile,")");
}







/*******************************
*           time.c       *
********************************/


/**
 *
 **/
void insert_initial_timed_actions( void ) {

  int i, j, fct, k, indlevel, lastlevel = 0, choice;
  neighb neighb;
  constraints constr;
  float cost, act_cost, slack;

 return;

  printf("\nINSERT TIMED ACTIONS : ");

  // Per ogni timed fact
  for (i = 0; i < gnum_timed_facts; i++) {
    fct = gtimed_fct_vect[i][0].position;
    // Per ogni intervallo
    for (j = 0; j < NUM_INTERVALS(fct); j++) {

      printf("\nEvaluate timed fact : %s interval %d", print_ft_name_string(fct, temp_name), j);
      
      slack = gtimed_fct_vect[i][j].duration;

      while (slack > 0.0) { 

	reset_neighborhood();
	cost = MAXFLOAT;
	choice = -1;

	// Per ogni azione che ha il timed nelle precondizioni
	for (k = 0; k < gft_conn[fct].num_PC; k++) {
	  indlevel = MAX(lastlevel, gef_conn[gft_conn[fct].PC[k]].level);
	  if (get_action_time(gft_conn[fct].PC[k], indlevel) <= slack) {
	    neighb.act_pos = gft_conn[fct].PC[k];
	    neighb.act_level = MAX(indlevel, gef_conn[neighb.act_pos].level);
	    neighb.constraint_type = C_T_INSERT_ACTION;
	    insert_element_in_neighb(&neighb);

	    printf("\nNew action in neighbors : %s", print_op_name_string(neighb.act_pos, temp_name));
	  }
	}

	// Evaluate neighbors
	for (k = 0; k < num_neighborhood; k++) {
	  
	  /****/
	  constr.action = -1;
	  constr.level = &vectlevel[neighb_vect[k] -> act_level + 1] -> level;
	  constr.fact = gef_conn[neighb_vect[k] -> act_pos].A[0];
	  constr.constraint_type = C_T_UNSUP_FACT;
	  Hvar.constr = &constr;
	  /****/

	  act_cost = action_cost(neighb_vect[k]);
	  if ((act_cost <= cost) || ((act_cost == cost) && (neighb_vect[k] -> act_level < neighb_vect[choice] -> act_level))) {
	    choice = k;
	    cost = act_cost;
	  }
	}
	
	// Insert action
	if (choice >= 0) {
	  printf("\n\nAction %s inserted in level %d", print_op_name_string(neighb_vect[choice] -> act_pos, temp_name), neighb_vect[choice] -> act_level);
	  insert_remove_action(neighb_vect[choice] -> act_pos, neighb_vect[choice] -> act_level, C_T_INSERT_ACTION, GpG.approximation_level);
	  lastlevel =  neighb_vect[choice] -> act_level;
	  slack -= get_action_time(neighb_vect[choice] -> act_pos, neighb_vect[choice] -> act_level);
	}
	// Exit while
	else break;
    
      }

    }

  }

  //exit(0);

}


/* Ritorna la posizione del goal supportato con istante maggiore */

int
get_max_time_goal ()
{

  int i;
  inform_list infGoal;
  float max_time;
  int pos = 0;


  max_time = 0.0;

  /* Per ogni goal supportato... */
  for (i = 0; i < GpG.curr_goal_state->num_F; i++)
    {
      infGoal =
	CONVERT_FACT_TO_INFORM (GpG.curr_goal_state->F[i],
				GpG.curr_plan_length);

      if (infGoal->w_is_true)
	{
	  /* Se l'istante a cui e' supportato il goal e' maggiore della durata del piano allora la durata e' uguale all'istante del goal */
	  if (infGoal->time_f > max_time)
	    {
	      pos = infGoal->position;
	      max_time = infGoal->time_f;
	    }
	}

#ifdef __TEST__
      else if (GpG.num_false_tot == 0)
	printf ("\nERRORE: nei GOAL");
#endif

    }
  GpG.total_time = max_time;

  return (pos);

}




/* Stabilisci vincoli di ordinamento. E' richiamata da insert_time se e' settato GpG.lowmemory */

void
temporal_constraints_trans_clos (inform_list infAction)
{

  int posAction;
  int level, ind_level;
  int posB;


  level = *infAction->level;

  /* Cerchiamo la prima posizione libera nella matrice degli ordinamenti e nel vettore di corrispondenza */
  for (posAction = 0; posAction < num_act_ord; posAction++)
    if (act_ord_vect[posAction] == NULL)
      break;

  if (posAction == num_act_ord)
    num_act_ord++;
  
  
  if (num_act_ord >= MAX_NUM_ACT) {
#ifdef __MY_OUTPUT__
    MSG_ERROR( WAR_MAX_NUM_ACT );
#else
    printf( WAR_MAX_NUM_ACT );
#endif    
    exit (1);
  } 

  
  infAction->ord_pos = posAction;
  act_ord_vect[posAction] = infAction;

  /* Livelli successivi, per propagazione in avanti */
  for (ind_level = level + 1; ind_level < GpG.curr_plan_length; ind_level++)
    {
      posB = vectlevel[ind_level]->action.ord_pos;

      /* Se l'azione del livello e' mutex con l'azione che deve essere inserita nella matrice allora viene introdotto l'ordinamento */
      if (vectlevel[ind_level]->action.w_is_used > 0
	  && check_mutex_action (infAction->position, ind_level) >= 0)
	{

	  /* Se non era presente nessun ordinamento allora e' un ordinamento di tipo 1 ( diretto ) */
	  if (mat_ord[posAction][posB] == 0)
	    {
	      mat_ord[posAction][posB] = 1;
	      forward_transitive_closure (posAction, posB);
	    }

	  /* Se era presente un ordinamento indiretto allora e' un ordinamento di tipo 3 ( diretto + indiretto ) */
	  if (mat_ord[posAction][posB] == 2)
	    mat_ord[posAction][posB] = 3;
	}
    }


  /* Livelli precedente per propagazione all'indietro */
  for (ind_level = level - 1; ind_level >= 0; ind_level--)
    {
      posB = vectlevel[ind_level]->action.ord_pos;

      /* Se l'azione del livello e' mutex con l'azione che deve essere inserita nella matrice allora viene introdotto l'ordinamento */
      if (check_mutex_action (infAction->position, ind_level) >= 0)
	{

	  if (vectlevel[ind_level]->action.w_is_used > 0)
	    {

	      /* Se non era presente nessun ordinamento allora e' un ordinamento di tipo 1 ( diretto ) */
	      if (mat_ord[posB][posAction] == 0)
		{
		  mat_ord[posB][posAction] = 1;
		  backward_transitive_closure (posAction, posB);
		}

	      /* Se era presente un ordinamento indiretto allora e' un ordinamento di tipo 3 ( diretto + indiretto ) */
	      if (mat_ord[posB][posAction] == 2)
		mat_ord[posB][posAction] = 3;
	    }

	  /* L'azione del livello non e' inserita */
#ifdef __TEST__

	  else if (DEBUG2)
	    printf ("Errore 2 nella gestione del tempo!!!");
#endif

	}
    }

#ifdef TEST_GR
  int ind, indx, indy;
  for (ind = 0; ind < num_act_ord; ind++)
    if (act_ord_vect[ind] != NULL)
      printf ("%d  ", *act_ord_vect[ind]->level);
    else
      printf ("   ");

  for (indx = 0; indx < num_act_ord; indx++)
    {
      printf ("\n");
      for (indy = 0; indy < num_act_ord; indy++)
	printf ("%d  ", mat_ord[indx][indy]);
    }

#endif

}







/* Rimuovi vincoli di ordinamento. E' richiamata da update_time se e' settato GpG.lowmemory */

void
remove_temporal_constraints_trans_clos (int posAction)
{

  int i, j;
  BOOLEAN recompute;

#ifdef __TEST__
  int ind, indx, indy;
#endif


  recompute = FALSE;
  /* Per tutti gli elementi della matrice... */
  for (j = 0; j < num_act_ord; j++)
    for (i = 0; i < num_act_ord; i++)

      /* Se e' presente un vincolo di ordinamento indiretto causato dall'azione che deve essere rimossa valutiamo il nuovo vincolo tra le due azioni */
      if (mat_ord[i][j] == 2 || mat_ord[i][j] == 3)
	if (mat_ord[i][posAction] != 0 && mat_ord[posAction][j] != 0)
	  {
	    /* Se esistono casi di questo tipo applichiamo l'algoritmo Floyd - Warshall al termine della scansione della matrice */
	    recompute = TRUE;

	    /* Se il vincolo era di tipo 3 ( indiretto + diretto ) allora diventa diretto */
	    if (mat_ord[i][j] == 3)
	      mat_ord[i][j] = 1;

	    /* Se il vincolo era diretto allora vediamo se esiste un vincolo diretto */
	    if (mat_ord[i][j] == 2)
	      {
		if (ARE_MUTEX_EF
		    (act_ord_vect[i]->position, act_ord_vect[j]->position))
		  mat_ord[i][j] = 1;
		else
		  mat_ord[i][j] = 0;
	      }
	  }

  /* Azzeriamo la riga e la colonna della matrice */
  for (i = 0; i < num_act_ord; i++)
    {
      mat_ord[i][posAction] = 0;
      mat_ord[posAction][i] = 0;
    }

  act_ord_vect[posAction] = NULL;

  if (posAction == (num_act_ord - 1))
    num_act_ord--;

  if (recompute)
    warshall_transitive_closure ();


#ifdef __TEST__

  for (ind = 0; ind < num_act_ord; ind++)
    if (act_ord_vect[ind] != NULL)
      printf ("%d  ", *act_ord_vect[ind]->level);
    else
      printf ("   ");

  for (indx = 0; indx < num_act_ord; indx++)
    {
      printf ("\n");
      for (indy = 0; indy < num_act_ord; indy++)
	printf ("%d  ", mat_ord[indx][indy]);
    }

#endif

}






/* Usata per correggere i vincoli di ordinamento dopo la cancellazione di un' azione: algoritmo di Floyd - Warshall */

void
warshall_transitive_closure ()
{
  register int i, j, k;

  /* Per ogni azione inserita nella matrice... */
  for (i = 0; i < num_act_ord; i++)
    if (act_ord_vect[i] != NULL)
      for (j = 0; j < num_act_ord; j++)

	if (act_ord_vect[j] != NULL)
	  {

	    /* Se l'azione i precede l'azione j allora tutte le azioni che seguono j devono seguire i */
	    if (mat_ord[i][j] != 0)

	      for (k = 0; k < num_act_ord; k++)
		{

		  /* Se i non ha vincoli con k allora e' un ordinamento di tipo 2 ( indiretto ) */
		  if (mat_ord[i][k] == 0 && mat_ord[j][k] != 0)
		    mat_ord[i][k] = 2;

		  /* Se i precede l'azione k in modo diretto allora e' un ordinamento di tipo 3 ( diretto + indiretto ) */
		  if (mat_ord[i][k] == 1 && mat_ord[j][k] != 0)
		    mat_ord[i][k] = 3;
		}
	  }
}







/* Chiusura transitiva in avanti: l'azione A precede l'azione B. Tutte le azioni ordinate dopo B devono essere ordinate anche dopo A. */

void
forward_transitive_closure (int posA, int posB)
{

  int i;


  for (i = 0; i < num_act_ord; i++)
    {

      if (TRUE || act_ord_vect[i] != NULL)
	{
	  /* Se A non ha vincoli con l'azione i-esima allora e' un ordinamento di tipo 2 ( indiretto ) */
	  if (mat_ord[posA][i] == 0 && mat_ord[posB][i] != 0)
	    mat_ord[posA][i] = 2;

	  /* Se l'azione A precede l'azione i-esima in modo diretto allora e' un ordinamento di tipo 3 ( diretto + indiretto ) */
	  if (mat_ord[posA][i] == 1 && mat_ord[posB][i] != 0)
	    mat_ord[posA][i] = 3;
	}
    }

}







/*  Chiusura transitiva all'indietro: l'azione B precede l'azione A. Tutte le azioni ordinate prima di B devono essere ordinate prima di A.
    Tutte le azioni che seguono A devono seguire tutte le azioni che precedono B */

void
backward_transitive_closure (int posA, int posB)
{
  int back_vect[MAX_NUM_ACT];
  int ini, fin, k;


  /* Vettore delle azioni che precedono A */
  back_vect[0] = posB;

  /* Per tutte le azioni che precedono A... */
  for (ini = 0, fin = 1; ini < fin; ini++)
    {
      //tutte le azioni che precedono B devono precedere A
      for (k = 0; k < num_act_ord; k++)
	if (TRUE || act_ord_vect[k] != NULL)
	  {
	    /* L'azione k precede l'azione di back_vect esaminata */

	    /* Se non c'era nessn vincolo tra k e A allora esiste un vincolo di tipo 2 ( indiretto ) */
	    if (mat_ord[k][posA] == 0 && mat_ord[k][back_vect[ini]] != 0)
	      {
		mat_ord[k][posA] = 2;
		back_vect[fin++] = k;
	      }

	    /* Se c'era un vincolo diretto tra k e A allora esiste un vincolo di tipo 3 ( diretto + indiretto ) */
	    if (mat_ord[k][posA] == 1 && mat_ord[k][back_vect[ini]] != 0)
	      {
		mat_ord[k][back_vect[ini]] = 3;
		back_vect[fin++] = k;
	      }
	  }

      //tutte le azioni che seguono A devono seguire B
      for (k = 0; k < num_act_ord; k++)
	{

	  if (TRUE || act_ord_vect[k] != NULL)
	    {
	      /* Se il primo elemento di back_vect non ha vincoli con k allora e' un ordinamento di tipo 2 ( indiretto ) */
	      if (mat_ord[back_vect[ini]][k] == 0 && mat_ord[posA][k] != 0)
		mat_ord[back_vect[ini]][k] = 2;

	      /* Se il primo elemento di back_vect ha un vincolo diretto con k allora e' un ordinamento di tipo 3 ( diretto + indiretto ) */
	      if (mat_ord[back_vect[ini]][k] == 1 && mat_ord[posA][k] != 0)
		mat_ord[back_vect[ini]][k] = 3;
	    }
	}
    }

}





/***************************
*         utilities.c      *
****************************/



/* Store adapted plan on file */

void
store_plan_using_bestfirst (char *fact_file_name)
{

  int i, curr_plan_length = 0, out;
  char cNameFile[256];
  char validate_string[MAX_LENGTH];
  FILE *fp;
  float temp_time;

  curr_plan_length = 0;

  GpG.total_cost = 0.0;
  GpG.num_actions = gnum_plan_ops;
  temp_time = 0.0;


  sprintf (cNameFile, "plan_bestfirst_%s.SOL", fact_file_name);

  if ((fp = fopen (cNameFile, "w")) == NULL)
    {
      MSG_ERROR (WAR_OPEN_FILE);
      exit (1);
    }

  fprintf (fp, "\n;; Version %s", VERSION);
  fprintf (fp, "\n;; Seed %d", seed);
  fprintf (fp, "\n;; Command line: %s", gcomm_line);
  fprintf (fp, "\n\n;;Problem:\t%s\t actions:\t%d\t \n", fact_file_name,
	   gnum_plan_ops);
  fprintf (fp, ";;Parsing time: %f\n\n",
	   gtempl_time + greach_time + grelev_time + gconn_time + gnum_time);




  for (i = 0; i < gnum_plan_ops; i++)
    {

      fprintf (fp, "\t %.3f:  ", temp_time + MIN_DELTA_TIME * i);
      print_file_action_name (fp, gplan_ops[i]);
      fprintf (fp, "[%.3f] ;; cost %.3f \n",
	       get_action_time (gplan_ops[i], i),
	       get_action_cost (gplan_ops[i]));

      temp_time += get_action_time (gplan_ops[i], i);
      GpG.total_cost += get_action_cost (gplan_ops[i]);
    }
  GpG.total_time = temp_time;




  fprintf (fp, "\nTime %d\n\n", (int) (gtotal_time * 1000.0 + 0.5));
  fclose (fp);

  if (GpG.validate)
    {
      strcpy (validate_string, VALIDATOR);
      strcat (validate_string, gops_file);
      strcat (validate_string, " ");
      strcat (validate_string, gfct_file);
      strcat (validate_string, " ");
      strcat (validate_string, cNameFile);
      printf ("\n\n%s\n", validate_string);
      fflush (stdout);
      out = system (validate_string);
    }

}









/***********************************
          CRITICAL PATH
************************************/

void is_action_on_critical_path_of_DummyAction (int act_pos, int act_level, float act_time)
{

  int i,ordering;
  float delta_time;
  inform_list TempAct;

  for(i=0; i < gnum_tmd_init_fcts; i++)
    {

      DummyAction = GET_ACTION_OF_LEVEL(gtimed_facts[i]->level);

      delta_time = gtimed_facts[i].inctime;

      for(TempAct=DummyAction; TempAct!=NULL; TempAct=DummyAction->action_f)
	{
      
	  if(act_level > *TempAct->level)
	    break;
      
	  ordering = constraint_type(act_pos, act_level, TempAct->position, *TempAct->level);
      
	  if (ordering == EA_SB)
	    {
	      if(delta_time < act_time - (TempAct->time_f - get_action_time(TempAct->position, *TempAct->level)))
		delta_time = act_time - (TempAct->time_f - get_action_time(TempAct->position, *TempAct->level));
	      
	    }
	  else
	    if (ordering == EA_EB) 
	      {
		if(delta_time < act_time - TempAct->time_f)
		  delta_time = act_time - TempAct->time_f;
	      }
	    else
	      if (ordering == SA_SB) 
		{
		  if(delta_time  < act_time - get_action_time (best_action, best_level) - (TempAct->time_f - get_action_time(TempAct->position, *TempAct->level)) )
		    delta_time = act_time - get_action_time (best_action, best_level) - (TempAct->time_f - get_action_time(TempAct->position, *TempAct->level));		    
		}
	      else
		if (ordering == SA_EB)
		  {
		    if(delta_time < (act_time - get_action_time (best_action, best_level)) - TempAct->time_f)
		      delta_time = act_time - get_action_time (best_action, best_level) - TempAct->time_f;
		  }
	  
	}

      gtimed_facts[i].inctime = delta_time;

    }

}







int compute_search_cost_for_timed_facts (int posAction, int levAction, float timeAction)
{
  int i;
  float slack;

  slack = gtimed_facts[i]->time - vectlevel[gtimed_facts[i]->level]->action.time_f;

  /* per tutte le azioni fittizie */
  for(i=0; i < gnum_tmd_init_fcts; i++) 
    {
      /* se non puo' esserci peggiornamento */
      if (slack > 0)
	if ( get_action_time(infAction->position, *infAction->level) < slack )
	  continue;

      delta_Time = is_action_on_critical_path(infAction, vectlevel[gtimed_facts[i]->level]->action);
      
      if(delta_Time > slack)
	return(ceil((deltaTime - slack) / mindurationxxxx  ))

    }
}





void insert_plan_in_vectlevel (PlanAction * plan)
{
  PlanAction *act;
  int i, j;

  if (!plan)
    return;
  j = 0;
#ifdef __TEST__
  printf ("\nVALORI NUMERICI al liv. %d:\n", j);
  for (k = 0; k < gnum_comp_var; k++)
    {
      printf ("%3d:%8.2f", k, vectlevel[j]->numeric->values[k]);
      printf ("%c\t", GET_BIT (gis_inertial, k) ? ' ' : '*');
    }
#endif

  check_consistency (j);

  for (act = plan, i = 0; act; act = act->next, i++)
    {


#ifdef __TEST__
      printf("\n----------------------------------------------------------\n");
#endif

      insert_remove_action (act->act_pos, i, C_T_INSERT_ACTION,GpG.approximation_level);

      for (j = 0; j <= i + 1; j++)
	{
#ifdef __TEST__
	  printf ("\nVALORI NUMERICI al liv. %d:\n", j);
	  for (k = 0; k < gnum_comp_var; k++)
	    {
	      printf ("%3d:%8.2f", k, vectlevel[j]->numeric->values[k]);
	      printf ("%c\t", GET_BIT (gis_inertial, k) ? ' ' : '*');
	    }
#endif

	  check_consistency (j);

#ifdef __TEST__
	  if (j != i + 1)
	    {
	      printf ("\n\nazione: ");
	      if (vectlevel[j]->action.position >= 0)
		print_op_name (vectlevel[j]->action.position);

	      printf ("\n");
	    }
#endif

	}
    }


#ifdef __TEST__
  printf ("\n--------------------------------\n-----------------------------");
  printf ("\nRIMOZIONE\n-----------------------\n------------------");
#endif

  for (act = plan, i = 0; act; act = act->next, i++)
    {

#ifdef __TEST__
      printf("\n----------------------------------------------------------\n");
#endif

      insert_remove_action (act->act_pos, i, C_T_REMOVE_ACTION,GpG.approximation_level);

      for (j = 0; j <= GpG.curr_plan_length; j++)
	{

#ifdef __TEST__
	  printf ("\nVALORI NUMERICI al liv. %d:\n", j);
	  for (k = 0; k < gnum_comp_var; k++)
	    {
	      printf ("%3d:%8.2f", k, vectlevel[j]->numeric->values[k]);
	      printf ("%c\t", GET_BIT (gis_inertial, k) ? ' ' : '*');
	    }
#endif
	  check_consistency (j);


#ifdef __TEST__
	  if (j != GpG.curr_plan_length)
	    {
	      printf ("\n\nazione: ");
	      if (vectlevel[j]->action.position >= 0)
		print_op_name (vectlevel[j]->action.position);
	      printf ("\n");
	    }
#endif
	
	}
    }

#ifdef __TEST__
  printf ("\n--------------------------------\n-----------------------------");
  printf ("\nREINSERIMENTO\n-----------------------\n------------------");
#endif

  for (act = plan, i = 0; act; act = act->next, i++)
    {


#ifdef __TEST__
      printf("\n----------------------------------------------------------\n");
#endif
      insert_remove_action (act->act_pos, i, C_T_INSERT_ACTION, GpG.approximation_level);

      for (j = 0; j <= GpG.curr_plan_length; j++)
	{

#ifdef __TEST__
	  printf ("\nVALORI NUMERICI al liv. %d:\n", j);
	  for (k = 0; k < gnum_comp_var; k++)
	    {
	      printf ("%3d:%8.2f", k, vectlevel[j]->numeric->values[k]);
	      printf ("%c\t", GET_BIT (gis_inertial, k) ? ' ' : '*');
	    }
#endif
	  check_consistency (j);


#ifdef __TEST__
	  if (j != GpG.curr_plan_length)
	    {
	      printf ("\n\nazione: ");
	      if (vectlevel[j]->action.position >= 0)
		print_op_name (vectlevel[j]->action.position);

	      printf ("\n");
	    }
#endif
	}
    }
}






Bool
are_ft_mutex (int first, int second)
{
  if (CONVERT_FACT_TO_VERTEX (first)->
      ft_exclusive_vect[GUID_BLOCK (second)] & GUID_MASK (second))
    return TRUE;
  else
    return FALSE;


}




int
test_level (char *cStr)
{
  int i;
  if (cStr[0] == ';')
    return 0;

  if (cStr[strlen (cStr) - 1] == ':')
    i = 1;
  else
    i = 0;

  return (i);
}



void
skip_pddl2_comment (FILE * fp)
{
  char charact;

  charact = fgetc (fp);

  if (charact == ';')
    {
      while (charact != '\n')
	charact = fgetc (fp);
    }

}



token
new_token (int size)
{
  char *tok;

  tok = (char *) calloc (size, sizeof (char));

  if (!tok)
    {
      fprintf (stderr, "%s", WAR_NO_MEMORY);
      exit (2);
    }

  return tok;
}








FtConn *lookup_go( token t )
{

}




// E' richiamata da quick_lookup_act  
EfConn *lookup_op( token t )
{
  
}





// E' richiamata da marco_store_adapted_plan 
int quick_lookup_act( action* act )
{

  EfConn * op=NULL;

  if (act->position<0)
    {
      op = lookup_op (act->name);
      act->position = op->position;
    }

  return (act->position);

}








/*******************************
*           walkplan.c           *
********************************/


// Define the set of action that removes the inconsistency.
// Associated with "tofix"
int define_neighborhood_for_threats (register anode_list node_tofix, int initialize)
{
  int i, j, level, propagation, skip;
  EfConn *o_tofix;
  register int el, cel;
  float cost, cost1;
  neighb temp_act;
  noop_not_in *temp_noop;
  skip = 0;


  /* Insert ME */
  o_tofix = CONVERT_ACTION_TO_VERTEX (node_tofix->position);
  temp_noop = node_tofix->treated;
  while (temp_noop)
    {
      
      // controllo se il fatto e' precondizione di almeno una azione e tuttora supportato
      if (vectlevel[*node_tofix->level + 1]->
	  fact[temp_noop->position].w_is_goal
	  && vectlevel[*node_tofix->level]->fact[temp_noop->position].
	  w_is_true)
	break;
      temp_noop = temp_noop->next;
    }
  if (temp_noop == NULL)
    return 0;
  
#ifdef __TEST__
  printf ("\n-------CHOOSE ACTIONS treated_c_l ");
#endif
  
      // Inserisco l'azione nel vicinato
  temp_act.act_pos = vectlevel[*node_tofix->level]->action.position;
  temp_act.act_level = *node_tofix->level;
  temp_act.constraint_type = C_T_REMOVE_ACTION;
  insert_element_in_neighb (&temp_act);
  
  
  
#ifdef __TEST__
  printf ("\n first act %s level %d ",  
	  print_op_name_string (temp_act.act_pos, temp_name),
	  temp_act.act_level);
#endif
  
      // Scelgo una azione che ha il fatto come precondizione e l'azione che rende supportato il fatto
  for (i = *node_tofix->level + 1; i < GpG.curr_plan_length; i++)
    if (vectlevel[i]->fact[temp_noop->position].w_is_used)
      {
	temp_act.act_pos = vectlevel[i]->action.position;
	temp_act.act_level = i;
	temp_act.constraint_type = C_T_REMOVE_ACTION;
	
	
#ifdef __TEST__
	printf ("\n second act %s level %d ",
		print_op_name_string (temp_act.act_pos, temp_name),
		temp_act.act_level);
	
#endif
	insert_element_in_neighb (&temp_act);
	break;
      }

  //Scelgo l'azione che ha il fatto come precondizione
  for (i = *node_tofix->level; i > 0; i--)
    if (vectlevel[i]->fact[temp_noop->position].w_is_true
	&&
	is_fact_in_additive_effects (GET_ACTION_POSITION_OF_LEVEL
				     (i - 1), temp_noop->position))
      {
	temp_act.act_pos = vectlevel[i - 1]->action.position;
	temp_act.act_level = i - 1;
	temp_act.constraint_type = C_T_REMOVE_ACTION;
	

#ifdef __TEST__
	printf ("\n Third act %s level %d ",
		print_op_name_string (temp_act.act_pos, temp_name),
		temp_act.act_level);
	
#endif 
	insert_element_in_neighb (&temp_act);
	break;
      }
  return num_neighborhood;
}






int
is_back_chain_fact_false (inform * fact)
{
  FtConn *noop;
  if (fact->w_is_goal == 0 || fact->w_is_true)
    return 0;
  if (*fact->level >= GpG.curr_plan_length)
    return 1;
  noop = CONVERT_FACT_TO_VERTEX (fact->position);
  return (!
	  (vectlevel[*fact->level + 1]->
	   noop_prec_act_vect[GUID_BLOCK (noop->position)] &
	   GUID_MASK (noop->position)));
}




float
set_pos_coefficient (float orig, unsigned int new_constr,
		     unsigned int total_constr, float delta)
{
  auto float step, n_delta = GpG.sqr_s_s;
  if (GpG.self_stabilizing == 0)
    return orig;
  if (total_constr && new_constr)
    step = delta * new_constr * ((float) total_constr) / ((float) GpG.num_false_tot);	// -  n_delta; // /(float)total_constr;
  else
    step = -n_delta;
  return (new_value (orig + step));
}

float
set_neg_coefficient (float orig, unsigned int new_constr,
		     unsigned int total_constr, float delta)
{
  auto float new_value = 0.0, step, n_delta = GpG.sqr_s_s;

  //  if (GpG.self_stabilizing==0)
  return orig;
  if (total_constr && new_constr)
    step = (-1) * delta * new_constr / (float) total_constr;	// + n_delta;// /(float)total_constr;
  else
    step = n_delta;
  new_value = orig + step;
  if (new_value > 0.0)
    new_value = 0.0;
  return new_value;
}




/* control the plan found */
void
verify_plan (State * start_state, State * end_state, int max_level)
{
  int i = 0, num;
  FtConn *ft;
  token_list i_t = NULL;

  /* setup the intial facts: lookup global 
   * fact strings from fact_table[0]
   */
  printf ("\n\nVerifing plan: :");
  insert_plan (max_level);
  reset_plan (max_level);
  for (num = 0, i = 0; i < start_state->num_F; i++, num++)

    {
      ft = &gft_conn[start_state->F[i]];
      CONVERT_FACT_TO_INFORM (ft->position, 0)->w_is_true = TRUE;
      vectlevel[0]->facT_vect[GUID_BLOCK (ft->position)] |=GUID_MASK (ft->position);
      forward_noop_propagation (ft->position, 0);
    }

  /* verify that the preconditions was verified and that doesn't exist ME relations */
  GpG.num_false_fa = 0;
  GpG.num_false_act = 0;
  while (i < num_neighborhood)
    {
      insert_remove_action (neighb_vect[i]->act_pos,
			    neighb_vect[i]->act_level,
			    neighb_vect[i]->constraint_type,
			    GpG.approximation_level);
      if (GpG.num_false_fa || GpG.num_false_act)
	{
	  printf ("\n $$$$$$$$$$$$$$$$ DANGER ACTION ERROR ");
	  print_op_name_string (neighb_vect[i]->act_pos, temp_name);
	  printf ("  %d\n", neighb_vect[i]->act_level);

	  //     if(DEBUG4) 
	  my_print_plan_all (max_level);
	  exit (0);
	}
      i++;
    }

  /* goals control */
  for (i = 0; i < end_state->num_F; i++)
    {
      ft = &gft_conn[end_state->F[i]];
      if (CONVERT_FACT_TO_INFORM (ft->position, max_level)->w_is_true == 0)
	{
	  printf
	    ("\n $$$$$$$$$$$$$$$$ Warning: GOALS ERROR, goal %s, level %d curr_lev %d\n",
	     i_t->item, max_level, GpG.curr_plan_length);

	  //  if(DEBUG4)
	  my_print_plan_all (max_level);
	  exit (0);
	}
    }
  fprintf (stderr, "\n Plan OK\n");
  printf (" Plan OK");
}




/* insert in temp_vect the plan found*/
void
insert_plan (int max_level)
{
  int i, level, curr_plan_length = 0;
  for (level = 0, i = 0, curr_plan_length++; level < max_level; level++)
    if (vectlevel[level]->action.position >= 0)

      {
	insert_ac_vect (act_vect,
			&gef_conn[vectlevel[level]->action.position],
			curr_plan_length, curr_plan_length);

	//GPG2  temp_vect[i++]=&vectlevel[level]->action;
	curr_plan_length++;
      }
}





void compare_fact_matrix()
{ 
  int i,j,all_right;
  all_right=TRUE;
  for(i=0;i<gnum_ft_conn;i++)
   for(j=0;j<gnum_ft_conn;j++)
     if(GET_BIT(FT_FT_mutex_lazza[i],j)^GET_BIT(FT_FT_mutex_bettini[i],j))
       { all_right=FALSE;      
        printf("%d ",i);
        print_ft_name(i);
        printf("%d ",j);
        print_ft_name(j);
        printf("\n Lazza: %d",GET_BIT(FT_FT_mutex_lazza[i],j));
printf(" Bet: %d ",GET_BIT(FT_FT_mutex_bettini[i],j));
      }

  if(all_right)
    printf("\n Calcolo OK FATTI\n");
  else
  printf("\n error \n");

}



void compare_action_matrix()
{ 
  int i,j,all_right;
  all_right=TRUE;
  for(i=0;i<gnum_ef_conn;i++)
   for(j=0;j<gnum_ef_conn;j++)
     if(GET_BIT(EF_EF_mutex_lazza[i],j)^GET_BIT(EF_EF_mutex_bettini[i],j))
       { all_right=FALSE;      
        printf("%d ",i);
        print_ft_name(i);
        printf("%d ",j);
        print_ft_name(j);
        printf("\n Lazza: %d",GET_BIT(EF_EF_mutex_lazza[i],j));
	printf(" Bet: %d ",GET_BIT(EF_EF_mutex_bettini[i],j));
      }

  if(all_right)
    printf("\n Calcolo OK AZIONI\n");
  else
  printf("\n error \n");

}

/*******************
        H_max.c
********************/




void
remove_temp_action (int act_pos, int level)
{
  int i;
  EfConn *act;
  static int first_call = 0, last_action = -1;
  inform_list infAction;
  noop_not_in *temp;

  if (first_call == 0)
    {
      // alloco bitvector per fatti 
      new_true_facts = alloc_vect (gnum_ft_block);
      new_false_facts = alloc_vect (gnum_ft_block);
    }

  if (last_action >= 0)
    {
      // azzero opportunamente i glocchi di interi resi precedentemente veri 
      memset (new_true_facts, 0, gnum_ft_block * sizeof (int));

      for (i = 0; i < gef_conn[last_action].num_A; i++)
	{

	  if (gef_conn[last_action].A[i] < 0)
	    continue;

	  new_false_facts[GUID_BLOCK (gef_conn[last_action].A[i])] = 0;
	}
    }

  last_action = act_pos;
  act = &gef_conn[act_pos];

#ifdef __TEST__

  if (CHECK_ACTION_POSTION_OF_LEVEL (act_pos, level) == FALSE)
    {
      MSG_ERROR ("");
      exit (0);
    }
#endif

  // If the action is in the subgraph, we want to remove it, 
  // else if the action is not in the subgraph, we want to insert it.   
  infAction = GET_ACTION_OF_LEVEL (level);

#ifdef __TEST__
  printf ("\nTEMP RA %s is_used %d time %d pos %d", act->name,
	  infAction->w_is_used, level, infAction->position);
#endif

  for (i = 0; i < gef_conn[last_action].num_A; i++)
    {

      if (gef_conn[last_action].A[i] < 0)
	continue;
      new_false_facts[GUID_BLOCK (gef_conn[last_action].A[i])] |=
	GUID_MASK (gef_conn[last_action].A[i]);
    }


  /*  azioni durative */
  if (gef_conn[last_action].sf != NULL)
    {
      for (i = 0; i < gef_conn[last_action].sf->num_A_start; i++)
	{

	  if (gef_conn[last_action].sf->A_start[i] < 0)
	    continue;
	  new_false_facts[GUID_BLOCK (gef_conn[last_action].sf->A_start[i])]
	    |= GUID_MASK (gef_conn[last_action].sf->A_start[i]);
	}
    }

  temp = infAction->add;
  while (temp != NULL)
    {
      new_true_facts[GUID_BLOCK (temp->position)] |=
	GUID_MASK (temp->position);
      temp = temp->next;
    }

  /* azioni durative */
  if (gef_conn[last_action].sf != NULL)
    {

      for (i = 0; i < gef_conn[last_action].num_D; i++)
	{
	  if (gef_conn[last_action].D[i] < 0)
	    continue;

	  new_true_facts[GUID_BLOCK (gef_conn[last_action].D[i])] |=
	    GUID_MASK (gef_conn[last_action].D[i]);
	}

      for (i = 0; i < gef_conn[last_action].sf->num_D_start; i++)
	{
	  if (is_fact_in_additive_effects
	      (last_action, gef_conn[last_action].sf->D_start[i]))
	    continue;

	  if (gef_conn[last_action].sf->D_start[i] < 0)
	    continue;
	  new_true_facts[GUID_BLOCK (gef_conn[last_action].sf->D_start[i])] |=
	    GUID_MASK (gef_conn[last_action].sf->D_start[i]);

	}
    }

  /* end azioni durative */



#ifdef __CONTROLLARE__

  // Set to 1 the bit in the act_vect mask, relative to the inserted action   
  //    if ( !act->is_noop )  
  //         vectlevel[level]->act_vect[GUID_BLOCK(act->position)] &= ~GUID_MASK(act->position); 
  // LPGI noop      else 
  //       { 
  //     vectlevel[level]->noop_act_vect[act->uid_block] &= ~act->uid_mask; 
  //     vectlevel[level]->noop_prec_act_vect[act->uid_block] &= ~act->uid_mask; 

  //       } 


  // Setto le precondizioni dell'azione 
  for (ma_ptr = act->bit_precond, i = 0; i < act->num_precond; i++)
    {

      // Non avendo a disposizione informazioni sul numero di azioni che hanno  il fatto come precondizione, rimuovo tutte le precondizioni 
      vectlevel[level]->prec_vect[(ma_ptr + i)->uid_block] &=
	~((ma_ptr + i)->uid_mask);
      vectlevel[level]->true_crit_vect[(ma_ptr + i)->uid_block] &=
	~((ma_ptr + i)->uid_mask);
      vectlevel[level]->false_crit_vect[(ma_ptr + i)->uid_block] &=
	~((ma_ptr + i)->uid_mask);
    }


  // Setto gli effetti additivi dell'azione 
  level++;
  for (ma_ptr = act->bit_add_effect, i = 0; i < act->num_add_effect; i++)
    {
      //       if( CHECK_FACT_POS( &vectlevel[level]->fact[(ma_ptr+i)] ,level  )  ) 

      // my_print_plan_level ( level); 

      // Se era un fatto critico vero diventa un fatto falso ed un fatto critico falso 
      vectlevel[level]->facT_vect[(ma_ptr + i)->uid_block] &= ( ~((ma_ptr + i)->uid_mask & vectlevel[level]->true_crit_vect[(ma_ptr + i)->uid_block]));	// se prima era  true_critic_vect ora e' false 
      vectlevel[level]->false_crit_vect[(ma_ptr + i)->uid_block] |= ((ma_ptr + i)->uid_mask & vectlevel[level]->true_crit_vect[(ma_ptr + i)->uid_block]);	// se prima era true_critic_vect ora sara' false_critc_vect 
      vectlevel[level]->true_crit_vect[(ma_ptr + i)->uid_block] &=
	~((ma_ptr +
	   i)->uid_mask & vectlevel[level]->true_crit_vect[(ma_ptr +
							    i)->uid_block]);
      // se era un true_critic_vect ora non lo e' piu' 
      // my_print_plan_level ( level); 
    }


#endif

}




void
compute_all_dg_unsup_facts_cost (node_cost_list n_cost)
{
  int i, j, fact_pos, level, max_lev, b_index = 0;
  node_cost loc_n_cost;
  float temp;
  inform_list inf_fact;
  int array_unsup[500];


#ifdef __TEST__
    int diff = 0;
#endif


  n_cost->act_cost = 0.0;
  n_cost->act_time = 0.0;
  n_cost->weight = 0.0;
  n_cost->num_actions = 0;


  for (i = 0; i < 500 && i < GpG.num_false_fa; i++)
    {
      array_unsup[i] = 0;
    }


#ifdef __TEST__
  printf ("\n\n %%%%%%%%%%%%%%%%%%%%%%%% COMPUTE ALL DG UNSUP FACTS COST");
#endif
  for (i = 0; i < GpG.num_false_fa; i++)
    {
      max_lev = 1000000;
      for (j = 0; j < GpG.num_false_fa; j++)
	{
	  if (max_lev > *unsup_fact[j]->level && array_unsup[j] == 0)
	    {
	      b_index = j;
	      max_lev = *unsup_fact[j]->level;
	    }
	}
      fact_pos = unsup_fact[b_index]->fact;
      array_unsup[b_index] = 1;
      level = *unsup_fact[b_index]->level;
      if (GET_BIT (Hvar.bit_vect_facts, fact_pos))
	{
#ifdef __TEST__
	  printf
	    ("\nFact %s already supported in COMPUTE_DG_LIST_COST, level %d",
	     print_ft_name_string (fact_pos, temp_name), level);
#endif
	  continue;
	}

      inf_fact = CONVERT_FACT_TO_INFORM (fact_pos, level);

      temp = compute_dg_facts_cost (fact_pos, level, &loc_n_cost, level);

      if (GpG.accurate_cost >= COMPUTE_DG_LIST_COST)
	{


	  n_cost->weight += loc_n_cost.weight;

	  n_cost->act_cost += loc_n_cost.act_cost;
	  n_cost->num_actions += loc_n_cost.num_actions;
	  if (n_cost->act_time < loc_n_cost.act_time
	      && loc_n_cost.act_time < MAX_COST)
	    n_cost->act_time = loc_n_cost.act_time;
#ifdef __TEST__
	  if (DEBUG3)
	    {
	      printf
		("\n %d COMPUTE DG  COST UNSUP FACT  fact %s, COST  weight %f cost %f time %f n_cost->num_actions  %d ",
		 ++diff, print_ft_name_string (fact_pos, temp_name),
		 n_cost->weight, n_cost->act_cost, n_cost->act_time,
		 n_cost->num_actions);

	    }

#endif

	}


    }
}





void
remove_ls_insert_fact_inlist (int pos)
{
  int i,stop=FALSE;


  return;
  if(GET_BIT(Hvar.bit_vect_facts, pos)==0)
    return;


  for(i=0; i<Hvar.num_facts_define_cost; i++)
    if(Hvar.list_ft_define_cost[i]==pos)
      {
	stop=TRUE;
	RESET_BIT(Hvar.bit_vect_facts, pos);
	Hvar.list_ft_define_cost[i]=Hvar.list_ft_define_cost[Hvar.num_facts_define_cost--] ;
      }
    
  if(stop==FALSE)
    printf("\nWarning remove_ls_insert_fact_inlist: the element is not inthe array");


}






// vecchia funzione di selezione delle inconsistenze

int
choose_inconsistence (int min_time,int numrestart)
{
  int tot;
  int unsup_pos;
  int fix_num, fix_timed;
  float k_weight_num_bool = 1;
  
  /*********************
  if (GpG.num_false_fa <= 0 && GpG.num_false_act <= 0 && GpG.num_false_num_fa <= 0 && GpG.num_false_tmd_fa <= 0) {
    if (GpG.optimize == 0 || GpG.num_solutions == 0)
      return 0;
    
    else {
      // Rimuovo un azione dell'action sugraoh in modo casuale al fine di introdurre delle inconsistenze e continuare il processo di ottimizzazione
      restart_search ();
    }
    }
  *****************/

  // If the reserch type is ANYTIME, use an apposite function that works on the first levels
  if (DEBUG3)
    {
      print_unsup_fact_vect ();
      print_unsup_num_facts ();
      print_unsup_timed_fact ();
    }
  if (DEBUG6 && GpG.accurate_cost >= COMPUTE_DG_SUM_COST)
    print_cost_of_unsupported_facts ();
 
  
  // If the # of false fact is 0, we have to choose an action to fix
  // If the # of false act is 0, we have to choose a fact to fix 
  // In these cases, the choosing tecnique is random
  
  if (GpG.num_false_fa <= 0 && GpG.num_false_num_fa <= 0 && GpG.num_false_tmd_fa <= 0) {
    /* inconsistenza azione */ 
    return fix_threated_fact (treated_c_l[MY_RANDOM % GpG.num_false_act]);
  }

  else /* inconsistenza fatto */ 
    //    if (GpG.num_false_act <= 0) {
    {

      fix_timed  = (MY_RANDOM % (int) (GpG.num_false_tmd_fa + GpG.num_false_fa + k_weight_num_bool * GpG.num_false_num_fa) ) < (GpG.num_false_tmd_fa);

      if( GpG.neighb_with_timed_fa && fix_timed )
	{
	  // Risolvo prima le inconsistenze temporali
	  if (GpG.num_false_tmd_fa > 0) {
	    //	    printf("T");
	    unsup_pos = MY_RANDOM % GpG.num_false_tmd_fa;
	    if (GpG.inc_choice_type != RANDOM_INC)	
	      unsup_pos = choose_min_cost_unsup_tmd_fact ();
	    return fix_unsup_timed_fact (unsup_tmd_facts[unsup_pos]);
	  }
	}
      else
	{
      fix_num =	(MY_RANDOM % (int) (GpG.num_false_fa + k_weight_num_bool * GpG.num_false_num_fa) ) > (GpG.num_false_fa);

      if (GpG.num_false_num_fa == 0)
	fix_num = FALSE;
      if (GpG.num_false_fa == 0) 
	fix_num = TRUE;

#ifdef __TEST_MIXED__
      printf("\n NUMERICO[1] /LOGICO[0] ");
      fflush (stdout);
      scanf("%d",&fix_num);
#endif
      if (fix_num) {
	/* inconsistenza numerica */
	assert (GpG.num_false_num_fa > 0);
	unsup_pos = MY_RANDOM % GpG.num_false_num_fa;
	if (GpG.inc_choice_type != RANDOM_INC)	
	  unsup_pos = choose_min_cost_unsup_num_fact ();


#ifdef __TEST_MIXED__
	printf("\n Inserisci fatto da supportare ");
	fflush (stdout);
	scanf("%d",&unsup_pos);
#endif
	return fix_unsup_num_fact (unsup_num_fact[unsup_pos]);
      }
      
      else {
	/* inconsistenza logica */
	assert (GpG.num_false_fa > 0);
	unsup_pos = MY_RANDOM % GpG.num_false_fa;
	if (GpG.inc_choice_type != RANDOM_INC)	
	  unsup_pos = choose_min_cost_unsup_fact ();


#ifdef __TEST_MIXED__
	printf("\n Inserisci fatto da supportare ");
	fflush (stdout);
	scanf("%d",&unsup_pos);
#endif
	return fix_unsup_fact (unsup_fact[unsup_pos]);
      }
    
    }
    }

  // If the # of false actions is above than the # of ME rels, there is a mistake, then correct it
  //  if (GpG.num_m_e < GpG.num_false_act ) GpG.num_m_e = GpG.num_false_act;
  // Same as above, but control the # of precs 
  //  if (GpG.num_prec < GpG.num_false_fa ) GpG.num_prec = GpG.num_false_fa;
  

 
  GpG.num_m_e = GpG.num_false_act;
  GpG.num_prec = GpG.num_false_fa;

  // Here, choose between a fact and an action, giving to the facts a different weight.
  // This choose is motivated by experimental results.
  tot = (int) ceil (GpG.weight_fact * GpG.num_prec + GpG.num_m_e);

  //     check_false_position();
  // III provo a rivuovere prima le mutex
  if ( ( MY_RANDOM  % tot) < GpG.num_m_e) {
    /* inconsistenza azione */
    unsup_pos = MY_RANDOM % GpG.num_false_act;
    return fix_threated_fact (treated_c_l[unsup_pos]);
  }
  
  else { /* inconsistenza fatto */

    /* scelgo tra un inconsistenza numerica o una logica */
    fix_num = (MY_RANDOM % (int) (GpG.num_false_fa + k_weight_num_bool * GpG.num_false_num_fa)) >
      (GpG.num_false_fa);
    
    if (GpG.num_false_num_fa == 0)
      fix_num = FALSE;
    if (GpG.num_false_fa == 0)
      fix_num = TRUE;
    
#ifdef __TEST_MIXED__
      printf("\n NUMERICO[1] /LOGICO[0] ");
      fflush (stdout);
      scanf("%d",&fix_num);
#endif
    if (fix_num) {
      /* inconsistenza numerica */
      assert (GpG.num_false_num_fa > 0);
      unsup_pos = MY_RANDOM % GpG.num_false_num_fa;
      if (GpG.inc_choice_type != RANDOM_INC)	
	unsup_pos = choose_min_cost_unsup_num_fact ();


#ifdef __TEST_MIXED__
      printf("\n Inserisci fatto da supportare ");
      fflush (stdout);
      scanf("%d",&unsup_pos);
#endif
      return fix_unsup_num_fact (unsup_num_fact[unsup_pos]);
    }
    
    else {
      /* inconsistenza logica */
      assert (GpG.num_false_fa > 0);
      unsup_pos = MY_RANDOM % GpG.num_false_fa;
      if (GpG.inc_choice_type != RANDOM_INC)	
	unsup_pos = choose_min_cost_unsup_fact ();


#ifdef __TEST_MIXED__
      printf("\n Inserisci fatto da supportare ");
      fflush (stdout);
      scanf("%d",&unsup_pos);
#endif
      return fix_unsup_fact (unsup_fact[unsup_pos]);
    }

  }

}







/**

 switch(choice->constraint_type)
    {
    case C_T_UNSUP_FACT: return fix_unsup_fact (choice);
      break;
    case C_T_UNSUP_NUM_FACT: return fix_unsup_num_fact(choice);
      break;
    case C_T_TREATED_CL: return fix_threated_fact (choice);
      break;
    case C_T_UNSUP_TMD_FACT: return fix_unsup_timed_fact(choice);
    }

**/
