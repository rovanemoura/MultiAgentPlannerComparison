#include <unistd.h>
#include "ff.h"
#include "adapt.h"
#include "gpg.h"
#include "walkplan.h"
#include "functions.h"
#include "dg_heuristics.h"

int * AA;
int * TmpState;



int ApplicableAction ( int * StateGoal, int NumGoal )
{
  /**
   * i, j, k, act_pos, ind: variabile di appoggio
   **/
  
  int i, j, k, act_pos, ind=0;
  static int *temp_action_vect;

  if (temp_action_vect == NULL)
    temp_action_vect = (int *) calloc (gnum_ef_block, sizeof (int));

  memset(temp_action_vect, 0, gnum_ef_block * sizeof (int));

  if (AA==NULL)
    AA = ( int * ) calloc (10000, sizeof (int));
      
  /**
   * Scandiamo il vettore G dei fatti da supportare per lo stato attuale
   **/
  for (i=0; i < NumGoal ; i++)
    {
      /**
       * Esaminiamo tutte le azioni che supportano i fatti G at end
       **/
      for ( j=0;  j < gft_conn[StateGoal[i]].num_A; j++)
	{
	  act_pos = gft_conn[StateGoal[i]].A[j];

	  if (GET_BIT(temp_action_vect, act_pos))
	      continue;
	  /**
	   * Se l'azione e' mutex con un fatto G non la si considera applicabile
	   **/
	  for ( k = 0; k < NumGoal; k++ )
	    {
	      if (k == i)
		continue;
	      
	      if (CONVERT_NOOP_TO_VERTEX(StateGoal[k])->ef_exclusive_vect[GUID_BLOCK (act_pos)] & GUID_MASK (act_pos))
		break;
	    }
	  /**
	   * Se l'azione non e' mutex con alcun fatto G, la inseriamo nel vettore delle azioni applicabili
	   **/
	  if ( k == NumGoal)
	    {
	      SET_BIT(temp_action_vect, act_pos);
	      AA[ind] = act_pos; 
	      ind ++;
	    }
	}
    }

  /**
   * Ritorna il numero di azioni applicabili
   **/
  return ind;
  
}


int NewGoalState(int Action, int *StateGoal, int NumGoal, int *NewStateGoal)
{
  int i, j, ind;
  static int *tmp_eff_vect;
  ind = 0;

  if (TmpState == NULL)
    TmpState = (int *) calloc ( 1000, sizeof (int) ); 

  if (tmp_eff_vect ==NULL)
    tmp_eff_vect = (int *) calloc (gnum_ft_block, sizeof (int ));

  memset(tmp_eff_vect, 0, gnum_ef_block * sizeof (int));

  for (j=0; j < gef_conn[Action].num_A; j++)
    SET_BIT(tmp_eff_vect, gef_conn[Action].A[j]);

  if (gef_conn[Action].sf)
    for (j=0; j < gef_conn[Action].sf->num_A_start; j++)
      SET_BIT(tmp_eff_vect, gef_conn[Action].sf->A_start[j]);

  for (i=0; i < NumGoal; i++)
    {
      if (GET_BIT(tmp_eff_vect,StateGoal[i])==0 )
	{
	  TmpState[ind]= StateGoal[i];
	  ind++;
	}
    }

  for (i=0; j < gef_conn[Action].num_PC; j++)
    {
      TmpState[ind]=gef_conn[Action].PC[j];
      ind++;
    }

  if (gef_conn[Action].sf)
    {
      for (i=0; j < gef_conn[Action].sf->num_PC_overall; j++)
	{
	  TmpState[ind]=gef_conn[Action].sf->PC_overall[j];
	  ind++;
	}
      for (i=0; j < gef_conn[Action].sf->num_PC_end; j++)
	{
	  TmpState[ind]=gef_conn[Action].sf->PC_end[j];
	  ind++;
	}
    }

  NewStateGoal= (int *) calloc (ind, sizeof(int));

  memcpy(NewStateGoal,TmpState, ind *sizeof(int));
  return (ind);

}
