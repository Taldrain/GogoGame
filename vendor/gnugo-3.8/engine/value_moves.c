/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * This is GNU Go, a Go program. Contact gnugo@gnu.org, or see       *
 * http://www.gnu.org/software/gnugo/ for more information.          *
 *                                                                   *
 * Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   *
 * 2008 and 2009 by the Free Software Foundation.                    *
 *                                                                   *
 * This program is free software; you can redistribute it and/or     *
 * modify it under the terms of the GNU General Public License as    *
 * published by the Free Software Foundation - version 3 or          *
 * (at your option) any later version.                               *
 *                                                                   *
 * This program is distributed in the hope that it will be useful,   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of    *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     *
 * GNU General Public License in file COPYING for more details.      *
 *                                                                   *
 * You should have received a copy of the GNU General Public         *
 * License along with this program; if not, write to the Free        *
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,       *
 * Boston, MA 02111, USA.                                            *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "gnugo.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "liberty.h"
#include "gg_utils.h"
#include "move_reasons.h"


/* Count how many distinct strings are (solidly) connected by the move
 * at (pos). Add a bonus for strings with few liberties. Also add
 * bonus for opponent strings put in atari or removed and for own
 * strings in atari adjacent to removed opponent strings.
 *
 * The parameter to_move should be set when color is the color to
 * move. (This function is called for both colors.)
 */
static int
move_connects_strings(int pos, int color, int to_move)
{
  int ss[4];
  int strings = 0;
  int own_strings = 0;
  int k, l;
  int fewlibs = 0;

  for (k = 0; k < 4; k++) {
    int ii = pos + delta[k];
    int origin;

    if (!ON_BOARD(ii) || board[ii] == EMPTY)
      continue;

    origin = find_origin(ii);

    for (l = 0; l < strings; l++)
      if (ss[l] == origin)
	break;

    if (l == strings) {
      ss[strings] = origin;
      strings++;
    }
  }

  for (k = 0; k < strings; k++) {
    if (worm[ss[k]].invincible)
      continue;
    if (board[ss[k]] == color) {
      int newlibs = approxlib(pos, color, MAXLIBS, NULL);
      own_strings++;
      if (newlibs >= countlib(ss[k])) {
	if (countlib(ss[k]) <= 4)
	  fewlibs++;
	if (countlib(ss[k]) <= 2)
	  fewlibs++;
      }
    }
    else {
      if (countlib(ss[k]) <= 2)
	fewlibs++;
      if (countlib(ss[k]) <= 1 && to_move) {
	int dummy[MAXCHAIN];
	fewlibs++;
	fewlibs += chainlinks2(ss[k], dummy, 1);
      }
    }
  }

  /* Do some thresholding. */
  if (fewlibs > 4)
    fewlibs = 4;
  if (to_move && is_ko(pos, color, NULL) && fewlibs > 1)
    fewlibs = 1;
  if (fewlibs == 0 && own_strings == 1)
    own_strings = 0;

  return own_strings + fewlibs;
}

/* Find saved dragons and worms, then call blunder_size(). */
static float
value_moves_get_blunder_size(int move, int color)
{
  signed char saved_dragons[BOARDMAX];
  signed char saved_worms[BOARDMAX];
  signed char safe_stones[BOARDMAX];

  get_saved_dragons(move, saved_dragons);
  get_saved_worms(move, saved_worms);

  mark_safe_stones(color, move, saved_dragons, saved_worms, safe_stones);
  
  return blunder_size(move, color, NULL, safe_stones);
}

static int
value_moves_confirm_safety(int move, int color)
{
  return (value_moves_get_blunder_size(move, color) == 0.0);
}


/* Test all moves which defend, attack, connect or cut to see if they
 * also attack or defend some other worm.
 *
 * FIXME: We would like to see whether an arbitrary move works to cut
 *        or connect something else too.
 */

static void
find_more_attack_and_defense_moves(int color)
{
  int unstable_worms[MAX_WORMS];
  int N = 0;  /* number of unstable worms */
  int ii;
  int k;
  int other = OTHER_COLOR(color);
  int cursor_at_start_of_line;
  
  TRACE("\nLooking for additional attack and defense moves. Trying moves ...\n");
  
  /* Identify the unstable worms and store them in a list. */
  for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
    if (IS_STONE(board[ii])
	&& worm[ii].origin == ii
	&& worm[ii].attack_codes[0] != 0
	&& worm[ii].defense_codes[0] != 0) {
      unstable_worms[N] = ii;
      N++;
    }
  }
  
  /* To avoid horizon effects, we temporarily increase the depth values. */
  increase_depth_values();
  
  for (ii = BOARDMIN; ii < BOARDMAX; ii++) {
    if (board[ii] != EMPTY)
      continue;

    /* Don't consider send-two-return-one moves here. */
    if (send_two_return_one(ii, color))
      continue;
    
    for (k = 0; k < MAX_REASONS; k++) {
      int r = move[ii].reason[k];
      
      if (r < 0)
	break;
      
      if (move_reasons[r].type == ATTACK_MOVE
	  || move_reasons[r].type == ATTACK_MOVE_GOOD_KO
	  || move_reasons[r].type == ATTACK_MOVE_BAD_KO
	  || move_reasons[r].type == DEFEND_MOVE
	  || move_reasons[r].type == DEFEND_MOVE_GOOD_KO
	  || move_reasons[r].type == DEFEND_MOVE_BAD_KO
	  || move_reasons[r].type == CONNECT_MOVE
	  || move_reasons[r].type == CUT_MOVE)
	break;
      /* FIXME: Add code for EITHER_MOVE and ALL_MOVE here. */
    }
    
    if (k == MAX_REASONS || move[ii].reason[k] == -1)
      continue;
    
    /* Try the move at (ii) and see what happens. */
    cursor_at_start_of_line = 0;
    TRACE("%1m ", ii);
    if (trymove(ii, color, "find_more_attack_and_defense_moves", NO_MOVE)) {
      for (k = 0; k < N; k++) {
	int aa = unstable_worms[k];
	
	/* string of our color, see if there still is an attack,
	 * unless we already know the move works as defense move.
	 */
	if (board[aa] == color
	    && !defense_move_reason_known(ii, unstable_worms[k])) {
	  int acode = attack(aa, NULL);
	  if (acode < worm[aa].attack_codes[0]) {
	    /* Maybe attack() doesn't find the attack. Try to
	     * attack with the stored attack move.
	     */
	    int defense_works = 1;
	    
	    if (trymove(worm[aa].attack_points[0], other, 
			"find_more_attack_and_defense_moves", 0)) {
	      if (!board[aa])
		defense_works = 0;
	      else {
		int this_acode = REVERSE_RESULT(find_defense(aa, NULL));
		if (this_acode > acode) {
		  acode = this_acode;
		  if (acode >= worm[aa].attack_codes[0])
		    defense_works = 0;
		}
	      }
	      popgo();
	    }
	    
	    if (defense_works) {
	      if (!cursor_at_start_of_line)
		TRACE("\n");
	      TRACE("%ofound extra point of defense of %1m at %1m code %d\n",
		    aa, ii, REVERSE_RESULT(acode));
	      cursor_at_start_of_line = 1;
	      add_defense_move(ii, aa, REVERSE_RESULT(acode));
	    }
	  }
	}
	
	/* string of opponent color, see if there still is a defense,
	 * unless we already know the move works as attack move.
	 */
	if (board[aa] == other
	    && !attack_move_reason_known(ii, unstable_worms[k])) {
	  
	  int dcode = find_defense(aa, NULL);
	  if (dcode < worm[aa].defense_codes[0]) {
	    /* Maybe find_defense() doesn't find the defense. Try to
	     * defend with the stored defense move.
	     *
	     * Another option is maybe there is no attack anymore
	     * (e.g. we pushed the worm into seki), find_defense()
	     * could easily fail in that case.
	     */
	    int attack_works = 1;

	    if (attack(aa, NULL) >= worm[aa].attack_codes[0]) {
	      if (trymove(worm[aa].defense_points[0], other, 
			  "find_more_attack_and_defense_moves", 0)) {
		int this_dcode = REVERSE_RESULT(attack(aa, NULL));
		if (this_dcode > dcode) {
		  dcode = this_dcode;
		  if (dcode >= worm[aa].defense_codes[0])
		    attack_works = 0;
		}
		popgo();
	      }
	    }
	    else
	      attack_works = 0;
	    
	    if (attack_works) {
	      if (!cursor_at_start_of_line)
		TRACE("\n");
	      TRACE("%ofound extra point of attack of %1m at %1m code %d\n",
		    aa, ii, REVERSE_RESULT(dcode));
	      cursor_at_start_of_line = 1;
	      add_attack_move(ii, aa, REVERSE_RESULT(dcode));
	    }
	  }
	}
      }
      popgo();
    }
  }
  
  TRACE("\n");
  decrease_depth_values();
}


/* Do the real job of find_more_owl_attack_and_defense_moves() with given
 * move reason at given position and for given target (`what').  This
 * function is used from induce_secondary_move_reasons() for upgrading
 * one specific move reason only.
 */
static void
do_find_more_owl_attack_and_defense_moves(int color, int pos,
					  int move_reason_type, int what)
{
  int k;
  int dd1 = NO_MOVE;
  int dd2 = NO_MOVE;
  int save_verbose;

  gg_assert(stackp == 0);
  
  /* Never consider moves of the send-two-return-one type here. */
  if (send_two_return_one(pos, color))
    return;

  /* Never consider moves playing into snapback here. */
  if (playing_into_snapback(pos, color))
    return;

  save_verbose = verbose;
  if (verbose > 0)
    verbose --;

  if (move_reason_type == STRATEGIC_ATTACK_MOVE
      || move_reason_type == STRATEGIC_DEFEND_MOVE)
    dd1 = what;
  else if (move_reason_type == ATTACK_MOVE
	   || move_reason_type == ATTACK_MOVE_GOOD_KO
	   || move_reason_type == ATTACK_MOVE_BAD_KO
	   || move_reason_type == DEFEND_MOVE
	   || move_reason_type == DEFEND_MOVE_GOOD_KO
	   || move_reason_type == DEFEND_MOVE_BAD_KO
	   || move_reason_type == VITAL_EYE_MOVE)
    dd1 = what;
  else if (move_reason_type == CONNECT_MOVE) {
    int worm1 = conn_worm1[what];
    int worm2 = conn_worm2[what];

    dd1 = dragon[worm1].origin;
    dd2 = dragon[worm2].origin;
    if (dd1 == dd2)
      dd2 = NO_MOVE;
  }
  else {
    verbose = save_verbose;
    return;
  }

  for (k = 0; k < 2; k++) {
    int dd = (k == 0 ? dd1 : dd2);

    if (dd == NO_MOVE)
      continue;

    /* Don't care about inessential dragons. */
    if (DRAGON2(dd).safety == INESSENTIAL)
      continue;

    if (DRAGON2(dd).owl_status != CRITICAL)
      continue;

    if ((move_reason_type == STRATEGIC_ATTACK_MOVE
	 || move_reason_type == ATTACK_MOVE
	 || move_reason_type == ATTACK_MOVE_GOOD_KO
	 || move_reason_type == ATTACK_MOVE_BAD_KO
	 || (move_reason_type == VITAL_EYE_MOVE
	     && board[dd] == OTHER_COLOR(color)))
	&& !owl_attack_move_reason_known(pos, dd)) {
      int kworm = NO_MOVE;
      int acode = owl_does_attack(pos, dd, &kworm);

      if (acode >= DRAGON2(dd).owl_attack_code) {
	add_owl_attack_move(pos, dd, kworm, acode);
	if (save_verbose)
	  gprintf("Move at %1m upgraded to owl attack on %1m (%s).\n",
		  pos, dd, result_to_string(acode));
      }
    }

    if ((move_reason_type == STRATEGIC_DEFEND_MOVE
	 || move_reason_type == CONNECT_MOVE
	 || move_reason_type == DEFEND_MOVE
	 || move_reason_type == DEFEND_MOVE_GOOD_KO
	 || move_reason_type == DEFEND_MOVE_BAD_KO
	 || (move_reason_type == VITAL_EYE_MOVE
	     && board[dd] == color))
	&& !owl_defense_move_reason_known(pos, dd)) {
      int kworm = NO_MOVE;
      /* FIXME: Better use owl_connection_defend() for CONNECT_MOVE ? */
      int dcode = owl_does_defend(pos, dd, &kworm);

      if (dcode >= DRAGON2(dd).owl_defense_code) {
	if (dcode == LOSS)
	  add_loss_move(pos, dd, kworm);
	else
	  add_owl_defense_move(pos, dd, dcode);
	if (save_verbose)
	  gprintf("Move at %1m upgraded to owl defense for %1m (%s).\n",
		  pos, dd, result_to_string(dcode));
      }
    }
  }

  verbose = save_verbose;
}



/* Try whether the move at (pos) for (color) is also an owl attack on
 * (target). (dist) is the distance to the dragon, and is used for a
 * safety heuristic: distant moves are only accepted if they kill within
 * few owl nodes.
 */
static void
try_large_scale_owl_attack(int pos, int color, int target, int dist)
{
  int owl_nodes_before;
  int owl_nodes_used;
  int kworm = NO_MOVE;
  int acode;
  int save_verbose = verbose;
  int save_owl_node_limit = owl_node_limit;
  
  ASSERT1(board[target] == OTHER_COLOR(color), pos);
  ASSERT1(!owl_attack_move_reason_known(pos, target), pos);
  DEBUG(DEBUG_LARGE_SCALE, "Trying large scale move %1m on %1m\n", pos, target);
  
  /* To avoid horizon effects, we temporarily increase 
   * the depth values to find the large scale attacks.
   */
  increase_depth_values(); 
  
  /* To reduce the amount of aji allowed for large scale
   * attacks, we reduce the owl limit to 350 nodes for
   * attacks at distance <= 1, and 150 nodes for attacks at
   * distance >= 2.
   */
  if (dist <= 1)
    owl_node_limit *= 0.35;
  else
    owl_node_limit *= 0.15;

  if (DRAGON2(target).owl_attack_node_count < owl_node_limit) {
    if (verbose > 0)
      verbose--;

    owl_nodes_before = get_owl_node_counter(); 
    acode = owl_does_attack(pos, target, &kworm);
    owl_nodes_used = get_owl_node_counter() - owl_nodes_before;
    
    if (acode >= DRAGON2(target).owl_attack_code
	&& acode == WIN) {
      add_owl_attack_move(pos, target, kworm, acode);
      DEBUG(DEBUG_LARGE_SCALE | DEBUG_MOVE_REASONS,
	    "Move at %1m owl-attacks %1m on a large scale(%s).\n", 
	    pos, target, result_to_string(acode));
    }
    else
      DEBUG(DEBUG_LARGE_SCALE,
	    "Move at %1m isn't a clean large scale attack on %1m (%s).\n",
	    pos, target, result_to_string(acode));
    
    DEBUG(DEBUG_LARGE_SCALE, "  owl nodes used = %d, dist = %d\n", 
	  owl_nodes_used, dist);
    /* Restore settings. */
    verbose = save_verbose;
  }
  decrease_depth_values(); 
  owl_node_limit = save_owl_node_limit;
}


#define MAXIMUM_LARGE_SCALE_DIST 	3

/* Test all the moves to see whether they can owl-attack a specific
 * dragon on a large scale . Tested moves are
 * 1. Moves that already have a move reason. 
 * 2. Are not too far away.
 *    The distance used is the Manhattan distance, and the maximum 
 *    distance is MAXIMUM_LARGE_SCALE_DIST.
 */
static void
find_large_scale_owl_attacks_on_dragon(int color, int target)
{
  int x, y;
  int x_min = board_size;
  int x_max = 0;
  int y_min = board_size;
  int y_max = 0;
  int dist;

  ASSERT1(board[target] == OTHER_COLOR(color), target);
  
  /* Find the physical extension of the dragon. */
  for (x = 0; x < board_size; x++)
    for (y = 0; y < board_size; y++) {
      if (is_same_dragon(target, POS(x, y))) {
	if (x < x_min)
	  x_min = x;
	if (x > x_max)
	  x_max = x;
	if (y < y_min)
	  y_min = y;
	if (y > y_max)
	  y_max = y;
      }
    }
  ASSERT1(x_min <= x_max && y_min <= y_max, target);

  /* Try to find large scale attacks.
   * We do this by first trying to find attacks at dist = 0, then
   * dist = 1, etc., up to MAXIMUM_LARGE_SCALE_DIST.
   */
  for (dist = 0; dist <= MAXIMUM_LARGE_SCALE_DIST; dist++)
    for (x = gg_max(x_min - dist, 0);
	 x <= gg_min(x_max + dist, board_size - 1); x++)
      for (y = gg_max(y_min - dist, 0);
	   y <= gg_min(y_max + dist, board_size - 1); y++) {
	int pos = POS(x, y);
	ASSERT1(ON_BOARD2(x, y), pos);
	
	if (board[pos] == EMPTY) {
	  int a, b, dx, dy;
	  a = abs(x - x_min);
	  b = abs(x - x_max);
	  dx = gg_min(a, b);
	  a = abs(y - y_min);
	  b = abs(y - y_max);
	  dy = gg_min(a, b);

	  if (gg_max(dx, dy) == dist  
	      && move[pos].reason[0] >= 0
	      && !owl_attack_move_reason_known(pos, target))
	    /* Maximum Manhatan distance, move reason known but no owl
	     * attack yet.
	     */
	    try_large_scale_owl_attack(pos, color, target, dist);
	  
	}
      }
}


/* Try large scale owl attacks against all enemy dragons that are
 * small (size <= 6) and critical.
 */
static void
find_large_scale_owl_attack_moves(int color)
{
  int d;

  DEBUG(DEBUG_LARGE_SCALE, "\nTrying to find large scale attack moves.\n");
  for (d = 0; d < number_of_dragons; d++) {
    int target = dragon2[d].origin;
    if (dragon[target].color == OTHER_COLOR(color)
	&& dragon[target].size <= 6
        && dragon[target].status == CRITICAL
	&& dragon2[d].owl_status == CRITICAL) {
      DEBUG(DEBUG_LARGE_SCALE, "Small critical dragon found at %1m\n", target);
      find_large_scale_owl_attacks_on_dragon(color, target);
    }
  }
}

/* Test certain moves to see whether they (too) can owl-attack or
 * defend an owl critical dragon. Tested moves are
 * 1. Strategical attacks or defenses for the dragon.
 * 2. Vital eye points for the dragon.
 * 3. Tactical attacks or defenses for a part of the dragon.
 * 4. Moves connecting the dragon to something else.
 */
static void
find_more_owl_attack_and_defense_moves(int color)
{
  int pos, pos2;
  int k;
  int dd = NO_MOVE;
  int worth_trying;
  int save_verbose;
  struct eye_data *our_eyes;
  struct eye_data *your_eyes;
  struct vital_eye_points *our_vital_points;
  struct vital_eye_points *your_vital_points;

  if (verbose)
    gprintf("\nTrying to upgrade strategical attack and defense moves.\n");

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
      
    for (k = 0; k < MAX_REASONS; k++) {
      int r = move[pos].reason[k];
      if (r < 0)
	break;

      do_find_more_owl_attack_and_defense_moves(color, pos,
						move_reasons[r].type,
						move_reasons[r].what);
    }
  }

  if (verbose)
    gprintf("\nTrying vital eye moves as owl attacks.\n");
  if (color == WHITE) {
    our_eyes = white_eye;
    your_eyes = black_eye;
    our_vital_points = white_vital_points;
    your_vital_points = black_vital_points;
  }
  else {
    our_eyes = black_eye;
    your_eyes = white_eye;
    our_vital_points = black_vital_points;
    your_vital_points = white_vital_points;
  }

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    if (our_eyes[pos].origin == pos
	&& our_vital_points[pos].defense_points[0] != NO_MOVE) {
      int k, dr;
      find_eye_dragons(pos, our_eyes, color, &dr, 1);
      for (k = 0; k < MAX_EYE_ATTACKS; k++) {
	int move = our_vital_points[pos].defense_points[k];
	if (move == NO_MOVE)
	  break;
	do_find_more_owl_attack_and_defense_moves(color, move,
						  VITAL_EYE_MOVE, dr);
      }
    }
    if (your_eyes[pos].origin == pos
	&& your_vital_points[pos].attack_points[0] != NO_MOVE) {
      int k, dr;
      find_eye_dragons(pos, your_eyes, OTHER_COLOR(color), &dr, 1);
      for (k = 0; k < MAX_EYE_ATTACKS; k++) {
	int move = your_vital_points[pos].attack_points[k];
	if (move == NO_MOVE)
	  break;
	do_find_more_owl_attack_and_defense_moves(color, move,
						  VITAL_EYE_MOVE, dr);
      }
    }
  }

  save_verbose = verbose;
  if (verbose > 0)
    verbose--;

  /* If two critical dragons are adjacent, test whether a move to owl
   * attack or defend one also is effective on the other.
   */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (IS_STONE(board[pos])
	&& dragon[pos].origin == pos
	&& DRAGON2(pos).owl_status == CRITICAL) {
      for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++) {
	if (board[pos2] != EMPTY)
	  continue;
	worth_trying = 0;
	for (k = 0; k < MAX_REASONS; k++) {
	  int r = move[pos2].reason[k];
	  
	  if (r < 0)
	    break;
	  if (move_reasons[r].type == OWL_ATTACK_MOVE
	      || move_reasons[r].type == OWL_ATTACK_MOVE_GOOD_KO
	      || move_reasons[r].type == OWL_ATTACK_MOVE_BAD_KO
	      || move_reasons[r].type == OWL_DEFEND_MOVE
	      || move_reasons[r].type == OWL_DEFEND_MOVE_GOOD_KO
	      || move_reasons[r].type == OWL_DEFEND_MOVE_BAD_KO) {
	    dd = move_reasons[r].what;
	    if (are_neighbor_dragons(dd, pos)) {
	      worth_trying = 1;
	      break;
	    }
	  } 
	  /* else ...
	     FIXME: what about the new OWL_ATTACK_MOVE_GAIN codes ?
	   */
	}

	if (worth_trying) {
	  if (board[pos] == color
	      && !owl_defense_move_reason_known(pos2, pos)) {
	    int kworm = NO_MOVE;
	    int dcode = owl_does_defend(pos2, pos, &kworm);
	    if (dcode >= DRAGON2(pos).owl_defense_code) {
	      if (dcode == LOSS)
		add_loss_move(pos2, pos, kworm);
	      else
		add_owl_defense_move(pos2, pos, dcode);
	      if (save_verbose)
	        gprintf("Move at %1m also owl defends %1m (%s).\n",
		        pos2, pos, result_to_string(dcode));
	    }

	  }
	  else if (board[pos] != color
		   && !owl_attack_move_reason_known(pos2, pos)) {
	    int kworm = NO_MOVE;
	    int acode = owl_does_attack(pos2, pos, &kworm);
	    if (acode >= DRAGON2(pos).owl_attack_code) {
	      add_owl_attack_move(pos2, pos, kworm, acode);
	      if (save_verbose)
	        gprintf("Move at %1m also owl attacks %1m (%s).\n",
		        pos2, pos, result_to_string(acode));
	    }
	  }
	}
      }
    }
  }

  verbose = save_verbose;
}

/* Tests whether the potential semeai move at (pos) with details given via
 * (*reason) works, and adds a semeai move if applicable.
 */
static void
try_potential_semeai_move(int pos, int color, struct move_reason *reason)
{
  int dr1 = semeai_target1[reason->what];
  int dr2 = semeai_target2[reason->what];
  int resulta, resultb, certain, old_certain;
  ASSERT1(IS_STONE(board[dr1]), pos);
  switch (reason->type) {
    case POTENTIAL_SEMEAI_ATTACK:
      owl_analyze_semeai_after_move(pos, color, dr1, dr2,
				    &resulta, &resultb, NULL,
				    1, &certain, 0);
      old_certain = DRAGON2(dr1).semeai_attack_certain;
      break;
    case POTENTIAL_SEMEAI_DEFENSE:
      old_certain = DRAGON2(dr1).semeai_defense_certain;
      /* In this case other dragon gets to move first after forced move. */
      owl_analyze_semeai_after_move(pos, color, dr2, dr1,
				    &resulta, &resultb, NULL,
				    1, &certain, 0);
      break;
    default:
      ASSERT1(0, pos);
  }
  if (resulta == 0 && resultb == 0
      && (certain || !old_certain)) {
    add_semeai_move(pos, dr1);
    DEBUG(DEBUG_SEMEAI,
	  "Potential semeai move at %1m for dragon at %1m is real\n",
	  pos, dr1);
  }
  else
    DEBUG(DEBUG_MOVE_REASONS, "Potential semeai move at %1m for %1m discarded\n",
	  pos, dr1);
}

/* This functions tests all potential semeai attack moves whether they work,
 * provided that there is at least one other move reasons stored for the
 * relevant position.
 */
static void
find_more_semeai_moves(int color)
{
  int pos;
  int save_verbose = verbose;

  if (verbose > 0)
    verbose--;

  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int k, r;
    int potential_semeai_move_found = 0;
    int other_move_reason_found = 0;

    if (!ON_BOARD1(pos))
      continue;
    for (k = 0; k < MAX_REASONS; k++) {
      r = move[pos].reason[k];
      if (r < 0)
	break;
      switch (move_reasons[r].type) {
	case POTENTIAL_SEMEAI_ATTACK:
	case POTENTIAL_SEMEAI_DEFENSE:
          potential_semeai_move_found = 1;
	  break;
	default:
	  other_move_reason_found = 1;
      }
    }
    if ((r < 0 || k == MAX_REASONS)
	&& !other_move_reason_found)
      continue;
    if (!potential_semeai_move_found)
      continue;

    for (k = 0; k < MAX_REASONS; k++) {
      int r = move[pos].reason[k];
      if (r < 0)
	break;
      if (move_reasons[r].type == POTENTIAL_SEMEAI_ATTACK
	  || move_reasons[r].type == POTENTIAL_SEMEAI_DEFENSE)
	try_potential_semeai_move(pos, color, &(move_reasons[r]));
    }
  }
  verbose = save_verbose;
}


/*
 * Any move that captures or defends a worm also potentially connects
 * or cuts the surrounding strings. Find these secondary move reasons
 * and verify them by connection reading.
 *
 * We also let an owl attack count as a strategical defense of our
 * neighbors of the owl attacked dragon. We only do this for
 * tactically safe dragons, however, because otherwise the effects of
 * capturing have already been taken into account elsewhere.
 *
 * Also, connecting moves played on inhibited points possibly remove
 * nearby connection inhibitions like in following example :
 * 
 * .OX.   The * move connects _all_ O stones together, not only
 * O...   the 2 lower ones.
 * XO*O
 * X.X.
 *
 */

static void
induce_secondary_move_reasons(int color)
{
  int pos;
  int k;
  int i, j;
  int aa;
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos))
      continue;
    
    for (k = 0; k < MAX_REASONS; k++) {
      int r = move[pos].reason[k];
      
      if (r < 0)
	break;
      
      if (move_reasons[r].type == ATTACK_MOVE
	  || move_reasons[r].type == DEFEND_MOVE) {
	int attack_move;
	int color_to_move;
	int num_adj, adjs[MAXCHAIN];
	
	aa = move_reasons[r].what;
	
	if (move_reasons[r].type == ATTACK_MOVE) {
	  attack_move = 1;
	  color_to_move = OTHER_COLOR(board[aa]);
	}
	else {
	  attack_move = 0;
	  color_to_move = board[aa];
	}
		
	if (worm[aa].defense_codes[0] == 0)
	  continue; /* No defense. */
	
	/* Don't care about inessential dragons. */
	if (DRAGON2(aa).safety == INESSENTIAL)
	  continue;
	
	/*
	 * If this is a defense move and the defense is futile for
	 * strategical reasons, we shouldn't induce a cutting move
	 * reason.
	 *
	 * FIXME: We may want to revise this policy.
	 */
	if (!attack_move && !move[pos].move_safety)
	  continue;
	
	num_adj = extended_chainlinks(aa, adjs, 1);
	
	for (i = 0; i < num_adj; i++) {
	  for (j = i+1; j < num_adj; j++) {
	    int adj1 = adjs[i];
	    int adj2 = adjs[j];
	    
	    if (board[adj1] != board[adj2])
	      continue;
	    if (attack_move
		&& board[adj1] != board[aa]
		&& !disconnect(adj1, adj2, NULL))
	      continue;
	    if (!attack_move
		&& board[adj1] != board[aa]
		&& !string_connect(adj1, adj2, NULL))
	      continue;
	    if (attack_move
		&& board[adj1] == board[aa])
	      continue;
	    if (!attack_move
		&& board[adj1] == board[aa]
		&& !disconnect(adj1, adj2, NULL))
	      continue;

	    if (trymove(pos, color_to_move, "induce_secondary_move_reasons",
			aa)) {
	      if (attack_move
		  && board[adj1] != board[aa]
		  && !disconnect(adj1, adj2, NULL)) {
		popgo();
		DEBUG(DEBUG_MOVE_REASONS,
		      "Connection move at %1m induced for %1m/%1m due to attack of %1m\n",
		      pos, adj1, adj2, aa);
		add_connection_move(pos, adj1, adj2);
		do_find_more_owl_attack_and_defense_moves(color, pos, CONNECT_MOVE,
							  find_connection(adj1, adj2));
	      }
	      else if (!attack_move
		       && board[adj1] != board[aa]
		       && !string_connect(adj1, adj2, NULL)) {
		popgo();
		DEBUG(DEBUG_MOVE_REASONS,
		      "Cut move at %1m induced for %1m/%1m due to defense of %1m\n",
		      pos, adj1, adj2, aa);
		add_cut_move(pos, adj1, adj2);
	      }
	      else if (!attack_move
		  && board[adj1] == board[aa]
		  && !disconnect(adj1, adj2, NULL)) {
		popgo();
		DEBUG(DEBUG_MOVE_REASONS,
		      "Connection move at %1m induced for %1m/%1m due to defense of %1m\n",
		      pos, adj1, adj2, aa);
		add_connection_move(pos, adj1, adj2);
		do_find_more_owl_attack_and_defense_moves(color, pos, CONNECT_MOVE,
							  find_connection(adj1, adj2));
	      }
	      else
		popgo();
	    }
	  }
	}

	/* Strategical attack move reason is induced for moves that
	 * defend neighbor strings of weak opponent dragons a.  We
	 * only count strings that are large (more than three stones)
	 * or adjoin at least two non-dead non-single-stone opponent
	 * dragons.
	 */
	if (!attack_move) {
	  int strategically_valuable = (worm[aa].size > 3);
	  signed char neighbor_dragons[BOARDMAX];

	  memset(neighbor_dragons, 0, sizeof(neighbor_dragons));

	  if (!strategically_valuable) {
	    int num_dragons = 0;

	    for (i = 0; i < num_adj; i++) {
	      int origin = dragon[adjs[i]].origin;

	      if (board[origin] != color_to_move
		  && neighbor_dragons[origin] != 1
		  && dragon[origin].size > 1
		  && dragon[origin].status != DEAD) {
		if (++num_dragons == 2) {
		  strategically_valuable = 1;
		  break;
		}

		neighbor_dragons[origin] = 1;
	      }
	    }
	  }

	  if (strategically_valuable) {
	    for (i = 0; i < num_adj; i++) {
	      int origin = dragon[adjs[i]].origin;

	      if (board[origin] != color_to_move
		  && neighbor_dragons[origin] != 2
		  && dragon[origin].status != DEAD
		  && dragon_weak(origin)) {
		DEBUG(DEBUG_MOVE_REASONS,
		      "Strategical attack move at %1m induced for %1m due to defense of %1m\n",
		      pos, origin, aa);
		add_strategical_attack_move(pos, origin);
		do_find_more_owl_attack_and_defense_moves(color, pos,
							  STRATEGIC_ATTACK_MOVE,
							  origin);

		neighbor_dragons[origin] = 2;
	      }
	    }
	  }
	}
      }
      else if (move_reasons[r].type == OWL_ATTACK_MOVE) {
	aa = move_reasons[r].what;
	for (i = 0; i < DRAGON2(aa).neighbors; i++) {
	  int bb = dragon2[DRAGON2(aa).adjacent[i]].origin;
	  if (dragon[bb].color == color && worm[bb].attack_codes[0] == 0
	      && !DRAGON2(bb).semeais) {
	    add_strategical_defense_move(pos, bb);
	    do_find_more_owl_attack_and_defense_moves(color, pos,
						      STRATEGIC_DEFEND_MOVE,
						      bb);
	    DEBUG(DEBUG_MOVE_REASONS, "Strategic defense at %1m induced for %1m due to owl attack on %1m\n",
		  pos, bb, aa);
	  }
	}
      }
      else if (move_reasons[r].type == CONNECT_MOVE
	       && cut_possible(pos, OTHER_COLOR(color))) {
	int worm1 = conn_worm1[move_reasons[r].what];
	int worm2 = conn_worm2[move_reasons[r].what];
	int pos2;
	for (pos2 = BOARDMIN; pos2 < BOARDMAX; pos2++) {
	  if (ON_BOARD(pos2) && board[pos2] == EMPTY
	      && cut_possible(pos2, OTHER_COLOR(color))
	      && square_dist(pos, pos2) <= 5) {
	    for (j = 0; j < 8; j++) {
	      int pos3 = pos2 + delta[j];
	      if (ON_BOARD(pos3) && board[pos3] == color
		  && !is_same_worm(pos3, worm1)
		  && !is_same_worm(pos3, worm2)) {
		if (trymove(pos, color, "induce_secondary_move_reasons-B",
			    worm1)) {
		  int break1 = disconnect(pos3, worm1, NULL);
		  int break2 = disconnect(pos3, worm2, NULL);
		  popgo();
		  
		  if (!break1) {
		    add_connection_move(pos, pos3, worm1);
		    do_find_more_owl_attack_and_defense_moves(color, pos, CONNECT_MOVE,
							      find_connection(pos3, worm1));
		    DEBUG(DEBUG_MOVE_REASONS, "Connection at %1m induced for %1m/%1m due to connection at %1m/%1m\n",
			  pos, worm1, worm2, pos3, worm1);
		  }
		  
		  if (!break2) {
		    add_connection_move(pos, pos3, worm2);
		    do_find_more_owl_attack_and_defense_moves(color, pos, CONNECT_MOVE,
							      find_connection(pos3, worm2));
		    DEBUG(DEBUG_MOVE_REASONS, "Connection at %1m induced for %1m/%1m due to connection at %1m/%1m\n",
			  pos, worm1, worm2, pos3, worm2);
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }
}


/* Examine the strategical and tactical safety of the moves. This is
 * used to decide whether or not the stone should generate influence
 * when the move is evaluated. The idea is to avoid overestimating the
 * value of strategically unsafe defense moves and connections of dead
 * dragons. This sets the move.move_safety field.
 */
static void
examine_move_safety(int color)
{
  int pos;
  int k;
  
  start_timer(3);
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int safety = 0;
    int tactical_safety = 0;
    if (!ON_BOARD(pos))
      continue;
    tactical_safety = is_known_safe_move(pos);
      
    for (k = 0; k < MAX_REASONS; k++) {
      int r = move[pos].reason[k];
      int type;
      int what;
      
      if (r == -1)
	break;
      type = move_reasons[r].type;
      what = move_reasons[r].what;
      switch (type) {
      case CUT_MOVE:
	/* We don't trust cut moves, unless some other move reason
	 * indicates they are safe.
	 */
	break;
      case OWL_DEFEND_MOVE:
      case OWL_DEFEND_MOVE_GOOD_KO:
      case OWL_DEFEND_MOVE_BAD_KO:
      case OWL_DEFEND_MOVE_LOSS:
	{
	  int ii;
	  for (ii = first_worm_in_dragon(what); ii != NO_MOVE; 
	       ii = next_worm_in_dragon(ii)) {
	    if (!play_connect_n(color, 0, 1, pos, ii, pos))
	      break;
	  }
	  if (ii != NO_MOVE) {
	    tactical_safety = 1;
	    safety = 1;
	  }
	  break;
	}
      case SEMEAI_MOVE:
      case MY_ATARI_ATARI_MOVE:
      case YOUR_ATARI_ATARI_MOVE:
      case EITHER_MOVE:         /* FIXME: More advanced handling? */
	tactical_safety = 1;
	safety = 1;
	break;
      case ALL_MOVE:
	/* We don't trust these, unless some other move reason
	 * indicates safety.
	 */
	break;
      case EXPAND_TERRITORY_MOVE:
      case EXPAND_MOYO_MOVE:
      case INVASION_MOVE:   /* A real invasion should be safe.
			       A sacrifice is something else.*/
        safety = 1;
	break;
      case ATTACK_MOVE:
      case ATTACK_MOVE_GOOD_KO:
      case ATTACK_MOVE_BAD_KO:
      case OWL_ATTACK_MOVE:
      case OWL_ATTACK_MOVE_GOOD_KO:
      case OWL_ATTACK_MOVE_BAD_KO:
      case OWL_ATTACK_MOVE_GAIN:
        {
	  int aa = NO_MOVE;
	  int bb = NO_MOVE;
	  int size;
	  int m;
	  int our_color_neighbors;
	  
	  if (type == ATTACK_MOVE
	      || type == ATTACK_MOVE_GOOD_KO
	      || type == ATTACK_MOVE_BAD_KO) {
	    aa = what;
	    size = worm[aa].effective_size;
	  } 
	  else if (type == OWL_ATTACK_MOVE_GAIN) {
	    aa = either_data[what].what2;
	    size = worm[aa].effective_size;
	  } 
	  else {
	    aa = what;
	    size = dragon[aa].effective_size;
	  }
	  
	  /* No worries if we catch something big. */
	  if (size >= 8) {
	    tactical_safety = 1;
	    safety = 1;
	    break;
	  }
	  
	  /* If the victim has multiple neighbor dragons of our
	   * color, we leave it to the connection move reason to
	   * determine safety.
	   *
	   * The exception is an owl_attack where we only require
	   * one neighbor to be alive.
	   */
	  our_color_neighbors = 0;
	  if (type == ATTACK_MOVE
	      || type == ATTACK_MOVE_GOOD_KO
	      || type == ATTACK_MOVE_BAD_KO) {
	    /* We could use the same code as for OWL_ATTACK_MOVE
	     * below if we were certain that the capturable string
	     * had not been amalgamated with a living dragon.
	     */
	    int num_adj, adjs[MAXCHAIN];
	    
	    num_adj = chainlinks(aa, adjs);
	    for (m = 0; m < num_adj; m++) {
	      int adj = adjs[m];
	      
	      if (board[adj] == color) {
		/* Check whether this string is part of the same
		 * dragon as an earlier string. We only want to
		 * count distinct neighbor dragons.
		 */
		int n;
		
		for (n = 0; n < m; n++)
		  if (dragon[adjs[n]].id == dragon[adj].id)
		    break;
		if (n == m) {
		  /* New dragon. */
		  our_color_neighbors++;
		  bb = adj;
		}
	      }
	    }
	  }
	  else {
	    for (m = 0; m < DRAGON2(aa).neighbors; m++)
	      if (DRAGON(DRAGON2(aa).adjacent[m]).color == color) {
		our_color_neighbors++;
		bb = dragon2[DRAGON2(aa).adjacent[m]].origin;
		if (dragon[bb].status == ALIVE) {
		  tactical_safety = 1;
		  safety = 1;
		}
	      }
	  }
	  
	  if (our_color_neighbors > 1)
	    break;
	  
	  /* It may happen in certain positions that no neighbor of
	   * our color is found. The working hypothesis is that
	   * the move is safe then. One example is a position like
	   *
	   * ----+
	   * OX.X|
	   * OOX.|
	   *  OOX|
	   *   OO|
	   *
	   * where the top right stone only has friendly neighbors
	   * but can be attacked.
	   *
	   * As a further improvement, we also look for a friendly
	   * dragon adjacent to the considered move.
	   */
	  
	  for (m = 0; m < 4; m++) {
	    int d = delta[m];
	    if (board[pos+d] == color) {
	      bb = pos + d;
	      break;
	    }
	  }
	  
	  if (bb == NO_MOVE) {
	    tactical_safety = 1;
	    safety = 1;
	    break;
	  }
	  
	  /* If the attacker is thought to be alive, we trust that
	   * sentiment.
	   */
	  if (dragon[bb].status == ALIVE) {
	    tactical_safety = 1;
	    safety = 1;
	    break;
	  }
	  
	  /* It remains the possibility that what we have captured
	   * is just a nakade shape. Ask the owl code whether this
	   * move saves our attacking dragon.
	   *
	   * FIXME: Might need to involve semeai code too here.
	   */
	  if (owl_does_defend(pos, bb, NULL)) {
	    tactical_safety = 1;
	    safety = 1;
	  }
	  break;
	}
      case DEFEND_MOVE:
      case DEFEND_MOVE_GOOD_KO:
      case DEFEND_MOVE_BAD_KO:
	{
	  int aa = what;
	  
	  if (dragon[aa].status == ALIVE)
	    /* It would be better if this never happened, but it does
	     * sometimes. The owl reading can be very slow then.
	     */
	    safety = 1;
	  
	  else if (!play_connect_n(color, 0, 1, pos, aa, pos)
		   && owl_does_defend(pos, aa, NULL))
	    safety = 1;
	  break;
	}
	
      case ATTACK_THREAT:
      case DEFEND_THREAT:
	break;

      case CONNECT_MOVE:
        {
	  int worm1 = conn_worm1[move_reasons[r].what];
	  int worm2 = conn_worm2[move_reasons[r].what];
	  int aa = dragon[worm1].origin;
	  int bb = dragon[worm2].origin;

	  if (aa == bb)
	    continue;

	  if (DRAGON2(aa).owl_status == ALIVE
	      || DRAGON2(bb).owl_status == ALIVE) {
	    tactical_safety = 1;
	    safety = 1;
	  }
	  else if ((DRAGON2(aa).owl_status == UNCHECKED
		    && dragon[aa].crude_status == ALIVE)
		   || (DRAGON2(bb).owl_status == UNCHECKED
		       && dragon[bb].crude_status == ALIVE)) {
	    tactical_safety = 1;
	    safety = 1;
	  }
	  else if (owl_connection_defends(pos, aa, bb)) {
	    tactical_safety = 1;
	    safety = 1;
	  }
	  break;
	}
      }
      if (safety == 1 && (tactical_safety == 1 || safe_move(pos, color)))
	break;
    }
      
    if (safety == 1 && (tactical_safety || safe_move(pos, color)))
      move[pos].move_safety = 1;
    else
      move[pos].move_safety = 0;
    
    time_report(3, "    examine_move_safety: ", pos, 1.0);
  }
}


/*
 * Returns the pre-computed weakness of a dragon, with corrections
 * according to ignore_dead_dragons.
 *
 * FIXME: Important to test more exactly how effective a strategical
 *        attack or defense of a weak dragon is. This can be done by
 *        measuring escape factor and moyo size after the move and
 *        compare with the old values. Also necessary to test whether
 *        an attack or defense of a critical dragon is effective.
 *        Notice that this wouldn't exactly go into this function but
 *        rather where it's called.
 */

float
dragon_weakness(int dr, int ignore_dead_dragons)
{
  int dragon_safety = DRAGON2(dr).safety;

  /* Kludge: If a dragon is dead, we return 1.0 in order not
   * to try to run away.
   */
  if (ignore_dead_dragons
      && (dragon_safety == DEAD
	  || dragon_safety == INESSENTIAL
	  || dragon_safety == TACTICALLY_DEAD))
    return 0.0;

  /* When scoring, we don't want to reinforce ALIVE dragons. */
  if (doing_scoring && dragon_safety == ALIVE)
    return 0.0;
  
  return DRAGON2(dr).weakness;
}

/*
 * Strategical value of connecting (or cutting) the dragon at (dragona)
 * to the dragon at (dragonb). Notice that this function is asymmetric.
 * This is because connection_value(a, b) is intended to measure the
 * strategical value on the a dragon from a connection to the b dragon.
 * 
 * Consider the following position:
 * +---------+
 * |XXO.O.OXX|
 * |.XOOOOOX.|
 * |XXXX.XXXX|
 * |.XOOXOOX.|
 * |XXO.X.O.X|
 * |OOOXXXOOO|
 * |..OOOOO..|
 * |.........|
 * +---------+
 * 
 * X has three dragons, one invincible to the left (A), one critical to
 * the right (B), and one dead in the center (C). The move at the cutting
 * point has three move reasons:
 * connect A and B
 * connect A and C
 * connect B and C
 * 
 * The strategical value on A of either connection is of course zero,
 * since it's very unconditionally alive. The strategical value on B is
 * high when it's connected to A but small (at least should be) from the
 * connection to C. Similarly for dragon C. In effect the total
 * strategical value of this move is computed as:
 * 
 * max(connection_value(A, B), connection_value(A, C))
 * + max(connection_value(B, A), connection_value(B, C))
 * + max(connection_value(C, A), connection_value(C, B))
 *
 * The parameter 'margin' is the margin by which we are ahead.
 * If this exceeds 20 points we value connections more.  This is because
 * we can afford to waste a move making sure of safety.
 */

static float
connection_value(int dragona, int dragonb, int tt, float margin)
{
  struct dragon_data2 *da = &DRAGON2(dragona);
  struct dragon_data2 *db = &DRAGON2(dragonb);
  float sizea = da->strategic_size;
  float sizeb = db->strategic_size;
  int safetya = da->safety;
  int safetyb = db->safety;
  float crude_weakness_a
    = crude_dragon_weakness(da->safety, &da->genus, da->lunch != NO_MOVE, 
			    da->moyo_territorial_value,
			    (float) da->escape_route);
  float crude_weakness_sum;
  struct eyevalue genus_sum;
  float terr_val = move[tt].territorial_value;
  float return_value;

  if (margin > 20.0)
    margin = 20.0;

  /* When scoring, we want to be restrictive with reinforcement moves.
   * Thus if both dragons are alive, strongly alive, or invincible, no
   * bonus is awarded.
   *
   * FIXME: Shouldn't it be sufficient to check this for dragon a?
   */
  if (doing_scoring) {
    if ((safetya == ALIVE
	 || safetya == STRONGLY_ALIVE
	 || safetya == INVINCIBLE)
	&& (safetyb == ALIVE
	    || safetyb == STRONGLY_ALIVE
	    || safetyb == INVINCIBLE))
      return 0.0;
  }

  if (safetyb == INESSENTIAL)
    return 0.0;
  
  if (crude_weakness_a == 0.0
      || dragon[dragona].status == DEAD)
    return 0.0;
  if (terr_val < 0.0)
    terr_val = 0.0;

  add_eyevalues(&da->genus, &db->genus, &genus_sum);
  /* FIXME: There is currently no sane way to take the escape values
   * into account. Hence we simply pretend they do not change.
   *
   * FIXME: terr_val is a very crude approximation to the expected
   * increase in moyo size. It's especially way off if the move at (tt)
   * (owl) defends some stones.
   */
  crude_weakness_sum
    = crude_dragon_weakness(safetyb, &genus_sum,
			    (da->lunch != NO_MOVE || db->lunch != NO_MOVE), 
			    da->moyo_territorial_value
			    + db->moyo_territorial_value
			    + terr_val,
			    (float) da->escape_route);

  /* Kludge: For a CRITICAL dragon, we use the usual effective
   * size and give a strategic effect bigger than 2.0 * effective size.
   * This is to match the "strategic bonus computation" in
   * estimate_strategical_value(). This prefers connection moves that
   * owl defend a dragon to other owl defense move.
   */
  if (dragon[dragona].status == CRITICAL) {
    float bonus = (0.4 - 0.3 * crude_weakness_sum) * sizea;

    if (bonus < 0.0)
      bonus = 0.0;

    /* If ahead, give extra bonus to connections. */
    if (margin > 0.0 && bonus > 0.0)
      bonus *= 1.0 + 0.05 * margin;
    return_value = 2.0 * sizea + bonus;
  }
  else {
    float old_burden = 2.0 * crude_weakness_a * soft_cap(sizea, 15.0);

    /* The new burden is the burden of defending new joint dragon; but
     * we share this burden proportionally with the other dragon.
     */
    float new_burden = 2.0 * crude_weakness_sum * soft_cap(sizea + sizeb, 15.0)
		       * sizea / (sizea + sizeb);

    return_value = 1.05 * (old_burden - new_burden);
    /* If ahead, give extra bonus to connections. */
    if (margin > 0.0)
      return_value *= 1.0 + 0.02 * margin;
  }

  if (return_value < 0.0)
    return_value = 0.0;

  return return_value;
}


/* 
 * This function computes the shape factor, which multiplies
 * the score of a move. We take the largest positive contribution
 * to shape and add 1 for each additional positive contribution found.
 * Then we take the largest negative contribution to shape, and
 * add 1 for each additional negative contribution. The resulting
 * number is raised to the power 1.05.
 *
 * The rationale behind this complicated scheme is that every
 * shape point is very significant. If two shape contributions
 * with values (say) 5 and 3 are found, the second contribution
 * should be devalued to 1. Otherwise the engine is too difficult to
 * tune since finding multiple contributions to shape can cause
 * significant overvaluing of a move.
 */

static float
compute_shape_factor(int pos)
{
  float exponent = move[pos].maxpos_shape - move[pos].maxneg_shape;

  ASSERT_ON_BOARD1(pos);
  if (move[pos].numpos_shape > 1)
    exponent += move[pos].numpos_shape - 1;
  if (move[pos].numneg_shape > 1)
    exponent -= move[pos].numneg_shape - 1;
  return pow(1.05, exponent);
}


/*
 * Usually the value of attacking a worm is twice its effective size,
 * but when evaluating certain move reasons we need to adjust this to
 * take effects on neighbors into account, e.g. for an attack_either
 * move reason. This does not apply to the attack and defense move
 * reasons, however, because then the neighbors already have separate
 * attack or defense move reasons (if such apply).
 *
 * If the worm has an adjacent (friendly) dead dragon we add its
 * value. At least one of the surrounding dragons must be alive. 
 * If not, the worm must produce an eye of sufficient size, and that 
 * should't be accounted for here.  As a guess, we suppose that
 * a critical dragon is alive for our purpose here.
 *
 * On the other hand if it has an adjacent critical worm, and
 * if (pos) does not defend that worm, we subtract the value of the
 * worm, since (pos) may be defended by attacking that worm. We make at
 * most one adjustment of each type.
 */

static float
adjusted_worm_attack_value(int pos, int ww)
{
  int num_adj;
  int adjs[MAXCHAIN];
  int has_live_neighbor = 0;
  float adjusted_value = 2 * worm[ww].effective_size;
  float adjustment_up = 0.0;
  float adjustment_down = 0.0;
  int s;

  num_adj = chainlinks(ww, adjs);
  for (s = 0; s < num_adj; s++) {
    int adj = adjs[s];

    if (dragon[adj].status != DEAD)
      has_live_neighbor = 1;

    if (dragon[adj].status == DEAD
	&& 2*dragon[adj].effective_size > adjustment_up)
      adjustment_up = 2*dragon[adj].effective_size;

    if (worm[adj].attack_codes[0] != 0
	&& !does_defend(pos, adj)
	&& 2*worm[adj].effective_size > adjustment_down)
      adjustment_down = 2*worm[adj].effective_size;
  }

  if (has_live_neighbor)
    adjusted_value += adjustment_up;
  adjusted_value -= adjustment_down;

  /* It can happen that the adjustment down was larger than the effective
   * size we started with. In this case we simply return 0.0. (This means
   * we ignore the respective EITHER_MOVE reason.)
   */
  if (adjusted_value > 0.0)
    return adjusted_value;
  else
    return 0.0;
}


/* The new (3.2) territorial evaluation overvalues moves creating a new
 * group in the opponent's sphere of influence. The influence module cannot
 * see that the opponent will gain by attacking the new (probably weak)
 * group.
 * This function uses some heuristics to estimate the strategic penalty
 * of invasion moves, and moves that try to run away with a group of size
 * 1 in front of opponent's strength.
 */
static float
strategic_penalty(int pos, int color)
{
  int k;
  float ret_val;
  
  /* We try to detect support from an alive friendly stone by checking
   * whether all neighboring intersections belong to the opponent's moyo.
   */
  for (k = 0; k < 4; k++)
    if (board[pos + delta[k]] == EMPTY
        && whose_area(OPPOSITE_INFLUENCE(color), pos + delta[k])
	   != OTHER_COLOR(color))
      return 0.0;
  if (whose_area(OPPOSITE_INFLUENCE(color), pos) != OTHER_COLOR(color))
    return 0.0;

  for (k = 0; k < MAX_REASONS; k++) {
    int r = move[pos].reason[k];
    if (r < 0)
      break;
    /* We assume that invasion moves can only have the move reasons listed
     * below.
     *
     * FIXME: EXPAND_TERRITORY should always be connected to our own
     *        stones.  Remove later when that change is done.
     */
    switch (move_reasons[r].type) {
#if 0
    case EXPAND_TERRITORY_MOVE:
#endif
    case EXPAND_MOYO_MOVE:
    case STRATEGIC_ATTACK_MOVE:
    case INVASION_MOVE:
      continue;
    /* If we find a tactical defense move, we just test whether it concerns
     * a single-stone-dragon; if not, we stop, if yes, we let the necessary
     * tests be made in the OWL_DEFEND_MOVE case.
     */
    case DEFEND_MOVE:
      {
	int target = move_reasons[r].what;
	if (dragon[target].size > 1)
	  return 0.0;
	continue;
      }
    /* An owl defense of a single stone might be a stupid attempt to run
     * away with an unimportant (kikashi like) stone. We assume this is the
     * case if this single stone has a strong hostile direct neighbor.
     */
    case OWL_DEFEND_MOVE:
      {
	int target = move_reasons[r].what;
	int has_strong_neighbor = 0;
	int has_weak_neighbor = 0;
	int i;
	/* We award no penalty for running away with a cutting stone. */
	if (dragon[target].size > 1
	    || worm[target].cutstone > 0
	    || worm[target].cutstone2 > 0)
	  return 0.0;
	/* Third line moves (or lower) are ok -- they try to live, not run
         * away.
         */
        if (edge_distance(pos) < 3)
	  return 0.0;
	
	for (i = 0; i < 4; i++)
	  if (board[target + delta[i]] == OTHER_COLOR(color)) {
	    if (dragon[target + delta[i]].size == 1) {
	      has_weak_neighbor = 1;
	      break;
	    }
	    switch (DRAGON2(target + delta[i]).safety) {
	    case INVINCIBLE:
	    case STRONGLY_ALIVE:
	      has_strong_neighbor = 1;
	      break;
	    case ALIVE:
	      if (DRAGON2(target + delta[i]).weakness > 0.4)
		has_weak_neighbor = 1;
	      break;
	    default:
	      has_weak_neighbor = 1;
	    }
	  }
	if (has_weak_neighbor || (!has_strong_neighbor))
	  return 0.0;
	else
	  continue;
      }
    default:
      return 0.0;
    }  
  }

  /* We have to make a guess how much the point where we want to play
   * is dominated by the opponent. The territorial valuation is a
   * good try here.
   */
  ret_val = influence_territory(INITIAL_INFLUENCE(OTHER_COLOR(color)),
      				pos, OTHER_COLOR(color));
  ret_val *= 12.0;
  ret_val = gg_max(0.0, ret_val);
  return ret_val;
}


/* True if pos is adjacent to a nondead stone of the given color. This
 * function can be called when stackp>0 but the result is given for
 * the position when stackp==0. It also checks for nondead stones two
 * steps away from pos if a move by color at pos cannot be cut off
 * from that stone.
 *
 * FIXME: Move this somewhere more generally accessible, probably
 *        utils.c
 */
int
adjacent_to_nondead_stone(int pos, int color)
{
  int k;

  int stack[MAXSTACK];
  int move_color[MAXSTACK];
  int saved_stackp = stackp;
  int result = 0;

  while (stackp > 0) {
    get_move_from_stack(stackp - 1, &stack[stackp - 1],
			&move_color[stackp - 1]);
    popgo();
  }

  if (trymove(pos, color, NULL, EMPTY)) {
    for (k = 0; k < 12; k++) {
      int pos2;
      if (k < 8)
	pos2 = pos + delta[k];
      else if (ON_BOARD(pos + delta[k - 8]))
	pos2 = pos + 2 * delta[k - 8];
      else
	continue;
      
      if (ON_BOARD(pos2)
	  && worm[pos2].color == color
	  && dragon[pos2].status != DEAD
	  && !disconnect(pos, pos2, NULL)) {
	result = 1;
	break;
      }
    }
    popgo();
  }

  while (stackp < saved_stackp)
    tryko(stack[stackp], move_color[stackp], NULL);
  
  return result;
}

static int
max_lunch_eye_value(int pos)
{
  int min;
  int probable;
  int max;
  
  estimate_lunch_eye_value(pos, &min, &probable, &max, 0);
  return max;
}

/*
 * Estimate the direct territorial value of a move at (pos) by (color).
 */
static void
estimate_territorial_value(int pos, int color, float our_score,
			   int disable_delta_territory_cache)
{
  int other = OTHER_COLOR(color);
  int k;
  int aa = NO_MOVE;
  int bb = NO_MOVE;
  
  float this_value = 0.0;
  float tot_value = 0.0;
  float secondary_value = 0.0;

  int does_block = 0;
  signed char safe_stones[BOARDMAX];
  float strength[BOARDMAX];

  set_strength_data(OTHER_COLOR(color), safe_stones, strength);
  
  for (k = 0; k < MAX_REASONS; k++) {
    int r = move[pos].reason[k];
    if (r < 0)
      break;
    if (move_reasons[r].status & TERRITORY_REDUNDANT)
      continue;

    this_value = 0.0;
    switch (move_reasons[r].type) {
    case ATTACK_MOVE:
    case ATTACK_MOVE_GOOD_KO:
    case ATTACK_MOVE_BAD_KO:
      aa = move_reasons[r].what;
      
      ASSERT1(board[aa] != color, aa);
      
      /* Defenseless stone. */
      if (worm[aa].defense_codes[0] == 0) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %1m:   %f (secondary) - attack on %1m (defenseless)\n",
	      pos, worm[aa].effective_size, aa);
	secondary_value += worm[aa].effective_size;
	does_block = 1;
	break;
      }

      this_value = 2 * worm[aa].effective_size;

      /* If the stones are dead, there is only a secondary value in
       * capturing them tactically as well.
       */
      if (dragon[aa].status == DEAD) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %1m:   %f (secondary) - attack on %1m (dead)\n",
	      pos, 0.2 * this_value, aa);
	secondary_value += 0.2 * this_value;
	does_block = 1;
	break;
      }

      /* Mark the string as captured, for evaluation in the influence code. */
      mark_changed_string(aa, safe_stones, strength, 0);
      TRACE("  %1m: attack on worm %1m\n", pos, aa);
      
      /* FIXME: How much should we reduce the value for ko attacks? */
      if (move_reasons[r].type == ATTACK_MOVE)
	this_value = 0.0;
      else if (move_reasons[r].type == ATTACK_MOVE_GOOD_KO) {
	this_value *= 0.3;
	TRACE("  %1m: -%f - attack on worm %1m only with good ko\n",
	      pos, this_value, aa);
      }	
      else if (move_reasons[r].type == ATTACK_MOVE_BAD_KO) {
	this_value *= 0.5;
	TRACE("  %1m: -%f - attack on worm %1m only with bad ko\n",
	      pos, this_value, aa);
      }
      
      tot_value -= this_value;
      does_block = 1;
      break;
      
    case DEFEND_MOVE:
    case DEFEND_MOVE_GOOD_KO:
    case DEFEND_MOVE_BAD_KO:
      aa = move_reasons[r].what;
      
      ASSERT1(board[aa] == color, aa);
      
      /* 
       * Estimate value 
       */
      this_value = 2 * worm[aa].effective_size;

      /* If the stones are dead, we use the convention that
       * defending them has a strategical value rather than
       * territorial. Admittedly this make more sense for attacks on
       * dead stones.
       */
      if (dragon[aa].status == DEAD) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %1m:   %f (secondary) - defense of %1m (dead)\n",
	      pos, 0.2 * this_value, aa);
	secondary_value += 0.2 * this_value;
	break;
      }
      
      if (DRAGON2(aa).owl_status == CRITICAL
	  && (owl_defense_move_reason_known(pos, aa)
	      < defense_move_reason_known(pos, aa))
	  && !semeai_move_reason_known(pos, aa)) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "  %1m:   %f (secondary) - ineffective defense of %1m (critical)\n",
	      pos, 0.2 * this_value, aa);
	secondary_value += 0.2 * this_value;
	break;
      }
      
      /* Mark the string as saved, for evaluation in the influence code. */
      mark_changed_string(aa, safe_stones, strength, INFLUENCE_SAVED_STONE);
      TRACE("  %1m: defense of worm %1m\n", pos, aa);
      
      /* FIXME: How much should we reduce the value for ko defenses? */
      if (move_reasons[r].type == DEFEND_MOVE)
	this_value = 0.0;
      else if (move_reasons[r].type == DEFEND_MOVE_GOOD_KO) {
	this_value *= 0.3;
	TRACE("  %1m: -%f - defense of worm %1m with good ko\n",
	      pos, this_value, aa);
      }	
      else if (move_reasons[r].type == DEFEND_MOVE_BAD_KO) {
	this_value *= 0.5;
	TRACE("  %1m: -%f - defense of worm %1m with bad ko\n",
	      pos, this_value, aa);
      }	
    
      tot_value -= this_value;

      /* If a move tactically defends an owl critical string, but
       * this move is not listed as an owl defense, it probably is
       * ineffective. The 0.45 factor is chosen so that even in
       * combination with bad ko it still has a positive net impact.
       */
      if (DRAGON2(aa).owl_status == CRITICAL
	  && (owl_defense_move_reason_known(pos, aa)
	      < defense_move_reason_known(pos, aa))) {
	this_value = 0.45 * (2 * worm[aa].effective_size);
	TRACE("  %1m: -%f - suspected ineffective defense of worm %1m\n",
	      pos, this_value, aa);
	tot_value -= this_value;
      }

      does_block = 1;
      break;

    case ATTACK_THREAT:
      aa = move_reasons[r].what;

      /* Make sure this is a threat to attack opponent stones. */
      ASSERT1(board[aa] == other, aa);
      
      if (dragon[aa].status == DEAD) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "    %1m: 0.0 - threatens to capture %1m (dead)\n", pos, aa);
	break;
      }

      /* The followup value of a move threatening to attack (aa)
       * is twice its effective size, with adjustments. If the
       * worm has an adjacent (friendly) dead dragon we add its
       * value. On the other hand if it has an adjacent critical
       * worm, and if (pos) does not defend that worm, we subtract
       * the value of the worm, since (aa) may be defended by
       * attacking that worm. We make at most one adjustment
       * of each type.
       *
       * No followup value is awarded if the defense move is a threat
       * back on our move because we're likely to end in gote then,
       * unless the move is unsafe anyway and played as a ko threat.
       *
       * FIXME: It might be possible that parts of the dragon
       *        can be cut in the process of capturing the (aa)
       *        worm. In that case, not the entire size of the 
       *        adjacent dead dragon should be counted as a positive
       *        adjustment.  However, it seems difficult to do this
       *        analysis, and in most cases it won't apply, so we
       *        leave it as it is for now.
       *
       * FIXME: The same analysis should be applied to
       *        DEFEND_THREAT,
       *        ATTACK_EITHER_MOVE, DEFEND_BOTH_MOVE. It should be 
       *        broken out as separate functions and dealt with in
       *        a structured manner.
       */

      if (trymove(pos, color, "estimate_territorial_value-A", NO_MOVE)) {
	int adjs[MAXCHAIN];
	float adjusted_value = 2 * worm[aa].effective_size;
	float adjustment_up = 0.0;
	float adjustment_down = 0.0;
	int s;
	int num_adj;
	int defense_move;

	/* In rare cases it may happen that the trymove() above
         * actually removed the string at aa.
	 */
	if (board[aa] == EMPTY)
	  num_adj = 0;
	else
	  num_adj = chainlinks(aa, adjs);

	/* No followup value if string can be defended with threat
         * against our move. An exception to this is when our move
         * isn't safe anyway and we play this only for the followup
         * value, typically as a ko threat. Though, "suspicious" owl 
	 * defenses (move_safety != 1) are still tested for possible
	 * backfires.
	 *
	 * This rule may be overwritten with patterns. See pattern
	 * Sente22 and related test trevord:950 for an example.
	 *
	 * FIXME: This is somewhat halfhearted since only one defense
	 * move is tested.
	 */
	if (!is_known_good_attack_threat(pos, aa)
	    && board[aa] != EMPTY
	    && (move[pos].move_safety == 1
		|| adjacent_to_nondead_stone(pos, color)
		|| owl_defense_move_reason_known(pos, -1))
	    && find_defense(aa, &defense_move) == WIN
	    && defense_move != NO_MOVE) {
	  int bad_followup;
	  int attack_move;

	  if (attack(pos, &attack_move) != WIN) {
	    int i;

	    if (trymove(defense_move, other,
			"estimate_territorial_value-b", NO_MOVE)) {
	      if (board[pos] == EMPTY || attack(pos, NULL) != 0) {
		popgo();
		popgo();
		break;
	      }

	      /* Now check all `ATTACK_MOVE' reasons for this same
	       * move.  If the defense against current threat makes a
	       * string attacked by this move defendable, we reduce
	       * the followup.
	       *
	       * Adjustments done later are concerned with current
	       * dragon states.  Here we actually try to check if
	       * opponent's reply to our move will have a followup in
	       * turn.
	       */
	      for (i = 0; i < MAX_REASONS; i++) {
		int reason = move[pos].reason[i];
		int attacked_string;
		if (reason < 0)
		  break;

		attacked_string = move_reasons[reason].what;
		if (move_reasons[reason].type == ATTACK_MOVE
		    && board[attacked_string] == other) {
		  int defense_code = find_defense(attacked_string, NULL);
		  double down_coefficient = 0.0;

		  switch (defense_code) {
		  case WIN:
		    down_coefficient = 2.0;
		    break;

		  case KO_A:
		    down_coefficient = 2.0 * 0.5;
		    break;

		  case KO_B:
		    down_coefficient = 2.0 * 0.7;
		    break;
		  }

		  if (adjustment_down
		      < (worm[attacked_string].effective_size
			 * down_coefficient)) {
		    adjustment_down = (worm[attacked_string].effective_size
				       * down_coefficient);
		  }
		}
	      }

	      popgo();
	    }
	  }
	  else {
	    /* Our move is attackable to begin with.  However, maybe
	     * the attack is not sufficient to defend opponent's
	     * string?
	     */
	    if (trymove(attack_move, other,
			"estimate_territorial_value-c", NO_MOVE)) {
	      if (attack(aa, NULL) == 0) {
		/* It is sufficient, no followup. */
		popgo();
		popgo();
		break;
	      }

	      popgo();
	    }

	    /* Heuristically reduce the followup, since our string
	     * will be still attackable if opponent defends his
	     * string.
	     */
	    adjustment_down = 2 * countstones(pos);
	  }

	  bad_followup = 0;
	  for (s = 0; s < num_adj; s++) {
	    int lib;
	    if (countlib(adjs[s]) == 1) {
	      findlib(adjs[s], 1, &lib);
	      if (trymove(lib, other,
		    	  "estimate_territorial_value-d", NO_MOVE)) {
		if (!attack(aa, NULL)
		    && (board[pos] == EMPTY || attack(pos, NULL) != 0)) {
		  popgo();
		  bad_followup = 1;
		  break;
		}
		popgo();
	      }
	    }
	  }
	  if (bad_followup) {
	    popgo();
	    break;
	  }
	}

	for (s = 0; s < num_adj; s++) {
	  int adj = adjs[s];

	  if (same_string(pos, adj))
	    continue;
	  if (dragon[adj].color == color
	      && dragon[adj].status == DEAD
	      && 2*dragon[adj].effective_size > adjustment_up)
	    adjustment_up = 2*dragon[adj].effective_size;
	  if (dragon[adj].color == color
	      && attack(adj, NULL)
	      && 2*worm[adj].effective_size > adjustment_down)
	    adjustment_down = 2*worm[adj].effective_size;
	}

	popgo();

	/* No followup if the string is not substantial. */
	{
	  int save_verbose = verbose;
	  if (verbose > 0)
	    verbose --;
	  if (move[pos].move_safety == 0
	      && !owl_substantial(aa)) {
	    verbose = save_verbose;
	    break;
	  }
	  verbose = save_verbose;
	}
	
	adjusted_value += adjustment_up;
	adjusted_value -= adjustment_down;
	if (adjusted_value > 0.0) {
	  add_followup_value(pos, adjusted_value);
	  TRACE("  %1m:   %f (followup) - threatens to capture %1m\n",
		pos, adjusted_value, aa);
	}
      }
      break;

    case DEFEND_THREAT:
      aa = move_reasons[r].what;

      /* Make sure this is a threat to defend our stones. */
      ASSERT1(board[aa] == color, aa);
      
      /* We don't trust tactical defense threats as ko threats, unless
       * the move is safe.
       */
      if (move[pos].move_safety == 0)
	break;

      /* No followup value if string can be attacked with threat
       * against our move. An exception to this is when our move
       * isn't safe anyway and we play this only for the followup
       * value, typically as a ko threat.
       *
       * FIXME: This is somewhat halfhearted since only one attack
       * move is tested.
       */
      if (trymove(pos, color, "estimate_territorial_value-A", NO_MOVE)) {
	int attack_move;
	if (move[pos].move_safety == 1
	    && attack(aa, &attack_move) == WIN
	    && attack_move != NO_MOVE) {
	  if (trymove(attack_move, other,
		      "estimate_territorial_value-b", NO_MOVE)) {
	    if (board[pos] == EMPTY || attack(pos, NULL) != 0) {
	      popgo();
	      popgo();
	      break;
	    }
	    popgo();
	  }
	}
	popgo();
      }
      
      add_followup_value(pos, 2 * worm[aa].effective_size);

      TRACE("  %1m:   %f (followup) - threatens to defend %1m\n",
	    pos, 2 * worm[aa].effective_size, aa);

      break;

    case UNCERTAIN_OWL_DEFENSE:
      /* This move reason is valued as a strategical value. */
      break;
      
    case CUT_MOVE:
    case EXPAND_MOYO_MOVE:
    case EXPAND_TERRITORY_MOVE:
    case INVASION_MOVE:
      does_block = 1;
      break;

    case CONNECT_MOVE:
      /* This used to always set does_block=1, but there is no
       * guarantee that a connection move is strategically safe. See
       * for example gunnar:72.
       */
      if (move[pos].move_safety)
	does_block = 1;
      break;
      
    case STRATEGIC_ATTACK_MOVE:
    case STRATEGIC_DEFEND_MOVE:
      /* Do not trust these when we are scoring. Maybe we shouldn't
       * trust them otherwise either but require them to be
       * accompanied by e.g. an EXPAND move reason.
       */
      if (!doing_scoring)
	does_block = 1;
      break;
      
    case SEMEAI_THREAT:
      aa = move_reasons[r].what;

      /* threaten to win the semeai as a ko threat */
      add_followup_value(pos, 2 * dragon[aa].effective_size);
      TRACE("  %1m: %f (followup) - threatens to win semeai for %1m\n",
	    pos, 2 * dragon[aa].effective_size, aa);
      break;
      
    case SEMEAI_MOVE:
    case OWL_ATTACK_MOVE:
    case OWL_ATTACK_MOVE_GOOD_KO:
    case OWL_ATTACK_MOVE_BAD_KO:
    case OWL_ATTACK_MOVE_GAIN:
    case OWL_DEFEND_MOVE:
    case OWL_DEFEND_MOVE_GOOD_KO:
    case OWL_DEFEND_MOVE_BAD_KO:
    case OWL_DEFEND_MOVE_LOSS:

      if (move_reasons[r].type == OWL_ATTACK_MOVE_GAIN
	  || move_reasons[r].type == OWL_DEFEND_MOVE_LOSS) {
        aa = either_data[move_reasons[r].what].what1;
        bb = either_data[move_reasons[r].what].what2;
      } 
      else {
	aa = move_reasons[r].what;
	bb = NO_MOVE;
      }

      /* If the dragon is a single ko stone, the owl code currently
       * won't detect that the owl attack is conditional. As a
       * workaround we deduct 0.5 points for the move here, but only
       * if the move is a liberty of the string.
       */
      if (dragon[aa].size == 1
	  && is_ko_point(aa)
	  && liberty_of_string(pos, aa)) {
	TRACE("  %1m: -0.5 - penalty for ko stone %1m (workaround)\n",
	      pos, aa);
	tot_value -= 0.5;
      }

      /* Mark the affected dragon for use in the territory analysis. */
      mark_changed_dragon(pos, color, aa, bb, move_reasons[r].type,
	  		  safe_stones, strength, &this_value);
      this_value *= 2.0;

      TRACE("  %1m: owl attack/defend for %1m\n", pos, aa);
      
      /* FIXME: How much should we reduce the value for ko attacks? */
      if (move_reasons[r].type == OWL_ATTACK_MOVE
	  || move_reasons[r].type == OWL_DEFEND_MOVE
	  || move_reasons[r].type == SEMEAI_MOVE)
	this_value = 0.0;
      else if (move_reasons[r].type == OWL_ATTACK_MOVE_GOOD_KO
	       || move_reasons[r].type == OWL_DEFEND_MOVE_GOOD_KO) {
	this_value *= 0.3;
	TRACE("  %1m: -%f - owl attack/defense of %1m only with good ko\n",
	      pos, this_value, aa);
      }	
      else if (move_reasons[r].type == OWL_ATTACK_MOVE_BAD_KO
	       || move_reasons[r].type == OWL_DEFEND_MOVE_BAD_KO) {
	this_value *= 0.5;
	TRACE("  %1m: -%f - owl attack/defense of %1m only with bad ko\n",
	      pos, this_value, aa);
      }
      else if (move_reasons[r].type == OWL_ATTACK_MOVE_GAIN
	       || move_reasons[r].type == OWL_DEFEND_MOVE_LOSS) {
	this_value = 0.0;
      }
      
      tot_value -= this_value;

      /* If the dragon is a single string which can be tactically
       * attacked, but this owl attack does not attack tactically, it
       * can be suspected to leave some unnecessary aji or even be an
       * owl misread. Therefore we give it a small penalty to favor
       * the moves which do attack tactically as well.
       *
       * One example is manyfaces:2 where the single stone S15 can be
       * tactically attacked at S16 but where 3.3.2 finds additional
       * owl attacks at R14 (clearly ineffective) and T15 (might work,
       * but leaves huge amounts of aji).
       */
      if ((move_reasons[r].type == OWL_ATTACK_MOVE
	   || move_reasons[r].type == OWL_ATTACK_MOVE_GOOD_KO
	   || move_reasons[r].type == OWL_ATTACK_MOVE_BAD_KO)
	  && dragon[aa].size == worm[aa].size
	  && worm[aa].attack_codes[0] == WIN
	  && worm[aa].defense_codes[0] != 0
	  && attack_move_reason_known(pos, aa) != WIN) {
	if (large_scale)
	  this_value = (2.0 + 0.05 * (2 * worm[aa].effective_size));
	else
	  this_value = 0.05 * (2 * worm[aa].effective_size);
	TRACE("  %1m: -%f - suspected ineffective owl attack of worm %1m\n",
	      pos, this_value, aa);
	tot_value -= this_value;
      }
      
      does_block = 1;
      break;

    case OWL_ATTACK_THREAT:
      aa = move_reasons[r].what;

      if (dragon[aa].status == DEAD) {
	DEBUG(DEBUG_MOVE_REASONS,
	      "    %1m: 0.0 - threatens to owl attack %1m (dead)\n", pos, aa);
	break;
      }

      /* The followup value of a move threatening to attack (aa) is
       * twice its effective size, unless it has an adjacent
       * (friendly) critical dragon. In that case it's probably a
       * mistake to make the threat since it can defend itself with
       * profit.
       *
       * FIXME: We probably need to verify that the critical dragon is
       * substantial enough that capturing it saves the threatened
       * dragon.
       */	 
      {
	float value = 2 * dragon[aa].effective_size;
	int s;

	for (s = 0; s < DRAGON2(aa).neighbors; s++) {
	  int d = DRAGON2(aa).adjacent[s];
	  int adj = dragon2[d].origin;

	  if (dragon[adj].color == color
	      && dragon[adj].status == CRITICAL
	      && dragon2[d].safety != INESSENTIAL
	      && !owl_defense_move_reason_known(pos, adj))
	    value = 0.0;
	}
	
	if (value > 0.0) {
	  add_followup_value(pos, value);
	  TRACE("  %1m: %f (followup) - threatens to owl attack %1m\n",
		pos, value, aa);
	}
      }
      break;

    case OWL_DEFEND_THREAT:
      aa = move_reasons[r].what;

      add_followup_value(pos, 2 * dragon[aa].effective_size);
      TRACE("  %1m: %f (followup) - threatens to owl defend %1m\n",
	    pos, 2 * dragon[aa].effective_size, aa);
      break;

    case OWL_PREVENT_THREAT:
      /* A move attacking a dragon whose defense can be threatened.
       */
      aa = move_reasons[r].what;

      if (dragon[aa].status != DEAD) {
       DEBUG(DEBUG_MOVE_REASONS,
             "    %1m: 0.0 - prevent defense threat (dragon is not dead)\n",
             pos);
       break;
      }

      /* If the opponent just added a stone to a dead dragon, then
       * attack it. If we are ahead, add a safety move here, at most
       * half the margin of victory.
       *
       * This does not apply if we are doing scoring.
       */
      if (!doing_scoring
	  && is_same_dragon(get_last_opponent_move(color), aa)) {
	this_value = 1.5 * dragon[aa].effective_size;
	TRACE("  %1m: %f - attack last move played, although it seems dead\n",
	      pos, this_value);
	tot_value += this_value * attack_dragon_weight;
      }
      else if (!doing_scoring && our_score > 0.0) {
	/* tm - devalued this bonus (3.1.17) */
	this_value = gg_min(0.9 * dragon[aa].effective_size,
			    our_score/2.0 - board_size/2.0 - 1.0);
	this_value = gg_max(this_value, 0);
	TRACE("  %1m: %f - attack %1m, although it seems dead, as we are ahead\n",
	      pos, this_value, aa);
	tot_value += this_value * attack_dragon_weight;
      }
      else {
	
	add_reverse_followup_value(pos, 2 * dragon[aa].effective_size);
	if (board[aa] == color)
	  TRACE("  %1m: %f (reverse followup) - prevent threat to attack %1m\n",
		pos, 2 * dragon[aa].effective_size, aa);
	else 
	  TRACE("  %1m: %f (reverse followup) - prevent threat to defend %1m\n",
		pos, 2 * dragon[aa].effective_size, aa);
      }
      break;
      
    case MY_ATARI_ATARI_MOVE:
      /* Add 1.0 to compensate for -1.0 penalty because the move is
       * thought to be a sacrifice.
       */
      this_value = move_reasons[r].what + 1.0;
      tot_value += this_value;
      TRACE("  %1m: %f - combination attack kills one of several worms\n",
	    pos, this_value);
      break;
      
    case YOUR_ATARI_ATARI_MOVE:
      /* Set does_block to force territorial valuation of the move.
       * That way we can prefer defenses against combination attacks
       * on dame points instead of inside territory.
       */
      does_block = 1;
      this_value = move_reasons[r].what;
      tot_value += this_value;
      TRACE("  %1m: %f - defends against combination attack on several worms\n",
	    pos, this_value);
      break;
    }
  }

  /* Currently no difference in the valuation between blocking and
   * expanding moves.
   */
  this_value = 0.0;

  mark_inessential_stones(OTHER_COLOR(color), safe_stones);

  if (move[pos].move_safety == 1
      && (is_known_safe_move(pos) || safe_move(pos, color) != 0)) {
    safe_stones[pos] = INFLUENCE_SAVED_STONE;
    strength[pos] = DEFAULT_STRENGTH;
    if (0)
      TRACE("  %1m: is a safe move\n", pos);
  }
  else {
    TRACE("  %1m: not a safe move\n", pos);
    safe_stones[pos] = 0;
    strength[pos] = 0.0;
  }
  
  /* We don't check for move safety here. This enables a territorial
   * evaluation for sacrifice moves that enable a break-through (or
   * an owl defense).
   */
  if (does_block
      && tryko(pos, color, "estimate_territorial_value")) {
    Hash_data safety_hash = goal_to_hashvalue(safe_stones);
    if (disable_delta_territory_cache
	|| !retrieve_delta_territory_cache(pos, color, &this_value, 
					   &move[pos].influence_followup_value,
					   OPPOSITE_INFLUENCE(color),
					   safety_hash)) {

      compute_influence(OTHER_COLOR(color), safe_stones, strength, 
	  		&move_influence, pos, "after move");
      increase_depth_values();
      break_territories(OTHER_COLOR(color), &move_influence, 0, pos);
      decrease_depth_values();
      this_value = influence_delta_territory(OPPOSITE_INFLUENCE(color),
	   				     &move_influence, color, pos);
      compute_followup_influence(&move_influence, &followup_influence,
	  			 pos, "followup");
                                 
      if (this_value != 0.0)
	TRACE("%1m: %f - change in territory\n", pos, this_value);
      else
	DEBUG(DEBUG_MOVE_REASONS, "%1m: 0.00 - change in territory\n", pos);
      move[pos].influence_followup_value
	= influence_delta_territory(&move_influence, &followup_influence,
	    			    color, pos);
      store_delta_territory_cache(pos, color, this_value,
	 			  move[pos].influence_followup_value,
				  OPPOSITE_INFLUENCE(color), safety_hash);	
    }
    else {
      if (this_value != 0.0)
	TRACE("%1m: %f - change in territory (cached)\n", pos, this_value);
      else
	DEBUG(DEBUG_MOVE_REASONS,
	      "%1m: 0.00 - change in territory (cached)\n", pos);
    }
    popgo();
    
  }

  tot_value += this_value;
  
  /* Test if min_territory or max_territory values constrain the
   * delta_territory value.
   */
  if (tot_value < move[pos].min_territory
      && move[pos].min_territory > 0) {
    tot_value = move[pos].min_territory;
    TRACE("  %1m:   %f - revised to meet minimum territory value\n", 
	  pos, tot_value);
  }
  if (tot_value > move[pos].max_territory) {
    tot_value = move[pos].max_territory;
    TRACE("  %1m:   %f - revised to meet maximum territory value\n",
	  pos, tot_value);
  }
    
  move[pos].territorial_value = tot_value;
  move[pos].secondary_value  += secondary_value;
}


/*
 * Estimate the strategical value of a move at (pos).
 */
static void
estimate_strategical_value(int pos, int color, float our_score,
    			   int use_thrashing_dragon_heuristics)
{
  int k;
  int l;
  int aa = NO_MOVE;
  int bb = NO_MOVE;
  float aa_value = 0.0;
  float bb_value = 0.0;
  
  float this_value = 0.0;
  float tot_value = 0.0;

  /* Strategical value of connecting or cutting dragons. */
  float dragon_value[BOARDMAX];

  for (aa = BOARDMIN; aa < BOARDMAX; aa++)
    dragon_value[aa] = 0.0;
  
  for (k = 0; k < MAX_REASONS; k++) {
    int r = move[pos].reason[k];
    if (r < 0)
      break;
    if (move_reasons[r].status & STRATEGICALLY_REDUNDANT)
      continue;
    
    this_value = 0.0;
    switch (move_reasons[r].type) {
      case ATTACK_MOVE:
      case ATTACK_MOVE_GOOD_KO:
      case ATTACK_MOVE_BAD_KO:
      case DEFEND_MOVE:
      case DEFEND_MOVE_GOOD_KO:
      case DEFEND_MOVE_BAD_KO:
	aa = move_reasons[r].what;
      
	/* Defenseless stone */
	if (worm[aa].defense_codes[0] == 0)
	  break;

	if (doing_scoring && dragon[aa].status == DEAD)
	  break;
	
	/* FIXME: This is totally ad hoc, just guessing the value of
         *        potential cutting points.
	 * FIXME: When worm[aa].cutstone2 == 1 we should probably add
	 *        a followup value.
	 */
	if (worm[aa].cutstone2 > 1 && !worm[aa].inessential) {
	  double ko_factor = 1;
	  if (move_reasons[r].type == ATTACK_MOVE_GOOD_KO
	      || move_reasons[r].type == DEFEND_MOVE_GOOD_KO) {
	    ko_factor = 0.6;
	  }
	  else if (move_reasons[r].type == ATTACK_MOVE_BAD_KO
		   || move_reasons[r].type == DEFEND_MOVE_BAD_KO) {
	    ko_factor = 0.4;
	  }
	  this_value = 10.0 * (worm[aa].cutstone2 - 1) * ko_factor;
	  TRACE("  %1m: %f - %1m cutstone\n", pos, this_value, aa);
	}
	
	tot_value += this_value;
	
	/* If the string is a lunch for a weak dragon, the attack or
         * defense has a strategical value. This can be valued along
	 * the same lines as strategic_attack/strategic_defend.
	 *
	 * No points are awarded if the lunch is an inessential dragon
	 * or worm.
	 */
	if (DRAGON2(aa).safety == INESSENTIAL
	    || worm[aa].inessential)
	  break;

	/* If the lunch has no potential to create eyes, no points. */
	if (max_lunch_eye_value(aa) == 0)
	  break;
	
	/* Can't use k in this loop too. */
	for (l = 0; l < next_lunch; l++)
	  if (lunch_worm[l] == aa) {
	    bb = lunch_dragon[l];

	    /* FIXME: This value cannot be computed without some measurement
	     * of how the actual move affects the dragon.  The dragon safety
	     * alone is not enough. The question is whether the dragon is
	     * threatened or defended by the move or not.  
	     */
	    this_value = 1.8 * soft_cap(DRAGON2(bb).strategic_size, 15.0)
			 * dragon_weakness(bb, 0);

	    /* If this dragon consists of only one worm and that worm
	     * can be tactically captured or defended by this move, we
	     * have already counted the points as territorial value,
	     * unless it's assumed to be dead.
	     */
	    if (dragon[bb].status != DEAD
		&& dragon[bb].size == worm[bb].size
		&& (attack_move_reason_known(pos, bb)
		    || defense_move_reason_known(pos, bb)))
	      this_value = 0.0;

	    /* If this dragon can be tactically attacked and the move
             * does not defend or attack, no points.
	     */
	    if (worm[bb].attack_codes[0] != 0
		&& ((color == board[bb] && !does_defend(pos, bb))
		    || (color == OTHER_COLOR(board[bb])
			&& !does_attack(pos, bb))))
	      this_value = 0.0;

	    /* If we are doing scoring, are alive, and the move loses
             * territory, no points.
	     */
	    if (doing_scoring
		&& move[pos].territorial_value < 0.0
		&& (DRAGON2(bb).safety == ALIVE
		    || DRAGON2(bb).safety == STRONGLY_ALIVE
		    || DRAGON2(bb).safety == INVINCIBLE))
	      this_value = 0.0;
	    
	    if (this_value > dragon_value[bb]) {
	      DEBUG(DEBUG_MOVE_REASONS,
		    "  %1m:   %f - %1m attacked/defended\n",
		    pos, this_value, bb);
	      dragon_value[bb] = this_value;
	    }
	  }

	break;
	
      case ATTACK_THREAT:
      case DEFEND_THREAT:
        break;

      case EITHER_MOVE:
	/* FIXME: Generalize this to more types of threats. */
	/* FIXME: We need a policy if a move has several EITHER_MOVE
	 * reasons. Most likely not all of them can be achieved.
	 */
	aa = either_data[move_reasons[r].what].what1;
	bb = either_data[move_reasons[r].what].what2;

	/* If both worms are dead, this move reason has no value. */
	if (dragon[aa].status == DEAD 
	    && dragon[bb].status == DEAD)
	  break;

	/* Also if there is a combination attack, we assume it covers
	 * the same thing.
	 * FIXME: This is only applicable as long as the only moves
	 *        handled by EITHER_MOVE are attacks.
	 */
	if (move_reason_known(pos, MY_ATARI_ATARI_MOVE, -1))
	  break;

	aa_value = adjusted_worm_attack_value(pos, aa);
	bb_value = adjusted_worm_attack_value(pos, bb);
	this_value = gg_min(aa_value, bb_value);

	TRACE("  %1m: %f - either attacks %1m (%f) or attacks %1m (%f)\n",
	      pos, this_value, aa, aa_value, bb, bb_value);

	tot_value += this_value;
	break;
	
      case ALL_MOVE:
	/* FIXME: Generalize this to more types of threats. */
	aa = all_data[move_reasons[r].what].what1;
	bb = all_data[move_reasons[r].what].what2;

	/* If both worms are dead, this move reason has no value. */
	if (dragon[aa].status == DEAD 
	    && dragon[bb].status == DEAD)
	  break;

	/* Also if there is a combination attack, we assume it covers
	 * the same thing.
	 */
	if (move_reason_known(pos, YOUR_ATARI_ATARI_MOVE, -1))
	  break;

	aa_value = worm[aa].effective_size;
	bb_value = worm[bb].effective_size;
	this_value = 2 * gg_min(aa_value, bb_value);

	TRACE("  %1m: %f - both defends %1m (%f) and defends %1m (%f)\n",
	      pos, this_value, aa, aa_value, bb, bb_value);

	tot_value += this_value;
	break;
	
      case CONNECT_MOVE:

	/* If the opponent just added a stone to a dead dragon, which is
	 * adjacent to both dragons being connected, then the connection
	 * is probably a good way to make sure the thrashing dragon
	 * stays dead. If we are ahead, add a safety move here, at most
	 * half the margin of victory.
	 *
	 * This does only apply if we decided earlier we want to use
	 * thrashing dragon heuristics.
	 */
      
	if (use_thrashing_dragon_heuristics) {
	  int cc;
	  aa = dragon[conn_worm1[move_reasons[r].what]].origin;
	  bb = dragon[conn_worm2[move_reasons[r].what]].origin;
	  cc = get_last_opponent_move(color);

	  if (cc != NO_MOVE
	      && thrashing_stone[cc]
	      && are_neighbor_dragons(aa, cc)
	      && are_neighbor_dragons(bb, cc)) {
	    if (aa == bb)
	      this_value = 1.6 * DRAGON2(cc).strategic_size;
	    else if (DRAGON2(aa).safety == INESSENTIAL
		     || DRAGON2(bb).safety == INESSENTIAL) {
	      if ((DRAGON2(aa).safety == INESSENTIAL
		   && max_lunch_eye_value(aa) == 0)
		  || (DRAGON2(bb).safety == INESSENTIAL
		      && max_lunch_eye_value(bb) == 0))
		this_value = 0.0;
	      else
		this_value = 0.8 * DRAGON2(cc).strategic_size;
	    }
	    else
	      this_value = 1.7 * DRAGON2(cc).strategic_size;
	    
	    if (this_value > dragon_value[dragon[cc].origin]) {
	      dragon_value[dragon[cc].origin] = this_value;
	      DEBUG(DEBUG_MOVE_REASONS,
		    "  %1m:   %f - connect %1m and %1m to attack thrashing dragon %1m\n",
		    pos, this_value, aa, bb, cc);
	    }
	  }
	}

	if (!move[pos].move_safety)
	  break;
	/* Otherwise fall through. */
      case CUT_MOVE:
	if (doing_scoring && !move[pos].move_safety)
	  break;

	aa = dragon[conn_worm1[move_reasons[r].what]].origin;
	bb = dragon[conn_worm2[move_reasons[r].what]].origin;

	if (aa == bb)
	  continue;

	/* If we are ahead by more than 20, value connections more strongly */
	if (our_score > 20.0)
	  this_value = connection_value(aa, bb, pos, our_score);
	else
	  this_value = connection_value(aa, bb, pos, 0);
	if (this_value > dragon_value[aa]) {
	  dragon_value[aa] = this_value;
          DEBUG(DEBUG_MOVE_REASONS,
		"  %1m:   %f - %1m cut/connect strategic value\n",
		pos, this_value, aa);
	}

	
	if (our_score > 20.0)
	  this_value = connection_value(bb, aa, pos, our_score);
	else
	  this_value = connection_value(bb, aa, pos, 0);
	if (this_value > dragon_value[bb]) {
	  dragon_value[bb] = this_value;
          DEBUG(DEBUG_MOVE_REASONS,
		"  %1m:   %f - %1m cut/connect strategic value\n",
		pos, this_value, bb);
	}
	
	break;
	
      case SEMEAI_MOVE:
	/*
	 * The strategical value of winning a semeai is
	 * own dragons (usually) becomes fully secure, while adjoining
	 * enemy dragons do not.
	 *
	 * FIXME: Valuation not implemented at all yet. 
	 */

	break;
	
      case STRATEGIC_ATTACK_MOVE:
      case STRATEGIC_DEFEND_MOVE:	
	/* The right way to do this is to estimate the safety of the
	 * dragon before and after the move. Unfortunately we are
	 * missing good ways to do this currently.
	 *
	 * Temporary solution is to only look at an ad hoc measure of
	 * the dragon safety and ignoring the effectiveness of the
	 * move.
	 *
	 * FIXME: Improve the implementation.
	 */
	aa = move_reasons[r].what;

	/* FIXME: This value cannot be computed without some
	 * measurement of how the actual move affects the dragon. The
	 * dragon safety alone is not enough. The question is whether
	 * the dragon is threatened by the move or not.
	 */
	if (use_thrashing_dragon_heuristics
	    && thrashing_stone[aa])
	  this_value = 1.7 * DRAGON2(aa).strategic_size;
	else
	  this_value = 1.8 * soft_cap(DRAGON2(aa).strategic_size, 15.0)
		       * dragon_weakness(aa, 1);

	/* No strategical attack value is awarded if the dragon at (aa)
	 * has an adjacent (friendly) critical dragon, which is not
	 * defended by this move. In that case it's probably a mistake
	 * to make the strategical attack since the dragon can defend
	 * itself with profit.
	 *
	 * FIXME: We probably need to verify that the critical dragon is
	 * substantial enough that capturing it saves the strategically
	 * attacked dragon.
	 */
	if (move_reasons[r].type == STRATEGIC_ATTACK_MOVE) {
	  int s;
	  
	  for (s = 0; s < DRAGON2(aa).neighbors; s++) {
	    int d = DRAGON2(aa).adjacent[s];
	    int adj = dragon2[d].origin;
	    
	    if (dragon[adj].color == color
		&& dragon[adj].status == CRITICAL
		&& dragon2[d].safety != INESSENTIAL
		&& !owl_defense_move_reason_known(pos, adj))
	      this_value = 0.0;
	  }
	}
	
	/* Multiply by attack_dragon_weight to try to find a best fit */
	this_value = this_value * attack_dragon_weight;
		
	if (this_value > dragon_value[aa]) {
	  dragon_value[aa] = this_value;
          DEBUG(DEBUG_MOVE_REASONS,
		"  %1m:   %f - %1m strategic attack/defend\n",
		pos, this_value, aa);

	}
	break;

      case UNCERTAIN_OWL_DEFENSE:
	aa = move_reasons[r].what;
	
	/* If there is an adjacent dragon which is critical we should
	 * skip this type of move reason, since attacking or defending
	 * the critical dragon is more urgent.
	 */
	{
	  int d;
	  int found_one = 0;
	  
	  for (d = 0; d < DRAGON2(aa).neighbors; d++)
	    if (DRAGON(DRAGON2(aa).adjacent[d]).status == CRITICAL)
	      found_one = 1;
	  if (found_one)
	    break;
	}
	
	/* If we are behind, we should skip this type of move reason. 
	 * If we are ahead, we should value it more. 
	 */
	if (our_score < 0.0)
	  this_value = 0.0;
	else 
	  this_value = gg_min(2*DRAGON2(aa).strategic_size, 0.65*our_score);
	
	if (this_value > dragon_value[aa]) {
	  dragon_value[aa] = this_value;
	  DEBUG(DEBUG_MOVE_REASONS,
		"  %1m:   %f - %1m uncertain owl defense bonus\n",
		pos, this_value, aa);
	}

	break;
    }
  }
  
  for (aa = BOARDMIN; aa < BOARDMAX; aa++) {
    if (dragon_value[aa] == 0.0)
      continue;

    ASSERT1(dragon[aa].origin == aa, aa);

    /* If this dragon is critical but not attacked/defended by this
     * move, we ignore the strategic effect.
     */
    if (dragon[aa].status == CRITICAL
	&& !owl_move_reason_known(pos, aa)) {
      DEBUG(DEBUG_MOVE_REASONS, "  %1m: 0.0 - disregarding strategic effect on %1m (critical dragon)\n",
	    pos, aa);
      continue;
    }
    
    /* If this dragon consists of only one worm and that worm can
     * be tactically captured or defended by this move, we have
     * already counted the points as territorial value, unless
     * it's assumed to be dead.
     * However, we still allow strategical excess value (see below)
     * in case the effective_size is substantially bigger (by 2.0)
     * than the actualy size.
     */
    if (dragon[aa].status != DEAD
	&& dragon[aa].size == worm[aa].size
	&& worm[aa].effective_size < worm[aa].size + 2.0
	&& (attack_move_reason_known(pos, aa)
	    || defense_move_reason_known(pos, aa))) {
      TRACE("  %1m:   %f - %1m strategic value already counted - A.\n",
	    pos, dragon_value[aa], aa);
      continue;
    }
    /* If the dragon has been owl captured, owl defended, or involved
     * in a semeai, we have likewise already counted the points as
     * territorial value.
     */
    if (attack_move_reason_known(pos, aa)
	|| defense_move_reason_known(pos, aa)
	|| (owl_move_reason_known(pos, aa)
	    && dragon[aa].status == CRITICAL)
	|| move_reason_known(pos, SEMEAI_MOVE, aa)) {
      /* But if the strategical value was larger than the territorial
       * value (e.g. because connecting to strong dragon) we award the
       * excess value as a bonus.
       */
      float excess_value = (dragon_value[aa] - 
			    2 * DRAGON2(aa).strategic_size);
      if (excess_value > 0.0) {
	TRACE("  %1m: %f - strategic bonus for %1m\n", pos, excess_value, aa);
	tot_value += excess_value;
      }
      else {
	TRACE("  %1m:   %f - %1m strategic value already counted - B.\n",
	      pos, dragon_value[aa], aa);
      }
      
      continue;
    }

    TRACE("  %1m: %f - strategic effect on %1m\n",
	  pos, dragon_value[aa], aa);
    tot_value += dragon_value[aa];
  }

  /* Finally, subtract penalty for invasion type moves. */
  this_value = strategic_penalty(pos, color);
  /* Multiply by invasion_malus_weight to allow us to fit the weight */
  this_value = this_value * invasion_malus_weight;
  if (this_value > 0.0) {
    TRACE("  %1m: %f - strategic penalty, considered as invasion.\n",
	  pos, -this_value);
    tot_value -= this_value;
  }

  move[pos].strategical_value = tot_value;
}


/* Compare two move reasons, used for sorting before presentation. */
static int
compare_move_reasons(const void *p1, const void *p2)
{
  const int mr1 = *(const int *) p1;
  const int mr2 = *(const int *) p2;

  if (move_reasons[mr1].type != move_reasons[mr2].type)
    return move_reasons[mr2].type - move_reasons[mr1].type;
  else
    return move_reasons[mr2].what - move_reasons[mr1].what;
}


/*
 * Combine the reasons for a move at (pos) into a simple numerical value.
 * These heuristics are now somewhat less ad hoc than before but probably
 * still need a lot of improvement.
 */
static float
value_move_reasons(int pos, int color, float pure_threat_value,
		   float our_score, int use_thrashing_dragon_heuristics)
{
  float tot_value;
  float shape_factor;

  gg_assert(stackp == 0);
  
  /* Is it an antisuji? */
  if (is_antisuji_move(pos))
    return 0.0; /* This move must not be played. End of story. */

  /* Never play on a vertex which is unconditional territory for
   * either player. There is absolutely nothing to gain.
   */
  if (worm[pos].unconditional_status != UNKNOWN)
    return 0.0;
  
  /* If this move has no reason at all, we can skip some steps. */
  if (move[pos].reason[0] >= 0
      || move[pos].min_territory > 0.0) {
    int num_reasons;

    /* Sort the move reasons. This makes it easier to visually compare
     * the reasons for different moves in the trace outputs.
     */
    num_reasons = 0;
    while (move[pos].reason[num_reasons] >= 0 && num_reasons < MAX_REASONS)
      num_reasons++;
    gg_sort(move[pos].reason, num_reasons, sizeof(move[pos].reason[0]),
	    compare_move_reasons);

    /* Discard move reasons that only duplicate another. */
    discard_redundant_move_reasons(pos);

    /* Estimate the value of various aspects of the move. The order
     * is significant. Territorial value must be computed before
     * strategical value. See connection_value().
     */
    estimate_territorial_value(pos, color, our_score, 0);
    estimate_strategical_value(pos, color, our_score,
			       use_thrashing_dragon_heuristics);
  }

  /* Introduction of strategical_weight and territorial_weight, 
   * for automatic fitting. (3.5.1)
   */
  tot_value = territorial_weight * move[pos].territorial_value + 
              strategical_weight * move[pos].strategical_value;

  shape_factor = compute_shape_factor(pos);

  if (tot_value > 0.0) {
    int c;
    float followup_value;

    /* Negative territorial followup doesn't make make sense. */
    if (move[pos].influence_followup_value < 0.0)
      move[pos].influence_followup_value = 0.0;
     
    followup_value = move[pos].followup_value
		     + move[pos].influence_followup_value;
    TRACE("  %1m:   %f - total followup value, added %f as territorial followup\n",
	  pos, followup_value, move[pos].influence_followup_value);

    /* In the endgame, there are a few situations where the value can
     * be 0 points + followup.  But we want to take the intersections first
     * were we actually get some points.  0.5 points is a 1 point ko which
     * is the smallest value that is actually worth something.
     */
    if (tot_value >= 0.5) {
      float old_tot_value = tot_value;
      float contribution;
      /* We adjust the value according to followup and reverse followup
       * values.
       */
      contribution = gg_min(gg_min(0.5 * followup_value
				   + 0.5 * move[pos].reverse_followup_value,
				   1.0 * tot_value
				   + followup_value),
			    1.1 * tot_value
			    + move[pos].reverse_followup_value);
      tot_value += contribution * followup_weight;
      /* The first case applies to gote vs gote situation, the
       * second to reverse sente, and the third to sente situations.
       * The usual rule is that a sente move should count at double
       * value. But if we have a 1 point move with big followup (i.e.
       * sente) we want to play that before a 2 point gote move. Hence
       * the factor 1.1 above.
       */
      
      if (contribution != 0.0) {
	TRACE("  %1m: %f - added due to followup (%f) and reverse followup values (%f)\n",
	      pos, contribution, followup_value,
	      move[pos].reverse_followup_value);
      }

      /* If a ko fight is going on, we should use the full followup
       * and reverse followup values in the total value. We save the
       * additional contribution for later access.
       */
      move[pos].additional_ko_value =
	followup_value 
	+ move[pos].reverse_followup_value 
	- (tot_value - old_tot_value);

      /* Not sure whether this could happen, but check for safety. */
      if (move[pos].additional_ko_value < 0.0)
	move[pos].additional_ko_value = 0.0;
    }
    else {
      move[pos].additional_ko_value =
	shape_factor * (move[pos].followup_value
			+ move[pos].reverse_followup_value);
    }

    tot_value += soft_cap(0.05 * move[pos].secondary_value, 0.4);
    if (move[pos].secondary_value != 0.0)
      TRACE("  %1m: %f - secondary\n", pos,
	    soft_cap(0.05 * move[pos].secondary_value, 0.4));

    if (move[pos].numpos_shape + move[pos].numneg_shape > 0) {
      /* shape_factor has already been computed. */
      float old_value = tot_value;
      /* Maximum 15 points of the territorial value will be weighted by shape_factor */
      if (move[pos].territorial_value < 15)
        tot_value *= shape_factor;
      else {
        float non_shape_val = move[pos].territorial_value - 15;
        tot_value = (tot_value - non_shape_val) * shape_factor + non_shape_val;
      }

      if (verbose) {
	/* Should all have been TRACE, except we want field sizes. */
	gprintf("  %1m: %f - shape ", pos, tot_value - old_value);
	fprintf(stderr,
		"(shape values +%4.2f(%d) -%4.2f(%d), shape factor %5.3f)\n",
		move[pos].maxpos_shape, move[pos].numpos_shape,
		move[pos].maxneg_shape, move[pos].numneg_shape,
		shape_factor);
      }
    }

    /* Add a special shape bonus for moves which connect own strings
     * or cut opponent strings.
     */
    c = (move_connects_strings(pos, color, 1)
	 + move_connects_strings(pos, OTHER_COLOR(color), 0));
    if (c > 0) {
      float shape_factor2 = pow(1.02, (float) c) - 1;
      float base_value = gg_max(gg_min(tot_value, 5.0), 1.0);
      if (verbose) {
	/* Should all have been TRACE, except we want field sizes. */
	gprintf("  %1m: %f - connects strings ", pos,
		base_value * shape_factor2);
	fprintf(stderr, "(connect value %d, shape factor %5.3f)\n", c,
		shape_factor2);
      }
      tot_value += base_value * shape_factor2;
    }

    /* Dame points which have a cut or connect move reason get a small
     * extra bonus because these have a tendency to actually be worth
     * a point.
     */
    if (tot_value < 0.3
	&& (move_reason_known(pos, CONNECT_MOVE, -1)
	    || move_reason_known(pos, CUT_MOVE, -1))) {
      float old_tot_value = tot_value;
      tot_value = gg_min(0.3, tot_value + 0.1);
      TRACE("  %1m: %f - cut/connect dame bonus\n", pos,
	    tot_value - old_tot_value);
    }
  }
  else {
    move[pos].additional_ko_value =
      shape_factor * (move[pos].followup_value +
		      gg_min(move[pos].followup_value,
			     move[pos].reverse_followup_value));
  }

  /* If the move is valued 0 or small, but has followup values and is
   * flagged as a worthwhile threat, add up to pure_threat_value to
   * the move.
   *
   * FIXME: We shouldn't have to call confirm_safety() here. It's
   * potentially too expensive.
   */
  if (pure_threat_value > 0.0 
      && move[pos].worthwhile_threat
      && tot_value <= pure_threat_value
      && board[pos] == EMPTY
      && move[pos].additional_ko_value > 0.0
      && is_legal(pos, color)
      && value_moves_confirm_safety(pos, color)) {
    float new_tot_value = gg_min(pure_threat_value,
				 tot_value
				 + 0.25 * move[pos].additional_ko_value);

    /* Make sure that moves with independent value are preferred over
     * those without.
     */
    new_tot_value *= (1.0 - 0.1 * (pure_threat_value - tot_value)
		      / pure_threat_value);
    
    if (new_tot_value > tot_value) {
      TRACE("  %1m: %f - carry out threat or defend against threat\n",
	    pos, new_tot_value - tot_value);
      tot_value = new_tot_value;
    }
  }
  
  /* min_value is now subject to reduction with a fitted weight (3.5.1) */
  move[pos].min_value = move[pos].min_value * minimum_value_weight;
  move[pos].max_value = move[pos].max_value * maximum_value_weight;
  
  /* Test if min_value or max_value values constrain the total value.
   * First avoid contradictions between min_value and max_value,
   * assuming that min_value is right.
   */
  if (move[pos].min_value > move[pos].max_value)
    move[pos].max_value = move[pos].min_value;

  /* If several moves have an identical minimum value, then GNU Go uses the
   * following secondary criterion (unless min_value and max_value agree, and
   * unless min_value is bigger than 25, in which case it probably comes from
   * a J or U pattern): 
   */
  if (move[pos].min_value < 25)
    move[pos].min_value += tot_value / 200;
  if (tot_value < move[pos].min_value
      && move[pos].min_value > 0) {
    tot_value = move[pos].min_value;
    TRACE("  %1m:   %f - minimum accepted value\n", pos, tot_value);
  }
  
  if (tot_value > move[pos].max_value) {
    tot_value = move[pos].max_value;
    TRACE("  %1m:   %f - maximum accepted value\n",
          pos, tot_value);
  }

  if (tot_value > 0
      || move[pos].territorial_value > 0
      || move[pos].strategical_value > 0) {
    TRACE("Move generation values %1m to %f\n", pos, tot_value);
    move_considered(pos, tot_value);
  }

  return tot_value;
}


/*
 * Loop over all possible moves and value the move reasons for each.
 */
static void
value_moves(int color, float pure_threat_value, float our_score,
            int use_thrashing_dragon_heuristics)
{
  int m, n;
  int pos;

  TRACE("\nMove valuation:\n");
  
  /* Visit the moves in the standard lexicographical order */
  for (n = 0; n < board_size; n++)
    for (m = board_size-1; m >= 0; m--) {
      pos = POS(m, n);

      move[pos].value = value_move_reasons(pos, color, 
					   pure_threat_value, our_score,
					   use_thrashing_dragon_heuristics);
      if (move[pos].value == 0.0)
	continue;
      
      /* Maybe this test should be performed elsewhere. This is just
       * to get some extra safety. We don't filter out illegal ko
       * captures here though, because if that is the best move, we
       * should reevaluate ko threats.
       */
      if (is_legal(pos, color) || is_illegal_ko_capture(pos, color)) {
	/* Add a random number between 0 and 0.01 to use in comparisons. */
	move[pos].value += 
	  0.01 * move[pos].random_number * move[pos].randomness_scaling;
      }
      else {
	move[pos].value = 0.0;
	TRACE("Move at %1m wasn't legal.\n", pos);
      }
    }
}


/* Print the values of all moves with values bigger than zero. */

void
print_all_move_values(FILE *output)
{
  int pos;
  
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos) || move[pos].final_value <= 0.0)
      continue;
      
    gfprintf(output, "%1M %f\n", pos, move[pos].final_value);
  }
}

/* Search through all board positions for the 10 highest valued
 * moves and print them.
 */

static void
print_top_moves(void)
{
  int k;
  int pos;
  float tval;
  
  for (k = 0; k < 10; k++) {
    best_moves[k] = NO_MOVE;
    best_move_values[k] = 0.0;
  }
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    if (!ON_BOARD(pos) || move[pos].final_value <= 0.0)
      continue;
      
    tval = move[pos].final_value;
    record_top_move(pos, tval);
  }
  
  if (verbose > 0 || (debug & DEBUG_TOP_MOVES)) {
    gprintf("\nTop moves:\n");
    for (k = 0; k < 10 && best_move_values[k] > 0.0; k++)
      gprintf("%d. %1M %f\n", k+1, best_moves[k], best_move_values[k]);
  }
}

/* Add a move to the list of top moves (if it is among the top ten) */

void
record_top_move(int pos, float val)
{
  int k;
  for (k = 9; k >= 0; k--)
    if (val > best_move_values[k]) {
      if (k < 9) {
	best_move_values[k+1] = best_move_values[k];
	best_moves[k+1] = best_moves[k];
      }
      best_move_values[k] = val;
      best_moves[k] = pos;
    }

  move[pos].final_value = val;
}

/* remove a rejected move from the list of top moves */

void
remove_top_move(int move)
{
  int k;
  for (k = 0; k < 10; k++) {
    if (best_moves[k] == move) {
      int l;
      for (l = k; l < 9; l++) {
	best_moves[l] = best_moves[l+1];
	best_move_values[l] = best_move_values[l+1];
      }
      best_moves[9] = NO_MOVE;
      best_move_values[9] = 0.0;
    }
  }
}

/* This function is called if the biggest move on board was an illegal
 * ko capture.
 */
static void
reevaluate_ko_threats(int ko_move, int color, float ko_value)
{
  int ko_stone = NO_MOVE;
  int opp_ko_move;
  int pos;
  int k;
  int type, what;
  int threat_does_work = 0;
  int ko_move_target;
  int num_good_threats = 0;
  int good_threats[BOARDMAX];
  int best_threat_quality = -1;
  float threat_size;

  ko_move_target = get_biggest_owl_target(ko_move);
  
  /* If the move is a simple ko recapture, find the ko stone. (If
   * it's not a simple ko recapture, then the move must be a superko
   * violation.)
   */
  if (is_illegal_ko_capture(ko_move, color)) {
    for (k = 0; k <= 3; k++) {
      ko_stone = ko_move + delta[k];
      if (ON_BOARD(ko_stone) && countlib(ko_stone) == 1)
	break;
    }
    ASSERT_ON_BOARD1(ko_stone);
  }
  
  TRACE("Reevaluating ko threats.\n");
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    int threat_quality = 0;

    if (!ON_BOARD(pos) || pos == ko_move)
      continue;
    if (move[pos].additional_ko_value <= 0.0) 
      continue;

    /* Otherwise we look for the biggest threat, and then check whether
     * it still works after ko has been resolved.
     */

    /* `additional_ko_value' includes reverse followup. While it is good to
     * play ko threats which eliminate other threats in turn, we should
     * always prefer threats that are larger than the value of the ko.
     */
    if (move[pos].followup_value < ko_value)
      threat_quality = -1;

    threat_size = 0.0;
    type = -1;
    what = -1;
    for (k = 0; k < MAX_REASONS; k++) {
      int r = move[pos].reason[k];
      if (r < 0)
	break;
      if (!(move_reasons[r].type & THREAT_BIT))
	continue;

      switch (move_reasons[r].type) {
      case ATTACK_THREAT:
      case DEFEND_THREAT:
	if (worm[move_reasons[r].what].effective_size
	    > threat_size) {
	  threat_size = worm[move_reasons[r].what].effective_size;
	  type = move_reasons[r].type;
	  what = move_reasons[r].what;
	}
	break;
      case OWL_ATTACK_THREAT:
      case OWL_DEFEND_THREAT:   
      case SEMEAI_THREAT:
	if (dragon[move_reasons[r].what].effective_size
	    > threat_size) {
	  threat_size = dragon[move_reasons[r].what]\
	    .effective_size;
	  type = move_reasons[r].type;
	  what = move_reasons[r].what;
	}
	break;
      default:
	/* This means probably someone has introduced a new threat type
	 * without adding the corresponding case above.
	 */
	gg_assert(0);
	break;
      }
    } 
    /* If there is no threat recorded, the followup value is probably
     * contributed by a pattern. We can do nothing but accept this value.
     * (although this does cause problems).
     *
     * FIXME: In the case of superko violation we have no ko_stone.
     * Presumably some of the tests below should be applicable anyway.
     * Currently we just say that any threat is ok.
     */
    if (type == -1 || ko_stone == NO_MOVE)
      threat_does_work = 1;
    else {
      if (trymove(pos, color, "reevaluate_ko_threats", ko_move)) {
	ASSERT_ON_BOARD1(ko_stone);
	if (!find_defense(ko_stone, &opp_ko_move))
	  threat_does_work = 1;
	else {
	  int threat_wastes_point = 0;
	  if (whose_area(OPPOSITE_INFLUENCE(color), pos) != EMPTY)
	    threat_wastes_point = 1;

	  if (trymove(opp_ko_move, OTHER_COLOR(color),
		      "reevaluate_ko_threats", ko_move)) {
	    switch (type) {
	    case ATTACK_THREAT:
	      /* In case the attack threat was a snapback move, there
	       * is no stone on the board to attack now and we check
	       * for a defense of the threatening move instead.
	       */
	      if (board[what] != EMPTY)
		threat_does_work = attack(what, NULL);
	      else
		threat_does_work = find_defense(pos, NULL);
	      break;
	    case DEFEND_THREAT:
	      threat_does_work = (board[what] != EMPTY
				  && find_defense(what, NULL));
	      break;
	    case OWL_ATTACK_THREAT:
	    case OWL_DEFEND_THREAT:
	      /* Should we call do_owl_attack/defense here?
	       * Maybe too expensive? For the moment we just assume
	       * that the attack does not work if it concerns the
	       * same dragon as ko_move. (Can this really happen?)
	       */
	      threat_does_work = (ko_move_target != what);
	    }
	    popgo();
	    
	    /* Is this a losing ko threat? */
	    if (threat_does_work && type == ATTACK_THREAT) {
	      int apos;
	      if (attack(pos, &apos)
		  && does_defend(apos, what)
		  && (forced_backfilling_moves[apos]
		      || (!is_proper_eye_space(apos)
			 && !false_eye_territory[apos]))) {
		threat_does_work = 0;
	      }
	    }

	    /* If we are fighting a tiny ko (1 - 2 points only), we pay
	     * extra attention to select threats that don't waste points.
	     * In particular, we don't play threats inside of opponent
	     * territory if they can be averted on a dame intersection.
	     */
	    if (ko_value < 1.0
		&& threat_does_work
		&& threat_quality >= 0
		&& (type == ATTACK_THREAT || type == DEFEND_THREAT)) {
	      int averting_pos;

	      if (type == ATTACK_THREAT)
		find_defense(what, &averting_pos);
	      else
		attack(what, &averting_pos);

	      /* `averting_pos' can be NO_MOVE sometimes, at least when
	       * when the the threat is a threat to attack. It is not
	       * clear what to do in such cases.
	       */
	      if (averting_pos != NO_MOVE) {
		int averting_wastes_point = 0;
		if (whose_territory(OPPOSITE_INFLUENCE(color), averting_pos)
		    != EMPTY)
		  averting_wastes_point = 1;
		threat_quality = averting_wastes_point - threat_wastes_point;
		if (threat_quality < 0)
 		  threat_does_work = 0;
	      }
	    }
	  }
	}
	popgo();
      }
    }
    
    if (threat_does_work) {
      if (threat_quality == best_threat_quality)
	good_threats[num_good_threats++] = pos;
      else if (threat_quality > best_threat_quality) {
	best_threat_quality = threat_quality;
	num_good_threats = 0;
	good_threats[num_good_threats++] = pos;
      }
      else
	DEBUG(DEBUG_MOVE_REASONS,
	      "%1m: no additional ko value (threat does not work as ko threat)\n", pos);
    }
  }

  for (k = 0; k < num_good_threats; k++) {
    pos = good_threats[k];

    /* If the move previously had no value, we need to add in the
     * randomness contribution now.
     *
     * FIXME: This is very ugly. Restructure the code so that the
     * randomness need only be considered in one place.
     */
    if (move[pos].value == 0.0) {
      move[pos].value += 
	0.01 * move[pos].random_number * move[pos].randomness_scaling;
    }

    TRACE("%1m: %f + %f = %f\n", pos, move[pos].value,
	  move[pos].additional_ko_value,
	  move[pos].value + move[pos].additional_ko_value);
    move[pos].value += move[pos].additional_ko_value;
  }
}


/* Redistribute points. When one move is declared a replacement for
 * another by a replacement move reason, the move values for the
 * inferior move are transferred to the replacement.
 */
static void
redistribute_points(void)
{
  int source;
  int target;

  for (target = BOARDMIN; target < BOARDMAX; target++)
    if (ON_BOARD(target))
      move[target].final_value = move[target].value;
  
  for (source = BOARDMIN; source < BOARDMAX; source++) {
    if (!ON_BOARD(source))
      continue;
    target = replacement_map[source];
    if (target == NO_MOVE)
      continue;

    TRACE("Redistributing points from %1m to %1m.\n", source, target);
    if (move[target].final_value < move[source].final_value) {
      TRACE("%1m is now valued %f.\n", target, move[source].final_value);
      move[target].final_value = move[source].final_value;
    }
    TRACE("%1m is now valued 0.\n", source);
    move[source].final_value = 0.0;
  }
}

/* This selects the best move available according to their valuations.
 * If the best move is an illegal ko capture, we add ko threat values.
 * If the best move is a blunder, it gets devalued and continue to look
 * for the best move.
 */
static int
find_best_move(int *the_move, float *value, int color,
	       int allowed_moves[BOARDMAX])
{
  int good_move_found = 0;
  signed char blunder_tested[BOARDMAX];
  float best_value = 0.0;
  int best_move = NO_MOVE;
  int pos;

  memset(blunder_tested, 0, sizeof(blunder_tested));

  while (!good_move_found) {
    best_value = 0.0;
    best_move = NO_MOVE;

    /* Search through all board positions for the highest valued move. */
    for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
      float this_value = move[pos].final_value;
      if (allowed_moves && !allowed_moves[pos])
	continue;
      if (!ON_BOARD(pos) || move[pos].final_value == 0.0)
	continue;
	
      if (this_value > best_value) {
	if (is_legal(pos, color) || is_illegal_ko_capture(pos, color)) {
	  best_value = this_value;
	  best_move = pos;
	}
	else {
	  TRACE("Move at %1m would be suicide.\n", pos);
	  remove_top_move(pos);
	  move[pos].value = 0.0;
	  move[pos].final_value = 0.0;
	}
      }
    }
    
    /* If the best move is an illegal ko capture, reevaluate ko
     * threats and search again.
     */
    if (best_value > 0.0
	&& (is_illegal_ko_capture(best_move, color)
	    || !is_allowed_move(best_move, color))) {
      TRACE("Move at %1m would be an illegal ko capture.\n", best_move);
      reevaluate_ko_threats(best_move, color, best_value);
      redistribute_points();
      time_report(2, "  reevaluate_ko_threats", NO_MOVE, 1.0);
      remove_top_move(best_move);
      move[best_move].value = 0.0;
      move[best_move].final_value = 0.0;
      print_top_moves();
      good_move_found = 0;
    }
    /* Call blunder_size() to check that we're not about to make a
     * blunder. Otherwise devalue this move and scan through all move
     * values once more.
     */
    else if (best_value > 0.0) {
      if (!blunder_tested[best_move]) {
	float blunder_size = value_moves_get_blunder_size(best_move, color);
	if (blunder_size > 0.0) {
	  TRACE("Move at %1m is a blunder, subtracting %f.\n", best_move,
		blunder_size);
	  remove_top_move(best_move);
	  move[best_move].value -= blunder_size;
	  move[best_move].final_value -= blunder_size;
	  TRACE("Move at %1m is now valued %f.\n", best_move,
		move[best_move].final_value);
	  record_top_move(best_move, move[best_move].final_value);
	  good_move_found = 0;
	  blunder_tested[best_move] = 1;
	}
	else
	  good_move_found = 1; /* Best move was not a blunder. */
      }
      else /* The move apparently was a blunder, but still the best move. */
	good_move_found = 1;
    }
    else
      good_move_found = 1; /* It's best to pass. */
  }
  
  if (best_value > 0.0 
      && best_move != NO_MOVE) {
    *the_move = best_move;
    *value = best_value;
    return 1;
  }

  return 0;
}


/*
 * Review the move reasons to find which (if any) move we want to play.
 *
 * The parameter pure_threat_value is the value assigned to a move
 * which only threatens to capture or kill something. The reason for
 * playing these is that the move may be effective because we have
 * misevaluated the dangers or because the opponent misplays.
 *
 * The array allowed_moves restricts which moves may be considered. If
 * NULL any move is allowed.
 */
int
review_move_reasons(int *the_move, float *value, int color,
		    float pure_threat_value, float our_score,
		    int allowed_moves[BOARDMAX],
		    int use_thrashing_dragon_heuristics)
{
  int save_verbose;

  current_color = color;
  
  start_timer(2);
  find_more_attack_and_defense_moves(color);
  time_report(2, "  find_more_attack_and_defense_moves", NO_MOVE, 1.0);

  if (get_level() >= 6) {
    find_more_owl_attack_and_defense_moves(color);
    time_report(2, "  find_more_owl_attack_and_defense_moves", NO_MOVE, 1.0);
  }

  if (large_scale && get_level() >= 6) {
    find_large_scale_owl_attack_moves(color);
    time_report(2, "  find_large_scale_owl_attack_moves", NO_MOVE, 1.0);
  }

  find_more_semeai_moves(color);
  time_report(2, "  find_more_semeai_moves", NO_MOVE, 1.0);

  save_verbose = verbose;
  if (verbose > 0)
    verbose--;
  examine_move_safety(color);
  time_report(2, "  examine_move_safety", NO_MOVE, 1.0);
  verbose = save_verbose;

  /* We can't do this until move_safety is known. */
  induce_secondary_move_reasons(color);
  time_report(2, "  induce_secondary_move_reasons", NO_MOVE, 1.0);
    
  if (printworms || verbose)
    list_move_reasons(stderr, NO_MOVE);

  /* Evaluate all moves with move reasons. */
  value_moves(color, pure_threat_value, our_score,
      	      use_thrashing_dragon_heuristics);
  time_report(2, "  value_moves", NO_MOVE, 1.0);

  /* Perform point redistribution */
  redistribute_points();

  /* Search through all board positions for the 10 highest valued
   * moves and print them.
   */
  print_top_moves();

  /* Select the highest valued move and return it. */
  return find_best_move(the_move, value, color, allowed_moves);
}


/*
 * Choosing a strategy based on the current score estimate 
 * and the game status (between 0.0 (start) and 1.0 (game over)).
 */

void 
choose_strategy(int color, float our_score, float game_status)
{

  minimum_value_weight  = 1.0;
  maximum_value_weight  = 1.0;
  territorial_weight    = 1.0;
  strategical_weight    = 1.0;
  attack_dragon_weight  = 1.0;
  invasion_malus_weight = 1.0;
  followup_weight       = 1.0;

  TRACE("  Game status = %f (0.0 = start, 1.0 = game over)\n", game_status);

  
  if (cosmic_gnugo) {

    if (game_status > 0.65 && our_score > 15.0) {
      
      /* We seem to be winning, so we use conservative settings. */
      minimum_value_weight  = 0.66;
      maximum_value_weight  = 2.0;
      territorial_weight    = 0.95; 
      strategical_weight    = 1.0; 
      attack_dragon_weight  = 1.1; 
      invasion_malus_weight = 1.3;
      followup_weight       = 1.1;
      TRACE("  %s is leading, using conservative settings.\n",
	    color == WHITE ? "White" : "Black");
    }
    else if (game_status > 0.16) {
      
      /* We're not winning enough yet, try aggressive settings. */
      minimum_value_weight  = 0.66;
      maximum_value_weight  = 2.0;
      territorial_weight    = 1.4;
      strategical_weight    = 0.5; 
      attack_dragon_weight  = 0.62;
      invasion_malus_weight = 2.0;
      followup_weight       = 0.62;
       
      /* If we're getting desesperate, try invasions as a last resort */
      if (game_status > 0.75 && our_score < -25.0)
        invasion_malus_weight = 0.2;
            
      TRACE("  %s is not winning enough, using aggressive settings.\n", 
	    color == WHITE ? "White" : "Black");
    }
  }
}

/* In order to get valid influence data after a move, we need to rerun
 * estimate_territorial_value() for that move. A prerequisite for
 * using this function is that move reasons have already been collected.
 *
 * This function should only be used for debugging purposes.
 */
void
prepare_move_influence_debugging(int pos, int color)
{
  float our_score;
  
  if (color == WHITE)
    our_score = black_score;
  else
    our_score = -white_score;

  estimate_territorial_value(pos, color, our_score, 1);
}


/* Compute probabilities of each move being played.  It is assumed
 * that the `move[]' array is filled with proper values (i.e. that
 * one of the genmove*() functions has been called).
 *
 * The value of each move `V_k' should be a uniformly distributed
 * random variable (`k' is a unique move index).  Let it have values
 * from the interval [l_k; u_k] .  Then move value has constant
 * probability density on the interval:
 *
 *		1
 *   d_k = -----------.
 *	    u_k - l_k
 *
 * We need to determine the probability of `V_k' being the largest of
 * {V_1, V_2, ..., V_n}.  Probability density is like follows:
 *
 *   D_k(t) = d_k * Product(P{V_i < t} for i != k), l_k <= t <= u_k,
 *
 * where P{A} is the probability of event `A'.  By integrating D_k(t)
 * from `l_k' to `u_k' we can find the probability in question:
 *
 *   P{V_k > V_i for i != k} = Integrate(D_k(t) dt from l_k to u_k).
 *
 * Function D_k(t) is a polynomial on each of subintervals produced by
 * points `l_k', `u_k', k = 1, ..., n.  When t < min(l_k), D_k(t) is
 * zero.  On other subintervals it can be evaluated by taking into
 * account that
 *
 *  P{V_i < t} = d_i * (t - l_i)  if t < u_i;
 *  P{V_i < t} = 1		  if t >= u_i.
 */
void
compute_move_probabilities(float probabilities[BOARDMAX])
{
  int k;
  int pos;
  int num_moves = 0;
  int moves[BOARDMAX];
  double lower_values[BOARDMAX];
  double upper_values[BOARDMAX];
  double densities[BOARDMAX];
  double common_lower_limit = 0.0;

  /* Find all moves with positive values. */
  for (pos = BOARDMIN; pos < BOARDMAX; pos++) {
    probabilities[pos] = 0.0;

    if (ON_BOARD(pos)) {
      /* FIXME: what about point redistribution? */
      if (move[pos].final_value > 0.0) {
	double scale = 0.01 * (double) move[pos].randomness_scaling;

	moves[num_moves] = pos;
	lower_values[num_moves] = ((double) move[pos].final_value
				   - (scale * move[pos].random_number));
	upper_values[num_moves] = lower_values[num_moves] + scale;
	densities[num_moves] = 1.0 / scale;

	if (lower_values[num_moves] > common_lower_limit)
	  common_lower_limit = lower_values[num_moves];

	num_moves++;
      }
    }
  }

  /* Compute probability of each move. */
  for (k = 0; k < num_moves; k++) {
    int i;
    double lower_limit = common_lower_limit;

    /* Iterate over subintervals for integration. */
    while (lower_limit < upper_values[k]) {
      int j;
      double upper_limit = upper_values[k];
      double span_power;
      double polynomial[BOARDMAX];
      int degree;

      degree = 0;
      polynomial[0] = 1.0;

      for (i = 0; i < num_moves; i++) {
	/* See if we need to decrease current subinterval. */
	if (upper_values[i] > lower_limit && upper_values[i] < upper_limit)
	  upper_limit = upper_values[i];
      }

      /* Build the probability density polynomial for the current
       * subinterval.
       */
      for (i = 0; i < num_moves; i++) {
	if (i != k && upper_values[i] >= upper_limit) {
	  polynomial[++degree] = 0.0;
	  for (j = degree; j > 0; j--) {
	    polynomial[j] = (densities[i]
			     * (polynomial[j - 1]
				+ ((lower_limit - lower_values[i])
				   * polynomial[j])));
	  }

	  polynomial[0] *= densities[i] * (lower_limit - lower_values[i]);
	}
      }

      /* And compute the integral of the polynomial on the current
       * subinterval.
       */
      span_power = 1.0;
      for (j = 0; j <= degree; j++) {
	span_power *= upper_limit - lower_limit;
	probabilities[moves[k]] += (polynomial[j] * span_power) / (j + 1);
      }

      /* Go on to the next subinterval. */
      lower_limit = upper_limit;
    }

    probabilities[moves[k]] *= densities[k];
  }
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
