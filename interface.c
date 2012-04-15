#include <stdio.h>
#include <stdlib.h>  /* for rand() and srand() */


/* Maximum allowed line length in GTP. */
#define GTP_BUFSIZE 1000

/* Status returned from callback functions. */
#define GTP_QUIT    -1
#define GTP_OK       0
#define GTP_FATAL    1

/* Whether the GTP command was successful. */
#define GTP_SUCCESS  0
#define GTP_FAILURE  1

/* Forward declarations. */
static int gtp_protocol_version(char *s);
static int gtp_name(char *s);
static int gtp_version(char *s);
static int gtp_known_command(char *s);
static int gtp_list_commands(char *s);
static int gtp_quit(char *s);
static int gtp_boardsize(char *s);
static int gtp_clear_board(char *s);
static int gtp_komi(char *s);
static int gtp_fixed_handicap(char *s);
static int gtp_place_free_handicap(char *s);
static int gtp_set_free_handicap(char *s);
static int gtp_play(char *s);
static int gtp_genmove(char *s);
static int gtp_final_score(char *s);
static int gtp_final_status_list(char *s);
static int gtp_showboard(char *s);

/* List of known commands. */
static struct gtp_command commands[] = {
  {"protocol_version",    gtp_protocol_version},
  {"name",                gtp_name},
  {"version",             gtp_version},
  {"known_command",    	  gtp_known_command},
  {"list_commands",    	  gtp_list_commands},
  {"quit",             	  gtp_quit},
  {"boardsize",        	  gtp_boardsize},
  {"clear_board",      	  gtp_clear_board},
  {"komi",        	  gtp_komi},
  {"fixed_handicap",   	  gtp_fixed_handicap},
  {"place_free_handicap", gtp_place_free_handicap},
  {"set_free_handicap",   gtp_set_free_handicap},
  {"play",            	  gtp_play},
  {"genmove",             gtp_genmove},
  {"final_score",         gtp_final_score},
  {"final_status_list",   gtp_final_status_list},
  {"showboard",        	  gtp_showboard},
  {NULL,                  NULL}
};


int
main(int argc, char **argv)
{
	caml_main(argv);
  unsigned int random_seed = 1;

  /* Optionally a random seed can be passed as an argument to the program. */
  if (argc > 1)
    sscanf(argv[1], "%u", &random_seed);
  srand(random_seed);
  
  /* Make sure that stdout is not block buffered. */
  setbuf(stdout, NULL);
  
  /* Inform the GTP utility functions about the initial board size. */
  gtp_internal_set_boardsize(board_size);

  /* Initialize the board. */
  init_brown();

  /* Process GTP commands. */
  gtp_main_loop(commands, stdin, NULL);

  return 0;
}

/* We are talking version 2 of the protocol. */
static int
gtp_protocol_version(char *s)
{
  return gtp_success("2");
}

static int
gtp_name(char *s)
{
  return gtp_success("Go Go Game");
}

static int
gtp_version(char *s)
{
  return gtp_success(VERSION_STRING);
}

static int
gtp_known_command(char *s)
{
  int i;
  char command_name[GTP_BUFSIZE];

  /* If no command name supplied, return false (this command never
   * fails according to specification).
   */
  if (sscanf(s, "%s", command_name) < 1)
    return gtp_success("false");
  
  for (i = 0; commands[i].name; i++)
    if (!strcmp(command_name, commands[i].name))
      return gtp_success("true");
  
  return gtp_success("false");
}

static int
gtp_list_commands(char *s)
{
  int i;

  gtp_start_response(GTP_SUCCESS);

  for (i = 0; commands[i].name; i++)
    gtp_printf("%s\n", commands[i].name);

  gtp_printf("\n");
  return GTP_OK;
}

static int
gtp_quit(char *s)
{
  gtp_success("");
  return GTP_QUIT;
}

static int
gtp_boardsize(char *s)
{
  int boardsize;

  if (sscanf(s, "%d", &boardsize) < 1)
    return gtp_failure("boardsize not an integer");
  
  if (boardsize < MIN_BOARD || boardsize > MAX_BOARD)
    return gtp_failure("unacceptable size");

  /*
   * TODO : call CAML code
   */
  
  return gtp_success("");
}

static int
gtp_clear_board(char *s)
{
  clear_board();
  return gtp_success("");
}

static int
gtp_komi(char *s)
{
  if (sscanf(s, "%f", &komi) < 1)
    return gtp_failure("komi not a float");
  
  return gtp_success("");
}

/* Common code for fixed_handicap and place_free_handicap. */
static int
place_handicap(char *s, int fixed)
{
	/*
	 * TODO : call CAML code
	 */
	return gtp_finish_response();
}

static int
gtp_fixed_handicap(char *s)
{
  return place_handicap(s, 1);
}

static int
gtp_place_free_handicap(char *s)
{
  return place_handicap(s, 0);
}

static int
gtp_set_free_handicap(char *s)
{
  /*
   * TODO : call CAML code
   */
  return gtp_success("");
}

static int
gtp_play(char *s)
{
	/*
	 * TODO : call CAML code
	 */

	return gtp_success("");
}

static int
gtp_genmove(char *s)
{
	/*
	 * TODO : call CAML code
	 */
	return gtp_finish_response();
}

/* Compute final score. We use area scoring since that is the only
 * option that makes sense for this move generation algorithm.
 */
static int
gtp_final_score(char *s)
{
	/*
	 * TODO : call CAML code
	 */
	return gtp_success("0");
}

static int
gtp_final_status_list(char *s)
{
	/*
	 * TODO : call CAML code
	 */
	return gtp_finish_response();
}

static int
gtp_showboard(char *s)
{

	/*
	 * TODO : call CAML code
	 */
  return gtp_finish_response();
}


/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
