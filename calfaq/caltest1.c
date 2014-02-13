#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "calfaq.h"

/* caltest FUNCTION ARGS ... */

char *usage_text = "\
usage: caltest [options] [action [args]...]...\n\
caltest performs date calculations using the 'calfaq' c library.\n\
\n\
Options:\n\
  h      show this help\n\
  n      no run: don't compute, just show the parsed tokens\n\
  v      verbose (talk a lot)\n\
";

int debug;
int norun;
int verbose;

#define xvprintf(FMT)           \
    va_list vargs;              \
    va_start(vargs, FMT);       \
    vfprintf(stderr,FMT,vargs); \
    va_end(vargs)

void errorf(char *fmt,...) {                xvprintf(fmt); exit(2); }
void  talk (char *msg)     { fputs(msg,stderr); fputs("\n",stderr); }
void dtalk (char *msg)     { if (debug) talk(msg);                  }
void vtalk (char *msg)     { if (verbose) talk(msg);                }
void  talkf(char *fmt,...) {                xvprintf(fmt);          }
void dtalkf(char *fmt,...) { if (debug) {   xvprintf(fmt); }        }
void vtalkf(char *fmt,...) { if (verbose) { xvprintf(fmt); }        }

void usage () {
  talk(usage_text);
  exit(0);
}

typedef struct {
  int    argc;    /* number of arguments */
  char **argv;    /* argument vector */
  int    ax;      /* arg index */
  char  *cp;      /*char pointer within the current arg */
  char   next_char;
} Parse_Data;

static Parse_Data parse_data;

void parse_init (int argc, char **argv) {
  parse_data.argc = argc;
  parse_data.argv = argv;
  parse_data.ax = 0;
  parse_data.cp = parse_data.argv[0];
  parse_data.next_char = 0;
}

typedef enum { NO_TYPE, ALPHA_TYPE, DIGIT_TYPE, SPACE_TYPE, PUNCT_TYPE, EOD_TYPE } TokenType;

typedef struct {
  char *token;
  int   len;
  TokenType type;
} Token;


/* char = next_char(); -- returns the next character from the argument vector, or 0 */

char next_char() {
  char c;
  if ((c = parse_data.next_char)) {
    parse_data.next_char = 0;
  } else if (parse_data.cp && 0 == (c = *(parse_data.cp++))) {
    int ax = ++parse_data.ax;	           /* move to the next argument */
    if (ax > parse_data.argc) {            /* end of all args? */
      parse_data.cp = 0;	           /* null cp mean's we are done */
      c = 0;                               /* no more characters */
    } else {
      parse_data.cp = parse_data.argv[ax]; /* setup to start at the next argument */
      c = ' ';                             /* return the space between arguments */
    }
  }
  return(c);
}

/* push_back_char(c) -- push char c back to the parse, for it to be picked up 
 * on the next token.
 */

void push_back_char(char c) {
  parse_data.next_char = c;
}

/* Token * = token_init(token, string, type)
 *
 * Initialize the token object with string and type.
 * The token object pointer is returned.
 */

Token * token_init (Token *token, char *str, TokenType type) {
  token->token = str;
  token->len = strlen(str);
  token->type = type;
  return(token);
}

/* Token *new_token(Token *token) -- make a copy of token and return the pointer to the copy */

Token * new_token(char *tokstr, TokenType type) {
  char *new_str = strcpy(malloc(1+strlen(tokstr)), tokstr);
  Token *new_tok = token_init(malloc(sizeof(Token)),new_str,type);
  return(new_tok);
}

/* free a token object */

void free_token(Token *old_tok) {
  free(old_tok->token);                 // free the token string
  free(old_tok);                        // free the token itself
}

TokenType char_type(char c) {
  return(    c == 0 ? EOD_TYPE   :
         isalpha(c) ? ALPHA_TYPE :
         isdigit(c) ? DIGIT_TYPE :
         isspace(c) ? SPACE_TYPE :
         ispunct(c) ? PUNCT_TYPE :
                        EOD_TYPE);
}

char * char_type_name(TokenType type) {
  char *name;
  switch(type) {
    case ALPHA_TYPE: name = "alpha"; break;
    case DIGIT_TYPE: name = "digit"; break;
    case SPACE_TYPE: name = "space"; break;
    case PUNCT_TYPE: name = "punct"; break;
    default:         name = "EOD";   break;
  }
  return(name);
}

/* char * = parse_token() -- return a pointer to the next token 
 * return 0 when there are no more tokens
 */

Token *parse_token () {
  static char tokstr[255];
  TokenType type = NO_TYPE;
  tokstr[0] = 0;
  int tx = 0, this_type;
  char c;
  while ((c = next_char()) != 0) {
    this_type = char_type(c);
    dtalkf("char: '%c', type: '%d'\n", c, this_type);
    if (!type || type == this_type) {
      tokstr[tx++] = c;        // accumlate the next character
      tokstr[tx] = 0;          // always leave it null-terminated
      type = this_type;        // set the next type
    } else {                   // terminate the token
      push_back_char(c);       // push the current char back for the next token
      break;                   // done with thie token
    }
  }
  if (c == 0) type = EOD_TYPE;
  return(new_token(tokstr,type)); // return a copy of the new token
}

void dump_token(Token *tok) {
  dtalkf("Token: '%s'\n", tok->token);
  dtalkf(" Type: %s\n", char_type_name(tok->type));
  dtalk("");
}


/*
 * parse the date actions
 * 
 * DATE                    -- convert to julian days
 * JDN                     -- convert Julian day number into a date
 * weeknum {DATE | JDN}    -- show the weeknum for the given DATE|JDN
 * weekday {DATE | JDN}    -- show the weekday for the given DATE|JDN
 * leapyear YEAR           -- show whether or not YEAR is a leap year
 * solarnum YEAR           -- show the solar number of YEAR
 * epact YEAR              -- show the epact for YEAR
 * paschal_moon YEAR       -- the date of the Paschal full moon
 * easter YEAR
 * easter_sunday YEAR      -- Easter Sunday (Gregorian calendar)
 * julian_period YEAR      -- convert absolute year to Julian
 *
 * Aliases for the commands:
 *   wn    -- weeknum
 *   wd    -- weekday
 *   ly    -- leapyear
 *   sn    -- solar number
 *   ep    -- epact
 *   pm    -- paschal moon
 *   e     -- easter
 *   es    -- easter sunday
 *   jp    -- julian period
 *
 * The DATE can be in any of the following formats:
 *
 *   YYYY-DD-MM
 *   MM/DD/YYYY
 *   DD.MM.YYYY
 *   DD-MMM-YYYY
 *   MMMM DD, YYYY
 *   DDD MMM DD HH:MM:SS YYYY TZONE
 */

void parse_actions () {
  Token *tok;
  TokenType type;
  do {
    tok = parse_token();
    type = tok->type;
    dump_token(tok);
    free_token(tok);
  } while (type != EOD_TYPE);
}

/* main program */

int main (int argc, char** argv) {

  int ch;
  debug = norun = verbose = 0;

  /* read the options */

  while ((ch = getopt(argc, argv, "dhnv")) != -1) {
    switch (ch) {
      case 'd': debug++ ;   break ;
      case 'n': norun++ ;   break ;
      case 'v': verbose++ ; break ;
      case '?': errorf("unknown option: '%c'", optopt); break;
      case ':': errorf("Missing argument to option '%c'", optopt); break;
      case 'h':
      default: usage();
    }
  }

  argc -= optind;
  argv += optind;

  /* Now parse the commands and arguments */ 

  if (argc == 0) usage();

  if (norun && debug)   fputs("norun = 1\n",stderr);
  if (verbose && debug) fputs("verbose = 1\n",stderr);

  parse_init(argc, argv);
  parse_actions();

  exit(0);
}

