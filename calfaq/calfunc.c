/* calfunc -- calendar functions */

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>
#include <pcre.h>
#include <time.h>

#include "calfaq.h"

/* caltest FUNCTION ARGS ... */

const char *usage_text = "\
usage: caltest [options] [action [args]...]...\n\
caltest performs date calculations using the 'calfaq' c library.\n\
\n\
Options:\n\
  d      debug mode\n\
  h      show this help\n\
  i      interactive mode -- read commands from STDIN\n\
  n      no run: don't compute, just show the parsed tokens\n\
  v      verbose (talk a lot)\n\
";

const char *help_text = "\
Commands: The commands may be given as unique abbreviations, or\n\
as the specific alternative (XX) in parentheses.\n\
\n\
[julian (j)]  DATE              -- convert DATE to Julian days\n\
[date (d)]    JDN               -- convert Julian Day Number (JDN) into a date\n\
absdays (a)   DATE              -- convert DATE to Absolute days\n\
adate (ad)    ABSDAYS           -- convert absolute days to a date\n\
weeknum       (wn) {DATE | JDN} -- the weeknum for the given DATE|JDN\n\
weekday       (wd) {DATE | JDN} -- the weekday for the given DATE|JDN\n\
leapyear      (ly) YEAR         -- whether or not YEAR is a leap year\n\
solarnum      (sn) YEAR         -- the solar number of YEAR\n\
epact         (ep) YEAR         -- the epact for YEAR\n\
paschal_moon  (pm) YEAR         -- the date of the Paschal full moon\n\
easter        (e)  YEAR         -- the date of Easter for an absolute YEAR\n\
easter_sunday (es) YEAR         -- Easter Sunday (Gregorian calendar) for YEAR\n\
julian_period (jp) YEAR         -- the Julian year for a given absolute YEAR\n\
\n\
The DATE can be in any of the following formats:\n\
\n\
   YYYY-MM-DD\n\
   MM/DD/YYYY\n\
   DD.MM.YYYY\n\
   DD-MMM-YYYY\n\
   MMMM DD, YYYY\n\
   DDD MMM DD HH:MM:SS YYYY TZONE\n\
";

int debug;
int norun;
int verbose;

#define xvprintf(FMT)           \
    va_list vargs;              \
    va_start(vargs, FMT);       \
    vfprintf(stderr,FMT,vargs); \
    va_end(vargs)

void error (const char *msg)     { fputs(msg,stderr); fputs("\n",stderr); exit(2); }
void  talk (const char *msg)     { fputs(msg,stderr); fputs("\n",stderr);          }
void dtalk (const char *msg)     { if (debug)     talk(msg);                       }
void vtalk (const char *msg)     { if (verbose)   talk(msg);                       }

void errorf(const char *fmt,...) {                xvprintf(fmt);          exit(2); }
void  talkf(const char *fmt,...) {                xvprintf(fmt);                   }
void dtalkf(const char *fmt,...) { if (debug) {   xvprintf(fmt); }                 }
void vtalkf(const char *fmt,...) { if (verbose) { xvprintf(fmt); }                 }

void usage (int more) {
  talk(usage_text);
  if (more) talk(help_text);
  exit(0);
}

/* These routines are not valid for date values with years outside of these
 * below range. */

const int YEAR_MIN = -4712;
const int YEAR_MAX =  3267;

const int GREGORIAN_START_YEAR  = 1582;
const int GREGORIAN_START_MONTH =   10;
const int GREGORIAN_START_DAY   =    4;

const int GREGORIAN_START_JDAY = 2299150;

/* The difference between Julian Day Number and an absolute day number is just an offset */
const int JDAY_ADAY_OFFSET = 1721425;

/* Generic RE parsing routines 
 *
 * int  re_match  char* REGEXP, char *INPUTSTRING, int *MATCHES, char ***SUBSTR_LIST
 * void re_done   char*** SUBSTRLIST
 */

#define OVECTOR_SIZE (3*30)      // size of the PCRE output vector; should be multiple of 3

/* re_match( char *REGEX, char *INPUTSTRING,int *MATCHES, char ***SUBSTR_LIST);
 *
 * Match REGEX against INPUTSTRING, returning the number of substrings,
 * including $0, in MATCHES, and the individual substrings in SUBSTRS.
 *
 * SUBSTR[0] is the entire matched string
 * SUBSTR[1] is the first substring
 * SUBSTR[2] is the second substring
 * etc. 
 *
 * Be sure to call "re_done(substr_list)" when there is no need for the substrings.
 */

int re_match(const char* re_str, const char* input, int* matches, const char*** substr_list) {

  const char *error;
  int erroffset;
  pcre *re;

  dtalkf("RE is '%s'\n", re_str);
  re = pcre_compile(              // compile the pcre
        re_str,	                  // parse a keyword
        0,
        &error,                   // where the error message will reside
        &erroffset,
        NULL);

  if (re == NULL)
    errorf("PCRE compilation error at offset %d: %s\n", erroffset, error);

  int ovector[OVECTOR_SIZE];      // this will hold the output vector (substrings)

  dtalkf("Matching RE against '%s'\n", input);
  int rc = pcre_exec(
         re,                      // the compiled regexp
         NULL,                    // no extra data -- we didn't study the pattern
         input,                   // we're parsing the input string
         strlen(input),           // how long is the parse_buffer?
         0,                       // start at the beginning
         0,                       // default options
         ovector,                 // the output vector for substr info
         OVECTOR_SIZE);           // the size of the output vector

  if (rc < 0) {                   // an error
    switch(rc) {
      case PCRE_ERROR_NOMATCH: 
        dtalkf("no match\n"); break;
      default: talkf("Matching error %d\n", rc); break;
    }
    pcre_free(re);                // release the pcre
    return 0;                    // no match
  } else if (rc == 0) {
    errorf("Internal error: Need more memory for ovector\n");
  }
  *matches = rc;                  // # of matches
  int err = pcre_get_substring_list(input, ovector, rc, substr_list);
  if (err) errorf("PCRE no memory error!\n");
  pcre_free(re);                  // release the RE
  return 1;                       // return success
}

/* re_done() */

void re_done(const char **substr_list) {
  pcre_free(substr_list);
}


/* parse_init -- initialize the parse buffer, from the command arguments */


void parse_init_from_arg_buffer(int argc, char** argv, char* buffer, int buf_size) {
  int i;
  buffer[0] = 0;
  for (i=0; i<argc; i++) {
    if (i == 0 )
      strlcpy(buffer, argv[i], buf_size);
    else {
      strlcat(buffer, " ", buf_size);
      strlcat(buffer, argv[i], buf_size);       /* append the next arg to the buffer */
    }
  }
  dtalkf("buffer: '%s'\n", buffer);
}


/*
 * parse the date actions
 * 
 * [julian] DATE           -- convert to julian days
 * [date] JDN              -- convert Julian day number into a date
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
 *   YYYY-MM-DD
 *   MM/DD/YYYY
 *   DD.MM.YYYY
 *   DD-MMM-YYYY
 *   MMMM DD, YYYY
 *   DDD MMM DD HH:MM:SS YYYY TZONE
 */

/* global variables used by the parsers */

int year, month, day;               // the parse_XXXX routines set year, month, and day
int jdn;                            // the julian dan number
int adays;                          // the absolute day number
int weekday, weeknum;               // the week day (0..7) and week number
const char *date_format; 
const char *timestr, *tzone;

/* validate a number to be between a min and max, or print the given error
 * message, which should contain a '%s' which holds the original input string.
 */

int parse_dates(const char *inputstr);

int valid_number (const char *numstr, int min, int max, char *errmsg, int *number) {
  int num = atoi(numstr);
  if (num < min || num > max) {
    talkf(errmsg, numstr);
    return 0;
  } else { 
    *number = num;
    return 1;
  }
}

int valid_year (const char *ystr, int *yearp) {
  return valid_number(ystr, -4712, 3267, "Bad year value: '%s'\n", yearp);
}

int valid_month (const char *mstr, int *monthp) {
  return valid_number(mstr, 1, 12, "Bad month value: '%s'\n", monthp);
}

int valid_month_name (const char *mstr, int *monthp) {
  static char *month_names[] = {
    "January", "February", "March", "April", "May", "June", "July",
    "August", "September", "October", "November", "December" };
  int i;
  for (i=0; i<=11; i++) {
    if (strncasecmp(month_names[i],mstr,strlen(mstr)) == 0) {
      *monthp = i+1;
      return 1;
    }
  }
  errorf("Bad month name: '%s'\n", mstr);
  return 0;
}

int valid_weekday_name(const char *dstr, int *weekdayp) {
  static char *day_names[] = { 
    "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
  };
  int i;
  for (i=0;i<7;i++) {
    if (strncasecmp(day_names[i],dstr,strlen(dstr)) == 0) {
      *weekdayp = i;
      return 1;
    }
  }
  return 0;
}

int valid_day (const char *dstr, int *dayp) {
  return valid_number(dstr, 1, 31, "Bad day value: '%s'\n", dayp);
}

int valid_time (const char *timestr, const char** tmstr) {
  *tmstr = timestr;
  return 1;
}

int valid_timezone (const char *tzonestr, const char **tzp) {
  *tzp = tzonestr;
  return 1;
}

/* These parse_XXXX routines return true (1) or false (0) indicating whether or
 * not the parse was successful
 */

int parse_date_yyyy_mm_dd(int argc, const char *args[]) {
  return valid_year(args[1],  &year)  &&
         valid_month(args[3], &month) &&
         valid_day(args[4],   &day);
}

int parse_date_mm_dd_yyyy(int argc, const char *args[]) {
  return valid_month(args[1], &month) &&
         valid_day(args[3],   &day)   &&
         valid_year(args[4],  &year);
}

int parse_date_dd_mm_yyyy(int argc, const char *args[]) {
  return valid_day(args[1],   &day)   &&
         valid_month(args[3], &month) &&
         valid_year(args[4],  &year);
}

int parse_date_dd_mmm_yyyy(int argc, const char *args[]) {
  return valid_day(args[1],        &day)   &&
         valid_month_name(args[3], &month) &&
         valid_year(args[4],       &year);
}

int parse_date_mmmm_dd_yyyy(int argc, const char *args[]) {
  return valid_month_name(args[1], &month) &&
         valid_day(args[3],        &day)   &&
         valid_year(args[4],       &year);
}

// "^(\\w{3}) (\\w{3}) ([ \\d]\\d) (\\d\\d:\\d\\d:\\d\\d) (\\d{4}) (\\w+)(?: |$)"
//     DDD      MMM        DD        HH_MM_SS               YYYY    TZONE

int parse_date_unix(int argc, const char *args[]) {
  int ok = valid_weekday_name(args[1], &weekday) &&
           valid_month_name(args[2],   &month)   &&
           valid_day(args[3],          &day)     &&
           valid_year(args[5],         &year);
    valid_time(args[4],     &timestr);       // we don't validate time here
    valid_timezone(args[6], &tzone);         // not much real validation
  return ok;
}

void show_date(int, int, int);

int style_for_date(int year, int month, int day) {
  if (year < GREGORIAN_START_YEAR ||
       (year == GREGORIAN_START_YEAR &&
         (month < GREGORIAN_START_MONTH ||
           (month == GREGORIAN_START_MONTH &&
             day < GREGORIAN_START_DAY))))
    return JULIAN;
  else
    return GREGORIAN;
}

int style_for_jdn(int jdn) {
  if (jdn < GREGORIAN_START_JDAY)
    return JULIAN;
  else
    return GREGORIAN;
}

/* show_date_for_jdn(char *ARGSTRING) */

int show_date_for_jdn(const char *argstr) {
  jdn = atoi(argstr);
  int style = style_for_jdn(jdn);
  jdn_to_date(style, jdn, &year, &month, &day);
  show_date(year, month, day);
  return 1;
}


int parse_date_jdn(int argc, const char *args[]) {
  return show_date_for_jdn(args[1]);
}

/* these functions are invoked by keyword comands, and take a string to parse
 * for futher args */

int kw_julian_date_arg(int argc, const char *argstr) {
  if (!parse_dates(argstr)) return 0;
  int style = style_for_date(year, month, day);
  jdn = date_to_jdn(style, year, month, day);
  printf("%d\n", jdn);
  return 1;
}

int kw_date_jdn_arg(int argc, const char *argstr) {
  return show_date_for_jdn(argstr);
}

// Convert JDN to AbsDays

int jdn_to_adays(int jdn) {
  return (jdn - JDAY_ADAY_OFFSET);
}

// convert ADAY to JDN

int adays_to_jdn(int adays) {
  return (adays + JDAY_ADAY_OFFSET);
}



int kw_adays_date_arg(int argc, const char *argstr) {
  if (!parse_dates(argstr)) return 0;
  int style = style_for_date(year, month, day);
  jdn = date_to_jdn(style, year, month, day);
  adays = jdn_to_adays(jdn);
  printf("%d\n", adays);
  return 1;
}

int kw_adate_aday_arg(int argc, const char *argstr) {
  adays = atoi(argstr);
  jdn = adays_to_jdn(adays);
  int style = style_for_jdn(jdn);
  jdn_to_date(style, jdn, &year, &month, &day);
  show_date(year, month, day);
  return 1;
}

int kw_weeknum_date_arg(int argc, const char *argstr) {
  if (!parse_dates(argstr)) return 0;
  int weekyear;
  week_number(year, month, day, &weeknum, &weekyear);
  if (year != weekyear)
    printf("%d %04d\n", weeknum, weekyear);
  else
    printf("%d\n", weeknum);
  return 1;
}

const char *weekday_names[] = { "Sun", "Mon", "Tues", "Wednes", "Thurs", "Fri", "Satur" };

int kw_weekday_date_arg(int argc, const char *argstr) {
  if (!parse_dates(argstr)) return 0;
  int style = style_for_date(year, month, day);
  weekday = day_of_week(style, year, month, day);
  printf ("%sday\n", weekday_names[weekday]);
  return 1;
}

int kw_leapyear_year_arg(int argc, const char *argstr) {
  year = atoi(argstr);
  int style = year < GREGORIAN_START_YEAR ? JULIAN : GREGORIAN;
  int leapyear = is_leap(style, year);
  printf("%4d %s a leap year\n", year, (leapyear ? "is" : "is not"));
  return 1;
}

int kw_solarnum_year_arg(int argc, const char *argstr) {
  year = atoi(argstr);
  // TODO: calculate solarnum
  return 1;
}
int kw_epact_year_arg(int argc, const char *argstr) {
  year = atoi(argstr);
  // TODO: calculate epact
  return 1;
}
int kw_paschalmoon_year_arg(int argc, const char *argstr) {
  year = atoi(argstr);
  // TODO: compute pascal_moon
  return 1;
}
int kw_easter_year_arg(int argc, const char *argstr) {
  year = atoi(argstr);
  // TODO: compute easter for given year
  return 1;
}
int kw_eastersunday_year_arg(int argc, const char *argstr) {
  year = atoi(argstr);
  // TODO: compute easter sunday (gregorian) for given year
  return 1;
}
int kw_julianperiod_year_arg(int argc, const char *argstr) {
  year = atoi(argstr);
  // TODO: compute easter sunday (gregorian) for given year
  return 1;
}

int kw_help_command(int argc, const char *argstr) {
  talk(help_text);
  return 1;
}

typedef struct {
  char *re_pat;
  int (*func)();
} RE_item;

RE_item date_parse_REs[] = {
  { "^\\s*(\\d{4})([/-])(\\d{1,2})\\2(\\d{1,2})(?: |$)",      &parse_date_yyyy_mm_dd },   // YYYY-MM-DD , YYYY/MM/DD
  { "^\\s*(\\d{1,2})([/-])(\\d{1,2})\\2(\\d{4})(?: |$)",      &parse_date_mm_dd_yyyy },   // MM/DD/YYYY , MM-DD-YYYY
  { "^\\s*(\\d{1,2})([.])(\\d{1,2})\\2(\\d{4})(?: |$)",       &parse_date_dd_mm_yyyy },   // DD.MM.YYYY
  { "^(?i)\\s*(\\d{1,2})([-])([a-z]{3})\\2(\\d{4})(?: |$)",   &parse_date_dd_mmm_yyyy },  // DD.MMM.YYYY
  { "^(?i)\\s*([a-z]{3,9}) +(\\d{1,2}),? *(\\d{4})(?: |$)",   &parse_date_mmmm_dd_yyyy }, // MMMM DD, YYYY
  { "^(?i)\\s*([a-z]{3}) ([a-z]{3}) ([ \\d]\\d) (\\d\\d:\\d\\d:\\d\\d) (\\d{4}) (\\w+)(?: |$)", 
                                                              &parse_date_unix },         // DDD MMM DD HH:MM:SS YYYY TZONE
  { "^\\s*(\\d+)(?:\\s|$)",                                   &parse_date_jdn },          // NNN (julian day number)
  { NULL, NULL }
};

/* parse_dates 
 * 
 * match an inputstring against a set of known date formats.  Extract the year,
 * month, and day (or more) into the substr_list, and return 1 for success;
 * zero otherwise.
 */

int parse_dates(const char *inputstr) {

  int rc = 0;
  int i = 0;                                // start index
  int matches = 0;
  const char **date_parts_list;
  const char *re_pat = NULL;
  int (*func)() = NULL;                     // function to call

  while (date_parse_REs[i].re_pat) {        // pattern?
    re_pat = date_parse_REs[i].re_pat;
    func   = date_parse_REs[i].func;
    rc = re_match(re_pat, inputstr, &matches, &date_parts_list);
    if (rc >= 1) break;
    func = NULL;
    i++;
  }
  if (rc == 0 || func == NULL) {
    talkf("Don't understand this: '%s'\n", inputstr);
    return 0;
  }
  return (*func)(matches, date_parts_list);        // call the function with parsed list of strings
}

/* keywords */

typedef enum { YEAR_ARG, DATE_ARG, JDN_ARG, ADAY_ARG, NO_ARG } ArgType;


typedef struct {
  char   *keyword;
  char   *alias;
  int   (*func)();
  ArgType type;
} KW_item;

KW_item parse_keywords[] =  {
  { "julian",       "j",  &kw_julian_date_arg,       DATE_ARG },
  { "date",         "d",  &kw_date_jdn_arg,          JDN_ARG  },
  { "adays",        "a",  &kw_adays_date_arg,        DATE_ARG },
  { "adate",        "ad", &kw_adate_aday_arg,        ADAY_ARG },
  { "weekday",      "wd", &kw_weekday_date_arg,      DATE_ARG },
  { "weeknum",      "wn", &kw_weeknum_date_arg,      DATE_ARG },
  { "leapyear",     "ly", &kw_leapyear_year_arg,     YEAR_ARG },
  { "solarnum",     "sn", &kw_solarnum_year_arg,     YEAR_ARG },
  { "epact",        "ep", &kw_epact_year_arg,        YEAR_ARG },
  { "paschalmoon",  "pm", &kw_paschalmoon_year_arg,  YEAR_ARG },
  { "easter",       "e",  &kw_easter_year_arg,       YEAR_ARG },
  { "eastersunday", "es", &kw_eastersunday_year_arg, YEAR_ARG },
  { "julianperiod", "jp", &kw_julianperiod_year_arg, YEAR_ARG },
  { "help",         "h",  &kw_help_command,          NO_ARG   },
  { NULL,           NULL, NULL,                      NO_ARG   },
};

/* rc = lookup_keyword(INPUT, KEYWORD_TABLE, *KEYWORD, *FUNCTION_PTR)
 *
 * Lookup INPUT in KEYWORDTABLE, allowing for unique abbreviations.  Multiple
 * matches are noted as an error, but only if there is not an exact match.
 *
 * rc == 1 for success, KEYWORD is set to the uniquely matched keyword, and
 * FUNCTION_PTR is set to the function associatd with the found keyword.
 */

int lookup_keyword(const char *input, KW_item kwtbl[], const char **keyword, int (**func)(), ArgType *type) {
  int i = 0, j, ret;
  int exact = 0;
  int in_len = strlen(input), kw_len;
  *func = NULL;                           // part of the result
  while (kwtbl[i].keyword != NULL) {
    kw_len = 0;                           // no matches yet
    if (strncasecmp(input, kwtbl[j = i++].keyword, in_len) == 0) { // match on the primary name?
      kw_len = strlen(kwtbl[j].keyword);
      if (in_len == kw_len) exact = 1;    // track exact matches
    } 
    if (strncasecmp(input, kwtbl[j].alias, in_len) == 0) {  // alias match?
      kw_len = strlen(kwtbl[j].alias);
      if (in_len == kw_len) exact = 1;    // track exact matches
    } 
    if (kw_len == 0) continue;            // no match, try next entry
    if (*func != NULL) 
      errorf("'%s' is ambiguous; use more letters\n", input);
    *keyword = kwtbl[j].keyword;	  // save the keyword
    *func    = kwtbl[j].func;	          // we want to find only one keyword
    *type    = kwtbl[j].type;	          // set the arg type, too
    if (exact) break;                     // exact matches trup ambiguities
  }
  return (*func != NULL);
}

/* rc = parse_keyword(INPUT)
 *
 * Parse the next keyword in the INPUT, and then parse the subsequent
 * argument as needed by the keyword.
 *
 * Return 1 -- sucess; 0 -- failure (no keyword)
 */

int parse_keyword(const char *input) {
  const char *keyword, *matched_keyword;
  int matches;
  const char **substr_list;
  int rc;

  // scan a keyword followed by a blank or EOL 
  rc = re_match("^\\s*([a-z]+)(?:\\s|$)", input, &matches, &substr_list);
  if (rc == 0) return 0;                  // fail early

  keyword = strdup(substr_list[1]);       // get the keyword
  dtalkf("Parsed keyword: '%s'\n", keyword);

  // get the start of the argument string
  const char *argstr = input + strlen(substr_list[0]);  
  re_done(substr_list);                   // release the substr list

  // lookup the keyword
  int (*func)();
  ArgType type;

  if (lookup_keyword(keyword, parse_keywords, &matched_keyword, &func, &type) == 0) {
    talkf("No match on '%s'\n", keyword);
    return -1;                            // error return
  }

  // found a keyword
  dtalkf("Matched keyword: '%s'\n", matched_keyword);

  if (type != NO_ARG) {
    // now parse the argment string -- it's okay if there are none
    rc = re_match("^\\s*(\\S.*)\\s*$", argstr, &matches, &substr_list);
    if (rc <= 0) {
      talkf("Keyword '%s' ('%s') needs an argument.\n", keyword, matched_keyword);
      return -1;                          // error return
    }
    argstr = strdup(substr_list[1]);      // copy the scanned arg
    re_done(substr_list);                 // release this substr list
  }
  return (*func)(1, argstr);              // invoke the parse function, with the args
}


/* trim_string(char *str)
 *
 * Turn trailing whitespace into nulls.
 */

const char *trim_string(char *str) {
  char *p = str + strlen(str);
  while (isspace(*p)) { *p-- = '\0'; }    // trim trailing whitespace
  p = str;
  while (isspace(*p)) p++;                // skip leading whitespace
  return p;
}

/* parse_actions -- read and process a command from the parse_buffer
 * (initialized by parse_init).
 */

int parse_action(const char * input_buffer) {

  const char *keyword, *args, **matched_keyword;
  int matches;
  const char **substr_list;
  int rc = parse_keyword(input_buffer);     // parse the keyword
  if (0 == rc) {                            // if no keyword, then
    return parse_dates(input_buffer);       // try to match the input buffer for dates
  }
  return 0;
}

void show_date(int year, int month, int day) {
  struct tm date;
  date.tm_mday = day;
  date.tm_mon  = month - 1;
  date.tm_year = year - 1900;
  int chars;
  char datestr[50];
  chars = strftime(datestr, 50, "%F", &date);
  if (chars > 0) {
    printf("%s\n", datestr);
  } else {
    talkf("Error in strftime: datestr = '%s'\n", datestr);
  }
}

#define BUF_SIZE 255
/* main program */

int main (int argc, char** argv) {

  int ch;
  debug = norun = verbose = 0;
  int interactive = 0;

  date_format = "%F";                   // the default date format

  /* read the options */

  while ((ch = getopt(argc, argv, "dihnv")) != -1) {
    switch (ch) {
      case 'd': debug++ ;        break ;
      case 'h': usage(1);
      case 'i': interactive = 1; break ;
      case 'n': norun++ ;        break ;
      case 'v': verbose++ ;      break ;
      case '?': errorf("unknown option: '%c'\n", optopt); break;
      case ':': errorf("Missing argument to option '%c'\n", optopt); break;
      default: usage(0);
    }
  }

  argc -= optind;
  argv += optind;

  /* Now parse the commands and arguments */ 

  static char parse_buffer[BUF_SIZE];

  if (interactive || argc == 0) {
    int line = 1;
    const char *bufp;
    while (!feof(stdin)) {
      if (isatty(0)) fprintf(stdout, "cal %d: ", line++);
      fgets(parse_buffer, BUF_SIZE, stdin);
      bufp = trim_string(parse_buffer);
      if (strlen(bufp) == 0) {          // don't parse empty lines
        line--;                         // don't count them either
      } else {
        parse_action(bufp);
      }
      parse_buffer[0] = '\0';
    }
  } else {
    parse_init_from_arg_buffer(argc, argv, parse_buffer, BUF_SIZE);
    parse_action(parse_buffer);
  }
  exit(0);
}

