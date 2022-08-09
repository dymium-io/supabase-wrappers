/*
  to enable debug
  #define _D
  otherwise
  #define _D if (0)
*/
#ifndef _D
#define _D if (0)
#endif
static inline void obfuscate(char *valstr) {
  int k;
  _D printf("Obfuscate: [%s] => ", valstr);
  for(k = 0; valstr[k] != 0; ++k) {
    char c = valstr[k];
    if(c >= '0' && c <= '9') {
      valstr[k] = '0' + (((unsigned int)(c - '0')) + 6553*(k+1)) % 10;
    } else if(c >= 'a' && c <= 'z') {
      valstr[k] = 'a' + (((unsigned int)(c - 'a')) + 2743*(k+13)) % 26;
    } else if(c >= 'A' && c <= 'Z') {
      valstr[k] = 'A' + (((unsigned int)(c - 'A')) + 1483*(k+285)) % 26;
    }
  }
  _D printf("[%s]\n",valstr);
  _D fflush(stdout);
}

static inline void redact_text(char *valstr) {
  int k;
  _D printf("Redact as string: [%s] => ", valstr);
  for(k=0; k != 3 && valstr[k] != 0; ++k) { valstr[k] = 'x'; } valstr[k] = 0;
  _D printf("[%s]\n",valstr);
  _D fflush(stdout);  
}

static inline void redact_number(char *valstr) {
  _D printf("Redact as number: [%s] => ", valstr);
  if(valstr[0] != 0) {
    valstr[0] = '0';
    valstr[1] = 0;
  }
  _D printf("[%s]\n",valstr);
  _D fflush(stdout);
}

static inline void redact_something(char *valstr) {
  int k;
  _D printf("Redact as something: [%s] => ", valstr);
  for(k = 0; valstr[k] != 0; ++k) {
    char c = valstr[k];
    if(c >= '0' && c <= '9') {
      valstr[k] = '0';
    } else if(c >= 'a' && c <= 'z') {
      valstr[k] = 'a';
    } else if(c >= 'A' && c <= 'Z') {
      valstr[k] = 'a';
    }
  }
  _D printf("[%s]\n",valstr);
  _D fflush(stdout);
}

static inline void redact_xml(char *valstr) {
  _D printf("Redact as xml: [%s] => ", valstr);
  if(strlen(valstr) + 1 >= sizeof("<?xml version=\"1.0\"?><_/>")) {
    memcpy(valstr, "<?xml version=\"1.0\"?><_/>", sizeof("<?xml version=\"1.0\"?><_/>"));
  } else if(strlen(valstr) + 1 >= sizeof("<_/>")) {
    memcpy(valstr, "<_/>", sizeof("<_/>"));
  } else if(valstr[0] != 0) {
    valstr[0] = 0;
  }
  _D printf("[%s]\n",valstr);
  _D fflush(stdout);
}

static inline void redact_json(char *valstr) {
  _D printf("Redact as json: [%s] => ", valstr);
  if(strlen(valstr) + 1 >= sizeof("{}")) {
    memcpy(valstr, "{}", sizeof("{}"));
  } else if(valstr[0] != 0) {
    valstr[0] = 0;
  }
  _D printf("[%s]\n",valstr);
  _D fflush(stdout);
}

static inline void redact_bytea(char *valstr) {
  _D printf("Redact as bytea: [%s] => ", valstr);
  if(strlen(valstr) + 1 >= sizeof("\\x")) {
    memcpy(valstr, "\\x", sizeof("\\x"));
  } else if(valstr[0] != 0) {
    valstr[0] = 0;
  }
  _D printf("[%s]\n",valstr);
  _D fflush(stdout);
}

static inline void redact_bool(char *valstr) {
  _D printf("Redact as bool: [%s] => ", valstr);
  if(strlen(valstr) + 1 >= sizeof("f")) {
    memcpy(valstr, "f", sizeof("f"));
  } else if(valstr[0] != 0) {
    valstr[0] = 0;
  }
  _D printf("[%s]\n",valstr);
  _D fflush(stdout);
}
