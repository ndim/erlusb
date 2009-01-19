#include <string.h>
#include <unistd.h>

#include <erl_interface.h>
#include <ei.h>

typedef unsigned char byte;

int foo(int x);
int bar(int y);

int write_exact(byte *buf, int len);
int read_exact(byte *buf, size_t size, int len);
int read_cmd(byte *buf, size_t size);
int write_cmd(byte *buf, int len);

int main() {
  ETERM *tuplep, *intp;
  ETERM *fnp, *argp;
  int res;
  byte buf[100];
  /* long allocated, freed; */

  erl_init(NULL, 0);

  while (read_cmd(buf, sizeof(buf)) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);
    argp = erl_element(2, tuplep);
    
    if (strncmp(ERL_ATOM_PTR(fnp), "foo", 3) == 0) {
      res = foo(ERL_INT_VALUE(argp));
    } else if (strncmp(ERL_ATOM_PTR(fnp), "bar", 3) == 0) {
      res = bar(ERL_INT_VALUE(argp));
    } else {
      res = 0;
    }

    intp = erl_mk_int(res);
    erl_encode(intp, buf);
    write_cmd(buf, erl_term_len(intp));

    erl_free_compound(tuplep);
    erl_free_term(fnp);
    erl_free_term(argp);
    erl_free_term(intp);
  }
  return 0;
}
      
int read_cmd(byte *buf, size_t size)
{
  int len;

  if (read_exact(buf, size, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, size, len);
}

int write_cmd(byte *buf, int len)
{
  byte li;

  li = (len >> 8) & 0xff;
  write_exact(&li, 1);
  
  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}

int read_exact(byte *buf, size_t size, int len)
{
  int i, got=0;

  if (((size_t)len)>size)
    return -1;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);

  return(len);
}

int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}

int foo(int x)
{
  /*
  fprintf(stdout, "[foo]");
  fflush(stdout);
  */
  return x+1;
}

int bar(int y)
{
  /*
  fprintf(stdout, "[bar]");
  fflush(stdout);
  */
  return y*2;
}
