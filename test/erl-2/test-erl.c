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
  ETERM *tuplep;
  ETERM *fnp, *argp;
  byte buf[220];
  int buf_len;

  erl_init(NULL, 0);

  while (read_cmd(buf, sizeof(buf)) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);
    argp = erl_element(2, tuplep);
    
    if (strncmp(ERL_ATOM_PTR(fnp), "foo", 3) == 0) {
      const int res = foo(ERL_INT_VALUE(argp));
      ETERM *intp = erl_mk_int(res);
      erl_encode(intp, buf);
      buf_len = erl_term_len(intp);
      erl_free_term(intp);
    } else if (strncmp(ERL_ATOM_PTR(fnp), "bar", 3) == 0) {
      const int res = bar(ERL_INT_VALUE(argp));
      ETERM *intp = erl_mk_int(res);
      erl_encode(intp, buf);
      buf_len = erl_term_len(intp);
      erl_free_term(intp);
    } else if (strncmp(ERL_ATOM_PTR(fnp), "xxx", 3) == 0) {
      ETERM *str = erl_mk_string("Humpf, Mops, Oerks!");
      erl_encode(str, buf);
      buf_len = erl_term_len(str);
      erl_free_term(str);
    } else if (strncmp(ERL_ATOM_PTR(fnp), "yyy", 3) == 0) {
      ETERM *str = erl_mk_estring("Humpf, Mops, Oerks!", 13);
      erl_encode(str, buf);
      buf_len = erl_term_len(str);
      erl_free_term(str);
    } else if (strncmp(ERL_ATOM_PTR(fnp), "fff", 3) == 0) {
      unsigned int i = 0;
      ETERM *strs[3];
      ETERM *lst;
      strs[i++] = erl_mk_string("Humpf");
      strs[i++] = erl_mk_string("Mops");
      strs[i++] = erl_mk_string("Oerks");
      lst = erl_mk_list(strs, (sizeof(strs)/sizeof(strs[0])));
      erl_encode(lst, buf);
      buf_len = erl_term_len(lst);
      for (i=0; i<(sizeof(strs)/sizeof(strs[0])); i++) {
	erl_free_term(strs[i]);
      }
      erl_free_term(lst);
    } else if (strncmp(ERL_ATOM_PTR(fnp), "aaa", 3) == 0) {
      unsigned int i = 0;
      ETERM *strs[3];
      ETERM *lst;
      strs[i++] = erl_mk_string("Humpf");
      strs[i++] = erl_mk_binary("Mops", strlen("Mops"));
      strs[i++] = erl_mk_string("Oerks");
      lst = erl_mk_tuple(strs, (sizeof(strs)/sizeof(strs[0])));
      erl_encode(lst, buf);
      buf_len = erl_term_len(lst);
      for (i=0; i<(sizeof(strs)/sizeof(strs[0])); i++) {
	erl_free_term(strs[i]);
      }
      erl_free_term(lst);
    } else {
      ETERM *trm = erl_mk_atom("function_name_error");
      erl_encode(trm, buf);
      buf_len = erl_term_len(trm);
      erl_free_term(trm);
    }

    write_cmd(buf, buf_len);

    erl_free_compound(tuplep);
    erl_free_term(fnp);
    erl_free_term(argp);
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
