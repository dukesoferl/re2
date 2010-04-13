// Copyright 2010 Tuncer Ayaz. All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

extern "C" {
#include "erl_nif.h"
}

#include <re2/re2.h>
#include <map>
#include <vector>

typedef struct
{
  RE2* re;
} re2_handle;

extern "C" {
  // Prototypes
  ERL_NIF_TERM re2_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
  ERL_NIF_TERM re2_match(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

  static ErlNifFunc nif_funcs[] =
    {
      {"new", 0, re2_new},
      {"new", 2, re2_new},
      {"match", 3, re2_match},
      {"match", 4, re2_match},
    };

  static void re2_resource_cleanup(ErlNifEnv* env, void* arg);
  static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

  ERL_NIF_INIT(re2, nif_funcs, &on_load, NULL, NULL, NULL);
} // extern "C"


// static variables
static ErlNifResourceType* re2_resource;
static ERL_NIF_TERM a_ok;
static ERL_NIF_TERM a_error;
static ERL_NIF_TERM a_match;
static ERL_NIF_TERM a_nomatch;
static ERL_NIF_TERM a_capture;
static ERL_NIF_TERM a_offset;
static ERL_NIF_TERM a_all;
static ERL_NIF_TERM a_all_but_first;
static ERL_NIF_TERM a_first;
static ERL_NIF_TERM a_none;
static ERL_NIF_TERM a_index;
static ERL_NIF_TERM a_binary;
static ERL_NIF_TERM a_err_alloc_binary;
static ERL_NIF_TERM a_err_offset_not_int;
static ERL_NIF_TERM a_err_malloc_a_id;
static ERL_NIF_TERM a_err_malloc_str_id;
static ERL_NIF_TERM a_err_re2_obj_not_ok;

static ERL_NIF_TERM error(ErlNifEnv* env, ERL_NIF_TERM err);
static void init_atoms(ErlNifEnv* env);

namespace {
  struct matchoptions {
    enum valuespec { VS_ALL,VS_ALL_BUT_FIRST,VS_FIRST,VS_NONE,VS_VLIST };
    enum capture_type { CT_INDEX,CT_LIST,CT_BINARY };

    int offset;
    enum valuespec vs;
    enum capture_type ct;
    ERL_NIF_TERM vlist;

    matchoptions():offset(0), vs(VS_ALL), ct(CT_BINARY) {};
    void info() const {
      printf("matchoptions offset:%d vs:%d ct:%d",
             offset,vs,ct);
      printf("\n");
    }
  };

  template <typename T>
  class autohandle {
  private: bool keep_;  T* ptr_;
  public:
    autohandle():keep_(false),ptr_(NULL){}
    autohandle(T* ptr,bool keep=false):keep_(keep),ptr_(ptr_){}
    void set(T* ptr,bool keep=false) { ptr_=ptr; keep_=keep; }
    ~autohandle() { if (!keep_) { delete ptr_; ptr_=NULL; } }
    T* operator->() { return ptr_; }
  };
}

static bool parse_matchoptions(ErlNifEnv* env, const ERL_NIF_TERM list,
                               matchoptions& opts, ERL_NIF_TERM *err);

static ERL_NIF_TERM mres(ErlNifEnv* env,
                         const re2::StringPiece& str,
                         const re2::StringPiece& match,
                         const matchoptions::capture_type ct);



ERL_NIF_TERM re2_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary rdata;

  if (argc == 0)
  {
    re2_handle* handle = (re2_handle*)enif_alloc_resource(
        env, re2_resource, sizeof(re2_handle));
    handle->re = NULL;
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(env, handle);
    return enif_make_tuple2(env, a_ok, result);
  }
  else if (argc == 2 && enif_inspect_iolist_as_binary(env, argv[0], &rdata))
  {
    const re2::StringPiece p((const char*)rdata.data, rdata.size);
    re2_handle* handle = (re2_handle*)enif_alloc_resource(
        env, re2_resource, sizeof(re2_handle));
    handle->re = new RE2(p);

    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(env, handle);
    return enif_make_tuple2(env, a_ok, result);
  }
  else
  {
    return enif_make_badarg(env);
  }
}

static void re2_resource_cleanup(ErlNifEnv* env, void* arg)
{
  // Delete any dynamically allocated memory stored in re2_handle
  re2_handle* handle = (re2_handle*)arg;
  if (handle->re != NULL)
  {
    delete handle->re;
    handle->re = NULL;
  }
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags =
    (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
  re2_resource = enif_open_resource_type(env, "re2_resource",
                                         &re2_resource_cleanup,
                                         flags,
                                         0);

  init_atoms(env);

  return 0;
}

ERL_NIF_TERM re2_match(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary sdata;
  ErlNifBinary rdata;
  if (enif_inspect_iolist_as_binary(env, argv[1], &sdata) &&
      enif_inspect_iolist_as_binary(env, argv[2], &rdata))
  {
    const re2::StringPiece s((const char*)sdata.data, sdata.size);
    const re2::StringPiece p((const char*)rdata.data, rdata.size);
    autohandle<RE2> re;
    re2_handle* handle;
    if (enif_get_resource(env, argv[0], re2_resource, (void**)&handle)
        && handle->re != NULL)
    {
      re.set(handle->re,true);
    }
    else
    {
      re.set(new RE2(p));
    }

    if (!re->ok())
      return error(env, a_err_re2_obj_not_ok);

    int n = re->NumberOfCapturingGroups()+1;
    re2::StringPiece group[n];

    if (argc < 3 || argc > 4)
      return enif_make_badarg(env);

    matchoptions opts;
    ERL_NIF_TERM opterr;
    if (argc == 4 && !parse_matchoptions(env, argv[3], opts, &opterr))
      return opterr;

    //opts.info();
    //printf("match '%s' '%s'\n", s.as_string().c_str(), p.as_string().c_str());
    if (re->Match(s,opts.offset,RE2::UNANCHORED,group,n)) {

      int start = 0;
      int arrsz = n;

      if (opts.vs == matchoptions::VS_NONE) {
        // return match atom only
        return a_match;

      } else if (opts.vs == matchoptions::VS_FIRST) {
        // return first match only
        ERL_NIF_TERM first = mres(env, s, group[0], opts.ct);
        return enif_make_tuple2(env, a_match,
            enif_make_list1(env, first));

      } else if (opts.vs == matchoptions::VS_ALL_BUT_FIRST) {
        // skip first match
        start = 1;
        arrsz--;
      }

      if (opts.vs == matchoptions::VS_VLIST) {
        // return subpatterns as specified in ValueList

        std::vector<ERL_NIF_TERM> vec;
        const std::map<std::string, int>& nmap = re->NamedCapturingGroups();
        ERL_NIF_TERM VL,VH,VT;

        // Limit ValueID atom()/string() length
        // as NIF API has no way to check size, yet.
        const size_t max_id_atom_len = 128+1;
        const size_t max_id_str_len = max_id_atom_len;

        // empty StringPiece for unfound ValueIds
        const re2::StringPiece empty;

        for (VL=opts.vlist; enif_get_list_cell(env, VL, &VH, &VT); VL=VT) {
          int nid = 0;
          if (enif_get_int(env, VH, &nid) && nid > 0) {

            // ValueID int()

            if (nid < n) {
              const re2::StringPiece match = group[nid];
              if (!match.empty())
                vec.push_back(mres(env, s, group[nid], opts.ct));
              else
                vec.push_back(mres(env, s, empty, opts.ct));

            } else {
              vec.push_back(mres(env, s, empty, opts.ct));
            }

          } else if (enif_is_atom(env, VH)) {

            // ValueID atom()

            char *a_id = (char*)malloc(max_id_atom_len);
            if (a_id == NULL)
              return error(env, a_err_malloc_a_id);

            if (enif_get_atom(env, VH, a_id, max_id_atom_len) > 0) {
              std::map<std::string, int>::const_iterator it =
                nmap.find(a_id);
              if (it != nmap.end()) {
                re2::StringPiece match = group[it->second];
                vec.push_back(mres(env, s, group[it->second], opts.ct));
              } else {
                vec.push_back(mres(env, s, empty, opts.ct));
              }
            }
            free(a_id);

          } else {

            // ValueID string()

            char *str_id = (char*)malloc(max_id_str_len);
            if (str_id == NULL)
              return error(env, a_err_malloc_str_id);

            if (enif_get_string(env, VH, str_id, max_id_str_len,
                  ERL_NIF_LATIN1) > 0)
            {
              std::map<std::string, int>::const_iterator it =
                nmap.find(str_id);

              if (it != nmap.end()) {
                re2::StringPiece match = group[it->second];
                vec.push_back(mres(env, s, group[it->second], opts.ct));
              } else {
                vec.push_back(mres(env, s, empty, opts.ct));
              }
            }
            free(str_id);
          }
        }
        return enif_make_tuple2(env,
                a_match,
                enif_make_list_from_array(env,&vec[0],vec.size()));

      } else {

        ERL_NIF_TERM arr[arrsz];
        for(int i = start, arridx=0; i < n; i++,arridx++)
          arr[arridx] = mres(env, s, group[i], opts.ct);

        return enif_make_tuple2(env,
            a_match,
            enif_make_list_from_array(env,arr,arrsz));
      }

    } else {

      return a_nomatch;
    }

  } else {

    return enif_make_badarg(env);
  }
}


//
// internal functions
//


static void init_atoms(ErlNifEnv* env)
{
  a_ok = enif_make_atom(env, "ok");
  a_error = enif_make_atom(env, "error");
  a_match = enif_make_atom(env, "match");
  a_nomatch = enif_make_atom(env, "nomatch");
  a_capture = enif_make_atom(env, "capture");
  a_offset = enif_make_atom(env, "offset");
  a_all = enif_make_atom(env, "all");
  a_all_but_first = enif_make_atom(env, "all_but_first");
  a_first = enif_make_atom(env, "first");
  a_none = enif_make_atom(env, "none");
  a_index = enif_make_atom(env, "index");
  a_binary = enif_make_atom(env, "binary");
  a_err_alloc_binary = enif_make_atom(env, "alloc_binary");
  a_err_offset_not_int = enif_make_atom(env, "offset_not_int");
  a_err_malloc_a_id = enif_make_atom(env, "malloc_a_id");
  a_err_malloc_str_id = enif_make_atom(env, "malloc_str_id");
  a_err_re2_obj_not_ok = enif_make_atom(env, "re2_obj_not_ok");
}

static ERL_NIF_TERM error(ErlNifEnv* env, ERL_NIF_TERM err)
{
  return enif_make_tuple2(env, a_error, err);
}

/*
Options = [ Option ]
Option = {offset, int()} | {capture, ValueSpec} | {capture, ValueSpec, Type}
Type = index | binary
ValueSpec = all | all_but_first | first | none | ValueList
ValueList = [ ValueID ]
ValueID = int() | string() | atom()
*/
static bool parse_matchoptions(ErlNifEnv* env, const ERL_NIF_TERM list,
                               matchoptions& opts, ERL_NIF_TERM *err)
{
  if (enif_is_empty_list(env, list))
    return true;

  ERL_NIF_TERM L,H,T;

  for (L=list; enif_get_list_cell(env, L, &H, &T); L=T) {
    const ERL_NIF_TERM *tuple;
    int tuplearity = -1;
    if (enif_get_tuple(env, H, &tuplearity, &tuple)) {

      if (tuplearity == 2 || tuplearity == 3) {

        // {offset,N} or {capture,ValueSpec}

        if (enif_is_identical(env, tuple[0], a_offset)) {

          // {offset, int()}

          int offset = 0;
          if (enif_get_int(env, tuple[1], &offset)) {
            opts.offset = offset;
          } else {
            *err = error(env, a_err_offset_not_int);
            return false;
          }

        } else if (enif_is_identical(env, tuple[0], a_capture)) {

          // {capture,ValueSpec,Type}

          bool vs_set = false;
          if (enif_is_atom(env, tuple[1])) {

            // ValueSpec = all | all_but_first | first | none

            if(enif_is_atom(env, tuple[1]) > 0) {
              if (enif_is_identical(env, tuple[1], a_all))
                opts.vs = matchoptions::VS_ALL;
              else if (enif_is_identical(env, tuple[1], a_all_but_first))
                opts.vs = matchoptions::VS_ALL_BUT_FIRST;
              else if (enif_is_identical(env, tuple[1], a_first))
                opts.vs = matchoptions::VS_FIRST;
              else if (enif_is_identical(env, tuple[1], a_none))
                opts.vs = matchoptions::VS_NONE;

              vs_set = true;
            }

          } else if (!enif_is_empty_list(env, tuple[1])) {

            // ValueSpec = ValueList
            // ValueList = [ ValueID ]
            // ValueID = int() | string() | atom()

            opts.vlist = tuple[1];
            vs_set = true;
            opts.vs = matchoptions::VS_VLIST;
          }

          // Type = index | binary

          if (tuplearity == 3 && vs_set) {
            if (enif_is_identical(env, tuple[2], a_index))
              opts.ct = matchoptions::CT_INDEX;
            else if (enif_is_identical(env, tuple[2], a_binary))
              opts.ct = matchoptions::CT_BINARY;
          }
        }
      }
    }
  }

  return true;
}

static ERL_NIF_TERM mres(ErlNifEnv* env,
                         const re2::StringPiece& str,
                         const re2::StringPiece& match,
                         const matchoptions::capture_type ct)
{
  switch (ct) {
    default:
    case matchoptions::CT_INDEX:
      int l, r;
      if (match.empty()) {
        l = -1;
        r = 0;
      } else {
        l = match.data() - str.data();
        r = l + match.size();
      }
      return enif_make_tuple2(env,
                              enif_make_int(env, l),
                              enif_make_int(env, r));
      break;
    case matchoptions::CT_BINARY:
      ErlNifBinary bmatch;
      if(!enif_alloc_binary(env, match.size(), &bmatch))
        return error(env, a_err_alloc_binary);
      memcpy(bmatch.data, match.data(), match.size());
      return enif_make_binary(env, &bmatch);
      break;
  }
}
