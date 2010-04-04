// Copyright 2010 Tuncer Ayaz. All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

extern "C" {
#include "erl_nif.h"
}

#include <re2/re2.h>
#include <map>
#include <vector>
#include <iostream>

static ErlNifResourceType* re2_RESOURCE;

typedef struct
{
} re2_handle;

extern "C" {
// Prototypes
ERL_NIF_TERM re2_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM re2_match(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
  {"new", 0, re2_new},
  {"match", 3, re2_match}
};

static void re2_resource_cleanup(ErlNifEnv* env, void* arg);
static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

ERL_NIF_INIT(re2, nif_funcs, &on_load, NULL, NULL, NULL);
} // extern "C"

static ERL_NIF_TERM error(ErlNifEnv* env, const char * msg)
{
  return enif_make_tuple2(env, enif_make_atom(env,"error"),
      enif_make_atom(env,msg));
}

ERL_NIF_TERM re2_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  re2_handle* handle = (re2_handle*)enif_alloc_resource(
      env, re2_RESOURCE, sizeof(re2_handle));
  ERL_NIF_TERM result = enif_make_resource(env, handle);
  enif_release_resource(env, handle);
  return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

#define RE2_ERR_ALLOCBIN "enif_alloc_binary"

ERL_NIF_TERM re2_match(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary sdata;
  ErlNifBinary rdata;
  if (enif_inspect_iolist_as_binary(env, argv[1], &sdata) &&
      enif_inspect_iolist_as_binary(env, argv[2], &rdata))
  {
    const re2::StringPiece s((const char*)sdata.data, sdata.size);
    const re2::StringPiece p((const char*)rdata.data, rdata.size);
    const RE2 re(p);

    printf("--- s\"%s\"  p\"%s\" cg:%d\n",
        s.as_string().c_str(),
        p.as_string().c_str(),
        re.NumberOfCapturingGroups());
    int n = re.NumberOfCapturingGroups()+1;
    re2::StringPiece group[n];

    if (re.Match(s,0,RE2::UNANCHORED,group,n)) {
      const std::map<std::string, int>& nmap = re.NamedCapturingGroups();
      if (nmap.size() > 0) {
        std::map<const std::string,const re2::StringPiece> matches;
        for (std::map<std::string, int>::const_iterator it = nmap.begin();
            it != nmap.end(); ++it)
        {
          int index = (it->second);
          const std::string name = it->first;
          const re2::StringPiece match = group[index];
          matches.insert(std::pair<const std::string,const re2::StringPiece>(
                name,match));
        }
        int arrsz = matches.size();
        ERL_NIF_TERM arr[arrsz];
        int i = 0;
        for (std::map<const std::string,const re2::StringPiece>::const_iterator
                it = matches.begin(); it != matches.end(); ++it)
        {
          const std::string name = it->first;
          const re2::StringPiece match = it->second;

          ErlNifBinary bname;
          if(!enif_alloc_binary(env, name.size(), &bname))
            return error(env, RE2_ERR_ALLOCBIN);
          memcpy(bname.data, name.data(), name.size());

          ErlNifBinary bmatch;
          if(!enif_alloc_binary(env, match.size(), &bmatch))
            return error(env, RE2_ERR_ALLOCBIN);
          memcpy(bmatch.data, match.data(), match.size());

          arr[i] = enif_make_tuple2(env,
              enif_make_binary(env, &bname), enif_make_binary(env, &bmatch));
          i++;
        }
        return enif_make_tuple2(env,
            enif_make_atom(env,"match"),
            enif_make_list_from_array(env,arr,arrsz));
      } else {
        std::vector<ERL_NIF_TERM> matches;
        for(int i=0; i<n; i++) {
          const re2::StringPiece match = group[i];

          ErlNifBinary bmatch;
          if(!enif_alloc_binary(env, match.size(), &bmatch))
            return error(env, RE2_ERR_ALLOCBIN);
          memcpy(bmatch.data, match.data(), match.size());

          matches.push_back(enif_make_binary(env, &bmatch));
        }
        return enif_make_tuple2(env,
            enif_make_atom(env,"match"),
            enif_make_list_from_array(env,&matches[0],matches.size()));
      }
    } else {
      return enif_make_atom(env,"nomatch");
    }
  } else {
    return enif_make_badarg(env);
  }
}

static void re2_resource_cleanup(ErlNifEnv* env, void* arg)
{
  // Delete any dynamically allocated memory stored in re2_handle
  // re2_handle* handle = (re2_handle*)arg;
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags =
    (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
  re2_RESOURCE = enif_open_resource_type(env, "re2_resource",
      &re2_resource_cleanup,
      flags,
      0);
  return 0;
}
