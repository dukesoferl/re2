// Copyright 2010-2015 Tuncer Ayaz. All Rights Reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "erl_nif.h"

#include <stdio.h>
#include <re2/re2.h>
#include <map>
#include <vector>

namespace {
    struct compileoptions {
        re2::RE2::Options re2opts;
    };

    struct matchoptions {
        enum valuespec { VS_ALL,VS_ALL_BUT_FIRST,VS_FIRST,VS_NONE,VS_VLIST };
        enum capture_type { CT_INDEX,CT_LIST,CT_BINARY };

        bool caseless;
        int offset;
        enum valuespec vs;
        enum capture_type ct;
        ERL_NIF_TERM vlist;

        matchoptions(ErlNifEnv* env)
            :caseless(false), offset(0), vs(VS_ALL), ct(CT_BINARY)
            { vlist = enif_make_list(env, 0); }
    };

    struct replaceoptions {
        bool global;
        replaceoptions():global(false) {}
    };

    template <typename T>
    class autohandle {
    private: bool keep_; T* ptr_;
    public:
        autohandle():keep_(false),ptr_(NULL){}
        autohandle(T* ptr,bool keep=false):keep_(keep), ptr_(ptr){}
        void set(T* ptr,bool keep=false) { ptr_=ptr; keep_=keep; }
        ~autohandle() { if (!keep_) { enif_free(ptr_); ptr_=NULL; } }
        T* operator->() const { return ptr_; }
        T* operator&() const { return ptr_; }
    };
}

struct re2_handle {
    // RE2 objects are thread safe. no locking required.
    re2::RE2* re;
};

//
// Use a union for pointer type conversion to avoid compiler warnings
// about strict-aliasing violations with gcc-4.1. gcc >= 4.2 does not
// emit the warning.
// TODO: Reconsider use of union once gcc-4.1 is obsolete?
//
union re2_handle_union {
    void* vp;
    re2_handle* p;
};

static void cleanup_handle(re2_handle* handle);
static void init_atoms(ErlNifEnv* env);
static bool parse_compile_options(ErlNifEnv* env, const ERL_NIF_TERM list,
                                  re2::RE2::Options& opts);
static bool parse_match_options(ErlNifEnv* env, const ERL_NIF_TERM list,
                                matchoptions& opts);
static void parse_match_capture_options(ErlNifEnv* env, matchoptions& opts,
                                        const ERL_NIF_TERM* tuple,
                                        int tuplearity);
static bool parse_replace_options(ErlNifEnv* env, const ERL_NIF_TERM list,
                                  replaceoptions& opts);
static ERL_NIF_TERM re2_match_ret_vlist(ErlNifEnv* env,
                                        const autohandle<re2::RE2>& re,
                                        const re2::StringPiece& s,
                                        const matchoptions& opts,
                                        std::vector<re2::StringPiece>& group,
                                        int n);
static ERL_NIF_TERM error(ErlNifEnv* env, const ERL_NIF_TERM err);
static ERL_NIF_TERM re2error(ErlNifEnv* env, const re2::RE2* const re);
static ERL_NIF_TERM mres(ErlNifEnv* env,
                         const re2::StringPiece& str,
                         const re2::StringPiece& match,
                         const matchoptions::capture_type ct);
static ERL_NIF_TERM rres(ErlNifEnv* env, const std::string& s);
static char* alloc_atom(ErlNifEnv* env, const ERL_NIF_TERM atom, unsigned* len);
static char* alloc_str(ErlNifEnv* env, const ERL_NIF_TERM list, unsigned* len);

extern "C" {
    // Prototypes
    static ERL_NIF_TERM re2_compile(ErlNifEnv* env, int argc,
                                    const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM re2_match(ErlNifEnv* env, int argc,
                                  const ERL_NIF_TERM argv[]);
    static ERL_NIF_TERM re2_replace(ErlNifEnv* env, int argc,
                                    const ERL_NIF_TERM argv[]);

    static ErlNifFunc nif_funcs[] =
    {
        {"compile", 1, re2_compile},
        {"compile", 2, re2_compile},
        {"match",   2, re2_match},
        {"match",   3, re2_match},
        {"replace", 3, re2_replace},
        {"replace", 4, re2_replace},
    };

    static void re2_resource_cleanup(ErlNifEnv* env, void* arg);
    static int on_load(ErlNifEnv* env, void** priv_data,
                       ERL_NIF_TERM load_info);

    ERL_NIF_INIT(re2, nif_funcs, &on_load, NULL, NULL, NULL)
} // extern "C"


// static variables
static ErlNifResourceType* re2_resource_type = NULL;
static ERL_NIF_TERM a_ok;
static ERL_NIF_TERM a_error;
static ERL_NIF_TERM a_match;
static ERL_NIF_TERM a_nomatch;
static ERL_NIF_TERM a_capture;
static ERL_NIF_TERM a_global;
static ERL_NIF_TERM a_offset;
static ERL_NIF_TERM a_all;
static ERL_NIF_TERM a_all_but_first;
static ERL_NIF_TERM a_first;
static ERL_NIF_TERM a_none;
static ERL_NIF_TERM a_index;
static ERL_NIF_TERM a_binary;
static ERL_NIF_TERM a_caseless;
static ERL_NIF_TERM a_max_mem;
static ERL_NIF_TERM a_err_alloc_binary;
static ERL_NIF_TERM a_err_enif_alloc;
static ERL_NIF_TERM a_err_get_atom;
static ERL_NIF_TERM a_err_get_string;
static ERL_NIF_TERM a_re2_NoError;
static ERL_NIF_TERM a_re2_ErrorInternal;
static ERL_NIF_TERM a_re2_ErrorBadEscape;
static ERL_NIF_TERM a_re2_ErrorBadCharClass;
static ERL_NIF_TERM a_re2_ErrorBadCharRange;
static ERL_NIF_TERM a_re2_ErrorMissingBracket;
static ERL_NIF_TERM a_re2_ErrorMissingParen;
static ERL_NIF_TERM a_re2_ErrorTrailingBackslash;
static ERL_NIF_TERM a_re2_ErrorRepeatArgument;
static ERL_NIF_TERM a_re2_ErrorRepeatSize;
static ERL_NIF_TERM a_re2_ErrorRepeatOp;
static ERL_NIF_TERM a_re2_ErrorBadPerlOp;
static ERL_NIF_TERM a_re2_ErrorBadUTF8;
static ERL_NIF_TERM a_re2_ErrorBadNamedCapture;
static ERL_NIF_TERM a_re2_ErrorPatternTooLarge;


static int on_load(ErlNifEnv* env, void**, ERL_NIF_TERM)
{
    ErlNifResourceFlags flags =
        (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "re2_resource",
                                                     &re2_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL)
        return -1;

    re2_resource_type = rt;

    init_atoms(env);

    return 0;
}

static void cleanup_handle(re2_handle* handle)
{
    if (handle->re != NULL)
    {
        handle->re->~RE2();
        enif_free(handle->re);
        handle->re = NULL;
    }
}

static void re2_resource_cleanup(ErlNifEnv*, void* arg)
{
    // Delete any dynamically allocated memory stored in re2_handle
    re2_handle* handle = (re2_handle*)arg;
    cleanup_handle(handle);
}

static ERL_NIF_TERM re2_compile(ErlNifEnv* env, int argc,
                                const ERL_NIF_TERM argv[])
{
    ErlNifBinary pdata;

    if (enif_inspect_iolist_as_binary(env, argv[0], &pdata))
    {
        const re2::StringPiece p((const char*)pdata.data, pdata.size);
        re2_handle* handle = (re2_handle*)enif_alloc_resource(
            re2_resource_type, sizeof(re2_handle));
        handle->re = NULL;

        re2::RE2::Options re2opts;
        re2opts.set_log_errors(false);

        if (argc == 2 && !parse_compile_options(env, argv[1], re2opts))
        {
            cleanup_handle(handle);
            enif_release_resource(handle);
            return enif_make_badarg(env);
        }

        re2::RE2 *re2 = (re2::RE2*)enif_alloc(sizeof(re2::RE2));
        if (re2 == NULL)
        {
            cleanup_handle(handle);
            enif_release_resource(handle);
            return error(env, a_err_enif_alloc);
        }
        handle->re = new (re2) re2::RE2(p, re2opts); // placement new

        if (!handle->re->ok()) {
            ERL_NIF_TERM error = re2error(env, handle->re);
            cleanup_handle(handle);
            enif_release_resource(handle);
            return error;
        }

        ERL_NIF_TERM result = enif_make_resource(env, handle);
        enif_release_resource(handle);
        return enif_make_tuple2(env, a_ok, result);
    }
    else
    {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM re2_match(ErlNifEnv* env, int argc,
                              const ERL_NIF_TERM argv[])
{
    ErlNifBinary sdata;

    if (enif_inspect_iolist_as_binary(env, argv[0], &sdata))
    {
        const re2::StringPiece s((const char*)sdata.data, sdata.size);
        autohandle<re2::RE2> re;
        union re2_handle_union handle;
        ErlNifBinary pdata;

        matchoptions opts(env);
        if (argc == 3 && !parse_match_options(env, argv[2], opts))
            return enif_make_badarg(env);

        if (enif_get_resource(env, argv[1], re2_resource_type, &handle.vp)
            && handle.p->re != NULL)
        {
            re.set(handle.p->re, true);

            if (opts.caseless) // caseless allowed either in compile or match
                return enif_make_badarg(env);
        }
        else if (enif_inspect_iolist_as_binary(env, argv[1], &pdata))
        {
            const re2::StringPiece p((const char*)pdata.data, pdata.size);
            re2::RE2::Options re2opts;
            re2opts.set_log_errors(false);
            if (opts.caseless)
                re2opts.set_case_sensitive(false);
            re2::RE2* re2 = (re2::RE2*)enif_alloc(sizeof(re2::RE2));
            if (re2 == NULL)
                return error(env, a_err_enif_alloc);
            re.set(new (re2) re2::RE2(p, re2opts)); // placement new
        }
        else
        {
            return enif_make_badarg(env);
        }

        if (!re->ok())
            return enif_make_badarg(env);

        int n = re->NumberOfCapturingGroups()+1;
        std::vector<re2::StringPiece> group;
        group.reserve(n);

        if (re->Match(s, opts.offset, s.size(),
                      re2::RE2::UNANCHORED, &group[0], n))
        {

            int start = 0;
            int arrsz = n;

            if (opts.vs == matchoptions::VS_NONE) {

                // return match atom only

                return a_match;

            } else if (opts.vs == matchoptions::VS_FIRST) {

                // return first match only

                ERL_NIF_TERM first = mres(env, s, group[0], opts.ct);
                if (enif_is_identical(first, a_err_alloc_binary)) {
                    return error(env, a_err_alloc_binary);
                } else {
                    return enif_make_tuple2(env, a_match,
                                            enif_make_list1(env, first));
                }

            } else if (opts.vs == matchoptions::VS_ALL_BUT_FIRST) {
                // skip first match
                start = 1;
                arrsz--;
            }

            if (opts.vs == matchoptions::VS_VLIST) {

                // return matched subpatterns as specified in ValueList

                return re2_match_ret_vlist(env, re, s, opts, group, n);

            } else {

                // return all or all_but_first matches

                ERL_NIF_TERM* arr =
                    (ERL_NIF_TERM*)enif_alloc(sizeof(ERL_NIF_TERM)*n);
                for(int i = start, arridx=0; i < n; i++,arridx++) {
                    ERL_NIF_TERM res = mres(env, s, group[i], opts.ct);
                    if (enif_is_identical(res, a_err_alloc_binary)) {
                        enif_free(arr);
                        return error(env, a_err_alloc_binary);
                    } else {
                        arr[arridx] = res;
                    }
                }

                ERL_NIF_TERM list = enif_make_list_from_array(env,arr,arrsz);
                enif_free(arr);

                return enif_make_tuple2(env, a_match, list);
            }

        } else {

            return a_nomatch;
        }

    } else {

        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM re2_match_ret_vlist(ErlNifEnv* env,
                                        const autohandle<re2::RE2>& re,
                                        const re2::StringPiece& s,
                                        const matchoptions& opts,
                                        std::vector<re2::StringPiece>& group,
                                        int n)
{
    std::vector<ERL_NIF_TERM> vec;
    const std::map<std::string, int>& nmap = re->NamedCapturingGroups();
    ERL_NIF_TERM VL,VH,VT;

    // empty StringPiece for unfound ValueIds
    const re2::StringPiece empty;

    for (VL=opts.vlist; enif_get_list_cell(env, VL, &VH, &VT); VL=VT) {
        int nid = 0;

        if (enif_get_int(env, VH, &nid) && nid > 0) {

            // ValueID int()

            if (nid < n) {
                const re2::StringPiece match = group[nid];
                ERL_NIF_TERM res;
                if (!match.empty())
                    res = mres(env, s, group[nid], opts.ct);
                else
                    res = mres(env, s, empty, opts.ct);

                if (enif_is_identical(res, a_err_alloc_binary))
                    return error(env, a_err_alloc_binary);
                else
                    vec.push_back(res);

            } else {
                vec.push_back(mres(env, s, empty, opts.ct));
            }

        } else if (enif_is_atom(env, VH)) {

            // ValueID atom()

            unsigned atom_len;
            char *a_id = alloc_atom(env, VH, &atom_len);
            if (a_id == NULL)
                return error(env, a_err_enif_alloc);

            if (enif_get_atom(env, VH, a_id, atom_len, ERL_NIF_LATIN1) > 0) {
                std::map<std::string, int>::const_iterator it =
                    nmap.find(a_id);

                ERL_NIF_TERM res;
                if (it != nmap.end())
                    res = mres(env, s, group[it->second], opts.ct);
                else
                    res = mres(env, s, empty, opts.ct);

                if (enif_is_identical(res, a_err_alloc_binary))
                    return error(env, a_err_alloc_binary);
                else
                    vec.push_back(res);
            }
            else
            {
                enif_free(a_id);
                return error(env, a_err_get_atom);
            }

            enif_free(a_id);

        } else {

            // ValueID string()

            unsigned str_len;
            char *str_id = alloc_str(env, VH, &str_len);
            if (str_id == NULL)
                return error(env, a_err_enif_alloc);

            if (enif_get_string(env, VH, str_id, str_len,
                                ERL_NIF_LATIN1) > 0)
            {
                std::map<std::string, int>::const_iterator it =
                    nmap.find(str_id);

                ERL_NIF_TERM res;
                if (it != nmap.end())
                    res = mres(env, s, group[it->second], opts.ct);
                else
                    res = mres(env, s, empty, opts.ct);

                if (enif_is_identical(res, a_err_alloc_binary))
                    return error(env, a_err_alloc_binary);
                else
                    vec.push_back(res);
            }
            else
            {
                enif_free(str_id);
                return error(env, a_err_get_string);
            }

            enif_free(str_id);
        }
    }

    ERL_NIF_TERM list = enif_make_list_from_array(env,&vec[0],vec.size());
    return enif_make_tuple2(env, a_match, list);
}

static ERL_NIF_TERM re2_replace(ErlNifEnv* env, int argc,
                                const ERL_NIF_TERM argv[])
{
    ErlNifBinary sdata, rdata;

    if (enif_inspect_iolist_as_binary(env, argv[0], &sdata) &&
        enif_inspect_iolist_as_binary(env, argv[2], &rdata))
    {
        std::string s((const char*)sdata.data, sdata.size);
        const re2::StringPiece r((const char*)rdata.data, rdata.size);
        autohandle<re2::RE2> re;
        union re2_handle_union handle;
        ErlNifBinary pdata;

        if (enif_get_resource(env, argv[1], re2_resource_type, &handle.vp)
            && handle.p->re != NULL)
        {
            re.set(handle.p->re, true);
        }
        else if (enif_inspect_iolist_as_binary(env, argv[1], &pdata))
        {
            const re2::StringPiece p((const char*)pdata.data, pdata.size);
            re2::RE2::Options re2opts;
            re2opts.set_log_errors(false);
            re2::RE2* re2 = (re2::RE2*)enif_alloc(sizeof(re2::RE2));
            if (re2 == NULL)
                return error(env, a_err_enif_alloc);
            re.set(new (re2) re2::RE2(p, re2opts)); // placement new
        }
        else
        {
            return enif_make_badarg(env);
        }

        if (!re->ok())
            return enif_make_badarg(env);

        replaceoptions opts;
        if (argc == 4 && !parse_replace_options(env, argv[3], opts))
            return enif_make_badarg(env);

        if (opts.global)
        {
            if (re2::RE2::GlobalReplace(&s, *(&re), r))
            {
                return rres(env, s);
            }
            else
            {
                return a_error;
            }
        }
        else
        {
            if (re2::RE2::Replace(&s, *(&re), r))
            {
                return rres(env, s);
            }
            else
            {
                return a_error;
            }
        }
    }
    else
    {
        return enif_make_badarg(env);
    }
}

//
// internal functions
//

static void init_atoms(ErlNifEnv* env)
{
    a_ok                         = enif_make_atom(env, "ok");
    a_error                      = enif_make_atom(env, "error");
    a_match                      = enif_make_atom(env, "match");
    a_nomatch                    = enif_make_atom(env, "nomatch");
    a_capture                    = enif_make_atom(env, "capture");
    a_global                     = enif_make_atom(env, "global");
    a_offset                     = enif_make_atom(env, "offset");
    a_all                        = enif_make_atom(env, "all");
    a_all_but_first              = enif_make_atom(env, "all_but_first");
    a_first                      = enif_make_atom(env, "first");
    a_none                       = enif_make_atom(env, "none");
    a_index                      = enif_make_atom(env, "index");
    a_binary                     = enif_make_atom(env, "binary");
    a_caseless                   = enif_make_atom(env, "caseless");
    a_max_mem                    = enif_make_atom(env, "max_mem");
    a_err_alloc_binary           = enif_make_atom(env, "alloc_binary");
    a_err_enif_alloc             = enif_make_atom(env, "enif_alloc");
    a_err_get_atom               = enif_make_atom(env, "enif_get_atom");
    a_err_get_string             = enif_make_atom(env, "enif_get_string");
    a_re2_NoError                = enif_make_atom(env, "no_error");
    a_re2_ErrorInternal          = enif_make_atom(env, "internal");
    a_re2_ErrorBadEscape         = enif_make_atom(env, "bad_escape");
    a_re2_ErrorBadCharClass      = enif_make_atom(env, "bad_char_class");
    a_re2_ErrorBadCharRange      = enif_make_atom(env, "bad_char_range");
    a_re2_ErrorMissingBracket    = enif_make_atom(env, "missing_bracket");
    a_re2_ErrorMissingParen      = enif_make_atom(env, "missing_paren");
    a_re2_ErrorTrailingBackslash = enif_make_atom(env, "trailing_backslash");
    a_re2_ErrorRepeatArgument    = enif_make_atom(env, "repeat_argument");
    a_re2_ErrorRepeatSize        = enif_make_atom(env, "repeat_size");
    a_re2_ErrorRepeatOp          = enif_make_atom(env, "repeat_op");
    a_re2_ErrorBadPerlOp         = enif_make_atom(env, "bad_perl_op");
    a_re2_ErrorBadUTF8           = enif_make_atom(env, "bad_utf8");
    a_re2_ErrorBadNamedCapture   = enif_make_atom(env, "bad_named_capture");
    a_re2_ErrorPatternTooLarge   = enif_make_atom(env, "pattern_too_large");
}

//
// Options = [ Option ]
// Option = caseless | {max_mem, int()}
//
static bool parse_compile_options(ErlNifEnv* env, const ERL_NIF_TERM list,
                                  re2::RE2::Options& opts)
{
    if (enif_is_empty_list(env, list))
        return true;

    ERL_NIF_TERM L,H,T;

    for (L=list; enif_get_list_cell(env, L, &H, &T); L=T) {
        const ERL_NIF_TERM *tuple;
        int tuplearity = -1;

        if (enif_is_identical(H, a_caseless)) {

            // caseless

            opts.set_case_sensitive(false);

        } else if (enif_get_tuple(env, H, &tuplearity, &tuple)) {

            if (tuplearity == 2) {

                if (enif_is_identical(tuple[0], a_max_mem)) {

                    // {max_mem, int()}

                    int max_mem = 0;
                    if (enif_get_int(env, tuple[1], &max_mem))
                        opts.set_max_mem(max_mem);
                    else
                        return false;

                }
            }
        } else {
            return false;
        }
    }

    return true;
}

//
// Options = [ Option ]
// Option = caseless | {offset, non_neg_integer()}
//          | {capture,ValueSpec} | {capture,ValueSpec,Type}
// Type = index | binary
// ValueSpec = all | all_but_first | first | none | ValueList
// ValueList = [ ValueID ]
// ValueID = int() | string() | atom()
//
static bool parse_match_options(ErlNifEnv* env, const ERL_NIF_TERM list,
                                matchoptions& opts)
{
    if (enif_is_empty_list(env, list))
        return true;

    ERL_NIF_TERM L,H,T;

    for (L=list; enif_get_list_cell(env, L, &H, &T); L=T) {
        const ERL_NIF_TERM *tuple;
        int tuplearity = -1;

        if (enif_is_identical(H, a_caseless)) {

            // caseless

            opts.caseless = true;

        } else if (enif_get_tuple(env, H, &tuplearity, &tuple)) {

            if (tuplearity == 2 || tuplearity == 3) {

                // {offset,N} or {capture,ValueSpec}

                if (enif_is_identical(tuple[0], a_offset)) {

                    // {offset, int()}

                    int offset = 0;
                    if (enif_get_int(env, tuple[1], &offset)) {
                        opts.offset = offset;
                    } else {
                        return false;
                    }

                } else if (enif_is_identical(tuple[0], a_capture)) {

                    // {capture,ValueSpec,Type}
                    parse_match_capture_options(env, opts, tuple, tuplearity);

                }
            }
        } else {
            return false;
        }
    }

    return true;
}


static void parse_match_capture_options(ErlNifEnv* env, matchoptions& opts,
                                        const ERL_NIF_TERM* tuple,
                                        int tuplearity)
{
    bool vs_set = false;
    if (enif_is_atom(env, tuple[1])) {

        // ValueSpec = all | all_but_first | first | none

        if (enif_is_atom(env, tuple[1]) > 0) {

            if (enif_is_identical(tuple[1], a_all))
                opts.vs = matchoptions::VS_ALL;
            else if (enif_is_identical(tuple[1], a_all_but_first))
                opts.vs = matchoptions::VS_ALL_BUT_FIRST;
            else if (enif_is_identical(tuple[1], a_first))
                opts.vs = matchoptions::VS_FIRST;
            else if (enif_is_identical(tuple[1], a_none))
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

        if (enif_is_identical(tuple[2], a_index))
            opts.ct = matchoptions::CT_INDEX;
        else if (enif_is_identical(tuple[2], a_binary))
            opts.ct = matchoptions::CT_BINARY;
    }
}

//
// Options = [ Option ]
// Option = global
//
static bool parse_replace_options(ErlNifEnv* env, const ERL_NIF_TERM list,
                                  replaceoptions& opts)
{
    if (enif_is_empty_list(env, list))
        return true;

    ERL_NIF_TERM L,H,T;

    for (L=list; enif_get_list_cell(env, L, &H, &T); L=T) {

        if (enif_is_identical(H, a_global))
            opts.global = true;
        else
            return false;
    }

    return true;
}

//
// build result for re2:replace
//
static ERL_NIF_TERM rres(ErlNifEnv* env, const std::string& s)
{
    ErlNifBinary bsubst;
    if(!enif_alloc_binary(s.size(), &bsubst))
        return error(env, a_err_alloc_binary);
    memcpy(bsubst.data, s.data(), s.size());
    return enif_make_binary(env, &bsubst);
}

//
// build result for re2:match
//
static ERL_NIF_TERM mres(ErlNifEnv* env,
                         const re2::StringPiece& str,
                         const re2::StringPiece& match,
                         const matchoptions::capture_type ct)
{
    switch (ct) {
    case matchoptions::CT_BINARY:
        ErlNifBinary bmatch;
        if(!enif_alloc_binary(match.size(), &bmatch))
            return a_err_alloc_binary;
        memcpy(bmatch.data, match.data(), match.size());
        return enif_make_binary(env, &bmatch);
    default:
    case matchoptions::CT_INDEX:
        int l, r;
        if (match.empty()) {
            l = -1;
            r = 0;
        } else {
            l = match.data() - str.data();
            r = match.size();
        }
        return enif_make_tuple2(env,
                                enif_make_int(env, l),
                                enif_make_int(env, r));
    }
}

static ERL_NIF_TERM error(ErlNifEnv* env, const ERL_NIF_TERM err)
{
    return enif_make_tuple2(env, a_error, err);
}

//
// convert RE2 error code to error term
//
static ERL_NIF_TERM re2error(ErlNifEnv* env, const re2::RE2* const re)
{
    ERL_NIF_TERM code;

    switch (re->error_code()) {
    case re2::RE2::ErrorInternal:          // Unexpected error
        code = a_re2_ErrorInternal;
        break;
        // Parse errors
    case re2::RE2::ErrorBadEscape:         // bad escape sequence
        code = a_re2_ErrorBadEscape;
        break;
    case re2::RE2::ErrorBadCharClass:      // bad character class
        code = a_re2_ErrorBadCharClass;
        break;
    case re2::RE2::ErrorBadCharRange:      // bad character class range
        code = a_re2_ErrorBadCharRange;
        break;
    case re2::RE2::ErrorMissingBracket:    // missing closing ]
        code = a_re2_ErrorMissingBracket;
        break;
    case re2::RE2::ErrorMissingParen:      // missing closing )
        code = a_re2_ErrorMissingParen;
        break;
    case re2::RE2::ErrorTrailingBackslash: // trailing \ at end of regexp
        code = a_re2_ErrorTrailingBackslash;
        break;
    case re2::RE2::ErrorRepeatArgument:    // repeat argument missing, e.g. "*"
        code = a_re2_ErrorRepeatArgument;
        break;
    case re2::RE2::ErrorRepeatSize:        // bad repetition argument
        code = a_re2_ErrorRepeatSize;
        break;
    case re2::RE2::ErrorRepeatOp:          // bad repetition operator
        code = a_re2_ErrorRepeatOp;
        break;
    case re2::RE2::ErrorBadPerlOp:         // bad perl operator
        code = a_re2_ErrorBadPerlOp;
        break;
    case re2::RE2::ErrorBadUTF8:           // invalid UTF-8 in regexp
        code = a_re2_ErrorBadUTF8;
        break;
    case re2::RE2::ErrorBadNamedCapture:   // bad named capture group
        code = a_re2_ErrorBadNamedCapture;
        break;
    case re2::RE2::ErrorPatternTooLarge:   // pattern too large (compile failed)
        code = a_re2_ErrorPatternTooLarge;
        break;
    default:
    case re2::RE2::NoError:
        code = a_re2_NoError;
        break;
    }

    ERL_NIF_TERM error = enif_make_string(env, re->error().c_str(),
                                          ERL_NIF_LATIN1);
    ERL_NIF_TERM error_arg = enif_make_string(env, re->error_arg().c_str(),
                                              ERL_NIF_LATIN1);

    return enif_make_tuple2(env, a_error,
                            enif_make_tuple3(env, code, error, error_arg));
}

static char* alloc_atom(ErlNifEnv* env, const ERL_NIF_TERM atom, unsigned* len)
{
    unsigned atom_len;
    if (!enif_get_atom_length(env, atom, &atom_len, ERL_NIF_LATIN1))
        return NULL;
    atom_len++;
    *len = atom_len;
    return (char*)enif_alloc(atom_len);
}

static char* alloc_str(ErlNifEnv* env, const ERL_NIF_TERM list, unsigned* len)
{
    unsigned list_len;
    if (!enif_get_list_length(env, list, &list_len))
        return NULL;
    list_len++;
    *len = list_len;
    return (char*)enif_alloc(list_len);
}
