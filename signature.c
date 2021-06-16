
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <libIDL/IDL.h>
#include <ccan/hash/hash.h>
#include <ccan/talloc/talloc.h>
#include <ccan/darray/darray.h>

#include "defs.h"


static char *struct_sig_str(void *talctx, IDL_tree struc)
{
	void *tmpctx = talloc_new(talctx);
	darray(char) buf = darray_new();

	darray_append_string(buf, IDL_IDENT(IDL_TYPE_STRUCT(struc).ident).repo_id);
	darray_append(buf, '=');
	IDL_LIST_FOREACH(cur, IDL_TYPE_STRUCT(struc).member_list) {
		IDL_tree m = IDL_LIST(cur).data,
			type = get_type_spec(IDL_MEMBER(m).type_spec);
		if(!IDL_NODE_IS_TYPE(type) && IDL_NODE_TYPE(type) != IDLN_NATIVE) {
			fprintf(stderr, "%s: warning: `%s' not a type, or native?\n",
				__func__, IDL_NODE_TYPE_NAME(type));
			continue;
		}

		char typ = 0, *tmp = NULL;
		switch(IDL_NODE_TYPE(type)) {
			case IDLN_TYPE_INTEGER:
				switch(IDL_TYPE_INTEGER(type).f_type) {
					case IDL_INTEGER_TYPE_SHORT: typ = 'h'; break;
					case IDL_INTEGER_TYPE_LONG: typ = 'i'; break;
					case IDL_INTEGER_TYPE_LONGLONG: typ = 'j'; break;
					default: fprintf(stderr, "invalid integer f_type\n"); abort();
				}
				if(!IDL_TYPE_INTEGER(type).f_signed) typ = toupper(typ);
				break;
			case IDLN_TYPE_OCTET: typ = 'o'; break;
			case IDLN_TYPE_CHAR: typ = 'c'; break;
			case IDLN_TYPE_WIDE_CHAR: typ = 'C'; break;
			case IDLN_TYPE_BOOLEAN: typ = 'b'; break;
			case IDLN_TYPE_STRING:
				tmp = talloc_asprintf(tmpctx, "s%u",
					(unsigned)IDL_INTEGER(IDL_TYPE_STRING(type).positive_int_const).value);
				break;
			case IDLN_TYPE_ENUM:
				tmp = talloc_asprintf(tmpctx, "E<%s>",
					IDL_IDENT(IDL_TYPE_ENUM(type).ident).repo_id);
				break;
			case IDLN_TYPE_ARRAY:
				tmp = talloc_asprintf(tmpctx, "a<%s",
					IDL_IDENT(IDL_TYPE_ARRAY(type).ident).repo_id);
				IDL_LIST_FOREACH(dim_cur, IDL_TYPE_ARRAY(type).size_list) {
					unsigned long long len = IDL_INTEGER(IDL_LIST(dim_cur).data).value;
					tmp = talloc_asprintf(tmpctx, "%s,%llu", tmp, len);
				}
				typ = '>';
				break;
			case IDLN_TYPE_SEQUENCE:
				tmp = talloc_asprintf(tmpctx, "q<%s,%llu>",
					IDL_IDENT(IDL_TYPE_SEQUENCE(type).simple_type_spec).repo_id,
					(unsigned long long)IDL_INTEGER(IDL_TYPE_SEQUENCE(type).positive_int_const).value);
				break;
			case IDLN_TYPE_STRUCT:
				tmp = talloc_asprintf(tmpctx, "S<%s>",
					IDL_IDENT(IDL_TYPE_STRUCT(type).ident).repo_id);
				break;
			case IDLN_TYPE_UNION:
				tmp = talloc_asprintf(tmpctx, "U<%s>",
					IDL_IDENT(IDL_TYPE_UNION(type).ident).repo_id);
				break;
			case IDLN_NATIVE:
				tmp = talloc_asprintf(tmpctx, "N<%s>", NATIVE_NAME(type));
				break;
			default: NOTDEFINED(type);
		}
		if(tmp != NULL) darray_append_string(buf, tmp);
		if(typ != 0) darray_append(buf, typ);
	}

	darray_append(buf, '\0');
	char *ret = talloc_strdup(talctx, buf.item);
	darray_free(buf);
	talloc_free(tmpctx);
	return ret;
}


char *struct_signature(void *talctx, IDL_tree struct_type)
{
	assert(IDL_NODE_TYPE(struct_type) == IDLN_TYPE_STRUCT);
	char *ss = struct_sig_str(talctx, struct_type);
	/* TODO: use a cryptographically strong hash here, such as siphash or
	 * sha256, and return a set of strings.
	 */
	return talloc_asprintf(talctx, "%#" PRIx64 "ull",
		hash64_stable(ss, strlen(ss), 0xdeadbeef01234567ull));
}
