/*
 * stub.c -- build stubs for an IDL interface
 * Copyright 2010  Kalle A. Sandström <ksandstr@iki.fi>
 *
 * This file is part of µiX.
 *
 * µiX is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * µiX is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with µiX.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdlib.h>
#include <glib.h>
#include <libIDL/IDL.h>
#include <llvm-c/Core.h>

#include "muidl.h"
#include "llvmutil.h"


static LLVMTypeRef stub_fn_type(
	struct llvm_ctx *ctx,
	struct method_info *inf)
{
	printf("%s: for `%s'\n", __func__, METHOD_NAME(inf->node));

	struct message_info *reply = NULL;
	if(inf->num_reply_msgs > 0) reply = inf->replies[0];

	int num_params = IDL_list_length(IDL_OP_DCL(inf->node).parameter_dcls),
		num_args = num_params * 2 + 2 + 1;
	T arg_types[num_args];
	for(int i=0; i<num_args; i++) arg_types[i] = NULL;
	int max_arg = -1;

	/* TODO: support "implicit" IPC destinations */
	arg_types[++max_arg] = ctx->wordt;	/* L4_ThreadId_t, peer */
	int arg_offset = 1;
	printf("%s: ipc destination goes in arg %d\n", __func__, max_arg);

	if(reply != NULL && reply->ret_type != NULL) {
		assert(IDL_NODE_TYPE(reply->ret_type) != IDLN_TYPE_SEQUENCE);
		arg_types[++max_arg] = LLVMPointerType(llvm_rigid_type(ctx,
			reply->ret_type), 0);
		printf("%s: retval goes in arg %d\n", __func__, max_arg);
		arg_offset++;
	}

	printf("%s: arg_offset is %d\n", __func__, arg_offset);
	T *at_base = &arg_types[arg_offset];
	for(IDL_tree cur = IDL_OP_DCL(inf->node).parameter_dcls;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree pdecl = IDL_LIST(cur).data,
			type = get_type_spec(IDL_PARAM_DCL(pdecl).param_type_spec);
		enum IDL_param_attr attr = IDL_PARAM_DCL(pdecl).attr;
		int nargs = 1;
		struct msg_param *p;
		if(attr == IDL_PARAM_IN) {
			p = find_pdecl(inf->request->params, pdecl);
			assert(p != NULL);
			if(is_value_type(type)) {
				at_base[p->arg_ix] = llvm_value_type(ctx, type);
			} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
				IDL_tree subtype = get_type_spec(
					IDL_TYPE_SEQUENCE(type).simple_type_spec);
				at_base[p->arg_ix + 0] = LLVMPointerType(
					llvm_rigid_type(ctx, subtype), 0);
				at_base[p->arg_ix + 1] = ctx->i32t;
				nargs = 2;
			} else {
				at_base[p->arg_ix] = LLVMPointerType(
					llvm_rigid_type(ctx, type), 0);
			}
		} else /* out, inout */ {
			assert(reply != NULL);
			p = find_pdecl(reply->params, pdecl);
			assert(p != NULL);
			if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
				IDL_tree subtype = get_type_spec(
					IDL_TYPE_SEQUENCE(type).simple_type_spec);
				at_base[p->arg_ix + 0] = LLVMPointerType(
					llvm_rigid_type(ctx, subtype), 0);
				at_base[p->arg_ix + 1] = LLVMPointerType(ctx->i32t, 0);
				nargs = 2;
			} else {
				at_base[p->arg_ix] = LLVMPointerType(
					llvm_rigid_type(ctx, type), 0);
			}
		}
		printf("%s: param in arg slots %d + [%d..%d]\n", __func__,
			arg_offset, p->arg_ix, p->arg_ix + nargs - 1);
		max_arg = MAX(max_arg, p->arg_ix + nargs - 1 + arg_offset);
	}

	if(max_arg < 0) {
		printf("%s: a very small stub! (void)\n", __func__);
		return LLVMFunctionType(ctx->i32t, NULL, 0, 0);
	} else {
		printf("%s: au naturel (num %d)\n", __func__, max_arg + 1);
		return LLVMFunctionType(ctx->i32t, arg_types, max_arg + 1, 0);
	}
}


static void build_ipc_stub(
	struct llvm_ctx *ctx,
	struct method_info *inf,
	int timeout_kind)
{
	/* FIXME: support timeouted stubs, eventually */
	assert(timeout_kind == 0);

	char *stubpfx = get_stub_prefix(inf->node),
		*opname = decapsify(METHOD_NAME(inf->node)),
		*stubname = g_strconcat(
			stubpfx == NULL ? "" : stubpfx,
			stubpfx == NULL ? "" : "_",
			opname,
			timeout_kind != 0 ? "_timeout" : "",
			NULL);
	g_free(stubpfx);
	g_free(opname);

	struct message_info *reply = NULL;
	if(inf->num_reply_msgs > 0) reply = inf->replies[0];
	T fn_type = stub_fn_type(ctx, inf);
	V fn = LLVMAddFunction(ctx->module, stubname, fn_type);
	g_free(stubname);

	ctx->builder = LLVMCreateBuilderInContext(ctx->ctx);
	BB entry_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "EntryBlock");

	int num_args = LLVMCountParams(fn);
	assert(num_args >= 1);
	V *args = g_new(V, num_args);
	LLVMGetParams(fn, args);

	/* TODO: support implicit IPC destinations */
	V ipc_dest = args[0];
	int arg_offset = 1;

	V *ret_args = NULL;
	if(reply != NULL && reply->ret_type != NULL) {
		ret_args = &args[arg_offset++];
	}

	/* prelude. */
	LLVMPositionBuilderAtEnd(ctx->builder, entry_bb);
	ctx->utcb = build_utcb_get(ctx);

	/* function exit. */
	BB exit_bb = add_sibling_block(ctx, "exit");
	LLVMPositionBuilderAtEnd(ctx->builder, exit_bb);
	V retval_phi = LLVMBuildPhi(ctx->builder, ctx->i32t, "retval.phi");
	LLVMBuildRet(ctx->builder, retval_phi);

	/* send-half. */
	LLVMPositionBuilderAtEnd(ctx->builder, entry_bb);
	V tag = build_msg_encoder(ctx, inf->request, NULL, &args[arg_offset],
		false);
	/* FIXME: generate "stritems" from params, load buffer registers */
	/* 0 is L4_Time_t for Never, in both timeout fields */
	ctx->tag = build_l4_ipc_call(ctx, ipc_dest, CONST_WORD(0),
		ipc_dest, tag, NULL, &ctx->mr1, &ctx->mr2);
	/* check for IPC errors. */
	BB err_bb = add_sibling_block(ctx, "ipcerror"),
		noerr_bb = add_sibling_block(ctx, "ipcok");
	V err_bit = LLVMBuildAnd(ctx->builder, ctx->tag,
		CONST_WORD(1 << 15), "ipc.tag.e");
	LLVMBuildCondBr(ctx->builder, LLVMBuildICmp(ctx->builder, LLVMIntEQ,
		CONST_WORD(0), err_bit, "ipc.error.p"), noerr_bb, err_bb);

	/* IPC error handler. */
	LLVMPositionBuilderAtEnd(ctx->builder, err_bb);
	V errcode = LLVMBuildLoad(ctx->builder,
		UTCB_ADDR_VAL(ctx, CONST_INT(-9), "ec.addr"), "ec.value");
	branch_set_phi(ctx, retval_phi, errcode);
	LLVMBuildBr(ctx->builder, exit_bb);

	/* IPC success path. */
	LLVMPositionBuilderAtEnd(ctx->builder, noerr_bb);
	/* TODO: check for exceptions */
	if(reply != NULL) {
		build_msg_decoder(ctx, ret_args, &args[arg_offset], reply, NULL,
			true);
	}
	branch_set_phi(ctx, retval_phi, CONST_WORD(0));
	LLVMBuildBr(ctx->builder, exit_bb);

	LLVMDisposeBuilder(ctx->builder);
	ctx->builder = NULL;

	g_free(args);
}


gboolean iter_build_stubs(IDL_tree_func_data *tf, void *ud)
{
	struct llvm_ctx *ctx = ud;
	switch(IDL_NODE_TYPE(tf->tree)) {
		case IDLN_LIST:
		case IDLN_MODULE:
		case IDLN_SRCFILE:
			return TRUE;

		default: return FALSE;

		case IDLN_INTERFACE: {
			GList *tagmask_list = NULL,
				*methods = analyse_methods_of_iface(ctx->pr,
					&tagmask_list, tf->tree);
			GLIST_FOREACH(cur, methods) {
				struct method_info *inf = cur->data;
				/* (TODO: don't build the timeoutless variant if given an
				 * attribute. see header.c for details.)
				 */
				build_ipc_stub(ctx, inf, 0);
				int tok = op_timeout_kind(inf->node);
				if(tok != 0) {
					/* TODO */
#if 0
					build_ipc_stub(ctx, inf, tok);
#endif
				}
			}
			g_list_foreach(methods, (GFunc)&free_method_info, NULL);
			g_list_free(methods);
			g_list_free(tagmask_list);
			return FALSE;
		}
	}
}
