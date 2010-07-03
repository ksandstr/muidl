/*
 * llvmutil.c -- utility things for use with the LLVM C API
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

#include "llvmutil.h"


void branch_set_phi(
	struct llvm_ctx *ctx,
	LLVMValueRef phi,
	LLVMValueRef val)
{
	BB current = LLVMGetInsertBlock(ctx->builder);
	LLVMAddIncoming(phi, &val, &current, 1);
}
