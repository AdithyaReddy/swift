//===--- Syntax.td - Top-level Syntax Tablegen ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file is the main input to the SwiftSyntax tablegen target,
// which generates both the interface and implementation of lib/Syntax
// entities.
//
//===----------------------------------------------------------------------===//

include "CommonSyntax.td"
include "TokenSyntax.td"
include "AttributeSyntax.td"
include "StmtSyntax.td"
include "DeclSyntax.td"
include "ExprSyntax.td"
include "PatternSyntax.td"
include "TypeSyntax.td"

