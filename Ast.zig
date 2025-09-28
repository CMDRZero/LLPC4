const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const types = @import("types.zig");

const Token = Tokenizer.Token;
const Type = types.Type;

pub const Struct = struct {
    name: Token,
    fields: [] Field,
    decls: [] Decl,

    pub const Field = struct {
        name: Token,
        @"type": Type,
        default: ?Expr,
    };
};

pub const Decl = union (enum) {
    assgExpr: AssgExpr,
    @"struct": Struct,
};

pub const AssgExpr = struct {
    dataQualf: types.DataQualf,
    lhs: Token,
    typeAnnot: ?Type,
    operator: Token,
    rhs: ?Expr,
};

pub const Expr = struct {
    main_token: Token,
    data: Data,

    pub const Data = union (enum) {
        op_binary: struct {
            result_type: ?void,
            lhs: *Expr,
            rhs: *Expr,
        },
        op_prefix_unary: struct {
            result_type: ?void,
            rhs: *Expr,
        },
        op_suffix_unary: struct {
            result_type: ?void,
            lhs: *Expr,
        },
    }; 
};